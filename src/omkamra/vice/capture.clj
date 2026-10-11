(ns omkamra.vice.capture
  "Non-GUI VICE lifecycle and v2 raw-capture orchestration.

  Responsibilities are split across threads with a bounded queue between them:

  * the **reader thread** opens the trace FIFO, parses text records with
    `omkamra.vice.trace`, and puts them on a bounded `ArrayBlockingQueue`.
    It reopens the FIFO after each end of stream, and stops when the session's
    stop flag is set.
  * the **controller thread** (the session worker) converts records into packed
    events, assembles chunks, and owns the boundary protocol.
  * a short-lived **resume future** starts execution after the controller is
    already draining, because monitor-log writing blocks both VICE and the
    reader when the bounded queue fills.

  Boundary protocol:

  1. an execute-only, non-stopping tracepoint produces trace records;
  2. when the approximate event target is crossed the controller disables
     `MonitorLogEnabled`, which pauses VICE and closes the log file;
  3. the reader therefore reaches end of stream, draining every record emitted
     before the pause into the closing chunk;
  4. the controller reads the complete canonical snapshot while VICE remains
     paused, then re-enables logging and resumes;
  5. the first event after the resume supplies the next chunk's boundary
     timestamp, so a chunk's snapshot and timing are committed at the *next*
     boundary, when its timing is already known.

  A stop commits the non-empty final chunk and finalizes the manifest. Any
  failure destroys the reserved capture directory."
  (:require [clojure.java.io :as io]
            [omkamra.vice :as vice]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.trace :as trace]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.snapshot :as snapshot])
  (:import [java.io Reader]
           [java.net ServerSocket]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(def ^:private program-entry-address 0xe144)
(def ^:private basic-program-address 0x0801)
(def ^:private basic-sys-token 0x9e)
(def ^:private basic-probe-bytes 128)
(def ^:private basic-max-lines 8)
(def ^:private default-connect-timeout-ms 30000)
(def ^:private default-connect-retry-ms 100)
(def ^:private default-autostart-timeout-ms 5000)
(def ^:private default-program-start-timeout-ms 30000)
(def ^:private default-event-target 100000)
(def ^:private default-queue-capacity 65536)
(def ^:private eof-sentinel ::eof)

(defn- fail
  [message data]
  (throw (ex-info message data)))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- elapsed-ms
  [start-nanos]
  (quot (- (System/nanoTime) start-nanos) 1000000))

(defn- canonical-file
  ^java.io.File [path]
  (.getCanonicalFile (io/file path)))

(defn- process-alive?
  [instance]
  (or (nil? (:process instance))
      (.isAlive ^Process (:process instance))))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

;; Input and dataset validation ---------------------------------------------

(defn- validate-input!
  [input]
  (when-not input
    (fail "Capture requires :input" {}))
  (let [file (canonical-file input)
        name (.toLowerCase (.getName file))]
    (when-not (.isFile file)
      (fail "Capture input must be an existing file" {:input input}))
    (when-not (or (.endsWith name ".prg")
                  (.endsWith name ".d64"))
      (fail "Capture input must be a .prg or .d64 file"
            {:input (.getPath file)}))
    (.getPath file)))

(defn- prepare-dataset-root!
  [dataset-root]
  (when-not dataset-root
    (fail "Capture requires :dataset-root" {}))
  (let [directory (canonical-file dataset-root)]
    (when (and (.exists directory) (not (.isDirectory directory)))
      (fail "Capture dataset root is not a directory"
            {:dataset-root (.getPath directory)}))
    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (fail "Could not create the capture dataset root"
              {:dataset-root (.getPath directory)})))
    (.getPath directory)))

(defn- make-capture-id
  [requested]
  (let [id (or requested
               (str "capture-"
                    (System/currentTimeMillis)
                    "-"
                    (subs (str (UUID/randomUUID)) 0 8)))]
    (when-not (and (string? id) (re-matches #"[A-Za-z0-9._-]+" id))
      (fail "Capture ID must contain only letters, numbers, '.', '_' or '-'"
            {:capture-id id}))
    id))

(defn- event-target
  [options]
  (let [target (or (:event-target options) default-event-target)]
    (when-not (pos-int? target)
      (fail "Capture :event-target must be a positive integer"
            {:event-target target}))
    target))

(defn- queue-capacity
  [options]
  (let [capacity (or (:record-queue-capacity options) default-queue-capacity)]
    (when-not (pos-int? capacity)
      (fail "Capture :record-queue-capacity must be a positive integer"
            {:record-queue-capacity capacity}))
    capacity))

;; VICE process and monitor --------------------------------------------------

(defn- vice-extra-args
  [options]
  (cond-> (vec (or (:extra-args options) []))
    (some? (:speed options)) (conj "-speed" (str (:speed options)))
    (true? (:warp? options)) (conj "-warp")))

(defn- connect-when-ready
  [instance {:keys [connect-timeout-ms connect-retry-ms]}]
  (let [deadline (+ (now-ms) (or connect-timeout-ms default-connect-timeout-ms))]
    (loop []
      (when-not (process-alive? instance)
        (fail "VICE exited before its binary monitor became ready"
              {:pid (:pid instance)}))
      (let [conn (try
                   (let [conn (vice/connect instance (fn [_ _] nil))]
                     (bm/ping conn)
                     conn)
                   (catch Throwable _ nil))]
        (cond
          conn conn
          (< (now-ms) deadline)
          (do (Thread/sleep (long (or connect-retry-ms default-connect-retry-ms)))
              (recur))
          :else
          (fail "Timed out waiting for the VICE binary monitor"
                {:pid (:pid instance)}))))))

(defn- set-logging!
  [session value]
  (bm/resource-set @(:conn session) {:name "MonitorLogEnabled" :value value}))

;; Program-entry start policy ------------------------------------------------

(defn- monitor-bytes
  "Read `[start end]` (inclusive) from CPU-visible memory through the monitor."
  ^bytes [conn start end]
  (:memory (bm/mem-get conn {:start start :end end :memspace 0})))

(defn- unsigned-byte
  [^bytes bytes index]
  (bit-and (int (aget bytes index)) 0xff))

(defn- little-endian-u16
  [^bytes bytes index]
  (bit-or (unsigned-byte bytes index)
          (bit-shift-left (unsigned-byte bytes (inc index)) 8)))

(defn- decimal-after
  "Parse the decimal operand at or after `start`.

  Skips spaces, and a parenthesized operand's opening `(` (BASIC accepts
  `SYS (2064)`). Returns nil when no digit is present before `limit`."
  [^bytes bytes start limit]
  (loop [index (long start)
         value nil]
    (if (< index (long limit))
      (let [byte (unsigned-byte bytes index)]
        (cond
          (or (= byte 0x20)
              (and (nil? value) (= byte 0x28))) (recur (inc index) value)
          (<= 0x30 byte 0x39)
          (recur (inc index) (+ (* (long (or value 0)) 10) (- byte 0x30)))
          :else value))
      value)))

(defn- sys-target-in-text
  "Decimal operand of the first BASIC SYS token in `[start limit)`, or nil."
  [^bytes bytes start limit]
  (loop [index (long start)]
    (when (< index (long limit))
      (if (= basic-sys-token (unsigned-byte bytes index))
        (decimal-after bytes (inc index) limit)
        (recur (inc index))))))

(defn- basic-sys-target
  "Return the machine-language entry of the autostarted BASIC stub, or nil.

  A PRG autostarted through VICE is usually a tokenized `SYS <address>` stub at
  `$0801`. Walking the program's line links and parsing the operand of the first
  SYS token yields the address of the program's first machine instruction."
  [conn]
  (let [bytes (monitor-bytes conn basic-program-address
                             (+ basic-program-address (dec basic-probe-bytes)))]
    (loop [offset 0
           lines 0]
      (if (or (> (+ offset 4) (alength bytes))
              (>= lines basic-max-lines))
        nil
        (let [link (little-endian-u16 bytes offset)]
          (if (zero? link)
            nil
            (let [text-start (+ offset 4)
                  next-offset (- link basic-program-address)
                  limit (if (and (>= next-offset text-start)
                                 (<= next-offset (alength bytes)))
                          next-offset
                          (alength bytes))]
              (or (sys-target-in-text bytes text-start limit)
                  (recur (long next-offset) (inc lines))))))))))

(defn- set-execute-checkpoint!
  [conn address]
  (bm/checkpoint-set conn {:start address
                           :end address
                           :stop? true
                           :enabled? true
                           :op bm/MON_CHECKPOINT_OP_EXECUTE
                           :temporary? false}))

(defn- set-program-start-checkpoint!
  [conn]
  (set-execute-checkpoint! conn program-entry-address))

(defn- wait-for-stop-at!
  [conn checkpoint-number address timeout-ms]
  (or (bm/await-event
       conn
       (fn [{:keys [response-type response]}]
         (and (= bm/MON_RESPONSE_CHECKPOINT_INFO response-type)
              (= checkpoint-number (:number response))
              (:hit? response)))
       timeout-ms)
      (fail "VICE did not reach the requested execution checkpoint"
            {:checkpoint-number checkpoint-number
             :address address}))
  (or (bm/await-event
       conn
       (fn [{:keys [response-type response]}]
         (and (= bm/MON_RESPONSE_STOPPED response-type)
              (= address (:pc response))))
       timeout-ms)
      (fail "VICE did not stop at the requested execution checkpoint"
            {:address address})))

(defn- wait-for-program-start!
  [conn checkpoint-number timeout-ms]
  (wait-for-stop-at! conn checkpoint-number program-entry-address timeout-ms))

(defn- refine-program-start!
  "Continue from the BASIC RUN stop to the stub's SYS target.

  The autostart path stops at `$E144` (BASIC RUN); the loaded stub is normally
  `SYS <address>`, so resume and stop again at that address. The capture then
  begins on the program's first machine instruction instead of inside the BASIC
  interpreter, and the initial snapshot is taken at that instruction. Returns
  the target it stopped at, or nil when the program has no SYS stub."
  [session program-checkpoint options]
  (let [conn @(:conn session)
        target (basic-sys-target conn)]
    (when (and target (not= target program-entry-address))
      (let [checkpoint (set-execute-checkpoint! conn target)]
        (reset! program-checkpoint checkpoint)
        (bm/resume conn)
        (wait-for-stop-at! conn (:number checkpoint) target
                           (or (:program-start-timeout-ms options)
                               default-program-start-timeout-ms))
        (bm/checkpoint-delete conn {:number (:number checkpoint)})
        (reset! program-checkpoint nil)
        target))))

(defn- autostart-options
  [session options]
  {:run-after-load? (if (contains? options :run-after-load?)
                      (:run-after-load? options)
                      true)
   :file-index 0
   :filename (:input session)
   :timeout-ms (or (:autostart-timeout-ms options)
                   default-autostart-timeout-ms)})

;; FIFO ----------------------------------------------------------------------

(defn- create-fifo!
  [session]
  (let [fifo (str "/tmp/omkamra-vice-v2/trace-" (System/nanoTime) ".fifo")]
    (.mkdirs (.getParentFile (io/file fifo)))
    (io/delete-file fifo true)
    (let [^java.util.List command ["mkfifo" fifo]
          process (.start (ProcessBuilder. command))]
      (when-not (.waitFor process 5 TimeUnit/SECONDS)
        (.destroyForcibly process)
        (fail "Timed out creating the trace FIFO" {:fifo fifo}))
      (when-not (zero? (.exitValue process))
        (fail "Could not create the trace FIFO"
              {:fifo fifo :exit-code (.exitValue process)})))
    (reset! (:fifo session) fifo)
    fifo))

(defn- start-fifo-reader!
  "Read the trace FIFO forever, reopening after each end of stream.

  After a boundary the log file is closed, so the reader reaches EOF, queues the
  sentinel, and blocks reopening the FIFO until logging is re-enabled. On final
  stop `:stop-requested` is set before logging is disabled, so the reader queues
  the last sentinel and then exits instead of reopening. The `recur` must stay
  inside the `when-not`; otherwise a stopped reader spins on an empty body."
  [session fifo]
  (let [^ArrayBlockingQueue queue (:queue session)
        stop-requested (:stop-requested session)
        reader-ref (:reader session)
        body (fn []
               (try
                 (loop []
                   (when-not @stop-requested
                     (let [reader (io/reader fifo)]
                       (reset! reader-ref reader)
                       (try
                         (trace/reduce-records!
                          reader
                          (fn [record] (.put queue record))
                          true)
                         (finally
                           (reset! reader-ref nil)))
                       (.put queue eof-sentinel))
                     (recur)))
                 (catch Throwable error
                   (try (.put queue {:error error})
                        (catch Throwable _ nil)))))
        thread (doto (Thread. ^Runnable body)
                 (.setName "vice-v2-reader")
                 (.setDaemon true)
                 (.start))]
    (reset! (:reader-thread session) thread)
    thread))

(defn- release-fifo-open!
  "Release a reader blocked inside its FIFO open.

  A POSIX FIFO opens for reading only once a writer appears, so a reader that
  has not yet opened blocks until VICE enables logging. Opening the write end
  briefly lets that open complete; the reader then reads EOF and leaves its loop
  because `:stop-requested` is set."
  [fifo]
  (when fifo
    (try
      (let [out (io/output-stream fifo)]
        (.close out))
      (catch Throwable _ nil))))

(defn- stop-fifo-reader!
  [session]
  (reset! (:stop-requested session) true)
  (when-let [reader @(:reader session)]
    (try (.close ^Reader reader) (catch Throwable _ nil)))
  (when-let [^Thread thread @(:reader-thread session)]
    ;; A reader still inside its blocking FIFO open cannot be interrupted, so
    ;; release it from a short-lived daemon thread. The writer open itself
    ;; blocks when no reader is waiting, hence the timeout rather than a plain
    ;; call on this thread.
    (when (and (.isAlive thread) (nil? @(:reader session)))
      (let [releaser (doto (Thread. #(release-fifo-open! @(:fifo session)))
                       (.setName "vice-v2-reader-release")
                       (.setDaemon true)
                       (.start))]
        (.join releaser 1000)))
    (.join thread 2000)
    (when (.isAlive thread)
      (.interrupt thread)
      (.join thread 1000))))

;; Metrics -------------------------------------------------------------------

(defn- start-metrics!
  [session]
  (reset! (:metrics session)
          {:event-target (:event-target session)
           :events 0
           :chunks 0
           :overshoot-total 0
           :overshoot-max 0
           :snapshot-ms-total 0
           :snapshot-ms-max 0
           :boundary-ms-total 0
           :boundary-ms-max 0
           :commit-ms-total 0
           :commit-ms-max 0
           :started-at (now-ms)}))

(defn- record-boundary-metrics!
  [session {:keys [events overshoot snapshot-ms boundary-ms commit-ms]}]
  (swap! (:metrics session)
         (fn [metrics]
           (-> metrics
               (assoc :events (+ (:events metrics) (long (or events 0)))
                      :chunks (inc (:chunks metrics)))
               (update :overshoot-total + (long (or overshoot 0)))
               (update :overshoot-max max (long (or overshoot 0)))
               (update :snapshot-ms-total + (long (or snapshot-ms 0)))
               (update :snapshot-ms-max max (long (or snapshot-ms 0)))
               (update :boundary-ms-total + (long (or boundary-ms 0)))
               (update :boundary-ms-max max (long (or boundary-ms 0)))
               (update :commit-ms-total + (long (or commit-ms 0)))
               (update :commit-ms-max max (long (or commit-ms 0)))))))

(defn- final-metrics
  [session]
  (let [metrics @(:metrics session)
        manifest (some-> (:capture session) :state deref)
        elapsed (- (now-ms) (:started-at metrics))
        event-count (or (:event-count manifest) 0)
        chunk-count (or (:chunk-count manifest) 0)]
    (assoc metrics
           :chunks chunk-count
           :events event-count
           :elapsed-ms elapsed
           :events-per-second (if (pos? elapsed)
                                (long (/ (* event-count 1000) elapsed))
                                0))))

;; Chunk commitment ----------------------------------------------------------

(defn- chunk-request
  [chunk boundary]
  {:event-count (count (:events chunk))
   :event-writer (fn [out] (event/write-events! out (:events chunk)))
   :ram (:ram (:snapshot chunk))
   :mem (:mem (:snapshot chunk))
   :snapshot (snapshot/canonical-snapshot (:snapshot chunk) (:timing chunk))
   :snapshot-timing (:timing chunk)
   :boundary boundary})

(defn- commit-chunk!
  [session chunk boundary]
  (let [started (System/nanoTime)]
    (recorder/commit-chunk! (:capture session) (chunk-request chunk boundary))
    (elapsed-ms started)))

;; Boundary, finish, and failure ---------------------------------------------

(defn- finish!
  [session chunk]
  (let [capture (:capture session)]
    (try
      (when (and (seq (:events chunk))
                 (:snapshot chunk)
                 (:timing chunk))
        (commit-chunk! session chunk {:kind :final :reason :stopped}))
      (let [result (recorder/finalize-capture! capture)]
        (swap! (:state session) assoc :status :stopped :result result)
        (assoc result :status :stopped))
      (catch Throwable error
        (recorder/abort-capture! capture)
        (throw error)))))

(defn- boundary!
  "Close the current chunk and open the next one.

  VICE is already paused by the logging disable. The next snapshot is read while
  it stays paused, then logging is re-enabled and execution resumes before the
  closing chunk is written, keeping the disk write off the pause path."
  [session chunk]
  (let [conn @(:conn session)
        boundary-start (System/nanoTime)
        snapshot-start (System/nanoTime)
        acquired (snapshot/acquire conn)
        snapshot-ms (elapsed-ms snapshot-start)
        next-chunk {:number (inc (:number chunk))
                    :snapshot acquired
                    :timing nil
                    :events []
                    :closing? false}]
    (set-logging! session 1)
    (bm/resume conn)
    (let [commit-ms (commit-chunk! session chunk
                                   {:kind :forced-size :reason :max-events})]
      (record-boundary-metrics!
       session {:events (count (:events chunk))
                :overshoot (- (count (:events chunk)) (:event-target session))
                :snapshot-ms snapshot-ms
                :boundary-ms (elapsed-ms boundary-start)
                :commit-ms commit-ms})
      next-chunk)))

(defn- fail-session!
  [session error]
  (when-let [capture (:capture session)]
    (try (recorder/abort-capture! capture) (catch Throwable _ nil)))
  (swap! (:state session) assoc
         :status :failed
         :error-message (.getMessage ^Throwable error))
  {:status :failed
   :capture-id (:capture-id session)
   :input (:input session)
   :dataset-root (:dataset-root session)
   :error error})

;; Controller ----------------------------------------------------------------

(defn- controller-loop!
  "Consume trace records from the reader, close chunks at the target, commit,
  and return the final capture result."
  [session initial-chunk]
  (let [^ArrayBlockingQueue queue (:queue session)]
    (loop [chunk initial-chunk]
      (let [item (.take queue)]
        (cond
          (= item eof-sentinel)
          (cond
            @(:stop-requested session) (finish! session chunk)
            (:closing? chunk) (recur (boundary! session chunk))
            :else (fail "Trace logging ended unexpectedly" {}))

          (and (map? item) (:error item))
          (throw (:error item))

          :else
          (let [trace-event (event/trace-record->event item)
                chunk (-> chunk
                          (cond-> (nil? (:timing chunk))
                            (assoc :timing
                                   (snapshot/timing-from-event trace-event)))
                          (update :events conj trace-event))]
            (if (and (not (:closing? chunk))
                     (>= (count (:events chunk)) (:event-target session)))
              (do
                (set-logging! session 0)
                (recur (assoc chunk :closing? true)))
              (recur chunk))))))))

;; Session lifecycle ---------------------------------------------------------

;; Captures started by this process and not yet finished, keyed by session id.
(defonce ^:private sessions (atom {}))

(defn- register-session!
  [session]
  (swap! sessions assoc (:session-id session) session)
  session)

(defn- unregister-session!
  [session]
  (swap! sessions dissoc (:session-id session)))

(defn- cleanup!
  [session]
  (stop-fifo-reader! session)
  (let [conn @(:conn session)]
    (when conn
      (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
           (catch Throwable _ nil))
      (when-let [number @(:trace-checkpoint session)]
        (try (bm/checkpoint-delete conn {:number number})
             (catch Throwable _ nil)))
      (when-let [prior @(:prior-ignored session)]
        (try (bm/ignore-unsolicited-types! conn prior)
             (catch Throwable _ nil)))
      (try (bm/drain-events conn) (catch Throwable _ nil)))
    (when-let [fifo @(:fifo session)]
      (io/delete-file fifo true))
    (when-let [instance @(:instance session)]
      (try (vice/stop instance) (catch Throwable _ nil)))
    (when conn
      (try (vice/close conn) (catch Throwable _ nil)))))

(defn- run-capture!
  [session options]
  (let [full-capture? (true? (:full-capture? options))
        conn @(:conn session)
        program-checkpoint (atom nil)]
    (try
      (when-not full-capture?
        (when (false? (:run-after-load? options))
          (fail ":run-after-load? false requires :full-capture? true" {}))
        (reset! program-checkpoint (set-program-start-checkpoint! conn))
        (bm/autostart conn (autostart-options session options))
        (wait-for-program-start! conn
                                 (:number @program-checkpoint)
                                 (or (:program-start-timeout-ms options)
                                     default-program-start-timeout-ms))
        (bm/checkpoint-delete conn {:number (:number @program-checkpoint)})
        (reset! program-checkpoint nil)
        (refine-program-start! session program-checkpoint options))
      (bm/drain-events conn)
      (bm/ping conn)
      (bm/drain-events conn)
      (let [initial (snapshot/acquire conn)
            trace-checkpoint (bm/checkpoint-set
                              conn
                              {:start 0
                               :end 0xffff
                               :stop? false
                               :enabled? true
                               :op bm/MON_CHECKPOINT_OP_EXECUTE
                               :temporary? false})]
        (reset! (:trace-checkpoint session) (:number trace-checkpoint))
        (reset! (:prior-ignored session) (bm/ignored-unsolicited-types conn))
        (bm/ignore-unsolicited-types! conn #{bm/MON_RESPONSE_CHECKPOINT_INFO})
        (let [fifo (create-fifo! session)]
          (bm/resource-set conn {:name "MonitorLogFileName" :value fifo})
          (start-fifo-reader! session fifo))
        ;; Enabling logging opens the FIFO writer so the reader can open it.
        (set-logging! session 1)
        (swap! (:state session) assoc :status :running)
        ;; The controller must be draining before VICE resumes: monitor-log
        ;; writing blocks both VICE and the reader once the queue is full.
        (if @(:stop-requested session)
          (set-logging! session 0)
          (future
            (try
              (if full-capture?
                (bm/autostart conn (autostart-options session options))
                (bm/resume conn))
              (catch Throwable error
                (try (.put ^ArrayBlockingQueue (:queue session) {:error error})
                     (catch Throwable _ nil))))))
        (finish! session
                 (controller-loop! session
                                   {:number 1
                                    :snapshot initial
                                    :timing nil
                                    :events []
                                    :closing? false})))
      (finally
        (when-let [number (:number @program-checkpoint)]
          (try (bm/checkpoint-delete conn {:number number})
               (catch Throwable _ nil)))))))

(defn- run-session!
  [session options]
  (let [result (try
                 (let [instance (vice/start
                                 {:executable (or (:executable options)
                                                  vice/default-executable)
                                  :address (:address options)
                                  :port (:monitor-port session)
                                  :extra-args (vice-extra-args options)})]
                   (reset! (:instance session) instance)
                   (swap! (:state session) assoc
                          :pid (:pid instance)
                          :status :connecting)
                   (let [conn (connect-when-ready instance options)]
                     (reset! (:conn session) conn)
                     (swap! (:state session) assoc
                            :status :starting
                            :monitor :connected)
                     (run-capture! session options)))
                 (catch Throwable error
                   (fail-session! session error)))]
    (cleanup! session)
    (unregister-session! session)
    (let [final (assoc result
                       :capture-directory (:directory (:capture session))
                       :metrics (final-metrics session))
          status (if (= :failed (:status result)) :failed :stopped)]
      (swap! (:state session) assoc :status status :result final)
      (deliver (:completion session) final)
      final)))

;; Public API ----------------------------------------------------------------

(defn start!
  "Reserve a capture and start VICE capture on a background worker.

  Required options: `:input` (an existing `.prg` or `.d64`) and
  `:dataset-root`. Optional: `:capture-id`, `:executable`, `:address`,
  `:port`, `:extra-args`, `:speed`, `:warp?`, `:full-capture?`,
  `:run-after-load?`, `:event-target`, `:vic-value-blacklist`,
  `:timing-model`, `:connect-timeout-ms`, `:connect-retry-ms`,
  `:autostart-timeout-ms`, `:program-start-timeout-ms`, and
  `:record-queue-capacity`.

  Returns a session handle immediately."
  [options]
  (let [input (validate-input! (:input options))
        dataset-root (prepare-dataset-root! (:dataset-root options))
        capture-id (make-capture-id (:capture-id options))
        target (event-target options)
        blacklist (set (or (:vic-value-blacklist options)
                           contract/default-vic-value-blacklist))
        _ (contract/validate-vic-value-blacklist! blacklist)
        capture (recorder/start-capture!
                 {:dataset-root dataset-root
                  :capture-id capture-id
                  :timing-model (:timing-model options)
                  :options {:event-target target
                            :vic-value-blacklist blacklist}})
        session {:session-id (str (UUID/randomUUID))
                 :capture-id capture-id
                 :input input
                 :dataset-root dataset-root
                 :event-target target
                 :capture capture
                 :monitor-port (or (:port options) (free-port))
                 :state (atom {:status :starting
                               :capture-id capture-id
                               :input input
                               :dataset-root dataset-root
                               :capture-directory (:directory capture)
                               :event-target target})
                 :instance (atom nil)
                 :conn (atom nil)
                 :queue (ArrayBlockingQueue. (int (queue-capacity options)))
                 :stop-requested (atom false)
                 :reader (atom nil)
                 :reader-thread (atom nil)
                 :fifo (atom nil)
                 :trace-checkpoint (atom nil)
                 :prior-ignored (atom nil)
                 :metrics (atom nil)
                 :worker (atom nil)
                 :completion (promise)}]
    (register-session! session)
    (start-metrics! session)
    (reset! (:worker session) (future (run-session! session options)))
    session))

(defn status
  "Return lightweight capture progress without stopping the capture."
  [session]
  (when-not (map? session)
    (throw (IllegalArgumentException. "Session must be a map")))
  (let [manifest (some-> (:capture session) :state deref)]
    (merge (select-keys session [:session-id :capture-id :input :dataset-root
                                 :event-target])
           (dissoc @(:state session) :result)
           {:metrics @(:metrics session)
            :chunk-count (:chunk-count manifest 0)
            :event-count (:event-count manifest 0)
            :capture-directory (:directory (:capture session))
            :queue-depth (.size ^ArrayBlockingQueue (:queue session))
            :reader-alive? (some-> ^Thread @(:reader-thread session) .isAlive)
            :worker-done? (some-> ^java.util.concurrent.Future @(:worker session) .isDone)
            :stop-requested? (boolean @(:stop-requested session))})))

(defn live-sessions
  "Lightweight summaries of captures started by this process and still running.

  Every entry is a `status` map, so a stopped capture whose reader or worker is
  still alive is visible as a leak instead of being silently forgotten."
  []
  (->> @sessions
       vals
       (map status)
       (sort-by :session-id)
       vec))

(defn stop-async!
  "Request capture shutdown without waiting.

  Sets the stop flag and disables monitor logging, which pauses VICE, closes the
  log file, and lets the controller drain the final chunk and finalize."
  [session]
  (when-not (map? session)
    (throw (IllegalArgumentException. "Session must be a map")))
  (reset! (:stop-requested session) true)
  (when-let [conn @(:conn session)]
    (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
         (catch Throwable _ nil)))
  (status session))

(defn stop!
  "Stop a capture, finalize it, and return the paths and metrics.

  Blocks until the worker completes. Throws on setup or capture failure with
  the failed result in `ex-data`."
  [session]
  (stop-async! session)
  (let [result (deref (:completion session) 60000 ::timeout)]
    (cond
      (= ::timeout result)
      (do
        (cleanup! session)
        (throw (ex-info "Timed out waiting for capture shutdown" {})))
      (= :failed (:status result))
      (throw (ex-info "VICE capture failed" (dissoc result :error) (:error result)))
      :else result)))
