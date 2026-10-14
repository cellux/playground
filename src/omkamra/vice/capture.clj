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
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.driver :as driver]
            [omkamra.vice.prg :as prg]
            [omkamra.vice.trace :as trace]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.snapshot :as snapshot])
  (:import [java.io Reader]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(def ^:private default-connect-timeout-ms 30000)
(def ^:private default-events-per-chunk 100000)
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

(defn- write-artifact-prg!
  "Write `artifact`'s self-relocating PRG to a fresh temp file and return its
  path; `cleanup!` deletes it."
  [artifact]
  (let [directory (io/file "/tmp/omkamra-vice-v2")]
    (.mkdirs directory)
    (let [file (io/file directory (str "artifact-" (System/nanoTime) ".prg"))]
      (prg/write! artifact file)
      (.getPath file))))

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

(defn- events-per-chunk
  [options]
  (let [target (or (:events-per-chunk options) default-events-per-chunk)]
    (when-not (pos-int? target)
      (fail "Capture :events-per-chunk must be a positive integer"
            {:events-per-chunk target}))
    target))

(defn- queue-capacity
  [options]
  (let [capacity (or (:record-queue-capacity options) default-queue-capacity)]
    (when-not (pos-int? capacity)
      (fail "Capture :record-queue-capacity must be a positive integer"
            {:record-queue-capacity capacity}))
    capacity))

(defn- from-pc-option
  [options]
  (when-let [pc (:from-pc options)]
    (when-not (and (integer? pc) (<= 0 pc 0xffff))
      (fail "Capture :from-pc must be a 16-bit address" {:from-pc pc}))
    pc))

(defn- max-events-option
  [options]
  (when-let [max-events (:max-events options)]
    (when-not (pos-int? max-events)
      (fail "Capture :max-events must be a positive integer"
            {:max-events max-events}))
    max-events))

(defn- instance-option
  [options]
  (when-let [instance (:instance options)]
    (when-not (and (map? instance)
                   (string? (:address instance))
                   (integer? (:port instance)))
      (fail "Capture :instance must be a VICE instance with :address and :port"
            {:instance instance}))
    instance))

;; VICE process and monitor --------------------------------------------------

(defn- vice-extra-args
  [options]
  (cond-> (vec (or (:extra-args options) []))
    (some? (:speed options)) (conj "-speed" (str (:speed options)))
    (true? (:warp? options)) (conj "-warp")))

(defn- set-logging!
  [session value]
  (bm/resource-set @(:conn session) {:name "MonitorLogEnabled" :value value}))

;; Program-entry start policy ------------------------------------------------

(defn- prepare-start-pc!
  "Load and run an input file, then choose the backend start address.

  Returns the address the running-instance backend should gate on, or nil when
  no gate is needed: a full capture starts before autostart, and an instance
  supplied by the caller starts wherever it already is. The autostart stop at
  `$E144` (BASIC RUN) is unavoidable; the follow-up wait for the stub's `SYS`
  target is what `:from-pc` replaces. When the input has no distinct `SYS`
  target (or `:from-pc` names `$E144`) the emulator is already at the entry and
  the backend must not gate again."
  [session options]
  (cond
    (and (nil? (:instance options)) (true? (:full-capture? options)))
    nil

    (:instance options)
    (:from-pc options)

    :else
    (driver/prepare-program-start! @(:conn session) (:input session) options)))

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
          {:events-per-chunk (:events-per-chunk session)
           :events 0
           :chunks 0
           :overshoot-total 0
           :overshoot-max 0
           :snapshot-ms-total 0
           :snapshot-ms-max 0
           :screenshot-ms-total 0
           :screenshot-ms-max 0
           :boundary-ms-total 0
           :boundary-ms-max 0
           :commit-ms-total 0
           :commit-ms-max 0
           :started-at (now-ms)}))

(defn- record-boundary-metrics!
  [session {:keys [events overshoot snapshot-ms screenshot-ms boundary-ms
                   commit-ms]}]
  (swap! (:metrics session)
         (fn [metrics]
           (-> metrics
               (assoc :events (+ (:events metrics) (long (or events 0)))
                      :chunks (inc (:chunks metrics)))
               (update :overshoot-total + (long (or overshoot 0)))
               (update :overshoot-max max (long (or overshoot 0)))
               (update :snapshot-ms-total + (long (or snapshot-ms 0)))
               (update :snapshot-ms-max max (long (or snapshot-ms 0)))
               (update :screenshot-ms-total + (long (or screenshot-ms 0)))
               (update :screenshot-ms-max max (long (or screenshot-ms 0)))
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
   :screenshot (:screenshot chunk)
   :boundary boundary})

(defn- acquire-screenshot!
  "Read the VIC canvas while VICE is already paused for a boundary snapshot.

  Only the monitor round trips happen here; encoding and storing the PNG is
  left to the chunk commit, which runs after execution resumes."
  [session]
  (let [conn @(:conn session)
        started (System/nanoTime)
        screenshot {:frame (bm/display-get conn {:use-vic-ii? true :format 0})
                    :palette (bm/palette-get conn {:use-vic-ii? true})}]
    {:screenshot screenshot
     :screenshot-ms (elapsed-ms started)}))

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
        {:keys [screenshot screenshot-ms]} (acquire-screenshot! session)
        next-chunk {:number (inc (:number chunk))
                    :snapshot acquired
                    :screenshot screenshot
                    :timing nil
                    :events []
                    :closing? false}]
    (set-logging! session 1)
    (bm/resume conn)
    (let [commit-ms (commit-chunk! session chunk
                                   {:kind :forced-size :reason :max-events})]
      (record-boundary-metrics!
       session {:events (count (:events chunk))
                :overshoot (- (count (:events chunk)) (:events-per-chunk session))
                :snapshot-ms snapshot-ms
                :screenshot-ms screenshot-ms
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

(defn- request-stop!
  "Flag the session for shutdown and pause VICE by disabling monitor logging.

  This closes the trace FIFO, so the reader reaches end of stream and the
  controller finalizes the last chunk once the current chunk is drained."
  [session]
  (reset! (:stop-requested session) true)
  (when-let [conn @(:conn session)]
    (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
         (catch Throwable _ nil))))

(defn- controller-loop!
  "Consume trace records from the reader, close chunks at the target, commit,
  and return the final capture result.

  `committed` counts events already written to earlier chunks. A chunk closes
  when it reaches `:events-per-chunk`; the capture stops when the total reaches
  `:max-events` (dropping any queued excess so the count is exact) or when
  `stop-async!` sets the stop flag."
  [session initial-chunk]
  (let [^ArrayBlockingQueue queue (:queue session)]
    (loop [chunk initial-chunk
           committed 0]
      (let [item (.take queue)]
        (cond
          (= item eof-sentinel)
          (cond
            @(:stop-requested session) (finish! session chunk)
            (:closing? chunk) (recur (boundary! session chunk)
                                     (+ committed (count (:events chunk))))
            :else (fail "Trace logging ended unexpectedly" {}))

          (and (map? item) (:error item))
          (throw (:error item))

          :else
          ;; Once stopping, discard queued records instead of appending them,
          ;; so a :max-events cap is not overshot by the backlog and a manual
          ;; stop freezes the chunk at the moment it was requested.
          (if @(:stop-requested session)
            (recur chunk committed)
            (let [trace-event (event/trace-record->event item)
                  chunk (-> chunk
                            (cond-> (nil? (:timing chunk))
                              (assoc :timing
                                     (snapshot/timing-from-event trace-event)))
                            (update :events conj trace-event))]
              (cond
                (and (:max-events session)
                     (>= (+ committed (count (:events chunk)))
                         (:max-events session)))
                (do (request-stop! session)
                    (recur chunk committed))

                (and (not (:closing? chunk))
                     (>= (count (:events chunk)) (:events-per-chunk session)))
                (do (set-logging! session 0)
                    (recur (assoc chunk :closing? true) committed))

                :else
                (recur chunk committed)))))))))

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
  (let [conn @(:conn session)
        external? (:external? session)]
    (when conn
      (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
           (catch Throwable _ nil))
      (when-let [number @(:trace-checkpoint session)]
        (try (bm/checkpoint-delete conn {:number number})
             (catch Throwable _ nil)))
      (when-let [prior @(:prior-ignored session)]
        (try (bm/ignore-unsolicited-types! conn prior)
             (catch Throwable _ nil)))
      (try (bm/drain-events conn) (catch Throwable _ nil))
      ;; A caller-supplied instance keeps running: resume it and leave its
      ;; process alone instead of terminating it with the capture.
      (when external?
        (try (bm/resume conn) (catch Throwable _ nil))))
    (when-let [fifo @(:fifo session)]
      (io/delete-file fifo true))
    (when-let [temp-input (:temp-input session)]
      (io/delete-file temp-input true))
    ;; Close the monitor connection before terminating the process, so the
    ;; connection reader sees an intentional close instead of a dropped stream.
    (when conn
      (try (driver/close conn) (catch Throwable _ nil)))
    (when-let [instance @(:instance session)]
      (when-not external?
        (try (driver/stop! instance) (catch Throwable _ nil))))))

(defn- capture-running-instance!
  "Running-instance capture backend, shared by the input and instance paths.

  The monitor connection is established and VICE is paused. `start-pc` gates
  the beginning of the trace with a temporary execute checkpoint; nil starts
  from the current position. A full capture instead power-cycles the machine
  and stops it at the reset entry (or `:from-pc`), so the whole boot sequence
  is recorded; its input, if any, is autostarted by the resume future."
  [session options start-pc]
  (let [full-capture? (and (nil? (:instance options))
                           (true? (:full-capture? options)))
        conn @(:conn session)
        checkpoint (atom nil)]
    (try
      (cond
        full-capture?
        (driver/reset-to-pc! conn
                             (or (:from-pc options) (driver/reset-vector conn))
                             checkpoint
                             (or (:program-start-timeout-ms options)
                                 driver/default-program-start-timeout-ms))

        start-pc
        (driver/await-pc! conn start-pc checkpoint
                          (or (:program-start-timeout-ms options)
                              driver/default-program-start-timeout-ms)))
      (bm/drain-events conn)
      (bm/ping conn)
      (bm/drain-events conn)
      (let [initial (snapshot/acquire conn)
            {:keys [screenshot]} (acquire-screenshot! session)
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
              (cond
                (and full-capture? (:input session))
                (driver/autostart! conn (:input session) options)

                ;; A pure boot capture starts on a fresh power cycle, so the
                ;; trace begins exactly at the reset vector rather than at a
                ;; drift-during-setup offset. An explicit :from-pc was already
                ;; gated for, so it is resumed from instead.
                (and full-capture? (nil? (:from-pc options)))
                (bm/reset conn {:what 1})

                :else
                (bm/resume conn))
              (catch Throwable error
                (try (.put ^ArrayBlockingQueue (:queue session) {:error error})
                     (catch Throwable _ nil))))))
        (finish! session
                 (controller-loop! session
                                   {:number 1
                                    :snapshot initial
                                    :screenshot screenshot
                                    :timing nil
                                    :events []
                                    :closing? false})))
      (finally
        (when-let [number (:number @checkpoint)]
          (try (bm/checkpoint-delete conn {:number number})
               (catch Throwable _ nil)))))))

(defn- run-session!
  [session options]
  (let [result (try
                 (let [instance (or (:instance options)
                                    (driver/start!
                                     {:executable (or (:executable options)
                                                      driver/default-executable)
                                      :address (:address options)
                                      :port (:monitor-port session)
                                      :extra-args (vice-extra-args options)}))]
                   (reset! (:instance session) instance)
                   (swap! (:state session) assoc
                          :pid (:pid instance)
                          :status :connecting)
                   (let [conn (driver/connect!
                               instance
                               (assoc options
                                      :connect-timeout-ms
                                      (or (:connect-timeout-ms options)
                                          default-connect-timeout-ms)))]
                     (reset! (:conn session) conn)
                     (swap! (:state session) assoc
                            :status :starting
                            :monitor :connected)
                     ;; The input path prepares autostart and computes the
                     ;; backend start address before handing over.
                     (capture-running-instance!
                      session options
                      (prepare-start-pc! session options))))
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
  "Reserve a capture and start it on a background worker.

  A capture starts a fresh VICE from an `:input` file or an in-memory
  `:artifact`, records the boot sequence (`:full-capture? true`, no input
  needed), or attaches to an already running `:instance` (as returned by
  `omkamra.vice/start!`). Exactly one of `:input`, `:artifact`, and `:instance`
  is required, together with `:dataset-root`.

  Input mode autostarts the `.prg`/`.d64`, waits for the BASIC RUN stop at
  `$E144`, and starts the trace at the loaded stub's `SYS` target, or at
  `:from-pc` when supplied. Instance mode records the running emulator from its
  current position, or from `:from-pc` when supplied. A full capture powers on
  a fresh VICE, stops at the reset entry (or at `:from-pc`), and records the
  boot sequence; `:input` is optional and, when given, is autostarted after the
  boot. A full capture cannot be combined with `:instance`. `:max-events` stops
  the capture once that many events have been collected.

  Optional: `:capture-id`, `:executable`, `:address`, `:port`, `:extra-args`,
  `:speed`, `:warp?`, `:full-capture?`, `:run-after-load?`, `:from-pc`,
  `:max-events`, `:events-per-chunk`, `:vic-value-blacklist`, `:timing-model`,
  `:connect-timeout-ms`, `:connect-retry-ms`, `:autostart-timeout-ms`,
  `:program-start-timeout-ms`, and `:record-queue-capacity`.

  Every committed chunk carries the VIC screenshot of its boundary instant as
  `chunks/chunk-NNNNNN.png`. The frame is read from the monitor while VICE is
  already paused for the boundary snapshot, and is encoded and stored on its
  own thread during the chunk commit, off the pause path.

  Returns a session handle immediately."
  [options]
  (let [artifact (:artifact options)
        _ (when (and artifact (:instance options))
            (fail "Capture accepts either :artifact or :instance, not both" {}))
        _ (when (and artifact (:input options))
            (fail "Capture accepts either :input or :artifact, not both" {}))
        _ (when (and artifact (true? (:full-capture? options)))
            (fail (str "Capture :full-capture? is meaningless for an injected "
                       ":artifact")
                  {}))
        ;; An artifact becomes a self-relocating PRG; the input path then
        ;; autostarts it and gates the trace at the entry point.
        temp-input (when artifact (write-artifact-prg! artifact))
        options (if artifact
                  (-> options
                      (dissoc :artifact)
                      (assoc :input temp-input)
                      (assoc :from-pc (or (:from-pc options)
                                          (:entry-point artifact))))
                  options)
        instance (instance-option options)
        full-capture? (true? (:full-capture? options))
        _ (when (and instance full-capture?)
            (fail (str "Capture :full-capture? powers on a fresh VICE; "
                       "it cannot be combined with :instance")
                  {}))
        _ (when (and instance (:input options))
            (fail "Capture accepts either :input or :instance, not both" {}))
        ;; A full capture may omit the input and record a pure boot sequence.
        input (if (or instance
                      (and full-capture? (nil? (:input options))))
                nil
                (validate-input! (:input options)))
        from-pc (from-pc-option options)
        max-events (max-events-option options)
        dataset-root (prepare-dataset-root! (:dataset-root options))
        capture-id (make-capture-id (:capture-id options))
        target (events-per-chunk options)
        blacklist (set (or (:vic-value-blacklist options)
                           contract/default-vic-value-blacklist))
        _ (contract/validate-vic-value-blacklist! blacklist)
        capture (recorder/start-capture!
                 {:dataset-root dataset-root
                  :capture-id capture-id
                  :timing-model (:timing-model options)
                  :options (cond-> {:events-per-chunk target
                                    :vic-value-blacklist blacklist}
                             from-pc (assoc :from-pc from-pc)
                             max-events (assoc :max-events max-events))})
        session {:session-id (str (UUID/randomUUID))
                 :capture-id capture-id
                 :input input
                 :temp-input temp-input
                 :dataset-root dataset-root
                 :events-per-chunk target
                 :from-pc from-pc
                 :max-events max-events
                 :external? (some? instance)
                 :capture capture
                 :monitor-port (if instance
                                 (:port instance)
                                 (or (:port options) (driver/free-port)))
                 :state (atom {:status :starting
                               :capture-id capture-id
                               :input input
                               :dataset-root dataset-root
                               :capture-directory (:directory capture)
                               :events-per-chunk target
                               :from-pc from-pc
                               :max-events max-events})
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
                                 :events-per-chunk])
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
  (request-stop! session)
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
