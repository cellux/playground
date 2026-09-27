(ns omkamra.vice.capture
  "Non-GUI orchestration for VICE PRG/D64 captures.

  `start!` performs setup asynchronously and returns a session handle. The
  session owns the VICE process, binary-monitor connection, and FIFO-backed
  decoder capture. `stop-async!` requests shutdown without waiting;
  `stop!` waits for setup if necessary, finalizes the capture, writes EDN and
  assembly output, and tears down VICE.

  Model-facing usage for a demo-capture request:

  1. Call `start!` with the user's existing `.prg` or `.d64` path and an
     output directory. Do not use the GUI namespace or manually orchestrate
     VICE, the monitor, or the FIFO.
  2. Retain the returned session handle and use `status` for progress checks.
     The session is running once its status is `:running`; the input has been
     loaded and, by default, execution has resumed with `:run-after-load?`
     set to true.
  3. Keep the session alive while the requested demo runs. When the user says
     the capture is complete, call `stop-async!` and poll `status` until the
     session is `:stopped`, or call `stop!` when a synchronous result is
     appropriate.
  4. Return or report the `:edn-path`, `:assembly-path`, and, when requested,
     `:profile-edn-path` from the stopped status or result. The EDN is the
     canonical artifact, the assembly is the compact, deduplicated rendering,
     and the profile EDN contains Clojure async-profiler stack samples.

  If the user requests loading without execution, pass
  `:run-after-load? false` to `start!`; otherwise do not override the default."
  (:require [clojure.java.io :as io]
            [omkamra.vice :as vice]
            [omkamra.vice.asm :as asm]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.profile :as profile]
            [omkamra.vice.decoder :as decoder])
  (:import [java.net ServerSocket]
           [java.util UUID]))

(def ^:private default-connect-timeout-ms 30000)
(def ^:private default-connect-retry-ms 100)
(def ^:private default-autostart-timeout-ms 5000)
(def ^:private terminal-statuses #{:stopped :failed})

(defn- fail
  [message data]
  (throw (ex-info message data)))

(defn- canonical-file
  [path]
  (try
    (.getCanonicalFile (io/file path))
    (catch java.io.IOException error
      (throw (ex-info "Could not resolve path"
                      {:path path}
                      error)))))

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

(defn- prepare-output-dir!
  [output-dir]
  (when-not output-dir
    (fail "Capture requires :output-dir" {}))
  (let [directory (canonical-file output-dir)]
    (when (and (.exists directory) (not (.isDirectory directory)))
      (fail "Capture output path is not a directory"
            {:output-dir (.getPath directory)}))
    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (fail "Could not create capture output directory"
              {:output-dir (.getPath directory)})))
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

(defn- free-port
  []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- monitor-port
  [port]
  (let [port (if (nil? port) (free-port) port)]
    (when-not (and (integer? port) (<= 0 port 65535))
      (fail "Monitor port must be an integer between 0 and 65535"
            {:port port}))
    (if (zero? port) (free-port) port)))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- process-alive?
  [instance]
  (or (nil? (:process instance))
      (.isAlive ^Process (:process instance))))

(defn- connect-when-ready
  [instance {:keys [timeout-ms retry-ms]}]
  (let [deadline (+ (now-ms) timeout-ms)]
    (loop [last-error nil]
      (when-not (process-alive? instance)
        (fail "VICE exited before its binary monitor became ready"
              {:instance (select-keys instance [:pid :command])
               :cause last-error}))
      (let [attempt (try
                      (let [conn (vice/connect instance (fn [_ _] nil))]
                        (try
                          ;; A socket can accept before the monitor is ready to
                          ;; process commands. PING verifies the complete
                          ;; monitor handshake.
                          (bm/ping conn)
                          {:conn conn}
                          (catch InterruptedException error
                            (vice/close conn)
                            (throw error))
                          (catch Throwable error
                            (vice/close conn)
                            {:error error})))
                      (catch InterruptedException error
                        (throw error))
                      (catch Throwable error
                        {:error error}))]
        (if-let [conn (:conn attempt)]
          conn
          (if (< (now-ms) deadline)
            (do
              (Thread/sleep (long retry-ms))
              (recur (:error attempt)))
            (throw (ex-info "Timed out waiting for VICE binary monitor"
                            {:timeout-ms timeout-ms
                             :instance (select-keys instance [:pid :command])}
                            (:error attempt)))))))))

(defn- update-status!
  [session status & data]
  (apply swap! (:state session) assoc :status status data))

(defn- request-stop!
  [session reason]
  (locking (:lifecycle-lock session)
    (let [state @(:state session)]
      (when (and (not (contains? terminal-statuses (:status state)))
                 (not (:finalization-started state)))
        (swap! (:state session)
               (fn [state]
                 (cond-> (assoc state :status :stopping)
                   (nil? (:stop-reason state))
                   (assoc :stop-reason reason))))
        (when-not (realized? (:stop-requested session))
          (deliver (:stop-requested session) reason)))))
  @(:state session))

(defn- start-profiler!
  [session options]
  (when-let [profile-options (profile/options options)]
    (reset! (:profiler session) (profile/start! profile-options))
    (swap! (:state session) assoc :profiling :running)
    nil))

(defn- stop-profiler!
  [session]
  (when-let [profiler @(:profiler session)]
    (let [result (profile/stop!
                  profiler
                  {:output-file (:profile-edn-path session)
                   :capture-id (:capture-id session)
                   :input (:input session)})]
      (swap! (:state session) assoc :profiling :stopped)
      result)))

(defn- cleanup-capture!
  [session]
  (when-let [capture @(:capture session)]
    (try
      (decoder/stop-capture capture)
      (catch Throwable error
        (swap! (:state session) assoc
               :capture-cleanup-error (.getMessage error)))
      (finally
        (reset! (:capture session) nil)))))

(defn- finalize-capture!
  [session capture]
  (update-status! session :finalizing)
  ;; Stop sampling before finalizing the FIFO so the profile describes the
  ;; steady-state Clojure capture rather than shutdown and artifact rendering.
  (stop-profiler! session)
  (let [artifact (decoder/stop-capture capture)
        capture-summary (try
                          (decoder/capture-status capture)
                          (catch Throwable _ nil))
        _ (swap! (:state session) assoc
                 :capture-summary capture-summary)
        _ (reset! (:capture session) nil)
        edn-path (:edn-path session)
        assembly-path (:assembly-path session)
        profile-edn-path (:profile-edn-path session)]
    (decoder/write-artifact! edn-path artifact)
    (asm/artifact->assembly artifact {:output-file assembly-path})
    (swap! (:state session) assoc :status :stopped)
    (cond-> {:status :stopped
             :capture-id (:capture-id session)
             :input (:input session)
             :edn-path edn-path
             :assembly-path assembly-path
             :stop-reason (:stop-reason @(:state session))
             :capture-summary capture-summary}
      profile-edn-path (assoc :profile-edn-path profile-edn-path))))

(defn- finalize-once!
  [session capture]
  (let [owner? (locking (:lifecycle-lock session)
                 (if (:finalization-started @(:state session))
                   false
                   (do
                     (swap! (:state session) assoc
                            :finalization-started true
                            :status :finalizing)
                     true)))]
    (if-not owner?
      @(:finalization session)
      (let [result (try
                     (finalize-capture! session capture)
                     (catch Throwable error
                       (cleanup-capture! session)
                       (swap! (:state session) assoc
                              :status :failed
                              :error-message (.getMessage error))
                       {:status :failed
                        :capture-id (:capture-id session)
                        :input (:input session)
                        :stop-reason (:stop-reason @(:state session))
                        :error error}))]
        (deliver (:finalization session) result)
        result))))

(defn- cleanup-resources!
  [session]
  (let [owner? (locking (:lifecycle-lock session)
                 (if (:cleanup-started @(:state session))
                   false
                   (do
                     (swap! (:state session) assoc :cleanup-started true)
                     true)))]
    (when owner?
      (try
        (when-let [conn @(:conn session)]
          (try
            (vice/close conn)
            (catch Throwable error
              (swap! (:state session) assoc
                     :monitor-cleanup-error (.getMessage error))))
          (reset! (:conn session) nil))
        (when-let [instance @(:instance session)]
          (try
            (vice/stop instance)
            (catch Throwable error
              (swap! (:state session) assoc
                     :vice-cleanup-error (.getMessage error))))
          (reset! (:instance session) nil))
        (finally
          (swap! (:state session) assoc
                 :monitor :closed
                 :vice :stopped
                 :cleanup-complete true))))))

(defn- speed-extra-args
  [options]
  (if (nil? (:speed options))
    []
    (let [speed (:speed options)]
      (when-not (and (integer? speed) (<= 0 speed))
        (fail "Capture :speed must be a non-negative integer percentage"
              {:speed speed}))
      ["-speed" (str speed)])))

(defn- vice-extra-args
  "Build VICE arguments, optionally setting speed and warp mode."
  [options]
  (cond-> (into (vec (or (:extra-args options) []))
                (speed-extra-args options))
    (true? (:warp? options)) (conj "-warp")))

(defn- wait-for-stop!
  [session instance capture]
  (try
    (loop []
      (let [signal (deref (:stop-requested session) 100 ::timeout)
            stream @(:stream-state capture)]
        (cond
          (not= ::timeout signal)
          signal

          (not (process-alive? instance))
          (do
            (request-stop! session {:kind :vice-exited
                                    :message "VICE exited unexpectedly"})
            (:stop-reason @(:state session)))

          (:error stream)
          (do
            (request-stop! session {:kind :capture-reader-error
                                    :error (:error stream)
                                    :message "FIFO capture reader failed"})
            (:stop-reason @(:state session)))

          :else
          (recur))))
    (catch InterruptedException error
      (request-stop! session {:kind :interrupted
                              :error error
                              :message "Capture worker was interrupted"})
      (:stop-reason @(:state session)))))

(defn- run-session!
  [session options]
  (try
    (let [instance (vice/start {:executable (or (:executable options)
                                                 vice/default-executable)
                                :address (:address options)
                                :port (:monitor-port session)
                                :extra-args (vice-extra-args options)})]
      (reset! (:instance session) instance)
      (swap! (:state session) assoc
             :pid (:pid instance)
             :command (:command instance))
      (let [conn (connect-when-ready
                  instance
                  {:timeout-ms (or (:connect-timeout-ms options)
                                   default-connect-timeout-ms)
                   :retry-ms (or (:connect-retry-ms options)
                                 default-connect-retry-ms)})]
        (reset! (:conn session) conn)
        (let [capture (decoder/start-capture
                       conn
                       {:metadata {:capture-id (:capture-id session)
                                   :input (:input session)}
                        :retain-samples? (boolean (:retain-samples? options))})]
          (reset! (:capture session) capture)
          (swap! (:state session) assoc :transport :fifo)
          (if (realized? (:stop-requested session))
            (finalize-once! session capture)
            (do
              (bm/autostart
               conn
               {:run-after-load? (if (contains? options :run-after-load?)
                                   (:run-after-load? options)
                                   true)
                :file-index 0
                :filename (:input session)
                :timeout-ms (or (:autostart-timeout-ms options)
                                default-autostart-timeout-ms)})
              (start-profiler! session options)
              (update-status! session :running)
              (wait-for-stop! session instance capture)
              (finalize-once! session capture))))))
    (catch Throwable error
      (try
        (stop-profiler! session)
        (catch Throwable profile-error
          (swap! (:state session) assoc
                 :profiling-cleanup-error (.getMessage profile-error))))
      (cleanup-capture! session)
      (swap! (:state session) assoc
             :status :failed
             :error-message (.getMessage error))
      {:status :failed
       :capture-id (:capture-id session)
       :input (:input session)
       :profile-edn-path (:profile-edn-path session)
       :stop-reason (:stop-reason @(:state session))
       :error error})
    (finally
      (cleanup-resources! session))))

(defn start!
  "Start an asynchronous non-GUI VICE capture.

  Required options:

  * `:input` - an existing `.prg` or `.d64` file
  * `:output-dir` - directory for `<capture-id>.edn` and `.asm`

  Optional options include `:capture-id`, `:executable`, `:address`, `:port`,
  `:extra-args`, `:speed`, `:warp?`, `:connect-timeout-ms`,
  `:connect-retry-ms`, `:autostart-timeout-ms`, `:run-after-load?`,
  `:retain-samples?`, and `:profile`. Set `:speed` to a non-negative integer
  percentage such as `200` to pass VICE's `-speed 200` option. `:speed 0`
  removes the speed limit while retaining normal video and sound behavior.
  Set `:warp?` true to pass VICE's `-warp` option, which also changes video
  refresh and sound behavior for maximum available speed. If both are set,
  warp mode takes precedence. Set `:profile true`
  to write `<capture-id>.profile.edn`, or
  provide an async-profiler option map such as
  `{:event :cpu :interval 1000000 :threads true}`. Profiling always targets
  this Clojure JVM, never the external VICE process, and starts once the
  capture is running. The default profile uses 1 ms CPU samples with per-thread
  stacks. Full per-instruction register/timing samples are disabled by default;
  enable them only for forensic captures. If `:port` is omitted (or zero), an
  available local monitor port is selected. VICE remains windowed; callers can
  provide additional VICE arguments through `:extra-args`.

  Returns immediately with a session handle. VICE startup, monitor connection,
  FIFO setup, and autostart happen on the session's background worker.
  "
  [{:keys [input output-dir capture-id port] :as options}]
  (let [_ (speed-extra-args options)
        input (validate-input! input)
        output-dir (prepare-output-dir! output-dir)
        capture-id (make-capture-id capture-id)
        monitor-port (monitor-port port)
        edn-path (.getPath (io/file output-dir (str capture-id ".edn")))
        assembly-path (.getPath (io/file output-dir (str capture-id ".asm")))
        profile-edn-path (when (profile/options options)
                           (.getPath (io/file output-dir
                                              (str capture-id ".profile.edn"))))
        session {:capture-id capture-id
                 :input input
                 :output-dir output-dir
                 :monitor-port monitor-port
                 :edn-path edn-path
                 :assembly-path assembly-path
                 :profile-edn-path profile-edn-path
                 :profiler (atom nil)
                 :state (atom {:status :starting
                               :capture-id capture-id
                               :input input
                               :output-dir output-dir
                               :monitor-port monitor-port})
                 :instance (atom nil)
                 :conn (atom nil)
                 :capture (atom nil)
                 :lifecycle-lock (Object.)
                 :stop-requested (promise)
                 :finalization (promise)
                 :completion (promise)}
        worker (future
                 (let [result (try
                                (run-session! session options)
                                (catch Throwable error
                                  (swap! (:state session) assoc
                                         :status :failed
                                         :error-message (.getMessage error))
                                  {:status :failed
                                   :capture-id (:capture-id session)
                                   :input (:input session)
                                   :profile-edn-path (:profile-edn-path session)
                                   :error error}))]
                   (deliver (:completion session) result)
                   ;; The session's completion promise is the result owner;
                   ;; do not retain the same artifact in the FutureTask result.
                   nil))]
    (assoc session :worker worker)))

(defn status
  "Return lightweight status for a capture session without stopping it."
  [session]
  (when-not (map? session)
    (throw (IllegalArgumentException. "Session must be a map")))
  (let [state @(:state session)
        capture @(:capture session)
        capture-status (when capture
                         (try
                           (decoder/capture-status capture)
                           (catch Throwable error
                             {:capture-status-error (.getMessage error)})))]
    (cond-> (merge (select-keys session [:capture-id :input :output-dir
                                         :monitor-port :edn-path :assembly-path])
                   (dissoc state :error-message)
                   (when (:error-message state)
                     {:error-message (:error-message state)}))
      (:profile-edn-path session) (assoc :profile-edn-path
                                          (:profile-edn-path session))
      capture-status (merge (select-keys capture-status
                                         [:event-count :instruction-count
                                          :block-count :block-run-count
                                          :reader-status :reader-alive?])))))

(defn stop-async!
  "Request capture shutdown without waiting for finalization.

  Returns immediately with the lightweight session status. Poll `status` until
  the session reaches `:stopped` or `:failed`; use `stop!` when the completed
  result is needed synchronously. Calling this more than once is harmless."
  [session]
  (when-not (map? session)
    (throw (IllegalArgumentException. "Session must be a map")))
  (request-stop! session {:kind :explicit
                          :message "Capture stopped by caller"})
  (status session))

(defn stop!
  "Stop a session, finalize its FIFO capture, and return output paths/results.

  This call blocks until startup/finalization completes. Calling it more than
  once returns the same result; only one caller performs finalization. A
  startup or finalization failure is thrown as an `ExceptionInfo` with the
  failed result in ex-data."
  [session]
  (when-not (map? session)
    (throw (IllegalArgumentException. "Session must be a map")))
  (stop-async! session)
  (try
    (let [result @(:completion session)]
      (if (= :failed (:status result))
        (throw (ex-info "VICE capture failed"
                        (dissoc result :error)
                        (:error result)))
        result))
    (catch InterruptedException error
      (request-stop! session {:kind :interrupted
                              :error error
                              :message "Waiting for capture shutdown was interrupted"})
      (throw error))))
