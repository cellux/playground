(ns omkamra.vice.decoder.capture
  "FIFO-backed recording and durable chunk capture lifecycle."
  (:require [clojure.java.io :as io]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.stream :as stream]
            [omkamra.vice.decoder.write :as write]
            [omkamra.vice.trace :as trace])
  (:import [java.io FileReader]
           [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(defn create-fifo!
  [^String fifo-path]
  (.mkdirs (.getParentFile (io/file fifo-path)))
  (io/delete-file fifo-path true)
  (let [^java.util.List command ["mkfifo" fifo-path]
        process (.start (ProcessBuilder. command))]
    (when-not (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Timed out creating monitor trace FIFO"
                      {:fifo-path fifo-path})))
    (when-not (zero? (.exitValue process))
      (throw (ex-info "Could not create monitor trace FIFO"
                      {:fifo-path fifo-path
                       :exit-code (.exitValue process)}))))
  fifo-path)

(def ^:private fifo-state-batch-size 512)

(defn update-stream-state!
  "Apply a stream update while excluding concurrent lifecycle updates.

  The ingestion reducer owns static interning state, so retrying it through an
  atom CAS race could duplicate its side effects. All capture lifecycle writes
  therefore share this lock with the FIFO reader."
  [stream-state f & args]
  (locking stream-state
    (apply swap! stream-state f args)))

(defn close-fifo-reader!
  [reader-ref reader-thread timeout-ms]
  (.join ^Thread reader-thread (long timeout-ms))
  (when (.isAlive ^Thread reader-thread)
    (when-let [reader @reader-ref]
      (try (.close ^java.io.Closeable reader)
           (catch Throwable _ nil)))
    (.interrupt ^Thread reader-thread)
    (.join ^Thread reader-thread 1000))
  (not (.isAlive ^Thread reader-thread)))

(def ^:private chunk-format :omkamra.vice/chunk-v1)
(def ^:private capture-format :omkamra.vice/capture-v1)
(def ^:private default-chunk-max-events 50000)
(def ^:private default-chunk-max-bytes 128000000)
(def ^:private default-chunk-queue-capacity 4)
(def ^:private default-writer-backpressure-ms 5000)
(def ^:private estimated-bytes-per-event 128)
(def ^:private writer-stop ::writer-stop)

(defn file-path
  [directory & parts]
  (.getPath (apply io/file directory parts)))

(defn write-edn-file!
  [file value]
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  file)

(defn atomic-write-edn!
  [file value]
  (let [target (.toPath (io/file file))
        partial (.toPath (io/file (str file ".partial")))]
    (write-edn-file! (.toFile partial) value)
    (try
      (Files/move partial target
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (catch AtomicMoveNotSupportedException _
        (Files/move partial target
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING]))))
    file))

(defn validate-chunk-options!
  [{:keys [chunk-max-events chunk-max-bytes chunk-queue-capacity
           writer-backpressure-ms]}]
  (doseq [[key value] [[:chunk-max-events chunk-max-events]
                       [:chunk-max-bytes chunk-max-bytes]
                       [:chunk-queue-capacity chunk-queue-capacity]
                       [:writer-backpressure-ms writer-backpressure-ms]]
          :when (some? value)]
    (when-not (pos-int? value)
      (throw (ex-info (str (name key) " must be a positive integer")
                      {key value})))))

(defn normalized-chunk-options
  [options]
  (validate-chunk-options! options)
  {:chunk-max-events (or (:chunk-max-events options)
                         default-chunk-max-events)
   :chunk-max-bytes (or (:chunk-max-bytes options)
                        default-chunk-max-bytes)
   :chunk-queue-capacity (or (:chunk-queue-capacity options)
                             default-chunk-queue-capacity)
   :writer-backpressure-ms (or (:writer-backpressure-ms options)
                               default-writer-backpressure-ms)})

(defn manifest-chunk-entry
  [{:keys [chunk-number event-range boundary summary]}]
  {:number chunk-number
   :file (str "chunks/" (artifact/chunk-file-name chunk-number))
   :event-range event-range
   :boundary boundary
   :summary summary})

(defn write-manifest!
  [capture-directory manifest-state]
  (atomic-write-edn! (file-path capture-directory "manifest.edn")
                     @manifest-state))

(defn make-manifest
  [capture-directory metadata chunk-options]
  {:format capture-format
   :capture-id (:capture-id metadata)
   :capture-directory capture-directory
   :input (:input metadata)
   :status :running
   :capture-mode :continuous
   :trace-transport :fifo
   :options (assoc
             (select-keys chunk-options [:chunk-max-events :chunk-max-bytes
                                         :chunk-queue-capacity])
             :full-capture? (true? (:full-capture? metadata)))
   :chunks []
   :current-chunk 1
   :event-count 0
   :analysis {:status :not-run
              :requested-stage nil
              :completed-stages []
              :stages {}
              :manifest-file "analysis/manifest.edn"}
   :finalized? false})

(defn writer-failure
  [writer-state]
  (:error @writer-state))

(defn update-writer-queue-metrics!
  [writer-state ^ArrayBlockingQueue queue]
  (let [depth (.size queue)]
    (swap! writer-state
           (fn [state]
             (-> state
                 (assoc :queue-depth depth)
                 (update :high-water-mark max depth))))))

(defn writer-timing!
  [writer-state finalize-ms write-ms byte-count]
  (swap! writer-state
         (fn [state]
           (let [write-ms-total (+ (long (or (:write-ms-total state) 0))
                                   write-ms)
                 bytes-written (+ (long (or (:bytes-written state) 0))
                                  byte-count)]
             (assoc state
                    :last-finalize-ms finalize-ms
                    :last-write-ms write-ms
                    :last-chunk-bytes byte-count
                    :max-finalize-ms (max (long (or (:max-finalize-ms state) 0))
                                          finalize-ms)
                    :max-write-ms (max (long (or (:max-write-ms state) 0))
                                       write-ms)
                    :finalize-ms-total (+ (long (or (:finalize-ms-total state) 0))
                                          finalize-ms)
                    :write-ms-total write-ms-total
                    :bytes-written bytes-written
                    :writer-throughput-bps (if (pos? write-ms-total)
                                             (long (/ (* bytes-written 1000)
                                                      write-ms-total))
                                             0))))))

(declare finalize-chunk)

(defn start-chunk-writer!
  [capture-directory manifest-state queue writer-state]
  (let [thread
        (Thread.
         (fn []
           (try
             (loop []
               (let [job (.take ^ArrayBlockingQueue queue)]
                 (update-writer-queue-metrics! writer-state queue)
                 (if (= writer-stop job)
                   (swap! writer-state assoc :status :stopped :queue-depth 0)
                   (let [{:keys [chunk metadata boundary]} job
                         finalize-start (System/nanoTime)
                         {:keys [chunk]} (finalize-chunk chunk metadata boundary)
                         finalize-ms (quot (- (System/nanoTime) finalize-start)
                                           1000000)
                         number (:chunk-number chunk)
                         filename (artifact/chunk-file-name number)
                         output-file (file-path capture-directory "chunks" filename)
                         write-start (System/nanoTime)]
                     ;; A chunk becomes visible in the manifest only after the
                     ;; final file has been atomically installed.
                     (atomic-write-edn! output-file chunk)
                     (swap! manifest-state
                            (fn [manifest]
                              (-> manifest
                                  (update :chunks conj (manifest-chunk-entry chunk))
                                  (assoc :current-chunk (inc number)
                                         :event-count (second (:event-range chunk))))))
                     (write-manifest! capture-directory manifest-state)
                     (let [write-ms (quot (- (System/nanoTime) write-start)
                                          1000000)
                           byte-count (.length (io/file output-file))]
                       (writer-timing! writer-state finalize-ms write-ms byte-count))
                     (swap! writer-state update :chunks-written (fnil inc 0))
                     (recur)))))
             (catch Throwable error
               (swap! writer-state assoc :status :failed :error error))))
         (str "vice-chunk-writer-" (System/nanoTime)))]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn enqueue-chunk!
  [^ArrayBlockingQueue queue writer-state job writer-backpressure-ms]
  (let [deadline (+ (System/nanoTime) (* 1000000 writer-backpressure-ms))]
    (loop [pressured? false]
      (when-let [error (writer-failure writer-state)]
        (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                        error)))
      (if (.offer queue job 100 TimeUnit/MILLISECONDS)
        (do
          (update-writer-queue-metrics! writer-state queue)
          (when pressured?
            (swap! writer-state update :backpressure-count (fnil inc 0)))
          nil)
        (if (< (System/nanoTime) deadline)
          (recur true)
          (throw (ex-info "Chunk writer queue remained full"
                          {:reason :chunk-writer-backpressure
                           :queue-capacity (.remainingCapacity queue)
                           :timeout-ms writer-backpressure-ms})))))))

(defn copy-memory
  [memory]
  (mapv write/u8 memory))

(defn open-chunk
  [number global-start initial-memory retain-samples?]
  {:number number
   :global-start global-start
   :initial-memory (copy-memory initial-memory)
   :ingester (stream/make-stream-ingester {:initial-memory initial-memory
                                           :retain-samples? retain-samples?})
   :stream-state (atom (stream/empty-stream-state))})

(defn chunk-event-count
  [chunk]
  @(:event-count (:ingester chunk)))

(defn chunk-estimated-bytes
  [chunk]
  (* estimated-bytes-per-event (chunk-event-count chunk)))

(defn chunk-limit-reason
  [chunk {:keys [chunk-max-events chunk-max-bytes]}]
  (cond
    (>= (chunk-event-count chunk) chunk-max-events) :max-events
    (>= (chunk-estimated-bytes chunk) chunk-max-bytes) :max-bytes
    :else nil))

(defn ingest-chunk-event!
  [chunk event]
  (let [stream-state (:stream-state chunk)
        ingester (:ingester chunk)]
    (locking stream-state
      (let [state ((:step ingester) @stream-state event)]
        (reset! stream-state (assoc state :event-count @(:event-count ingester)))))))

(defn finalize-chunk
  [chunk metadata boundary]
  (let [{:keys [number global-start initial-memory ingester stream-state]} chunk
        _ (when-not (:sealed? chunk)
            (update-stream-state! stream-state (:complete ingester)))
        events (stream/instruction-block-stream stream-state ingester)
        analysis (stream/analysis-snapshot (:analysis-state ingester))
        final-memory (copy-memory (:memory @(:analysis-state ingester)))
        local-count (:event-count events)
        event-range [global-start (+ global-start local-count)]
        artifact (stream/minimal-pipeline-artifact
                  events initial-memory final-memory
                  (merge metadata {:event-range event-range
                                   :chunk-number number})
                  analysis)
        summary {:event-count local-count
                 :instruction-count (count (:instructions events))
                 :block-count (count (:blocks events))
                 :write-count (count (:writes analysis))}]
    {:chunk {:format chunk-format
             :capture-id (:capture-id metadata)
             :chunk-number number
             :event-range event-range
             :event-index-scope :chunk-local
             :previous-chunk (when (> number 1) (dec number))
             :next-chunk (when-not (= :final (:kind boundary)) (inc number))
             :boundary boundary
             :local-event-count local-count
             :events events
             :stages (:stages artifact)
             :summary summary}
     :final-memory final-memory
     :summary summary}))

(defn publish-open-chunk!
  [coordinator chunk]
  (swap! coordinator assoc
         :chunk-number (:number chunk)
         :chunk-event-count (chunk-event-count chunk)
         :global-event-count (+ (:global-start chunk) (chunk-event-count chunk))
         :estimated-open-chunk-bytes (chunk-estimated-bytes chunk)))

(defn seal-chunk!
  "Finish only the mutable transducer work needed before ownership transfer.

  The expensive artifact projection remains on the writer thread. Completing
  here is necessary because the final inferred write changes the memory image
  from which the next chunk starts; it is bounded to one pending event and the
  final basic-block tail."
  [chunk]
  (let [{:keys [ingester stream-state]} chunk]
    (update-stream-state! stream-state (:complete ingester))
    (assoc chunk :sealed? true)))

(defn close-open-chunk!
  [coordinator metadata chunk-options boundary]
  (let [chunk (:open-chunk @coordinator)]
    (when (pos? (chunk-event-count chunk))
      (let [chunk (seal-chunk! chunk)
            final-memory (copy-memory
                          (:memory @(:analysis-state (:ingester chunk))))
            next-chunk (open-chunk (inc (:number chunk))
                                   (+ (:global-start chunk)
                                      (chunk-event-count chunk))
                                   final-memory
                                   (:retain-samples? @coordinator))]
        ;; Transfer the sealed mutable chunk to the bounded queue. The reader
        ;; never performs artifact projection or EDN serialization.
        (enqueue-chunk! (:writer-queue @coordinator)
                        (:writer-state @coordinator)
                        {:chunk chunk
                         :metadata metadata
                         :boundary boundary}
                        (:writer-backpressure-ms chunk-options))
        (swap! coordinator assoc :open-chunk next-chunk)
        (publish-open-chunk! coordinator next-chunk)
        chunk))))

(defn ingest-chunked-batch!
  [coordinator metadata chunk-options records]
  (doseq [event records]
    (let [chunk (:open-chunk @coordinator)]
      (ingest-chunk-event! chunk event)
      (publish-open-chunk! coordinator chunk)
      (when-let [reason (chunk-limit-reason chunk chunk-options)]
        (close-open-chunk! coordinator metadata chunk-options
                           {:kind :forced-size :reason reason})))))

(defn start-chunked-fifo-reader!
  [fifo-path coordinator metadata chunk-options reader-ref on-error!]
  (let [thread
        (Thread.
         (fn []
           (try
             (with-open [reader (FileReader. fifo-path)]
               (reset! reader-ref reader)
               (swap! coordinator assoc :reader-status :streaming)
               (let [batch (volatile! (transient []))
                     flush! (fn []
                              (let [records (persistent! @batch)]
                                (vreset! batch (transient []))
                                (when (seq records)
                                  (ingest-chunked-batch! coordinator metadata
                                                         chunk-options records))))
                     consume! (fn [record]
                                (let [next-batch (conj! @batch record)]
                                  (if (= fifo-state-batch-size (count next-batch))
                                    (do
                                      (vreset! batch (transient []))
                                      (ingest-chunked-batch!
                                       coordinator metadata chunk-options
                                       (persistent! next-batch)))
                                    (vreset! batch next-batch))))]
                 (try
                   (trace/reduce-records! reader consume!
                                          (:retain-samples? @coordinator))
                   (finally (flush!))))
               (swap! coordinator assoc :reader-status :eof))
             (catch java.io.IOException error
               (when-not (#{:stopping :stopped} (:reader-status @coordinator))
                 (swap! coordinator assoc :reader-status :error :reader-error error)
                 (on-error! error)))
             (catch Throwable error
               (swap! coordinator assoc :reader-status :error :reader-error error)
               (on-error! error))))
         (str "vice-chunk-fifo-" (System/nanoTime)))]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn shutdown-chunk-writer!
  [coordinator timeout-ms]
  (let [{:keys [writer-queue writer-thread writer-state]} @coordinator
        deadline (+ (System/nanoTime) (* 1000000 timeout-ms))]
    ;; Preserve FIFO order: the sentinel is accepted only after every chunk
    ;; job. Unlike an unconditional put, this also notices a dead writer.
    (loop []
      (when-let [error (writer-failure writer-state)]
        (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                        error)))
      (cond
        (.offer ^ArrayBlockingQueue writer-queue writer-stop 100 TimeUnit/MILLISECONDS) nil
        (< (System/nanoTime) deadline) (recur)
        :else (throw (ex-info "Chunk writer queue did not drain"
                              {:reason :chunk-writer-timeout
                               :timeout-ms timeout-ms}))))
    (.join ^Thread writer-thread (long timeout-ms))
    (when (.isAlive ^Thread writer-thread)
      (throw (ex-info "Chunk writer did not stop"
                      {:reason :chunk-writer-timeout :timeout-ms timeout-ms})))
    (when-let [error (writer-failure writer-state)]
      (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                      error)))))

(defn start-capture
  "Start a bounded, FIFO-backed chunked capture while VICE is paused.

  Each closed chunk is persisted by a bounded background writer. Options are
  `:capture-directory`, `:chunk-max-events`, `:chunk-max-bytes`,
  `:chunk-queue-capacity`, `:writer-backpressure-ms`, `:metadata`, and
  `:retain-samples?`. `:capture-directory` must already exist and is never
  replaced; chunks are written below its `chunks/` directory."
  ([conn] (start-capture conn {}))
  ([conn {:keys [fifo-path metadata checkpoint-op retain-samples? capture-directory]
          :or {checkpoint-op 4 retain-samples? false}
          :as options}]
   (when-not capture-directory
     (throw (ex-info "Chunked capture requires :capture-directory" {})))
   (let [chunk-options (normalized-chunk-options options)
         fifo-path (or fifo-path (str "/tmp/omkamra-vice/trace-" (System/nanoTime) ".fifo"))
         capture-directory (.getCanonicalPath (io/file capture-directory))
         chunks-directory (io/file capture-directory "chunks")
         _ (when-not (.isDirectory (io/file capture-directory))
             (throw (ex-info "Capture directory must exist" {:capture-directory capture-directory})))
         _ (when-not (.mkdirs chunks-directory)
             (when-not (.isDirectory chunks-directory)
               (throw (ex-info "Could not create chunks directory"
                               {:capture-directory capture-directory}))))
         manifest-state (atom (make-manifest capture-directory metadata chunk-options))
         queue (ArrayBlockingQueue. (int (:chunk-queue-capacity chunk-options)))
         writer-state (atom {:status :starting :queue-depth 0 :high-water-mark 0
                             :chunks-written 0 :backpressure-count 0
                             :last-finalize-ms 0 :last-write-ms 0
                             :last-chunk-bytes 0 :max-finalize-ms 0
                             :max-write-ms 0 :finalize-ms-total 0
                             :write-ms-total 0 :bytes-written 0
                             :writer-throughput-bps 0})
         writer-thread (start-chunk-writer! capture-directory manifest-state queue writer-state)
         state (atom {:status :starting :instruction-count 0})
         reader-ref (atom nil)
         reader-thread (atom nil)
         checkpoint-number (atom nil)
         prior-ignored-types (bm/ignored-unsolicited-types conn)]
     (try
       (write-manifest! capture-directory manifest-state)
       (swap! writer-state assoc :status :running)
       (bm/drain-events conn)
       (bm/ping conn)
       (bm/drain-events conn)
       (let [initial-memory (mapv write/u8 (:memory (bm/mem-get conn {:start 0 :end 65535})))
             coordinator (atom {:open-chunk (open-chunk 1 0 initial-memory retain-samples?)
                                :chunk-number 1
                                :chunk-event-count 0
                                :global-event-count 0
                                :estimated-open-chunk-bytes 0
                                :chunks-written 0
                                :reader-status :starting
                                :retain-samples? retain-samples?
                                :writer-queue queue
                                :writer-state writer-state
                                :writer-thread writer-thread})]
         (create-fifo! fifo-path)
         (reset! reader-thread
                 (start-chunked-fifo-reader!
                  fifo-path coordinator metadata chunk-options reader-ref
                  (fn [_]
                    ;; Stop VICE's FIFO writer before the reader closes. This
                    ;; prevents a reader-side failure from becoming SIGPIPE in
                    ;; VICE and preserves the original failure reason.
                    (try
                      (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
                      (catch Throwable _ nil)))))
         (bm/ignore-unsolicited-types! conn (conj prior-ignored-types
                                                  bm/MON_RESPONSE_CHECKPOINT_INFO))
         (let [checkpoint (bm/checkpoint-set conn {:start 0 :end 0xffff
                                                   :stop? false :enabled? true
                                                   :op checkpoint-op :temporary? false})]
           (reset! checkpoint-number (:number checkpoint))
           (bm/resource-set conn {:name "MonitorLogFileName" :value fifo-path})
           (bm/resource-set conn {:name "MonitorLogEnabled" :value 1})
           (reset! state {:status :running :instruction-count 0 :transport :fifo})
           {:kind :omkamra.vice/chunked-capture-v1
            :conn conn :fifo-path fifo-path :checkpoint-number checkpoint-number
            :metadata metadata :capture-directory capture-directory
            :manifest-state manifest-state :state state :coordinator coordinator
            ;; Capture orchestration uses this shared state to notice FIFO
            ;; reader failures and unexpected VICE exits while running.
            :stream-state coordinator
            :chunk-options chunk-options :reader-ref reader-ref
            :reader-thread @reader-thread :prior-ignored-types prior-ignored-types}))
       (catch Throwable error
         (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0}) (catch Throwable _ nil))
         (when-let [number @checkpoint-number]
           (try (bm/checkpoint-delete conn {:number number}) (catch Throwable _ nil)))
         (bm/ignore-unsolicited-types! conn prior-ignored-types)
         (when-let [thread @reader-thread] (close-fifo-reader! reader-ref thread 1000))
         (.offer queue writer-stop)
         (.join writer-thread 1000)
         (io/delete-file fifo-path true)
         (throw error))))))

(defn chunked-capture-status
  "Return bounded progress metadata without loading closed chunk artifacts."
  [capture]
  (let [{:keys [coordinator state]} capture
        {:keys [chunk-number chunk-event-count global-event-count reader-status reader-error
                writer-state]} @coordinator
        writer @writer-state]
    (merge (dissoc @state :artifact)
           {:event-count global-event-count
            :chunk-number chunk-number
            :chunk-event-count chunk-event-count
            :chunk-count (:chunks-written writer)
            :chunks-written (:chunks-written writer)
            :chunks-pending (:queue-depth writer)
            :writer-status (:status writer)
            :writer-queue-depth (:queue-depth writer)
            :writer-high-water-mark (:high-water-mark writer)
            :writer-backpressure-count (:backpressure-count writer)
            :last-finalize-ms (:last-finalize-ms writer)
            :last-write-ms (:last-write-ms writer)
            :last-chunk-bytes (:last-chunk-bytes writer)
            :max-finalize-ms (:max-finalize-ms writer)
            :max-write-ms (:max-write-ms writer)
            :finalize-ms-total (:finalize-ms-total writer)
            :write-ms-total (:write-ms-total writer)
            :bytes-written (:bytes-written writer)
            :writer-throughput-bps (:writer-throughput-bps writer)
            :reader-status reader-status
            :reader-alive? (.isAlive ^Thread (:reader-thread capture))
            :reader-error (some-> reader-error .getMessage)})))

(defn stop-chunked-capture
  [capture]
  (let [{:keys [conn fifo-path checkpoint-number state coordinator metadata
                manifest-state capture-directory reader-ref reader-thread
                prior-ignored-types chunk-options]} capture]
    (if-let [result (:result @state)]
      result
      (try
        (swap! state assoc :status :stopping)
        (swap! coordinator assoc :reader-status :stopping)
        (try
          (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
          (catch Throwable _ nil))
        (when-not (close-fifo-reader! reader-ref reader-thread 5000)
          (throw (ex-info "FIFO trace reader did not stop" {:fifo-path fifo-path})))
        (when-let [error (:reader-error @coordinator)] (throw error))
        (close-open-chunk! coordinator metadata chunk-options {:kind :final :reason :stopped})
        (shutdown-chunk-writer! coordinator 30000)
        ;; Capture finalization owns only raw persistence. Derived output is a
        ;; separate, explicitly requested operation in omkamra.vice.analysis.
        (swap! manifest-state assoc :status :stopped :finalized? true)
        (write-manifest! capture-directory manifest-state)
        (let [result {:status :stopped
                      :capture-directory capture-directory
                      :manifest-path (file-path capture-directory "manifest.edn")
                      :event-count (:event-count @manifest-state)
                      :chunk-count (count (:chunks @manifest-state))}]
          (swap! state assoc :status :stopped :result result)
          result)
        (catch Throwable error
          (swap! state assoc :status :failed :error error)
          (swap! manifest-state assoc :status :failed :finalized? false
                 :failure {:reason (or (:reason (ex-data error)) :capture-error)
                           :message (.getMessage error)})
          (try (write-manifest! capture-directory manifest-state) (catch Throwable _ nil))
          (throw error))
        (finally
          (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0}) (catch Throwable _ nil))
          (when-let [number @checkpoint-number]
            (try (bm/checkpoint-delete conn {:number number}) (catch Throwable _ nil)))
          (bm/ignore-unsolicited-types! conn prior-ignored-types)
          (close-fifo-reader! reader-ref reader-thread 1000)
          (io/delete-file fifo-path true)
          (bm/drain-events conn)
          (reset! reader-ref nil)
          (reset! checkpoint-number nil))))))

(defn capture-status
  "Return lightweight chunked-recorder progress without loading closed chunks."
  [capture]
  (chunked-capture-status capture))

(defn stop-capture
  "Finalize a chunked capture, draining its writer before returning the manifest result."
  [capture]
  (stop-chunked-capture capture))
