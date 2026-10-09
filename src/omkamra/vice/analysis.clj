(ns omkamra.vice.analysis
  "Ordered, replaceable offline analysis for committed VICE captures."
  (:refer-clojure :exclude [run!])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [omkamra.vice.decoder :as decoder])
  (:import
   [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]
   [java.time Instant]
   [java.util.concurrent Callable ExecutionException Executors TimeUnit]))

(def ^:private analysis-format :omkamra.vice/analysis-v1)
(def ^:private stage-format :omkamra.vice/analysis-stage-v1)
(def ^:private active-runs (atom #{}))
;; Expanded write analysis can transiently require substantial heap for one
;; large raw chunk. One worker is safe by default; callers with sufficient heap
;; can opt into bounded overlap explicitly.
(def ^:private default-chunk-parallelism 1)

(defn- file-path
  [directory & parts]
  (.getPath (apply io/file directory parts)))

(defn- now
  []
  (str (Instant/now)))

(defn- write-edn-file!
  [file value]
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  file)

(defn- atomic-write-edn!
  [file value]
  (let [target (.toPath (io/file file))
        partial (.toPath (io/file (str file ".partial")))]
    (.mkdirs (.getParentFile (.toFile target)))
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

(defn- delete-tree!
  [file]
  (when (.isDirectory ^java.io.File file)
    (doseq [child (.listFiles ^java.io.File file)]
      (delete-tree! child)))
  (io/delete-file file true))

(defn- move!
  [source target]
  (try
    (Files/move (.toPath (io/file source))
                (.toPath (io/file target))
                (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (catch AtomicMoveNotSupportedException _
      (Files/move (.toPath (io/file source))
                  (.toPath (io/file target))
                  (make-array StandardCopyOption 0)))))

(defn- replace-directory!
  "Install a fully written stage directory while retaining the old output until
  the replacement is ready. A failed install restores the previous directory."
  [staging target]
  (let [target-file (io/file target)
        backup (str target ".previous")]
    (delete-tree! (io/file backup))
    (if-not (.exists target-file)
      (move! staging target)
      (do
        (move! target backup)
        (try
          (move! staging target)
          (delete-tree! (io/file backup))
          (catch Throwable error
            (when-not (.exists target-file)
              (move! backup target))
            (throw error)))))))

(defn- stage-directory
  [capture-directory id]
  (file-path capture-directory "analysis" "stages" (name id)))

(defn- stage-output-file
  [id]
  (str "analysis/stages/" (name id) "/index.edn"))

(defn- stage-configuration
  [options stage]
  (or (get-in options [:config (:id stage)]) {}))

(defn- stage-chunk-file
  [stage-directory chunk-number]
  (file-path stage-directory "chunks"
             (format "chunk-%06d.edn" chunk-number)))

(defn- read-stage-chunk
  [capture-directory completed-stages stage-id chunk-number]
  (let [stage-directory (get-in completed-stages
                                [stage-id :output-directory])]
    (when-not stage-directory
      (throw (ex-info "Required analysis stage output is unavailable"
                      {:stage stage-id
                       :chunk-number chunk-number})))
    (edn/read-string
     (slurp (stage-chunk-file (file-path capture-directory stage-directory)
                              chunk-number)))))

(defn- init-chunk-stage!
  "Create the mutable state for one staged, chunk-at-a-time projection.

  Stage state is private to one analysis request.  The broadcast coordinator
  owns raw-chunk reads; stages own only their current derived chunk and their
  staged output index."
  [{:keys [output-directory capture-manifest] :as context}]
  (.mkdirs (io/file output-directory "chunks"))
  {:context context
   :chunk-count (count (:chunks capture-manifest))
   ;; Several chunk workers may persist independent files concurrently. The
   ;; counters/index are the only shared stage state and are updated after a
   ;; chunk file has been atomically installed.
   :completed (atom 0)
   :entries (atom (sorted-map))})

(defn- persist-stage-chunk!
  ([state raw-chunk derived]
   (persist-stage-chunk! state raw-chunk derived derived))
  ([state raw-chunk derived output]
   (let [{:keys [output-directory stage progress!]} (:context state)
         chunk-number (:chunk-number raw-chunk)
         output-file (stage-chunk-file output-directory chunk-number)]
     (atomic-write-edn! output-file derived)
     (let [completed
           (locking state
             (swap! (:entries state)
                    assoc chunk-number
                    {:number chunk-number
                     :file (str "chunks/"
                                (format "chunk-%06d.edn" chunk-number))
                     :event-range (:event-range raw-chunk)})
             (swap! (:completed state) inc))]
       (progress! {:stage (:id stage)
                   :chunks-completed completed
                   :chunk-count (:chunk-count state)
                   :current-chunk chunk-number})
       {:state state
        :output output}))))

(defn- writes-stage-step
  [state raw-chunk _dependencies]
  (let [writes-chunk (decoder/derive-writes-chunk raw-chunk)]
    (persist-stage-chunk! state raw-chunk writes-chunk
                          (assoc writes-chunk
                                 ::decoder/write-source
                                 (decoder/writes-source raw-chunk writes-chunk)))))

(defn- close-chunk-stage!
  [{:keys [context chunk-count entries]}]
  (let [{:keys [output-directory capture-manifest stage]} context
        {:keys [number id version]} stage
        index {:format stage-format
               :stage number
               :stage-id id
               :version version
               :capture-id (:capture-id capture-manifest)
               :source-format (:format capture-manifest)
               :chunk-count chunk-count
               :chunks (vec (vals @entries))}]
    (atomic-write-edn! (file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count}))

(defn- structure-stage-step
  [state raw-chunk _dependencies]
  (persist-stage-chunk! state raw-chunk
                        (decoder/derive-structure-chunk raw-chunk)))

(defn- video-stage-step
  [state raw-chunk dependencies]
  (let [writes-chunk (or (:writes dependencies)
                         (throw (ex-info "Video analysis requires writes output"
                                         {:chunk-number (:chunk-number raw-chunk)})))
        writes (decoder/writes-source raw-chunk writes-chunk)]
    (persist-stage-chunk! state raw-chunk
                          (decoder/derive-video-chunk raw-chunk writes))))

(defn- assets-stage-step
  [state raw-chunk dependencies]
  (let [writes-chunk (or (:writes dependencies)
                         (throw (ex-info "Assets analysis requires writes output"
                                         {:chunk-number (:chunk-number raw-chunk)})))
        video-chunk (or (:video dependencies)
                        (throw (ex-info "Assets analysis requires video output"
                                        {:chunk-number (:chunk-number raw-chunk)})))
        writes (decoder/writes-source raw-chunk writes-chunk)]
    (persist-stage-chunk! state raw-chunk
                          (decoder/derive-assets-chunk raw-chunk video-chunk writes))))

(def ^:private stage-registry
  [{:id :writes
    :version "writes-v2"
    :depends-on []
    :init init-chunk-stage!
    :step writes-stage-step
    :close close-chunk-stage!}
   {:id :structure
    :version "structure-v1"
    :depends-on []
    :init init-chunk-stage!
    :step structure-stage-step
    :close close-chunk-stage!}
   {:id :video
    :version "video-v1"
    :depends-on [:writes]
    :init init-chunk-stage!
    :step video-stage-step
    :close close-chunk-stage!}
   {:id :assets
    :version "assets-v1"
    :depends-on [:writes :video]
    :init init-chunk-stage!
    :step assets-stage-step
    :close close-chunk-stage!}])

(def ^:private stage-definitions
  (into {} (map (juxt :id identity) stage-registry)))

(def ^:private all-stage-id :all)

(defn- stage-dependencies
  [stage-id]
  (if (= all-stage-id stage-id)
    (mapv :id stage-registry)
    (if-let [stage (get stage-definitions stage-id)]
      (:depends-on stage)
      (throw (ex-info "Unknown analysis stage"
                      {:stage stage-id
                       :available (conj (mapv :id stage-registry)
                                        all-stage-id)})))))

(defn- stage-definition
  [stage-id]
  (or (get stage-definitions stage-id)
      (throw (ex-info "Virtual analysis target is not executable"
                      {:stage stage-id}))))

(defn- stage-plan
  "Resolve a target into a dependency-ordered concrete stage plan."
  [target]
  (letfn [(visit [stage-id visiting visited result]
            (when (contains? visiting stage-id)
              (throw (ex-info "Analysis stage dependency cycle"
                              {:stage stage-id})))
            (if (contains? visited stage-id)
              [visited result]
              (let [dependencies (stage-dependencies stage-id)
                    [visited result]
                    (reduce (fn [[visited result] dependency]
                              (visit dependency
                                     (conj visiting stage-id)
                                     visited
                                     result))
                            [visited result]
                            dependencies)]
                [(conj visited stage-id)
                 (if (= all-stage-id stage-id)
                   result
                   (conj result (stage-definition stage-id)))])))]
    (second (visit target #{} #{} []))))

(defn stages
  "Return the concrete registered analysis stages.

  Stages are named and declare `:depends-on`; `:all` is a virtual target that
  resolves to every concrete stage.  A stage implements the uniform
  `:init`/`:step`/`:close` reducer contract; implementation functions are not
  exposed through this descriptive API."
  [_capture-directory]
  (mapv #(dissoc % :init :step :close) stage-registry))

(defn iterate-chunks
  "Return a lazy sequence of immutable raw chunks in manifest order."
  [capture-directory]
  (let [manifest (decoder/read-capture-manifest capture-directory)]
    (map (fn [{:keys [number]}]
           (decoder/read-chunk capture-directory number))
         (:chunks manifest))))

(defn- analysis-manifest-file
  [capture-directory]
  (file-path capture-directory "analysis" "manifest.edn"))

(defn- new-analysis-manifest
  [capture-directory capture-manifest]
  {:format analysis-format
   :capture-id (:capture-id capture-manifest)
   :capture-directory capture-directory
   :source-format (:format capture-manifest)
   :status :not-run
   :requested-stage nil
   :completed-stages []
   :stages {}
   :manifest-file "analysis/manifest.edn"})

(defn- normalize-analysis-manifest
  [manifest]
  (let [known-stage-ids (set (keys stage-definitions))]
    (-> manifest
        (update :stages #(select-keys (or % {}) known-stage-ids))
        (update :completed-stages
                #(->> (or % [])
                      (filter known-stage-ids)
                      vec))
        (update :requested-stage
                #(when (or (= all-stage-id %)
                           (contains? known-stage-ids %))
                   %)))))

(defn- read-analysis-manifest
  [capture-directory capture-manifest]
  (let [file (io/file (analysis-manifest-file capture-directory))]
    (normalize-analysis-manifest
     (if (.isFile file)
       (edn/read-string (slurp file))
       (new-analysis-manifest capture-directory capture-manifest)))))

(defn- write-analysis-manifest!
  [capture-directory manifest]
  (atomic-write-edn! (analysis-manifest-file capture-directory) manifest))

(defn- analysis-pointer
  [analysis-manifest]
  {:status (:status analysis-manifest)
   :requested-stage (:requested-stage analysis-manifest)
   :completed-stages (:completed-stages analysis-manifest)
   :stages (:stages analysis-manifest)
   :manifest-file "analysis/manifest.edn"})

(defn- write-capture-analysis-pointer!
  [capture-directory capture-manifest analysis-manifest]
  (atomic-write-edn!
   (file-path capture-directory "manifest.edn")
   (assoc capture-manifest :analysis (analysis-pointer analysis-manifest))))

(defn- require-finalized-capture!
  [capture-manifest]
  (when-not (and (= :stopped (:status capture-manifest))
                 (:finalized? capture-manifest))
    (throw (ex-info "Analysis requires a finalized stopped capture"
                    {:status (:status capture-manifest)
                     :finalized? (:finalized? capture-manifest)}))))

(defn- chunk-parallelism
  [options]
  (let [parallelism (or (:chunk-parallelism options)
                        default-chunk-parallelism)]
    (when-not (pos-int? parallelism)
      (throw (ex-info ":chunk-parallelism must be a positive integer"
                      {:chunk-parallelism parallelism})))
    parallelism))

(defn- target-stage-id
  [stage]
  (let [target (or stage all-stage-id)
        target (if (= :latest target) all-stage-id target)]
    (when-not (or (= all-stage-id target)
                  (contains? stage-definitions target))
      (throw (ex-info "Unknown analysis stage"
                      {:stage stage
                       :available (conj (mapv :id stage-registry)
                                        all-stage-id)})))
    target))

(defn- dependency-signature
  [analysis-manifest stage]
  (into {}
        (map (fn [dependency]
               [dependency
                (select-keys (get-in analysis-manifest
                                     [:stages dependency])
                             [:id :version :configuration])]))
        (:depends-on stage)))

(defn- stage-compatible?
  [analysis-manifest stage configuration]
  (let [stage-id (:id stage)
        completed (get-in analysis-manifest [:stages stage-id])]
    (and (= :complete (:status completed))
         (= stage-id (:id completed))
         (= (:version stage) (:version completed))
         (= configuration (:configuration completed))
         (= (dependency-signature analysis-manifest stage)
            (:dependency-signature completed))
         (.isFile (io/file (:capture-directory analysis-manifest)
                           (:index-file completed))))))

(defn- stage-order
  [stage-id]
  (or (some->> stage-registry
               (map-indexed vector)
               (some (fn [[index stage]]
                       (when (= stage-id (:id stage)) index))))
      Integer/MAX_VALUE))

(defn- completed-stage-ids
  [analysis-manifest]
  (->> (:stages analysis-manifest)
       (keep (fn [[stage-id stage]]
               (when (= :complete (:status stage)) stage-id)))
       (sort-by stage-order)
       vec))

(defn- stage-progress-files
  [stage-directory]
  (let [directory (io/file stage-directory "chunks")
        files (when (.isDirectory directory)
                (filter #(and (.isFile ^java.io.File %)
                              (re-matches #"chunk-\d+\.edn" (.getName %)))
                        (.listFiles directory)))]
    {:chunks-completed (count files)
     :current-chunk (some->> files
                             (map #(.getName ^java.io.File %))
                             (keep #(second (re-matches #"chunk-(\d+)\.edn" %)))
                             (map #(Long/parseLong %))
                             seq
                             (apply max 0))}))

(defn- legacy-progress->staged-progress
  [capture-directory capture-manifest progress]
  (let [active-stages (->> stage-registry
                           (map :id)
                           (filter #(let [partial (io/file
                                                   (str (stage-directory
                                                         capture-directory %)
                                                        ".partial"))]
                                      (.isDirectory partial)))
                           set)
        stages (into {}
                     (map (fn [stage-id]
                            [stage-id
                             (assoc (stage-progress-files
                                     (str (stage-directory capture-directory stage-id)
                                          ".partial"))
                                    :chunk-count (count (:chunks capture-manifest)))])
                          active-stages))]
    {:chunks-total (count (:chunks capture-manifest))
     :stages stages
     :active-stages active-stages
     :last-completed (when-let [stage (:stage progress)]
                       {:stage stage
                        :chunk (:current-chunk progress)})
     :legacy? true}))

(defn- analysis-progress
  [capture-directory capture-manifest analysis-manifest]
  (let [progress (:progress analysis-manifest)]
    (if (and progress (:stages progress))
      progress
      (when progress
        (legacy-progress->staged-progress capture-directory
                                          capture-manifest
                                          progress)))))

(defn status
  "Return capture and analysis status without reading any chunk artifact."
  [capture-directory]
  (let [capture-manifest (decoder/read-capture-manifest capture-directory)
        analysis-manifest (read-analysis-manifest capture-directory capture-manifest)]
    {:capture-status (:status capture-manifest)
     :analysis-status (:status analysis-manifest)
     :analysis-requested-stage (:requested-stage analysis-manifest)
     :analysis-progress (analysis-progress capture-directory capture-manifest
                                           analysis-manifest)
     :analysis-failure (:failure analysis-manifest)
     :analysis-stages (->> (:stages analysis-manifest)
                           (sort-by (comp stage-order key))
                           (mapv val))
     :chunk-count (count (:chunks capture-manifest))
     :event-count (:event-count capture-manifest)}))

(defn- begin-analysis!
  [capture-directory capture-manifest analysis-manifest target]
  (let [running (-> analysis-manifest
                    (assoc :status :running
                           :requested-stage target
                           :started-at (now)
                           :progress nil)
                    (dissoc :failure))]
    (write-analysis-manifest! capture-directory running)
    (write-capture-analysis-pointer! capture-directory capture-manifest running)
    running))

(defn- claim-run!
  [capture-directory]
  (locking active-runs
    (when (contains? @active-runs capture-directory)
      (throw (ex-info "Analysis is already running for this capture"
                      {:capture-directory capture-directory})))
    (swap! active-runs conj capture-directory)))

(defn- release-run!
  [capture-directory]
  (swap! active-runs disj capture-directory))

(defn- plan-execution
  [analysis-manifest plan options force?]
  (loop [remaining plan
         compatible []
         compatible-ids #{}
         to-run []]
    (if-let [stage (first remaining)]
      (let [configuration (stage-configuration options stage)
            dependencies-compatible?
            (every? compatible-ids (:depends-on stage))
            compatible?
            (and (not force?)
                 dependencies-compatible?
                 (stage-compatible? analysis-manifest stage configuration))]
        (if compatible?
          (recur (rest remaining)
                 (conj compatible (:id stage))
                 (conj compatible-ids (:id stage))
                 to-run)
          (recur (rest remaining)
                 compatible
                 compatible-ids
                 (conj to-run stage))))
      {:compatible compatible
       :to-run to-run})))

(defn- stage-implementation!
  [stage]
  (doseq [operation [:init :step :close]]
    (when-not (fn? (get stage operation))
      (throw (ex-info "Analysis stage has no reducer operation"
                      {:stage (dissoc stage :init :step :close)
                       :operation operation}))))
  stage)

(defn- initialize-stage-worker!
  [capture-directory capture-manifest options progress! stage]
  (let [stage (stage-implementation! stage)
        id (:id stage)
        target-directory (stage-directory capture-directory id)
        staging-directory (str target-directory ".partial")
        _ (delete-tree! (io/file staging-directory))
        context {:capture-directory capture-directory
                 :output-directory staging-directory
                 :capture-manifest capture-manifest
                 :configuration (stage-configuration options stage)
                 :stage (dissoc stage :init :step :close)
                 :progress! progress!}]
    {:stage stage
     :target-directory target-directory
     :staging-directory staging-directory
     :state ((:init stage) context)}))

(defn- initialize-stage-workers!
  [capture-directory capture-manifest options progress! stages active-stage]
  (loop [workers []
         [stage & remaining] stages]
    (if-not stage
      workers
      (let [id (:id stage)]
        (reset! active-stage id)
        (let [worker
              (try
                (initialize-stage-worker! capture-directory capture-manifest
                                          options progress! stage)
                (catch Throwable error
                  (doseq [{:keys [staging-directory]} workers]
                    (delete-tree! (io/file staging-directory)))
                  ;; `:init` can fail after creating its own directory, before
                  ;; a worker record exists to identify it.
                  (delete-tree! (io/file (str (stage-directory capture-directory id)
                                              ".partial")))
                  (throw error)))]
          (recur (conj workers worker) remaining))))))

(defn- materialize-dependency
  [raw-chunk dependency output]
  (if (= dependency :writes)
    (assoc output ::decoder/write-source
           (decoder/writes-source raw-chunk output))
    output))

(defn- dependency-output
  "Resolve one dependency once for the current raw chunk.

  `outputs` holds results from stages active in this task. `dependency-cache`
  holds compatible committed outputs, so siblings such as video and assets do
  not reread or rematerialize the same writes-stage chunk."
  [capture-directory completed-stages raw-chunk outputs dependency-cache
   dependency chunk-number]
  (cond
    (contains? outputs dependency)
    [(get outputs dependency) dependency-cache]

    (contains? dependency-cache dependency)
    [(get dependency-cache dependency) dependency-cache]

    :else
    (let [output (->> (read-stage-chunk capture-directory completed-stages
                                        dependency chunk-number)
                      (materialize-dependency raw-chunk dependency))]
      [output (assoc dependency-cache dependency output)])))

(defn- run-broadcast-chunk!
  [capture-directory completed-stages workers active-stage chunk-number]
  (let [raw-chunk (decoder/read-chunk capture-directory chunk-number)]
    (reduce
     (fn [{:keys [outputs dependency-cache]} {:keys [stage state]}]
       (let [id (:id stage)
             _ (reset! active-stage id)
             {:keys [dependencies dependency-cache]}
             (reduce
              (fn [{:keys [dependencies dependency-cache]} dependency]
                (let [[output dependency-cache]
                      (dependency-output capture-directory completed-stages
                                         raw-chunk outputs dependency-cache
                                         dependency chunk-number)]
                  {:dependencies (assoc dependencies dependency output)
                   :dependency-cache dependency-cache}))
              {:dependencies {} :dependency-cache dependency-cache}
              (:depends-on stage))
             {:keys [output]} ((:step stage) state raw-chunk dependencies)]
         {:outputs (assoc outputs id output)
          :dependency-cache dependency-cache}))
     {:outputs {} :dependency-cache {}}
     workers)
    ;; The reducer result contains per-chunk dependency/output references and
    ;; must not escape the task. Returning nil lets completed Future instances
    ;; release the raw chunk, lazy sources, and derived values immediately.
    nil))

(defn- run-broadcast-pipeline!
  "Derive requested stages with a bounded pool of chunk-local fan-out tasks.

  A task reads one raw chunk, runs its stage reducers in dependency order, and
  releases every intermediate result on completion.  Therefore assets for one
  chunk can overlap video or writes work for another chunk without making live
  memory proportional to capture length. Stage files remain private until all
  tasks and stage closers have succeeded."
  [capture-directory capture-manifest completed-stages workers active-stage
   chunk-parallelism]
  (if (empty? workers)
    {}
    (let [executor (Executors/newFixedThreadPool chunk-parallelism)
          futures (mapv (fn [{:keys [number]}]
                          (.submit executor
                                   ^Callable
                                   (reify Callable
                                     (call [_]
                                       (run-broadcast-chunk!
                                        capture-directory completed-stages workers
                                        active-stage number)))))
                        (:chunks capture-manifest))]
      (try
        (doseq [future futures]
          (try
            (.get future)
            (catch ExecutionException error
              (throw (.getCause error)))))
        (.shutdown executor)
        (when-not (.awaitTermination executor 1 TimeUnit/MINUTES)
          (throw (ex-info "Analysis chunk workers did not stop"
                          {:chunk-parallelism chunk-parallelism})))
        (reduce
         (fn [results {:keys [stage state] :as worker}]
           (reset! active-stage (:id stage))
           (assoc results (:id stage)
                  {:worker worker
                   :result ((:close stage) state)}))
         {}
         workers)
        (catch Throwable error
          (.shutdownNow executor)
          (.awaitTermination executor 1 TimeUnit/MINUTES)
          (throw error))
        (finally
          (when-not (.isTerminated executor)
            (.shutdownNow executor)
            (.awaitTermination executor 1 TimeUnit/MINUTES)))))))

(defn- completed-stage
  [manifest stage configuration target-directory result]
  (let [id (:id stage)]
    (assoc manifest
           :progress nil
           :failure nil
           :stages
           (assoc (:stages manifest) id
                  {:id id
                   :version (:version stage)
                   :depends-on (:depends-on stage)
                   :dependency-signature (dependency-signature manifest stage)
                   :status :complete
                   :configuration configuration
                   :index-file (stage-output-file id)
                   :output-directory
                   (subs target-directory (inc (count (:capture-directory manifest))))
                   :result result
                   :completed-at (now)})
           :completed-stages
           (conj (vec (remove #{id} (:completed-stages manifest))) id))))

(defn run!
  "Run missing or stale named stages for a finalized capture.

  `:stage` is a stage ID such as `:structure`, `:video`, `:assets`, or the
  virtual target `:all` (the default). `:force?` reruns the selected target's
  dependency closure. `:chunk-parallelism` bounds concurrent chunk-local
  workers (default 1). `:config` maps stage IDs to configuration maps; a
  changed configuration invalidates that stage and its downstream dependents.

  Requested stages are broadcast over each raw chunk in dependency order, so
  an `:all` request parses every raw chunk once rather than once per stage.
  Each stage still writes to an isolated staging directory and is published
  only after every requested worker has completed successfully."
  ([capture-directory]
   (run! capture-directory {}))
  ([capture-directory {:keys [stage force?] :as options}]
   (let [capture-directory (.getCanonicalPath (io/file capture-directory))
         chunk-parallelism (chunk-parallelism options)
         capture-manifest (decoder/read-capture-manifest capture-directory)
         _ (require-finalized-capture! capture-manifest)
         target (target-stage-id stage)
         plan (stage-plan target)]
     (claim-run! capture-directory)
     (try
       (let [analysis-manifest (-> (read-analysis-manifest capture-directory
                                                           capture-manifest)
                                   (assoc :capture-directory capture-directory))
             {:keys [compatible to-run]}
             (plan-execution analysis-manifest plan options force?)
             invalidated (mapv :id to-run)
             running (begin-analysis! capture-directory capture-manifest
                                      analysis-manifest target)
             manifest-state (atom running)
             manifest-lock (Object.)
             active-stage (atom nil)
             progress! (fn [progress]
                         ;; A task completes stages out of order, but each
                         ;; durable progress rewrite must retain all per-stage
                         ;; counters rather than replacing progress with the
                         ;; latest callback.
                         (locking manifest-lock
                           (let [updated
                                 (swap! manifest-state
                                        update :progress
                                        (fn [current]
                                          (let [current (or current
                                                            {:chunks-total
                                                             (count (:chunks
                                                                     capture-manifest))
                                                             :stages {}
                                                             :active-stages
                                                             (set (map :id to-run))})
                                                stage (:stage progress)]
                                            (assoc current
                                                   :stages
                                                   (assoc-in (:stages current)
                                                             [stage]
                                                             (select-keys
                                                              progress
                                                              [:chunks-completed
                                                               :chunk-count
                                                               :current-chunk]))
                                                   :last-completed
                                                   {:stage stage
                                                    :chunk (:current-chunk progress)}))))]
                             (write-analysis-manifest! capture-directory updated)
                             (write-capture-analysis-pointer!
                              capture-directory capture-manifest updated))))
             workers (atom [])]
         (try
           (reset! workers
                   (initialize-stage-workers!
                    capture-directory capture-manifest options progress! to-run
                    active-stage))
           (let [results (run-broadcast-pipeline!
                          capture-directory capture-manifest
                          (:stages analysis-manifest) @workers active-stage
                          chunk-parallelism)
                 manifest
                 (reduce
                  (fn [manifest {:keys [id] :as stage}]
                    (let [{:keys [worker result]} (get results id)
                          {:keys [target-directory staging-directory]} worker
                          _ (reset! active-stage id)
                          _ (replace-directory! staging-directory target-directory)
                          completed (completed-stage
                                     manifest stage
                                     (stage-configuration options stage)
                                     target-directory result)]
                      (reset! manifest-state completed)
                      (write-analysis-manifest! capture-directory completed)
                      (write-capture-analysis-pointer!
                       capture-directory capture-manifest completed)
                      completed))
                  @manifest-state
                  to-run)
                 complete (assoc manifest
                                 :status :complete
                                 :completed-stages (completed-stage-ids manifest)
                                 :completed-at (now)
                                 :progress nil)
                 result {:status :complete
                         :requested-stage target
                         :executed-stages (mapv :id to-run)
                         :skipped-stages compatible
                         :invalidated-stages invalidated
                         :analysis-manifest "analysis/manifest.edn"}]
             (write-analysis-manifest! capture-directory complete)
             (write-capture-analysis-pointer! capture-directory capture-manifest complete)
             result)
           (catch Throwable error
             (doseq [{:keys [staging-directory]} @workers]
               (delete-tree! (io/file staging-directory)))
             (let [id (or @active-stage (:id (first to-run)))
                   stage (get stage-definitions id)
                   failed (assoc @manifest-state
                                 :status :failed
                                 :failure {:stage id
                                           :stage-id id
                                           :version (:version stage)
                                           :message (.getMessage error)
                                           :failed-at (now)})]
               (write-analysis-manifest! capture-directory failed)
               (write-capture-analysis-pointer!
                capture-directory capture-manifest failed))
             (throw error))))
       (finally
         (release-run! capture-directory))))))

(defn run-async!
  "Start `run!` on a background future and return the worker.

  Use `status` for durable progress and failure information. Concurrent runs
  for one capture are rejected by `run!`."
  ([capture-directory]
   (run-async! capture-directory {}))
  ([capture-directory options]
   (future (run! capture-directory options))))
