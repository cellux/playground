(ns omkamra.vice.analysis.runner
  "Analysis manifest orchestration and dependency-ordered stage execution."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [omkamra.vice.analysis.common :as common]
            [omkamra.vice.analysis.stages :as stages]
            [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.video :as video])
  (:import [java.util.concurrent Callable ExecutionException Executors TimeUnit]))

(def ^:private stage-registry
  [{:id :writes
    :version "writes-v2"
    :depends-on []
    :init stages/init-chunk-stage!
    :step stages/writes-stage-step
    :close stages/close-chunk-stage!}
   {:id :structure
    :version "structure-v3"
    :depends-on []
    :init stages/init-chunk-stage!
    :step stages/structure-stage-step
    :close stages/close-chunk-stage!}
   {:id :video
    :version "video-v1"
    :depends-on [:writes]
    :init stages/init-chunk-stage!
    :step stages/video-stage-step
    :close stages/close-chunk-stage!}
   {:id :assets
    :version "assets-v1"
    :depends-on [:writes :video]
    :init stages/init-chunk-stage!
    :step stages/assets-stage-step
    :close stages/close-chunk-stage!}
   {:id :features
    :version "features-v15"
    :depends-on [:writes :structure :video]
    :init stages/init-features-stage!
    :step stages/features-stage-step
    :close stages/close-features-stage!}
   {:id :classification
    :version "classification-v19"
    :depends-on [:features]
    :init stages/init-chunk-stage!
    :step stages/classification-stage-step
    :close stages/close-classification-stage!}
   {:id :segments
    :version "segments-v21"
    :depends-on [:classification]
    :init stages/init-chunk-stage!
    :step stages/segments-stage-step
    :close stages/close-segments-stage!}
   {:id :disassembly
    :version "disassembly-v12"
    :depends-on [:segments]
    :init stages/init-chunk-stage!
    :step stages/disassembly-stage-step
    :close stages/close-disassembly-stage!}])

(def ^:private stage-definitions
  (into {} (map (juxt :id identity) stage-registry)))

(def ^:private all-stage-id :all)

(defn stage-dependencies
  [stage-id]
  (if (= all-stage-id stage-id)
    (mapv :id stage-registry)
    (if-let [stage (get stage-definitions stage-id)]
      (:depends-on stage)
      (throw (ex-info "Unknown analysis stage"
                      {:stage stage-id
                       :available (conj (mapv :id stage-registry)
                                        all-stage-id)})))))

(defn stage-definition
  [stage-id]
  (or (get stage-definitions stage-id)
      (throw (ex-info "Virtual analysis target is not executable"
                      {:stage stage-id}))))

(defn stage-plan
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
  (let [manifest (artifact/read-capture-manifest capture-directory)]
    (map (fn [{:keys [number]}]
           (artifact/read-chunk capture-directory number))
         (:chunks manifest))))

(defn analysis-manifest-file
  [capture-directory]
  (common/file-path capture-directory "analysis" "manifest.edn"))

(defn new-analysis-manifest
  [capture-directory capture-manifest]
  {:format common/analysis-format
   :capture-id (:capture-id capture-manifest)
   :capture-directory capture-directory
   :source-format (:format capture-manifest)
   :status :not-run
   :requested-stage nil
   :completed-stages []
   :stages {}
   :manifest-file "analysis/manifest.edn"})

(defn normalize-analysis-manifest
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

(defn read-analysis-manifest
  [capture-directory capture-manifest]
  (let [file (io/file (analysis-manifest-file capture-directory))]
    (normalize-analysis-manifest
     (if (.isFile file)
       (edn/read-string (slurp file))
       (new-analysis-manifest capture-directory capture-manifest)))))

(defn write-analysis-manifest!
  [capture-directory manifest]
  (common/atomic-write-edn! (analysis-manifest-file capture-directory) manifest))

(defn analysis-pointer
  [analysis-manifest]
  {:status (:status analysis-manifest)
   :requested-stage (:requested-stage analysis-manifest)
   :completed-stages (:completed-stages analysis-manifest)
   :stages (:stages analysis-manifest)
   :manifest-file "analysis/manifest.edn"})

(defn write-capture-analysis-pointer!
  [capture-directory capture-manifest analysis-manifest]
  (common/atomic-write-edn!
   (common/file-path capture-directory "manifest.edn")
   (assoc capture-manifest :analysis (analysis-pointer analysis-manifest))))

(defn require-finalized-capture!
  [capture-manifest]
  (when-not (and (= :stopped (:status capture-manifest))
                 (:finalized? capture-manifest))
    (throw (ex-info "Analysis requires a finalized stopped capture"
                    {:status (:status capture-manifest)
                     :finalized? (:finalized? capture-manifest)}))))

(defn chunk-parallelism
  [options]
  (let [parallelism (or (:chunk-parallelism options)
                        common/default-chunk-parallelism)]
    (when-not (pos-int? parallelism)
      (throw (ex-info ":chunk-parallelism must be a positive integer"
                      {:chunk-parallelism parallelism})))
    parallelism))

(defn target-stage-id
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

(defn dependency-signature
  [analysis-manifest stage]
  (into {}
        (map (fn [dependency]
               [dependency
                (select-keys (get-in analysis-manifest
                                     [:stages dependency])
                             [:id :version :configuration])]))
        (:depends-on stage)))

(defn stage-compatible?
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

(defn stage-order
  [stage-id]
  (or (some->> stage-registry
               (map-indexed vector)
               (some (fn [[index stage]]
                       (when (= stage-id (:id stage)) index))))
      Integer/MAX_VALUE))

(defn completed-stage-ids
  [analysis-manifest]
  (->> (:stages analysis-manifest)
       (keep (fn [[stage-id stage]]
               (when (= :complete (:status stage)) stage-id)))
       (sort-by stage-order)
       vec))

(defn analysis-progress
  [analysis-manifest]
  (let [progress (:progress analysis-manifest)]
    (when (and progress (:stages progress))
      progress)))

(defn status
  "Return capture and analysis status without reading any chunk artifact."
  [capture-directory]
  (let [capture-manifest (artifact/read-capture-manifest capture-directory)
        analysis-manifest (read-analysis-manifest capture-directory capture-manifest)]
    {:capture-status (:status capture-manifest)
     :analysis-status (:status analysis-manifest)
     :analysis-requested-stage (:requested-stage analysis-manifest)
     :analysis-progress (analysis-progress analysis-manifest)
     :analysis-failure (:failure analysis-manifest)
     :feature-diagnostics (get-in analysis-manifest [:stages :features :result :summary])
     :classification-diagnostics
     (get-in analysis-manifest [:stages :classification :result :summary])
     :segment-diagnostics
     (get-in analysis-manifest [:stages :segments :result :summary])
     :disassembly-diagnostics
     (get-in analysis-manifest [:stages :disassembly :result :summary])
     :analysis-stages (->> (:stages analysis-manifest)
                           (sort-by (comp stage-order key))
                           (mapv val))
     :chunk-count (count (:chunks capture-manifest))
     :event-count (:event-count capture-manifest)}))

(defn begin-analysis!
  [capture-directory capture-manifest analysis-manifest target]
  (let [running (-> analysis-manifest
                    (assoc :status :running
                           :requested-stage target
                           :started-at (common/now)
                           :progress nil)
                    (dissoc :failure))]
    (write-analysis-manifest! capture-directory running)
    (write-capture-analysis-pointer! capture-directory capture-manifest running)
    running))

(defn claim-run!
  [capture-directory]
  (locking common/active-runs
    (when (contains? @common/active-runs capture-directory)
      (throw (ex-info "Analysis is already running for this capture"
                      {:capture-directory capture-directory})))
    (swap! common/active-runs conj capture-directory)))

(defn release-run!
  [capture-directory]
  (swap! common/active-runs disj capture-directory))

(defn plan-execution
  [analysis-manifest plan options force?]
  (loop [remaining plan
         compatible []
         compatible-ids #{}
         to-run []]
    (if-let [stage (first remaining)]
      (let [configuration (common/stage-configuration options stage)
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

(defn stage-implementation!
  [stage]
  (doseq [operation [:init :step :close]]
    (when-not (fn? (get stage operation))
      (throw (ex-info "Analysis stage has no reducer operation"
                      {:stage (dissoc stage :init :step :close)
                       :operation operation}))))
  stage)

(defn initialize-stage-worker!
  [capture-directory capture-manifest completed-stages options progress! stage]
  (let [stage (stage-implementation! stage)
        id (:id stage)
        target-directory (common/stage-directory capture-directory id)
        staging-directory (str target-directory ".partial")
        _ (common/delete-tree! (io/file staging-directory))
        context {:capture-directory capture-directory
                 :output-directory staging-directory
                 :capture-manifest capture-manifest
                 :completed-stages completed-stages
                 :configuration (common/stage-configuration options stage)
                 :stage (dissoc stage :init :step :close)
                 :progress! progress!}]
    {:stage stage
     :target-directory target-directory
     :staging-directory staging-directory
     :state ((:init stage) context)}))

(defn initialize-stage-workers!
  [capture-directory capture-manifest completed-stages options progress! stages active-stage]
  (loop [workers []
         [stage & remaining] stages]
    (if-not stage
      workers
      (let [id (:id stage)]
        (reset! active-stage id)
        (let [worker
              (try
                (initialize-stage-worker! capture-directory capture-manifest
                                          completed-stages options progress! stage)
                (catch Throwable error
                  (doseq [{:keys [staging-directory]} workers]
                    (common/delete-tree! (io/file staging-directory)))
                  ;; `:init` can fail after creating its own directory, before
                  ;; a worker record exists to identify it.
                  (common/delete-tree! (io/file (str (common/stage-directory capture-directory id)
                                                     ".partial")))
                  (throw error)))]
          (recur (conj workers worker) remaining))))))

(defn attach-active-stage-directories
  [workers]
  (let [directories (into {}
                          (map (juxt (comp :id :stage) :staging-directory)
                               workers))]
    (mapv #(update % :state assoc-in [:context :active-stage-directories]
                   directories)
          workers)))

(defn materialize-dependency
  [raw-chunk dependency output]
  (if (= dependency :writes)
    (assoc output ::video/write-source
           (video/writes-source raw-chunk output))
    output))

(defn dependency-output
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
    (let [output (->> (common/read-stage-chunk capture-directory completed-stages
                                               dependency chunk-number)
                      (materialize-dependency raw-chunk dependency))]
      [output (assoc dependency-cache dependency output)])))

(defn run-broadcast-chunk!
  [capture-directory completed-stages workers active-stage chunk-number]
  (let [raw-chunk (artifact/read-chunk capture-directory chunk-number)]
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

(defn run-broadcast-pipeline!
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

(defn completed-stage
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
                   :index-file (common/stage-output-file id)
                   :output-directory
                   (subs target-directory (inc (count (:capture-directory manifest))))
                   :result result
                   :completed-at (common/now)})
           :completed-stages
           (conj (vec (remove #{id} (:completed-stages manifest))) id))))

(defn run!
  "Run missing or stale named stages for a finalized capture.

  `:stage` is a stage ID such as `:structure`, `:video`, `:assets`,
  `:segments`, or `:disassembly`, or the virtual target `:all` (the default). `:force?` reruns the selected target's
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
         capture-manifest (artifact/read-capture-manifest capture-directory)
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
                                                               :current-chunk
                                                               :phase
                                                               :segments-completed
                                                               :segment-count]))
                                                   :last-completed
                                                   {:stage stage
                                                    :chunk (:current-chunk progress)}))))]
                             (write-analysis-manifest! capture-directory updated)
                             (write-capture-analysis-pointer!
                              capture-directory capture-manifest updated))))
             workers (atom [])]
         (try
           (reset! workers
                   (attach-active-stage-directories
                    (initialize-stage-workers!
                     capture-directory capture-manifest (:stages analysis-manifest)
                     options progress! to-run active-stage)))
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
                          _ (common/replace-directory! staging-directory target-directory)
                          completed (completed-stage
                                     manifest stage
                                     (common/stage-configuration options stage)
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
                                 :completed-at (common/now)
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
               (common/delete-tree! (io/file staging-directory)))
             (let [id (or @active-stage (:id (first to-run)))
                   stage (get stage-definitions id)
                   failed (assoc @manifest-state
                                 :status :failed
                                 :failure {:stage id
                                           :stage-id id
                                           :version (:version stage)
                                           :message (.getMessage error)
                                           :failed-at (common/now)})]
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
