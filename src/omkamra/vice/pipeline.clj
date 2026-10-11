(ns omkamra.vice.pipeline
  "Dependency-graph analysis driver with stage reuse.

  A caller asks for one target stage; the driver discovers its dependency
  closure from a static graph, builds any stage whose inputs changed, and
  reuses any stage whose committed generation already matches. Callers never
  open or pass upstream images by hand.

  Reuse is signature based. A stage's signature folds together its schema
  version, its configuration, the capture manifest checksum, and the
  signatures of its upstream stages. The driver records that signature in the
  analysis manifest; when the recorded signature still matches, the existing
  generation is opened instead of rebuilt. Upstream signatures flow forward, so
  rebuilding one stage automatically invalidates everything downstream of it,
  while unrelated stages stay reusable."
  (:require [omkamra.vice.analysis :as analysis]
            [omkamra.vice.assets :as assets]
            [omkamra.vice.audio :as audio]
            [omkamra.vice.classification :as classification]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.disassembly :as disassembly]
            [omkamra.vice.features :as features]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.segments :as segments]
            [omkamra.vice.video :as video])
  (:import [java.security MessageDigest]))

(def dependencies
  "Static stage dependency graph. Capture is implicit for every stage."
  {:dedupe #{}
   :video #{}
   :assets #{:video}
   :audio #{:dedupe}
   :features #{:dedupe}
   :classification #{:features :audio}
   :segments #{:features :classification}
   :disassembly #{:dedupe :segments}})

(def versions
  {:dedupe dedupe/dedupe-version
   :video video/video-version
   :assets assets/assets-version
   :audio audio/audio-version
   :features features/features-version
   :classification classification/classification-version
   :segments segments/segments-version
   :disassembly disassembly/disassembly-version})

(def stage-defaults
  "Semantic default configuration per stage, where the stage defines one."
  {:video video/default-configuration
   :assets assets/default-configuration
   :audio audio/default-configuration
   :features features/default-configuration
   :classification classification/default-configuration})

(def configuration-keys
  "Configuration keys that participate in a stage's signature.

  Any other key is ignored, so operational knobs which do not change a
  generation's content (worker counts, batch sizes) neither force a rebuild nor
  prevent reuse."
  {:dedupe #{:max-window-occurrences :leveling-threshold-events}
   :video (set (keys video/default-configuration))
   :assets (set (keys assets/default-configuration))
   :audio (set (keys audio/default-configuration))
   :features (set (keys features/default-configuration))
   :classification (set (keys classification/default-configuration))
   :segments #{}
   :disassembly (set (keys disassembly/default-configuration))})

(defn normalize-configuration
  "Reduce a stage configuration to the canonical map used for signatures.

  Defaults are folded in and only signature-relevant keys are kept, so passing
  an explicit default is indistinguishable from passing nothing."
  [stage configuration]
  (let [defaults (get stage-defaults stage {})]
    (into (sorted-map)
          (select-keys (merge defaults configuration)
                       (get configuration-keys stage #{})))))

(defn- sha256-hex
  [^String value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes value "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn signature
  "Deterministic reuse signature for a stage build.

  `upstream` maps each direct dependency to its own signature."
  [stage configuration capture-sha upstream]
  (sha256-hex (pr-str {:stage stage
                       :version (get versions stage)
                       :configuration configuration
                       :capture-sha capture-sha
                       :upstream (into (sorted-map) upstream)})))

(defn topo-order
  "Dependency-first order for `stage` and its transitive dependencies."
  [stage]
  (let [seen (volatile! #{})
        out (volatile! [])]
    (letfn [(visit [s]
              (when-not (contains? @seen s)
                (vswap! seen conj s)
                (doseq [dependency (sort (get dependencies s))]
                  (visit dependency))
                (vswap! out conj s)))]
      (visit stage))
    @out))

;; Stage adapters ------------------------------------------------------------

(defn- open-stage!
  [stage dataset-root]
  (case stage
    :dedupe (dedupe/open dataset-root)
    :video (video/open dataset-root)
    :assets (assets/open dataset-root)
    :audio (audio/open dataset-root)
    :features (features/open dataset-root)
    :classification (classification/open dataset-root)
    :segments (segments/open dataset-root)
    :disassembly (disassembly/open dataset-root)))

(defn- build-stage!
  [stage {:keys [capture] :as context} configuration]
  (let [images @(:images context)]
    (case stage
      :dedupe (dedupe/build! capture {:configuration configuration})
      :video (video/build! capture {:configuration configuration})
      :assets (assets/build! capture (:video images)
                             {:configuration configuration})
      :audio (audio/build! capture (:dedupe images)
                           {:configuration configuration})
      :features (features/build! capture (:dedupe images)
                                 {:configuration configuration})
      :classification (classification/build! (:features images) (:audio images)
                                             {:configuration configuration})
      :segments (segments/build! (:classification images) (:features images)
                                 {:configuration configuration})
      :disassembly (disassembly/build! (:dedupe images) (:segments images)
                                       {:configuration configuration}))))

;; Driver --------------------------------------------------------------------

(defn- stage-configuration
  [options stage]
  (get-in options [:configuration stage]))

(defn- stage-provenance
  "The generation name currently committed (or staged) for each dependency.

  Recorded on a stage's entry so reuse also depends on the *inputs actually
  consumed*, not only on their signatures: a defective rebuild that keeps its
  signature but produces a new generation changes its own generation name and
  therefore invalidates any downstream entry that recorded the old one."
  [context stage]
  (let [manifest (analysis/read-manifest (:dataset-root context))]
    (into (sorted-map)
          (map (fn [dependency]
                 [dependency (get-in manifest [:stages dependency :generation])]))
          (get dependencies stage))))

(defn reuse-decision
  "Whether a committed stage entry can be reused.

  Returns `:reuse` when version, signature, and recorded input provenance all
  match; `:backfill` when the entry predates provenance recording (it is
  reused once and its provenance recorded); and `:rebuild` otherwise — a
  missing entry, a version or signature change, a forced build, or provenance
  naming different upstream generations (which catches a defective upstream
  rebuild that kept its signature but produced a new generation)."
  [entry expected-version expected-signature provenance forced?]
  (cond
    (nil? entry) :rebuild
    (not= (:version entry) expected-version) :rebuild
    (not= (:signature entry) expected-signature) :rebuild
    forced? :rebuild
    (nil? (:provenance entry)) :backfill
    (= (:provenance entry) provenance) :reuse
    :else :rebuild))

(defn- ensure-stage!
  [context stage]
  (let [dataset-root (:dataset-root context)
        capture-sha (:capture-sha context)
        configuration (normalize-configuration
                       stage (stage-configuration (:options context) stage))
        upstream (into {}
                       (map (fn [dependency]
                              [dependency (get @(:signatures context) dependency)]))
                       (get dependencies stage))
        expected (signature stage configuration capture-sha upstream)
        provenance (stage-provenance context stage)
        entry (analysis/stage-entry dataset-root stage)
        forced? (contains? (:force context) stage)
        decision (reuse-decision entry (get versions stage) expected
                                 provenance forced?)]
    (case decision
      :reuse
      (swap! (:reused context) conj stage)

      :backfill
      (do (swap! (:reused context) conj stage)
          (analysis/commit-stage! dataset-root stage
                                  (assoc entry :provenance provenance)))

      :rebuild
      (let [result (build-stage! stage context configuration)
            committed (analysis/stage-entry dataset-root stage)]
        (when-let [directory (:directory result)]
          (swap! (:created context) conj directory))
        (analysis/commit-stage! dataset-root stage
                                (assoc committed
                                       :signature expected
                                       :provenance provenance))
        (swap! (:built context) conj stage)
        (swap! (:results context) assoc stage result)))
    (swap! (:signatures context) assoc stage expected)
    (swap! (:images context) assoc stage (open-stage! stage dataset-root))
    expected))

(defn normalize-targets
  "Normalize a build request into a set of known stage ids.

  Accepts a single stage keyword, a collection of stage keywords, or `:all`."
  [stage]
  (let [targets (cond
                  (= stage :all) (set (keys dependencies))
                  (keyword? stage) #{stage}
                  (sequential? stage) (set stage)
                  :else (throw (ex-info (str "Analysis target must be a stage, a "
                                             "collection of stages, or :all")
                                        {:reason :invalid-analysis-target
                                         :target stage})))]
    (doseq [target targets]
      (when-not (contains? dependencies target)
        (throw (ex-info "Unknown analysis stage"
                        {:reason :unknown-stage :stage target}))))
    targets))

(defn closure
  "Dependency-first order for a set of target stages and their dependencies."
  [targets]
  (vec (distinct (mapcat topo-order (sort targets)))))

(def ^:private downstream-count
  "Number of stages that transitively depend on each stage.

  Used only as a scheduling priority, so the critical path is scheduled into an
  early batch and is not delayed behind an independent branch."
  (into {}
        (map (fn [stage]
               [stage (count (filter #(some #{stage} (topo-order %))
                                     (keys dependencies)))]))
        (keys dependencies)))

(defn- ready?
  "True when every dependency of `stage` has already been built."
  [remaining stage]
  (every? #(not (contains? remaining %)) (get dependencies stage)))

(defn- run-batch!
  "Build `batch` concurrently and rethrow the first failure."
  [context batch]
  (let [futures (mapv (fn [stage] [stage (future (ensure-stage! context stage))])
                      batch)
        failures (reduce (fn [acc [stage worker]]
                           (try (deref worker) acc
                                (catch Throwable error
                                  (conj acc [stage error]))))
                         [] futures)]
    (when (seq failures)
      (throw (ex-info "Parallel stage build failed"
                      {:reason :stage-build-failure
                       :failures (mapv first failures)}
                      (second (first failures)))))))

(defn- run-frontier!
  [context frontier parallelism]
  (doseq [batch (partition-all parallelism frontier)]
    (run-batch! context batch)))

(defn default-stage-parallelism
  "Default number of stages built concurrently: the available processors.

  Frontiers are small (at most three stages), so this lets every independent
  branch run at once. It can be lowered with `:stage-parallelism` on a
  memory-constrained host or when stages are themselves internally parallel."
  []
  (max 1 (.availableProcessors (Runtime/getRuntime))))

(defn build!
  "Ensure `stage` and its dependencies are up to date and return their images.

  `dataset-root` is a finalized v2 capture dataset. `stage` is a stage
  keyword, a collection of stage keywords, or `:all`. Independent branches of
  the dependency closure are built concurrently, in dependency-first frontier
  order, bounded by `:stage-parallelism` (default: the available processors);
  read-only stages that are already up to date are reused. Options:

  * `:configuration` — map of stage id to that stage's configuration;
  * `:force` — collection of stages to rebuild even when their signature
    matches;
  * `:reader` — an already opened capture reader to reuse;
  * `:stage-parallelism` — maximum number of stages built concurrently.

  Returns `{:images {stage image} :signatures {stage signature}
  :built [...] :reused [...]}`, with `:built`/`:reused` in dependency-first
  order. Every stage image is opened, so a caller can inspect the closure
  without opening anything itself."
  [dataset-root stage & [options]]
  (let [targets (normalize-targets stage)
        options (or options {})
        parallelism (or (:stage-parallelism options) (default-stage-parallelism))]
    (when-not (pos-int? parallelism)
      (throw (ex-info "Analysis :stage-parallelism must be a positive integer"
                      {:reason :invalid-stage-parallelism
                       :stage-parallelism parallelism})))
    (let [capture (or (:reader options) (reader/open dataset-root))
          transaction (atom {})
          context {:dataset-root dataset-root
                   :capture capture
                   :capture-sha (analysis/capture-manifest-sha
                                 (reader/directory capture))
                   :options options
                   :force (set (:force options))
                   :images (atom {})
                   :signatures (atom {})
                   :built (atom [])
                   :reused (atom [])
                   :results (atom {})
                   :created (atom [])}
          order (closure targets)
          position (into {} (map-indexed (fn [index s] [s index]) order))]
      ;; Bind one transaction for the whole invocation: stage commits are
      ;; staged and become visible in a single atomic rewrite only after every
      ;; stage has succeeded. On failure nothing is published and the
      ;; generations this invocation created are removed.
      (binding [analysis/*transaction* transaction]
        (try
          (let [result
                (loop [remaining (set order)]
                  (if (empty? remaining)
                    {:images @(:images context)
                     :signatures @(:signatures context)
                     :built (vec (sort-by position @(:built context)))
                     :reused (vec (sort-by position @(:reused context)))}
                    (let [frontier (->> remaining
                                        (filter #(ready? remaining %))
                                        (sort-by (fn [s] [(- (long (downstream-count s)))
                                                          (position s)]))
                                        vec)]
                      (when (empty? frontier)
                        (throw (ex-info "Analysis stage dependency cycle"
                                        {:reason :stage-cycle :remaining remaining})))
                      (run-frontier! context frontier parallelism)
                      (recur (reduce disj remaining frontier)))))]
            (analysis/publish! dataset-root transaction)
            result)
          (catch Throwable error
            (doseq [directory @(:created context)]
              (when directory (analysis/delete-generation! directory)))
            (throw error)))))))

(defn image
  "Convenience accessor for a stage image from a `build!` result."
  [result stage]
  (get-in result [:images stage]))

(def all-stages
  "Every stage in dependency-first order."
  (vec (distinct (mapcat topo-order (sort (keys dependencies))))))

(defn adopt!
  "Record reuse signatures for already-committed generations.

  Generations created before signatures existed cannot be reused until their
  signature is recorded. `adopt!` computes each selected stage's signature from
  its *recorded* configuration and upstream signatures, in dependency order,
  and writes it into the manifest entry.

  Adoption trusts that the selected generations were built from their current
  upstream generations. Select only stages known to be consistent; a stale
  downstream generation must be rebuilt rather than adopted. Options: `:only`
  (a set of stages to adopt; defaults to all). Returns the signature map."
  [dataset-root & [{:keys [only]}]]
  (let [capture (reader/open dataset-root)
        capture-sha (analysis/capture-manifest-sha (reader/directory capture))
        signatures (atom {})]
    (doseq [stage all-stages
            :when (and (or (nil? only) (contains? only stage))
                       (analysis/stage-entry dataset-root stage))]
      (let [entry (analysis/stage-entry dataset-root stage)
            configuration (normalize-configuration stage (:configuration entry))
            upstream (into {}
                           (map (fn [dependency]
                                  [dependency (get @signatures dependency)]))
                           (get dependencies stage))
            signature (signature stage configuration capture-sha upstream)
            provenance (into (sorted-map)
                             (map (fn [dependency]
                                    [dependency (get-in (analysis/read-manifest dataset-root)
                                                        [:stages dependency :generation])]))
                             (get dependencies stage))]
        (analysis/commit-stage! dataset-root stage
                                (assoc entry
                                       :signature signature
                                       :provenance provenance))
        (swap! signatures assoc stage signature)))
    @signatures))
