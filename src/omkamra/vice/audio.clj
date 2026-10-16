(ns omkamra.vice.audio
  "The exact `:audio` music-evidence projection.

  `:audio` scans exact SID writes through the shared capture reader and joins
  them with the `:dedupe` block/context mappings by global event index. It
  retains no SID write history or values, only compact evidence:

  * total SID-write count and density;
  * counts by execution context, block, instruction PC, and canonical register;
  * sustained activity over ordered logical units (the dedupe windows);
  * candidate `:music`-role evidence for sustained SID-writing handlers.

  Music evidence is not exclusive; it only contributes a `:music` role to later
  classification.

  Generation layout:

  ```text
  analysis/audio/<generation>/
    index.edn
  ```"
  (:require [clojure.java.io :as io]
            [omkamra.vice.analysis.core :as analysis]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.parallel :as parallel]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store]))

(def audio-version 1)
(def audio-format :omkamra.vice/audio-v2)

(def default-configuration
  {:min-sid-writes-per-unit 8
   :min-sustained-units 2
   :candidate-min-writes 64})

(defn- flatten-fragments
  "Flatten dedupe occurrences into a start-sorted fragment list."
  [occurrences]
  (->> occurrences
       (mapcat (fn [occurrence]
                 (map (fn [[start end]]
                        {:start start :end end
                         :block (:block-id occurrence)
                         :context (:context-id occurrence)})
                      (:fragments occurrence))))
       (sort-by :start)
       vec))

(defn- attribute-sid-writes
  "Attribute each SID write to the dedupe fragment covering its event index."
  [fragments sid-writes]
  (let [frags (vec fragments)
        cursor (volatile! 0)
        initial {:by-context {} :by-block {} :by-pc {} :attributed 0}]
    (reduce
     (fn [acc write]
       (let [event (:event-index write)]
         (loop []
           (let [index @cursor]
             (cond
               (>= index (count frags)) acc

               (< event (:start (nth frags index)))
               (do (vreset! cursor index) acc)

               (< event (:end (nth frags index)))
               (let [fragment (nth frags index)]
                 (vreset! cursor index)
                 (-> acc
                     (update-in [:by-context (:context fragment)] (fnil inc 0))
                     (update-in [:by-block (:block fragment)] (fnil inc 0))
                     (update-in [:by-pc (:pc write)] (fnil inc 0))
                     (update :attributed inc)))

               :else
               (do (vreset! cursor (inc index)) (recur)))))))
     initial
     sid-writes)))

(defn- merge-counts [left right]
  (merge-with + (or left {}) (or right {})))

(defn- window-evidence
  [reader trace window]
  (let [start (:start window)
        end (:end window)
        occurrences (:occurrences (dedupe/read-range trace start end))
        sid-writes (reader/scan-sid-writes reader [start end] conj [])
        fragments (flatten-fragments occurrences)
        attributed (attribute-sid-writes fragments sid-writes)
        registers (frequencies (map :register sid-writes))]
    {:start start
     :end end
     :sid-writes (count sid-writes)
     :attributed (:attributed attributed)
     :by-context (:by-context attributed)
     :by-block (:by-block attributed)
     :by-pc (:by-pc attributed)
     :by-register registers}))

(defn- sustained-runs
  [units]
  (let [sustained? #(>= (:sid-writes %) (:min-sid-writes-per-unit %))]
    (loop [remaining units
           current nil
           runs []]
      (if-let [unit (first remaining)]
        (if (sustained? unit)
          (recur (rest remaining)
                 (if current
                   (-> current
                       (update :event-range (fn [[start _]] [start (:end unit)]))
                       (update :unit-count inc)
                       (update :sid-writes + (:sid-writes unit)))
                   {:event-range [(:start unit) (:end unit)]
                    :unit-count 1
                    :sid-writes (:sid-writes unit)})
                 runs)
          (recur (rest remaining)
                 nil
                 (cond-> runs current (conj current))))
        (cond-> runs current (conj current))))))

(defn build!
  "Build and commit an `:audio` generation.

  `reader` is a `omkamra.vice.reader/open!` handle and `trace` is an opened
  `omkamra.vice.dedupe` image. Options: `:configuration` overriding
  `default-configuration`."
  ([reader trace] (build! reader trace {}))
  ([reader trace {:keys [configuration]
                  :or {configuration default-configuration}}]
   (let [configuration (merge default-configuration configuration)
         dataset-root (reader/dataset-root reader)
         capture-id (reader/capture-id reader)
         capture-sha (analysis/capture-manifest-sha (reader/directory reader))
         windows (:windows trace)
         parallelism (or (:parallelism configuration) parallel/default-parallelism)
         evidence (vec (mapcat (fn [batch]
                                 (parallel/pmap-ordered
                                  #(window-evidence reader trace %) batch parallelism))
                               (partition-all (max 1 (* 2 parallelism)) windows)))
         unit-count (count evidence)
         sustained (mapv #(assoc % :min-sid-writes-per-unit
                                 (:min-sid-writes-per-unit configuration))
                         evidence)
         runs (sustained-runs sustained)
         by-context (reduce (fn [acc unit] (merge-counts acc (:by-context unit)))
                            {} evidence)
         by-block (reduce (fn [acc unit] (merge-counts acc (:by-block unit)))
                          {} evidence)
         by-pc (reduce (fn [acc unit] (merge-counts acc (:by-pc unit)))
                       {} evidence)
         by-register (reduce (fn [acc unit] (merge-counts acc (:by-register unit)))
                             {} evidence)
         total (reduce + 0 (map :sid-writes evidence))
         block-unit-count (reduce (fn [acc unit]
                                    (reduce (fn [acc block]
                                              (update acc block (fnil inc 0)))
                                            acc (keys (:by-block unit))))
                                  {} evidence)
         context-unit-count (reduce (fn [acc unit]
                                      (reduce (fn [acc context]
                                                (update acc context (fnil inc 0)))
                                              acc (keys (:by-context unit))))
                                    {} evidence)
         candidate-min (:candidate-min-writes configuration)
         min-units (:min-sustained-units configuration)
         music-candidates
         (->> (concat
               (for [[block writes] by-block
                     :when (and (>= writes candidate-min)
                                (>= (get block-unit-count block 0) min-units))]
                 {:kind :block :block block :sid-writes writes
                  :units (get block-unit-count block 0)})
               (for [[context writes] by-context
                     :when (and (>= writes candidate-min)
                                (>= (get context-unit-count context 0) min-units))]
                 {:kind :context :context context :sid-writes writes
                  :units (get context-unit-count context 0)}))
              (sort-by (juxt :kind :sid-writes))
              vec)
         index {:format audio-format
                :version audio-version
                :capture-id capture-id
                :capture-manifest-sha256 capture-sha
                :configuration configuration
                :summary {:sid-write-count total
                          :unit-count unit-count
                          :sustained-unit-count
                          (count (filter #(>= (:sid-writes %)
                                              (:min-sid-writes-per-unit configuration))
                                         evidence))
                          :sustained-run-count (count runs)
                          :candidate-count (count music-candidates)}
                :sustained-runs runs
                :music-candidates music-candidates
                :by-context by-context
                :by-block by-block
                :by-pc by-pc
                :by-register by-register
                :units (mapv #(select-keys % [:start :end :sid-writes :attributed])
                             evidence)}
         {:keys [generation directory relative]}
         (analysis/new-generation! dataset-root :audio)]
     (try
       (store/atomic-write-edn! (io/file directory "index.edn") index)
       (analysis/commit-stage!
        dataset-root :audio
        {:generation generation
         :directory relative
         :version audio-version
         :capture-id capture-id
         :capture-manifest-sha256 capture-sha
         :configuration configuration
         :summary (:summary index)})
       {:dataset-root dataset-root
        :directory (str directory)
        :index index}
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open!
  "Open the committed `:audio` generation for a dataset root."
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :audio)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :audio generation"
                      {:reason :missing-audio-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= audio-format (:format index))
        (throw (ex-info "Not a v2 audio image"
                        {:reason :not-an-audio-image :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index})))

(defn status
  "Return the committed audio stage entry for a dataset, or nil."
  [dataset-root]
  (analysis/stage-entry dataset-root :audio))

(defn evidence
  "Return the compact music evidence."
  [audio]
  (:index audio))

(defn music-candidates
  [audio]
  (:music-candidates (:index audio)))

(defn music-blocks
  "Block ids the audio stage flagged as music candidates."
  [audio]
  (into #{}
        (comp (filter #(= :block (:kind %))) (keep :block))
        (music-candidates audio)))

(defn music-contexts
  "Context ids the audio stage flagged as music candidates."
  [audio]
  (into #{}
        (comp (filter #(= :context (:kind %))) (keep :context))
        (music-candidates audio)))
