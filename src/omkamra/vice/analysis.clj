(ns omkamra.vice.analysis
  "High-level inspection API for finalized VICE/C64 analysis datasets.

  This namespace is the analysis layer of the public `omkamra.vice` facade.
  `build-pipeline!` explicitly creates or reuses committed generations;
  `open-analysis` only opens the finalized capture and generations already
  committed in its analysis manifest. `list-segments` and `get-segment`
  provide compact execution overviews and references for drilling into raw
  events, structural execution, disassembly, and screenshots.

  Manifest/generation persistence used by the pipeline and stage namespaces
  lives in `omkamra.vice.analysis.core`. Detailed data access remains in
  `omkamra.vice.reader`, `dedupe`, `segments`, `disassembly`, `report`, and
  the other stage namespaces."
  (:require
   [clojure.set :as set]
   [omkamra.vice.analysis.core :as core]
   [omkamra.vice.assets :as assets]
   [omkamra.vice.audio :as audio]
   [omkamra.vice.classification :as classification]
   [omkamra.vice.dedupe :as dedupe]
   [omkamra.vice.disassembly :as disassembly]
   [omkamra.vice.features :as features]
   [omkamra.vice.pipeline :as pipeline]
   [omkamra.vice.reader :as reader]
   [omkamra.vice.report :as report]
   [omkamra.vice.segments :as segments]
   [omkamra.vice.video :as video]))

(def ^:private stage-openers
  {:dedupe dedupe/open!
   :video video/open!
   :assets assets/open!
   :audio audio/open!
   :features features/open!
   :classification classification/open!
   :segments segments/open!
   :disassembly disassembly/open!
   :report report/open!})

(def ^:private stage-keys
  [:dedupe :video :assets :audio :features :classification :segments
   :disassembly :report])

(defn build-pipeline!
  "Build or reuse analysis stages for a finalized capture dataset.

  `stage` is one stage keyword, a collection of stage keywords, or `:all`.
  Dependencies are built first, independent stages may run concurrently, and
  matching committed generations are reused. The result contains opened stage
  images plus `:built` and `:reused` stage lists.

  This is an explicit build step; `open-analysis` never builds missing stages.
  See `omkamra.vice.pipeline/build!` for stage configuration, forcing,
  parallelism, and the dependency graph."
  ([dataset-root stage]
   (pipeline/build! dataset-root stage))
  ([dataset-root stage options]
   (pipeline/build! dataset-root stage options)))

(defn- stage-summary
  [entry]
  (select-keys entry [:generation :version :configuration :summary]))

(defn- overview
  [capture manifest]
  (let [stages (or (:stages manifest) {})
        available (set (keys (select-keys stages stage-keys)))]
    {:capture-id (reader/capture-id capture)
     :event-count (reader/event-count capture)
     :chunk-count (reader/chunk-count capture)
     :available-stages available
     :missing-stages (set/difference (set stage-keys) available)
     :stage-summaries (into (sorted-map)
                            (map (fn [stage]
                                   [stage (stage-summary (get stages stage))]))
                            available)}))

(defn open-analysis
  "Open a finalized dataset and its already-committed analysis stages.

  This function never builds stages. It returns a map with this shape:

  ```clojure
  {:dataset-root ...
   :capture capture-reader
   :overview {:capture-id ... :event-count ... :chunk-count ...
              :available-stages #{...} :missing-stages #{...}
              :stage-summaries {...}}
   :dedupe ... :video ... :assets ... :audio ... :features ...
   :classification ... :segments ... :disassembly ... :report ...}
  ```

  Stage keys are present with nil values when their generation is absent. The
  raw capture is always validated and opened with `reader/open!`; a manifest
  entry for a stage is opened with that stage's validating `open!` function.
  Missing optional stages are therefore reported in `:overview`, while a
  committed corrupt or incompatible stage raises its validation error instead
  of being silently treated as missing.

  Start with `(list-segments analysis)` and `(get-segment analysis id)`.
  Their results contain event ranges and file/screenshot references; use the
  returned `:capture` and stage images with the lower-level APIs documented in
  `reader`, `dedupe`, `disassembly`, and `report` for full content."
  [dataset-root]
  (let [capture (reader/open! dataset-root)
        manifest (core/read-manifest (reader/dataset-root capture))
        stages (or (:stages manifest) {})
        opened (into {}
                     (map (fn [stage]
                            [stage (when (contains? stages stage)
                                     ((get stage-openers stage)
                                      (reader/dataset-root capture)))])
                          stage-keys))]
    (merge {:dataset-root (reader/dataset-root capture)
            :capture capture
            :overview (overview capture manifest)}
           opened)))

(defn- missing-stage!
  [analysis-result stage operation]
  (throw (ex-info (str operation " requires committed :" (name stage)
                       " analysis stage")
                  {:reason :missing-analysis-stage
                   :stage stage
                   :dataset-root (:dataset-root analysis-result)
                   :operation operation
                   :hint (str "Run (build-pipeline! dataset-root :" (name stage)
                              ") first.")})))

(defn- stage!
  [analysis-result stage operation]
  (or (get analysis-result stage)
      (missing-stage! analysis-result stage operation)))

(defn- report-segment
  [report-image segment-id]
  (some #(when (= segment-id (:segment-id %)) %)
        (report/segments report-image)))

(defn- disassembly-segment
  [disassembly-image segment-id]
  (some #(when (= segment-id (:segment-id %)) %)
        (disassembly/segments disassembly-image)))

(defn- function-ref
  [namespace functions]
  {:namespace namespace
   :functions (mapv symbol functions)})

(defn- segment-references
  [analysis-result segment]
  (let [segment-id (:segment-id segment)
        event-range (:event-range segment)
        disassembly-image (:disassembly analysis-result)
        report-image (:report analysis-result)
        disassembly-entry (when disassembly-image
                            (disassembly-segment disassembly-image segment-id))
        report-entry (when report-image
                       (report-segment report-image segment-id))]
    {:raw-capture {:handle-key :capture
                   :event-range event-range
                   :functions (function-ref
                               'omkamra.vice.reader
                               ['read-events 'read-writes 'replay-state
                                'boundary-snapshots 'chunk-screenshots])}
     :dedupe (if (:dedupe analysis-result)
               {:handle-key :dedupe
                :functions (function-ref
                            'omkamra.vice.dedupe
                            ['read-range 'read-block 'read-context])}
               {:available? false
                :reason :missing-stage
                :stage :dedupe})
     :disassembly (if disassembly-entry
                    {:handle-key :disassembly
                     :segment-id segment-id
                     :asm-file (:asm-file disassembly-entry)
                     :clj-file (:clj-file disassembly-entry)
                     :functions (function-ref
                                 'omkamra.vice.disassembly
                                 ['read-file 'segments])}
                    {:available? false
                     :reason :missing-stage-or-segment
                     :stage :disassembly})
     :screenshots (if report-entry
                    {:handle-key :report
                     :screenshots
                     (mapv #(select-keys % [:chunk-number :event-index :timing
                                            :target :fallback? :file])
                           (report/screenshots report-entry))
                     :functions (function-ref
                                 'omkamra.vice.report
                                 ['screenshots 'html-file 'html])}
                    {:available? false
                     :reason :missing-stage-or-segment
                     :stage :report})
     :segments {:handle-key :segments
                :functions (function-ref
                            'omkamra.vice.segments
                            ['file-groups 'audit])}}))

(defn- segment-overview
  [analysis-result segment]
  (assoc (select-keys segment [:segment-id :kind :roles :irq-roles :signals
                               :confidence :event-range :event-count :phases
                               :handler-contexts :context-ids])
         :references (segment-references analysis-result segment)))

(defn list-segments
  "Return compact overviews of the semantic execution segments.

  `analysis-result` is the map returned by `open-analysis`. Each overview
  contains segment identity, kind, roles, event range, phase/context summaries,
  and references to lower-level APIs. It deliberately does not contain raw
  events, assembly text, or screenshot bytes.

  If the segments generation is absent, this explains that the caller must
  build `:segments` (or a dependent target) with `build-pipeline!` first."
  [analysis-result]
  (let [image (stage! analysis-result :segments "list-segments")]
    (mapv #(segment-overview analysis-result %) (segments/segments image))))

(defn get-segment
  "Return one compact segment overview with drill-down references.

  `segment-id` is the logical id returned by `list-segments`. The result points
  to the raw capture reader and global half-open `:event-range`, the dedupe
  image for structural occurrences, committed disassembly `:asm-file` and
  `:clj-file` paths, and curated report screenshot descriptors when those
  stages are available. It does not eagerly load any of those payloads.

  Use `(disassembly/read-file (:disassembly analysis-result) asm-file)` for
  assembly text, `(reader/read-events (:capture analysis-result) event-range)`
  for raw events, and the referenced report/reader functions for screenshots."
  [analysis-result segment-id]
  (let [image (stage! analysis-result :segments "get-segment")
        segment (some #(when (= segment-id (:segment-id %)) %)
                      (segments/segments image))]
    (if segment
      (segment-overview analysis-result segment)
      (throw (ex-info (str "Unknown analysis segment " segment-id)
                      {:reason :unknown-segment
                       :segment-id segment-id
                       :dataset-root (:dataset-root analysis-result)})))))
