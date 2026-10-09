(ns omkamra.vice.analysis-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.analysis.common]
            [omkamra.vice.analysis.runner]
            [omkamra.vice.analysis.segments]
            [omkamra.vice.decoder :as decoder]
            [omkamra.vice.decoder.capture]
            [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.structure :as structure]
            [omkamra.vice.decoder.video :as video]
            [omkamra.vice.decoder.write :as write]))

(defn- delete-tree!
  [file]
  (when (.isDirectory file)
    (doseq [child (.listFiles file)]
      (delete-tree! child)))
  (.delete file))

(defn- private
  [symbol]
  (var-get (ns-resolve 'omkamra.vice.decoder.capture symbol)))

(defn- analysis-private
  [symbol]
  (let [namespace (case symbol
                    stage-registry 'omkamra.vice.analysis.runner
                    read-stage-chunk 'omkamra.vice.analysis.common
                    selected-execution 'omkamra.vice.analysis.segments
                    raw-chunk->render-chunk 'omkamra.vice.analysis.segments
                    segment-descriptors 'omkamra.vice.analysis.segments
                    frame-epochs 'omkamra.vice.analysis.segments
                    segment-boundary-audit 'omkamra.vice.analysis.segments)]
    (var-get (ns-resolve namespace symbol))))

(defn- with-raw-capture
  [f]
  (let [directory (doto (java.io.File/createTempFile "omkamra-analysis-" "")
                    (.delete)
                    (.mkdirs))
        _ (.mkdirs (io/file directory "chunks"))
        options ((private 'normalized-chunk-options)
                 {:chunk-max-events 2 :chunk-queue-capacity 2})
        metadata {:capture-id "analysis-test" :input "/tmp/demo.prg"}
        manifest (atom ((private 'make-manifest) (.getPath directory)
                                                 metadata options))
        queue (java.util.concurrent.ArrayBlockingQueue. 2)
        writer-state (atom {:status :running :queue-depth 0 :high-water-mark 0
                            :chunks-written 0 :backpressure-count 0})
        writer ((private 'start-chunk-writer!) (.getPath directory) manifest
                                               queue writer-state)
        coordinator (atom {:open-chunk ((private 'open-chunk)
                                        1 0 (byte-array 65536) false)
                           :retain-samples? false
                           :writer-queue queue
                           :writer-state writer-state
                           :writer-thread writer})
        records [[0x1000 [0xea] 1 1 0 0 0 nil "........" nil]
                 [0x1001 [0x60] 1 2 0 0 0 nil "........" nil]
                 [0x1002 [0xea] 1 3 0 0 0 nil "........" nil]]]
    (try
      ((private 'write-manifest!) (.getPath directory) manifest)
      ((private 'ingest-chunked-batch!) coordinator metadata options records)
      ((private 'close-open-chunk!) coordinator metadata options
                                    {:kind :final :reason :stopped})
      ((private 'shutdown-chunk-writer!) coordinator 5000)
      (swap! manifest assoc :status :stopped :finalized? true)
      ((private 'write-manifest!) (.getPath directory) manifest)
      (f (.getPath directory))
      (finally
        (delete-tree! directory)))))

(deftest disassembly-omits-kernal-rom-but-keeps-high-memory-ram
  (let [selected-execution (analysis-private 'selected-execution)
        raw-chunk->render-chunk (analysis-private 'raw-chunk->render-chunk)
        render (fn [chunk] (raw-chunk->render-chunk chunk))
        memory (vec (repeat 65536 0))
        instruction {:address 0xe000
                     :bytes [0xea]
                     :mnemonic "NOP"
                     :mode :imp
                     :operand ""
                     :text "NOP"}
        execution {:event-count 1
                   :instructions [instruction]
                   :blocks [{:id 0 :instruction-ids [0]}]
                   :block-runs [[0 1]]}
        raw-chunk {:event-range [0 1]
                   :events {:event-count 1
                            :block-runs [[0 1]]}
                   :stages {:memory {:initial (assoc memory 1 0x37)
                                     :writes {:mnemonics []
                                              :kinds []
                                              :writes []}}
                            :structure {:execution execution}}}]
    (is (empty? (:blocks (selected-execution (render raw-chunk) [0 1] true))))
    (let [ram-raw (assoc-in raw-chunk [:events :event-count] 2)
          ram-raw (assoc-in ram-raw [:stages :structure :execution :event-count] 2)
          ram-raw (assoc-in ram-raw [:stages :structure :execution :block-runs] [[0 2]])
          ram-raw (assoc-in ram-raw [:events :block-runs] [[0 2]])
          ram-raw (assoc-in ram-raw [:stages :memory :writes]
                            {:mnemonics ["STA"]
                             :kinds ["store"]
                             :writes [[0 0xe000 1 0x35 0 0 0 0 0 0 0 0 0]]})]
      (is (= [[0]]
             (mapv :instruction-ids
                   (:blocks (selected-execution (render ram-raw) [0 2] true)))))
      (let [basic-raw (assoc-in raw-chunk [:stages :structure :execution
                                           :instructions 0 :address]
                                0xa000)
            basic-raw (assoc-in basic-raw [:stages :memory :initial 1] 0x03)]
        (is (empty? (:blocks (selected-execution (render basic-raw) [0 1] true))))
        (is (= [[0]]
               (mapv :instruction-ids
                     (:blocks
                      (selected-execution
                       (render (assoc-in basic-raw [:stages :memory :initial 1] 0x02))
                       [0 1]
                       true)))))))))

(deftest routine-descriptor-selects-per-frame-writing-pcs
  (let [routine-descriptor omkamra.vice.analysis.segments/routine-descriptor
        aggregate omkamra.vice.analysis.segments/aggregate-write-pcs
        configuration {:routine-min-pc-count 4
                       :routine-pc-relative-threshold 0.1}
        units [{:event-range [0 100] :sid-write-pcs {0x1000 10 0x1010 10 0x2000 1}}
               {:event-range [100 200] :sid-write-pcs {0x1000 10 0x1010 10}}]
        routine (routine-descriptor
                 (aggregate units [0 200] :sid-write-pcs) :music configuration)]
    ;; Both per-frame writing PCs are anchors; the single one-off poke is not.
    (is (= #{0x1000 0x1010} (:anchor-pcs routine)))
    (is (= [0x1000 0x1010] (:pc-range routine)))
    (is (= :music (:kind routine)))
    ;; A routine that never crosses the floor is not reported.
    (is (nil? (routine-descriptor {0x2000 1} :music configuration)))))

(deftest selected-execution-can-carve-one-routine-by-anchor-pc
  (let [selected-execution (analysis-private 'selected-execution)
        instructions [{:address 0x1000 :bytes [0xea] :mnemonic "NOP" :mode :imp}
                      {:address 0x1010 :bytes [0xea] :mnemonic "NOP" :mode :imp}]
        execution {:event-count 2
                   :instructions instructions
                   :blocks [{:id 0 :instruction-ids [0]}
                            {:id 1 :instruction-ids [1]}]}
        render {:chunk-number 1 :event-range [0 2] :event-count 2
                :execution execution :block-runs [[0 1] [1 1]]
                :initial-port 0x37 :port-writes []}]
    (is (= [[0] [1]]
           (mapv :instruction-ids
                 (:blocks (selected-execution render [0 2] false)))))
    ;; With an anchor only the block containing that PC is materialized.
    (is (= [[1]]
           (mapv :instruction-ids
                 (:blocks (selected-execution render [0 2] false #{0x1010})))))))

(deftest stages-expose-a-broadcast-reducer-contract
  (let [registry (var-get (ns-resolve 'omkamra.vice.analysis.runner 'stage-registry))
        stage (first registry)]
    (is (= [:id :version :depends-on :init :step :close]
           (keys stage)))
    (is (every? fn? ((juxt :init :step :close) stage)))))

(deftest analysis-is-explicit-and-reuses-compatible-stage-output
  (with-raw-capture
    (fn [directory]
      (testing "raw capture finalization does not create derived output"
        (is (not (.exists (io/file directory "analysis"))))
        (is (= :not-run (:analysis-status (analysis/status directory)))))
      (testing "the requested stage writes a versioned replaceable result"
        (let [result (analysis/run! directory {:stage :structure})
              manifest (decoder/read-capture-manifest directory)
              analysis-manifest
              (read-string (slurp (io/file directory "analysis" "manifest.edn")))]
          (is (= [:structure] (:executed-stages result)))
          (is (= [] (:skipped-stages result)))
          (is (= :stopped (:status manifest)))
          (is (= true (:finalized? manifest)))
          (is (= :complete (get-in manifest [:analysis :status])))
          (is (= "analysis/manifest.edn" (get-in manifest [:analysis :manifest-file])))
          (is (= :complete (:status analysis-manifest)))
          (is (= "structure-v3"
                 (get-in analysis-manifest [:stages :structure :version])))
          (is (.isFile (io/file directory "analysis" "stages"
                                "structure" "index.edn")))))
      (testing "later stages can be requested without rerunning structure"
        (let [result (analysis/run! directory {:stage :all})]
          (is (= [:writes :video :assets :features :classification :segments
                  :disassembly]
                 (:executed-stages result)))
          (is (= [:structure] (:skipped-stages result)))
          (is (.isFile (io/file directory "analysis" "stages"
                                "video" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "assets" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "features" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "classification" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "segments" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "disassembly" "index.edn")))
          (is (pos? (get-in (analysis/status directory)
                            [:feature-diagnostics :event-count])))
          (is (map? (:classification-diagnostics
                     (analysis/status directory)))))))))

(deftest named-targets-resolve-transitive-dependencies
  (with-raw-capture
    (fn [directory]
      (let [assets-result (analysis/run! directory {:stage :assets})]
        (is (= [:writes :video :assets] (:executed-stages assets-result)))
        (is (= [] (:skipped-stages assets-result))))
      (let [all-result (analysis/run! directory {:stage :all})]
        (is (= [:structure :features :classification :segments :disassembly]
               (:executed-stages all-result)))
        (is (= [:writes :video :assets] (:skipped-stages all-result)))))))

(deftest broadcasts-each-raw-chunk-to-all-requested-stages-once
  (with-raw-capture
    (fn [directory]
      (let [read-chunk decoder/read-chunk
            read-stage-chunk (ns-resolve 'omkamra.vice.analysis.common
                                         'read-stage-chunk)
            raw-chunk-reads (atom 0)]
        (with-redefs-fn
          {#'artifact/read-chunk
           (fn [& args]
             (swap! raw-chunk-reads inc)
             (apply read-chunk args))
           read-stage-chunk
           (fn [& _]
             (throw (ex-info "broadcast dependencies must stay in memory" {})))
           #'write/expand-writes
           (fn [& _]
             (throw (ex-info "eager write expansion should not run" {})))}
          #(is (= [:writes :structure :video :assets :features :classification
                   :segments :disassembly]
                  (:executed-stages (analysis/run! directory {:stage :all})))))
        ;; The fixture has two chunks. Before broadcasting, each of the four
        ;; stages independently parsed both raw chunk EDN files. The throwing
        ;; replacement above also proves that assets receives video's in-flight
        ;; result rather than rereading its staged output. The eager
        ;; expansion path is also forbidden: workers consume compact records
        ;; through fresh streaming sources.
        (is (= 2 @raw-chunk-reads))))))

(deftest caches-one-committed-writes-dependency-per-chunk
  (with-raw-capture
    (fn [directory]
      (analysis/run! directory {:stage :writes :chunk-parallelism 1})
      (let [read-stage-chunk (ns-resolve 'omkamra.vice.analysis.common
                                         'read-stage-chunk)
            original-read-stage-chunk (var-get read-stage-chunk)
            stage-reads (atom [])]
        (with-redefs-fn
          {read-stage-chunk
           (fn [& args]
             (swap! stage-reads conj (nth args 2))
             (apply original-read-stage-chunk args))
           #'write/expand-writes
           (fn [& _]
             (throw (ex-info "eager write expansion should not run" {})))}
          #(is (= [:video :assets]
                  (:executed-stages
                   (analysis/run! directory {:stage :assets
                                             :chunk-parallelism 1})))))
        ;; Video and assets share the committed descriptor read for each raw
        ;; chunk and consume its compact source without eager expansion.
        (is (= [:writes :writes] @stage-reads))))))

(deftest semantic-segments-retain-exact-source-slices-and-overlapping-activities
  (let [segment-descriptors
        (var-get (ns-resolve 'omkamra.vice.analysis.segments 'segment-descriptors))
        capture {:chunks [{:number 1 :event-range [0 100]}
                          {:number 2 :event-range [100 200]}
                          {:number 3 :event-range [200 300]}
                          {:number 4 :event-range [300 400]}]}
        loader {:kind :loader :event-range [210 300] :confidence 0.9
                :signals [:iec-register-activity]}
        decruncher {:kind :decruncher :event-range [280 390] :confidence 0.9
                    :signals [:high-write-rate]}
        segments (segment-descriptors
                  capture
                  {:activities [loader decruncher]}
                  [{:event-range [0 60] :fingerprint-id "a"}
                   {:event-range [80 180] :fingerprint-id "a"}
                   {:event-range [600 900] :fingerprint-id "b"}]
                  {:frame-gap-events 100
                   :min-demopart-events 50
                   :transition-gap-events 20
                   :activity-confidence 0.5
                   :materialize-assembly? false})
        transitions (filterv #(= :transition (:kind %)) segments)
        loader-segment (some #(when (= :loader (:role %)) %) transitions)
        decruncher-segment (some #(when (= :decruncher (:role %)) %) transitions)
        parts (filterv #(contains? #{:demopart :effect-candidate} (:kind %))
                       segments)]
    ;; A loader and a decruncher that merely touch in time are separate
    ;; segments, each owning its own routine code.
    (is (= [210 300] (:event-range loader-segment)))
    (is (= :loader (:role loader-segment)))
    (is (= [280 390] (:event-range decruncher-segment)))
    (is (= :decruncher (:role decruncher-segment)))
    (is (every? :source-event-range
                (concat (:activities loader-segment)
                        (:activities decruncher-segment))))
    (is (= [[{:chunk-number 1 :event-range [0 100]}
             {:chunk-number 2 :event-range [100 180]}]
            []]
           ;; The first part crosses a physical boundary. The second is
           ;; outside this tiny capture and deliberately has no source slice.
           (mapv :source-chunks parts)))
    (is (= [[0 180] [600 900]] (mapv :event-range parts)))
    (is (= [] (:activities (first parts))))))

(deftest semantic-segments-split-only-at-stable-frame-signature-changes
  (let [frame-epochs (analysis-private 'frame-epochs)
        segment-descriptors (analysis-private 'segment-descriptors)
        boundary-audit (analysis-private 'segment-boundary-audit)
        configuration {:frame-gap-events 100
                       :min-demopart-events 50
                       :transition-gap-events 20
                       :activity-confidence 0.5
                       :signature-stability-frames 3
                       :signature-vic-write-bucket 64}
        frame (fn [start pc]
                {:event-range [start (+ start 100)]
                 :fingerprint-id (str pc "-" start)
                 :irq-entry-pcs [pc]
                 :d012-write-count 1
                 :vic-write-count 128})
        stable-frames (concat (map #(frame % 0x1000) [0 100 200 300])
                              (map #(frame % 0x2000) [400 500 600 700]))
        transient-frames (concat (map #(frame % 0x1000) [0 100 200 300])
                                 (map #(frame % 0x2000) [400 500])
                                 (map #(frame % 0x1000) [600 700]))
        capture {:chunks [{:number 1 :event-range [0 400]}
                          {:number 2 :event-range [400 800]}]}
        loader {:kind :loader :event-range [450 550] :confidence 0.9
                :signals [:iec-register-activity]}
        stable-epochs (frame-epochs stable-frames configuration)
        segments (segment-descriptors capture {:activities [loader]}
                                      stable-frames configuration)
        epochs (filterv #(contains? #{:demopart :effect-candidate} (:kind %))
                        segments)
        parts (filterv #(= :demopart (:kind %)) segments)
        audit (boundary-audit capture segments)]
    ;; A new IRQ signature becomes a boundary at its first candidate frame,
    ;; but only after it remains stable for the configured confirmation window.
    (is (= [[0 400] [400 800]] (mapv :event-range stable-epochs)))
    ;; The initial stable epoch is retained for review, but topology change
    ;; confirms only the later epoch as a semantic demopart.
    (is (= [:effect-candidate :demopart] (mapv :kind epochs)))
    (is (= [[400 800]] (mapv :event-range parts)))
    (is (= :stable-frame-signature-change
           (get-in (first parts) [:signature-change :kind])))
    ;; A short alternate signature is normal effect variation, not a part.
    (is (= [[0 800]]
           (mapv :event-range (frame-epochs transient-frames configuration))))
    ;; The overlapping loader is explicit in both the descriptor and audit;
    ;; the later disassembly is therefore marked compound rather than silently
    ;; presented as demopart-only code.
    (is (= :compound (:role (first parts))))
    (is (= "analysis/stages/disassembly/assemblies/segment-000002.asm"
           (get-in audit [:segments 1 :assembly-file])))
    (is (= :touching (get-in audit [:adjacencies 0 :relationship])))
    (is (= [] (:unclassified-gaps audit)))))

(deftest handler-subset-fluctuation-does-not-split-a-part
  (let [frame-epochs (analysis-private 'frame-epochs)
        configuration {:frame-gap-events 100
                       :min-demopart-events 50
                       :signature-stability-frames 3
                       :signature-vic-write-bucket 64
                       :signature-d012-write-distance 1
                       :signature-vic-write-distance 1}
        frame (fn [start handlers]
                {:event-range [start (+ start 100)]
                 :irq-handler-pcs handlers
                 :d012-write-count 0
                 :vic-write-count 0})
        fluctuating (concat (map #(frame % [0x0943 0x6039 0x6073])
                                 [0 100 200 300 400])
                            (map #(frame % [0x6039 0x6073]) [500 600 700])
                            (map #(frame % [0x0943 0x6039 0x6073]) [800 900]))
        disjoint (concat (map #(frame % [0x1000 0x1100]) [0 100 200 300])
                         (map #(frame % [0x2000 0x2100]) [400 500 600 700]))]
    ;; A handler that runs in some frames but not others is sampling noise:
    ;; the smaller set is a subset of the larger one, so the epoch holds.
    (is (= 1 (count (frame-epochs fluctuating configuration))))
    ;; A change in which handlers are installed is a real part boundary.
    (is (= 2 (count (frame-epochs disjoint configuration))))))

(deftest default-kernal-handler-epochs-are-not-parts
  (let [segment-descriptors (analysis-private 'segment-descriptors)
        attach-file-groups omkamra.vice.analysis.segments/attach-file-groups
        frame (fn [start handlers]
                {:event-range [start (+ start 100000)]
                 :irq-handler-pcs handlers
                 :d012-write-count 0
                 :vic-write-count 0})
        configuration {:frame-gap-events 100000
                       :min-demopart-events 1
                       :transition-gap-events 250000
                       :activity-confidence 0.35
                       :signature-stability-frames 1
                       :signature-vic-write-bucket 64}
        capture {:chunks [{:number 1 :event-range [0 400000]}]}
        ;; A demopart activity confirms stable execution, but an epoch whose
        ;; IRQ never leaves the KERNAL default handler is still not a part.
        classification {:activities [{:kind :demopart
                                      :event-range [50000 150000]
                                      :confidence 1.0
                                      :signals [:stable-irq-fingerprint]}
                                     {:kind :demopart
                                      :event-range [250000 350000]
                                      :confidence 1.0
                                      :signals [:stable-irq-fingerprint]}]}
        frames [(frame 0 [0xea31])
                (frame 100000 [0xea31])
                (frame 200000 [0x1000])
                (frame 300000 [0x1000])]
        descriptors (segment-descriptors capture classification frames
                                         configuration)
        by-range (into {} (map (juxt :event-range identity) descriptors))]
    (is (true? (:default-irq-handler? (get by-range [0 200000]))))
    (is (= :effect-candidate (:kind (get by-range [0 200000]))))
    (is (true? (:custom-irq-handler? (get by-range [200000 400000]))))
    (is (= :demopart (:kind (get by-range [200000 400000]))))
    ;; File grouping counts only custom-handler epochs as parts.
    (let [grouping (attach-file-groups capture classification
                                       (mapv #(if (:default-irq-handler? %)
                                                (assoc % :file-id 1)
                                                (assoc % :file-id 1))
                                             descriptors)
                                       configuration)]
      (is (= [1]
             (mapv :part-candidate-count (:ranges grouping)))))))

(deftest trace-backed-file-grouping-and-gap-review
  (let [capture {:chunks [{:number 1 :event-range [0 100]}]}
        classification {:activities [{:kind :loader
                                      :event-range [40 45]
                                      :confidence 0.8
                                      :signals [:kernal-iec-call]}
                                     {:kind :loader
                                      :event-range [80 85]
                                      :confidence 0.8
                                      :signals [:kernal-iec-call]}]}
        descriptors [{:segment-id 1 :kind :effect-candidate :event-range [0 30]}
                     {:segment-id 2 :kind :transition :event-range [40 45]}
                     {:segment-id 3 :kind :demopart :event-range [50 70]}
                     {:segment-id 4 :kind :transition :event-range [80 85]}
                     {:segment-id 5 :kind :effect-candidate :event-range [90 100]}]
        grouping (omkamra.vice.analysis.segments/attach-file-groups
                  capture classification descriptors
                  {:transition-gap-events 10})]
    (is (= [[0 40] [40 80] [80 100]]
           (mapv :event-range (:ranges grouping))))
    (is (= [[1] [2 3] [4 5]]
           (mapv :segment-ids (:ranges grouping))))
    (is (= [:underfull :underfull :underfull]
           (mapv :part-count-status (:ranges grouping))))
    (is (= :raster-heavy
           (omkamra.vice.analysis.segments/gap-cause
            {:unit-count 2
             :irq-frame-count 1
             :write-count 100
             :ram-write-count 40
             :peripheral-write-ratio 0.6
             :iec-register-write-count 200
             :max-written-code-execution 0
             :max-frontier-movement 0}
            (omkamra.vice.analysis.common/normalize-segment-configuration {}))))
    ;; A transfer is recognized by sustained serial-bus writing, which is a
    ;; general signal rather than a code shape, and outranks the peripheral
    ;; alias heuristic that would otherwise call it raster-heavy.
    (is (= :loader-transfer
           (omkamra.vice.analysis.segments/gap-cause
            {:unit-count 2
             :irq-frame-count 0
             :serial-active-unit-ratio 1.0
             :max-serial-access-count 200
             :serial-access-count 400
             :write-count 100
             :ram-write-count 40
             :peripheral-write-ratio 0.6
             :iec-register-write-count 200
             :max-written-code-execution 0
             :max-frontier-movement 0}
            (omkamra.vice.analysis.common/normalize-segment-configuration {}))))
    ;; A RAM-dominant, serial-free phase decodes data already in RAM.
    (is (= :decruncher-stretch
           (omkamra.vice.analysis.segments/gap-cause
            {:unit-count 4
             :irq-frame-count 0
             :decode-unit-ratio 1.0
             :serial-access-count 0
             :write-count 400
             :ram-write-count 400
             :peripheral-write-ratio 0.0
             :max-written-code-execution 0
             :max-frontier-movement 0}
            (omkamra.vice.analysis.common/normalize-segment-configuration {}))))))

(deftest a-failed-broadcast-branch-does-not-publish-other-staged-output
  (with-raw-capture
    (fn [directory]
      (with-redefs [video/derive-assets-chunk
                    (fn [& _]
                      (throw (ex-info "asset derivation failed" {})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"asset derivation failed"
                              (analysis/run! directory {:stage :all}))))
      ;; Writes, structure, and video completed their private staging trees,
      ;; but none becomes visible when a dependent worker fails.
      (is (not (.exists (io/file directory "analysis" "stages" "writes"))))
      (is (not (.exists (io/file directory "analysis" "stages" "structure"))))
      (is (not (.exists (io/file directory "analysis" "stages" "video"))))
      (is (= :failed (:analysis-status (analysis/status directory)))))))

(deftest analysis-failure-preserves-last-successful-stage-output
  (with-raw-capture
    (fn [directory]
      (analysis/run! directory {:stage :structure})
      (let [index-file (io/file directory "analysis" "stages"
                                "structure" "index.edn")
            before (slurp index-file)]
        (with-redefs [structure/derive-structure-chunk
                      (fn [_]
                        (throw (ex-info "derived analysis failed" {})))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"derived analysis failed"
                                (analysis/run! directory {:stage :structure :force? true}))))
        (is (= before (slurp index-file)))
        (is (= :stopped (:status (decoder/read-capture-manifest directory))))
        (is (= :failed (:analysis-status (analysis/status directory))))
        ;; Existing compatible output is still usable, and a later request
        ;; clears the failed run state without rewriting that output.
        (is (= [:structure]
               (:skipped-stages (analysis/run! directory {:stage :structure}))))))))

(deftest analysis-recovers-stale-partial-output
  (with-raw-capture
    (fn [directory]
      (let [partial (io/file directory "analysis" "stages"
                             "structure.partial")]
        (.mkdirs partial)
        (spit (io/file partial "incomplete.edn") "incomplete")
        (analysis/run! directory {:stage :structure})
        (is (not (.exists partial)))
        (is (= :complete (:analysis-status (analysis/status directory))))))))
