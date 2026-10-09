(ns omkamra.vice.decoder-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.decoder :as decoder]))

(defn- trace-event
  [{:keys [pc bytes raster-line cpu-cycle a x y sp flags global-cycle]}]
  [pc bytes raster-line cpu-cycle a x y sp flags global-cycle])

(deftest streaming-transducers-intern-instructions-and-basic-blocks
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'instruction-block-stream))
        ingester (make-ingester)
        state (atom
               (reduce (:step ingester) (empty-state)
                       (for [index (range 12)]
                         (trace-event
                          {:pc (+ 0x1000 (mod index 2))
                           :bytes [(if (even? index) 0xea 0x60)]
                           :raster-line 1 :cpu-cycle index
                           :a 0 :x 0 :y 0 :sp 0xff
                           :flags "........" :global-cycle index}))))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= 12 (:event-count stream)))
    (is (= 2 (count (:instructions stream))))
    (is (= [{:id 0 :instruction-ids [0 1]}] (:blocks stream)))
    ;; Consecutive occurrences of the same block are run-length encoded.
    (is (= [[0 6]] (:block-runs stream)))
    ;; Full register samples are opt-in rather than retained by default.
    (is (nil? (:samples stream)))
    (is (nil? (:occurrences stream)))))

(deftest conditional-branch-to-fall-through-is-deduplicated
  (let [control-flow-successors (var-get (ns-resolve 'omkamra.vice.decoder
                                                     'control-flow-successors))
        branch {:pc 0xc000
                :bytes [0xd0 0x00]
                :mnemonic "BNE"}]
    (is (= #{0xc002}
           (control-flow-successors nil branch)))))

(deftest streaming-splits-at-backward-control-flow-target
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'instruction-block-stream))
        ingester (make-ingester {:initial-memory (byte-array 65536)})
        events [[0xc000 [0xa2 0x00] nil nil nil nil nil nil nil nil]
                [0xc002 [0xea] nil nil nil nil nil nil nil nil]
                [0xc003 [0xe8] nil nil nil nil nil nil nil nil]
                [0xc004 [0xd0 0xfc] nil nil nil nil nil nil nil nil]]
        state (atom (reduce (:step ingester) (empty-state) events))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= [[0] [1 2 3]]
           (mapv :instruction-ids (:blocks stream))))))

(deftest streaming-ingestion-splits-at-control-flow-discontinuity
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'instruction-block-stream))
        ingester (make-ingester {:initial-memory (byte-array 65536)})
        events [[0xe5cd [0xa5 0xc6] 1 0 nil nil nil nil nil nil]
                [0xff48 [0x48] 1 2 nil nil nil nil nil nil]
                [0xff49 [0x8a] 1 4 nil nil nil nil nil nil]]
        state (atom (reduce (:step ingester) (empty-state) events))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= [[0] [1 2]]
           (mapv :instruction-ids (:blocks stream))))))

(deftest retains-full-samples-only-when-requested
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'instruction-block-stream))
        ingester (make-ingester {:retain-samples? true})
        events [[0x1000 [0xea] 1 0 1 2 3 4 "........" 5]]
        state (atom (reduce (:step ingester) (empty-state) events))]
    (swap! state (:complete ingester))
    (is (= [[1 0 1 2 3 4 "........" 5]]
           (:samples (finish-stream state ingester))))))

(deftest compact-write-records-round-trip
  (let [compact-write (var-get (ns-resolve 'omkamra.vice.decoder
                                           'compact-write-data))
        writes [{:event-index 10 :pc 0x1000 :address 0xd018
                 :value 3 :old-value 0 :raster-line 2 :cpu-cycle 5
                 :instruction-raster-line 2 :instruction-cpu-cycle 2
                 :write-cycle-offset 2 :mnemonic "STA" :kind :store}]
        compact (compact-write writes)]
    (is (= :omkamra.vice/write-records-v1 (:format compact)))
    (is (= [[10 0x1000 0xd018 3 0 2 5 2 2 2 0 0]]
           (:writes compact)))
    (is (= writes (decoder/expand-writes compact)))))

(defn- delete-tree!
  [file]
  (when (.isDirectory file)
    (doseq [child (.listFiles file)]
      (delete-tree! child)))
  (.delete file))

(deftest chunked-recorder-persists-local-artifacts-and-global-ranges
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder %))
        directory (doto (java.io.File/createTempFile "omkamra-chunks-" "")
                    (.delete)
                    (.mkdirs))
        _ (.mkdirs (java.io.File. directory "chunks"))
        options ((private 'normalized-chunk-options)
                 {:chunk-max-events 2 :chunk-queue-capacity 2})
        metadata {:capture-id "chunk-test" :input "/tmp/demo.prg"}
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
      (is (not (.exists (io/file directory "analysis"))))
      (let [analysis (analysis/run! (.getPath directory) {:stage :all})
            structure-index (read-string
                             (slurp (io/file directory "analysis" "stages"
                                             "structure" "index.edn")))
            writes (read-string
                    (slurp (io/file directory "analysis" "stages"
                                    "writes" "chunks"
                                    "chunk-000001.edn")))
            structure (read-string
                       (slurp (io/file directory "analysis" "stages"
                                       "structure" "chunks"
                                       "chunk-000001.edn")))
            video (read-string
                   (slurp (io/file directory "analysis" "stages"
                                   "video" "chunks"
                                   "chunk-000001.edn")))
            assets (read-string
                    (slurp (io/file directory "analysis" "stages"
                                    "assets" "chunks"
                                    "chunk-000001.edn")))
            stored-manifest (decoder/read-capture-manifest (.getPath directory))
            first-chunk (decoder/read-chunk (.getPath directory) 1)
            second-chunk (decoder/read-chunk (.getPath directory) 2)]
        (is (= :omkamra.vice/capture-v1 (:format stored-manifest)))
        (is (= [[0 2] [2 3]] (mapv :event-range (:chunks stored-manifest))))
        (is (= {:kind :forced-size :reason :max-events}
               (:boundary first-chunk)))
        (is (= 2 (:local-event-count first-chunk)))
        (is (= #{:decoded :memory :structure :semantics}
               (set (keys (:stages first-chunk)))))
        (is (nil? (get-in first-chunk [:stages :video])))
        (is (nil? (get-in first-chunk
                          [:stages :structure :execution :node-versions])))
        (is (= 2 (:next-chunk first-chunk)))
        (is (= 1 (:previous-chunk second-chunk)))
        (is (= :final (get-in second-chunk [:boundary :kind])))
        (is (= 2 (:chunks-written @writer-state)))
        (is (= :complete (:status analysis)))
        (is (= [:writes :structure :video :assets]
               (:executed-stages analysis)))
        (is (= 2 (:chunk-count structure-index)))
        (is (= :omkamra.vice/writes-chunk-v2 (:format writes)))
        (is (= {:stage :memory
                :key :writes
                :format :omkamra.vice/write-records-v1}
               (get-in writes [:stages :writes :source])))
        (is (= :omkamra.vice/structure-chunk-v1 (:format structure)))
        (is (map? (get-in structure [:stages :structure :execution])))
        (is (= :omkamra.vice/video-chunk-v1 (:format video)))
        (is (map? (get-in video [:stages :video :vic])))
        (is (= :omkamra.vice/assets-chunk-v1 (:format assets)))
        (is (vector? (get-in assets [:stages :assets :samples])))
        (is (= 2 (count (:chunks stored-manifest)))))
      (finally
        (delete-tree! directory)))))

(deftest chunk-writer-backpressure-is-explicit
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder %))
        queue (java.util.concurrent.ArrayBlockingQueue. 1)
        writer-state (atom {:status :running})]
    (.put queue :already-full)
    (try
      (is (= :chunk-writer-backpressure
             (:reason (ex-data
                       (try
                         ((private 'enqueue-chunk!) queue writer-state
                                                    {:chunk-number 1} 1)
                         (catch clojure.lang.ExceptionInfo error error))))))
      (finally
        (.clear queue)))))

(deftest writer-failure-is-retained-for-capture-shutdown
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder %))
        directory (doto (java.io.File/createTempFile "omkamra-writer-error-" "")
                    (.delete)
                    (.mkdirs))
        ;; A file at `chunks` makes the writer's chunk output path invalid.
        chunks-file (io/file directory "chunks")
        _ (spit chunks-file "not a directory")
        manifest (atom ((private 'make-manifest) (.getPath directory)
                                                 {:capture-id "writer-error" :input "x"}
                                                 ((private 'normalized-chunk-options) {})))
        queue (java.util.concurrent.ArrayBlockingQueue. 1)
        writer-state (atom {:status :running :queue-depth 0 :high-water-mark 0
                            :chunks-written 0 :backpressure-count 0})
        writer ((private 'start-chunk-writer!) (.getPath directory) manifest
                                               queue writer-state)]
    (try
      (.put queue {:chunk {:chunk-number 1
                           :event-range [0 1]
                           :boundary {:kind :final}
                           :summary {}}})
      (.join writer 5000)
      (is (= :failed (:status @writer-state)))
      (is (instance? Throwable (:error @writer-state)))
      (finally
        (delete-tree! directory)))))
