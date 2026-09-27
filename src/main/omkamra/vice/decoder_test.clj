(ns omkamra.vice.decoder-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.decoder :as decoder]))

(defn- trace-event
  [{:keys [pc bytes raster-line cpu-cycle a x y sp flags global-cycle]}]
  [pc bytes raster-line cpu-cycle a x y sp flags global-cycle])

(deftest parser-transducer-emits-only-execution-trace-records
  (let [parser (var-get (ns-resolve 'omkamra.vice.decoder
                                    'monitor-trace-records-xf))
        records (into [] (parser)
                     ["#1 (Trace EXEC 1000) 1/$01, 2/$02"
                      ".C:1000 EA NOP - A:00 X:00 Y:00 SP:FF ........ 42"
                      "#2 (Trace LOAD 1001) 1/$01, 3/$03"
                      ".C:1001 EA NOP - A:00 X:00 Y:00 SP:FF ........ 43"])]
    (is (= [[0x1000 [0xea] 1 2 0 0 0 255 "........" 42]]
           records))))

(deftest parser-accepts-vice-trace-spacing
  (let [parser (var-get (ns-resolve 'omkamra.vice.decoder
                                    'monitor-trace-records-xf))
        records (into [] (parser)
                     ["#1 (Trace  exec fce4)    0/$000,   8/$08"
                      ".C:fce4  78          SEI            - A:55 X:FF Y:A7 SP:fd N.-..I.C          8"])]
    (is (= [[0xfce4 [0x78] 0 8 0x55 0xff 0xa7 0xfd "N.-..I.C" 8]]
           records))))

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

(deftest publishes-stream-state-once-per-batch
  (let [ingest-stream-batch (var-get (ns-resolve 'omkamra.vice.decoder
                                                 'ingest-stream-batch!))
        stream-state (atom {:event-count 0})
        publications (atom 0)
        watch-key ::batch-publication
        ingester {:step (fn [state event]
                          (update state :event-count + event))}]
    (add-watch stream-state watch-key
               (fn [_ _ _ _]
                 (swap! publications inc)))
    (try
      (ingest-stream-batch stream-state ingester [1 2 3])
      (is (= {:event-count 6} @stream-state))
      (is (= 1 @publications))
      (finally
        (remove-watch stream-state watch-key)))))

(deftest releases-stream-ingestion-state-after-finalization
  (let [release-stream-state (var-get (ns-resolve 'omkamra.vice.decoder
                                                   'release-stream-state!))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                            'make-stream-ingester))
        ingester (make-ingester)
        stream-state (atom {:status :stopped
                            :event-count 123
                            :block-runs (vec (repeat 2 {:block-id 0}))
                            :samples (vec (repeat 123 [0 1 2]))
                            :first-by-pc {1 {:event-index 0}}})]
    (reset! (:instruction-state ingester)
            {:instruction-ids {} :instructions (vec (repeat 4 {:id 0}))})
    (reset! (:block-state ingester)
            {:block-ids {} :blocks (vec (repeat 3 {:id 0}))})
    (release-stream-state stream-state ingester)
    (is (= {:status :stopped
            :event-count 123
            :instruction-count 4
            :block-count 3
            :block-run-count 2
            :error nil}
           @stream-state))))
