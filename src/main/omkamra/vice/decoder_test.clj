(ns omkamra.vice.decoder-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.decoder :as decoder]))

(deftest parser-transducer-emits-only-execution-trace-records
  (let [parser (var-get (ns-resolve 'omkamra.vice.decoder
                                    'monitor-trace-records-xf))
        records (into [] (parser)
                     ["#1 (Trace EXEC 1000) 1/$01, 2/$02"
                      ".C:1000 EA NOP - A:00 X:00 Y:00 SP:FF ........ 42"
                      "#2 (Trace LOAD 1001) 1/$01, 3/$03"
                      ".C:1001 EA NOP - A:00 X:00 Y:00 SP:FF ........ 43"])]
    (is (= [{:operation :exec
             :pc 0x1000
             :bytes [0xea]
             :vice-text "NOP"
             :raster-line 1
             :cpu-cycle 2
             :a 0 :x 0 :y 0 :sp 255
             :flags "........"
             :global-cycle 42}]
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
                         {:operation :exec
                          :pc (+ 0x1000 (mod index 2))
                          :bytes [(if (even? index) 0xea 0x60)]
                          :vice-text (if (even? index) "NOP" "RTS")
                          :raster-line 1 :cpu-cycle index
                          :a 0 :x 0 :y 0 :sp 0xff
                          :flags "........" :global-cycle index})))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= 12 (:event-count stream)))
    (is (= 2 (count (:instructions stream))))
    (is (= [{:id 0 :instruction-ids [0 1]}] (:blocks stream)))
    (is (= 6 (count (:block-runs stream))))
    ;; Dynamic samples remain lossless, but no per-event PC/byte image remains.
    (is (= 12 (count (:samples stream))))
    (is (nil? (:occurrences stream)))))

(deftest streaming-splits-at-backward-control-flow-target
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                          'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder
                                            'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'instruction-block-stream))
        ingester (make-ingester {:initial-memory (byte-array 65536)})
        events [{:operation :exec :pc 0xc000 :bytes [0xa2 0x00]
                 :vice-text "LDX"}
                {:operation :exec :pc 0xc002 :bytes [0xea]
                 :vice-text "NOP"}
                {:operation :exec :pc 0xc003 :bytes [0xe8]
                 :vice-text "INX"}
                {:operation :exec :pc 0xc004 :bytes [0xd0 0xfc]
                 :vice-text "BNE"}]
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
        events [{:operation :exec :pc 0xe5cd :bytes [0xa5 0xc6]
                 :vice-text "LDA" :raster-line 1 :cpu-cycle 0}
                {:operation :exec :pc 0xff48 :bytes [0x48]
                 :vice-text "PHA" :raster-line 1 :cpu-cycle 2}
                {:operation :exec :pc 0xff49 :bytes [0x8a]
                 :vice-text "TXA" :raster-line 1 :cpu-cycle 4}]
        state (atom (reduce (:step ingester) (empty-state) events))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= [[0] [1 2]]
           (mapv :instruction-ids (:blocks stream))))))

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
