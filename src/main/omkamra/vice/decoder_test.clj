(ns omkamra.vice.decoder-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.decoder :as decoder]))

(deftest disassembles-mos-6510-addressing-modes
  (let [memory (byte-array 65536)]
    (aset-byte memory 0x1000 (unchecked-byte 0xa9)) ; LDA #$42
    (aset-byte memory 0x1001 (unchecked-byte 0x42))
    (aset-byte memory 0x1002 (unchecked-byte 0xd0)) ; BNE $1000
    (aset-byte memory 0x1003 (unchecked-byte 0xfc))
    (aset-byte memory 0x1004 (unchecked-byte 0x4c)) ; JMP $1234
    (aset-byte memory 0x1005 (unchecked-byte 0x34))
    (aset-byte memory 0x1006 (unchecked-byte 0x12))
    (is (= "LDA #$42" (:text (decoder/disassemble memory 0x1000))))
    (is (= "BNE $1000" (:text (decoder/disassemble memory 0x1002))))
    (is (= "JMP $1234" (:text (decoder/disassemble memory 0x1004))))))

(deftest renders-all-canonical-execution-spans
  (let [instructions [{:pc 0x1000 :bytes [0xea]
                       :raster-line 1 :cpu-cycle 2 :mnemonic "NOP"}
                      {:pc 0x1001 :bytes [0x4c 0 16]
                       :raster-line 1 :cpu-cycle 5 :mnemonic "JMP"}]
        execution (decoder/versioned-execution
                   instructions [{:kind :non-irq :start-index 0 :end-index 2}])
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)]
    (is (.contains assembly "; span non-irq events 0..1"))
    (is (.contains assembly "$1000  EA       NOP"))
    (is (.contains assembly "$1001  4C 00 10 JMP $1000"))))

(deftest artifact-assembly-uses-one-rendering-model
  (let [execution {:format :omkamra.vice/versioned-execution-v3
                   :instructions
                   [{:id 0 :address 0x2000 :bytes [0xea]
                     :mnemonic "NOP" :mode :imp :operand "" :text "NOP"}]
                   :blocks [{:id 0 :instruction-ids [0]}]
                   :block-runs [{:start-index 0 :end-index 2
                                 :block-id 0 :iterations 2}]
                   :spans [{:kind :non-irq :start-index 0 :end-index 2}]}
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)
        output-file (java.io.File/createTempFile "omkamra-assembly-" ".asm")]
    (try
      (is (= assembly
             (slurp (decoder/artifact->assembly
                     artifact {:output-file output-file}))))
      (is (= 1 (count (re-seq #"\$2000  EA" assembly))))
      (is (.contains assembly "; block-run events 0..1 block=0 iterations=2"))
      (finally
        (.delete output-file)))))

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

(deftest pipeline-replays-instruction-block-raw-events
  (let [memory (byte-array 65536)
        stream {:format :omkamra.vice/instruction-block-stream-v3
                :event-count 2
                :sample-keys [:raster-line :cpu-cycle :a :x :y :sp :flags
                              :global-cycle]
                :instructions [{:id 0 :operation :exec :pc 0x1000
                                :bytes [0xea] :vice-text "NOP"}]
                :blocks [{:id 0 :instruction-ids [0]}]
                :block-runs [{:start-index 0 :end-index 2
                              :iterations 2 :block-id 0}]
                :samples [[1 2 0 0 0 255 "........" 0]
                          [2 3 0 0 0 255 "........" 1]]}
        artifact (-> (decoder/raw-artifact
                      stream {:initial-memory memory :final-memory memory})
                     (decoder/run-pipeline {:compact? true}))]
    (is (= stream (get-in artifact [:raw :events])))
    (is (= 2 (get-in artifact [:stages :structure :execution :event-count])))
    (is (= 1 (count (get-in artifact
                            [:stages :structure :execution :instructions]))))
    (is (nil? (get-in artifact [:stages :normalized-events])))
    (is (nil? (get-in artifact [:stages :decoded :instructions])))))

(deftest interns-instructions-and-basic-blocks-with-node-versions
  (let [instructions (mapv (fn [[pc bytes raster-line]]
                             {:pc pc :bytes bytes :raster-line raster-line
                              :cpu-cycle 0 :text (format "$%04X" pc)
                              :mnemonic (if (= pc 0x1002) "JMP" "LDA")})
                           [[0x1000 [0xa9 0] 0] [0x1002 [0x4c 0 16] 1]
                            [0x1000 [0xa9 0] 2] [0x1002 [0x4c 0 16] 3]
                            [0x1000 [0xa9 0] 4] [0x1002 [0x4c 0 16] 5]
                            [0x1000 [0xa9 0] 6] [0x1002 [0x4c 0 16] 7]
                            [0x1000 [0xa9 0] 8] [0x1002 [0x4c 0 16] 9]
                            [0x1000 [0xa9 0] 10] [0x1002 [0x4c 0 16] 11]
                            [0x1000 [0xa9 255] 12]])
        spans [{:kind :non-irq :start-index 0 :end-index 6}
               {:kind :non-irq :start-index 6 :end-index 13}]
        execution (decoder/versioned-execution instructions spans)
        runs (:block-runs execution)]
    ;; The repeated two-instruction basic block has one dictionary entry even
    ;; though it is executed six times.
    (is (= 3 (count (:instructions execution))))
    (is (= [{:id 0 :instruction-ids [0 1]}
            {:id 1 :instruction-ids [2]}]
           (:blocks execution)))
    (is (= [0 0 0 0 0 0 1] (mapv :block-id runs)))
    ;; Changed bytes at $1000 create its next chronological incarnation.
    (is (= [0 1]
           (mapv :version (filter #(= 0x1000 (:address %))
                                  (:node-versions execution)))))
    (is (= [2] (:instruction-ids (second (:blocks execution)))))))

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
