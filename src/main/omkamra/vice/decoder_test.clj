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

(deftest artifact-assembly-renders-interned-blocks
  (let [instructions [{:pc 0x1000 :bytes [0xea]
                       :raster-line 1 :cpu-cycle 2 :mnemonic "NOP"}
                      {:pc 0x1001 :bytes [0x4c 0 16]
                       :raster-line 1 :cpu-cycle 5 :mnemonic "JMP"}]
        execution (decoder/versioned-execution
                   instructions [{:kind :non-irq :start-index 0 :end-index 2}])
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)]
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
      (is (not (.contains assembly "; chronological span/block timeline")))
      (finally
        (.delete output-file)))))

(deftest assembly-deduplicates-operand-variants
  (let [execution {:format :omkamra.vice/versioned-execution-v3
                   :instructions
                   [{:id 0 :address 0x0079 :bytes [0xad 0x04 0x02]
                     :mnemonic "LDA" :mode :abs :operand "$0204"
                     :text "LDA $0204"}
                    {:id 1 :address 0x007c :bytes [0xc9 0x3a]
                     :mnemonic "CMP" :mode :imm :operand "#$3A"
                     :text "CMP #$3A"}
                    {:id 2 :address 0x007e :bytes [0xb0 0x0a]
                     :mnemonic "BCS" :mode :rel :operand "$008A"
                     :text "BCS $008A"}
                    {:id 3 :address 0x0079 :bytes [0xad 0x18 0x03]
                     :mnemonic "LDA" :mode :abs :operand "$0318"
                     :text "LDA $0318"}]
                   :blocks [{:id 0 :instruction-ids [0 1 2]}
                            {:id 1 :instruction-ids [3 1 2]}]
                   :block-runs [{:start-index 0 :end-index 3
                                 :block-id 0 :iterations 1}
                                {:start-index 3 :end-index 6
                                 :block-id 1 :iterations 1}]
                   :spans [{:kind :non-irq :start-index 0 :end-index 6}]}
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)]
    ;; The exact execution model remains variant-aware.
    (is (= 2 (count (:blocks execution))))
    ;; The assembly projection emits one structural block and masks the
    ;; changing absolute operand instead of printing both code images.
    (is (= 1 (count (re-seq #"(?m)^; block " assembly))))
    (is (.contains assembly "; block 0, 3 instructions, 2 code-image variants"))
    (is (.contains assembly "$0079  AD ?? ?? LDA $????"))
    (is (not (.contains assembly "; chronological span/block timeline")))))

(deftest assembly-masks-varying-immediate-operands
  (let [execution {:format :omkamra.vice/versioned-execution-v3
                   :instructions [{:id 0 :address 0xd012 :bytes [0xa9 0x07]
                                   :mnemonic "LDA" :mode :imm :operand "#$07"
                                   :text "LDA #$07"}
                                  {:id 1 :address 0xd012 :bytes [0xa9 0x0f]
                                   :mnemonic "LDA" :mode :imm :operand "#$0F"
                                   :text "LDA #$0F"}]
                   :blocks [{:id 0 :instruction-ids [0]}
                            {:id 1 :instruction-ids [1]}]
                   :block-runs [{:start-index 0 :end-index 1
                                 :block-id 0 :iterations 1}
                                {:start-index 1 :end-index 2
                                 :block-id 1 :iterations 1}]
                   :spans [{:kind :irq :start-index 0 :end-index 2}]}
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)]
    (is (= 1 (count (re-seq #"(?m)^; block " assembly))))
    (is (.contains assembly "$D012  A9 ??    LDA #$??"))))

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

(deftest splits-block-at-backward-control-flow-target
  (let [instructions [{:pc 0xc000 :bytes [0xa2 0x00] :mnemonic "LDX"
                       :mode :imm :operand "#$00" :text "LDX #$00"}
                      {:pc 0xc002 :bytes [0xea] :mnemonic "NOP"
                       :mode :imp :operand "" :text "NOP"}
                      {:pc 0xc003 :bytes [0xe8] :mnemonic "INX"
                       :mode :imp :operand "" :text "INX"}
                      {:pc 0xc004 :bytes [0xd0 0xfc] :mnemonic "BNE"
                       :mode :rel :operand "$C002" :text "BNE $C002"}]
        execution (decoder/versioned-execution
                   instructions [{:kind :non-irq :start-index 0 :end-index 4}])]
    (is (= [[0] [1 2 3]]
           (mapv :instruction-ids (:blocks execution))))
    (is (= [[0 1] [1 4]]
           (mapv (juxt :start-index :end-index)
                 (:block-runs execution))))))

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

(deftest separates-irq-handler-from-interrupted-caller-block
  (let [instructions [{:pc 0xe5cd :bytes [0xa5 0xc6] :mnemonic "LDA"}
                       {:pc 0xff48 :bytes [0x48] :mnemonic "PHA"}
                       {:pc 0xff49 :bytes [0x8a] :mnemonic "TXA"}
                       {:pc 0xff4a :bytes [0x48] :mnemonic "PHA"}
                       {:pc 0xff4b :bytes [0x98] :mnemonic "TYA"}
                       {:pc 0xff4c :bytes [0x48] :mnemonic "PHA"}
                       {:pc 0xff4d :bytes [0xba] :mnemonic "TSX"}
                       {:pc 0xff4e :bytes [0xbd 0x04 0x01] :mnemonic "LDA"}
                       {:pc 0xff51 :bytes [0x29 0x10] :mnemonic "AND"}
                       {:pc 0xff53 :bytes [0xf0 0x03] :mnemonic "BEQ"}]
        irq-sections (var-get (ns-resolve 'omkamra.vice.decoder
                                          'irq-sections))
        spans (:spans (irq-sections instructions (byte-array 65536)))
        execution (decoder/versioned-execution instructions spans)]
    (is (= [[0] [1 2 3 4 5 6 7 8 9]]
           (mapv :instruction-ids (:blocks execution))))
    (is (= [[0 1] [1 10]]
           (mapv (juxt :start-index :end-index)
                 (:block-runs execution))))))

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
