(ns omkamra.vice.decoder-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest assembly-preserves-execution-timing
  (let [memory (byte-array 65536)]
    (aset-byte memory 0x2000 (unchecked-byte 0xea)) ; NOP
    (is (= "; line 123 cycle 17\n$2000  EA       NOP"
           (decoder/trace->assembly [{:pc 0x2000
                                      :raster-line 123
                                      :cpu-cycle 17}]
                                     memory)))))

(deftest parses-vice-monitor-trace
  (let [log "#1 (Trace  exec 1093)    0/$000,   0/$00\n.C:1093  4C 93 10    JMP $1093      - A:00 X:28 Y:CD SP:fd ..-...ZC   66004848"
        entry (first (decoder/parse-monitor-trace log))]
    (is (= {:operation :exec
            :pc 0x1093 :raster-line 0 :cpu-cycle 0
            :bytes [0x4c 0x93 0x10]
            :a 0 :x 0x28 :y 0xcd :sp 0xfd
            :flags "..-...ZC" :global-cycle 66004848
            :vice-text "JMP $1093"}
           entry))))

(deftest assembly-uses-captured-instruction-bytes
  (is (= "; line 001 cycle 02\n$1093  4C 93 10 JMP $1093"
         (decoder/trace->assembly
          [{:pc 0x1093 :raster-line 1 :cpu-cycle 2
            :bytes [0x4c 0x93 0x10]}]
          nil))))

(deftest renders-all-canonical-execution-spans
  (let [instructions [{:pc 0x1000 :bytes [0xea]
                       :raster-line 1 :cpu-cycle 2}
                      {:pc 0x1001 :bytes [0x4c 0 16]
                       :raster-line 1 :cpu-cycle 5}]
        execution (decoder/versioned-execution
                   instructions [{:kind :non-irq :start-index 0 :end-index 2}])
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (decoder/artifact->assembly artifact)]
    (is (.contains assembly "; span non-irq events 0..1"))
    (is (.contains assembly "$1000  EA       NOP"))
    (is (.contains assembly "$1001  4C 00 10 JMP $1000"))))

(deftest artifact-assembly-uses-one-rendering-model
  (let [execution {:format :omkamra.vice/versioned-execution-v2
                   :instruction-definitions
                   [{:id 0 :address 0x2000 :bytes [0xea]
                     :mnemonic "NOP" :mode :imp :operand "" :text "NOP"}]
                   :sequences [{:id 0 :definition-ids [0]}]
                   :runs [{:kind :sequence :start-index 0 :end-index 2
                           :sequence-id 0 :iterations 2}]
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
      (is (.contains assembly "; run sequence events 0..1 sequence=0 iterations=2"))
      (finally
        (.delete output-file)))))

(deftest streaming-state-compresses-loops-before-finalization
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder
                                          'empty-stream-state))
        append-event (var-get (ns-resolve 'omkamra.vice.decoder
                                          'append-stream-event))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder
                                           'dictionary-event-stream))
        state (atom
               (reduce append-event (empty-state)
                       (for [index (range 12)]
                         {:operation :exec
                          :pc (+ 0x1000 (mod index 2))
                          :bytes [(if (even? index) 0xea 0x60)]
                          :vice-text (if (even? index) "NOP" "RTS")
                          :raster-line 1 :cpu-cycle index
                          :a 0 :x 0 :y 0 :sp 0xff
                          :flags "........" :global-cycle index})))
        stream (finish-stream state)]
    (is (= 12 (:event-count stream)))
    (is (= 2 (count (:definitions stream))))
    (is (= [{:id 0 :definition-ids [0 1]}] (:sequences stream)))
    (is (= [{:kind :loop :start-index 0 :end-index 12
             :iterations 6 :sequence-id 0}]
           (:runs stream)))
    ;; Dynamic samples remain lossless, but no per-event PC/byte image remains.
    (is (= 12 (count (:samples stream))))
    (is (nil? (:occurrences stream)))))

(deftest pipeline-replays-dictionary-coded-raw-events
  (let [memory (byte-array 65536)
        stream {:format :omkamra.vice/dictionary-event-stream-v1
                :event-count 2
                :definitions [{:id 0 :operation :exec :pc 0x1000
                               :bytes [0xea] :vice-text "NOP"}]
                :occurrences [{:definition-id 0 :raster-line 1 :cpu-cycle 2}
                              {:definition-id 0 :raster-line 2 :cpu-cycle 3}]}
        artifact (-> (decoder/raw-artifact
                      stream {:initial-memory memory :final-memory memory})
                     (decoder/run-pipeline {:compact? true}))]
    (is (= stream (get-in artifact [:raw :events])))
    (is (= 2 (get-in artifact [:stages :structure :execution :event-count])))
    (is (= 1 (count (get-in artifact
                            [:stages :structure :execution
                             :instruction-definitions]))))
    (is (nil? (get-in artifact [:stages :normalized-events])))
    (is (nil? (get-in artifact [:stages :decoded :instructions])))))

(deftest interns-instruction-definitions-sequences-and-node-versions
  (let [instructions (mapv (fn [[pc bytes raster-line]]
                             {:pc pc :bytes bytes :raster-line raster-line
                              :cpu-cycle 0 :text (format "$%04X" pc)})
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
        runs (mapcat :runs (:spans execution))]
    ;; The repeated two-instruction loop body has one dictionary entry even
    ;; though it occurs in both spans.
    (is (= 3 (count (:instruction-definitions execution))))
    (is (= 2 (count (:sequences execution))))
    (is (= 1 (count (filter #(= [0 1] (:definition-ids %))
                            (:sequences execution)))))
    (is (= #{0 1} (set (map :sequence-id runs))))
    ;; Changed bytes at $1000 create its next chronological incarnation.
    (is (= [0 1]
           (mapv :version (filter #(= 0x1000 (:address %))
                                  (:node-versions execution)))))
    (is (= (mapv #(select-keys % [:pc :bytes :raster-line :cpu-cycle :text])
                 instructions)
           (mapv #(select-keys % [:pc :bytes :raster-line :cpu-cycle :text])
                 (decoder/expand-execution execution))))))

(deftest interns-sequences-across-capture-boundaries
  (let [memory (byte-array 65536)
        _ (aset-byte memory 0x1000 (unchecked-byte 0xea))
        _ (aset-byte memory 0x1001 (unchecked-byte 0x4c))
        _ (aset-byte memory 0x1002 (unchecked-byte 0x00))
        _ (aset-byte memory 0x1003 (unchecked-byte 0x10))
        events [{:pc 0x1000 :raster-line 0 :cpu-cycle 0}
                {:pc 0x1001 :raster-line 0 :cpu-cycle 3}
                {:pc 0x1000 :raster-line 0 :cpu-cycle 6}
                {:pc 0x1001 :raster-line 0 :cpu-cycle 9}
                {:pc 0x1000 :raster-line 0 :cpu-cycle 12}
                {:pc 0x1001 :raster-line 0 :cpu-cycle 15}]
        capture (decoder/raw-artifact events {:initial-memory memory
                                               :final-memory memory})
        session (decoder/execution-session [capture capture])
        execution (get-in session [:stages :structure :execution])]
    (is (= 12 (:event-count execution)))
    (is (= 1 (count (:sequences execution))))
    (is (= [0 1] (get-in execution [:sequences 0 :definition-ids])))
    (is (= 2 (count (get-in session [:raw :metadata :captures]))))))

(deftest capture-trace-produces-pipeline-artifact
  (let [memory (byte-array 65536)
        _ (aset-byte memory 0x1000 (unchecked-byte 0x8d)) ; STA $D018
        _ (aset-byte memory 0x1001 (unchecked-byte 0x18))
        _ (aset-byte memory 0x1002 (unchecked-byte 0xd0))
        _ (aset-byte memory 0x1003 (unchecked-byte 0xea)) ; NOP
        _ (aset-byte memory 0xfffe (unchecked-byte 0x00)) ; IRQ vector $1000
        _ (aset-byte memory 0xffff (unchecked-byte 0x10))
        samples (atom [{:pc 0x1000 :raster-line 50 :cpu-cycle 5 :a 0x18}
                       {:pc 0x1003 :raster-line 50 :cpu-cycle 9 :a 0x18}
                       {:pc 0x1000 :raster-line 50 :cpu-cycle 5 :a 0x18}])
        resumed? (atom false)
        registers (fn [{:keys [pc raster-line cpu-cycle a]}]
                    {1 {:value pc} 2 {:value raster-line} 3 {:value cpu-cycle}
                     4 {:value a} 5 {:value 0} 6 {:value 0}
                     7 {:value 0} 8 {:value 0}})]
    (with-redefs [omkamra.vice.binary-monitor/drain-events (fn [& _] [])
                  omkamra.vice.binary-monitor/ping (fn [& _] {})
                  omkamra.vice.binary-monitor/registers-available
                  (fn [& _] {1 {:name "PC"} 2 {:name "LIN"} 3 {:name "CYC"}
                             4 {:name "A"} 5 {:name "X"} 6 {:name "Y"}
                             7 {:name "SP"} 8 {:name "FL"}})
                  omkamra.vice.binary-monitor/registers-get
                  (fn [& _] (registers (let [sample (first @samples)]
                                          (swap! samples rest)
                                          sample)))
                  omkamra.vice.binary-monitor/advance-and-wait (fn [& _] {})
                  omkamra.vice.binary-monitor/mem-get (fn [& _] {:memory memory})
                  omkamra.vice.binary-monitor/resume (fn [& _] (reset! resumed? true))]
      (let [capture (decoder/capture-trace {} {:min-instructions 2
                                                :include-display? false
                                                :bulk? false})]
        (is (= :frame (get-in capture [:frame :reason])))
        (is (= [{:address 0xd018 :value 0x18 :kind :store :inferred? true}]
               (mapv #(select-keys % [:address :value :kind :inferred? true])
                     (get-in capture [:stages :decoded :writes]))))
        (is (= 0x18 (get-in capture [:stages :video :vic :final-state 0xd018])))
        (is (= [[50 5]]
               (mapv (juxt (comp :raster-line :trigger)
                           (comp :cpu-cycle :trigger))
                     (get-in capture [:stages :structure :irq-sections]))))
        (is @resumed?)))))
