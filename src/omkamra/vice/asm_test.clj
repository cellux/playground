(ns omkamra.vice.asm-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.asm :as asm]))

(deftest disassembles-mos-6510-addressing-modes
  (let [memory (byte-array 65536)]
    (aset-byte memory 0x1000 (unchecked-byte 0xa9)) ; LDA #$42
    (aset-byte memory 0x1001 (unchecked-byte 0x42))
    (aset-byte memory 0x1002 (unchecked-byte 0xd0)) ; BNE $1000
    (aset-byte memory 0x1003 (unchecked-byte 0xfc))
    (aset-byte memory 0x1004 (unchecked-byte 0x4c)) ; JMP $1234
    (aset-byte memory 0x1005 (unchecked-byte 0x34))
    (aset-byte memory 0x1006 (unchecked-byte 0x12))
    (is (= "LDA #$42" (:text (asm/disassemble memory 0x1000))))
    (is (= "BNE $1000" (:text (asm/disassemble memory 0x1002))))
    (is (= "JMP $1234" (:text (asm/disassemble memory 0x1004))))))

(deftest artifact-assembly-renders-interned-blocks
  (let [execution {:format :omkamra.vice/versioned-execution-v3
                   :instructions [{:id 0 :address 0x1000 :bytes [0xea]
                                   :mnemonic "NOP" :mode :imp :operand "" :text "NOP"}
                                  {:id 1 :address 0x1001 :bytes [0x4c 0 16]
                                   :mnemonic "JMP" :mode :abs :operand "$1000" :text "JMP $1000"}]
                   :blocks [{:id 0 :instruction-ids [0 1]}]}
        artifact {:format :omkamra.vice/pipeline-v1
                  :stages {:structure {:execution execution}}}
        assembly (asm/artifact->assembly artifact)]
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
        assembly (asm/artifact->assembly artifact)
        output-file (java.io.File/createTempFile "omkamra-assembly-" ".asm")]
    (try
      (is (= assembly
             (slurp (asm/artifact->assembly
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
        assembly (asm/artifact->assembly artifact)]
    ;; The exact execution model remains variant-aware.
    (is (= 2 (count (:blocks execution))))
    ;; The assembly projection emits one structural block and masks the
    ;; changing absolute operand instead of printing both code images.
    (is (= 1 (count (re-seq #"(?m)^; block " assembly))))
    (is (.contains assembly "; block 0, 3 instructions, 2 code-image variants"))
    (is (.contains assembly "$0079  AD ?? ?? LDA $????"))
    (is (not (.contains assembly "; chronological span/block timeline")))))

(deftest stream-assembly-deduplicates-templates-across-executions
  (let [execution (fn [value]
                    {:instructions [{:id 0 :address 0x1000 :bytes [0xa9 value]
                                     :mnemonic "LDA" :mode :imm
                                     :operand (format "#$%02X" value)}]
                     :blocks [{:id 0 :instruction-ids [0]}]})
        writer (java.io.StringWriter.)]
    (asm/write-executions-assembly! writer [(execution 1) (execution 2)])
    (let [assembly (str writer)]
      (is (= 1 (count (re-seq #"(?m)^; block " assembly))))
      (is (.contains assembly "; block 0, 1 instructions, 2 code-image observations"))
      (is (.contains assembly "$1000  A9 ??    LDA #$??")))))

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
        assembly (asm/artifact->assembly artifact)]
    (is (= 1 (count (re-seq #"(?m)^; block " assembly))))
    (is (.contains assembly "$D012  A9 ??    LDA #$??"))))

