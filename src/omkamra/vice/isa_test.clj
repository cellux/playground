(ns omkamra.vice.isa-test
  "Tests for the shared 6510 vocabulary: mode metadata, documented vs
  undocumented status, and opcode selection used by both the assembler and
  the disassembler."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.isa :as isa]))

(deftest opcode-table-is-complete
  (is (= 256 (count isa/opcode-table)))
  (testing "every opcode is indexed exactly once"
    (is (= 256 (reduce + (map count (vals isa/mnemonic-mode-opcodes)))))))

(deftest mnemonic-mode-index-covers-the-table
  (testing "the selector index reconstructs the table"
    (is (= (map-indexed (fn [opcode {:keys [mnemonic mode]}]
                          [opcode [mnemonic mode]])
                        isa/opcode-table)
           (sort-by first
                    (for [[[mnemonic mode] opcodes] isa/mnemonic-mode-opcodes
                          opcode opcodes]
                      [opcode [mnemonic mode]])))))
  (testing "modes-for agrees with the index"
    (doseq [[mnemonic mode] (keys isa/mnemonic-mode-opcodes)]
      (is (contains? (isa/modes-for mnemonic) mode)))))

(deftest mode-metadata-is-consistent
  (doseq [[mode width] isa/mode-width]
    (is (contains? isa/mode-operand-width mode))
    (is (= width (inc (long (isa/mode-operand-width mode))))
        (str mode " operand bytes plus opcode equal the instruction width"))
    (is (contains? isa/mode-operand-kind mode)))
  (is (= isa/memory-modes
         (into #{} (remove #{:imp :acc :imm :rel} (keys isa/mode-width))))))

(deftest documented-opcodes
  (testing "the legal NMOS 6502 opcode count is stable"
    (is (= 151 (count isa/documented-opcodes))))
  (testing "documented mnemonics are exactly those with a legal opcode"
    (is (= isa/documented-mnemonics
           (into #{}
                 (keep (fn [[opcode {:keys [mnemonic]}]]
                         (when (contains? isa/documented-opcodes opcode)
                           mnemonic)))
                 (map-indexed vector isa/opcode-table)))))
  (testing "the duplicate SBC immediate selects the documented opcode"
    (is (= 0xe9 (isa/preferred-opcode "SBC" :imm)))
    (is (not= 0xeb (isa/preferred-opcode "SBC" :imm))))
  (testing "the documented NOP is implied, undocumented variants remain"
    (is (= 0xea (isa/preferred-opcode "NOP" :imp)))
    (is (isa/documented-mnemonic? "NOP"))
    (is (not (isa/documented-opcode? 0x1a)))
    (is (isa/valid-opcode? "NOP" 0x1a)))
  (testing "undocumented mnemonics never select a documented opcode"
    (doseq [mnemonic ["KIL" "SLO" "RLA" "LAX" "SAX" "DCP" "ISC" "ANC"
                      "ALR" "ARR" "XAA" "AXS" "AHX" "SHY" "SHX" "TAS"
                      "LAS"]]
      (is (not (isa/documented-mnemonic? mnemonic)) mnemonic))))

(deftest preferred-opcode-prefers-documented
  (doseq [[[mnemonic mode] opcodes] isa/mnemonic-mode-opcodes]
    (let [preferred (isa/preferred-opcode mnemonic mode)]
      (is (isa/valid-opcode? mnemonic preferred))
      (is (contains? (set opcodes) preferred))
      (when-let [documented (first (filter isa/documented-opcode? opcodes))]
        (is (= documented preferred)
            (str mnemonic " " mode " selects the documented opcode"))
        (is (isa/documented-opcode? preferred)
            (str mnemonic " " mode " selected a documented opcode"))))))

(deftest branch-classification
  (doseq [mnemonic ["BPL" "BMI" "BVC" "BVS" "BCC" "BCS" "BNE" "BEQ"]]
    (is (isa/branch? mnemonic)))
  (doseq [mnemonic ["JMP" "JSR" "NOP" "LDA"]]
    (is (not (isa/branch? mnemonic)))))
