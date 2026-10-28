(ns omkamra.vice.examples.demo-test
  "Guards the example fixture's assembly and linking without starting VICE."
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.examples.demo :as demo]))

(deftest example-links-into-one-image
  (let [artifact (assembler/link demo/border-cycle)
        bytes (vec (:bytes (first (:segments artifact))))]
    (is (= 0xC000 (:entry-point artifact)))
    (is (= 1 (count (:segments artifact))))
    (is (= 'omkamra.vice.examples.demo/border-colors
           (:name (first (:assets artifact)))))
    (is (= [0x78          ; SEI
            0xa9 0x00      ; LDA #$00
            0xa9 0x00      ; LDA #$00
            0xee 0x20 0xd0 ; INC $D020
            0x4c 0x05 0xc0 ; JMP $C005
            0x0d 0xc0]     ; .word border-colors
           (mapv (fn [b] (bit-and (int b) 0xff))
                 (subvec bytes 0 13))))))
