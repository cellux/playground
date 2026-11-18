(ns omkamra.assembler-test
  "Tests for the ISA-neutral assembler and disassembler API.

  Everything here goes through the public multimethods and the target
  descriptor contract, so the tests stay valid for any target. The 6502
  dialect's own behaviour is covered by `omkamra.assembler.6502-test`."
  (:require
   [clojure.test :refer [deftest is testing]]
   [omkamra.assembler :as assembler]
   [clojure.string :as str]
   [omkamra.assembler.core :as core]
   [omkamra.assembler.6502 :as mos6502]))

(def target-keys
  "The target-descriptor keys the generic assembler reads."
  [:isa :address-max :byte-order :register-names :memory-modes
   :mode-operand-width :mnemonic-symbol? :infer-mode :instruction-item
   :decode-instructions :serialize-asm])

;; A second, deliberately tiny target: enough to prove that dispatch and the
;; ISA boundary are real, and to build a foreign object for the mismatch tests.
(defmethod assembler/target ::toy
  [_]
  {:isa ::toy
   :address-max 0xffff
   :byte-order :big-endian
   :register-names #{}
   :memory-modes #{}
   :mode-operand-width {}
   :mnemonic-symbol? (fn [x] (and (symbol? x) (= "nop" (name x))))
   :infer-mode (fn [_ _] :imp)
   :instruction-item (fn [_target _form _index meta]
                       {:type :instruction
                        :mnemonic "NOP"
                        :mode :imp
                        :opcode 0
                        :bytes [0]
                        :size 1
                        :meta meta
                        :relocations nil})
   :decode-instructions (fn [_ {:keys [address bytes]}]
                          (mapv (fn [offset]
                                  {:pc (+ (long address) (long offset))
                                   :opcode (bit-and (int (nth bytes offset)) 0xff)
                                   :bytes [(bit-and (int (nth bytes offset)) 0xff)]
                                   :mnemonic "NOP"
                                   :mode :imp
                                   :width 1})
                                (range (count bytes))))
   :serialize-asm (fn [value _] (str "; toy " (:kind value)))})

;; Target contract -----------------------------------------------------------

(deftest the-6502-target-describes-itself
  (let [target (assembler/target {:isa :6502})]
    (is (= :6502 (:isa target)))
    (doseq [k target-keys]
      (is (contains? target k) (str "the target descriptor defines " k)))))

(deftest an-unknown-isa-is-a-structured-error
  (is (= :unknown-isa
         (try (assembler/target {:isa ::no-such-isa}) nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
  (testing "an unknown ISA fails when assembling, not silently"
    (is (= :unknown-isa
           (try
             (assembler/assemble {:kind :asm :isa ::no-such-isa :source []})
             nil
             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

;; Deferred descriptors ------------------------------------------------------

(deftest asm-descriptor-shape
  (let [descriptor (assembler/asm-descriptor
                    :6502 '((rts)) {:namespace 'omkamra.assembler-test})]
    (is (assembler/descriptor? descriptor))
    (is (= :asm (:kind descriptor)))
    (is (= :6502 (:isa descriptor)))
    (is (= 'omkamra.assembler-test (:namespace descriptor)))
    (is (nil? (:object-id descriptor)))))

(deftest assemble-rejects-non-descriptors
  (is (= :not-a-descriptor
         (try (assembler/assemble '((rts))) nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))

(deftest assemble-rejects-an-isa-mismatch
  ;; Dispatch itself guarantees that `assemble` picks the descriptor's own
  ;; target, so the guard is exercised directly.
  (let [descriptor (assembler/asm-descriptor ::toy '((nop)) {})]
    (is (= :isa-mismatch
           (try
             (core/assemble-graph (assembler/target {:isa :6502}) descriptor)
             nil
             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest an-anonymous-descriptor-can-be-the-root
  (let [descriptor (mos6502/asm* '((%org 0xc000) (%entry :start) :start (rts))
                                 {:namespace 'omkamra.assembler-test})
        graph (assembler/assemble descriptor)
        artifact (assembler/link graph {:regions [{:start 0xc000 :end 0xd000}]
                                        :reserved []})]
    (is (= :omkamra.assembler.core/root (:root graph)))
    (is (= 0xc000 (:entry-point artifact)))))

;; Object graphs -------------------------------------------------------------

(mos6502/define-asm cyclic-a
  (%entry :a)
  :a
  (jsr cyclic-b)
  (rts))

(mos6502/define-asm cyclic-b
  (%entry :b)
  :b
  (jsr cyclic-a)
  (rts))

(deftest mutually-recursive-objects-form-a-cycle-in-one-graph
  (let [graph (assembler/assemble cyclic-a)]
    (is (= #{:omkamra.assembler-test/cyclic-a :omkamra.assembler-test/cyclic-b}
           (set (keys (:objects graph))))
        "both objects are snapshotted once, not nested recursively")
    (is (= [:omkamra.assembler-test/b]
           (:objects (:dependencies (assembler/object-graph->object graph))))
        "the reference names the dependency's entry label")
    (testing "and the cycle links"
      (is (assembler/artifact?
           (assembler/link graph {:regions [{:start 0xc000 :end 0xd000}]
                                  :reserved []}))))))

(deftest serialize-data-renders-a-graph-by-identity
  (let [graph (assembler/assemble cyclic-a)
        text (assembler/serialize-data graph)]
    (is (string? text))
    (is (str/includes? text ":root"))
    (testing "the rendering terminates and round-trips as data"
      (let [data (read-string text)]
        (is (= #{:omkamra.assembler-test/cyclic-a :omkamra.assembler-test/cyclic-b}
               (set (keys (:objects data)))))
        (is (= :omkamra.assembler-test/cyclic-a (:root data)))))))

(def ^:private foreign-object
  "A toy-ISA object exporting a label a 6502 object can be made to reference."
  {:kind :object
   :isa ::toy
   :object-id :omkamra.assembler-test/foreign
   :namespace 'omkamra.assembler-test
   :source '[(nop)]
   :sections [{:index 0 :base :object :align 1 :size 1
               :items [{:type :instruction :bytes [0] :size 1
                        :meta nil :relocations []}]
               :labels {} :relocations []}]
   :labels {}
   :exports [:foreign-here]
   :relocations []
   :constraints {}
   :entries []
   :entry :foreign-here
   :dependencies {:objects [] :assets []}})

(def ^:private foreign-label :omkamra.assembler-test/foreign-here)

(deftest a-graph-with-two-isas-is-rejected
  ;; The foreign object exports a label the 6502 object references, so the
  ;; walk reaches it and the ISA guard fires.
  (let [cross-isa (fn []
                    (-> (mos6502/asm* (list (list '%org 0xc000)
                                            (list 'jsr foreign-label)
                                            '(rts))
                                      {:namespace 'omkamra.assembler-test})
                        assembler/assemble))]
    (is (= :isa-mismatch
           (try (cross-isa) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (testing "an unreferenced foreign object is simply not part of the graph"
      (is (assembler/artifact?
           (assembler/link
            (assembler/assemble
             (mos6502/asm* '((%org 0xc000) (rts))
                           {:namespace 'omkamra.assembler-test}))
            {:objects [foreign-object]
             :regions [{:start 0xc000 :end 0xd000}]
             :reserved []}))))))

(deftest link-is-idempotent-and-strict
  (let [artifact (assembler/build
                  (mos6502/asm* '((%org 0xc000) (rts))
                                {:namespace 'omkamra.assembler-test})
                  {:regions [{:start 0xc000 :end 0xd000}]})]
    (is (identical? artifact (assembler/link artifact)))
    (is (= :not-linkable
           (try (assembler/link {:kind :nonsense}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

;; Multimethod dispatch ------------------------------------------------------

(deftest serialize-asm-dispatches-on-the-isa
  (let [toy (assembler/asm-descriptor ::toy '((nop)) {})]
    (is (= "; toy :asm" (assembler/serialize-asm toy)))
    (is (re-find #"RTS" (assembler/serialize-asm
                         (mos6502/asm* '((rts))
                                       {:namespace 'omkamra.assembler-test}))))))

(deftest build-is-assemble-then-link
  (let [descriptor (mos6502/asm* '((%org 0x2000) (%db [1 2 3]))
                                 {:namespace 'omkamra.assembler-test})
        built (assembler/build descriptor {:regions [{:start 0x2000 :end 0x3000}]})]
    (is (assembler/artifact? built))
    (is (= [{:start-address 0x2000 :bytes [1 2 3]}] (:segments built)))))

;; Data directives -----------------------------------------------------------

(deftest wide-data-directives-are-generic
  (let [descriptor (mos6502/asm* '((%db [0x12])
                                   (%dw [0x1234])
                                   (%dd [0x12345678])
                                   (%dq [0x0102030405060708]))
                                 {:namespace 'omkamra.assembler-test})
        object (-> (assembler/assemble descriptor) assembler/object-graph->object)
        bytes (vec (mapcat :bytes (:items (first (:sections object)))))]
    (is (= [0x12
            0x34 0x12
            0x78 0x56 0x34 0x12
            0x08 0x07 0x06 0x05 0x04 0x03 0x02 0x01]
           bytes)
        "multi-byte directives emit little-endian by default")))

(deftest wide-data-directives-check-their-range
  (doseq [source ['((%dw [0x10000]))
                  '((%dd [0x100000000]))
                  '((%db [256]))]]
    (is (= :operand-out-of-range
           (try
             (assembler/assemble (mos6502/asm* source
                                               {:namespace 'omkamra.assembler-test}))
             nil
             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        (pr-str source))))

;; ISA-neutral assets --------------------------------------------------------

(assembler/define-resource ^{:at 0xc100} bytes-resource
  "omkamra/vice/assembler_test/bytes.bin")

(deftest define-resource-resolves-classpath-paths
  (is (assembler/asset? bytes-resource))
  (is (= "omkamra/vice/assembler_test/bytes.bin" (:resource bytes-resource)))
  (testing "the bytes are read when the graph is assembled"
    (let [artifact (assembler/link
                    (assembler/assemble
                     (mos6502/asm* '((%org 0xc100)
                                     (%include bytes-resource))
                                   {:namespace 'omkamra.assembler-test}))
                    {:regions [{:start 0xc100 :end 0xc200}]
                     :reserved []})
          segment (first (:segments artifact))]
      (is (= [1 2 3 255] (:bytes segment)))
      (is (= {:at 0xc100} (:constraints (first (:assets artifact))))))))

(deftest assets-do-not-carry-an-isa
  (is (nil? (:isa bytes-resource))
      "the same resource can be included by any target"))

(deftest build-asset-normalizes-data-and-placement
  (is (= {:kind :asset :name 't :data [1 2 3] :size 3
          :constraints {:align 4}}
         (assembler/build-asset 't {:data [1 2 3] :align 4})))
  (testing "an out-of-range byte is rejected"
    (is (= :asset-byte-out-of-range
           (try (assembler/build-asset 't {:data [256]}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

;; Disassembler --------------------------------------------------------------

(deftest disassembly-is-a-linear-sweep-of-structured-data
  (let [instructions (assembler/disassemble {:isa :6502
                                             :address 0x0800
                                             :bytes [0xa9 0x2a 0x8d 0x00 0x20]})]
    (is (= [{:pc 0x0800 :opcode 0xa9 :mnemonic "LDA" :mode :imm
             :operand 0x2a :operand-kind :immediate :width 2
             :bytes [0xa9 0x2a]}
            {:pc 0x0802 :opcode 0x8d :mnemonic "STA" :mode :abs
             :operand 0x2000 :operand-kind :absolute :width 3
             :bytes [0x8d 0x00 0x20]}]
           instructions))))

(deftest the-6502-wrapper-adds-its-isa
  (is (= (assembler/disassemble {:isa :6502 :address 0 :bytes [0x60]})
         (mos6502/disassemble {:address 0 :bytes [0x60]})))
  (testing "the two-argument form takes an address and bytes"
    (is (= [{:pc 0x1000 :opcode 0x60 :mnemonic "RTS" :mode :imp
             :operand nil :operand-kind :implied :width 1 :bytes [0x60]}]
           (mos6502/disassemble 0x1000 [0x60])))))

(deftest truncated-disassembly-is-an-error
  (is (= :truncated-instruction
         (try
           (assembler/disassemble {:isa :6502 :address 0x0800
                                   :bytes [0xa9]})
           nil
           (catch clojure.lang.ExceptionInfo e
             (let [data (ex-data e)]
               (is (= 0x0800 (:address data)))
               (is (= 2 (:required data)))
               (:reason data)))))))

(deftest disassembly-of-a-foreign-target-uses-that-target
  (is (= [{:pc 0 :opcode 7 :bytes [7] :mnemonic "NOP" :mode :imp :width 1}]
         (assembler/disassemble {:isa ::toy :address 0 :bytes [7]}))))
