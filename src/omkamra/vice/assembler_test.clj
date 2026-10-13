(ns omkamra.vice.assembler-test
  "Tests for assembler source normalization, mode inference, symbol
  resolution, serialization round-tripping, and `define-asm` identity."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.assembler :as assembler]))

(def screen-address 0x0400)

(def charset-asset
  (assembler/->Asset 'omkamra.vice.assembler-test/charset
                     (vec (range 8)) nil nil))

(assembler/define-asm entry-object
  (%entry :start)
  :start
  (lda [screen-address])
  (jsr :helper)
  :helper
  (lda <:start)
  (rts))

;; Source normalization ------------------------------------------------------

(deftest quoted-source-is-normalized
  (let [object (assembler/assemble
                '((lda 0x56)
                  (lda [0x2000])
                  (%label :loop)
                  (jmp :loop))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(lda 86)
             (lda [8192])
             :loop
             (jmp :loop)]
           (:source object)))
    (is (= [:loop] (keys (:labels object))))))

(deftest single-instruction-source
  (let [object (assembler/assemble '(rts)
                                   {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(rts)] (:source object)))))

(deftest directives-are-normalized
  (let [object (assembler/assemble
                '((%db [5 10 20])
                  (%dw [0x1234])
                  (%align 256)
                  (%region [0x0800 0xa000])
                  (%org 0x4000)
                  (%entry :start)
                  :start
                  (%meta {:block-id 12} (lda [0x2000]))
                  (%meta :role :main (rts))
                  (lda (%volatile [0x2000])))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(%db [5 10 20])
             (%dw [4660])
             (%align 256)
             (%region [2048 40960])
             (%org 16384)
             (%entry :start)
             :start
             (%meta {:block-id 12} (lda [8192]))
             (%meta {:role :main} (rts))
             (lda (%volatile [8192]))]
           (:source object)))))

(deftest meta-value-annotation
  (let [object (assembler/assemble
                '((lda (%meta {:role :operand} [0x2000])))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(lda (%meta {:role :operand} [8192]))] (:source object)))))

;; Label diagnostics ---------------------------------------------------------

(deftest forward-and-backward-labels
  (let [object (assembler/assemble
                '(:a
                  (bne :b)
                  :b
                  (jmp :a))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= [:a :b] (keys (:labels object))))))

(deftest duplicate-label-diagnostic
  (let [error (try
                (assembler/assemble '(:a :a (rts))
                                    {:ns 'omkamra.vice.assembler-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :duplicate-label (:reason (ex-data error))))
    (is (= [:a] (:labels (ex-data error))))))

(deftest unresolved-label-diagnostic-keeps-location
  (let [form (with-meta '(jmp :missing) {:line 7 :column 3})
        error (try
                (assembler/assemble [form]
                                    {:ns 'omkamra.vice.assembler-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :unresolved-label (:reason (ex-data error))))
    (is (= 7 (get-in (ex-data error) [:location :line])))
    (is (= 3 (get-in (ex-data error) [:location :column])))))

;; Symbol resolution ---------------------------------------------------------

(deftest constants-are-resolved
  (let [object (assembler/assemble
                '((lda [screen-address]))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(lda [1024])] (:source object)))))

(deftest assets-become-qualified-symbols
  (let [object (assembler/assemble
                '((lda [charset-asset])
                  (%dw [charset-asset]))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[(lda [omkamra.vice.assembler-test/charset-asset])
             (%dw [omkamra.vice.assembler-test/charset-asset])]
           (:source object)))
    (is (= '[omkamra.vice.assembler-test/charset-asset]
           (:assets (:dependencies object))))))

(deftest relocation-shorthand
  (is (= '[:start (%db [(< :start)])]
         (:source (assembler/assemble
                   '(:start (%db [<:start]))
                   {:ns 'omkamra.vice.assembler-test}))))
  (is (= '[:start (lda (< :start))]
         (:source (assembler/assemble
                   '(:start (lda (< :start)))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest unresolved-symbol-diagnostic
  (let [error (try
                (assembler/assemble '((lda [does-not-exist]))
                                    {:ns 'omkamra.vice.assembler-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :unresolved-symbol (:reason (ex-data error))))))

;; Mode inference ------------------------------------------------------------

(deftest mode-inference
  (testing "immediate, absolute, zero page, and indexed"
    (is (= :imm (assembler/infer-mode 'lda 0x56)))
    (is (= :abs (assembler/infer-mode 'lda '[0x2000])))
    (is (= :zp (assembler/infer-mode 'lda '[0x20])))
    (is (= :zp (assembler/infer-mode 'lda '[%zp 0x2000])))
    (is (= :zpx (assembler/infer-mode 'lda '[0x20 x])))
    (is (= :absx (assembler/infer-mode 'lda '[0x2000 x])))
    (is (= :zpy (assembler/infer-mode 'ldx '[0x20 y])))
    (is (= :absy (assembler/infer-mode 'ldx '[0x2000 y]))))
  (testing "indirect memory modes"
    (is (= :indx (assembler/infer-mode 'lda '[[0x20 x]])))
    (is (= :indy (assembler/infer-mode 'lda '[[0x20] y]))))
  (testing "indirect versus direct JMP"
    (is (= :abs (assembler/infer-mode 'jmp 0x2000)))
    (is (= :abs (assembler/infer-mode 'jmp :label)))
    (is (= :ind (assembler/infer-mode 'jmp '[0x2000])))
    (is (= :ind (assembler/infer-mode 'jmp '[:label]))))
  (testing "accumulator, implied, and relative"
    (is (= :acc (assembler/infer-mode 'asl 'a)))
    (is (= :imp (assembler/infer-mode 'nop nil)))
    (is (= :rel (assembler/infer-mode 'bne :loop))))
  (testing "dotted mnemonics take their mode from the exact opcode"
    (is (= :imp (assembler/infer-mode 'nop.3a nil))))
  (testing "unsupported shapes are rejected"
    (is (thrown? clojure.lang.ExceptionInfo (assembler/infer-mode 'lda nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (assembler/infer-mode 'ldy '[0x20 y])))))

;; Serialization -------------------------------------------------------------

(deftest external-label-dependencies
  (let [object (assembler/assemble
                '((jsr :omkamra.vice.examples.common/clear-screen))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[:omkamra.vice.examples.common/clear-screen]
           (:objects (:dependencies object))))))

(deftest entry-option-overrides-directive
  (let [object (assembler/assemble
                '((%entry :a) :a :b (rts))
                {:ns 'omkamra.vice.assembler-test :entry :b})]
    (is (= :b (:entry object)))))

(deftest raw-opcode-directive
  (is (= '[(%opcode 234)]
         (:source (assembler/assemble
                   '((%opcode 0xea))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest serialize-clj-round-trips
  (let [object (assembler/assemble
                '((%entry :start)
                  :start
                  (lda 0x56)
                  (lda [0x2000])
                  (asl a)
                  (bne :loop)
                  :loop
                  (jmp [0x2000])
                  (rts))
                {:ns 'omkamra.vice.assembler-test})
        text (assembler/serialize-clj object)
        reread (read-string text)
        object2 (assembler/assemble reread
                                    {:ns 'omkamra.vice.assembler-test})]
    (is (= (:source object) (:source object2)))
    (is (= (:labels object) (:labels object2)))
    (is (= (:entry object) (:entry object2)))))

(deftest serialize-clj-accepts-raw-forms
  (let [forms '[(lda 0x56) (rts)]]
    (is (= (assembler/serialize-clj forms)
           (assembler/serialize-clj {:kind :object :source forms})))))

(deftest define-asm-identity
  (is (= :omkamra.vice.assembler-test/entry-object (:object-id entry-object)))
  (is (= 'omkamra.vice.assembler-test (:namespace entry-object)))
  (is (= :start (:entry entry-object)))
  (is (identical? entry-object
                  (get @assembler/object-registry
                       :omkamra.vice.assembler-test/entry-object)))
  (is (= '[(%entry :start)
           :start
           (lda [1024])
           (jsr :helper)
           :helper
           (lda (< :start))
           (rts)]
         (:source entry-object))))

;; SCI expression evaluation and assets --------------------------------------

(def twice (fn [x] (* 2 x)))

(def magic 0x42)

(assembler/define-asset inline-asset [0 1 2 255])

(assembler/define-asset resource-asset
  {:resource "omkamra/vice/assembler_test/bytes.bin"
   :constraints {:alignment 256}})

(deftest expressions-evaluate
  (is (= '[(lda 3)
           (lda [1028])
           (%db [10 255 52 18])
           (%dw [4660])
           (%align 256)]
         (:source (assembler/assemble
                   '((lda (+ 1 2))
                     (lda [(+ 1024 4)])
                     (%db [(+ 5 5) (u8 0x1ff) (lo 0x1234) (hi 0x1234)])
                     (%dw [(word 0x34 0x12)])
                     (%align (* 2 128)))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest host-symbols-resolve-in-expressions
  (is (= '[(lda 84) (lda [66])]
         (:source (assembler/assemble
                   '((lda (twice 42)) (lda [magic]))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest multi-operand-relocation-operators-are-comparisons
  (is (= '[(lda 1)]
         (:source (assembler/assemble
                   '((lda (if (< 1 2) 1 0)))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest expression-errors-keep-location
  (let [inner (list 'no-such-fn 1)
        form (with-meta (list 'lda inner) {:line 11 :column 5})
        error (try
                (assembler/assemble [form]
                                    {:ns 'omkamra.vice.assembler-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :expression-error (:reason (ex-data error))))
    (is (= 0 (get-in (ex-data error) [:location :index])))))

(deftest jvm-interop-and-eval-are-blocked
  (doseq [source ['((lda (System/exit 0)))
                  '((lda (eval '(+ 1 2))))
                  '((lda (slurp "x")))]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (assembler/assemble source
                                     {:ns 'omkamra.vice.assembler-test})))))

(deftest inline-assets
  (is (= [0 1 2 255]
         (mapv (fn [b] (bit-and (int b) 0xff)) (seq (:data inline-asset)))))
  (is (= '[(lda [omkamra.vice.assembler-test/inline-asset])]
         (:source (assembler/assemble
                   '((lda [inline-asset]))
                   {:ns 'omkamra.vice.assembler-test})))))

(deftest resource-assets
  (is (= [1 2 3 255]
         (mapv (fn [b] (bit-and (int b) 0xff)) (seq (:data resource-asset)))))
  (is (= {:alignment 256} (:constraints resource-asset)))
  (is (= "omkamra/vice/assembler_test/bytes.bin" (:resource resource-asset))))

(deftest asset-dependencies
  (let [object (assembler/assemble
                '((lda [inline-asset]) (%dw [resource-asset]))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= '[omkamra.vice.assembler-test/inline-asset
             omkamra.vice.assembler-test/resource-asset]
           (:assets (:dependencies object))))))

;; Code generation -----------------------------------------------------------

(defn- section-bytes [object]
  (vec (mapcat :bytes (:items (first (:sections object))))))

(defn- relocations [object]
  (:relocations object))

(defn- assemble-only [source]
  (assembler/assemble source {:ns 'omkamra.vice.assembler-test}))

(deftest instruction-encodings
  (is (= [0xa9 0x56] (section-bytes (assemble-only '((lda 0x56))))))
  (is (= [0xad 0x00 0x20] (section-bytes (assemble-only '((lda [0x2000]))))))
  (is (= [0xa5 0x20] (section-bytes (assemble-only '((lda [0x20]))))))
  (is (= [0xa5 0x20] (section-bytes (assemble-only '((lda [%zp 0x20]))))))
  (is (= [0xb5 0x20] (section-bytes (assemble-only '((lda [%zp 0x20 x]))))))
  (is (= [0xbd 0x00 0x20] (section-bytes (assemble-only '((lda [0x2000 x]))))))
  (is (= [0xbe 0x00 0x20] (section-bytes (assemble-only '((ldx [0x2000 y]))))))
  (is (= [0xa1 0x20] (section-bytes (assemble-only '((lda [[0x20 x]]))))))
  (is (= [0xb1 0x20] (section-bytes (assemble-only '((lda [[0x20] y]))))))
  (is (= [0x4c 0x00 0x20] (section-bytes (assemble-only '((jmp 0x2000))))))
  (is (= [0x6c 0x00 0x20] (section-bytes (assemble-only '((jmp [0x2000]))))))
  (is (= [0x0a] (section-bytes (assemble-only '((asl a))))))
  (is (= [0xea] (section-bytes (assemble-only '((nop))))))
  (is (= [0x3a] (section-bytes (assemble-only '((nop.3a))))))
  (is (= [0x3a] (section-bytes (assemble-only '((%opcode 0x3a))))))
  (is (= [0xd0 0x00 0x60]
         (section-bytes (assemble-only '((bne :loop) :loop (rts)))))))

(deftest byte-and-word-directives
  (is (= [1 2 255] (section-bytes (assemble-only '((%db [1 2 255]))))))
  (is (= [0x34 0x12] (section-bytes (assemble-only '((%dw [0x1234]))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (assemble-only '((%db [256])))))
  (is (thrown? clojure.lang.ExceptionInfo
               (assemble-only '((%dw [65536])))))
  (is (thrown? clojure.lang.ExceptionInfo
               (assemble-only '((lda 0x100))))))

(deftest alignment-and-org
  (let [object (assemble-only '((%db [1]) (%align 4) (%db [2])))]
    (is (= [1 0 0 0 2] (section-bytes object)))
    (is (= 4 (:alignment (first (:sections object))))))
  (let [object (assemble-only '((%db [1]) (%org 0x4000) (%db [2])))]
    (is (= [0x4000] (mapv :base (rest (:sections object)))))
    (is (= [2] (vec (:bytes (first (:items (second (:sections object)))))))))
  (is (= [{:start 2048 :end 40960}]
         (:regions (:constraints
                    (assemble-only '((%region [0x0800 0xa000]) (rts)))))))
  (is (= [{:start 2048}]
         (:regions (:constraints
                    (assemble-only '((%region 0x0800) (rts))))))))

(deftest relocation-records
  (let [object (assemble-only '(:start
                                (lda [:start])
                                (lda <:start)
                                (lda >:start)
                                (bne :start)
                                (jsr :omkamra.vice.examples.common/clear-screen)
                                (%dw [:start])))
        relocations (relocations object)]
    (is (= '[:address :low :high :relative :address :address]
           (mapv :kind relocations)))
    (is (= [2 1 1 1 2 2] (mapv :width relocations)))
    (is (= {:type :label :name :start} (:target (first relocations))))
    (is (= {:type :external
            :name :omkamra.vice.examples.common/clear-screen}
           (:target (nth relocations 4))))
    (is (= {:type :label :name :start} (:target (last relocations))))))

(deftest invalid-dotted-mnemonic-is-rejected
  (is (thrown? clojure.lang.ExceptionInfo (assemble-only '((nop.zz)))))
  (is (thrown? clojure.lang.ExceptionInfo (assemble-only '((lda (nop.3a)))))))

(deftest serialize-asm-rendering
  (let [object (assemble-only '(:start
                                (lda 0x56)
                                (%db [1 2])
                                (%org 0x4000)
                                :fixed
                                (rts)))
        text (assembler/serialize-asm object)]
    (is (str/includes? text "LDA #$56"))
    (is (str/includes? text ".byte $01, $02"))
    (is (str/includes? text ".org $4000"))
    (is (str/includes? text "fixed:"))))

;; Linker --------------------------------------------------------------------

(defn- error-reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- link-forms [source options]
  (assembler/link (assemble-only source) options))

(deftest link-primary-object
  (let [artifact (link-forms '((%entry :start) :start (lda 0x01) (rts)) {})]
    (is (assembler/artifact? artifact))
    (is (= 0x800 (:entry-point artifact)))
    (is (= 1 (count (:segments artifact))))
    (is (= [0xa9 1 0x60]
           (mapv (fn [b] (bit-and (int b) 0xff))
                 (:bytes (first (:segments artifact))))))
    (is (= {:start 2048} (:labels (first (:objects artifact)))))
    (is (identical? artifact (assembler/link artifact)))))

(deftest link-discovers-object-dependencies
  (let [common (assembler/assemble
                '(:clear-screen-fn (rts))
                {:ns 'omkamra.vice.assembler-test :object-id :t/common})
        main (assembler/assemble
              '((%entry :start)
                :start
                (jsr :omkamra.vice.assembler-test/clear-screen-fn)
                (rts))
              {:ns 'omkamra.vice.assembler-test})
        artifact (assembler/link main {:objects [common]})]
    (is (= 2 (count (:objects artifact))))
    (is (= [:t/common nil]
           (sort-by (fn [id] (if id 0 1))
                    (map :object-id (:objects artifact)))))
    (is (= 0x800 (:entry-point artifact)))
    (is (= 0x804 (get (:labels (second (:objects artifact))) :clear-screen-fn)))))

(deftest link-rejects-ambiguous-and-unresolved-externals
  (let [ns-sym 'omkamra.vice.assembler-test
        main (assembler/assemble
              '((jsr :omkamra.vice.assembler-test/dup))
              {:ns ns-sym})
        a (assembler/assemble '(:dup (rts)) {:ns ns-sym :object-id :t/a})
        b (assembler/assemble '(:dup (rts)) {:ns ns-sym :object-id :t/b})]
    (is (= :ambiguous-external
           (error-reason #(assembler/link main {:objects [a b]}))))
    (is (= :unresolved-external
           (error-reason #(assembler/link
                           (assembler/assemble
                            '((jsr :omkamra.vice.assembler-test/nope))
                            {:ns ns-sym})
                           {}))))))

(deftest link-places-assets
  (let [artifact (link-forms '((%dw [inline-asset]) (rts)) {})]
    (is (= 1 (count (:assets artifact))))
    (is (= 'omkamra.vice.assembler-test/inline-asset
           (:name (first (:assets artifact)))))
    (is (= (:start-address (first (:assets artifact)))
           (let [bytes (:bytes (first (:segments artifact)))]
             (+ (bit-and (int (aget bytes 0)) 0xff)
                (bit-shift-left (bit-and (int (aget bytes 1)) 0xff) 8)))))))

(deftest placement
  (testing "default first-fit"
    (is (= 0x800
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms '(:s (rts)) {}))))))))))
  (testing "exact and interval regions"
    (is (= 0x4000
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms
                                               '((%region 0x4000) (rts))
                                               {})))))))))
    (is (= 0x3000
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms
                                               '((%region [0x3000 0x4000]) (rts))
                                               {}))))))))))
  (testing "range override"
    (is (= 0x5000
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms '((rts))
                                                          {:ranges [[0x5000 0x6000]]}))))))))))
  (testing "org bypasses placement"
    (let [artifact (link-forms '((%org 0x2000) :fixed (rts)) {})]
      (is (= 0x2000 (:start-address (first (:segments artifact))))))))

(deftest placement-failures
  (is (= :overlap
         (error-reason
          (fn []
            (link-forms '((%org 0x1000) (rts)
                                        (%org 0x1000) (rts)) {})))))
  (is (= :unsatisfiable-placement
         (error-reason
          (fn []
            (link-forms '((%region [0x3000 0x3001]) (%db [1 2])) {}))))))

(deftest relocation-validation
  (is (= :branch-out-of-range
         (error-reason
          (fn []
            (link-forms '((bne :far) (rts)
                                     (%org 0x2000) :far (rts)) {})))))
  (is (= :zero-page-overflow
         (error-reason
          (fn []
            (link-forms '(:s (lda [%zp :s])) {}))))))

(deftest entry-selection
  (let [object (assembler/assemble
                '((%entry :a) :a (%entry :b) :b (rts))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= :multiple-entries (error-reason #(assembler/link object))))
    (is (= 0x800 (:entry-point (assembler/link object {:entry :b}))))))
