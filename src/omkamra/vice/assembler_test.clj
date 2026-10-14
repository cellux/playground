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
   :constraints {:align 256}})

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

(def plain-bytes
  "Inline data as a plain Clojure vector: no asset ceremony."
  [7 8 9])

(defn- linked-bytes-at
  [artifact address length]
  (let [segment (first (filter (fn [{:keys [start-address bytes]}]
                                 (<= (long start-address)
                                     (long address)
                                     (+ (long start-address) (count bytes))))
                               (:segments artifact)))]
    (mapv #(bit-and (int %) 0xff)
          (take length (drop (- (long address) (long (:start-address segment)))
                             (:bytes segment))))))

(deftest plain-defs-of-bytes-are-assets
  (is (= '[(lda [omkamra.vice.assembler-test/plain-bytes])]
         (:source (assembler/assemble
                   '((lda [plain-bytes]))
                   {:ns 'omkamra.vice.assembler-test}))))
  (testing "the linker places it like any other asset"
    (let [artifact (assembler/link
                    (assembler/assemble
                     '((%org 0x1000)
                       (lda [plain-bytes]))
                     {:ns 'omkamra.vice.assembler-test})
                    {:ranges [[0x1000 0x2000]]})
          asset (first (:assets artifact))]
      (is (= ['omkamra.vice.assembler-test/plain-bytes] (mapv :name (:assets artifact))))
      (is (= [7 8 9] (linked-bytes-at artifact (:start-address asset) 3))))))

(defn syntax-quoted-macro
  "An assembly macro in the idiomatic style: syntax-quote, no `list` calls.

  Syntax-quote qualifies its symbols, so the expansion arrives as
  `…-test/ldx`, `…-test/x` and so on. The assembler accepts a qualified
  mnemonic or directive by name (and canonicalises a qualified index
  register), which is what makes macros writable this way."
  [label table]
  `[(ldx 0x00)
    ~label
    (lda [~table x])
    (iny)
    (bne ~label)])

(deftest syntax-quoted-macros-assemble
  (is (= '[(ldx 0)
           :loop
           (lda [omkamra.vice.assembler-test/inline-asset x])
           (iny)
           (bne :loop)]
         (:source (assembler/assemble
                   '((syntax-quoted-macro :loop inline-asset))
                   {:ns 'omkamra.vice.assembler-test}))))
  (testing "the qualified index register still selects absolute,X"
    (let [artifact (assembler/link
                    (assembler/assemble
                     '((%org 0x1000)
                       (syntax-quoted-macro :loop inline-asset))
                     {:ns 'omkamra.vice.assembler-test})
                    {:ranges [[0x1000 0x2000]]})]
      (is (= [0xa2 0x00                 ; LDX #$00
              0xbd]                     ; LDA abs,X
             (mapv #(bit-and (int %) 0xff)
                   (take 3 (:bytes (first (:segments artifact))))))))))

(deftest resource-assets
  (is (= [1 2 3 255]
         (mapv (fn [b] (bit-and (int b) 0xff)) (seq (:data resource-asset)))))
  (is (= {:align 256} (:constraints resource-asset)))
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
    (is (= 4 (:align (first (:sections object))))))
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
    (is (= 0x900 (:entry-point artifact)))
    (is (= 1 (count (:segments artifact))))
    (is (= [0xa9 1 0x60]
           (mapv (fn [b] (bit-and (int b) 0xff))
                 (:bytes (first (:segments artifact))))))
    (is (= {:start 0x900} (:labels (first (:objects artifact)))))
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
    (is (= 0x900 (:entry-point artifact)))
    (is (= 0x904 (get (:labels (second (:objects artifact))) :clear-screen-fn)))))

;; Object entries ------------------------------------------------------------

(assembler/define-asm default-entry-object
  (lda 0x01)
  (rts))

(assembler/define-asm default-entry-caller
  (%entry :start)
  :start
  (jsr default-entry-object)
  (rts))

(def data-only-object
  (assembler/assemble '((%db [1 2 3]))
                      {:ns 'omkamra.vice.assembler-test
                       :object-id :omkamra.vice.assembler-test/data-only}))

(deftest define-asm-adds-a-default-entry-label
  (is (nil? (:entry default-entry-object)))
  (is (= {:section 0 :offset 0}
         (get (:labels default-entry-object) :default-entry-object)))
  (testing "an object that declares %entry gets no default label"
    (is (nil? (get (:labels entry-object) :entry-object)))))

(deftest object-vars-resolve-to-their-entry
  (testing "the Var reference becomes an external label for the entry"
    (is (some #{'(jsr :omkamra.vice.assembler-test/default-entry-object)}
              (:source default-entry-caller)))
    (is (= [:omkamra.vice.assembler-test/default-entry-object]
           (:objects (:dependencies default-entry-caller)))))
  (testing "and links to the entry label's address"
    (let [artifact (assembler/link default-entry-caller)
          handler (first (filter #(= :omkamra.vice.assembler-test/default-entry-object
                                     (:object-id %))
                                 (:objects artifact)))]
      (is (some? handler))
      (is (= 0x904 (get (:labels handler) :default-entry-object))))))

(deftest object-name-label-must-name-the-first-instruction
  (is (= :object-entry-label-mismatch
         (error-reason
          #(assembler/assemble '((rts) :irq (rts))
                               {:ns 'omkamra.vice.assembler-test
                                :object-id :omkamra.vice.assembler-test/irq}))))
  (testing "naming the first instruction is accepted"
    (is (= [:irq]
           (keys (:labels
                  (assembler/assemble '(:irq (rts))
                                      {:ns 'omkamra.vice.assembler-test
                                       :object-id :omkamra.vice.assembler-test/irq})))))))

(deftest object-without-entry-cannot-be-referenced
  (is (nil? (get (:labels data-only-object) :data-only)))
  (is (= :no-object-entry
         (error-reason
          #(assembler/assemble '((lda (< data-only-object)))
                               {:ns 'omkamra.vice.assembler-test})))))

;; %labels -------------------------------------------------------------------

(deftest labels-directive-renames-its-labels
  (let [object (assembler/assemble
                '((%labels [:loop] :loop (inx) (bne :loop)))
                {:ns 'omkamra.vice.assembler-test})
        [label inx bne] (:source object)]
    (is (keyword? label))
    (is (not= :loop label))
    (is (= '(inx) inx))
    (is (= label (second bne)))
    (is (= [label] (keys (:labels object))))))

(deftest labels-directive-is-unique-per-use
  (let [object (assembler/assemble
                '((%labels [:loop] :loop (inx) (bne :loop))
                  (%labels [:loop] :loop (dex) (bne :loop)))
                {:ns 'omkamra.vice.assembler-test})
        labels (keys (:labels object))]
    (is (= 2 (count labels)))
    (is (= 2 (count (distinct labels))))))

(deftest labels-directive-scopes-its-body
  (let [object (assembler/assemble
                '((%labels [:loop] :loop (bne :loop))
                  :loop)
                {:ns 'omkamra.vice.assembler-test})
        labels (set (keys (:labels object)))]
    (is (contains? labels :loop) "the outer label is untouched")
    (is (= 2 (count labels)) "and the scoped one is separate")))

(deftest labels-directive-validates-its-names
  (is (= :invalid-labels
         (error-reason
          (fn [] (assembler/assemble '((%labels :loop (inx)))
                                     {:ns 'omkamra.vice.assembler-test}))))))

;; %reserve ------------------------------------------------------------------

(def reserve-anchor [1 2 3 4 5 6 7 8])

(deftest reserve-records-its-range
  (let [object (assembler/assemble
                '((%org 0x9000) :start (%reserve 0x0800 0x100) (rts))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= [[0x0800 0x0900]] (:reserved (:constraints object))))))

(deftest reserve-accepts-a-range-vector
  (let [object (assembler/assemble
                '((%org 0x9000) :start (%reserve [0x0800 0x0900]) (rts))
                {:ns 'omkamra.vice.assembler-test})]
    (is (= [[0x0800 0x0900]] (:reserved (:constraints object))))))

(deftest reserve-keeps-the-linker-out-of-the-range
  (let [object (assembler/assemble
                '((%org 0x9000) :start (%reserve 0x0800 0x100)
                                (lda [reserve-anchor])
                                (rts))
                {:ns 'omkamra.vice.assembler-test
                 :object-id :omkamra.vice.assembler-test/reserved})
        artifact (assembler/link object)
        anchor (first (filter #(= 'omkamra.vice.assembler-test/reserve-anchor
                                  (:name %))
                              (:assets artifact)))]
    (is (= 0x0900 (:start-address anchor))
        "the floating asset skips the reserved range")))

(deftest reserve-validates-its-arguments
  (is (= :invalid-reserve
         (error-reason
          (fn [] (assembler/assemble '((%reserve :nope 4))
                                     {:ns 'omkamra.vice.assembler-test}))))))

(deftest reserve-rejects-overlap
  (is (= :overlap
         (error-reason
          (fn [] (assembler/link
                  (assembler/assemble '((%org 0xC000) :start (%reserve 0xC000 16) (rts))
                                      {:ns 'omkamra.vice.assembler-test})))))))

;; implicit-asset alignment ---------------------------------------------------

(def ^{:align 256} aligned-inline [1 2 3 4 5 6 7 8])

(deftest implicit-asset-honours-alignment-metadata
  (let [object (assembler/assemble
                '((%org 0x2000) (%reserve 0x0800 2) (lda [aligned-inline,x]))
                {:ns 'omkamra.vice.assembler-test})
        artifact (assembler/link object)
        asset (first (filter #(= 'omkamra.vice.assembler-test/aligned-inline
                                 (:name %))
                             (:assets artifact)))]
    (is (= 0 (mod (:start-address asset) 256))
        "the inline table aligns itself to a page")))

(def ^{:align true} nine-bytes (vec (repeat 9 1)))
(def ^{:align true} twenty-one-bytes (vec (repeat 21 1)))

(defn- placed-asset
  "The linked start address of implicit asset `sym` in a small object."
  [sym]
  (let [object (assembler/assemble
                (list '(%org 0x4000) '(%reserve 0x0800 2) (list 'lda (vector sym 'x)))
                {:ns 'omkamra.vice.assembler-test
                 :object-id (keyword "omkamra.vice.assembler-test"
                                     (str "align-" (name sym)))})
        artifact (assembler/link object)
        qualified (symbol "omkamra.vice.assembler-test" (name sym))]
    (:start-address (first (filter #(= qualified (:name %)) (:assets artifact))))))

(deftest align-true-uses-the-next-power-of-two
  (is (= 0x0900 (placed-asset 'nine-bytes)) "9 bytes -> align 16")
  (is (= 0x0900 (placed-asset 'twenty-one-bytes)) "21 bytes -> align 32")
  (is (= {:align 32}
         (:constraints (assembler/build-asset 't {:data (vec (repeat 21 1))
                                                  :align true})))
      "define-asset accepts :align true too"))

;; string assets --------------------------------------------------------------

(def ^{:charset :screen-codes :at 0x3000} screen-coded-string "AB C1:9'Z")
(def ^{:charset :petscii :at 0x3100} petscii-coded-string "Ab1!")
(def ^{:at 0x3200} ascii-coded-string "Hi!")
(def ^{:align true} aligned-string "abc")
(def ^{:charset :ebcdic} unknown-charset-string "hi")
(def ^{:charset :screen-codes} unencodable-string "ok\ttab")
(def empty-string-asset "")

(defn- asset-bytes
  "The linked bytes of asset `sym`, which must be pinned alone at its address."
  [sym]
  (let [object (assembler/assemble
                (list '(%org 0x4000) (list 'lda (list '< sym)) (list 'ldx (list '> sym)))
                {:ns 'omkamra.vice.assembler-test
                 :object-id (keyword "omkamra.vice.assembler-test"
                                     (str "string-" (name sym)))})
        artifact (assembler/link object)
        qualified (symbol "omkamra.vice.assembler-test" (name sym))
        address (:start-address (first (filter #(= qualified (:name %))
                                               (:assets artifact))))
        segment (first (filter #(= (long address) (long (:start-address %)))
                               (:segments artifact)))]
    (mapv #(bit-and (int %) 0xff) (:bytes segment))))

(deftest a-string-var-is-an-asset-in-the-charset-it-names
  (is (= [1 2 0x20 3 0x31 0x3A 0x39 0x27 26] (asset-bytes 'screen-coded-string))
      ":screen-codes: letters run from $01, $20-$3F are unchanged")
  (is (= [0x41 0x42 0x31 0x21] (asset-bytes 'petscii-coded-string))
      ":petscii: letters run from $41, upper and lower case fold")
  (is (= [0x48 0x69 0x21] (asset-bytes 'ascii-coded-string))
      ":ascii is the default"))

(deftest a-string-asset-takes-the-same-placement-metadata
  (is (= 0x3000 (placed-asset 'screen-coded-string)) ":at pins the asset")
  (is (= 0 (mod (placed-asset 'aligned-string) 4)) ":align true still applies"))

(deftest a-string-asset-rejects-what-it-cannot-encode
  (is (= :unknown-charset
         (error-reason #(link-forms '((lda (< unknown-charset-string))) {}))))
  (is (= :unencodable-character
         (error-reason #(link-forms '((lda (< unencodable-string))) {}))))
  (is (= :empty-asset
         (error-reason #(link-forms '((lda (< empty-string-asset))) {})))))

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
    (is (= 0x900
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
    (is (= 0x900 (:entry-point (assembler/link object {:entry :b}))))))

;; operand expressions --------------------------------------------------------

;; A label used by the link-time expression tests, and an object to reference
;; from an expression (so the external-label path is covered too).
(assembler/define-asm expression-target :start (rts))

(defn- artifact-bytes [artifact]
  (mapv #(bit-and (int %) 0xff) (:bytes (first (:segments artifact)))))

(defn- bytes-at-address
  "The linked bytes starting at `address`, which must lie inside one segment."
  [artifact address length]
  (let [segment (first (filter (fn [{:keys [start-address bytes]}]
                                 (<= (long start-address) (long address)
                                     (dec (+ (long start-address) (count bytes)))))
                               (:segments artifact)))]
    (mapv #(bit-and (int %) 0xff)
          (take length (drop (- (long address) (long (:start-address segment)))
                             (:bytes segment))))))

(defn- little-endian-bytes [n]
  [(bit-and (long n) 0xff) (bit-and (bit-shift-right (long n) 8) 0xff)])

(deftest operand-expressions-are-evaluated-at-link-time
  ;; A form mentioning a label (or asset) is kept whole and evaluated by the
  ;; linker, where the addresses exist; nested arithmetic and `let` are just
  ;; Clojure in the assembled namespace.
  (let [artifact (link-forms '((%org 0x2000)
                               :volume
                               (lda 0x18)
                               (inc (+ :volume 1))
                               (lda [(+ :volume (* 2 3))])
                               (lda (< (+ :volume 1)))
                               (lda (> (+ :volume 1)))
                               (lda [(let [n 3] (+ :volume n))])
                               (lda (+ 1 2)))
                             {})]
    (is (= [0xa9 0x18
            0xee 0x01 0x20          ; INC $2001: the operand byte itself
            0xad 0x06 0x20          ; LDA $2006: (* 2 3)
            0xa9 0x01 0xa9 0x20     ; the low and high bytes of $2001
            0xad 0x03 0x20          ; LDA $2003: a let-bound offset
            0xa9 0x03]              ; a constant expression still folds here
           (artifact-bytes artifact)))))

(deftest operand-expressions-may-reference-assets-and-objects
  (let [artifact (link-forms '((%dw [(+ inline-asset 4)])
                               (lda [(+ expression-target 1)]))
                             {})
        inline (:start-address
                (first (filter #(= 'omkamra.vice.assembler-test/inline-asset
                                   (:name %))
                               (:assets artifact))))
        owner (first (filter #(= (keyword "omkamra.vice.assembler-test"
                                          "expression-target")
                                 (:object-id %))
                             (:objects artifact)))
        ;; the primary object is the one assembled without an `:object-id`
        section (first (get-in (first (filter #(nil? (:object-id %))
                                              (:objects artifact)))
                               [:placement :sections]))
        bytes (bytes-at-address artifact (:base section) (:size section))]
    (is (= (concat (little-endian-bytes (+ inline 4))
                   [0xad]
                   (little-endian-bytes (inc (long (get-in owner
                                                           [:placement :sections 0 :base])))))
           bytes))))

(deftest operand-expressions-keep-their-errors
  (testing "a label that does not exist is still caught while assembling"
    (is (= :unresolved-label
           (error-reason #(link-forms '((lda [(+ :missing 1)])) {})))))
  (testing "a bad expression is reported when linking"
    (is (= :expression-error
           (error-reason (fn [] (link-forms '((%org 0x2000)
                                              :here
                                              (lda [(quot :here 0)]))
                                            {})))))))

(deftest operand-expressions-take-qualified-constants
  ;; Syntax-quote inside an assembly macro qualifies every name, including the
  ;; namespace's own constants. Those are constants, not asset references.
  (let [object (assemble-only '((%org 0x2000)
                                :here
                                (lda [(+ omkamra.vice.assembler-test/magic :here)])))
        artifact (assembler/link object)]
    (is (empty? (:assets (:dependencies object)))
        "a qualified constant does not become an asset dependency")
    (is (= [0xad 0x42 0x20] (artifact-bytes artifact))
        "it folds to $2042")))
