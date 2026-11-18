(ns omkamra.assembler.6502-test
  "Tests for the 6502/6510 target: source normalization, mode inference,
  symbol resolution, instruction encoding, assets, relocation, serialization,
  and the object graph."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [omkamra.assembler :as assembler]
            [omkamra.assembler.6502 :as mos6502]))

;; Test helpers ---------------------------------------------------------------

(def default-regions
  "The C64 placement profile a VICE program would link with."
  [{:start 0xc000 :end 0xd000}
   {:start 0x0900 :end 0xa000}
   {:start 0xa000 :end 0xc000}
   {:start 0xe000 :end 0x10000}])

(defn asm-object
  "Assemble `forms` into the root relocatable object of a fresh graph.

  Accepts the historical `:ns` option and defaults it to this namespace."
  ([forms] (asm-object forms {}))
  ([forms options]
   (let [options (-> options
                     (set/rename-keys {:ns :namespace})
                     (update :namespace #(or % 'omkamra.assembler.6502-test)))]
     (assembler/object-graph->object
      (assembler/assemble (mos6502/asm* forms options))))))

(defn link-target
  "Link an object graph or object with the C64 placement profile."
  ([target] (link-target target {}))
  ([target options]
   (assembler/link target (merge {:regions default-regions :reserved []}
                                 options))))

(defn infer-mode-6502 [mnemonic operand]
  (assembler/infer-mode :6502 mnemonic operand))

(defn asset-of
  "An asset descriptor, the shape `->Asset` used to build."
  [name data constraints resource]
  {:kind :asset
   :name name
   :data (vec data)
   :size (count data)
   :constraints constraints
   :resource resource})

(def screen-address 0x0400)

(def ^{:size 8 :at 0x3c00} macro-address-asset [])

(defn macro-uses-addressed-vector
  []
  (let [address ^{:clj-kondo/ignore [:type-mismatch]}
        (+ macro-address-asset 3)]
    `[(lda [~address])]))

(defn evaluated-generator
  [n]
  (vec (repeat (long n) '(nop))))

(def charset-asset
  (asset-of 'omkamra.assembler.6502-test/charset
            (vec (range 8)) nil nil))

(def region-asset
  (asset-of 'omkamra.assembler.6502-test/region-asset
            (vec (range 4)) nil nil))

(def fixed-placement-asset
  (asset-of 'omkamra.assembler.6502-test/fixed-placement-asset
            [0xaa] {:at 0xc000} nil))

(def entry-object
  (asm-object
   '((%entry :start) :start (lda [screen-address]) (jsr :helper) :helper (lda <:start) (rts))
   {:object-id :omkamra.assembler.6502-test/entry-object}))

(def ^{:doc "An assembly object with a Var docstring."} documented-object
  (asm-object
   '((%entry :start) :start (rts))
   {:object-id :omkamra.assembler.6502-test/documented-object}))

(deftest quoted-source-is-normalized
  (let [object (asm-object
                '((lda 0x56)
                  (lda [0x2000])
                  (%label :loop)
                  (jmp :loop))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(lda 86)
             (lda [8192])
             :loop
             (jmp :loop)]
           (:source object)))
    (is (= [:loop] (keys (:labels object))))))

(deftest single-instruction-source
  (let [object (asm-object '(rts)
                           {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(rts)] (:source object)))))

(deftest assembly-macro-resolves-fixed-address-vector
  (let [object (asm-object
                '((macro-uses-addressed-vector))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(lda [0x3c03])] (:source object)))))

(deftest function-calls-evaluate-and-expand
  (testing "ordinary generator arguments are evaluated"
    (is (= (vec (repeat 13 '(nop)))
           (:source (asm-object
                     '((evaluated-generator (- 17 4)))
                     {:ns 'omkamra.assembler.6502-test})))))
  (testing "ordinary functions may return lazy sequences of forms"
    (is (= '[(nop) (nop) (nop)]
           (:source (asm-object
                     '((repeat 3 (quote (nop))))
                     {:ns 'omkamra.assembler.6502-test}))))))

(deftest directives-are-normalized
  (let [object (asm-object
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
                {:ns 'omkamra.assembler.6502-test})]
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
  (let [object (asm-object
                '((lda (%meta {:role :operand} [0x2000])))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(lda (%meta {:role :operand} [8192]))] (:source object)))))

(deftest include-is-an-asset-alias
  (let [object (asm-object
                '((%include charset-asset))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(%include omkamra.assembler.6502-test/charset-asset)]
           (:source object)))
    (is (= ['omkamra.assembler.6502-test/charset-asset]
           (get-in object [:dependencies :assets])))))

(deftest forward-and-backward-labels
  (let [object (asm-object
                '(:a
                  (bne :b)
                  :b
                  (jmp :a))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= [:a :b] (keys (:labels object))))))

(deftest duplicate-label-diagnostic
  (let [error (try
                (asm-object '(:a :a (rts))
                            {:ns 'omkamra.assembler.6502-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :duplicate-label (:reason (ex-data error))))
    (is (= [:a] (:labels (ex-data error))))))

(deftest unresolved-label-diagnostic-keeps-location
  (let [form (with-meta '(jmp :missing) {:line 7 :column 3})
        error (try
                (asm-object [form]
                            {:ns 'omkamra.assembler.6502-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :unresolved-label (:reason (ex-data error))))
    (is (= 7 (get-in (ex-data error) [:location :line])))
    (is (= 3 (get-in (ex-data error) [:location :column])))))

(deftest constants-are-resolved
  (let [object (asm-object
                '((lda [screen-address]))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(lda [1024])] (:source object)))))

(deftest assets-become-qualified-symbols
  (let [object (asm-object
                '((lda [charset-asset])
                  (%dw [charset-asset]))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[(lda [omkamra.assembler.6502-test/charset-asset])
             (%dw [omkamra.assembler.6502-test/charset-asset])]
           (:source object)))
    (is (= '[omkamra.assembler.6502-test/charset-asset]
           (:assets (:dependencies object))))))

(deftest relocation-shorthand
  (is (= '[:start (%db [(< :start)])]
         (:source (asm-object
                   '(:start (%db [<:start]))
                   {:ns 'omkamra.assembler.6502-test}))))
  (is (= '[:start (lda (< :start))]
         (:source (asm-object
                   '(:start (lda (< :start)))
                   {:ns 'omkamra.assembler.6502-test})))))

(deftest unresolved-symbol-diagnostic
  (let [error (try
                (asm-object '((lda [does-not-exist]))
                            {:ns 'omkamra.assembler.6502-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :unresolved-symbol (:reason (ex-data error))))))

(deftest mode-inference
  (testing "immediate, absolute, zero page, and indexed"
    (is (= :imm (infer-mode-6502 'lda 0x56)))
    (is (= :abs (infer-mode-6502 'lda '[0x2000])))
    (is (= :zp (infer-mode-6502 'lda '[0x20])))
    (is (= :zp (infer-mode-6502 'lda '[%zp 0x2000])))
    (is (= :zpx (infer-mode-6502 'lda '[0x20 x])))
    (is (= :absx (infer-mode-6502 'lda '[0x2000 x])))
    (is (= :zpy (infer-mode-6502 'ldx '[0x20 y])))
    (is (= :absy (infer-mode-6502 'ldx '[0x2000 y]))))
  (testing "indirect memory modes"
    (is (= :indx (infer-mode-6502 'lda '[[0x20 x]])))
    (is (= :indy (infer-mode-6502 'lda '[[0x20] y]))))
  (testing "indirect versus direct JMP"
    (is (= :abs (infer-mode-6502 'jmp 0x2000)))
    (is (= :abs (infer-mode-6502 'jmp :label)))
    (is (= :ind (infer-mode-6502 'jmp '[0x2000])))
    (is (= :ind (infer-mode-6502 'jmp '[:label]))))
  (testing "accumulator, implied, and relative"
    (is (= :acc (infer-mode-6502 'asl 'a)))
    (is (= :imp (infer-mode-6502 'nop nil)))
    (is (= :rel (infer-mode-6502 'bne :loop))))
  (testing "accumulator shifts accept an omitted A"
    (doseq [mnemonic '[asl lsr rol ror]]
      (is (= :acc (infer-mode-6502 mnemonic nil)))
      (let [shorthand (asm-object (list (list mnemonic))
                                  {:ns 'omkamra.assembler.6502-test})
            explicit (asm-object (list (list mnemonic 'a))
                                 {:ns 'omkamra.assembler.6502-test})
            instruction (fn [object]
                          (select-keys (first (get-in object [:sections 0 :items]))
                                       [:mnemonic :mode :opcode]))]
        (is (= (instruction explicit)
               (instruction shorthand))))))
  (testing "dotted mnemonics take their mode from the exact opcode"
    (is (= :imp (infer-mode-6502 'nop.3a nil))))
  (testing "unsupported shapes are rejected"
    (is (thrown? clojure.lang.ExceptionInfo (infer-mode-6502 'lda nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (infer-mode-6502 'ldy '[0x20 y])))))

(deftest external-label-dependencies
  (let [object (asm-object
                '((jsr :omkamra.vice.examples.common/clear-screen))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[:omkamra.vice.examples.common/clear-screen]
           (:objects (:dependencies object))))))

(deftest entry-option-overrides-directive
  (let [object (asm-object
                '((%entry :a) :a :b (rts))
                {:ns 'omkamra.assembler.6502-test :entry :b})]
    (is (= :b (:entry object)))))

(deftest raw-byte-directive
  (is (= '[(%db [234])]
         (:source (asm-object
                   '((%db 0xea))
                   {:ns 'omkamra.assembler.6502-test})))))

(deftest serialize-clj-round-trips
  (let [object (asm-object
                '((%entry :start)
                  :start
                  (lda 0x56)
                  (lda [0x2000])
                  (asl a)
                  (bne :loop)
                  :loop
                  (jmp [0x2000])
                  (rts))
                {:ns 'omkamra.assembler.6502-test})
        text (assembler/serialize-data object)
        reread (read-string text)
        object2 (asm-object reread
                            {:ns 'omkamra.assembler.6502-test})]
    (is (= (:source object) (:source object2)))
    (is (= (:labels object) (:labels object2)))
    (is (= (:entry object) (:entry object2)))))

(deftest serialize-clj-accepts-raw-forms
  (let [forms '[(lda 0x56) (rts)]]
    (is (= (assembler/serialize-data forms)
           (assembler/serialize-data {:kind :object :source forms})))))

(mos6502/define-asm deferred-object
  "A Var docstring."
  (%entry :deferred-start)
  :deferred-start
  (lda [screen-address])
  (rts))

(mos6502/define-asm deferred-caller
  (%entry :caller-start)
  :caller-start
  (jsr deferred-object)
  (rts))

(deftest define-asm-makes-a-deferred-descriptor
  (is (= "A Var docstring." (:doc (meta #'deferred-object))))
  (is (assembler/descriptor? deferred-object))
  (is (= :6502 (:isa deferred-object)))
  (is (= :omkamra.assembler.6502-test/deferred-object
         (:object-id deferred-object)))
  (is (= 'omkamra.assembler.6502-test (:namespace deferred-object)))
  (testing "nothing is assembled until the graph is walked"
    (is (nil? (:sections deferred-object)))
    (is (= :asm (:kind deferred-object)))))

(deftest define-asm-identity
  (is (= :omkamra.assembler.6502-test/entry-object (:object-id entry-object)))
  (is (= 'omkamra.assembler.6502-test (:namespace entry-object)))
  (is (= :start (:entry entry-object)))
  (is (= '[(%entry :start)
           :start
           (lda [1024])
           (jsr :helper)
           :helper
           (lda (< :start))
           (rts)]
         (:source entry-object))))

(deftest a-var-reference-names-the-objects-entry
  (is (= '[(jsr :omkamra.assembler.6502-test/deferred-start)]
         (filter #(and (seq? %) (= 'jsr (first %)))
                 (:source (assembler/object-graph->object
                           (assembler/assemble deferred-caller)))))))

(deftest re-evaluating-a-define-asm-form-changes-the-next-assembly
  (let [graph (assembler/assemble deferred-caller)]
    (is (contains? (:objects graph)
                   :omkamra.assembler.6502-test/deferred-object))
    (testing "the caller's reference resolves through the Var to its entry"
      (is (= [:omkamra.assembler.6502-test/deferred-start]
             (:objects (:dependencies (assembler/object-graph->object graph))))))
    (is (some? (link-target graph)))))

(def twice (fn [x] (* 2 x)))

(def magic 0x42)

(def inline-asset [0 1 2 255])

(assembler/define-resource ^{:align 256} resource-asset "omkamra/vice/assembler_test/bytes.bin")

(deftest expressions-evaluate
  (is (= '[(lda 3)
           (lda [1028])
           (%db [10 255 52 18])
           (%dw [4660])
           (%align 256)]
         (:source (asm-object
                   '((lda (+ 1 2))
                     (lda [(+ 1024 4)])
                     (%db [(+ 5 5) (u8 0x1ff) (lo 0x1234) (hi 0x1234)])
                     (%dw [(word 0x34 0x12)])
                     (%align (* 2 128)))
                   {:ns 'omkamra.assembler.6502-test})))))

(deftest host-symbols-resolve-in-expressions
  (is (= '[(lda 84) (lda [66])]
         (:source (asm-object
                   '((lda (twice 42)) (lda [magic]))
                   {:ns 'omkamra.assembler.6502-test})))))

(deftest multi-operand-relocation-operators-are-comparisons
  (is (= '[(lda 1)]
         (:source (asm-object
                   '((lda (if (< 1 2) 1 0)))
                   {:ns 'omkamra.assembler.6502-test})))))

(deftest expression-errors-keep-location
  (let [inner (list 'no-such-fn 1)
        form (with-meta (list 'lda inner) {:line 11 :column 5})
        error (try
                (asm-object [form]
                            {:ns 'omkamra.assembler.6502-test})
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :expression-error (:reason (ex-data error))))
    (is (= 0 (get-in (ex-data error) [:location :index])))))

(deftest jvm-interop-and-eval-are-blocked
  (doseq [source ['((lda (System/exit 0)))
                  '((lda (eval '(+ 1 2))))
                  '((lda (slurp "x")))]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (asm-object source
                             {:ns 'omkamra.assembler.6502-test})))))

(deftest inline-assets
  (testing "the Var holds the bytes; there is no asset ceremony"
    (is (= [0 1 2 255] (vec inline-asset))))
  (is (= '[(lda [omkamra.assembler.6502-test/inline-asset])]
         (:source (asm-object
                   '((lda [inline-asset]))
                   {:ns 'omkamra.assembler.6502-test})))))

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
  (is (= '[(lda [omkamra.assembler.6502-test/plain-bytes])]
         (:source (asm-object
                   '((lda [plain-bytes]))
                   {:ns 'omkamra.assembler.6502-test}))))
  (testing "the linker places it like any other asset"
    (let [artifact (link-target
                    (asm-object
                     '((%org 0x1000)
                       (lda [plain-bytes]))
                     {:ns 'omkamra.assembler.6502-test})
                    {:regions [{:start 0x1000 :end 0x2000}]})
          asset (first (:assets artifact))]
      (is (= ['omkamra.assembler.6502-test/plain-bytes] (mapv :name (:assets artifact))))
      (is (= [7 8 9] (linked-bytes-at artifact (:start-address asset) 3))))))

(defmacro syntax-quoted-macro
  "A raw assembly macro in the idiomatic style: syntax-quote, no `list` calls.

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
           (lda [omkamra.assembler.6502-test/inline-asset x])
           (iny)
           (bne :loop)]
         (:source (asm-object
                   '((syntax-quoted-macro :loop inline-asset))
                   {:ns 'omkamra.assembler.6502-test}))))
  (testing "the qualified index register still selects absolute,X"
    (let [artifact (link-target
                    (asm-object
                     '((%org 0x1000)
                       (syntax-quoted-macro :loop inline-asset))
                     {:ns 'omkamra.assembler.6502-test})
                    {:regions [{:start 0x1000 :end 0x2000}]})]
      (is (= [0xa2 0x00                 ; LDX #$00
              0xbd]                     ; LDA abs,X
             (mapv #(bit-and (int %) 0xff)
                   (take 3 (:bytes (first (:segments artifact))))))))))

(deftest resource-assets
  (testing "the Var holds a lightweight descriptor; bytes load at assembly time"
    (is (assembler/asset? resource-asset))
    (is (nil? (:data resource-asset)))
    (is (= "omkamra/vice/assembler_test/bytes.bin" (:resource resource-asset))))
  (testing "assembling the asset reads the classpath resource"
    (let [artifact (link-target
                    (asm-object '((%org 0x1000) (%include resource-asset))))
          asset (first (filter #(= 'omkamra.assembler.6502-test/resource-asset
                                   (:name %))
                               (:assets artifact)))]
      (is (= [1 2 3 255] (linked-bytes-at artifact (:start-address asset) 4)))
      (is (= {:align 256} (:constraints asset))))))

(deftest asset-placement-options-are-top-level
  (is (= {:align 256 :regions [{:start 0x2000 :end 0x3000}] :at 0x2400}
         (:constraints
          (assembler/build-asset
           'placement
           {:data [1 2 3]
            :align 256
            :regions [{:start 0x2000 :end 0x3000}]
            :at 0x2400})))))

(deftest asset-dependencies
  (let [object (asm-object
                '((lda [inline-asset]) (%dw [resource-asset]))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= '[omkamra.assembler.6502-test/inline-asset
             omkamra.assembler.6502-test/resource-asset]
           (:assets (:dependencies object))))))

(defn- section-bytes [object]
  (vec (mapcat :bytes (:items (first (:sections object))))))

(defn- relocations [object]
  (:relocations object))

(defn- assemble-only [source]
  (asm-object source {:ns 'omkamra.assembler.6502-test}))

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
  (is (= [0x3a] (section-bytes (assemble-only '((%db [0x3a]))))))
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

(defn- error-reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- link-forms [source options]
  (link-target (assemble-only source) options))

(deftest link-primary-object
  (let [artifact (link-forms '((%entry :start) :start (lda 0x01) (rts)) {})]
    (is (assembler/artifact? artifact))
    (is (= 0xC000 (:entry-point artifact)))
    (is (= 1 (count (:segments artifact))))
    (is (= [0xa9 1 0x60]
           (mapv (fn [b] (bit-and (int b) 0xff))
                 (:bytes (first (:segments artifact))))))
    (is (= {:start 0xC000} (:labels (first (:objects artifact)))))
    (is (identical? artifact (link-target artifact)))))

(deftest link-discovers-object-dependencies
  (let [common (asm-object
                '(::clear-screen-fn (rts))
                {:ns 'omkamra.assembler.6502-test :object-id :t/common})
        main (asm-object
              '((%entry :start)
                :start
                (jsr :omkamra.assembler.6502-test/clear-screen-fn)
                (rts))
              {:ns 'omkamra.assembler.6502-test})
        artifact (link-target main {:objects [common]})]
    (is (= 2 (count (:objects artifact))))
    (is (= [:t/common nil]
           (sort-by (fn [id] (if id 0 1))
                    (map :object-id (:objects artifact)))))
    (is (= 0xC000 (:entry-point artifact)))
    (is (= 0xC004 (get (:labels (second (:objects artifact))) :clear-screen-fn)))))

(def default-entry-object
  (asm-object
   '((lda 0x01) (rts))
   {:object-id :omkamra.assembler.6502-test/default-entry-object}))

(def default-entry-caller
  (asm-object
   '((%entry :start) :start (jsr default-entry-object) (rts))
   {:object-id :omkamra.assembler.6502-test/default-entry-caller}))

(def explicit-entry-after-data
  (asm-object
   '((%db [0xaa])
     (%entry :start)
     :start
     (rts))
   {:ns 'omkamra.assembler.6502-test
    :object-id :omkamra.assembler.6502-test/explicit-entry-after-data}))

(def data-only-object
  (asm-object '((%db [1 2 3]))
              {:ns 'omkamra.assembler.6502-test
               :object-id :omkamra.assembler.6502-test/data-only}))

(deftest define-asm-records-an-external-only-default-entry
  (is (nil? (:entry default-entry-object)))
  (is (= {:section 0 :offset 0}
         (:default-entry default-entry-object)))
  (is (nil? (get (:labels default-entry-object) :default-entry-object)))
  (testing "an object that declares %entry gets no implicit default entry"
    (is (nil? (:default-entry entry-object)))
    (is (nil? (get (:labels entry-object) :entry-object)))))

(deftest link-uses-first-instruction-as-primary-entry
  (is (= 0xC000 (:entry-point (link-target default-entry-object))))
  (testing "an explicit entry still wins over the first instruction"
    (is (= 0xC001 (:entry-point (link-target explicit-entry-after-data))))))

(deftest object-vars-resolve-to-their-entry
  (testing "the Var reference becomes an external label for the entry"
    (is (some #{'(jsr :omkamra.assembler.6502-test/default-entry-object)}
              (:source default-entry-caller)))
    (is (= [:omkamra.assembler.6502-test/default-entry-object]
           (:objects (:dependencies default-entry-caller)))))
  (testing "and links to the entry label's address"
    (let [artifact (link-target default-entry-caller)
          handler (first (filter #(= :omkamra.assembler.6502-test/default-entry-object
                                     (:object-id %))
                                 (:objects artifact)))]
      (is (some? handler))
      (is (nil? (get (:labels handler) :default-entry-object)))
      (is (= [0x20 0x04 0xC0 0x60 0xA9 0x01 0x60]
             (mapv #(bit-and (int %) 0xff)
                   (:bytes (first (:segments artifact)))))))))

(deftest object-without-entry-cannot-be-referenced
  (is (nil? (get (:labels data-only-object) :data-only)))
  (is (= :no-object-entry
         (error-reason
          #(asm-object '((lda (< data-only-object)))
                       {:ns 'omkamra.assembler.6502-test})))))

(deftest labels-directive-renames-its-labels
  (let [object (asm-object
                '((%labels [:loop] :loop (inx) (bne :loop)))
                {:ns 'omkamra.assembler.6502-test})
        [label inx bne] (:source object)]
    (is (keyword? label))
    (is (not= :loop label))
    (is (= '(inx) inx))
    (is (= label (second bne)))
    (is (= [label] (keys (:labels object))))))

(deftest labels-directive-is-unique-per-use
  (let [object (asm-object
                '((%labels [:loop] :loop (inx) (bne :loop))
                  (%labels [:loop] :loop (dex) (bne :loop)))
                {:ns 'omkamra.assembler.6502-test})
        labels (keys (:labels object))]
    (is (= 2 (count labels)))
    (is (= 2 (count (distinct labels))))))

(deftest labels-directive-scopes-its-body
  (let [object (asm-object
                '((%labels [:loop] :loop (bne :loop))
                  :loop)
                {:ns 'omkamra.assembler.6502-test})
        labels (set (keys (:labels object)))]
    (is (contains? labels :loop) "the outer label is untouched")
    (is (= 2 (count labels)) "and the scoped one is separate")))

(deftest labels-directive-validates-its-names
  (is (= :invalid-labels
         (error-reason
          (fn [] (asm-object '((%labels :loop (inx)))
                             {:ns 'omkamra.assembler.6502-test}))))))

(def reserve-anchor [1 2 3 4 5 6 7 8])

(deftest reserve-records-its-range
  (let [object (asm-object
                '((%org 0x9000) :start (%reserve 0x0800 0x100) (rts))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= [[0x0800 0x0900]] (:reserved (:constraints object))))))

(deftest reserve-accepts-a-range-vector
  (let [object (asm-object
                '((%org 0x9000) :start (%reserve [0x0800 0x0900]) (rts))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= [[0x0800 0x0900]] (:reserved (:constraints object))))))

(deftest reserve-keeps-the-linker-out-of-the-range
  (let [object (asm-object
                '((%org 0x9000) :start (%reserve 0x0800 0x100)
                                (lda [reserve-anchor])
                                (rts))
                {:ns 'omkamra.assembler.6502-test
                 :object-id :omkamra.assembler.6502-test/reserved})
        artifact (link-target object)
        anchor (first (filter #(= 'omkamra.assembler.6502-test/reserve-anchor
                                  (:name %))
                              (:assets artifact)))]
    (is (= 0xC000 (:start-address anchor))
        "the floating asset skips the reserved range")))

(deftest reserve-validates-its-arguments
  (is (= :invalid-reserve
         (error-reason
          (fn [] (asm-object '((%reserve :nope 4))
                             {:ns 'omkamra.assembler.6502-test}))))))

(deftest reserve-rejects-overlap
  (is (= :overlap
         (error-reason
          (fn [] (link-target
                  (asm-object '((%org 0xC000) :start (%reserve 0xC000 16) (rts))
                              {:ns 'omkamra.assembler.6502-test})))))))

(def ^{:align 256} aligned-inline [1 2 3 4 5 6 7 8])

(deftest implicit-asset-honours-alignment-metadata
  (let [object (asm-object
                '((%org 0x2000) (%reserve 0x0800 2) (lda [aligned-inline,x]))
                {:ns 'omkamra.assembler.6502-test})
        artifact (link-target object)
        asset (first (filter #(= 'omkamra.assembler.6502-test/aligned-inline
                                 (:name %))
                             (:assets artifact)))]
    (is (= 0 (mod (:start-address asset) 256))
        "the inline table aligns itself to a page")))

(def ^{:align true} nine-bytes (vec (repeat 9 1)))

(def ^{:align true} twenty-one-bytes (vec (repeat 21 1)))

(def metadata-address 0x3600)

(def ^{:at metadata-address} metadata-address-bytes [1 2 3])

(def ^{:at 0x3700 :size 16} reserved-bytes [])

(def ^{:at 0x3800 :size 4} sized-bytes [1 2 3 4 5])

(def ^{:at 0x3900 :size 4} short-sized-bytes [6 7])

(def ^{:at 0x3a00 :size 4} sized-reservation [8 9])

(def ^{:at 0x3a03} overlapping-byte [10])

(defn- placed-asset
  "The linked start address of implicit asset `sym` in a small object."
  [sym]
  (let [object (asm-object
                (list '(%org 0x4000) '(%reserve 0x0800 2) (list 'lda (vector sym 'x)))
                {:ns 'omkamra.assembler.6502-test
                 :object-id (keyword "omkamra.assembler.6502-test"
                                     (str "align-" (name sym)))})
        artifact (link-target object)
        qualified (symbol "omkamra.assembler.6502-test" (name sym))]
    (:start-address (first (filter #(= qualified (:name %)) (:assets artifact))))))

(deftest align-true-uses-the-next-power-of-two
  (is (= 0xC000 (placed-asset 'nine-bytes)) "9 bytes -> align 16")
  (is (= 0xC000 (placed-asset 'twenty-one-bytes)) "21 bytes -> align 32")
  (is (= {:align 32}
         (:constraints (assembler/build-asset 't {:data (vec (repeat 21 1))
                                                  :align true})))
      "build-asset accepts :align true too"))

(deftest asset-metadata-expressions-are-evaluated
  (is (= metadata-address (placed-asset 'metadata-address-bytes))
      "an inline asset may name its placement address in metadata"))

(deftest empty-vector-assets-can-reserve-uninitialized-memory
  (is (= 0x3700 (placed-asset 'reserved-bytes))
      "a sized empty vector has a relocatable address")
  (let [artifact (link-forms '((lda (< reserved-bytes))) {})
        segment-addresses (set (map :start-address (:segments artifact)))]
    (is (not (contains? segment-addresses 0x3700))
        "uninitialized storage emits no bytes")))

(deftest sized-vector-assets-reserve-the-whole-area
  (is (= :overlap
         (error-reason
          #(link-forms '((lda (< sized-reservation))
                         (lda (< overlapping-byte))) {})))))

(def ^{:encoding :screen-codes :at 0x3000} screen-coded-string "AB C1:9'Z")

(def ^{:encoding :petscii :at 0x3100} petscii-coded-string "Ab1!")

(def ^{:at 0x3200} ascii-coded-string "Hi!")

(def ^{:align true} aligned-string "abc")

(def ^{:encoding :ebcdic} unknown-encoding-string "hi")

(def ^{:encoding :screen-codes} unencodable-string "ok\ttab")

(def empty-string-asset "")

(defn- asset-bytes
  "The linked bytes of asset `sym`, which must be pinned alone at its address."
  [sym]
  (let [object (asm-object
                (list '(%org 0x4000) (list 'lda (list '< sym)) (list 'ldx (list '> sym)))
                {:ns 'omkamra.assembler.6502-test
                 :object-id (keyword "omkamra.assembler.6502-test"
                                     (str "string-" (name sym)))})
        artifact (link-target object)
        qualified (symbol "omkamra.assembler.6502-test" (name sym))
        address (:start-address (first (filter #(= qualified (:name %))
                                               (:assets artifact))))
        segment (first (filter #(= (long address) (long (:start-address %)))
                               (:segments artifact)))]
    (mapv #(bit-and (int %) 0xff) (:bytes segment))))

(deftest sized-vector-assets-reserve-and-initialize-a-prefix
  (is (= [6 7] (asset-bytes 'short-sized-bytes))
      "the uninitialized suffix emits no bytes")
  (testing "data that does not fit its :size is rejected, never truncated"
    (is (= :asset-size-exceeded
           (error-reason #(asset-bytes 'sized-bytes))))))

(deftest a-string-var-is-an-asset-in-the-encoding-it-names
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
  (is (= :unknown-encoding
         (error-reason #(link-forms '((lda (< unknown-encoding-string))) {}))))
  (is (= :unencodable-character
         (error-reason #(link-forms '((lda (< unencodable-string))) {}))))
  (is (= :empty-asset
         (error-reason #(link-forms '((lda (< empty-string-asset))) {})))))

(def ^{:encoding :screen-codes :at 0x3300 :terminator 0xff} terminated-string "AB")

(def ^{:at 0x3400 :terminator 0xff} terminated-bytes [7 8])

(def ^{:align true :terminator 0xff} terminated-five-bytes "abcd")

(def ^{:at 0x3500 :terminator 256} out-of-range-terminator "AB")

(defn- asset-constraints
  "The linked placement constraints of implicit asset `sym`."
  [sym]
  (let [object (asm-object
                (list '(%org 0x4000) (list 'lda (vector sym 'x)))
                {:ns 'omkamra.assembler.6502-test
                 :object-id (keyword "omkamra.assembler.6502-test"
                                     (str "terminator-" (name sym)))})
        qualified (symbol "omkamra.assembler.6502-test" (name sym))]
    (:constraints (first (filter #(= qualified (:name %))
                                 (:assets (link-target object)))))))

(deftest an-asset-may-end-in-a-terminator-byte
  (is (= [1 2 0xff] (asset-bytes 'terminated-string))
      ":terminator appends a code the encoding itself cannot spell")
  (is (= [7 8 0xff] (asset-bytes 'terminated-bytes))
      ":terminator appends to a byte-vector asset too"))

(deftest a-terminator-counts-toward-the-asset-size
  (is (= {:align 8} (asset-constraints 'terminated-five-bytes))
      "four characters plus the terminator are five bytes, which align to 8"))

(deftest a-terminator-must-fit-in-one-byte
  (is (= :invalid-terminator
         (error-reason #(link-forms '((lda (< out-of-range-terminator))) {})))))

(deftest link-rejects-ambiguous-and-unresolved-externals
  (let [ns-sym 'omkamra.assembler.6502-test
        main (asm-object
              '((jsr :omkamra.assembler.6502-test/dup))
              {:ns ns-sym})
        a (asm-object '(::dup (rts)) {:ns ns-sym :object-id :t/a})
        b (asm-object '(::dup (rts)) {:ns ns-sym :object-id :t/b})]
    (is (= :ambiguous-external
           (error-reason #(link-target main {:objects [a b]}))))
    (is (= :unresolved-external
           (error-reason #(link-target
                           (asm-object
                            '((jsr :omkamra.assembler.6502-test/nope))
                            {:ns ns-sym})
                           {}))))))

(deftest unqualified-labels-are-private
  (let [ns-sym 'omkamra.assembler.6502-test
        ;; Two objects may reuse the same private label; neither is visible
        ;; from outside.
        a (asm-object '(:loop (rts)) {:ns ns-sym :object-id :t/priv-a})
        b (asm-object '(:loop (rts)) {:ns ns-sym :object-id :t/priv-b})
        caller (asm-object
                '((%entry :start)
                  :start
                  (jsr :omkamra.assembler.6502-test/loop)
                  (rts))
                {:ns ns-sym})]
    (is (false? (contains? (set (:exports a)) :loop)))
    (is (false? (contains? (set (:exports b)) :loop)))
    (is (= :unresolved-external
           (error-reason #(link-target caller {:objects [a b]}))))))

(deftest qualified-labels-are-exported
  (let [ns-sym 'omkamra.assembler.6502-test
        helper (asm-object '(::loop (rts))
                           {:ns ns-sym :object-id :t/pub})
        caller (asm-object
                '((%entry :start)
                  :start
                  (jsr :omkamra.assembler.6502-test/loop)
                  (rts))
                {:ns ns-sym})
        artifact (link-target caller {:objects [helper]})]
    (is (contains? (set (:exports helper)) :loop))
    (is (= 2 (count (:objects artifact))))))

(deftest a-label-may-not-export-into-a-foreign-namespace
  (is (= :foreign-label
         (error-reason
          #(asm-object '(:other.ns/loop (rts))
                       {:ns 'omkamra.assembler.6502-test}))))
  (is (= :foreign-label
         (error-reason
          (fn [] (asm-object '((%label :other.ns/loop) (rts))
                             {:ns 'omkamra.assembler.6502-test}))))))

(deftest entry-labels-are-exported
  (testing "an explicit %entry label"
    (is (contains? (set (:exports entry-object)) :start)))
  (testing "the external-only default entry named after the object"
    (is (contains? (set (:exports default-entry-object))
                   :default-entry-object))))

(deftest numeric-labels-resolve-to-the-nearest-definition
  (let [object (asm-object
                '(:1 (nop) :1 (rts) :2 (jmp :1b) (jmp :1f) :1 (rts))
                {:ns 'omkamra.assembler.6502-test})
        source (vec (:source object))]
    (is (= (nth source 2) (second (nth source 5)))
        ":1b picks the nearest preceding :1")
    (is (= (nth source 7) (second (nth source 6)))
        ":1f picks the nearest following :1")
    (is (= 3 (count (set [(nth source 0) (nth source 2) (nth source 7)])))
        "each :1 is a distinct private label")
    (is (= 4 (count (:labels object))))))

(deftest numeric-labels-link-directionally
  (let [object (asm-object
                '((%org 0x1000)
                  :1 (nop) (jmp :1b)
                  (nop)
                  :1 (nop) (jmp :1f)
                  :1 (rts))
                {:ns 'omkamra.assembler.6502-test})
        bytes (mapv #(bit-and (int %) 0xff)
                    (:bytes (first (:segments (link-target object)))))]
    (is (= [0x4c 0x00 0x10] (subvec bytes 1 4)) ":1b jumps to $1000")
    (is (= [0x4c 0x09 0x10] (subvec bytes 6 9)) ":1f jumps to $1009")))

(deftest numeric-labels-coexist-with-ordinary-labels
  (let [object (asm-object
                '((%entry :start)
                  :start
                  (ldx 0)
                  :1 (inx)
                  (cpx 8)
                  (bne :1b)
                  (rts))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= :start (:entry object)))
    (is (contains? (:labels object) :start))
    (is (= 2 (count (:labels object))) "the ordinary label plus one :1")))

(deftest a-bare-numeric-label-is-a-hard-error
  (is (= :numeric-label-needs-direction
         (error-reason #(asm-object '(:1 (jmp :1))
                                    {:ns 'omkamra.assembler.6502-test})))
      "a definition given as a reference names no direction"))

(deftest unresolved-numeric-labels-report-the-direction
  (is (= :unresolved-numeric-label
         (error-reason #(asm-object '(:1 (jmp :1f))
                                    {:ns 'omkamra.assembler.6502-test}))))
  (is (= :unresolved-numeric-label
         (error-reason #(asm-object '((jmp :1b) :1 (rts))
                                    {:ns 'omkamra.assembler.6502-test})))))

(deftest numeric-labels-stay-out-of-entries-and-declarations
  (is (= :numeric-label-entry
         (error-reason
          (fn [] (asm-object '((%entry :1) :1 (rts))
                             {:ns 'omkamra.assembler.6502-test})))))
  (is (= :invalid-numeric-label
         (error-reason #(asm-object '(:1f (rts))
                                    {:ns 'omkamra.assembler.6502-test})))))

(deftest objects-without-numeric-labels-are-untouched
  (is (= '[(%entry :start) :start (jmp :start)]
         (:source (asm-object '((%entry :start) :start (jmp :start))
                              {:ns 'omkamra.assembler.6502-test})))
      "the rewriting pass is a no-op when no numeric labels appear"))

(deftest link-places-assets
  (let [artifact (link-forms '((%dw [inline-asset]) (rts)) {})]
    (is (= 1 (count (:assets artifact))))
    (is (= 'omkamra.assembler.6502-test/inline-asset
           (:name (first (:assets artifact)))))
    (is (= (:start-address (first (:assets artifact)))
           (let [bytes (:bytes (first (:segments artifact)))]
             (+ (bit-and (int (nth bytes 0)) 0xff)
                (bit-shift-left (bit-and (int (nth bytes 1)) 0xff) 8)))))))

(deftest placement
  (testing "default first-fit"
    (is (= 0xC000
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms '(:s (rts)) {}))))))))))
  (testing "explicit regions can request the BASIC page"
    (is (= 0x0800
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms
                                               '((%region 0x0800) (rts))
                                               {}))))))))))
  (testing "fixed assets are claimed before automatic sections"
    (let [artifact (link-forms '((%include fixed-placement-asset) (rts)) {})
          asset (first (:assets artifact))
          section (first (:sections (:placement (first (:objects artifact)))))]
      (is (= 0xc000 (:start-address asset)))
      (is (= 0xc001 (:base section)))))
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
  (testing "region override"
    (is (= 0x5000
           (:base (first (:sections
                          (:placement (first (:objects
                                              (link-forms '((rts))
                                                          {:regions [{:start 0x5000
                                                                      :end 0x6000}]}))))))))))
  (testing "org bypasses placement"
    (let [artifact (link-forms '((%org 0x2000) :fixed (rts)) {})]
      (is (= 0x2000 (:start-address (first (:segments artifact))))))))

(deftest placement-failures
  (is (= :removed-option
         (error-reason
          (fn []
            (link-forms '((rts)) {:ranges [[0x5000 0x6000]]})))))
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
  (let [object (asm-object
                '((%entry :a) :a (%entry :b) :b (rts))
                {:ns 'omkamra.assembler.6502-test})]
    (is (= :multiple-entries (error-reason #(link-target object))))
    (is (= 0xC000 (:entry-point (link-target object {:entry :b}))))))

(def expression-target
  (asm-object
   '(:start (rts))
   {:object-id :omkamra.assembler.6502-test/expression-target}))

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
                (first (filter #(= 'omkamra.assembler.6502-test/inline-asset
                                   (:name %))
                               (:assets artifact))))
        owner (first (filter #(= (keyword "omkamra.assembler.6502-test"
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
  ;; Syntax-quote inside an assembly generator qualifies every name, including the
  ;; namespace's own constants. Those are constants, not asset references.
  (let [object (assemble-only '((%org 0x2000)
                                :here
                                (lda [(+ omkamra.assembler.6502-test/magic :here)])))
        artifact (link-target object)]
    (is (empty? (:assets (:dependencies object)))
        "a qualified constant does not become an asset dependency")
    (is (= [0xad 0x42 0x20] (artifact-bytes artifact))
        "it folds to $2042")))

(deftest a-linked-object-inherits-the-referrers-region
  (let [helper (asm-object
                '(::helper-fn (rts))
                {:ns 'omkamra.assembler.6502-test :object-id :t/region-helper})
        primary (asm-object
                 '((%entry :start)
                   (%region [0xC000 0xD000])
                   :start
                   (jsr :omkamra.assembler.6502-test/helper-fn)
                   (rts))
                 {:ns 'omkamra.assembler.6502-test})
        artifact (link-target primary {:objects [helper]})
        bases (mapv #(long (:base (first (get-in % [:placement :sections]))))
                    (:objects artifact))]
    (is (every? (fn [base] (<= 0xC000 base 0xD000)) bases)
        "the linked-in helper inherits the primary's region")))

(deftest an-explicit-region-overrides-the-inherited-one
  (let [helper (asm-object
                '((%region [0x8000 0x9000])
                  ::helper-fn (rts))
                {:ns 'omkamra.assembler.6502-test :object-id :t/region-explicit})
        primary (asm-object
                 '((%entry :start)
                   (%region [0xC000 0xD000])
                   :start
                   (jsr :omkamra.assembler.6502-test/helper-fn)
                   (rts))
                 {:ns 'omkamra.assembler.6502-test})
        artifact (link-target primary {:objects [helper]})]
    (is (= #{0xC000 0x8000}
           (set (map :start-address (:segments artifact))))
        "the helper keeps its own region")))

(deftest a-linked-asset-inherits-the-referrers-region
  (let [primary (asm-object
                 '((%entry :start)
                   (%region [0xC000 0xD000])
                   (%include omkamra.assembler.6502-test/region-asset)
                   :start
                   (rts))
                 {:ns 'omkamra.assembler.6502-test})
        artifact (link-target primary)]
    (is (<= 0xC000 (long (:start-address (first (:assets artifact)))) 0xD000))))

(deftest an-asset-linked-in-by-diverging-regions-needs-its-own
  (let [a (asm-object
           '((%region [0x8000 0x9000])
             (%include omkamra.assembler.6502-test/region-asset)
             ::a-fn (rts))
           {:ns 'omkamra.assembler.6502-test :object-id :t/region-a})
        b (asm-object
           '((%region [0x9000 0xA000])
             (%include omkamra.assembler.6502-test/region-asset)
             ::b-fn (rts))
           {:ns 'omkamra.assembler.6502-test :object-id :t/region-b})
        primary (asm-object
                 '((%entry :start)
                   :start
                   (jsr :omkamra.assembler.6502-test/a-fn)
                   (jsr :omkamra.assembler.6502-test/b-fn)
                   (rts))
                 {:ns 'omkamra.assembler.6502-test})]
    (is (= :ambiguous-asset-regions
           (error-reason #(link-target primary {:objects [a b]}))))))
