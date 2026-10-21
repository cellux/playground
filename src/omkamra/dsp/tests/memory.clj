(ns omkamra.dsp.tests.memory
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            #_{:clj-kondo/ignore [:unused-namespace]}
            [omkamra.dsp.library.delay :as delay]
            #_{:clj-kondo/ignore [:unused-referred-var]}
            [omkamra.dsp.harness.fixtures :refer [constant-shape-write gain shifted-buffer struct-pole]]))

(deftest fixed-size-state-tables-work-across-backends
  (let [definition {:dsp/kind :function
                    :name 'table-tap
                    :params [{:name 'sample :type :float}
                             {:name 'slot :type :int}]
                    :state [{:name 'table :type :float :size 4
                             :init [0.0 0.0 0.0 0.0]}]
                    :process {:input :sample :controls ['slot]}
                    :body ['(let [previous (state-load table slot)]
                              (do
                                (state-store table slot sample)
                                previous))]}
        render (fn [target]
                 (let [artifact (dsp/compile definition {:target target :entry :process})
                       input (float-array [2.0 3.0])
                       output (float-array 2)]
                   (dsp/invoke artifact input output 2 2)
                   (vec output)))
        js (dsp/compile definition {:target :js :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (= [0.0 2.0] (render :interpreter)))
    (is (= [0.0 2.0] (render :jvm)))
    (is (str/includes? (:source js) "new Float32Array"))
    ;; The table is indexed inside the arena, at its own state offset.
    (is (str/includes? (:source js) "(0 + "))
    (is (= :linear-memory (get-in wasm [:wasm-abi :state-storage])))
    (is (= {:storage :linear-memory :offset 8192 :bytes 16 :count 4}
           (get-in wasm [:memory :regions :state])))
    (is (str/includes? (:wat wasm) "(i32.mul (local.get $slot) (i32.const 4))"))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"outside the declared range"
         (dsp/compile (assoc-in definition [:body 0]
                                '(state-load table 4))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"explicit state-store"
         (dsp/compile (assoc-in definition [:state 0 :next] 0.0)
                      {:entry :process})))))

(deftest local-arrays-use-the-instance-scratch-arena
  (let [definition {:dsp/kind :function
                    :name 'scratch-tap
                    :params [{:name 'sample :type :float}]
                    :state [{:name 'previous :init 0.0
                             :next '(+ sample (* 0.5 previous))}]
                    :process {:input :sample}
                    :return-type :float
                    :body ['(let [buf (array :float 2)]
                              (do (aset buf 0 previous)
                                  (aset buf 1 sample)
                                  (+ (aget buf 0) (aget buf 1))))]}
        artifact (dsp/compile definition {:entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        js (dsp/compile definition {:target :js :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})
        input (float-array [1.0 2.0 3.0])
        output (float-array 3)
        jvm-output (float-array 3)]
    ;; The ABI separates persistent state from per-invocation scratch, even
    ;; though both share one arena.
    (is (= [{:name 'buf :type :float :size 2 :index 1}]
           (get-in artifact [:abi :scratch])))
    (is (= {:ownership :instance :persistent? false :region :scratch
            :values [{:name 'buf :type :float :size 2 :index 1
                      :persistent? false}]}
           (get-in artifact [:abi :scratch-layout])))
    (is (= 0 (get-in artifact [:abi :state-layout :values 0 :index])))
    ;; Interpreter and JVM agree; scratch offsets continue after state.
    (dsp/invoke artifact input output 3)
    (dsp/invoke jvm input jvm-output 3)
    (is (= [1.0 3.0 5.5] (vec output)))
    (is (= (vec output) (vec jvm-output)))
    ;; JavaScript indexes the same arena array for scratch slots.
    (is (str/includes? (:source js) "(1 + 0)"))
    ;; Wasm lays scratch out directly after state in linear memory.
    (is (= {:storage :linear-memory :persistent? false
            :offset 8196 :bytes 8 :count 2}
           (get-in wasm [:memory :regions :scratch])))
    (is (= 8192 (get-in wasm [:memory :regions :state :offset])))))

(deftest local-array-bounds-and-diagnostics
  (let [base {:dsp/kind :function
              :name 'scratch-bounds
              :params [{:name 'sample :type :float}]
              :process {:input :sample}
              :return-type :float
              :body ['(let [buf (array :float 2)] (aget buf 0))]}
        dynamic (assoc base
                       :params [{:name 'sample :type :float}
                                {:name 'i :type :int}]
                       :process {:input :sample
                                 :controls [{:name 'i :type :int}]}
                       :body ['(let [buf (array :float 2)] (aget buf i))])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"outside the declared range"
         (dsp/compile (assoc-in base [:body 0]
                                '(let [buf (array :float 2)] (aget buf 4)))
                      {:entry :process})))
    (is (some #(= :dynamic-array-index (:dsp/warning %))
              (:diagnostics (:abi (dsp/compile dynamic {:entry :process})))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"aget/aset"
         (dsp/compile (assoc-in base [:body 0] '(let [buf (array :float 2)] buf))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"require the :process entry"
         (dsp/compile base {:entry :function})))))

(deftest state-records-and-constant-tables
  (let [array-record {:dsp/kind :function
                      :name 'record-array
                      :params [{:name 'sample :type :float} {:name 'i :type :int}]
                      :state [{:name 'voices
                               :size 4
                               :fields [{:name :phase :type :float :init 0.0}
                                        {:name :gain :type :float :init 1.0}]}]
                      :process {:input :sample :controls [{:name 'i :type :int}]}
                      :return-type :float
                      :body ['(let [previous (field voices i :phase)]
                                (do (set-field! voices i :phase (+ previous sample))
                                    (* (field voices i :gain) previous)))]}
        artifact (dsp/compile array-record {:entry :process})
        jvm (dsp/compile array-record {:target :jvm :entry :process})
        input (float-array [1.0 2.0]) output (float-array 2) jvm-output (float-array 2)]
    ;; A record's fields are laid out struct-of-arrays: one leaf block per
    ;; field, each `:size` records wide, offsets continuing after state.
    (is (= [{:name 'voices.phase :type :float :size 4
             :init [0.0 0.0 0.0 0.0] :index 0}
            {:name 'voices.gain :type :float :size 4
             :init [1.0 1.0 1.0 1.0] :index 4}]
           (:state (:ir artifact))))
    (is (= [{:path [:phase] :type :float :offset 0 :size 4}
            {:path [:gain] :type :float :offset 4 :size 4}]
           (get-in artifact [:abi :records 0 :fields])))
    (dsp/invoke artifact input output 2 1)
    (dsp/invoke jvm input jvm-output 2 1)
    (is (= (vec output) (vec jvm-output)))
    ;; A named struct resolves through the linker and keeps its identity.
    (is (= 'omkamra.dsp.harness.fixtures/test-pole-state
           (get-in (dsp/compile struct-pole {:entry :process})
                   [:abi :records 0 :struct])))
    ;; A struct-level `:constant` default makes instances read-only.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"read-only"
         (dsp/compile constant-shape-write {:entry :process})))
    ;; The shorthand field list defaults to :float / 0.0.
    (is (= [{:name 'previous :type :float :init 0.0}]
           (:fields @#'omkamra.dsp.harness.fixtures/test-pole-state)))
    ;; Constant tables are materialized once and never written.
    (let [constant-def {:dsp/kind :function
                        :name 'table-write
                        :params [{:name 'sample :type :float} {:name 'i :type :int}]
                        :state [{:name 'w :type :float :size 4
                                 :init [0.0 0.5 1.0 1.5] :constant true}]
                        :process {:input :sample :controls [{:name 'i :type :int}]}
                        :return-type :float
                        :body ['(do (state-store w i sample) sample)]}]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"read-only"
           (dsp/compile constant-def {:entry :process}))))))

(deftest clojure-style-field-access
  (let [definition {:dsp/kind :function
                    :name 'field-forms
                    :params [{:name 'sample :type :float}]
                    :state [{:name 'z
                             :fields [{:name :x1 :type :float :init 0.0}
                                      {:name :x2 :type :float :init 0.0}]}]
                    :process {:input :sample}
                    :return-type :float
                    ;; `(:x1 z)` reads, `(set! (:x1 z) ...)` writes, and
                    ;; `(z :x2)` is the record in call position.
                    :body ['(let [previous (:x1 z)]
                              (do (set! (:x1 z) sample)
                                  (+ previous (z :x2))))]}
        artifact (dsp/compile definition {:entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 2.0 3.0]) output (float-array 3) jvm-output (float-array 3)]
    (dsp/invoke artifact input output 3)
    (dsp/invoke jvm input jvm-output 3)
    (is (= [0.0 1.0 2.0] (vec output)))
    (is (= (vec output) (vec jvm-output)))
    ;; `field`/`set-field!` remain equivalent spellings.
    (is (= (:state (:ir artifact))
           (let [equivalent (assoc-in definition [:body 0]
                                      '(let [previous (field z :x1)]
                                         (do (set-field! z :x1 sample)
                                             (+ previous (field z :x2)))))]
             (:state (:ir (dsp/compile equivalent {:entry :process}))))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"declared state record"
         (dsp/compile (assoc-in definition [:body 0] '(:missing nope))
                      {:entry :process})))))

(deftest circular-delay-lines-and-interpolation
  (let [render (fn [definition controls frames]
                 (let [artifact (dsp/compile definition {:target :jvm :entry :process
                                                         :precision :f32})
                       input (float-array (assoc (vec (repeat frames 0.0)) 0 1.0))
                       output (float-array frames)]
                   (apply dsp/invoke artifact input output frames controls)
                   (vec output)))
        nonzero (fn [xs] (vec (keep-indexed (fn [index x]
                                              (when (not= 0.0 x) index))
                                            xs)))
        delay-line (resolve 'omkamra.dsp.library.delay/delay-line)
        allpass (resolve 'omkamra.dsp.library.delay/allpass-delay-line)]
    ;; Ordering: the sample is written before the taps are read, so a zero
    ;; delay returns the current input.
    (is (= [1.0 0.0 0.0 0.0] (render delay-line [0.0] 4)))
    ;; An integer delay moves the impulse by exactly that many samples.
    (is (= [3] (nonzero (render delay-line [3.0] 6))))
    ;; A half-sample delay splits the impulse across the two neighbouring taps.
    (let [out (render delay-line [3.5] 6)]
      (is (= [0.5 0.5] [(nth out 3) (nth out 4)])))
    ;; The first-order allpass tap is stable and centred on the same delay.
    (let [out (render allpass [3.5] 8)]
      (is (= 3 (first (nonzero out))))
      (is (every? #(Float/isFinite %) out)))
    (is (= [1.0 0.0 0.0 0.0] (render allpass [0.0] 4)))))

(deftest record-buffers-and-element-indices
  (let [definition {:dsp/kind :function
                    :name 'record-buffer
                    :params [{:name 'sample :type :float} {:name 'index :type :int}]
                    :state [{:name 'store
                             :fields [{:name :line :type :float :size 4}
                                      {:name :cursor :type :int :init 0}]}]
                    :process {:input :sample :controls [{:name 'index :type :int}]}
                    :return-type :float
                    :body ['(let [previous (aget store index)]
                              (do (aset store index sample)
                                  (set-field! store :cursor (+ (:cursor store) 1))
                                  previous))]}
        artifact (dsp/compile definition {:entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 2.0 3.0]) output (float-array 3) jvm-output (float-array 3)]
    ;; A record owns a contiguous 4-slot buffer followed by its scalar cursor.
    (is (= [{:name 'store.line :type :float :size 4
             :init [0.0 0.0 0.0 0.0] :index 0}
            {:name 'store.cursor :type :int :init 0 :index 4}]
           (:state (:ir artifact))))
    (is (= [{:path [:line] :type :float :offset 0 :size 4}
            {:path [:cursor] :type :int :offset 4 :size 1}]
           (get-in artifact [:abi :records 0 :fields])))
    (apply dsp/invoke artifact input output 3 [1])
    (apply dsp/invoke jvm input jvm-output 3 [1])
    (is (= (vec output) (vec jvm-output)))
    ;; `(field rec :line k)` is the explicit spelling of the element index.
    (is (= (:state (:ir artifact))
           (:state (:ir (dsp/compile
                         (assoc-in definition [:body 0]
                                   '(let [previous (field store :line index)]
                                      (do (set-field! store :line index sample)
                                          (set-field! store :cursor (+ (field store :cursor) 1))
                                          previous)))
                         {:entry :process})))))
    ;; An array field without an index is a place, so it cannot be a scalar
    ;; result; a scalar field rejects an element index.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"array places must be indexed"
         (dsp/compile (assoc-in definition [:body 0] '(field store :line))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not take an element index"
         (dsp/compile (assoc-in definition [:body 0] '(field store :cursor 0))
                      {:entry :process})))
    ;; `aget`/`aset` need exactly one array field to drop the field name.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not expose a single array place"
         (dsp/compile (assoc-in definition [:state 0 :fields]
                                [{:name :cursor :type :int :init 0}])
                      {:entry :process})))
    ;; `(aget (:line store) k)` is the same place as `(aget store k)`, and a
    ;; `let` may alias it.
    (is (= (:state (:ir artifact))
           (:state (:ir (dsp/compile
                         (assoc-in definition [:body 0]
                                   '(let [buf (:line store)]
                                      (let [previous (aget buf index)]
                                        (do (aset buf index sample)
                                            (set-field! store :cursor (+ (:cursor store) 1))
                                            previous))))
                         {:entry :process})))))
    ;; An array of records is not one array place: its rows are strided, so the
    ;; terse `(aget store k)` needs both indices (see `record-array-fields`).
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"record index and an element index"
         (dsp/compile (assoc-in definition [:state 0]
                                {:name 'store :size 2
                                 :fields [{:name :line :type :float :size 4}]})
                      {:entry :process})))))

(deftest record-array-fields
  ;; An array of records may own an array field (H5): the leaf is one flat block
  ;; of `records * elements` slots carrying `:stride`, so a record index and an
  ;; element index fold into one flat index (`record * stride + element`) and
  ;; every backend keeps a constant base offset and a single element index.
  (let [wavetable {:dsp/kind :function
                   :name 'wavetable-bank
                   :params [{:name 'sample :type :float}
                            {:name 'bank :type :int}
                            {:name 'index :type :int}]
                   :state [{:name 'tables
                            :size 3
                            :constant true
                            :fields [{:name :samples :type :float :size 4
                                      :init [0.0 0.25 0.5 0.75]}]}]
                   :process {:input :sample
                             :controls [{:name 'bank :type :int}
                                        {:name 'index :type :int}]}
                   :return-type :float
                   :body ['(field tables bank :samples index)]}
        artifact (dsp/compile wavetable {:entry :process})
        jvm (dsp/compile wavetable {:target :jvm :entry :process})
        read-bank (fn [compiled bank index]
                    (let [input (float-array 1) output (float-array 1)]
                      (apply dsp/invoke compiled input output 1 [bank index])
                      (nth output 0)))
        cases [[2 3] [0 0] [1 2] [0 1]]]
    ;; One flat constant block, the field's initializers repeated once per
    ;; record, and the leaf carries the per-record stride.
    (is (= [{:name 'tables.samples :type :float :size 12
             :init [0.0 0.25 0.5 0.75 0.0 0.25 0.5 0.75 0.0 0.25 0.5 0.75]
             :index 0}]
           (:state (:ir artifact))))
    (is (= [{:path [:samples] :type :float :offset 0 :size 12 :stride 4}]
           (get-in artifact [:abi :records 0 :fields])))
    ;; Two dynamic indices folded into `bank * 4 + index`.
    (is (= {:op :binary :operator :+
            :left {:op :binary :operator :*
                   :left {:op :local :name 'bank :type :int}
                   :right {:op :const :type :int :value 4}
                   :type :int}
            :right {:op :local :name 'index :type :int}
            :type :int}
           (get-in artifact [:ir :frame-body :statements 0 :init :element-index])))
    ;; A fully constant access folds at compile time, so a bank read with literal
    ;; indices is still a constant.
    (is (= {:op :const :type :int :value 6}
           (get-in (dsp/compile (assoc-in wavetable [:body 0]
                                          '(field tables 1 :samples 2))
                                {:entry :process})
                   [:ir :frame-body :statements 0 :init :element-index])))
    ;; Every record reads its own slice of the bank, on the interpreter and the
    ;; JVM alike. The same shape is a shared catalog case
    ;; (`:constant-table-bank`), so it also runs on JavaScript and Wasm.
    (is (= [0.75 0.0 0.5 0.25] (mapv (fn [[bank index]] (read-bank artifact bank index))
                                     cases)))
    (is (= (mapv (fn [[bank index]] (read-bank artifact bank index)) cases)
           (mapv (fn [[bank index]] (read-bank jvm bank index)) cases)))
    ;; A record array owning an array field is a bank of writable buffers, and
    ;; the strided layout keeps the records independent.
    (let [lines {:dsp/kind :function
                 :name 'delay-bank
                 :params [{:name 'sample :type :float}
                          {:name 'which :type :int}
                          {:name 'index :type :int}]
                 :state [{:name 'voices
                          :size 2
                          :fields [{:name :line :type :float :size 3
                                    :init [1.0 2.0 3.0]}
                                   {:name :cursor :type :int :init 0}]}]
                 :process {:input :sample
                           :controls [{:name 'which :type :int}
                                      {:name 'index :type :int}]}
                 :return-type :float
                 :body ['(let [previous (field voices which :line index)]
                           (do (set-field! voices which :line index sample)
                               (+ previous
                                  (field voices (- 1 which) :line index))))]}
          explicit (dsp/compile lines {:entry :process})
          explicit-jvm (dsp/compile lines {:target :jvm :entry :process})
          terse (dsp/compile (assoc-in lines [:body 0]
                                       '(let [previous (aget voices which index)]
                                          (do (aset voices which index sample)
                                              (+ previous
                                                 (aget voices (- 1 which) index)))))
                             {:entry :process})
          render (fn [compiled]
                   (let [input (float-array [10.0 20.0 30.0])
                         output (float-array 3)]
                     (apply dsp/invoke compiled input output 3 [1 2])
                     (vec output)))]
      (is (= [{:name 'voices.line :type :float :size 6
               :init [1.0 2.0 3.0 1.0 2.0 3.0] :index 0}
              {:name 'voices.cursor :type :int :size 2 :init [0 0] :index 6}]
             (:state (:ir explicit))))
      (is (= [{:path [:line] :type :float :offset 0 :size 6 :stride 3}
              {:path [:cursor] :type :int :offset 6 :size 2}]
             (get-in explicit [:abi :records 0 :fields])))
      ;; `(aset voices i k v)` is exactly `(set-field! voices i :field k v)`.
      (is (= (:ir (:process (:ir explicit))) (:ir (:process (:ir terse)))))
      ;; Writing record 1 leaves record 0's slice alone, so the read-back of the
      ;; other record is the constant 3.0 while this record's value advances.
      ;; Each artifact is invoked once: an invocation continues the instance's
      ;; state, so a wrong stride would smear across records and show up here.
      (let [interpreter-run (render explicit)
            jvm-run (render explicit-jvm)
            terse-run (render terse)]
        (is (= [6.0 13.0 23.0] interpreter-run))
        (is (= interpreter-run jvm-run))
        (is (= interpreter-run terse-run))))
    ;; Both indices are required, and each is checked when it is constant.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"array field needs an element index"
         (dsp/compile (assoc-in wavetable [:body 0] '(field tables bank :samples))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"field needs a record index"
         (dsp/compile (assoc-in wavetable [:body 0] '(field tables :samples index))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"record index is outside the declared range"
         (dsp/compile (assoc-in wavetable [:body 0] '(field tables 3 :samples 0))
                      {:entry :process})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"array index is outside the declared range"
         (dsp/compile (assoc-in wavetable [:body 0] '(field tables 0 :samples 4))
                      {:entry :process})))
    ;; A bank of constant tables stays read-only.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"read-only"
         (dsp/compile (assoc-in wavetable [:body 0]
                                '(set-field! tables bank :samples index sample))
                      {:entry :process})))))

(deftest dynamic-memory-access-and-checks
  (let [definition {:dsp/kind :function
                    :name 'delayed-sample
                    :params [{:name 'sample :type :float}
                             {:name 'offset :type :int}]
                    :process {:input :sample
                              :controls [{:name :offset :type :int}]}
                    :return-type :float
                    :body ['(buffer-load :input offset)]}
        artifact (dsp/compile definition {:entry :process})
        input (float-array [10.0 20.0 30.0])
        output (float-array 2)]
    (is (= {:max-frame-offset 0
            :dynamic-index? true
            :element-bytes 4
            :alignment 4}
           (get-in artifact [:abi :memory :input])))
    (is (= [{:dsp/warning :dynamic-buffer-index
             :buffer-id :input
             :dsp/path [:memory :input]}]
           (:diagnostics (:abi artifact))))
    (dsp/process! artifact {:input input :output output :frames 2
                            :controls [1]})
    (is (= [20.0 30.0] (vec output)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"index is out of bounds"
         (dsp/process! artifact {:input input :output output :frames 1
                                 :controls [-1]})))
    (doseq [target [:jvm :js]]
      (is (= target
             (:target (dsp/compile definition {:target target :entry :process}))))))
  (let [artifact (dsp/compile shifted-buffer {:entry :process})]
    (is (= 1 (get-in artifact [:abi :memory :output :max-frame-offset])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"frame-access requirement"
         (dsp/invoke artifact (float-array [1.0 2.0])
                     (float-array [0.0 0.0]) 2 1.0)))))
