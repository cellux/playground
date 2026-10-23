(ns omkamra.dsp.tests.ir
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.fixtures :refer [add-bias boolean-gate clip gain
                                                  one-pole shifted-buffer stereo-cross
                                                  stereo-state-ramp sum-count]]
            [omkamra.dsp.ir :as ir]))

(deftest canonical-form-is-ordered-and-explicit-about-numbers
  ;; The serialization two worlds compare and a cache keys on (D77). Its two
  ;; properties are exactly the two places the hosts differ, so each is pinned
  ;; by the fact it makes true.
  (testing "order is not a fact: a map or set built either way is one value"
    (is (= (ir/canonical-form {:a 1 :b 2})
           (ir/canonical-form {:b 2 :a 1})))
    (is (= (ir/canonical-form #{:a :b}) (ir/canonical-form #{:b :a})))
    (is (= (ir/canonical-text {:a [1 2] :b {:c 3}})
           (ir/canonical-text {:b {:c 3} :a [1 2]}))))
  (testing "every number is its bits, so zero and negative zero differ"
    (is (not= (ir/canonical-form 0.0) (ir/canonical-form -0.0)))
    (is (= {:dsp/number "0"} (ir/canonical-form 0.0))
        "positive zero is the all-zero pattern"))
  (testing "a value's type is carried by the IR's :type, not by the number"
    ;; The number is the same bits, which is the only cross-platform way to name
    ;; it; the `:type` beside it is what makes it a float.
    (is (= {:op :const :type :float :value {:dsp/number "4607182418800017408"}}
           (ir/canonical-form {:op :const :type :float :value 1.0})))
    (is (= {:op :const :type :int :value {:dsp/number "4607182418800017408"}}
           (ir/canonical-form {:op :const :type :int :value 1}))))
  (testing "NaN is one value, and its bits are that value"
    (is (= (ir/canonical-form ##NaN) (ir/canonical-form ##NaN)))
    (is (not= (ir/canonical-form ##NaN) (ir/canonical-form ##Inf)))))

(deftest interpreter-vertical-slice
  (let [artifact (dsp/compile gain {:target :interpreter})]
    (is (= :interpreter (:target artifact)))
    (is (= {:kind :function
            :name 'gain
            :params [:float :float]
            :return-type :float
            :effects #{}}
           (:abi artifact)))
    (is (= (float 6.0) (dsp/invoke artifact 2.0 3.0)))))

(deftest jvm-vertical-slice
  (let [artifact (dsp/compile gain {:target :jvm})]
    (is (= :jvm (:target artifact)))
    (is (class? (:class artifact)))
    (is (pos? (alength ^bytes (:bytes artifact))))
    (is (= (float 6.0) (dsp/invoke artifact 2.0 3.0)))))

(deftest constants-and-arithmetic-are-lowered
  (let [interpreter (dsp/compile add-bias)
        jvm (dsp/compile add-bias {:target :jvm})]
    (is (= (float 1.25) (dsp/invoke interpreter 1.0)))
    (is (= (float 1.25) (dsp/invoke jvm 1.0)))))

(deftest comparisons-and-conditionals-are-lowered
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile clip {:target target})]
      (is (= (float 0.0) (dsp/invoke artifact -1.0 0.0 1.0)))
      (is (= (float 0.25) (dsp/invoke artifact 0.25 0.0 1.0)))
      (is (= (float 1.0) (dsp/invoke artifact 2.0 0.0 1.0)))
      (is (Float/isNaN (dsp/invoke artifact Float/NaN 0.0 1.0)))
      (is (= (float 1.0) (dsp/invoke artifact Float/POSITIVE_INFINITY
                                     0.0
                                     1.0))))))

(deftest mutable-locals-and-while-are-lowered
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile sum-count {:target target})]
      (is (= (float 0.0) (dsp/invoke artifact 0.0)))
      (is (= (float 6.0) (dsp/invoke artifact 4.0)))
      (is (= (float 45.0) (dsp/invoke artifact 10.0))))))

(deftest uses-explicit-statements
  (let [ir (:ir (dsp/compile sum-count))
        statements (get-in ir [:body :statements])]
    (is (= :block (get-in ir [:body :op])))
    (is (= [:declare :declare :while :return]
           (mapv :op statements)))
    (is (= [:assign :assign]
           (mapv :op (get-in statements [2 :body :statements]))))))

(deftest typed-integer-ir-and-interpreter
  (let [definition {:dsp/kind :function
                    :name 'sum-integers
                    :params [{:name 'count :type :int}]
                    :return-type :int
                    :body ['(let [index 0
                                  total 0]
                              (do
                                (while (< index count)
                                  (do
                                    (set! total (+ total index))
                                    (set! index (+ index 1))))
                                total))]}
        artifact (dsp/compile definition)
        locals (get-in artifact [:ir :locals])]
    (is (= :int (:return-type (:ir artifact))))
    (is (= [{:name 'index :type :int}
            {:name 'total :type :int}]
           locals))
    (is (= 0 (dsp/invoke artifact 0)))
    (is (= 6 (dsp/invoke artifact 4)))
    (is (= 45 (dsp/invoke artifact 10)))))

(deftest bitwise-operators-reject-invalid-forms
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"wrong type"
       (dsp/compile {:dsp/kind :function
                     :name 'bad-bitwise-type
                     :params ['x]
                     :return-type :int
                     :body ['(bit-and x 1.0)]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"wrong arity"
       (dsp/compile {:dsp/kind :function
                     :name 'bad-bitwise-arity
                     :params ['a 'b]
                     :return-type :int
                     :body ['(bit-not a b)]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"wrong arity"
       (dsp/compile {:dsp/kind :function
                     :name 'bad-bitwise-unary
                     :params ['a]
                     :return-type :int
                     :body ['(bit-shift-left a)]}))))

(defn- operator-probe
  "Compile a one-expression function on `target` and invoke it."
  [body params return-type target & args]
  (apply dsp/invoke
         (dsp/compile {:dsp/kind :function
                       :name 'operator-probe
                       :params params
                       :return-type return-type
                       :body (vec body)}
                      {:target target})
         args))

(defn- operator-probe-refusal
  "The message and data of lowering a one-expression function that must fail."
  [body params return-type]
  (try
    (dsp/compile {:dsp/kind :function
                  :name 'operator-probe
                  :params params
                  :return-type return-type
                  :body (vec body)})
    :accepted
    (catch clojure.lang.ExceptionInfo e
      [(.getMessage e) (ex-data e)])))

(def ^:private probe-floats ['x 'y 'z])

(def ^:private probe-ints [{:name 'a :type :int}
                           {:name 'b :type :int}
                           {:name 'c :type :int}])

(deftest operators-fold-three-or-more-operands
  (testing "arithmetic folds left, exactly as the nested form, on both backends"
    (doseq [target [:interpreter :jvm]]
      (is (= (float 7.5)
             (operator-probe ['(+ x y z)] probe-floats :float target 1.5 2.0 4.0)))
      ;; `(- x y z)` is `((- x y) z)` and `(/ x y z)` is `((/ x y) z)`, which
      ;; no other grouping reproduces, so this pins the fold direction.
      (is (= (float -0.84375)
             (operator-probe ['(* (- x y z) (/ x y z))]
                             probe-floats :float target 1.5 2.0 4.0)))
      (is (= (operator-probe ['(+ (+ x y) z)] probe-floats :float target 1.5 2.0 4.0)
             (operator-probe ['(+ x y z)] probe-floats :float target 1.5 2.0 4.0)))))
  (testing "`and` folds left, and its value drives a branch"
    (doseq [target [:interpreter :jvm]]
      (is (= (float 1.0)
             (operator-probe ['(if (and (> x y) (< y z) (> z x)) 1.0 0.0)]
                             probe-floats :float target 2.0 1.0 3.0)))
      (is (= (float 0.0)
             (operator-probe ['(if (and (> x y) (< y z) (> z x)) 1.0 0.0)]
                             probe-floats :float target 1.5 2.0 4.0)))))
  (testing "the bitwise folders fold left"
    (doseq [target [:interpreter :jvm]]
      (is (= 0 (operator-probe ['(bit-and a b c)] probe-ints :int target 1 2 4)))
      (is (= 7 (operator-probe ['(bit-or a b c)] probe-ints :int target 1 2 4)))
      (is (= 7 (operator-probe ['(bit-xor a b c)] probe-ints :int target 1 2 4)))))
  (testing "folding is lowering-only: the IR keeps two-operand nodes"
    (let [binary-nodes (fn [body params return-type]
                         (->> (:body (:ir (dsp/compile {:dsp/kind :function
                                                        :name 'operator-probe
                                                        :params params
                                                        :return-type return-type
                                                        :body (vec body)})))
                              (tree-seq coll? seq)
                              (filter #(and (map? %) (= :binary (:op %))))
                              count))]
      (is (= 1 (binary-nodes ['(+ x y)] probe-floats :float)))
      (is (= 2 (binary-nodes ['(+ x y z)] probe-floats :float)))
      (is (= 3 (binary-nodes ['(+ x y z x)] probe-floats :float))
          "four operands are three nodes, not one n-ary node"))))

(deftest operators-keep-their-arity-floor
  (testing "arithmetic and the bitwise/logical folders need two operands"
    (doseq [[body params return-type operator]
            [[['(+ x)] probe-floats :float '+]
             [['(- x)] probe-floats :float '-]
             [['(and x)] probe-floats :boolean 'and]
             [['(or x)] probe-floats :boolean 'or]
             [['(bit-and a)] probe-ints :int 'bit-and]]]
      (let [[message data] (operator-probe-refusal body params return-type)]
        (is (str/includes? message "at least two operands") (str body))
        (is (= operator (:operator data)) (str body))
        (is (= 1 (:actual data)) (str body)))))
  (testing "chained comparisons are refused: they would read the middle twice"
    (doseq [body '[(< x y z) (= x y z) (not= x y z)]]
      (let [[message _] (operator-probe-refusal [body] probe-floats :boolean)]
        (is (str/includes? message "exactly two operands") (str body)))))
  (testing "shifts and `min` stay binary"
    (is (str/includes? (first (operator-probe-refusal ['(bit-shift-left a b c)]
                                                      probe-ints :int))
                       "wrong arity"))
    (is (str/includes? (first (operator-probe-refusal ['(min x y z)]
                                                      probe-floats :float))
                       "wrong number of arguments")))
  (testing "mixing int and float is still refused rather than promoted"
    (is (str/includes? (first (operator-probe-refusal ['(+ x 1)] probe-floats :float))
                       "wrong type"))))

(deftest bitwise-operators-are-emitted-by-js-and-wasm
  (let [definition (:definition (catalog/case-by-id :bitwise-lfsr))
        js (dsp/compile definition {:target :js :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (str/includes? (:source js) "&"))
    (is (str/includes? (:source js) "|"))
    (is (str/includes? (:source js) "^"))
    (is (str/includes? (:source js) "~"))
    (is (str/includes? (:source js) "<<"))
    (is (str/includes? (:source js) ">>"))
    (is (str/includes? (:source js) ">>>"))
    (is (str/includes? (:wat wasm) "i32.and"))
    (is (str/includes? (:wat wasm) "i32.or"))
    (is (str/includes? (:wat wasm) "i32.xor"))
    (is (str/includes? (:wat wasm) "i32.shl"))
    (is (str/includes? (:wat wasm) "i32.shr_s"))
    (is (str/includes? (:wat wasm) "i32.shr_u"))))

(deftest bounded-loops-and-loop-control
  (let [definition {:dsp/kind :function
                    :name 'bounded-sum
                    :params [{:name 'count :type :int}]
                    :return-type :int
                    :body ['(let [index 0
                                  total 0]
                              (do
                                (while (< index count)
                                  (do
                                    (set! index (+ index 1))
                                    (if (= index 3)
                                      (continue)
                                      (if (= index 6)
                                        (break)
                                        (set! total (+ total index)))))
                                  {:max-iterations 10})
                                total))]}
        option-definition {:dsp/kind :function
                           :name 'option-bounded
                           :params [{:name 'count :type :int}]
                           :return-type :int
                           :options {:loop-bound 4}
                           :body ['(let [index 0]
                                     (do
                                       (while (< index count)
                                         (set! index (+ index 1)))
                                       index))]}
        artifact (dsp/compile definition)
        option-artifact (dsp/compile option-definition)]
    (is (= 12 (dsp/invoke artifact 20)))
    (let [jvm (dsp/compile definition {:target :jvm})]
      (is (= 12 (dsp/invoke jvm 20))))
    (is (= 4 (dsp/invoke option-artifact 4)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"exceeded its iteration bound"
         (dsp/invoke option-artifact 10)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"only valid inside while"
         (dsp/compile
          {:dsp/kind :function
           :name 'bad-break
           :params []
           :return-type :int
           :body ['(do (break) 0)]})))))

(deftest conversions-and-boolean-values
  (let [conversion {:dsp/kind :function
                    :name 'rounded-offset
                    :params [{:name 'sample :type :float}]
                    :return-type :float
                    :body ['(int->float (+ (float->int sample) 2))]}
        predicate {:dsp/kind :function
                   :name 'invert
                   :params [{:name 'enabled :type :boolean}]
                   :return-type :boolean
                   :body ['(not enabled)]}]
    (is (= (float 5.0)
           (dsp/invoke (dsp/compile conversion) 3.9)))
    (is (false? (dsp/invoke (dsp/compile predicate) true)))
    (is (true? (dsp/invoke (dsp/compile predicate) false)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"wrong type"
         (dsp/compile
          {:dsp/kind :function
           :name 'bad-conversion
           :params [{:name 'count :type :int}]
           :return-type :float
           :body ['(int->float 1.0)]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not support"
         (dsp/compile
          {:dsp/kind :function
           :name 'bad-boolean-order
           :params [{:name 'enabled :type :boolean}]
           :return-type :boolean
           :body ['(< enabled false)]})))))

(deftest typed-ir-lowered-by-jvm-and-javascript
  (let [definition {:dsp/kind :function
                    :name 'integer-identity
                    :params [{:name 'count :type :int}]
                    :return-type :int
                    :body ['count]}
        jvm (dsp/compile definition {:target :jvm})
        javascript (dsp/compile definition {:target :js})]
    (is (= 7 (dsp/invoke jvm 7)))
    ;; The int result is coerced for the JavaScript target; the exact spelling of
    ;; the coercion belongs to Squint.
    (is (str/includes? (:source javascript) "| 0"))))

(deftest boolean-operators-short-circuit-and-compose
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile boolean-gate {:target target})]
      (is (= (float 0.5) (dsp/invoke artifact 0.5 0.0 1.0)))
      (is (= (float 0.0) (dsp/invoke artifact -0.5 0.0 1.0)))
      (is (= (float 0.0) (dsp/invoke artifact 1.5 0.0 1.0)))
      (is (= (float 0.0) (dsp/invoke artifact Float/NaN 0.0 1.0))))))

(deftest comparisons-have-consistent-float-semantics
  (doseq [[operator left right expected]
          [['< 0.0 1.0 1.0]
           ['< 1.0 0.0 0.0]
           ['<= 1.0 1.0 1.0]
           ['<= 1.0 0.0 0.0]
           ['> 1.0 0.0 1.0]
           ['> 0.0 1.0 0.0]
           ['>= 1.0 1.0 1.0]
           ['>= 0.0 1.0 0.0]
           ['= 1.0 1.0 1.0]
           ['= 1.0 0.0 0.0]
           ['= -0.0 0.0 1.0]
           ['not= 1.0 1.0 0.0]
           ['not= 1.0 0.0 1.0]
           ['= Float/NaN Float/NaN 0.0]
           ['not= Float/NaN Float/NaN 1.0]]]
    (let [definition {:dsp/kind :function
                      :name 'compare-values
                      :params ['x 'y]
                      :body [(list 'if (list operator 'x 'y) 1.0 0.0)]}]
      (doseq [target [:interpreter :jvm]]
        (is (= (float expected)
               (dsp/invoke (dsp/compile definition {:target target})
                           left
                           right)))))))

(deftest multichannel-processes-all-channels
  ;; D40: a multi-channel body writes each output channel explicitly; there is
  ;; no per-channel loop. `stereo-cross`: out0 = in0 + in1, out1 = in0 - in1.
  (let [array-class (Class/forName "[F")
        input (into-array array-class [(float-array [1.0 2.0])
                                       (float-array [3.0 4.0])])
        output (into-array array-class [(float-array [99.0 99.0])
                                        (float-array [99.0 99.0])])]
    (doseq [target [:interpreter :jvm]]
      (let [artifact (dsp/compile stereo-cross
                                  {:target target :entry :process})]
        ;; No `:channels` was declared: the compiler counted 2 from the body.
        (is (= 2 (get-in artifact [:abi :inputs 0 :channels])))
        (is (= 2 (get-in artifact [:abi :channels :count])))
        (dsp/invoke artifact input output 2)
        (is (= [[4.0 6.0] [-2.0 -2.0]]
               (mapv vec output)))))))

(deftest a-multi-form-defn-body-is-an-implicit-do
  ;; `dsp/defn` mirrors `clojure.core/defn`: several body forms become one
  ;; `(do ...)`, so `stereo-cross` needs no explicit `do`.
  (let [body (:body @#'stereo-cross)]
    (is (= 1 (count body)))
    (is (= 'do (first (first body))))))

(deftest stateful-multichannel-runs-once-per-frame
  ;; The point of D40: shared state advances once per frame, not once per
  ;; channel. A channel loop would advance `counter` twice as fast.
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile stereo-state-ramp
                                {:target target :entry :process})
          output (into-array (Class/forName "[F")
                             [(float-array 3) (float-array 3)])]
      (dsp/process! artifact {:output output :frames 3
                              :sample-rate 48000.0 :controls []})
      (is (= [[0.0 1.0 2.0] [0.0 2.0 4.0]] (mapv vec output))))))

(deftest explicit-buffer-loads-and-stores-work
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile shifted-buffer {:target target :entry :process})
          input (float-array [1.0 2.0 3.0])
          output (float-array [99.0 99.0 99.0])]
      (is (= [{:id :input :direction :input :channels 1 :type :float}
              {:id :output :direction :output :channels 1 :type :float}]
             (:buffers (:abi artifact))))
      (dsp/invoke artifact input output 2 10.0)
      (is (= [3.0 5.0 12.0] (vec output))))))

(deftest effects-and-memory-descriptors
  (let [artifact (dsp/compile shifted-buffer {:target :interpreter
                                              :entry :process})
        ir (:ir artifact)
        statements (get-in ir [:frame-body :statements])
        explicit-store (first statements)
        output-declaration (second statements)]
    (is (= #{:read-memory :write-memory} (:effects ir)))
    (is (= #{:read-memory :write-memory} (:effects (:abi artifact))))
    (is (= #{:write-memory} (:effects explicit-store)))
    (is (nil? (:effects (:value explicit-store))))
    (is (= {:op :buffer-store
            :buffer-id :output
            :channel 0
            :index {:op :frame-offset :offset 1 :type :int}}
           (select-keys explicit-store [:op :buffer-id :channel :index])))
    (is (= #{:read-memory}
           (get-in output-declaration [:init :left :effects]))))
  (let [artifact (dsp/compile one-pole {:target :interpreter :entry :process})
        ir (:ir artifact)]
    (is (= :block (get-in ir [:frame-body :op])))
    (is (= [{:id :input :direction :input :type :float :channels 1}
            {:id :output :direction :output :type :float :channels 1}]
           (:buffers ir)))
    (is (= #{:read-memory :write-memory :read-state :write-state}
           (:effects ir)))))

;; ---------------------------------------------------------------------------
;; Inlining (D47)
;; ---------------------------------------------------------------------------

;; A callee that asks to be copied into its call sites, and one that does not, so
;; a call site can be told apart by its callee's request.
(omkamra.dsp/defn ^:inline inlining-helper
  [sample]
  (* sample 2.0))

(omkamra.dsp/defn called-helper
  [sample]
  (+ sample 1.0))

(omkamra.dsp/defn calls-both-helpers
  [sample]
  (+ (inlining-helper sample) (called-helper sample)))

(deftest inlining-is-per-callee
  (let [calls (fn [ir]
                (into []
                      (comp (filter #(and (map? %) (= :call (:op %))))
                            (map :function-id))
                      (tree-seq coll? seq ir)))
        lowered (:ir (dsp/compile #'calls-both-helpers))
        all (ir/inline-calls lowered)
        none (ir/inline-calls lowered #{})
        partial (ir/inline-calls lowered #{'omkamra.dsp.tests.ir/inlining-helper})]
    (is (true? (:inline inlining-helper))
        "^:inline on the definition is captured")
    (is (not (contains? called-helper :inline))
        "a definition that asks for nothing carries no flag")
    ;; The one-argument call is the target that cannot call: nothing survives.
    (is (empty? (:definitions all)))
    (is (empty? (calls all)))
    ;; With nothing requested every definition stays a call, the one that asked
    ;; to be inlined included.
    (is (= 2 (count (:definitions none))))
    (is (= 2 (count (calls none))))
    ;; A subset is copied at its call sites only.
    (is (= 1 (count (:definitions partial)))
        "the requested callee is gone, the other stays")
    (is (= ['omkamra.dsp.tests.ir/called-helper] (calls partial))
        "and only the unrequested call site survives")))
