(ns omkamra.dsp.tests.ir
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.fixtures :refer [add-bias boolean-gate clip gain
                                                  one-pole shifted-buffer stereo-cross
                                                  stereo-state-ramp sum-count]]))

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
