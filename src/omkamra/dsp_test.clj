(ns omkamra.dsp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            #_{:clj-kondo/ignore [:unused-namespace]}
            [omkamra.dsp.call-fixture :as calls]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.acceptance :as acceptance])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(dsp/defn gain
  [sample amount]
  (* sample amount))

(dsp/defn add-bias
  [sample]
  (+ sample 0.25))

(def one-pole (:definition (acceptance/case-by-id :one-pole)))

(defn- thread-allocation-meter
  []
  (let [bean (ManagementFactory/getThreadMXBean)]
    (when (and (instance? ThreadMXBean bean)
               (.isThreadAllocatedMemorySupported ^ThreadMXBean bean))
      (when-not (.isThreadAllocatedMemoryEnabled ^ThreadMXBean bean)
        (.setThreadAllocatedMemoryEnabled ^ThreadMXBean bean true))
      {:bean bean
       :thread-id (.getId (Thread/currentThread))})))

(defn- allocated-bytes
  [{:keys [bean thread-id]} f]
  (let [before (.getThreadAllocatedBytes ^ThreadMXBean bean thread-id)]
    (f)
    (- (.getThreadAllocatedBytes ^ThreadMXBean bean thread-id) before)))

(defn- raw-process-noop
  [_artifact _input _output _frames _sample-rate _float-controls _int-controls]
  nil)

(dsp/defn clip
  [sample lower upper]
  (if (< sample lower)
    lower
    (if (> sample upper)
      upper
      sample)))

(def gated-one-pole (:definition (acceptance/case-by-id :gated-one-pole)))

(dsp/defn sum-count
  [count]
  (let [i 0.0
        sum 0.0]
    (do
      (while (< i count)
        (do
          (set! sum (+ sum i))
          (set! i (+ i 1.0))))
      sum)))

(dsp/defn boolean-gate
  [sample lower upper]
  (if (and (>= sample lower)
           (not (> sample upper)))
    sample
    0.0))

(dsp/defn shifted-buffer
  [sample amount]
  (do
    (buffer-store :output 1 (+ sample amount))
    (+ (buffer-load :input 1) sample)))

(dsp/defn channel-mix
  [sample]
  (+ sample (buffer-load :input 1 0)))

(dsp/defn explicit-gain
  [{:params [{:name sample :type :float}
             {:name amount :type :float}]
    :return-type :float
    :process {:input :sample
              :controls ['amount]}
    :options {:checks :none}}
   (* sample amount)])

(dsp/defn call-fixture-twice
  [sample]
  (calls/double-sample (calls/double-sample sample)))

(dsp/defn call-fixture-chain
  [sample]
  (calls/add-one (call-fixture-twice sample)))

(deftest definition-is-captured-as-data
  (is (= :function (:dsp/kind gain)))
  (is (= ['sample 'amount] (:params gain)))
  (is (= '(* sample amount) (first (:body gain))))
  (is (= :float (:return-type gain))))

(deftest descriptors-normalize
  (let [shorthand (descriptor/normalize gain)
        explicit (descriptor/normalize explicit-gain)]
    (is (= [{:name 'sample :type :float}
            {:name 'amount :type :float}]
           (:params explicit)))
    (is (= (:params shorthand)
           (:params (descriptor/normalize
                     (assoc gain :params ['sample 'amount])))))
    (is (= 'omkamra.dsp-test (:namespace (:source explicit-gain))))
    (is (integer? (:line (:source explicit-gain))))
    (is (integer? (:column (:source explicit-gain))))
    (is (= [{:name 'amount :type :float}]
           (get-in explicit [:process :controls])))
    (is (= :none (get-in explicit [:options :checks])))))

(deftest shorthand-and-explicit-descriptors-normalize-identically
  (is (= (descriptor/normalize gain)
         (descriptor/normalize
          {:dsp/kind :function
           :name 'gain
           :params [{:name 'sample :type :float}
                    {:name 'amount :type :float}]
           :return-type :float
           :body ['(* sample amount)]
           :source (:source gain)}))))

(deftest options-are-visible-and-validated
  (let [artifact (dsp/compile gain)]
    (is (= {:target :interpreter
            :entry :function
            :precision :f32
            :channels 1
            :checks :development
            :optimize false
            :loop-bound nil}
           (:options artifact))))
  (testing "unknown options"
    (let [error (try
                  (dsp/compile gain {:not-an-option true})
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :invalid-definition (-> error ex-data :dsp/error)))
      (is (= [:options :not-an-option] (-> error ex-data :dsp/path)))
      (is (some? (-> error ex-data :malli/explanation)))))
  (testing "invalid logical types"
    (let [error (try
                  (descriptor/normalize
                   {:dsp/kind :function
                    :name 'bad-type
                    :params [{:name 'sample :type :decimal}]
                    :body [1.0]})
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :invalid-definition (-> error ex-data :dsp/error)))
      (is (= [:params 0 0] (-> error ex-data :dsp/path)))
      (is (some? (-> error ex-data :malli/explanation))))))

(deftest state-and-process-validation
  (let [normalized (descriptor/normalize
                    {:dsp/kind :function
                     :name 'typed-state
                     :params [{:name 'sample :type :float}]
                     :state [{:name 'enabled
                              :type :boolean
                              :init false
                              :next true}]
                     :body [1.0]})]
    (is (= [{:name 'enabled :type :boolean :init false :next true}]
           (:state normalized))))
  (testing "state transitions must match their declarations"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"state transition"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'bad-state
           :params [{:name 'sample :type :float}]
           :state [{:name 'enabled
                    :type :boolean
                    :init false
                    :next 1.0}]
           :body [1.0]}))))
  (testing "process metadata"
    (let [normalized (descriptor/normalize explicit-gain)]
      (is (= {:name 'sample :type :float :channels 1}
             (get-in normalized [:process :input])))
      (is (= {:name 'output :type :float :channels 1}
             (get-in normalized [:process :output])))
      (is (= {:init true :reset true}
             (get-in normalized [:process :lifecycle]))))))

(deftest validation-rejects-malformed-descriptors
  (testing "parameter and state names share one scope"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"parameter and state names"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'colliding
           :params ['sample]
           :state [{:name 'sample}]
           :body [1.0]}))))
  (testing "process metadata is validated before IR lowering"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must name a symbol"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'missing-input
           :params []
           :process {}
           :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"ports must have type"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'bad-port
           :params ['sample]
           :process {:input {:name 'sample :type :boolean}}
           :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"signature names must be unique"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'duplicate-signature
           :params ['sample 'output]
           :process {}
           :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"buffer channel counts"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'bad-buffer
           :params ['sample]
           :process {:buffers [{:name :input :direction :input :channels 0}
                               {:name :output :direction :output}]}
           :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"input and output buffers"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'missing-buffers
           :params ['sample]
           :process {:buffers [{:name :state :direction :state}]}
           :body [1.0]}))))
  (testing "control defaults are typed source values"
    (is (= 0.5
           (get-in (descriptor/normalize
                    {:dsp/kind :function
                     :name 'defaulted
                     :params ['sample 'amount]
                     :process {:controls [{:name 'amount :default 0.5}]}
                     :body [1.0]})
                   [:process :controls 0 :default])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"initializers must be numeric"
         (descriptor/normalize
          {:dsp/kind :function
           :name 'bad-default
           :params ['sample 'amount]
           :process {:controls [{:name 'amount :default false}]}
           :body [1.0]})))))

(deftest unsupported-host-values-are-rejected-before-lowering
  (let [error (try
                (descriptor/normalize
                 {:dsp/kind :function
                  :name 'bad-host-value
                  :params ['sample]
                  :body [(Object.)]})
                nil
                (catch clojure.lang.ExceptionInfo error error))]
    (is (= :invalid-definition (-> error ex-data :dsp/error)))
    (is (= [:body 0] (-> error ex-data :dsp/path)))))

(deftest macro-validation-uses-structured-diagnostics
  (let [error (try
                ((deref #'dsp/defn)
                 '(dsp/defn invalid-macro-definition [sample sample]
                    sample)
                 {}
                 'invalid-macro-definition
                 '[sample sample]
                 'sample)
                nil
                (catch clojure.lang.ExceptionInfo error error))]
    (is (= :invalid-definition (-> error ex-data :dsp/error)))
    (is (= [:params] (-> error ex-data :dsp/path)))
    (is (map? (-> error ex-data :dsp/source)))))

(deftest inferred-dependencies-reject-explicit-declarations
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"invalid shape"
       (descriptor/normalize
        {:dsp/kind :function
         :name 'old-style-dependencies
         :params ['sample]
         :dependencies ['other]
         :body ['sample]}))))

(deftest links-ordinary-calls
  (let [unit (dsp/link #'call-fixture-chain)
        lowered (dsp/compile unit)
        jvm (dsp/compile unit {:target :jvm})
        javascript (dsp/compile unit {:target :js})
        wasm (dsp/compile unit {:target :wasm :entry :process})
        entry (get-in unit [:definitions 'omkamra.dsp-test/call-fixture-chain])]
    (is (= 'omkamra.dsp-test/call-fixture-chain (:entry unit)))
    (is (= ['omkamra.dsp.call-fixture/add-one
            'omkamra.dsp.call-fixture/double-sample
            'omkamra.dsp-test/call-fixture-twice
            'omkamra.dsp-test/call-fixture-chain]
           (:order unit)))
    (is (= ['omkamra.dsp.call-fixture/add-one
            'omkamra.dsp-test/call-fixture-twice]
           (get-in unit [:graph 'omkamra.dsp-test/call-fixture-chain])))
    (is (= '(dsp/call omkamra.dsp.call-fixture/add-one
                      (dsp/call omkamra.dsp-test/call-fixture-twice sample))
           (first (:body entry))))
    (is (= (float 9.0) (dsp/invoke lowered 2.0)))
    (is (= (float 9.0) (dsp/invoke jvm 2.0)))
    (is (not (str/includes? (:source javascript) "dsp/call")))
    (is (str/includes? (:wat wasm) "f32.mul"))))

(deftest links-anonymous-definitions
  (let [helper {:dsp/kind :function
                :name 'helper
                :params ['sample]
                :body ['(* sample 3.0)]
                :source {:namespace 'dsp.anonymous}}
        entry {:dsp/kind :function
               :name 'entry
               :params ['sample]
               :body ['(helper sample)]
               :source {:namespace 'dsp.anonymous}}
        unit (dsp/link entry {:definitions {'dsp.anonymous/helper helper}})]
    (is (= ['dsp.anonymous/helper 'dsp.anonymous/entry] (:order unit)))
    (is (= (float 6.0) (dsp/invoke (dsp/compile unit) 2.0)))))

(deftest linker-diagnostics
  (testing "missing calls retain definition source and form path"
    (let [error (try
                  (dsp/link {:dsp/kind :function
                             :name 'missing
                             :params ['sample]
                             :body ['(unknown sample)]
                             :source {:namespace 'dsp.missing}})
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :invalid-definition (-> error ex-data :dsp/error)))
      (is (= [:body 0] (-> error ex-data :dsp/path)))
      (is (= '(unknown sample) (-> error ex-data :dsp/form)))
      (is (= 'dsp.missing/unknown (-> error ex-data :resolved-id)))))
  (testing "cycles are rejected with their dependency path"
    (let [a {:dsp/kind :function :name 'a :params ['x]
             :body ['(b x)] :source {:namespace 'dsp.cycle}}
          b {:dsp/kind :function :name 'b :params ['x]
             :body ['(a x)] :source {:namespace 'dsp.cycle}}
          error (try
                  (dsp/link a {:definitions {'dsp.cycle/b b}})
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :cyclic-dependency (-> error ex-data :dsp/error)))
      (is (= ['dsp.cycle/a 'dsp.cycle/b 'dsp.cycle/a]
             (-> error ex-data :dsp/cycle)))))
  (testing "an entry cannot conflict with an explicitly supplied definition"
    (let [entry {:dsp/kind :function :name 'entry :params ['x]
                 :body ['x] :source {:namespace 'dsp.duplicate}}
          duplicate (assoc entry :body [1.0])
          error (try
                  (dsp/link entry {:definitions {'dsp.duplicate/entry duplicate}})
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :invalid-definition (-> error ex-data :dsp/error)))
      (is (= 'dsp.duplicate/entry (-> error ex-data :definition-id))))))

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
    (is (str/includes? (:source javascript) "count | 0"))))

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
  (let [array-class (Class/forName "[F")
        input (into-array array-class [(float-array [1.0 2.0])
                                       (float-array [10.0 20.0])])
        output (into-array array-class [(float-array [99.0 99.0])
                                        (float-array [99.0 99.0])])]
    (doseq [target [:interpreter :jvm]]
      (let [artifact (dsp/compile channel-mix
                                  {:target target :entry :process :channels 2})]
        (is (= 2 (get-in artifact [:abi :inputs 0 :channels])))
        (dsp/invoke artifact input output 2)
        (is (= [[11.0 22.0] [20.0 40.0]]
               (mapv vec output)))))))

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

(deftest process-abi-runs-single-channel-blocks
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile gain {:target target :entry :process})
          input (float-array [1.0 -2.0 0.5])
          output (float-array [99.0 99.0 99.0 99.0])]
      (is (= :process (:entry artifact)))
      (is (= [{:name 'amount :type :float}]
             (:controls (:abi artifact))))
      (is (nil? (dsp/invoke artifact input output 2 2.0)))
      (is (= [2.0 -4.0 99.0 99.0] (vec output))))))

(deftest jvm-checked-and-unchecked-primitive-process-adapters
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            artifact (dsp/compile gain {:target :jvm
                                        :entry :process
                                        :precision precision})
            input (if f64?
                    (double-array [1.0 -2.0])
                    (float-array [1.0 -2.0]))
            output (if f64?
                     (double-array 2)
                     (float-array 2))
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (if f64?
          (aset-double ^doubles float-controls 0 2.0)
          (aset-float ^floats float-controls 0 (float 2.0)))
        (is (if f64?
              (instance? omkamra.dsp.jvm.DspProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.jvm.DspProcess (:unchecked artifact))))
        (is (nil? (dsp/process-checked! artifact input output 2 48000.0
                                        float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (java.util.Arrays/fill output (if f64? 0.0 (float 0.0)))
        (is (nil? (dsp/process-unchecked! artifact input output 2 48000.0
                                          float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"float controls have the wrong size"
             (dsp/process-checked! artifact input output 2 48000.0
                                   (if f64? (double-array 0) (float-array 0))
                                   int-controls)))))))

(deftest jvm-unchecked-process-adapter-supports-multichannel-arrays
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            array-class (if f64? (Class/forName "[D") (Class/forName "[F"))
            artifact (dsp/compile gain {:target :jvm
                                        :entry :process
                                        :channels 2
                                        :precision precision})
            input (into-array array-class
                              [(if f64? (double-array [1.0 2.0])
                                   (float-array [1.0 2.0]))
                               (if f64? (double-array [3.0 4.0])
                                   (float-array [3.0 4.0]))])
            output (into-array array-class
                               [(if f64? (double-array 2) (float-array 2))
                                (if f64? (double-array 2) (float-array 2))])
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (if f64?
          (aset-double ^doubles float-controls 0 2.0)
          (aset-float ^floats float-controls 0 (float 2.0)))
        (is (if f64?
              (instance? omkamra.dsp.jvm.DspMultiProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.jvm.DspMultiProcess (:unchecked artifact))))
        (dsp/process-checked! artifact input output 2 48000.0
                              float-controls int-controls)
        (is (= [[2.0 4.0] [6.0 8.0]] (mapv vec output)))
        (dsp/process-unchecked! artifact input output 2 48000.0
                                float-controls int-controls)
        (is (= [[2.0 4.0] [6.0 8.0]] (mapv vec output)))))))

(deftest jvm-unchecked-process-adapter-does-not-allocate-after-warmup
  (if-let [meter (thread-allocation-meter)]
    (let [artifact (dsp/compile gain {:target :jvm :entry :process})
          input (float-array (repeat 64 1.0))
          output (float-array 64)
          float-controls (get-in artifact [:controls :float])
          int-controls (get-in artifact [:controls :int])
          calls 10000]
      (aset-float ^floats float-controls 0 (float 2.0))
      (dotimes [_ calls]
        (raw-process-noop artifact input output 64 48000.0
                          float-controls int-controls)
        (dsp/process-unchecked! artifact input output 64 48000.0
                                float-controls int-controls))
      (let [baseline (allocated-bytes
                      meter
                      #(dotimes [_ calls]
                         (raw-process-noop artifact input output 64 48000.0
                                           float-controls int-controls)))
            processed (allocated-bytes
                       meter
                       #(dotimes [_ calls]
                          (dsp/process-unchecked! artifact input output 64 48000.0
                                                  float-controls int-controls)))]
        (is (<= processed baseline)
            (str "unchecked JVM process allocated " processed
                 " bytes; fixed-arity baseline allocated " baseline))))
    (is true "thread allocation counters are unavailable on this JVM")))

(deftest process-zero-frames-leaves-output-untouched
  (let [artifact (dsp/compile gain {:entry :process})
        input (float-array [1.0])
        output (float-array [7.0])]
    (is (nil? (dsp/invoke artifact input output 0 2.0)))
    (is (= [7.0] (vec output)))))

(deftest conditional-state-updates-are-evaluated-per-frame
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile gated-one-pole {:target target :entry :process})
          input (float-array [1.0 -1.0 0.0 1.0])
          output (float-array 4)]
      (dsp/invoke artifact input output 4 0.5)
      (is (= [1.0 0.0 0.0 1.5] (vec output))))))

(deftest stateful-process-preserves-state-between-blocks-and-resets
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile one-pole {:target target :entry :process})
          input (float-array [1.0 0.0 0.0])
          output (float-array 3)]
      (is (= [{:name 'previous :type :float :init 0.0}]
             (:state (:abi artifact))))
      (dsp/invoke artifact input output 3 0.5)
      (is (= [1.0 0.5 0.25] (vec output)))
      (dsp/invoke artifact (float-array [0.0]) output 1 0.5)
      (is (= 0.125 (aget output 0)))
      (dsp/reset! artifact)
      (dsp/invoke artifact (float-array [0.0]) output 1 0.5)
      (is (= 0.0 (aget output 0))))))

(deftest process-backends-match-reference-blocks
  (let [block-lengths [0 1 127 128 256]]
    (doseq [target [:interpreter :jvm]]
      (let [artifact (dsp/compile one-pole {:target target :entry :process})]
        (dsp/reset! artifact)
        (loop [block 0
               previous 0.0]
          (when (< block (count block-lengths))
            (let [frames (nth block-lengths block)
                  input (float-array
                         (map #(float (* 0.25
                                         (Math/sin (+ % (* 17 block)))))
                              (range frames)))
                  output (float-array (if (zero? frames)
                                        [77.0]
                                        (repeat frames 0.0)))
                  expected (float-array frames)
                  next-previous
                  (loop [frame 0
                         previous previous]
                    (if (= frame frames)
                      previous
                      (let [next (float (+ (double (aget input frame))
                                           (* 0.5 previous)))]
                        (aset-float expected frame next)
                        (recur (inc frame) next))))]
              (dsp/invoke artifact input output frames 0.5)
              (if (zero? frames)
                (is (= 77.0 (aget output 0)))
                (is (= (vec expected) (vec output))))
              (recur (inc block) next-previous))))))))

(deftest acceptance-cases-match-interpreter-and-jvm
  (doseq [case acceptance/cases
          precision (:precisions case)
          target [:interpreter :jvm]]
    (testing (str (:id case) " " (name precision) " " (name target))
      (let [artifact (dsp/compile (:definition case)
                                  {:target target
                                   :entry :process
                                   :precision precision})
            real-array (if (= :f64 precision) double-array float-array)]
        (dsp/reset! artifact)
        (loop [block 0
               state (acceptance/initial-state case)]
          (when (< block (count acceptance/block-lengths))
            (let [frames (nth acceptance/block-lengths block)
                  samples (acceptance/input-samples precision case block frames)
                  input (real-array samples)
                  output (real-array (if (zero? frames)
                                       [77.0]
                                       (repeat frames 0.0)))
                  [next-state expected] (acceptance/reference-block
                                         precision case state samples)]
              (dsp/process! artifact {:input input
                                      :output output
                                      :frames frames
                                      :sample-rate acceptance/sample-rate
                                      :controls (:controls case)})
              (if (zero? frames)
                (is (= 77.0 (aget output 0)))
                (is (every? true?
                            (map (fn [actual expected]
                                   (< (Math/abs (- actual expected))
                                      (if (= :f64 precision) 1.0e-12 1.0e-6)))
                                 (vec output) expected))))
              (recur (inc block) next-state))))
        (dsp/reset! artifact)
        (let [input (real-array [0.0])
              output (real-array [99.0])
              [_ expected] (acceptance/reference-block
                            precision case (acceptance/initial-state case) [0.0])]
          (dsp/process! artifact {:input input
                                  :output output
                                  :frames 1
                                  :sample-rate acceptance/sample-rate
                                  :controls (:controls case)})
          (is (= expected (vec output))))))))

(deftest process-abi-describes-instance-bindings-and-physical-memory
  (let [f32 (dsp/compile one-pole {:target :interpreter
                                   :entry :process
                                   :precision :f32})
        f64 (dsp/compile one-pole {:target :interpreter
                                   :entry :process
                                   :precision :f64})
        multi (dsp/compile gain {:target :interpreter
                                 :entry :process
                                 :channels 2})
        f32-artifacts [(dsp/compile one-pole {:target :jvm
                                              :entry :process
                                              :precision :f32})
                       (dsp/compile one-pole {:target :js
                                              :entry :process
                                              :precision :f32})
                       (dsp/compile one-pole {:target :wasm
                                              :entry :process
                                              :precision :f32})]
        f64-artifacts [(dsp/compile one-pole {:target :jvm
                                              :entry :process
                                              :precision :f64})
                       (dsp/compile one-pole {:target :js
                                              :entry :process
                                              :precision :f64})
                       (dsp/compile one-pole {:target :wasm
                                              :entry :process
                                              :precision :f64})]
        expected (fn [precision element-type bytes view]
                   {:precision precision
                    :logical-type :float
                    :element-type element-type
                    :element-bytes bytes
                    :alignment bytes
                    :views {:jvm view
                            :js (if (= :f64 precision) :float64-array :float32-array)
                            :wasm element-type}})]
    (is (= {:count 2 :binding :loop :type :int}
           (:channels (:abi multi))))
    (is (= {:role :channel-index :type :int :binding :loop}
           (get-in (:abi multi) [:bindings :channel])))
    (doseq [[artifact element-bytes alignment element-type]
            [[f32 4 4 :f32] [f64 8 8 :f64]]]
      (let [abi (:abi artifact)]
        (is (= 1 (:abi-version abi)))
        (is (= :process (:entry abi)))
        (is (= element-type (get-in abi [:memory-layout :element-type])))
        (is (= element-bytes (get-in abi [:memory-layout :element-bytes])))
        (is (= alignment (get-in abi [:memory-layout :alignment])))
        (is (= (expected (:precision abi) element-type element-bytes
                         (if (= :f64 (:precision abi)) :double-array :float-array))
               (:physical-type artifact)))
        (is (= (:physical-type artifact)
               (get-in abi [:memory-layout :physical-type])))
        (doseq [target-artifact (if (= :f64 (:precision abi))
                                  f64-artifacts
                                  f32-artifacts)]
          (is (= (:physical-type artifact)
                 (:physical-type target-artifact)))
          (is (= (:physical-type target-artifact)
                 (get-in (:abi target-artifact) [:physical-type]))))
        (is (= {:role :frame-count :argument-index 2}
               (select-keys (get-in abi [:bindings :frames])
                            [:role :argument-index])))
        (is (= {:role :sample-rate :argument-index 3}
               (select-keys (get-in abi [:bindings :sample-rate])
                            [:role :argument-index])))
        (is (= :instance (get-in abi [:state-layout :ownership])))
        (is (= [:init :reset] (:lifecycle-operations abi)))
        (is (= :persistent (get-in abi [:instance :state])))
        (is (nil? (dsp/initialize! artifact)))))))

(deftest stateful-functions-require-process-entry
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"require the :process entry"
       (dsp/compile one-pole))))

(deftest javascript-backend-emits-es-module
  (let [{:keys [source module] :as artifact}
        (dsp/compile one-pole {:target :js :entry :process})]
    (is (= :js (:target artifact)))
    (is (= {:format :es-module :exports [:createKernel]} module))
    (is (str/includes? source "export function createKernel()"))
    (is (str/includes? source "new Float32Array"))
    (is (str/includes? source "Math.fround"))
    (is (str/includes? source "function init()"))
    (is (str/includes? source "function reset()"))
    (is (str/includes? source "return {process, init, reset}"))))

(deftest javascript-and-jvm-f64-backends
  (let [javascript (dsp/compile one-pole {:target :js
                                          :entry :process
                                          :precision :f64})
        jvm (dsp/compile one-pole {:target :jvm
                                   :entry :process
                                   :precision :f64})
        scalar {:dsp/kind :function
                :name 'f64-add
                :params [{:name 'x :type :float}
                         {:name 'y :type :float}]
                :return-type :float
                :body ['(+ x y)]}
        scalar-js (dsp/compile scalar {:target :js :precision :f64})
        scalar-jvm (dsp/compile scalar {:target :jvm :precision :f64})
        input (double-array [1.0 0.0 -1.0])
        output (double-array 3)]
    (is (str/includes? (:source javascript) "Float64Array"))
    (is (not (str/includes? (:source javascript) "Math.fround")))
    (is (instance? (Class/forName "[D") (get-in jvm [:state :float])))
    (is (= :f64 (get-in jvm [:options :precision])))
    (is (not (str/includes? (:source scalar-js) "Math.fround")))
    (is (< (Math/abs (- 0.3 (dsp/invoke scalar-jvm 0.1 0.2)))
           1.0e-15))
    (dsp/process! jvm {:input input
                       :output output
                       :frames 3
                       :sample-rate 48000.0
                       :controls [0.5]})
    (is (= [1.0 0.5 -0.75] (vec output)))))

(deftest interpreter-f64-is-the-precision-reference
  (let [artifact (dsp/compile one-pole {:target :interpreter
                                        :entry :process
                                        :precision :f64})
        input (double-array [1.0 0.0 -1.0])
        output (double-array 3)]
    (is (= :f64 (get-in artifact [:ir :precision])))
    (is (instance? (Class/forName "[D") (:state artifact)))
    (dsp/process! artifact {:input input
                            :output output
                            :frames 3
                            :controls [0.5]})
    (is (= [1.0 0.5 -0.75] (vec output)))
    (dsp/reset! artifact)
    (is (= 0.0 (aget ^doubles (:state artifact) 0)))))

(deftest javascript-and-wasm-backends-emit-conditionals
  (let [{js-source :source} (dsp/compile clip {:target :js})
        {wat :wat} (dsp/compile clip {:target :wasm :entry :process})]
    (is (str/includes? js-source "if ("))
    (is (str/includes? js-source " < "))
    (is (str/includes? wat "f32.lt"))
    (is (str/includes? wat "(if "))))

(deftest javascript-and-wasm-backends-emit-all-comparisons
  (doseq [[operator javascript-token wasm-operator]
          [['= "===" "f32.eq"]
           ['not= "!==" "f32.ne"]
           ['< "<" "f32.lt"]
           ['<= "<=" "f32.le"]
           ['> ">" "f32.gt"]
           ['>= ">=" "f32.ge"]]]
    (let [definition {:dsp/kind :function
                      :name 'compare-values
                      :params ['x 'y]
                      :body [(list 'if (list operator 'x 'y) 1.0 0.0)]}
          {js-source :source} (dsp/compile definition {:target :js})
          {wat :wat} (dsp/compile definition {:target :wasm :entry :process})]
      (is (str/includes? js-source javascript-token))
      (is (str/includes? wat wasm-operator)))))

(deftest javascript-and-wasm-backends-emit-buffer-access
  (let [{js-source :source} (dsp/compile shifted-buffer
                                         {:target :js :entry :process})
        {wat :wat} (dsp/compile shifted-buffer
                                {:target :wasm :entry :process})]
    (is (str/includes? js-source "input[(frame + 1)]"))
    (is (str/includes? js-source "output[(frame + 1)]"))
    (is (str/includes? wat "(f32.load"))
    (is (str/includes? wat "(f32.store"))))

(deftest javascript-and-wasm-backends-emit-multichannel-access
  (let [{js-source :source} (dsp/compile channel-mix
                                         {:target :js :entry :process
                                          :channels 2})
        {wat :wat memory :memory}
        (dsp/compile channel-mix
                     {:target :wasm :entry :process :channels 2})]
    (is (str/includes? js-source "input[channel][frame]"))
    (is (str/includes? js-source "output[channel][frame]"))
    (is (str/includes? wat "$channel"))
    (is (= 8192 (:output-offset memory)))
    (is (= 16384 (:required-bytes memory)))))

(deftest wasm-backend-emits-wat-and-abi
  (let [{:keys [wat module memory] :as artifact}
        (dsp/compile one-pole {:target :wasm :entry :process})]
    (is (= :wasm (:target artifact)))
    (is (= {:format :wat :exports [:memory :init :reset :process]}
           (select-keys module [:format :exports])))
    (is (= {:page-size 65536
            :initial-pages 1
            :element-type :f32
            :element-bytes 4
            :alignment 4
            :frame-capacity 1024
            :channel-stride 4096
            :input-offset 0
            :output-offset 4096
            :required-bytes 8192}
           (select-keys memory [:page-size :initial-pages :element-type
                                :element-bytes :alignment :frame-capacity
                                :channel-stride :input-offset :output-offset
                                :required-bytes])))
    (is (= {:offset 0 :channels 1 :frame-capacity 1024 :bytes 4096
            :storage :linear-memory}
           (get-in memory [:regions :input])))
    (is (= {:storage :globals :count 1}
           (get-in memory [:regions :state])))
    (is (= :wasm-process (get-in artifact [:wasm-abi :kind])))
    (is (str/starts-with? wat "(module "))
    (is (str/includes? wat "(export \"init\")"))
    (is (str/includes? wat "(export \"reset\")"))
    (is (str/includes? wat "(export \"process\")"))
    (is (str/includes? wat "(global $state_0"))))

(deftest wasm-f64-artifact-emits-physical-layout
  (let [artifact (dsp/compile one-pole {:target :wasm
                                        :entry :process
                                        :precision :f64})]
    (is (str/includes? (:wat artifact) "f64.load"))
    (is (str/includes? (:wat artifact) "f64.store"))
    (is (= :f64 (get-in artifact [:memory :element-type])))
    (is (= 8 (get-in artifact [:memory :element-bytes])))
    (is (= 512 (get-in artifact [:memory :frame-capacity])))
    (is (= {:format :wasm :source :wat :generator :browser-wabt :bytes nil}
           (:binary artifact)))))

(deftest malformed-dsp-is-rejected
  (testing "unknown locals"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"unknown local"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['y]}))))
  (testing "unsupported operators"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"missing definition"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(Math/sin x)]})))))

(deftest typed-process-context-and-lifecycle
  (let [definition {:dsp/kind :function
                    :name 'typed-process
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'counter :type :int :init 0
                             :next '(+ counter step)}
                            {:name 'open :type :boolean :init false
                             :next 'enabled}]
                    :process {:input :sample
                              :frames {:name :block-frames}
                              :sample-rate {:name :hz}
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :return-type :float
                    :body ['(if open
                              (+ (int->float counter)
                                 (/ hz (int->float block-frames)))
                              sample)]}
        artifact (dsp/compile definition {:entry :process})
        input (float-array [1.0 2.0])
        output (float-array 2)]
    (is (= {:name 'block-frames :type :int} (get-in artifact [:abi :frames])))
    (is (= {:name 'hz :type :float} (get-in artifact [:abi :sample-rate])))
    (is (= [{:name 'enabled :type :boolean}
            {:name 'step :type :int}]
           (:controls (:abi artifact))))
    (is (= [{:name 'counter :type :int :init 0}
            {:name 'open :type :boolean :init false}]
           (:state (:abi artifact))))
    (is (= {:init true :reset true} (:lifecycle (:abi artifact))))
    (is (nil? (dsp/process! artifact {:input input
                                      :output output
                                      :frames 2
                                      :sample-rate 48000.0
                                      :controls [true 1]})))
    (is (= [1.0 24001.0] (vec output)))
    (dsp/reset! artifact)
    (dsp/process! artifact {:input input
                            :output output
                            :frames 1
                            :sample-rate 48000.0
                            :controls [false 3]})
    (is (= 1.0 (aget output 0)))
    (let [jvm-artifact (dsp/compile definition {:target :jvm :entry :process})
          jvm-output (float-array 2)]
      (is (= {:float-count 0 :int-count 2}
             (select-keys (:state-layout jvm-artifact) [:float-count :int-count])))
      (is (some #(= "omkamra.dsp.jvm.DspKernel" (.getName ^Class %))
                (.getInterfaces ^Class (:class jvm-artifact))))
      (is (some? (:kernel jvm-artifact)))
      (is (= {:float "[F" :int "[I"}
             (into {} (map (fn [[storage values]]
                             [storage (.getName (class values))])
                           (:state jvm-artifact)))))
      (dsp/process! jvm-artifact {:input input :output jvm-output :frames 2
                                  :sample-rate 48000.0
                                  :controls [true 1]})
      (is (= [1.0 24001.0] (vec jvm-output)))
      (dsp/reset! jvm-artifact)
      (dsp/process! jvm-artifact {:input input :output jvm-output :frames 1
                                  :sample-rate 48000.0
                                  :controls [false 3]})
      (is (= 1.0 (aget jvm-output 0))))))

(deftest jvm-process-supports-typed-controls-and-sample-rate
  (let [definition {:dsp/kind :function
                    :name 'typed-controls
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'previous :type :float :init 0.0
                             :next 'sample}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :body ['(if enabled
                              (+ sample
                                 (+ (int->float step)
                                    (/ sample-rate 1000.0)))
                              previous)]}
        artifact (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 2.0])
        output (float-array 2)]
    (dsp/process! artifact {:input input :output output :frames 2
                            :sample-rate 2000.0
                            :controls [true 3]})
    (is (= [6.0 7.0] (vec output)))))

(deftest interpreter-and-jvm-differential
  (let [definition {:dsp/kind :function
                    :name 'differential-process
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'count :type :int :init 0
                             :next '(+ count step)}
                            {:name 'previous :type :float :init 0.0
                             :next 'sample}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :body ['(if enabled
                              (+ sample (int->float count))
                              previous)]}
        interpreter (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        blocks [[1.0 2.0] [3.0] [4.0 5.0 6.0]]]
    (doseq [[block-index samples] (map-indexed vector blocks)]
      (let [input (float-array samples)
            interpreter-output (float-array (count samples))
            jvm-output (float-array (count samples))
            controls [(even? block-index) 2]]
        (dsp/process! interpreter {:input input :output interpreter-output
                                   :frames (count samples) :controls controls})
        (dsp/process! jvm {:input input :output jvm-output
                           :frames (count samples) :controls controls})
        (is (= (vec interpreter-output) (vec jvm-output)))))))

(deftest wasm-lowers-loop-control
  (let [definition {:dsp/kind :function
                    :name 'wasm-loop-control
                    :params [{:name 'sample :type :float}
                             {:name 'limit :type :int}]
                    :process {:input :sample
                              :controls [{:name :limit :type :int}]}
                    :body ['(let [index 0
                                  total sample]
                              (do
                                (while (< index limit)
                                  (do
                                    (set! index (+ index 1))
                                    (if (= index 2)
                                      (continue)
                                      (if (= index 4)
                                        (break)
                                        (set! total (+ total 1.0))))))
                                total))]}
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (str/includes? (:wat wasm) "$source-loop-1"))
    (is (str/includes? (:wat wasm) "$source-loop-done-1"))
    (is (str/includes? (:wat wasm) "(br $source-loop-1)"))
    (is (str/includes? (:wat wasm) "(br $source-loop-done-1)"))))

(deftest bounded-process-loops-are-lowered-across-native-targets
  (let [definition {:dsp/kind :function
                    :name 'bounded-process-loop
                    :params [{:name 'sample :type :float}
                             {:name 'limit :type :int}]
                    :process {:input :sample
                              :controls [{:name :limit :type :int}]}
                    :return-type :float
                    :body ['(let [index 0
                                  total sample]
                              (do
                                (while (< index limit)
                                  (do
                                    (set! index (+ index 1))
                                    (set! total (+ total 1.0)))
                                  {:max-iterations 2})
                                total))]}
        interpreter (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})
        process! (fn [artifact limit]
                   (let [input (float-array [1.0])
                         output (float-array 1)]
                     (dsp/process! artifact {:input input :output output :frames 1
                                             :controls [limit]})
                     (aget output 0)))]
    (is (= 2.0 (process! interpreter 1)))
    (is (= 2.0 (process! jvm 1)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"loop exceeded its iteration bound"
                          (process! interpreter 3)))
    (is (thrown-with-msg? IllegalStateException
                          #"loop exceeded its iteration bound"
                          (process! jvm 3)))
    (is (str/includes? (:wat wasm) "$source-loop-count-1"))
    (is (str/includes? (:wat wasm) "(unreachable)"))
    (let [f64-wasm (dsp/compile definition {:target :wasm :entry :process
                                            :precision :f64})
          f64-jvm (dsp/compile definition {:target :jvm :entry :process
                                           :precision :f64})
          input (double-array [1.0])
          output (double-array 1)]
      (is (str/includes? (:wat f64-wasm) "(local $source-loop-count-1 i32)"))
      (is (str/includes? (:wat f64-wasm) "(f64.const 1.0)"))
      (dsp/process! f64-jvm {:input input :output output :frames 1 :controls [1]})
      (is (= 2.0 (aget output 0)))
      (is (thrown-with-msg? IllegalStateException
                            #"loop exceeded its iteration bound"
                            (dsp/process! f64-jvm {:input input :output output
                                                   :frames 1 :controls [3]}))))))

(deftest typed-js-and-wasm-artifacts
  (let [definition {:dsp/kind :function
                    :name 'typed-artifact
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'amount :type :int}]
                    :state [{:name 'count :type :int :init 0
                             :next '(+ count amount)}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :amount :type :int}]}
                    :body ['(if enabled
                              (+ sample (int->float count))
                              sample)]}
        javascript (dsp/compile definition {:target :js :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (str/includes? (:source javascript) "sampleRate"))
    (is (str/includes? (:source javascript) "state[0] | 0"))
    (is (str/includes? (:wat wasm) "(global $state_0 (mut i32)"))
    (is (= false (get-in javascript [:realtime :allocations?])))
    (is (= false (get-in wasm [:realtime :allocations?])))))

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

(deftest malformed-conditionals-are-rejected
  (testing "float conditions"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"wrong type"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(if x 1.0 0.0)]}))))
  (testing "boolean values used as floats"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"wrong type"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(+ (< x 0.0) 1.0)]}))))
  (testing "logical operators require boolean operands"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"wrong type"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(if (and x true) 1.0 0.0)]}))))
  (testing "logical operators enforce arity"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"wrong arity"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(if (not x true) 1.0 0.0)]}))))
  (testing "buffer operations require a process entry"
    (is
     (thrown-with-msg?
      clojure.lang.ExceptionInfo
      #"unknown process buffer"
      (dsp/compile
       {:dsp/kind :function
        :name 'bad
        :params ['x]
        :body ['(buffer-load :input x)]})))))
