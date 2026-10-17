(ns omkamra.dsp.tests.descriptor
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.harness.fixtures :refer [declared-development-gain explicit-gain gain]]))

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
    (is (= 'omkamra.dsp.harness.fixtures (:namespace (:source explicit-gain))))
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
            :loop-bound nil
            :wasm-math :polynomial}
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

(deftest capability-diagnostics-name-what-a-target-serves
  (testing "a default is always something the target actually serves"
    (is (= :development (get-in (dsp/compile gain) [:options :checks]))
        "the interpreter implements runtime validation")
    (doseq [target [:jvm :js :wasm]]
      (is (= :none (get-in (dsp/compile gain {:target target :entry :process})
                           [:options :checks]))
          (str target " must not claim validation it does not implement"))))
  (testing "the entry default is not derived from the target"
    ;; Deliberate: the same definition compiled for JS and for Wasm must not
    ;; quietly become a function on one and a process on the other. Wasm serves
    ;; one entry, so a defaulted entry is an unsupported request and says so.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"it serves :process entries, not :function"
         (dsp/compile gain {:target :wasm})))
    (is (= :function (:entry (dsp/compile gain)))
        "the default entry is unchanged, and still valid where it is served"))
  (testing "runtime validation is an interpreter-only capability"
    (doseq [target [:jvm :js :wasm]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"it serves :checks :none, not :development"
           (dsp/compile gain {:target target :checks :development}))
          (name target)))
  ;; The positive twin. Without it the loop above would pass if the check
  ;; rejected every target, or if :development were unsupported everywhere.
    (is (= :development (:checks (:options (dsp/compile gain {:target :interpreter
                                                              :checks :development}))))))
  (testing "the Wasm lookup table exists only at f32"
    (is (= :table (:wasm-math (:options (dsp/compile gain {:target :wasm
                                                           :entry :process
                                                           :wasm-math :table})))))
    (is (= :polynomial (:wasm-math (:options (dsp/compile gain {:target :wasm
                                                                :entry :process
                                                                :precision :f64
                                                                :wasm-math :polynomial})))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"it serves :wasm-math :polynomial at :f64, not :table"
         (dsp/compile gain {:target :wasm :entry :process :precision :f64
                            :wasm-math :table}))))
  (testing "Wasm serves the process entry only"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"it serves :process entries, not :function"
         (dsp/compile gain {:target :wasm :entry :function})))
    (is (= :process (:entry (dsp/compile gain {:target :wasm :entry :process})))))
  (testing "every unsupported request is reported, not just the first"
    (let [data (try (dsp/compile gain {:target :wasm :entry :function
                                       :precision :f64 :wasm-math :table})
                    nil
                    (catch clojure.lang.ExceptionInfo error (ex-data error)))]
      (is (= :unsupported-capability (:dsp/error data)))
      (is (= :wasm (:target data)))
    ;; Two distinct asks are unsupported here; one error lists both.
      (is (= #{:entry :wasm-math} (set (map :capability (:capabilities data)))))))
  (testing "a definition may declare an option a target cannot serve"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"it serves :checks :none, not :development"
         (dsp/compile declared-development-gain {:target :js :entry :process})))
    ;; The positive twin: the same declaration is honoured where it can be, so
    ;; the error above is about the target rather than about the declaration.
    (is (= :development (:checks (:options (dsp/compile declared-development-gain)))))
    ;; A definition declaring something its target does serve is fine.
    (is (= :none (:checks (:options (dsp/compile explicit-gain
                                                 {:target :js :entry :process})))))))

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
         #"must declare output"
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
