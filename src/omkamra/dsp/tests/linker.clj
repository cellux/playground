(ns omkamra.dsp.tests.linker
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [call-fixture-chain]]))

(deftest links-ordinary-calls
  (let [unit (dsp/link #'call-fixture-chain)
        lowered (dsp/compile unit)
        jvm (dsp/compile unit {:target :jvm})
        javascript (dsp/compile unit {:target :js})
        wasm (dsp/compile unit {:target :wasm :entry :process})
        entry (get-in unit [:definitions 'omkamra.dsp.harness.fixtures/call-fixture-chain])]
    (is (= 'omkamra.dsp.harness.fixtures/call-fixture-chain (:entry unit)))
    (is (= ['omkamra.dsp.harness.call-fixture/add-one
            'omkamra.dsp.harness.call-fixture/double-sample
            'omkamra.dsp.harness.fixtures/call-fixture-twice
            'omkamra.dsp.harness.fixtures/call-fixture-chain]
           (:order unit)))
    (is (= ['omkamra.dsp.harness.call-fixture/add-one
            'omkamra.dsp.harness.fixtures/call-fixture-twice]
           (get-in unit [:graph 'omkamra.dsp.harness.fixtures/call-fixture-chain])))
    (is (= '(dsp/call omkamra.dsp.harness.call-fixture/add-one
                      (dsp/call omkamra.dsp.harness.fixtures/call-fixture-twice sample))
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
