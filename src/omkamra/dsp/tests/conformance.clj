(ns omkamra.dsp.tests.conformance
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.conformance :as conformance]))

(deftest interpreter-oracle-conformance
  ;; The interpreter computes expected outputs from the case data; the JVM
  ;; backend is compared against it. No hand-written reference is involved.
  (doseq [case catalog/all-cases
          precision (:precisions case)]
    (testing (str (:id case) " " (name precision)
                  (when (catalog/function-case? case) " function"))
      (let [plan (conformance/plan case precision)
            expected (conformance/run-plan
                      (conformance/target-artifact case precision :interpreter)
                      plan)
            actual (conformance/run-plan
                    (conformance/target-artifact case precision :jvm)
                    plan)]
        (is (conformance/results-match? :jvm precision expected actual))))))
