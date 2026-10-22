(ns omkamra.dsp.tests.equivalence
  "The exact comparison the compiler equivalence check rests on (P13, D79).

  Every test here is a way `=` would give the wrong answer: two numbers that are
  equal but are different values, two that are not equal but are the same value,
  and a value whose type only one platform can see."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp.harness.conformance :as conformance]
            [omkamra.dsp.harness.diff :as diff]))

(deftest numbers-compare-by-their-bits
  (testing "zero and negative zero are different values that `=` calls equal"
    (is (not (diff/same-leaf? 0.0 -0.0)))
    (is (not (diff/same-leaf? 0.0 (- 0.0)))
        "a computed negative zero is not zero"))
  (testing "NaN is one value, which `=` never says"
    (is (diff/same-leaf? Double/NaN Double/NaN))
    (is (diff/same-leaf? Float/NaN Float/NaN)))
  (testing "and the platform-blind case: an integral float and the integer"
    ;; On the JVM these are different types and `(= 1 1.0)` is false; in
    ;; ClojureScript they are the same value. The comparison is fair only if it
    ;; agrees with the platform that cannot tell them apart.
    (is (diff/same-leaf? 1 1.0))
    (is (= (diff/same-leaf? 1.0 1.0) (diff/same-leaf? 1 1)))))

(deftest a-difference-says-where-it-is
  (let [difference (diff/first-difference {:a [1 2 {:b 3.5}]} {:a [1 2 {:b 2.5}]})]
    (is (= [:a 2 :b] (:path difference)))
    (is (= 3.5 (:actual difference)))
    (is (= 2.5 (:expected difference))))
  (is (nil? (diff/first-difference {:a #{1 2} :b [1 2.0]} {:b [1 2] :a #{2 1}}))))

(deftest map-keys-are-a-set-and-not-an-order
  (testing "the same keys in a different order agree"
    (is (nil? (diff/first-difference (array-map :a 1 :b 2 :c 3)
                                     (array-map :c 3 :b 2 :a 1)))))
  (testing "a missing key is reported as the key sets, not as an absent value"
    (let [difference (diff/first-difference {:a 1 :b 2} {:a 1 :c 2})]
      (is (= [] (:path difference)))
      (is (= [":a" ":b"] (:actual difference)))
      (is (= [":a" ":c"] (:expected difference))))))

(deftest a-set-is-compared-as-a-whole
  (is (nil? (diff/first-difference #{1 2 3} #{3 1 2})))
  (is (some? (diff/first-difference #{1 2} #{1 2 3}))))

(deftest a-sequence-is-compared-position-by-position
  (is (nil? (diff/first-difference [1 2 3] [1 2 3])))
  (is (= [1] (:path (diff/first-difference [1 2 3] [1 9 3]))))
  (is (= [3] (:path (diff/first-difference [1 2 3] [1 2 3 4])))))

(deftest the-payload-is-the-catalog-the-browser-can-compile
  (let [{:keys [cases uncompilable]} (conformance/equivalence-payload)
        library (filter (comp :library? val) cases)]
    (is (= 60 (count cases)))
    (is (empty? uncompilable))
    (is (every? (fn [[id entry]]
                  (and (string? id)
                       (some? (:definition entry))
                       (some? (:normalized entry))
                       (some? (:unit entry))
                       (some? (:ir entry))))
                cases))
    (testing "the cases whose unit reaches a library definition are marked"
      ;; They are exactly the ones the browser cannot link until P13d gives it
      ;; a registry, so the mark is what keeps the check honest about the gap.
      (is (= 8 (count library)))
      (is (every? (fn [[_ entry]] (contains? entry :library?)) cases)))))
