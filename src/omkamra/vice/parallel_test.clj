(ns omkamra.vice.parallel-test
  "Tests for the ordered parallel-map helper."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.parallel :as parallel]))

(deftest pmap-ordered-preserves-order
  (testing "results match a sequential mapv"
    (is (= (mapv inc (range 100)) (parallel/pmap-ordered inc (range 100) 4)))
    (is (= (mapv inc (range 100)) (parallel/pmap-ordered inc (range 100) 1))))
  (testing "degenerate inputs are handled"
    (is (= [] (parallel/pmap-ordered inc [] 4)))
    (is (= [0] (parallel/pmap-ordered identity [0] 4))))
  (testing "exceptions propagate"
    (is (thrown? Exception
                 (parallel/pmap-ordered #(/ 1 %) [1 0] 2)))))
