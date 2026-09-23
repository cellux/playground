(ns omkamra.supercollider.env-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider.env :as env]))

(deftest common-envelope-constructors
  (is (= [0.0 1.0 0.0]
         (:levels (env/perc 0.01 1.0))))
  (is (= [0.01 1.0]
         (:times (env/perc 0.01 1.0))))
  (is (= 2 (:release-node (env/adsr 0.01 0.3 0.5 1.0))))
  (is (= 1 (:release-node (env/asr 0.01 1.0 1.0))))
  (is (= 4 (count (:levels (env/linen 0.01 1.0 1.0))))))

(deftest envelope-array-uses-synthdef-format
  (is (= [0.0 2 -99 -99
          1.0 0.01 5 -4.0
          0.0 1.0 5 -4.0]
         (env/as-array (env/perc 0.01 1.0))))
  (is (= 1.01 (env/duration (env/perc 0.01 1.0))))
  (is (env/sustained? (env/asr 0.01 1.0 1.0))))

(deftest envelope-validation
  (is (thrown-with-msg? IllegalArgumentException
                          #"same size"
                          (env/step [0 1] [1])))
  (is (thrown-with-msg? IllegalArgumentException
                          #"unknown envelope curve"
                          (env/Env [0 1] [1] :unknown))))
