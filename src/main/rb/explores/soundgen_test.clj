(ns rb.explores.soundgen-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.cgen.core :as c]
            [rb.explores.soundgen :as soundgen]))

(c/defn ^double native-ramp [^double t ^double gain]
  (return (* t gain)))

(deftest native-render-uses-the-generic-cgen-binary-invocation
  (let [{:keys [sample-rate samples]}
        (soundgen/render-cgen native-ramp 0.01 1000 2.0)]
    (is (= 1000 sample-rate))
    (is (= 10 (alength ^doubles samples)))
    (is (= 0.0 (aget ^doubles samples 0)))
    (is (< (Math/abs (- 0.018 (aget ^doubles samples 9))) 1.0e-12))))

(deftest literal-cgen-kick-is-anonymous-and-playable
  (let [{:keys [sample-rate samples]}
        (soundgen/cgen-kick {:frequency 170.0
                             :end-frequency 38.0
                             :pitch-decay 0.035
                             :duration 0.01
                             :decay 0.3
                             :click-gain 0.0
                             :sample-rate 1000})]
    (is (= 1000 sample-rate))
    (is (= 10 (alength ^doubles samples)))
    (is (= 0.0 (aget ^doubles samples 0)))
    (is (some #(not (zero? %)) samples))))
