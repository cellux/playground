(ns omkamra.supercollider.clock-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider.clock :as clock]))

(deftest beat-and-second-conversion
  (let [c (clock/create {:bpm 60})]
    (is (= 4.0 (clock/beat->seconds c 4.0)))
    (is (= 4.0 (clock/seconds->beat c 4.0)))
    (clock/set-tempo! c 4.0 120.0)
    (is (= 6.0 (clock/beat->seconds c 8.0)))
    (is (= 8.0 (clock/seconds->beat c 6.0)))
    (clock/stop! c)))

(deftest wake-scheduler-delivers-logical-target
  (let [c (clock/create)
        wake (clock/schedule-wake! c {:seconds 0.0})]
    (is (= {:logical-seconds 0.0 :target {:seconds 0.0}}
           (clojure.core.async/<!! wake)))
    (clock/stop! c)))
