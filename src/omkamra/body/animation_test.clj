(ns omkamra.body.animation-test
  "Tests for clips, sampling, blending and deterministic playback."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.animation :as animation]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def skeleton sk/canonical-skeleton)

(defn- control-at [clip time control-id]
  (pose/control-value skeleton (animation/sample clip time) control-id))

(defn- sample-clip []
  (let [a (pose/pose)
        b (pose/set-control skeleton (pose/pose) :left-shoulder-abduct 1.0)]
    (animation/transitions [a b a] 2000 {:loop? false})))

(deftest clip-construction
  (let [clip (sample-clip)]
    (is (animation/clip? clip))
    (is (= 3 (count (animation/keys-of clip))))
    (is (= 2000 (animation/duration clip)))
    (is (false? (animation/loop? clip))))
  (testing "keys are sorted by time"
    (let [a (pose/pose)
          b (pose/set-control skeleton (pose/pose) :spine-flex 0.2)
          clip (animation/clip {:keys [{:time 1000 :pose b} {:time 0 :pose a}]})]
      (is (= [0 1000] (mapv :time (animation/keys-of clip))))))
  (testing "duplicate key times are rejected"
    (is (thrown? clojure.lang.ExceptionInfo
                 (animation/clip {:keys [{:time 0 :pose (pose/pose)}
                                         {:time 0 :pose (pose/pose)}]}))))
  (testing "an empty clip is rejected"
    (is (thrown? clojure.lang.ExceptionInfo (animation/clip {:keys []})))))

(deftest sampling
  (let [clip (sample-clip)]
    (testing "the clip starts at rest and peaks at the second key"
      (is (< (control-at clip 0 :left-shoulder-abduct) 1.0e-6))
      (is (< (m/abs (- 1.0 (control-at clip 1000 :left-shoulder-abduct))) 1.0e-6))
      (is (< (control-at clip 2000 :left-shoulder-abduct) 1.0e-6)))
    (testing "sampling before the start and after the end clamps"
      (is (= (animation/sample clip -100) (animation/sample clip 0)))
      (is (= (animation/sample clip 5000) (animation/sample clip 2000))))
    (testing "sampling is monotonic through a rising segment"
      (let [values (map #(control-at clip % :left-shoulder-abduct) (range 0 1001 100))]
        (is (= values (sort values)))))))

(deftest easing-shapes
  (is (= 0.0 (animation/easing :linear 0.0)))
  (is (= 1.0 (animation/easing :linear 1.0)))
  (is (= 0.5 (animation/easing :smoothstep 0.5)))
  (is (< (animation/easing :ease-in 0.25) 0.25))
  (is (> (animation/easing :ease-out 0.25) 0.25))
  (is (= 0.5 (animation/easing :ease-in-out 0.5))))

(deftest blending
  (let [a (pose/pose)
        b (pose/set-control skeleton (pose/pose) :left-knee-flex 1.0)
        half (animation/blend a b 0.5)]
    (is (< (m/abs (- 0.5 (pose/control-value skeleton half :left-knee-flex))) 1.0e-6))
    (testing "blending partial poses treats missing joints as identity"
      (let [partial {:omkamra.body.pose/version 1
                     :skeleton sk/skeleton-id
                     :root {:translation [0.0 0.0 0.0]
                            :rotation m/quat-identity}
                     :bones {:head (m/q-from-axis-angle [1 0 0] 1.0)}}
            result (animation/blend a partial 0.5)]
        (is (< (m/abs (- 0.5 (nth (pose/joint-euler result :head) 0))) 1.0e-6)))))
  (testing "the root transform blends too"
    (let [a (pose/pose)
          b (pose/translate-root (pose/pose) [1.0 0.0 0.0])
          half (animation/blend a b 0.5)]
      (is (= [0.5 0.0 0.0] (get-in half [:root :translation]))))))

(deftest timeline-playback
  (let [clip (sample-clip)
        timeline (animation/timeline clip)
        advanced (animation/advance clip timeline 500)]
    (testing "advance moves the timeline and samples a pose"
      (is (= 500.0 (:time advanced)))
      (is (pose/pose? (:pose advanced)))
      (is (false? (:finished? advanced))))
    (testing "a paused timeline does not move"
      (let [paused (animation/pause timeline)
            still (animation/advance clip paused 500)]
        (is (= 0.0 (:time still)))))
    (testing "the rate scales the advance"
      (let [fast (animation/set-rate timeline 2.0)
            advanced (animation/advance clip fast 250)]
        (is (= 500.0 (:time advanced)))))
    (testing "seeking is clamped into the clip"
      (is (= 2000.0 (:time (animation/seek clip timeline 9999))))
      (is (= 0.0 (:time (animation/seek clip timeline -5)))))
    (testing "a non-looping clip finishes and stays finished"
      (let [ended (reduce (fn [timeline _]
                            (:timeline (animation/advance clip timeline 500)))
                          timeline
                          (range 5))]
        (is (animation/finished? clip ended))
        (is (= 2000.0 (:time ended))))))
  (testing "looping wraps around"
    (let [a (pose/pose)
          b (pose/set-control skeleton (pose/pose) :spine-flex 0.4)
          clip (animation/transitions [a b] 1000 {:loop? true})
          timeline (animation/timeline clip {:time 900})
          advanced (animation/advance clip timeline 300)]
      (is (= 200.0 (:time advanced)))
      (is (false? (:finished? advanced))))))

(deftest serialization-round-trip
  (let [clip (sample-clip)
        data (animation/to-data skeleton clip)
        restored (animation/from-data skeleton data)]
    (is (= 3 (count (animation/keys-of restored))))
    (is (= (animation/duration clip) (animation/duration restored)))
    (testing "all sampled poses survive the round trip"
      (doseq [time [0 500 1000 1500 2000]]
        (is (= (:bones (animation/sample clip time))
               (:bones (animation/sample restored time))))))))