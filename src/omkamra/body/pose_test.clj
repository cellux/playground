(ns omkamra.body.pose-test
  "Tests for pose data, builders, validation and serialization."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def skeleton sk/canonical-skeleton)

(deftest identity-pose
  (let [p (pose/pose)]
    (is (pose/pose? p))
    (is (= {} (:bones p)))
    (is (= m/v-zero (get-in p [:root :translation])))
    (is (= m/quat-identity (get-in p [:root :rotation])))
    (is (pose/valid? skeleton p))))

(deftest builders-compose
  (testing "raw rotations are normalized"
    (let [p (pose/set-rotation (pose/pose) :left-forearm [0.0 0.0 0.0 2.0])]
      (is (< (m/abs (- 1.0 (m/q-norm (pose/rotation p :left-forearm)))) 1.0e-9))))
  (testing "rotate composes onto the existing rotation"
    (let [p (-> (pose/pose)
                (pose/rotate :left-forearm [1.0 0.0 0.0] 0.3)
                (pose/rotate :left-forearm [1.0 0.0 0.0] 0.2))]
      (is (< (m/q-angle (pose/rotation p :left-forearm)
                        (m/q-from-axis-angle [1.0 0.0 0.0] 0.5))
             1.0e-9))))
  (testing "root helpers add to the root transform"
    (let [p (-> (pose/pose)
                (pose/translate-root [0.1 0.0 0.0])
                (pose/translate-root [0.0 0.2 0.0]))]
      (is (= [0.1 0.2 0.0] (get-in p [:root :translation])))))
  (testing "merge-poses lets the overlay win per joint"
    (let [base (pose/set-control skeleton (pose/pose) :spine-flex 0.4)
          overlay (pose/set-control skeleton (pose/pose) :left-elbow-flex 0.5)
          merged (pose/merge-poses base overlay)]
      (is (not= m/quat-identity (pose/rotation merged :spine-lumbar-1)))
      (is (not= m/quat-identity (pose/rotation merged :left-forearm))))))

(deftest controls-write-only-their-axis
  (let [p (pose/set-control skeleton (pose/pose) :spine-flex 0.3)
        p (pose/set-control skeleton p :spine-twist 0.4)]
    (testing "both controls persist on the same joints"
      (is (< (m/abs (- 0.3 (pose/control-value skeleton p :spine-flex))) 1.0e-6))
      (is (< (m/abs (- 0.4 (pose/control-value skeleton p :spine-twist))) 1.0e-6)))))

(deftest validation-reports-problems
  (testing "a valid pose has no errors"
    (let [{:keys [valid? errors warnings]} (pose/validate skeleton (pose/pose))]
      (is valid?)
      (is (empty? errors))
      (is (empty? warnings))))
  (testing "unknown joints are errors"
    (let [{:keys [valid? errors]} (pose/validate skeleton
                                                 (assoc (pose/pose) :bones {:no-such-joint m/quat-identity}))]
      (is (not valid?))
      (is (= :unknown-joint (:type (first errors))))))
  (testing "malformed quaternions are errors"
    (let [{:keys [valid? errors]} (pose/validate skeleton
                                                 (assoc (pose/pose) :bones {:head [0.0 0.0 0.0]}))]
      (is (not valid?))
      (is (= :invalid-rotation (:type (first errors))))))
  (testing "limit violations are warnings, not errors"
    (let [p (pose/set-rotation (pose/pose) :spine-lumbar-1
                               (m/q-from-axis-angle [0.0 0.0 1.0] 1.0))
          {:keys [valid? warnings]} (pose/validate skeleton p)]
      (is valid?)
      (is (seq warnings))
      (is (= :limit-violation (:type (first warnings))))))
  (testing "a pose for another skeleton is rejected"
    (let [p (assoc (pose/pose) :skeleton :something-else)]
      (is (not (pose/valid? skeleton p))))))

(deftest serialization-round-trip
  (let [p (pose/set-control skeleton (pose/pose) :spine-flex 0.25)
        p (pose/set-control skeleton p :left-knee-flex 0.6)
        p (pose/translate-root p [0.0 0.1 0.0])
        data (pose/to-data skeleton p)
        restored (pose/from-data skeleton data)]
    (is (= (:bones p) (:bones restored)))
    (is (= (get-in p [:root :translation]) (get-in restored [:root :translation])))
    (testing "serialized values are plain numbers and vectors"
      (is (every? number? (get-in data [:root :translation])))
      (is (every? vector? (vals (:bones data))))))
  (testing "an invalid document cannot be serialized"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pose/to-data skeleton (assoc (pose/pose) :bones {:nope [1 2 3 4]})))))
  (testing "an unsupported version is rejected"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pose/from-data skeleton {:omkamra.body.pose/version 99})))))