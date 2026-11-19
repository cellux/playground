(ns omkamra.body.intent-test
  "Tests for the intent layer: controls, raw joints, targets and diagnostics."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.intent :as intent]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def skeleton sk/canonical-skeleton)

(deftest intent-documents
  (let [i (intent/intent {:controls {:spine-flex 0.2}})]
    (is (intent/intent? i))
    (is (= sk/skeleton-id (:skeleton i)))
    (is (intent/intent? (intent/intent)))))

(deftest control-and-joint-resolution
  (let [result (intent/resolve skeleton
                               (intent/intent
                                {:controls {:left-knee-flex 0.5}
                                 :joints {:left-forearm {:euler [-0.4 0.0 0.0]}}}))]
    (is (= :ok (:status result)))
    (testing "the controls and joints are both applied"
      (is (not= m/quat-identity (pose/rotation (:pose result) :left-shin)))
      (is (< (m/q-angle (pose/rotation (:pose result) :left-forearm)
                        (m/euler->q [-0.4 0.0 0.0]))
             1.0e-6)))
    (testing "the diagnostics echo the semantic controls"
      (is (= 0.5 (get-in result [:diagnostics :controls :left-knee-flex]))))))

(deftest base-pose-is-the-starting-point
  (let [base (pose/set-control skeleton (pose/pose) :spine-twist 0.3)
        result (intent/resolve skeleton
                               (intent/intent {:base base
                                               :controls {:spine-flex 0.1}}))]
    (is (< (m/abs (- 0.3 (pose/control-value skeleton (:pose result) :spine-twist)))
           1.0e-6))
    (is (< (m/abs (- 0.1 (pose/control-value skeleton (:pose result) :spine-flex)))
           1.0e-6))))

(deftest multi-limb-intent
  (let [result (intent/resolve skeleton
                               (intent/intent
                                {:controls {:spine-flex 0.15}
                                 :targets {:left-arm {:position [0.35 1.5 0.25]}
                                           :right-arm {:position [-0.35 1.5 0.25]}
                                           :left-leg {:position [0.12 0.08 0.1]}}
                                 :directions {:left-elbow [0 0 -1]}
                                 :mode :best-effort}))]
    (is (= :ok (:status result)))
    (is (every? :ok? (get-in result [:diagnostics :targets])))))

(deftest strict-intent-failure
  (let [result (intent/resolve skeleton
                               (intent/intent
                                {:mode :strict
                                 :targets {:left-arm {:position [3.0 3.0 3.0]}}}))]
    (is (= :unreachable (:status result)))
    (is (seq (get-in result [:diagnostics :targets])))))

(deftest invalid-intents-are-reported-not-thrown
  (testing "unknown controls"
    (let [result (intent/resolve skeleton (intent/intent {:controls {:nope 1}}))]
      (is (= :invalid (:status result)))
      (is (= :unknown-control (:type (first (get-in result [:diagnostics :errors])))))))
  (testing "malformed targets"
    (let [result (intent/resolve skeleton (intent/intent {:targets {:left-arm {:position [0 1]}}}))]
      (is (= :invalid (:status result)))
      (is (= :invalid-target (:type (first (get-in result [:diagnostics :errors])))))))
  (testing "targets may name only supported limbs"
    (let [result (intent/resolve skeleton (intent/intent {:targets {:left-tentacle {:position [0 0 0]}}}))]
      (is (= :invalid (:status result)))))
  (testing "an invalid intent still returns a usable pose"
    (is (pose/pose? (:pose (intent/resolve skeleton (intent/intent {:controls {:nope 1}}))))))
  (testing "an unsupported version is invalid"
    (let [result (intent/resolve skeleton {})]
      (is (= :invalid (:status result)))
      (is (= :unsupported-version (:type (first (get-in result [:diagnostics :errors]))))))))

(deftest intent-data-is-plain-and-serializable
  (let [i (intent/intent {:controls {:spine-flex 0.2}
                          :targets {:left-arm {:position [0.3 1.4 0.2]}}
                          :directions {:left-elbow [0 0 -1]}})]
    (is (= 1 (:omkamra.body.intent/version i)))
    (is (every? number? (:position (:left-arm (:targets i)))))))

(deftest clamp-can-be-disabled
  (let [raw (intent/resolve skeleton
                            (intent/intent
                             {:controls {:left-elbow-flex 1.0}
                              :options {:clamp? false}}))]
    (is (= :ok (:status raw)))))

(deftest resolved-poses-are-limit-clean-by-default
  (let [result (intent/resolve skeleton
                               (intent/intent
                                {:controls {:left-shoulder-abduct 1.0
                                            :left-knee-flex 0.8}
                                 :targets {:right-arm {:position [-0.5 1.3 0.2]}}}))]
    (is (empty? (sk/limit-violations skeleton (:pose result))))))