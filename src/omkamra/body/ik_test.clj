(ns omkamra.body.ik-test
  "Tests for the deterministic inverse kinematics solver."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.ik :as ik]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def skeleton sk/canonical-skeleton)
(def identity-pose (pose/pose))

(defn- target-error [result limb]
  (:error (first (filter #(= limb (:limb %)) (get-in result [:diagnostics :targets])))))

(defn- solved-pose [result] (:pose result))

(deftest reachable-single-target
  (doseq [[limb position] [[:left-arm [0.35 1.55 0.15]]
                           [:right-arm [-0.35 1.55 0.15]]
                           [:left-leg [0.12 0.10 0.10]]
                           [:right-leg [-0.12 0.10 -0.10]]]]
    (testing (str limb " reaches " position)
      (let [result (ik/solve skeleton identity-pose
                             {:targets {limb {:position position}}
                              :adjust-root? false
                              :mode :best-effort})]
        (is (= :ok (:status result)))
        (is (< (target-error result limb) 0.006))))))

(deftest multi-limb-pose
  (let [result (ik/solve skeleton identity-pose
                         {:targets {:left-arm {:position [0.4 1.5 0.2]}
                                    :right-arm {:position [-0.4 1.5 0.2]}
                                    :left-leg {:position [0.15 0.08 0.15]}
                                    :right-leg {:position [-0.15 0.08 -0.15]}}
                          :directions {:left-elbow [0.4 0 -1.0]
                                       :right-elbow [-0.4 0 -1.0]}
                          :mode :best-effort})]
    (is (= :ok (:status result)))
    (doseq [{:keys [limb error ok?]} (get-in result [:diagnostics :targets])]
      (is ok? (str limb " error " error)))))

(deftest pole-direction-changes-the-bend-plane
  (let [target {:left-arm {:position [0.35 1.2 0.35]}}
        backwards (ik/solve skeleton identity-pose
                            {:targets target
                             :directions {:left-elbow [0.0 0.0 -1.0]}
                             :adjust-root? false :passes 1})
        outwards (ik/solve skeleton identity-pose
                           {:targets target
                            :directions {:left-elbow [1.0 0.0 0.0]}
                            :adjust-root? false :passes 1})
        elbow-z (fn [result]
                  (nth (:position (get (sk/evaluate skeleton (solved-pose result))
                                       :left-forearm))
                       2))
        elbow-x (fn [result]
                  (nth (:position (get (sk/evaluate skeleton (solved-pose result))
                                       :left-forearm))
                       0))]
    (is (< (elbow-z backwards) (elbow-z outwards))
        "elbows asked to point backwards sit further back than elbows pointing sideways")
    (is (> (elbow-x outwards) (elbow-x backwards)))))

(deftest best-effort-and-strict-modes
  (let [unreachable {:targets {:left-arm {:position [2.0 3.0 1.0]}}}
        best (ik/solve skeleton identity-pose (assoc unreachable :mode :best-effort
                                                     :adjust-root? false))
        strict (ik/solve skeleton identity-pose (assoc unreachable :mode :strict))]
    (testing "best-effort returns the closest pose plus diagnostics"
      (is (= :best-effort (:status best)))
      (is (pos? (:error (first (get-in best [:diagnostics :targets])))))
      (is (zero? (m/v-distance (:position (get (sk/evaluate skeleton (solved-pose best)) :pelvis))
                               (:position (get (sk/evaluate skeleton identity-pose) :pelvis))))))
    (testing "strict mode refuses and leaves the pose untouched"
      (is (= :unreachable (:status strict)))
      (is (= identity-pose (solved-pose strict)))
      (is (seq (get-in strict [:diagnostics :targets]))))))

(deftest root-adjustment
  (testing "an out-of-reach target pulls the root towards it when allowed"
    (let [target {:left-arm {:position [0.55 1.7 0.4]}}
          with-root (ik/solve skeleton identity-pose
                              {:targets target :mode :best-effort
                               :adjust-root? true :root-adjust-limit 0.3})
          locked (ik/solve skeleton identity-pose
                           {:targets target :mode :best-effort
                            :adjust-root? true :locks #{:root}
                            :root-adjust-limit 0.0})]
      (is (pos? (m/v-len (get-in with-root [:diagnostics :root-adjustment]))))
      (is (= 0.0 (m/v-len (get-in locked [:diagnostics :root-adjustment]))))))
  (testing "root adjustment stays within its limit"
    (let [result (ik/solve skeleton identity-pose
                           {:targets {:left-arm {:position [3.0 3.0 3.0]}}
                            :adjust-root? true :root-adjust-limit 0.1})
          adjustment (get-in result [:diagnostics :root-adjustment])]
      (is (<= (m/v-len adjustment) (+ 0.1001 1.0e-6))))))

(deftest solved-poses-respect-limits
  (let [result (ik/solve skeleton identity-pose
                         {:targets {:left-arm {:position [0.3 1.6 0.2]}
                                    :right-leg {:position [-0.2 0.1 0.4]}}
                          :mode :best-effort})
        violations (sk/limit-violations skeleton (solved-pose result))]
    (is (empty? violations))))

(deftest determinism
  (let [options {:targets {:left-arm {:position [0.4 1.5 0.2]}
                           :right-leg {:position [-0.2 0.1 0.3]}}
                 :mode :best-effort}
        a (ik/solve skeleton identity-pose options)
        b (ik/solve skeleton identity-pose options)]
    (is (= (solved-pose a) (solved-pose b)))
    (is (= (:diagnostics a) (:diagnostics b)))))