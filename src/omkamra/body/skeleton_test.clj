(ns omkamra.body.skeleton-test
  "Tests for the canonical skeleton, forward kinematics and joint limits."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def skeleton sk/canonical-skeleton)

(deftest inventory-contains-the-whole-body
  (testing "every pose-relevant region is represented"
    (doseq [region [:pelvis :spine :ribcage :head :shoulder :arm :hand :finger
                    :leg :foot :toe]]
      (is (seq (sk/region-bones skeleton region))
          (str "region " region " has joints"))))
  (testing "fingers and toes are mandatory, in detail"
    (is (= 30 (count (sk/region-bones skeleton :finger))))
    (is (= #{:thumb :index :middle :ring :little}
           (set (map (fn [{:keys [id]}]
                       (keyword (second (re-find #"left-(\w+)-" (name id)))))
                     (filter #(str/starts-with? (name (:id %)) "left-")
                             (sk/region-bones skeleton :finger))))))
    (is (= 2 (count (filter #(re-matches #"left-toe1-.*" (name (:id %)))
                            (sk/region-bones skeleton :toe))))))
  (testing "joints are parents-first, so a single pass evaluates the tree"
    (let [positions (into {} (map-indexed (fn [index id] [id index])
                                          (sk/joint-ids skeleton)))]
      (doseq [{:keys [id parent]} (sk/bones skeleton)
              :when parent]
        (is (< (get positions parent) (get positions id))
            (str id " comes after its parent " parent))))))

(deftest hierarchy
  (testing "chains run root first"
    (is (= [:pelvis :spine-lumbar-1 :spine-lumbar-2 :spine-lumbar-3
            :spine-lumbar-4 :spine-lumbar-5 :spine-thoracic-1 :left-scapula
            :left-clavicle :left-upper-arm :left-forearm :left-hand
            :left-index-proximal]
           (sk/chain skeleton :left-index-proximal))))
  (testing "descendants include the whole subtree"
    (is (= 15 (count (sk/descendants skeleton :left-hand))))
    (is (some #{:left-index-proximal} (sk/descendants skeleton :left-forearm)))))

(deftest rest-pose-geometry
  (let [transforms (sk/evaluate skeleton (pose/pose))]
    (testing "the pelvis sits at hip height and the head above it"
      (is (< (m/abs (- 0.95
                       (second (:position (get transforms :pelvis)))))
             1.0e-9))
      (is (> (second (:position (get transforms :head)))
             (second (:position (get transforms :pelvis))))))
    (testing "the body is mirror-symmetric about the YZ plane"
      (doseq [[left right] [[:left-hand :right-hand]
                            [:left-thigh :right-thigh]
                            [:left-foot :right-foot]]]
        (let [[lx ly lz] (:position (get transforms left))
              [rx ry rz] (:position (get transforms right))]
          (is (< (m/abs (+ lx rx)) 1.0e-9) (str left " and " right " mirror X"))
          (is (< (m/abs (- ly ry)) 1.0e-9))
          (is (< (m/abs (- lz rz)) 1.0e-9)))))
    (testing "the left limbs are on +X, the body's left"
      (is (pos? (first (:position (get transforms :left-hand)))))
      (is (neg? (first (:position (get transforms :right-hand))))))))

(deftest forward-kinematics-follows-parents
  (let [bend (pose/set-control skeleton (pose/pose) :left-knee-flex 1.0)
        transforms (sk/evaluate skeleton bend)
        knee (:position (get transforms :left-shin))
        ankle (:position (get transforms :left-foot))]
    (testing "a bent knee swings the foot behind the knee"
      (is (< (nth ankle 2) (nth knee 2))))
    (testing "the rest of the body does not move"
      (is (= (:position (get transforms :pelvis))
             (:position (get (sk/evaluate skeleton (pose/pose)) :pelvis)))))))

(deftest joint-limits
  (testing "clamping an impossible rotation reports a violation"
    (let [rotation (m/q-from-axis-angle [0.0 0.0 1.0] 1.0)
          [clamped violations] (sk/rotation->euler-limits skeleton :spine-lumbar-1 rotation)]
      (is (seq violations))
      (is (> (m/q-angle clamped rotation) 1.0e-3)
          "the clamped rotation differs from the impossible input")
      (is (seq (:violations (sk/clamp skeleton (pose/set-rotation (pose/pose)
                                                                  :spine-lumbar-1 rotation))))))
    (testing "a legal rotation is untouched and reports nothing"
      (let [rotation (m/q-from-axis-angle [1.0 0.0 0.0] 0.1)
            [clamped violations] (sk/rotation->euler-limits skeleton :spine-lumbar-1 rotation)]
        (is (empty? violations))
        (is (< (m/q-angle clamped rotation) 1.0e-9)))))
  (testing "clamp keeps the pose otherwise identical"
    (let [original (pose/set-control skeleton (pose/pose) :left-elbow-flex 0.5)
          {:keys [pose violations]} (sk/clamp skeleton original)]
      (is (empty? violations))
      (is (= (:bones original) (:bones pose))))))

(deftest semantic-controls
  (testing "every documented control exists"
    (doseq [control-id [:spine-flex :spine-twist :spine-side
                        :neck-flex :neck-turn :neck-side
                        :left-shoulder-flex :left-shoulder-abduct :left-scapula-elevate
                        :left-elbow-flex :left-wrist-flex :left-wrist-deviate
                        :left-fingers-curl :left-fingers-spread :left-thumb-curl
                        :left-hip-flex :left-hip-abduct :left-hip-rotate :left-knee-flex
                        :left-ankle-flex :left-toes-curl
                        :right-shoulder-flex :right-knee-flex :right-toes-curl]]
      (is (some? (sk/control skeleton control-id))
          (str "control " control-id))))
  (testing "positive flexion bends the knee the anatomically right way"
    (let [bend (pose/set-control skeleton (pose/pose) :left-knee-flex 1.0)
          transforms (sk/evaluate skeleton bend)]
      (is (< (nth (:position (get transforms :left-foot)) 2)
             (nth (:position (get transforms :left-shin)) 2)))))
  (testing "shoulder abduction raises the left arm outward and upward"
    (let [raised (pose/set-control skeleton (pose/pose) :left-shoulder-abduct 1.0)
          hand (:position (get (sk/evaluate skeleton raised) :left-hand))]
      (is (> (first hand) 0.4))
      (is (> (second hand) 1.2))))
  (testing "controls are readable and stay within their range"
    (let [raised (pose/set-control skeleton (pose/pose) :left-shoulder-abduct 1.0)]
      (is (< (m/abs (- 1.0 (pose/control-value skeleton raised :left-shoulder-abduct)))
             1.0e-6)))))