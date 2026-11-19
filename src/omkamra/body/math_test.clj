(ns omkamra.body.math-test
  "Tests for the vector and quaternion algebra. These run on the JVM, which is
  what proves the core is independent of Three.js and the browser."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.body.math :as m]))

(def ^:private epsilon 1.0e-9)

(defn- close? [a b]
  (< (m/q-angle a b) 1.0e-6))

(deftest vector-algebra
  (testing "addition, subtraction and scaling"
    (is (= [3 3 3] (m/v+ [1 1 1] [2 2 2])))
    (is (= [1 1 1] (m/v- [3 3 3] [2 2 2])))
    (is (= [2 4 6] (m/v-scale [1 2 3] 2))))
  (testing "unit vectors and degeneracy"
    (is (m/v-approx= [1.0 0.0 0.0] (m/v-unit [3 0 0]) epsilon))
    (is (= m/v-zero (m/v-unit m/v-zero))))
  (testing "cross product follows the right-hand rule"
    (is (m/v-approx= [0.0 0.0 1.0] (m/v-cross [1 0 0] [0 1 0]) epsilon))
    (is (m/v-approx= [1.0 0.0 0.0] (m/v-cross [0 1 0] [0 0 1]) epsilon))))

(deftest quaternion-algebra
  (testing "conjugation inverts"
    (let [q (m/q-from-axis-angle [0.3 1.0 0.2] 0.9)]
      (is (close? m/quat-identity (m/q-mul (m/q-conj q) q)))
      (is (close? m/quat-identity (m/q-mul q (m/q-conj q))))))
  (testing "rotation about an axis leaves the axis fixed"
    (let [q (m/q-from-axis-angle [0.0 1.0 0.0] 1.2)]
      (is (m/v-approx= [0.0 1.0 0.0] (m/q-rotate q [0.0 1.0 0.0]) 1.0e-9))))
  (testing "composition order: q-mul a b rotates by b, then a"
    (let [a (m/q-from-axis-angle [1.0 0.0 0.0] 0.5)
          b (m/q-from-axis-angle [0.0 0.0 1.0] 0.3)
          v [0.0 1.0 0.0]]
      (is (m/v-approx= (m/q-rotate a (m/q-rotate b v))
                       (m/q-rotate (m/q-mul a b) v)
                       1.0e-9))
      (testing "so the conjugate of a composed rotation recovers the tail"
        (is (close? b (m/q-mul (m/q-conj a) (m/q-mul a b))))))))

(deftest euler-round-trip
  (doseq [euler [[0.0 0.0 0.0]
                 [0.4 0.0 0.0]
                 [0.0 -1.1 0.0]
                 [0.0 0.0 2.2]
                 [0.4 -0.9 1.3]
                 [-1.6 0.4 -0.7]]]
    (let [q (m/euler->q euler)]
      (is (< (m/q-angle q (m/euler->q (m/q->euler q))) 1.0e-9)
          (str "euler round trip for " euler)))))

(deftest slerp-endpoints
  (let [a (m/q-from-axis-angle [0.0 0.0 1.0] 0.2)
        b (m/q-from-axis-angle [0.0 0.0 1.0] 2.4)]
    (is (close? a (m/q-slerp a b 0.0)))
    (is (close? b (m/q-slerp a b 1.0)))
    (testing "slerp of nearly identical rotations stays a unit quaternion"
      (let [c (m/q-from-axis-angle [0.0 0.0 1.0] 0.2000001)
            blended (m/q-slerp a c 0.5)]
        (is (= 4 (count blended)))
        (is (< (m/abs (- 1.0 (m/q-norm blended))) 1.0e-9))))))

(deftest from-to-rotation
  (let [from [0.0 -1.0 0.0]
        to (m/v-unit [0.2 0.6 1.0])
        q (m/q-from-to from to)]
    (is (m/v-approx= to (m/q-rotate q from) 1.0e-9)))
  (testing "antiparallel vectors still produce a usable rotation"
    (let [q (m/q-from-to [0.0 1.0 0.0] [0.0 -1.0 0.0])]
      (is (m/v-approx= [0.0 -1.0 0.0] (m/q-rotate q [0.0 1.0 0.0]) 1.0e-9)))))

(deftest clamping
  (is (= 0.5 (m/clamp 0.2 0.5 1.0)))
  (is (= 1.0 (m/clamp 2.0 0.5 1.0)))
  (is (= 0.7 (m/clamp 0.7 0.5 1.0))))