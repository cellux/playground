(ns omkamra.body.math
  "Small, dependency-free vector and quaternion algebra for the body model.

  Conventions (D2 in DECISIONS.md):

  - vectors are plain `[x y z]` vectors, quaternions plain `[x y z w]` vectors;
  - the world is right-handed and Y-up; the body faces +Z, its left side is +X;
  - Euler angles use the `:xyz` order of Three.js, that is the rotation matrix
    `Rx * Ry * Rz`, so pose data converts to a Three.js `Euler` with order
    `XYZ` without reordering. A quaternion is the canonical rotation
    representation; Euler angles appear only at the limits and control
    boundary."
  (:refer-clojure :exclude [abs]))

(def pi
  "π as a platform-neutral constant."
  #?(:clj Math/PI :cljs js/Math.PI))

(defn sin [x] #?(:clj (Math/sin (double x)) :cljs (js/Math.sin x)))
(defn cos [x] #?(:clj (Math/cos (double x)) :cljs (js/Math.cos x)))
(defn sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn abs [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn asin [x] #?(:clj (Math/asin (double x)) :cljs (js/Math.asin x)))
(defn atan2 [y x] #?(:clj (Math/atan2 (double y) (double x)) :cljs (js/Math.atan2 y x)))
(defn acos [x] #?(:clj (Math/acos (double x)) :cljs (js/Math.acos x)))
(defn deg->rad [d] (* d (/ pi 180.0)))
(defn rad->deg [r] (* r (/ 180.0 pi)))

(defn clamp [x lower upper]
  (if (< x lower) lower (if (> x upper) upper x)))

(def v-zero [0.0 0.0 0.0])
(def v-up [0.0 1.0 0.0])

(defn v3 [x y z] [(double x) (double y) (double z)])
(defn v+ [a b] [(+ (nth a 0) (nth b 0)) (+ (nth a 1) (nth b 1)) (+ (nth a 2) (nth b 2))])
(defn v- [a b] [(- (nth a 0) (nth b 0)) (- (nth a 1) (nth b 1)) (- (nth a 2) (nth b 2))])
(defn v-scale [a s] [(* (nth a 0) s) (* (nth a 1) s) (* (nth a 2) s)])
(defn v-neg [a] (v-scale a -1.0))
(defn v-dot [a b] (+ (* (nth a 0) (nth b 0)) (* (nth a 1) (nth b 1)) (* (nth a 2) (nth b 2))))
(defn v-cross [a b]
  [(- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
   (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
   (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))])
(defn v-len [a] (sqrt (v-dot a a)))
(defn v-distance [a b] (v-len (v- a b)))
(defn v-lerp [a b t]
  (v+ (v-scale a (- 1.0 t)) (v-scale b t)))
(defn v-unit
  "Normalize `a`, returning `v-zero` for a degenerate vector."
  [a]
  (let [length (v-len a)]
    (if (< length 1.0e-12)
      v-zero
      (v-scale a (/ 1.0 length)))))
(defn v-approx= [a b epsilon]
  (<= (v-distance a b) epsilon))

(def quat-identity [0.0 0.0 0.0 1.0])

(defn quat [x y z w] [(double x) (double y) (double z) (double w)])
(defn q+ [a b] [(+ (nth a 0) (nth b 0)) (+ (nth a 1) (nth b 1)) (+ (nth a 2) (nth b 2)) (+ (nth a 3) (nth b 3))])
(defn q-scale [a s]
  [(* (nth a 0) s) (* (nth a 1) s) (* (nth a 2) s) (* (nth a 3) s)])
(defn q-conj [a] [(- (nth a 0)) (- (nth a 1)) (- (nth a 2)) (nth a 3)])
(defn q-dot [a b]
  (+ (* (nth a 0) (nth b 0)) (* (nth a 1) (nth b 1))
     (* (nth a 2) (nth b 2)) (* (nth a 3) (nth b 3))))
(defn q-norm [a] (sqrt (q-dot a a)))

(defn q-mul
  "Hamilton product `a * b`: the rotation `b` composed after `a`."
  [a b]
  (let [[ax ay az aw] a
        [bx by bz bw] b]
    [(- (+ (* aw bx) (* ax bw) (* ay bz)) (* az by))
     (- (+ (* aw by) (* ay bw) (* az bx)) (* ax bz))
     (- (+ (* aw bz) (* az bw) (* ax by)) (* ay bx))
     (- (* aw bw) (* ax bx) (* ay by) (* az bz))]))

(defn q-unit
  "Normalize `q`, falling back to the identity for a degenerate quaternion."
  [q]
  (let [length (q-norm q)]
    (if (< length 1.0e-12)
      quat-identity
      (q-scale q (/ 1.0 length)))))

(defn q-rotate
  "Rotate vector `v` by quaternion `q`."
  [q v]
  (let [q (q-unit q)
        p (q-mul (q-mul q [(nth v 0) (nth v 1) (nth v 2) 0.0]) (q-conj q))]
    [(nth p 0) (nth p 1) (nth p 2)]))

(defn q-from-axis-angle
  "Quaternion rotating by `angle` radians around `axis`."
  [axis angle]
  (let [axis (v-unit axis)]
    (if (= v-zero axis)
      quat-identity
      (let [half (/ angle 2.0)
            s (sin half)]
        [(* (nth axis 0) s) (* (nth axis 1) s) (* (nth axis 2) s) (cos half)]))))

(defn q-axis-angle
  "Return `[axis angle]` of a quaternion, angle in radians in `[0, 2π]`."
  [q]
  (let [q (if (< (nth q 3) 0.0) (q-scale q -1.0) q)
        [x y z w] (q-unit q)
        length (sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< length 1.0e-12)
      [v-up 0.0]
      [[(/ x length) (/ y length) (/ z length)] (* 2.0 (acos (clamp w -1.0 1.0)))])))

(defn q-angle
  "Magnitude of the relative rotation between `a` and `b`, in radians."
  [a b]
  (let [dot (abs (q-dot (q-unit a) (q-unit b)))]
    (* 2.0 (acos (clamp dot -1.0 1.0)))))

(defn q-inverse [q] (q-conj (q-unit q)))

(defn q-slerp
  "Spherical interpolation from `a` to `b`; falls back to linear blending for
  nearly parallel quaternions."
  [a b t]
  (let [a (q-unit a)
        b (q-unit b)
        dot (q-dot a b)
        b (if (< dot 0.0) (q-scale b -1.0) b)
        dot (abs dot)]
    (if (> dot 0.9995)
      (q-unit (mapv (fn [index]
                      (+ (* (nth a index) (- 1.0 t)) (* (nth b index) t)))
                    (range 4)))
      (let [theta (* (acos (clamp dot -1.0 1.0)))
            sin-theta (sin theta)
            wa (/ (sin (* (- 1.0 t) theta)) sin-theta)
            wb (/ (sin (* t theta)) sin-theta)]
        (q-unit (q+ (q-scale a wa) (q-scale b wb)))))))

(defn q-from-to
  "Shortest-arc quaternion rotating unit vector `from` onto unit vector `to`."
  [from to]
  (let [a (v-unit from)
        b (v-unit to)
        dot (clamp (v-dot a b) -1.0 1.0)]
    (cond
      (> dot 0.999999) quat-identity
      (< dot -0.999999)
      (let [axis (v-cross a (if (< (abs (nth a 0)) 0.9) [1.0 0.0 0.0] [0.0 1.0 0.0]))]
        (q-from-axis-angle axis pi))
      :else
      (let [axis (v-cross a b)]
        (q-unit [(nth axis 0) (nth axis 1) (nth axis 2) (+ 1.0 dot)])))))

(defn euler->q
  "Quaternion of Euler angles `[rx ry rz]` in the `:xyz` order `Rx * Ry * Rz`."
  [[rx ry rz]]
  (q-mul (q-mul (q-from-axis-angle [1.0 0.0 0.0] rx)
                (q-from-axis-angle [0.0 1.0 0.0] ry))
         (q-from-axis-angle [0.0 0.0 1.0] rz)))

(defn q->m3
  "Rotation matrix as a vector of three rows."
  [q]
  (let [[x y z w] (q-unit q)
        xx (* x x) yy (* y y) zz (* z z)
        xy (* x y) xz (* x z) yz (* y z)
        wx (* w x) wy (* w y) wz (* w z)]
    [[(- 1.0 (* 2.0 (+ yy zz))) (* 2.0 (- xy wz)) (* 2.0 (+ xz wy))]
     [(* 2.0 (+ xy wz)) (- 1.0 (* 2.0 (+ xx zz))) (* 2.0 (- yz wx))]
     [(* 2.0 (- xz wy)) (* 2.0 (+ yz wx)) (- 1.0 (* 2.0 (+ xx yy)))]]))
(defn q->euler
  "Euler angles `[rx ry rz]` in the `:xyz` order `Rx * Ry * Rz`.

  This is the inverse of `euler->q`; at gimbal lock the Z angle is reported as
  zero and the X angle absorbs the rotation."
  [q]
  (let [[[r00 r01 r02] [_r10 r11 r12] [_r20 r21 r22]] (q->m3 q)
        sy (clamp r02 -1.0 1.0)
        ry (asin sy)
        c2 (cos ry)]
    (if (< (abs c2) 1.0e-6)
      [(atan2 r21 r11) ry 0.0]
      [(atan2 (- r12) r22) ry (atan2 (- r01) r00)])))

(defn round
  "Round to `digits` decimal places; used when printing or serializing."
  [x digits]
  (let [factor (#?(:clj Math/pow :cljs js/Math.pow) 10.0 digits)]
    (/ (#?(:clj Math/round :cljs js/Math.round) (* (double x) factor)) factor)))

(defn q-approx= [a b epsilon]
  (or (<= (q-angle a b) epsilon)
      (<= (q-angle a (q-scale b -1.0)) epsilon)))