(ns omkamra.body.ik
  "Deterministic inverse kinematics for the major limb chains.

  The solver is cyclic-coordinate descent (CCD) over the two joints of a limb
  (shoulder and elbow, hip and knee), followed by a pole constraint that fixes
  the bend plane. Every joint rotation the solver writes is clamped into the
  joint's own limits, so a solved pose is anatomically limited by construction.

  Determinism is part of the contract (D6): the same skeleton, pose, targets
  and options always produce the same result. Limb order is fixed, iteration
  counts are fixed, and no randomness is used.

  Limits of v1: the solver adjusts joint rotations and, when allowed, the root
  translation. Torso lean and shoulder-girdle translation are not solved; the
  caller controls them through poses and controls."
  (:require [omkamra.body.math :as m]
            [omkamra.body.skeleton :as sk]))

(def limb-chains
  "The configurable chains the solver understands.

  `:joints` are the joints CCD rotates; `:end` is the joint the effector point
  belongs to, `:tip` the effector in that joint's local frame; `:pole-key` names
  the world direction control for the middle joint; `:default-pole` is used
  when the caller supplies none."
  {:left-arm {:limb :left-arm
              :joints [:left-upper-arm :left-forearm]
              :end :left-hand
              :tip [0.0 -0.085 0.0]
              :pole-key :left-elbow
              :default-pole [0.0 0.0 -1.0]
              :label "Left hand"}
   :right-arm {:limb :right-arm
               :joints [:right-upper-arm :right-forearm]
               :end :right-hand
               :tip [0.0 -0.085 0.0]
               :pole-key :right-elbow
               :default-pole [0.0 0.0 -1.0]
               :label "Right hand"}
   :left-leg {:limb :left-leg
              :joints [:left-thigh :left-shin]
              :end :left-foot
              :tip [0.0 -0.060 0.0]
              :pole-key :left-knee
              :default-pole [0.0 0.0 1.0]
              :label "Left foot"}
   :right-leg {:limb :right-leg
               :joints [:right-thigh :right-shin]
               :end :right-foot
               :tip [0.0 -0.060 0.0]
               :pole-key :right-knee
               :default-pole [0.0 0.0 1.0]
               :label "Right foot"}})

(def ^:private limb-order [:left-arm :right-arm :left-leg :right-leg])

(def ^:private limb-rank (zipmap limb-order (range)))

(defn limb-chain [limb]
  (get limb-chains limb))

(def ^:private defaults
  {:mode :best-effort
   :iterations 12
   :passes 2
   :tolerance 0.005
   :adjust-root? true
   :root-adjust-limit 0.25
   :locks #{}})

(defn- parent-world-rotation
  "World rotation of the frame a joint's local rotation is expressed in."
  [skeleton transforms joint-id]
  (if-let [parent (:parent (sk/joint skeleton joint-id))]
    (:rotation (get transforms parent))
    m/quat-identity))

(defn- rotate-joint-world
  "Compose a world-space rotation `delta` onto a joint and clamp it into its
  limits."
  [skeleton pose joint-id delta]
  (let [transforms (sk/evaluate skeleton pose)
        parent-rotation (parent-world-rotation skeleton transforms joint-id)
        local-delta (m/q-mul (m/q-conj parent-rotation)
                             (m/q-mul delta parent-rotation))
        current (m/q-unit (get-in pose [:bones joint-id] m/quat-identity))
        composed (m/q-mul local-delta current)
        [clamped _] (sk/rotation->euler-limits skeleton joint-id composed)]
    (assoc-in pose [:bones joint-id] clamped)))

(defn- effector-position [transforms {:keys [end tip]}]
  (sk/tip-position transforms end tip))

(defn- hinge-axis
  "The single free local axis of a hinge joint, or nil for a ball joint.

  A joint whose limits leave one axis with more than half a radian of travel
  and no other axis with more than 0.5 radian is a hinge; rotating it about
  anything else would be clamped away and make CCD oscillate."
  [skeleton joint-id]
  (let [limits (:limits (sk/joint skeleton joint-id))
        [wide narrow] (split-with (fn [[_axis [lower upper]]]
                                    (> (- upper lower) 0.5))
                                  limits)]
    (when (and (= 1 (count wide)) (every? (fn [[_axis [lower upper]]]
                                            (<= (- upper lower) 0.5))
                                          narrow))
      (ffirst wide))))

(defn- ccd-rotation
  "World rotation that turns `to-effector` towards `to-target` at a joint."
  [skeleton transforms joint-id to-effector to-target]
  (let [effector-direction (m/v-unit to-effector)
        target-direction (m/v-unit to-target)]
    (if-let [axis (hinge-axis skeleton joint-id)]
      (let [joint-rotation (:rotation (get transforms joint-id))
            world-axis (m/q-rotate joint-rotation
                                   (case axis
                                     :x [1.0 0.0 0.0]
                                     :y [0.0 1.0 0.0]
                                     :z [0.0 0.0 1.0]))
            angle (m/atan2 (m/v-dot (m/v-cross effector-direction target-direction)
                                    world-axis)
                           (m/v-dot effector-direction target-direction))]
        (m/q-from-axis-angle world-axis angle))
      (m/q-from-to effector-direction target-direction))))

(defn- ccd-pass
  "One CCD sweep over the joints of a chain, innermost joint first."
  [skeleton pose chain target]
  (reduce
   (fn [pose joint-id]
     (let [transforms (sk/evaluate skeleton pose)
           base (:position (get transforms joint-id))
           effector (effector-position transforms chain)
           to-effector (m/v- effector base)
           to-target (m/v- target base)]
       (if (or (< (m/v-len to-effector) 1.0e-6)
               (< (m/v-len to-target) 1.0e-6))
         pose
         (rotate-joint-world skeleton pose joint-id
                             (ccd-rotation skeleton transforms joint-id
                                           to-effector to-target)))))
   pose
   (reverse (:joints chain))))

(defn- any-perpendicular
  "Some unit vector perpendicular to unit vector `d`."
  [d]
  (let [candidate (if (< (m/abs (nth d 0)) 0.9)
                    [1.0 0.0 0.0]
                    [0.0 1.0 0.0])]
    (m/v-unit (m/v-cross d candidate))))

(defn- hinge-local-axis [axis]
  (case axis
    :x [1.0 0.0 0.0]
    :y [0.0 1.0 0.0]
    :z [0.0 0.0 1.0]))

(defn- analytic-solve
  "Place a two-joint limb so its effector reaches `target`.

  The elbow (or knee) is a hinge and the shoulder (or hip) a ball joint. The
  chain is placed analytically: the law of cosines gives the upper-arm
  direction towards the pole side, a twist about the upper arm turns the
  hinge axis into the plane of the target, and the hinge is bent to point the
  effector at the target. Unlike CCD this cannot get stuck on the wrong side
  of a one-sided hinge limit."
  [skeleton pose chain target pole]
  (let [{:keys [joints]} chain
        base-joint (first joints)
        mid-joint (second joints)
        transforms (sk/evaluate skeleton pose)
        base (:position (get transforms base-joint))
        mid (:position (get transforms mid-joint))
        effector (effector-position transforms chain)
        l1 (m/v-distance base mid)
        l2 (m/v-distance mid effector)
        to-target (m/v- target base)
        distance (m/v-len to-target)
        direction (m/v-unit to-target)]
    (if (or (< distance 1.0e-6) (< l1 1.0e-6) (< l2 1.0e-6))
      pose
      (let [reach (m/clamp distance
                           (+ (m/abs (- l1 l2)) 1.0e-4)
                           (- (+ l1 l2) 1.0e-4))
            cosine (m/clamp (/ (+ (* l1 l1) (* reach reach) (* l2 l2 -1.0))
                               (* 2.0 l1 reach))
                            -1.0 1.0)
            alpha (m/acos cosine)
            pole-perpendicular (m/v-unit
                                (m/v- pole
                                      (m/v-scale direction (m/v-dot pole direction))))
            pole-perpendicular (if (= m/v-zero pole-perpendicular)
                                 (any-perpendicular direction)
                                 pole-perpendicular)
            upper-direction (m/v+ (m/v-scale direction (m/cos alpha))
                                  (m/v-scale pole-perpendicular (m/sin alpha)))
            current-upper (m/v-unit (m/v- mid base))
            pose (rotate-joint-world skeleton pose base-joint
                                     (m/q-from-to current-upper upper-direction))
            transforms (sk/evaluate skeleton pose)
            mid (:position (get transforms mid-joint))
            upper (m/v-unit (m/v- mid base))
            to-target (m/v- target mid)
            target-direction (m/v-unit to-target)
            axis (or (hinge-axis skeleton mid-joint) :x)
            limits (get (:limits (sk/joint skeleton mid-joint)) axis)
            [lower upper-limit] limits
            plane-normal (m/v-unit (m/v-cross upper target-direction))
            bend (m/acos (m/clamp (m/v-dot upper target-direction) -1.0 1.0))
            ;; Positive travel around `plane-normal` extends the hinge, its
            ;; negative the other way; prefer the direction the joint allows,
            ;; flexion first.
            sign (if (and (< (m/abs bend) 1.0e-6)
                          (<= lower 0.0 upper-limit))
                   1.0
                   (if (and (<= lower (- bend) upper-limit) (< lower 0.0))
                     -1.0
                     (if (<= lower bend upper-limit) 1.0 -1.0)))
            desired-axis (m/v-scale plane-normal sign)]
        (if (< (m/v-len plane-normal) 1.0e-9)
          pose
          (let [joint-rotation (:rotation (get transforms mid-joint))
                current-axis (m/q-rotate joint-rotation (hinge-local-axis axis))
                perpendicular (fn [v]
                                (m/v-unit (m/v- v (m/v-scale upper (m/v-dot v upper)))))
                current-perpendicular (perpendicular current-axis)
                desired-perpendicular (perpendicular desired-axis)
                twist (if (or (= m/v-zero current-perpendicular)
                              (= m/v-zero desired-perpendicular))
                        0.0
                        (m/atan2 (m/v-dot (m/v-cross current-perpendicular
                                                     desired-perpendicular)
                                          upper)
                                 (m/v-dot current-perpendicular desired-perpendicular)))
                pose (if (< (m/abs twist) 1.0e-9)
                       pose
                       (rotate-joint-world skeleton pose base-joint
                                           (m/q-from-axis-angle upper twist)))
                transforms (sk/evaluate skeleton pose)
                mid (:position (get transforms mid-joint))
                effector (effector-position transforms chain)
                from (m/v-unit (m/v- effector mid))
                to (m/v-unit (m/v- target mid))
                angle (m/atan2 (m/v-dot (m/v-cross from to) desired-axis)
                               (m/v-dot from to))]
            (rotate-joint-world skeleton pose mid-joint
                                (m/q-from-axis-angle desired-axis angle))))))))

(defn- solve-limb
  [{:keys [iterations] :as options} skeleton pose {:keys [limb position pole]}]
  (if-let [chain (limb-chain limb)]
    (let [pole (m/v-unit (or pole (:default-pole chain)))
          iterations (or iterations (:iterations options))
          place (fn [pose]
                  (let [placed (analytic-solve skeleton pose chain position pole)]
                    (loop [pose placed, iteration 0]
                      (if (>= iteration iterations)
                        pose
                        (let [next-pose (ccd-pass skeleton pose chain position)
                              error (m/v-distance
                                     (effector-position (sk/evaluate skeleton next-pose) chain)
                                     position)]
                          (if (< error 1.0e-4)
                            next-pose
                            (recur next-pose (inc iteration))))))))]
      (place pose))
    pose))

(defn- apply-orientation
  "Rotate the terminal joint so its world rotation matches `orientation`.

  Applied after position solving; it can trade positional accuracy for
  orientation and is therefore only used when the caller asks for it."
  [skeleton pose {:keys [limb orientation]}]
  (if-let [chain (limb-chain limb)]
    (let [transforms (sk/evaluate skeleton pose)
          end (:end chain)
          current (:rotation (get transforms end))
          desired (m/q-unit orientation)
          delta (m/q-mul desired (m/q-conj current))]
      (rotate-joint-world skeleton pose end delta))
    pose))

(defn- target-entry
  [skeleton pose {:keys [limb position]} tolerance]
  (let [chain (limb-chain limb)
        achieved (effector-position (sk/evaluate skeleton pose) chain)
        error (m/v-distance achieved position)]
    {:limb limb
     :label (:label chain)
     :target (mapv double position)
     :achieved (mapv double achieved)
     :error error
     :ok? (<= error tolerance)}))

(defn- normalise-targets [targets]
  (into []
        (keep (fn [[limb {:keys [position] :as target}]]
                (when (and (limb-chain limb)
                           (vector? position)
                           (= 3 (count position)))
                  (assoc target :limb limb))))
        targets))

(defn- sort-targets [targets]
  ;; Descending priority, then the fixed limb order, so equal priorities are
  ;; deterministic.
  (sort-by (fn [{:keys [limb] :as target}]
             [(- (or (:priority target) 1)) (get limb-rank limb 99)])
           targets))

(defn- root-adjusted-pose
  "Translate the root by a fraction of the mean unmet-target error."
  [skeleton pose targets options]
  (let [locks (set (:locks options))
        limit (:root-adjust-limit options)]
    (if (or (contains? locks :root) (contains? locks :pelvis) (empty? targets))
      pose
      (let [transforms (sk/evaluate skeleton pose)
            errors (for [{:keys [limb position]} targets
                         :let [chain (limb-chain limb)
                               achieved (effector-position transforms chain)]]
                     (m/v- position achieved))
            mean (m/v-scale (reduce m/v+ m/v-zero errors) (/ 1.0 (count errors)))
            mean (if (> (m/v-len mean) limit)
                   (m/v-scale (m/v-unit mean) limit)
                   mean)]
        (if (< (m/v-len mean) 1.0e-6)
          pose
          (assoc-in pose [:root :translation]
                    (m/v+ (get-in pose [:root :translation] m/v-zero) mean)))))))

(defn solve
  "Solve inverse kinematics for `pose`.

  Options:

  - `:targets` — `{limb {:position [x y z] :orientation [x y z w]
    :priority n}}` with limbs `:left-arm`, `:right-arm`, `:left-leg`,
    `:right-leg`;
  - `:directions` — `{pole-key [x y z]}` such as `:left-elbow` or
    `:left-knee`, in world space;
  - `:mode` — `:best-effort` (default) or `:strict`;
  - `:iterations` — CCD sweeps per pass (default 12);
  - `:passes` — outer passes over all targets (default 2);
  - `:tolerance` — accepted position error in metres (default 0.005);
  - `:adjust-root?` — allow root translation when targets are out of reach
    (default true);
  - `:root-adjust-limit` — maximum root translation in metres (default 0.25);
  - `:locks` — `#{:root}` or `#{:pelvis}` to forbid root adjustment.

  Returns `{:status :ok|:best-effort|:unreachable, :pose pose,
  :diagnostics {...}}`. `:strict` mode returns the input pose unchanged and
  status `:unreachable` when any target misses its tolerance."
  [skeleton pose options]
  (let [options (merge defaults options)
        targets (normalise-targets (:targets options))
        directions (:directions options)
        ordered (sort-targets targets)
        with-directions (map (fn [target]
                               (assoc target :pole (get directions
                                                        (:pole-key (limb-chain (:limb target))))))
                             ordered)
        iterations (max 1 (:iterations options))
        solve-pass (fn [pose targets]
                     (reduce (fn [pose target]
                               (solve-limb options skeleton pose target))
                             pose
                             targets))
        posed (loop [pose pose, pass 0]
                (if (>= pass (:passes options))
                  pose
                  (recur (solve-pass pose with-directions) (inc pass))))
        posed (if (:adjust-root? options)
                (let [posed (root-adjusted-pose skeleton posed targets options)]
                  (solve-pass posed with-directions))
                posed)
        posed (reduce (fn [pose target]
                        (if (:orientation target)
                          (apply-orientation skeleton pose target)
                          pose))
                      posed
                      with-directions)
        entries (mapv (fn [{:keys [limb position]}]
                        (let [target (first (filter #(= limb (:limb %)) with-directions))
                              entry (target-entry skeleton posed target (:tolerance options))]
                          (assoc entry
                                 :target (mapv double position)
                                 :priority (or (:priority target) 1))))
                      with-directions)
        all-ok? (every? :ok? entries)
        fitted (not all-ok?)]
    (if (and (= :strict (:mode options)) fitted)
      {:status :unreachable
       :pose pose
       :diagnostics {:mode :strict
                     :tolerance (:tolerance options)
                     :targets entries
                     :iterations iterations
                     :passes (:passes options)
                     :message "at least one target is out of reach"
                     :violations (sk/limit-violations skeleton posed)}}
      {:status (if all-ok? :ok :best-effort)
       :pose posed
       :diagnostics {:mode (:mode options)
                     :tolerance (:tolerance options)
                     :targets entries
                     :iterations iterations
                     :passes (:passes options)
                     :root-adjustment (m/v- (get-in posed [:root :translation] m/v-zero)
                                            (get-in pose [:root :translation] m/v-zero))
                     :violations (sk/limit-violations skeleton posed)}})))