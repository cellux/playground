(ns omkamra.body.skeleton
  "The canonical, render-independent human body skeleton and forward kinematics.

  A skeleton is plain data: a parent-ordered sequence of joint descriptions plus
  the reference dimensions of the canonical body. A joint describes where a bone
  starts (`:offset` from the parent joint origin, in the parent's rest frame),
  how long it is (`:length` along the local +Y axis) and how far it may rotate
  (`:limits`). Rest rotations are the identity, so at rest every local axis is
  parallel to the world axes and every joint sits at its anatomical position.

  Conventions (see `omkamra.body.math` and D2 in DECISIONS.md):

  - Y is up, Z is forward (the body faces the viewer at +Z), X is the body's
    left;
  - a rotation about +X moves a child that lies below its parent (offset along
    -Y) towards -Z;
  - limits are per-axis Euler intervals in the joint's local frame, using the
    `:xyz` order of `omkamra.body.math`.

  The inventory contains every pose-relevant structure: the lumbar, thoracic
  and cervical spine, ribs and sternum, scapulae and clavicles, the arms, hands
  with all finger segments, the legs, feet and toes. Individual ribs are rigid
  children of their thoracic vertebra; they are modelled so a skin can bind
  them, not so they can be posed independently."
  (:refer-clojure :exclude [descendants])
  (:require [clojure.string :as str]
            [omkamra.body.math :as m]))

(def skeleton-id :omkamra.body/canonical)
(def schema-version 1)
(def reference-height 1.75)

(defn- joint-spec
  [id parent offset length limits label region]
  {:id id
   :parent parent
   :offset (mapv double offset)
   :rest m/quat-identity
   :length (double length)
   :limits limits
   :dofs (vec (sort-by {:x 0 :y 1 :z 2} (keys limits)))
   :label label
   :region region})

(defn- axis-limits
  ([x] (axis-limits x nil nil))
  ([x y] (axis-limits x y nil))
  ([x y z]
   (cond-> {:x x}
     y (assoc :y y)
     z (assoc :z z))))

;; ---------------------------------------------------------------------------
;; Canonical inventory

(def ^:private lumbar-levels 5)
(def ^:private thoracic-levels 12)
(def ^:private cervical-levels 7)

(def ^:private hand-length 0.085)

(def ^:private finger-specs
  "Fingers in thumb-to-little order: `[finger lateral-offset segment-lengths]`,
  where the lateral offset is measured from the hand's midline towards the
  body's left."
  [[:thumb 0.030 [0.036 0.030 0.024]]
   [:index 0.022 [0.040 0.026 0.020]]
   [:middle 0.006 [0.045 0.029 0.021]]
   [:ring -0.010 [0.041 0.027 0.020]]
   [:little -0.024 [0.034 0.021 0.018]]])

(def ^:private finger-segments
  {:thumb [:metacarpal :proximal :distal]
   :index [:proximal :middle :distal]
   :middle [:proximal :middle :distal]
   :ring [:proximal :middle :distal]
   :little [:proximal :middle :distal]})

(def ^:private toe-specs
  "Toes in big-to-little order: `[number medial-offset segment-lengths]`, with
  the offset measured from the foot's midline towards the big toe."
  [[1 0.028 [0.038 0.024]]
   [2 0.012 [0.034 0.020]]
   [3 0.000 [0.030 0.018]]
   [4 -0.014 [0.026 0.016]]
   [5 -0.026 [0.022 0.014]]])

(defn- spine-joints []
  (concat
   (mapcat
    (fn [level]
      (let [id (keyword (str "spine-lumbar-" level))]
        [(joint-spec id
                     (if (= 1 level) :pelvis (keyword (str "spine-lumbar-" (dec level))))
                     [0.0 (if (= 1 level) 0.030 0.035) 0.0]
                     0.035
                     (axis-limits [(- 0.18) 0.18] [(- 0.10) 0.10] [(- 0.08) 0.08])
                     (str "Lumbar vertebra " level)
                     :spine)]))
    (range 1 (inc lumbar-levels)))
   (mapcat
    (fn [level]
      (let [id (keyword (str "spine-thoracic-" level))
            parent (if (= 1 level)
                     (keyword (str "spine-lumbar-" lumbar-levels))
                     (keyword (str "spine-thoracic-" (dec level))))]
        [(joint-spec id parent [0.0 0.025 0.0] 0.025
                     (axis-limits [(- 0.10) 0.10] [(- 0.10) 0.10] [(- 0.08) 0.08])
                     (str "Thoracic vertebra " level)
                     :spine)]))
    (range 1 (inc thoracic-levels)))
   (mapcat
    (fn [level]
      (let [id (keyword (str "spine-cervical-" level))
            parent (if (= 1 level)
                     (keyword (str "spine-thoracic-" thoracic-levels))
                     (keyword (str "spine-cervical-" (dec level))))]
        [(joint-spec id parent [0.0 0.020 0.0] 0.020
                     (axis-limits [(- 0.25) 0.30] [(- 0.30) 0.30] [(- 0.20) 0.20])
                     (str "Cervical vertebra " level)
                     :spine)]))
    (range 1 (inc cervical-levels)))))

(defn- rib-joints []
  (for [level (range 1 (inc thoracic-levels))
        side [:left :right]
        :let [sign (if (= side :left) 1.0 -1.0)]]
    (joint-spec (keyword (str (name side) "-rib-" level))
                (keyword (str "spine-thoracic-" level))
                [(* sign 0.020) 0.0 0.010]
                (* 0.11 (+ 0.55 (/ level 16.0)))
                (axis-limits [0.0 0.0])
                (str "Rib " level " (" (name side) ")")
                :ribcage)))

(defn- head-joints []
  [(joint-spec :sternum :spine-thoracic-2 [0.0 0.060 0.085] 0.14
               (axis-limits [0.0 0.0]) "Sternum" :ribcage)
   (joint-spec :head (keyword (str "spine-cervical-" cervical-levels)) [0.0 0.020 0.0] 0.15
               (axis-limits [(- 0.35) 0.35] [(- 0.60) 0.60] [(- 0.35) 0.35])
               "Head" :head)])

(defn- finger-joints [side hand]
  (let [side-name (name side)
        sign (if (= side :left) 1.0 -1.0)]
    (for [[finger offset lengths] finger-specs
          :let [segments (finger-segments finger)]
          index (range (count segments))
          :let [segment (nth segments index)
                id (keyword (str side-name "-" (name finger) "-" (name segment)))
                parent (if (zero? index)
                         hand
                         (keyword (str side-name "-" (name finger)
                                       "-" (name (nth segments (dec index))))))
                length (nth lengths index)
                y-offset (if (zero? index)
                           (- (+ hand-length length))
                           (- (nth lengths (dec index))))]]
      (joint-spec id parent
                  (if (zero? index)
                    [(* sign offset) y-offset 0.0]
                    [0.0 y-offset 0.0])
                  length
                  (axis-limits [(- 1.65) 0.10] [(- 0.10) 0.10] [(- 0.35) 0.35])
                  (str (str/capitalize (name finger)) " " (name segment)
                       " (" side-name ")")
                  :finger))))

(defn- arm-joints [side]
  (let [sign (if (= side :left) 1.0 -1.0)
        side-name (name side)
        scapula (keyword (str side-name "-scapula"))
        clavicle (keyword (str side-name "-clavicle"))
        upper-arm (keyword (str side-name "-upper-arm"))
        forearm (keyword (str side-name "-forearm"))
        hand (keyword (str side-name "-hand"))]
    (concat
     [(joint-spec scapula :spine-thoracic-1
                  [(* sign 0.055) 0.0 (- 0.030)] 0.09
                  (axis-limits [(- 0.20) 0.20] [(- 0.20) 0.20] [(- 0.25) 0.25])
                  (str "Scapula (" side-name ")") :shoulder)
      (joint-spec clavicle scapula
                  [(* sign 0.055) 0.035 0.020] 0.13
                  (axis-limits [(- 0.20) 0.20] [(- 0.20) 0.20] [(- 0.15) 0.15])
                  (str "Clavicle (" side-name ")") :shoulder)
      (joint-spec upper-arm clavicle
                  [(* sign 0.135) (- 0.020) 0.0] 0.28
                  (axis-limits [(- 2.90) 2.90] [(- 1.55) 1.55] [(- 0.30) 2.90])
                  (str "Upper arm (" side-name ")") :arm)
      (joint-spec forearm upper-arm [0.0 -0.280 0.0] 0.25
                  (axis-limits [(- 2.60) 0.05] [(- 0.15) 0.15] [(- 0.15) 0.15])
                  (str "Forearm (" side-name ")") :arm)
      (joint-spec hand forearm [0.0 -0.250 0.0] hand-length
                  (axis-limits [(- 1.20) 1.20] [(- 0.15) 0.15] [(- 0.55) 0.55])
                  (str "Hand (" side-name ")") :hand)]
     (finger-joints side hand))))

(defn- toe-joints [side foot]
  (let [side-name (name side)
        sign (if (= side :left) 1.0 -1.0)]
    (for [[number medial-offset lengths] toe-specs
          index (range (count lengths))
          :let [prefix (if (= number 1)
                         (keyword (str side-name "-toe1"))
                         (keyword (str side-name "-toe" number)))
                id (keyword (str (name prefix) "-"
                                 (name (if (zero? index) :proximal :distal))))
                parent (if (zero? index)
                         foot
                         (keyword (str (name prefix) "-proximal")))
                length (nth lengths index)]]
      (joint-spec id parent
                  (if (zero? index)
                    [(* sign (- medial-offset)) -0.045 0.075]
                    [0.0 0.0 (- (nth lengths 0))])
                  length
                  (if (= number 1)
                    (axis-limits [(- 0.10) 1.10] [(- 0.15) 0.15] [(- 0.10) 0.10])
                    (axis-limits [(- 0.20) 0.90] [(- 0.15) 0.15] [(- 0.10) 0.10]))
                  (str (if (= number 1) "Big toe" (str "Toe " number))
                       " " (name (if (zero? index) :proximal :distal))
                       " (" side-name ")")
                  :toe))))

(defn- leg-joints [side]
  (let [sign (if (= side :left) 1.0 -1.0)
        side-name (name side)
        thigh (keyword (str side-name "-thigh"))
        shin (keyword (str side-name "-shin"))
        foot (keyword (str side-name "-foot"))]
    (concat
     [(joint-spec thigh :pelvis
                  [(* sign 0.090) (- 0.060) 0.0] 0.42
                  (axis-limits [(- 1.90) 0.70] [(- 0.70) 0.70] [(- 0.50) 0.90])
                  (str "Thigh (" side-name ")") :leg)
      (joint-spec shin thigh [0.0 -0.420 0.0] 0.42
                  (axis-limits [0.0 2.60] [(- 0.10) 0.10] [(- 0.10) 0.10])
                  (str "Shin (" side-name ")") :leg)
      (joint-spec foot shin [0.0 -0.420 0.0] 0.10
                  (axis-limits [(- 0.45) 0.95] [(- 0.15) 0.15] [(- 0.10) 0.10])
                  (str "Foot (" side-name ")") :foot)]
     (toe-joints side foot))))

(defn build-canonical-skeleton
  "Build the canonical skeleton as plain data."
  []
  {:id skeleton-id
   :schema-version schema-version
   :units :meters
   :height reference-height
   :up [0.0 1.0 0.0]
   :forward [0.0 0.0 1.0]
   :left [1.0 0.0 0.0]
   :root :pelvis
   :bones (vec (concat
                [(joint-spec :pelvis nil [0.0 0.95 0.0] 0.16
                             (axis-limits [(- 0.20) 0.20] [(- 0.40) 0.40] [(- 0.20) 0.20])
                             "Pelvis" :pelvis)]
                (spine-joints)
                (rib-joints)
                (head-joints)
                (arm-joints :left)
                (arm-joints :right)
                (leg-joints :left)
                (leg-joints :right)))
   :regions [:pelvis :spine :ribcage :head :shoulder :arm :hand :finger
             :leg :foot :toe]})

(def canonical-skeleton
  "The canonical body skeleton used when a caller does not supply one."
  (build-canonical-skeleton))

;; ---------------------------------------------------------------------------
;; Skeleton access

(defn joint
  "Return the joint description with `joint-id`, or nil."
  [skeleton joint-id]
  (some #(when (= joint-id (:id %)) %) (:bones skeleton)))

(defn bones [skeleton] (:bones skeleton))

(defn joint-ids [skeleton] (mapv :id (:bones skeleton)))

(defn region-bones [skeleton region]
  (filterv #(= region (:region %)) (:bones skeleton)))

(defn bone->children [skeleton]
  (reduce (fn [index id]
            (if-let [parent (:parent (joint skeleton id))]
              (update index parent (fnil conj []) id)
              index))
          {}
          (joint-ids skeleton)))

(defn children
  "Direct child joint ids of `joint-id`."
  [skeleton joint-id]
  (get (bone->children skeleton) joint-id []))

(defn chain
  "Joint ids from the root to `joint-id`, inclusive."
  [skeleton joint-id]
  (loop [id joint-id, path ()]
    (if id
      (recur (:parent (joint skeleton id)) (cons id path))
      (vec path))))

(defn root-id [skeleton] (:root skeleton))

(defn height [skeleton] (:height skeleton))

(defn valid-bone? [skeleton joint-id]
  (boolean (joint skeleton joint-id)))

(defn descendants
  "All joint ids below `joint-id`, parents first."
  [skeleton joint-id]
  (loop [remaining (vec (children skeleton joint-id))
         result []]
    (if-let [id (peek remaining)]
      (recur (into (pop remaining) (children skeleton id))
             (conj result id))
      result)))

;; ---------------------------------------------------------------------------
;; Forward kinematics

(defn evaluate
  "Evaluate a posed skeleton into world transforms.

  Returns `{joint-id {:position [x y z] :rotation [x y z w]}}` where
  `:rotation` maps local joint axes into world axes and `:position` is the
  joint origin. The root transform comes from `(get-in pose [:root ...])`; all
  other transforms follow the parent chain."
  [skeleton pose]
  (let [root-translation (get-in pose [:root :translation] m/v-zero)
        root-rotation (m/q-unit (get-in pose [:root :rotation] m/quat-identity))
        rotations (:bones pose)]
    (reduce
     (fn [acc {:keys [id parent offset rest]}]
       (let [local (m/q-unit (get rotations id m/quat-identity))
             parent-transform (when parent (get acc parent))
             parent-position (if parent-transform (:position parent-transform) m/v-zero)
             parent-rotation (if parent-transform
                               (:rotation parent-transform)
                               root-rotation)
             position (if parent
                        (m/v+ parent-position (m/q-rotate parent-rotation offset))
                        (m/v+ offset root-translation))
             rotation (m/q-unit
                       (m/q-mul parent-rotation
                                (m/q-mul (or rest m/quat-identity) local)))]
         (assoc acc id {:position position :rotation rotation})))
     {}
     (:bones skeleton))))

(defn tip-position
  "World position of a point given in a joint's local frame."
  [transforms joint-id local-point]
  (let [{:keys [position rotation]} (get transforms joint-id)]
    (m/v+ position (m/q-rotate rotation local-point))))

(defn world-delta->local
  "Convert a world-space rotation delta into the local rotation delta of a
  joint: a new world rotation `delta * old` corresponds to the local rotation
  `dt * old-local`, with `dt` this function's result."
  [transforms joint-id delta]
  (let [parent-rotation (:rotation (get transforms joint-id))]
    (m/q-mul (m/q-conj parent-rotation) (m/q-mul delta parent-rotation))))

;; ---------------------------------------------------------------------------
;; Limits

(defn rotation->euler-limits
  "Clamp a joint's local rotation into its per-axis limits.

  Returns `[clamped-rotation violations]`, where each violation is
  `{:bone id :axis axis :value angle :limit [lower upper]}`."
  [skeleton joint-id rotation]
  (if-let [{:keys [limits]} (joint skeleton joint-id)]
    (let [euler (m/q->euler rotation)
          [clamped violations]
          (reduce-kv
           (fn [[values violations] axis index]
             (let [[lower upper] (get limits axis)
                   value (nth euler index)
                   limited (m/clamp value lower upper)]
               [(assoc values index limited)
                (if (> (m/abs (- value limited)) 1.0e-9)
                  (conj violations {:bone joint-id
                                    :axis axis
                                    :value value
                                    :limit [lower upper]})
                  violations)]))
           [euler []]
           {:x 0 :y 1 :z 2})]
      [(m/euler->q clamped) violations])
    [rotation []]))

(defn clamp
  "Clamp every joint of `pose` into its limits.

  Returns `{:pose pose' :violations [...]}`; the clamped pose is a new value and
  the argument is untouched."
  [skeleton pose]
  (reduce
   (fn [{:keys [pose] :as result} joint-id]
     (let [rotation (m/q-unit (get-in pose [:bones joint-id] m/quat-identity))
           [clamped violations] (rotation->euler-limits skeleton joint-id rotation)]
       (cond-> (update result :violations into violations)
         (not= clamped rotation)
         (assoc-in [:pose :bones joint-id] clamped))))
   {:pose pose :violations []}
   (keys (:bones pose))))

(defn limit-violations
  "List every joint of `pose` whose rotation lies outside its limits."
  [skeleton pose]
  (:violations (clamp skeleton pose)))

;; ---------------------------------------------------------------------------
;; Semantic controls

(def ^:private distributed
  (fn [ids amount] (into {} (map (fn [id] [id amount]) ids))))

(defn- side-controls [side]
  (let [sign (if (= side :left) 1.0 -1.0)
        n (name side)
        id (fn [suffix] (keyword (str n "-" suffix)))
        toe-ids (fn [segment]
                  (for [number (range 1 6)
                        :let [prefix (if (= number 1)
                                       (keyword (str n "-toe1"))
                                       (keyword (str n "-toe" number)))]]
                    [(keyword (str (name prefix) "-" (name segment)))
                     (cond
                       (not= segment :proximal) 0.4
                       (= number 1) 0.6
                       :else 0.6)]))]
    [{:id (id "scapula-elevate") :group :shoulder
      :label (str "Scapula elevation (" n ")")
      :axis :z :range 0.35 :sign (- sign) :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "scapula") 0.6 (id "clavicle") 0.4}}
     {:id (id "shoulder-flex") :group :shoulder
      :label (str "Shoulder flexion, raise forward (" n ")")
      :axis :x :range 2.60 :sign -1.0 :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "upper-arm") 1.0}}
     {:id (id "shoulder-abduct") :group :shoulder
      :label (str "Shoulder abduction, raise sideways (" n ")")
      :axis :z :range 2.60 :sign sign :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "upper-arm") 1.0}}
     {:id (id "shoulder-rotate") :group :shoulder
      :label (str "Shoulder internal/external rotation (" n ")")
      :axis :y :range 1.20 :sign sign :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "upper-arm") 1.0}}
     {:id (id "elbow-flex") :group :arm
      :label (str "Elbow flexion (" n ")")
      :axis :x :range 2.50 :sign -1.0 :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "forearm") 1.0}}
     {:id (id "wrist-flex") :group :hand
      :label (str "Wrist flexion, palm toward forearm (" n ")")
      :axis :x :range 1.10 :sign -1.0 :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "hand") 1.0}}
     {:id (id "wrist-deviate") :group :hand
      :label (str "Wrist deviation, thumb side (" n ")")
      :axis :z :range 0.50 :sign (- sign) :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "hand") 1.0}}
     {:id (id "fingers-curl") :group :hand
      :label (str "Finger curl (" n ")")
      :axis :x :range 1.40 :sign -1.0 :min 0.0 :max 1.0
      :unit :normalized
      :bones (into {}
                   (for [finger [:thumb :index :middle :ring :little]
                         segment (finger-segments finger)
                         :when (not (and (= finger :thumb) (= segment :metacarpal)))]
                     [(keyword (str n "-" (name finger) "-" (name segment)))
                      (if (= segment :distal) 0.55 0.45)]))}
     {:id (id "fingers-spread") :group :hand
      :label (str "Finger spread (" n ")")
      :axis :z :range 0.30 :sign sign :min -1.0 :max 1.0
      :unit :normalized
      :bones (into {}
                   (for [finger [:thumb :index :middle :ring :little]
                         :let [segment (first (finger-segments finger))]]
                     [(keyword (str n "-" (name finger) "-" (name segment)))
                      (case finger :thumb 0.8 :index 1.0 :middle 0.25 :ring -0.55 :little -0.9)]))}
     {:id (id "thumb-curl") :group :hand
      :label (str "Thumb curl (" n ")")
      :axis :x :range 1.10 :sign -1.0 :min 0.0 :max 1.0
      :unit :normalized
      :bones {(keyword (str n "-thumb-proximal")) 0.5
              (keyword (str n "-thumb-distal")) 0.5}}
     {:id (id "hip-flex") :group :leg
      :label (str "Hip flexion, knee forward (" n ")")
      :axis :x :range 1.70 :sign -1.0 :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "thigh") 1.0}}
     {:id (id "hip-abduct") :group :leg
      :label (str "Hip abduction, knee outward (" n ")")
      :axis :z :range 0.80 :sign sign :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "thigh") 1.0}}
     {:id (id "hip-rotate") :group :leg
      :label (str "Hip rotation (" n ")")
      :axis :y :range 0.60 :sign sign :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "thigh") 1.0}}
     {:id (id "knee-flex") :group :leg
      :label (str "Knee flexion (" n ")")
      :axis :x :range 2.50 :sign 1.0 :min 0.0 :max 1.0
      :unit :normalized
      :bones {(id "shin") 1.0}}
     {:id (id "ankle-flex") :group :foot
      :label (str "Ankle plantar flexion, toes down (" n ")")
      :axis :x :range 0.85 :sign 1.0 :min -1.0 :max 1.0
      :unit :normalized
      :bones {(id "foot") 1.0}}
     {:id (id "toes-curl") :group :foot
      :label (str "Toe curl (" n ")")
      :axis :x :range 1.00 :sign 1.0 :min 0.0 :max 1.0
      :unit :normalized
      :bones (into {} (concat (toe-ids :proximal) (toe-ids :distal)))}]))

(defn controls
  "Return the semantic control table for `skeleton` as plain data.

  Each control maps a normalized `[-1, 1]` value onto weighted per-bone axis
  rotations: `{:id :group :label :axis :range :sign :bones :min :max :unit}`.
  `:range` is the rotation in radians at value 1; `:bones` maps joint ids to
  weights."
  [_skeleton]
  (let [lumbar (mapv #(keyword (str "spine-lumbar-" %)) (range 1 (inc lumbar-levels)))
        thoracic (mapv #(keyword (str "spine-thoracic-" %)) (range 1 (inc thoracic-levels)))
        cervical (mapv #(keyword (str "spine-cervical-" %)) (range 1 (inc cervical-levels)))]
    (vec
     (concat
      [{:id :spine-flex :group :spine :label "Spine flexion, forward"
        :axis :x :range 0.55 :sign -1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (merge (distributed lumbar 0.55) (distributed thoracic 0.45))}
       {:id :spine-twist :group :spine :label "Spine twist, towards the left"
        :axis :y :range 0.60 :sign 1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (merge (distributed lumbar 0.45) (distributed thoracic 0.55))}
       {:id :spine-side :group :spine :label "Spine side bend, towards the left"
        :axis :z :range 0.50 :sign -1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (merge (distributed lumbar 0.55) (distributed thoracic 0.45))}
       {:id :neck-flex :group :neck :label "Neck flexion, chin down"
        :axis :x :range 0.45 :sign -1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (distributed (conj cervical :head) 0.30)}
       {:id :neck-turn :group :neck :label "Neck turn, towards the left"
        :axis :y :range 0.45 :sign 1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (distributed cervical 0.30)}
       {:id :neck-side :group :neck :label "Neck side bend, towards the left"
        :axis :z :range 0.30 :sign -1.0 :min -1.0 :max 1.0 :unit :normalized
        :bones (distributed cervical 0.20)}]
      (side-controls :left)
      (side-controls :right)))))

(defn control
  "Return the control with `control-id`, or nil."
  [skeleton control-id]
  (some #(when (= control-id (:id %)) %) (controls skeleton)))

(defn control-ids [skeleton]
  (mapv :id (controls skeleton)))