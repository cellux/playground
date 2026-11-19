(ns omkamra.body.pose
  "Serializable body poses and the builder API around them.

  A pose is plain data (D3):

      {:omkamra.body.pose/version 1
       :skeleton :omkamra.body/canonical
       :root {:translation [0 0 0] :rotation [0 0 0 1]}
       :bones {:left-elbow [0 0 0 1] ...}}

  `:root` is an absolute world transform of the skeleton root; every `:bones`
  entry is a local rotation relative to the joint's rest rotation. Rotations
  are quaternions `[x y z w]` (D2), which makes a pose independent of Euler
  ordering and safe to blend. Semantic controls (D4,
  `omkamra.body.skeleton/controls`) read and write the same `:bones` map, so a
  pose has exactly one representation no matter how it was authored."
  (:require [omkamra.body.math :as m]
            [omkamra.body.skeleton :as sk]))

(def schema-version 1)

(defn pose
  "Create an identity pose.

  Options: `:skeleton` (the skeleton id the pose is written for), `:root`
  (`{:translation :rotation}`), `:bones` (a map of joint id to quaternion)."
  ([]
   (pose nil))
  ([{:keys [skeleton root bones]}]
   {:omkamra.body.pose/version schema-version
    :skeleton (or skeleton sk/skeleton-id)
    :root (merge {:translation m/v-zero :rotation m/quat-identity} root)
    :bones (or bones {})}))

(defn pose? [value]
  (and (map? value)
       (= schema-version (get value :omkamra.body.pose/version))))

(defn version [pose] (get pose :omkamra.body.pose/version))

(defn root [pose] (:root pose))

(defn rotations [pose] (:bones pose))

(defn rotation
  "Local rotation of a joint; the identity when the joint is unposed."
  [pose joint-id]
  (m/q-unit (get-in pose [:bones joint-id] m/quat-identity)))

(defn joint-euler
  "Local rotation of a joint as Euler angles in the `:xyz` order."
  [pose joint-id]
  (m/q->euler (rotation pose joint-id)))

(defn set-rotation
  "Set a joint's local rotation to a quaternion."
  [pose joint-id quaternion]
  (assoc-in pose [:bones joint-id] (m/q-unit quaternion)))

(defn set-euler
  "Set a joint's local rotation from Euler angles in the `:xyz` order."
  [pose joint-id euler]
  (set-rotation pose joint-id (m/euler->q euler)))

(defn rotate
  "Compose a rotation onto a joint's existing local rotation.

  The axis is interpreted in the joint's local frame: the resulting local
  rotation is `delta * current`, so the delta is applied in the parent frame."
  [pose joint-id axis angle]
  (set-rotation pose joint-id
                (m/q-mul (m/q-from-axis-angle axis angle) (rotation pose joint-id))))

(defn set-root
  "Replace the root transform fields present in `transform`."
  [pose transform]
  (update pose :root merge transform))

(defn translate-root [pose translation]
  (set-root pose {:translation (m/v+ (get-in pose [:root :translation] m/v-zero)
                                     translation)}))

(defn set-root-rotation [pose quaternion]
  (set-root pose {:rotation (m/q-unit quaternion)}))

(defn merge-poses
  "Overlay `overlay` on `base`: the overlay's root fields and joint rotations
  win, everything else is preserved."
  [base overlay]
  (-> base
      (update :root merge (:root overlay))
      (update :bones merge (:bones overlay))))

(defn- control-angle
  [control value]
  (* (:sign control) (:range control) (m/clamp value (:min control) (:max control))))

(defn set-control
  "Set a semantic control to a normalized value in `[-1, 1]`.

  Only the control's axis is written on each of its joints; other axes the
  caller authored are preserved. A control whose value changes the rotation
  beyond a joint's limits is not clamped here — use
  `omkamra.body.skeleton/clamp` when a limits-respecting result is required."
  [skeleton pose control-id value]
  (if-let [control (sk/control skeleton control-id)]
    (let [angle (control-angle control value)
          axis-index ({:x 0 :y 1 :z 2} (:axis control))]
      (reduce-kv
       (fn [pose joint-id weight]
         (let [euler (joint-euler pose joint-id)
               euler (assoc euler axis-index (+ (nth euler axis-index) (* angle weight)))]
           (set-euler pose joint-id euler)))
       pose
       (:bones control)))
    (throw (ex-info "unknown control"
                    {:control control-id
                     :available (sk/control-ids skeleton)}))))

(defn set-controls
  "Set several semantic controls, in map iteration order."
  [skeleton pose control-values]
  (reduce-kv (fn [pose control-id value]
               (set-control skeleton pose control-id value))
             pose
             control-values))

(defn control-value
  "Read a semantic control back from a pose.

  The value is recovered from the first weighted joint of the control and is
  therefore only exact for poses authored through controls; a pose authored
  with raw joint rotations reports the closest normalized approximation."
  [skeleton pose control-id]
  (if-let [control (sk/control skeleton control-id)]
    (let [axis-index ({:x 0 :y 1 :z 2} (:axis control))
          [joint-id weight] (first (filter (fn [[_ weight]] (not (zero? weight)))
                                           (:bones control)))]
      (if joint-id
        (let [angle (nth (joint-euler pose joint-id) axis-index)
              scale (* (:sign control) (:range control) weight)]
          (if (zero? scale)
            (:min control)
            (m/clamp (/ angle scale) (:min control) (:max control))))
        (:min control)))
    (throw (ex-info "unknown control"
                    {:control control-id
                     :available (sk/control-ids skeleton)}))))

(defn controls
  "Read every semantic control of `skeleton` from `pose`."
  [skeleton pose]
  (into {} (map (fn [control] [(:id control) (control-value skeleton pose (:id control))])
                (sk/controls skeleton))))

(defn- finite-number? [x]
  (and (number? x) #?(:clj (Double/isFinite (double x))
                      :cljs (js/isFinite x))))

(defn- quaternion-error [q]
  (cond
    (not (and (vector? q) (= 4 (count q)))) :not-a-quaternion
    (not (every? finite-number? q)) :not-finite
    (< (m/q-norm q) 1.0e-9) :degenerate
    :else nil))

(defn validate
  "Validate `pose` against `skeleton` without throwing.

  Returns `{:valid? bool :errors [...] :warnings [...]}`. Errors are
  structural problems a caller must fix: wrong schema version, an unknown
  joint, a malformed quaternion, a malformed root. Warnings are anatomical
  limit violations, which callers may choose to clamp or accept."
  [skeleton pose]
  (let [version-error (when-not (pose? pose)
                        {:type :unsupported-version
                         :message "not a supported pose document"
                         :expected schema-version
                         :actual (get pose :omkamra.body.pose/version)})
        skeleton-error (when-let [pose-skeleton (:skeleton pose)]
                         (when (not= pose-skeleton (:id skeleton))
                           {:type :skeleton-mismatch
                            :message "pose was authored for another skeleton"
                            :expected (:id skeleton)
                            :actual pose-skeleton}))
        root-errors
        (when (map? pose)
          (let [{:keys [translation rotation]} (:root pose)]
            (concat
             (when (and translation
                        (not (and (vector? translation) (= 3 (count translation))
                                  (every? finite-number? translation))))
               [{:type :invalid-root
                 :message "root translation must be a vector of three numbers"}])
             (when (and rotation
                        (quaternion-error rotation))
               [{:type :invalid-root
                 :message "root rotation must be a quaternion"}]))))
        joint-errors
        (for [[joint-id rotation] (:bones pose)
              :let [error (cond
                            (nil? (sk/joint skeleton joint-id)) :unknown-joint
                            :else (quaternion-error rotation))]
              :when error]
          {:type (if (= :unknown-joint error) :unknown-joint :invalid-rotation)
           :joint joint-id
           :message (if (= :unknown-joint error)
                      "pose refers to a joint the skeleton does not have"
                      "joint rotation is not a usable quaternion")})
        violations (when (and (pose? pose) (empty? joint-errors))
                     (sk/limit-violations skeleton pose))]
    {:valid? (and (nil? version-error)
                  (nil? skeleton-error)
                  (empty? root-errors)
                  (empty? joint-errors))
     :errors (vec (remove nil? (concat [version-error skeleton-error]
                                       root-errors
                                       joint-errors)))
     :warnings (vec (for [violation violations]
                      {:type :limit-violation
                       :message "joint rotation exceeds its anatomical limits"
                       :joint (:bone violation)
                       :axis (:axis violation)
                       :value (:value violation)
                       :limit (:limit violation)}))}))

(defn valid? [skeleton pose]
  (:valid? (validate skeleton pose)))

(defn to-data
  "Return a serialization-ready copy of `pose`.

  Every supported field is already plain data; this function validates the
  document, rejects an unsupported schema version and returns the pose with
  normalized quaternions."
  [skeleton pose]
  (let [{:keys [valid? errors]} (validate skeleton pose)]
    (when-not valid?
      (throw (ex-info "cannot serialize an invalid pose"
                      {:errors errors})))
    (-> pose
        (update :bones (fn [bones]
                         (into {} (map (fn [[joint-id rotation]]
                                         [joint-id (m/q-unit rotation)]))
                               bones)))
        (update :root (fn [root]
                        (-> root
                            (update :translation #(mapv double (or % m/v-zero)))
                            (update :rotation #(m/q-unit (or % m/quat-identity)))))))))

(defn from-data
  "Validate and normalize a serialized pose document.

  Throws `ex-info` with `:errors` when the document is invalid. Accepts a
  `:skeleton` id other than the canonical skeleton's only when it matches
  `skeleton`."
  [skeleton data]
  (let [{:keys [valid? errors]} (validate skeleton data)]
    (when-not valid?
      (throw (ex-info "invalid pose document"
                      {:errors errors})))
    (to-data skeleton data)))