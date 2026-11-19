(ns omkamra.body.intent
  "The intent layer: semantic commands resolved into a concrete pose.

  An intent is plain data (D5) and describes what the body should do rather
  than which joints should turn:

      {:omkamra.body.intent/version 1
       :base pose                       ; optional starting pose
       :controls {:spine-flex 0.2 :left-knee-flex 0.6}
       :joints {:left-forearm [0 0.1 0 0.99]}   ; optional raw rotations
       :root {:translation [0 0 0]}
       :targets {:left-arm {:position [0.3 1.5 0.2]}
                 :right-arm {:position [-0.3 1.5 0.2] :priority 2}
                 :left-leg {:position [0.12 0.05 0.08]}}
       :directions {:left-elbow [0 0 -1] :left-knee [0 0 1]}
       :mode :best-effort}

  Resolution is deterministic: start from `:base` (or the identity pose), apply
  the root, then semantic controls, then raw joint rotations, clamp into the
  anatomical limits when asked, and finally run inverse kinematics for the
  targets. The result always contains a pose and structured diagnostics, so a
  generated program can inspect what happened instead of guessing."
  (:refer-clojure :exclude [resolve])
  (:require [omkamra.body.ik :as ik]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def schema-version 1)

(defn intent
  "Create an intent document with defaults."
  ([]
   (intent nil))
  ([{:keys [base controls joints root targets directions mode options]}]
   (cond-> {:omkamra.body.intent/version schema-version
            :skeleton sk/skeleton-id}
     base (assoc :base base)
     (seq controls) (assoc :controls controls)
     (seq joints) (assoc :joints joints)
     root (assoc :root root)
     (seq targets) (assoc :targets targets)
     (seq directions) (assoc :directions directions)
     mode (assoc :mode mode)
     (seq options) (assoc :options options))))

(defn intent? [value]
  (and (map? value)
       (= schema-version (get value :omkamra.body.intent/version))))

(defn- normalise-rotation [rotation]
  (cond
    (and (vector? rotation) (= 4 (count rotation))) rotation
    (and (map? rotation) (vector? (:euler rotation))) (m/euler->q (:euler rotation))
    (and (map? rotation) (vector? (:axis-angle rotation)))
    (let [[axis angle] (:axis-angle rotation)] (m/q-from-axis-angle axis angle))
    :else nil))

(defn validate
  "Validate an intent against `skeleton`.

  Returns `{:valid? bool :errors [...] :warnings [...]}`. Unknown controls,
  unknown joints, malformed rotations, unknown limbs or malformed target
  positions are errors; they are reported instead of thrown so a generated
  program can correct itself."
  [skeleton intent]
  (let [version-error (when-not (intent? intent)
                        {:type :unsupported-version
                         :message "not a supported intent document"
                         :expected schema-version
                         :actual (get intent :omkamra.body.intent/version)})
        skeleton-error (when (and (:skeleton intent)
                                  (not= (:skeleton intent) (:id skeleton)))
                         {:type :skeleton-mismatch
                          :message "intent was authored for another skeleton"
                          :expected (:id skeleton)
                          :actual (:skeleton intent)})
        control-errors (for [[control-id _] (:controls intent)
                             :when (nil? (sk/control skeleton control-id))]
                         {:type :unknown-control
                          :control control-id
                          :message "intent refers to an unknown control"})
        joint-errors (for [[joint-id rotation] (:joints intent)
                           :let [normalised (normalise-rotation rotation)]
                           :when (or (nil? (sk/joint skeleton joint-id))
                                     (nil? normalised))]
                       {:type (if (nil? (sk/joint skeleton joint-id))
                                :unknown-joint
                                :invalid-rotation)
                        :joint joint-id
                        :message (if (nil? (sk/joint skeleton joint-id))
                                   "intent refers to a joint the skeleton does not have"
                                   "joint rotation is not a usable rotation")})
        target-errors (for [[limb target] (:targets intent)
                            :when (or (nil? (ik/limb-chain limb))
                                      (not (and (vector? (:position target))
                                                (= 3 (count (:position target))))))]
                        {:type :invalid-target
                         :limb limb
                         :message "target must name a limb and give a three-element position"})
        base-errors (when-let [base (:base intent)]
                      (when-not (pose/pose? base)
                        [{:type :invalid-base
                          :message ":base must be a pose document"}]))
        errors (vec (remove nil? (concat [version-error skeleton-error]
                                         control-errors
                                         joint-errors
                                         target-errors
                                         base-errors)))]
    {:valid? (empty? errors)
     :errors errors
     :warnings []}))

(defn- clamp-if [options skeleton pose]
  (if (false? (:clamp? options))
    {:pose pose :violations []}
    (sk/clamp skeleton pose)))

(defn resolve
  "Resolve `intent` into `{:status :pose :diagnostics}`.

  Status is `:invalid` when the intent cannot be interpreted, `:unreachable`
  when a strict solve failed, `:ok` when every target is satisfied, and
  `:best-effort` when a best-effort solve produced the closest pose it could.
  Invalid intents return the identity pose and never throw."
  ([skeleton intent]
   (resolve skeleton intent nil))
  ([skeleton intent context]
   (let [{:keys [valid? errors]} (validate skeleton intent)]
     (if-not valid?
       {:status :invalid
        :pose (or (:base intent) (pose/pose))
        :diagnostics {:errors errors
                      :warnings []
                      :targets []}}
       (let [options (merge {:iterations 12
                             :passes 2
                             :tolerance 0.005
                             :adjust-root? true
                             :root-adjust-limit 0.25
                             :clamp? true}
                            (:options intent)
                            (select-keys (or context {}) [:tolerance :iterations]))
             base (or (:base intent) (pose/pose))
             base (merge base (select-keys (or context {}) [:skeleton]))
             with-root (if-let [root (:root intent)]
                         (pose/set-root base (merge {:translation (get-in base [:root :translation])
                                                     :rotation (get-in base [:root :rotation])}
                                                    root))
                         base)
             with-controls (pose/set-controls skeleton with-root (:controls intent))
             with-joints (reduce-kv (fn [pose joint-id rotation]
                                      (if-let [quaternion (normalise-rotation rotation)]
                                        (pose/set-rotation pose joint-id quaternion)
                                        pose))
                                    with-controls
                                    (:joints intent))
             clamped (clamp-if options skeleton with-joints)
             solved (ik/solve skeleton (:pose clamped)
                              {:targets (:targets intent)
                               :directions (:directions intent)
                               :mode (or (:mode intent) :best-effort)
                               :iterations (:iterations options)
                               :passes (:passes options)
                               :tolerance (:tolerance options)
                               :adjust-root? (:adjust-root? options)
                               :root-adjust-limit (:root-adjust-limit options)
                               :locks (get options :locks #{})})
             final (if (false? (:clamp? options))
                     solved
                     (update solved :pose #(:pose (sk/clamp skeleton %))))]
         (update final :diagnostics
                 (fn [diagnostics]
                   (assoc diagnostics
                          :controls (select-keys (:controls intent)
                                                 (map :id (sk/controls skeleton)))
                          :initial-violations (:violations clamped)))))))))

(defn apply-intent
  "Resolve `intent` and return only the resulting pose."
  [skeleton intent]
  (:pose (resolve skeleton intent)))

(defn targets-satisfied?
  "True when the resolved diagnostics report every target within tolerance."
  [result]
  (and (= :ok (:status result))
       (every? :ok? (get-in result [:diagnostics :targets]))))