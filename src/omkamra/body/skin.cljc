(ns omkamra.body.skin
  "The skin contract: how presentation data attaches to the canonical body.

  A skin is plain data (D10) with three separable concerns:

  1. **presentation** — shapes, materials and (optionally) textures;
  2. **deformation** — the mapping from canonical joints to the bones of a
     rigged asset, or the procedural shapes driven by canonical transforms;
  3. **capabilities** — what the skin provides beyond the required contract.

  Two kinds exist:

  - `:procedural` skins describe geometry per canonical joint and are built by
    `omkamra.body.three/build-skin-object`;
  - `:rigged` skins reference an external asset and a joint-to-bone mapping
    built by `omkamra.body.three/bind-rig-object`.

  Every supported skin must cover the complete canonical body contract,
  including fingers and toes (D9). A skin may add internal detail it never
  exposes; it may not omit a required pose-relevant structure.

      {:omkamra.body.skin/version 1
       :id :omkamra.body.skins/mannequin
       :label \"Procedural mannequin\"
       :skeleton :omkamra.body/canonical
       :kind :procedural
       :capabilities #{:fingers :toes}
       :materials {:skin {:color 0xd8c8b8 :roughness 0.85 :metalness 0.0}}
       :shapes [{:joint :pelvis :shape :capsule :radius 0.105
                 :direction [0 1 0] :length 0.18 :material :skin} ...]
       :rig nil}"
  (:require [omkamra.body.skeleton :as sk]))

(def schema-version 1)

(defn skin
  "Create a skin descriptor with defaults."
  [{:keys [id label skeleton kind capabilities materials shapes rig asset version]}]
  (cond-> {:omkamra.body.skin/version schema-version
           :id id
           :label (or label (name id))
           :skeleton (or skeleton sk/skeleton-id)
           :kind (or kind :procedural)
           :capabilities (set capabilities)
           :materials (or materials {})
           :shapes (vec shapes)}
    version (assoc :version version)
    rig (assoc :rig rig)
    asset (assoc :asset asset)))

(defn skin? [value]
  (and (map? value)
       (= schema-version (get value :omkamra.body.skin/version))))

(defn procedural? [value]
  (= :procedural (:kind value)))

(defn rigged? [value]
  (= :rigged (:kind value)))

(defn capabilities [value] (set (:capabilities value)))

(defn shape-joints
  "The canonical joints a procedural skin draws."
  [value]
  (into #{} (map :joint (:shapes value))))

(defn mapped-joints
  "The canonical joints a rigged skin drives."
  [value]
  (into #{} (keys (get-in value [:rig :bones]))))

(defn covered-joints
  "The canonical joints the skin controls, whatever its kind."
  [value]
  (case (:kind value)
    :procedural (shape-joints value)
    :rigged (mapped-joints value)
    #{}))

(defn required-joints
  "Canonical joints every supported skin must cover.

  Fingers and toes are required (D9): a yoga body must be able to curl toes and
  spread fingers. The rib cage is presentation detail — ribs and the sternum
  follow the thoracic spine — so a skin may draw or bind them without exposing
  them as deformable structures."
  [skeleton]
  (into #{} (map :id) (remove #(= :ribcage (:region %)) (sk/bones skeleton))))

(defn missing-joints
  "Required canonical joints the skin does not cover."
  [skeleton value]
  (let [required (required-joints skeleton)
        covered (covered-joints value)]
    (vec (sort (remove covered required)))))

(defn missing-regions
  "Regions without any mapped or drawn joint, excluding the rib cage.

  A rigged skin must represent every region of the body even when its rig is
  coarser than the canonical skeleton; see D18."
  [skeleton value]
  (let [covered (covered-joints value)]
    (vec (sort (for [region (:regions skeleton)
                     :when (and (not= region :ribcage)
                                (not-any? (fn [joint] (contains? covered (:id joint)))
                                          (sk/region-bones skeleton region)))]
                 region)))))

(def ^:private detailed-regions
  "Regions whose every canonical joint must be mapped by a rigged skin.

  Fingers and toes are the articulation that distinguishes a yoga-grade body
  from a mannequin (D9), and MakeHuman's rig covers them completely."
  #{:finger :toe})

(defn missing-detailed-joints
  "Canonical finger and toe joints a rigged skin does not map."
  [skeleton value]
  (let [covered (covered-joints value)]
    (vec (sort (for [joint (sk/bones skeleton)
                     :when (and (contains? detailed-regions (:region joint))
                                (not (contains? covered (:id joint))))]
                 (:id joint))))))

(defn validate
  "Validate a skin descriptor against `skeleton`.

  Returns `{:valid? bool :errors [...] :warnings [...]}`. Structural problems
  are errors; a procedural skin that lets the torso be drawn by a few shapes
  instead of every vertebra is a warning, because the contract is about what a
  consumer can ask the skin to do, not about one mesh per joint."
  [skeleton value]
  (let [version-error (when-not (skin? value)
                        {:type :unsupported-version
                         :message "not a supported skin document"
                         :expected schema-version
                         :actual (get value :omkamra.body.skin/version)})
        skeleton-error (when (and (:skeleton value)
                                  (not= (:skeleton value) (:id skeleton)))
                         {:type :skeleton-mismatch
                          :message "skin was authored for another skeleton"
                          :expected (:id skeleton)
                          :actual (:skeleton value)})
        kind-error (when-not (#{:procedural :rigged} (:kind value))
                     {:type :unknown-kind
                      :message "a skin is :procedural or :rigged"
                      :actual (:kind value)})
        unknown-joints (for [{:keys [joint]} (:shapes value)
                             :when (not (sk/joint skeleton joint))]
                         {:type :unknown-joint
                          :joint joint
                          :message "shape refers to a joint the skeleton does not have"})
        unknown-materials (for [{:keys [material]} (:shapes value)
                                :when (and material
                                           (not (contains? (:materials value) material)))]
                            {:type :unknown-material
                             :material material
                             :message "shape refers to a material the skin does not define"})
        rig-error (when (and (rigged? value)
                             (not (seq (get-in value [:rig :bones]))))
                    {:type :incomplete-rig
                     :message "a rigged skin needs a joint-to-bone mapping"})
        missing (when (and (not version-error) (not kind-error))
                  (missing-joints skeleton value))
        missing-regions (when (and (not version-error) (not kind-error) (rigged? value))
                          (missing-regions skeleton value))
        missing-detailed (when (and (not version-error) (not kind-error) (rigged? value))
                           (missing-detailed-joints skeleton value))
        region-error (when (seq missing-regions)
                       {:type :incomplete-regions
                        :message "a rigged skin must represent every body region"
                        :regions missing-regions})
        detailed-error (when (seq missing-detailed)
                         {:type :incomplete-articulation
                          :message "a rigged skin must map every finger and toe joint"
                          :missing missing-detailed})
        errors (vec (remove nil? (concat [version-error skeleton-error kind-error
                                          rig-error]
                                         unknown-joints
                                         unknown-materials
                                         [region-error detailed-error])))]
    {:valid? (empty? errors)
     :errors errors
     :warnings (if (and (seq missing) (procedural? value))
                 [{:type :partial-presentation
                   :message "the procedural skin draws the body with fewer shapes than joints"
                   :undrawn missing}]
                 [])}))