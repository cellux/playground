(ns omkamra.body.skins.makehuman
  "The detailed default skin: MakeHuman's CC0 base mesh and default rig.

  The asset is built by `omkamra.body.skins.makehuman.build` from MakeHuman's
  CC0 base mesh, default skeleton and weights (see
  `resources/omkamra/body/skins/MAKEHUMAN.md`), and cached as
  `makehuman.glb`: 14,677 vertices, 27,036 triangles, 163 bones including all
  finger and toe segments. It is a neutral, unclothed, realistic adult body.

  Rigged skins map canonical joints onto the asset's bones (D18). MakeHuman's
  rig is coarser than the canonical skeleton in a few places — five spine
  bones instead of twenty-four, three neck bones, one bone per limb segment —
  so the mapping samples the canonical chain at the nearest joint for each
  asset bone. Canonical joints that have no asset bone are not lost: the
  adapter drives every mapped bone by its canonical *world* rotation, so a
  bone inherits all rotations of the canonical joints above it.

  The rig also contains bones the canonical body does not expose (facial
  muscles, eyes, tongue, intermediate limb bones). They are left at rest and
  simply follow their parents."
  (:require [omkamra.body.skin :as skin]))

(def skin-id :omkamra.body.skins/makehuman)

(def asset-url
  "Where the examples' development server publishes the committed GLB.

  An application embedding the library serves the asset itself and passes its
  own URL to `:asset :url`."
  "/omkamra/body/dev/assets/makehuman")

(def ^:private side->makehuman
  {:left "L"
   :right "R"})

(defn- side-bones
  "Canonical-to-MakeHuman bone mapping for one body side.

  Bone names come from MakeHuman's default rig. The arm chain uses the
  shoulder, elbow and wrist bones (`upperarm01`, `lowerarm01`, `wrist`); the
  intermediate `shoulder01`, `upperarm02`, `lowerarm02` and the metacarpals
  are left at rest. Toes use the first and last segment of each MakeHuman toe
  chain, skipping MakeHuman's middle segment.
  "
  [side]
  (let [mh (get side->makehuman side)
        n (name side)
        k (fn [suffix] (keyword (str n "-" suffix)))
        bone (fn [name] (str name "." mh))]
    (merge
     {(k "clavicle") (bone "clavicle")
      (k "upper-arm") (bone "upperarm01")
      (k "forearm") (bone "lowerarm01")
      (k "hand") (bone "wrist")
      (k "thumb-metacarpal") (bone "finger1-1")
      (k "thumb-proximal") (bone "finger1-2")
      (k "thumb-distal") (bone "finger1-3")
      (k "thigh") (bone "upperleg01")
      (k "shin") (bone "lowerleg01")
      (k "foot") (bone "foot")
      (k "toe1-proximal") (bone "toe1-1")
      (k "toe1-distal") (bone "toe1-2")}
     ;; Fingers 2..5 in canonical order index, middle, ring, little.
     (into {}
           (for [[finger makehuman-finger] [[:index 2] [:middle 3] [:ring 4] [:little 5]]
                 [segment makehuman-segment] [[:proximal 1] [:middle 2] [:distal 3]]]
             [(keyword (str n "-" (name finger) "-" (name segment)))
              (bone (str "finger" makehuman-finger "-" makehuman-segment))]))
     ;; Toes 2..5 keep their first and last MakeHuman segments.
     (into {}
           (for [number (range 2 6)
                 [segment makehuman-segment] [[:proximal 1] [:distal 3]]]
             [(keyword (str n "-toe" number "-" (name segment)))
              (bone (str "toe" number "-" makehuman-segment))])))))

(def bones
  "Canonical joint id to MakeHuman bone name.

  The spine samples the canonical chain at eight heights, so a bend is
  distributed over the rig rather than concentrated in one bone; the head
  bone follows the whole cervical chain."
  (merge
   {:pelvis "root"
    :spine-lumbar-1 "spine05"
    :spine-lumbar-4 "spine04"
    :spine-thoracic-2 "spine03"
    :spine-thoracic-7 "spine02"
    :spine-thoracic-12 "spine01"
    :spine-cervical-1 "neck01"
    :spine-cervical-4 "neck02"
    :spine-cervical-7 "neck03"
    :head "head"}
   (side-bones :left)
   (side-bones :right)))

(def descriptor
  "The MakeHuman skin as data: presentation, deformation and capabilities.

  `:rig :bones` is the canonical-to-asset mapping; `:rig :skeleton-root` names
  the asset node that acts as the skeleton root."
  (skin/skin
   {:id skin-id
    :label "MakeHuman detailed body"
    :kind :rigged
    :capabilities #{:fingers :toes :detailed :makehuman}
    :asset {:format :glb
            :url asset-url
            :license "CC0-1.0"
            :attribution "MakeHuman base mesh and default rig, CC0 1.0"}
    :rig {:bones bones
          :skeleton-root "root"}}))

(defn preview []
  {:id (:id descriptor)
   :label (:label descriptor)
   :kind (:kind descriptor)
   :mapped-joints (count bones)
   :license (get-in descriptor [:asset :license])})