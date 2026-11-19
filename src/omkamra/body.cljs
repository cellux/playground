(ns omkamra.body
  "Entry point for the render-independent body model and its browser viewer.

  The library is layered so each layer can be used on its own:

  - `omkamra.body.skeleton` — the canonical body, forward kinematics and
    anatomical limits;
  - `omkamra.body.pose` — serializable poses, builders and semantic controls;
  - `omkamra.body.intent` — semantic intent documents resolved into poses;
  - `omkamra.body.ik` — deterministic multi-limb inverse kinematics;
  - `omkamra.body.animation` — clips, blending and explicit-time playback;
  - `omkamra.body.skin` — the skin contract;
  - `omkamra.body.skins.mannequin` — the default procedural skin;
  - `omkamra.body.three` — the Three.js adapter;
  - `omkamra.body.viewer` — the self-contained viewer.

  The helpers below are the shortest useful path: a default skin, a pose from
  semantic controls, a viewer that plays a sequence of poses.

      (require '[omkamra.body :as body])
      (body/mount! (js/document.getElementById \"stage\") {})
      (body/show! viewer (body/pose-of {:left-shoulder-abduct 1.0}))

  Nothing in this namespace or below it knows about yoga. Poses, transitions
  and any domain vocabulary belong to the calling application."
  (:require [omkamra.body.animation :as animation]
            [omkamra.body.ik :as ik]
            [omkamra.body.intent :as intent]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as skeleton]
            [omkamra.body.skin :as skin]
            [omkamra.body.skins.makehuman :as makehuman]
            [omkamra.body.skins.mannequin :as mannequin]
            [omkamra.body.three :as three]
            [omkamra.body.viewer :as viewer]))

(def canonical-skeleton
  "The canonical body skeleton: every pose-relevant joint, its rest offset,
  length and anatomical limits."
  skeleton/canonical-skeleton)

(defn controls
  "The semantic control table as data."
  ([] (controls canonical-skeleton))
  ([skeleton] (skeleton/controls skeleton)))

(defn identity-pose
  "An identity pose; the pose every skeleton starts from."
  ([] (pose/pose))
  ([options] (pose/pose options)))

(defn pose-of
  "A pose from a map of semantic controls.

      (pose-of {:spine-flex 0.3 :left-knee-flex 0.5})

  Values are normalized to `[-1, 1]`; see `controls` for the table."
  ([control-values] (pose-of canonical-skeleton control-values))
  ([skeleton control-values]
   (pose/set-controls skeleton (pose/pose) control-values)))

(defn resolve-intent
  "Resolve an intent document into `{:status :pose :diagnostics}`."
  ([document] (resolve-intent canonical-skeleton document))
  ([skeleton document] (intent/resolve skeleton document)))

(defn solve-ik
  "Solve limb targets for a pose; see `omkamra.body.ik/solve`."
  ([options] (solve-ik canonical-skeleton options))
  ([skeleton options]
   (ik/solve skeleton (or (:base options) (pose/pose)) options)))

(defn makehuman-skin
  "The detailed default skin: MakeHuman's CC0 body (D20)."
  [] makehuman/descriptor)

(defn mannequin-skin
  "The procedural fallback skin, drawn directly from the canonical skeleton."
  ([] (mannequin-skin canonical-skeleton))
  ([skeleton] (mannequin/descriptor skeleton)))

(defn default-skin
  "The detailed MakeHuman skin; the mannequin is the instant/fallback skin."
  [] (makehuman-skin))

(defn mount!
  "Mount a viewer, rendering with the detailed MakeHuman skin.

  The mannequin is shown immediately and replaced by the detailed body once
  the GLB has loaded (D19), so a page never waits on a 1.4 MiB asset. Options
  are passed to `omkamra.body.viewer/mount!` plus:

  - `:skin` — a skin descriptor to use instead of MakeHuman; a procedural
    descriptor is used as is, a rigged one loads like the default;
  - `:fallback-skin` — the procedural skin shown while loading and when
    loading fails (default: the mannequin);
  - `:on-skin-loaded` — `(fn [{:keys [object3d gltf rig]}])`;
  - `:on-skin-error` — `(fn [error])`; the fallback skin stays visible.

  A preloaded `:rig` skips loading entirely."
  ([element] (mount! element {}))
  ([element options]
   (let [skeleton (or (:skeleton options) canonical-skeleton)
         skin (or (:skin options) (default-skin))
         fallback (or (:fallback-skin options) (mannequin/descriptor skeleton))
         initial (if (and (skin/rigged? skin) (nil? (:rig options)))
                   fallback
                   skin)
         on-loaded (:on-skin-loaded options)
         on-error (:on-skin-error options)
         viewer (viewer/mount! element
                               (merge {:skeleton skeleton
                                       :skin initial
                                       :pose (pose/pose)}
                                      (dissoc options :skin :fallback-skin
                                              :on-skin-loaded :on-skin-error)))]
     (when (and (skin/rigged? skin) (nil? (:rig options)))
       (.then (three/load-rig! skeleton skin)
              (fn [^js loaded]
                ((:set-rig! viewer) (.-rig loaded))
                (when on-loaded (on-loaded loaded)))
              (fn [error]
                (when on-error (on-error error)))))
     viewer)))

(defn show!
  "Display a pose in a mounted viewer."
  [viewer-object pose-value]
  ((:set-pose! viewer-object) pose-value)
  viewer-object)

(defn play!
  "Play a clip in a mounted viewer."
  [viewer-object clip]
  ((:set-clip! viewer-object) clip)
  ((:play! viewer-object))
  viewer-object)

(defn transition!
  "Animate through `poses` over `duration-ms`, optionally looping."
  ([poses duration-ms] (transition! canonical-skeleton poses duration-ms nil))
  ([skeleton poses duration-ms options]
   (animation/transitions poses duration-ms
                          (merge {:skeleton (:id skeleton)} options))))

(defn dispose!
  "Dispose a mounted viewer."
  [viewer-object]
  ((:dispose! viewer-object)))

(defn blink!
  "Development helper: prove the browser build is live by appending a line of
  text to the document body. Used by the examples' smoke check."
  []
  (let [line (.createElement js/document "div")]
    (set! (.-textContent line) "omkamra.body loaded")
    (.appendChild (.-body js/document) line)))