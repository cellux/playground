(ns omkamra.body.three
  "Three.js adapter: turns evaluated canonical transforms into scene objects.

  Two paths exist (D11):

  - `build-skin-object` builds Three.js geometry for a `:procedural` skin
    descriptor, one `THREE.Group` per canonical joint, and returns an update
    function that copies evaluated world transforms onto those groups;
  - `bind-rig-object` drives an existing rigged asset (for example a glTF
    mesh) whose bones are named by the skin's `:rig :bones` mapping, applying
    each canonical joint rotation relative to the asset's rest pose.

  A skin may supply `:rig :update!`, a function of `[pose transforms]` called
  after the standard bone updates; that is v1's hook for corrective morphs or
  pose-space deformation (D11).

  Nothing here leaks into the core: `omkamra.body.skeleton`,
  `omkamra.body.pose`, `omkamra.body.ik` and `omkamra.body.animation` never
  require Three.js."
  (:require [clojure.string :as str]
            ["three" :as three]
            ["three/examples/jsm/loaders/GLTFLoader.js" :refer [GLTFLoader]]
            [omkamra.body.math :as m]
            [omkamra.body.skin :as skin]
            [omkamra.body.skeleton :as sk]))

(defn- make-material [{:keys [color roughness metalness vertex-colors]
                       :or {color 0xcccccc roughness 0.8 metalness 0.0}}]
  (three/MeshStandardMaterial.
   (clj->js (cond-> {:color color :roughness roughness :metalness metalness}
              vertex-colors (assoc :vertexColors true)))))

(defn- make-materials [descriptor]
  (let [materials (into {}
                        (map (fn [[name spec]]
                               [name (make-material spec)]))
                        (:materials descriptor))]
    (if (contains? materials :skin)
      materials
      (assoc materials :skin (make-material {})))))

(defn- capsule-geometry [radius length]
  (let [body-length (max (- (double length) (* 2.0 (double radius))) 0.001)]
    (three/CapsuleGeometry. (double radius) body-length 4 10)))

(defn- shape-geometry [{:keys [shape radius direction length size]}]
  (case shape
    :capsule (capsule-geometry radius (or length (when direction (m/v-len direction)) radius))
    :box (let [[w h d] size] (three/BoxGeometry. w h d))
    :sphere (three/SphereGeometry. (or radius 1.0) 20 14)
    (throw (ex-info "unknown skin shape" {:shape shape}))))

(defn- shape-placement
  "Local position and rotation of a shape inside its joint's group.

  A capsule lies along `:direction`, default +Y, and is centered halfway along
  it; boxes and spheres are centered on their `:offset`."
  [{:keys [shape direction offset length]}]
  (let [direction (if (or (nil? direction) (= m/v-zero direction))
                    [0.0 1.0 0.0]
                    direction)
        unit (m/v-unit direction)
        length (or length (m/v-len direction))
        offset (or offset m/v-zero)]
    (if (= shape :capsule)
      {:position (m/v+ offset (m/v-scale unit (/ (double length) 2.0)))
       :rotation (m/q-from-to [0.0 1.0 0.0] unit)}
      {:position offset
       :rotation m/quat-identity})))

(defn- place!
  "Copy a `[x y z]` position and `[x y z w]` quaternion onto a Three.js object."
  [object position rotation]
  (let [p (.-position object)
        q (.-quaternion object)]
    (set! (.-x p) (double (nth position 0)))
    (set! (.-y p) (double (nth position 1)))
    (set! (.-z p) (double (nth position 2)))
    (when rotation
      (set! (.-x q) (double (nth rotation 0)))
      (set! (.-y q) (double (nth rotation 1)))
      (set! (.-z q) (double (nth rotation 2)))
      (set! (.-w q) (double (nth rotation 3))))))

(defn- add-shape! [group materials geometry-cache spec]
  (let [{:keys [scale material]} spec
        geometry (or (get @geometry-cache spec)
                     (let [geometry (shape-geometry spec)]
                       (swap! geometry-cache assoc spec geometry)
                       geometry))
        mesh (three/Mesh. geometry (get materials material (:skin materials)))
        {:keys [position rotation]} (shape-placement spec)]
    (place! mesh position rotation)
    (when scale
      (set! (.. mesh -scale -x) (double (nth scale 0)))
      (set! (.. mesh -scale -y) (double (nth scale 1)))
      (set! (.. mesh -scale -z) (double (nth scale 2))))
    (.add group mesh)
    mesh))

(defn build-skin-object
  "Build scene objects for a procedural skin.

  Returns `{:object3d THREE.Group :joints {joint-id THREE.Group}
  :!update (fn [transforms]) :dispose (fn [])}`. The group can be added to a
  scene as it is; `:!update` copies evaluated world transforms onto it."
  [_skeleton descriptor]
  (let [materials (make-materials descriptor)
        geometry-cache (atom {})
        root (three/Group.)
        joints (reduce
                (fn [groups spec]
                  (let [joint (:joint spec)
                        group (or (get groups joint)
                                  (let [group (three/Group.)]
                                    (.add root group)
                                    group))]
                    (add-shape! group materials geometry-cache spec)
                    (assoc groups joint group)))
                {}
                (:shapes descriptor))
        update! (fn [transforms _pose]
                  (doseq [[joint group] joints]
                    (when-let [{:keys [position rotation]} (get transforms joint)]
                      (place! group position rotation))))
        dispose (fn []
                  (doseq [geometry (vals @geometry-cache)]
                    (.dispose geometry))
                  (doseq [material (vals materials)]
                    (.dispose material)))]
    {:object3d root
     :joints joints
     :!update update!
     :dispose dispose}))

;; ---------------------------------------------------------------------------
;; Rigged assets

(defn- find-bone
  "Find a bone by name, tolerating glTF loader name sanitization.

  Three.js' GLTFLoader removes reserved characters from node names, so
  MakeHuman's `upperleg01.L` becomes `upperleg01L`. The exact name is tried
  first, then the sanitized variants."
  [^js object3d bone-name]
  (let [name (name bone-name)]
    (or (.getObjectByName object3d name)
        (.getObjectByName object3d (str/replace name #"[.\s]" ""))
        (.getObjectByName object3d (str/replace name #"[.\s]" "_")))))

(defn- bone-depth
  "How many parents a Three.js object has; used to process rig bones before
  their children."
  [^js bone]
  (loop [node bone depth 0]
    (if-let [parent (.-parent node)]
      (recur parent (inc depth))
      depth)))

(defn- quaternion->vector [^js quaternion]
  [(.-x quaternion) (.-y quaternion) (.-z quaternion) (.-w quaternion)])

(defn- set-quaternion! [^js bone [x y z w]]
  (let [quaternion (.-quaternion bone)]
    (set! (.-x quaternion) (double x))
    (set! (.-y quaternion) (double y))
    (set! (.-z quaternion) (double z))
    (set! (.-w quaternion) (double w))))

(defn- collect-bones [^js object3d]
  (let [bones (atom [])]
    (.traverse object3d (fn [^js node]
                          (when (.-isBone node) (swap! bones conj node))))
    @bones))

(defn- parent-bone [^js bone]
  (loop [parent (.-parent bone)]
    (cond
      (nil? parent) nil
      (.-isBone parent) parent
      :else (recur (.-parent parent)))))

(defn bind-rig-object
  "Bind an existing rigged `object3d` to the canonical joints.

  The skin's `:rig :bones` maps canonical joint ids to bone names in the
  asset; unmapped canonical joints are not lost, because every mapped bone is
  driven by its canonical joint's *world* rotation (D18).

  Every bone of the rig is tracked, mapped or not, so a mapped bone whose rig
  parent is an unmapped intermediate bone (MakeHuman has several) still sees
  its parent's current world rotation. Unmapped bones keep their loaded local
  rotation and simply follow their parents.

  At bind time each mapped bone's rest world rotation is captured; a canonical
  pose rotation is then applied in world space on top of that rest rotation.
  The asset therefore keeps its own rest pose (MakeHuman's is an A-pose), and
  a pose is a set of rotations relative to rest, which is what makes poses
  portable between skins up to their rest-pose differences (D12, D18).
  Bone positions are never driven, so each asset keeps its proportions.

  Options read from the skin: `:rig :scale` (uniform, default 1.0) and
  `:rig :update!`, a function of `[pose transforms]` called after the bones
  move; that hook is where corrective morphs live in v1 (D11).

  Returns `{:object3d :!update :dispose :missing}`."
  [_skeleton descriptor ^js object3d]
  (let [mapping (get-in descriptor [:rig :bones])
        world-rotation (three/Quaternion.)
        _ (.updateMatrixWorld object3d true)
        entries (mapv (fn [^js bone]
                        (.getWorldQuaternion bone world-rotation)
                        {:bone bone
                         :parent (parent-bone bone)
                         :rest-local (quaternion->vector (.-quaternion bone))
                         :rest-world (quaternion->vector world-rotation)
                         :depth (bone-depth bone)})
                      (collect-bones object3d))
        by-bone (into {} (map (fn [entry] [(:bone entry) entry]) entries))
        joint->bone (into {}
                          (keep (fn [[joint bone-name]]
                                  (when-let [bone (find-bone object3d bone-name)]
                                    (when (contains? by-bone bone)
                                      [joint bone]))))
                          mapping)
        bone->joint (into {} (map (fn [[joint bone]] [bone joint]) joint->bone))
        ordered (vec (sort-by :depth entries))
        missing (vec (sort (remove (set (keys joint->bone)) (keys mapping))))
        update-hook (get-in descriptor [:rig :update!])
        scale-factor (double (or (get-in descriptor [:rig :scale]) 1.0))
        _ (when (not= 1.0 scale-factor)
            (.setScalar (.-scale object3d) scale-factor))
        update! (fn [transforms pose]
                  (let [worlds (volatile! {})]
                    (doseq [{:keys [bone parent rest-local rest-world]} ordered]
                      (let [parent-world (if parent (get @worlds parent) m/quat-identity)
                            joint (get bone->joint bone)
                            world (if joint
                                    (m/q-mul (m/q-unit (:rotation (get transforms joint)))
                                             rest-world)
                                    (m/q-mul parent-world rest-local))]
                        (when joint
                          (set-quaternion! bone (m/q-mul (m/q-conj parent-world) world)))
                        (vswap! worlds assoc bone world)))
                    (when update-hook
                      (update-hook pose transforms))))]
    {:object3d object3d
     :!update update!
     :missing missing
     :dispose (fn [])}))

(defn load-rig!
  "Load a GLB asset named by the skin's `:asset :url` and bind it.

  Returns a JavaScript Promise resolving to
  `{:object3d :gltf :rig}` and rejecting with the loader error. The caller
  decides what to show while loading and what to do on failure; the viewer
  keeps the current skin until a rig arrives."
  [skeleton descriptor]
  (let [url (get-in descriptor [:asset :url])]
    (js/Promise.
     (fn [resolve reject]
       (let [loader (GLTFLoader.)]
         (.load loader url
                (fn [gltf]
                  (let [object3d (.-scene gltf)]
                    (resolve #js {:object3d object3d
                                  :gltf gltf
                                  :rig (bind-rig-object skeleton descriptor object3d)})))
                nil
                (fn [error] (reject error))))))))

;; ---------------------------------------------------------------------------
;; Debug visuals

(defn- axis-geometry []
  (let [positions (js/Float32Array.
                   #js [0 0 0 0.04 0 0
                        0 0 0 0 0.04 0
                        0 0 0 0 0 0.04])
        colors (js/Float32Array.
                #js [1 0.2 0.2 1 0.2 0.2
                     0.2 1 0.2 0.2 1 0.2
                     0.2 0.4 1 0.2 0.4 1])
        geometry (three/BufferGeometry.)]
    (.setAttribute geometry "position" (three/BufferAttribute. positions 3))
    (.setAttribute geometry "color" (three/BufferAttribute. colors 3))
    geometry))

(defn build-debug-object
  "Joint markers and local axes for inspecting a posed body.

  Returns the same shape as `build-skin-object`: one group per canonical joint
  with a small sphere and a colored local-axis cross."
  [skeleton]
  (let [root (three/Group.)
        sphere-geometry (three/SphereGeometry. 0.007 6 4)
        sphere-material (three/MeshBasicMaterial.
                         (clj->js {:color 0xffaa33}))
        line-geometry (axis-geometry)
        line-material (three/LineBasicMaterial.
                       (clj->js {:vertexColors true}))
        joints (reduce
                (fn [groups {:keys [id]}]
                  (let [group (three/Group.)]
                    (.add group (three/Mesh. sphere-geometry sphere-material))
                    (.add group (three/LineSegments. line-geometry line-material))
                    (.add root group)
                    (assoc groups id group)))
                {}
                (sk/bones skeleton))
        update! (fn [transforms _pose]
                  (doseq [[joint group] joints]
                    (when-let [{:keys [position rotation]} (get transforms joint)]
                      (place! group position rotation))))]
    {:object3d root
     :joints joints
     :!update update!
     :dispose (fn []
                (.dispose sphere-geometry)
                (.dispose sphere-material)
                (.dispose line-geometry)
                (.dispose line-material))}))

;; ---------------------------------------------------------------------------
;; Body objects

(defn- procedural-skin-object [skeleton descriptor]
  (when (skin/procedural? descriptor)
    (build-skin-object skeleton descriptor)))

(defn create-body
  "Create a renderable body object from a skeleton and a skin descriptor.

  Options: `:debug?` adds joint markers; `:rig` supplies an already bound rig
  (from `load-rig!` or `bind-rig-object`) for a `:rigged` skin.

  Returns `{:object3d :!update :transforms :pose :attach! :dispose}`.
  `:!update` takes a pose, evaluates it and updates whatever skin is attached;
  `:attach!` swaps in a rig once it has loaded (D19)."
  ([skeleton descriptor]
   (create-body skeleton descriptor nil))
  ([skeleton descriptor {:keys [debug? rig]}]
   (let [skin-object (or rig (procedural-skin-object skeleton descriptor))
         debug-object (when debug? (build-debug-object skeleton))
         root (three/Group.)
         skin-state (atom skin-object)
         state (atom nil)
         attach! (fn [rig-object]
                   (when-let [current @skin-state]
                     (.remove root (:object3d current)))
                   (reset! skin-state rig-object)
                   (.add root (:object3d rig-object))
                   rig-object)]
     (when skin-object
       (.add root (:object3d skin-object)))
     (when debug-object
       (.add root (:object3d debug-object)))
     (let [body
           {:object3d root
            :skin-state skin-state
            :debug-object debug-object
            :descriptor descriptor
            :skeleton skeleton
            :!update (fn [pose]
                       (let [transforms (sk/evaluate skeleton pose)]
                         (reset! state {:pose pose :transforms transforms})
                         (when-let [skin-object @skin-state]
                           ((:!update skin-object) transforms pose))
                         (when debug-object
                           ((:!update debug-object) transforms pose))
                         transforms))
            :attach! attach!
            :transforms (fn [] (:transforms @state))
            :pose (fn [] (:pose @state))
            :dispose (fn []
                       (when-let [skin-object @skin-state]
                         ((:dispose skin-object)))
                       (when debug-object
                         ((:dispose debug-object))))}]
       body))))

(defn update!
  "Evaluate `pose` and push the result into a body object."
  [body pose]
  ((:!update body) pose))

(defn attach-rig!
  "Attach a bound rigged object's root to the body's renderable group."
  [body ^js rig-object]
  (.add ^js (:object3d body) (:object3d rig-object))
  (assoc body :skin-object rig-object))

(defn dispose!
  "Release the GPU resources a body owns."
  [body]
  ((:dispose body)))