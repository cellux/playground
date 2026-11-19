(ns omkamra.body.viewer
  "A self-contained Three.js viewer plus the lower-level embedding path.

  `mount!` owns everything a page needs: renderer, scene, camera, lights,
  orbit input (mouse, touch, wheel) and the animation loop. It returns a
  viewer handle whose functions are the public API.

  Applications that already own a renderer, scene or loop do not use
  `mount!`; they build the body with `omkamra.body.three/create-body`, add
  `(:object3d body)` to their own scene, and call
  `omkamra.body.three/update!` with a pose from their own loop or a sampled
  clip. Both paths update the same render-independent body state, so a pose
  behaves identically in either.

  The viewer is intentionally framework-agnostic: it takes a DOM element and
  plain options, and never assumes React, Reagent or a particular page
  structure."
  (:require ["three" :as three]
            [omkamra.body.animation :as animation]
            [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]
            [omkamra.body.three :as body-three]))

(defn- aspect [element]
  (let [width (max (.-clientWidth element) 1)
        height (max (.-clientHeight element) 1)]
    (/ width height)))

(defn- make-renderer [element]
  (let [renderer (three/WebGLRenderer.
                  (clj->js {:antialias true :alpha false}))
        canvas (.-domElement renderer)]
    (set! (.-position (.-style canvas)) "absolute")
    (set! (.-left (.-style canvas)) "0")
    (set! (.-top (.-style canvas)) "0")
    (set! (.-width (.-style canvas)) "100%")
    (set! (.-height (.-style canvas)) "100%")
    (set! (.-display (.-style canvas)) "block")
    (set! (.-touchAction (.-style canvas)) "none")
    (.setPixelRatio renderer (min (or js/window.devicePixelRatio 1.0) 2.0))
    (.setSize renderer (max (.-clientWidth element) 1) (max (.-clientHeight element) 1) false)
    (.appendChild element canvas)
    renderer))

(defn- make-camera [element {:keys [fov]}]
  (let [camera (three/PerspectiveCamera.
                (or fov 45.0) (aspect element) 0.02 100.0)]
    camera))

(defn- make-lights []
  (let [hemisphere (three/HemisphereLight. 0xffffff 0x2a2f3a 0.85)
        key (three/DirectionalLight. 0xffffff 1.35)
        fill (three/DirectionalLight. 0xbcd0ff 0.45)]
    (.set (.-position key) 1.6 3.2 2.4)
    (.set (.-position fill) -2.2 1.4 -1.8)
    [hemisphere key fill]))

(defn- add-floor [scene]
  (let [geometry (three/CircleGeometry. 1.6 48)
        material (three/MeshStandardMaterial.
                  (clj->js {:color 0x2b303a :roughness 1.0 :metalness 0.0}))
        floor (three/Mesh. geometry material)]
    (.set (.-rotation floor) (- (/ m/pi 2.0)) 0.0 0.0)
    (set! (.. floor -position -y) 0.001)
    (.add scene floor)
    floor))

(defn- apply-orbit!
  "Position the camera on the orbit sphere looking at the target."
  [^js camera {:keys [yaw pitch distance target]}]
  (let [pitch (m/clamp pitch -1.45 1.45)
        cos-pitch (m/cos pitch)
        [tx ty tz] target
        x (+ tx (* distance cos-pitch (m/sin yaw)))
        y (+ ty (* distance (m/sin pitch)))
        z (+ tz (* distance cos-pitch (m/cos yaw)))]
    (.set (.-position camera) x y z)
    (.lookAt camera (three/Vector3. tx ty tz))))

(defn- install-orbit-input!
  "Pointer, wheel and touch orbit control written against the DOM only, so the
  viewer has no dependency on Three.js example modules."
  [element state! on-change!]
  (let [pointers (atom {})
        last (atom nil)
        pinch-distance (atom nil)
        pointer-count (fn [] (count @pointers))
        begin! (fn [event]
                 (let [id (.-pointerId event)]
                   (swap! pointers assoc id {:x (.-clientX event)
                                             :y (.-clientY event)})
                   (when (= 1 (pointer-count))
                     (reset! last {:x (.-clientX event) :y (.-clientY event)}))
                   (when (= 2 (pointer-count))
                     (let [[a b] (vals @pointers)]
                       (reset! pinch-distance
                               (js/Math.hypot (- (:x a) (:x b))
                                              (- (:y a) (:y b))))))))
        move! (fn [event]
                (let [id (.-pointerId event)]
                  (when (contains? @pointers id)
                    (swap! pointers assoc id {:x (.-clientX event)
                                              :y (.-clientY event)})
                    (cond
                      (= 2 (pointer-count))
                      (let [[a b] (vals @pointers)
                            distance (js/Math.hypot (- (:x a) (:x b))
                                                    (- (:y a) (:y b)))]
                        (when @pinch-distance
                          (let [factor (/ @pinch-distance (max distance 1.0))]
                            (swap! state! update :distance
                                   (fn [d] (m/clamp (* d factor) 0.6 8.0)))))
                        (reset! pinch-distance distance)
                        (on-change!))

                      (= 1 (pointer-count))
                      (when @last
                        (let [dx (- (.-clientX event) (:x @last))
                              dy (- (.-clientY event) (:y @last))]
                          (reset! last {:x (.-clientX event) :y (.-clientY event)})
                          (swap! state! (fn [orbit]
                                          (-> orbit
                                              (update :yaw - (* dx 0.008))
                                              (update :pitch + (* dy 0.008)))))
                          (on-change!)))))))
        end! (fn [event]
               (swap! pointers dissoc (.-pointerId event))
               (when (< (pointer-count) 2)
                 (reset! pinch-distance nil))
               (when (empty? @pointers)
                 (reset! last nil)))
        wheel! (fn [event]
                 (.preventDefault event)
                 (let [factor (if (pos? (.-deltaY event)) 1.08 0.925)]
                   (swap! state! update :distance
                          (fn [d] (m/clamp (* d factor) 0.6 8.0)))
                   (on-change!)))]
    (.addEventListener element "pointerdown" begin!)
    (.addEventListener element "pointermove" move!)
    (.addEventListener element "pointerup" end!)
    (.addEventListener element "pointercancel" end!)
    (.addEventListener element "wheel" wheel! (clj->js {:passive false}))
    {:dispose! (fn []
                 (.removeEventListener element "pointerdown" begin!)
                 (.removeEventListener element "pointermove" move!)
                 (.removeEventListener element "pointerup" end!)
                 (.removeEventListener element "pointercancel" end!)
                 (.removeEventListener element "wheel" wheel!))}))

(defn mount!
  "Mount a body viewer into `element`.

  Options:

  - `:skeleton` — defaults to the canonical skeleton;
  - `:skin` — a procedural skin descriptor, required;
  - `:pose` — initial pose, defaults to the identity pose;
  - `:clip` — an animation clip to control playback;
  - `:orbit` — `{:yaw :pitch :distance :target}`, defaults to a front view;
  - `:debug?` — show joint markers and axes;
  - `:background` — scene background color (default `0x11151c`);
  - `:on-frame` — `(fn [{:keys [delta time]}] ...)`, called after every render.

  Returns a viewer handle; every function is stable and can be stored. Showing
  a still pose with `:set-pose!` clears any active clip, so a selected pose is
  not immediately overwritten by the next playback frame; set a clip again to
  resume animation.

  Rendering is on demand (D16): the loop renders only after something changed
  — a new pose, a seek, an orbit gesture, a resize, or a playing clip — so a
  still body costs no GPU or CPU time."
  [element {:keys [skeleton skin rig pose clip orbit debug? background on-frame]}]
  (let [skeleton (or skeleton sk/canonical-skeleton)
        renderer (make-renderer element)
        canvas (.-domElement renderer)
        scene (three/Scene.)
        camera (make-camera element nil)
        lights (make-lights)
        floor (add-floor scene)
        body (body-three/create-body skeleton skin {:debug? debug? :rig rig})
        body-state (atom body)
        orbit-state (atom (merge {:yaw 0.0
                                  :pitch 0.12
                                  :distance 3.4
                                  :target [0.0 0.95 0.0]}
                                 orbit))
        current-pose (atom (or pose (pose/pose)))
        clip-state (atom clip)
        timeline (atom (when clip (animation/timeline clip)))
        last-frame (atom nil)
        running (atom true)
        dirty (atom true)
        loop-running (atom false)
        schedule! (atom (fn [] nil))
        clip-playing? (fn []
                        (boolean (when-let [clip @clip-state]
                                   (and clip (:playing? @timeline)))))
        time (atom 0.0)
        update-body! (fn [next-pose]
                       (body-three/update! @body-state next-pose))
        resize! (fn []
                  (let [width (max (.-clientWidth element) 1)
                        height (max (.-clientHeight element) 1)]
                    (.setSize renderer width height false)
                    (set! (.-aspect camera) (/ width height))
                    (.updateProjectionMatrix camera)))
        render! (fn [] (.render renderer scene camera))
        mark-dirty! (fn []
                      (reset! dirty true)
                      ((deref schedule!)))
        input (install-orbit-input! element orbit-state
                                    (fn []
                                      (apply-orbit! camera @orbit-state)
                                      (mark-dirty!)))]
    (set! (.-background scene) (three/Color. (or background 0x11151c)))
    (doseq [light lights]
      (.add scene light))
    (.add scene (:object3d body))
    (update-body! @current-pose)
    (apply-orbit! camera @orbit-state)
    (.addEventListener js/window "resize" resize!)
    (letfn [(request-frame! []
              (when (and @running (not @loop-running))
                (reset! loop-running true)
                (js/requestAnimationFrame loop!)))
            (frame! [timestamp]
              (when @running
                (let [delta (if @last-frame (- timestamp @last-frame) 0.0)]
                  (reset! last-frame timestamp)
                  (when-let [clip @clip-state]
                    (when (:playing? @timeline)
                      (let [result (animation/advance clip @timeline delta)]
                        (reset! timeline (if (:finished? result)
                                           (animation/pause (:timeline result))
                                           (:timeline result)))
                        (reset! time (:time result))
                        (reset! current-pose (:pose result))
                        (update-body! (:pose result))
                        (reset! dirty true))))
                  (when @dirty
                    (reset! dirty false)
                    (render!)
                    (when on-frame
                      (on-frame {:delta delta :time @time}))))))
            (loop! [timestamp]
              (reset! loop-running false)
              (frame! timestamp)
              ;; A playing clip needs the next frame even though the render
              ;; itself cleared the dirty flag.
              (when (and @running (or @dirty (clip-playing?)))
                (request-frame!)))]
      (reset! schedule! request-frame!)
      (request-frame!)
      (let [viewer
            {:element element
             :skeleton skeleton
             :scene scene
             :camera camera
             :renderer renderer
             :body body-state
             :orbit orbit-state
             :pose current-pose
             :clip clip-state
             :timeline timeline
             :time time
             :resize! (fn []
                        (resize!)
                        (mark-dirty!))
             :render! render!
             :frame! request-frame!
             :set-pose! (fn [next-pose]
                          ;; Showing a still pose takes over from any clip;
                          ;; otherwise the next playback frame would replace
                          ;; the pose immediately. Callers that want playback
                          ;; again set a clip with `:set-clip!`.
                          (reset! clip-state nil)
                          (reset! timeline nil)
                          (reset! time 0.0)
                          (reset! current-pose next-pose)
                          (update-body! next-pose)
                          (mark-dirty!))
             :pose! (fn [] @current-pose)
             :set-clip! (fn [next-clip]
                          (reset! clip-state next-clip)
                          (reset! timeline (when next-clip (animation/timeline next-clip)))
                          (reset! time 0.0)
                          (when next-clip
                            (let [first-pose (animation/sample next-clip 0.0)]
                              (reset! current-pose first-pose)
                              (update-body! first-pose)))
                          (mark-dirty!))
             :play! (fn []
                      (swap! timeline animation/play)
                      (reset! last-frame nil)
                      (mark-dirty!))
             :pause! (fn []
                       (swap! timeline animation/pause)
                       (mark-dirty!))
             :seek! (fn [time-ms]
                      (when-let [clip @clip-state]
                        (swap! timeline animation/seek clip time-ms)
                        (reset! time (double time-ms))
                        (let [sampled (animation/sample clip time-ms)]
                          (reset! current-pose sampled)
                          (update-body! sampled))
                        (mark-dirty!)))
             :set-rate! (fn [rate] (swap! timeline animation/set-rate rate))
             :set-orbit! (fn [next-orbit]
                           (swap! orbit-state merge next-orbit)
                           (apply-orbit! camera @orbit-state)
                           (mark-dirty!))
             :orbit! (fn [] @orbit-state)
             :set-skin! (fn [descriptor]
                          (let [next-body (body-three/create-body skeleton descriptor
                                                                  {:debug? debug?})]
                            (.remove scene (:object3d @body-state))
                            (body-three/dispose! @body-state)
                            (.add scene (:object3d next-body))
                            (reset! body-state next-body)
                            (update-body! @current-pose)
                            (mark-dirty!)
                            next-body))
             :set-rig! (fn [rig-object]
                         ;; Swap the renderable skin without touching the body
                         ;; state, pose or camera (D19).
                         ((:attach! @body-state) rig-object)
                         (update-body! @current-pose)
                         (mark-dirty!)
                         rig-object)
             :skin! (fn [] (:descriptor @body-state))
             :dispose! (fn []
                         (reset! running false)
                         (.removeEventListener js/window "resize" resize!)
                         ((:dispose! input))
                         (body-three/dispose! @body-state)
                         (.dispose (.-geometry floor))
                         (.dispose (.-material floor))
                         (.dispose renderer)
                         (when-let [parent (.-parentNode canvas)]
                           (.removeChild parent canvas)))}]
        ;; Exposed for browser tooling and agents as plain JavaScript; the
        ;; library itself never reads it. A ClojureScript map is not readable
        ;; with JavaScript property access, so the fields a check needs are
        ;; copied out explicitly.
        (set! (.-omkamraBody js/window)
              #js {"viewer" viewer
                   "scene" scene
                   "camera" camera
                   "renderer" renderer
                   "frameCount" (fn [] (.. renderer -info -render -frame))
                   "playing" (fn [] (clip-playing?))
                   "play" (fn [] ((:play! viewer)))
                   "pause" (fn [] ((:pause! viewer)))
                   "seek" (fn [time] ((:seek! viewer) time))
                   "pose" (fn [] (clj->js @current-pose))
                   "orbit" (fn [] (clj->js @orbit-state))})
        viewer))))

(defn unmount!
  "Dispose a viewer returned by `mount!`."
  [viewer]
  ((:dispose! viewer)))
