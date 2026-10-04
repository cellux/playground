(ns rb.explores.threejs.earth
  (:require ["three" :as three]))

(def earth-texture-url
  "https://threejs.org/examples/textures/planets/earth_atmos_2048.jpg")

(def earth-height-url
  "https://raw.githubusercontent.com/jeromeetienne/threex.planets/master/images/earthbump1k.jpg")

(defonce app-state (atom nil))

(defn- clamp
  [value lower upper]
  (max lower (min upper value)))

(defn- apply-rotation!
  [{:keys [globe rotation-x rotation-y]}]
  (set! (.-x (.-rotation globe)) rotation-x)
  (set! (.-y (.-rotation globe)) rotation-y))

(defn- resize!
  [{:keys [camera renderer]}]
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (.setSize renderer width height false)))

(defn- add-instructions!
  []
  (let [instructions (.createElement js/document "div")]
    (set! (.-textContent instructions)
          "Drag to rotate the Earth · release to resume its slow rotation")
    (let [style (.-style instructions)]
      (set! (.-position style) "fixed")
      (set! (.-left style) "50%")
      (set! (.-bottom style) "24px")
      (set! (.-transform style) "translateX(-50%)")
      (set! (.-padding style) "10px 14px")
      (set! (.-borderRadius style) "999px")
      (set! (.-background style) "rgba(0, 0, 0, 0.55)")
      (set! (.-color style) "rgba(255, 255, 255, 0.9)")
      (set! (.-fontFamily style) "system-ui, sans-serif")
      (set! (.-fontSize style) "14px")
      (set! (.-pointerEvents style) "none")
      (set! (.-userSelect style) "none"))
    (.appendChild (.-body js/document) instructions)))

(defn- install-drag-controls!
  [{:keys [canvas]}]
  (set! (.. canvas -style -touchAction) "none")
  (.addEventListener
   canvas
   "pointerdown"
   (fn [event]
     (.setPointerCapture canvas (.-pointerId event))
     (swap! app-state assoc
            :dragging true
            :last-x (.-clientX event)
            :last-y (.-clientY event))))
  (.addEventListener
   canvas
   "pointermove"
   (fn [event]
     (when (:dragging @app-state)
       (let [dx (- (.-clientX event) (:last-x @app-state))
             dy (- (.-clientY event) (:last-y @app-state))
             next-state
             (swap! app-state
                    (fn [state]
                      (-> state
                          (assoc :last-x (.-clientX event)
                                 :last-y (.-clientY event))
                          (update :rotation-x #(clamp (+ % (* dy 0.01))
                                                      (- (/ js/Math.PI 2))
                                                      (/ js/Math.PI 2)))
                          (update :rotation-y + (* dx 0.01)))))]
         (apply-rotation! next-state)))))
  (.addEventListener
   canvas
   "pointerup"
   (fn [_event]
     (swap! app-state assoc :dragging false)))
  (.addEventListener
   canvas
   "pointercancel"
   (fn [_event]
     (swap! app-state assoc :dragging false))))

(defn- animate!
  [time]
  (when-let [state @app-state]
    (when (:active? state)
      (let [elapsed (if-let [last-time (:last-time state)]
                      (- time last-time)
                      0)
            next-state
            (swap! app-state
                   (fn [state]
                     (cond-> (assoc state :last-time time)
                       (not (:dragging state))
                       (update :rotation-y + (* elapsed 0.00015)))))]
        (apply-rotation! next-state)
        (.render (:renderer next-state)
                 (:scene next-state)
                 (:camera next-state))
        (js/requestAnimationFrame animate!)))))

(defn init
  []
  (when @app-state
    (swap! app-state assoc :active? false))
  (set! (.-innerHTML (.-body js/document)) "")
  (set! (.. js/document -body -style -margin) "0")
  (set! (.. js/document -body -style -overflow) "hidden")
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)
        scene (three/Scene.)
        camera (three/PerspectiveCamera. 38 (/ width height) 0.1 100)
        renderer (three/WebGLRenderer. #js {:antialias true})
        geometry (three/SphereGeometry. 1 192 128)
        material (three/MeshPhongMaterial. #js {:color 0x7799bb
                                                :displacementBias 0
                                                :displacementScale 0.028
                                                :shininess 18})
        globe (three/Mesh. geometry material)
        ambient (three/AmbientLight. 0x9fb8d8 1.4)
        sunlight (three/DirectionalLight. 0xffffff 2.4)
        texture-loader (three/TextureLoader.)
        state {:active? true
               :camera camera
               :canvas (.-domElement renderer)
               :globe globe
               :last-time nil
               :renderer renderer
               :rotation-x 0
               :rotation-y 0
               :scene scene}]
    (set! (.-background scene) (three/Color. 0x02040a))
    (.setPixelRatio renderer (min 2 (.-devicePixelRatio js/window)))
    (.setSize renderer width height false)
    (.set (.-position camera) 0 0 3.1)
    (.set (.-position sunlight) 4 2 5)
    (.add scene ambient)
    (.add scene sunlight)
    (.add scene globe)
    (.appendChild (.-body js/document) (.-domElement renderer))
    (.load texture-loader
           earth-texture-url
           (fn [texture]
             (set! (.-map material) texture)
             (set! (.-needsUpdate material) true)))
    (.load texture-loader
           earth-height-url
           (fn [texture]
             (set! (.-bumpMap material) texture)
             (set! (.-bumpScale material) 0.012)
             (set! (.-displacementMap material) texture)
             (set! (.-needsUpdate material) true)))
    (reset! app-state state)
    (install-drag-controls! state)
    (.addEventListener js/window "resize" #(resize! state))
    (add-instructions!)
    (js/requestAnimationFrame animate!)))
