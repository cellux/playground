(ns rb.explores.demo-effects.copperbars)

(def bar-count 50)
(defonce app-state (atom nil))

(defn- resize!
  [{:keys [canvas ctx]}]
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)
        pixel-ratio (min 2 (.-devicePixelRatio js/window))]
    (set! (.-width canvas) (js/Math.round (* width pixel-ratio)))
    (set! (.-height canvas) (js/Math.round (* height pixel-ratio)))
    (set! (.. canvas -style -width) (str width "px"))
    (set! (.. canvas -style -height) (str height "px"))
    (.setTransform ^js ctx pixel-ratio 0 0 pixel-ratio 0 0)
    (swap! app-state assoc
           :height height
           :pixel-ratio pixel-ratio
           :width width)))

(defn- copper-color
  [index time lightness]
  (let [hue (+ 12 (* 12 (js/Math.sin (+ (* index 0.37)
                                        (* time 0.00015)))))
        saturation (+ 68 (* 20 (js/Math.sin (+ (* index 0.21) 1))))]
    (str "hsl(" hue ", " saturation "%, " lightness "%)")))

(defn- draw-background!
  [^js ctx width height]
  (let [gradient (.createLinearGradient ctx 0 0 0 height)]
    (.addColorStop gradient 0 "#09070b")
    (.addColorStop gradient 0.5 "#211019")
    (.addColorStop gradient 1 "#050509")
    (set! (.-fillStyle ctx) gradient)
    (.fillRect ctx 0 0 width height)))

(defn- draw-bars!
  [^js ctx width height time]
  (let [spacing (/ (* width 0.84) (dec bar-count))
        start-x (* width 0.08)
        amplitude (min (* height 0.24) 180)]
    ;; Draw the bars back-to-front. Their horizontal overlap makes the wave
    ;; read as one layered, snake-like copper ribbon.
    (doseq [index (reverse (range bar-count))]
      (let [phase (+ (* index 0.27) (* time 0.0012))
            bar-width (max 5 (* spacing 1.75))
            bar-height (* height (+ 0.34 (* 0.16 (js/Math.sin (+ phase 1.1)))))
            center-y (+ (* height 0.5) (* amplitude (js/Math.sin phase)))
            x (- (+ start-x (* index spacing)) (* 0.5 bar-width))
            y (- center-y (* 0.5 bar-height))
            gradient (.createLinearGradient ctx x y (+ x bar-width) y)]
        (.addColorStop gradient 0 (copper-color index time 18))
        (.addColorStop gradient 0.18 (copper-color index time 46))
        (.addColorStop gradient 0.42 (copper-color index time 78))
        (.addColorStop gradient 0.55 (copper-color index time 96))
        (.addColorStop gradient 0.76 (copper-color index time 52))
        (.addColorStop gradient 1 (copper-color index time 16))
        (set! (.-fillStyle ctx) gradient)
        (set! (.-shadowColor ctx) "rgba(255, 116, 38, 0.3)")
        (set! (.-shadowBlur ctx) 12)
        (.fillRect ctx x y bar-width bar-height)
        (set! (.-shadowBlur ctx) 0)
        (set! (.-fillStyle ctx) (copper-color index time 100))
        (.fillRect ctx
                   (+ x (* bar-width 0.46))
                   (+ y (* bar-height 0.03))
                   (max 1 (* bar-width 0.07))
                   (* bar-height 0.94))))))

(defn draw-effect!
  [{:keys [ctx width height]} time]
  (draw-background! ctx width height)
  (draw-bars! ctx width height time))

(defn- animate!
  [time]
  (when-let [state @app-state]
    (draw-effect! state time)
    (swap! app-state assoc :frame (js/requestAnimationFrame animate!))))

(defn init
  []
  (when-let [{:keys [frame]} @app-state]
    (js/cancelAnimationFrame frame))
  (set! (.-innerHTML (.-body js/document)) "")
  (set! (.. js/document -body -style -margin) "0")
  (set! (.. js/document -body -style -width) "100vw")
  (set! (.. js/document -body -style -height) "100vh")
  (set! (.. js/document -body -style -overflow) "hidden")
  (let [canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")
        state {:canvas canvas
               :ctx ctx
               :frame nil
               :height 0
               :pixel-ratio 1
               :width 0}]
    (set! (.. canvas -style -display) "block")
    (.appendChild (.-body js/document) canvas)
    (reset! app-state state)
    (resize! state)
    (.addEventListener js/window "resize" #(resize! state))
    (swap! app-state assoc :frame (js/requestAnimationFrame animate!))))
