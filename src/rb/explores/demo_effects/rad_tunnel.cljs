(ns rb.explores.demo-effects.rad-tunnel)

(def ring-count 42)
(def spoke-count 18)
(def tempo-bpm 174)
(def beat-ms (/ 60000 tempo-bpm))
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
    (swap! app-state assoc :height height :width width)))

(defn- hsl
  [hue saturation lightness alpha]
  (str "hsla(" hue "," saturation "%," lightness "%," alpha ")"))

(defn- draw-background!
  [^js ctx width height pulse]
  (let [center-x (* width 0.5)
        center-y (* height 0.5)
        radius (* 0.7 (max width height))
        gradient (.createRadialGradient ctx center-x center-y 0
                                        center-x center-y radius)]
    (.addColorStop gradient 0 (hsl (+ 278 (* pulse 18)) 72 (+ 16 (* pulse 10)) 1))
    (.addColorStop gradient 0.45 "#09051b")
    (.addColorStop gradient 1 "#010107")
    (set! (.-fillStyle ctx) gradient)
    (.fillRect ctx 0 0 width height)))

(defn- draw-spokes!
  [^js ctx center-x center-y radius time pulse]
  (set! (.-lineWidth ctx) (+ 0.5 (* pulse 1.4)))
  (set! (.-strokeStyle ctx) (hsl 190 90 62 (+ 0.12 (* pulse 0.2))))
  (doseq [index (range spoke-count)]
    (let [angle (+ (* index (/ (* 2 js/Math.PI) spoke-count))
                   (* time 0.0008))
          end-x (+ center-x (* radius (js/Math.cos angle)))
          end-y (+ center-y (* radius (js/Math.sin angle)))]
      (.beginPath ctx)
      (.moveTo ctx center-x center-y)
      (.lineTo ctx end-x end-y)
      (.stroke ctx))))

(defn- draw-ring!
  [^js ctx center-x center-y radius index time pulse]
  (let [sides 12
        wobble (* radius 0.025)
        rotation (+ (* time 0.00045)
                    (* index 0.08)
                    (* pulse 0.08))]
    (.beginPath ctx)
    (doseq [side (range sides)]
      (let [angle (+ rotation (* side (/ (* 2 js/Math.PI) sides)))
            local-radius (+ radius
                            (* wobble
                               (js/Math.sin (+ (* side 1.7)
                                               (* time 0.002)
                                               (* index 0.4)))))
            x (+ center-x (* local-radius (js/Math.cos angle)))
            y (+ center-y (* local-radius (js/Math.sin angle)))]
        (if (zero? side)
          (.moveTo ctx x y)
          (.lineTo ctx x y))))
    (.closePath ctx)
    (set! (.-lineWidth ctx) (max 1.0 (* 0.7 (/ 1 radius) 220)))
    (set! (.-strokeStyle ctx)
          (hsl (+ 185 (* 115 (mod (+ (* index 0.11) (* time 0.00012)) 1)))
               94
               (+ 49 (* pulse 25))
               (+ 0.34 (* pulse 0.42))))
    (set! (.-shadowColor ctx) (hsl 285 100 62 0.8))
    (set! (.-shadowBlur ctx) (+ 2 (* pulse 10)))
    (.stroke ctx)
    (set! (.-shadowBlur ctx) 0)))

(defn- draw-beat-flash!
  [^js ctx center-x center-y radius pulse]
  (when (> pulse 0.72)
    (let [gradient (.createRadialGradient ctx center-x center-y 0
                                          center-x center-y radius)]
      (.addColorStop gradient 0 (hsl 320 100 80 (* (- pulse 0.72) 0.22)))
      (.addColorStop gradient 1 (hsl 320 100 60 0))
      (set! (.-fillStyle ctx) gradient)
      (.fillRect ctx (- center-x radius) (- center-y radius)
                 (* 2 radius) (* 2 radius)))))

(defn draw-effect!
  [{:keys [ctx width height]} time]
  (let [center-x (* width 0.5)
        center-y (* height 0.5)
        pulse (max 0 (js/Math.sin (* 2 js/Math.PI (/ time beat-ms))))
        tunnel-depth (* 0.5 (min width height))]
    (draw-background! ctx width height pulse)
    (draw-spokes! ctx center-x center-y tunnel-depth time pulse)
    (doseq [index (reverse (range ring-count))]
      (let [z (+ 0.08 (* (mod (+ (* index (/ 1 ring-count))
                                 (- (* time 0.00032)))
                              1)
                         0.92))
            radius (* tunnel-depth (/ 0.12 z))]
        (when (< radius (* 1.5 (max width height)))
          (draw-ring! ctx center-x center-y radius index time pulse))))
    (draw-beat-flash! ctx center-x center-y (* 0.42 (max width height)) pulse)))

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
               :width 0}]
    (set! (.. canvas -style -display) "block")
    (.appendChild (.-body js/document) canvas)
    (reset! app-state state)
    (resize! state)
    (.addEventListener js/window "resize" #(resize! state))
    (swap! app-state assoc :frame (js/requestAnimationFrame animate!))))
