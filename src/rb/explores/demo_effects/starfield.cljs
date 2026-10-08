(ns rb.explores.demo-effects.starfield)

(def star-count 500)
(defonce app-state (atom nil))

(defn- random-star
  []
  {:brightness (+ 0.45 (* (rand) 0.55))
   :size (+ 0.5 (* (rand) 1.5))
   :x (- (* 2 (rand)) 1)
   :y (- (* 2 (rand)) 1)
   :z (+ 0.08 (* (rand) 0.92))})

(defn- reset-star
  []
  (random-star))

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

(defn advance-stars
  [stars elapsed]
  (mapv (fn [{:keys [z] :as star}]
          (let [next-z (- z (* elapsed 0.00032))]
            (if (< next-z 0.04)
              (reset-star)
              (assoc star :z next-z))))
        stars))

(defn- draw-background!
  [^js ctx width height]
  (let [gradient (.createRadialGradient ctx
                                        (* width 0.5)
                                        (* height 0.5)
                                        0
                                        (* width 0.5)
                                        (* height 0.5)
                                        (* 0.72 (max width height)))]
    (.addColorStop gradient 0 "#101c36")
    (.addColorStop gradient 0.45 "#050b1a")
    (.addColorStop gradient 1 "#010207")
    (set! (.-fillStyle ctx) gradient)
    (.fillRect ctx 0 0 width height)))

(defn- draw-stars!
  [^js ctx width height stars]
  (let [center-x (* width 0.5)
        center-y (* height 0.5)
        focal (* 0.58 (min width height))]
    (doseq [{:keys [brightness size x y z]} (sort-by :z > stars)]
      (let [screen-x (+ center-x (* (/ x z) focal))
            screen-y (+ center-y (* (/ y z) focal))]
        (when (and (> screen-x -12)
                   (< screen-x (+ width 12))
                   (> screen-y -12)
                   (< screen-y (+ height 12)))
          (let [radius (min 5 (max 0.45 (* size (/ 0.08 z))))
                alpha (* brightness (min 1.0 (* 1.6 (- 1 z))))]
            (set! (.-fillStyle ctx)
                  (str "rgba(190, 220, 255, " alpha ")"))
            (.beginPath ctx)
            (.arc ctx screen-x screen-y radius 0 (* 2 js/Math.PI))
            (.fill ctx)))))))

(defn draw-effect!
  [{:keys [ctx width height stars]}]
  (draw-background! ctx width height)
  (draw-stars! ctx width height stars))

(defn make-stars
  []
  (vec (repeatedly star-count random-star)))

(defn- animate!
  [time]
  (when-let [state @app-state]
    (let [elapsed (if-let [last-time (:last-time state)]
                    (min 50 (- time last-time))
                    16.7)
          next-state (-> state
                         (assoc :last-time time)
                         (update :stars advance-stars elapsed))]
      (reset! app-state next-state)
      (draw-effect! next-state)
      (swap! app-state assoc :frame (js/requestAnimationFrame animate!)))))

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
               :last-time nil
               :pixel-ratio 1
               :stars (vec (repeatedly star-count random-star))
               :width 0}]
    (set! (.. canvas -style -display) "block")
    (.appendChild (.-body js/document) canvas)
    (reset! app-state state)
    (resize! state)
    (.addEventListener js/window "resize" #(resize! state))
    (swap! app-state assoc :frame (js/requestAnimationFrame animate!))))
