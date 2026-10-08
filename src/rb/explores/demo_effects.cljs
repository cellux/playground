(ns rb.explores.demo-effects
  (:require [rb.explores.demo-effects.copperbars :as copperbars]
            [rb.explores.demo-effects.neon-terrain :as neon-terrain]
            [rb.explores.demo-effects.rad-tunnel :as rad-tunnel]
            [rb.explores.demo-effects.starfield :as starfield]))

(def effects [:copperbars :starfield :rad-tunnel :neon-terrain])
(def effect-labels {:copperbars "COPPER BARS"
                    :starfield "STARFIELD"
                    :rad-tunnel "RADIAL TUNNEL"
                    :neon-terrain "VHS TERRAIN"})
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

(defn- draw-label!
  [^js ctx width height effect-index]
  (let [effect (nth effects effect-index)]
    (set! (.-font ctx) "600 13px system-ui, sans-serif")
    (set! (.-textAlign ctx) "center")
    (set! (.-fillStyle ctx) "rgba(255, 255, 255, 0.72)")
    (.fillText ctx
               (str (effect-labels effect) "  ·  ←/→ SWITCH")
               (* width 0.5)
               (- height 24))))

(defn- draw!
  [{:keys [canvas effect-index] :as state} time]
  (let [effect (nth effects effect-index)]
    (set! (.. canvas -style -filter)
          (if (= :neon-terrain effect)
            "contrast(1.18) saturate(1.35)"
            "none"))
    (case effect
      :copperbars (copperbars/draw-effect! state time)
      :starfield (starfield/draw-effect! state)
      :rad-tunnel (rad-tunnel/draw-effect! state time)
      :neon-terrain (neon-terrain/draw-effect! state time)))
  (draw-label! (:ctx state) (:width state) (:height state) effect-index))

(defn- switch-effect!
  [amount]
  (swap! app-state update :effect-index #(mod (+ % amount) (count effects))))

(defn- handle-keydown!
  [event]
  (case (.-key event)
    "ArrowLeft" (do (.preventDefault event) (switch-effect! -1))
    "ArrowRight" (do (.preventDefault event) (switch-effect! 1))
    nil))

(defn- animate!
  [time]
  (when-let [state @app-state]
    (let [elapsed (if-let [last-time (:last-time state)]
                    (min 50 (- time last-time))
                    16.7)
          next-state (-> state
                         (assoc :last-time time)
                         (update :stars starfield/advance-stars elapsed))]
      (reset! app-state next-state)
      (draw! next-state time)
      (swap! app-state assoc :frame (js/requestAnimationFrame animate!)))))

(defn init
  []
  (when-let [{:keys [frame key-handler resize-handler]} @app-state]
    (js/cancelAnimationFrame frame)
    (when key-handler
      (.removeEventListener js/window "keydown" key-handler))
    (when resize-handler
      (.removeEventListener js/window "resize" resize-handler)))
  (set! (.-innerHTML (.-body js/document)) "")
  (set! (.. js/document -body -style -margin) "0")
  (set! (.. js/document -body -style -width) "100vw")
  (set! (.. js/document -body -style -height) "100vh")
  (set! (.. js/document -body -style -overflow) "hidden")
  (let [canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")
        key-handler handle-keydown!
        resize-handler #(resize! @app-state)
        state {:canvas canvas
               :ctx ctx
               :effect-index 0
               :frame nil
               :height 0
               :key-handler key-handler
               :last-time nil
               :pixel-ratio 1
               :resize-handler resize-handler
               :stars (starfield/make-stars)
               :width 0}]
    (set! (.. canvas -style -display) "block")
    (.appendChild (.-body js/document) canvas)
    (reset! app-state state)
    (resize! state)
    (.addEventListener js/window "keydown" key-handler)
    (.addEventListener js/window "resize" resize-handler)
    (swap! app-state assoc :frame (js/requestAnimationFrame animate!))))
