(ns omkamra.body.examples
  "The v1 examples application.

  Everything yoga-specific lives here, in example data: the library itself
  knows only joints, controls, targets and clips. The page demonstrates loading
  the default skin, orbiting, direct posing, semantic controls, multi-limb
  intent resolution, clip playback and debug visuals."
  (:require [omkamra.body :as body]
            [omkamra.body.animation :as animation]
            [omkamra.body.intent :as intent]))

(def skeleton body/canonical-skeleton)

;; ---------------------------------------------------------------------------
;; Example poses: yoga-like, but ordinary data

(defn mountain
  "Standing, arms at the sides, feet together."
  []
  (body/pose-of {:spine-flex -0.02
                 :left-hip-abduct 0.02
                 :right-hip-abduct 0.02
                 :left-hip-rotate 0.08
                 :right-hip-rotate 0.08}))

(defn warrior-two
  "Wide stance, front knee bent, arms extended sideways, gaze to the front."
  []
  (body/pose-of {:left-hip-abduct 0.55
                 :right-hip-abduct 0.55
                 :left-hip-rotate 0.35
                 :right-hip-rotate -0.15
                 :left-knee-flex 0.55
                 :right-knee-flex 0.08
                 :left-shoulder-abduct 0.62
                 :right-shoulder-abduct 0.62
                 :left-elbow-flex 0.05
                 :right-elbow-flex 0.05
                 :spine-twist -0.12
                 :neck-turn 0.45
                 :left-toes-curl 0.15
                 :right-toes-curl 0.15}))

(defn tree
  "One leg lifted and turned out, arms overhead."
  []
  (body/pose-of {:left-knee-flex 0.05
                 :right-hip-abduct 0.85
                 :right-hip-flex 0.30
                 :right-hip-rotate 0.35
                 :right-knee-flex 1.15
                 :right-toes-curl 0.4
                 :left-shoulder-flex 0.95
                 :right-shoulder-flex 0.95
                 :left-elbow-flex 0.18
                 :right-elbow-flex 0.18
                 :left-shoulder-abduct 0.12
                 :right-shoulder-abduct 0.12
                 :spine-flex -0.05
                 :neck-turn 0.15}))

(defn forward-fold
  "Hinge at the hips, spine long, arms hanging."
  []
  (body/pose-of {:left-hip-flex 0.9
                 :right-hip-flex 0.9
                 :spine-flex 0.55
                 :neck-flex 0.2
                 :left-knee-flex 0.1
                 :right-knee-flex 0.1
                 :left-shoulder-flex 0.35
                 :right-shoulder-flex 0.35
                 :left-fingers-curl 0.1
                 :right-fingers-curl 0.1}))

(defn toe-curl
  "Standing with strongly curled toes, the detail the canonical skin must keep."
  []
  (body/pose-of {:left-toes-curl 1.0
                 :right-toes-curl 1.0
                 :left-ankle-flex 0.15
                 :right-ankle-flex 0.15
                 :spine-flex -0.05
                 :left-hip-abduct 0.02
                 :right-hip-abduct 0.02}))

(defn hands-and-feet
  "A four-limb intent: both hands and both feet placed by inverse kinematics,
  with the root lifting and shifting to make the pose reachable."
  []
  (:pose
   (intent/resolve skeleton
                   (intent/intent
                    {:controls {:spine-flex 0.2}
                     :targets {:left-arm {:position [0.32 0.75 0.42]}
                               :right-arm {:position [-0.32 0.75 0.42]}
                               :left-leg {:position [0.14 0.06 -0.30]}
                               :right-leg {:position [-0.14 0.06 -0.30]}}
                     :directions {:left-elbow [0.5 0 0.6]
                                  :right-elbow [-0.5 0 0.6]
                                  :left-knee [0.2 0 -1]
                                  :right-knee [-0.2 0 -1]}
                     :mode :best-effort}))))

(def poses
  [{:id :mountain :label "Mountain" :build mountain}
   {:id :warrior-two :label "Warrior II" :build warrior-two}
   {:id :tree :label "Tree" :build tree}
   {:id :forward-fold :label "Forward fold" :build forward-fold}
   {:id :toe-curl :label "Toe curl" :build toe-curl}
   {:id :hands-and-feet :label "Hands + feet (IK)" :build hands-and-feet}])

(defn demo-clip
  "A short transition that exercises interpolation and IK-solved poses."
  []
  (animation/transitions [(mountain) (warrior-two) (tree) (mountain)]
                         6000
                         {:loop? true :easing :smoothstep}))

;; ---------------------------------------------------------------------------
;; Page

(defn- style! [element styles]
  (doseq [[property value] styles]
    (aset (.-style element) (name property) value))
  element)

(defn- element [tag attributes children]
  (let [node (.createElement js/document tag)]
    (doseq [[attribute value] attributes]
      (if (= attribute :style)
        (style! node value)
        (.setAttribute node (name attribute) value)))
    (doseq [child children]
      (.appendChild node (if (string? child)
                           (.createTextNode js/document child)
                           child)))
    node))

(defn- button [label on-click]
  (let [node (element "button"
                      {:type "button"
                       :style {:font "inherit"
                               :padding "6px 10px"
                               :margin "0 4px 4px 0"
                               :borderRadius "999px"
                               :border "1px solid rgba(255,255,255,0.25)"
                               :background "rgba(255,255,255,0.08)"
                               :color "#e9eef5"
                               :cursor "pointer"}}
                      [label])]
    (.addEventListener node "click" on-click)
    node))

(defn- status-line [text]
  (element "div"
           {:style {:marginTop "8px"
                    :font "12px/1.5 ui-monospace, SFMono-Regular, monospace"
                    :color "#9fb0c4"
                    :whiteSpace "pre-wrap"}}
           [text]))

(defn- show-status! [node text]
  (set! (.-textContent node) text))

(defonce state (atom nil))

(defn- mount-viewer!
  "Mount (or remount) the viewer in `container`, keeping the current pose."
  [container options]
  (when-let [previous (:viewer @state)]
    ((:dispose! previous)))
  (let [viewer (body/mount! container options)]
    (reset! state (assoc @state :viewer viewer))
    viewer))

(defn init
  "Browser entry point (Shadow `:init-fn`)."
  []
  (let [root (or (.getElementById js/document "app")
                 (let [node (element "div"
                                     {:id "app"
                                      :style {:maxWidth "980px"
                                              :margin "0 auto"
                                              :padding "24px 18px 48px"
                                              :color "#e9eef5"
                                              :font "15px/1.5 system-ui, sans-serif"
                                              :background "#0b0f15"
                                              :minHeight "100vh"}}
                                     [])]
                   (.appendChild (.-body js/document) node)
                   node))
        container (element "div"
                           {:id "stage"
                            :style {:position "relative"
                                    :width "100%"
                                    :height "62vh"
                                    :minHeight "360px"
                                    :background "#11151c"
                                    :borderRadius "14px"
                                    :overflow "hidden"
                                    :touchAction "none"
                                    :touchUserSelect "none"}}
                           [])
        status (status-line "")
        pose-buttons (element "div" {:style {:marginTop "12px"}} [])
        hint (element "p"
                      {:style {:margin "10px 0 0"
                               :color "#7f8ea1"
                               :font "13px/1.5 system-ui, sans-serif"}}
                      ["Drag to orbit, scroll or pinch to zoom. Poses are ordinary data; transitions are clips; the last one is solved by multi-limb IK. The library itself contains no yoga."])
        debug-node (element "label"
                            {:style {:display "inline-flex"
                                     :gap "6px"
                                     :alignItems "center"
                                     :marginLeft "12px"
                                     :color "#c7d3e0"
                                     :font "13px system-ui, sans-serif"}}
                            [(element "input" {:type "checkbox"} [])
                             "debug joints"])
        debug-checkbox (.-firstChild debug-node)]
    (.appendChild root container)
    (.appendChild root pose-buttons)
    (.appendChild root hint)
    (.appendChild root status)
    (.appendChild pose-buttons
                  (button "Play sequence"
                          (fn []
                            (body/play! (:viewer @state) (demo-clip))
                            (show-status! status "playing: Mountain -> Warrior II -> Tree -> Mountain")))
                  (button "Pause" (fn [] ((:pause! (:viewer @state)))))
                  (button "Resume"
                          (fn []
                            (let [viewer (:viewer @state)]
                              (when (nil? @(:clip viewer))
                                (body/play! viewer (demo-clip)))
                              ((:play! viewer)))))
                  (button "0.5x" (fn [] ((:set-rate! (:viewer @state)) 0.5)))
                  (button "1x" (fn [] ((:set-rate! (:viewer @state)) 1.0)))
                  (button "2x" (fn [] ((:set-rate! (:viewer @state)) 2.0))))
    (.appendChild pose-buttons debug-node)
    (doseq [{:keys [label build]} poses]
      (.appendChild pose-buttons
                    (button label
                            (fn []
                              (let [viewer (:viewer @state)]
                                (body/show! viewer (build))
                                (show-status! status (str "pose: " label)))))))
    (let [mount! (fn [options]
                   (mount-viewer!
                    container
                    (merge {:debug? false
                            :on-skin-loaded
                            (fn [_]
                              (show-status! status "detailed MakeHuman skin ready"))
                            :on-skin-error
                            (fn [error]
                              (show-status! status
                                            (str "detailed skin failed, using the mannequin: " error)))}
                           options)))
          viewer (mount! {})]
      (.addEventListener debug-checkbox "change"
                         (fn [_]
                           (let [enabled (.-checked debug-checkbox)
                                 current ((:pose! (:viewer @state)))]
                             (mount! {:debug? enabled})
                             (body/show! (:viewer @state) current))))
      (body/show! viewer (mountain))
      (show-status! status "ready: mannequin first, detailed skin loading")
      (body/blink!))))
