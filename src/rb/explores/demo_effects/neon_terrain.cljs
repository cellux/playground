(ns rb.explores.demo-effects.neon-terrain)

(def row-count 40)
(def column-count 34)
(def row-spacing 1.25)
(def near-distance 3.5)
(def camera-height 8.0)
(def camera-pitch (* 26 (/ js/Math.PI 180)))

(defn- hsl
  [hue saturation lightness alpha]
  (str "hsla(" hue "," saturation "%," lightness "%," alpha ")"))

(defn- terrain-height
  [x z time]
  (+ (* 2.1 (js/Math.sin (+ (* x 0.27)
                            (* z 0.11)
                            (* time 0.00012))))
     (* 1.15 (js/Math.sin (+ (* x 0.58)
                             (* z 0.23)
                             1.7)))
     (* 0.55 (js/Math.cos (+ (* x 1.2)
                             (* z 0.4))))))

(defn- project-point
  [x z height-value focal center-x center-y]
  ;; Transform the world into a camera pitched down toward the terrain. This
  ;; is a real perspective projection: nearby ground falls below the viewport
  ;; while the distance converges on a high horizon.
  (let [relative-y (- height-value camera-height)
        cos-pitch (js/Math.cos camera-pitch)
        sin-pitch (js/Math.sin camera-pitch)
        camera-y (+ (* relative-y cos-pitch) (* z sin-pitch))
        camera-z (+ (* (- relative-y) sin-pitch) (* z cos-pitch))
        perspective (/ focal camera-z)]
    {:x (+ center-x (* x perspective))
     :y (- center-y (* camera-y perspective))}))

(defn- make-row
  [row time width focal center-x center-y travel]
  (let [camera-offset (mod travel row-spacing)
        z (- (+ near-distance (* row row-spacing)) camera-offset)
        world-z (+ travel z)
        world-half-width (* 0.62 (/ width focal) z)
        points (mapv (fn [column]
                       (let [x (- (* column (/ (* 2 world-half-width)
                                               (dec column-count)))
                                  world-half-width)
                             height-value (terrain-height x world-z time)]
                         (project-point x z height-value focal center-x center-y)))
                     (range column-count))]
    {:points points
     :z z}))

(defn- draw-polyline!
  [^js ctx points]
  (when (seq points)
    (.beginPath ctx)
    (let [first-point (first points)]
      (.moveTo ctx (:x first-point) (:y first-point))
      (doseq [{:keys [x y]} (rest points)]
        (.lineTo ctx x y)))
    (.stroke ctx)))

(defn- draw-sky!
  [^js ctx width height]
  (let [gradient (.createLinearGradient ctx 0 0 0 height)]
    (.addColorStop gradient 0 "#090018")
    (.addColorStop gradient 0.48 "#18052b")
    (.addColorStop gradient 0.52 "#300b43")
    (.addColorStop gradient 1 "#020107")
    (set! (.-fillStyle ctx) gradient)
    (.fillRect ctx 0 0 width height)))

(defn- draw-mountains!
  [^js ctx width height time]
  (let [focal (* 0.82 (min width height))
        center-x (* width 0.5)
        center-y (* height 0.52)
        horizon (- center-y (* focal (js/Math.tan camera-pitch)))
        travel (* time 0.006)
        rows (mapv #(make-row % time width focal center-x center-y travel)
                   (range row-count))]
    (set! (.-lineCap ctx) "round")
    (set! (.-lineJoin ctx) "round")
    (set! (.-shadowBlur ctx) 9)
    (set! (.-shadowColor ctx) "rgba(0, 238, 255, 0.9)")
    (doseq [[index {:keys [points z]}] (map-indexed vector rows)]
      (set! (.-lineWidth ctx) (max 0.55 (* 1.4 (/ 1 (max 0.8 z)))))
      (set! (.-strokeStyle ctx)
            (hsl (+ 174 (* 32 (js/Math.sin (+ (* index 0.3)
                                              (* time 0.0002)))))
                 94
                 (+ 48 (* 14 (js/Math.sin (* index 0.18))))
                 (+ 0.32 (* 0.45 (min 1 (/ 8 z))))))
      (draw-polyline! ctx points))
    (doseq [[near-row far-row] (map vector rows (rest rows))]
      (set! (.-lineWidth ctx) 0.7)
      (set! (.-strokeStyle ctx) (hsl 276 94 66 0.54))
      (doseq [[near-point far-point] (map vector (:points near-row)
                                          (:points far-row))]
        (.beginPath ctx)
        (.moveTo ctx (:x near-point) (:y near-point))
        (.lineTo ctx (:x far-point) (:y far-point))
        (.stroke ctx)))
    (set! (.-shadowBlur ctx) 0)
    (set! (.-strokeStyle ctx) "rgba(255, 55, 220, 0.85)")
    (set! (.-lineWidth ctx) 1.4)
    (.beginPath ctx)
    (.moveTo ctx 0 horizon)
    (.lineTo ctx width horizon)
    (.stroke ctx)))

(defn- draw-vhs-overlay!
  [^js ctx width height time]
  (set! (.-globalCompositeOperation ctx) "screen")
  (set! (.-fillStyle ctx) "rgba(130, 210, 255, 0.055)")
  (doseq [y (range 0 height 3)]
    (.fillRect ctx 0 y width 1))
  (set! (.-fillStyle ctx) "rgba(255, 60, 210, 0.08)")
  (doseq [_ (range 95)]
    (let [x (* width (rand))
          y (* height (rand))
          size (+ 1 (* 3 (rand)))]
      (.fillRect ctx x y size 1)))
  (when (< (mod (* time 0.004) 13) 1)
    (let [y (* height (rand))
          band-height (+ 2 (* 8 (rand)))]
      (set! (.-fillStyle ctx) "rgba(255, 70, 220, 0.16)")
      (.fillRect ctx 0 y width band-height)
      (set! (.-fillStyle ctx) "rgba(0, 230, 255, 0.12)")
      (.fillRect ctx (* width 0.04) (+ y 2) (* width 0.92) 1)))
  (set! (.-globalCompositeOperation ctx) "source-over")
  (set! (.-fillStyle ctx) "rgba(0, 0, 0, 0.16)")
  (.fillRect ctx 0 0 width 5)
  (.fillRect ctx 0 (- height 5) width 5))

(defn draw-effect!
  [{:keys [ctx width height]} time]
  (draw-sky! ctx width height)
  (draw-mountains! ctx width height time)
  (draw-vhs-overlay! ctx width height time))
