(ns rb.explores.quil
  (:require [quil.core :as q]))

(defn setup
  []
  (q/set-state!
   :font (q/create-font "Droid Sans Mono" 80)))

                                        ; define function which draws spiral
(defn draw []
                                        ; make background white
  (q/cursor :wait)
  (q/background 255)

                                        ; move origin point to centre of the sketch
                                        ; by default origin is in the left top corner
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
                                        ; parameter t goes 0, 0.01, 0.02, ..., 99.99, 100
    (doseq [t (range 0 100 0.005)]
                                        ; draw a point with x = t * sin(t) and y = t * cos(t)
      (q/point (* t (q/sin t))
               (* t (q/cos t)))))
  
  (q/fill [120 6 180])
  (q/text-font (q/state :font))
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (let [fc (q/frame-count)]
      (q/scale (+ 0.1 (Math/abs (* 3 (Math/sin (* 1/24 fc))))))
      (let [r0 (* 3 (Math/sin (* 1/64 fc)))
            z-count 16]
        (doseq [i (range z-count)]
          (q/fill [120 6 180 (* i (/ 256 z-count))])
          (q/with-rotation [(+ r0 (* i (* (/ Math/PI 4) (Math/sin (* 1/32 fc)))))]
            (q/text-char \Z 0 0)))))))

; run sketch
(q/defsketch trigonometry
  :size [300 300]
  :setup setup
  :draw draw)

(q/available-fonts)
