(ns omkamra.dsp.library.math
  "Portable scalar helpers: clipping, windows, interpolation, and allpass
  coefficients. Nothing here has state."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

(dsp/defn clip
  "Hard-limit `sample` to the closed range [`lo`, `hi`].

  A thin composition of the `min`/`max` builtins.  It exists partly because it
  is genuinely useful and partly because it is the smallest proof that a
  separately defined DSP function can be reused by other definitions through
  the ordinary call mechanism on every target.
"
  [sample lo hi]
  (min hi (max lo sample)))

(dsp/defn triangular-window
  "Bartlett (triangular) window over the normalised phase `phase`.

  Rises linearly from 0 at phase 0 to 1 at 0.5 and back to 0 at 1, extending
  linearly outside [0, 1].  A raised-cosine (Hann) window is deliberately not
  provided yet: it needs `cos`, which belongs to P3.
"
  [phase]
  (- 1.0 (abs (- (* 2.0 phase) 1.0))))

(dsp/defn linear-interpolate
  "Linear interpolation between two samples at fractional position `frac` in
  [0, 1).
"
  [x0 x1 frac]
  (+ (* (- 1.0 frac) x0) (* frac x1)))

(dsp/defn allpass-coefficient
  "First-order allpass coefficient for the fractional position `frac` in
  [0, 1). The allpass recursion `y = a * (x - previous) + x-below` has a
  flatter magnitude response than linear interpolation.
"
  [frac]
  (/ (- 1.0 frac) (+ 1.0 frac)))

;; Catmull-Rom cubic interpolation between `x1` and `x2` at fractional position
;; `frac` in [0, 1), using `x0` and `x3` as the neighbours that set the tangents.
;; It is exact on a linear ramp and continuous through the first derivative at the
;; sample points, which is what makes it the usual choice for a fractional delay
;; tap or a resampler. Stateless.
(dsp/defn cubic-interpolate
  [x0 x1 x2 x3 frac]
  (let [t2 (* frac frac)
        t3 (* t2 frac)
        a1 (* 0.5 (- x2 x0))
        a2 (+ (- x0 (* 2.5 x1)) (- (* 2.0 x2) (* 0.5 x3)))
        a3 (+ (- (* 1.5 x1) (* 0.5 x0)) (- (* 0.5 x3) (* 1.5 x2)))]
    (+ (+ x1 (* a1 frac)) (+ (* a2 t2) (* a3 t3)))))
