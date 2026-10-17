(ns omkamra.dsp.library.effect
  "Waveshaping and other effects."
  #?(:clj (:require [omkamra.dsp :as dsp]
                    #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
                    [omkamra.dsp.library.delay :refer [delay-line]])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; Soft saturation: `tanh` under a drive control, normalised so that a full-scale
;; input reaches +-1 at any drive. Stateless, and `tanh` is a P3 intrinsic on
;; every target, so it needs no table or emulation. `drive` is clamped away from
;; zero because the normaliser would otherwise divide by it.
(dsp/defn distortion
  [sample drive]
  {:process {:controls [{:name drive}]}}
  (let [d (max drive 1.0e-4)]
    (/ (tanh (* d sample)) (tanh d))))

(dsp/defstruct stereo-pair
  [left right])

(dsp/defn stereo-spread
  "Haas-style stereo spreading: a delayed copy becomes the side signal, which
  widens a mono input without decorrelating it. `width` 0 is mono and 1 is fully
  spread. Both outputs share one delay line, so they stay consistent; note that
  this returns a record, so it is a module rather than a process entry — writing
  two channels needs a multi-channel process (P8).
"
  [sample width delay]
  {:return-type stereo-pair}
  (let [side (- (delay-line sample delay) sample)]
    {:left (+ sample (* width side))
     :right (- sample (* width side))}))

(dsp/defn pan
  "Equal-power pan of a mono `sample` to a stereo pair.

  `position` runs -1 (hard left) to 1 (hard right), 0 centre, and the channel
  gains keep total power constant (`left^2 + right^2 = sample^2`). Returns a
  `stereo-pair`, so it is a module: a process entry projects it and writes the
  two channels (D40). Pair it with a `:pan` control (`set-pan!`) to let the host
  place a voice, or call it with a computed position to pan inside the DSP."
  [sample position]
  {:return-type stereo-pair}
  (let [t (* 0.5 (+ (clamp position -1.0 1.0) 1.0))]
    {:left (* sample (sqrt (- 1.0 t)))
     :right (* sample (sqrt t))}))
