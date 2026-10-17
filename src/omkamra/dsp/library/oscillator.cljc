(ns omkamra.dsp.library.oscillator
  "Oscillators."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

(dsp/defn sine-oscillator
  "Sine oscillator: `phase` advances by `frequency / sample-rate` each sample
  and wraps at 1.0, so `sin` sees an angle in turns scaled to radians.
"
  [frequency]
  {:state [{:name phase
            :init 0.0
            :next (let [advanced (+ phase (* frequency (/ 1.0 sample-rate)))]
                    (if (>= advanced 1.0) (- advanced 1.0) advanced))}]
   :process {:input false :controls [{:name frequency}]}}
  (sin (* 6.283185307179586 phase)))

;; --- Band-limited oscillators (PolyBLEP) ------------------------------------

(dsp/defn polyblep
  "Two-sample polynomial band-limited step correction for a jump at phase 0.

  `t` is the phase in [0, 1) and `dt` the per-sample phase increment. The
  correction is subtracted from a naive sawtooth (one downward jump per period)
  and added/subtracted at a pulse's two edges. Pure: no state, no table, so it
  is portable and allocation-free. It flattens the discontinuity over two
  samples, which removes the worst aliasing at a cost of a little brightness.
"
  [t dt]
  (if (< t dt)
    (let [x (/ t dt)]
      (- (- (* 2.0 x) (* x x)) 1.0))
    (if (> t (- 1.0 dt))
      (let [x (/ (- t 1.0) dt)]
        (+ (+ (* x x) (* 2.0 x)) 1.0))
      0.0)))

(dsp/defn saw-oscillator
  "Band-limited sawtooth: a rising ramp from -1 to 1, corrected at the wrap.
  A generator (`:input false`), so it is called `(saw-oscillator frequency)`
  rather than carrying an ignored sample argument.
"
  [frequency]
  {:state [{:name phase
            :init 0.0
            :next (fract (+ phase (/ frequency sample-rate)))}]
   :process {:input false :controls [{:name frequency}]}}
  (let [dt (/ frequency sample-rate)]
    (- (- (* 2.0 phase) 1.0) (polyblep phase dt))))

(dsp/defn pulse-oscillator
  "Band-limited pulse of width `width` in (0, 1); 0.5 is a square. Two edges per
  period, so two corrections.
"
  [frequency width]
  {:state [{:name phase
            :init 0.0
            :next (fract (+ phase (/ frequency sample-rate)))}]
   :process {:input false :controls [{:name frequency} {:name width}]}}
  (let [dt (/ frequency sample-rate)
        naive (if (< phase width) 1.0 -1.0)]
    (- (+ naive (polyblep phase dt))
       (polyblep (fract (+ phase (- 1.0 width))) dt))))

(dsp/defn square-oscillator
  "A square is the half-width pulse, reached through the ordinary call
  mechanism, so it needs no state of its own.
"
  [frequency]
  {:process {:input false :controls [{:name frequency}]}}
  (pulse-oscillator frequency 0.5))

;; The triangle is the integral of the band-limited square, so it inherits the
;; square's band limiting. The integral lives in a record (explicit update
;; order); the small leak removes the integrator's DC, and the gain is scaled so
;; the ramp reaches +-1. The leak makes the amplitude a mild function of
;; frequency — about +-1.5% across 50 Hz..2 kHz — which is documented rather
;; than corrected. The leak also gives the first cycle a start-up transient (its
;; DC removal has no history yet), so the amplitude settles after one period.
(dsp/defstruct triangle-state
  [phase integrated])

(dsp/defn triangle-oscillator
  "Band-limited triangle: the integral of the band-limited square.

  The leaky integrator removes the integral's DC and is scaled to reach +-1; the
  amplitude is a mild function of frequency (+-1.5% across 50 Hz..2 kHz) and the
  first cycle has a start-up transient, both documented rather than corrected."
  [frequency]
  {:state [{:name s :struct triangle-state}]
   :process {:input false :controls [{:name frequency}]}}
  (let [dt (/ frequency sample-rate)
        square (- (+ (if (< (:phase s) 0.5) 1.0 -1.0)
                     (polyblep (:phase s) dt))
                  (polyblep (fract (+ (:phase s) 0.5)) dt))
        integrated (+ (* (* 5.19 dt) square)
                      (* (- 1.0 (* 4.0 dt)) (:integrated s)))]
    (do
      (set! (:phase s) (fract (+ (:phase s) dt)))
      (set! (:integrated s) integrated)
      integrated)))

;; --- Modulation -------------------------------------------------------------

;; A low-frequency oscillator for modulation, unipolar in [0, 1]: sawtooth
;; (`shape` 0), triangle (1), or sine (2). The same module at audio rate is an
;; ordinary oscillator, so it needs no separate state.
(dsp/defn lfo
  [frequency shape]
  {:state [{:name phase
            :init 0.0
            :next (fract (+ phase (* frequency (/ 1.0 sample-rate))))}]
   :process {:input false :controls [{:name frequency} {:name shape :type :int}]}}
  (if (< shape 1)
    phase
    (if (< shape 2)
      (- 1.0 (abs (- (* 2.0 phase) 1.0)))
      (* 0.5 (+ 1.0 (sin (* 6.283185307179586 phase)))))))

;; --- Hard sync --------------------------------------------------------------

(dsp/defstruct sync-state
  [master slave])

(dsp/defn sync-oscillator
  "Hard sync: a slave sawtooth at `frequency * ratio` whose phase restarts every
  time a master at `frequency` wraps. The restart is placed at the master's
  sub-sample zero crossing rather than on the sample grid, which removes the
  timing quantisation that otherwise dominates; the reset step itself is not
  BLEP-corrected, so this is the one oscillator here that is not band-limited,
  and it aliases at high `ratio`.
"
  [frequency ratio]
  {:state [{:name s :struct sync-state}]
   :process {:input false :controls [{:name frequency} {:name ratio}]}}
  (let [dtm (* frequency (/ 1.0 sample-rate))
        raw (+ (:master s) dtm)
        wrapped (>= raw 1.0)
        overshoot (- raw 1.0)
        master (if wrapped overshoot raw)
        slave (if wrapped
                (* overshoot ratio)
                (fract (+ (:slave s) (* dtm ratio))))]
    (do
      (set! (:master s) master)
      (set! (:slave s) slave)
      (- (* 2.0 slave) 1.0))))
