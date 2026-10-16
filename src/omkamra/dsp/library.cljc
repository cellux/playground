(ns omkamra.dsp.library
  "First entries of the portable DSP standard library (P2 deliverable 5).

  This is the seed of the library named by P7: DSP functions written in the
  DSP language itself, composed from builtins rather than host calls.  It is a
  `.cljc` namespace on purpose.  The JVM compiler expands `dsp/defn` today, and
  the same source forms are intended to be read and compiled by the browser
  compiler running in SCI once the front end is portable (P13).  Nothing here
  uses a target-specific operation."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; Hard-limit `sample` to the closed range [`lo`, `hi`].
;;
;; A thin composition of the `min`/`max` builtins.  It exists partly because it
;; is genuinely useful and partly because it is the smallest proof that a
;; separately defined DSP function can be reused by other definitions through
;; the ordinary call mechanism on every target.
(dsp/defn clip
  [sample lo hi]
  (min hi (max lo sample)))

;; Bartlett (triangular) window over the normalised phase `phase`.
;;
;; Rises linearly from 0 at phase 0 to 1 at 0.5 and back to 0 at 1, extending
;; linearly outside [0, 1].  A raised-cosine (Hann) window is deliberately not
;; provided yet: it needs `cos`, which belongs to P3.
(dsp/defn triangular-window
  [phase]
  (- 1.0 (abs (- (* 2.0 phase) 1.0))))

;; Sine oscillator: `phase` advances by `frequency / sample-rate` each sample
;; and wraps at 1.0, so `sin` sees an angle in turns scaled to radians.
(dsp/defn sine-oscillator
  [sample frequency]
  {:state [{:name phase
            :init 0.0
            :next (let [advanced (+ phase (* frequency (/ 1.0 sample-rate)))]
                    (if (>= advanced 1.0) (- advanced 1.0) advanced))}]
   :process {:controls [{:name frequency}]}}
  (sin (* 6.283185307179586 phase)))

;; One-pole approach factor for an exponential segment of `time` seconds.  The
;; lower bound only guards division by zero (or a negative time); a zero time
;; then gives an instant segment rather than a stalled one.
(dsp/defn adsr-coefficient
  [time rate]
  (- 1.0 (exp (- 0.0 (/ 1.0 (* (max time 1.0e-6) rate))))))

;; Exponential ADSR.  The input sample is the gate (`> 0` is on); the state is
;; the stage (0 idle, 1 attack, 2 decay, 3 sustain, 4 release) and the current
;; level.  Each segment approaches its target by a fixed per-sample factor.
;; Stage changes use small thresholds rather than exact equality: an
;; exponential approach only reaches its target in the limit.
(dsp/defn exponential-adsr
  [gate attack decay sustain release]
  {:state [{:name level
            :init 0.0
            :next (let [a (adsr-coefficient attack sample-rate)
                        d (adsr-coefficient decay sample-rate)
                        r (adsr-coefficient release sample-rate)]
                    (if (= stage 1) (+ level (* a (- 1.0 level)))
                        (if (= stage 2) (+ level (* d (- sustain level)))
                            (if (= stage 3) sustain
                                (if (= stage 4) (* level (- 1.0 r))
                                    0.0)))))}
           {:name stage
            :type :int
            :init 0
            :next (if (> gate 0.0)
                    (if (= stage 4) 1
                        (if (= stage 1) (if (>= level 0.999) 2 1)
                            (if (= stage 2) (if (<= level (+ sustain 0.001)) 3 2)
                                (if (= stage 3) 3 1))))
                    (if (= stage 4) (if (<= level 0.001) 0 4) 4))}]
   :process {:controls [{:name attack} {:name decay}
                        {:name sustain} {:name release}]}}
  level)
