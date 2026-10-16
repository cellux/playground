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

;; A named state-record shape, declared once and reused by a state declaration.
;; The shorthand field list gives :float fields initialised to 0.0. The linker
;; resolves the `:struct` reference and inlines the fields.
(dsp/defstruct one-pole-state
  [previous])

;; The one-pole, with its state held in a named record rather than a bare slot.
;; Field access uses the Clojure-style forms: `(:field record)` to read and
;; `(set! (:field record) value)` to write.
(dsp/defn one-pole-record
  [sample coefficient]
  {:state [{:name history :struct one-pole-state}]
   :process {:controls [{:name coefficient}]}}
  (let [previous (:previous history)]
    (do (set! (:previous history) (+ sample (* coefficient previous)))
        previous)))

;; --- P6: composed stateful modules ------------------------------------------

;; A reusable one-pole *module*. Because it is stateful, each call site owns its
;; own `previous` (D19), so a voice can hold several independent one-poles.
(dsp/defn one-pole
  [sample coefficient]
  {:state [{:name previous
            :init 0.0
            :next (+ sample (* coefficient previous))}]}
  (+ sample (* coefficient previous)))

;; Two independent one-poles in parallel, composed only through the call
;; mechanism. The second uses half the coefficient, so any sharing of state
;; between the two call sites would show up as a wrong response.
(dsp/defn one-pole-voice
  [sample coefficient]
  {:process {:controls [{:name coefficient}]}}
  (* 0.5 (+ (one-pole sample coefficient)
            (one-pole sample (* 0.5 coefficient)))))

;; A state-free module that calls a stateful one twice. Its own body is a single
;; expression, so it exercises nested partitions: the pair has no state of its
;; own, yet owns two one-poles.
(dsp/defn one-pole-pair
  [sample coefficient]
  (* 0.5 (+ (one-pole sample coefficient)
            (one-pole sample (* 0.5 coefficient)))))

;; Two pairs, so four independent one-poles: two levels of call-site ownership.
(dsp/defn one-pole-pair-voice
  [sample coefficient]
  {:process {:controls [{:name coefficient}]}}
  (+ (one-pole-pair sample coefficient)
     (* 0.5 (one-pole-pair sample (* 2.0 coefficient)))))

;; --- P5: delay lines, circular buffers, and interpolation -------------------

;; Linear interpolation between two samples at fractional position `frac` in
;; [0, 1).
(dsp/defn linear-interpolate
  [x0 x1 frac]
  (+ (* (- 1.0 frac) x0) (* frac x1)))

;; First-order allpass coefficient for the fractional position `frac` in
;; [0, 1). The allpass recursion `y = a * (x - previous) + x-below` has a
;; flatter magnitude response than linear interpolation.
(dsp/defn allpass-coefficient
  [frac]
  (/ (- 1.0 frac) (+ 1.0 frac)))

;; Circular delay line of 64 samples with a linearly interpolated fractional
;; tap. It is a *module*: it owns its record, so several lines can be composed
;; independently. Every call site gets its own buffer (D19); before P6 it had to
;; be a process, because calls rejected stateful callees.
;;
;; The cursor is this frame's write position and advances by one every frame.
;; Ordering: the new sample is written *first*, then the taps are read, then the
;; cursor advances, so a zero delay returns the current input and every tap is
;; consistent within a frame. A delay of `d` samples reads the value written `d`
;; frames ago; delays of 64 or more alias around the buffer, and a negative
;; delay is undefined.
(dsp/defn delay-line
  [sample delay]
  {:state [{:name line
            :fields [{:name buffer :type :float :size 64}
                     {:name cursor :type :int :init 0}]}]}
  (let [cursor (:cursor line)]
    (do
      (aset line cursor sample)
      (let [whole (float->int delay)
            frac (- delay (int->float whole))
            x0 (aget line (mod (- cursor whole) 64))
            x1 (aget line (mod (- cursor (+ whole 1)) 64))]
        (do (set! (:cursor line) (mod (+ cursor 1) 64))
            (linear-interpolate x0 x1 frac))))))

;; The module used as a process: the control is the port.
(dsp/defn delay-line-voice
  [sample delay]
  {:process {:controls [{:name delay}]}}
  (delay-line sample delay))

;; Two independent lines with different delays, proving the call sites do not
;; share the buffer.
(dsp/defn delay-line-pair-voice
  [sample delay]
  {:process {:controls [{:name delay}]}}
  (* 0.5 (+ (delay-line sample delay)
            (delay-line sample (* 2.0 delay)))))

;; The same line with a first-order allpass fractional tap; the recursion is the
;; standard `y = a * (x - previous) + x-below`. The record owns the buffer, the
;; cursor, and the previous allpass output.
(dsp/defn allpass-delay-line
  [sample delay]
  {:state [{:name line
            :fields [{:name buffer :type :float :size 64}
                     {:name cursor :type :int :init 0}
                     {:name previous :type :float :init 0.0}]}]}
  (let [cursor (:cursor line)]
    (do
      (aset line cursor sample)
      (let [whole (float->int delay)
            frac (- delay (int->float whole))
            a (allpass-coefficient frac)
            x0 (aget line (mod (- cursor whole) 64))
            x1 (aget line (mod (- cursor (+ whole 1)) 64))
            y (+ (* a (- x0 (:previous line))) x1)]
        (do (set! (:cursor line) (mod (+ cursor 1) 64))
            (set! (:previous line) y)
            y)))))

(dsp/defn allpass-delay-line-voice
  [sample delay]
  {:process {:controls [{:name delay}]}}
  (allpass-delay-line sample delay))

;; --- P6c: a composed filter voice -------------------------------------------

;; RBJ biquad lowpass as a module. Its history lives in a named record so the
;; update order is explicit and visible: the output is computed from the current
;; history, then the history shifts. Direct Form I; the coefficients are
;; recomputed each sample, because controls are per-sample in this architecture.
(dsp/defstruct biquad-state
  [x1 x2 y1 y2])

(dsp/defn biquad
  [sample cutoff q]
  {:state [{:name s :struct biquad-state}]}
  (let [w0 (* 6.283185307179586 (/ cutoff sample-rate))
        cosw (cos w0)
        alpha (/ (sin w0) (* 2.0 q))
        a0 (+ 1.0 alpha)
        b0n (/ (- 1.0 cosw) (* 2.0 a0))
        b1n (/ (- 1.0 cosw) a0)
        ;; a1 = -2*cos(w0) and a2 = 1-alpha; the subtraction is folded into these
        ;; signs so the recursion is a plain sum.
        a1n (/ (* 2.0 cosw) a0)
        a2n (/ (- alpha 1.0) a0)
        y (+ (+ (+ (+ (* b0n sample)
                      (* b1n (:x1 s)))
                   (* b0n (:x2 s)))
                (* a1n (:y1 s)))
             (* a2n (:y2 s)))]
    (do
      (set! (:x2 s) (:x1 s))
      (set! (:x1 s) sample)
      (set! (:y2 s) (:y1 s))
      (set! (:y1 s) y)
      y)))

;; The P6 done-when voice: a stateful biquad and a stateful exponential ADSR,
;; composed only through calls. `exponential-adsr` keeps its `:process` metadata
;; but is called here as a module — one definition, two usage modes (D19). The
;; envelope output is the filter input, so the gate opens the filter.
(dsp/defn filter-voice
  [gate cutoff q attack decay sustain release]
  {:process {:controls [{:name cutoff} {:name q}
                        {:name attack} {:name decay}
                        {:name sustain} {:name release}]}}
  (biquad (exponential-adsr gate attack decay sustain release) cutoff q))

;; A module with several *outputs*. The Chamberlin state-variable filter yields
;; lowpass, bandpass, and highpass at once; the return is a named record, erased
;; at lowering, so each call site simply gets one local per output.
(dsp/defstruct svf-state
  [low band])

(dsp/defstruct svf-output
  [low band high])

(dsp/defn state-variable-filter
  [sample cutoff q]
  {:state [{:name s :struct svf-state}]
   :return-type svf-output}
  (let [f (* 2.0 (sin (* 3.141592653589793 (/ cutoff sample-rate))))
        q1 (/ 1.0 q)
        low (+ (:low s) (* f (:band s)))
        high (- (- sample low) (* q1 (:band s)))
        band (+ (* f high) (:band s))]
    (do
      (set! (:low s) low)
      (set! (:band s) band)
      {:low low :band band :high high})))

;; A voice using two of the three outputs: lowpass plus a scaled highpass.
(dsp/defn svf-voice
  [sample cutoff q]
  {:process {:controls [{:name cutoff} {:name q}]}}
  (let [out (state-variable-filter sample cutoff q)]
    (+ (:low out) (* 0.5 (:high out)))))
