(ns omkamra.dsp.library.filter
  "Filters: one-pole, biquad, and state-variable."
  #?(:clj (:require [omkamra.dsp :as dsp]
                    #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
                    [omkamra.dsp.library.envelope :refer [exponential-adsr]])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; A named state-record shape, declared once and reused by a state declaration.
;; The shorthand field list gives :float fields initialised to 0.0. The linker
;; resolves the `:struct` reference and inlines the fields.
(dsp/defstruct one-pole-state
  [previous])

(dsp/defn one-pole-record
  "The one-pole, with its state held in a named record rather than a bare slot.
  Field access uses the Clojure-style forms: `(:field record)` to read and
  `(set! (:field record) value)` to write.
"
  [sample coefficient]
  {:state [{:name history :struct one-pole-state}]
   :process {:controls [{:name coefficient}]}}
  (let [previous (:previous history)]
    (do (set! (:previous history) (+ sample (* coefficient previous)))
        previous)))

(dsp/defn one-pole
  "A reusable one-pole *module*. Because it is stateful, each call site owns its
  own `previous` (D19), so a voice can hold several independent one-poles.
"
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

;; RBJ biquad lowpass as a module. Its history lives in a named record so the
;; update order is explicit and visible: the output is computed from the current
;; history, then the history shifts. Direct Form I; the coefficients are
;; recomputed each sample, because controls are per-sample in this architecture.
(dsp/defstruct biquad-state
  [x1 x2 y1 y2])

(dsp/defn biquad
  "RBJ biquad lowpass, Direct Form I. The history lives in a record so the update
  order is explicit, and the coefficients are recomputed per sample because
  controls are per-sample in this architecture."
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
  "Chamberlin state-variable filter: one pass yields lowpass, bandpass, and
  highpass together (a record return), so the three outputs share one history.
  Stable while the cutoff stays below a sixth of the sample rate."
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

(dsp/defn biquad-multimode
  "One biquad, four responses. A single feedback history suffices because only
  one mode is active at a time, and all four responses share the RBJ
  denominator: `mode` selects lowpass (0), bandpass (1), highpass (2), or notch
  (3). Lowpass mode is bit-identical to `biquad`, which the tests assert.
"
  [sample cutoff q mode]
  {:state [{:name s :struct biquad-state}]
   :process {:controls [{:name cutoff} {:name q} {:name mode :type :int}]}}
  (let [w0 (* 6.283185307179586 (/ cutoff sample-rate))
        cosw (cos w0)
        alpha (/ (sin w0) (* 2.0 q))
        a0 (+ 1.0 alpha)
        a1n (/ (* 2.0 cosw) a0)
        a2n (/ (- alpha 1.0) a0)
        b0 (if (< mode 1)
             (/ (- 1.0 cosw) (* 2.0 a0))
             (if (< mode 2)
               (/ alpha a0)
               (if (< mode 3)
                 (/ (+ 1.0 cosw) (* 2.0 a0))
                 (/ 1.0 a0))))
        b1 (if (< mode 1)
             (/ (- 1.0 cosw) a0)
             (if (< mode 2)
               0.0
               (if (< mode 3)
                 (/ (- 0.0 (+ 1.0 cosw)) a0)
                 (/ (* -2.0 cosw) a0))))
        ;; Only the bandpass differs in the sign of its second numerator term.
        b2 (if (= mode 1) (- 0.0 b0) b0)
        y (+ (+ (+ (+ (* b0 sample)
                      (* b1 (:x1 s)))
                   (* b2 (:x2 s)))
                (* a1n (:y1 s)))
             (* a2n (:y2 s)))]
    (do
      (set! (:x2 s) (:x1 s))
      (set! (:x1 s) sample)
      (set! (:y2 s) (:y1 s))
      (set! (:y1 s) y)
      y)))
