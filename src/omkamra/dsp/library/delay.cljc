(ns omkamra.dsp.library.delay
  "Delay lines and circular buffers."
  #?(:clj (:require [omkamra.dsp :as dsp]
                    ;; The `refer`s resolve through the quoted DSP bodies, which
                    ;; clj-kondo cannot see as uses.
                    #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
                    [omkamra.dsp.library.math
                     :refer [allpass-coefficient linear-interpolate]]
                    #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
                    [omkamra.dsp.library.oscillator :refer [lfo]])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

(dsp/defn delay-line
  "Circular delay line of 64 samples with a linearly interpolated fractional
  tap. It is a *module*: it owns its record, so several lines can be composed
  independently. Every call site gets its own buffer (D19); before P6 it had to
  be a process, because calls rejected stateful callees.

  The cursor is this frame's write position and advances by one every frame.
  Ordering: the new sample is written *first*, then the taps are read, then the
  cursor advances, so a zero delay returns the current input and every tap is
  consistent within a frame. A delay of `d` samples reads the value written `d`
  frames ago; delays of 64 or more alias around the buffer, and a negative
  delay is undefined.
"
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

(dsp/defn allpass-delay-line
  "The same line with a first-order allpass fractional tap; the recursion is the
  standard `y = a * (x - previous) + x-below`. The record owns the buffer, the
  cursor, and the previous allpass output.
"
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

(dsp/defn chorus
  "Chorus: a delay line whose tap is swept by a sine LFO, mixed with the dry
  signal. `depth` is the sweep in samples and `mix` the wet proportion, so depth
  0 or mix 0 leaves the signal untouched. The LFO is the ordinary oscillator
  module at a low rate, so the sweep needs no machinery of its own.
"
  [sample rate depth mix]
  {:process {:controls [{:name rate} {:name depth} {:name mix}]}}
  (let [d (* depth (lfo 0.0 rate 2))
        wet (delay-line sample d)]
    (+ (* (- 1.0 mix) sample) (* mix wet))))
