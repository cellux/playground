(ns omkamra.dsp.library.noise
  "Portable noise sources.

  Both are deterministic: the white generator is a 32-bit xorshift built from the
  P1 integer bitwise operations, so every target produces the same samples from
  the same seed. `seed` decorrelates instances, which is what a voice needs; it is
  a control rather than state because a compile-time state initialiser cannot
  vary per instance."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; White noise in [-1, 1).
;;
;; A 32-bit xorshift (three shifts and xors) mapped to a float by taking the top
;; 24 bits, which are exactly representable in both f32 and f64. The seed
;; *rotates* the state before that extraction: a rotation is a bit permutation,
;; so it leaves the distribution intact while making two seeds decorrelated,
;; where adding or xoring a seed would skew or barely shift the top bits. The
;; state must never reach zero, which is xorshift's fixed point, hence `:init 1`.
(dsp/defn white-noise
  [seed]
  {:state [{:name x
            :type :int
            :init 1
            :next (let [a (bit-xor x (bit-shift-left x 13))
                        b (bit-xor a (unsigned-bit-shift-right a 17))]
                    (bit-xor b (bit-shift-left b 5)))}]
   :process {:input false :controls [{:name seed :type :int}]}}
  (let [r (bit-and seed 31)
        v (bit-or (bit-shift-left x r)
                  (unsigned-bit-shift-right x (- 32 r)))]
    (- (/ (int->float (unsigned-bit-shift-right v 8)) 8388608.0) 1.0)))

;; Pink noise: white noise shaped by the seven one-pole sections of Paul
;; Kellet's filter, which approximates -3 dB/octave across the audio band. The
;; section outputs live in a record so the update order is explicit: every
;; section reads its own previous output and the current white sample, and the
;; sum is formed from the *updated* values.
(dsp/defstruct pink-state
  [b0 b1 b2 b3 b4 b5 b6])

(dsp/defn pink-noise
  [seed]
  {:state [{:name s :struct pink-state}]
   :process {:input false :controls [{:name seed :type :int}]}}
  (let [white (white-noise seed)]
    (do
      (set! (:b0 s) (+ (* 0.99886 (:b0 s)) (* white 0.0555179)))
      (set! (:b1 s) (+ (* 0.99332 (:b1 s)) (* white 0.0750759)))
      (set! (:b2 s) (+ (* 0.96900 (:b2 s)) (* white 0.1538520)))
      (set! (:b3 s) (+ (* 0.86650 (:b3 s)) (* white 0.3104856)))
      (set! (:b4 s) (+ (* 0.55000 (:b4 s)) (* white 0.5329522)))
      (set! (:b5 s) (- (* -0.7616 (:b5 s)) (* white 0.0168980)))
      (let [sum (+ (+ (+ (:b0 s) (:b1 s)) (+ (:b2 s) (:b3 s)))
                   (+ (:b4 s) (+ (:b5 s) (:b6 s))))]
        (do
          (set! (:b6 s) (* white 0.115926))
          (* (+ sum (* white 0.5362)) 0.11))))))
