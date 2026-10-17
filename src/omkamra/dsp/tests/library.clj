(ns omkamra.dsp.tests.library
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.library.control :as control]
            [omkamra.dsp.library.delay :as delay]
            [omkamra.dsp.library.doc :as doc]
            [omkamra.dsp.library.effect :as effect]
            [omkamra.dsp.library.envelope :as envelope]
            [omkamra.dsp.library.filter :as filter]
            [omkamra.dsp.library.math :as math]
            [omkamra.dsp.library.noise :as noise]
            [omkamra.dsp.library.oscillator :as oscillator]
            [omkamra.dsp.library.pitch :as pitch]
            [omkamra.dsp.harness.fixtures :refer [naive-saw stereo-sum]]))

(deftest band-limited-oscillators
  ;; PolyBLEP saw/pulse/square and the integrated triangle: the interpreter and
  ;; the JVM agree (the catalog covers JavaScript and Wasm), and the correction
  ;; measurably reduces the naive waveform's step discontinuity.
  (let [render (fn [v controls target frames]
                 (let [artifact (dsp/compile v {:target target :entry :process})
                       output (float-array frames)]
                   (dsp/process! artifact
                                 {:input (float-array frames) :output output
                                  :frames frames :sample-rate 44100.0
                                  :controls controls})
                   (vec output)))
        max-step (fn [xs]
                   (->> (map (fn [a b] (Math/abs (double (- b a)))) xs (rest xs))
                        (reduce max 0.0)))
        oscillators [[#'oscillator/saw-oscillator [440.0]]
                     [#'oscillator/pulse-oscillator [440.0 0.3]]
                     [#'oscillator/square-oscillator [440.0]]
                     [#'oscillator/triangle-oscillator [440.0]]]]
    (doseq [[v controls] oscillators]
      (let [interpreter (render v controls :interpreter 4096)
            jvm (render v controls :jvm 4096)
            ;; The triangle's leaky integrator settles after its first period.
            steady (subvec jvm 2048)]
        (is (= interpreter jvm))
        (is (every? #(Float/isFinite %) jvm))
        (is (every? #(<= -1.05 (double %) 1.05) steady))
        (is (some #(> (double %) 0.5) steady))
        (is (some #(< (double %) -0.5) steady))))
    ;; The PolyBLEP correction spreads the saw's wrap over two samples, so its
    ;; largest step is far smaller than the naive sawtooth's.
    (is (< (max-step (subvec (render #'oscillator/saw-oscillator [440.0] :interpreter 4096) 2048))
           (max-step (subvec (render #'naive-saw [440.0] :interpreter 4096) 2048))))))

(deftest noise-and-lfo
  ;; Portable noise: deterministic per target, decorrelated by seed, and pink
  ;; measurably more correlated than white (lag-1 autocorrelation).
  (let [render (fn [v controls target frames]
                 (let [artifact (dsp/compile v {:target target :entry :process})
                       output (float-array frames)]
                   (dsp/process! artifact {:input (float-array frames) :output output
                                           :frames frames :sample-rate 44100.0
                                           :controls controls})
                   (mapv double (vec output))))
        mean (fn [xs] (/ (reduce + xs) (count xs)))
        rms (fn [xs] (Math/sqrt (/ (reduce + (map #(* % %) xs)) (count xs))))
        autocorr1 (fn [xs]
                    (let [m (mean xs) d (map #(- % m) xs)]
                      (/ (reduce + (map * d (rest d)))
                         (reduce + (map #(* % %) d)))))
        white (render #'noise/white-noise [7] :interpreter 16384)
        pink (render #'noise/pink-noise [7] :interpreter 16384)]
    (doseq [v [#'noise/white-noise #'noise/pink-noise]]
      (let [interpreter (render v [7] :interpreter 4096)
            jvm (render v [7] :jvm 4096)]
        (is (= interpreter jvm))
        (is (every? Double/isFinite jvm))
        (is (not= interpreter (render v [8] :interpreter 4096)))))
    ;; Uniform white: range [-1, 1), ~zero mean, rms 1/sqrt(3).
    (is (every? #(<= -1.0 % 1.0) white))
    (is (< (Math/abs (mean white)) 0.02))
    (is (< (Math/abs (- (rms white) (/ 1.0 (Math/sqrt 3.0)))) 0.02))
    (is (< (Math/abs (autocorr1 white)) 0.05))
    ;; Pink: strongly autocorrelated, and quieter for the same input.
    (is (> (autocorr1 pink) 0.5))
    (is (< (rms pink) (rms white))))
  ;; The LFO is a unipolar shape selector.
  (let [lfo (fn [shape]
              (let [artifact (dsp/compile #'oscillator/lfo {:target :interpreter :entry :process})
                    frames 4410 output (float-array frames)]
                (dsp/process! artifact {:input (float-array frames) :output output
                                        :frames frames :sample-rate 44100.0
                                        :controls [2.0 shape]})
                (mapv double (vec output))))
        shapes (mapv lfo [0 1 2])]
    (doseq [shape shapes]
      (is (every? #(<= 0.0 % 1.0) shape)))
    (is (= 3 (count (distinct (map #(vec (take 100 %)) shapes)))))))

(deftest control-effects-and-interpolation
  (let [render (fn [v controls target frames input]
                 (let [artifact (dsp/compile v {:target target :entry :process})
                       output (float-array frames)]
                   (dsp/process! artifact {:input (float-array input) :output output
                                           :frames frames :sample-rate 44100.0
                                           :controls controls})
                   (mapv double (vec output))))
        ones (vec (repeat 512 1.0))
        smoothed (render #'control/smoothing [1.0 0.01] :interpreter 3000
                         (vec (repeat 3000 1.0)))
        slewed (render #'control/slew-limit [1.0 2.0] :interpreter 3000
                       (vec (repeat 3000 1.0)))
        one-time-constant (- 1.0 (/ 1.0 Math/E))
        slew-step (- (nth slewed 1) (nth slewed 0))
        expected-slope (/ 2.0 44100.0)]
    ;; Smoothing reaches 1 - 1/e after exactly one time constant, and rises.
    (is (< (Math/abs (- (nth smoothed 441) one-time-constant)) 0.01))
    (is (< (nth smoothed 2999) 1.0))
    (is (= smoothed (sort smoothed)))
    ;; Slew limiting bounds the slope to rate/sample-rate, so a step is a ramp.
    (is (< (Math/abs (- slew-step expected-slope)) 1.0e-9))
    (is (= slewed (sort slewed)))
    ;; Distortion is odd, normalised to +-1 at full scale, and expansive.
    (doseq [x [1.0 0.1 0.0 -1.0]]
      (let [actual (first (render #'effect/distortion [4.0] :interpreter 1 [x]))
            expected (/ (Math/tanh (* 4.0 x)) (Math/tanh 4.0))]
        (is (< (Math/abs (- actual expected)) 1.0e-6))))
    ;; Catmull-Rom is exact on a linear ramp.
    (doseq [frac [0.0 0.25 0.5 0.75]]
      (let [actual (first (render #'math/cubic-interpolate [1.0 2.0 3.0 frac]
                                  :interpreter 1 [0.0]))]
        (is (< (Math/abs (- actual (+ 1.0 frac))) 1.0e-6))))
    ;; All four agree between the interpreter and the JVM.
    (doseq [[v controls input] [[#'control/smoothing [1.0 0.01] ones]
                                [#'control/slew-limit [1.0 2.0] ones]
                                [#'effect/distortion [4.0] (vec (repeat 512 0.5))]
                                [#'math/cubic-interpolate [1.0 2.0 3.0 0.5]
                                 (vec (repeat 512 0.0))]]]
      (is (= (render v controls :interpreter 512 input)
             (render v controls :jvm 512 input))))))

(deftest multimode-envelopes-chorus-and-sync
  (let [render (fn [v controls target frames input]
                 (let [artifact (dsp/compile v {:target target :entry :process})
                       output (float-array frames)]
                   (dsp/process! artifact {:input (float-array input) :output output
                                           :frames frames :sample-rate 44100.0
                                           :controls controls})
                   (mapv double (vec output))))
        sine (mapv #(Math/sin (* 0.05 %)) (range 1000))
        gates (vec (repeat 1000 1.0))]
    ;; Lowpass mode is bit-identical to the single-mode biquad; the four modes
    ;; then differ from one another.
    (is (= (render #'filter/biquad [1000.0 0.7] :interpreter 1000 sine)
           (render #'filter/biquad-multimode [1000.0 0.7 0] :interpreter 1000 sine)))
    (is (= 4 (count (distinct (mapv #(render #'filter/biquad-multimode [1000.0 0.7 %]
                                             :interpreter 1000 sine)
                                    [0 1 2 3])))))
    ;; AR reaches 1 in exactly `attack` seconds and never overshoots.
    (let [ar (render #'envelope/ar-envelope [0.01 0.01] :interpreter 1000 gates)]
      (is (< (Math/abs (- (nth ar 440) 1.0)) 1.0e-6))
      (is (= ar (sort ar))))
    (is (every? Double/isFinite (render #'envelope/linear-adsr [0.0002 0.0001 0.4 0.0002]
                                        :interpreter 1000 gates)))
    ;; Chorus at mix 0 is the dry signal, exactly.
    (is (= (mapv #(double (float %)) sine)
           (render #'delay/chorus [10.0 8.0 0.0] :interpreter 1000 sine)))
    ;; Hard sync restarts the slave once per slave cycle, so the number of
    ;; discontinuities is frequency * ratio over a second.
    (let [synced (render #'oscillator/sync-oscillator [100.0 3.0] :interpreter 44100
                         (vec (repeat 44100 0.0)))]
      (is (= 300 (count (filter #(> (Math/abs %) 1.0) (map - synced (rest synced)))))))
    ;; A record-returning module whose two outputs are phase-coherent: the left
    ;; and right terms share the delayed copy, so they sum to twice the input up
    ;; to f32 rounding in the products (checked through a process that projects
    ;; the record).
    (let [expected (mapv #(* 2.0 (double (float %))) sine)
          actual (render #'stereo-sum [0.7 8.0] :interpreter 1000 sine)
          worst (apply max (map (fn [a b] (Math/abs (- a b))) actual expected))]
      (is (< worst 1.0e-4)))
    (doseq [[v controls] [[#'filter/biquad-multimode [1000.0 0.7 2]]
                          [#'envelope/ar-envelope [0.01 0.02]]
                          [#'envelope/linear-adsr [0.0002 0.0001 0.4 0.0002]]
                          [#'delay/chorus [2.0 8.0 0.7]]
                          [#'oscillator/sync-oscillator [220.0 2.0]]]]
      (is (= (render v controls :interpreter 512 gates)
             (render v controls :jvm 512 gates))))))

(deftest library-reference-is-current
  ;; `LIBRARY.md` is rendered from the descriptors themselves (docstrings carry
  ;; what a descriptor cannot: latency and modulation behaviour). Regenerate it
  ;; with `(omkamra.dsp.library.doc/-main)`.
  (is (= (doc/render) (slurp (doc/doc-file)))
      "LIBRARY.md is stale; run omkamra.dsp.library.doc/-main")
  (doseq [ns-sym doc/library-namespaces
          [name _] (doc/module-definitions ns-sym)]
    (is (str/includes? (doc/render) (str ns-sym "/" name)))))

(deftest pitch-conversion
  ;; The pitch-domain defaults (D35): `cents->ratio` is the ratio, and
  ;; `pitch->frequency` is equal temperament with bend and detune applied. That
  ;; they agree with the host's own tuning is the point — the conversion moved
  ;; into the kernel, so the two must not drift.
  (let [invoke (fn [v & args]
                 (double (apply dsp/invoke
                                (dsp/compile v {:target :jvm :entry :function})
                                args)))]
    (is (< (Math/abs (- (invoke #'pitch/cents->ratio 1200.0) 2.0)) 1.0e-6)
        "an octave is a ratio of two")
    (is (< (Math/abs (- (invoke #'pitch/cents->ratio 100.0)
                        (Math/pow 2.0 (/ 100.0 1200.0))))
           1.0e-6)
        "a semitone is 100 cents")
    (is (< (Math/abs (- (invoke #'pitch/pitch->frequency 69.0 0.0 0.0 440.0 69.0) 440.0))
           1.0e-4))
    (is (< (Math/abs (- (invoke #'pitch/pitch->frequency 60.0 0.0 0.0 440.0 69.0) 261.6256))
           1.0e-3))
    (is (< (Math/abs (- (invoke #'pitch/pitch->frequency 69.0 200.0 0.0 440.0 69.0) 493.8833))
           1.0e-3)
        "200 cents up is two semitones")
    (is (< (Math/abs (- (invoke #'pitch/pitch->frequency 69.0 0.0 -100.0 440.0 69.0) 415.3047))
           1.0e-3)
        "detune is cents too")))

;; D40/P10d: the `pan` module. `panned-stereo` projects the pair into the two
;; output channels.
(dsp/defn panned-stereo
  [position]
  {:process {:input false :controls [{:name position}]}}
  (let [pair (effect/pan 1.0 position)]
    (buffer-store :output 0 0 (:left pair))
    (buffer-store :output 1 0 (:right pair))))

(deftest pan-is-equal-power
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile panned-stereo {:target target :entry :process})
          render (fn [position]
                   (let [output (into-array (Class/forName "[F")
                                            [(float-array 1) (float-array 1)])
                         ^objects out output]
                     (dsp/process! artifact {:output out :frames 1
                                             :sample-rate 48000.0
                                             :controls [position]})
                     [(double (aget ^floats (aget out 0) 0))
                      (double (aget ^floats (aget out 1) 0))]))]
      (is (= [1.0 0.0] (render -1.0)) (str "hard left on " target))
      (is (= [0.0 1.0] (render 1.0)) (str "hard right on " target))
      (let [[left right] (render 0.0)]
        (is (< (Math/abs (- left (Math/sqrt 0.5))) 1.0e-5)
            (str "centre left on " target))
        (is (< (Math/abs (- right (Math/sqrt 0.5))) 1.0e-5)
            (str "centre right on " target))
        (is (< (Math/abs (- (+ (* left left) (* right right)) 1.0)) 1.0e-5)
            (str "centre keeps power on " target))))))
