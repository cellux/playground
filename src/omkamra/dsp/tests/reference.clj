(ns omkamra.dsp.tests.reference
  "The P11 reference synthesizer acceptance fixture: the instrument and its
  modulation matrix.

  The oracle is the reference interpreter hosting the same pool, exactly as for
  the P10 synth runtime: the matrix and the render loop are host code, so the
  JVM pool must produce the interpreter's samples bit for bit. The matrix is
  data, so its sources are checked one at a time, and every stated property
  carries a negative twin — a second run that must differ, or an identical run
  that must not — so nothing passes vacuously."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp.graph :as graph]
            [omkamra.dsp.harness.fixtures :refer [allocated-bytes
                                                  thread-allocation-meter]]
            [omkamra.dsp.instrument :as inst]
            [omkamra.dsp.synth.pool :as pool]
            [omkamra.dsp.examples.reference-synth :as ref]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- planar-output
  "A fresh planar f32 block, two channels of `frames` samples."
  [frames]
  (into-array (Class/forName "[F") [(float-array frames) (float-array frames)]))

(defn- render-live-phrase
  "Play `events` through a live reference instrument, returning the two channels
  as vectors of raw f32 bits.

  Raw bits rather than numbers, because the comparison is exact: bits are
  NaN-equal as NaN and distinguish +0.0 from -0.0.

  Events are `[:on note velocity]`, `[:off note]`, `[:controller n value]`,
  `[:pan index value]`, and `[:render frames]`."
  [instrument events]
  (let [block (planar-output 512)
        left (java.util.ArrayList.)
        right (java.util.ArrayList.)]
    (doseq [[event & args] events]
      (case event
        :on (ref/note-on! instrument (first args) (second args))
        :off (ref/note-off! instrument (first args))
        :controller (ref/set-controller! instrument (first args) (second args))
        :pan (ref/set-pan! instrument (first args) (second args))
        :effect (ref/set-effect-control! instrument (first args) (second args))
        :render (let [frames (first args)]
                  (ref/render! instrument block frames)
                  (dotimes [index frames]
                    (.add left (Float/floatToRawIntBits
                                (aget ^floats (aget ^objects block 0) index)))
                    (.add right (Float/floatToRawIntBits
                                 (aget ^floats (aget ^objects block 1) index)))))))
    {:left (vec left) :right (vec right)}))

(defn- render-phrase
  "Play `events` through a fresh reference instrument on `target`."
  [target events]
  (render-live-phrase (ref/reference-synth {:target target}) events))

(defn- audible?
  "True when a channel has any non-zero sample."
  [bits]
  (boolean (some #(not (zero? (int %))) bits)))

(defn- bits->floats
  "Decode a channel's raw f32 bits into doubles."
  [bits]
  (mapv #(double (Float/intBitsToFloat (int %))) bits))

(defn- count-calls
  "How many times the fully-qualified `id` appears as a call head in a body."
  [form id]
  (cond
    (seq? form) (+ (if (= id (first form)) 1 0)
                   (reduce + (map #(count-calls % id) form)))
    (coll? form) (reduce + (map #(count-calls % id) form))
    :else 0))

;; ---------------------------------------------------------------------------
;; The instrument
;; ---------------------------------------------------------------------------

(deftest the-reference-instrument-renders-a-phrase
  (let [silent (render-phrase :interpreter [[:render 128]])
        held (render-phrase :interpreter [[:on 60 0.8] [:render 128] [:render 128]])]
    (is (not (audible? (:left silent))) "nothing sounds before a note")
    (is (audible? (:left held)) "a held note sounds")
    (is (audible? (:right held)) "and it reaches both channels")))

(deftest interpreter-and-jvm-agree-on-the-reference-instrument
  (let [phrase [[:on 60 0.8] [:render 64] [:render 64]
                [:controller 1 0.7] [:render 64]
                [:on 64 0.6] [:render 64]
                [:off 60] [:render 64] [:off 64] [:render 64]]
        expected (render-phrase :interpreter phrase)
        actual (render-phrase :jvm phrase)]
    (is (audible? (:left expected))
        "the phrase has audio, so the comparison is not vacuous")
    (is (= expected actual))))

(deftest a-full-pool-steals-a-voice
  (let [instrument (ref/reference-synth {:target :interpreter})
        capacity (:capacity instrument)]
    (dotimes [offset (inc capacity)]
      (ref/note-on! instrument (+ 60 offset) 0.8))
    (let [stats (pool/pool-stats (:pool instrument))]
      (is (= (inc capacity) (:note-ons stats)))
      (is (>= (:steals stats) 1) "the ninth note takes a voice from the first"))))

(deftest the-unison-bank-calls-three-detuned-copies
  (let [bank @#'omkamra.dsp.examples.reference-synth/oscillator-bank
        unison @#'omkamra.dsp.examples.reference-synth/unison-oscillator-bank
        id (graph/definition-id bank)]
    (is (= 3 (count-calls (:body unison) id))
        "unison emits one call per copy")
    (is (= 1 (count-calls (:body bank) 'omkamra.dsp.library.oscillator/saw-oscillator))
        "and the bank itself calls its saw once, so the count is real")))

;; ---------------------------------------------------------------------------
;; The modulation matrix
;; ---------------------------------------------------------------------------

(defn- matrix-run
  [events]
  (render-phrase :interpreter (vec (concat events [[:render 128] [:render 128]]))))

(deftest every-matrix-source-changes-the-render
  (testing "each source has a value that changes the audio"
    (doseq [[label a b]
            [[:velocity [[:on 60 0.2]] [[:on 60 1.0]]]
             [:note [[:on 48 0.8]] [[:on 72 0.8]]]
             [:mod-wheel [[:on 60 0.8]] [[:on 60 0.8] [:controller 1 0.9]]]
             [:controller-74 [[:on 60 0.8]] [[:on 60 0.8] [:controller 74 1.0]]]]]
      (is (audible? (:left (matrix-run a))) (str label " renders audio"))
      (is (not= (matrix-run a) (matrix-run b)) (str label " changes the render"))))
  (testing "and a repeat of the same events is identical, so the difference is the source's"
    (is (= (matrix-run [[:on 60 0.8] [:controller 1 0.9]])
           (matrix-run [[:on 60 0.8] [:controller 1 0.9]])))))

(deftest the-stereo-voice-pans
  (let [center (matrix-run [[:on 60 0.8]])
        left (matrix-run [[:on 60 0.8] [:pan 0 -1.0]])
        right (matrix-run [[:on 60 0.8] [:pan 0 1.0]])]
    (is (= (:left center) (:right center))
        "a centred voice reaches both channels equally")
    (is (audible? (:left left)))
    (is (not (audible? (:right left))) "hard left is silent on the right")
    (is (not (audible? (:left right))) "hard right is silent on the left")
    (is (audible? (:right right)))))

;; ---------------------------------------------------------------------------
;; The post-mix chain
;; ---------------------------------------------------------------------------

(deftest the-post-mix-chain-changes-the-output
  (let [phrase [[:on 60 0.8] [:render 128] [:render 128]]
        with-chain (render-phrase :interpreter phrase)
        without-chain (render-live-phrase
                       (ref/build-instrument (inst/instrument ref/description)
                                             {:target :interpreter})
                       phrase)]
    (is (audible? (:left with-chain)))
    (is (audible? (:left without-chain)))
    (is (not= with-chain without-chain)
        "the post-mix effects are audible against the same voice without them")))

(deftest a-post-mix-control-changes-the-output
  (let [dry (render-phrase :interpreter
                           [[:on 60 0.8] [:effect 'chorus-mix 0.0]
                            [:render 128] [:render 128]])
        wet (render-phrase :interpreter
                           [[:on 60 0.8] [:effect 'chorus-mix 1.0]
                            [:render 128] [:render 128]])]
    (is (audible? (:left dry)))
    (is (not= dry wet) "the chorus mix changes the render")
    (testing "and the same setting reproduces it, so the difference is the control's"
      (is (= wet (render-phrase :interpreter
                                [[:on 60 0.8] [:effect 'chorus-mix 1.0]
                                 [:render 128] [:render 128]]))))))

(deftest the-post-mix-chain-is-one-instance-per-channel
  (let [synth (ref/reference-synth {:target :interpreter})]
    (is (= 2 (count (:buses synth)))
        "a stereo voice gets one bus instance per channel")
    (is (= 7 (count (:bus-control-names synth)))
        "the chain exposes the effects' global controls")))

(deftest interpreter-and-jvm-agree-with-post-mix-controls
  (let [phrase [[:on 60 0.8]
                [:effect 'chorus-mix 0.9] [:effect 'drive 3.0]
                [:render 128] [:render 64]
                [:off 60] [:render 128]]
        expected (render-phrase :interpreter phrase)
        actual (render-phrase :jvm phrase)]
    (is (audible? (:left expected)))
    (is (= expected actual))))

;; ---------------------------------------------------------------------------
;; Determinism and real-time behavior
;; ---------------------------------------------------------------------------

(deftest block-size-does-not-change-the-render
  (let [phrase (fn [block]
                 (vec (concat [[:on 60 0.8] [:controller 74 0.5]]
                              (repeat 4 [:render block]))))
        whole (render-phrase :interpreter (phrase 64))
        split (render-phrase :interpreter
                             (vec (concat [[:on 60 0.8] [:controller 74 0.5]]
                                          (repeat 8 [:render 32]))))]
    (is (audible? (:left whole)))
    (is (= whole split))))

(deftest the-filter-stays-stable-under-extreme-modulation
  ;; Drive every modulation source to its extreme and let the filter, envelopes,
  ;; and delay ring: nothing may go non-finite or run away. The voice clamps the
  ;; SVF cutoff below a sixth of the sample rate and its damping above zero,
  ;; which is the stability bound; this is the end-to-end version of that.
  (let [phrase [[:on 60 1.0]
                [:controller 1 1.0]
                [:controller 74 1.0]
                [:render 128] [:render 128] [:render 128] [:render 128]
                [:on 72 1.0] [:render 128] [:render 128]
                [:on 84 1.0] [:render 128]
                [:off 60] [:off 72] [:off 84]
                [:render 128] [:render 128] [:render 128] [:render 128]]
        {:keys [left right]} (render-phrase :interpreter phrase)
        values (concat (bits->floats left) (bits->floats right))]
    (is (audible? left))
    (is (every? #(Double/isFinite (double %)) values)
        "extreme modulation must not produce a non-finite sample")
    (is (< (reduce max 0.0 (map #(Math/abs (double %)) values)) 8.0)
        "and must not run away")))

(deftest the-reference-voice-smoothes-its-modulated-controls
  ;; Zipper noise is a discontinuity when a control steps at a block boundary.
  ;; The voice declares per-frame smoothing (D27) on every *continuous* control
  ;; the matrix modulates, so a block-rate change becomes a ramp inside the
  ;; kernel; the ramp mechanism itself is tested by the automation suite.
  (let [controls (get-in @#'omkamra.dsp.examples.reference-synth/reference-voice
                         [:process :controls])]
    (doseq [target [:cutoff :resonance :pulse-width]]
      (let [control (first (filter #(= (name target) (name (:name %))) controls))]
        (is (:smooth control) (str target " declares per-frame smoothing"))))
    (is (nil? (:smooth (first (filter #(= "pan" (name (:name %))) controls))))
        "pan is host-set and left bare, so a hard pan reaches silence")))

(deftest the-reference-instrument-does-not-allocate-in-the-audio-path
  ;; The same discipline as the P10 pool check: the bound guards the class of
  ;; reflective and boxing mistakes that cost kilobytes per call, and the
  ;; negative control shows it has teeth. This one adds the matrix loop on top
  ;; of the pool, so it is the whole host path.
  (if-let [meter (thread-allocation-meter)]
    (let [instrument (ref/reference-synth {:target :jvm})
          block (planar-output 128)
          cycle! (fn []
                   (ref/note-on! instrument 60 0.8)
                   (ref/render! instrument block 128)
                   (ref/note-off! instrument 60)
                   (ref/render! instrument block 128))
          _ (dotimes [_ 2000] (cycle!))
          iterations 1000
          bytes (allocated-bytes meter (fn [] (dotimes [_ iterations] (cycle!))))
          ;; The whole host path per cycle, measured at ~1.2 KB; the allowance is
          ;; deliberately below the class of mistake it guards: the reflective
          ;; planar `aget` this test caught was ~390 KB per cycle.
          allowance (* 4096 iterations)]
      (is (<= bytes allowance)
          (str "allocated " bytes " bytes over " iterations " cycles ("
               (double (/ bytes (double iterations))) " per cycle)"))
      (testing "and the bound has teeth: the interpreter host is far above it"
        (let [reference (ref/reference-synth {:target :interpreter})
              ;; The interpreter's IR evaluation allocates gigabytes per cycle,
              ;; so a couple of cycles already dwarf the allowance; running more
              ;; only makes the suite slow.
              _ (dotimes [_ 2]
                  (ref/note-on! reference 60 0.8)
                  (ref/render! reference block 128)
                  (ref/note-off! reference 60)
                  (ref/render! reference block 128))
              reference-bytes (allocated-bytes
                               meter
                               (fn []
                                 (dotimes [_ 2]
                                   (ref/note-on! reference 60 0.8)
                                   (ref/render! reference block 128)
                                   (ref/note-off! reference 60)
                                   (ref/render! reference block 128))))]
          (is (> reference-bytes allowance)
              (str "the interpreter host allocates " reference-bytes
                   " bytes, which must exceed the " allowance " byte allowance")))))
    (is true "thread allocation measurement is unavailable on this JVM")))
