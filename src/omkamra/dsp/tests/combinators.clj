(ns omkamra.dsp.tests.combinators
  "Tests for `omkamra.dsp.combinators` and the definition-value substrate.

  The hand-written equivalent of each combinator is the structural oracle, and
  the interpreter and the JVM must agree bit for bit."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.examples.demo-synth :as demo]
            [omkamra.dsp.harness.fixtures :as fixtures]))

(deftest a-call-emitting-transform-links-an-anonymous-value
  ;; D37: an anonymous `dsp/fn` value has no Var for the call that `unison`
  ;; emits to resolve, so the combinator carries the original in
  ;; `:dsp/dependencies` and `link` supplies it — the derived value compiles and
  ;; runs on its own.
  (let [voice (dsp/fn [note detune]
                {:process {:input false :controls [{:name note} {:name detune}]}}
                (+ note detune))
        id (symbol "omkamra.dsp.tests.combinators" (name (:name voice)))
        transformed (comb/unison voice {:count 3 :detune 20.0})
        render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 16)]
                   (dsp/process! artifact {:output output :frames 16
                                           :sample-rate 48000.0
                                           :controls [60.0 0.0]})
                   (mapv #(Float/floatToRawIntBits (float %)) output)))
        unit (dsp/link transformed)]
    (is (= {id voice} (:dsp/dependencies transformed))
        "it records the original under its fully-qualified id")
    (is (= 2 (count (:definitions unit)))
        "the derived value carries its original into the linked unit")
    (doseq [target [:interpreter :jvm]]
      (is (= (vec (repeat 16 (Float/floatToRawIntBits (float 180.0))))
             (render transformed target))
          (str "and it renders the summed copies on " target)))))

(deftest transforms-compose-and-carry-dependencies-transitively
  ;; D37: a transform of a transform still links, because `link` walks
  ;; `:dsp/dependencies` transitively, and a body-rewriting transform keeps the
  ;; wrapped voice's control metadata.
  (let [voice (dsp/fn [note detune]
                {:process {:input false :controls [{:name note} {:name detune}]}}
                (+ note detune))
        render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 4)]
                   (dsp/process! artifact {:output output :frames 4
                                           :sample-rate 48000.0
                                           :controls [60.0 0.0]})
                   (mapv #(Float/floatToRawIntBits (float %)) output)))]
    (testing "smooth then unison"
      (let [value (-> voice (comb/smooth :detune 0.01) (comb/unison {:count 2 :detune 10.0}))]
        (is (= 2 (count (:definitions (dsp/link value)))))
        (is (= [{:name 'note} {:name 'detune :smooth 0.01}]
               (get-in value [:process :controls]))
            "the wrapped voice keeps its control mode")
        (doseq [target [:interpreter :jvm]]
          (is (= (vec (repeat 4 (Float/floatToRawIntBits (float 120.0))))
                 (render value target))))))
    (testing "unison then unison keeps the whole chain"
      (let [value (-> voice (comb/unison {:count 2 :detune 10.0})
                      (comb/unison {:count 2 :detune 4.0}))]
        (is (= 3 (count (:definitions (dsp/link value))))
            "both derived calls and the original travel with the value")
        (doseq [target [:interpreter :jvm]]
          (is (= (vec (repeat 4 (Float/floatToRawIntBits (float 240.0))))
                 (render value target))))))))

(deftest a-through-transform-equals-hand-wiring
  ;; D36/D37: `through` pipes the voice through a module whose extra parameters
  ;; are literals, so the wrapper keeps the voice's interface and the module's
  ;; state is per call site (D19) — the hand-wired voice is the oracle.
  (let [render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 256)]
                   (dsp/process! artifact {:output output :frames 256
                                           :sample-rate 48000.0
                                           :controls [1.0 60.0 0.0]})
                   (mapv #(Float/floatToRawIntBits (float %)) output)))]
    (doseq [target [:interpreter :jvm]]
      (is (= (render demo/damped-saw-by-hand target)
             (render demo/damped-saw target))
          (str "the through transform matches hand wiring on " target)))
    (is (not= (render demo/damped-saw :jvm) (render demo/saw-tone :jvm))
        "and the effect changes the signal")))

(deftest a-mix-transform-builds-a-three-oscillator-synth
  ;; `mix` sums two voices with the same parameters; chaining it over three tones
  ;; is exactly the hand-written `dsp/defn` that calls all three and adds them.
  (let [render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 256)]
                   (dsp/process! artifact {:output output :frames 256
                                           :sample-rate 48000.0
                                           :controls [1.0 60.0 0.0]})
                   (mapv #(Float/floatToRawIntBits (float %)) output)))]
    (doseq [target [:interpreter :jvm]]
      (is (= (render demo/three-oscillator-by-hand target)
             (render demo/three-oscillator target))
          (str "mix matches the hand-written synth on " target))
      (is (= (render demo/three-oscillator target)
             (render demo/three-oscillator-by-sum target))
          (str "and reducing over mix agrees on " target)))
    (is (not= (render demo/three-oscillator :jvm) (render demo/saw-tone :jvm))
        "and the mix is more than one tone")))

(deftest a-fixed-n-poly-voice-has-independent-slots
  ;; D38: `poly` is the static half of polyphony — `count` call sites, with a
  ;; per-slot copy of only the per-slot controls — and the hand-written chord is
  ;; the oracle.
  (let [render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 512)]
                   (dsp/process! artifact {:output output :frames 512
                                           :sample-rate 48000.0
                                           ;; detune, then (gate, note) per slot
                                           :controls [0.0 1.0 60.0
                                                      1.0 64.0
                                                      1.0 67.0]})
                   (mapv #(Float/floatToRawIntBits (float %)) output)))]
    (doseq [target [:interpreter :jvm]]
      (is (= (render demo/three-voice-poly-by-hand target)
             (render demo/three-voice-poly target))
          (str "the fixed-N poly voice matches the hand-written chord on " target)))))

(deftest a-share-combinator-evaluates-a-voice-once
  ;; D38: sharing is a `let`. The signal is unchanged, but the duplicated voice
  ;; owns two state partitions instead of one — which is what the combinator
  ;; buys.
  (let [render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       output (float-array 256)]
                   (dsp/process! artifact {:output output :frames 256
                                           :sample-rate 48000.0
                                           :controls [1.0 60.0 0.0]})
                   {:output (mapv #(Float/floatToRawIntBits (float %)) output)
                    :state (:state-size (:ir artifact))}))
        shared (render demo/shared-dry-wet :interpreter)
        hand (render demo/shared-dry-wet-by-hand :interpreter)
        duplicated (render demo/duplicated-dry-wet-by-hand :interpreter)]
    (is (= hand shared) "the shared form matches its hand-written oracle")
    (is (= (:output shared) (:output duplicated))
        "duplicating the voice does not change the signal")
    (is (< (:state shared) (:state duplicated))
        "but sharing owns fewer state slots")
    (doseq [target [:interpreter :jvm]]
      (is (= (:output (render demo/shared-dry-wet-by-hand target))
             (:output (render demo/shared-dry-wet target)))
          (str "share matches hand wiring on " target)))))

(deftest control-transforms-match-the-inline-declaration
  ;; `smooth`/`slew` set exactly the metadata an inline `:smooth`/`:slew`
  ;; declares, so `descriptor/normalize` desugars both identically — and they
  ;; work on anonymous `dsp/fn` values, because they only edit metadata.
  (let [plain (dsp/fn [sample amount] {:process {:controls [{:name amount :default 0.0}]}}
                (* sample amount))
        inline-smooth (dsp/fn [sample amount]
                        {:process {:controls [{:name amount :smooth 0.01 :default 0.0}]}}
                        (* sample amount))
        inline-slew (dsp/fn [sample amount]
                      {:process {:controls [{:name amount :slew 0.01 :default 0.0}]}}
                      (* sample amount))
        render (fn [value target]
                 (let [artifact (dsp/compile value {:target target :entry :process})
                       frames 256
                       input (float-array (repeat frames 1.0))
                       output (float-array frames)]
                   (dsp/process! artifact {:input input :output output :frames frames
                                           :sample-rate 48000.0 :controls [1.0]})
                   (vec output)))]
    (doseq [target [:interpreter :jvm]]
      (is (= (render inline-smooth target) (render (comb/smooth plain :amount 0.01) target))
          (str "smooth matches the inline declaration on " target))
      (is (= (render inline-slew target) (render (comb/slew plain :amount 0.01) target))
          (str "slew matches the inline declaration on " target)))
    (testing "and the transforms really change the output"
      (is (= (vec (repeat 256 1.0)) (render plain :jvm))
          "the plain gain moves instantly")
      (is (not= (render plain :jvm) (render (comb/smooth plain :amount 0.01) :jvm))))
    (testing "a control the voice does not have is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such control"
                            (comb/smooth plain :cutoff 0.01))))))

(deftest poly-carries-a-voices-control-declarations
  ;; The interface of a polyphonic voice is the voice's interface with the
  ;; per-slot controls expanded, so a declaration has to travel with the name: a
  ;; shared control that arrived as an ordinary scalar, or a default that vanished,
  ;; would make a fused voice a different voice (D50, D52).
  (testing "a shared control stays shared, and is never expanded per slot"
    (let [fused (comb/poly (deref #'fixtures/shared-gain) {:count 2 :per-slot []})]
      (is (= '[amount] (mapv :name (get-in fused [:process :controls]))))
      (is (true? (:shared (first (get-in fused [:process :controls])))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (comb/poly (deref #'fixtures/shared-gain)
                              {:count 2 :per-slot [:amount]}))
          "a shared control is one region for the whole process")))
  (testing "a lane cannot be polyphonic at all"
    (is (thrown? clojure.lang.ExceptionInfo
                 (comb/poly (deref #'fixtures/lane-on-frequency)
                            {:count 2 :per-slot [:frequency]}))
        "a lane belongs to the entry process"))
  (testing "and an undeclared control is still just a name"
    (let [fused (comb/poly (deref #'fixtures/sine-voice)
                           {:count 2 :per-slot [:gate :frequency]})]
      (is (= '[attack decay sustain release gate_0 frequency_0 gate_1 frequency_1]
             (mapv :name (get-in fused [:process :controls])))))))

;; ---------------------------------------------------------------------------
;; Per-slot skipping (M6b)
;; ---------------------------------------------------------------------------

(dsp/defn free-running-voice
  "A generator whose output is its own oscillator, so whether a call site was
  evaluated at all is visible even while its gate is shut: a skipped slot
  contributes nothing where the plain voice keeps advancing (M6b)."
  [gate frequency]
  {:process {:input false}}
  (omkamra.dsp.library.oscillator/sine-oscillator frequency))

(defn- one-slot-artifact [voice target]
  (dsp/compile (dsp/link (comb/poly voice {:count 1 :per-slot [:gate :frequency]}))
               {:target target :precision :f32 :entry :process}))

(defn- run-gates [artifact gates]
  (let [output (float-array 8)]
    (mapv (fn [gate]
            (dsp/process! artifact {:output output :frames (int 8)
                                    :sample-rate 48000.0 :controls [gate 440.0]})
            (vec output))
          gates)))

(defn- peak [samples]
  (reduce max 0.0 (map (fn [x] (Math/abs (double x))) samples)))

(deftest a-guard-skips-a-slot-that-has-never-been-gated
  ;; The whole point of the guard: a call site whose gate has never opened is not
  ;; evaluated, so a fused slot a pool has not allocated costs a comparison. The
  ;; unguarded voice is the negative twin -- it advances with the gate shut. The
  ;; interpreter is the oracle, so both hosts must agree.
  (doseq [target [:interpreter :jvm]]
    (let [plain (one-slot-artifact (deref #'free-running-voice) target)
          guarded (one-slot-artifact (comb/guard (deref #'free-running-voice)) target)]
      (when (= :jvm target)
        (is (= 1 (count (:partitions guarded)))
            "the guard keeps the slot's own partition, which a reset clears"))
      (is (pos? (peak (first (run-gates plain [0.0]))))
          (str (name target) ": the plain voice advances even with its gate shut"))
      (is (zero? (peak (first (run-gates guarded [0.0]))))
          (str (name target) ": the guarded voice is not evaluated until gated")))))

(deftest a-guard-latches-through-a-release
  ;; The latch is the difference between a gate check and a *correct* gate check:
  ;; a pool closes the gate to release a voice, so a slot that has sounded must
  ;; keep being evaluated while its tail runs. The unguarded voice is the oracle.
  (doseq [target [:interpreter :jvm]]
    (let [plain (one-slot-artifact (deref #'free-running-voice) target)
          guarded (one-slot-artifact (comb/guard (deref #'free-running-voice)) target)
          gates (concat (repeat 3 1.0) (repeat 8 0.0))]
      (is (= (run-gates plain gates) (run-gates guarded gates))
          (str (name target)
               ": closing the gate does not clear the latch, so the tail is identical"))
      (is (pos? (peak (last (run-gates guarded gates))))
          (str (name target) ": the guarded slot is still evaluated after the gate closes")))))

(deftest a-reset-clears-a-guard
  ;; Per-call-site ownership is what makes the latch resettable: a stolen or freed
  ;; slot is reset, so its guard starts closed again and the slot is skippable
  ;; once more.
  (doseq [target [:interpreter :jvm]]
    (let [plain (one-slot-artifact (deref #'free-running-voice) target)
          guarded (one-slot-artifact (comb/guard (deref #'free-running-voice)) target)]
      (run-gates guarded [1.0])
      (dsp/reset! guarded)
      (run-gates plain [1.0])
      (dsp/reset! plain)
      (is (zero? (peak (first (run-gates guarded [0.0]))))
          (str (name target) ": after a reset the latch is clear and the slot skipped"))
      (is (pos? (peak (first (run-gates plain [0.0]))))
          (str (name target) ": where the unguarded voice keeps advancing")))))

(deftest guard-needs-a-gate-control
  (let [voice (dsp/fn [frequency] {:process {:input false}} frequency)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"gate control"
                          (comb/guard voice)))))
