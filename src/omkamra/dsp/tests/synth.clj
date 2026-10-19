(ns omkamra.dsp.tests.synth
  "The host-side synth runtime: description, voice pool, allocation, lifecycle
  (P10).

  Every check here is about the *runtime*, not about DSP code, so the oracle is
  the reference interpreter hosting the same pool: a pool's output has to equal
  what the same events produce through the portable interpreter, sample for
  sample. Where a property is stated rather than compared (allocation order,
  stealing policy, block-size invariance) it carries a negative twin, so it
  cannot pass vacuously."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.examples.demo-synth :as demo]
            [omkamra.dsp.library.filter :as filter]
            [omkamra.dsp.synth :as synth]
            [omkamra.dsp.hosts :as host]
            [omkamra.dsp.synth.pool :as pool]
            [omkamra.dsp.harness.fixtures :as fixtures
             :refer [allocated-bytes
                     containment-loud-description
                     containment-reset-description
                     containment-runaway-description
                     lane-bend-description
                     lane-frequency-description
                     lane-only-description
                     lane-voice-description
                     shared-voice-description
                     sine-voice-description
                     thread-allocation-meter]]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- compiled
  "Compile a description. The second argument is *compile options* (`:target`,
  `:precision`); to change the synth itself, `assoc` into the description."
  ([description] (compiled description {}))
  ([description compile-options]
   (host/compile-synth description compile-options)))

(defn- pool-for
  [description]
  (host/synth-pool (compiled description)))

(deftest a-pool-delivers-a-shared-lane-to-every-voice
  ;; A pool lane is the process's, not a voice's: one region, written once, read
  ;; by every sounding voice. The pool must hand it to the kernel and the kernel
  ;; to the generated process, or the lane silently reads nothing.
  (let [bundle (compiled lane-voice-description)
        render (fn [target]
                 (let [pool (host/synth-pool (compiled lane-voice-description
                                                       {:target target}))
                       output (float-array 4)]
                   (pool/set-lane! pool 'amount (float-array [1.0 2.0 3.0 4.0]))
                   (pool/note-on! pool 60 1.0)
                   (pool/render! pool output 4 48000.0)
                   (vec output)))]
    (testing "the composed value is the scalar plus the per-frame lane"
      (is (= [11.0 12.0 13.0 14.0] (render :jvm)))
      (is (= [11.0 12.0 13.0 14.0] (render :interpreter))
          "the two hosts agree"))
    (testing "a lane nobody wrote leaves the scalar standing"
      (let [pool (host/synth-pool bundle)
            output (float-array 4)]
        (pool/note-on! pool 60 1.0)
        (pool/render! pool output 4 48000.0)
        (is (= [10.0 10.0 10.0 10.0] (vec output))
            "an untouched lane reads zero, so the composed value is the scalar")))
    (testing "a shared lane reaches voices that are not sounding yet"
      ;; Every voice has its own region (M4c), so the shared setter has to reach
      ;; all of them: a voice nobody has started still has to read the curve
      ;; when it does.
      (let [pool (host/synth-pool bundle)
            output (float-array 4)]
        (pool/set-lane! pool 'amount (float-array [1.0 1.0 1.0 1.0]))
        (pool/note-on! pool 60 1.0)
        (pool/note-on! pool 64 1.0)
        (pool/render! pool output 4 48000.0)
        (is (= [22.0 22.0 22.0 22.0] (vec output))
            "two voices, each reading 10 + 1")))
    (testing "the lane is addressed by name, however it is spelled"
      (let [pool (host/synth-pool bundle)
            output (float-array 4)]
        (pool/set-lane! pool "amount" (float-array [0.5 0.5 0.5 0.5]))
        (pool/note-on! pool 60 1.0)
        (pool/render! pool output 4 48000.0)
        (is (= [10.5 10.5 10.5 10.5] (vec output)))))
    (testing "a name the voice does not declare is refused"
      (let [pool (host/synth-pool bundle)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such lane"
                              (pool/set-lane! pool 'nope (float-array [1.0]))))))
    (testing "a per-voice lane wants the per-voice setter"
      (let [per-voice (host/synth-pool
                       (compiled (assoc-in lane-voice-description
                                           [:parameters :amount :scope] :per-voice)))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not shared"
                              (pool/set-lane! per-voice 'amount (float-array [1.0]))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is shared"
                              (pool/set-voice-lane! (host/synth-pool bundle) 0 'amount
                                                    (float-array [1.0]))))))))

(deftest a-pool-delivers-a-per-voice-lane-to-each-voice
  ;; A per-voice lane is per voice for real: every voice has its own region, so
  ;; two voices sounding one block read two different curves. The scope is the
  ;; control's, so nothing but a `:per-voice` parameter is needed (M4c).
  (let [description (assoc-in lane-voice-description
                              [:parameters :amount :scope] :per-voice)
        render (fn [target]
                 (let [pool (host/synth-pool (compiled description {:target target}))
                       output (float-array 4)]
                   (pool/set-voice-lane! pool 0 'amount (float-array [1.0 2.0 3.0 4.0]))
                   (pool/set-voice-lane! pool 1 'amount (float-array [10.0 10.0 10.0 10.0]))
                   (pool/note-on! pool 60 1.0)
                   (pool/note-on! pool 64 1.0)
                   (pool/render! pool output 4 48000.0)
                   (vec output)))]
    (testing "each voice reads its own lane, and the block is their sum"
      (is (= [31.0 32.0 33.0 34.0] (render :jvm))
          "(10+1)+(10+10) through (10+4)+(10+10)")
      (is (= [31.0 32.0 33.0 34.0] (render :interpreter))
          "the two hosts agree"))
    (testing "a voice whose lane nobody wrote keeps its own scalar"
      (let [pool (host/synth-pool (compiled description))
            output (float-array 4)]
        (pool/set-voice-lane! pool 0 'amount (float-array [1.0 1.0 1.0 1.0]))
        (pool/note-on! pool 60 1.0)
        (pool/note-on! pool 64 1.0)
        (pool/render! pool output 4 48000.0)
        (is (= [21.0 21.0 21.0 21.0] (vec output))
            "11 from voice 0 and 10 from voice 1")))
    (testing "a lane on a runtime control is per voice without a declaration"
      ;; `frequency` is not in the control plan's parameter list — the pool
      ;; resolves it per voice — so a lane beside it is per voice too.
      (let [pool (host/synth-pool (compiled lane-frequency-description))
            output (float-array 4)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not shared"
                              (pool/set-lane! pool 'frequency (float-array [1.0]))))
        (pool/set-voice-lane! pool 0 'frequency (float-array [1.0 2.0 3.0 4.0]))
        (pool/note-on! pool 60 1.0)
        (pool/render! pool output 4 48000.0)
        (let [values (mapv double (vec output))]
          (is (apply < values) "the lane advances the voice's own frequency")
          (is (every? #(< 261.0 % 266.0) values)
              "the note's 261.6 Hz plus a lane of 1..4, not something else"))))
    (testing "a per-channel control's lane has no delivery, so it is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"per-channel"
                            (host/synth-pool (compiled lane-bend-description)))))
    (testing "a lane-only control is per voice unless a parameter says shared"
      (let [pool (host/synth-pool (compiled lane-only-description))
            output (float-array 4)]
        (pool/set-voice-lane! pool 0 'amount (float-array [0.5 0.5 0.5 0.5]))
        (pool/note-on! pool 60 1.0)
        (pool/render! pool output 4 48000.0)
        (is (every? #(not (zero? %)) (vec output))
            "the lane reached a control that has no scalar at all"))
      (let [shared (host/synth-pool
                    (compiled (assoc-in lane-only-description
                                        [:parameters] {:amount {:scope :shared}})))
            output (float-array 4)]
        (pool/set-lane! shared 'amount (float-array [0.5 0.5 0.5 0.5]))
        (pool/note-on! shared 60 1.0)
        (pool/render! shared output 4 48000.0)
        (is (every? #(not (zero? %)) (vec output))
            "a declared :shared lane-only control takes the shared setter")))
    (testing "a lane knows nothing about a voice the pool does not have"
      (let [pool (host/synth-pool (compiled description))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no voice at that index"
                              (pool/set-voice-lane! pool 2 'amount (float-array [1.0]))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such lane"
                              (pool/set-voice-lane! pool 0 'nope (float-array [1.0]))))))))

(defn- render-bits
  "Play `events` into a fresh pool and return every rendered sample as raw bits.

  Bits rather than numbers, because the comparison this suite makes is exact:
  raw bits are NaN-equal as NaN and distinguish +0.0 from -0.0, the same
  discipline the conformance harness uses."
  [bundle events frames]
  (let [pool (host/synth-pool bundle)
        capacity (pool/pool-max-block-frames pool)
        f64? (= :f64 (:precision (:artifact bundle)))
        output (if f64? (double-array capacity) (float-array capacity))
        bits (java.util.ArrayList.)]
    (doseq [[event payload] events]
      (case event
        :on (pool/note-on! pool (:note payload)
                           (get payload :velocity 1.0) (get payload :channel 0))
        :off (pool/note-off! pool (:note payload) (get payload :channel 0))
        :render (do (pool/render! pool output frames 48000.0)
                    (if f64?
                      (dotimes [index frames]
                        (.add bits (Double/doubleToRawLongBits (aget ^doubles output index))))
                      (dotimes [index frames]
                        (.add bits (Float/floatToRawIntBits (aget ^floats output index))))))))
    (vec bits)))

(defn- bits-add
  "The bit pattern of adding two f32 samples, in the order the pool mixes."
  [a b]
  (Float/floatToRawIntBits
   (float (+ (Float/intBitsToFloat (int a)) (Float/intBitsToFloat (int b))))))

(defn- states
  [pool]
  (mapv :state (pool/voice-snapshot pool)))

;; ---------------------------------------------------------------------------
;; Description and tuning
;; ---------------------------------------------------------------------------

(deftest note-number-tuning-is-equal-temperament
  (let [tuning synth/default-tuning]
    (is (= 440.0 (synth/note->frequency 69 tuning)))
    (is (= 220.0 (synth/note->frequency 57 tuning)))
    (is (= 880.0 (synth/note->frequency 81 tuning)))
    (is (< (Math/abs (- 261.6255653005986 (synth/note->frequency 60 tuning))) 1.0e-9))
    (testing "and the tuning is whatever the description says it is"
      (is (not= (synth/note->frequency 60 tuning)
                (synth/note->frequency 60 {:reference 432.0 :reference-note 69}))))
    (testing "neighbouring notes differ, so a table of constants cannot pass"
      (is (not= (synth/note->frequency 60 tuning)
                (synth/note->frequency 61 tuning))))))

(deftest description-validation-refuses-nonsense-and-is-idempotent
  (let [description (assoc sine-voice-description :id :validated)
        normalized (synth/normalize-description description)]
    (testing "a valid description normalises, and normalising again changes nothing"
      (is (= normalized (synth/normalize-description normalized)))
      (is (= 4 (:capacity normalized)))
      (is (= :poly (:mode normalized)))
      (is (= synth/default-held-notes (:held-notes normalized)))
      (is (= :oldest (:stealing normalized)))
      (is (= synth/default-tuning (:tuning normalized)))
      (is (= [:attack :decay :release :sustain] (sort (keys (:parameters normalized)))))
      (is (= {} (get-in normalized [:voice :bindings])))
      ;; the definition is a symbol, which is what makes a description portable
      (is (symbol? (get-in normalized [:voice :definition]))))
    (testing "every malformed field is refused, and named in the message"
      (doseq [[label fragment malformed]
              [["zero capacity" #"capacity must be a positive"
                {:capacity 0}]
               ["absurd capacity" #"at most the maximum"
                {:capacity (inc synth/max-capacity)}]
               ["unsupported mode" #"voice mode is not supported"
                {:mode :paraphonic}]
               ["monophonic mode with more than one voice" #"requires exactly one voice"
                {:mode :mono}]
               ["absurd held-note capacity" #"held-note capacity"
                {:held-notes (inc synth/max-held-notes)}]
               ["unsupported stealing" #"stealing policy is not supported"
                {:stealing :loudest}]
               ["unsupported channel count" #"output channels must be 1 or 2"
                {:channels 3}]
               ["unknown top-level key" #"unknown keys"
                {:capcity 4}]
               ["zero block size" #"maximum block size"
                {:max-block-frames 0}]
               ["missing definition" #"must name a definition"
                {:voice {:definition nil}}]
               ["definition that is not a name" #"must be a Var or a qualified symbol"
                {:voice {:definition 42}}]
               ["bindings that are not a map" #"bindings must be a map"
                {:voice {:definition #'filter/filter-voice :bindings []}}]
               ["a binding that is not a name" #"must name a parameter"
                {:voice {:definition #'filter/filter-voice :bindings {:gate 42}}}]
               ["an unsupported parameter scope" #"scope is not supported"
                {:parameters {:cutoff {:scope :global}}}]
               ["a default below its minimum" #"below its minimum"
                {:parameters {:cutoff {:default 1.0 :min 2.0}}}]
               ["an enum that is not a set of numbers" #"enum must be a non-empty set"
                {:parameters {:lfo-shape {:enum [0 1]}}}]
               ["a parameter name the runtime reserves" #"reserved by the runtime"
                {:parameters {:gate {:default 1.0}}}]
               ["a parameter key the schema does not define" #"unknown keys"
                {:parameters {:cutoff {:bogus 1.0}}}]]]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo fragment
                              (synth/normalize-description (merge description malformed)))
            label)))))

(deftest control-plan-follows-the-abi-not-the-declaration-order
  (let [abi-controls (:controls (:abi (:artifact (compiled sine-voice-description))))
        names (mapv (comp name :name) abi-controls)
        plan (synth/control-plan (synth/normalize-description sine-voice-description)
                                 abi-controls)]
    (testing "the plan is one entry per ABI control, in ABI order"
      (is (= 6 (count plan)))
      (is (= names (mapv (comp name :control) plan))
          "a plan index must be an ABI index"))
    (testing "the runtime's own sources bind without a declaration"
      (is (= {:kind :runtime :index 0}
             (:storage (first (filter #(= "gate" (:control %)) plan)))))
      (is (= :per-voice (get-in (first (filter #(= "attack" (:control %)) plan))
                                [:storage :kind]))))
    (testing "and the declaration's own order does not move it"
      (let [reversed (synth/normalize-description
                      (update sine-voice-description :parameters
                              (fn [m] (into (sorted-map-by (fn [a b]
                                                             (compare (str b) (str a))))
                                            m))))]
        (is (= names (mapv (comp name :control) (synth/control-plan reversed abi-controls))))))
    (testing "a binding that names an undeclared parameter is refused"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"does not declare"
           (synth/control-plan
            (synth/normalize-description
             (assoc-in sine-voice-description [:voice :bindings :release] :nope))
            abi-controls))))
    (testing "and a binding for a control the voice does not have is refused"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"does not have"
           (synth/control-plan
            (synth/normalize-description
             (assoc-in sine-voice-description [:voice :bindings :bogus] :attack))
            abi-controls))))))

(deftest a-voice-must-be-a-generator
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"must be a generator with no input port"
       (host/compile-synth
        {:id :not-a-voice
         :voice {:definition #'filter/filter-voice}})))
  ;; a positive twin: the generator voice compiles, and its ABI has no input
  (let [bundle (host/compile-synth sine-voice-description)]
    (is (= [:output] (mapv :id (get-in bundle [:artifact :abi :buffers]))))
    (is (false? (get-in bundle [:realtime :allocations?]))
        "a pool is only allowed over a kernel that reports no allocation")))

;; ---------------------------------------------------------------------------
;; Allocation and stealing
;; ---------------------------------------------------------------------------

(deftest allocation-takes-the-lowest-free-voice
  (let [pool (pool-for sine-voice-description)]
    (is (= [0 1 2 3] (mapv #(pool/note-on! pool %) [72 60 64 76])))
    (is (= [:active :active :active :active] (states pool)))
    (is (= 4 (:high-water (pool/pool-stats pool))))))

(deftest stealing-is-deterministic-and-follows-the-declared-policy
  ;; The note order is chosen so that the three policies disagree: the oldest
  ;; note-on is the highest note, the lowest is second, the highest is last.
  (let [notes [72 60 64 76]]
    (testing ":oldest gives up the voice whose note-on is oldest"
      (let [pool (pool-for (assoc sine-voice-description :stealing :oldest))]
        (doseq [note notes] (pool/note-on! pool note))
        (is (= 0 (pool/note-on! pool 80)))))
    (testing ":lowest-note gives up the voice sounding the lowest note"
      (let [pool (pool-for (assoc sine-voice-description :stealing :lowest-note))]
        (doseq [note notes] (pool/note-on! pool note))
        (is (= 1 (pool/note-on! pool 80)))))
    (testing ":highest-note gives up the voice sounding the highest note"
      (let [pool (pool-for (assoc sine-voice-description :stealing :highest-note))]
        (doseq [note notes] (pool/note-on! pool note))
        (is (= 3 (pool/note-on! pool 80)))))
    (testing "a releasing voice is given up before any sounding one"
      (let [pool (pool-for (assoc sine-voice-description :stealing :lowest-note))]
        (doseq [note notes] (pool/note-on! pool note))
        (pool/note-off! pool 72)                ; voice 0, the oldest and highest
        ;; `:lowest-note` alone would take voice 1 (note 60); the release wins
        (is (= 0 (pool/note-on! pool 80)))))
    (testing "and the newest voice is never the one given up"
      (let [pool (pool-for (assoc sine-voice-description :stealing :oldest))]
        (doseq [note notes] (pool/note-on! pool note))
        (is (not= 3 (pool/note-on! pool 80)))))
    (testing "the same sequence always steals the same voice"
      (let [render (fn []
                     (let [pool (pool-for (assoc sine-voice-description :stealing :oldest))]
                       (mapv #(pool/note-on! pool %) (concat notes [80 81 82 83]))))]
        (is (= (render) (render)))
        (is (= [0 1 2 3 0 1 2 3] (render)))))))

(deftest stealing-resets-the-voice-it-takes
  ;; A stolen voice must be indistinguishable from a fresh one, or the output of
  ;; a sequence would depend on which voice happened to be taken, and when.
  (let [single (assoc sine-voice-description :capacity 1)
        render (fn [events]
                 (render-bits (compiled single) (concat events (repeat 2 [:render])) 64))
        fresh (render [[:on {:note 61}]])
        ;; Two blocks sound, then the steal, then two more blocks: the last 128
        ;; frames are what the stolen voice produced.
        after-steal (render [[:on {:note 60}] [:render] [:render] [:on {:note 61}]])]
    (is (= fresh (vec (drop 128 after-steal)))
        "the blocks after a steal match a voice that was fresh all along")
    (is (not= fresh (render [[:on {:note 62}]]))
        "and the comparison would notice if the pitches differed")))

(deftest note-off-releases-every-voice-playing-that-note
  (let [pool (pool-for sine-voice-description)]
    (pool/note-on! pool 60)
    (pool/note-on! pool 60)
    (pool/note-on! pool 64)
    (is (= [:active :active :active] (states pool)))
    (is (= 2 (pool/note-off! pool 60)) "both voices on the repeated note release")
    (is (= [:releasing :releasing :active] (states pool)))
    (testing "a release is not repeatable, and an unmatched note-off is ignored"
      (is (= 0 (pool/note-off! pool 60)))
      (is (= 0 (pool/note-off! pool 59)))
      (is (= [:releasing :releasing :active] (states pool))))
    (testing "all-notes-off releases the rest"
      (is (= 1 (pool/all-notes-off! pool)))
      (is (= [:releasing :releasing :releasing] (states pool))))
    (testing "and the counters say what happened"
      (let [stats (pool/pool-stats pool)]
        (is (= 3 (:note-ons stats)))
        (is (= 3 (:released stats)))
        (is (= 3 (:note-offs stats)))
        (is (= 3 (:high-water stats)))
        (is (zero? (:steals stats)))))))

(deftest reset-and-destroy-return-the-pool-to-a-known-state
  (let [pool (pool-for sine-voice-description)]
    (pool/note-on! pool 60)
    (pool/note-on! pool 64)
    (pool/render! pool (float-array 64) 64 48000.0)
    (is (nil? (pool/reset-pool! pool)))
    (is (empty? (pool/voice-snapshot pool)) "reset frees every voice")
    (is (= 0 (pool/note-on! pool 67)) "and the lowest voice is free again")
    (is (nil? (pool/destroy-pool! pool)))
    (testing "a destroyed pool refuses to be used"
      (doseq [[label call] [["note-on" #(pool/note-on! pool 60)]
                            ["note-off" #(pool/note-off! pool 60)]
                            ["render" #(pool/render! pool (float-array 8) 8 48000.0)]
                            ["reset" #(pool/reset-pool! pool)]]]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"has been destroyed" (call))
            label)))))

;; ---------------------------------------------------------------------------
;; Voice modes
;; ---------------------------------------------------------------------------

(defn- modes
  "The sounding voices of a pool, reduced to what a mode test asks about."
  [pool]
  (mapv #(select-keys % [:index :note :state]) (pool/voice-snapshot pool)))

(deftest mono-keeps-one-voice-and-has-no-fallback
  (let [pool (pool-for (assoc sine-voice-description :mode :mono :capacity 1))]
    (is (= [0 0 0] (mapv #(pool/note-on! pool %) [60 64 67]))
        "every note-on retriggers the one voice")
    (is (= [{:index 0 :note 67 :state :active}] (modes pool)))
    (testing "a note-off that is not the sounding note is ignored"
      (is (= 0 (pool/note-off! pool 60)))
      (is (= [{:index 0 :note 67 :state :active}] (modes pool))))
    (testing "releasing the sounding note releases the voice"
      (is (= 1 (pool/note-off! pool 67)))
      (is (= [:releasing] (mapv :state (modes pool)))))
    (testing "and `:mono` never falls back to a note it did not remember"
      (is (= 0 (pool/note-off! pool 60)))
      (is (= [:releasing] (mapv :state (modes pool)))))))

(deftest legato-falls-back-without-retriggering
  ;; The stack is [60 64 67] with 67 sounding; releasing a note *under* the
  ;; sounding one changes nothing, and the sounding one falls back to 60.
  (let [pool (pool-for (assoc sine-voice-description :mode :legato :capacity 1))]
    (doseq [note [60 64 67]] (pool/note-on! pool note))
    (testing "a release underneath the sounding note changes nothing"
      (is (= 0 (pool/note-off! pool 64)))
      (is (= [{:index 0 :note 67 :state :active}] (modes pool))))
    (testing "releasing the sounding note falls back, still sounding"
      (is (= 0 (pool/note-off! pool 67)))
      (is (= [{:index 0 :note 60 :state :active}] (modes pool))))
    (testing "and releasing the last held note releases the voice"
      (is (= 1 (pool/note-off! pool 60)))
      (is (= [:releasing] (mapv :state (modes pool)))))
    (testing "an unmatched note-off is still ignored"
      (is (= 0 (pool/note-off! pool 64)))
      (is (= [:releasing] (mapv :state (modes pool)))))))

(deftest legato-holds-the-envelope-where-retrigger-restarts-it
  (let [render (fn [mode]
                 (render-bits (compiled (assoc sine-voice-description :mode mode :capacity 1))
                              [[:on {:note 60}] [:render] [:on {:note 64}] [:render]] 64))
        legato (render :legato)
        retrigger (render :retrigger)
        sample (fn [block index] (Float/intBitsToFloat (int (nth block index))))]
    (is (not= legato retrigger) "the two modes really do differ")
    (testing "a retrigger restarts the envelope from zero"
      (is (< (Math/abs (double (sample retrigger 64))) 1.0e-3)))
    (testing "legato keeps its level, so the note change is audible only in pitch"
      (is (> (Math/abs (double (sample legato 64))) 1.0e-3)))
    (testing "and `:retrigger` keeps the same note stack as `:legato`"
      (let [pool (pool-for (assoc sine-voice-description :mode :retrigger :capacity 1))]
        (doseq [note [60 64 67]] (pool/note-on! pool note))
        (pool/note-off! pool 67)
        (is (= [{:index 0 :note 64 :state :active}] (modes pool)))))))

(deftest every-mode-renders-what-the-interpreter-renders
  (doseq [mode [:mono :legato :retrigger]]
    (let [events [[:on {:note 60}] [:render] [:on {:note 64}] [:render]
                  [:off {:note 64}] [:render] [:off {:note 60}] [:render]]
          description (assoc sine-voice-description :mode mode :capacity 1)]
      (is (= (render-bits (compiled description) events 64)
             (render-bits (compiled description {:target :interpreter}) events 64))
          (str mode " renders the same on the JVM and the interpreter")))))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(deftest a-pool-renders-what-the-interpreter-renders
  ;; The oracle for the whole runtime: the same events, through a JVM pool and
  ;; through the portable interpreter, must produce the same samples.
  (let [events (concat [[:on {:note 60 :velocity 1.0}]]
                       (repeat 3 [:render])
                       [[:off {:note 60}]]
                       (repeat 2 [:render])
                       [[:on {:note 64 :velocity 0.5}]]
                       (repeat 3 [:render]))]
    (is (= (render-bits (compiled sine-voice-description) events 64)
           (render-bits (compiled sine-voice-description {:target :interpreter}) events 64)))
    ;; negative twin: a different note really does produce different samples, so
    ;; the comparison above is not two silent renders agreeing with each other
    (is (not= (render-bits (compiled sine-voice-description) [[:on {:note 60}] [:render]] 64)
              (render-bits (compiled sine-voice-description) [[:on {:note 61}] [:render]] 64)))))

(deftest a-pool-mixes-its-voices-and-they-stay-independent
  (let [solo (fn [note]
               (render-bits (compiled sine-voice-description)
                            [[:on {:note note}] [:render] [:render]] 64))
        both (render-bits (compiled sine-voice-description)
                          [[:on {:note 60}] [:on {:note 64}] [:render] [:render]] 64)]
    (is (= both (mapv bits-add (solo 60) (solo 64)))
        "the mix is the sum of the same voices rendered alone")
    (is (not (every? zero? both)) "and it is not silence")))

(deftest rendering-is-invariant-under-block-size
  ;; The same two notes over the same 128 frames, partitioned two ways.
  (let [notes [[:on {:note 60 :velocity 1.0}] [:on {:note 64 :velocity 0.8}]]
        one (render-bits (compiled sine-voice-description) (concat notes [[:render]]) 128)
        two (render-bits (compiled sine-voice-description) (concat notes (repeat 2 [:render])) 64)]
    (is (= one two)
        "two 64-frame blocks must render what one 128-frame block renders")
    (is (not (every? zero? one)) "and the comparison is over real audio")))

(deftest render-refuses-a-block-larger-than-the-declared-maximum
  (let [pool (pool-for sine-voice-description)
        output (float-array 256)]
    (is (nil? (pool/render! pool output 128 48000.0)) "exactly the maximum is allowed")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"larger than its declared maximum"
                          (pool/render! pool output 129 48000.0)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"negative number of frames"
                          (pool/render! pool output -1 48000.0)))
    (is (nil? (pool/render! pool output 0 48000.0)) "an empty block is legal")))

(deftest a-pool-does-not-allocate-in-the-audio-path
  ;; What this guards is the whole class of reflective and boxing mistakes that
  ;; cost kilobytes per call: an unhinted `aset` is a reflective `RT.aset` call
  ;; at about 24 KB (D26), and one such call per control per note would show up
  ;; here immediately. The bound is therefore set *well* below that class rather
  ;; than at zero. Two things are known to remain, both bounded per block and
  ;; independent of block size: Clojure cannot implement a primitive-hinted
  ;; protocol method (`reify` and `deftype` both refuse), so the `frames` and
  ;; `sample-rate` arguments of the kernel seam box, and a `case` boxes its
  ;; result. Removing the last few hundred bytes would mean giving up the seam
  ;; that makes one pool drive any host.
  (if-let [meter (thread-allocation-meter)]
    (let [cycle! (fn [pool output]
                   (pool/note-on! pool 60 0.5)
                   (pool/render! pool output 128 48000.0)
                   (pool/note-off! pool 60)
                   (pool/render! pool output 128 48000.0))
          output (float-array 128)
          jvm-pool (pool-for sine-voice-description)
          _ (dotimes [_ 20000] (cycle! jvm-pool output))
          iterations 5000
          bytes (allocated-bytes meter (fn [] (dotimes [_ iterations] (cycle! jvm-pool output))))
          allowance (* 2048 iterations)]
      (is (<= bytes allowance)
          (str "allocated " bytes " bytes over " iterations
               " cycles (" (double (/ bytes (double iterations))) " per cycle)"))
      (testing "and the bound has teeth: the allocating reference path is far above it"
        (let [reference (host/synth-pool
                         (compiled sine-voice-description {:target :interpreter}))
              _ (dotimes [_ 50] (cycle! reference output))
              reference-bytes (allocated-bytes meter
                                               (fn [] (dotimes [_ 50] (cycle! reference output))))]
          (is (> reference-bytes allowance)
              (str "the interpreter host allocates " reference-bytes
                   " bytes, which must exceed the " allowance " byte allowance")))))
    (is true "thread allocation measurement is unavailable on this JVM")))

;; ---------------------------------------------------------------------------
;; The host control delivery and the example instrument
;; ---------------------------------------------------------------------------

(deftest a-host-can-publish-a-voices-controls
  ;; `set-voice-controls!` is the pool's one affordance for a host that computes
  ;; control values itself, such as a modulation layer. It is deliberately
  ;; unopinionated, so the check is only that the values reach the voice and that
  ;; the boundaries are enforced.
  (let [pool (pool-for sine-voice-description)
        values (double-array (pool/control-count pool))
        render (fn [] (let [output (float-array 128)]
                        (pool/render! pool output 128 48000.0)
                        (vec output)))]
    (is (= 6 (pool/control-count pool)) "one value per ABI control")
    (pool/note-on! pool 60)
    (let [from-plan (render)]
      (is (not (every? zero? from-plan)) "the plan's own values really sound")
      (pool/set-voice-controls! pool 0 values)
      (is (not= from-plan (render))
          "and a host's values reach the voice instead of them"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no voice at that index"
                          (pool/set-voice-controls! pool 4 values)))
    (pool/destroy-pool! pool)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"has been destroyed"
                          (pool/set-voice-controls! pool 0 values)))))

(deftest the-demo-synth-is-an-instrument-modulation-can-shape
  (let [render (fn render
                 ([configure] (render configure 0.8))
                 ([configure velocity]
                  (let [synth (demo/demo-synth)
                        output (float-array 256)]
                    (demo/note-on! synth 60 velocity)
                    (configure synth)
                    (demo/render! synth output 256)
                    (vec output))))
        plain (render (fn [_]))]
    (is (not (every? zero? plain)) "the instrument makes sound")
    (testing "velocity is a per-voice source"
      (is (not= plain (render (fn [_]) 0.1))))
    (testing "the mod wheel is a global source and changes the timbre"
      (is (not= plain (render #(demo/set-controller! % 1 1.0)))))
    (testing "pitch bend is a global source"
      (is (not= plain (render #(demo/set-pitch-bend! % 0 1.0)))))
    (testing "and the routing is kernel-agnostic: the interpreter host agrees"
      (let [interpreter (fn [configure]
                          (let [synth (demo/demo-synth {:target :interpreter})
                                output (float-array 256)]
                            (demo/note-on! synth 60 0.8)
                            (configure synth)
                            (demo/render! synth output 256)
                            (vec output)))]
        (is (= (render #(demo/set-controller! % 1 1.0))
               (interpreter #(demo/set-controller! % 1 1.0))))))))

(deftest the-pool-delivers-the-pitch-domain-sources
  ;; `:bend` and `:detune` are standard sources now (D35): the pool resolves them
  ;; from its global source bank and per-voice detune, so a host no longer has to.
  (let [bundle (host/compile-synth demo/description)
        pool (host/synth-pool bundle)
        plan (:control-plan (pool/pool-config pool))
        position (into {} (map-indexed (fn [index entry] [(name (:control entry)) index]) plan))
        values (double-array (pool/control-count pool))
        value (fn [index control]
                (pool/read-controls! pool index values)
                (aget ^doubles values (int (get position (name control)))))]
    (testing "a note-on opens the gate and delivers note, bend, and detune"
      (pool/note-on! pool 60)
      (is (= 1.0 (value 0 :gate)))
      (is (= 60.0 (value 0 :note)))
      (is (= 0.0 (value 0 :bend)))
      (is (= 0.0 (value 0 :detune))))
    (testing "bend is cents, scaled by the declared range"
      (pool/set-pitch-bend! pool 0 1.0)
      (is (= 200.0 (value 0 :bend)))
      (pool/set-pitch-bend! pool 0 -0.5)
      (is (= -100.0 (value 0 :bend))))
    (testing "a release closes the gate but keeps the pitch"
      (pool/note-off! pool 60)
      (is (= 0.0 (value 0 :gate)))
      (is (= 60.0 (value 0 :note))))
    (testing "a bad channel is a programming error"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such channel"
                            (pool/set-pitch-bend! pool 16 1.0))))))

(deftest set-detune-reaches-the-detune-source
  ;; `:detune` is a standard *host* source now (D36): the pool's per-voice detune
  ;; is set with `set-detune!`, not by a unison mode, and a fresh voice is in tune.
  (let [pool (pool-for demo/description)
        plan (:control-plan (pool/pool-config pool))
        position (into {} (map-indexed (fn [index entry] [(name (:control entry)) index]) plan))
        values (double-array (pool/control-count pool))
        detune (fn [index]
                 (pool/read-controls! pool index values)
                 (aget ^doubles values (int (get position (name :detune)))))]
    (pool/note-on! pool 60)
    (is (= 0.0 (detune 0)) "a fresh voice is in tune")
    (pool/set-detune! pool 0 -20.0)
    (is (= -20.0 (detune 0)))
    (testing "a new note starts a voice in tune again"
      (pool/note-on! pool 64)
      (is (= 0.0 (detune 1))))
    (testing "a bad index is a programming error"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no voice at that index"
                            (pool/set-detune! pool 99 1.0))))))

;; ---------------------------------------------------------------------------
;; The parameter layer (M2)
;; ---------------------------------------------------------------------------

(dsp/defn level-voice
  "A generator whose output *is* its `level` control, so a parameter change is
  directly visible in the block and a per-voice value can be read per voice."
  [gate note level]
  {:process {:input false}}
  level)

(dsp/defn defaulted-voice
  "A generator with a control default, so an inferred parameter inherits it."
  [gate amount]
  {:process {:input false :controls [{:name amount :default 0.75}]}}
  amount)

(deftest a-parameter-defaults-from-its-declaration
  (let [description {:id :defaulted :capacity 2 :mode :poly :max-block-frames 32
                     :voice {:definition #'defaulted-voice}}
        pool (pool-for description)]
    (is (= 0.75 (pool/voice-parameter-value pool 0 :amount))
        "a control with no declaration infers a parameter from its default")
    (is (= {:scope :per-voice :index 0} (pool/parameter-slot pool :amount)))
    (is (= 0.005 (pool/voice-parameter-value
                  (pool-for (assoc sine-voice-description :id :declared-default))
                  0 :attack))
        "a declared default is the parameter's default")))

(deftest a-shared-parameter-is-one-value-every-voice-reads
  (let [description {:id :shared-param :capacity 3 :mode :poly :max-block-frames 32
                     :parameters {:level {:scope :shared :default 0.25}}
                     :voice {:definition #'level-voice}}
        pool (pool-for description)
        output (float-array 4)]
    (is (= 0.25 (pool/parameter-value pool :level)))
    (is (= {:scope :shared :index 0} (pool/parameter-slot pool :level)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not per voice"
                          (pool/set-voice-parameter! pool 0 :level 0.5)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such parameter"
                          (pool/set-parameter! pool :bogus 0.5)))
    (pool/set-parameter! pool :level 0.5)
    (is (= 0.5 (pool/parameter-value pool :level)))
    (dotimes [index 3] (pool/note-on! pool (+ 60 index)))
    (pool/render! pool output 4 48000.0)
    (is (= [1.5 1.5 1.5 1.5] (mapv double (vec output)))
        "three voices each read the same shared cell")))

(deftest a-per-voice-parameter-addresses-one-voice
  (let [description {:id :per-voice-param :capacity 2 :mode :poly :max-block-frames 32
                     :parameters {:level {:default 0.1}}
                     :voice {:definition #'level-voice}}
        pool (pool-for description)
        plan (:control-plan (pool/pool-config pool))
        position (into {} (map-indexed (fn [index entry] [(name (:control entry)) index])
                                       plan))
        values (double-array (pool/control-count pool))
        level-of (fn [index]
                   (pool/read-controls! pool index values)
                   (aget ^doubles values (int (get position "level"))))]
    (is (= 0.1 (level-of 0)))
    (is (= 0.1 (level-of 1)))
    (pool/set-voice-parameter! pool 1 :level 0.7)
    (is (= 0.1 (level-of 0)) "voice 0 is untouched")
    (is (= 0.7 (level-of 1)) "voice 1 changed")
    (is (= 0.1 (pool/voice-parameter-value pool 0 :level)))
    (is (= 0.7 (pool/voice-parameter-value pool 1 :level)))))

(deftest a-setting-reaches-a-sounding-voice
  (let [pool (pool-for {:id :reach :capacity 1 :mode :poly :max-block-frames 16
                        :voice {:definition #'level-voice}})
        output (float-array 4)]
    (pool/note-on! pool 60 1.0)
    (pool/set-voice-parameter! pool 0 :level 0.5)
    (pool/render! pool output 4 48000.0)
    (is (= [0.5 0.5 0.5 0.5] (mapv double (vec output)))
        "setting a parameter re-resolves the sounding voice")))

(deftest a-binding-renames-a-controls-parameter
  (let [description {:id :renamed :capacity 1 :mode :poly :max-block-frames 32
                     :parameters {:gain {:default 0.3}}
                     :voice {:definition #'defaulted-voice
                             :bindings {:amount :gain}}}
        pool (pool-for description)]
    (is (= 0.3 (pool/voice-parameter-value pool 0 :gain)))
    (is (nil? (pool/parameter-slot pool :amount))
        "the control name is not itself a parameter when it is bound elsewhere")
    (pool/set-voice-parameter! pool 0 :gain 0.9)
    (is (= 0.9 (pool/voice-parameter-value pool 0 :gain)))))

(deftest a-unison-transform-is-the-sum-of-detuned-copies
  ;; D36: `unison` is a plain function over a definition value, `dsp/def` names
  ;; the result, and the generated voice is exactly N detuned copies summed. The
  ;; structural oracle renders those copies separately and sums them — no pool
  ;; mode involved — and the match is bit-exact: the copies' state, their control
  ;; values, and the summation order are the same.
  (let [base demo/description
        render (fn [description target]
                 (let [pool (host/synth-pool (host/compile-synth description {:target target}))
                       output (float-array 512)]
                   (pool/note-on! pool 60 0.8)
                   (pool/render! pool output 512 48000.0)
                   (mapv #(Float/floatToRawIntBits (float %)) output)))
        copy (fn [target detune]
               (let [pool (host/synth-pool (host/compile-synth (assoc base :capacity 1)
                                                               {:target target}))
                     output (float-array 512)]
                 (pool/note-on! pool 60 0.8)
                 ;; `:detune` is a runtime source now; setting it re-resolves the
                 ;; voice's controls, so the next render carries it.
                 (pool/set-detune! pool 0 detune)
                 (pool/render! pool output 512 48000.0)
                 (mapv #(Float/floatToRawIntBits (float %)) output)))
        unison (fn [target]
                 (render (assoc base :capacity 1
                                :voice (assoc (:voice base) :definition #'demo/unison-demo-voice))
                         target))]
    (doseq [target [:interpreter :jvm]]
      (let [copies (mapv #(copy target %) [-20.0 0.0 20.0])
            expected (reduce (fn [sum bits] (mapv bits-add sum bits)) copies)
            actual (unison target)]
        (is (= expected actual)
            (str "the transform is the sum of its detuned copies on " target))
        (is (not= actual (first copies))
            (str "and it is not a single copy on " target))))))

(deftest dsp-fn-is-an-anonymous-definition-value
  ;; `dsp/fn` is to `dsp/defn` what `clojure.core/fn` is to `defn`: the same
  ;; capture, returned as a value, so a definition composes with `->`.
  (let [anonymous (dsp/fn [sample amount] {:process {:controls [{:name amount}]}}
                    (* sample amount))
        render (fn [value]
                 (let [artifact (dsp/compile value {:target :jvm :entry :process})
                       input (float-array [1.0 1.0 1.0 1.0])
                       output (float-array 4)]
                   (dsp/process! artifact {:input input :output output :frames 4
                                           :sample-rate 48000.0 :controls [0.5]})
                   (vec output)))]
    (is (map? anonymous))
    (is (= :function (:dsp/kind anonymous)))
    (is (symbol? (:name anonymous)) "it carries a generated internal name")
    (is (= 'omkamra.dsp.tests.synth (get-in anonymous [:source :namespace]))
        "and the defining namespace's source")
    (is (= [0.5 0.5 0.5 0.5] (render anonymous)))))

;; P10c: `activity` is a builtin slot the voice writes; the pool frees a released
;; voice when it reads <= 0. A voice that does not write it keeps until stolen.
(dsp/defn countdown-voice
  [gate note]
  {:process {:input false}
   :state [{:name remaining :type :int :init 3
            :next (max 0 (- remaining 1))}]}
  (do (store-special :activity (if (<= remaining 0) 0 1))
      (* gate 0.5)))

(dsp/defn no-activity-voice
  [gate note]
  {:process {:input false}}
  (* gate 0.5))

(def activity-test-description
  {:id :activity-test
   :capacity 1 :mode :poly :max-block-frames 128
   :voice {:definition #'countdown-voice}})

(def no-activity-test-description
  {:id :no-activity-test
   :capacity 1 :mode :poly :max-block-frames 128
   :voice {:definition #'no-activity-voice}})

(deftest a-declared-activity-frees-a-released-voice
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth activity-test-description
                                                    {:target target}))
          output (float-array 8)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 4 48000.0)
      (is (= 1 (count (pool/voice-snapshot pool)))
          (str "the voice is active before release on " target))
      (pool/note-off! pool 60)
      (pool/render! pool output 1 48000.0)
      (is (empty? (pool/voice-snapshot pool))
          (str "a declared activity of 0 frees the released voice on " target)))))

(deftest a-voice-without-activity-stays-until-stolen
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth no-activity-test-description
                                                    {:target target}))
          output (float-array 128)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 1 48000.0)
      (pool/note-off! pool 60)
      (pool/render! pool output 100 48000.0)
      (is (= [:releasing] (mapv :state (pool/voice-snapshot pool)))
          (str "without a declaration the voice is kept until stolen on " target)))))

;; Declares `:activity` but writes it only when the countdown ends, so before that
;; the builtin slot keeps its non-zero default and the released voice stays alive.
(dsp/defn default-activity-voice
  [gate note]
  {:process {:input false}
   :state [{:name remaining :type :int :init 3
            :next (max 0 (- remaining 1))}]}
  (if (> remaining 0)
    (* gate 0.5)
    (do (store-special :activity 0)
        (* gate 0.5))))

(deftest a-declared-activity-defaults-to-active
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool
                (host/compile-synth
                 {:id :default-activity :capacity 1 :mode :poly :max-block-frames 64
                  :voice {:definition #'default-activity-voice}}
                 {:target target}))
          output (float-array 8)]
      (pool/note-on! pool 60 1.0)
      (pool/note-off! pool 60)
      (pool/render! pool output 1 48000.0)
      (is (= [:releasing] (mapv :state (pool/voice-snapshot pool)))
          (str "the non-zero default keeps the released voice alive on " target))
      (pool/render! pool output 8 48000.0)
      (is (empty? (pool/voice-snapshot pool))
          (str "and writing 0 frees it on " target)))))

(deftest the-runtime-config-carries-the-special-mask
  (is (= 1 (long (:special-mask (:runtime (host/compile-synth
                                           activity-test-description))))))
  (is (= 0 (long (:special-mask (:runtime (host/compile-synth
                                           no-activity-test-description)))))))

;; P10c: non-finite containment at the kernel seam (D39). The runaway and loud
;; voices live in `omkamra.dsp.harness.fixtures`, shared with the browser
;; testbed, so both suites exercise the same voices under every policy.
(deftest a-non-finite-voice-is-contained
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth containment-runaway-description
                                                    {:target target}))
          output (float-array 4)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 1 48000.0)
      (is (= 1.0 (double (aget output 0)))
          (str "a finite sample is untouched on " target))
      (pool/note-off! pool 60)
      (pool/render! pool output 4 48000.0)
      (is (every? (fn [index] (zero? (double (aget output index)))) (range 4))
          (str "an infinite sample is contained to 0 on " target)))))

(deftest a-runaway-magnitude-is-clamped
  ;; Clamping a *finite* runaway is a `:sanitize` behavior; `:reset` only acts on
  ;; non-finite samples.
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth containment-loud-description
                                                    {:target target
                                                     :containment :sanitize}))
          output (float-array 2)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 2 48000.0)
      (is (= 1.0e6 (double (aget output 0)))
          (str "a runaway magnitude is clamped to the ceiling on " target)))))

(deftest containment-can-be-disabled
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth containment-runaway-description
                                                    {:target target
                                                     :containment :off}))
          output (float-array 2)]
      (pool/note-on! pool 60 1.0)
      (pool/note-off! pool 60)
      (pool/render! pool output 2 48000.0)
      (is (not (Double/isFinite (double (aget output 0))))
          (str "with :off a non-finite sample reaches the block on " target)))))

;; P10c: the `:reset` policy silences a non-finite block and resets the voice's
;; state so it recovers; `:sanitize` only contains the samples and lets state
;; advance. The voice's `age` starts at 0 -> 1/0 = infinity, then advances.
(deftest a-reset-policy-silences-and-recovers-a-voice
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth containment-reset-description
                                                    {:target target
                                                     :containment :reset}))
          output (float-array 1)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 1 48000.0)
      (is (= 0.0 (double (aget output 0)))
          (str "a non-finite block is silenced on " target))
      (is (= 1 (:containment (pool/pool-stats pool)))
          (str "the containment counter increments on " target))
      (pool/render! pool output 1 48000.0)
      (is (= 0.0 (double (aget output 0)))
          (str "the voice was reset, so the next block fails again on " target))
      (is (= 2 (:containment (pool/pool-stats pool)))
          (str "the counter keeps counting on " target)))))

(deftest a-sanitize-policy-does-not-reset
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth containment-reset-description
                                                    {:target target
                                                     :containment :sanitize}))
          output (float-array 1)]
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 1 48000.0)
      (is (= 0.0 (double (aget output 0)))
          (str "the first block is contained to 0 on " target))
      (pool/render! pool output 1 48000.0)
      (is (= 1.0 (double (aget output 0)))
          (str "without a reset the state advanced on " target))
      (is (zero? (:containment (pool/pool-stats pool)))
          (str "sanitize does not count a containment on " target)))))

(deftest an-unknown-containment-mode-is-refused
  (doseq [target [:interpreter :jvm]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unknown non-finite containment mode"
         (host/compile-synth containment-runaway-description
                             {:target target :containment :bogus}))
        (str "an unknown containment mode fails on " target))))

;; D40/P10d: a stereo voice renders a *planar* block (two float arrays). The
;; pool sums voices plainly; panning is a voice-level concern, not the pool's.
(dsp/defn stereo-gate-voice
  [gate note]
  {:process {:input false}}
  (buffer-store :output 0 0 (* gate 0.5))
  (buffer-store :output 1 0 (* gate 0.25)))

(def stereo-voice-description
  {:id :stereo-voice :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'stereo-gate-voice}})

(deftest a-stereo-voice-renders-a-planar-block
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth stereo-voice-description
                                                    {:target target}))
          output (into-array (Class/forName "[F")
                             [(float-array 4) (float-array 4)])]
      (is (= 2 (get-in (host/compile-synth stereo-voice-description
                                           {:target target})
                       [:runtime :channels])))
      (pool/note-on! pool 60 1.0)
      (pool/render! pool output 4 48000.0)
      (is (= [0.5 0.5 0.5 0.5] (mapv double (aget output 0)))
          (str "left channel on " target))
      (is (= [0.25 0.25 0.25 0.25] (mapv double (aget output 1)))
          (str "right channel on " target)))))

;; P10d: `:pan` is a standard per-voice source, the stereo counterpart of
;; `:detune`. A voice that binds it is host-panned; one that does not is free to
;; pan inside its own DSP.
(dsp/defn pan-voice
  [gate pan]
  {:process {:input false}}
  (buffer-store :output 0 0 pan)
  (buffer-store :output 1 0 (* -1.0 pan)))

(def pan-voice-description
  {:id :pan-voice :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'pan-voice}})

(deftest a-per-voice-pan-reaches-the-voice
  (doseq [target [:interpreter :jvm]]
    (let [pool (host/synth-pool (host/compile-synth pan-voice-description
                                                    {:target target}))
          output (into-array (Class/forName "[F")
                             [(float-array 2) (float-array 2)])
          index (pool/note-on! pool 60 1.0)]
      (pool/render! pool output 2 48000.0)
      (is (= [0.0 0.0] (mapv double (aget output 0)))
          (str "a fresh voice is centred on " target))
      (is (= [0.0 0.0] (mapv double (aget output 1))))
      (pool/set-pan! pool index 0.5)
      (pool/render! pool output 2 48000.0)
      (is (= [0.5 0.5] (mapv double (aget output 0)))
          (str "left carries pan on " target))
      (is (= [-0.5 -0.5] (mapv double (aget output 1)))
          (str "right carries -pan on " target)))))

(deftest a-shared-control-comes-from-the-kernels-own-region
  ;; A shared control is not a voice's storage: the kernel owns one region for
  ;; the whole pool, so a broadcast write is one call that every instance reads,
  ;; including voices that start after it (M5).
  (doseq [target [:jvm :interpreter]]
    (let [bundle (host/synth-bundle shared-voice-description
                                    {:target target :containment :off})
          kernel (host/host-kernel (:artifact bundle) {:containment :off})
          first-voice (pool/create-voice kernel 4)
          second-voice (pool/create-voice kernel 4)
          output (float-array 4)]
      (pool/write-shared! kernel 'amount (float-array [0.5 0.5 0.5 0.5]))
      (pool/write-voice-controls! kernel first-voice (double-array 0))
      (pool/mix-voice! kernel first-voice output 4 48000.0)
      (is (= [1.5 1.5 1.5 1.5] (mapv double (vec output)))
          (str (name target) ": the first voice reads the shared region"))
      (pool/write-voice-controls! kernel second-voice (double-array 0))
      (pool/mix-voice! kernel second-voice output 4 48000.0)
      (is (= [3.0 3.0 3.0 3.0] (mapv double (vec output)))
          (str (name target) ": a voice that never saw the write reads it too"))
      (testing "a name the voice does not declare is refused"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no such shared control"
                              (pool/write-shared! kernel 'nope (float-array [1.0]))))))))

(deftest a-shared-parameter-reaches-every-voice-through-one-region
  ;; A parameter bound to a `:shared` control lives in the pool-wide region the
  ;; kernel owns, not in per-voice control storage, so one write serves every
  ;; voice — including a voice that starts later (M5).
  (let [render (fn [target value]
                 (let [pool (host/synth-pool (compiled shared-voice-description
                                                       {:target target}))
                       output (float-array 4)]
                   (when value (pool/set-parameter! pool 'amount value))
                   (pool/note-on! pool 60 1.0)
                   (pool/note-on! pool 64 1.0)
                   (pool/render! pool output 4 48000.0)
                   (vec output)))]
    (testing "the declared default is in the region, and both voices read it"
      (is (= [2.0 2.0 2.0 2.0] (render :jvm nil)) "two voices of 0.0 + 1.0")
      (is (= [2.0 2.0 2.0 2.0] (render :interpreter nil)) "the two hosts agree"))
    (testing "one write is enough for every voice"
      (is (= [3.0 3.0 3.0 3.0] (render :jvm 0.5)))
      (is (= [3.0 3.0 3.0 3.0] (render :interpreter 0.5))))
    (testing "a shared control cannot bind to a per-voice parameter"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"must bind to a :shared parameter"
           (compiled (assoc-in shared-voice-description
                               [:parameters :amount :scope] :per-voice)))))
    (testing "an undeclared shared control is inferred as shared"
      (let [runtime (:runtime (compiled (update shared-voice-description
                                                :parameters dissoc :amount)))]
        (is (= :shared (:scope (get (:parameters runtime) :amount))))
        (is (= [:amount] (:shared-parameters runtime)))
        (is (= :amount (:parameter (first (:shared-plan runtime)))))))))

(deftest every-voice-has-an-instance-key-of-its-own
  ;; The pool will mix one *instance* per block and skip the rest of its slots, so
  ;; a key has to be unique per instance: two voices sharing one would drop a
  ;; voice out of the mix, silently, and only when a second voice sounds. Pin the
  ;; key function itself, before anything consumes it (M6).
  (doseq [target [:jvm :interpreter]]
    (let [bundle (host/synth-bundle sine-voice-description
                                    {:target target :containment :off})
          kernel (host/host-kernel (:artifact bundle) {:containment :off})
          voices (mapv (fn [_] (pool/create-voice kernel 128)) (range 4))
          keys (mapv (fn [voice] (pool/instance-key kernel voice)) voices)]
      (is (apply distinct? keys)
          (str (name target) ": four voices have four keys"))
      (is (= keys (sort keys))
          (str (name target) ": keys increase with creation order"))
      (is (every? pos? keys)
          (str (name target) ": 0 is left free for an unkeyed handle")))))

(defn- fake-kernel
  "A `Kernel` whose handles share one instance key per `slots-per-instance`.

  This stands in for a fused kernel: S voice handles, one instance, one key.
  `mixes` records `[key serial]` for every `mix-voice!` the pool makes, and each
  mix adds 1.0 to the block, so a mix that should have happened is visible in the
  output."
  [slots-per-instance mixes]
  (let [created (volatile! 0)]
    (reify pool/Kernel
      (create-voice [_ _]
        (let [serial (long @created)]
          (vswap! created inc)
          {:key (inc (quot serial slots-per-instance)) :serial serial}))
      (instance-key [_ voice] (:key voice))
      (destroy-voice! [_ _] nil)
      (reset-voice! [_ _] nil)
      (write-voice-controls! [_ _ _] nil)
      (write-voice-lane! [_ _ _ _] nil)
      (write-shared! [_ _ _] nil)
      (read-specials! [_ _ _] nil)
      (clear-output! [_ output frames]
        (dotimes [index frames] (aset ^floats output index 0.0)))
      (mix-voice! [_ voice output _ _]
        (swap! mixes conj [(:key voice) (:serial voice)])
        (aset ^floats output 0 (float (+ (aget ^floats output 0) 1.0)))
        false))))

(deftest a-pool-mixes-one-instance-once-per-block
  ;; The consumer of `instance-key`: when several slots share an instance, the pool
  ;; must run that instance once per block — not once per slot — while still mixing
  ;; every *other* instance. A fake kernel makes both halves visible (M6), and the
  ;; mark array's size is asserted too: a field that is one slot off reads another
  ;; field, which is how the first attempt failed.
  (let [mixes (atom [])
        runtime (:runtime (host/synth-bundle sine-voice-description {:target :jvm}))
        pool (pool/voice-pool runtime (fake-kernel 2 mixes))
        output (float-array 8)]
    (is (= (pool/pool-capacity pool)
           (alength ^longs (.-instance-mixed pool)))
        "one mark per owner slot, so never more than one per slot")
    ;; Every slot sounds, including the *last* key — the one an off-by-one in the
    ;; per-block clearing would skip from the second block on.
    (dotimes [_ (pool/pool-capacity pool)] (pool/note-on! pool 60 1.0))
    (reset! mixes [])
    (pool/render! pool output 4 48000.0)
    (is (= [[1 0] [2 2]] @mixes)
        "one mix for the first instance (voices 0 and 1) and one for the second")
    (is (= [2.0 0.0 0.0 0.0] (mapv double (take 4 (vec output))))
        "both instances reached the block")
    (reset! mixes [])
    (pool/render! pool output 4 48000.0)
    (is (= [[1 0] [2 2]] @mixes)
        "and again in the next block: the marks are per block, every key included")))

(deftest every-voice-is-mixed-when-every-voice-has-its-own-key
  ;; The same consumer, in the shape every kernel has today: one instance per
  ;; voice, so the marks must never suppress a mix — including for the last key,
  ;; which is where the first version's per-block clearing was short by one (M6).
  (let [mixes (atom [])
        runtime (:runtime (host/synth-bundle sine-voice-description {:target :jvm}))
        pool (pool/voice-pool runtime (fake-kernel 1 mixes))
        output (float-array 4)]
    (dotimes [_ (pool/pool-capacity pool)] (pool/note-on! pool 60 1.0))
    (dotimes [_ 2]
      (reset! mixes [])
      (pool/render! pool output 4 48000.0)
      (is (= [[1 0] [2 1] [3 2] [4 3]] @mixes)
          "four voices, four instances, four mixes"))
    (is (= [4.0 0.0 0.0 0.0] (mapv double (take 4 (vec output))))
        "all four instances reached the block")))

(deftest a-fused-synth-is-one-process-for-several-voices
  ;; The instrument option (D52): `{:fusion {:slots S}}` compiles the voice as one
  ;; `(poly S voice)` process, and everything above it -- the description's
  ;; parameters, the pool, note-ons -- is unchanged, because the pool stays
  ;; base-shaped and the kernel owns the mapping (D50).
  (let [description (assoc fixtures/echo-voice-description :capacity 4 :max-block-frames 128)
        options {:target :jvm :precision :f32 :containment :off}
        plain (host/compile-synth description options)
        fused (host/compile-synth description (assoc options :fusion {:slots 4}))]
    (testing "the runtime carries what a kernel on another host needs, and not otherwise"
      (is (= {:slots 4
              :controls [:gate :frequency :delay :mix]
              :per-slot [:gate :frequency :delay :mix]}
             (:fusion (:runtime fused))))
      (is (nil? (:fusion (:runtime plain))))
      (is (= [0 0] [(:special-mask (:runtime fused)) (:special-count (:runtime fused))])
          "a voice that declares no special publishes none, plain or fused"))
    (testing "the artifact is one process with one partition per slot"
      (is (= 4 (count (:partitions (:artifact fused)))))
      (is (= 4 (count (:call-partitions (:ir (:artifact fused)))))
          "the entry calls the voice once per slot, which is what a slot is")
      (is (= 4 (count (:partitions (:artifact fused))))
          "and every call site owns the state partition a reset needs")
      (is (not= (alength ^bytes (:bytes (:artifact plain)))
                (alength ^bytes (:bytes (:artifact fused))))))
    (testing "and the pool renders what a pool of separate voices renders"
      ;; One voice sounding, so the two lowerings sum the same things in the same
      ;; order and can be compared sample for sample -- the anchor the whole
      ;; fusion question rests on (D45). The idle slots contribute exactly zero:
      ;; their control slices are untouched, so their gate is 0.
      (let [a (pool/voice-pool (:runtime plain) (:kernel plain))
            b (pool/voice-pool (:runtime fused) (:kernel fused))
            x (float-array 128)
            y (float-array 128)]
        (pool/note-on! a 69 1.0)
        (pool/note-on! b 69 1.0)
        (dotimes [block 3]
          (pool/render! a x 128 48000.0)
          (pool/render! b y 128 48000.0)
          (is (= (vec x) (vec y)) (str "block " block)))
        (is (= [1 1 1 1] (mapv #(pool/instance-key (:kernel fused) %) (.-handles b)))
            "four voices, one instance")))
    (testing "a voice that declares a shared control keeps it across fusion"
      ;; What `poly` used to lose: it redeclared the controls by name alone, so a
      ;; shared control arrived as an ordinary scalar and the fused interface had
      ;; no shared region at all (D50, D52).
      (let [bundle (host/synth-bundle (assoc fixtures/shared-voice-description :capacity 2)
                                      (assoc options :fusion {:slots 2}))]
        (is (= '[amount] (mapv :name (:shared (:abi (:artifact bundle)))))
            "the fused interface declares the control the voice declared")
        (is (= [{:name "amount" :offset 0 :required? true}] (:shared (:runtime bundle))))
        (is (= {:slots 2 :controls [] :per-slot []} (:fusion (:runtime bundle)))
            "a shared control is hoisted, so nothing is expanded per slot")
        (is (thrown? clojure.lang.ExceptionInfo
                     (host/compile-synth (assoc fixtures/shared-voice-description :capacity 2)
                                         (assoc options :fusion {:slots 2})))
            "and a fused voice with nothing per slot has nothing to fuse")))
    (testing "the option is closed and checked"
      (is (thrown? clojure.lang.ExceptionInfo
                   (host/compile-synth description (assoc options :fusion {:slots 0}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (host/compile-synth description (assoc options :fusion {:slots 2
                                                                           :chunk 2})))))))

(deftest a-fused-pool-preserves-a-release-tail
  ;; M6b: the guard skips slots a pool has not gated, but a slot that *has*
  ;; sounded keeps being evaluated through its release -- so a fused pool and a
  ;; plain pool render a note-on/note-off/tail bit for bit. The plain pool is the
  ;; oracle; a bare gate check (no latch) would cut the tail and fail this.
  (let [description (assoc fixtures/echo-voice-description
                           :capacity 4 :max-block-frames 128)
        options {:target :jvm :precision :f32 :containment :off}
        plain (host/compile-synth description options)
        fused (host/compile-synth description (assoc options :fusion {:slots 4}))
        a (pool/voice-pool (:runtime plain) (:kernel plain))
        b (pool/voice-pool (:runtime fused) (:kernel fused))
        x (float-array 128)
        y (float-array 128)]
    (pool/note-on! a 69 1.0)
    (pool/note-on! b 69 1.0)
    (dotimes [_ 3]
      (pool/render! a x 128 48000.0)
      (pool/render! b y 128 48000.0)
      (is (= (vec x) (vec y)) "the gated block agrees"))
    (pool/note-off! a 69)
    (pool/note-off! b 69)
    (dotimes [block 24]
      (pool/render! a x 128 48000.0)
      (pool/render! b y 128 48000.0)
      (is (= (vec x) (vec y)) (str "release block " block)))
    (is (pos? (reduce max 0.0 (map (fn [v] (Math/abs (double v))) (vec y))))
        "and the released fused voice is still producing its tail"))
  (testing "the guard is a compile option, not runtime configuration"
    (let [description (assoc fixtures/echo-voice-description
                             :capacity 4 :max-block-frames 128)
          options {:target :jvm :precision :f32 :containment :off}
          guarded (host/compile-synth description (assoc options :fusion {:slots 4}))
          bare (host/compile-synth description (assoc options
                                                      :fusion {:slots 4 :guard false}))]
      (is (not= (alength ^bytes (:bytes (:artifact guarded)))
                (alength ^bytes (:bytes (:artifact bare))))
          "guarding changes the compiled artifact")
      (is (= (:fusion (:runtime guarded)) (:fusion (:runtime bare)))
          "but nothing about the runtime configuration"))))

(deftest a-fused-pool-frees-a-released-slot-from-its-own-activity
  ;; M6 seam 4: a fused slot publishes the voice's `:activity` from its call
  ;; partition, so the pool frees a finished voice exactly as it does for a plain
  ;; voice. The tail matters: the voice reports active for a few blocks after its
  ;; gate closes, so a fused kernel that could not read its slot's special (and so
  ;; saw a stale zero) would free on the first release block, where the plain pool
  ;; still holds the tail. The plain pool is the oracle.
  (let [description (assoc fixtures/activity-voice-description :capacity 2)
        options {:target :jvm :precision :f32 :containment :off}
        plain (host/compile-synth description options)
        fused (host/compile-synth description (assoc options :fusion {:slots 2}))]
    (testing "the runtime reads the voice's specials whether or not it is fused"
      (is (= [1 1] [(:special-mask (:runtime plain)) (:special-count (:runtime plain))]))
      (is (= [1 1] [(:special-mask (:runtime fused)) (:special-count (:runtime fused))]))
      (is (= [1 1]
             (mapv #(long (or (:special-mask %) 0))
                   (:call-partitions (:ir (:artifact fused)))))
          "every slot carries the voice's special"))
    (doseq [[label bundle] [["plain" plain] ["fused" fused]]]
      (let [voice-pool (pool/voice-pool (:runtime bundle) (:kernel bundle))
            output (float-array 128)
            stats (fn [] (pool/pool-stats voice-pool))]
        (pool/note-on! voice-pool 69 1.0)
        (pool/render! voice-pool output 128 48000.0)
        (pool/note-off! voice-pool 69)
        (pool/render! voice-pool output 128 48000.0)
        (is (= 1 (count (pool/voice-snapshot voice-pool)))
            (str label ": the release tail is held after one block, not freed"))
        ;; The tail reaches zero on the third release block.
        (dotimes [_ 4] (pool/render! voice-pool output 128 48000.0))
        (is (empty? (pool/voice-snapshot voice-pool))
            (str label ": the voice is freed once its envelope reaches zero"))
        (is (zero? (:steals (stats)))
            (str label ": and it was freed, not stolen"))
        (is (= 1 (:released (stats)))
            (str label ": the release was counted"))))))
