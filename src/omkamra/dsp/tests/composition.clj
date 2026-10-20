(ns omkamra.dsp.tests.composition
  (:require [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.library.delay :as delay]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.filter :as filter]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.library.pitch]))

;; Two voices whose unit has **nine** definitions, so the linker's
;; `:definitions` map is a hash-map and no longer iteration-ordered. The outer
;; one only wraps the inner, so it must pass its controls straight through.
(omkamra.dsp/defn nested-inner
  [gate note bend detune cutoff q attack decay sustain release]
  {:process {:input false}}
  (omkamra.dsp.library.filter/biquad-multimode
   (* (omkamra.dsp.library.envelope/exponential-adsr gate attack decay sustain release)
      (omkamra.dsp.library.oscillator/saw-oscillator
       (omkamra.dsp.library.pitch/pitch->frequency note bend detune 440.0 69.0)))
   cutoff q 0))

(omkamra.dsp/defn nested-outer
  [gate note bend detune cutoff q attack decay sustain release]
  {:process {:input false}}
  (nested-inner gate note bend detune cutoff q attack decay sustain release))

(deftest nested-composition-keeps-dependency-order-past-eight-definitions
  ;; Regression. Past eight definitions the linker's `:definitions` map becomes
  ;; a hash-map, so lowering and state sizing must iterate its `:order`. Before
  ;; the fix a callee could be lowered *after* its caller, the call site would
  ;; allocate a zero-size partition, and the arena was undersized (the
  ;; interpreter threw, the JVM emitted unverifiable bytecode, and JavaScript
  ;; compiled to silently wrong code). Nine definitions here.
  (let [controls [1.0 60.0 0.0 0.0 1000.0 1.0 0.01 0.1 0.7 0.2]
        run (fn [definition target]
              (let [artifact (dsp/compile definition {:target target :entry :process})
                    output (float-array 64)]
                (dsp/process! artifact {:output output :frames 64 :sample-rate 44100.0
                                        :controls controls})
                [(:state-size (:ir artifact)) (vec output)]))
        [inner-size inner] (run #'nested-inner :jvm)
        [outer-size outer] (run #'nested-outer :jvm)]
    (is (= 7 inner-size) "adsr (2) + saw (1) + biquad record (4)")
    (is (= 7 outer-size)
        "the wrapper's arena includes the callee's whole partition")
    (is (not (every? zero? inner)) "and the voice sounds")
    (is (= inner outer) "the wrapper passes its controls straight through")
    (is (= 7 (:state-size (:ir (dsp/compile #'nested-outer
                                            {:target :interpreter :entry :process}))))
        "the interpreter arena has the same size")))

(deftest record-owning-modules-compose
  ;; A module that owns a record/buffer composes like any other: each call site
  ;; gets its own line, so two lines with different delays stay independent.
  (let [interpreter (dsp/compile #'delay/delay-line-pair-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'delay/delay-line-pair-voice
                         {:target :jvm :entry :process})
        frames 8
        input (float-array (assoc (vec (repeat frames 0.0)) 0 1.0))
        output (float-array frames) jvm-output (float-array frames)]
    ;; Two independent records: 64-slot buffer plus cursor each.
    (is (= 2 (count (:records (:ir interpreter)))))
    (is (= 130 (get-in interpreter [:abi :arena :regions :state :count])))
    (apply dsp/invoke interpreter input output frames [2.0])
    (apply dsp/invoke jvm input jvm-output frames [2.0])
    (is (= (vec output) (vec jvm-output)))
    ;; Delay 2 puts the impulse at frame 2, delay 4 at frame 4, half scale each.
    (is (= [2 4] (vec (keep-indexed (fn [index x]
                                      (when (not= 0.0 x) index))
                                    (vec output)))))
    (is (= [0.5 0.5] [(nth output 2) (nth output 4)]))))

(deftest stateful-module-composition
  ;; A stateful module called twice: each call site owns its own partition, so
  ;; the two one-poles inside `one-pole-voice` are independent (D19).
  (let [interpreter (dsp/compile #'filter/one-pole-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'filter/one-pole-voice
                         {:target :jvm :entry :process})
        input (float-array [1.0 0.0 0.0 0.0 0.0]) output (float-array 5)
        jvm-output (float-array 5)]
    (is (= [{:name 'previous :type :float :init 0.0 :index 0}
            {:name 'previous :type :float :init 0.0 :index 1}]
           (:state (:ir interpreter))))
    (is (= 2 (get-in interpreter [:abi :arena :regions :state :count])))
    (apply dsp/invoke interpreter input output 5 [0.5])
    (apply dsp/invoke jvm input jvm-output 5 [0.5])
    (is (= (vec output) (vec jvm-output)))
    ;; Impulse response of 0.5 * (one-pole(0.5) + one-pole(0.25)). Sharing state
    ;; between the two call sites would give 0.3125 at the second sample.
    (is (= [1.0 0.375 0.15625 0.0703125 0.033203125]
           (map double (vec output)))))
  ;; Nested ownership: a state-free module that owns two one-poles, called twice
  ;; from the process, gives four independent partitions.
  (let [interpreter (dsp/compile #'filter/one-pole-pair-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'filter/one-pole-pair-voice
                         {:target :jvm :entry :process})
        input (float-array [1.0 0.0 0.0 0.0]) output (float-array 4)
        jvm-output (float-array 4)]
    (is (= 4 (count (:state (:ir interpreter)))))
    (apply dsp/invoke interpreter input output 4 [0.5])
    (apply dsp/invoke jvm input jvm-output 4 [0.5])
    (is (= (vec output) (vec jvm-output)))
    (is (every? pos? (rest (vec output))))))

(deftest filter-voice-composition
  ;; The P6 done-when: a stateful biquad and a stateful exponential ADSR composed
  ;; into one voice module, identical on the interpreter and the JVM.
  (let [render (fn [target]
                 (let [artifact (dsp/compile #'filter/filter-voice
                                             {:target target :entry :process})
                       frames 24
                       gate (float-array (concat (repeat 4 0.0) (repeat 20 1.0)))
                       output (float-array frames)]
                   (dsp/process! artifact
                                 {:input gate :output output :frames frames
                                  :sample-rate 44100.0
                                  :controls [2000.0 0.7 0.0002 0.0001 0.4 0.0002]})
                   (vec output)))
        interpreter (render :interpreter)
        jvm (render :jvm)
        artifact (dsp/compile #'filter/filter-voice
                              {:target :interpreter :entry :process})]
    ;; Biquad history (4 fields, one record) plus the ADSR's level and stage.
    (is (= 6 (count (:state (:ir artifact)))))
    (is (= 1 (count (:records (:ir artifact)))))
    (is (= interpreter jvm))
    (is (every? #(Float/isFinite %) jvm))
    (is (every? #(<= 0.0 % 1.0) jvm))
    ;; The envelope opens the filter smoothly and monotonically.
    (is (apply <= (subvec jvm 4)))))

(deftest multi-output-module-composition
  ;; A module returning a named record: one local per output, projected by field.
  (let [render (fn [target]
                 (let [artifact (dsp/compile #'filter/svf-voice
                                             {:target target :entry :process})
                       frames 16
                       input (float-array (assoc (vec (repeat frames 0.0)) 0 1.0))
                       output (float-array frames)]
                   (dsp/process! artifact
                                 {:input input :output output :frames frames
                                  :sample-rate 44100.0 :controls [500.0 0.7]})
                   (vec output)))
        interpreter (render :interpreter)
        jvm (render :jvm)
        artifact (dsp/compile #'filter/svf-voice
                              {:target :interpreter :entry :process})]
    ;; The callee owns the filter record; the three outputs are erased to locals.
    (is (= 2 (count (:state (:ir artifact)))))
    (is (= 1 (count (:records (:ir artifact)))))
    (is (= interpreter jvm))
    (is (every? #(Float/isFinite %) jvm))
    (is (some #(not= 0.0 %) jvm))))

(deftest if-branches-are-separate-scopes
  ;; A local declared in one `if` branch is not visible in the other, so the same
  ;; name may be reused there: it simply gets a fresh IR name. Genuine shadowing
  ;; of an in-scope binding is still rejected.
  (let [definition {:dsp/kind :function
                    :name 'branch-scopes
                    :params [{:name 'sample :type :float}]
                    :return-type :float
                    :process {:controls []}
                    :body ['(if (> sample 0.0)
                              (let [v (* sample 2.0)] v)
                              (let [v (- 0.0 sample)] v))]}
        artifact (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 -2.0 3.0]) output (float-array 3) jvm-output (float-array 3)]
    (is (= '[v v__1] (mapv :name (take 2 (:locals (:ir artifact))))))
    (apply dsp/invoke artifact input output 3 [])
    (apply dsp/invoke jvm input jvm-output 3 [])
    (is (= [2.0 2.0 6.0] (vec output)))
    (is (= (vec output) (vec jvm-output)))
    ;; Branches with *different* types must not share a slot either.
    (let [mixed (assoc-in definition [:body 0]
                          '(if (> sample 0.0)
                             (let [v (float->int sample)] (int->float v))
                             (let [v (* sample 2.0)] v)))
          mixed-interpreter (dsp/compile mixed {:target :interpreter :entry :process})
          mixed-jvm (dsp/compile mixed {:target :jvm :entry :process})]
      (apply dsp/invoke mixed-interpreter input output 3 [])
      (apply dsp/invoke mixed-jvm input jvm-output 3 [])
      (is (= [1.0 -4.0 3.0] (vec output)))
      (is (= (vec output) (vec jvm-output))))
    ;; Real shadowing is still a diagnostic.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"cannot shadow an in-scope binding"
         (dsp/compile (assoc-in definition [:body 0] '(let [sample 1.0] sample))
                      {:entry :process})))))

;; A callee that uses its argument twice, and a caller that passes it the output
;; of a stateful module. A computed argument must be evaluated once, not once per
;; use: without argument hoisting the generated targets inlined the one-pole call
;; twice against the one partition, advancing it twice and diverging from the
;; interpreter.
(omkamra.dsp/defn twice-argument
  [x]
  (+ x x))

(omkamra.dsp/defn stateful-argument
  [sample]
  {:process {:controls []}}
  (twice-argument (filter/one-pole sample 0.5)))

(deftest a-computed-argument-is-evaluated-once
  (let [run (fn [target]
              (let [artifact (dsp/compile #'stateful-argument {:target target :entry :process})
                    input (float-array (repeat 8 1.0))
                    output (float-array 8)]
                (dsp/process! artifact {:input input :output output :frames 8
                                        :sample-rate 48000.0 :controls []})
                (vec output)))]
    (is (= (run :interpreter) (run :jvm))
        "the stateful argument advances once per frame in both targets")))

;; A scalar state slot written from the body. This is the general mechanism a
;; voice uses to publish a host-readable value (the builtin `activity` slot is
;; one such slot), instead of only computing state through `:next`.
(omkamra.dsp/defn body-writable-state
  [sample]
  {:state [{:name level :type :float :init 0.0}]
   :process {:controls []}}
  (do (set! level (+ level 1.0))
      level))

(deftest a-body-can-write-a-scalar-state-slot
  (let [run (fn [target]
              (let [artifact (dsp/compile #'body-writable-state
                                          {:target target :entry :process})
                    input (float-array 3)
                    output (float-array 3)]
                (dsp/process! artifact {:input input :output output :frames 3
                                        :sample-rate 48000.0 :controls []})
                (vec output)))]
    (is (= [1.0 2.0 3.0] (run :interpreter))
        "the written slot persists across frames on the interpreter")
    (is (= [1.0 2.0 3.0] (run :jvm))
        "and the JVM writes the same slot")))

;; A builtin special slot: `store-special` writes it, and the ABI exposes it for
;; the host (D39). This is the slot the voice pool reads to free a finished
;; voice; here the definition reads it back to prove the round trip.
(omkamra.dsp/defn activity-slot-voice
  [sample]
  {:process {:controls []}}
  (do (store-special :activity 7)
      (int->float activity)))

(deftest a-builtin-special-slot-is-writable-and-exposed
  (let [run (fn [target]
              (let [artifact (dsp/compile #'activity-slot-voice
                                          {:target target :entry :process})
                    input (float-array 2)
                    output (float-array 2)]
                (dsp/process! artifact {:input input :output output :frames 2
                                        :sample-rate 48000.0 :controls []})
                {:out (vec output)
                 :special (:special-slots (:abi artifact))
                 :mask (:special-mask (:abi artifact))}))]
    (doseq [target [:interpreter :jvm]]
      (let [{:keys [out special mask]} (run target)]
        (is (= [7.0 7.0] out)
            (str "store-special round-trips through the builtin slot on " target))
        (is (= :int (get-in special [:activity :type])))
        (is (= 'activity (get-in special [:activity :name])))
        (is (= 0 (get-in special [:activity :bit]))
            (str "the special's fixed bit is in the ABI on " target))
        (is (= 0 (get-in special [:activity :index]))
            (str "the special occupies the low state slot on " target))
        (is (= 1 (long mask))
            (str "the ABI exposes the special mask on " target))))))

;; A callee may publish a special of its own (M6 seam 4). It is not the entry's
;; contract, so it does not appear in the ABI's `:special-slots`; instead the call
;; site's partition records it, so a fused host can read one slot's activity and
;; free it. The slot is buried in the partition, but it is published, not lost.
(omkamra.dsp/defn special-callee
  [x]
  (do (store-special :activity 1)
      x))

(omkamra.dsp/defn special-caller
  []
  {:process {:input false :controls []}}
  (special-callee 0.5))

(deftest a-callee-special-is-published-per-partition
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile #'special-caller {:target target :entry :process})
          partition (first (:call-partitions (:ir artifact)))]
      (is (= 1 (count (:call-partitions (:ir artifact))))
          (str (name target) ": the callee owns one partition"))
      (is (= [{:special :activity :bit 0 :type :int :size 1 :index 0}]
             (:specials partition))
          (str (name target) ": its activity slot is recorded against the call site"))
      (is (= 1 (long (:special-mask partition))))
      (is (= 0 (long (get-in artifact [:abi :special-mask])))
          (str (name target) ": it is still not the entry's own contract"))
      (is (= {} (get-in artifact [:abi :special-slots]))))))

;; A process-shaped callee writes its own `:output` buffers, so a call site must
;; own the region they land in: `(with-buffers ...)` binds them (F2, D57). The
;; region reuses the call-site state partition (D19), so no new ABI parameter is
;; needed -- the callee's buffer access lowers to state addressing relative to the
;; partition base it is already handed.
(omkamra.dsp/defn region-voice
  [gate]
  {:process {:input false :channels 2 :controls [{:name gate}]}
   :return-type :void}
  (buffer-store :output 0 0 (* gate 2.0))
  (buffer-store :output 1 0 (* gate 3.0)))

(omkamra.dsp/defn fused-regions
  [g0 g1]
  {:process {:input false :channels 2 :controls [{:name g0} {:name g1}]}
   :return-type :void}
  (with-buffers :slot-0 (region-voice g0))
  (with-buffers :slot-1 (region-voice g1))
  (buffer-store :output 0 0 (+ (buffer-load :slot-0 0) (buffer-load :slot-1 0)))
  (buffer-store :output 1 0 (+ (buffer-load :slot-0 1 0) (buffer-load :slot-1 1 0))))

(deftest a-fused-process-voice-writes-the-call-sites-region
  ;; F2a/F2b. Two process-shaped voices, each into its own per-call-site region --
  ;; a state slot in its own call partition (D57) -- summed by the fused entry.
  ;; The reference interpreter is the oracle, and the JVM target must match it
  ;; bit for bit; a region-writing callee is handed the current frame, since a
  ;; per-sample callee has no frame local of its own.
  (doseq [target [:interpreter :jvm]]
    (let [fused (dsp/compile #'fused-regions {:target target :entry :process})
          plain (dsp/compile #'region-voice {:target target :entry :process})
          frames 8
          render (fn [artifact controls]
                   (let [output (make-array Float/TYPE 2 frames)]
                     (apply dsp/invoke artifact nil output frames controls)
                     output))
          rendered (render fused [0.25 0.5])
          p0 (render plain [0.25])
          p1 (render plain [0.5])
          expected (fn [channel]
                     (mapv + (aget p0 channel) (aget p1 channel)))]
      ;; The fused entry is exactly the sum of the two voices' regions, frame by
      ;; frame and channel by channel -- not one voice, and not a shared region.
      (is (= (expected 0) (vec (aget rendered 0))) (str (name target) ": left"))
      (is (= (expected 1) (vec (aget rendered 1))) (str (name target) ": right"))
      ;; Non-vacuous: the regions are audible, so a sum of two zeros would not pass.
      (is (some #(not= 0.0 %) (aget rendered 0)) (str (name target) ": audible"))
      ;; The regions are *state*, not buffers (D57): the entry keeps only its own
      ;; `:output`, and the two regions are one slot each in a call partition.
      (is (= [[:output :output 2]]
             (mapv (juxt :id :direction :channels) (:buffers (:ir fused))))
          (str (name target) ": regions are not buffers"))
      (is (= 2 (count (:call-partitions (:ir fused))))
          (str (name target) ": one partition per slot"))
      (is (= (* 2 2 1024) (get-in fused [:ir :state-size]))
          (str (name target) ": each region is channels * frame-stride")))))

(omkamra.dsp/defn plain-region-caller
  [g]
  {:process {:input false :channels 2 :controls [{:name g}]}
   :return-type :void}
  (region-voice g)
  (buffer-store :output 0 0 0.0))

(deftest a-callee-that-writes-its-output-must-be-given-a-region
  ;; The refusal is scoped to `with-buffers` (D57): a process-shaped callee called
  ;; plainly still cannot reach the caller's ports.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"cannot access the caller's process buffers"
       (dsp/compile #'plain-region-caller
                    {:target :interpreter :entry :process}))))

(omkamra.dsp/defn two-outputs
  [g]
  {:process {:input false :channels 2 :controls [{:name g}]
             :buffers [{:name :left :direction :output}
                       {:name :right :direction :output}]}
   :return-type :void}
  (buffer-store :left 0 0 g)
  (buffer-store :right 0 0 g))

(omkamra.dsp/defn sugar-two-outputs
  [g]
  {:process {:input false :channels 2 :controls [{:name g}]}
   :return-type :void}
  (with-buffers :slot-0 (two-outputs g))
  (buffer-store :output 0 0 (buffer-load :slot-0 0)))

(omkamra.dsp/defn map-two-outputs
  [g]
  {:process {:input false :channels 2 :controls [{:name g}]}
   :return-type :void}
  (with-buffers {:left :slot-0} (two-outputs g))
  (buffer-store :output 0 0 (buffer-load :slot-0 0)))

(deftest with-buffers-refuses-an-unbindable-region
  ;; The bare sugar needs a single output buffer; a map must cover them all. Both
  ;; refusals name the map form, so a region is never silently left unwritten.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"exactly one output buffer"
       (dsp/compile #'sugar-two-outputs {:target :interpreter :entry :process})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"bind exactly the callee's output buffers"
       (dsp/compile #'map-two-outputs {:target :interpreter :entry :process}))))
