(ns omkamra.dsp.tests.jvm
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.harness.bench :as bench]
            [omkamra.dsp.harness.fixtures :as fixtures
             :refer [allocated-bytes call-fixture-arena
                     call-fixture-inlined
                     call-fixture-twice gain one-pole
                     raw-process-noop sine-voice stereo-cross
                     thread-allocation-meter]]
            [omkamra.dsp.hosts :as hosts]
            [omkamra.dsp.hosts.fusion :as fusion]
            [omkamra.dsp.hosts.jvm :as jvm-host]
            [omkamra.dsp.synth.pool :as pool]))

(deftest the-benchmark-arms-are-really-different-lowerings
  ;; `harness/bench` weighs a *pool* of N voice instances against one `(poly N voice)`
  ;; *fused* artifact, and D45's and D47's numbers rest on it. The comparison means
  ;; nothing unless each arm is the lowering it claims to be, so this pins all three.
  (let [voice (var-get #'sine-voice)
        {fused-unit :fused inlined-unit :fused-inlined} (bench/arms voice 4)
        fused ((var-get #'bench/artifact) fused-unit :jvm :f32)
        inlined ((var-get #'bench/artifact) inlined-unit :jvm :f32)]
    (testing "the fused arm calls, the inlined arm copies, and copying is bigger"
      (is (< 1 (count (:definitions (:ir fused)))))
      (is (empty? (:definitions (:ir inlined))))
      (is (< (alength ^bytes (:bytes fused)) (alength ^bytes (:bytes inlined)))))
    (testing "and the two compute the same block"
      (let [fused-context ((var-get #'bench/context) fused)
            inlined-context ((var-get #'bench/context) inlined)]
        (is (= (vec ((var-get #'bench/run-block) fused fused-context))
               (vec ((var-get #'bench/run-block) inlined inlined-context)))))))
  (testing "the pool arm is one voice artifact, whatever the polyphony"
    (let [bytes-at (fn [voices]
                     (alength ^bytes (:bytes (:artifact (bench/pool-arm voices :jvm :f32)))))]
      (is (= (bytes-at 4) (bytes-at 32)))))
  (testing "at N=1 the pool and the fused lowering compute the same block"
    ;; The strongest statement available about two lowerings of one voice, and the
    ;; anchor the fusion slice will need: it is what makes a fused instrument's audio
    ;; a promise rather than a hope.
    (let [voice (var-get #'sine-voice)
          fused ((var-get #'bench/artifact) (:fused (bench/arms voice 1)) :jvm :f32)
          fused-context ((var-get #'bench/context) fused)
          fused-block (vec ((var-get #'bench/run-block) fused fused-context))
          {:keys [pool block]} (bench/pool-arm 1 :jvm :f32)]
      (pool/render! pool block bench/block-frames bench/sample-rate)
      (is (= fused-block (vec block))))))

(deftest the-jvm-emits-a-method-per-callable-definition
  (let [method-names (fn [artifact]
                       (into #{}
                             (map #(.getName ^java.lang.reflect.Method %))
                             (.getDeclaredMethods ^Class (:class artifact))))
        callee-method? #(str/starts-with? % "dsp$call$")]
    (testing "a callable definition becomes a method, and the call stays a call"
      (let [artifact (dsp/compile call-fixture-arena {:target :jvm :entry :process})]
        (is (= '[omkamra.dsp.harness.call-fixture/accumulate
                 omkamra.dsp.harness.call-fixture/counting-bias]
               (vec (keys (:definitions (:ir artifact))))))
        (is (some callee-method? (method-names artifact)))))
    (testing "the interpreter is handed the same call graph it always executed"
      (is (= 2 (count (:definitions (:ir (dsp/compile call-fixture-arena
                                                      {:target :interpreter
                                                       :entry :process})))))))
    (testing "an `^:inline` callee is copied instead, so no method is emitted"
      (let [artifact (dsp/compile call-fixture-inlined {:target :jvm :entry :process})]
        (is (empty? (:definitions (:ir artifact))))
        (is (not-any? callee-method? (method-names artifact)))))
    (testing "every target now calls a definition it was not told to inline"
      (is (seq (:definitions (:ir (dsp/compile call-fixture-arena
                                               {:target :wasm :entry :process}))))))
    (testing "a function entry stays flat: it has no arena for a partition"
      (is (empty? (:definitions (:ir (dsp/compile call-fixture-twice
                                                  {:target :jvm
                                                   :entry :function}))))))))

(deftest jvm-checked-and-unchecked-primitive-process-adapters
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            artifact (dsp/compile gain {:target :jvm
                                        :entry :process
                                        :precision precision})
            input (if f64?
                    (double-array [1.0 -2.0])
                    (float-array [1.0 -2.0]))
            output (if f64?
                     (double-array 2)
                     (float-array 2))
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (if f64?
          (aset-double ^doubles float-controls 0 2.0)
          (aset-float ^floats float-controls 0 (float 2.0)))
        (is (if f64?
              (instance? omkamra.dsp.targets.jvm.DspProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.targets.jvm.DspProcess (:unchecked artifact))))
        (is (nil? (dsp/process-checked! artifact input output 2 48000.0
                                        float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (java.util.Arrays/fill output (if f64? 0.0 (float 0.0)))
        (is (nil? (dsp/process-unchecked! artifact input output 2 48000.0
                                          float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"float controls have the wrong size"
             (dsp/process-checked! artifact input output 2 48000.0
                                   (if f64? (double-array 0) (float-array 0))
                                   int-controls)))))))

(deftest jvm-unchecked-process-adapter-supports-multichannel-arrays
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            array-class (if f64? (Class/forName "[D") (Class/forName "[F"))
            artifact (dsp/compile stereo-cross {:target :jvm
                                                :entry :process
                                                :precision precision})
            input (into-array array-class
                              [(if f64? (double-array [1.0 2.0])
                                   (float-array [1.0 2.0]))
                               (if f64? (double-array [3.0 4.0])
                                   (float-array [3.0 4.0]))])
            output (into-array array-class
                               [(if f64? (double-array 2) (float-array 2))
                                (if f64? (double-array 2) (float-array 2))])
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (is (if f64?
              (instance? omkamra.dsp.targets.jvm.DspMultiProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.targets.jvm.DspMultiProcess (:unchecked artifact))))
        (dsp/process-checked! artifact input output 2 48000.0
                              float-controls int-controls)
        (is (= [[4.0 6.0] [-2.0 -2.0]] (mapv vec output)))
        (dsp/process-unchecked! artifact input output 2 48000.0
                                float-controls int-controls)
        (is (= [[4.0 6.0] [-2.0 -2.0]] (mapv vec output)))))))

(deftest jvm-unchecked-process-adapter-does-not-allocate-after-warmup
  (if-let [meter (thread-allocation-meter)]
    (let [artifact (dsp/compile gain {:target :jvm :entry :process})
          input (float-array (repeat 64 1.0))
          output (float-array 64)
          float-controls (get-in artifact [:controls :float])
          int-controls (get-in artifact [:controls :int])
          calls 10000]
      (aset-float ^floats float-controls 0 (float 2.0))
      (dotimes [_ calls]
        (raw-process-noop artifact input output 64 48000.0
                          float-controls int-controls)
        (dsp/process-unchecked! artifact input output 64 48000.0
                                float-controls int-controls))
      (let [baseline (allocated-bytes
                      meter
                      #(dotimes [_ calls]
                         (raw-process-noop artifact input output 64 48000.0
                                           float-controls int-controls)))
            processed (allocated-bytes
                       meter
                       #(dotimes [_ calls]
                          (dsp/process-unchecked! artifact input output 64 48000.0
                                                  float-controls int-controls)))
            ;; Comparing the two totals for exact equality passes with no
            ;; headroom at all: the measured no-op loop already reports a few
            ;; bytes of its own, so a single stray byte anywhere in the measured
            ;; region fails the check. Allow a few bytes per call instead.
            ;;
            ;; That is still a real bound rather than a fudge, because every
            ;; possible per-call allocation is far larger: a boxed scalar or a
            ;; small object is tens of bytes, and the regression this guards
            ;; against — an unhinted primitive-array access — was ~24 KB per
            ;; call (D26). This bound is ~6000x below it.
            allowance (* 4 calls)]
        (is (<= processed (+ baseline allowance))
            (str "unchecked JVM process allocated " processed
                 " bytes against a fixed-arity baseline of " baseline
                 " over " calls " calls (allowed " allowance " bytes)"))))
    (is true "thread allocation counters are unavailable on this JVM")))

(deftest javascript-and-jvm-f64-backends
  (let [javascript (dsp/compile one-pole {:target :js
                                          :entry :process
                                          :precision :f64})
        jvm (dsp/compile one-pole {:target :jvm
                                   :entry :process
                                   :precision :f64})
        scalar {:dsp/kind :function
                :name 'f64-add
                :params [{:name 'x :type :float}
                         {:name 'y :type :float}]
                :return-type :float
                :body ['(+ x y)]}
        scalar-js (dsp/compile scalar {:target :js :precision :f64})
        scalar-jvm (dsp/compile scalar {:target :jvm :precision :f64})
        input (double-array [1.0 0.0 -1.0])
        output (double-array 3)]
    (is (str/includes? (:source javascript) "Float64Array"))
    (is (not (str/includes? (:source javascript) "Math.fround")))
    (is (instance? (Class/forName "[D") (get-in jvm [:state :float])))
    (is (= :f64 (get-in jvm [:options :precision])))
    (is (not (str/includes? (:source scalar-js) "Math.fround")))
    (is (< (Math/abs (- 0.3 (dsp/invoke scalar-jvm 0.1 0.2)))
           1.0e-15))
    (dsp/process! jvm {:input input
                       :output output
                       :frames 3
                       :sample-rate 48000.0
                       :controls [0.5]})
    (is (= [1.0 0.5 -0.75] (vec output)))))

(deftest jvm-process-supports-typed-controls-and-sample-rate
  (let [definition {:dsp/kind :function
                    :name 'typed-controls
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'previous :type :float :init 0.0
                             :next 'sample}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :body ['(if enabled
                              (+ sample
                                 (+ (int->float step)
                                    (/ sample-rate 1000.0)))
                              previous)]}
        artifact (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 2.0])
        output (float-array 2)]
    (dsp/process! artifact {:input input :output output :frames 2
                            :sample-rate 2000.0
                            :controls [true 3]})
    (is (= [6.0 7.0] (vec output)))))

(deftest the-realistic-voice-arms-agree-bit-for-bit
  ;; A voice with a delay line is exactly the case D47's scratch caveat warned
  ;; about, so it is the case the fused lowering has to match (M6, D45): at one
  ;; sounding voice the pool's block and the fused process's block must be
  ;; identical, sample for sample. The two artifacts also differ in the way the
  ;; argument rests on — a pool instantiates *one* voice artifact N times, so its
  ;; size is flat in N, while the fused process grows with its slots.
  (let [one (bench/realistic-row 1 {:blocks 4 :warmup 1})
        eight (bench/realistic-row 8 {:blocks 4 :warmup 1})]
    (is (true? (:same-block? one))
        "one voice: the pool and the fused process compute the same audio")
    (is (nil? (:same-block? eight))
        "above one voice there is no single block to compare")
    (is (= (:pool-bytes one) (:pool-bytes eight))
        "the pool's artifact is one voice, however many instances run it")
    (is (< (:fused-bytes one) (:fused-bytes eight))
        "the fused artifact holds its slots")))

;; ---------------------------------------------------------------------------
;; Fusion: several voices in one process (M6a)
;; ---------------------------------------------------------------------------

(defn- fused-bundle
  "A pool's runtime + base artifact, and one `(poly slots echo-voice)` artifact
  with the mapping between them.

  The fused voice goes through `comb/guard`, which is what `synth-bundle`'s fusion
  option compiles (M6b), so the seam tests exercise the shipping lowering — and the
  guard's latch is therefore part of each slot's partition, which is what these
  tests reset."
  [slots]
  (let [bundle (hosts/synth-bundle (assoc fixtures/echo-voice-description
                                          :capacity slots :max-block-frames 128
                                          :stealing :oldest)
                                   {:target :jvm :precision :f32 :containment :off})
        base (:artifact bundle)
        fused (dsp/compile (dsp/link (comb/poly (comb/guard (deref #'fixtures/echo-voice))
                                                {:count slots
                                                 :per-slot [:gate :frequency]}))
                           {:target :jvm :entry :process :precision :f32})]
    {:bundle bundle
     :base base
     :fused fused
     :mapping (fusion/fused-mapping (:controls (:abi base)) [:gate :frequency]
                                    (:controls (:abi fused)) slots)}))

(defn- fused-pool
  "A pool over the plain artifact and one over the fused one, same runtime."
  [{:keys [bundle base fused mapping]}]
  {:plain (pool/voice-pool (:runtime bundle) (jvm-host/jvm-kernel base :off))
   :fused (pool/voice-pool (:runtime bundle) (jvm-host/fused-jvm-kernel fused mapping :off))})

(deftest the-fused-mapping-is-the-shape-poly-expands
  ;; `poly` answers a question about *interfaces*: the per-slot controls become
  ;; `gate_0`, `frequency_0`, ... and the rest are hoisted to the top. The mapping
  ;; is what says which of those a base control became, so the pool can keep
  ;; writing base values (M6a).
  (let [{:keys [base fused mapping]} (fused-bundle 3)]
    (is (= '[gate frequency delay mix] (mapv :name (:controls (:abi base)))))
    (is (= '[delay mix gate_0 frequency_0 gate_1 frequency_1 gate_2 frequency_2]
           (mapv :name (:controls (:abi fused)))))
    (is (= {:slots 3 :slot-controls 2 :hoisted-count 2
            :hoisted [false false true true]
            :fused-position [-1 -1 0 1]
            :slot-index [0 1 -1 -1]}
           mapping))
    (testing "and it answers where each slot's copy of a control is"
      (is (= [2 4 6] (mapv #(fusion/fused-position mapping % 0) (range 3)))
          "slot 0, 1, 2 read the gate they were given")
      (is (= [3 5 7] (mapv #(fusion/fused-position mapping % 1) (range 3)))
          "and the frequency beside it")
      (is (= [0 1] (mapv #(fusion/fused-position mapping 0 %) [2 3]))
          "a hoisted control is the same position in every slot")))
  (testing "an interface that is not that shape is refused at construction"
    (let [{:keys [base fused]} (fused-bundle 2)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (fusion/fused-mapping (:controls (:abi base)) [:gate :frequency]
                                         (:controls (:abi fused)) 3))
          "a slot count the artifact does not hold")
      (is (thrown? clojure.lang.ExceptionInfo
                   (fusion/fused-mapping (:controls (:abi base)) [:delay]
                                         (:controls (:abi fused)) 2))
          "a per-slot set that is not the one the artifact expanded"))))

(deftest a-fused-instance-renders-a-slot-like-the-pool-renders-that-voice
  ;; The two seams together, and the reason to have them: the pool stays
  ;; base-shaped, so a fused pool is driven exactly like a plain one, and its
  ;; audio is the pool's audio. Slot by slot, because two voices sounding at once
  ;; sum in a different order (and only *then* are the two lowerings merely equal,
  ;; not identical) — the same reason the bench compares at one voice (M6a).
  (let [{:keys [plain fused]} (fused-pool (fused-bundle 2))
        blocks (fn [] [(float-array 128) (float-array 128)])
        [a b] (blocks)]
    (pool/note-on! plain 72 1.0)
    (pool/note-on! fused 72 1.0)
    (pool/render! plain a 128 48000.0)
    (pool/render! fused b 128 48000.0)
    (is (= (vec a) (vec b))
        "slot 0 renders the voice a plain pool renders")
    (testing "and every slot has its own controls, not the last one written"
      ;; Slot 1 carries note 72 while its neighbour carries 69 and is gated shut,
      ;; which is the assertion the whole mapping exists for: if the write for
      ;; slot 1 landed on slot 0's copy, this block would be note 69's sound --
      ;; or silence -- rather than 72's.
      (let [{:keys [plain fused]} (fused-pool (fused-bundle 2))
            [a b] (blocks)
            values (double-array 4)]
        (pool/note-on! plain 72 1.0)
        (pool/note-on! fused 69 1.0)
        (pool/note-on! fused 72 1.0)
        (pool/read-controls! fused 0 values)
        (is (= [1.0 440.0 4.5 0.5] (vec values))
            "voice 0's base plan, in ABI order")
        (aset values 0 0.0)
        (pool/set-voice-controls! fused 0 values)
        (pool/render! plain a 128 48000.0)
        (pool/render! fused b 128 48000.0)
        (is (= (vec a) (vec b))
            "slot 1 alone sounds exactly as note 72 does on its own")))))

(deftest a-fused-kernels-slots-share-an-instance
  ;; What the pool's per-block mark keys on (seam 1): `slots` handles, one
  ;; instance, so the process runs once per block however many slots sound.
  (let [{:keys [fused]} (fused-pool (fused-bundle 4))
        kernel (.-kernel fused)
        handles (.-handles fused)]
    (is (= [1 1 1 1] (mapv #(pool/instance-key kernel %) handles))
        "four handles, one instance key")
    (is (= 4 (count handles)))
    (pool/note-on! fused 60 1.0)
    (let [block (float-array 128)]
      (pool/render! fused block 128 48000.0)
      (is (pos? (apply max (map double (vec block))))
          "the instance rendered the one slot that sounds")))
  (testing "and destroying the pool releases the one instance it holds"
    (let [{:keys [fused]} (fused-pool (fused-bundle 4))]
      (pool/destroy-pool! fused)
      (is (false? (deref (.-live? fused)))))))

(deftest stealing-a-fused-slot-leaves-its-neighbour-alone
  ;; The point of per-slot reset (M6a, seam 3): the fused state is partitioned per
  ;; call site, so restoring one slot restores *only* it. Slot 0 is gated shut from
  ;; the start, which makes its contribution exactly zero and lets the block be
  ;; compared bit for bit; a whole-instance reset would restart slot 1's envelope
  ;; at the steal, and that is what this notices.
  (let [{:keys [plain fused]} (fused-pool (fused-bundle 2))
        gate-shut (fn [pool index]
                    (let [values (double-array 4)]
                      (pool/read-controls! pool index values)
                      (aset values 0 0.0)
                      (pool/set-voice-controls! pool index values)))
        a (float-array 128)
        b (float-array 128)]
    (pool/note-on! plain 72 1.0)
    (pool/note-on! fused 69 1.0)
    (pool/note-on! fused 72 1.0)
    (gate-shut fused 0)
    (dotimes [_ 2]
      (pool/render! plain a 128 48000.0)
      (pool/render! fused b 128 48000.0)
      (is (= (vec a) (vec b)) "the two lowerings agree before the steal"))
    (is (= [0 1] (mapv :index (pool/voice-snapshot fused))))
    (is (= 0 (pool/note-on! fused 60 1.0))
        "a third note has no free voice and takes the oldest, which is slot 0")
    (gate-shut fused 0)
    (pool/render! plain a 128 48000.0)
    (pool/render! fused b 128 48000.0)
    (is (= (vec a) (vec b))
        "slot 1 kept its envelope and its delay line across the steal")))

(deftest a-fused-slot-publishes-its-own-activity
  ;; M6 seam 4: `read-specials!` for a fused voice returns that *slot's* activity
  ;; from its call partition -- not the entry's (which has none). Two slots, one
  ;; held and one released past its tail, must read differently, and the sentinel
  ;; makes the assertion bite: a readback that wrote nothing would leave it.
  (let [bundle (hosts/compile-synth
                (assoc fixtures/activity-voice-description :capacity 2)
                {:target :jvm :precision :f32 :containment :off
                 :fusion {:slots 2}})
        kernel (:kernel bundle)
        slot-0 (pool/create-voice kernel 128)
        slot-1 (pool/create-voice kernel 128)
        output (float-array 128)
        target (double-array 1)
        activity (fn [voice]
                   (aset target 0 42.0)
                   (pool/read-specials! kernel voice target)
                   (aget target 0))]
    (pool/write-voice-controls! kernel slot-0 (double-array [1.0 440.0]))
    (pool/write-voice-controls! kernel slot-1 (double-array [1.0 440.0]))
    (pool/mix-voice! kernel slot-0 output 128 48000.0)
    (is (= 1.0 (activity slot-0)) "a sounding slot reads active")
    (is (= 1.0 (activity slot-1)) "as does its neighbour")
    ;; Release slot 0 and let its short tail finish; slot 1 keeps sounding.
    (pool/write-voice-controls! kernel slot-0 (double-array [0.0 440.0]))
    (dotimes [_ 5] (pool/mix-voice! kernel slot-0 output 128 48000.0))
    (is (= 0.0 (activity slot-0)) "the finished slot reads inactive")
    (is (= 1.0 (activity slot-1)) "and its neighbour still reads active")))
