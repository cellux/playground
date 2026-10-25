(ns omkamra.dsp.harness.bench
  "Time one voice on the JVM all three ways: **pool**, **fused**, **fused-inlined**.

  D45 measured a pool of N instances against one `(poly N voice)` artifact and found
  the JVM refusing to compile the unrolled method somewhere between N=12 and N=14.
  That was a property of *inlining*, not of the DSP: `dsp/compile` copied the voice
  into every call site, so the artifact held N copies of it, and D47 changes that
  default. This harness isolates the variables, one arm each:

  - **pool** — N instances of the *voice* artifact, driven by `synth/pool` exactly as
    a host drives them: one note-on per voice, then one `render!` per block;
  - **fused** — the same voice lowered to one `(poly N voice)` process, what
    `dsp/compile` does now: one function per definition, N calls;
  - **fused-inlined** — the *same linked unit* with `:inline true` on every
    definition, which is what every target did before stage 2 and what produced the
    cliff.

  The pool arm is what the boundary-crossing argument needs as data: no annotation
  inside a kernel can remove N host-side kernel invocations per block, so this is the
  only way to see what fusion buys. It runs with `:containment :off` to match
  `dsp/process!` on the fused arms — both then do the same audio work — where a real
  host runs containment (the default) and pays a scan per voice block for it.

  The two fused units are linked from one `(poly N voice)` form and differ only in that
  flag. The voice is supplied to `dsp/link` as a definition rather than resolved by
  name, because the linker resolves a callee through its Var and would otherwise
  discard a descriptor the harness modified.

  Every arm runs on one schedule — the pool's note 69 is 440 Hz, the frequency the
  fused arms are handed — and at N=1 the pool's block is compared **bit for bit**
  against the fused one, which is the strongest statement available about two lowerings
  of the same voice. The two fused arms are compared the same way at every N
  (`java.util.Arrays/equals`, the criterion the conformance suite uses, so NaN is a
  value rather than a difference). `:finite?` reports whether that block is finite at
  all: a schedule that produces NaN is a broken measurement, not a fast one.

  Timing is criterium's, round-robin over all three arms, so their samples alternate
  and drift in the machine reaches each equally. Mean and standard deviation are in
  milliseconds per 128-frame block, the block length D45 used, and artifact size
  beside them, because size is the other half of what D47 claims. The pool's size is
  the *one* voice artifact it instantiates N times — the same number at every N, which
  is the point of it.

  JavaScript and Wasm are not measured here: their artifacts cannot run on the JVM, so
  the browser testbed owns those and has the same three arms. See `DECISIONS.md` D45
  and D47."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [criterium.core :as criterium]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.harness.fixtures :as fixtures]
            [omkamra.dsp.hosts.jvm :as hosts]
            [omkamra.dsp.synth.bundle :as bundle]
            [omkamra.dsp.synth.pool :as pool])
  (:import [java.util Arrays]))

(def block-frames
  "Frames per timed block; D45's procedure."
  128)

(def sample-rate
  "A real sample rate, because it is what makes the voice's audio finite: the
  plain `:invoke` callable passes zero, and `(/ frequency 0.0)` is NaN."
  44100.0)

(def default-voices
  "The voice counts worth seeing: either side of the D45 cliff, and beyond it."
  [4 8 12 14 16 24 32])

(def ^:private criterium-options
  "Criterium settles the JIT far more thoroughly than D45's 200-block warm-up, so
  these are steady-state numbers; the settings keep a full sweep to minutes rather
  than an hour. A DSP kernel allocates nothing, so a forced collection before each
  sample buys nothing and costs a lot."
  {:samples 25
   :warmup-jit-period 2.0e9
   :target-execution-time 2.0e8
   :gc-before-sample false
   :supress-jvm-option-warnings true})

(defn unit
  "The `(poly N voice)` linked unit for one fused arm (D47).

  `:fused` is what `dsp/compile` does now — one function per definition, called;
  `:fused-inlined` is the same unit with every definition asking to be copied, which is
  what every target did before stage 2 and what produced the D45 cliff. The inlined arm
  edits the *linked* unit rather than handing fresh descriptors back to `dsp/link`: the
  unit's definitions are already normalised, and the source schema rejects them
  (`:process`, for one, has been expanded by then)."
  [arm voice voices]
  (let [fused (dsp/link (comb/poly voice {:count voices}))]
    (if (= :fused-inlined arm)
      (update fused :definitions
              (fn [definitions]
                (into (empty definitions)
                      (map (fn [[id definition]] [id (assoc definition :inline true)]))
                      definitions)))
      fused)))

(defn arms
  "Both fused lowerings of `(poly N voice)`."
  [voice voices]
  {:fused (unit :fused voice voices)
   :fused-inlined (unit :fused-inlined voice voices)})

(defn- artifact
  [unit target precision]
  (dsp/compile unit {:target target :entry :process :precision precision}))

(defn- audio-array
  "A mono output buffer of the artifact's physical element type."
  [precision frames]
  (if (= :f64 precision) (double-array frames) (float-array frames)))

(defn- audio-copy
  "A copy of a rendered block, because the benchmark below overwrites the buffer."
  [precision samples]
  (if (= :f64 precision)
    (Arrays/copyOf ^doubles samples (alength ^doubles samples))
    (Arrays/copyOf ^floats samples (alength ^floats samples))))

(defn pool-arm
  "A live pool of `sounding` instances of the *voice* artifact (D45's other arm).

  The same voice the fused arms unroll, driven the way a host drives a pool: one
  note-on per sounding voice — at MIDI note 69, so every voice sounds the 440 Hz the
  fused arms are handed — then one `render!` per block.

  `capacity` is what the pool is sized for and `sounding` how many of those voices
  are actually playing, which is the variable fusion is sensitive to: the pool calls
  its kernel only for sounding slots, while a fused process evaluates every slot it
  was compiled with. Defaults to every slot sounding.

  `capacity` is a host-side number, which is the pool's other point."
  ([capacity target precision] (pool-arm capacity target precision capacity))
  ([capacity target precision sounding]
   (let [bundle (bundle/synth-bundle (assoc fixtures/sine-voice-description
                                            :capacity capacity
                                            :max-block-frames block-frames)
                                     {:target target
                                      :precision precision
                                      :containment :off})
         pool (pool/voice-pool (:runtime bundle)
                               (hosts/host-kernel (:artifact bundle)
                                                  {:containment :off}))
         block (audio-array precision block-frames)]
     (dotimes [_ sounding]
       (pool/note-on! pool 69 1.0))
     {:pool pool :block block :artifact (:artifact bundle)})))

(defn- control-value
  "One fixed schedule for every arm and every voice count: the shared parameters
  from `fixtures/sine-voice-description`, and every slot gated on so each voice
  does its full work."
  [name]
  (let [name (str name)]
    (cond
      (str/starts-with? name "gate") 1.0
      (= name "frequency") 440.0
      (= name "attack") 0.005
      (= name "decay") 0.1
      (= name "sustain") 0.7
      (= name "release") 0.2
      :else 0.0)))

(defn- gate-slot
  "The slot a `gate` control belongs to, or nil for a shared control.

  `poly` expands a per-slot control to `gate_0`, `gate_1`, …; this reads that back,
  so a run can leave the high slots silent without touching anything else."
  [name]
  (when-let [[_ index] (re-find #"^gate[_-](\d+)$" (str name))]
    (parse-long index)))

(defn- context
  "The ABI context `dsp/process!` takes: an output buffer, a real sample rate, and
  the schedule in ABI order. Slots at or above `sounding` are gated off; nil (the
  one-argument arity) leaves every slot gated on."
  ([artifact] (context artifact nil))
  ([artifact sounding]
   (let [ir (:process-ir artifact)
         frames (int block-frames)
         audio (if (= :f64 (:precision ir)) double-array float-array)]
     (cond-> {:output (audio frames)
              :frames frames
              :sample-rate sample-rate
              :controls (mapv (fn [control]
                                (let [slot (gate-slot (:name control))]
                                  (if (and slot sounding (>= slot sounding))
                                    0.0
                                    (control-value (:name control)))))
                              (:controls ir))}
       (some #(= :input (:direction %)) (:buffers ir)) (assoc :input (audio frames))))))

(defn- run-block
  "Run one block, returning the output buffer it wrote."
  [artifact context]
  (dsp/process! artifact context)
  (:output context))

(defn- finite?
  [precision samples]
  (if (= :f64 precision)
    (every? #(not (Double/isNaN (double %))) ^doubles samples)
    (every? #(not (Float/isNaN (float %))) ^floats samples)))

(defn- same-block?
  "Bit for bit: NaN equals NaN here, so a difference is a difference."
  [precision a b]
  (if (= :f64 precision)
    (Arrays/equals ^doubles a ^doubles b)
    (Arrays/equals ^floats a ^floats b)))

(defn- ms
  "Milliseconds from criterium's seconds, taking the point estimate out of its
  bootstrap interval."
  [estimate]
  (* 1000.0 (criterium/point-estimate estimate)))

(defn- sd-ms
  [variance]
  (* 1000.0 (Math/sqrt (criterium/point-estimate variance))))

(defn- measure-row
  "Criterium statistics for one block of each of the three arms, sampled
  round-robin, plus each artifact's size, whether the two fused arms agree on their
  first block, and — at one sounding voice, where the two lowerings compute the same
  audio — whether the pool agrees with them too."
  [voices sounding target precision]
  (let [sounding (min (or sounding voices) voices)
        voice (var-get #'fixtures/sine-voice)
        units (arms voice voices)
        fused (artifact (:fused units) target precision)
        fused-inlined (artifact (:fused-inlined units) target precision)
        fused-context (context fused sounding)
        inlined-context (context fused-inlined sounding)
        ;; Compared before the benchmark runs, and *copied*, because it overwrites
        ;; every one of these buffers.
        fused-block (audio-copy precision (run-block fused fused-context))
        inlined-block (audio-copy precision (run-block fused-inlined inlined-context))
        fused-matches? (same-block? precision fused-block inlined-block)
        finite-block? (and (finite? precision fused-block)
                           (finite? precision inlined-block))
        {:keys [pool block artifact]} (pool-arm voices target precision sounding)
        pool-block (do (pool/render! pool block block-frames sample-rate)
                       (audio-copy precision block))
        [pool-stats fused-stats inlined-stats]
        (criterium/benchmark-round-robin*
         [{:f (fn [] (pool/render! pool block block-frames sample-rate)) :expr-string "pool"}
          {:f (fn [] (dsp/process! fused fused-context)) :expr-string "fused"}
          {:f (fn [] (dsp/process! fused-inlined inlined-context))
           :expr-string "fused-inlined"}]
         criterium-options)]
    {:voices voices
     :pool {:ms (ms (:mean pool-stats))
            :sd (sd-ms (:sample-variance pool-stats))
            :bytes (alength ^bytes (:bytes artifact))}
     :fused {:ms (ms (:mean fused-stats))
             :sd (sd-ms (:sample-variance fused-stats))
             :bytes (alength ^bytes (:bytes fused))}
     :fused-inlined {:ms (ms (:mean inlined-stats))
                     :sd (sd-ms (:sample-variance inlined-stats))
                     :bytes (alength ^bytes (:bytes fused-inlined))}
     :same-output? fused-matches?
     :sounding sounding
     :pool-matches-fused? (when (= 1 sounding)
                            (same-block? precision pool-block fused-block))
     :finite? finite-block?}))

(defn run!
  "Time one voice all three ways for each N, returning one row per voice count:

      {:voices 16
       :pool {:ms 0.310 :sd 0.002 :bytes 2400}
       :fused {:ms 0.166 :sd 0.002 :bytes 2353}
       :fused-inlined {:ms 4.057 :sd 0.216 :bytes 200569}
       :same-output? true
       :finite? true}

  The pool's `:bytes` is its one voice artifact, instantiated N times; the other two
  are the whole fused artifact, so they grow with N and it does not.

  Options: `:voices` (default `default-voices`), `:sounding` (default: every voice
  playing, which is what a full patch looks like), `:target` (default `:jvm`),
  `:precision` (default `:f32`). A row whose arms disagree, or whose audio is not
  finite, is visible rather than silently averaged."
  ([] (run! {}))
  ([{:keys [voices sounding target precision]
     :or {voices default-voices target :jvm precision :f32}}]
   (mapv (fn [count]
           ;; At large N the inlined arm can be too big to build at all, which is a
           ;; result rather than a crash: `:artifact size O(code x call sites)` has a
           ;; hard ceiling.
           (try
             (measure-row count sounding target precision)
             (catch Exception e
               {:voices count :failed (ex-message e)})))
         voices)))

(defn report
  "Render `run!`'s rows as a markdown table."
  [rows]
  (str "| voices | pool ms/block | fused ms/block | fused-inlined ms/block"
       " | pool bytes | fused bytes | fused-inlined bytes | same block | pool = fused | finite |\n"
       "|---|---|---|---|---|---|---|---|---|---|\n"
       (str/join
        "\n"
        (map (fn [{:keys [voices pool fused fused-inlined same-output?
                          pool-matches-fused? finite? failed]}]
               (if failed
                 (format "| %d | — | — | — | — | — | — | — | — | %s |" voices failed)
                 (format (str "| %d | %.3f ± %.3f | %.3f ± %.3f | %.3f ± %.3f"
                              " | %d | %d | %d | %s | %s | %s |")
                         voices
                         (:ms pool) (:sd pool)
                         (:ms fused) (:sd fused)
                         (:ms fused-inlined) (:sd fused-inlined)
                         (:bytes pool) (:bytes fused) (:bytes fused-inlined)
                         same-output? (if (nil? pool-matches-fused?)
                                        "—"
                                        pool-matches-fused?)
                         finite?)))
             rows))))

;; ---------------------------------------------------------------------------
;; The realistic voice: is fusion worth it when the voice does real work?
;; ---------------------------------------------------------------------------
;;
;; The three arms above measure `sine-voice`, which is the smallest thing that is
;; a voice. That answers "what does the boundary cost?" but not "is fusion worth
;; it?", because a voice with a delay line does enough work per sample that the
;; boundary may not matter. `echo-voice` is that voice, and this is the same
;; comparison on it: a pool of N instances against one `(poly N echo-voice)`
;; process, all slots sounding, plus the equivalence check that makes the
;; comparison meaningful — at one voice the two lowerings must produce identical
;; samples, not merely similar ones.

(defn- echo-control-value
  "The schedule the echo arms are handed: every slot open, and the effect
  parameters at the same values the description defaults to, so the pool arm and
  the fused arm receive identical numbers.

  A fused interface spells a per-slot control `frequency_0`, `frequency_1`, ..., so
  the slot suffix is stripped before the name is matched -- otherwise the fused arm
  would be handed `0.0` where the pool arm is handed the note (D52)."
  [name]
  (let [name (str/replace (str name) #"_\d+$" "")]
    (cond
      (str/starts-with? name "gate") 1.0
      (= name "frequency") 440.0
      (= name "delay") 4.5
      (= name "mix") 0.5
      :else 0.0)))

(defn- echo-pool-arm
  "A live pool of `voices` echo voices, `sounding` of them playing."
  [voices target precision sounding]
  (let [bundle (bundle/synth-bundle (assoc fixtures/echo-voice-description
                                           :capacity voices
                                           :max-block-frames block-frames)
                                    {:target target :precision precision
                                     :containment :off})
        pool (pool/voice-pool (:runtime bundle)
                              (hosts/host-kernel (:artifact bundle)
                                                 {:containment :off}))
        block (audio-array precision block-frames)]
    (dotimes [_ sounding]
      (pool/note-on! pool 69 1.0))
    {:pool pool :block block :artifact (:artifact bundle)}))

(defn- echo-fused-artifact
  "One fused `echo-voice` process, built the way the instrument option builds one.

  Deliberately *through* the option rather than a `(poly N voice)` written here:
  the option decides which controls a fused instance expands per slot (D52), and a
  bench that chose its own set would be timing a lowering nobody runs. `guard?`
  selects the per-slot skipping the option applies by default (M6b)."
  [voices target precision guard?]
  (:artifact (bundle/synth-bundle (assoc fixtures/echo-voice-description
                                         :capacity voices
                                         :max-block-frames block-frames)
                                  {:target target :precision precision
                                   :containment :off
                                   :fusion {:slots voices :guard guard?}})))

(defn- echo-context
  "The ABI context for a fused echo artifact: slots below `sounding` open."
  ([artifact] (echo-context artifact nil))
  ([artifact sounding]
   (let [ir (:process-ir artifact)
         frames (int block-frames)
         audio (if (= :f64 (:precision ir)) double-array float-array)]
     {:output (audio frames)
      :frames frames
      :sample-rate sample-rate
      :controls (mapv (fn [control]
                        (let [slot (gate-slot (:name control))]
                          (if (and slot sounding (>= slot sounding))
                            0.0
                            (echo-control-value (:name control)))))
                      (:controls ir))})))

(defn- run-blocks!
  "Run `blocks` blocks of `arm`, returning the elapsed nanoseconds."
  [arm blocks]
  (let [t0 (System/nanoTime)]
    (dotimes [_ blocks] (arm))
    (- (System/nanoTime) t0)))

(defn- block-match?
  "Whether two rendered blocks are identical, bit for bit (NaN as a value)."
  [precision left right]
  (Arrays/equals (if (= :f64 precision) ^doubles left ^floats left)
                 (if (= :f64 precision) ^doubles right ^floats right)))

(defn realistic-row
  "One row of the realistic-voice sweep at `voices` slots, `sounding` of them playing.

  Returns milliseconds per block for the pool and for the fused process, the
  artifacts' sizes, and — at one voice, where the two lowerings compute the same
  audio — whether their first block agrees bit for bit. `:guard false` measures the
  fused lowering without the per-slot skip (M6b)."
  [voices {:keys [target precision blocks warmup sounding guard]
           :or {target :jvm precision :f32 blocks 2000 warmup 500 guard true}}]
  (let [sounding (min (or sounding voices) voices)
        {:keys [pool block artifact]} (echo-pool-arm voices target precision sounding)
        fused (echo-fused-artifact voices target precision guard)
        fused-context (echo-context fused sounding)
        pool-run (fn [] (pool/render! pool block block-frames sample-rate))
        fused-run (fn [] (dsp/process! fused fused-context))
        matches? (when (= 1 voices)
                   (pool/render! pool block block-frames sample-rate)
                   (dsp/process! fused fused-context)
                   (block-match? precision block (:output fused-context)))]
    (dotimes [_ warmup] (pool-run) (fused-run))
    (let [pool-ns (run-blocks! pool-run blocks)
          fused-ns (run-blocks! fused-run blocks)]
      {:voices voices
       :sounding sounding
       :pool-ms (/ (double pool-ns) blocks 1e6)
       :fused-ms (/ (double fused-ns) blocks 1e6)
       :pool-bytes (alength ^bytes (:bytes artifact))
       :fused-bytes (alength ^bytes (:bytes fused))
       :same-block? matches?})))

(defn realistic-sweep!
  "The realistic-voice row for each slot count in `voices`.

  Options: `:voices` (default `[1 2 4 8 16 32]`), `:target`, `:precision`,
  `:blocks`, and `:warmup`."
  ([] (realistic-sweep! {}))
  ([{:keys [voices] :or {voices [1 2 4 8 16 32]} :as options}]
   (mapv (fn [count] (realistic-row count options)) voices)))

