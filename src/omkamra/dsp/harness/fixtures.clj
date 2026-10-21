(ns omkamra.dsp.harness.fixtures
  (:require [omkamra.dsp :as dsp]
            ;; Load-bearing, and invisible to clj-kondo: the fixtures below call
            ;; these definitions from *quoted* bodies, and the linker resolves a
            ;; qualified symbol with `find-ns`, which only sees namespaces that
            ;; are already loaded. A require whose names appear nowhere in code
            ;; is still doing work here.
            [omkamra.dsp.harness.call-fixture]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.filter :as filter]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.instrument :as inst]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.event :as event]
            [omkamra.dsp.schedule :as schedule])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(omkamra.dsp/defn gain
  [sample amount]
  (* sample amount))

(omkamra.dsp/defn add-bias
  [sample]
  (+ sample 0.25))

(omkamra.dsp/defn follow-amount
  "Output the `amount` control every frame.

  A generator whose samples *are* the applied control value, so a timestamped
  control change is directly visible in the rendered output. It is the kernel
  the worker-to-worklet event-transport check renders."
  [amount]
  {:process {:input false :controls [{:name amount}]}}
  amount)

(def one-pole (:definition (omkamra.dsp.harness.catalog/case-by-id :one-pole)))

;; A named state-record shape and a definition that uses it, so the linker's
;; `:struct` resolution is exercised from real source.
(omkamra.dsp/defstruct test-pole-state
  [previous])

(omkamra.dsp/defn struct-pole
  [sample coefficient]
  {:state [{:name history :struct test-pole-state}]
   :process {:controls [{:name coefficient}]}}
  (let [previous (field history :previous)]
    (do (set-field! history :previous (+ sample (* coefficient previous)))
        previous)))

;; A struct-level option is a default for every state slot that references it.
(omkamra.dsp/defstruct constant-shape
  [a]
  {:constant true})

(omkamra.dsp/defn constant-shape-write
  [sample]
  {:state [{:name z :struct constant-shape}] :process {}}
  (do (set! (:a z) sample) (:a z)))

(defn thread-allocation-meter
  []
  (let [bean (ManagementFactory/getThreadMXBean)]
    (when (and (instance? ThreadMXBean bean)
               (.isThreadAllocatedMemorySupported ^ThreadMXBean bean))
      (when-not (.isThreadAllocatedMemoryEnabled ^ThreadMXBean bean)
        (.setThreadAllocatedMemoryEnabled ^ThreadMXBean bean true))
      {:bean bean
       :thread-id (.getId (Thread/currentThread))})))

(defn allocated-bytes
  [{:keys [bean thread-id]} f]
  (let [before (.getThreadAllocatedBytes ^ThreadMXBean bean thread-id)]
    (f)
    (- (.getThreadAllocatedBytes ^ThreadMXBean bean thread-id) before)))

(defn raw-process-noop
  [_artifact _input _output _frames _sample-rate _float-controls _int-controls]
  nil)

(omkamra.dsp/defn clip
  [sample lower upper]
  (if (< sample lower)
    lower
    (if (> sample upper)
      upper
      sample)))

(def gated-one-pole (:definition (omkamra.dsp.harness.catalog/case-by-id :gated-one-pole)))

(omkamra.dsp/defn sum-count
  [count]
  (let [i 0.0
        sum 0.0]
    (do
      (while (< i count)
        (do
          (set! sum (+ sum i))
          (set! i (+ i 1.0))))
      sum)))

(omkamra.dsp/defn boolean-gate
  [sample lower upper]
  (if (and (>= sample lower)
           (not (> sample upper)))
    sample
    0.0))

(omkamra.dsp/defn shifted-buffer
  [sample amount]
  (do
    (buffer-store :output 1 (+ sample amount))
    (+ (buffer-load :input 1) sample)))

(omkamra.dsp/defn channel-mix
  [sample]
  (+ sample (buffer-load :input 1 0)))

;; D40: one evaluation per frame; a multi-channel body writes its output
;; channels explicitly. `out0 = in0 + in1`, `out1 = in0 - in1`. Writing `:output`
;; is what makes it a process, so no `:process` map is needed even though the
;; body is `:void`.
(omkamra.dsp/defn stereo-cross
  [sample]
  (buffer-store :output 0 0 (+ sample (buffer-load :input 1 0)))
  (buffer-store :output 1 0 (- sample (buffer-load :input 1 0))))

;; D40: a *stateful* generator writing two channels from one shared state slot.
;; If the model looped channels, `counter` would advance once per channel; the
;; test asserts it advances once per frame instead.
(omkamra.dsp/defn stereo-state-ramp
  []
  {:process {:input false :controls []}
   :state [{:name counter :type :int :init 0
            :next (+ counter 1)}]}
  (do
    (buffer-store :output 0 0 (int->float counter))
    (buffer-store :output 1 0 (int->float (* 2 counter)))))

(omkamra.dsp/defn explicit-gain
  [{:params [{:name sample :type :float}
             {:name amount :type :float}]
    :return-type :float
    :process {:input :sample
              :controls ['amount]}
    :options {:checks :none}}
   (* sample amount)])

(omkamra.dsp/defn declared-development-gain
  "A definition that asks for runtime validation, which only the interpreter
  implements. Declaring it in the definition must count as asking for it."
  [{:params [{:name sample :type :float}]
    :return-type :float
    :process {}
    :options {:checks :development}}
   sample])

(omkamra.dsp/defn call-fixture-twice
  [sample]
  (omkamra.dsp.harness.call-fixture/double-sample (omkamra.dsp.harness.call-fixture/double-sample sample)))

(omkamra.dsp/defn call-fixture-chain
  [sample]
  (omkamra.dsp.harness.call-fixture/add-one (call-fixture-twice sample)))

(omkamra.dsp/defn call-fixture-arena
  "Two partitions of one stateful callee, plus a callee whose state is integer.

  The entry's own state sits between the two `accumulate` partitions, so a base
  derived from the wrong index reads the *other* site's accumulator rather than
  merely something wrong, and the two storages are both in play."
  [sample]
  {:state [{:name own :type :float :init 1.0 :next own}]}
  (+ (+ own (omkamra.dsp.harness.call-fixture/accumulate sample))
     (+ (omkamra.dsp.harness.call-fixture/accumulate (* sample 0.5))
        (omkamra.dsp.harness.call-fixture/counting-bias sample))))

(omkamra.dsp/defn call-fixture-nested
  "A callee that calls a stateful callee, so a method passes on a derived base."
  [sample]
  {:state [{:name own :type :float :init 0.5 :next own}]}
  (+ own (omkamra.dsp.harness.call-fixture/accumulate-twice sample)))

(omkamra.dsp/defn call-fixture-int-led
  "An entry whose own int slot leaves the callee's partition four bytes into an f64
  arena (D47). `call-fixture/mixed-state` explains what that is for."
  [sample]
  {:state [{:name ticks :type :int :init 0 :next (+ ticks 1)}]}
  (+ (omkamra.dsp.harness.call-fixture/mixed-state sample 0.5)
     (int->float ticks)))

(omkamra.dsp/defn call-fixture-inlined
  "A call site of an `^:inline` callee, which stays copied on every target."
  [sample]
  {:state [{:name own :type :float :init 0.0 :next own}]}
  (+ own (omkamra.dsp.harness.call-fixture/inlined-gain sample 2.0)))

(omkamra.dsp/defn naive-saw
  [sample frequency]
  {:state [{:name phase
            :init 0.0
            :next (fract (+ phase (/ frequency sample-rate)))}]}
  (- (* 2.0 phase) 1.0))

(omkamra.dsp/defn stereo-sum
  [sample width delay]
  {:process {:controls [{:name width} {:name delay}]}}
  (let [out (omkamra.dsp.library.effect/stereo-spread sample width delay)]
    (+ (:left out) (:right out))))

(omkamra.dsp/defn ramp-generator
  "A generator: `:input false`, so every parameter is a control (D23)."
  [increment]
  {:state [{:name s :type :float :init 0.0 :next (+ s increment)}]
   :process {:input false}}
  s)

;; --- P9: timestamped events and block-relative scheduling -------------------

(defn array-set!
  "Set one element of an f32/f64 primitive array, chosen at run time."
  [array index value precision]
  (java.lang.reflect.Array/set array (int index)
                               (if (= :f64 precision) (double value) (float value))))

(defn array-get
  [array index]
  (double (java.lang.reflect.Array/get array (int index))))

(defn event-queue-of
  "An ingress queue holding a `[frame value]` parameter timeline for one control."
  ([timeline] (event-queue-of :coefficient timeline))
  ([control-name timeline]
   (let [queue (omkamra.dsp.event/event-queue (max 4 (count timeline)))]
     (doseq [[frame value] timeline]
       (omkamra.dsp.event/push-event! queue
                                      (omkamra.dsp.event/validate-event {:frame frame :sequence frame
                                                                         :type :parameter :name control-name
                                                                         :value value})
                                      0))
     queue)))

;; Five parameter changes inside one 200-frame block, including two adjacent
;; frames (40 and 41) so a one-frame dispatch error cannot hide.
(def event-timeline [[17 0.1] [40 0.9] [41 0.2] [155 0.75] [199 0.5]])

(def event-input (mapv #(Math/sin (* 0.05 %)) (range 200)))

(defn render-timeline
  "Render the 200-frame automation timeline in blocks of `block-size`.

  The input is sliced per block, and `render-block!` slices it again per segment,
  so this drives the whole host path rather than a shortcut."
  [block-size target precision]
  (let [artifact (omkamra.dsp/compile #'omkamra.dsp.library.filter/one-pole
                                      {:target target :entry :process :precision precision})
        total 200
        array (if (= :f64 precision) double-array float-array)
        output (array total)
        block-in (array block-size)
        block-out (array block-size)
        input-scratch (array block-size)
        output-scratch (array block-size)
        scheduler (omkamra.dsp.schedule/scheduler (event-queue-of event-timeline))
        process! (fn [context] (omkamra.dsp/process! artifact context))]
    (loop [start 0
           controls [0.5]]
      (if (>= start total)
        (mapv double (vec output))
        (let [length (min block-size (- total start))]
          (dotimes [index length]
            (array-set! block-in index (nth event-input (+ start index)) precision))
          (let [result (omkamra.dsp.schedule/render-block!
                        scheduler
                        {:start start
                         :frames length
                         :input block-in
                         :output block-out
                         :input-scratch input-scratch
                         :output-scratch output-scratch
                         :sample-rate 48000.0
                         :controls controls
                         :control-index {:coefficient 0}
                         :process! process!})]
            (dotimes [index length]
              (array-set! output (+ start index) (array-get block-out index) precision))
            (recur (+ start length) (:controls result))))))))

(defn render-with-timeline
  [timeline]
  (let [artifact (omkamra.dsp/compile #'omkamra.dsp.library.filter/one-pole
                                      {:target :interpreter :entry :process :precision :f32})
        block-in (float-array 200)
        block-out (float-array 200)
        scratch (float-array 200)
        scheduler (omkamra.dsp.schedule/scheduler (event-queue-of timeline))]
    (dotimes [index 200]
      (aset block-in index (float (nth event-input index))))
    (omkamra.dsp.schedule/render-block!
     scheduler
     {:start 0 :frames 200 :input block-in :output block-out
      :input-scratch scratch :output-scratch scratch
      :sample-rate 48000.0 :controls [0.5]
      :control-index {:coefficient 0}
      :process! (fn [context] (omkamra.dsp/process! artifact context))})
    (mapv double (vec block-out))))

(defn render-single-parameter-event
  "Render the timeline with one parameter event at `frame` (or none when nil)."
  [frame]
  (let [timeline (if frame [[frame 0.1]] [])]
    (render-with-timeline timeline)))

(defn render-manual-split
  "Render the same 200 frames split by hand: coefficient `before` up to `split`,
  then `after`. This is what a sample-accurate dispatch at `split` must equal."
  [split after]
  (let [artifact (omkamra.dsp/compile #'omkamra.dsp.library.filter/one-pole
                                      {:target :interpreter :entry :process :precision :f32})
        output (float-array 200)]
    (doseq [[start length coefficient] [[0 split 0.5] [split (- 200 split) after]]]
      (let [input (float-array length)
            out (float-array length)]
        (dotimes [index length]
          (aset input index (float (nth event-input (+ start index)))))
        (omkamra.dsp/process! artifact {:input input :output out :frames length
                                        :sample-rate 48000.0 :controls [coefficient]})
        (dotimes [index length]
          (aset output (+ start index) (aget out index)))))
    (mapv double (vec output))))

(defn first-difference
  [left right]
  (first (keep-indexed (fn [index [a b]] (when (not= a b) index))
                       (map vector left right))))

;; A target change inside the block, so smoothing has something to work on.
(def smoothed-targets [[17 0.9] [143 0.2]])

(defn render-automation
  "Render the shared 200-frame input through `definition-var` in blocks of
  `block-size`, applying `targets` as parameter events to the single control."
  [definition-var control-name targets block-size target precision]
  (let [artifact (omkamra.dsp/compile definition-var {:target target :entry :process
                                                      :precision precision})
        total 200
        array (if (= :f64 precision) double-array float-array)
        output (array total)
        block-in (array block-size)
        block-out (array block-size)
        input-scratch (array block-size)
        output-scratch (array block-size)
        scheduler (omkamra.dsp.schedule/scheduler (event-queue-of control-name targets))
        process! (fn [context] (omkamra.dsp/process! artifact context))]
    (loop [start 0
           controls [0.0]]
      (if (>= start total)
        (mapv double (vec output))
        (let [length (min block-size (- total start))]
          (dotimes [index length]
            (array-set! block-in index (nth event-input (+ start index)) precision))
          (let [result (omkamra.dsp.schedule/render-block!
                        scheduler
                        {:start start
                         :frames length
                         :input block-in
                         :output block-out
                         :input-scratch input-scratch
                         :output-scratch output-scratch
                         :sample-rate 48000.0
                         :controls controls
                         :control-index {control-name 0}
                         :process! process!})]
            (dotimes [index length]
              (array-set! output (+ start index) (array-get block-out index) precision))
            (recur (+ start length) (:controls result))))))))

(defn render-gain
  "Drive one of the declared-mode gain definitions from a constant target, with
  an all-ones input so the output *is* the control's value."
  [definition-var target-value frames]
  (let [artifact (omkamra.dsp/compile definition-var {:target :interpreter :entry :process})
        output (float-array frames)]
    (omkamra.dsp/process! artifact {:input (float-array (repeat frames 1.0))
                                    :output output
                                    :frames frames
                                    :sample-rate 48000.0
                                    :controls [target-value]})
    (mapv double (vec output))))

(defn one-pole-samples
  "The f32 one-pole recurrence `y += (target - y) * coefficient`, computed here
  with an explicit rounding after every operation, as an independent oracle for
  what the kernel's per-frame update must produce."
  [target coefficient frames]
  (loop [index 0
         y (float 0.0)
         values []]
    (if (= index frames)
      values
      (recur (inc index)
             (float (+ y (float (* (float (- target y)) (float coefficient)))))
             (conj values (double y))))))

(defn abi-controls
  [definition-var]
  (mapv #(select-keys % [:name :type :smooth :slew])
        (:controls (:abi (omkamra.dsp/compile definition-var {:target :interpreter
                                                              :entry :process})))))

(omkamra.dsp/defn lane-gain
  "Gain whose amount arrives one float per frame from a host-supplied lane."
  [sample amount]
  {:process {:controls [{:name amount :lane true}]}}
  (* sample amount))

(omkamra.dsp/defn lane-smoothed-gain
  "The same lane, additionally smoothed by the kernel: the smoother's target is
  the per-frame lane value, so a host curve and kernel smoothing compose."
  [sample amount]
  {:process {:controls [{:name amount :lane true :smooth 0.5}]}}
  (* sample amount))

(omkamra.dsp/defn smoothed-amount
  "A generator whose whole output *is* one smoothed control, so a duplicated ramp
  is visible in the sample itself: two poles make the value move at roughly half
  speed through the first frames. The control's `:default` is the ramp's start
  and the description's parameter is its target."
  [gate amount]
  {:process {:input false
             :controls [{:name gate}
                        {:name amount :smooth 0.5 :default 0.0}]}}
  amount)

(def smoothed-amount-description
  "A synth description over `smoothed-amount`: a module-shaped voice with a `gate`
  control, which is exactly what `synthesize` wraps in `guard` and `poly` (F1).
  `amount` starts at 0 in the voice and is resolved to 1 by the pool, so the first
  block shows whether its ramp is one pole or two."
  {:id :smoothed-amount
   :capacity 2
   :max-block-frames 8
   :parameters {:amount {:default 1.0}}
   :voice {:definition #'smoothed-amount}})

(omkamra.dsp/defn two-lane-mix
  "Two independent lanes in one region."
  [sample a b]
  {:process {:controls [{:name a :lane true} {:name b :lane true}]}}
  (* sample (+ a b)))

(omkamra.dsp/defn lane-carried-control
  "A control the host sets block-rate, with an *optional* per-frame lane beside
  it that the kernel composes through the `(lane control)` accessor. With no
  lane data the lane reads zero, so the scalar stands unchanged."
  [amount]
  {:process {:input false :controls [{:name amount :has-lane true}]}}
  (+ amount (lane amount)))

(omkamra.dsp/defn lane-on-frequency
  "A lane beside a *runtime* control: the pool resolves `frequency` per voice, so
  its lane is per voice too (M4c)."
  [frequency]
  {:process {:input false :controls [{:name frequency :has-lane true}]}}
  (+ frequency (lane frequency)))

(omkamra.dsp/defn lane-on-bend
  "A lane beside a *per-channel* runtime control: `bend` is the pool's global
  source bank indexed by channel, so its lane is a curve per channel and each
  voice reads the curve of the channel it is on (H3)."
  [bend]
  {:process {:input false :controls [{:name bend :has-lane true}]}}
  (+ bend (lane bend)))

(omkamra.dsp/defn shared-gain
  "A control read from the process's *shared* region: one value for every
  instance of the process, written once by the host rather than once per voice
  (M5). The region has the same shape as a lane region, so a host may also write
  a per-frame global curve."
  [amount]
  {:process {:input false :controls [{:name amount :shared true}]}}
  (+ amount 1.0))

(omkamra.dsp/defn lane-only-automation
  "A control that exists *only* as a lane: the ABI reports it under `:lanes` and
  not among the scalar controls, so a host's parameter declaration is the only
  thing that says whose curve it is."
  [amount]
  {:process {:input false :controls [{:name amount :lane true}]}}
  (* 2.0 amount))

(omkamra.dsp/defn gated-lane-only
  "A lane-only control beside a `gate`, the per-slot control that lets a fused
  build tell its slots apart at all (F4). The automation has no scalar and is
  hoisted, so nothing about the *control* distinguishes one slot from another:
  the only thing that can is which slice of the lane region a call site reads."
  [gate amount]
  {:process {:input false
             :controls [{:name gate} {:name amount :lane true}]}}
  (* 2.0 amount))

(def gated-lane-only-description
  "A synth description over `gated-lane-only`: one per-voice lane with no
  scalar at all, and the runtime control that makes the voice fusable."
  {:id :gated-lane-only
   :capacity 2
   :max-block-frames 8
   :parameters {:amount {:scope :per-voice :default 10.0}}
   :voice {:definition #'gated-lane-only}})

(omkamra.dsp/defn gated-lane-carried
  "A `gate` beside a control that keeps its scalar *and* carries a lane
  (`:has-lane`), which is the per-slot case: the control is expanded per slot, so
  each slot has its own scalar, while the lane is one lane the slots read from
  their own slices (F4). The `gate` is also what `guard` latches on, so the voice
  has state of its own to fuse with."
  [gate amount]
  {:process {:input false
             :controls [{:name gate} {:name amount :has-lane true}]}}
  (+ amount (lane amount)))

(def gated-lane-carried-description
  "A synth description over `gated-lane-carried`: the per-voice lane of
  `lane-voice-description`, in a voice that can be fused."
  {:id :gated-lane-carried
   :capacity 2
   :max-block-frames 8
   :parameters {:amount {:scope :per-voice :default 10.0}}
   :voice {:definition #'gated-lane-carried}})

(omkamra.dsp/defn bus-gain-voice
  "A voice whose whole output *is* its gate, so it renders exactly 1.0 for every
  frame it sounds. A post-mix stage's gain is then the output itself, which is
  what makes the chain's per-frame delivery legible in the samples (H2)."
  [gate]
  {:process {:input false :controls [{:name gate}]}}
  gate)

(def bus-lane-description
  "A synth description over `bus-gain-voice`: one voice, and the instrument
  parameter a post-mix stage is driven by."
  {:id :bus-lane
   :capacity 2
   :max-block-frames 8
   :parameters {:post-amount {:scope :shared :default 1.0}}
   :voice {:definition #'bus-gain-voice}})

(omkamra.dsp/defn shared-post-gain
  "A post-mix stage whose gain is read from the bus's *shared* region rather than
  as a block-rate scalar: one region for the whole instrument, so a global curve
  reaches every channel's instance without being written once per channel (M5,
  one level up from the voices)."
  [sample amount]
  {:process {:controls [{:name amount :shared true}]}}
  (* sample amount))

(def bus-lane-instrument
  "An instrument whose *post-mix chain* declares a lane (H2).

  The chain is one mono processor run once per output channel, and this stage's
  gain arrives one float per frame from the instrument's lane region — so it is
  the chain's own delivery the samples show, with the voice contributing exactly
  1.0 while it sounds."
  (-> (inst/instrument bus-lane-description)
      (inst/with-effect lane-gain [:post-amount])))

(def bus-shared-instrument
  "The same chain with its stage reading a shared control instead: the bus's own
  per-frame region, filled from the instrument parameter of the same name (H2)."
  (-> (inst/instrument bus-lane-description)
      (inst/with-effect shared-post-gain [:post-amount])))

(omkamra.dsp/defn stateless-voice
  "A voice with *no state at all*: it renders the note it was started on, scaled
  by its velocity, so a fused pool of them is legible in the samples and there is
  nothing a slot's reset could restore (H4)."
  [note velocity]
  {:process {:input false}}
  (* note velocity))

(def stateless-voice-description
  "A synth description over `stateless-voice`: two of them can share one instance
  without either having anything of its own."
  {:id :stateless
   :capacity 2
   :max-block-frames 8
   :voice {:definition #'stateless-voice}})

(omkamra.dsp/defn lane-in-callee
  "A callee that reaches for a lane. A callee has no lane region of its own, so
  the region is handed to it: the entry laid it out, and a call site passes it on
  (F3). Reading `(lane amount)` here therefore reads the *caller's* slice for
  `amount`, not a region of this definition's own."
  [amount]
  {:process {:input false}}
  (+ amount (lane amount)))

(omkamra.dsp/defn lane-carrying-caller
  "Calls `lane-in-callee`, so the accessor has to lower inside a callee module."
  [amount]
  {:process {:input false :controls [{:name amount :has-lane true}]}}
  (lane-in-callee amount))

(def lane-voice-description
  "A synth description over `lane-carried-control`. `amount` is *shared*, which
  is one curve every voice reads (M4a); `assoc` in `:per-voice` for the per-voice
  case (M4c)."
  {:id :lane-voice
   :capacity 2
   :max-block-frames 8
   :parameters {:amount {:scope :shared :default 10.0}}
   :voice {:definition #'lane-carried-control}})

(def shared-voice-description
  "A synth description over `shared-gain`: `amount` is read from the pool-wide
  shared region rather than from a voice's control storage, so a host writes it
  once for the whole pool (M5)."
  {:id :shared-voice
   :capacity 2
   :max-block-frames 8
   :parameters {:amount {:scope :shared :default 0.0}}
   :voice {:definition #'shared-gain}})

(def lane-frequency-description
  "A synth description over `lane-on-frequency`: `frequency` is a runtime control
  the pool resolves per voice, so its lane is per voice with no declaration."
  {:id :lane-frequency
   :capacity 2
   :max-block-frames 8
   :voice {:definition #'lane-on-frequency}})

(def lane-bend-description
  "A synth description over `lane-on-bend`, whose lane is per channel."
  {:id :lane-bend
   :capacity 1
   :max-block-frames 8
   :voice {:definition #'lane-on-bend}})

(def lane-only-description
  "A synth description over `lane-only-automation`, whose only control is a lane."
  {:id :lane-only
   :capacity 1
   :max-block-frames 8
   :voice {:definition #'lane-only-automation}})

(defn render-lane
  [definition-var lanes frames]
  (let [artifact (omkamra.dsp/compile definition-var {:target :interpreter :entry :process})
        output (float-array frames)]
    (omkamra.dsp/process! artifact {:input (float-array (repeat frames 1.0))
                                    :lanes lanes
                                    :output output
                                    :frames frames
                                    :sample-rate 48000.0
                                    :controls []})
    (mapv double (vec output))))

(defn render-on
  "Render one block on a target that exposes the context adapter, with lanes."
  [definition-var target precision lanes frames]
  (let [artifact (omkamra.dsp/compile definition-var {:target target :entry :process
                                                      :precision precision})
        real (fn [values] (if (= :f64 precision) (double-array values) (float-array values)))
        output (real (repeat frames 0.0))]
    (omkamra.dsp/process! artifact {:input (real (repeat frames 1.0))
                                    :lanes (into {} (map (fn [[lane values]] [lane (real values)])) lanes)
                                    :output output
                                    :frames frames
                                    :sample-rate 48000.0
                                    :controls []})
    (mapv double (vec output))))

;; ---------------------------------------------------------------------------
;; Synth runtime fixtures (P10)
;; ---------------------------------------------------------------------------

(omkamra.dsp/defn sine-voice
  "The smallest thing that is a voice: a sine carrier shaped by an exponential
  ADSR. Both parts are stateful library modules, so a voice exercises P6
  composition end to end through the pool as well.

  The library calls are fully qualified on purpose. `dsp/call` resolves a
  qualified symbol against the aliases of the namespace that *initiates the
  link* -- here a test namespace -- so an alias that only exists in this file
  would not resolve."
  [gate frequency attack decay sustain release]
  {:process {:input false}}
  (* (omkamra.dsp.library.envelope/exponential-adsr gate attack decay sustain release)
     (omkamra.dsp.library.oscillator/sine-oscillator frequency)))

(omkamra.dsp/defn echo-voice
  "A *realistic* voice, for the fusion question: a generator with a delay line in
  it. The line's buffer is a record field — scratch — and D47 keeps scratch
  inlined, so `poly` has to copy the whole line into every slot. This is the voice
  the pool's `sine-voice` measurement cannot stand in for (M6, D45)."
  [gate frequency delay mix]
  {:process {:input false}}
  (let [dry (omkamra.dsp.library.oscillator/sine-oscillator frequency)
        wet (omkamra.dsp.library.delay/delay-line dry delay)]
    (* (omkamra.dsp.library.envelope/exponential-adsr gate 0.005 0.1 0.7 0.2)
       (+ (* (- 1.0 mix) dry) (* mix wet)))))

(def echo-voice-description
  "A synth description over `echo-voice`: the *realistic* profile for the fusion
  question (M6, D45). The two effect parameters default to the values the fused
  arms are handed, so a pool of these voices and one `(poly S echo-voice)` process
  are driven with identical numbers and can be compared sample for sample."
  {:id :echo-voice
   :capacity 4
   :max-block-frames 128
   :parameters {:delay {:default 4.5}
                :mix {:default 0.5}}
   :voice {:definition #'echo-voice}})

(def sine-voice-description
  "A synth description over `sine-voice`. The definition is a Var here because
  that is convenient in a test; normalisation turns it into the symbol that
  makes a description serialisable."
  {:id :sine-voice
   :capacity 4
   :max-block-frames 128
   :parameters {:attack {:default 0.005}
                :decay {:default 0.1}
                :sustain {:default 0.7}
                :release {:default 0.2}}
   :voice {:definition #'sine-voice}})

(dsp/defn activity-voice
  "A module-shaped generator that publishes `:activity`, derived from a linear
  envelope: active while the envelope sounds, finished once it has reached zero.
  The release tail (~0.01 s here, a few blocks) is load-bearing for the test — an
  activity that dropped to 0 the instant the gate closed could not tell a working
  per-slot readback from a readback that returned nothing (both read 0). `poly`
  can stack it because it returns a signal rather than writing the process's
  buffers."
  [gate frequency]
  {:process {:input false}}
  (let [env (omkamra.dsp.library.envelope/linear-adsr gate 0.005 0.1 0.7 0.01)]
    (do
      (store-special :activity (if (<= env 0.0) 0 1))
      (* env (omkamra.dsp.library.oscillator/sine-oscillator frequency)))))

(def activity-voice-description
  "A synth description over `activity-voice`: the fixture for a fused pool that
  frees a released slot from its own `:activity` (M6 seam 4)."
  {:id :activity-voice
   :capacity 2
   :max-block-frames 128
   :voice {:definition #'activity-voice}})

;; ---------------------------------------------------------------------------
;; The browser host (P10d)
;; ---------------------------------------------------------------------------
;;
;; A stereo, stateful voice that declares `:activity`, so the browser pool test
;; exercises what a JS/Wasm kernel has to provide beyond rendering: two planar
;; channels, per-frame shared state, a readable special slot, and freeing a
;; voice after release.
(dsp/defn browser-stereo-voice
  [gate]
  {:process {:input false}
   :state [{:name phase :type :float :init 0.0
            :next (+ phase 1.0)}]}
  (do
    (store-special :activity (if (<= gate 0.0) 0 1))
    (buffer-store :output 0 0 (* gate (* 0.001 phase)))
    (buffer-store :output 1 0 (* gate (- 0.0 (* 0.001 phase))))))

(def browser-stereo-description
  {:id :browser-stereo
   :capacity 2
   :mode :poly
   :max-block-frames 128
   :voice {:definition #'browser-stereo-voice}})

(dsp/defn fused-browser-voice
  "A *module-shaped* voice for the browser fused arms (M6c): it returns a signal
  rather than writing the caller's buffers, so `poly` can stack it, and it has
  both a per-voice envelope (state) and a delay line (scratch), so a per-slot
  reset has something to restore and a whole-instance reset would be noticed. It
  is arithmetic — no transcendental — so the differential check against the
  interpreter is exact. Like the JVM fixture it publishes `:activity` from an
  envelope with a short tail, so a readback that returned nothing would free the
  released slot a block early."
  [gate frequency delay mix]
  {:process {:input false}}
  (let [envelope (omkamra.dsp.library.envelope/linear-adsr gate 0.005 0.1 0.7 0.01)
        dry (* gate frequency)
        wet (omkamra.dsp.library.delay/delay-line dry delay)]
    (do
      (store-special :activity (if (<= envelope 0.0) 0 1))
      (* envelope (+ (* (- 1.0 mix) dry) (* mix wet))))))

(def fused-browser-description
  "A synth description over `fused-browser-voice`: the browser fused arms render a
  pool of these against one `(poly slots voice)` process (M6c)."
  {:id :fused-browser
   :capacity 2
   :max-block-frames 128
   :stealing :oldest
   :parameters {:delay {:default 4.5}
                :mix {:default 0.5}}
   :voice {:definition #'fused-browser-voice}})

;; ---------------------------------------------------------------------------
;; The containment fixtures (P10c)
;; ---------------------------------------------------------------------------
;;
;; Shared by the JVM suite (`omkamra.dsp.tests.synth`) and the browser testbed,
;; so the containment policies are exercised against the same voices everywhere.
(dsp/defn containment-runaway-voice
  "Divides a constant by the gate, so a released voice produces an infinity."
  [gate note]
  {:process {:input false}}
  (/ 1.0 gate))

(dsp/defn containment-loud-voice
  "Multiplies by a huge constant, so a held voice produces a finite runaway."
  [gate note]
  {:process {:input false}}
  (* gate 1.0e9))

(dsp/defn containment-reset-voice
  "Divides by an ascending age starting at 0, so the first block is non-finite
  and later blocks are finite; `:reset` returns the age to 0, `:sanitize` does
  not."
  [gate note]
  {:process {:input false}
   :state [{:name age :type :int :init 0
            :next (min 2 (+ age 1))}]}
  (/ 1.0 (int->float age)))

(def containment-runaway-description
  {:id :containment-runaway :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'containment-runaway-voice}})

(def containment-loud-description
  {:id :containment-loud :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'containment-loud-voice}})

(def containment-reset-description
  {:id :containment-reset :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'containment-reset-voice}})
