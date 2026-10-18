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

(omkamra.dsp/defn two-lane-mix
  "Two independent lanes in one region."
  [sample a b]
  {:process {:controls [{:name a :lane true} {:name b :lane true}]}}
  (* sample (+ a b)))

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

(def sine-voice-description
  "A synth description over `sine-voice`. The definition is a Var here because
  that is convenient in a test; normalisation turns it into the symbol that
  makes a description serialisable."
  {:id :sine-voice
   :capacity 4
   :max-block-frames 128
   :voice {:definition #'sine-voice
           :controls {:gate {:source :gate}
                      :frequency {:source :frequency}
                      :attack {:constant 0.005}
                      :decay {:constant 0.1}
                      :sustain {:constant 0.7}
                      :release {:constant 0.2}}}})

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
   :voice {:definition #'browser-stereo-voice
           :controls {:gate {:source :gate}}}})

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
   :voice {:definition #'containment-runaway-voice
           :controls {:gate {:source :gate} :note {:source :note}}}})

(def containment-loud-description
  {:id :containment-loud :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'containment-loud-voice
           :controls {:gate {:source :gate} :note {:source :note}}}})

(def containment-reset-description
  {:id :containment-reset :capacity 1 :mode :poly :max-block-frames 64
   :voice {:definition #'containment-reset-voice
           :controls {:gate {:source :gate} :note {:source :note}}}})
