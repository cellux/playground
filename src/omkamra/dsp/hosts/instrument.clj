(ns omkamra.dsp.hosts.instrument
  "Lower an `omkamra.dsp.instrument` value to a **program**, and run it.

  The Clojure instrument states voices, polyphony, and a post-mix chain; this
  namespace decides what that becomes. The first lowering folds the effect chain
  into one mono bus definition, compiles it to one kernel, and reuses the P10
  voice machinery (`synth-bundle`) for the pool. A *program* is data plus
  artifacts, so the same value can be lowered differently later — an unrolled
  poly kernel, a stereo bus, a fused instrument kernel — without touching the
  source layer."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.graph :as graph]
            [omkamra.dsp.hosts :as hosts]
            [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.instrument :as instrument]
            [omkamra.dsp.synth.pool :as pool]))

;; ---------------------------------------------------------------------------
;; The post-mix chain, lowered
;; ---------------------------------------------------------------------------

(dsp/defn bus-input
  "The identity mono processor a post-mix chain starts from: the summed voices,
  one output channel's worth."
  [sample]
  {:process {:controls []}}
  sample)

(defn- control-declarations
  "A processor's control declarations, keyed by name.

  A `:process` says what *kind* of control each of its parameters is: whether it
  is a per-frame lane (`:lane`, `:has-lane`), a shared variable, a `:smooth` /
  `:slew` value, or carries a `:default`. A derived interface has to carry those
  across, or a post-mix control silently loses the kind it was declared with."
  [processor]
  (into {}
        (map (fn [control] [(:name control) control]))
        (get-in processor [:process :controls])))

(defn- chain-effect
  "Wrap processor `bus` in processor `effect`, extending the interface with the
  effect's controls.

  This is `comb/through` for a *processor*: `bus`'s first parameter is its signal
  input, kept at the front, and the effect's controls (the symbols in
  `arguments`) are appended. The body is `(effect (bus …) arguments…)`.

  Each control keeps the declaration it was written with, whether it came from
  the accumulated bus or from the effect just wrapped. The declaration has to
  reach the *entry* process: the derived body calls the effect, and the effect
  evaluates its own controls, but the region a lane or a shared control is read
  from belongs to the entry, so this is where the compiler looks for it."
  [bus effect arguments]
  (let [bus-params (vec (graph/parameter-symbols bus))
        effect-params (vec (rest (graph/parameter-symbols effect)))
        ;; The source layer normalizes already; this keeps a hand-built value
        ;; honest too, and it is idempotent.
        arguments (mapv graph/normalize-control-argument arguments)]
    (when (empty? bus-params)
      (throw (ex-info "a post-mix chain needs a signal input" {:bus bus})))
    (when-not (= (count effect-params) (count arguments))
      (throw (ex-info "a post-mix effect's arguments do not match its parameters"
                      {:effect (graph/definition-id effect)
                       :parameters effect-params
                       :arguments arguments})))
    (let [added (vec (remove (set bus-params) (filter symbol? arguments)))
          params (into bus-params added)
          ;; The effect's declaration wins for a name it introduces; the bus's
          ;; stands for a control an earlier stage contributed. The effect names
          ;; its controls by its own *parameters*, while the chain names them by
          ;; the arguments, so the two are paired positionally.
          effect-declarations (control-declarations effect)
          argument-declarations (into {}
                                      (keep (fn [[parameter argument]]
                                              (when (symbol? argument)
                                                [(graph/normalize-control-name argument)
                                                 (get effect-declarations parameter)]))
                                            (map vector effect-params arguments)))
          declarations (merge (control-declarations bus) argument-declarations)]
      (graph/derived-definition
       bus
       (apply list (graph/definition-id effect)
              (graph/definition-call bus bus-params)
              arguments)
       {:params params
        :controls (mapv (fn [control]
                          (merge (get declarations control) {:name control}))
                        (vec (rest params)))}))))

(defn lower-bus
  "Fold the instrument's post-mix chain into one mono processor definition, or
  nil when there is no chain."
  [value]
  (let [effects (instrument/effects value)]
    (when (seq effects)
      (reduce (fn [bus {:keys [effect arguments]}]
                (chain-effect bus effect arguments))
              bus-input
              effects))))

;; ---------------------------------------------------------------------------
;; Lowering to a program
;; ---------------------------------------------------------------------------

(defn- control-slots
  "Per ABI control position, where its value lives in the physical arrays.

  The JVM's unchecked adapter takes the physical arrays directly, so the host
  has to place each value; this is that mapping, resolved once."
  [layout]
  (let [entries (:entries layout)
        entry-count (clojure.core/count entries)
        float-slot (long-array entry-count)
        int-slot (long-array entry-count)]
    (doseq [entry entries]
      (let [position (long (:state-index entry))]
        (if (= :float (:storage entry))
          (do (aset float-slot position (long (:index entry)))
              (aset int-slot position -1))
          (do (aset float-slot position -1)
              (aset int-slot position (long (:index entry)))))))
    {:float-slot float-slot :int-slot int-slot}))

(defn compile-instrument
  "Lower an instrument value to a program.

  The program is `{:dsp/kind :program, :bundle …, :bus …, :routes …}`: the voice
  bundle from `synth-bundle` (artifact + runtime configuration) plus the compiled
  mono bus (or nil), at one target and precision."
  ([value] (compile-instrument value {}))
  ([value compile-options]
   (let [description (instrument/description value)
         bundle (hosts/synth-bundle description compile-options)
         target (:target (:artifact bundle))
         precision (get-in bundle [:artifact :physical-type :precision])
         bus (lower-bus value)
         bus-artifact (when bus
                        (dsp/compile bus {:target target
                                          :precision precision
                                          :entry :process}))]
     {:dsp/kind :program
      :id (:id description)
      :target target
      :bundle bundle
      :bus bus-artifact
      :effects (instrument/effects value)
      :routes (:routes value)})))

;; ---------------------------------------------------------------------------
;; The live instrument
;; ---------------------------------------------------------------------------

(defn- shared-parameter-index
  "The index of each shared parameter, by name, for binding a post-mix control."
  [runtime]
  (into {}
        (map-indexed (fn [index parameter]
                       [(name parameter) (long index)]))
        (:shared-parameters runtime)))

(defn- bus-shared-index
  "Per bus control, the index of the `:shared` parameter it reads.

  A post-mix control binds to the parameter of the same name. The chain is
  instrument-level, so only a `:shared` parameter may drive it; a control with no
  such parameter is refused. A *scalar* control and a `:shared` one bind the same
  way: the difference is only where the kernel reads the value from."
  [runtime abi-controls]
  (let [by-name (shared-parameter-index runtime)]
    (mapv (fn [control]
            (let [control-name (name (:name control))]
              (if-let [index (get by-name control-name)]
                (long index)
                (throw (ex-info "a post-mix control has no shared parameter"
                                {:control control-name
                                 :parameters (vec (sort (keys by-name)))})))))
          abi-controls)))

(defn- region-layout
  "A region's slices by name: the offset each control's slice starts at.

  A lane, a shared variable, and a per-voice lane slice all have the same shape,
  so one lookup serves them."
  [controls]
  (into {}
        (map (fn [control]
               [(name (:name control)) (long (:offset control))]))
        controls))

(declare write-bus-controls! write-bus-shared!)

(defn instrument-pool
  "Build the live instrument from a program: the voice pool plus one bus
  instance per output channel.

  `:containment` selects the voice pool's non-finite policy, as in
  `hosts/compile-synth`."
  ([program] (instrument-pool program {}))
  ([program {:keys [containment] :or {containment :reset}}]
   (let [bundle (:bundle program)
         runtime (:runtime bundle)
         ;; Fusion is a property of the compiled voice, so it travels in the
         ;; runtime the bundle already carries (F1). Without this the pool builds
         ;; a plain kernel over a fused artifact, which throws at the first
         ;; note-on; a process-shaped voice is still refused earlier, by `poly`.
         kernel (hosts/host-kernel (:artifact bundle)
                                   {:containment containment
                                    :fusion (:fusion runtime)})
         live-pool (pool/voice-pool runtime kernel)
         shared (pool/shared-parameter-values live-pool)
         channels (long (:channels runtime))
         bus (:bus program)
         abi-controls (if bus (vec (get-in bus [:abi :controls])) [])
         control-names (mapv :name abi-controls)
         shared-index (when bus (long-array (bus-shared-index runtime abi-controls)))
         control-values (double-array (count abi-controls))
         target (when bus (:target bus))
         layout (:control-layout bus)
         f64? (= :f64 (get-in bus [:physical-type :precision]))
         slots (when (and bus (= :jvm target)) (control-slots layout))
         ;; The chain is instrument-level, so its regions are: one lane region
         ;; and one shared region, written once and handed to every channel's
         ;; instance — the same shape the pool gives every voice, one level up
         ;; (M5). A bus is not fused, so a lane has one slice and there is no
         ;; slot term in the addressing.
         bus-lanes (if bus (vec (get-in bus [:abi :lanes])) [])
         bus-shared (if bus (vec (get-in bus [:abi :shared])) [])
         lane-layout (region-layout bus-lanes)
         lane-stride (long (or (get-in bus [:abi :lane-stride]) 0))
         lane-region (when (seq lane-layout)
                       (mixing/real-array f64? (* lane-stride (count lane-layout))))
         shared-layout (region-layout bus-shared)
         shared-stride (long (or (get-in bus [:abi :shared-stride]) 0))
         shared-region (when (seq shared-layout)
                         (mixing/real-array f64? (* shared-stride (count shared-layout))))
         ;; A `:shared` control reads the bus's own region, and the value comes
         ;; from the instrument parameter of the same name — exactly the rule a
         ;; scalar bus control follows, so both bind by name and both refuse a
         ;; control no parameter drives.
         shared-slices (when (seq shared-layout)
                         (mapv (fn [control index]
                                 {:name (name (:name control))
                                  :offset (long (:offset control))
                                  :parameter (long index)})
                               bus-shared
                               (bus-shared-index runtime bus-shared)))]
     ;; A bus control's initial value is its shared parameter's default.
     (when bus
       (dotimes [position (count abi-controls)]
         (aset control-values position
               (aget ^doubles shared (aget ^longs shared-index position)))))
     (let [live (cond->
                 {:program program
                  :pool live-pool
                  :channels channels
                  :buses (when bus
                           (mapv (fn [_] (dsp/create-instance bus)) (range channels)))
                  :bus-target target
                  :bus-f64? f64?
                  :bus-control-names control-names
                  :bus-control-values control-values
                  :bus-controls (volatile! (vec control-values))
                  :bus-shared shared
                  :bus-shared-index shared-index
                  :bus-lane-region lane-region
                  :bus-lane-layout lane-layout
                  :bus-lane-stride lane-stride
                  :bus-shared-region shared-region
                  :bus-shared-stride shared-stride
                  :bus-shared-slices shared-slices
                  :sample-rate 48000.0}
                  slots
                  (assoc :bus-slots slots
                         :bus-float-controls (if f64?
                                               (double-array (long (:float-count layout)))
                                               (float-array (long (:float-count layout))))
                         :bus-int-controls (int-array (long (:int-count layout)))))]
       ;; The JVM bus reads its own physical arrays, so seed them from the
       ;; shared parameters' defaults before the first render. Its regions are
       ;; host-owned, so they are seeded here too.
       (when (:bus-slots live)
         (write-bus-controls! live))
       (write-bus-shared! live)
       live))))

(defn pool-of
  "The voice pool, for a host that publishes its own control values."
  [live]
  (:pool live))

(defn- write-bus-controls!
  "Copy the values array into the JVM bus's physical control arrays.

  Resolved once at construction (`:bus-slots`), so this is not re-entered on
  the audio path — only when a control is set."
  [live]
  (let [^doubles values (:bus-control-values live)
        {:keys [float-slot int-slot]} (:bus-slots live)
        positions (clojure.core/count (:bus-control-names live))
        f64? (:bus-f64? live)]
    (dotimes [position positions]
      (let [slot (aget ^longs float-slot position)]
        (if (>= slot 0)
          (if f64?
            (aset-double ^doubles (:bus-float-controls live) (int slot)
                         (aget values position))
            (aset-float ^floats (:bus-float-controls live) (int slot)
                        (float (aget values position))))
          (aset-int ^ints (:bus-int-controls live)
                    (int (aget ^longs int-slot position))
                    (long (aget values position)))))))
  nil)

(defn- write-bus-shared!
  "Broadcast every shared post-mix control's parameter across its slice.

  A `:shared` control is read one float per *frame*, so its parameter's current
  value has to sit in every frame of the slice — the same broadcast a shared
  region gets on every target (M5). Resolved once at construction, so this is
  off the audio path: it runs when the instrument is built and when a parameter
  is set."
  [live]
  (when-let [region (:bus-shared-region live)]
    (let [^doubles shared (:bus-shared live)
          stride (long (:bus-shared-stride live))
          f64? (:bus-f64? live)]
      (doseq [{:keys [offset parameter]} (:bus-shared-slices live)]
        (let [value (aget shared (long parameter))
              offset (long offset)]
          (if f64?
            (let [^doubles target region]
              (dotimes [frame stride]
                (aset-double target (int (+ offset frame)) value)))
            (let [^floats target region]
              (dotimes [frame stride]
                (aset-float target (int (+ offset frame)) (float value)))))))))
  nil)

(defn- refresh-bus-values!
  "Re-read every bus control from its shared parameter, and refresh the vector
  the portable (interpreter) bus runner reads."
  [live]
  (when-let [^longs indices (:bus-shared-index live)]
    (let [^doubles shared (:bus-shared live)
          ^doubles values (:bus-control-values live)]
      (dotimes [position (alength indices)]
        (aset values position (aget shared (aget indices position))))
      (vreset! (:bus-controls live) (vec values))))
  (write-bus-shared! live)
  nil)

(defn set-bus-lane!
  "Set a per-frame curve for one of the post-mix chain's lanes.

  `lane` names a control the compiled chain declares a lane for (a `:lane` or
  `:has-lane` control on a stage), and `values` is one float per frame, however
  the host spells it. The region belongs to the *instrument*, not to a channel:
  the chain is one processor run once per channel over the same summed signal,
  so every channel's instance reads the same curve — the pool's shared lane, one
  level up (M4c/M5).

  Takes effect on the next `render!`."
  [live lane values]
  (let [region (:bus-lane-region live)]
    (when-not region
      (throw (ex-info "the post-mix chain declares no lanes" {:lanes []})))
    (let [layout (:bus-lane-layout live)
          key (if (string? lane) lane (clojure.core/name lane))
          offset (get layout key)]
      (when-not offset
        (throw (ex-info "the post-mix chain declares no such lane"
                        {:lane lane :lanes (vec (sort (keys layout)))})))
      (let [stride (long (:bus-lane-stride live))
            length (min (mixing/value-length values) stride)
            f64? (:bus-f64? live)
            offset (long offset)]
        (if f64?
          (let [^doubles target region]
            (dotimes [index length]
              (aset-double target (int (+ offset index))
                           (double (mixing/value-at values index)))))
          (let [^floats target region]
            (dotimes [index length]
              (aset-float target (int (+ offset index))
                          (float (mixing/value-at values index))))))))
    nil))

(defn set-parameter!
  "Set a `:shared` parameter. Every sounding voice and the post-mix bus follow
  it; takes effect on the next `render!`."
  [live name value]
  (pool/set-parameter! (:pool live) name value)
  (refresh-bus-values! live)
  (when (:bus-slots live)
    (write-bus-controls! live))
  nil)

(defn set-voice-parameter!
  "Set a `:per-voice` parameter for voice `index`. Takes effect on that voice's
  next render."
  [live index name value]
  (pool/set-voice-parameter! (:pool live) index name value))

(defn parameter-value
  "The current value of a `:shared` parameter."
  [live name]
  (pool/parameter-value (:pool live) name))

(defn set-bus-control!
  "Set a global post-mix control by name. It is a `:shared` parameter, so the
  voices read the same value. Takes effect on the next `render!`."
  [live control value]
  (let [names (:bus-control-names live)
        index (first (keep-indexed (fn [position name]
                                     (when (= (clojure.core/name control)
                                              (clojure.core/name name))
                                       position))
                                   names))]
    (when-not index
      (throw (ex-info "the post-mix chain has no such control"
                      {:control control :controls names})))
    (set-parameter! live (keyword (clojure.core/name control)) value)))

(defn- run-bus!
  "Run one channel's bus instance over `buffer`, in place.

  The JVM path uses the unchecked primitive adapter, exactly as the voice pool
  does; the interpreter (and, later, a browser host) uses the context adapter,
  where the extra allocation is acceptable because it is not the audio path.

  Both are handed the chain's two regions: the lane region a per-frame curve is
  written into, and the shared region a `:shared` control is read from. They
  belong to the instrument, so every channel's instance sees the same ones."
  [live bus buffer frames sample-rate]
  (case (:bus-target live)
    :jvm (dsp/process-unchecked! bus buffer buffer frames (double sample-rate)
                                 (:bus-float-controls live)
                                 (:bus-int-controls live)
                                 (:bus-lane-region live)
                                 (:bus-shared-region live))
    (dsp/process! bus {:input buffer :output buffer :frames frames
                       :sample-rate sample-rate
                       :controls @(:bus-controls live)
                       :lanes (:bus-lane-region live)
                       :shared (:bus-shared-region live)})))

(defn note-on!
  "Start `note` at `velocity`, returning the voice index it took."
  ([live note] (note-on! live note 1.0))
  ([live note velocity] (pool/note-on! (:pool live) note velocity)))

(defn note-off!
  "Release every voice sounding `note`."
  [live note]
  (pool/note-off! (:pool live) note))

(defn all-notes-off!
  "Release every sounding voice."
  [live]
  (pool/all-notes-off! (:pool live)))

(defn render!
  "Render `frames` samples of the whole instrument into `output`.

  The pool mixes the voices into `output`; each output channel is then passed
  through its own bus instance, in place. A planar block for a stereo
  instrument, a flat array for mono."
  ([live output frames]
   (render! live output frames (:sample-rate live)))
  ([live output frames sample-rate]
   (pool/render! (:pool live) output frames sample-rate)
   (when-let [buses (:buses live)]
     (let [channels (long (:channels live))
           mono? (= 1 channels)]
       (dotimes [channel channels]
         (let [buffer (if mono?
                        output
                        (aget ^objects output channel))]
           (run-bus! live (nth buses channel) buffer frames sample-rate)))))
   nil))
