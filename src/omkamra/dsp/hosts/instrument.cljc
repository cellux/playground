(ns omkamra.dsp.hosts.instrument
  "Lower an `omkamra.dsp.instrument` value to a **program**, and run it.

  The Clojure instrument states voices, polyphony, and a post-mix chain; this
  namespace decides what that becomes. The first lowering folds the effect chain
  into one mono bus definition, compiles it to one kernel, and reuses the P10
  voice machinery (`synth-bundle`) for the pool. A *program* is data plus
  artifacts, so the same value can be lowered differently later — an unrolled
  poly kernel, a stereo bus, a fused instrument kernel — without touching the
  source layer.

  **The two halves have different worlds.** The *lowering* is JVM-side: it
  captures `dsp/defn` definitions and compiles the voice through `synth-bundle`.
  The *live host* is portable — it holds only primitive arrays and the pool API,
  and drives the bus through a **bus kernel** — so it runs wherever a voice
  `Kernel` and a bus kernel are, on the JVM or in the browser."
  (:require #?(:clj [omkamra.dsp :as dsp])
            #?(:clj [omkamra.dsp.graph :as graph])
            #?(:clj [omkamra.dsp.hosts :as hosts])
            #?(:clj [omkamra.dsp.instrument :as instrument])
            [omkamra.dsp.synth.pool :as pool]))

#?(:clj
   (do
     ;; ---------------------------------------------------------------------
     ;; The post-mix chain, lowered (JVM: it captures a `dsp/defn` definition)
     ;; ---------------------------------------------------------------------

     (dsp/defn bus-input
       "The identity mono processor a post-mix chain starts from: the summed
       voices, one output channel's worth."
       [sample]
       {:process {:controls []}}
       sample)

     (defn- control-declarations
       "A processor's control declarations, keyed by name.

       A `:process` says what *kind* of control each of its parameters is:
       whether it is a per-frame lane (`:lane`, `:has-lane`), a shared variable,
       a `:smooth` / `:slew` value, or carries a `:default`. A derived interface
       has to carry those across, or a post-mix control silently loses the kind
       it was declared with."
       [processor]
       (into {}
             (map (fn [control] [(:name control) control]))
             (get-in processor [:process :controls])))

     (defn- chain-effect
       "Wrap processor `bus` in processor `effect`, extending the interface with
       the effect's controls.

       This is `comb/through` for a *processor*: `bus`'s first parameter is its
       signal input, kept at the front, and the effect's controls (the symbols in
       `arguments`) are appended. The body is `(effect (bus …) arguments…)`.

       Each control keeps the declaration it was written with, whether it came
       from the accumulated bus or from the effect just wrapped. The declaration
       has to reach the *entry* process: the derived body calls the effect, and
       the effect evaluates its own controls, but the region a lane or a shared
       control is read from belongs to the entry, so this is where the compiler
       looks for it."
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
               ;; The effect's declaration wins for a name it introduces; the
               ;; bus's stands for a control an earlier stage contributed. The
               ;; effect names its controls by its own *parameters*, while the
               ;; chain names them by the arguments, so the two are paired
               ;; positionally.
               effect-declarations (control-declarations effect)
               argument-declarations
               (into {}
                     (keep (fn [[parameter argument]]
                             (when (symbol? argument)
                               [(graph/normalize-control-name argument)
                                (get effect-declarations parameter)]))
                           (map vector effect-params arguments)))
               declarations (merge (control-declarations bus)
                                   argument-declarations)]
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
       "Fold the instrument's post-mix chain into one mono processor definition,
       or nil when there is no chain."
       [value]
       (let [effects (instrument/effects value)]
         (when (seq effects)
           (reduce (fn [bus {:keys [effect arguments]}]
                     (chain-effect bus effect arguments))
                   bus-input
                   effects))))

     ;; -------------------------------------------------------------------
     ;; Lowering to a program
     ;; -------------------------------------------------------------------

     (defn compile-instrument
       "Lower an instrument value to a program.

       The program is `{:dsp/kind :program, :bundle …, :bus …, :routes …}`: the
       voice bundle from `synth-bundle` (artifact + runtime configuration) plus
       the compiled mono bus (or nil), at one target and precision."
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
           :routes (:routes value)})))))

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

(defn- publish-bus-shared!
  "Broadcast every `:shared` post-mix control's parameter across its slice.

  A `:shared` control is read one float per frame, so its parameter's current
  value sits in every frame of the slice — the same broadcast a voice's shared
  region gets (M5) — and a scalar control binds the same way. The region belongs
  to the instrument, so writing it once reaches every channel's instance. Runs
  when the instrument is built and when a parameter is set, off the audio path."
  [live]
  (when-let [bus-kernel (:bus-kernel live)]
    (when-let [^longs indices (:bus-shared-index live)]
      (let [^doubles shared (:bus-shared live)
            names (:bus-shared-names live)]
        (dotimes [position (alength indices)]
          ((:write-shared! bus-kernel)
           (name (nth names position))
           (aget shared (aget indices position)))))))
  nil)

(defn- refresh-bus-values!
  "Re-read every positional bus control from its shared parameter.

  A post-mix control is bound to the `:shared` parameter of the same name, and
  the bus kernel maps the vector this fills into whatever its target needs on
  the next render. Runs when a parameter is set, off the audio path."
  [live]
  (when-let [^longs indices (:bus-control-index live)]
    (let [^doubles shared (:bus-shared live)
          ^doubles values (:bus-control-values live)]
      (dotimes [position (alength indices)]
        (aset values position (aget shared (aget indices position))))))
  (publish-bus-shared! live)
  nil)

(defn- default-voice-kernel
  "The voice `Kernel` for a program's artifact in this world.

  The JVM builds it from the compiled artifact and the runtime's fusion policy.
  A browser host has no compiler-side assembler here, so it passes `:kernel`
  built from the served bundle."
  [bundle options]
  #?(:clj
     (hosts/host-kernel (:artifact bundle)
                        {:containment (get options :containment :reset)
                         :fusion (:fusion (:runtime bundle))})
     :cljs
     (throw (ex-info "an instrument pool needs an explicit :kernel in this world"
                     {:description (:id (:description bundle))
                      :options (vec (sort (keys options)))}))))

(defn- default-bus-kernel
  "The bus kernel for a program's compiled bus in this world.

  The JVM builds it from the bus artifact. A browser host passes `:bus-kernel`
  built from the served bundle's bus artifact."
  [bus]
  #?(:clj (hosts/bus-kernel bus)
     :cljs (throw (ex-info (str "an instrument pool needs an explicit"
                                " :bus-kernel in this world")
                           {:target (:target bus) :entry (:entry bus)}))))

(defn instrument-pool
  "Build the live instrument from a program: the voice pool plus one bus
  instance per output channel.

  `:kernel` and `:bus-kernel` are the host's voice `Kernel` and bus kernel. The
  JVM builds them from the program's artifacts when they are not supplied; a
  browser host passes the ones it built from the served bundle. `:containment`
  selects the voice pool's non-finite policy, as in `hosts/compile-synth`."
  ([program] (instrument-pool program {}))
  ([program {:keys [kernel bus-kernel] :as options}]
   (let [bundle (:bundle program)
         runtime (:runtime bundle)
         ;; Fusion is a property of the compiled voice, so it travels in the
         ;; runtime the bundle already carries (F1). Without this the pool builds
         ;; a plain kernel over a fused artifact, which throws at the first
         ;; note-on; a process-shaped voice is still refused earlier, by `poly`.
         voice-kernel (or kernel (default-voice-kernel bundle options))
         live-pool (pool/voice-pool runtime voice-kernel)
         shared (pool/shared-parameter-values live-pool)
         channels (long (:channels runtime))
         bus (:bus program)
         bus-kernel (or bus-kernel (when bus (default-bus-kernel bus)))
         abi-controls (if bus (vec (get-in bus [:abi :controls])) [])
         control-names (mapv :name abi-controls)
         ;; A block-rate control is bound to the `:shared` parameter of the same
         ;; name; the chain is instrument-level, so a control no parameter drives
         ;; is refused here.
         control-index (when bus
                         (long-array (bus-shared-index runtime abi-controls)))
         control-values (double-array (count abi-controls))
         ;; The chain's own regions are its lanes and its `:shared` controls —
         ;; neither is in the positional list — and both bind by name too. The
         ;; region belongs to the instrument, so the bus kernel owns it and every
         ;; channel's instance reads the same one (M4c/M5).
         bus-lanes (if bus (vec (get-in bus [:abi :lanes])) [])
         bus-shared (if bus (vec (get-in bus [:abi :shared])) [])
         shared-index (when (seq bus-shared)
                        (long-array (bus-shared-index runtime bus-shared)))
         buses (when bus
                 (mapv (fn [_] ((:create-bus bus-kernel))) (range channels)))]
     ;; A positional bus control's initial value is its shared parameter's
     ;; default.
     (when bus
       (dotimes [position (count abi-controls)]
         (aset control-values position
               (aget ^doubles shared (aget ^longs control-index position)))))
     (let [live {:program program
                 :pool live-pool
                 :channels channels
                 :buses buses
                 :bus-kernel bus-kernel
                 :bus-lane-names (mapv :name bus-lanes)
                 :bus-control-names control-names
                 :bus-control-values control-values
                 :bus-control-index control-index
                 :bus-shared shared
                 :bus-shared-names (mapv :name bus-shared)
                 :bus-shared-index shared-index
                 :sample-rate 48000.0}]
       ;; The regions are seeded from the shared parameters' defaults before the
       ;; first render, exactly as the positional controls are.
       (refresh-bus-values! live)
       live))))

(defn pool-of
  "The voice pool, for a host that publishes its own control values."
  [live]
  (:pool live))

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
  (when (empty? (:bus-lane-names live))
    (throw (ex-info "the post-mix chain declares no lanes" {:lanes []})))
  ((:write-lane! (:bus-kernel live)) lane values)
  nil)

(defn set-parameter!
  "Set a `:shared` parameter. Every sounding voice and the post-mix bus follow
  it; takes effect on the next `render!`."
  [live name value]
  (pool/set-parameter! (:pool live) name value)
  (refresh-bus-values! live)
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

  The bus kernel owns the target's representation — the JVM's physical control
  arrays, a Wasm instance's memory, the interpreter's state — so this is one call
  in every host, handed the chain's control vector and the regions the kernel
  keeps."
  [live bus buffer frames sample-rate]
  ((:run-bus! (:bus-kernel live)) bus buffer buffer frames sample-rate
                                  (:bus-control-values live)))

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
