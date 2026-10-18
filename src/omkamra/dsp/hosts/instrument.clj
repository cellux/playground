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

(defn- chain-effect
  "Wrap processor `bus` in processor `effect`, extending the interface with the
  effect's controls.

  This is `comb/through` for a *processor*: `bus`'s first parameter is its signal
  input, kept at the front, and the effect's controls (the symbols in
  `arguments`) are appended. The body is `(effect (bus …) arguments…)`."
  [bus effect arguments]
  (let [bus-params (vec (graph/parameter-symbols bus))
        effect-params (vec (rest (graph/parameter-symbols effect)))
        arguments (vec arguments)]
    (when (empty? bus-params)
      (throw (ex-info "a post-mix chain needs a signal input" {:bus bus})))
    (when-not (= (count effect-params) (count arguments))
      (throw (ex-info "a post-mix effect's arguments do not match its parameters"
                      {:effect (graph/definition-id effect)
                       :parameters effect-params
                       :arguments arguments})))
    (let [added (vec (remove (set bus-params) (filter symbol? arguments)))
          params (into bus-params added)]
      (graph/derived-definition
       bus
       (apply list (graph/definition-id effect)
              (graph/definition-call bus bus-params)
              arguments)
       {:params params
        :controls (mapv (fn [control] {:name control}) (vec (rest params)))}))))

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

(defn instrument-pool
  "Build the live instrument from a program: the voice pool plus one bus
  instance per output channel.

  `:containment` selects the voice pool's non-finite policy, as in
  `hosts/compile-synth`."
  ([program] (instrument-pool program {}))
  ([program {:keys [containment] :or {containment :reset}}]
   (let [bundle (:bundle program)
         kernel (hosts/host-kernel (:artifact bundle) {:containment containment})
         live-pool (pool/voice-pool (:runtime bundle) kernel)
         channels (long (get-in bundle [:runtime :channels]))
         bus (:bus program)
         abi-controls (if bus (vec (get-in bus [:abi :controls])) [])
         control-names (mapv :name abi-controls)
         control-values (double-array
                         (map (fn [control] (double (or (:default control) 0.0)))
                              abi-controls))
         target (when bus (:target bus))
         layout (:control-layout bus)
         f64? (= :f64 (get-in bus [:physical-type :precision]))
         slots (when (and bus (= :jvm target)) (control-slots layout))]
     (cond->
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
       :sample-rate 48000.0}
       slots
       (assoc :bus-slots slots
              :bus-float-controls (if f64?
                                    (double-array (long (:float-count layout)))
                                    (float-array (long (:float-count layout))))
              :bus-int-controls (int-array (long (:int-count layout))))))))

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

(defn set-bus-control!
  "Set a global post-mix control by name. Takes effect on the next `render!`."
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
    (aset ^doubles (:bus-control-values live) (long index) (double value))
    (vreset! (:bus-controls live) (vec (:bus-control-values live)))
    (when (:bus-slots live)
      (write-bus-controls! live))
    nil))

(defn- run-bus!
  "Run one channel's bus instance over `buffer`, in place.

  The JVM path uses the unchecked primitive adapter, exactly as the voice pool
  does; the interpreter (and, later, a browser host) uses the context adapter,
  where the extra allocation is acceptable because it is not the audio path."
  [live bus buffer frames sample-rate]
  (case (:bus-target live)
    :jvm (dsp/process-unchecked! bus buffer buffer frames (double sample-rate)
                                 (:bus-float-controls live)
                                 (:bus-int-controls live))
    (dsp/process! bus {:input buffer :output buffer :frames frames
                       :sample-rate sample-rate
                       :controls @(:bus-controls live)})))

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
