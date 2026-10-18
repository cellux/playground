(ns omkamra.dsp.examples.reference-synth
  "The P11 reference synthesizer: the end-to-end acceptance instrument.

  **What this is.** The v1 acceptance fixture from `PLAN.md`: two band-limited
  oscillators plus noise, hard sync, PWM and unison; an amp envelope, a filter
  envelope, and LFOs; a resonant filter; a host-side modulation matrix; stereo
  output; and (from P11b) a post-mix effect stage. It is assembled from library
  blocks and combinators (D38) rather than written as one large voice, so its
  structure is inspectable and each block is already conformance-tested on its
  own.

  **The modulation matrix is deliberately an example, not core** (D34). It stays
  in this namespace until the end of P11, when it is studied for patterns worth
  promoting. It is a host-side policy — `base + depth x source` — on top of the
  pool's unopinionated delivery channel `set-voice-controls!`; the DSP voice
  knows nothing about it.

  **Signal flow.**

      oscillator-bank (mono, unison'd)
        saw + pulse (PWM) + hard sync            <- band-limited except sync
        LFO: vibrato + PWM depth
      reference-source (mono)
        + white noise
        x amp ADSR
        -> resonant SVF (filter ADSR opens the cutoff)
        -> soft distortion
      reference-voice (stereo)
        equal-power pan -> two output channels

  **Why the stereo wrapper is separate.** `unison` sums its copies, so it needs
  a scalar-returning voice; a voice that writes `:output` channels returns
  nothing to sum. So the source is unison'd in mono and the stereo wrapper pans
  the result. The effect/pan module returns a record, which the wrapper projects
  into the two channels explicitly (D40).

  **The host is portable in shape.** The instrument wraps a live pool and keeps
  only primitive arrays, so the matrix and render loop touch nothing but the
  pool API; the JVM constructor is the only part that reaches for the compiler.
  This namespace is `.clj` because P11a builds and checks the instrument on the
  JVM; P11c extracts the portable half into `.cljc` for the browser hosts."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.hosts.instrument :as inst-host]
            [omkamra.dsp.instrument :as inst]
            [omkamra.dsp.library.delay]
            [omkamra.dsp.library.effect]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.filter]
            [omkamra.dsp.library.noise]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.library.pitch]
            [omkamra.dsp.synth.pool :as pool]))

;; ---------------------------------------------------------------------------
;; The oscillator bank
;; ---------------------------------------------------------------------------

(dsp/defn oscillator-bank
  "Mono, unison-able oscillator bank: a band-limited saw and a band-limited
  pulse (PWM), plus a hard-synced slave saw, blended by `osc-mix` and
  `sync-mix`.

  One LFO drives both vibrato (in cents, added to `detune`) and the pulse width,
  which is what makes the PWM move. `unison` detunes the whole bank, so each copy
  has its own phase, LFO, and PWM offset."
  [note bend detune osc-mix pulse-width pwm-depth sync-mix sync-ratio
   lfo-rate lfo-shape vibrato-depth]
  {:process {:input false
             :controls [{:name note} {:name bend} {:name detune}
                        {:name osc-mix} {:name pulse-width} {:name pwm-depth}
                        {:name sync-mix} {:name sync-ratio}
                        {:name lfo-rate} {:name lfo-shape :type :int}
                        {:name vibrato-depth}]}}
  (let [vib (omkamra.dsp.library.oscillator/lfo lfo-rate lfo-shape)
        bipolar (- (* 2.0 vib) 1.0)
        frequency (omkamra.dsp.library.pitch/pitch->frequency
                   note bend (+ detune (* vibrato-depth bipolar)) 440.0 69.0)
        width (clamp (+ pulse-width (* pwm-depth bipolar)) 0.05 0.95)
        saw (omkamra.dsp.library.oscillator/saw-oscillator frequency)
        pulse (omkamra.dsp.library.oscillator/pulse-oscillator frequency width)
        synced (omkamra.dsp.library.oscillator/sync-oscillator frequency sync-ratio)]
    (+ (+ (* (- 1.0 osc-mix) saw)
          (* osc-mix pulse))
       (* sync-mix synced))))

;; Three detuned copies, summed. The `detune` control is what `unison` offsets;
;; the derived value carries a fresh name and the original in
;; `:dsp/dependencies`, so it links like any other definition (D37).
(dsp/def unison-oscillator-bank
  (-> oscillator-bank
      (comb/unison {:count 3 :detune 14.0 :gain 0.5})))

;; ---------------------------------------------------------------------------
;; The mono voice
;; ---------------------------------------------------------------------------

(dsp/defn reference-source
  "The mono reference voice: the unison'd oscillator bank plus white noise,
  shaped by an amp envelope and a resonant state-variable lowpass whose cutoff
  is opened by a second envelope, then soft-clipped.

  The filter cutoff is clamped below a sixth of the sample rate, which is the
  Chamberlin SVF's documented stability bound, so a modulated cutoff cannot
  drive it unstable."
  [gate note bend detune
   osc-mix pulse-width pwm-depth sync-mix sync-ratio
   noise-level
   cutoff resonance filter-amount
   attack decay sustain release
   filter-attack filter-decay filter-sustain filter-release
   lfo-rate lfo-shape vibrato-depth drive]
  {:process {:input false
             :controls [{:name gate} {:name note} {:name bend} {:name detune}
                        {:name osc-mix} {:name pulse-width} {:name pwm-depth}
                        {:name sync-mix} {:name sync-ratio}
                        {:name noise-level}
                        {:name cutoff} {:name resonance} {:name filter-amount}
                        {:name attack} {:name decay} {:name sustain} {:name release}
                        {:name filter-attack} {:name filter-decay}
                        {:name filter-sustain} {:name filter-release}
                        {:name lfo-rate} {:name lfo-shape :type :int}
                        {:name vibrato-depth} {:name drive}]}}
  (let [osc (unison-oscillator-bank
             note bend detune osc-mix pulse-width pwm-depth sync-mix sync-ratio
             lfo-rate lfo-shape vibrato-depth)
        noise (omkamra.dsp.library.noise/white-noise 12345)
        amp (omkamra.dsp.library.envelope/exponential-adsr
             gate attack decay sustain release)
        filter-env (omkamra.dsp.library.envelope/exponential-adsr
                    gate filter-attack filter-decay filter-sustain filter-release)
        signal (* amp (+ osc (* noise-level noise)))
        modulated-cutoff (clamp (* cutoff (+ 1.0 (* filter-amount filter-env)))
                                20.0 8000.0)
        damping (max resonance 0.05)
        filtered (omkamra.dsp.library.filter/state-variable-filter
                  signal modulated-cutoff damping)]
    (omkamra.dsp.library.effect/distortion (:low filtered) drive)))

;; ---------------------------------------------------------------------------
;; The stereo voice
;; ---------------------------------------------------------------------------

(dsp/defn reference-voice
  "The stereo reference voice: the mono source panned by an equal-power `pan`
  control into two output channels.

  `pan` is a standard per-voice source the matrix may modulate (`set-pan!` is
  the pool's counterpart), so a voice can be placed either by the host or by its
  own DSP."
  [gate note bend detune pan
   osc-mix pulse-width pwm-depth sync-mix sync-ratio
   noise-level
   cutoff resonance filter-amount
   attack decay sustain release
   filter-attack filter-decay filter-sustain filter-release
   lfo-rate lfo-shape vibrato-depth drive]
  {:process {:input false
             ;; The continuous controls the matrix modulates are smoothed per
             ;; frame (D27): the host sets them once per block, and a bare step
             ;; would click (zipper noise). Each starts at its declared default,
             ;; so a fresh voice opens at its base settings rather than at zero.
             ;; `pan` is left bare: the host sets it per voice and the pan law is
             ;; already smooth across the field.
             :controls [{:name gate} {:name note} {:name bend} {:name detune}
                        {:name pan}
                        {:name osc-mix} {:name pulse-width :smooth 0.02 :default 0.5}
                        {:name pwm-depth}
                        {:name sync-mix} {:name sync-ratio}
                        {:name noise-level}
                        {:name cutoff :smooth 0.01 :default 800.0}
                        {:name resonance :smooth 0.02 :default 0.6}
                        {:name filter-amount}
                        {:name attack} {:name decay} {:name sustain} {:name release}
                        {:name filter-attack} {:name filter-decay}
                        {:name filter-sustain} {:name filter-release}
                        {:name lfo-rate} {:name lfo-shape :type :int}
                        {:name vibrato-depth} {:name drive}]}}
  (let [pair (omkamra.dsp.library.effect/pan
              (reference-source
               gate note bend detune
               osc-mix pulse-width pwm-depth sync-mix sync-ratio
               noise-level
               cutoff resonance filter-amount
               attack decay sustain release
               filter-attack filter-decay filter-sustain filter-release
               lfo-rate lfo-shape vibrato-depth drive)
              pan)]
    (buffer-store :output 0 0 (:left pair))
    (buffer-store :output 1 0 (:right pair))))

;; ---------------------------------------------------------------------------
;; The description
;; ---------------------------------------------------------------------------

(def description
  "The reference instrument as `omkamra.dsp.synth` data.

  The standard sources (`gate`, `note`, `bend`, `detune`, `pan`) come from the
  pool; every other control has a base value here that the modulation matrix
  offsets. `lfo-shape` is the LFO's integer shape selector (triangle by
  default); `sync-mix` starts off and is the matrix's job to open."
  {:id :reference-synth
   :capacity 8
   :max-block-frames 512
   :bend-range 200.0
   :voice
   {:definition #'reference-voice
    :controls
    {:gate {:source :gate}
     :note {:source :note}
     :bend {:source :bend}
     :detune {:source :detune}
     :pan {:source :pan}
     :osc-mix {:constant 0.5}
     :pulse-width {:constant 0.5}
     :pwm-depth {:constant 0.15}
     :sync-mix {:constant 0.0}
     :sync-ratio {:constant 2.0}
     :noise-level {:constant 0.05}
     :cutoff {:constant 800.0}
     :resonance {:constant 0.6}
     :filter-amount {:constant 1.5}
     :attack {:constant 0.005}
     :decay {:constant 0.15}
     :sustain {:constant 0.6}
     :release {:constant 0.3}
     :filter-attack {:constant 0.005}
     :filter-decay {:constant 0.2}
     :filter-sustain {:constant 0.3}
     :filter-release {:constant 0.3}
     :lfo-rate {:constant 5.0}
     :lfo-shape {:constant 1}
     :vibrato-depth {:constant 5.0}
     :drive {:constant 1.0}}}})

;; ---------------------------------------------------------------------------
;; The modulation matrix
;; ---------------------------------------------------------------------------

(def modulation-matrix
  "The reference instrument's modulation matrix, as pure data.

  Rows are *sources*; each row routes that source to one or more
  `{:target control :depth amount}` entries, applied as
  `value = base + depth x source-value`. Sources are the per-voice
  `:velocity` and `:note` (in octaves from middle C, so a depth is a musical
  distance), or a global MIDI controller `{:controller n}` in 0..1.

  Pitch (`bend`, `detune`) is deliberately absent: it is a standard source the
  voice consumes directly (D35), not a route onto another control.

  This is host-side policy, deliberately not core (D34). It is studied at the
  end of P11 to see whether any of it should be promoted, and in what form."
  [{:source :velocity
    :routes [{:target :cutoff :depth 1800.0}
             {:target :resonance :depth -0.15}
             {:target :osc-mix :depth 0.25}]}
   {:source :note
    :routes [{:target :cutoff :depth 1200.0}
             {:target :pulse-width :depth 0.15}]}
   {:source {:controller 1}
    :routes [{:target :cutoff :depth 3000.0}
             {:target :resonance :depth -0.25}
             {:target :pulse-width :depth 0.3}
             {:target :pan :depth 0.5}]}
   {:source {:controller 74}
    :routes [{:target :osc-mix :depth 0.5}
             {:target :sync-mix :depth 0.4}
             {:target :noise-level :depth 0.2}]}])

(def ^:private controller-count 128)

(defn- compile-matrix
  "Flatten the matrix into routes with a resolved control `:position`, once, at
  construction. A target the voice does not declare is refused here rather than
  silently dropping modulation."
  [positions]
  (into []
        (mapcat (fn [{:keys [source routes]}]
                  (mapv (fn [{:keys [target depth]}]
                          (let [target (name target)]
                            (when-not (contains? positions target)
                              (throw (ex-info "the modulation matrix names an unknown target"
                                              {:target target
                                               :targets (vec (sort (keys positions)))})))
                            {:source source
                             :position (long (get positions target))
                             :depth (double depth)}))
                        routes)))
        modulation-matrix))

;; ---------------------------------------------------------------------------
;; The instrument
;; ---------------------------------------------------------------------------

(def reference-instrument
  "The instrument as an `omkamra.dsp.instrument` value: the voice description,
  the modulation matrix, and a post-mix chain of three global effects.

  The chain is Clojure-level. The effects are library definitions; the lowering
  (`omkamra.dsp.hosts.instrument`) decides that they become one mono bus kernel
  per output channel, run once over the summed voices. Their controls are named
  by the arguments, so they are global — not per voice — and the host sets them.
  The delay is last, so its feedback tail continues after the voices stop."
  (-> (inst/instrument description)
      (inst/routes modulation-matrix)
      (inst/with-effect omkamra.dsp.library.delay/chorus
        [:chorus-rate :chorus-depth :chorus-mix])
      (inst/with-effect omkamra.dsp.library.effect/distortion
        [:drive])
      (inst/with-effect omkamra.dsp.library.delay/feedback-delay
        [:delay-time :delay-feedback :delay-mix])))

(defn build-instrument
  "Build a live reference instrument from an `omkamra.dsp.instrument` value:
  lower it to a program, create the live pool and bus, and add the host-side
  modulation storage (the reference matrix).

  Exposed so a test can lower a *variant* of the instrument — for example the
  same voice with no post-mix chain — and compare. `compile-options` selects the
  target, precision, and containment policy."
  ([value]
   (build-instrument value {}))
  ([value compile-options]
   (let [program (inst-host/compile-instrument value compile-options)
         live (inst-host/instrument-pool program)
         live-pool (inst-host/pool-of live)
         plan (vec (:control-plan (get-in program [:bundle :runtime])))
         positions (into {} (map-indexed (fn [index entry]
                                           [(name (:control entry)) index])
                                         plan))
         capacity (long (pool/pool-capacity live-pool))]
     (when (seq (:bus-control-names live))
       (doseq [[control value] [['chorus-rate 0.6] ['chorus-depth 12.0]
                                ['chorus-mix 0.35] ['drive 1.5]
                                ['delay-time 12000.0] ['delay-feedback 0.45]
                                ['delay-mix 0.35]]]
         (inst-host/set-bus-control! live control value)))
     (merge live
            {:plan plan
             :positions positions
             :routes (object-array (compile-matrix positions))
             :capacity capacity
             ;; One control scratch, reused for every voice and block: the pool
             ;; copies. The host-side sources the routes read: gate, note, bend,
             ;; detune, and pan come from the pool's plan; velocity and the
             ;; controller bank live here.
             :values (double-array (pool/control-count live-pool))
             :notes (long-array capacity)
             :velocities (double-array capacity)
             :controllers (double-array controller-count)}))))

(defn reference-synth
  "Build the reference instrument (see `reference-instrument`)."
  ([]
   (build-instrument reference-instrument {}))
  ([compile-options]
   (build-instrument reference-instrument compile-options)))

;; ---------------------------------------------------------------------------
;; Playing it
;; ---------------------------------------------------------------------------

(defn note-on!
  "Start `note` at `velocity`, returning the voice index it took. The host keeps
  the note and velocity for that index, because the matrix reads them as
  sources and the pool may have stolen the voice."
  ([instrument note] (note-on! instrument note 1.0))
  ([instrument note velocity]
   (let [index (long (pool/note-on! (:pool instrument) note velocity))]
     (aset ^longs (:notes instrument) index (long note))
     (aset ^doubles (:velocities instrument) index (double velocity))
     index)))

(defn note-off!
  "Release every voice sounding `note`. Returns how many the pool released."
  [instrument note]
  (pool/note-off! (:pool instrument) note))

(defn all-notes-off!
  "Release every sounding voice."
  [instrument]
  (pool/all-notes-off! (:pool instrument)))

(defn set-controller!
  "Set controller `controller` (0..127) to `value` (0..1). Global: it applies to
  every voice on the next block."
  [instrument controller value]
  (aset ^doubles (:controllers instrument) (long controller) (double value))
  nil)

(defn set-pitch-bend!
  "Set channel `channel`'s pitch bend, in -1..1; the pool scales it by the
  description's `:bend-range` and delivers it as `bend` in cents."
  [instrument channel value]
  (pool/set-pitch-bend! (:pool instrument) channel value))

(defn set-pan!
  "Set voice `index`'s pan position in -1..1 (`:pan` is a standard source)."
  [instrument index value]
  (pool/set-pan! (:pool instrument) index value))

;; ---------------------------------------------------------------------------
;; Routing and rendering
;; ---------------------------------------------------------------------------

(defn- modulate!
  "Apply every matrix route to `values` for voice `index`, in route order.

  The instrument is passed whole rather than decomposed so the fn stays under
  Clojure's four-argument limit for primitive-hinted parameters; the array
  lookups it does are per voice, not per sample."
  [instrument ^doubles values ^long index]
  (let [^objects routes (:routes instrument)
        ^longs notes (:notes instrument)
        ^doubles velocities (:velocities instrument)
        ^doubles controllers (:controllers instrument)]
    (dotimes [route-index (alength routes)]
      (let [route (aget routes route-index)
            source (:source route)
            position (long (:position route))
            source-value (if (keyword? source)
                           (case source
                             :velocity (aget velocities index)
                             :note (/ (- (double (aget notes index)) 60.0) 12.0))
                           (aget controllers (long (:controller source))))]
        (aset values position
              (double (+ (aget values position)
                         (* (double (:depth route)) source-value)))))))
  values)

(defn render!
  "Render `frames` samples of the reference instrument into `output`, applying
  the modulation matrix.

  For every voice the pool resolves its plan (gate, note, bend, detune, pan),
  the matrix adds its per-source deltas, and the result is published back, then
  the pool mixes every sounding voice into `output`. `output` is a planar block
  (an array of channel arrays) for the stereo voice. Allocates nothing."
  ([instrument output frames]
   (render! instrument output frames (:sample-rate instrument)))
  ([instrument output frames sample-rate]
   (let [pool (:pool instrument)
         ^doubles values (:values instrument)
         capacity (long (:capacity instrument))]
     (dotimes [index capacity]
       (pool/read-controls! pool index values)
       (modulate! instrument values index)
       (pool/set-voice-controls! pool index values))
     (inst-host/render! instrument output frames sample-rate)
     nil)))

(defn set-effect-control!
  "Set a global post-mix control by name (for example `'chorus-mix`)."
  [instrument control value]
  (inst-host/set-bus-control! instrument control value))
