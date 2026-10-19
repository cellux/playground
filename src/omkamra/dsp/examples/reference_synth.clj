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
  pool API. That portable half — the modulation matrix and the render loop — now
  lives in `omkamra.dsp.reference` (`.cljc`) and is shared with the browser
  hosts; this namespace keeps the instrument value and the JVM constructor, which
  is the only part that reaches for the compiler."
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
            [omkamra.dsp.reference :as reference]))

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

  The filter cutoff is clamped below a sixth of the sample rate, and the damping
  is clamped above the Chamberlin SVF's stability bound for the resulting
  cutoff, so a modulated cutoff cannot drive the filter unstable."
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
        ;; The Chamberlin SVF is stable only while its damping exceeds
        ;; `f / (2 - f^2/2)`, where `f = 2 sin(pi fc/fs)`. The cutoff clamp
        ;; alone is not enough: at `fc = fs/6`, `f` is 1 and the damping must
        ;; exceed 2/3, well above this voice's low-resonance settings. Clamp the
        ;; damping up, with margin, so a modulated cutoff cannot drive the filter
        ;; unstable and an approximate `sin` cannot sit it on the boundary.
        filter-f (* 2.0 (sin (* 3.141592653589793 (/ modulated-cutoff sample-rate))))
        min-damping (* 1.25 (/ filter-f (- 2.0 (* (* 0.5 filter-f) filter-f))))
        damping (max (max resonance 0.05) min-damping)
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
   velocity mod-wheel brightness
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
                        ;; Modulation sources the matrix reads (D45/M3): the voice
                        ;; does not use them in DSP, but a kernel-local source
                        ;; must be a control.
                        {:name velocity} {:name mod-wheel} {:name brightness}
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
   :parameters
   {:osc-mix {:default 0.5}
    :pulse-width {:default 0.5}
    :pwm-depth {:default 0.15}
    :sync-mix {:default 0.0}
    :sync-ratio {:default 2.0}
    :noise-level {:default 0.05}
    :cutoff {:default 800.0 :min 20.0 :max 8000.0 :unit :hz :curve :log}
    :resonance {:default 0.6}
    :filter-amount {:default 1.5}
    :attack {:default 0.005}
    :decay {:default 0.15}
    :sustain {:default 0.6}
    :release {:default 0.3}
    :filter-attack {:default 0.005}
    :filter-decay {:default 0.2}
    :filter-sustain {:default 0.3}
    :filter-release {:default 0.3}
    :lfo-rate {:default 5.0}
    :lfo-shape {:enum #{0 1 2 3} :default 1}
    :vibrato-depth {:default 5.0}
    :drive {:default 1.0}
    ;; The matrix's global sources: `:shared`, so every voice reads one cell.
    :mod-wheel {:scope :shared :default 0.0}
    :brightness {:scope :shared :default 0.0}
    ;; The post-mix chain's globals: `:shared`, so the bus binds them.
    :chorus-rate {:scope :shared :default 0.6}
    :chorus-depth {:scope :shared :default 12.0}
    :chorus-mix {:scope :shared :default 0.35}
    :post-drive {:scope :shared :default 1.5}
    :delay-time {:scope :shared :default 12000.0}
    :delay-feedback {:scope :shared :default 0.45}
    :delay-mix {:scope :shared :default 0.35}}
   :voice {:definition #'reference-voice}})

;; The modulation matrix and the loop that applies it live in
;; `omkamra.dsp.reference`, so the browser hosts run the same host code; see
;; `reference/modulation-matrix` and `reference/new-host`.

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
      (inst/routes reference/modulation-matrix)
      (inst/with-effect omkamra.dsp.library.delay/chorus
        [:chorus-rate :chorus-depth :chorus-mix])
      (inst/with-effect omkamra.dsp.library.effect/distortion
        [:post-drive])
      (inst/with-effect omkamra.dsp.library.delay/feedback-delay
        [:delay-time :delay-feedback :delay-mix])))

(defn build-instrument
  "Build a live reference instrument from an `omkamra.dsp.instrument` value:
  lower it to a program, create the live pool and bus, and add the portable
  matrix host (`omkamra.dsp.reference`).

  Exposed so a test can lower a *variant* of the instrument — for example the
  same voice with no post-mix chain — and compare. `compile-options` selects the
  target, precision, and containment policy."
  ([value]
   (build-instrument value {}))
  ([value compile-options]
   (let [program (inst-host/compile-instrument value compile-options)
         live (inst-host/instrument-pool
               program (select-keys compile-options [:containment]))
         live-pool (inst-host/pool-of live)
         plan (vec (:control-plan (get-in program [:bundle :runtime])))
         matrix (or (:routes value) reference/modulation-matrix)]
     ;; The post-mix controls default from the description's `:shared`
     ;; parameters, which `instrument-pool` wires the bus to.
     (merge live (reference/new-host live-pool plan matrix)))))

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
  "Start `note` at `velocity`, returning the voice index it took."
  ([instrument note] (reference/note-on! instrument note))
  ([instrument note velocity] (reference/note-on! instrument note velocity)))

(defn note-off!
  "Release every voice sounding `note`. Returns how many the pool released."
  [instrument note]
  (reference/note-off! instrument note))

(defn all-notes-off!
  "Release every sounding voice."
  [instrument]
  (reference/all-notes-off! instrument))

(defn set-controller!
  "Set controller `controller` (0..127) to `value` (0..1). Global: it applies to
  every voice on the next block."
  [instrument controller value]
  (reference/set-controller! instrument controller value))

(defn set-pitch-bend!
  "Set channel `channel`'s pitch bend, in -1..1; the pool scales it by the
  description's `:bend-range` and delivers it as `bend` in cents."
  [instrument channel value]
  (reference/set-pitch-bend! instrument channel value))

(defn set-pan!
  "Set voice `index`'s pan position in -1..1 (`:pan` is a standard source)."
  [instrument index value]
  (reference/set-pan! instrument index value))

;; ---------------------------------------------------------------------------
;; Routing and rendering
;; ---------------------------------------------------------------------------

(defn render!
  "Render `frames` samples of the reference instrument into `output`, applying
  the modulation matrix.

  The matrix loop is `omkamra.dsp.reference/apply-modulation!`, shared with the
  browser hosts; the pool and bus render is `inst-host/render!`. `output` is a
  planar block (an array of channel arrays) for the stereo voice. Allocates
  nothing."
  ([instrument output frames]
   (render! instrument output frames (:sample-rate instrument)))
  ([instrument output frames sample-rate]
   (reference/apply-modulation! instrument)
   (inst-host/render! instrument output frames sample-rate)
   nil))

(defn set-effect-control!
  "Set a global post-mix control by name (for example `'chorus-mix`)."
  [instrument control value]
  (inst-host/set-bus-control! instrument control value))
