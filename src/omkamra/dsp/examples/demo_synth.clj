(ns omkamra.dsp.examples.demo-synth
  "A small example synthesizer built on `omkamra.dsp.synth`, and the home of one
  possible host-side modulation wiring.

  **What this is.** An example, not a core mechanism. `omkamra.dsp.synth`
  compiles a voice and runs a pool, and it deliberately keeps *no* opinion about
  how modulation is computed or routed (`DECISIONS.md` D34): there are many ways
  to wire it — a declarative route table like this one, a per-target callback,
  routing done in DSP source, or a first-class modulation signal in the language.
  This namespace demonstrates the first of those end to end, so the approach can
  be evaluated and, if it earns it, promoted to core with a decision of its own.

  **The one core affordance it uses.**
  `omkamra.dsp.synth.pool/set-voice-controls!` is the pool's delivery channel: it
  publishes a `double` array in ABI order to a voice. That is all the pool needs
  to offer. Everything below — sources, targets, depths, and even what a depth
  *means* — is the example's own policy.

  **Pitch is a standard source, converted in DSP.** The voice takes `note`,
  `bend`, and `detune` (cents) and turns them into Hz with the library's
  `omkamra.dsp.library.pitch/pitch->frequency`, so pitch bend is neither a route
  nor host math: the host only scales the player's bend by the declared range,
  and the kernel applies it per sample. That is the shape the runtime's standard
  vocabulary will take.

  **Parameter modulation is a route.** A route is
  `{:source ... :target :control :depth n}` and is applied as
  `value = base + depth * source-value`. Sources are per-voice (`:velocity`,
  `:note`) or global (`{:controller n}`). `:note` reads in octaves from middle C
  (C4 = 0, C5 = 1), so a depth is a musical distance rather than a raw MIDI
  delta.

  **The demo is a thin layer over the pool's sources.** For each voice
  `read-controls!` gives the plan's values (gate open while the voice sounds, and
  `note`/`bend`/`detune` resolved by the pool's plan and its global source bank),
  the routes add their parameter deltas, and `set-voice-controls!` publishes the
  result. So the runtime delivers the standard sources and the demo only decides
  the parameter modulation — no duplicated plan resolution — and the loop still
  allocates nothing. Only velocity, which is not a control of this voice, and the
  controller bank are tracked here.

  **What it grows into.** The remaining P10 slices (voice/effect tails, stereo
  spreading, the browser host) and the P11 reference synthesizer build on this
  instrument rather than starting over, which is why the modulation wiring lives
  here rather than in the core."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.filter]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.library.pitch]
            [omkamra.dsp.hosts.jvm :as host]
            [omkamra.dsp.synth.pool :as pool]))

;; ---------------------------------------------------------------------------
;; The voice
;; ---------------------------------------------------------------------------

(dsp/defn demo-voice
  "A band-limited sawtooth shaped by an exponential ADSR and filtered by a
  multimode biquad (lowpass mode).

  Its pitch controls are in the standard *pitch domain* — `note`, `bend`,
  `detune` — and the library's `pitch->frequency` converts them to Hz in the
  kernel. The rest are ordinary parameters (`cutoff`, `resonance`) and envelope
  times. The library calls are fully qualified so the voice does not depend on
  how this namespace happens to be aliased, the same discipline the harness
  fixtures follow.

  The tuning is passed as literals, which is exactly the duplication the
  declarative form (b) would remove by injecting the description's `:tuning`.
"
  [gate note bend detune cutoff resonance attack decay sustain release]
  {:process {:input false}}
  (omkamra.dsp.library.filter/biquad-multimode
   (* (omkamra.dsp.library.envelope/exponential-adsr gate attack decay sustain release)
      (omkamra.dsp.library.oscillator/saw-oscillator
       (omkamra.dsp.library.pitch/pitch->frequency note bend detune 440.0 69.0)))
   cutoff resonance 0))

;; ---------------------------------------------------------------------------
;; Voices built from combinators
;; ---------------------------------------------------------------------------
;;
;; The combinators live in `omkamra.dsp.combinators`: plain functions over
;; definition values, composed with `->` and named by `dsp/def` (D36/D38). The
;; hand-written equivalents below are the structural oracles.

(dsp/def unison-demo-voice
  (-> demo-voice
      (comb/unison {:count 3 :detune 20.0})))

(dsp/def smoothed-demo-voice
  (-> demo-voice
      (comb/smooth :cutoff 0.02)))

;; Three oscillator voices sharing one interface, and the synth built by mixing.

(dsp/defn saw-tone
  [gate note detune]
  {:process {:input false}}
  (* (omkamra.dsp.library.envelope/exponential-adsr gate 0.005 0.08 0.6 0.2)
     (omkamra.dsp.library.oscillator/saw-oscillator
      (omkamra.dsp.library.pitch/pitch->frequency note 0.0 detune 440.0 69.0))))

(dsp/defn square-tone
  [gate note detune]
  {:process {:input false}}
  (* (omkamra.dsp.library.envelope/exponential-adsr gate 0.005 0.08 0.6 0.2)
     (omkamra.dsp.library.oscillator/square-oscillator
      (omkamra.dsp.library.pitch/pitch->frequency note 0.0 detune 440.0 69.0))))

(dsp/defn triangle-tone
  [gate note detune]
  {:process {:input false}}
  (* (omkamra.dsp.library.envelope/exponential-adsr gate 0.005 0.08 0.6 0.2)
     (omkamra.dsp.library.oscillator/triangle-oscillator
      (omkamra.dsp.library.pitch/pitch->frequency note 0.0 detune 440.0 69.0))))

(dsp/def three-oscillator
  (-> saw-tone
      (comb/mix square-tone)
      (comb/mix triangle-tone)))

(dsp/defn three-oscillator-by-hand
  "The mixed synth written out, as the oracle for `mix`."
  [gate note detune]
  {:process {:input false}}
  (+ (+ (saw-tone gate note detune)
        (square-tone gate note detune))
     (triangle-tone gate note detune)))

;; A per-voice effect: a stateful one-pole after the voice, at a fixed
;; coefficient, and its hand-wired oracle.

(dsp/def damped-saw
  (-> saw-tone
      (comb/through omkamra.dsp.library.filter/one-pole [0.9])))

(dsp/defn damped-saw-by-hand
  [gate note detune]
  {:process {:input false}}
  (omkamra.dsp.library.filter/one-pole (saw-tone gate note detune) 0.9))

;; A fixed-N polyphonic voice, and the hand-written chord as the oracle.

(dsp/def three-voice-poly
  (comb/poly saw-tone 3))

(dsp/defn three-voice-poly-by-hand
  [detune
   gate_0 note_0
   gate_1 note_1
   gate_2 note_2]
  {:process {:input false}}
  (+ (+ (saw-tone gate_0 note_0 detune)
        (saw-tone gate_1 note_1 detune))
     (saw-tone gate_2 note_2 detune)))

(dsp/def three-oscillator-by-sum
  (comb/sum [saw-tone square-tone triangle-tone]))

;; Sharing one voice between a dry path and a filtered path: the voice is
;; evaluated once, not twice.

(dsp/def shared-dry-wet
  (comb/share saw-tone
              (fn [dry]
                (list '+
                      dry
                      (list '* 0.5
                            (list 'omkamra.dsp.library.filter/one-pole dry 0.5))))))

(dsp/defn shared-dry-wet-by-hand
  [gate note detune]
  {:process {:input false}}
  (let [dry (saw-tone gate note detune)]
    (+ dry (* 0.5 (omkamra.dsp.library.filter/one-pole dry 0.5)))))

(dsp/defn duplicated-dry-wet-by-hand
  [gate note detune]
  {:process {:input false}}
  (+ (saw-tone gate note detune)
     (* 0.5
        (omkamra.dsp.library.filter/one-pole
         (saw-tone gate note detune) 0.5))))

;; ---------------------------------------------------------------------------
;; The description and the routing
;; ---------------------------------------------------------------------------

(def description
  "The demo instrument, as `omkamra.dsp.synth` data. `note` and `gate` are the
  pool's own plan sources; everything else is a control the example's host layer
  supplies. Pitch is not a plan source at all — it is the voice's own conversion
  of the pitch-domain controls."
  {:id :demo-synth
   :capacity 8
   :max-block-frames 512
   :parameters {:cutoff {:default 1000.0}
                :resonance {:default 1.5}
                :attack {:default 0.01}
                :decay {:default 0.2}
                :sustain {:default 0.7}
                :release {:default 0.4}}
   :voice {:definition #'demo-voice}})

(def default-routes
  "One possible *parameter* modulation wiring, as pure data. Velocity and note
  are per-voice; controller 1 (the mod wheel) is global. Pitch is deliberately
  absent: bend and detune are standard sources the voice consumes directly, not
  routes onto a control."
  [{:source :velocity :target :cutoff :depth 2000.0}
   {:source :note :target :cutoff :depth 1500.0}
   {:source {:controller 1} :target :cutoff :depth 3000.0}
   {:source {:controller 1} :target :resonance :depth 2.0}])

(def ^:private controller-count 128)

(defn- compiled-routes
  "Resolve each route's target to a control position, once, at construction."
  [routes positions]
  (mapv (fn [route]
          (let [target (name (:target route))]
            (when-not (contains? positions target)
              (throw (ex-info "the demo synth has no such control"
                              {:target (:target route)
                               :controls (vec (sort (keys positions)))})))
            (assoc route :position (long (get positions target)))))
        routes))

;; ---------------------------------------------------------------------------
;; The instrument
;; ---------------------------------------------------------------------------

(defn demo-synth
  "Build the example instrument: a compiled voice, its pool, and the preallocated
  storage the routing needs. `compile-options` is passed to
  `omkamra.dsp.hosts.jvm/compile-synth`."
  ([] (demo-synth {}))
  ([compile-options]
   (let [bundle (host/compile-synth description compile-options)
         pool (host/synth-pool bundle)
         config (pool/pool-config pool)
         plan (vec (:control-plan config))
         positions (into {} (map-indexed (fn [index entry]
                                           [(name (:control entry)) index])
                                         plan))
         capacity (long (pool/pool-capacity pool))]
     {:bundle bundle
      :pool pool
      :plan plan
      :positions positions
      :routes (compiled-routes default-routes positions)
      :capacity capacity
      :sample-rate 48000.0
      ;; One control scratch, reused for every voice and block: the pool copies.
      :values (double-array (pool/control-count pool))
      ;; The per-voice modulation sources the routes read. Gate, note, bend, and
      ;; detune come from the pool itself now, so only the two that are not
      ;; controls (velocity) and the global controller bank live here.
      :notes (long-array capacity)
      :velocities (double-array capacity)
      :controllers (double-array controller-count)})))

;; ---------------------------------------------------------------------------
;; Playing it
;; ---------------------------------------------------------------------------

(defn note-on!
  "Start `note` at `velocity`, returning the voice index it took. The demo
  remembers the note for that index, because the pool may have stolen a voice and
  the routes read the note as a source."
  ([synth note] (note-on! synth note 1.0))
  ([synth note velocity]
   (let [index (long (pool/note-on! (:pool synth) note velocity))]
     (aset ^longs (:notes synth) index (long note))
     (aset ^doubles (:velocities synth) index (double velocity))
     index)))

(defn note-off!
  "Release every voice sounding `note`. Returns how many the pool released."
  [synth note]
  (pool/note-off! (:pool synth) note))

(defn all-notes-off!
  "Release every sounding voice."
  [synth]
  (pool/all-notes-off! (:pool synth)))

(defn set-controller!
  "Set controller `controller` (0..127) to `value` (0..1). Global: it applies to
  every voice on the next block."
  [synth controller value]
  (aset ^doubles (:controllers synth) (long controller) (double value))
  nil)

(defn set-pitch-bend!
  "Set channel `channel`'s pitch bend, in -1..1. The pool scales it by the
  description's `:bend-range` and delivers it as the `bend` control in cents."
  [synth channel value]
  (pool/set-pitch-bend! (:pool synth) channel value))

;; ---------------------------------------------------------------------------
;; Routing and rendering
;; ---------------------------------------------------------------------------

(defn- source-value
  "The current value of a route's source for voice `index`."
  ^double [source synth ^long index]
  (if (keyword? source)
    (case source
      :velocity (aget ^doubles (:velocities synth) index)
      :note (/ (- (double (aget ^longs (:notes synth) index)) 60.0) 12.0))
    (aget ^doubles (:controllers synth) (long (:controller source)))))

(defn- modulate!
  "Apply every route to `values` for voice `index`, in route order."
  ^doubles [^doubles values routes synth ^long index]
  (doseq [route routes]
    (let [value (source-value (:source route) synth index)
          position (long (:position route))]
      (aset values position
            (double (+ (aget values position)
                       (* (double (:depth route)) value))))))
  values)

(defn render!
  "Render `frames` samples of the demo synth into `output`, applying the routing.

  For every voice the pool resolves the plan (the gate from the voice's state,
  and `note`/`bend`/`detune` from its own sources), the routes add their parameter
  deltas, and the result is published back. That is the whole modulation layer: it
  never recomputes a value the pool already knows. Allocates nothing."
  ([synth output frames] (render! synth output frames (:sample-rate synth)))
  ([synth output frames sample-rate]
   (let [pool (:pool synth)
         ^doubles values (:values synth)
         routes (:routes synth)
         capacity (long (:capacity synth))]
     (dotimes [index capacity]
       (pool/read-controls! pool index values)
       (modulate! values routes synth index)
       (pool/set-voice-controls! pool index values))
     (pool/render! pool output frames (double sample-rate))
     nil)))

(comment
  ;; For the REPL: one block with the mod wheel open, then the peak amplitude.
  ;; `render!` fills the start of `output`, so a phrase is a sequence of blocks
  ;; with the caller slicing (or the P9 scheduler doing it).
  (let [synth (demo-synth)
        output (float-array 128)]
    (note-on! synth 60 0.7)
    (render! synth output 128)
    (let [open (reduce max 0.0 (map #(Math/abs (double %)) output))]
      (set-controller! synth 1 0.8)
      (render! synth output 128)
      (let [closed (reduce max 0.0 (map #(Math/abs (double %)) output))]
        [open closed]))))
