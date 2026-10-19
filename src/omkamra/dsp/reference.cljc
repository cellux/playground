(ns omkamra.dsp.reference
  "The reference synthesizer's portable host half: the modulation matrix and the
  render loop over a live pool.

  The matrix is host policy, deliberately not core (D34): rows of
  `{source -> [{:target :depth}]}`, applied as `base + depth x source` on top of
  the pool's control plan. It is pure data, and the loop that applies it touches
  only the pool API and primitive arrays, so both halves live here once for every
  host — the JVM example that builds its live pool through `hosts.instrument` and
  the browser hosts that build theirs from a served runtime.

  The voice DSP and the post-mix chain are the *instrument value*
  (`omkamra.dsp.examples.reference-synth`); this namespace is the host code that
  drives a pool built from it. It is `.cljc` so the browser targets run the same
  matrix as the JVM, which is what makes the reference synth an end-to-end check
  of the whole system rather than a JVM-only one."
  (:require [omkamra.dsp.modulation :as modulation]
            [omkamra.dsp.synth.pool :as pool]))

(def modulation-matrix
  "The reference instrument's modulation matrix, as pure data.

  Rows are *sources*; each row routes that source to one or more
  `{:target control :depth amount :offset bias}` entries, applied as
  `value = base + depth x (source-value + offset)`. Sources are the per-voice
  `:velocity` and `:note` (biased to middle C by `:offset`, so a depth is a
  musical distance), or a global parameter the host sets (`:mod-wheel`,
  `:brightness`) in 0..1.

  Pitch (`bend`, `detune`) is deliberately absent: it is a standard source the
  voice consumes directly (D35), not a route onto another control.

  This is host-side policy, deliberately not core (D34). It is studied at the
  end of P11 to see whether any of it should be promoted, and in what form."
  [{:source :velocity
    :routes [{:target :cutoff :depth 1800.0}
             {:target :resonance :depth -0.15}
             {:target :osc-mix :depth 0.25}]}
   {:source :note
    :routes [{:target :cutoff :depth 100.0 :offset -60.0}
             {:target :pulse-width :depth 0.0125 :offset -60.0}]}
   {:source :mod-wheel
    :routes [{:target :cutoff :depth 3000.0}
             {:target :resonance :depth -0.25}
             {:target :pulse-width :depth 0.3}
             {:target :pan :depth 0.5}]}
   {:source :brightness
    :routes [{:target :osc-mix :depth 0.5}
             {:target :sync-mix :depth 0.4}
             {:target :noise-level :depth 0.2}]}])

(defn- plan-scopes
  "The scope of each control name in a runtime control plan: `:shared` when its
  parameter is shared, `:per-voice` otherwise (the runtime's own sources)."
  [plan]
  (into {}
        (map (fn [entry]
               [(keyword (:control entry))
                (if (= :shared (get-in entry [:storage :kind])) :shared :per-voice)]))
        plan))

(defn new-host
  "Build the portable host state over a live `pool`.

  `plan` is the runtime control plan and `matrix` the routes, exactly as the
  served bundle or the lowered program carries them. The matrix is turned into an
  *instrument reducer* and a *publish* step, both closed over the plan (M3): the
  render loop reads the pool's parameter values, reduces them by the matrix, and
  publishes the modulated controls — the hand-written loop it replaces."
  [pool plan matrix]
  (let [scopes (plan-scopes plan)
        ;; A host with no matrix (the browser bundle) still gets a host; an
        ;; empty matrix is a no-op reducer.
        matrix (or matrix [])]
    {:pool pool
     :plan plan
     :matrix matrix
     :scopes scopes
     :capacity (long (pool/pool-capacity pool))
     :state (modulation/instrument-state matrix scopes)
     :reduce (modulation/instrument-reducer matrix scopes)
     :publish (modulation/publisher matrix scopes)}))

;; ---------------------------------------------------------------------------
;; Playing it
;; ---------------------------------------------------------------------------

(defn note-on!
  "Start `note` at `velocity`, returning the voice index it took. Note and
  velocity are the pool's runtime sources, so the matrix reads them from the
  resolved controls."
  ([host note] (note-on! host note 1.0))
  ([host note velocity]
   (pool/note-on! (:pool host) note velocity)))

(defn note-off!
  "Release every voice sounding `note`. Returns how many the pool released."
  [host note]
  (pool/note-off! (:pool host) note))

(defn all-notes-off!
  "Release every sounding voice."
  [host]
  (pool/all-notes-off! (:pool host)))

(def ^:private controller-parameters
  "The MIDI controllers the reference matrix reads, as instrument parameters."
  {1 :mod-wheel 74 :brightness})

(defn set-controller!
  "Set the shared parameter a MIDI controller drives, in 0..1. Global: every
  voice reads the one cell on the next block. A controller the matrix does not
  read is ignored."
  [host controller value]
  (when-let [parameter (get controller-parameters (long controller))]
    (pool/set-parameter! (:pool host) parameter (double value)))
  nil)

(defn set-pitch-bend!
  "Set channel `channel`'s pitch bend, in -1..1; the pool scales it by the
  description's `:bend-range` and delivers it as `bend` in cents."
  [host channel value]
  (pool/set-pitch-bend! (:pool host) channel value))

(defn set-pan!
  "Set voice `index`'s pan position in -1..1 (`:pan` is a standard source)."
  [host index value]
  (pool/set-pan! (:pool host) index value))

;; ---------------------------------------------------------------------------
;; Routing and rendering
;; ---------------------------------------------------------------------------

(defn apply-modulation!
  "Reduce the instrument's current parameter values by the matrix and publish the
  modulated controls to the pool.

  The generated reducer and publish step (`omkamra.dsp.modulation`) replace the
  hand-written loop (M3); the matrix is still the data it always was. Call this
  before `pool/render!` for each block; the render itself is the host's — the JVM
  adds its bus through `hosts.instrument`, the browser through its own bus
  runner."
  [host]
  (let [pool (:pool host)]
    ((:publish host) pool ((:reduce host) ((:state host) pool)))
    nil))
