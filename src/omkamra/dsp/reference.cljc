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
  (:require [omkamra.dsp.synth.pool :as pool]))

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

(defn control-positions
  "Map each control name in a runtime control plan to its index."
  [plan]
  (into {}
        (map-indexed (fn [index entry]
                       [(name (:control entry)) index])
                     plan)))

(defn compile-routes
  "Flatten `matrix` into routes with a resolved control `:position`.

  A target the voice does not declare is refused here rather than silently
  dropping modulation."
  [matrix positions]
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
        matrix))

(defn new-host
  "Build the portable host state over a live `pool`.

  `plan` is the runtime control plan and `matrix` the raw routes, exactly as the
  served bundle or the lowered program carries them. The host keeps only
  primitive arrays and reusable scratch, so the matrix and the render loop touch
  nothing but the pool API."
  [pool plan matrix]
  (let [capacity (long (pool/pool-capacity pool))
        positions (control-positions plan)]
    {:pool pool
     :plan plan
     :positions positions
     ;; A plain vector of maps: the per-voice loop indexes it, and nothing here
     ;; needs the target's own array representation, so none is chosen.
     :routes (compile-routes matrix positions)
     :capacity capacity
     :values (pool/make-doubles (pool/control-count pool))
     :notes (pool/make-longs capacity)
     :velocities (pool/make-doubles capacity)
     :controllers (pool/make-doubles controller-count)}))

;; ---------------------------------------------------------------------------
;; Playing it
;; ---------------------------------------------------------------------------

(defn note-on!
  "Start `note` at `velocity`, returning the voice index it took. The host keeps
  the note and velocity for that index, because the matrix reads them as sources
  and the pool may have stolen the voice."
  ([host note] (note-on! host note 1.0))
  ([host note velocity]
   (let [index (long (pool/note-on! (:pool host) note velocity))]
     (aset ^longs (:notes host) index (long note))
     (aset ^doubles (:velocities host) index (double velocity))
     index)))

(defn note-off!
  "Release every voice sounding `note`. Returns how many the pool released."
  [host note]
  (pool/note-off! (:pool host) note))

(defn all-notes-off!
  "Release every sounding voice."
  [host]
  (pool/all-notes-off! (:pool host)))

(defn set-controller!
  "Set controller `controller` (0..127) to `value` (0..1). Global: it applies to
  every voice on the next block."
  [host controller value]
  (aset ^doubles (:controllers host) (long controller) (double value))
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

(defn- modulate!
  "Apply every matrix route to `values` for voice `index`, in route order."
  [host ^doubles values ^long index]
  (let [routes (:routes host)
        ^longs notes (:notes host)
        ^doubles velocities (:velocities host)
        ^doubles controllers (:controllers host)]
    (dotimes [route-index (count routes)]
      (let [route (nth routes route-index)
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

(defn apply-modulation!
  "Publish every voice's modulated controls to the pool.

  For every voice the pool resolves its plan (gate, note, bend, detune, pan),
  the matrix adds its per-source deltas, and the result is published back. Call
  this before `pool/render!` for each block; the render itself is the host's —
  the JVM adds its bus through `hosts.instrument`, the browser through its own
  bus runner. Allocates nothing."
  [host]
  (let [pool (:pool host)
        ^doubles values (:values host)
        capacity (long (:capacity host))]
    (dotimes [index capacity]
      (pool/read-controls! pool index values)
      (modulate! host values index)
      (pool/set-voice-controls! pool index values))
    nil))
