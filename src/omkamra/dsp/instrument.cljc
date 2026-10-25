(ns omkamra.dsp.instrument
  "The Clojure-level instrument graph: a voice, its polyphony, and a post-mix
  effect chain, composed as ordinary data.

  This is the **source layer**. Nothing here knows about kernels, storage,
  channel layout, or how many artifacts a target needs — that is the lowering's
  business (see `omkamra.dsp.instrument.program`). The value states *semantics*:

  * `voice` names the DSP definition the polyphony runs and maps its per-voice
    controls;
  * `with-effect` appends a stage to the post-mix chain, a processor applied
    once to the summed voices rather than once per voice;
  * `routes` carries the (provisional) modulation matrix.

  Transforms are plain functions over the value, so `->` composes them, exactly
  as the definition-value combinators do (D36/D38). The compiler is free to
  reorder or fuse what it finds here; the organization at this level is not a
  promise about the kernel."
  (:require [omkamra.dsp.graph :as graph]))

(defn instrument
  "Start an instrument value from the synth description's top-level fields —
  `:id`, `:capacity`, `:mode`, `:max-block-frames`, `:bend-range`, `:stealing`.

  A description that already carries `:voice` is accepted as-is, so an existing
  `omkamra.dsp.synth` map can be lifted directly."
  [options]
  {:dsp/kind :instrument
   :description options
   :effects []})

(defn voice
  "Set the instrument's voice: a DSP definition (a Var, a qualified symbol, or a
  definition value) and the map from its control names to sources/constants."
  [instrument definition controls]
  (assoc-in instrument [:description :voice]
            {:definition definition :controls controls}))

(defn routes
  "Attach the modulation matrix. This is host policy that lives above the kernel
  and is deliberately not part of core (D34); it is carried here so a lowering
  can hand it to its host."
  [instrument matrix]
  (assoc instrument :routes matrix))

(defn with-effect
  "Append a **post-mix** effect stage.

  `effect` is a processor definition — its first parameter is the signal and the
  rest are its controls. `arguments` supplies those controls, one form each:
  a literal is baked in, a name becomes an instrument-level (global) control the
  host can set. A name may be written as a symbol or a keyword (`:chorus-rate`),
  and is normalized to the symbol the kernel uses (see
  `omkamra.dsp.graph/normalize-control-argument`). The stage runs once over the
  summed voices, after the pool."
  [instrument effect arguments]
  (update instrument :effects conj
          {:effect effect
           :arguments (mapv graph/normalize-control-argument arguments)}))

(defn effects
  "The post-mix chain, in order."
  [instrument]
  (:effects instrument))

(defn voice-definition
  "The voice's DSP definition, or nil."
  [instrument]
  (get-in instrument [:description :voice :definition]))

(defn description
  "The underlying synth description (top-level fields plus `:voice`)."
  [instrument]
  (:description instrument))
