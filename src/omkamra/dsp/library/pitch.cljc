(ns omkamra.dsp.library.pitch
  "Pitch: the standard pitch-domain controls and their conversion to frequency.

  A voice in this system takes pitch in the **pitch domain** — a MIDI `note`
  plus `bend` and `detune` in cents — and converts it to Hz in the kernel, per
  sample, rather than receiving a precomputed frequency from the host
  (`DECISIONS.md` D35). These functions are the default conversions; a voice is
  free to define its own (they are ordinary `dsp/defn`s), and a declarative form
  that injects the tuning and the algorithm is a later step."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

(dsp/defn cents->ratio
  "The frequency ratio of `cents` cents: `2^(cents/1200)`. A semitone is 100
  cents, so this is how bend and detune multiply a frequency.
"
  [cents]
  (pow 2.0 (/ cents 1200.0)))

(dsp/defn pitch->frequency
  "The frequency in Hz of MIDI `note`, bent by `bend` cents and detuned by
  `detune` cents, in a tuning of `reference` Hz at `reference-note` (the usual
  tuning is 440.0 at 69).

  Equal temperament, evaluated in the kernel: a bend or a detune is applied per
  sample rather than quantised to a block boundary, and it is inlined into the
  voice's frame loop, so it costs no more than the `pow` it is.
"
  [note bend detune reference reference-note]
  (* reference
     (cents->ratio (+ (+ (* (- note reference-note) 100.0) bend) detune))))
