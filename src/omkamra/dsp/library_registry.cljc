(ns omkamra.dsp.library-registry
  "The DSP library as a registry seed (P13d).

  This is the one namespace that depends on the library *and* the registry, and
  it is deliberately a leaf: the registry stays generic so the compiler can use
  it without the library in reach, and this is the consumer that reads the
  library in. A page requires it to have the library at hand; the JVM's static
  path does not need it at all, and its dynamic path loads it lazily.

  `ns-publics` is written out once per namespace, with a *literal* symbol,
  because that is what ClojureScript's macro requires: it expands to the
  compiler's own intern list, and only a literal namespace can be named. The
  descriptors it yields are the ones the JVM links — the equivalence check says
  so per definition — so reading them this way cannot drift from the JVM's."
  (:require [omkamra.dsp.library.control]
            [omkamra.dsp.library.delay]
            [omkamra.dsp.library.effect]
            [omkamra.dsp.library.envelope]
            [omkamra.dsp.library.filter]
            [omkamra.dsp.library.math]
            [omkamra.dsp.library.noise]
            [omkamra.dsp.library.oscillator]
            [omkamra.dsp.library.pitch]
            [omkamra.dsp.registry :as registry]))

(def library-namespaces
  "The DSP library namespaces, in reference order."
  '[omkamra.dsp.library.math
    omkamra.dsp.library.pitch
    omkamra.dsp.library.oscillator
    omkamra.dsp.library.noise
    omkamra.dsp.library.envelope
    omkamra.dsp.library.filter
    omkamra.dsp.library.delay
    omkamra.dsp.library.control
    omkamra.dsp.library.effect])

(def library-definitions
  "The whole DSP library, as `{qualified-id descriptor}`."
  (into {}
        (mapcat registry/definitions-of
                [(ns-publics 'omkamra.dsp.library.math)
                 (ns-publics 'omkamra.dsp.library.pitch)
                 (ns-publics 'omkamra.dsp.library.oscillator)
                 (ns-publics 'omkamra.dsp.library.noise)
                 (ns-publics 'omkamra.dsp.library.envelope)
                 (ns-publics 'omkamra.dsp.library.filter)
                 (ns-publics 'omkamra.dsp.library.delay)
                 (ns-publics 'omkamra.dsp.library.control)
                 (ns-publics 'omkamra.dsp.library.effect)])))

(defn library-registry
  "A registry seeded with the whole DSP library."
  []
  (registry/seed library-definitions))
