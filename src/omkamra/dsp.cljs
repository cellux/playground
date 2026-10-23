(ns omkamra.dsp
  "The browser world's face of the DSP entry points (P13d).

  A browser app consumes the DSP language in two pieces, one per side of the
  macro/runtime split ClojureScript requires:

      (:require        [omkamra.dsp :as dsp])   ; runtime: dsp/compile
      (:require-macros [omkamra.dsp :as dsp])   ; macros:  dsp/defn, dsp/fn, …

  which is the same arrangement `cljs.core.async` uses for `go` and `chan`. The
  macros come from the `.clj` side, compiled into the page as data — a definition
  written statically is captured by the JVM's own macro, exactly as the library's
  definitions are. This file is the runtime side: one `compile` that reaches a
  definition the app already has, or evaluates a source string, and returns an
  artifact the sequencing layer drives.

  The world's two defaults differ from the JVM's only because ClojureScript has
  no runtime namespace resolution (`find-ns`/`ns-resolve` are macros here): a
  registry resolver stands in for the Var lookup, and the DSP library is seeded
  into both the resolver and the SCI front end."
  (:refer-clojure :exclude [compile])
  (:require [omkamra.dsp.library-registry :as library]
            [omkamra.dsp.registry :as registry]
            [omkamra.dsp.source :as source]))

(def ^:private world
  "The browser world's defaults: the library descriptors the SCI front end seeds
  from, and a resolver over them, because a ClojureScript build has no runtime
  namespace to look a call up in."
  (let [definitions library/library-definitions]
    {:library definitions
     :resolve (registry/resolver (registry/seed definitions))}))

(defn compile
  "Compile a DSP definition — or a source string that defines one — for a target.

  `definition` is a descriptor the app captured with `dsp/defn` (or a linked
  unit), or a string of DSP forms. This is the browser world's face of
  `omkamra.dsp.source/compile`; it supplies the library seed and the registry
  resolver the world needs, and everything else is the shared pipeline.

  A definition the app wrote that calls another of its own definitions must be
  supplied through `:definitions` — `{qualified-id descriptor}` — because there
  is no namespace to resolve it in. Calls to the DSP library resolve
  automatically. A source string resolves through SCI on this side."
  ([definition]
   (compile definition {}))
  ([definition options]
   (source/compile definition (merge world options))))
