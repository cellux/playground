(ns omkamra.dsp.source
  "The one entry point that compiles a DSP definition, however the caller has it
  (P13d).

  A caller either already has a definition — a descriptor, or on the JVM a Var —
  or has source text that defines one. `compile` takes both and returns the same
  artifact, so an app that writes its DSP in ordinary namespaces and an app that
  lets its users type DSP into an editor share one call, one artifact, and one
  sequencing layer above it.

  The two inputs differ only in the evaluator: a definition links and compiles,
  while a source string is first evaluated to a definition (SCI in both worlds,
  or the JVM's own reader and macros with `{:evaluator :native}`) and then
  compiled the same way. That evaluation is the one step with a security
  boundary, so it is an explicit option rather than something `compile` does
  silently.

  Linking resolves calls through `:resolve`, and a definition a call-emitting
  transformer carries in `:dsp/dependencies` is supplied automatically — the same
  behaviour `omkamra.dsp/link` has always had, now portable."
  (:refer-clojure :exclude [compile])
  (:require [omkamra.dsp.capture :as capture]
            [omkamra.dsp.compiler :as compiler]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.sci :as sci]))

(def front-end-options
  "Options `compile` consumes itself; everything else is a compiler option."
  #{:resolve :definitions :library :evaluator :definition})

(defn- value-of
  [entry]
  (if (var? entry) @entry entry))

(defn- unit?
  [value]
  (= :compilation-unit (:dsp/kind value)))

(defn link-definition
  "Link `definition` into a compilation unit.

  Resolves ordinary calls through the caller's `:resolve` (or the JVM's own
  namespace lookup), and adds the definitions a call-emitting transformer
  recorded in `:dsp/dependencies` transitively."
  [definition options]
  (let [supplied (:definitions options)
        carried (capture/carried-definitions value-of (cons definition (vals supplied)))]
    (linker/link definition
                 (cond-> {:definitions (merge carried supplied)}
                   (:resolve options) (assoc :resolve (:resolve options))))))

(defmulti compile
  "Compile a DSP definition — or a source string that defines one — for a target.

  `definition` is a descriptor, a linked unit, a JVM Var, or a string of DSP
  forms. `options` carries the compiler's options (`:target`, `:entry`,
  `:precision`, `:channels`, …) plus the front end's:

  - `:definition` — which definition a source string defines, when it defines
    more than one;
  - `:evaluator`  — `:sci` (default, both worlds) or `:native` (JVM only);
  - `:resolve`    — the linker's resolver; defaults to the JVM's Var lookup;
  - `:definitions`— extra `{id descriptor}` the link may reach (a browser app's
    own definitions, since ClojureScript has no runtime namespace resolution);
  - `:library`    — `{id descriptor}` seeded into the SCI context so a source can
    `require` the library.

  Returns the target's artifact, the same shape `omkamra.dsp.compiler/compile`
  returns; how it is instantiated is the target's business."
  (fn [definition _options]
    (if (string? definition) :source :definition)))

(defmulti evaluate
  "Evaluate a source string to a definition, dispatching on `:evaluator`.

  `:sci` is the default and the only option in the browser world; `:native` uses
  the JVM's own reader and macros and is registered by the JVM world face."
  (fn [_source options] (or (:evaluator options) :sci)))

(defmethod evaluate :sci
  [source options]
  (sci/evaluate-source source options))

(defmethod compile :definition
  [definition options]
  (let [options (or options {})
        unit (if (unit? definition)
               definition
               (link-definition definition options))]
    (compiler/compile unit (apply dissoc options front-end-options))))

(defmethod compile :source
  [source options]
  (let [{:keys [definition definitions]} (evaluate source options)]
    (compile definition (update options :definitions merge definitions))))
