(ns omkamra.dsp
  "A small C-like DSP language embedded in Clojure data.

  The initial slice supports single-expression functions over the logical
  `:float` type, mutable locals, `do`, `set!`, `while`, float comparisons,
  boolean `and`/`or`/`not`, conditional expressions, explicit mono buffer
  loads/stores, and a channel-configurable block-processing ABI.
  Definitions are data; their bodies are never run as ordinary Clojure
  expressions."
  (:refer-clojure :exclude [compile defn defstruct fn reset!])
  (:require [omkamra.dsp.capture :as capture]
            [omkamra.dsp.compiler :as compiler]
            [omkamra.dsp.instance :as instance]
            [omkamra.dsp.instance.jvm :as instance-jvm]
            [omkamra.dsp.readers]
            [omkamra.dsp.source :as source]
            [omkamra.dsp.targets.jvm :as jvm]))

;; The reading of a definition form is portable and lives in `omkamra.dsp.capture`
;; (P13d, D82); what stays here is the JVM's half of the definition site — the
;; Clojure namespace an alias or a refer must be resolved through.

(clojure.core/defn- macro-source
  [form]
  (capture/source-of form (ns-name *ns*)))

(clojure.core/defn- var-symbol
  [v]
  (symbol (str (-> v meta :ns ns-name)) (str (-> v meta :name))))

(clojure.core/defn- macro-aliases
  [ns]
  (into {} (map (clojure.core/fn [[alias target]] [alias (ns-name target)])) (ns-aliases ns)))

(clojure.core/defn- macro-refers
  [ns]
  (into {}
        (keep (clojure.core/fn [[reference v]]
                (let [target-ns (-> v meta :ns ns-name)]
                  (when-not (= 'clojure.core target-ns)
                    [reference (var-symbol v)]))))
        (ns-refers ns)))

(clojure.core/defn- macro-fail
  [message form path data]
  (capture/fail message (macro-source form) form path data))

(clojure.core/defmacro defn
  "Capture a DSP function definition as a descriptor.

  The shorthand form is:

    (dsp/defn gain [sample amount] (* sample amount))

  As in `defn`, several body expressions are an implicit `(do ...)`:

    (dsp/defn stereo-cross [sample]
      (buffer-store :output 0 0 sample)
      (buffer-store :output 1 0 sample))

  An explicit descriptor may be packed into the parameter vector:

    (dsp/defn gain
      [{:params [{:name sample :type :float}]
        :return-type :float}
       sample])

  A definition asks to be **copied into its call sites** rather than called with
  `^:inline` on its name, or with `:inline true` in the descriptor. Inlining is
  the exception, not the default (D47), because a target may not be able to call:

    (dsp/defn ^:inline clamp01 [x] (max 0.0 (min 1.0 x)))

  Both forms capture source data; DSP bodies are never executed as ordinary
  Clojure expressions during definition or compilation."
  [name & args]
  (when-not (capture/valid-definition-name? name)
    (macro-fail "dsp/defn name must be a non-blank symbol"
                &form [:name] {:name name}))
  (list 'def name
        (list 'quote (capture/capture-function "dsp/defn" name &form (ns-name *ns*)
                                               (macro-aliases *ns*) (macro-refers *ns*)
                                               args))))

(clojure.core/defmacro fn
  "Capture an anonymous DSP definition as a value.

  `dsp/fn` is to `dsp/defn` what `clojure.core/fn` is to `defn`: it captures the
  same descriptor but returns it as a value instead of binding a Var, so a
  definition composes with `->` and with transformers:

    (def voice
      (-> (dsp/fn [sample amount] {:process {:controls [{:name amount}]}}
            (* sample amount))
          (unison {:count 3})))

  The descriptor carries a generated internal name and the defining namespace's
  source, so it links directly; `dsp/def` replaces the name when the value is
  bound to a Var.

  As in `dsp/defn`, an anonymous definition asks to be copied into its call
  sites rather than called, with `:inline true` in its descriptor — or with
  `^:inline` on the whole form:

    (def clipper
      (-> (dsp/fn [sample] {:inline true} (max -1.0 (min 1.0 sample)))
          (unison {:count 3})))"
  [& args]
  (list 'quote (capture/capture-function "dsp/fn" (gensym "dsp-fn-") &form (ns-name *ns*)
                                         (macro-aliases *ns*) (macro-refers *ns*)
                                         args)))

(clojure.core/defmacro defstruct
  "Capture a DSP state-record shape as data bound to a Var.

  Like `dsp/defn`, the shape takes a name followed by a body whose definitions
  and options are embedded DSP data rather than executable Clojure:

    (dsp/defstruct biquad-state
      [x1 x2 y1 y2])                     ; shorthand: :float fields, 0.0

    (dsp/defstruct biquad-state
      [{:name x1 :type :float :init 0.0} ; explicit field descriptors
       {:name taps :fields [{:name a} {:name b}]}]
      {:constant true})                  ; optional options map

  A process state declaration references the Var with `{:name z :struct
  biquad-state}`; the linker resolves the reference using the declaring
  namespace and inlines the fields. The description is portable data so a
  browser compiler can read the same forms."
  [name params & body]
  (when-not (capture/valid-definition-name? name)
    (macro-fail "dsp/defstruct name must be a non-blank symbol"
                &form [:name] {:name name}))
  (list 'def name
        (list 'quote (capture/capture-struct name &form (ns-name *ns*) params body))))

(clojure.core/defmacro def
  "Bind `name` to a DSP definition produced by a transformer.

  `dsp/defn` captures a definition from a source form; `dsp/def` takes an
  ordinary Clojure expression whose *value* is already a definition, so derived
  voices compose with `->`:

    (dsp/def unison-lead
      (-> lead-voice
          (unison {:count 3 :detune 15})))

  The Var holds the descriptor and is resolvable by the linker exactly like a
  `dsp/defn` Var. `:name` and `:source` are stamped here, so a transformer does
  not need to know where it is being defined."
  [name definition]
  (when-not (capture/valid-definition-name? name)
    (macro-fail "dsp/def name must be a non-blank symbol"
                &form [:name] {:name name}))
  (let [source (macro-source &form)]
    `(~'def ~name
            (let [definition# ~definition]
              (when-not (and (map? definition#) (= :function (:dsp/kind definition#)))
                (throw (ex-info "dsp/def expects a DSP definition value"
                                {:name '~name :definition definition#})))
              (assoc definition# :name '~name :source (quote ~source))))))

(clojure.core/defn link
  "Link a DSP descriptor or Var into a compilation unit.

  A linked unit resolves ordinary DSP calls using the defining namespace's
  Clojure alias/refer mappings. Anonymous descriptors can provide reachable
  definitions with `{:definitions {qualified-id descriptor-or-var}}`.

  A definition produced by a call-emitting transformer carries the definitions it
  calls in `:dsp/dependencies`; they are supplied here automatically, so a
  transform of an anonymous `dsp/fn` value links like one of a named voice."
  ([definition]
   (link definition {}))
  ([definition options]
   (source/link-definition definition (or options {}))))

;; The one target the portable pipeline cannot hold: `insn` is a JVM library, so
;; the JVM backend registers itself as `compile-target`'s `:jvm` method from this
;; side (P13c, D77). The pipeline is the same one the browser runs; only the final
;; target step is JVM-only, and this is that step.
(clojure.core/defmethod compiler/compile-target :jvm
  [_target {:keys [entry lowered abi options]}]
  (case entry
    :function
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile (assoc lowered :options options))]
      (merge (compiler/artifact :jvm :function lowered abi invoke options)
             jvm-artifact))

    :process
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile-process (assoc lowered :options options))
          compiled (merge (compiler/artifact :jvm :process lowered abi invoke options)
                          jvm-artifact)]
      (assoc compiled :initialize (:reset compiled)))))

;; The JVM world's own reader and macros, for a caller that would rather have a
;; source string read by Clojure than by SCI. It is registered as `source`'s
;; `:native` evaluator, so it is only reached when a string is compiled with
;; `{:evaluator :native}`.
(clojure.core/defmethod source/evaluate :native
  [source {:keys [definition]}]
  (binding [*ns* (or (find-ns 'user) *ns*)]
    (load-string source)
    (let [v (resolve definition)]
      (when-not (var? v)
        (throw (ex-info "the DSP source did not define the requested name"
                        {:definition definition})))
      {:definition @v
       ;; `find-ns` reaches them, so no supplied definitions are needed.
       :definitions {}})))

(clojure.core/defn- library-definitions
  "The DSP library descriptors, loaded on demand.

  A static-only JVM app — one that writes its DSP with the macros and never
  compiles a string — never reaches this, so it never pulls
  `omkamra.dsp.library-registry` (or SCI) in."
  []
  (deref (requiring-resolve 'omkamra.dsp.library-registry/library-definitions)))

(clojure.core/defn compile
  "Compile a DSP definition — or a source string that defines one — for a target.

  This is the JVM world's face of `omkamra.dsp.source/compile`: it supplies the
  two things the world owns — the JVM backend, registered as
  `compiler/compile-target`'s `:jvm` method, and the DSP library the SCI front end
  seeds from. It reaches a definition the same way it always has, through `link`
  and the JVM's own namespace lookup, and a source string through the same SCI
  front end the browser uses (or `{:evaluator :native}` for Clojure itself).

  The Wasm target returns WAT plus its linear-memory ABI; the browser-side WABT
  assembler turns that WAT into a binary module. The default `:entry` is
  `:function`, a scalar expression. Use `{:entry :process}` to compile the same
  definition as a block processor."
  ([definition]
   (compile definition {}))
  ([definition compile-options]
   (source/compile definition (merge {:library (library-definitions)}
                                     compile-options))))

;; The instance lifecycle lives in `omkamra.dsp.instance` (P13d): the JVM host
;; drives an instance from its own body, and it should not have to load this
;; macro namespace — the definition forms, the SCI front end, the library
;; registry — to do it. What stays here is a re-export, not a wrapper, so
;; `dsp/process!` is the same function as `instance/process!`: one
;; implementation, one docstring, one `:arglists`, and a stack frame that names
;; `omkamra.dsp.instance`.

(clojure.core/defmacro ^:private defalias
  "Intern `name` here, holding the function `target` names, and carry its doc
  and arglists, so the facade re-exports rather than wraps.

  The `:arglists` metadata is quoted because a `def` name's metadata is evaluated
  by the compiler — the reason `clojure.core/defn` quotes it too."
  [name target]
  (let [source (meta (resolve target))
        m (cond-> (select-keys source [:doc])
            (:arglists source) (assoc :arglists (list 'quote (:arglists source))))]
    `(def ~(with-meta name m) ~target)))

(defalias process-instance? instance/process-instance?)
(defalias create-instance instance/create-instance)
(defalias destroy! instance/destroy!)
(defalias invoke instance/invoke)
(defalias process! instance/process!)
(defalias process-checked! instance-jvm/process-checked!)
(defalias process-unchecked! instance-jvm/process-unchecked!)
(defalias initialize! instance/initialize!)
(defalias reset! instance/reset!)
