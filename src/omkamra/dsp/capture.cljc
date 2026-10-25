(ns omkamra.dsp.capture
  "Portable reads of `dsp/defn`, `dsp/fn`, `dsp/defstruct` and `dsp/def` forms
  into descriptors.

  The JVM macros in `omkamra.dsp` and the browser's SCI macros (P13d, D82) both
  `dsp/defn`-capture here, so a definition reads the same in either world. What
  differs between them is only the definition site: this namespace never asks the
  ambient namespace anything. A caller supplies the source location (file, line,
  column, namespace) and the resolved name maps — `aliases`, `{alias ns}`, and
  `refers`, `{name qualified-id}`, both already minus `clojure.core` — so the JVM
  macro can read Clojure's namespaces and the SCI macro can read SCI's.

  Everything here is data in and data out: a capture returns a descriptor map,
  never a form. The macro that wraps it in a `def` is the host's own, which is
  what keeps a ClojureScript-compiled capture usable from an SCI macro whose
  expansion is Clojure."
  (:require [clojure.string :as str]
            [omkamra.dsp.source-reader :as source-reader]))

(defn valid-definition-name?
  [name]
  (and (symbol? name)
       (not (str/blank? (clojure.core/name name)))))

(defn source-of
  "The source location of a macro's `&form`, with the definition namespace.

  `:line` and `:column` come from the reader; the namespace is the name the
  caller passes. `:file` deliberately does not: Clojure's reader does not stamp a
  form with one and ClojureScript's does, and a descriptor that identified itself
  differently in each world would not be comparable — which is exactly what
  linking a library definition the page compiled revealed. A namespace and a
  position name a definition well enough; a front end that knows a file can layer
  it on beside the descriptor rather than inside it."
  [form ns-name]
  (let [m (meta form)
        ;; A browser re-reads printed forms, so its reader's own `:line` is not
        ;; the original one; the original travels under its own key (D91).
        line (or (get m source-reader/source-line-key) (:line m))
        column (or (get m source-reader/source-column-key) (:column m))]
    (cond-> {:namespace ns-name}
      (some? line) (assoc :line line)
      (some? column) (assoc :column column))))

(defn fail
  [message source form path data]
  (throw (ex-info message
                  (merge {:dsp/error :invalid-definition
                          :dsp/source source
                          :dsp/path path
                          :dsp/form form}
                         data))))

(defn reference-symbols
  "Every symbol that appears in head position of a form, as a set.

  This is the set of names a body might resolve through an alias or a refer; the
  `:resolution` snapshot keeps only the ones it actually uses."
  [forms]
  (letfn [(walk [form]
            (cond
              (seq? form) (concat (when (symbol? (first form)) [(first form)])
                                  (mapcat walk (rest form)))
              (coll? form) (mapcat walk form)
              :else []))]
    (set (mapcat walk forms))))

(defn referenced-aliases
  "Only the aliases `references` names, as `{alias namespace}`."
  [references aliases]
  (into {}
        (keep (fn [reference]
                (when-let [alias (some-> reference namespace symbol)]
                  (when-let [target (get aliases alias)]
                    [alias target]))))
        references))

(defn referenced-refers
  "Only the unqualified refers `references` names, as `{name qualified-id}`."
  [references refers]
  (into {}
        (keep (fn [reference]
                (when (nil? (namespace reference))
                  (when-let [qualified (get refers reference)]
                    [reference qualified]))))
        references))

(defn resolution
  "The `:resolution` snapshot a body carries: the aliases and refers it uses."
  [references aliases refers]
  {:aliases (referenced-aliases references aliases)
   :refers (referenced-refers references refers)})

(defn box-state-literals
  "Box the float literals of every `:next` state transition, and of nothing else
  in the state declaration: an `:init` is a declared value, typed by the slot it
  initializes (`descriptor/normalize-initializer` coerces it from the declared
  type), and needs no box.

  `source-reader/box-entry` recovers a literal's type from the state map's own
  source text when it carries one (the SCI path) and from the host value
  otherwise (the JVM macro path), so this is one of the two places the worlds
  diverge, and only the SCI path ever sees a text."
  [states]
  (when states
    (mapv (fn [state]
            (source-reader/box-entry state :next))
          states)))

(defn capture-function
  "Build a `:function` descriptor from `dsp/defn`/`dsp/fn` arguments.

  `label` names the form for diagnostics, `name` is the definition's name (or a
  generated one for an anonymous `dsp/fn`), `form` is the macro's `&form` for the
  source location, `ns-name` the defining namespace, and `aliases`/`refers` the
  definition site's resolved names (minus `clojure.core`)."
  [label name form ns-name aliases refers args]
  (let [source (source-of form ns-name)
        [doc args] (if (string? (first args))
                     [(first args) (next args)]
                     [nil args])
        [params & body] args
        explicit-spec? (or (map? params)
                           (and (vector? params)
                                (= 2 (count params))
                                (map? (first params))
                                (contains? (first params) :params)))
        ;; `body-parent` is the form whose children are the body forms, which is
        ;; how `box-body` finds a *bare number* body's text (D92): the body is a
        ;; suffix of that form's children. It is the definition form, except in
        ;; the packed explicit-spec shape, whose body lives inside the parameter
        ;; vector itself.
        [spec body body-parent]
        (cond
          (and (vector? params)
               (= 2 (count params))
               (map? (first params))
               (contains? (first params) :params))
          [(first params) [(second params)] params]

          (map? params)
          [params body form]

          :else
          [{:params params} body form])
        explicit-params? explicit-spec?
        params (:params spec)
        [options body] (if (and (not explicit-params?)
                                (seq body)
                                (map? (first body)))
                         [(first body) (next body)]
                         [{} body])
        spec (merge spec options)
        ;; A callee asks to be copied into its call sites rather than called,
        ;; with `^:inline` metadata (on the name for `dsp/defn`, on the form for
        ;; `dsp/fn`) or an `:inline true` spec key (D47).
        inline? (boolean (or (:inline spec)
                             (:inline (meta name))
                             (:inline (meta form))))]
    (when-not (vector? params)
      (fail (str label " params must be a vector")
            source form [:params] {:name name :params params}))
    (when (and (not explicit-params?) (not (every? symbol? params)))
      (fail (str label " parameters must be symbols")
            source form [:params] {:name name :params params}))
    (when-not (= (count params)
                 (count (set (map #(if (symbol? %) % (:name %)) params))))
      (fail (str label " parameter names must be unique")
            source form [:params] {:name name :params params}))
    (when-not (pos? (count body))
      (fail (str label " requires at least one body expression")
            source form [:body] {:name name :body body}))
    (when (contains? spec :dependencies)
      (fail (str label " infers dependencies from ordinary DSP calls")
            source form [:dependencies] {:name name}))
    (let [references (reference-symbols body)]
      (cond-> {:dsp/kind :function
               :name name
               :params params
               :return-type (or (:return-type spec) :float)
               :state (box-state-literals (:state spec))
               ;; Like `clojure.core/defn`, several body forms are an implicit
               ;; `(do ...)`. A single form is left exactly as written, so a
               ;; captured descriptor's `[:body 0]` is still the author's form.
               :body (let [boxed (source-reader/box-body body-parent body)]
                       (if (= 1 (count body)) boxed [(cons 'do boxed)]))
               :source source
               :resolution (resolution references aliases refers)}
        (some? doc) (assoc :doc doc)
        inline? (assoc :inline true)
        (contains? spec :process)
        (assoc :process (:process spec))
        (contains? spec :options)
        (assoc :options (:options spec))
        (contains? spec :compiler-options)
        (assoc :compiler-options (:compiler-options spec))))))

(defn capture-struct
  "Build a `:struct` descriptor from `dsp/defstruct` arguments."
  [name form ns-name params body]
  (let [source (source-of form ns-name)
        [options body] (if (and (seq body) (map? (first body)))
                         [(first body) (next body)]
                         [{} body])]
    (when-not (vector? params)
      (fail "dsp/defstruct requires a vector of field definitions"
            source form [:params] {:params params}))
    (when (seq body)
      (fail "dsp/defstruct takes field definitions and one optional options map"
            source form [:body] {:body body}))
    (let [fields (mapv (fn [field]
                         (if (symbol? field)
                           {:name field :type :float :init 0.0}
                           field))
                       params)]
      (merge {:dsp/kind :struct
              :name name
              :fields fields
              :source source}
             options))))

(defn carried-definitions
  "Collect the definitions reachable from `entries` through `:dsp/dependencies`.

  A call-emitting transformer returns a definition whose body calls the original,
  which for an anonymous `dsp/fn` value has no Var to resolve. The transformer
  records the original in `:dsp/dependencies`; here those become supplied
  definitions, transitively, so a chain of transforms links.

  `value-of` unwraps an entry to its definition value: the JVM's is `deref` of a
  Var, the browser's is the value itself (P13d)."
  [value-of entries]
  (loop [pending (vec entries)
         found {}]
    (if (empty? pending)
      found
      (let [entry (peek pending)
            value (value-of entry)
            dependencies (when (map? value) (:dsp/dependencies value))
            fresh (when (map? dependencies)
                    (reduce-kv (fn [m id dependency]
                                 (if (contains? found id)
                                   m
                                   (assoc m id dependency)))
                               {}
                               dependencies))]
        (recur (into (pop pending) (vals fresh))
               (merge found fresh))))))
