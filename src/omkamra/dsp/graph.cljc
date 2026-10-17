(ns omkamra.dsp.graph
  "The definition-value substrate: identity, calls, and derivation.

  A combinator is a plain function from definition values to a definition value.
  These are the primitives it needs — the fully-qualified id of a value, its
  parameter names, a call form, and `derived-definition` — and they are public so
  a caller can write its own combinator without reaching into this library.

  A definition value is data, and `derived-definition` keeps it self-contained:
  it carries the wrapped value's interface and records that value in
  `:dsp/dependencies`, so a derived value links even when its callee is an
  anonymous `dsp/fn` with no Var to resolve (D37). The static graph is assembled
  in Clojure and crosses into the compiler once, at `dsp/link` (D38).")

(defn definition-id
  "The fully-qualified id of a definition, as the linker computes it.

  The name is qualified with the definition's `:source :namespace` unless it is
  already qualified."
  [definition]
  (let [name (:name definition)
        namespace (get-in definition [:source :namespace])]
    (if (namespace name)
      name
      (symbol (str namespace) (str name)))))

(defn parameter-symbols
  "The parameter names of a definition, as symbols."
  [definition]
  (mapv #(if (symbol? %) % (:name %)) (:params definition)))

(defn definition-call
  "A call form to `definition` with `args` (the id is already resolved, so the
  linker does not need the caller's alias context)."
  [definition args]
  (apply list (definition-id definition) args))

(defn derived-definition
  "A fresh definition whose body is `body`, derived from `voice`.

  Everything needed to link and normalize the result — parameters, return type,
  the process declaration, compiler options, and source — is carried over from
  `voice` rather than enumerated here, so a combinator never has to track new
  interface fields. The result gets a fresh internal name (`dsp/def` names it
  when a Var is wanted) and records `voice` in `:dsp/dependencies`, so a body
  that calls `voice` links even when `voice` is an anonymous value. Carrying a
  named voice too keeps the two cases identical and costs nothing.

  `overrides` may replace `:params` and `:controls`, for a combinator that
  changes the interface — a fixed-N `poly` voice adds one control per slot."
  ([voice body]
   (derived-definition voice body nil))
  ([voice body overrides]
   (let [{:keys [params controls]} overrides
         id (definition-id voice)]
     (cond-> (assoc voice
                    :name (gensym "dsp-fn-")
                    :body [body]
                    :state nil
                    :resolution {:aliases {} :refers {}}
                    :dsp/dependencies (assoc (:dsp/dependencies voice) id voice))
       params (assoc :params params)
       controls (assoc-in [:process :controls] controls)))))
