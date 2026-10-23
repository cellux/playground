(ns omkamra.dsp.registry
  "A definition registry: the portable replacement for Var/`ns-resolve` linking
  (P13d, D82).

  The linker never invokes a referenced Var; it asks a resolver for a descriptor
  by fully-qualified id (D77). On the JVM the default resolver looks the id up in
  a loaded namespace. A browser has no Vars to look in, and a page that compiles
  *source* has definitions that exist only as descriptors, so both worlds hand
  `linker/link` a `:resolve` backed by a registry: a map from id to descriptor,
  seeded with the DSP library and extended by whatever the page captured.

  The library is seeded from the namespaces themselves rather than from a second
  copy of their contents. The library is `.cljc` and compiled by the same
  `dsp/defn` macro the JVM uses, so the descriptors `ns-publics` hands back are
  the ones the JVM links — the equivalence check says so per definition, which is
  why a registry can read them by reflection without the descriptors drifting.

  `ns-publics` is written out once per namespace, with a *literal* symbol,
  because that is what ClojureScript's macro requires: it expands to the compiler's
  own intern list, so there is no runtime namespace to look a name up in. Those
  literals live in `omkamra.dsp.library-registry`, which is the only thing that
  depends on the library; this namespace stays generic, so the compiler can use it
  without the library in reach.")

;; No `:require`: this namespace is the seam, and the library is one consumer of
;; it (`omkamra.dsp.library-registry`), not a dependency of it.

(defn definition?
  "True for a DSP descriptor the linker can resolve: a function or a struct.

  A struct is a definition too — a `:struct` state reference or a record return
  type resolves through the registry exactly as a call does — so both kinds are
  seeded and registered."
  [value]
  (and (map? value) (contains? #{:function :struct} (:dsp/kind value))))

(defn definition-id
  "The fully-qualified id a descriptor is reachable by: its own name when it is
  qualified, else its defining namespace and name."
  [definition]
  (let [definition-name (:name definition)
        ns-name (get-in definition [:source :namespace])]
    (cond
      (namespace definition-name) definition-name
      (some? ns-name) (symbol (str ns-name) (name definition-name))
      :else definition-name)))

(defn definitions-of
  "The DSP descriptors among a namespace's publics, as `{qualified-id descriptor}`.

  `publics` is what `ns-publics` returns: a map from an unqualified name to the
  Var. The id comes from the descriptor's own name and source namespace, so it is
  the id the JVM linker would use, not the referring namespace's."
  [publics]
  (into {}
        (keep (fn [[_ v]]
                (let [value (if (var? v) @v v)]
                  (when (definition? value)
                    [(definition-id value) value]))))
        publics))

(defn register!
  "Add definitions to a registry, keyed by their own fully-qualified ids.

  The last definition of an id wins, so re-evaluating a source replaces its
  definitions rather than accumulating stale ones."
  [registry definitions]
  (swap! registry merge
         (into {} (map (juxt definition-id identity)) definitions))
  registry)

(defn register-one!
  "Add one definition and return it, for a macro that has a descriptor in hand."
  [registry definition]
  (register! registry [definition])
  definition)

(defn seed
  "A registry holding `definitions`, a map of `{qualified-id descriptor}`."
  [definitions]
  (atom (into {} definitions)))

(defn lookup
  "The descriptor registered for `id`, or nil."
  [registry id]
  (get @registry id))

(defn resolver
  "A `linker/link` `:resolve` over a registry: `[canonical-id descriptor]`.

  The registry's keys are already canonical, so the id is returned as found —
  the linker uses it to name the dependency and to detect a duplicate reaching."
  [registry]
  (fn [id]
    (when-let [definition (get @registry id)]
      [id definition])))
