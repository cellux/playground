(ns omkamra.dsp.sci
  "Evaluate user DSP source with SCI (P13d, D82).

  A DSP definition is captured by a macro, not evaluated: `(dsp/defn voice [s]
  (* s 2.0))` becomes a descriptor and the body stays data. So a page (or a JVM)
  that compiles *source* needs a reader and a macroexpander, and nothing else —
  which is exactly what SCI is. This namespace builds a context seeded with the
  DSP library, injects the compiled capture functions (P13d's bridge), evaluates
  a small bootstrap that defines `dsp/defn`/`dsp/fn`/`dsp/defstruct`/`dsp/def` as
  SCI macros, and returns the definitions the source produced.

  The macros are *SCI source*, not host macros. A host macro created with
  `new-macro-var` runs in its compiled namespace: its `*ns*` is not SCI's, and
  SCI exports no alias accessor a host could use on the namespace object it can
  reach. An interpreted macro reads `(ns-name *ns*)`, `(ns-aliases *ns*)` and
  `(ns-refers *ns*)` for free, which is what a definition's `:resolution` needs.
  The capture itself stays compiled — the macro is a thin reader of the
  definition site, and `omkamra.dsp.capture` turns the form into a descriptor the
  same way the JVM macro does, so the two produce identical data."
  (:require [omkamra.dsp.capture :as capture]
            [omkamra.dsp.combinators]
            [omkamra.dsp.readers :as readers]
            [omkamra.dsp.registry :as registry]
            [sci.core :as sci]))

(def ^:private host-namespaces
  "Host namespaces a source may *call*, as namespace values (not descriptors).

  The DSP library is seeded as descriptors, because a library call is linked
  rather than run. A combinator is the other kind: `(comb/unison voice {...})`
  runs while the source is evaluated and builds a derived definition, so what SCI
  needs is the namespace's functions themselves. They are ordinary data
  transformers, so a sandbox can have them the way it has arithmetic."
  {'omkamra.dsp.combinators
   (into {}
         (map (fn [[name var]] [name (deref var)]))
         (ns-publics 'omkamra.dsp.combinators))})

(def ^:private bootstrap
  "The SCI-source macros a DSP definition is written with.

  Defined in SCI, not injected as host macros, so their bodies can read the
  definition site's namespace with SCI's own functions. `aliases` and `refers`
  are ordinary SCI functions called from the macro bodies: during expansion the
  dynamic `*ns*` is the caller's namespace, so they read the *caller's* mappings,
  which is what `omkamra.dsp.capture` resolves against."
  "(ns omkamra.dsp.sci-support)

(def aliases
  (fn []
    (into {}
          (map (fn [[alias ns-obj]] [alias (ns-name ns-obj)]))
          (ns-aliases *ns*))))

(def refers
  (fn []
    (into {}
          (keep (fn [[sym v]]
                  (let [m (meta v)
                        target-ns (:ns m)
                        ns-sym (when target-ns (ns-name target-ns))]
                    (when (and ns-sym (not= 'clojure.core ns-sym))
                      [sym (symbol (str ns-sym) (str (:name m)))]))))
          (ns-refers *ns*))))

(ns omkamra.dsp
  (:require [omkamra.dsp.capture :as capture]
            [omkamra.dsp.sci-support :as support]))

(defmacro defn [name & args]
  (let [descriptor (capture/function \"dsp/defn\" name &form (ns-name *ns*)
                                     (support/aliases) (support/refers) args)]
    (capture/register! descriptor)
    (list 'def name (list 'quote descriptor))))

(defmacro fn [& args]
  (let [descriptor (capture/function \"dsp/fn\" (gensym \"dsp-fn-\") &form (ns-name *ns*)
                                     (support/aliases) (support/refers) args)]
    (capture/register! descriptor)
    (list 'quote descriptor)))

(defmacro defstruct [name params & body]
  (let [descriptor (capture/struct name &form (ns-name *ns*) params body)]
    (capture/register! descriptor)
    (list 'def name (list 'quote descriptor))))

(defmacro def [name definition]
  (let [source (merge (select-keys (meta &form) [:line :column])
                      {:namespace (ns-name *ns*)})]
    `(def ~name
       (let [d# ~definition
             stamped# (assoc d# :name '~name :source '~source)]
         (when-not (and (map? d#) (= :function (:dsp/kind d#)))
           (throw (ex-info \"dsp/def expects a DSP definition value\"
                           {:name '~name :definition d#})))
         (capture/register! stamped#)
         stamped#))))
")

(defn- library-namespaces
  "The library grouped into the `{namespace {name descriptor}}` shape SCI's
  `:namespaces` option takes, so a source can `require` a library namespace."
  [library]
  (reduce-kv (fn [namespaces id definition]
               (assoc-in namespaces
                         [(symbol (namespace id)) (symbol (name id))]
                         definition))
             {}
             library))

(defn evaluate-source
  "Evaluate `source` (a string of forms) and return the definition it names.

  `library` is a map of `{qualified-id descriptor}` — the definitions the source
  may call — and `definition` is the name to return. The result is
  `{:definition descriptor :definitions {id descriptor}}`, where `:definitions`
  is everything the source defined, so a call from the entry to another of the
  source's own definitions links.

  The evaluator is fresh per call: no state carries between two sources, so a
  re-evaluated source replaces its definitions rather than accumulating."
  [source {:keys [definition library]}]
  (let [captured (atom [])
        context (sci/init
                 {:namespaces
                  (merge
                   host-namespaces
                   (library-namespaces library)
                   {'omkamra.dsp.capture
                    {'function capture/capture-function
                     'struct capture/capture-struct
                     ;; The macro's bridge back into the host: each captured
                     ;; descriptor is collected for the caller.
                     'register! (fn [descriptor]
                                  (swap! captured conj descriptor)
                                  descriptor)}})
                  :readers {'dsp/float readers/float-literal}})]
    (sci/eval-string* context bootstrap)
    ;; A source without its own `ns` form lands in `user`, not in the bootstrap's
    ;; last namespace.
    (sci/eval-string* context "(ns user)")
    (sci/eval-string* context source)
    (let [definitions @captured
          value (or (some (fn [candidate]
                            (when (= definition (registry/definition-id candidate))
                              candidate))
                          definitions)
                    (some (fn [candidate]
                            (when (= definition (:name candidate)) candidate))
                          definitions)
                    (try
                      (sci/eval-string* context (str definition))
                      ;; `:default` is ClojureScript's catch-all; the JVM wants a
                      ;; class, and there is no shared spelling of "anything".
                      (catch #?(:clj Exception :cljs :default) error
                        (throw (ex-info (str "the source did not define " definition
                                             "; the capture saw "
                                             (pr-str (mapv registry/definition-id definitions)))
                                        {:definition definition
                                         :captured (mapv registry/definition-id definitions)}
                                        error)))))]
      {:definition value
       :definitions (into {} (map (juxt registry/definition-id identity)) definitions)})))
