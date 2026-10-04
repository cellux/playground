(ns omkamra.cgen.linker
  "Dependency discovery and name lowering for cgen definitions.

  A dependency is written as a qualified keyword, for example `::twice`.
  This mirrors pygen and lets a Clojure form refer to a generated C function
  without evaluating that Clojure var while the DSL is being read."
  (:require [clojure.string :as str]))

(defn- dependency-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- resolve-dependency-var [kw]
  (let [ns-sym (symbol (namespace kw))
        var-sym (symbol (name kw))
        v (ns-resolve ns-sym var-sym)]
    (when-not (var? v)
      (throw (ex-info "unable to resolve cgen dependency keyword to a var"
                      {:keyword kw :namespace ns-sym :name var-sym})))
    v))

(defn- cgen-definition [v]
  (let [definition (:cgen/definition (meta v))]
    (when-not (and (map? definition) (= :function (:cgen/kind definition)))
      (throw (ex-info "linked var must be defined with cgen/defn"
                      {:var v :definition definition})))
    definition))

(defn- collect-dependency-keywords [form]
  (letfn [(walk [x [seen ordered]]
            (cond
              (dependency-keyword? x)
              (if (contains? seen x)
                [seen ordered]
                [(conj seen x) (conj ordered x)])

              (seq? x) (reduce (fn [acc item] (walk item acc)) [seen ordered] x)
              (vector? x) (reduce (fn [acc item] (walk item acc)) [seen ordered] x)
              (map? x) (reduce (fn [acc [k v]] (walk v (walk k acc))) [seen ordered] x)
              (set? x) (reduce (fn [acc item] (walk item acc)) [seen ordered] x)
              :else [seen ordered]))]
    (second (walk form [#{} []]))))

(defn- sha1-hex [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-1")]
    (apply str (map #(format "%02x" (bit-and % 0xff))
                    (.digest digest (.getBytes s "UTF-8"))))))

(defn- sanitize-ident-fragment [s]
  (let [sanitized (str/replace s #"[^A-Za-z0-9_]" "_")]
    (if (re-matches #"[A-Za-z_].*" sanitized)
      sanitized
      (str "_" sanitized))))

(defn emitted-symbol [v]
  (let [identity (str (-> v meta :ns ns-name) "/" (-> v meta :name name))]
    (symbol (str "cgen_"
                 (sanitize-ident-fragment (-> v meta :name name))
                 "__"
                 (subs (sha1-hex identity) 0 12)))))

(defn- dependency-symbol [kw]
  (let [v (resolve-dependency-var kw)]
    (cgen-definition v)
    (emitted-symbol v)))

(defn- rewrite-dependencies [form]
  (cond
    (dependency-keyword? form) (dependency-symbol form)
    (seq? form) (apply list (map rewrite-dependencies form))
    (vector? form) (mapv rewrite-dependencies form)
    (map? form) (into (empty form)
                      (map (fn [[k v]] [(rewrite-dependencies k)
                                        (rewrite-dependencies v)]))
                      form)
    (set? form) (into #{} (map rewrite-dependencies form))
    :else form))

(defn- linked-form [name {:keys [params body return-type]}]
  (list* 'def
         (with-meta name {:tag return-type})
         (rewrite-dependencies params)
         (map rewrite-dependencies body)))

(defn- link-root [root-name root-node dependency-vars]
  (let [visited (atom #{})
        visiting (atom #{})
        forms (atom [])]
    (letfn [(emit-var! [v]
              (when-not (contains? @visited v)
                (if (contains? @visiting v)
                  nil
                  (do
                    (swap! visiting conj v)
                    (let [{:keys [params body]} (cgen-definition v)]
                      (doseq [dependency (mapcat collect-dependency-keywords
                                                 (cons params body))]
                        (emit-var! (resolve-dependency-var dependency)))
                      (swap! forms conj
                             (linked-form (emitted-symbol v)
                                          (cgen-definition v))))
                    (swap! visiting disj v)
                    (swap! visited conj v)))))]
      (doseq [v dependency-vars]
        (emit-var! v))
      (swap! forms conj (linked-form root-name root-node))
      {:forms @forms :entry root-name})))

(defn link-entry
  "Returns `{ :forms [...], :entry symbol }` for a cgen function Var.

  All reachable definitions are emitted once. The C emitter emits prototypes,
  so recursive and mutually recursive functions are valid."
  [entry-var]
  (let [root-node (cgen-definition entry-var)
        dependencies (map #(resolve-dependency-var %)
                          (mapcat collect-dependency-keywords
                                  (cons (:params root-node) (:body root-node))))]
    (link-root (emitted-symbol entry-var) root-node dependencies)))

(defn anonymous-symbol
  "Returns a C-safe symbol for an anonymous cgen definition."
  [name]
  (let [name (if (symbol? name) (clojure.core/name name) (str name))]
    (symbol (str "cgen_anon_" (sanitize-ident-fragment name)))))

(defn link-anonymous
  "Links an inline cgen function definition without creating a Clojure Var.

  The definition must contain `:name`, `:params`, `:return-type`, and `:body`."
  [{:keys [name params return-type body] :as definition}]
  (when-not (and (symbol? name)
                 (vector? params)
                 (contains? #{:int64 :double} return-type)
                 (sequential? body))
    (throw (ex-info "invalid anonymous cgen definition"
                    {:definition definition})))
  (let [dependencies (map #(resolve-dependency-var %)
                          (mapcat collect-dependency-keywords
                                  (cons params body)))]
    (link-root (anonymous-symbol name)
               (assoc definition :params (vec params) :body (vec body))
               dependencies)))
