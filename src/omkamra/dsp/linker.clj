(ns omkamra.dsp.linker
  "Namespace-aware dependency linking for DSP descriptors.

  Linkers consume descriptor data; they never invoke referenced Vars. A linked
  unit contains every reachable definition in dependency-before-dependent
  order, and rewrites ordinary source calls to resolved `(dsp/call id ...)`
  forms for portable IR lowering."
  (:require [omkamra.dsp.descriptor :as descriptor]))

(def ^:private language-forms
  '#{if let do set! while break continue buffer-load buffer-store
     int->float float->int
     + - * / = not= < <= > >= and or not dsp/call})

(defn- fail
  [message data]
  (throw (ex-info message (merge {:dsp/error :invalid-definition} data))))

(defn- definition-id
  [{:keys [name source]}]
  (let [ns-name (:namespace source)]
    (cond
      (namespace name) name
      (some? ns-name) (symbol (str ns-name) (clojure.core/name name))
      :else name)))

(defn- var-id
  [v]
  (symbol (str (-> v meta :ns ns-name)) (str (-> v meta :name))))

(defn- dsp-var-definition
  [v]
  (when-not (var? v)
    (fail "DSP reference does not resolve to a Var" {:reference v}))
  (let [value @v]
    (when-not (and (map? value) (= :function (:dsp/kind value)))
      (fail "DSP reference must resolve to a value defined with dsp/defn"
            {:var v :value value}))
    (descriptor/normalize value)))

(defn- as-definition
  [entry]
  (cond
    (var? entry) [(var-id entry) (dsp-var-definition entry)]
    (map? entry) (let [definition (descriptor/normalize entry)]
                   [(definition-id definition) definition])
    :else (fail "DSP linking expects a descriptor map or Var"
                {:entry entry})))

(defn- normalize-definitions
  [definitions]
  (when-not (or (nil? definitions) (map? definitions))
    (fail "DSP linker :definitions must be a map" {:definitions definitions}))
  (reduce-kv
   (fn [index supplied-id entry]
     (when-not (symbol? supplied-id)
       (fail "DSP linker definition keys must be symbols"
             {:definition-id supplied-id :definition entry}))
     (let [[actual-id definition] (as-definition entry)
           id (if (namespace supplied-id) supplied-id actual-id)]
       (when (and (namespace supplied-id)
                  (namespace actual-id)
                  (not= supplied-id actual-id))
         (fail "DSP linker definition key does not match its descriptor name"
               {:definition-id supplied-id :descriptor-id actual-id
                :definition definition}))
       (when (contains? index id)
         (fail "DSP linker definitions have duplicate fully-qualified names"
               {:definition-id id :definition definition}))
       (assoc index id definition)))
   {}
   (or definitions {})))

(defn- source-data
  [definition path form]
  {:dsp/source (:source definition)
   :dsp/path path
   :dsp/form form
   :dsp/definition (definition-id definition)})

(defn- callable-form?
  [form]
  (and (seq? form)
       (symbol? (first form))
       (not (contains? language-forms (first form)))))

(defn- call-sites
  "Return call-head symbols in deterministic source order, with paths.

  The small DSP language has no quoted executable forms, so every nested list
  headed by a non-language symbol is an ordinary DSP call candidate."
  [definition]
  (letfn [(walk [form path out]
            (cond
              (seq? form)
              (let [out (if (callable-form? form)
                          (conj out {:symbol (first form) :path path :form form})
                          out)]
                (reduce-kv (fn [out index child]
                             (walk child (conj path index) out))
                           out
                           (vec form)))

              (vector? form)
              (reduce-kv (fn [out index child]
                           (walk child (conj path index) out))
                         out form)

              (map? form)
              (reduce (fn [out [key value]]
                        (walk value (conj path key) out))
                      out form)

              (set? form)
              (reduce (fn [out value] (walk value path out)) out (sort-by pr-str form))

              :else out))]
    (let [body-calls (reduce-kv (fn [out index form]
                                  (walk form [:body index] out))
                                []
                                (:body definition))]
      (reduce-kv (fn [out index state]
                   (if (some? (:next state))
                     (walk (:next state) [:state index :next] out)
                     out))
                 body-calls
                 (vec (:state definition))))))

(defn- resolve-symbol-id
  [definition reference]
  (let [{:keys [aliases refers]} (:resolution definition)
        caller-ns (some-> definition :source :namespace str)
        id (if-let [prefix (namespace reference)]
             (let [target-ns (or (get aliases (symbol prefix)) prefix)]
               (symbol (str target-ns) (name reference)))
             (or (get refers reference)
                 (when caller-ns (symbol caller-ns (name reference)))
                 reference))]
    id))

(defn- resolve-var-definition
  [id]
  (when-let [ns-part (namespace id)]
    (when-let [target-ns (find-ns (symbol ns-part))]
      (when-let [v (ns-resolve target-ns (symbol (name id)))]
        [(var-id v) (dsp-var-definition v)]))))

(defn- buffer-access?
  [form]
  (cond
    (seq? form) (or (contains? #{'buffer-load 'buffer-store} (first form))
                    (some buffer-access? form))
    (vector? form) (some buffer-access? form)
    (map? form) (some buffer-access? (vals form))
    (set? form) (some buffer-access? form)
    :else false))

(defn- pure-definition?
  [definition]
  (and (empty? (:state definition))
       (not-any? buffer-access? (:body definition))))

(defn- resolve-definition
  [definitions definition call]
  (let [id (resolve-symbol-id definition (:symbol call))
        resolved (or (when-let [candidate (get definitions id)] [id candidate])
                     (resolve-var-definition id)
                     (fail "DSP call refers to a missing definition"
                           (merge (source-data definition (:path call) (:form call))
                                  {:reference (:symbol call)
                                   :resolved-id id})))]
    (when-not (pure-definition? (second resolved))
      (fail "DSP calls currently require stateless, buffer-free functions"
            (merge (source-data definition (:path call) (:form call))
                   {:reference (:symbol call)
                    :resolved-id (first resolved)})))
    resolved))

(defn- rewrite-calls
  [definition resolve-call]
  (letfn [(rewrite [form path]
            (cond
              (seq? form)
              (let [items (map-indexed (fn [index item]
                                         (rewrite item (conj path index)))
                                       form)]
                (if (callable-form? form)
                  (let [[id _] (resolve-call {:symbol (first form)
                                              :path path
                                              :form form})]
                    (list* 'dsp/call id (rest items)))
                  (apply list items)))

              (vector? form) (mapv #(rewrite % (conj path %2)) form (range))
              (map? form) (into (empty form)
                                (map (fn [[key value]]
                                       [key (rewrite value (conj path key))])
                                     form))
              (set? form) (into #{} (map #(rewrite % path)) form)
              :else form))]
    (-> definition
        (update :body
                #(mapv (fn [form index] (rewrite form [:body index])) % (range)))
        (update :state
                #(mapv (fn [state index]
                         (if (some? (:next state))
                           (update state :next
                                   (fn [form]
                                     (rewrite form [:state index :next])))
                           state))
                       %
                       (range))))))

(defn link
  "Link a DSP descriptor/Var into a deterministic compilation unit.

  `:definitions` is an optional map of fully-qualified definition IDs to
  descriptor maps or DSP Vars. It is primarily for anonymous descriptors and
  tests. Vars reachable through ordinary namespace resolution are discovered
  automatically."
  ([entry] (link entry {}))
  ([entry {:keys [definitions] :as options}]
   (when-not (every? #{:definitions} (keys options))
     (fail "DSP linker received unsupported options" {:options options}))
   (let [[entry-id entry-definition] (as-definition entry)
         supplied (normalize-definitions definitions)
         supplied (if-let [existing (get supplied entry-id)]
                    (if (= existing entry-definition)
                      supplied
                      (fail "DSP linker definitions duplicate the entry definition"
                            {:definition-id entry-id
                             :entry entry-definition
                             :definition existing}))
                    (assoc supplied entry-id entry-definition))
         definitions (atom supplied)
         graph (atom {})
         order (atom [])
         visiting (atom [])
         visited (atom #{})]
     (letfn [(resolve-call [definition call]
               (let [[id resolved] (resolve-definition @definitions definition call)]
                 (when-let [existing (get @definitions id)]
                   (when-not (= existing resolved)
                     (fail "DSP linker found duplicate fully-qualified names"
                           {:definition-id id
                            :existing existing
                            :definition resolved})))
                 (swap! definitions assoc id resolved)
                 [id resolved]))
             (visit! [id definition]
               (cond
                 (contains? @visited id) nil
                 (some #{id} @visiting)
                 (let [cycle (->> (conj @visiting id)
                                  (drop-while #(not= id %))
                                  vec)]
                   (throw (ex-info "DSP dependencies contain a cycle"
                                   (merge (source-data definition [:body] nil)
                                          {:dsp/error :cyclic-dependency
                                           :dsp/cycle cycle}))))
                 :else
                 (do
                   (swap! visiting conj id)
                   (let [calls (call-sites definition)
                         dependencies (reduce (fn [ids call]
                                                (let [id (first (resolve-call definition call))]
                                                  (if (some #{id} ids)
                                                    ids
                                                    (conj ids id))))
                                              []
                                              calls)
                         rewritten (rewrite-calls definition
                                                  #(resolve-call definition %))]
                     (swap! graph assoc id dependencies)
                     (doseq [dependency dependencies]
                       (visit! dependency (get @definitions dependency)))
                     (swap! definitions assoc id rewritten)
                     (swap! visiting pop)
                     (swap! visited conj id)
                     (swap! order conj id)))))]
       (visit! entry-id entry-definition)
       (let [order @order]
         {:dsp/kind :compilation-unit
          :entry entry-id
          :definitions (into (array-map)
                             (map (fn [id] [id (get @definitions id)]) order))
          :graph (into (array-map)
                       (map (fn [id] [id (vec (get @graph id))]) order))
          :order order})))))
