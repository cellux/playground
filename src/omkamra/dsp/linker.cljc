(ns omkamra.dsp.linker
  "Namespace-aware dependency linking for DSP descriptors.

  Linkers consume descriptor data; they never invoke referenced Vars. A linked
  unit contains every reachable definition in dependency-before-dependent
  order, and rewrites ordinary source calls to resolved `(dsp/call id ...)`
  forms for portable IR lowering."
  (:require [omkamra.dsp.builtins :as builtins]
            [omkamra.dsp.descriptor :as descriptor]))

(def ^:private language-forms
  (into '#{if let do set! while break continue buffer-load buffer-store lane
           with-buffers
           state-load state-store store-special array array-load array-store aget aset
           field set-field! int->float float->int
           + - * / = not= < <= > >= and or not
           bit-and bit-or bit-xor bit-not
           bit-shift-left bit-shift-right unsigned-bit-shift-right
           dsp/call}
        builtins/operators))

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

#?(:clj
   (defn- var-id
     [v]
     (symbol (str (-> v meta :ns ns-name)) (str (-> v meta :name)))))

(declare resolve-structs)

#?(:clj
   (defn- var-value
     "The value a Var holds, for a caller that resolved one."
     [v]
     (when-not (var? v)
       (fail "DSP reference does not resolve to a Var" {:reference v}))
     @v))

(defn- definition-value
  "Normalize a definition value: it must be a `:function` descriptor, with its
  struct references inlined. `runtime` is the failure data, so a caller can say
  what it resolved."
  [resolve-id value runtime]
  (when-not (and (map? value) (= :function (:dsp/kind value)))
    (fail "DSP reference must resolve to a value defined with dsp/defn" runtime))
  (descriptor/normalize (resolve-structs resolve-id value)))

#?(:clj
   (defn- jvm-resolver
     "The default resolver: the value of the Var a fully-qualified id names in a
     loaded namespace, as `[canonical-id value]`, or nil.

     The canonical id is the Var's own, so a definition reachable through a refer
     resolves to the namespace that defines it rather than the one that referred
     it. The browser has no Vars to look in, so it hands `link` a `:resolve` of its
     own (P13, D77) — and `synth` already accepts a symbol where the JVM accepts a
     Var."
     [id]
     (when-let [ns-part (namespace id)]
       (when-let [target-ns (find-ns (symbol ns-part))]
         (when-let [v (ns-resolve target-ns (symbol (name id)))]
           (when (var? v) [(var-id v) @v])))))

   :cljs
   (defn- jvm-resolver
     "There is no JVM to look a name up in: a browser always supplies `:resolve`."
     [_id]
     nil))

(defn- id-resolver
  "The resolver to use: the caller's, or the JVM's Var lookup."
  [options]
  (or (:resolve options) jvm-resolver))

(defn- as-definition
  [entry resolve-id]
  (cond
    #?@(:clj
        [(var? entry)
         (let [value (var-value entry)]
           [(var-id entry)
            (definition-value resolve-id value {:var entry :value value})])]
        :cljs [])
    (map? entry) (let [definition (descriptor/normalize (resolve-structs resolve-id entry))]
                   [(definition-id definition) definition])
    :else (fail "DSP linking expects a descriptor map or Var"
                {:entry entry})))

(defn- normalize-definitions
  [definitions resolve-id]
  (when-not (or (nil? definitions) (map? definitions))
    (fail "DSP linker :definitions must be a map" {:definitions definitions}))
  (reduce-kv
   (fn [index supplied-id entry]
     (when-not (symbol? supplied-id)
       (fail "DSP linker definition keys must be symbols"
             {:definition-id supplied-id :definition entry}))
     (let [[actual-id definition] (as-definition entry resolve-id)
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

(defn- state-record-names
  "State names declared as records; a form headed by one is field access,
  not a call to a function of that name."
  [definition]
  (into #{} (comp (filter :fields) (map :name)) (:state definition)))

(defn- callable-form?
  [record-names form]
  (and (seq? form)
       (symbol? (first form))
       (not (contains? language-forms (first form)))
       (not (contains? record-names (first form)))))

(defn- with-buffers-form?
  "`(with-buffers <binding> <call>)`: the third element is an ordinary call whose
  callee may reach the caller's process buffers, because the form binds them."
  [form]
  (and (= 'with-buffers (first form)) (= 3 (count form))))

(defn- call-sites
  "Return call-head symbols in deterministic source order, with paths.

  The small DSP language has no quoted executable forms, so every nested list
  headed by a non-language symbol is an ordinary DSP call candidate. A call
  directly inside `with-buffers` is marked `:buffered? true`: its callee is
  allowed to access process buffers (F2)."
  [definition]
  (let [record-names (state-record-names definition)]
    (letfn [(walk [form path buffered? out]
              (cond
                (seq? form)
                (let [out (if (callable-form? record-names form)
                            (conj out {:symbol (first form) :path path :form form
                                       :buffered? buffered?})
                            out)
                      buffered-child (when (with-buffers-form? form) 2)]
                  (reduce-kv (fn [out index child]
                               (walk child (conj path index)
                                     (= index buffered-child)
                                     out))
                             out
                             (vec form)))

                (vector? form)
                (reduce-kv (fn [out index child]
                             (walk child (conj path index) false out))
                           out form)

                (map? form)
                (reduce (fn [out [key value]]
                          (walk value (conj path key) false out))
                        out form)

                (set? form)
                (reduce (fn [out value] (walk value path false out)) out (sort-by pr-str form))

                :else out))]
      (let [body-calls (reduce-kv (fn [out index form]
                                    (walk form [:body index] false out))
                                  []
                                  (:body definition))]
        (reduce-kv (fn [out index state]
                     (if (some? (:next state))
                       (walk (:next state) [:state index :next] false out)
                       out))
                   body-calls
                   (vec (:state definition)))))))

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

(defn- resolve-struct-var
  [resolve-id id]
  (when-let [[_ value] (resolve-id id)]
    (descriptor/normalize-struct value)))

(def ^:private logical-return-types #{:float :int :boolean :void})

(defn- resolve-return-struct
  "Resolve a struct-typed `:return-type` into `:record` plus the struct's fields,
  so a module can return several typed values (multi-output)."
  [resolve-id definition]
  (let [return-type (:return-type definition)]
    (if (or (nil? return-type) (contains? logical-return-types return-type))
      definition
      (let [reference (if (and (seq? return-type) (= 'quote (first return-type)))
                        (second return-type)
                        return-type)
            id (resolve-symbol-id definition reference)
            resolved (or (resolve-struct-var resolve-id id)
                         (fail "DSP :return-type refers to a missing struct"
                               (merge (source-data definition [:return-type] return-type)
                                      {:reference reference :resolved-id id})))]
        (assoc definition
               :return-type :record
               :return-record {:struct-name id :fields (:fields resolved)})))))

(defn- resolve-structs
  "Inline `:struct <symbol>` state references with the struct's fields.

  A struct reference resolves through the same captured namespace resolution
  used for calls, so an aliased struct from another namespace resolves
  identically. The struct's fully-qualified id is retained as `:struct-name`."
  [resolve-id definition]
  (let [definition (resolve-return-struct resolve-id definition)]
    (if (seq (:state definition))
      (update
       definition
       :state
       (fn [states]
         (mapv (fn [state]
                 (if-let [reference (:struct state)]
                   (let [reference (if (and (seq? reference)
                                            (= 'quote (first reference)))
                                     (second reference)
                                     reference)
                         id (resolve-symbol-id definition reference)
                         resolved (or (resolve-struct-var resolve-id id)
                                      (fail "DSP :struct refers to a missing struct"
                                            (merge (source-data definition [:state] state)
                                                   {:reference reference
                                                    :resolved-id id})))]
                     ;; Struct options are defaults for the state slot.
                     (-> (merge (select-keys resolved [:constant]) state)
                         (dissoc :struct)
                         (assoc :struct-name id
                                :fields (:fields resolved))))
                   state))
               states)))
      definition)))

(defn- resolve-var-definition
  "Resolve a fully-qualified id to `[canonical-id definition]`, or nil."
  [resolve-id id]
  (when-let [[resolved-id value] (resolve-id id)]
    [resolved-id (definition-value resolve-id value {:reference id :value value})]))

(defn- buffer-access?
  [form]
  (cond
    (seq? form) (or (contains? #{'buffer-load 'buffer-store} (first form))
                    (some buffer-access? form))
    (vector? form) (some buffer-access? form)
    (map? form) (some buffer-access? (vals form))
    (set? form) (some buffer-access? form)
    :else false))

(defn- definition-buffer-access?
  [definition]
  (or (some buffer-access? (:body definition))
      (some #(some buffer-access? [(:next %)]) (:state definition))))

(defn- resolve-definition
  [definitions definition call resolve-id]
  (let [id (resolve-symbol-id definition (:symbol call))
        resolved (or (when-let [candidate (get definitions id)] [id candidate])
                     (resolve-var-definition resolve-id id)
                     (fail "DSP call refers to a missing definition"
                           (merge (source-data definition (:path call) (:form call))
                                  {:reference (:symbol call)
                                   :resolved-id id})))]
    ;; A module owns its own state and buffers, but it may not reach into the
    ;; caller's I/O ports: `buffer-load`/`buffer-store` name process buffers.
    ;; A call inside `with-buffers` is the exception (F2, D57): it binds those
    ;; buffers to regions the call site owns, so the callee writes its own output
    ;; rather than the caller's.
    (when (and (definition-buffer-access? (second resolved))
               (not (:buffered? call)))
      (fail "DSP modules cannot access the caller's process buffers"
            (merge (source-data definition (:path call) (:form call))
                   {:reference (:symbol call)
                    :resolved-id (first resolved)})))
    resolved))

(defn- rewrite-calls
  [definition resolve-call]
  (let [record-names (state-record-names definition)]
    (letfn [(rewrite [form path buffered?]
              (cond
                (seq? form)
                (let [buffered-child (when (with-buffers-form? form) 2)
                      items (map-indexed (fn [index item]
                                           (rewrite item (conj path index)
                                                    (= index buffered-child)))
                                         form)]
                  (if (callable-form? record-names form)
                    (let [[id _] (resolve-call {:symbol (first form)
                                                :path path
                                                :form form
                                                :buffered? buffered?})]
                      (list* 'dsp/call id (rest items)))
                    (apply list items)))

                (vector? form) (mapv #(rewrite %1 (conj path %2) false) form (range))
                (map? form) (into (empty form)
                                  (map (fn [[key value]]
                                         [key (rewrite value (conj path key) false)])
                                       form))
                (set? form) (into #{} (map #(rewrite % path false)) form)
                :else form))]
      (-> definition
          (update :body
                  #(mapv (fn [form index] (rewrite form [:body index] false)) % (range)))
          (update :state
                  #(mapv (fn [state index]
                           (if (some? (:next state))
                             (update state :next
                                     (fn [form]
                                       (rewrite form [:state index :next] false)))
                             state))
                         %
                         (range)))))))

(defn link
  "Link a DSP descriptor/Var into a deterministic compilation unit.

  `:definitions` is an optional map of fully-qualified definition IDs to
  descriptor maps or DSP Vars. It is primarily for anonymous descriptors and
  tests. Vars reachable through ordinary namespace resolution are discovered
  automatically."
  ([entry] (link entry {}))
  ([entry {:keys [definitions] :as options}]
   (when-not (every? #{:definitions :resolve} (keys options))
     (fail "DSP linker received unsupported options" {:options options}))
   (let [resolve-id (id-resolver options)
         [entry-id entry-definition] (as-definition entry resolve-id)
         supplied (normalize-definitions definitions resolve-id)
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
               (let [[id resolved] (resolve-definition @definitions definition call
                                                       resolve-id)]
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
