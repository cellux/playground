(ns omkamra.pygen.linker
  (:require [clojure.string :as str]))

(defn- dependency-keyword? [x]
  (and (keyword? x)
       (some? (namespace x))))

(defn- resolve-dependency-var [kw]
  (let [ns-sym (symbol (namespace kw))
        var-sym (symbol (name kw))
        v (ns-resolve ns-sym var-sym)]
    (when-not (var? v)
      (throw (ex-info "unable to resolve dependency keyword to a var"
                      {:keyword kw :namespace ns-sym :name var-sym})))
    v))

(defn- py-node-from-var [v]
  (let [value (var-get v)
        kind (:pygen/kind value)]
    (when-not (map? value)
      (throw (ex-info "linked var must hold a py node map"
                      {:var v :value value})))
    (when-not (contains? #{:function :value} kind)
      (throw (ex-info "linked var must declare :pygen/kind as :function or :value"
                      {:var v :value value :kind kind})))
    value))

(defn- collect-dependency-keywords [form]
  (letfn [(walk [x [seen ordered]]
            (cond
              (dependency-keyword? x)
              (if (contains? seen x)
                [seen ordered]
                [(conj seen x) (conj ordered x)])

              (seq? x)
              (reduce (fn [acc item] (walk item acc))
                      [seen ordered]
                      x)

              (vector? x)
              (reduce (fn [acc item] (walk item acc))
                      [seen ordered]
                      x)

              (map? x)
              (reduce (fn [acc [k v]]
                        (let [acc' (walk k acc)]
                          (walk v acc')))
                      [seen ordered]
                      x)

              (set? x)
              (reduce (fn [acc item] (walk item acc))
                      [seen ordered]
                      x)

              :else
              [seen ordered]))]
    (second (walk form [#{} []]))))

(defn- node-dependency-vars [node]
  (let [kind (:pygen/kind node)
        forms (case kind
                :function (cons (:params node) (:body node))
                :value [(:expr node)]
                (throw (ex-info "unsupported py node kind"
                                {:node node :kind kind})))]
    (mapv resolve-dependency-var
          (mapcat collect-dependency-keywords forms))))

(defn- sanitize-ident-fragment [s]
  (-> s
      (str/replace #"[.\-]" "_")))

(defn- sha1-hex [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-1")
        bytes (.digest digest (.getBytes s "UTF-8"))]
    (apply str
           (map (fn [b]
                  (format "%02x" (bit-and b 0xff)))
                bytes))))

(defn- short-var-hash [v]
  (let [identity (str (-> v meta :ns ns-name)
                      "/"
                      (-> v meta :name name))]
    (subs (sha1-hex identity) 0 8)))

(defn- emitted-symbol [v]
  (let [var-name (-> v meta :name name)]
    (symbol (str (sanitize-ident-fragment var-name)
                 "__"
                 (short-var-hash v)))))

(defn- dependency-symbol [kw]
  (let [v (resolve-dependency-var kw)]
    (py-node-from-var v)
    (emitted-symbol v)))

(defn- rewrite-dependency-references [form]
  (cond
    (dependency-keyword? form)
    (dependency-symbol form)

    (seq? form)
    (map rewrite-dependency-references form)

    (vector? form)
    (mapv rewrite-dependency-references form)

    (map? form)
    (into (empty form)
          (map (fn [[k v]]
                 [(rewrite-dependency-references k)
                  (rewrite-dependency-references v)]))
          form)

    (set? form)
    (into #{} (map rewrite-dependency-references form))

    :else
    form))

(defn- linked-form [v node]
  (let [kind (:pygen/kind node)
        self-sym (emitted-symbol v)]
    (case kind
      :function (let [params (rewrite-dependency-references (:params node))
                      body (map rewrite-dependency-references
                                (:body node))]
                  (list* 'def self-sym params body))
      :value (list 'assign! self-sym
                   (rewrite-dependency-references (:expr node)))
      (throw (ex-info "unsupported py node kind"
                      {:var v :node node :kind kind})))))

(defn link-module-forms [forms]
  (when-not (and (sequential? forms)
                 (every? seq? forms))
    (throw (ex-info "link-module-forms expects a sequence of top-level forms"
                    {:forms forms})))
  (let [visited (atom #{})
        visiting (atom #{})
        out (atom [])]
    (letfn [(emit-var! [v]
              (cond
                (contains? @visited v) nil
                (contains? @visiting v) nil
                :else
                (do
                  (swap! visiting conj v)
                  (let [node (py-node-from-var v)]
                    (doseq [dep-var (node-dependency-vars node)]
                      (emit-var! dep-var))
                    (swap! out conj (linked-form v node)))
                  (swap! visiting disj v)
                  (swap! visited conj v))))]
      (doseq [form forms]
        (doseq [kw (collect-dependency-keywords form)]
          (emit-var! (resolve-dependency-var kw)))
        (swap! out conj (rewrite-dependency-references form)))
      @out)))
