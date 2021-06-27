(ns oben.core.api
  (:refer-clojure :exclude [cast resolve defmacro defmulti defmethod])
  (:require [clojure.core :as clj])
  (:require [clojure.string :as str])
  (:require [clojure.walk :as walk]))

(defn make-tid
  [& name-components]
  (keyword (str (ns-name *ns*))
           (str/join "." name-components)))

(defn make-type
  {:style/indent 1}
  [compile-fn & opts]
  (apply vary-meta compile-fn
         merge
         {:kind :oben/TYPE}
         opts))

(defn type?
  [t]
  (and (fn? t)
       (= :oben/TYPE (:kind (meta t)))))

(declare constant-value)

(defn replace-constant-nodes-with-values
  [args]
  (walk/postwalk constant-value args))

(clj/defmacro define-typeclass
  [name parents & fdecl]
  (let [typeclass-tid (make-tid name)]
    `(let [tid-counter# (atom 0)
           constructor# (fn ~@fdecl)
           construct-type# (memoize
                            (fn [args#]
                              (let [tid# (make-tid '~name (swap! tid-counter# inc))]
                                (derive tid# ~typeclass-tid)
                                (-> (apply constructor# args#)
                                    (vary-meta
                                     merge {:class ~typeclass-tid
                                            :tid tid#})))))]
       (def ~name
         (with-meta
           (fn [& args#]
             (construct-type# (replace-constant-nodes-with-values args#)))
           {:kind :oben/TYPECLASS
            :tid ~typeclass-tid}))
       ~@(for [p parents]
           `(derive ~typeclass-tid ~p))
       #'~name)))

(defn typeclass?
  [t]
  (and (fn? t)
       (= :oben/TYPECLASS (:kind (meta t)))))

(def tid-of-typeclass (comp :tid meta))

(clj/defmacro define-type
  ([name constructor-form parents]
   (let [tid (make-tid name)]
     `(let [t# ~constructor-form
            ptid# (:tid (meta t#))]
        (def ~name (vary-meta t# assoc
                              :tid ~tid
                              :name '~name))
        ~@(for [p parents]
            `(derive ~tid ~p))
        (derive ~tid ptid#)
        #'~name)))
  ([name constructor-form]
   `(define-type ~name ~constructor-form [])))

(def tid-of-type (comp :tid meta))

(defn tid-of-type-or-typeclass
  [x]
  (cond (type? x) (tid-of-type x)
        (typeclass? x) (tid-of-typeclass x)
        :else (throw (ex-info "expected type or typeclass" {:arg x}))))

(defn node?
  [x]
  (and (fn? x)
       (= :oben/NODE (:kind (meta x)))))

(def nodeclass-of (comp :class meta))

(def type-of-node (comp :type meta))
(def type-of type-of-node)

(def tid-of-node (comp tid-of-type type-of))

(derive :oben/HostValue :oben/Any)

(derive :oben/HostBoolean :oben/HostValue)

(derive :oben/HostNumber :oben/HostValue)
(derive :oben/HostInteger :oben/HostNumber)
(derive :oben/HostFloat :oben/HostNumber)

(derive :oben/HostKeyword :oben/HostValue)
(derive :oben/HostVector :oben/HostValue)
(derive :oben/HostMap :oben/HostValue)

(defn host-value?
  [x]
  (or
   (boolean? x)
   (number? x)
   (keyword? x)
   (vector? x)
   (map? x)))

(defn tid-of-host-value
  [x]
  (cond
    (boolean? x) :oben/HostBoolean
    (float? x) :oben/HostFloat
    (integer? x) :oben/HostInteger
    (keyword? x) :oben/HostKeyword
    (vector? x) :oben/HostVector
    (map? x) :oben/HostMap
    :else (throw (ex-info "no tid for host value" {:host-value x}))))

(clj/defmulti parse-host-value tid-of-host-value)

(defn tid-of-value
  [x]
  (cond (node? x) (tid-of-node x)
        (host-value? x) (tid-of-host-value x)
        :else (throw (ex-info "no tid for value" {:value x}))))

(clj/defmulti cast
  "Returns an AST node which casts `x` to type `t`.
  If the cast cannot be accomplished without information loss, throws
  an error unless `force?` is true."
  (fn [t x force?] [(tid-of-type t) (tid-of-value x)]))

(derive :oben/Value :oben/Any)

(defn tangible-type?
  [t]
  (isa? (tid-of-type t) :oben/Value))

;; get-ubertype

(clj/defmulti get-ubertype
  "Returns the closest type to which both `t1` and `t2` can be cast to."
  (fn [t1 t2] [(tid-of-type t1)
               (tid-of-type t2)]))

(clj/defmethod get-ubertype :default
  [t1 t2]
  nil)

(defn unseen?
  [t]
  (isa? (tid-of-type t) :oben.core.types.Unseen/%Unseen))

(defn ubertype-of
  ([t]
   t)
  ([t1 t2]
   (cond (= t1 t2) t1
         (unseen? t1) t2
         (unseen? t2) t1
         :else (or (get-ubertype t1 t2)
                   (get-ubertype t2 t1)
                   (throw (ex-info "Cannot find übertype"
                                   {:t1 t1 :t2 t2})))))
  ([t1 t2 t3 & ts]
   (apply ubertype-of (ubertype-of t1 t2) t3 ts)))

(defn constant-node?
  [x]
  (and (node? x)
       (= (nodeclass-of x) :oben/constant)))

(defn constant-value
  [x]
  (if (constant-node? x)
    (:value (meta x))
    x))

(defn fnode?
  [x]
  (and (node? x)
       (= (nodeclass-of x) :oben/fn)))

(defn- oben-fn?
  [x]
  (and (fn? x)
       (= :oben/FN (:kind (meta x)))))

(clj/defmacro defmacro
  [& args]
  `(let [m# (clj/defmacro ~@args)]
     (alter-meta! m# dissoc :macro)
     (alter-var-root m# vary-meta assoc :kind :oben/MACRO)
     m#))

(defn oben-macro?
  [x]
  (and (fn? x)
       (= :oben/MACRO (:kind (meta x)))))

(clj/defmacro defmulti
  [name]
  `(clj/defmulti ~name
     (comp (partial mapv tid-of-value) vector)))

(clj/defmacro defmethod
  [multifn dispatch-val & fn-tail]
  (assert (vector? dispatch-val))
  `(clj/defmethod ~multifn
     ~(mapv (comp tid-of-type-or-typeclass eval) dispatch-val)
     ~@fn-tail))

(defn multifn?
  [x]
  (instance? clojure.lang.MultiFn x))

(defn drop-meta
  [x]
  (with-meta x nil))

(defn- var-in-namespace?
  [v ns]
  (and (var? v) (= (:ns (meta v)) ns)))

(defn- find-oben-var
  [sym]
  (let [ns-oben-core (the-ns 'oben.core)
        sym-without-ns (symbol (name sym))
        v (ns-resolve ns-oben-core sym-without-ns)]
    (if (var-in-namespace? v ns-oben-core)
      v nil)))

(defn- find-clojure-var
  [sym]
  (let [v (clj/resolve sym)]
    (if (var-in-namespace? v (the-ns 'clojure.core))
      (throw (ex-info "oben symbol resolves to a var in clojure.core" {:sym sym}))
      v)))

(defn resolve
  ([sym env]
   (or (get env sym)
       (when-let [v (or (find-oben-var sym)
                        (find-clojure-var sym))]
         (let [value (var-get v)]
           (if (instance? clojure.lang.IMeta value)
             (or (:oben/node (meta value)) value)
             value)))
       (throw (ex-info "cannot resolve symbol" {:sym sym}))))
  ([sym]
   (resolve sym {})))

(defn resolve-type-from-meta
  ([x env]
   (if-let [m (meta x)]
     (if-let [tag (:tag m)]
       tag
       (throw (ex-info "no tag field in metadata of value"
                       {:value x :meta m})))
     (throw (ex-info "no metadata on value"
                     {:value x}))))
  ([x]
   (resolve-type-from-meta x {})))

(defn all-stars?
  [s]
  (every? (partial = \*) s))

(defn wrap-in-ptr
  [form count]
  (if (zero? count)
    form
    (recur (list 'oben.core.types.Ptr/Ptr form)
           (dec count))))

(defn replace-stars-with-ptr
  [form]
  (if (list? form)
    (let [[op & rest] form]
      (if (and (symbol? op) (all-stars? (str op)))
        (let [pointee (first rest)]
          (assert pointee)
          (assert (nil? (next rest)))
          (wrap-in-ptr pointee (count (str op))))
        form))
    form))

(defn move-types-to-tags
  [expr]
  (assert (sequential? expr))
  (loop [result []
         forms (seq expr)
         m nil]
    (if-let [head (first forms)]
      (cond (:tag (meta head))
            (recur (conj result (if (vector? head)
                                  (with-meta
                                    (move-types-to-tags head)
                                    (meta head))
                                  head))
                   (next forms)
                   nil)

            (nil? m)
            (recur result
                   (next forms)
                   {:tag head})

            :else
            (recur result
                   (cons (with-meta head m) (next forms))
                   nil))
      (cond (list? expr) (apply list result)
            (vector? expr) result
            :else (seq result)))))

(defn tagged?
  [x]
  (:tag (meta x)))

(defn quote-all-except-locals
  [form env]
  (let [result (cond (symbol? form)
                     (if (and (not (tagged? form))
                              (contains? env form))
                       form
                       (list 'quote form))

                     (vector? form)
                     (apply list 'vector (map #(quote-all-except-locals % env) form))

                     (map? form)
                     (reduce-kv (fn [result k v]
                                  (assoc result
                                         (quote-all-except-locals k env)
                                         (quote-all-except-locals v env)))
                                {} form)

                     (sequential? form)
                     (apply list 'list (map #(quote-all-except-locals % env) form))

                     :else form)]
    (if (and (instance? clojure.lang.IMeta form) (meta form))
      (if (:tag (meta form))
        (list 'with-meta
              result
              (update (meta form) :tag quote-all-except-locals env))
        result)
      result)))

(defn split-after
  [pred coll]
  (let [[beg end] (split-with (complement pred) coll)]
    (vector (concat beg (list (first end))) (next end))))
