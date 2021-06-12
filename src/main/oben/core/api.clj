(ns oben.core.api
  (:refer-clojure :exclude [cast resolve])
  (:require [clojure.core :as clj]))

(defn make-tid
  [name]
  (keyword (str (ns-name *ns*))
           (str (clojure.core/name name))))

(defmacro define-typeclass
  [name parents args & body]
  (let [tid (make-tid name)]
    `(do (def ~name
           (memoize
            (with-meta
              (fn [~@args]
                (-> (do ~@body)
                    (merge {:kind :oben/TYPE
                            :class ~tid})
                    (vary-meta assoc :tid ~tid)))
              {:kind :oben/TYPECLASS})))
         ~@(for [p parents]
             `(derive ~tid ~p)))))

(defmacro define-type
  ([name type-constructor parents]
   (let [tid (make-tid name)]
     `(let [t# ~type-constructor
            ptid# (:tid (meta t#))]
        (def ~name (vary-meta t# assoc :tid ~tid))
        ~@(for [p parents]
            `(derive ~tid ~p))
        (derive ~tid ptid#))))
  ([name type-constructor]
   `(define-type ~name ~type-constructor [])))

(defn type?
  [t]
  (and (map? t)
       (= (:kind t) :oben/TYPE)))

(def tid-of-type (comp :tid meta))

(defn node?
  [x]
  (and (fn? x)
       (= :oben/NODE (:kind (meta x)))))

(def type-of-node (comp :type meta))
(def type-of type-of-node)

(def tid-of-node (comp :tid meta type-of))

(defn nodeclass-of
  [node]
  (:class (meta node)))

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

(defn tid-of
  [x]
  (cond (node? x) (tid-of-node x)
        (host-value? x) (tid-of-host-value x)
        :else (throw (ex-info "no tid for value" {:value x}))))

(defmulti parse-host-value tid-of-host-value)

(defmulti compile-type
  "Compiles an Oben type into the corresponding LLVM type."
  (fn [t] (tid-of-type t)))

(defmulti cast
  "Returns an AST node which casts `x` to type `t`.
  If the cast cannot be accomplished without information loss, throws
  an error unless `force?` is true."
  (fn [t x force?] [(tid-of-type t) (tid-of x)]))

(derive :oben/Value :oben/Any)

(defn tangible-type?
  [t]
  (isa? (tid-of-type t) :oben/Value))

;; Void

;; we call this %Void to prevent a clash with java.lang.Void
(define-typeclass %Void [:oben/Any]
  [])

(define-type %void (%Void))

(defmethod compile-type ::%Void
  [t]
  :void)

;; Unseen

(define-typeclass %Unseen [:oben/Any]
  [])

(define-type %unseen (%Unseen))

(defmethod compile-type ::%Unseen
  [t]
  :void)

;; get-ubertype

(defmulti get-ubertype
  "Returns the closest type to which both `t1` and `t2` can be cast to."
  (fn [t1 t2] [(tid-of-type t1)
               (tid-of-type t2)]))

(defmethod get-ubertype :default
  [t1 t2]
  nil)

(defn ubertype-of
  ([t]
   t)
  ([t1 t2]
   (cond (= t1 t2) t1
         (= t1 %unseen) t2
         (= t2 %unseen) t1
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

(defn oben-macro?
  [x]
  (and (fn? x)
       (= :oben/MACRO (:kind (meta x)))))

(defn multifn?
  [x]
  (instance? clojure.lang.MultiFn x))

(defn drop-meta
  [x]
  (with-meta x nil))

(defn- find-oben-var
  [sym]
  (let [sym-without-ns (symbol (name sym))
        v (ns-resolve (the-ns 'oben.core) sym-without-ns)]
    (if (= (:ns (meta v)) (the-ns 'oben.core))
      v nil)))

(defn- find-clojure-var
  [sym]
  (let [v (clj/resolve sym)]
    (if (= (:ns (meta v)) (the-ns 'clojure.core))
      (throw (ex-info "oben symbol resolves to a var in clojure.core" {:sym sym}))
      v)))

(defn resolve
  ([sym env]
   (or (get env sym)
       (when-let [v (or (find-oben-var sym)
                        (find-clojure-var sym))]
         (let [value (var-get v)]
           (if (oben-fn? value)
             (:fnode (meta value))
             value)))
       (throw (ex-info "cannot resolve symbol" {:sym sym}))))
  ([sym]
   (resolve sym {})))

(defn resolve-type-from-meta
  [x]
  (if-let [m (meta x)]
    (if-let [tag (:tag m)]
      (if (symbol? tag)
        (resolve tag)
        (eval tag))
      (throw (ex-info "no tag field in metadata of value"
                      {:value x :meta m})))
    (throw (ex-info "no metadata on value"
                    {:value x}))))

(defn all-stars?
  [s]
  (every? (partial = \*) s))

(defn wrap-in-ptr
  [form count]
  (if (zero? count)
    form
    (recur (list 'oben.core.types.Ptr/Ptr form)
           (dec count))))

(defn sanitize-type-form
  [t env]
  (cond (symbol? t)
        (if (contains? env t)
          t
          `(resolve '~t))

        (list? t)
        (let [[op & rest] t]
          (if (and (symbol? op) (all-stars? (str op)))
            (let [pointee (first rest)]
              (assert pointee)
              (assert (nil? (next rest)))
              (sanitize-type-form (wrap-in-ptr pointee (count (str op))) env))
            (map #(sanitize-type-form % env) t)))

        :else t))

(defn quote-tag-if-unbound-symbol
  [m env]
  (if (and (symbol? (:tag m))
           (not (contains? env (:tag m))))
    (update m :tag #(list 'quote %))
    m))

(defn sanitize-typed-forms
  [forms env obj-form? quote-obj? process-all?]
  (letfn [(q [obj]
            `(with-meta '~obj ~(meta obj)))]
    (loop [result []
           forms forms
           m nil]
      (if-let [head (first forms)]
        (if (and (nil? (meta head)) (nil? m))
          (recur result (next forms) {:tag (sanitize-type-form head env)})
          (cond (obj-form? head)
                (let [obj (-> (if m
                                (vary-meta head merge m)
                                head)
                              (vary-meta quote-tag-if-unbound-symbol env))
                      obj (if quote-obj? (q obj) obj)]
                  (if process-all?
                    (recur (conj result obj) (next forms) nil)
                    (vector obj (next forms))))

                (list? head)
                (recur result (next forms) (assoc m :tag (sanitize-type-form head env)))))
        result))))
