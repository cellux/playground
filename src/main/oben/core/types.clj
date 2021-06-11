(ns oben.core.types
  (:refer-clojure :exclude [compile cast])
  (:require [clojure.core :as clj])
  (:require [midje.sweet :as m]))

(defn make-tid
  [name]
  (keyword (str (ns-name *ns*))
           (str (clj/name name))))

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

(defn typeclass?
  [x]
  (and (fn? x)
       (= (:kind (meta x)) :oben/TYPECLASS)))

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

(derive ::HostKeyword ::HostValue)
(derive ::HostVector ::HostValue)
(derive ::HostMap ::HostValue)

(defn host-value?
  [x]
  (or (keyword? x)
      (vector? x)
      (map? x)))

(defn tid-of-host-value
  [x]
  (cond (keyword? x) ::HostKeyword
        (vector? x) ::HostVector
        (map? x) ::HostMap
        :else (throw (ex-info "no tid for host value" {:host-value x}))))

(defn tid-of
  [x]
  (cond (node? x) (tid-of-node x)
        (host-value? x) (tid-of-host-value x)
        :else (throw (ex-info "no tid for value" {:value x}))))

(defmulti compile
  "Compiles an Oben type into the corresponding LLVM type."
  (fn [t] (tid-of-type t)))

(defmulti resize
  "Returns a type with the typeclass of `t` but with size `size`."
  (fn [t size] (tid-of-type t)))

(defmulti cast
  "Returns an AST node which casts `x` to type `t`.
  If the cast cannot be accomplished without information loss, throws
  an error unless `force?` is true."
  (fn [t x force?] [(tid-of-type t) (tid-of x)]))

;; we call this %Void to prevent a clash with java.lang.Void
(define-typeclass %Void [::Any]
  [])

(defmethod compile ::%Void
  [t]
  :void)

(define-type %void (%Void))

(define-typeclass %Unseen [::Any]
  [])

(defmethod compile ::%Unseen
  [t]
  :void)

(define-type %unseen (%Unseen))

(derive ::Value ::Any)

(defn tangible?
  [t]
  (isa? (tid-of-type t) ::Value))

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
