(ns oben.lang.types
  (:refer-clojure :exclude [compile cast])
  (:require [midje.sweet :as m]))

(defmacro define-type
  [name args & body]
  `(def ~name
     (memoize
      (fn [~@args]
        (merge (do ~@body)
               {:kind :oben/TYPE}
               {:class ~(keyword (str (ns-name *ns*))
                                 (str (clojure.core/name name)))})))))

(defn type?
  [t]
  (and (map? t)
       (= (:kind t) :oben/TYPE)))

(def type-of (comp :type meta))
(def typeclass-of (comp :class :type meta))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmulti compile (fn [t] (:class t)))
(defmulti resize (fn [t size] (:class t)))
(defmulti cast (fn [t node force?] [(:class t) (typeclass-of node)]))

(define-type None [])

(defmethod compile ::None
  [t]
  :void)

(define-type Int
  [size]
  {:size size})

(m/facts
 (m/fact (Int 32) => {:kind :oben/TYPE :class ::Int :size 32}))

(defmethod compile ::Int
  [t]
  [:integer (:size t)])

(defmethod resize ::Int
  [t newsize]
  (Int newsize))

(define-type SInt
  [size]
  {:size size})

(defmethod compile ::SInt
  [t]
  [:integer (:size t)])

(defmethod resize ::SInt
  [t newsize]
  (SInt newsize))

(define-type FP
  [size]
  {:size size})

(defmethod compile ::FP
  [t]
  (case (:size t)
    32 :float
    64 :double))

(defmethod resize ::FP
  [t newsize]
  (FP newsize))

(define-type Fn
  [return-type param-types]
  {:return-type return-type
   :param-types param-types})

(defmethod compile ::Fn
  [{:keys [return-type param-types]}]
  [:fn
   (compile return-type)
   (mapv compile param-types)])

(defmulti get-uber-type (fn [t1 t2] [(:class t1) (:class t2)]))

(defmethod get-uber-type [::Int ::Int]
  [t1 t2]
  (Int (max (:size t1) (:size t2))))

(defmethod get-uber-type [::Int ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::Int]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::Int ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::Int]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod get-uber-type [::SInt ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::SInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod get-uber-type [::FP ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))
