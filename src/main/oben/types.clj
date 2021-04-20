(ns oben.types
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
  (and (map? t) (= (:kind t) :oben/TYPE)))

(def type-of (comp :type meta))
(def typeclass-of (comp :class :type meta))
(def params->typeclasses (comp (partial mapv typeclass-of) vector))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmulti compile (fn [t] (:class t)))
(defmulti resize (fn [t size] (:class t)))
(defmulti cast* (fn [t x] [(:class t) (typeclass-of x)]))

(defn cast
  [t x]
  (if (= (:class t) (typeclass-of x))
    x
    (cast* t x)))

(define-type None [])

;; used as the type of return forms
(define-type Return [])

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
