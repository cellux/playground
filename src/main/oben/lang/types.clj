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
(def params->typeclasses (comp (partial mapv typeclass-of) vector))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmulti compile (fn [t] (:class t)))
(defmulti resize (fn [t size] (:class t)))
(defmulti cast* (fn [t node] [(:class t) (typeclass-of node)]))

(defn cast
  [t node]
  (if (= t (type-of node))
    node
    (cast* t node)))

(define-type None [])

(defmethod compile ::None
  [t]
  :void)

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

(defmethod cast* [::Int ::Int]
  [t node]
  (let [t-size (:size t)
        node-size (:size (type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          `(zext ~node ~t-size)
          :else
          (throw (ex-info "rejected implicit narrowing conversion")))))

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
