(ns oben.types
  (:refer-clojure :exclude [compile defmulti])
  (:require [clojure.core :as clj])
  (:require [midje.sweet :as m]))

(defmacro define-type
  [name args & body]
  `(def ~name
     (memoize
      (fn [~@args]
        (-> {:kind ::TYPE
             :uses #{}}
            (merge (do ~@body))
            (assoc :class ~(keyword (str (ns-name *ns*))
                                    (str name))))))))

(defn type?
  [t]
  (and (map? t) (= (:kind t) ::TYPE)))

(def type-of (comp :type meta))
(def typeclass-of (comp :class :type meta))
(def params->typeclasses (comp (partial mapv typeclass-of) vector))

(defn has-typeclass?
  [c x]
  (= (typeclass-of x) c))

(defmacro with-type
  [t x]
  `(with-meta ~x {:type ~t}))

(defmacro defmulti
  [name]
  `(clj/defmulti ~name params->typeclasses))

(clj/defmulti compile :class)

(define-type Int
  [size]
  {:size size})

(m/facts
 (m/fact (Int 32) => {:kind ::TYPE :class ::Int :size 32 :uses #{}}))

(defmethod compile ::Int
  [t]
  [:integer (:size t)])

(define-type FP
  [size]
  {:size size})

(defmethod compile ::FP
  [t]
  (case (:size t)
    32 :float
    64 :double))

(define-type Fn
  [return-type param-types]
  {:return-type return-type
   :param-types param-types})

(defmethod compile ::Fn
  [{:keys [return-type param-types]}]
  [:fn
   (compile return-type)
   (mapv compile param-types)])
