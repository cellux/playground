(ns oben.core.protocols.Ord
  (:refer-clojure :exclude [< <= >= >])
  (:require [oben.core.api :as o])
  (:require [clojure.string :as str]))

(defn method-not-found
  [op & args]
  (ex-info (format "method not found: (%s %s)" op (str/join " " (map o/tid-of-value args))) {}))

(defmacro define-nary-cmp-op
  [op]
  `(do
     (o/defmulti ~op)
     (defmethod ~op :default
       ([~'x]
        (throw (method-not-found '~op ~'x)))
       ([~'x ~'y]
        (throw (method-not-found '~op ~'x ~'y)))
       ([~'x ~'y ~'z & ~'rest]
        (bit-and (~op ~'x ~'y) (apply ~op ~'y ~'z ~'rest))))))

(define-nary-cmp-op <)
(define-nary-cmp-op <=)
(define-nary-cmp-op >=)
(define-nary-cmp-op >)
