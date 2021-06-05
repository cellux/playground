(ns oben.core.protocols.Ord
  (:refer-clojure :exclude [< <= >= >])
  (:require [oben])
  (:require [oben.core.types :as t])
  (:require [clojure.string :as str]))

(defn method-not-found
  [op & args]
  (ex-info (format "method not found: (%s %s)" op (str/join " " (map t/tid-of args))) {}))

(defmacro define-nary-cmp-op
  [op]
  `(do
     (oben/defmulti ~op)
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
