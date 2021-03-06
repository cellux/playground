(ns oben.core.protocols.Eq
  (:refer-clojure :exclude [= !=])
  (:require [oben.core.api :as o])
  (:require [clojure.string :as str]))

(defn method-not-found
  [op & args]
  (ex-info (format "method not found: (%s %s)" op (str/join " " (map o/tid-of-value args))) {}))

(defmacro define-nary-op
  [op]
  `(do
     (o/defmulti ~op)
     (defmethod ~op :default
       ([~'x]
        (throw (method-not-found '~op ~'x)))
       ([~'x ~'y]
        (throw (method-not-found '~op ~'x ~'y)))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~op (~op ~'x ~'y) ~'z ~'rest)))))

(define-nary-op =)
(define-nary-op !=)
