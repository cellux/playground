(ns oben.core.protocols.Eq
  (:refer-clojure :exclude [= !=])
  (:require [oben])
  (:require [oben.core.types :as t])
  (:require [clojure.string :as str]))

(defn method-not-found
  [op & args]
  (ex-info (format "method not found: (%s %s)" op (str/join " " (map t/tid-of args))) {}))

(defmacro define-nary-op
  [op]
  `(do
     (oben/defmulti ~op)
     (defmethod ~op :default
       ([~'x]
        (throw (method-not-found '~op ~'x)))
       ([~'x ~'y]
        (throw (method-not-found '~op ~'x ~'y)))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~op (~op ~'x ~'y) ~'z ~'rest)))))

(define-nary-op =)
(define-nary-op !=)
