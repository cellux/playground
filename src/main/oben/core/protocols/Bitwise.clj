(ns oben.core.protocols.Bitwise
  (:refer-clojure :exclude [bit-and
                            bit-or
                            bit-xor
                            bit-shift-left
                            bit-shift-right
                            bit-not
                            bit-and-not])
  (:require [oben])
  (:require [oben.core.api :as o])
  (:require [clojure.string :as str]))

(defn method-not-found
  [op & args]
  (ex-info (format "method not found: (%s %s)" op (str/join " " (map o/tid-of-value args))) {}))

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

(define-nary-op bit-and)
(define-nary-op bit-or)
(define-nary-op bit-xor)

(oben/defmulti bit-shift-left)
(oben/defmulti bit-shift-right)

(oben/defmulti bit-not)

(defn bit-and-not
  [lhs rhs]
  (bit-and lhs (bit-not rhs)))
