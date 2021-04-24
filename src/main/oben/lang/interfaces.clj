(ns oben.lang.interfaces
  (:require [oben.lang.types :as t]))

(defmulti make-binary-op-compiler
  (fn [op lhs rhs]
    [op (t/typeclass-of lhs)]))
