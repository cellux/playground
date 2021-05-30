(ns oben.core.interfaces
  (:require [oben.core.types :as t]))

(defmulti make-binary-op-compiler
  (fn [op lhs rhs]
    [op (t/tid-of lhs)]))

(defmulti make-compare-op-compiler
  (fn [op lhs rhs]
    [op (t/tid-of lhs)]))
