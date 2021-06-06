(ns oben.core.types.Aggregate
  (:require [oben.core.types :as t]))

(derive :oben.core.types/Aggregate :oben.core.types/Value)

(defmulti get-element-type (fn [t key] (t/tid-of-type t)))
(defmulti get-element-index (fn [t key] (t/tid-of-type t)))
