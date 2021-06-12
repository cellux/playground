(ns oben.core.types.Aggregate
  (:require [oben.core.api :as o]))

(derive :oben/Aggregate :oben/Value)

(defmulti get-element-type (fn [t key] (o/tid-of-type t)))
(defmulti get-element-index (fn [t key] (o/tid-of-type t)))
