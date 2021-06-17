(ns oben.core.protocols.Iterable
  (:require [oben.core.api :as o]))

(defmulti init #(o/tid-of-value %1))
(defmulti has-next #(o/tid-of-value %1))
(defmulti next #(o/tid-of-value %1))
