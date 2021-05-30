(ns oben.core.protocols.Iterable
  (:require [oben.core.types :as t]))

(defmulti init #(t/tid-of %1))
(defmulti has-next #(t/tid-of %1))
(defmulti next #(t/tid-of %1))
