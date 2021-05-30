(ns oben.core.protocols.Container
  (:refer-clojure :exclude [get put])
  (:require [oben]))

(oben/defmulti get)
(oben/defmulti put)
