(ns oben.core.protocols.Container
  (:refer-clojure :exclude [get-in get put])
  (:require [oben]))

(oben/defmulti get-in)
(oben/defmulti get)
(oben/defmulti put)
