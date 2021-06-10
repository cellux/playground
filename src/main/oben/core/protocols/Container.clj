(ns oben.core.protocols.Container
  (:refer-clojure :exclude [get-in get
                            assoc assoc-in])
  (:require [oben]))

(oben/defmulti get-in)
(oben/defmulti get)

(oben/defmulti put-in)
(oben/defmulti put)

(oben/defmulti assoc-in)
(oben/defmulti assoc)
