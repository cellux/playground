(ns oben.core.protocols.Container
  (:refer-clojure :exclude [get-in get
                            assoc assoc-in])
  (:require [oben.core.api :as o]))

(o/defmulti get-in)
(o/defmulti get)

(o/defmulti put-in)
(o/defmulti put)

(o/defmulti assoc-in)
(o/defmulti assoc)
