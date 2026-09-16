(ns oben.core.protocols.Place
  (:refer-clojure :exclude [load])
  (:require [oben.core.api :as o]))

;; A Place represents addressable storage. Containers expose access paths via
;; `at`; places provide the primitive read/write operations for those paths.
(o/defmulti load)
(o/defmulti store!)
