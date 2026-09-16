(ns oben.core.protocols.Logical
  (:refer-clojure :exclude [and or not])
  (:require [oben.core.api :as o]))

;; Logical operations are separate from bitwise operations. Implementations
;; are responsible for preserving short-circuit evaluation when the operation
;; has more than one operand.
(o/defmulti and)
(o/defmulti or)
(o/defmulti not)
