(ns oben.core.protocols.Conditional
  "Value-producing conditional expressions.

  Dispatch includes the condition as well as the two arms.  This lets a
  language-specific implementation apply contextual rules, such as C's
  treatment of otherwise-untyped integer constants in a C conditional."
  (:require [clojure.core :as clj]
            [oben.core.api :as o]))

(clj/defmulti select
  (fn [condition then else]
    [(o/tid-of-value condition)
     (o/tid-of-value then)
     (o/tid-of-value else)]))
