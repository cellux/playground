(ns oben.core.types.Void
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx]))

(o/define-typeclass %Void [:oben/Any]
  []
  (o/make-type #(ctx/save-ir % :void)))

(def %void (%Void))
