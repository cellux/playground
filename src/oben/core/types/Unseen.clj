(ns oben.core.types.Unseen
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx]))

(o/define-typeclass %Unseen [:oben/Any]
  []
  (o/make-type #(ctx/save-ir % :void)))

(def %unseen (%Unseen))
