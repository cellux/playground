(ns oben.core.types.array
  (:require [oben.core.types :as t])
  (:require [midje.sweet :as m]))

(t/define-typeclass Array [::t/Value]
  [element-type size]
  {:element-type element-type
   :size size})

(defmethod t/compile ::Array
  [{:keys [element-type size]}]
  [:array (t/compile element-type) size])
