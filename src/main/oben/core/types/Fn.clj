(ns oben.core.types.Fn
  (:require [oben.core.types :as t])
  (:require [midje.sweet :as m]))

(t/define-typeclass Fn [::t/Value]
  [return-type param-types]
  {:return-type return-type
   :param-types param-types})

(defmethod t/compile ::Fn
  [{:keys [return-type param-types]}]
  [:fn
   (t/compile return-type)
   (mapv t/compile param-types)])
