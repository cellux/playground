(ns oben.core.types.Fn
  (:require [oben.core.api :as o])
  (:require [midje.sweet :as m]))

(o/define-typeclass Fn [:oben/Value]
  [return-type param-types]
  {:return-type return-type
   :param-types param-types})

(defmethod o/compile-type ::Fn
  [{:keys [return-type param-types]}]
  [:fn
   (o/compile-type return-type)
   (mapv o/compile-type param-types)])
