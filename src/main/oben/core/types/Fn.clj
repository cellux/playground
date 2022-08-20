(ns oben.core.types.Fn
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [midje.sweet :as m]))

(o/define-typeclass Fn [:oben/Value]
  [return-type param-types]
  (o/make-type
   (fn [ctx]
     (letfn [(compile-return-type [ctx]
               (ctx/compile-type ctx return-type))
             (compile-param-types [ctx]
               (reduce ctx/compile-type ctx param-types))
             (save-ir [ctx]
               (ctx/save-ir
                ctx
                [:fn
                 (ctx/compiled-type ctx return-type)
                 (mapv #(ctx/compiled-type ctx %) param-types)]))]
       (-> ctx
           compile-return-type
           compile-param-types
           save-ir)))
   {:return-type return-type
    :param-types param-types}))

(o/defmacro %Fn
  [return-type param-types]
  (Fn (o/parse return-type &env)
      (o/parse param-types &env)))
