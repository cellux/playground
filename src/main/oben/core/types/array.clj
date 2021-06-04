(ns oben.core.types.array
  (:require [oben.core.types :as t])
  (:require [oben.core.types.numbers :as numbers])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(t/define-typeclass Array [::t/Aggregate]
  [element-type size]
  {:element-type element-type
   :size size})

(defmethod t/compile ::Array
  [{:keys [element-type size]}]
  [:array (t/compile element-type) size])

(defmethod Container/get [::Array ::numbers/Int]
  [self index]
  (assert (ast/constant? index))
  (let [atype (t/type-of self)
        elt (:element-type atype)]
    (ast/make-node elt
      (fn [ctx]
        (letfn [(compile-extractvalue [ctx]
                  (ctx/compile-instruction
                   ctx (ir/extractvalue (ctx/compiled ctx self)
                                        [(ctx/compiled ctx index)]
                                        {})))]
          (-> ctx
              (ctx/compile-node self)
              (ctx/compile-node index)
              compile-extractvalue))))))

(oben/defmacro %array
  [element-type initializer]
  (let [size (count initializer)
        elt (ast/parse element-type &env)
        array-type (Array elt size)]
    (ast/constant array-type initializer)))
