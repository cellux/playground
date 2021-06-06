(ns oben.core.types.Array
  (:require [oben.core.types :as t])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Aggregate :as Aggregate])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(t/define-typeclass Array [:oben.core.types/Aggregate]
  [element-type size]
  {:element-type element-type
   :size size})

(defmethod t/compile ::Array
  [{:keys [element-type size]}]
  [:array (t/compile element-type) size])

(defmethod Container/get [::Array ::Number/UInt]
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

(defmethod Aggregate/get-element-type ::Array
  [t key]
  (:element-type t))

(defmethod Aggregate/get-element-index ::Array
  [t key]
  key)

(oben/defmacro %array
  [element-type initializer]
  (let [size (count initializer)
        elt (ast/parse element-type &env)
        array-type (Array elt size)]
    (ast/constant array-type initializer)))
