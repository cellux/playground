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

(defmethod ast/parse-host-value-as-type [::Array ::t/HostVector]
  [t elems]
  (assert (= (count elems) (:size t)))
  (assert (every? #(ast/parse-host-value-as-type (:element-type t) %) elems))
  (ast/constant t elems))

(defn find-element-type
  [type indices]
  (if (seq indices)
    (find-element-type (:element-type type) (next indices))
    type))

(defn array-index?
  [node]
  (and (ast/constant? node)
       (isa? ::Number/UInt (t/tid-of node))))

(defmethod Container/get-in [::Array ::t/HostVector]
  [self indices]
  (assert (every? array-index? indices))
  (let [atype (t/type-of self)
        return-type (find-element-type atype indices)]
    (ast/make-node return-type
      (fn [ctx]
        (letfn [(compile-indices [ctx]
                  (reduce ctx/compile-node ctx indices))
                (compile-extractvalue [ctx]
                  (ctx/compile-instruction
                   ctx (ir/extractvalue
                        (ctx/compiled ctx self)
                        (mapv #(ctx/compiled ctx %) indices)
                        {})))]
          (-> ctx
              (ctx/compile-node self)
              compile-indices
              compile-extractvalue))))))

(defmethod Container/get [::Array ::Number/UInt]
  [self index]
  (Container/get-in self [index]))

(defmethod Container/assoc-in [::Array ::t/HostVector ::t/Value]
  [self indices value]
  (assert (every? array-index? indices))
  (let [atype (t/type-of self)
        value-type (find-element-type atype indices)
        return-type atype]
    (ast/make-node return-type
      (fn [ctx]
        (letfn [(compile-indices [ctx]
                  (reduce ctx/compile-node ctx indices))
                (compile-value [ctx]
                  (ctx/compile-node ctx value))
                (compile-insertvalue [ctx]
                  (ctx/compile-instruction
                   ctx (ir/insertvalue
                        (ctx/compiled ctx self)
                        (ctx/compiled ctx value)
                        (mapv #(ctx/compiled ctx %) indices)
                        {})))]
          (-> ctx
              (ctx/compile-node self)
              compile-value
              compile-indices
              compile-insertvalue))))))

(defmethod Container/assoc [::Array ::Number/UInt ::t/Value]
  [self index value]
  (Container/assoc-in self [index] value))

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
    (ast/parse-host-value-as-type array-type initializer)))
