(ns oben.core.types.Aggregate
  (:require [oben.core.api :as o])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(derive :oben/Aggregate :oben/Value)

(defmulti valid-key? (fn [t key] (o/tid-of-type t)))
(defmulti get-element-type (fn [t key] (o/tid-of-type t)))
(defmulti get-element-index (fn [t key] (o/tid-of-type t)))

(defn find-innermost-element-type
  [t indices]
  (if (seq indices)
    (let [first-index (first indices)
          _ (assert (valid-key? t first-index))
          element-type (get-element-type t first-index)]
      (find-innermost-element-type element-type (next indices)))
    t))

(defmethod Container/get-in [:oben/Aggregate :oben/HostVector]
  [self indices]
  (let [atype (o/type-of self)
        return-type (find-innermost-element-type atype indices)]
    (ast/make-node return-type
      (fn [ctx]
        (letfn [(compile-indices [ctx]
                  (reduce ctx/compile-node ctx indices))
                (compile-extractvalue [ctx]
                  (ctx/compile-instruction
                   ctx (ir/extractvalue
                        (ctx/compiled-node ctx self)
                        (mapv #(ctx/compiled-node ctx %) indices)
                        {})))]
          (-> ctx
              (ctx/compile-node self)
              compile-indices
              compile-extractvalue))))))

(defmethod Container/get [:oben/Aggregate :oben/Value]
  [self index]
  (Container/get-in self [index]))

(defmethod Container/assoc-in [:oben/Aggregate :oben/HostVector :oben/Value]
  [self indices value]
  (let [atype (o/type-of self)
        value-type (find-innermost-element-type atype indices)
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
                        (ctx/compiled-node ctx self)
                        (ctx/compiled-node ctx value)
                        (mapv #(ctx/compiled-node ctx %) indices)
                        {})))]
          (-> ctx
              (ctx/compile-node self)
              compile-value
              compile-indices
              compile-insertvalue))))))

(defmethod Container/assoc [:oben/Aggregate :oben/Value :oben/Value]
  [self index value]
  (Container/assoc-in self [index] value))
