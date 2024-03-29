(ns oben.core.types.Aggregate
  (:require [oben.core.api :as o])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(derive :oben/Aggregate :oben/Value)

(defmulti valid-key? (fn [t key] (o/tid-of-type t)))
(defmulti get-element-index (fn [t key] (o/tid-of-type t)))
(defmulti get-element-type (fn [t key] (o/tid-of-type t)))

(defn find-innermost-element-type
  [t keys]
  (if (seq keys)
    (let [first-key (first keys)
          _ (assert (valid-key? t first-key))
          element-type (get-element-type t first-key)]
      (recur element-type (next keys)))
    t))

(defn get-element-indices
  [t keys]
  (when (seq keys)
    (let [first-key (first keys)
          _ (assert (valid-key? t first-key))
          element-type (get-element-type t first-key)]
      (cons (get-element-index t first-key)
            (get-element-indices element-type (next keys))))))

(defn get-element-in
  [self keys]
  (let [t (o/type-of self)
        return-type (find-innermost-element-type t keys)
        indices (get-element-indices t keys)]
    (o/make-node return-type
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

(defn get-element
  [self key]
  (get-element-in self [key]))

(defmethod Container/get-in [:oben/Aggregate :oben/HostVector]
  [self keys]
  (get-element-in self keys))

(defmethod Container/get [:oben/Aggregate :oben/Any]
  [self key]
  (get-element self key))

(defn assoc-element-in
  [self keys value]
  (let [t (o/type-of self)
        indices (get-element-indices t keys)]
    (o/make-node t
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

(defn assoc-element
  [self key value]
  (assoc-element-in self [key] value))

(defmethod Container/assoc-in [:oben/Aggregate :oben/HostVector :oben/Value]
  [self keys value]
  (assoc-element-in self keys value))

(defmethod Container/assoc [:oben/Aggregate :oben/Any :oben/Value]
  [self key value]
  (assoc-element self key value))
