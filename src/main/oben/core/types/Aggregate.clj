(ns oben.core.types.Aggregate
  (:require [oben.core.api :as o])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(derive :oben/Aggregate :oben/Value)

(defmulti valid-key? (fn [t key] (o/tid-of-type t)))
(defmulti parse-key (fn [t key] (o/tid-of-type t)))
(defmulti get-element-type (fn [t key] (o/tid-of-type t)))

(defn aggregate-type?
  [t]
  (isa? (o/tid-of-type t) ::Aggregate))

(defn find-innermost-element-type
  [t keys]
  (if (seq keys)
    (let [first-key (first keys)
          _ (assert (valid-key? t first-key))
          element-type (get-element-type t first-key)]
      (recur element-type (next keys)))
    t))

(defn parse-keys
  [t keys]
  (when (seq keys)
    (let [first-key (first keys)
          _ (assert (valid-key? t first-key))
          element-type (get-element-type t first-key)]
      (cons (parse-key t first-key)
            (parse-keys element-type (next keys))))))

(defmethod Container/get-in [:oben/Aggregate :oben/HostVector]
  [self keys]
  (let [atype (o/type-of self)
        return-type (find-innermost-element-type atype keys)
        indices (parse-keys atype keys)]
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

(defmethod Container/get [:oben/Aggregate :oben/Any]
  [self key]
  (Container/get-in self [key]))

(defmethod Container/assoc-in [:oben/Aggregate :oben/HostVector :oben/Value]
  [self keys value]
  (let [atype (o/type-of self)
        indices (parse-keys atype keys)]
    (o/make-node atype
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

(defmethod Container/assoc [:oben/Aggregate :oben/Any :oben/Value]
  [self key value]
  (Container/assoc-in self [key] value))
