(ns oben.core.types.Array
  (:require [oben.core.api :as o])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Aggregate :as Aggregate])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(o/define-typeclass Array [:oben/Aggregate]
  [element-type size]
  (o/make-type
      (fn [ctx]
        (letfn [(compile-element-type [ctx]
                  (ctx/compile-type ctx element-type))
                (save-ir [ctx]
                  (ctx/save-ir
                   ctx
                   [:array
                    (ctx/compiled-type ctx element-type)
                    size]))]
          (-> ctx
              compile-element-type
              save-ir)))
    {:element-type element-type
     :size size}))

(defmethod o/cast [::Array :oben/HostVector]
  [t elems force?]
  (let [{:keys [element-type size]} (meta t)]
    (assert (= (count elems) size))
    (let [casted-elems (mapv #(o/cast element-type % false) elems)]
      (assert (every? #(= element-type %) (map o/type-of-node casted-elems)))
      (ast/make-constant-node
          t elems
          (fn [ctx]
            (letfn [(compile-array-type [ctx]
                      (ctx/compile-type ctx t))
                    (compile-elems [ctx]
                      (reduce ctx/compile-node ctx casted-elems))
                    (save-ir [ctx]
                      (ctx/save-ir
                       ctx
                       (ir/const (ctx/compiled-type ctx t)
                                 (mapv #(ctx/compiled-node ctx %)
                                       casted-elems))))]
              (-> ctx
                  compile-array-type
                  compile-elems
                  save-ir)))))))

(defn find-innermost-element-type
  [t indices]
  (if (seq indices)
    (let [{:keys [element-type]} (meta t)]
      (find-innermost-element-type element-type (next indices)))
    t))

(defn array-index?
  [node]
  (and (o/constant-node? node)
       (isa? (o/tid-of-node node) ::Number/UInt)))

(defmethod Container/get-in [::Array :oben/HostVector]
  [self indices]
  (assert (every? array-index? indices))
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

(defmethod Container/get [::Array ::Number/UInt]
  [self index]
  (Container/get-in self [index]))

(defmethod Container/assoc-in [::Array :oben/HostVector :oben/Value]
  [self indices value]
  (assert (every? array-index? indices))
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

(defmethod Container/assoc [::Array ::Number/UInt :oben/Value]
  [self index value]
  (Container/assoc-in self [index] value))

(defmethod Aggregate/get-element-type ::Array
  [t key]
  (:element-type (meta t)))

(defmethod Aggregate/get-element-index ::Array
  [t key]
  key)

(defn %array
  [element-type initializer]
  (let [size (count initializer)
        array-type (Array element-type size)]
    (o/cast array-type initializer false)))
