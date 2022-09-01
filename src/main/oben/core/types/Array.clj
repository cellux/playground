(ns oben.core.types.Array
  (:require [oben.core.api :as o])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Aggregate :as Aggregate])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(o/define-typeclass Array [:oben/Aggregate]
  [element-type size & [array-opts]]
  (let [{:keys [name]} array-opts]
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
     {:name name
      :element-type element-type
      :size size})))

(o/defmacro %Array
  [element-type size & [array-opts]]
  (Array (o/parse element-type &env) size array-opts))

(defn array-type?
  [t]
  (isa? (o/tid-of-type t) ::Array))

(defmethod o/sizeof ::Array
  [t]
  (let [{:keys [element-type size]} (meta t)]
    (* (o/alignof element-type) size)))

(defmethod o/alignof ::Array
  [t]
  (o/alignof (:element-type (meta t))))

(defmethod o/cast [::Array :oben/HostVector]
  [t elems force?]
  (let [{:keys [element-type size]} (meta t)]
    (assert (= (count elems) size))
    (let [casted-elems (mapv #(o/cast element-type % false) elems)]
      (assert (every? #(= element-type %) (map o/type-of-node casted-elems)))
      (o/make-constant-node
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

(defmethod Aggregate/valid-key? ::Array
  [t key]
  (and (o/constant-node? key)
       (isa? (o/tid-of-node key) ::Number/UInt)))

(defmethod Aggregate/get-element-type ::Array
  [t key]
  (:element-type (meta t)))

(defn %array
  [element-type initializer]
  (let [size (count initializer)
        array-type (Array element-type size)]
    (o/cast array-type initializer false)))
