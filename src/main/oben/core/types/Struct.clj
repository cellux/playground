(ns oben.core.types.Struct
  (:require [oben.core.api :as o])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types.Aggregate :as Aggregate])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(o/define-typeclass Struct [:oben/Aggregate]
  [fields]
  (let [field-types (mapv o/resolve-type-from-meta fields)
        field-names (mapv (comp keyword o/drop-meta) fields)
        packed? (:packed? (meta fields))]
    (assert (= (count (set field-names)) (count field-names)))
    (o/make-type
     (fn [ctx]
       (letfn [(compile-field-types [ctx]
                 (reduce ctx/compile-type ctx field-types))
               (save-ir [ctx]
                 (ctx/save-ir
                  ctx
                  [(if packed? :packed-struct :struct)
                   (ctx/get-assigned-name ctx)
                   (mapv #(ctx/compiled-type ctx %) field-types)]))]
         (-> ctx
             compile-field-types
             save-ir)))
     {:field-names field-names
      :field-types field-types
      :packed? packed?
      :name->index (zipmap field-names (range))})))

(defmethod o/cast [::Struct :oben/HostVector]
  [t elems force?]
  (let [{:keys [field-names field-types name->index]} (meta t)]
    (assert (= (count elems) (count field-names)))
    (let [casted-elems (mapv #(o/cast %1 %2 false) field-types elems)]
      (ast/make-constant-node
          t elems
          (fn [ctx]
            (letfn [(compile-struct-type [ctx]
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
                  compile-struct-type
                  compile-elems
                  save-ir)))))))

(defmethod o/cast [::Struct :oben/HostMap]
  [t fields force?]
  (let [{:keys [field-names field-types name->index]} (meta t)]
    (assert (= (count fields) (count field-names)))
    (assert (every? name->index (keys fields)))
    (o/cast t (mapv fields field-names) force?)))

(defmethod Aggregate/valid-key? ::Struct
  [t key]
  (and (keyword? key)
       (let [{:keys [name->index]} (meta t)]
         (name->index key))))

(defmethod Aggregate/parse-key ::Struct
  [t key]
  (let [{:keys [field-types name->index]} (meta t)]
    (if-let [field-index (name->index key)]
      (Number/make-constant-number-node Number/%u64 field-index)
      (throw (ex-info "struct field not found" {:t t :key key})))))

(defmethod Aggregate/get-element-type ::Struct
  [t key]
  (let [{:keys [field-types name->index]} (meta t)]
    (if-let [field-index (name->index key)]
      (get (:field-types (meta t)) field-index)
      (throw (ex-info "struct field not found" {:t t :key key})))))

(o/defmacro %struct
  [fields initializer]
  `(cast (Struct ~fields) ~initializer))
