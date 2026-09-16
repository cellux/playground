(ns oben.core.types.Ptr
  (:require [oben.core.api :as o])
  (:require [oben.core.target :as target])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.protocols.Place :as Place])
  (:require [oben.core.protocols.Algebra :as Algebra])
  (:require [oben.core.types.Number :as Number])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(o/define-typeclass Ptr [:oben/Value]
  [object-type]
  (o/make-type
   (fn [ctx]
     (letfn [(compile-object-type [ctx]
               (ctx/compile-type ctx object-type))
             (save-ir [ctx]
               (ctx/save-ir ctx [:ptr (ctx/compiled-type ctx object-type)]))]
       (-> ctx
           compile-object-type
           save-ir)))
   {:object-type object-type}))

(defmethod o/cast [::Ptr ::Ptr]
  [t node force?]
  (cond (= t (o/type-of node))
        node

        (let [node-object-type (:object-type (meta (o/type-of node)))]
          (and (isa? (o/tid-of-type node-object-type) :oben.core.types.Array/Array)
               (let [element-type (:element-type (meta node-object-type))]
                 (= element-type (:object-type (meta t))))))
        (o/parse `(gep ~node [0 0]))

        :else (throw (ex-info "invalid ptr->ptr cast"
                              {:to-type (meta t)
                               :from-type (meta (o/type-of node))}))))

(defmethod o/cast [::Ptr :oben/HostNil]
  [t node force?]
  (o/make-constant-node
   t nil
   (fn [ctx]
     (letfn [(compile-type [ctx]
               (ctx/compile-type ctx t))
             (save-ir [ctx]
               (ctx/save-ir ctx (ir/const (ctx/compiled-type ctx t) nil)))]
       (-> ctx
           compile-type
           save-ir)))))

(defn ptrtoint
  ([node size]
   (let [node-type (o/type-of node)
         result-size (o/constant->value size)
         result-type (Number/UInt result-size)]
     (if (o/constant-node? node)
       (let [value (o/constant->value node)]
         (if (nil? value)
           (Number/make-constant-number-node result-type 0)
           (throw (ex-info "value of ptr constants must be nil" {:value value}))))
       (o/make-node result-type
         (fn [ctx]
           (let [ctx (ctx/compile-type ctx result-type)
                 ctx (ctx/compile-node ctx node)
                 ins (ir/ptrtoint (ctx/compiled-node ctx node)
                                  (ctx/compiled-type ctx result-type)
                                  {})]
             (ctx/compile-instruction ctx ins)))
         {:class ::ptrtoint}))))
  ([node]
   (ptrtoint node (target/attr :address-size))))

(defmethod o/cast [::Number/UInt ::Ptr]
  [t node force?]
  (let [t-size (:size (meta t))]
    (if (= t-size 1)
      ;; TODO we should create a dedicated Bool type and use that
      ;; instead of special-casing the ptr->i1 conversion
      (o/parse (list '!= (ptrtoint node) 0))
      (ptrtoint node t-size))))

;; inttoptr TODO
;; bitcast TODO

(defn pointer-node?
  [x]
  (and (o/node? x)
       (isa? (o/tid-of-node x) ::Ptr)))

(defmethod o/sizeof* ::Ptr
  [ctx _t]
  (bit-shift-right (ctx/target-attr ctx :address-size) 3))

(defn %deref
  [ptr-node]
  (let [{:keys [object-type]} (meta (o/type-of ptr-node))]
    (o/make-node object-type
      (fn [ctx]
        (letfn [(compile-pointer [ctx]
                  (ctx/compile-node ctx ptr-node))
                (load-object [ctx]
                  (ctx/compile-instruction
                   ctx (ir/load (ctx/compiled-node ctx ptr-node) {})))]
          (-> ctx
              compile-pointer
              load-object)))
      {:class :oben/deref})))

(defmethod Place/load [::Ptr]
  [ptr]
  (%deref ptr))

(defmethod Place/store! [::Ptr :oben/Value]
  [ptr value]
  (let [object-type (:object-type (meta (o/type-of ptr)))
        value (o/cast object-type value false)]
    (o/make-node
     object-type
     (fn [ctx]
       (letfn [(compile-store [ctx]
                 (ctx/compile-instruction
                  ctx
                  (ir/store (ctx/compiled-node ctx value)
                            (ctx/compiled-node ctx ptr)
                            {})))
               (save-ir [ctx]
                 (ctx/save-ir ctx (ctx/compiled-node ctx value)))]
         (-> ctx
             (ctx/compile-node value)
             (ctx/compile-node ptr)
             compile-store
             save-ir)))
     {:class :oben/store!})))

(defmethod Container/get-in [::Ptr :oben/HostVector]
  [ptr ks]
  (Place/load (Container/at-in ptr ks)))

(defmethod Container/get [::Ptr :oben/Value]
  [ptr key]
  (Container/get-in ptr [key]))

(defmethod Container/get [::Ptr :oben/Any]
  [ptr key]
  (Container/get-in ptr [key]))

(defmethod Container/at-in [::Ptr :oben/HostVector]
  [ptr keys]
  (let [{:keys [object-type]} (meta (o/type-of ptr))
        tid (o/tid-of-type object-type)]
    (cond
      (isa? tid :oben/Aggregate)
      (o/parse `(gep ~ptr [0 ~@keys]))

      (= 1 (count keys))
      (o/parse `(gep ~ptr [~(first keys)]))

      :else
      (throw (ex-info "cannot address nested elements of a non-aggregate pointer"
                      {:pointer-type (o/type-of ptr)
                       :keys keys})))))

(defmethod Container/at [::Ptr :oben/Value]
  [ptr key]
  (Container/at-in ptr [key]))

(defmethod Container/at [::Ptr :oben/Any]
  [ptr key]
  (Container/at-in ptr [key]))

(defmethod Container/assoc-in! [::Ptr :oben/HostVector :oben/Value]
  [ptr ks val]
  (Place/store! (Container/at-in ptr ks) val))

(defmethod Container/assoc! [::Ptr :oben/Value :oben/Value]
  [ptr key val]
  (Container/assoc-in! ptr [key] val))

(defmethod Container/assoc! [::Ptr :oben/Any :oben/Value]
  [ptr key val]
  (Container/assoc-in! ptr [key] val))

(defmethod Algebra/+ [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [~offset]))

(defmethod Algebra/- [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [(- ~offset)]))
