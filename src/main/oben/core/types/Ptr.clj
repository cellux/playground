(ns oben.core.types.Ptr
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Container :as Container])
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

(defn pointer-type?
  [t]
  (isa? (o/tid-of-type t) ::Ptr))

(defn pointer-node?
  [x]
  (and (o/node? x) (pointer-type? (o/type-of x))))

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

(defmethod Container/get-in [::Ptr :oben/HostVector]
  [ptr ks]
  (let [{:keys [object-type]} (meta (o/type-of ptr))
        tid (o/tid-of-type object-type)]
    (cond (isa? tid :oben/Aggregate)
          `(deref (gep ~ptr [0 ~@ks]))
          :else `(get-in (deref ~ptr) ~ks))))

(defmethod Container/get [::Ptr :oben/Value]
  [ptr key]
  (Container/get-in ptr [key]))

(defmethod Container/put-in [::Ptr :oben/HostVector :oben/Value]
  [ptr ks val]
  (let [{:keys [object-type]} (meta (o/type-of ptr))
        tid (o/tid-of-type object-type)]
    (cond (isa? tid :oben/Aggregate)
          `(set! (gep ~ptr [0 ~@ks]) ~val)
          :else `(put-in (deref ~ptr) ~ks ~val))))

(defmethod Container/put [::Ptr :oben/Value :oben/Value]
  [ptr key val]
  (Container/put-in ptr [key] val))

(defmethod Algebra/+ [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [~offset]))

(defmethod Algebra/- [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [(- ~offset)]))
