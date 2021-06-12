(ns oben.core.types.Ptr
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Container :as Container])
  (:require [oben.core.protocols.Algebra :as Algebra])
  (:require [oben.core.types.Number :as Number])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(t/define-typeclass Ptr [::t/Value]
  [object-type]
  {:object-type object-type})

(defn pointer-type?
  [t]
  (isa? (t/tid-of-type t) ::Ptr))

(defn pointer-node?
  [x]
  (and (ast/node? x) (pointer-type? (t/type-of x))))

(defmethod t/compile ::Ptr
  [{:keys [object-type]}]
  [:ptr (t/compile object-type)])

(defn %deref
  [ptr-node]
  (let [object-type (:object-type (t/type-of ptr-node))]
    (ast/make-node object-type
      (fn [ctx]
        (letfn [(compile-pointer [ctx]
                  (ctx/compile-node ctx ptr-node))
                (load-object [ctx]
                  (ctx/compile-instruction
                   ctx (ir/load (ctx/compiled-node ctx ptr-node) {})))]
          (-> ctx
              compile-pointer
              load-object)))
      {:class :oben/deref
       :children #{ptr-node}})))

(defmethod Container/get-in [::Ptr ::t/HostVector]
  [ptr ks]
  (let [object-type (:object-type (t/type-of ptr))
        tid (t/tid-of-type object-type)]
    (cond (isa? tid :oben.core.types/Aggregate)
          `(deref (gep ~ptr [0 ~@ks]))
          :else `(get-in (deref ~ptr) ~ks))))

(defmethod Container/get [::Ptr ::t/Value]
  [ptr key]
  (Container/get-in ptr [key]))

(defmethod Container/put-in [::Ptr ::t/HostVector ::t/Value]
  [ptr ks val]
  (let [object-type (:object-type (t/type-of ptr))
        tid (t/tid-of-type object-type)]
    (cond (isa? tid :oben.core.types/Aggregate)
          `(set! (gep ~ptr [0 ~@ks]) ~val)
          :else `(put-in (deref ~ptr) ~ks ~val))))

(defmethod Container/put [::Ptr ::t/Value ::t/Value]
  [ptr key val]
  (Container/put-in ptr [key] val))

(defmethod Algebra/+ [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [~offset]))

(defmethod Algebra/- [::Ptr ::Number/Int]
  [ptr offset]
  `(gep ~ptr [(- ~offset)]))
