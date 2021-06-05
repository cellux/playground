(ns oben.core.types.ptr
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(t/define-typeclass Ptr [::t/Value]
  [element-type]
  {:element-type element-type})

(defn pointer-type?
  [t]
  (isa? (t/tid-of-type t) ::Ptr))

(defmethod t/compile ::Ptr
  [{:keys [element-type]}]
  [:ptr (t/compile element-type)])

(defn %deref
  [ptr-node]
  (let [elt (:element-type (t/type-of ptr-node))]
    (ast/make-node elt
      (fn [ctx]
        (letfn [(compile-pointer [ctx]
                  (ctx/compile-node ctx ptr-node))
                (load-pointee [ctx]
                  (ctx/compile-instruction
                   ctx (ir/load (ctx/compiled ctx ptr-node) {})))]
          (-> ctx
              compile-pointer
              load-pointee)))
      {:class :oben/deref
       :children #{ptr-node}})))
