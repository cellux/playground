(ns oben.core.types.ptr
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m])
  (:use [oben.core.types.numbers :only [%i8 %i16 %i32 %i64]]))

(t/define-typeclass Ptr [::t/Value]
  [element-type]
  {:element-type element-type})

(defn pointer-type?
  [t]
  (isa? (t/tid-of-type t) ::Ptr))

(defmethod t/compile ::Ptr
  [{:keys [element-type]}]
  [:ptr (t/compile element-type)])

(defn type*of
  "If `node` is a pointer to a non-pointer type `t`, returns `t`.
  Otherwise returns the type of `node`."
  [node]
  (let [t (t/type-of node)]
    (if (pointer-type? t)
      (let [elt (:element-type t)]
        (if (pointer-type? elt)
          t elt))
      t)))

(defmethod t/get-ubertype [::Ptr ::Ptr]
  [t1 t2]
  nil)

(defmethod t/get-ubertype [::t/Value ::Ptr]
  [t1 t2]
  (let [elt (:element-type t2)]
    (if (pointer-type? elt)
      nil
      (t/ubertype-of t1 elt))))

(m/facts
 (m/fact (t/ubertype-of (Ptr %i8) (Ptr %i32)) => (m/throws "Cannot find übertype"))
 (m/fact (t/ubertype-of %i8 (Ptr %i32)) => %i32)
 (m/fact (t/get-ubertype %i8 (Ptr (Ptr %i32))) => nil)
 (m/fact (t/ubertype-of %i8 (Ptr (Ptr %i32))) => (m/throws "Cannot find übertype"))
 (m/fact (t/get-ubertype (Ptr (Ptr %i32)) %i8) => nil)
 (m/fact (t/ubertype-of (Ptr (Ptr %i32)) %i8) => (m/throws "Cannot find übertype")))

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

(defmethod t/cast [::t/Value ::Ptr]
  [t ptr-node force?]
  (let [node (%deref ptr-node)
        node-type (t/type-of node)]
    (t/cast (t/ubertype-of t node-type) node force?)))
