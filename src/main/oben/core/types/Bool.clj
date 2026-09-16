(ns oben.core.types.Bool
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Bitwise :as Bitwise])
  (:require [omkamra.llvm.ir :as ir]))

;; Bool has the same LLVM representation as i1, but is intentionally distinct
;; from UInt 1 so boolean operations and conversions can dispatch separately.
(o/define-typeclass Bool [:oben/Value]
  []
  (o/make-type #(ctx/save-ir % [:integer 1])))

(def %bool (Bool))

(defmethod o/sizeof* ::Bool
  [_ctx _type]
  1)

(defn make-constant-bool-node
  [value]
  (let [value (boolean value)]
    (o/make-constant-node
     %bool value
     (fn [ctx]
       (letfn [(compile-type [ctx]
                 (ctx/compile-type ctx %bool))
               (save-ir [ctx]
                 (ctx/save-ir ctx (ir/const [:integer 1] value)))]
         (-> ctx
             compile-type
             save-ir))))))

(defmethod o/parse-host-value :oben/HostBoolean
  [value]
  (make-constant-bool-node value))

(defmethod o/cast [::Bool ::Bool]
  [_type node _force?]
  node)

(defn- boolean-binary
  [op lhs rhs]
  (o/make-node %bool
    (fn [ctx]
      (let [ctx (ctx/compile-node ctx lhs)
            ctx (ctx/compile-node ctx rhs)
            ins (op (ctx/compiled-node ctx lhs)
                    (ctx/compiled-node ctx rhs)
                    {})]
        (ctx/compile-instruction ctx ins)))
    {:class :oben/bool-binop}))

(defmethod Bitwise/bit-and [::Bool ::Bool]
  [lhs rhs]
  (boolean-binary ir/and lhs rhs))

(defmethod Bitwise/bit-or [::Bool ::Bool]
  [lhs rhs]
  (boolean-binary ir/or lhs rhs))

(defmethod Bitwise/bit-xor [::Bool ::Bool]
  [lhs rhs]
  (boolean-binary ir/xor lhs rhs))
