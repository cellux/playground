(ns oben.lang.core.methods
  (:require [oben.lang.types :as t])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.context :as ctx])
  (:require [oben.lang.llvm :as llvm])
  (:require [omkamra.llvm.ir :as ir]))

(defmethod t/cast [::t/Int ::t/Int]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (llvm/zext node t-size)
          force?
          (llvm/trunc node t-size)
          :else
          (throw (ex-info "rejected narrowing Int->Int conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::t/SInt ::t/SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (llvm/sext node t-size)
          force?
          (llvm/trunc node t-size)
          :else
          (throw (ex-info "rejected narrowing SInt->SInt conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::t/FP ::t/FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (llvm/fpext node t-size)
          force?
          (llvm/fptrunc node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->FP conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::t/FP ::t/Int]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (llvm/uitofp node t-size)))

(defmethod t/cast [::t/FP ::t/SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (llvm/sitofp node t-size)))

(defmethod t/cast [::t/Int ::t/FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (or (>= t-size node-size) force?)
          (llvm/fptoui node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->Int conversion")))))

(defmethod t/cast [::t/SInt ::t/FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (or (>= t-size node-size) force?)
          (llvm/fptosi node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->SInt conversion")))))

(defmethod oben.lang.core/+ [::t/Int ::t/Int]
  [x y]
  (let [[s1 s2] (map (comp :size t/type-of) [x y])
        result-size (max s1 s2)
        result-type (t/Int result-size)
        x (if (< s1 result-size) (llvm/zext x result-size) x)
        y (if (< s2 result-size) (llvm/zext y result-size) y)]
    (ast/make-node result-type
      (fn [ctx]
        (let [ctx (ctx/compile-nodes ctx [x y])
              ins (ir/add (ctx/compiled ctx x)
                          (ctx/compiled ctx y)
                          {})]
          (ctx/compile-instruction ctx ins))))))
