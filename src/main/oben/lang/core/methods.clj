(ns oben.lang.core.methods
  (:require [oben.lang.core.functions :refer [%zext]])
  (:require [oben.lang.types :as t])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(defmethod oben.lang.core/+ [::t/Int ::t/Int]
  [x y]
  (let [[s1 s2] (map (comp :size t/type-of) [x y])
        result-size (max s1 s2)
        result-type (t/Int result-size)
        x (if (< s1 result-size) (%zext x result-size) x)
        y (if (< s2 result-size) (%zext y result-size) y)]
    (ast/make-node result-type
      (fn [ctx]
        (let [ctx (ctx/compile-nodes ctx [x y])
              ins (ir/add (ctx/compiled ctx x)
                          (ctx/compiled ctx y)
                          {})]
          (ctx/compile-instruction ctx ins))))))
