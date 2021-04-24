(ns oben.lang.core.functions
  (:require [oben.lang.core.types :refer [%void]])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.types :as t])
  (:require [oben.lang.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(defn %nop
  [& args]
  (ast/make-node %void identity))

(defn %do
  ([head & body]
   (if (seq body)
     (ast/make-node (t/type-of (last body))
       (fn [ctx]
         (ctx/compile-nodes ctx (cons head body))))
     head))
  ([]
   (%nop)))

(defn %cast
  [target-type node]
  (if (= target-type (t/type-of node))
    node
    (t/cast target-type node false)))

(defn %cast!
  [target-type node]
  (if (= target-type (t/type-of node))
    node
    (t/cast target-type node true)))
