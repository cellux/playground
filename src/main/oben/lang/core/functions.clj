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

(def %cast t/cast)

(defmacro define-conversion-op
  [op]
  `(defn ~(symbol (str "%" op))
     [node# size#]
     (let [node-type# (t/type-of node#)
           result-size# (ast/constant-value size#)
           result-type# (t/resize node-type# result-size#)]
       (ast/make-node result-type#
         (fn [ctx#]
           (let [ctx# (ctx/compile-node ctx# node#)
                 ins# (~(symbol "omkamra.llvm.ir" (str op))
                       (ctx/compiled ctx# node#)
                       (t/compile result-type#)
                       {})]
             (ctx/compile-instruction ctx# ins#)))))))

(define-conversion-op zext)
(define-conversion-op trunc)
