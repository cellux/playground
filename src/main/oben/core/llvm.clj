(ns oben.core.llvm
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(defmacro define-resize-op
  [op]
  `(defn ~op
     [~'node ~'size]
     (let [node-type# (t/type-of ~'node)
           result-size# (ast/constant-value ~'size)
           result-type# (t/resize node-type# result-size#)]
       (ast/make-node result-type#
         (fn [ctx#]
           (let [ctx# (ctx/compile-node ctx# ~'node)
                 ins# (~(symbol "omkamra.llvm.ir" (str op))
                       (ctx/compiled ctx# ~'node)
                       (t/compile result-type#)
                       {})]
             (ctx/compile-instruction ctx# ins#)))))))

(define-resize-op trunc)
(define-resize-op zext)
(define-resize-op sext)
(define-resize-op fptrunc)
(define-resize-op fpext)

(defmacro define-conversion-op
  [op result-typeclass]
  `(defn ~op
     [~'node ~'size]
     (let [node-type# (t/type-of ~'node)
           result-size# (ast/constant-value ~'size)
           result-type# (~result-typeclass result-size#)]
       (ast/make-node result-type#
         (fn [ctx#]
           (let [ctx# (ctx/compile-node ctx# ~'node)
                 ins# (~(symbol "omkamra.llvm.ir" (str op))
                       (ctx/compiled ctx# ~'node)
                       (t/compile result-type#)
                       {})]
             (ctx/compile-instruction ctx# ins#)))))))

(define-conversion-op fptoui t/Int)
(define-conversion-op fptosi t/SInt)
(define-conversion-op uitofp t/FP)
(define-conversion-op sitofp t/FP)
;; (define-conversion-op ptrtoint)
;; (define-conversion-op inttoptr)
;; (define-conversion-op bitcast)
