(ns oben.lang.core.functions
  (:require [oben.lang.core.types :refer [%void]])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.types :as t])
  (:require [oben.lang.context :as ctx])
  (:require [oben.lang.interfaces :as interfaces])
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

(defmacro define-make-binary-op-compiler-method
  [op tc make-ir]
  `(defmethod interfaces/make-binary-op-compiler [~op ~tc]
     [~'_ ~'lhs ~'rhs]
     (fn [~'ctx]
       (let [~'ins (~make-ir
                    (ctx/compiled ~'ctx ~'lhs)
                    (ctx/compiled ~'ctx ~'rhs)
                    {})]
         (ctx/compile-instruction ~'ctx ~'ins)))))

(define-make-binary-op-compiler-method :add ::t/Int ir/add)
(define-make-binary-op-compiler-method :add ::t/SInt ir/add)
(define-make-binary-op-compiler-method :add ::t/FP ir/fadd)

(define-make-binary-op-compiler-method :sub ::t/Int ir/sub)
(define-make-binary-op-compiler-method :sub ::t/SInt ir/sub)
(define-make-binary-op-compiler-method :sub ::t/FP ir/fsub)

(define-make-binary-op-compiler-method :mul ::t/Int ir/mul)
(define-make-binary-op-compiler-method :mul ::t/SInt ir/mul)
(define-make-binary-op-compiler-method :mul ::t/FP ir/fmul)

(define-make-binary-op-compiler-method :div ::t/Int ir/udiv)
(define-make-binary-op-compiler-method :div ::t/SInt ir/sdiv)
(define-make-binary-op-compiler-method :div ::t/FP ir/fdiv)

(defmacro define-binary-operator
  [op]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([]
        (%nop))
       ([~'x]
        ~'x)
       ([~'x ~'y]
        (let [result-type# (t/get-uber-type (t/type-of ~'x)
                                                    (t/type-of ~'y))
              ~'x (t/cast result-type# ~'x false)
              ~'y (t/cast result-type# ~'y false)]
          (ast/make-node result-type#
            (fn [ctx#]
              (let [ctx# (ctx/compile-nodes ctx# [~'x ~'y])
                    compile# (interfaces/make-binary-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~fname (~fname ~'x ~'y) ~'z ~'rest)))))

(define-binary-operator add)
(define-binary-operator sub)
(define-binary-operator mul)
(define-binary-operator div)
