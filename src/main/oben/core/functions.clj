(ns oben.core.functions
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.interfaces :as if])
  (:require [omkamra.llvm.ir :as ir]))

(defn %nop
  [& args]
  (ast/make-node t/%void identity))

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
  `(defmethod if/make-binary-op-compiler [~op ~tc]
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
  [op make-unary-form]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x]
        ~((eval make-unary-form) 'x))
       ([~'x ~'y]
        (let [result-type# (t/get-uber-type (t/type-of ~'x)
                                                    (t/type-of ~'y))
              ~'x (t/cast result-type# ~'x false)
              ~'y (t/cast result-type# ~'y false)]
          (ast/make-node result-type#
            (fn [ctx#]
              (let [ctx# (ctx/compile-nodes ctx# [~'x ~'y])
                    compile# (if/make-binary-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~fname (~fname ~'x ~'y) ~'z ~'rest)))))

(define-binary-operator add identity)
(define-binary-operator sub (fn [sym] `(list '- 0 ~sym)))
(define-binary-operator mul identity)
(define-binary-operator div identity)
