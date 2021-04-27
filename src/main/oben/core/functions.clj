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
         (reduce ctx/compile-node ctx (cons head body))))
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

(define-make-binary-op-compiler-method :rem ::t/Int ir/urem)
(define-make-binary-op-compiler-method :rem ::t/SInt ir/srem)
(define-make-binary-op-compiler-method :rem ::t/FP ir/frem)

(define-make-binary-op-compiler-method :and ::t/Int ir/and)
(define-make-binary-op-compiler-method :and ::t/SInt ir/and)

(define-make-binary-op-compiler-method :or ::t/Int ir/or)
(define-make-binary-op-compiler-method :or ::t/SInt ir/or)

(define-make-binary-op-compiler-method :xor ::t/Int ir/xor)
(define-make-binary-op-compiler-method :xor ::t/SInt ir/xor)

(defmacro define-binary-op
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
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-binary-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply ~fname (~fname ~'x ~'y) ~'z ~'rest)))))

(define-binary-op add identity)
(define-binary-op sub (fn [sym] `(list '- 0 ~sym)))
(define-binary-op mul identity)
(define-binary-op div identity)
(define-binary-op rem identity)
(define-binary-op and identity)
(define-binary-op or identity)
(define-binary-op xor identity)

(defmacro define-make-compare-op-compiler-method
  [op tc make-ir pred]
  `(defmethod if/make-compare-op-compiler [~op ~tc]
     [~'_ ~'lhs ~'rhs]
     (fn [~'ctx]
       (let [~'ins (~make-ir ~pred
                    (ctx/compiled ~'ctx ~'lhs)
                    (ctx/compiled ~'ctx ~'rhs)
                    {})]
         (ctx/compile-instruction ~'ctx ~'ins)))))

(define-make-compare-op-compiler-method := ::t/Int ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::t/Int ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::t/Int ir/icmp :ult)
(define-make-compare-op-compiler-method :<= ::t/Int ir/icmp :ule)
(define-make-compare-op-compiler-method :>= ::t/Int ir/icmp :uge)
(define-make-compare-op-compiler-method :> ::t/Int ir/icmp :ugt)

(define-make-compare-op-compiler-method := ::t/SInt ir/icmp :eq)
(define-make-compare-op-compiler-method :!= ::t/SInt ir/icmp :ne)
(define-make-compare-op-compiler-method :< ::t/SInt ir/icmp :slt)
(define-make-compare-op-compiler-method :<= ::t/SInt ir/icmp :sle)
(define-make-compare-op-compiler-method :>= ::t/SInt ir/icmp :sge)
(define-make-compare-op-compiler-method :> ::t/SInt ir/icmp :sgt)

(define-make-compare-op-compiler-method := ::t/FP ir/fcmp :oeq)
(define-make-compare-op-compiler-method :!= ::t/FP ir/fcmp :one)
(define-make-compare-op-compiler-method :< ::t/FP ir/fcmp :olt)
(define-make-compare-op-compiler-method :<= ::t/FP ir/fcmp :ole)
(define-make-compare-op-compiler-method :>= ::t/FP ir/fcmp :oge)
(define-make-compare-op-compiler-method :> ::t/FP ir/fcmp :ogt)

(defmacro define-compare-op
  [op]
  (let [fname (symbol (str "%" op))
        op-keyword (keyword op)]
    `(defn ~fname
       ([~'x ~'y]
        (let [uber-type# (t/get-uber-type (t/type-of ~'x)
                                          (t/type-of ~'y))
              ~'x (t/cast uber-type# ~'x false)
              ~'y (t/cast uber-type# ~'y false)]
          (ast/make-node t/%i1
            (fn [ctx#]
              (let [ctx# (ctx/compile-node ctx# ~'x)
                    ctx# (ctx/compile-node ctx# ~'y)
                    compile# (if/make-compare-op-compiler ~op-keyword ~'x ~'y)]
                (compile# ctx#))))))
       ([~'x ~'y ~'z & ~'rest]
        (apply %and (map #(~fname %1 %2)
                         (partition
                          2 1
                          (list* ~'x ~'y ~'z ~'rest))))))))

(define-compare-op =)
(define-compare-op !=)
(define-compare-op <)
(define-compare-op <=)
(define-compare-op >=)
(define-compare-op >)
