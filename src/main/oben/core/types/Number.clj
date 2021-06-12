(ns oben.core.types.Number
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.protocols.Eq :as Eq])
  (:require [oben.core.protocols.Ord :as Ord])
  (:require [oben.core.protocols.Algebra :as Algebra])
  (:require [oben.core.protocols.Bitwise :as Bitwise])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(defn integer-size
  [n]
  (cond
    (<= 0 n 1) 1
    (<= -128 n 255) 8
    (<= -32768 n 65535) 16
    (<= -2147483648 n 4294967295) 32
    (<= -9223372036854775808 n 18446744073709551615N) 64
    :else (throw (ex-info "integer constant too big"
                          {:value n}))))

(m/tabular
 (m/facts
  (m/fact (integer-size ?n) => ?size))
 ?n ?size
 0 1
 1 1
 -1 8
 127 8 -127 8
 128 8 -128 8
 129 8 -129 16
 255 8 -255 16
 256 16 -256 16
 32767 16 -32767 16
 32768 16 -32768 16
 32769 16 -32769 32
 65535 16 -65535 32
 65536 32 -65536 32
 2147483647 32 -2147483647 32
 2147483648 32 -2147483648 32
 2147483649 32 -2147483649 64
 4294967295 32 -4294967295 64
 4294967296 64 -4294967296 64
 9223372036854775807 64
 -9223372036854775807 64
 9223372036854775808 64
 -9223372036854775808 64
 9223372036854775809 64
 -9223372036854775809 (m/throws clojure.lang.ExceptionInfo "integer constant too big")
 18446744073709551615N 64
 18446744073709551616N (m/throws clojure.lang.ExceptionInfo "integer constant too big"))

(defn float-size
  [x]
  (cond
    (= (.floatValue x) x) 32
    (= (.doubleValue x) x) 64
    :else (throw (ex-info "float constant too big"
                          {:value x}))))

(m/facts
 (m/fact (float-size 0.0) => 32)
 (m/fact (float-size 1.0) => 32)
 (m/fact (float-size -1.0) => 32)
 (m/fact (float-size Float/MAX_VALUE) => 32)
 (m/fact (float-size Double/MAX_VALUE) => 64))

(derive ::Number ::t/Value)

(derive ::Int ::Number)

(defmulti resize
  "Returns a numeric type with the typeclass of `t` but with size `size`."
  (fn [t size] (t/tid-of-type t)))

;; UInt

(t/define-typeclass UInt [::Int]
  [size]
  {:size size})

(m/facts
 (m/fact (UInt 32) => {:kind :oben/TYPE :class ::UInt :size 32}))

(defmethod t/compile ::UInt
  [t]
  [:integer (:size t)])

(defmethod resize ::UInt
  [t newsize]
  (UInt newsize))

(t/define-type %u1 (UInt 1))
(t/define-type %u8 (UInt 8))
(t/define-type %u16 (UInt 16))
(t/define-type %u32 (UInt 32))
(t/define-type %u64 (UInt 64))

;; SInt

(t/define-typeclass SInt [::Int]
  [size]
  {:size size})

(defmethod t/compile ::SInt
  [t]
  [:integer (:size t)])

(defmethod resize ::SInt
  [t newsize]
  (SInt newsize))

(t/define-type %s1 (SInt 1))
(t/define-type %s8 (SInt 8))
(t/define-type %s16 (SInt 16))
(t/define-type %s32 (SInt 32))
(t/define-type %s64 (SInt 64))

;; FP

(t/define-typeclass FP [::Number]
  [size]
  {:size size})

(defmethod t/compile ::FP
  [t]
  (case (:size t)
    32 :float
    64 :double))

(defmethod resize ::FP
  [t newsize]
  (FP newsize))

(t/define-type %f32 (FP 32))
(t/define-type %f64 (FP 64))

(defmethod t/get-ubertype [::UInt ::UInt]
  [t1 t2]
  (UInt (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::SInt ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::SInt ::UInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::SInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::UInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(m/facts
 (m/fact (t/ubertype-of (UInt 8) (UInt 32)) => (UInt 32))
 (m/fact (t/ubertype-of (SInt 8) (UInt 32)) => (SInt 32))
 (m/fact (t/ubertype-of (UInt 8) (FP 64)) => (FP 64)))

;; parsing host numbers

(defn make-constant-number-node
  [type host-value]
  (ast/make-constant-node type host-value
                          (fn [ctx]
                            (let [const (ir/const (t/compile type) host-value)]
                              (ctx/save-ir ctx const)))))

(defmethod ast/parse-host-value ::t/HostInteger
  [n]
  (let [type (if (neg? n)
               (SInt (integer-size n))
               (UInt (integer-size n)))]
    (make-constant-number-node type n)))

(defmethod ast/parse-host-value ::t/HostFloat
  [x]
  (let [type (FP (float-size x))]
    (make-constant-number-node type x)))

;; resize ops

(def resize-constant-fns
  {'trunc (fn [value size]
            (bit-and value (- (bit-shift-left 1 size) 1)))
   'zext (fn [value size] value)
   'sext (fn [value size] value)
   'fptrunc (fn [value size]
              (cond (= size 32) (float value)
                    :else (throw (ex-info "cannot fptrunc constant" {:value value}))))
   'fpext (fn [value size] value)})

(defmacro define-resize-op
  [op]
  `(defn ~op
     [~'node ~'size]
     (let [node-type# (t/type-of ~'node)
           result-size# (ast/constant-value ~'size)
           result-type# (resize node-type# result-size#)]
       (if (ast/constant? ~'node)
         (make-constant-number-node result-type#
                                    (~(resize-constant-fns op)
                                     (ast/constant-value ~'node)
                                     result-size#))
         (ast/make-node result-type#
           (fn [ctx#]
             (let [ctx# (ctx/compile-node ctx# ~'node)
                   ins# (~(symbol "omkamra.llvm.ir" (str op))
                         (ctx/compiled-node ctx# ~'node)
                         (t/compile result-type#)
                         {})]
               (ctx/compile-instruction ctx# ins#)))
           {:class ~(keyword (str (ns-name *ns*)) (str op))
            :children #{~'node}})))))

(define-resize-op trunc)
(define-resize-op zext)
(define-resize-op sext)
(define-resize-op fptrunc)
(define-resize-op fpext)

;; conversion ops

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
                       (ctx/compiled-node ctx# ~'node)
                       (t/compile result-type#)
                       {})]
             (ctx/compile-instruction ctx# ins#)))
         {:class ~(keyword (str (ns-name *ns*)) (str op))
          :children #{~'node}}))))

(define-conversion-op fptoui UInt)
(define-conversion-op fptosi SInt)
(define-conversion-op uitofp FP)
(define-conversion-op sitofp FP)

;; (define-conversion-op ptrtoint)
;; (define-conversion-op inttoptr)
;; (define-conversion-op bitcast)

(defmethod t/cast [::UInt ::UInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (integer-size (ast/constant-value node))
                    node-size)]
    (cond
      (= t-size node-size)
      node
      (> t-size node-size)
      (zext node t-size)
      (or (<= real-size t-size) force?)
      (trunc node t-size)
      :else
      (throw (ex-info "rejected narrowing UInt->UInt conversion"
                      {:from node-size :to t-size})))))

(defmethod t/cast [::UInt ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (integer-size (ast/constant-value node))
                    node-size)
        unsigned-type (UInt node-size)
        unsigned-node (vary-meta node assoc :type unsigned-type)]
    (cond
      (= t-size node-size)
      unsigned-node
      (> t-size node-size)
      (zext unsigned-node t-size)
      (or (<= real-size t-size) force?)
      (trunc unsigned-node t-size)
      :else
      (throw (ex-info "rejected narrowing SInt->UInt conversion"
                      {:from node-size :to t-size})))))

(defmethod t/cast [::UInt ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (float-size (ast/constant-value node))
                    node-size)]
    (cond
      (or (>= t-size node-size) (<= real-size t-size) force?)
      (fptoui node t-size)
      :else
      (throw (ex-info "rejected narrowing FP->UInt conversion")))))

(defmethod t/cast [::SInt ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (integer-size (ast/constant-value node))
                    node-size)]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (sext node t-size)
          (or (<= real-size t-size) force?)
          (trunc node t-size)
          :else
          (throw (ex-info "rejected narrowing SInt->SInt conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::SInt ::UInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (integer-size (ast/constant-value node))
                    node-size)
        signed-type (SInt node-size)
        signed-node (vary-meta node assoc :type signed-type)]
    (cond (= t-size node-size)
          signed-node
          (> t-size node-size)
          (sext signed-node t-size)
          (or (<= real-size t-size) force?)
          (trunc signed-node t-size)
          :else
          (throw (ex-info "rejected narrowing SInt->UInt conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::SInt ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (float-size (ast/constant-value node))
                    node-size)]
    (cond (or (>= t-size node-size) (<= real-size t-size) force?)
          (fptosi node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->SInt conversion")))))

(defmethod t/cast [::FP ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))
        real-size (if (ast/constant? node)
                    (float-size (ast/constant-value node))
                    node-size)]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (fpext node t-size)
          (or (<= real-size t-size) force?)
          (fptrunc node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->FP conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::FP ::UInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (uitofp node t-size)))

(defmethod t/cast [::FP ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (sitofp node t-size)))

;; algebraic and bitwise ops

(defmacro define-binary-op
  [op-multifn arg-typeclass make-ir]
  `(letfn [(doit# [~'lhs ~'rhs]
             (let [result-type# (t/ubertype-of (t/type-of ~'lhs)
                                               (t/type-of ~'rhs))
                   ~'lhs (ast/parse (list 'cast result-type# ~'lhs))
                   ~'rhs (ast/parse (list 'cast result-type# ~'rhs))]
               (if (isa? (t/tid-of-type result-type#) ~arg-typeclass)
                 (ast/make-node result-type#
                   (fn [~'ctx]
                     (let [compile-op# (fn [~'ctx]
                                         (let [~'ins (~make-ir
                                                      (ctx/compiled-node ~'ctx ~'lhs)
                                                      (ctx/compiled-node ~'ctx ~'rhs)
                                                      {})]
                                           (ctx/compile-instruction ~'ctx ~'ins)))]
                       (-> ~'ctx
                           (ctx/compile-node ~'lhs)
                           (ctx/compile-node ~'rhs)
                           compile-op#)))
                   {:class :oben/binop
                    :children (set [~'lhs ~'rhs])})
                 (~op-multifn ~'lhs ~'rhs))))]
     (defmethod ~op-multifn [~arg-typeclass ::Number]
       [~'lhs ~'rhs]
       (doit# ~'lhs ~'rhs))))

(define-binary-op Algebra/+ ::Int ir/add)
(define-binary-op Algebra/+ ::FP ir/fadd)

(define-binary-op Algebra/- ::Int ir/sub)
(define-binary-op Algebra/- ::FP ir/fsub)

(define-binary-op Algebra/* ::Int ir/mul)
(define-binary-op Algebra/* ::FP ir/fmul)

(define-binary-op Algebra// ::UInt ir/udiv)
(define-binary-op Algebra// ::SInt ir/sdiv)
(define-binary-op Algebra// ::FP ir/fdiv)

(define-binary-op Algebra/% ::UInt ir/urem)
(define-binary-op Algebra/% ::SInt ir/srem)
(define-binary-op Algebra/% ::FP ir/frem)

(define-binary-op Bitwise/bit-and ::Int ir/and)
(define-binary-op Bitwise/bit-or ::Int ir/or)
(define-binary-op Bitwise/bit-xor ::Int ir/xor)

(define-binary-op Bitwise/bit-shift-left ::Int ir/shl)

(define-binary-op Bitwise/bit-shift-right ::UInt ir/lshr)
(define-binary-op Bitwise/bit-shift-right ::SInt ir/ashr)

(defmethod Algebra/- [::Number]
  [x]
  `(- (s8 0) ~x))

(defmethod Bitwise/bit-not [::Int]
  [x]
  (let [size (:size (t/type-of x))
        mask (if (< size 64)
               (- (bit-shift-left 1 size) 1)
               0xffffffffffffffffN)]
    (ast/parse `(bit-xor ~x ~mask))))

;; comparison ops

(defmacro define-compare-op
  [op-multifn arg-typeclass make-ir pred]
  `(letfn [(doit# [~'lhs ~'rhs]
             (let [ubertype# (t/ubertype-of (t/type-of ~'lhs)
                                            (t/type-of ~'rhs))
                   ~'lhs (ast/parse (list 'cast ubertype# ~'lhs))
                   ~'rhs (ast/parse (list 'cast ubertype# ~'rhs))]
               (if (isa? (t/tid-of-type ubertype#) ~arg-typeclass)
                 (ast/make-node %u1
                   (fn [~'ctx]
                     (let [compile-op# (fn [~'ctx]
                                         (let [~'ins (~make-ir ~pred
                                                      (ctx/compiled-node ~'ctx ~'lhs)
                                                      (ctx/compiled-node ~'ctx ~'rhs)
                                                      {})]
                                           (ctx/compile-instruction ~'ctx ~'ins)))]
                       (-> ~'ctx
                           (ctx/compile-node ~'lhs)
                           (ctx/compile-node ~'rhs)
                           compile-op#)))
                   {:class :oben/binop
                    :children (set [~'lhs ~'rhs])})
                 (~op-multifn ~'lhs ~'rhs))))]
     (defmethod ~op-multifn [~arg-typeclass ::Number]
       [~'lhs ~'rhs]
       (doit# ~'lhs ~'rhs))))

(define-compare-op Eq/= ::Int ir/icmp :eq)
(define-compare-op Eq/= ::FP ir/fcmp :oeq)

(define-compare-op Eq/!= ::Int ir/icmp :ne)
(define-compare-op Eq/!= ::FP ir/fcmp :one)

(define-compare-op Ord/< ::UInt ir/icmp :ult)
(define-compare-op Ord/< ::SInt ir/icmp :slt)
(define-compare-op Ord/< ::FP ir/fcmp :olt)

(define-compare-op Ord/<= ::UInt ir/icmp :ule)
(define-compare-op Ord/<= ::SInt ir/icmp :sle)
(define-compare-op Ord/<= ::FP ir/fcmp :ole)

(define-compare-op Ord/>= ::UInt ir/icmp :uge)
(define-compare-op Ord/>= ::SInt ir/icmp :sge)
(define-compare-op Ord/>= ::FP ir/fcmp :oge)

(define-compare-op Ord/> ::UInt ir/icmp :ugt)
(define-compare-op Ord/> ::SInt ir/icmp :sgt)
(define-compare-op Ord/> ::FP ir/fcmp :ogt)
