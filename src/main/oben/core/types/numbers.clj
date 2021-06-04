(ns oben.core.types.numbers
  (:require [oben.core.types :as t])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(derive ::Number ::t/Value)

(t/define-typeclass Int [::Number]
  [size]
  {:size size})

(m/facts
 (m/fact (Int 32) => {:kind :oben/TYPE :class ::Int :size 32}))

(defmethod t/compile ::Int
  [t]
  [:integer (:size t)])

(defmethod t/resize ::Int
  [t newsize]
  (Int newsize))

(t/define-type %i1 (Int 1))
(t/define-type %i8 (Int 8))
(t/define-type %i16 (Int 16))
(t/define-type %i32 (Int 32))
(t/define-type %i64 (Int 64))

;; SInt

(t/define-typeclass SInt [::Number]
  [size]
  {:size size})

(defmethod t/compile ::SInt
  [t]
  [:integer (:size t)])

(defmethod t/resize ::SInt
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

(defmethod t/resize ::FP
  [t newsize]
  (FP newsize))

(t/define-type %f32 (FP 32))
(t/define-type %f64 (FP 64))

(defmethod t/get-ubertype [::Int ::Int]
  [t1 t2]
  (Int (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::SInt ::SInt]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::SInt ::Int]
  [t1 t2]
  (SInt (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::FP]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::SInt]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(defmethod t/get-ubertype [::FP ::Int]
  [t1 t2]
  (FP (max (:size t1) (:size t2))))

(m/facts
 (m/fact (t/ubertype-of (Int 8) (Int 32)) => (Int 32))
 (m/fact (t/ubertype-of (SInt 8) (Int 32)) => (SInt 32))
 (m/fact (t/ubertype-of (Int 8) (FP 64)) => (FP 64)))

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
             (ctx/compile-instruction ctx# ins#)))
         {:class ~(keyword (str (ns-name *ns*)) (str op))
          :children #{~'node}}))))

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
             (ctx/compile-instruction ctx# ins#)))
         {:class ~(keyword (str (ns-name *ns*)) (str op))
          :children #{~'node}}))))

(define-conversion-op fptoui Int)
(define-conversion-op fptosi SInt)
(define-conversion-op uitofp FP)
(define-conversion-op sitofp FP)

;; (define-conversion-op ptrtoint)
;; (define-conversion-op inttoptr)
;; (define-conversion-op bitcast)

(defmethod t/cast [::Int ::Int]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond
      (= t-size node-size)
      node
      (> t-size node-size)
      (zext node t-size)
      (= t-size 1)
      (ast/parse `(> ~node 0))
      force?
      (trunc node t-size)
      :else
      (throw (ex-info "rejected narrowing Int->Int conversion"
                      {:from node-size :to t-size})))))

(defmethod t/cast [::Int ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond
      (= t-size node-size)
      node
      (> t-size node-size)
      (zext node t-size)
      (= t-size 1)
      (ast/parse `(!= ~node 0))
      force?
      (trunc node t-size)
      :else
      (throw (ex-info "rejected narrowing SInt->Int conversion"
                      {:from node-size :to t-size})))))

(defmethod t/cast [::Int ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond
      (or (>= t-size node-size) force?)
      (fptoui node t-size)
      (= t-size 1)
      (ast/parse `(!= ~node 0.0))
      :else
      (throw (ex-info "rejected narrowing FP->Int conversion")))))

(defmethod t/cast [::SInt ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (sext node t-size)
          force?
          (trunc node t-size)
          :else
          (throw (ex-info "rejected narrowing SInt->SInt conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::SInt ::Int]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (sext node t-size)
          force?
          (trunc node t-size)
          :else
          (throw (ex-info "rejected narrowing SInt->Int conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::SInt ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (or (>= t-size node-size) force?)
          (fptosi node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->SInt conversion")))))

(defmethod t/cast [::FP ::FP]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (cond (= t-size node-size)
          node
          (> t-size node-size)
          (fpext node t-size)
          force?
          (fptrunc node t-size)
          :else
          (throw (ex-info "rejected narrowing FP->FP conversion"
                          {:from node-size :to t-size})))))

(defmethod t/cast [::FP ::Int]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (uitofp node t-size)))

(defmethod t/cast [::FP ::SInt]
  [t node force?]
  (let [t-size (:size t)
        node-size (:size (t/type-of node))]
    (sitofp node t-size)))

(defn integer-size
  [n]
  (cond
    (<= -128 n 255) 8
    (<= -32768 n 65535) 16
    (<= -2147483648 n 4294967295) 32
    (<= -9223372036854775808 n 18446744073709551613) 64
    :else (throw (ex-info "integer constant too big"
                          {:value n}))))

(m/tabular
 (m/facts
  (m/fact (integer-size ?n) => ?size))
 ?n ?size
 0 8
 1 8 -1 8
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
 9223372036854775807 64 -9223372036854775807 64
 9223372036854775808 64 -9223372036854775808 64
 9223372036854775809 64 -9223372036854775809 (m/throws clojure.lang.ExceptionInfo "integer constant too big"))

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

(defn determine-constant-type-for-int
  [x]
  (if (neg? x)
    (SInt (integer-size x))
    (Int (integer-size x))))

(defmethod ast/determine-constant-type java.lang.Byte
  [x]
  (determine-constant-type-for-int x))

(defmethod ast/determine-constant-type java.lang.Short
  [x]
  (determine-constant-type-for-int x))

(defmethod ast/determine-constant-type java.lang.Integer
  [x]
  (determine-constant-type-for-int x))

(defmethod ast/determine-constant-type java.lang.Long
  [x]
  (determine-constant-type-for-int x))

(defn determine-constant-type-for-float
  [x]
  (FP (float-size x)))

(defmethod ast/determine-constant-type java.lang.Float
  [x]
  (determine-constant-type-for-float x))

(defmethod ast/determine-constant-type java.lang.Double
  [x]
  (determine-constant-type-for-float x))
