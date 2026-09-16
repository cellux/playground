(ns oben.c
  "C-oriented semantic types and operations.

   Requiring this namespace registers type-directed implementations for C
   values. Core operators remain unchanged for non-C values; plain operators
   acquire C semantics when their operands have C integer types."
  (:refer-clojure :exclude [char double float int long short])
  (:require [clojure.core :as clj]
            [oben.core.api :as o]
            [oben.core.context :as ctx]
            [oben.core.target :as target]
            [oben.core.protocols.Algebra :as Algebra]
            [oben.core.protocols.Bitwise :as Bitwise]
            [oben.core.protocols.Eq :as Eq]
            [oben.core.protocols.Ord :as Ord]
            [oben.core.types.Number :as N]
            [omkamra.llvm.ir :as ir]))

(o/define-typeclass CInt [:oben/Value]
  [bits signed?]
  (o/make-type
   #(ctx/save-ir % [:integer bits])
   {:bits bits
    :signed? signed?}))

(o/define-typeclass CFloat [:oben/Value]
  [bits]
  (o/make-type
   (let [ir-type (case bits
                   32 :float
                   64 :double
                   (throw (ex-info "unsupported C floating-point size"
                                   {:bits bits})))]
     #(ctx/save-ir % ir-type))
   {:bits bits}))

(def i8 (CInt 8 true))
(def u8 (CInt 8 false))
(def i16 (CInt 16 true))
(def u16 (CInt 16 false))
(def i32 (CInt 32 true))
(def u32 (CInt 32 false))
(def i64 (CInt 64 true))
(def u64 (CInt 64 false))

(def f32 (CFloat 32))
(def f64 (CFloat 64))

(defn- attr
  [target name default]
  (get (target/attrs* target) name default))

(o/defportable char
  [target]
  (CInt (attr target :c-char-size 8)
        (attr target :c-char-signed? true)))

(o/defportable short
  [target]
  (CInt (attr target :c-short-size 16) true))

(o/defportable ushort
  [target]
  (CInt (attr target :c-short-size 16) false))

(o/defportable int
  [target]
  (CInt (attr target :c-int-size 32) true))

(o/defportable uint
  [target]
  (CInt (attr target :c-int-size 32) false))

(o/defportable long
  [target]
  (CInt (attr target :c-long-size 64) true))

(o/defportable ulong
  [target]
  (CInt (attr target :c-long-size 64) false))

(o/defportable float
  [target]
  (CFloat (attr target :c-float-size 32)))

(o/defportable double
  [target]
  (CFloat (attr target :c-double-size 64)))

(defmethod o/sizeof* ::CInt
  [_ctx type]
  (quot (:bits (meta type)) 8))

(defmethod o/sizeof* ::CFloat
  [_ctx type]
  (quot (:bits (meta type)) 8))

(defn int32
  "Creates a C-style signed 32-bit integer value."
  [value]
  (o/cast i32 value false))

(defn uint32
  "Creates a C-style unsigned 32-bit integer value."
  [value]
  (o/cast u32 value false))

(defn int64
  "Creates a C-style signed 64-bit integer value."
  [value]
  (o/cast i64 value false))

(defn uint64
  "Creates a C-style unsigned 64-bit integer value."
  [value]
  (o/cast u64 value false))

(defn float32
  "Creates a C-style 32-bit floating-point value."
  [value]
  (o/cast f32 value false))

(defn float64
  "Creates a C-style 64-bit floating-point value."
  [value]
  (o/cast f64 value false))

(defn- normalize-constant
  [type value]
  (let [{:keys [bits signed?]} (meta type)
        modulus (bit-shift-left 1 bits)
        value (mod value modulus)]
    (if (and signed? (>= value (bit-shift-left 1 (dec bits))))
      (- value modulus)
      value)))

(defn- resize-c-node
  [type node op]
  (if (o/constant-node? node)
    (o/make-constant-node type
                          (normalize-constant type (o/constant->value node))
                          (fn [ctx]
                            (let [ctx (ctx/compile-type ctx type)]
                              (ctx/save-ir
                               ctx
                               (ir/const (ctx/compiled-type ctx type)
                                         (normalize-constant type
                                                             (o/constant->value node)))))))
    (o/make-node
     type
     (fn [ctx]
       (let [ctx (ctx/compile-type ctx type)
             ctx (ctx/compile-node ctx node)
             instruction (op (ctx/compiled-node ctx node)
                             (ctx/compiled-type ctx type)
                             {})]
         (ctx/compile-instruction ctx instruction)))
     {:class ::conversion})))

(defmethod o/cast [::CInt ::CInt]
  [type node _force?]
  (let [{to-bits :bits} (meta type)
        {from-bits :bits from-signed? :signed?} (meta (o/type-of node))]
    (cond
      (= to-bits from-bits)
      (if (o/constant-node? node)
        (o/make-constant-node type
                              (normalize-constant type
                                                 (o/constant->value node))
                              node)
        (vary-meta node assoc :type type))

      (> to-bits from-bits)
      (resize-c-node type node (if from-signed? ir/sext ir/zext))

      :else
      (resize-c-node type node ir/trunc))))

(defmethod o/cast [::CInt :oben/HostInteger]
  [type value _force?]
  (let [value (normalize-constant type value)]
    (o/make-constant-node
     type
     value
     (fn [ctx]
       (let [ctx (ctx/compile-type ctx type)]
         (ctx/save-ir ctx (ir/const (ctx/compiled-type ctx type) value)))))))

(defn- retag-number
  [type node]
  (vary-meta node assoc :type type))

(defn- normalize-float
  [type value]
  (if (= 32 (:bits (meta type)))
    (clj/float value)
    (clj/double value)))

(defn- resize-c-float-node
  [type node op]
  (if (o/constant-node? node)
    (o/make-constant-node type
                          (normalize-float type (o/constant->value node))
                          (fn [ctx]
                            (let [ctx (ctx/compile-type ctx type)]
                              (ctx/save-ir
                               ctx
                               (ir/const (ctx/compiled-type ctx type)
                                         (normalize-float
                                          type
                                          (o/constant->value node)))))))
    (o/make-node
     type
     (fn [ctx]
       (let [ctx (ctx/compile-type ctx type)
             ctx (ctx/compile-node ctx node)
             instruction (op (ctx/compiled-node ctx node)
                             (ctx/compiled-type ctx type)
                             {})]
         (ctx/compile-instruction ctx instruction)))
     {:class ::conversion})))

(defmethod o/cast [::CFloat ::CFloat]
  [type node _force?]
  (let [to-bits (:bits (meta type))
        from-bits (:bits (meta (o/type-of node)))]
    (cond
      (= to-bits from-bits)
      (if (o/constant-node? node)
        (o/make-constant-node type
                              (normalize-float type
                                              (o/constant->value node))
                              node)
        (vary-meta node assoc :type type))
      (> to-bits from-bits) (resize-c-float-node type node N/fpext)
      :else (resize-c-float-node type node N/fptrunc))))

(defmethod o/cast [::CFloat :oben/HostFloat]
  [type value _force?]
  (o/make-constant-node
   type
   (normalize-float type value)
   (fn [ctx]
     (let [ctx (ctx/compile-type ctx type)]
       (ctx/save-ir ctx
                     (ir/const (ctx/compiled-type ctx type)
                               (normalize-float type value)))))))

(defmethod o/cast [::CFloat :oben/HostInteger]
  [type value _force?]
  (o/cast type (o/parse-host-value value) true))

(defmethod o/cast [::CFloat ::CInt]
  [type node _force?]
  (let [bits (:bits (meta type))
        op (if (:signed? (meta (o/type-of node))) N/sitofp N/uitofp)]
    (retag-number type (op node bits))))

(defmethod o/cast [::CFloat ::N/Int]
  [type node _force?]
  (let [bits (:bits (meta type))
        op (if (isa? (o/tid-of-type (o/type-of node)) ::N/SInt)
             N/sitofp
             N/uitofp)]
    (retag-number type (op node bits))))

(defmethod o/cast [::CFloat ::N/FP]
  [type node _force?]
  (let [to-bits (:bits (meta type))
        from-bits (:size (meta (o/type-of node)))]
    (if (= to-bits from-bits)
      (retag-number type node)
      (retag-number type
                    ((if (> to-bits from-bits) N/fpext N/fptrunc)
                     node to-bits)))))

(defmethod o/cast [::CInt ::CFloat]
  [type node _force?]
  (let [bits (:bits (meta type))
        op (if (:signed? (meta (o/type-of node))) N/fptosi N/fptoui)]
    (retag-number type (op node bits))))

(defmethod o/cast [::CInt ::N/FP]
  [type node _force?]
  (let [bits (:bits (meta type))
        op (if (isa? (o/tid-of-type (o/type-of node)) ::N/SInt)
             N/fptosi
             N/fptoui)]
    (retag-number type (op node bits))))

(defmethod o/cast [::CInt ::N/UInt]
  [type node _force?]
  (let [bits (:bits (meta type))
        from-bits (:size (meta (o/type-of node)))]
    (retag-number type
                  (if (= bits from-bits)
                    node
                    (if (> bits from-bits)
                      (N/zext node bits)
                      (N/trunc node bits))))))

(defmethod o/cast [::CInt ::N/SInt]
  [type node _force?]
  (let [bits (:bits (meta type))
        from-bits (:size (meta (o/type-of node)))]
    (retag-number type
                  (if (= bits from-bits)
                    node
                    (if (> bits from-bits)
                      (N/sext node bits)
                      (N/trunc node bits))))))

(defn- c-int-type?
  [type]
  (isa? (o/tid-of-type type) ::CInt))

(defn- number->c-type
  [type]
  (CInt (:size (meta type))
        (isa? (o/tid-of-type type) ::N/SInt)))

(defn- as-c-node
  [node]
  (if (c-int-type? (o/type-of node))
    node
    (o/cast (number->c-type (o/type-of node)) node false)))

(defn- promoted-type
  [type]
  (let [int-type (int (target/current))
        bits (:bits (meta type))]
    (if (< bits (:bits (meta int-type)))
      int-type
      type)))

(defn- common-type
  [lhs-type rhs-type]
  (let [lhs (promoted-type lhs-type)
        rhs (promoted-type rhs-type)
        lhs-bits (:bits (meta lhs))
        rhs-bits (:bits (meta rhs))
        lhs-signed? (:signed? (meta lhs))
        rhs-signed? (:signed? (meta rhs))]
    (cond
      (and (= lhs-signed? rhs-signed?)
           (>= lhs-bits rhs-bits))
      lhs

      (and (= lhs-signed? rhs-signed?)
           (< lhs-bits rhs-bits))
      rhs

      (and lhs-signed? (> lhs-bits rhs-bits))
      lhs

      (and rhs-signed? (> rhs-bits lhs-bits))
      rhs

      :else
      (CInt (max lhs-bits rhs-bits) false))))

(defmethod o/get-ubertype [::CInt ::CInt]
  [t1 t2]
  (common-type t1 t2))

(defn- c-binary-node
  [lhs rhs instruction-fn]
  (let [lhs (as-c-node lhs)
        rhs (as-c-node rhs)
        result-type (common-type (o/type-of lhs) (o/type-of rhs))
        lhs (o/cast result-type lhs false)
        rhs (o/cast result-type rhs false)]
    (o/make-node
     result-type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx lhs)
             ctx (ctx/compile-node ctx rhs)
             instruction (instruction-fn (ctx/compiled-node ctx lhs)
                                         (ctx/compiled-node ctx rhs))]
         (ctx/compile-instruction ctx instruction)))
     {:class ::binary-op})))

(defn- c-unary-node
  [node instruction-fn]
  (let [node (as-c-node node)
        type (promoted-type (o/type-of node))]
    (o/make-node
     type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx node)
             instruction (instruction-fn (ctx/compiled-node ctx node)
                                         (ctx/compiled-type ctx type))]
         (ctx/compile-instruction ctx instruction)))
     {:class ::unary-op})))

(defmacro define-c-binary-op
  [multifn instruction-fn]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (c-binary-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (c-binary-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (c-binary-node lhs# rhs# ~instruction-fn))))

(define-c-binary-op Algebra/+ #(ir/add %1 %2 {}))
(define-c-binary-op Algebra/- #(ir/sub %1 %2 {}))
(define-c-binary-op Algebra/* #(ir/mul %1 %2 {}))

(defmethod Algebra// [::CInt ::CInt]
  [lhs rhs]
  (let [signed? (:signed? (meta (common-type (o/type-of lhs)
                                            (o/type-of rhs))))]
    (c-binary-node lhs rhs
                   (if signed?
                     #(ir/sdiv %1 %2 {})
                     #(ir/udiv %1 %2 {})))))

(defmethod Algebra/% [::CInt ::CInt]
  [lhs rhs]
  (let [signed? (:signed? (meta (common-type (o/type-of lhs)
                                            (o/type-of rhs))))]
    (c-binary-node lhs rhs
                   (if signed?
                     #(ir/srem %1 %2 {})
                     #(ir/urem %1 %2 {})))))

(defmethod Algebra// [::CInt ::N/Int]
  [lhs rhs]
  (Algebra// lhs (as-c-node rhs)))

(defmethod Algebra// [::N/Int ::CInt]
  [lhs rhs]
  (Algebra// (as-c-node lhs) rhs))

(defmethod Algebra/% [::CInt ::N/Int]
  [lhs rhs]
  (Algebra/% lhs (as-c-node rhs)))

(defmethod Algebra/% [::N/Int ::CInt]
  [lhs rhs]
  (Algebra/% (as-c-node lhs) rhs))

(defmethod Algebra/- [::CInt]
  [node]
  (let [node (as-c-node node)
        type (promoted-type (o/type-of node))
        zero (o/cast type 0 false)]
    (c-binary-node zero node #(ir/sub %1 %2 {}))))

(defmacro define-c-compare-op
  [multifn predicate]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (let [result# (c-binary-node lhs# rhs#
                                    (fn [lhs# rhs#]
                                      (ir/icmp ~predicate lhs# rhs# {})))]
         (vary-meta result# assoc :type N/%u1)))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (~multifn (as-c-node lhs#) rhs#))))

(define-c-compare-op Eq/= :eq)
(define-c-compare-op Eq/!= :ne)

(defmacro define-c-ordered-op
  [multifn signed-predicate unsigned-predicate]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (let [lhs-type# (o/type-of lhs#)
             rhs-type# (o/type-of rhs#)
             result-type# (common-type lhs-type# rhs-type#)
             predicate# (if (:signed? (meta result-type#))
                          ~signed-predicate
                          ~unsigned-predicate)]
         (o/make-node
          N/%u1
          (fn [ctx#]
            (let [lhs# (o/cast result-type# lhs# false)
                  rhs# (o/cast result-type# rhs# false)
                  ctx# (ctx/compile-node ctx# lhs#)
                  ctx# (ctx/compile-node ctx# rhs#)]
              (ctx/compile-instruction
               ctx#
               (ir/icmp predicate#
                        (ctx/compiled-node ctx# lhs#)
                        (ctx/compiled-node ctx# rhs#)
                        {}))))
          {:class ::comparison})))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (~multifn (as-c-node lhs#) rhs#))))

(define-c-ordered-op Ord/< :slt :ult)
(define-c-ordered-op Ord/<= :sle :ule)
(define-c-ordered-op Ord/>= :sge :uge)
(define-c-ordered-op Ord/> :sgt :ugt)

(define-c-binary-op Bitwise/bit-and #(ir/and %1 %2 {}))
(define-c-binary-op Bitwise/bit-or #(ir/or %1 %2 {}))
(define-c-binary-op Bitwise/bit-xor #(ir/xor %1 %2 {}))

(defmethod Bitwise/bit-not [::CInt]
  [node]
  (let [node (as-c-node node)
        type (promoted-type (o/type-of node))]
    (o/make-node
     type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx node)
             instruction (ir/xor (ctx/compiled-node ctx node)
                                 (ir/const (ctx/compiled-type ctx type)
                                           (normalize-constant type -1))
                                 {})]
         (ctx/compile-instruction ctx instruction)))
     {:class ::unary-op})))

(defn- c-float-type?
  [type]
  (isa? (o/tid-of-type type) ::CFloat))

(defn- float-bits
  [type]
  (cond
    (c-float-type? type) (:bits (meta type))
    (isa? (o/tid-of-type type) ::N/FP) (:size (meta type))
    :else nil))

(defn- common-float-type
  [lhs-type rhs-type]
  (CFloat (max 32 (or (float-bits lhs-type) 0)
             (or (float-bits rhs-type) 0))))

(defn- c-float-node
  [lhs rhs instruction-fn]
  (let [result-type (common-float-type (o/type-of lhs) (o/type-of rhs))
        lhs (o/cast result-type lhs false)
        rhs (o/cast result-type rhs false)]
    (o/make-node
     result-type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx lhs)
             ctx (ctx/compile-node ctx rhs)
             instruction (instruction-fn (ctx/compiled-node ctx lhs)
                                         (ctx/compiled-node ctx rhs))]
         (ctx/compile-instruction ctx instruction)))
     {:class ::float-op})))

(defmacro define-c-float-binary-op
  [multifn instruction-fn]
  `(do
     (defmethod ~multifn [::CFloat ::CFloat]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CFloat ::CInt]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CInt ::CFloat]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CFloat ::N/Number]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::N/Number ::CFloat]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))))

(define-c-float-binary-op Algebra/+ #(ir/fadd %1 %2 {}))
(define-c-float-binary-op Algebra/- #(ir/fsub %1 %2 {}))
(define-c-float-binary-op Algebra/* #(ir/fmul %1 %2 {}))
(define-c-float-binary-op Algebra// #(ir/fdiv %1 %2 {}))

(defmethod Algebra/- [::CFloat]
  [node]
  (let [type (o/type-of node)
        zero (o/cast type 0.0 false)]
    (c-float-node zero node #(ir/fsub %1 %2 {}))))

(defmacro define-c-float-compare-op
  [multifn predicate]
  `(do
     (defmethod ~multifn [::CFloat ::CFloat]
       [lhs# rhs#]
       (let [result# (c-float-node lhs# rhs#
                                   (fn [lhs# rhs#]
                                     (ir/fcmp ~predicate lhs# rhs# {})))]
         (vary-meta result# assoc :type N/%u1)))
     (defmethod ~multifn [::CFloat ::CInt]
       [lhs# rhs#]
       (~multifn lhs# (o/cast (o/type-of lhs#) rhs# false)))
     (defmethod ~multifn [::CInt ::CFloat]
       [lhs# rhs#]
       (~multifn (o/cast (o/type-of rhs#) lhs# false) rhs#))
     (defmethod ~multifn [::CFloat ::N/Number]
       [lhs# rhs#]
       (~multifn lhs# (o/cast (o/type-of lhs#) rhs# false)))
     (defmethod ~multifn [::N/Number ::CFloat]
       [lhs# rhs#]
       (~multifn (o/cast (o/type-of rhs#) lhs# false) rhs#))))

(define-c-float-compare-op Eq/= :oeq)
;; C's != is true for NaN, so use LLVM's unordered-not-equal predicate.
(define-c-float-compare-op Eq/!= :une)
(define-c-float-compare-op Ord/< :olt)
(define-c-float-compare-op Ord/<= :ole)
(define-c-float-compare-op Ord/>= :oge)
(define-c-float-compare-op Ord/> :ogt)
