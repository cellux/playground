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
            [oben.core.protocols.Logical :as Logical]
            [oben.core.protocols.Conditional :as Conditional]
            [oben.core.protocols.Eq :as Eq]
            [oben.core.protocols.Ord :as Ord]
            [oben.core.types.Number :as N]
            [oben.core.types.Bool :as Bool]
            [oben.core.types.Ptr :as Ptr]
            [oben.core.nodes :as nodes]
            [omkamra.llvm.ir :as ir]))

;; C's conversion rules are based on type rank, not merely representation
;; width.  In particular, an LP64 target still has distinct `int` and `long`
;; types even though both are i64.  Keep their semantic identity and rank in
;; the type constructor, separately from the LLVM integer width.
(def ^:private rank-char 1)
(def ^:private rank-short 2)
(def ^:private rank-int 3)
(def ^:private rank-long 4)

(o/define-typeclass CInt [:oben/Value]
  [c-type bits signed? rank]
  (o/make-type
   #(ctx/save-ir % [:integer bits])
   {:c-type c-type
    :bits bits
    :signed? signed?
    :rank rank}))

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

(def i8 (CInt :i8 8 true rank-char))
(def u8 (CInt :i8 8 false rank-char))
(def i16 (CInt :i16 16 true rank-short))
(def u16 (CInt :i16 16 false rank-short))
(def i32 (CInt :i32 32 true rank-int))
(def u32 (CInt :i32 32 false rank-int))
(def i64 (CInt :i64 64 true rank-long))
(def u64 (CInt :i64 64 false rank-long))

(def f32 (CFloat 32))
(def f64 (CFloat 64))

(defn- attr
  [target name default]
  (get (target/attrs* target) name default))

(o/defportable char
  [target]
  (CInt :char
        (attr target :c-char-size 8)
        (attr target :c-char-signed? true)
        rank-char))

(o/defportable short
  [target]
  (CInt :short (attr target :c-short-size 16) true rank-short))

(o/defportable ushort
  [target]
  (CInt :short (attr target :c-short-size 16) false rank-short))

(o/defportable int
  [target]
  (CInt :int (attr target :c-int-size 32) true rank-int))

(o/defportable uint
  [target]
  (CInt :int (attr target :c-int-size 32) false rank-int))

(o/defportable long
  [target]
  (CInt :long (attr target :c-long-size 64) true rank-long))

(o/defportable ulong
  [target]
  (CInt :long (attr target :c-long-size 64) false rank-long))

(o/defportable float
  [target]
  (CFloat (attr target :c-float-size 32)))

(o/defportable double
  [target]
  (CFloat (attr target :c-double-size 64)))

(defn- c-int-type
  []
  (int (target/current)))

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
        ;; Build these as BigInts: primitive Clojure long shifts by 64 wrap
        ;; the shift count, while C supports 64-bit integer types.
        modulus (reduce *' 1N (repeat bits 2N))
        value (mod value modulus)]
    (let [result (if (and signed?
                           (>= value
                               (reduce *' 1N
                                       (repeat (dec bits) 2N))))
                   (- value modulus)
                   value)]
      ;; Keep ordinary constants as JVM longs for the LLVM IR helpers; retain
      ;; BigInt only when an unsigned 64-bit value needs it.
      (if (<= Long/MIN_VALUE result Long/MAX_VALUE)
        (clj/long result)
        result))))

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
      (> to-bits from-bits) (resize-c-float-node type node ir/fpext)
      :else (resize-c-float-node type node ir/fptrunc))))

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

(defmethod o/cast [::CFloat ::Bool/Bool]
  [type node _force?]
  (o/cast type (o/cast (c-int-type) node false) false))

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
        op (if (:signed? (meta type)) N/fptosi N/fptoui)]
    (retag-number type (op node bits))))

(defmethod o/cast [::CInt ::N/FP]
  [type node _force?]
  (let [bits (:bits (meta type))
        op (if (:signed? (meta type)) N/fptosi N/fptoui)]
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

(defn- normalize-core-int-constant
  [type value]
  (let [bits (:size (meta type))
        modulus (reduce *' 1N (repeat bits 2N))
        value (mod value modulus)]
    (if (and (isa? (o/tid-of-type type) ::N/SInt)
             (>= value (quot modulus 2)))
      (- value modulus)
      value)))

(defn- c-int->core-int
  "Converts a C integer to an Oben core integer without changing its bits.

  GEP indices are core integers.  This conversion is therefore needed for C
  pointer arithmetic, while retaining the C signedness when an extension is
  necessary."
  [type node]
  (let [to-bits (:size (meta type))
        {from-bits :bits signed? :signed?} (meta (o/type-of node))]
    (if (o/constant-node? node)
      ;; `resize-c-node` normalizes C metadata (`:bits`/`:signed?`), whereas
      ;; core Number types use `:size`; construct their constant directly.
      (N/make-constant-number-node
       type
       (normalize-core-int-constant type (o/constant->value node)))
      (cond
        (= to-bits from-bits) (vary-meta node assoc :type type)
        (> to-bits from-bits) (resize-c-node type node
                                             (if signed? ir/sext ir/zext))
        :else (resize-c-node type node ir/trunc)))))

(defmethod o/cast [::N/UInt ::CInt]
  [type node _force?]
  (c-int->core-int type node))

(defmethod o/cast [::N/SInt ::CInt]
  [type node _force?]
  (c-int->core-int type node))

(defmethod o/cast [::Ptr/Ptr ::CInt]
  [type node _force?]
  ;; An integer arm is a C null pointer constant only when it is the
  ;; compile-time integer constant zero.  Do not silently turn arbitrary
  ;; integers into pointers during an implicit conditional conversion.
  (if (and (o/constant-node? node)
           (zero? (o/constant->value node)))
    (o/cast type nil false)
    (throw (ex-info "only integer constant zero converts to a pointer in a C conditional"
                    {:type type
                     :node node}))))

(defmethod o/cast [::CInt ::Bool/Bool]
  [type node _force?]
  (if (o/constant-node? node)
    (o/make-constant-node
     type
     (if (o/constant->value node) 1 0)
     (fn [ctx]
       (let [ctx (ctx/compile-type ctx type)]
         (ctx/save-ir ctx
                       (ir/const (ctx/compiled-type ctx type)
                                 (if (o/constant->value node) 1 0))))))
    (o/make-node
     type
     (fn [ctx]
       (let [ctx (ctx/compile-type ctx type)
             ctx (ctx/compile-node ctx node)
             instruction (ir/zext (ctx/compiled-node ctx node)
                                  (ctx/compiled-type ctx type)
                                  {})]
         (ctx/compile-instruction ctx instruction)))
     {:class ::conversion})))

(defn- c-scalar-to-bool-node
  [node zero host-predicate ir-fn]
  (if (o/constant-node? node)
    (Bool/make-constant-bool-node
     (host-predicate (o/constant->value node)
                     (o/constant->value zero)))
    (o/make-node
     Bool/%bool
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx node)
             ctx (ctx/compile-node ctx zero)
             instruction (ir-fn (ctx/compiled-node ctx node)
                                (ctx/compiled-node ctx zero))]
         (ctx/compile-instruction ctx instruction)))
     {:class ::conversion})))

(defmethod o/cast [::Bool/Bool ::CInt]
  [_type node _force?]
  (c-scalar-to-bool-node
   node
   (o/cast (o/type-of node) 0 false)
   #(not= %1 %2)
   #(ir/icmp :ne %1 %2 {})))

(defmethod o/cast [::Bool/Bool ::CFloat]
  [_type node _force?]
  (c-scalar-to-bool-node
   node
   (o/cast (o/type-of node) 0.0 false)
   #(not= %1 %2)
   #(ir/fcmp :une %1 %2 {})))

(defn- c-int-result
  [bool-node]
  (o/cast (c-int-type) bool-node false))

(defn- c-int-type?
  [type]
  (isa? (o/tid-of-type type) ::CInt))

(defn- rank-for-bits
  [bits]
  (cond
    (<= bits 8) rank-char
    (<= bits 16) rank-short
    (<= bits 32) rank-int
    :else rank-long))

(defn- number->c-type
  [type]
  (let [bits (:size (meta type))]
    (CInt :core-integer
          bits
          (isa? (o/tid-of-type type) ::N/SInt)
          (rank-for-bits bits))))

(defn- as-c-node
  [node]
  (if (c-int-type? (o/type-of node))
    node
    (o/cast (number->c-type (o/type-of node)) node false)))

(defn- max-integer-value
  [type]
  (let [{:keys [bits signed?]} (meta type)
        exponent (if signed? (dec bits) bits)]
    (dec (reduce *' 1N (repeat exponent 2N)))))

(defn- unsigned-variant
  "Returns the corresponding unsigned C type without changing its identity or
  rank.  LLVM's width is an implementation detail of that semantic type."
  [type]
  (let [{:keys [c-type bits rank]} (meta type)]
    (CInt c-type bits false rank)))

(defn- promoted-type
  [type]
  (let [int-type (c-int-type)]
    (if (< (:rank (meta type)) (:rank (meta int-type)))
      ;; C promotes a lower-rank integer to int when int can represent every
      ;; value; otherwise it promotes to unsigned int.
      (if (<= (max-integer-value type) (max-integer-value int-type))
        int-type
        (unsigned-variant int-type))
      type)))

(defn- common-type
  [lhs-type rhs-type]
  (let [lhs (promoted-type lhs-type)
        rhs (promoted-type rhs-type)
        lhs-signed? (:signed? (meta lhs))
        rhs-signed? (:signed? (meta rhs))
        lhs-rank (:rank (meta lhs))
        rhs-rank (:rank (meta rhs))]
    (cond
      ;; Same signedness: use the type with the greater conversion rank.
      (= lhs-signed? rhs-signed?)
      (if (>= lhs-rank rhs-rank) lhs rhs)

      ;; An unsigned type whose rank is at least the signed type wins.
      (and (not lhs-signed?) (>= lhs-rank rhs-rank))
      lhs

      (and (not rhs-signed?) (>= rhs-rank lhs-rank))
      rhs

      ;; Otherwise the signed type wins only if it can represent all values
      ;; of the unsigned type.  If not, use its corresponding unsigned type.
      (and lhs-signed? (>= (max-integer-value lhs)
                           (max-integer-value rhs)))
      lhs

      (and rhs-signed? (>= (max-integer-value rhs)
                           (max-integer-value lhs)))
      rhs

      lhs-signed?
      (unsigned-variant lhs)

      :else
      (unsigned-variant rhs))))

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
        type (promoted-type (o/type-of node))
        node (o/cast type node false)]
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
         (c-int-result (vary-meta result# assoc :type Bool/%bool))))
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
         (c-int-result
          (o/make-node
           Bool/%bool
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
           {:class ::comparison}))))
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

(defn- c-shift-node
  "Builds a C shift expression.

  C promotes both operands independently, but the result type and the LLVM
  operand width are determined by the promoted left operand. The right-hand
  value is converted to that width only to satisfy LLVM's instruction shape;
  shift counts outside the valid range already have undefined behavior in C.
  "
  [lhs rhs instruction-fn]
  (let [lhs (as-c-node lhs)
        rhs (as-c-node rhs)
        result-type (promoted-type (o/type-of lhs))
        promoted-rhs (promoted-type (o/type-of rhs))
        lhs (o/cast result-type lhs false)
        rhs (o/cast result-type (o/cast promoted-rhs rhs false) false)]
    (o/make-node
     result-type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx lhs)
             ctx (ctx/compile-node ctx rhs)
             instruction (instruction-fn
                         result-type
                         (ctx/compiled-node ctx lhs)
                         (ctx/compiled-node ctx rhs))]
         (ctx/compile-instruction ctx instruction)))
     {:class ::shift-op})))

(defmacro define-c-shift-op
  [multifn instruction-fn]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (c-shift-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (c-shift-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (c-shift-node lhs# rhs# ~instruction-fn))))

(define-c-shift-op Bitwise/bit-shift-left
  (fn [_type lhs rhs] (ir/shl lhs rhs {})))

(define-c-shift-op Bitwise/bit-shift-right
  (fn [type lhs rhs]
    (if (:signed? (meta type))
      (ir/ashr lhs rhs {})
      (ir/lshr lhs rhs {}))))

(defmethod Bitwise/bit-not [::CInt]
  [node]
  (c-unary-node node
                (fn [node type]
                  (ir/xor node (ir/const type -1) {}))))

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

(defn- bool-type?
  [type]
  (isa? (o/tid-of-type type) ::Bool/Bool))

(defn- integer-conditional-type
  "Returns the C integer type corresponding to a conditional arm, or nil.

  Oben booleans are treated like C _Bool for this purpose: they undergo the
  integer promotions when paired with a C integer arm."
  [type]
  (cond
    (c-int-type? type) type
    (bool-type? type) (c-int-type)
    (isa? (o/tid-of-type type) ::N/Int) (number->c-type type)
    :else nil))

(defn- c-pointer-type?
  [type]
  (isa? (o/tid-of-type type) ::Ptr/Ptr))

(defn- null-pointer-constant?
  [node]
  (and (o/constant-node? node)
       (or (c-int-type? (o/type-of node))
           (isa? (o/tid-of-type (o/type-of node)) ::N/Int))
       (zero? (o/constant->value node))))

(defn- c-pointer-conditional-type
  [lhs-type rhs-type]
  (let [lhs-object-type (:object-type (meta lhs-type))
        rhs-object-type (:object-type (meta rhs-type))
        unqualified (fn [type] (vary-meta type dissoc :qualifiers))]
    (when (= (unqualified lhs-object-type)
             (unqualified rhs-object-type))
      ;; The result points at a type qualified with the union of the arm
      ;; qualifiers, as required by C's conditional operator rules.
      (let [object-type (vary-meta
                         (unqualified lhs-object-type)
                         assoc
                         :qualifiers
                         (into (o/qualifiers lhs-object-type)
                               (o/qualifiers rhs-object-type)))]
        (Ptr/Ptr object-type)))))

(defn- c-conditional-type
  [lhs-type rhs-type]
  (let [lhs-float? (or (c-float-type? lhs-type)
                       (isa? (o/tid-of-type lhs-type) ::N/FP))
        rhs-float? (or (c-float-type? rhs-type)
                       (isa? (o/tid-of-type rhs-type) ::N/FP))
        lhs-int (integer-conditional-type lhs-type)
        rhs-int (integer-conditional-type rhs-type)]
    (cond
      (or lhs-float? rhs-float?)
      (common-float-type lhs-type rhs-type)

      (and lhs-int rhs-int)
      (common-type lhs-int rhs-int)

      (and (c-pointer-type? lhs-type)
           (c-pointer-type? rhs-type))
      (or (c-pointer-conditional-type lhs-type rhs-type)
          (throw (ex-info "conditional pointer types are incompatible"
                          {:lhs-type lhs-type
                           :rhs-type rhs-type})))

      :else
      (o/ubertype-of lhs-type rhs-type))))

(defn- c-conditional
  [condition then-node else-node]
  (let [then-type (o/type-of then-node)
        else-type (o/type-of else-node)
        result-type (cond
                      (and (c-pointer-type? then-type)
                           (null-pointer-constant? else-node))
                      then-type

                      (and (null-pointer-constant? then-node)
                           (c-pointer-type? else-type))
                      else-type

                      :else
                      (c-conditional-type then-type else-type))]
    (nodes/make-conditional-node condition
                                  then-node
                                  else-node
                                  result-type)))

(defmacro define-c-conditional
  [lhs-type rhs-type]
  `(defmethod Conditional/select [:oben/Any ~lhs-type ~rhs-type]
     [condition# then-node# else-node#]
     (c-conditional condition# then-node# else-node#)))

;; C arithmetic arms use the usual arithmetic conversions.  The conditional
;; protocol dispatches on all three operands, allowing the C condition to give
;; otherwise-untyped numeric arms their C interpretation.
(define-c-conditional ::CInt ::CInt)
(define-c-conditional ::CInt ::CFloat)
(define-c-conditional ::CFloat ::CInt)
(define-c-conditional ::CFloat ::CFloat)
(define-c-conditional ::CInt ::N/Number)
(define-c-conditional ::N/Number ::CInt)
(define-c-conditional ::CFloat ::N/Number)
(define-c-conditional ::N/Number ::CFloat)
(define-c-conditional ::CInt ::Bool/Bool)
(define-c-conditional ::Bool/Bool ::CInt)
(define-c-conditional ::CFloat ::Bool/Bool)
(define-c-conditional ::Bool/Bool ::CFloat)
(define-c-conditional ::Ptr/Ptr ::Ptr/Ptr)
(define-c-conditional ::Ptr/Ptr ::CInt)
(define-c-conditional ::CInt ::Ptr/Ptr)
(define-c-conditional ::Ptr/Ptr ::N/Int)
(define-c-conditional ::N/Int ::Ptr/Ptr)

;; An explicitly C-typed condition gives C semantics to otherwise-untyped
;; numeric arms as well.  This is what makes `(if c-condition 1 2)` produce
;; a C int rather than an Oben integer whose width was inferred from the
;; literal alone.
(defmacro define-c-conditioned-conditional
  [condition-type]
  `(defmethod Conditional/select [~condition-type ::N/Number ::N/Number]
     [condition# then-node# else-node#]
     (c-conditional condition# then-node# else-node#)))

(define-c-conditioned-conditional ::CInt)
(define-c-conditioned-conditional ::CFloat)
(define-c-conditioned-conditional ::Ptr/Ptr)

(defn conditional
  "C's value-producing conditional expression.

  The ordinary Oben `if` form uses the same dispatch after requiring this
  namespace; this named entry point is useful when constructing an expression
  programmatically."
  [condition then-node else-node]
  (Conditional/select condition then-node else-node))

(defn- c-pointer-offset
  [ptr offset]
  ;; `nodes/%gep` uses core integer indices.  Preserve C signedness during the
  ;; conversion so negative offsets remain negative at the pointer width.
  (nodes/%gep ptr [(o/cast (N/UInt (target/attr :address-size)) offset false)]))

(defmethod Algebra/+ [::Ptr/Ptr ::CInt]
  [ptr offset]
  (c-pointer-offset ptr offset))

(defmethod Algebra/- [::Ptr/Ptr ::CInt]
  [ptr offset]
  (c-pointer-offset ptr (Algebra/- offset)))

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
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CFloat ::Bool/Bool]
       [lhs# rhs#]
       (c-float-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::Bool/Bool ::CFloat]
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
         (c-int-result (vary-meta result# assoc :type Bool/%bool))))
     (defmethod ~multifn [::CFloat ::CInt]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))
     (defmethod ~multifn [::CInt ::CFloat]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))
     (defmethod ~multifn [::CFloat ::N/Number]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))
     (defmethod ~multifn [::N/Number ::CFloat]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))
     (defmethod ~multifn [::CFloat ::Bool/Bool]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))
     (defmethod ~multifn [::Bool/Bool ::CFloat]
       [lhs# rhs#]
       (let [type# (common-float-type (o/type-of lhs#)
                                      (o/type-of rhs#))]
         (~multifn (o/cast type# lhs# false)
                   (o/cast type# rhs# false))))))

(define-c-float-compare-op Eq/= :oeq)
;; C's != is true for NaN, so use LLVM's unordered-not-equal predicate.
(define-c-float-compare-op Eq/!= :une)
(define-c-float-compare-op Ord/< :olt)
(define-c-float-compare-op Ord/<= :ole)
(define-c-float-compare-op Ord/>= :oge)
(define-c-float-compare-op Ord/> :ogt)

(defn- c-logical-zero
  []
  (o/cast (c-int-type) 0 false))

(defn- c-logical-one
  []
  (o/cast (c-int-type) 1 false))

(defn- c-logical-and
  [lhs rhs]
  (let [lhs (o/cast Bool/%bool lhs false)
        rhs (o/cast Bool/%bool rhs false)
        zero (c-logical-zero)
        one (c-logical-one)]
    (list 'if lhs
          (list 'if rhs one zero)
          zero)))

(defn- c-logical-or
  [lhs rhs]
  (let [lhs (o/cast Bool/%bool lhs false)
        rhs (o/cast Bool/%bool rhs false)
        zero (c-logical-zero)
        one (c-logical-one)]
    (list 'if lhs
          one
          (list 'if rhs one zero))))

(defn- c-logical-not
  [node]
  (let [node (o/cast Bool/%bool node false)
        zero (c-logical-zero)
        one (c-logical-one)]
    (list 'if node zero one)))

(defmacro define-c-logical-binary-op
  [multifn implementation]
  `(do
     ~@(for [dispatch# '([::CInt ::CInt]
                         [::CInt ::CFloat]
                         [::CFloat ::CInt]
                         [::CFloat ::CFloat]
                         [::CInt ::Bool/Bool]
                         [::Bool/Bool ::CInt]
                         [::CFloat ::Bool/Bool]
                         [::Bool/Bool ::CFloat]
                         [::CInt ::N/Number]
                         [::N/Number ::CInt]
                         [::CFloat ::N/Number]
                         [::N/Number ::CFloat]
                         [::Ptr/Ptr ::Ptr/Ptr]
                         [::Ptr/Ptr ::CInt]
                         [::CInt ::Ptr/Ptr]
                         [::Ptr/Ptr ::CFloat]
                         [::CFloat ::Ptr/Ptr]
                         [::Ptr/Ptr ::Bool/Bool]
                         [::Bool/Bool ::Ptr/Ptr]
                         [::Ptr/Ptr ::N/Number]
                         [::N/Number ::Ptr/Ptr])]
       `(defmethod ~multifn ~dispatch#
          [lhs# rhs#]
          (~implementation lhs# rhs#)))))

(define-c-logical-binary-op Logical/and c-logical-and)
(define-c-logical-binary-op Logical/or c-logical-or)

(defmethod Logical/not [::CInt]
  [node]
  (c-logical-not node))

(defmethod Logical/not [::CFloat]
  [node]
  (c-logical-not node))

(defmethod Logical/not [::Ptr/Ptr]
  [node]
  (c-logical-not node))
