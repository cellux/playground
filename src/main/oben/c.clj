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
            [oben.core.protocols.Place :as Place]
            [oben.core.protocols.Eq :as Eq]
            [oben.core.protocols.Ord :as Ord]
            [oben.core.types.Number :as N]
            [oben.core.types.Bool :as Bool]
            [oben.core.types.Ptr :as Ptr]
            [oben.core.types.Fn :as Fn]
            [oben.core.types.Void :as Void]
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
(def ^:private rank-float 1)
(def ^:private rank-double 2)

(o/define-typeclass CInt [:oben/Value]
  [c-type bits signed? rank]
  (o/make-type
   #(ctx/save-ir % [:integer bits])
   {:c-type c-type
    :bits bits
    :signed? signed?
    :rank rank}))

(o/define-typeclass CFloat [:oben/Value]
  [c-type bits rank]
  (o/make-type
   (let [ir-type (case bits
                   32 :float
                   64 :double
                   (throw (ex-info "unsupported C floating-point size"
                                   {:bits bits})))]
     #(ctx/save-ir % ir-type))
   {:c-type c-type
    :bits bits
    :rank rank}))

(def i8 (CInt :i8 8 true rank-char))
(def u8 (CInt :i8 8 false rank-char))
(def i16 (CInt :i16 16 true rank-short))
(def u16 (CInt :i16 16 false rank-short))
(def i32 (CInt :i32 32 true rank-int))
(def u32 (CInt :i32 32 false rank-int))
(def i64 (CInt :i64 64 true rank-long))
(def u64 (CInt :i64 64 false rank-long))

(def f32 (CFloat :float 32 rank-float))
(def f64 (CFloat :double 64 rank-double))

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
  (CFloat :float (attr target :c-float-size 32) rank-float))

(o/defportable double
  [target]
  (CFloat :double (attr target :c-double-size 64) rank-double))

(defn- c-int-type
  []
  (int (target/current)))

(defn- c-sizeof
  [ctx type]
  (let [char-size (ctx/target-attr ctx :c-char-size)]
    (when-not (= char-size 8)
      (throw (ex-info "C targets with non-8-bit CHAR_BIT are unsupported"
                      {:c-char-size char-size})))
    (quot (:bits (meta type)) 8)))

(defmethod o/sizeof* ::CInt
  [ctx type]
  (c-sizeof ctx type))

(defmethod o/sizeof* ::CFloat
  [ctx type]
  (c-sizeof ctx type))

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

(defn- conversion-node
  "Constructs a C conversion while preserving destination-specific constants.

  `normalize` receives the destination type and a host constant; `op` is the
  LLVM conversion used for non-constants.  Integers and floating point values
  differ only in constant normalization, not in lowering shape."
  [type node normalize op]
  (if (o/constant-node? node)
    (let [value (normalize type (o/constant->value node))]
      (o/make-constant-node
       type value
       (fn [ctx]
         (let [ctx (ctx/compile-type ctx type)]
           (ctx/save-ir ctx (ir/const (ctx/compiled-type ctx type) value))))))
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
      (conversion-node type node normalize-constant
                       (if from-signed? ir/sext ir/zext))

      :else
      (conversion-node type node normalize-constant ir/trunc))))

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
      (> to-bits from-bits) (conversion-node type node normalize-float ir/fpext)
      :else (conversion-node type node normalize-float ir/fptrunc))))

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
      ;; C and core Number types carry different width metadata, so construct
      ;; the core integer constant directly.
      (N/make-constant-number-node
       type
       (normalize-core-int-constant type (o/constant->value node)))
      (cond
        (= to-bits from-bits) (vary-meta node assoc :type type)
        (> to-bits from-bits) (conversion-node type node normalize-core-int-constant
                                               (if signed? ir/sext ir/zext))
        :else (conversion-node type node normalize-core-int-constant ir/trunc)))))

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

(declare bool-type?)

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
  "Gives an integer operand its C semantic type before integer promotions."
  [node]
  (let [type (o/type-of node)]
    (cond
      (c-int-type? type) node
      (bool-type? type) (o/cast (c-int-type) node false)
      (isa? (o/tid-of-type type) ::N/Int)
      (o/cast (number->c-type type) node false)
      :else
      (throw (ex-info "not a C integer operand" {:type type :node node})))))

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

(declare usual-arithmetic-conversions)

(defn- c-binary-node
  [lhs rhs instruction-fn]
  (let [{:keys [type lhs rhs]} (usual-arithmetic-conversions lhs rhs)]
    (o/make-node
     type
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
       (c-binary-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::CInt ::Bool/Bool]
       [lhs# rhs#]
       (c-binary-node lhs# rhs# ~instruction-fn))
     (defmethod ~multifn [::Bool/Bool ::CInt]
       [lhs# rhs#]
       (c-binary-node lhs# rhs# ~instruction-fn))))

(define-c-binary-op Algebra/+ #(ir/add %1 %2 {}))
(define-c-binary-op Algebra/- #(ir/sub %1 %2 {}))
(define-c-binary-op Algebra/* #(ir/mul %1 %2 {}))

(defmethod Algebra// [::CInt ::CInt]
  [lhs rhs]
  (let [signed? (:signed? (meta (:type (usual-arithmetic-conversions lhs rhs))))]
    (c-binary-node lhs rhs
                   (if signed?
                     #(ir/sdiv %1 %2 {})
                     #(ir/udiv %1 %2 {})))))

(defmethod Algebra/% [::CInt ::CInt]
  [lhs rhs]
  (let [signed? (:signed? (meta (:type (usual-arithmetic-conversions lhs rhs))))]
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

(defmethod Algebra// [::CInt ::Bool/Bool]
  [lhs rhs]
  (Algebra// lhs (as-c-node rhs)))

(defmethod Algebra// [::Bool/Bool ::CInt]
  [lhs rhs]
  (Algebra// (as-c-node lhs) rhs))

(defmethod Algebra/% [::CInt ::N/Int]
  [lhs rhs]
  (Algebra/% lhs (as-c-node rhs)))

(defmethod Algebra/% [::N/Int ::CInt]
  [lhs rhs]
  (Algebra/% (as-c-node lhs) rhs))

(defmethod Algebra/% [::CInt ::Bool/Bool]
  [lhs rhs]
  (Algebra/% lhs (as-c-node rhs)))

(defmethod Algebra/% [::Bool/Bool ::CInt]
  [lhs rhs]
  (Algebra/% (as-c-node lhs) rhs))

(defmethod Algebra/- [::CInt]
  [node]
  (let [node (as-c-node node)
        type (promoted-type (o/type-of node))
        zero (o/cast type 0 false)]
    (c-binary-node zero node #(ir/sub %1 %2 {}))))

(defn- c-comparison-node
  "Builds an i1 comparison directly after the usual arithmetic conversions."
  [lhs rhs instruction-fn]
  (let [{:keys [type lhs rhs]} (usual-arithmetic-conversions lhs rhs)
        bool-node
        (o/make-node
         Bool/%bool
         (fn [ctx]
           (let [ctx (ctx/compile-node ctx lhs)
                 ctx (ctx/compile-node ctx rhs)
                 instruction (instruction-fn type
                                             (ctx/compiled-node ctx lhs)
                                             (ctx/compiled-node ctx rhs))]
             (ctx/compile-instruction ctx instruction)))
         {:class ::comparison})]
    (c-int-result bool-node)))

(defmacro define-c-compare-op
  [multifn predicate]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (c-comparison-node lhs# rhs#
                          (fn [_type# lhs# rhs#]
                            (ir/icmp ~predicate lhs# rhs# {}))))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (~multifn (as-c-node lhs#) rhs#))
     (defmethod ~multifn [::CInt ::Bool/Bool]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::Bool/Bool ::CInt]
       [lhs# rhs#]
       (~multifn (as-c-node lhs#) rhs#))))

(define-c-compare-op Eq/= :eq)
(define-c-compare-op Eq/!= :ne)

(defmacro define-c-ordered-op
  [multifn signed-predicate unsigned-predicate]
  `(do
     (defmethod ~multifn [::CInt ::CInt]
       [lhs# rhs#]
       (c-comparison-node
        lhs# rhs#
        (fn [type# lhs# rhs#]
          (ir/icmp (if (:signed? (meta type#))
                     ~signed-predicate
                     ~unsigned-predicate)
                   lhs# rhs# {}))))
     (defmethod ~multifn [::CInt ::N/Int]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::N/Int ::CInt]
       [lhs# rhs#]
       (~multifn (as-c-node lhs#) rhs#))
     (defmethod ~multifn [::CInt ::Bool/Bool]
       [lhs# rhs#]
       (~multifn lhs# (as-c-node rhs#)))
     (defmethod ~multifn [::Bool/Bool ::CInt]
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

(defn- float-rank
  [type]
  (if-let [bits (float-bits type)]
    (or (:rank (meta type))
        (case bits
          32 rank-float
          64 rank-double
          (throw (ex-info "unsupported floating-point type"
                          {:type type}))))
    0))

(defn- common-float-type
  [lhs-type rhs-type]
  ;; `float` and `double` remain semantically distinct even when a target
  ;; lowers both to the same LLVM width.  The higher C conversion rank wins;
  ;; width breaks ties only for non-C numeric operands.
  (let [lhs-rank (float-rank lhs-type)
        rhs-rank (float-rank rhs-type)
        winner (if (>= lhs-rank rhs-rank) lhs-type rhs-type)
        bits (max (or (float-bits lhs-type) 0)
                  (or (float-bits rhs-type) 0))
        rank (max lhs-rank rhs-rank)
        c-type (:c-type (meta winner))]
    (CFloat (or c-type (if (= rank rank-double) :double :float))
            bits
            rank)))

(defn- floating-type?
  [type]
  (or (c-float-type? type)
      (isa? (o/tid-of-type type) ::N/FP)))

(defn- arithmetic-type?
  [type]
  (or (floating-type? type)
      (c-int-type? type)
      (bool-type? type)
      (isa? (o/tid-of-type type) ::N/Int)))

(defn usual-arithmetic-conversions
  "Applies C17's usual arithmetic conversions to two scalar operands.

  Returns `{:type result-type :lhs converted-lhs :rhs converted-rhs}`.  This
  is the sole conversion kernel for C arithmetic, comparisons, and arithmetic
  conditional arms.  Integer promotions happen before signed/unsigned rank
  selection; if either operand is floating point, both operands are converted
  to the widest C floating type."
  [lhs rhs]
  (let [lhs-type (o/type-of lhs)
        rhs-type (o/type-of rhs)]
    (when-not (and (arithmetic-type? lhs-type)
                   (arithmetic-type? rhs-type))
      (throw (ex-info "usual arithmetic conversions require arithmetic operands"
                      {:lhs-type lhs-type :rhs-type rhs-type})))
    (if (or (floating-type? lhs-type) (floating-type? rhs-type))
      (let [type (common-float-type lhs-type rhs-type)]
        {:type type
         :lhs (o/cast type lhs false)
         :rhs (o/cast type rhs false)})
      (let [lhs (as-c-node lhs)
            rhs (as-c-node rhs)
            type (common-type (o/type-of lhs) (o/type-of rhs))]
        {:type type
         :lhs (o/cast type lhs false)
         :rhs (o/cast type rhs false)}))))

(defn- bool-type?
  [type]
  (isa? (o/tid-of-type type) ::Bool/Bool))

(defn- c-pointer-type?
  [type]
  (isa? (o/tid-of-type type) ::Ptr/Ptr))

(defn- null-pointer-constant?
  [node]
  (and (o/constant-node? node)
       (or (c-int-type? (o/type-of node))
           (isa? (o/tid-of-type (o/type-of node)) ::N/Int))
       (zero? (o/constant->value node))))

(defn- void-object-type?
  [type]
  (isa? (o/tid-of-type type) ::Void/%Void))

(defn- unqualified-type
  [type]
  (vary-meta type dissoc :qualifiers))

(defn- c-compatible-object-type
  "Returns the C composite object type for two pointed-to types, or nil.

  C permits an object pointer to combine with void*.  For ordinary objects,
  retain the exact semantic identity; representation compatibility alone is
  not sufficient."
  [lhs-type rhs-type]
  (let [lhs (unqualified-type lhs-type)
        rhs (unqualified-type rhs-type)]
    (cond
      (= lhs rhs)
      lhs

      (void-object-type? lhs)
      lhs

      (void-object-type? rhs)
      rhs

      :else
      nil)))

(defn- c-composite-pointer-type
  [lhs-type rhs-type]
  (when-let [object-type
             (c-compatible-object-type
              (:object-type (meta lhs-type))
              (:object-type (meta rhs-type)))]
    ;; The result points at a type qualified with the union of the arm
    ;; qualifiers, as required by C's conditional operator rules.
    (let [qualifiers (into (o/qualifiers (:object-type (meta lhs-type)))
                           (o/qualifiers (:object-type (meta rhs-type))))
          object-type (if (seq qualifiers)
                        (vary-meta object-type assoc :qualifiers qualifiers)
                        object-type)]
      (Ptr/Ptr object-type))))

(defn- c-conditional-type
  "Selects the result type for the non-arithmetic conditional cases.

  Arithmetic arms are handled by `usual-arithmetic-conversions` before this
  helper is called."
  [lhs-type rhs-type]
  (if (and (c-pointer-type? lhs-type)
           (c-pointer-type? rhs-type))
    (or (c-composite-pointer-type lhs-type rhs-type)
        (throw (ex-info "conditional pointer types are incompatible"
                        {:lhs-type lhs-type :rhs-type rhs-type})))
    (o/ubertype-of lhs-type rhs-type)))

(defn- c-conditional
  [condition then-node else-node]
  (let [then-type (o/type-of then-node)
        else-type (o/type-of else-node)]
    (cond
      (and (c-pointer-type? then-type)
           (null-pointer-constant? else-node))
      (nodes/make-conditional-node condition then-node else-node then-type)

      (and (null-pointer-constant? then-node)
           (c-pointer-type? else-type))
      (nodes/make-conditional-node condition then-node else-node else-type)

      ;; C17 6.5.15 applies the usual arithmetic conversions to arithmetic
      ;; conditional arms, exactly as binary arithmetic and comparisons do.
      (and (arithmetic-type? then-type) (arithmetic-type? else-type))
      (let [{:keys [type lhs rhs]}
            (usual-arithmetic-conversions then-node else-node)]
        (nodes/make-conditional-node condition lhs rhs type))

      :else
      (nodes/make-conditional-node condition then-node else-node
                                   (c-conditional-type then-type else-type)))))

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

(defmethod Algebra/+ [::CInt ::Ptr/Ptr]
  [offset ptr]
  (c-pointer-offset ptr offset))

(defmethod Algebra/+ [::N/Int ::Ptr/Ptr]
  [offset ptr]
  (c-pointer-offset ptr offset))

(defmethod Algebra/- [::Ptr/Ptr ::CInt]
  [ptr offset]
  (c-pointer-offset ptr (Algebra/- offset)))

(defn- c-pointer-difference
  [lhs rhs]
  (let [lhs-object-type (:object-type (meta (o/type-of lhs)))
        rhs-object-type (:object-type (meta (o/type-of rhs)))
        object-type (c-compatible-object-type lhs-object-type rhs-object-type)]
    (when (or (nil? object-type)
              (void-object-type? object-type))
      (throw (ex-info "pointer subtraction requires compatible object pointers"
                      {:lhs-type (o/type-of lhs)
                       :rhs-type (o/type-of rhs)})))
    (let [result-type (N/SInt (target/attr :address-size))
          lhs (o/cast result-type (Ptr/ptrtoint lhs) false)
          rhs (o/cast result-type (Ptr/ptrtoint rhs) false)
          element-size (o/sizeof (target/ctx) object-type)
          difference-node
          (o/make-node
           result-type
           (fn [ctx]
             (let [ctx (ctx/compile-type ctx result-type)
                   ctx (ctx/compile-node ctx lhs)
                   ctx (ctx/compile-node ctx rhs)
                   instruction (ir/sub (ctx/compiled-node ctx lhs)
                                       (ctx/compiled-node ctx rhs)
                                       {})]
               (ctx/compile-instruction ctx instruction)))
           {:class ::pointer-difference})]
      (if (= element-size 1)
        difference-node
        (o/make-node
         result-type
         (fn [ctx]
           (let [ctx (ctx/compile-type ctx result-type)
                 ctx (ctx/compile-node ctx difference-node)
                 instruction (ir/sdiv
                             (ctx/compiled-node ctx difference-node)
                             (ir/const (ctx/compiled-type ctx result-type)
                                       element-size)
                             {})]
             (ctx/compile-instruction ctx instruction)))
         {:class ::pointer-difference})))))

(defmethod Algebra/- [::Ptr/Ptr ::Ptr/Ptr]
  [lhs rhs]
  (c-pointer-difference lhs rhs))

(defn- c-null-pointer
  [ptr integer]
  (if (null-pointer-constant? integer)
    (o/cast (o/type-of ptr) integer false)
    (throw (ex-info "pointer comparison requires integer constant zero"
                    {:pointer-type (o/type-of ptr)
                     :integer integer}))))

(defmacro define-c-null-pointer-comparison
  [multifn]
  `(do
     (defmethod ~multifn [::Ptr/Ptr ::CInt]
       [ptr# integer#]
       (~multifn ptr# (c-null-pointer ptr# integer#)))
     (defmethod ~multifn [::CInt ::Ptr/Ptr]
       [integer# ptr#]
       (~multifn (c-null-pointer ptr# integer#) ptr#))
     (defmethod ~multifn [::Ptr/Ptr ::N/Int]
       [ptr# integer#]
       (~multifn ptr# (c-null-pointer ptr# integer#)))
     (defmethod ~multifn [::N/Int ::Ptr/Ptr]
       [integer# ptr#]
       (~multifn (c-null-pointer ptr# integer#) ptr#))))

(define-c-null-pointer-comparison Eq/=)
(define-c-null-pointer-comparison Eq/!=)

(defmacro define-c-float-binary-op
  [multifn instruction-fn]
  `(do
     ~@(for [dispatch# '([::CFloat ::CFloat]
                          [::CFloat ::CInt]
                          [::CInt ::CFloat]
                          [::CFloat ::N/Number]
                          [::N/Number ::CFloat]
                          [::CFloat ::Bool/Bool]
                          [::Bool/Bool ::CFloat])]
         `(defmethod ~multifn ~dispatch#
            [lhs# rhs#]
            (c-binary-node lhs# rhs# ~instruction-fn)))))

(define-c-float-binary-op Algebra/+ #(ir/fadd %1 %2 {}))
(define-c-float-binary-op Algebra/- #(ir/fsub %1 %2 {}))
(define-c-float-binary-op Algebra/* #(ir/fmul %1 %2 {}))
(define-c-float-binary-op Algebra// #(ir/fdiv %1 %2 {}))

(defmethod Algebra/- [::CFloat]
  [node]
  (let [type (o/type-of node)
        zero (o/cast type 0.0 false)]
    (c-binary-node zero node #(ir/fsub %1 %2 {}))))

(defmacro define-c-float-compare-op
  [multifn predicate]
  `(do
     ~@(for [dispatch# '([::CFloat ::CFloat]
                          [::CFloat ::CInt]
                          [::CInt ::CFloat]
                          [::CFloat ::N/Number]
                          [::N/Number ::CFloat]
                          [::CFloat ::Bool/Bool]
                          [::Bool/Bool ::CFloat])]
         `(defmethod ~multifn ~dispatch#
            [lhs# rhs#]
            (c-comparison-node lhs# rhs#
                               (fn [_type# lhs# rhs#]
                                 (ir/fcmp ~predicate lhs# rhs# {})))))))

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

(defn- c-incrementable-type?
  [type]
  (or (c-int-type? type)
      (c-float-type? type)
      (bool-type? type)
      (and (c-pointer-type? type)
           (let [object-type (:object-type (meta type))]
             (and (not (void-object-type? object-type))
                  (not (isa? (o/tid-of-type object-type) ::Fn/Fn)))))))

(defn- c-update-place
  [place update-fn operation]
  (when-not (Place/place? place)
    (throw (ex-info "C increment/decrement requires a place"
                    {:place place})))
  (let [value-type (o/type-of (Place/load place))]
    (when-not (c-incrementable-type? value-type)
      (throw (ex-info "C increment/decrement requires a C scalar or object pointer place"
                      {:place place
                       :type value-type})))
    (let [one (o/cast (c-int-type) 1 false)]
      (update-fn place #(operation % one)))))

(defn pre-inc!
  "C prefix increment: updates `place` and returns its new value."
  [place]
  (c-update-place place Place/pre-update! Algebra/+))

(defn post-inc!
  "C postfix increment: updates `place` and returns its old value."
  [place]
  (c-update-place place Place/post-update! Algebra/+))

(defn pre-dec!
  "C prefix decrement: updates `place` and returns its new value."
  [place]
  (c-update-place place Place/pre-update! Algebra/-))

(defn post-dec!
  "C postfix decrement: updates `place` and returns its old value."
  [place]
  (c-update-place place Place/post-update! Algebra/-))
