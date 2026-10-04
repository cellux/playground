(ns oben.c
  "C-oriented semantic types and operations.

   Requiring this namespace registers C-specific implementations for core
   operator multimethods. Existing behavior for non-C values is preserved;
   ordinary operator forms acquire C semantics when their operands have C
   semantic types."
  (:refer-clojure :exclude [char double float fn for int long short])
  (:require [clojure.core :as clj]
            [clojure.set :as set]
            [oben.core :as oben]
            [oben.core.api :as o]
            [oben.core.context :as ctx]
            [oben.core.target :as target]
            [oben.c.target :as c-target]
            [oben.core.protocols.Algebra :as Algebra]
            [oben.core.protocols.Assignment :as Assignment]
            [oben.core.protocols.Bitwise :as Bitwise]
            [oben.core.protocols.Logical :as Logical]
            [oben.core.protocols.Conditional :as Conditional]
            [oben.core.protocols.Callable :as Callable]
            [oben.core.protocols.Semantics :as Semantics]
            [oben.core.protocols.Container :as Container]
            [oben.core.protocols.Place :as Place]
            [oben.core.protocols.Eq :as Eq]
            [oben.core.protocols.Ord :as Ord]
            [oben.core.types.Number :as N]
            [oben.core.types.Bool :as Bool]
            [oben.core.types.Ptr :as Ptr]
            [oben.core.types.Array :as Array]
            [oben.core.types.Fn :as Fn]
            [oben.core.types.Void :as Void]
            [oben.core.nodes :as nodes]
            [omkamra.llvm.ir :as ir]))

(def create-target c-target/create)

(clj/defmacro with-target
  "Executes body with a temporary C-compatible target selected."
  [opts & body]
  `(c-target/with-target ~opts ~@body))

;; C's conversion rules are based on type rank, not merely representation
;; width.  In particular, an LP64 target still has distinct `int` and `long`
;; types even though both are i64.  Keep their semantic identity and rank in
;; the type constructor, separately from the LLVM integer width.
(def ^:private rank-char 1)
(def ^:private rank-short 2)
(def ^:private rank-int 3)
(def ^:private rank-long 4)
(def ^:private rank-long-long 5)
(def ^:private rank-float 1)
(def ^:private rank-double 2)

(o/define-typeclass ^:private CInt [:oben/Value]
  [c-type bits signed? rank]
  (o/make-type
   #(ctx/save-ir % [:integer bits])
   {:c-type c-type
    :bits bits
    :signed? signed?
    :rank rank}))

(o/define-typeclass ^:private CFloat [:oben/Value]
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

(o/define-typeclass ^:private CBool [::Bool/Bool]
  [bits]
  (o/make-type
   #(ctx/save-ir % [:integer bits])
   {:bits bits}))

;; C pointers have the same LLVM representation as core pointers, but they
;; carry C expression semantics. Keeping that distinction in the type
;; hierarchy lets C extend shared operator multimethods without intercepting
;; ordinary Oben pointers.
(o/define-typeclass ^:private CPtr [::Ptr/Ptr]
  [object-type]
  (o/make-type
   (Ptr/Ptr object-type)
   {:object-type object-type}))

(defn- c-pointer-type?
  [type]
  (isa? (o/tid-of-type type) ::CPtr))

(defn- pointer-type?
  [type]
  (isa? (o/tid-of-type type) ::Ptr/Ptr))

(defn- c-pointer-type
  "Returns C's semantic pointer type for `type`, preserving its pointee."
  [type]
  (if (c-pointer-type? type)
    type
    (do
      (when-not (pointer-type? type)
        (throw (ex-info "expected a pointer type" {:type type})))
      (CPtr (:object-type (meta type))))))

(defn- c-pointer-node
  "Retags a representation-compatible core pointer as a C pointer value.

   The wrapper compiles the original node rather than using `vary-meta`: the
   latter creates a distinct compiler identity for allocas and globals, which
   would allocate duplicate storage."
  [node]
  (if (c-pointer-type? (o/type-of node))
    node
    (let [type (c-pointer-type (o/type-of node))]
      (o/make-node
       type
       (clj/fn [ctx]
         (let [ctx (ctx/compile-node ctx node)]
           (ctx/save-ir ctx (ctx/compiled-node ctx node))))
       {:class ::c-pointer}))))

;; A C pointer and a core pointer always have identical LLVM representation.
;; Retagging therefore needs no bitcast instruction. C-to-C casts retain the
;; core pointer implementation because differing pointees require an LLVM
;; bitcast.
(defmethod o/cast [::CPtr ::CPtr]
  [type node force?]
  ((get-method o/cast [::Ptr/Ptr ::Ptr/Ptr]) type node force?))

(defmethod o/cast [::CPtr ::Ptr/Ptr]
  [_type node _force?]
  (c-pointer-node node))

(defn- rank-for-bits
  [bits char-size short-size int-size long-size fallback]
  (cond
    (<= bits char-size) rank-char
    (<= bits short-size) rank-short
    (<= bits int-size) rank-int
    (<= bits long-size) rank-long
    :else fallback))

(defn- rank-for-target-bits
  [target bits]
  (rank-for-bits bits
                  (target/attr* target :c-char-size)
                  (target/attr* target :c-short-size)
                  (target/attr* target :c-int-size)
                  (target/attr* target :c-long-size)
                  rank-long-long))

;; `_Bool` has boolean value semantics, but its object size is target-dependent.
;; The C conversion methods below already promote it through c/int.
(o/defportable _Bool
  [target]
  (CBool (target/attr* target :c-bool-size)))

(o/defportable signed-char
  [target]
  (CInt :signed-char
        (target/attr* target :c-char-size)
        true
        rank-char))

(o/defportable unsigned-char
  [target]
  (CInt :unsigned-char
        (target/attr* target :c-char-size)
        false
        rank-char))

(o/defportable char
  [target]
  (CInt :char
        (target/attr* target :c-char-size)
        (target/attr* target :c-char-signed?)
        rank-char))

(o/defportable short
  [target]
  (CInt :short (target/attr* target :c-short-size) true rank-short))

(o/defportable ushort
  [target]
  (CInt :short (target/attr* target :c-short-size) false rank-short))

(o/defportable int
  [target]
  (CInt :int (target/attr* target :c-int-size) true rank-int))

(o/defportable uint
  [target]
  (CInt :int (target/attr* target :c-int-size) false rank-int))

(o/defportable long
  [target]
  (CInt :long (target/attr* target :c-long-size) true rank-long))

(o/defportable ulong
  [target]
  (CInt :long (target/attr* target :c-long-size) false rank-long))

(o/defportable long-long
  [target]
  (CInt :long-long
        (target/attr* target :c-long-long-size)
        true
        rank-long-long))

(o/defportable ulong-long
  [target]
  (CInt :long-long
        (target/attr* target :c-long-long-size)
        false
        rank-long-long))

(o/defportable size_t
  [target]
  (let [bits (target/attr* target :c-size-t-size)]
    (CInt :size_t
          bits
          false
          (target/attr* target :c-size-t-rank))))

(o/defportable ptrdiff_t
  [target]
  (let [bits (target/attr* target :c-ptrdiff-t-size)]
    (CInt :ptrdiff_t
          bits
          true
          (target/attr* target :c-ptrdiff-t-rank))))

(o/defportable float
  [target]
  (CFloat :float (target/attr* target :c-float-size) rank-float))

(o/defportable double
  [target]
  (CFloat :double (target/attr* target :c-double-size) rank-double))

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

(defmethod o/sizeof* ::CBool
  [ctx type]
  (c-sizeof ctx type))

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
       (clj/fn [ctx]
         (let [ctx (ctx/compile-type ctx type)]
           (ctx/save-ir ctx (ir/const (ctx/compiled-type ctx type) value))))))
    (o/make-node
     type
     (clj/fn [ctx]
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
     (clj/fn [ctx]
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
   (clj/fn [ctx]
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

(defmethod o/cast [::CPtr ::CInt]
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
     (clj/fn [ctx]
       (let [ctx (ctx/compile-type ctx type)]
         (ctx/save-ir ctx
                       (ir/const (ctx/compiled-type ctx type)
                                 (if (o/constant->value node) 1 0))))))
    (o/make-node
     type
     (clj/fn [ctx]
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
     (clj/fn [ctx]
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

(defn- number->c-type
  [type]
  (let [bits (:size (meta type))]
    (CInt :core-integer
          bits
          (isa? (o/tid-of-type type) ::N/SInt)
          (rank-for-target-bits (target/current) bits))))

(defn- as-c-node
  "Gives an integer operand its C semantic type before integer promotions."
  [node]
  (let [node (Semantics/expression-value :c17 node)
        type (o/type-of node)]
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
     (clj/fn [ctx]
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
     (clj/fn [ctx]
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
         (clj/fn [ctx]
           (let [ctx (ctx/compile-node ctx lhs)
                 ctx (ctx/compile-node ctx rhs)
                 instruction (instruction-fn type
                                             (ctx/compiled-node ctx lhs)
                                             (ctx/compiled-node ctx rhs))]
             (ctx/compile-instruction ctx instruction)))
         {:class ::comparison})]
    (c-int-result bool-node)))

(defmacro define-c-comparison-op
  [multifn instruction-fn]
  (let [lhs (gensym "lhs")
        rhs (gensym "rhs")
        dispatches [[::CInt ::N/Int]
                    [::N/Int ::CInt]
                    [::CInt ::Bool/Bool]
                    [::Bool/Bool ::CInt]]]
    `(do
       (defmethod ~multifn [::CInt ::CInt]
         [~lhs ~rhs]
         (c-comparison-node ~lhs ~rhs ~instruction-fn))
       ~@(clj/for [[lhs-type rhs-type] dispatches]
           `(defmethod ~multifn [~lhs-type ~rhs-type]
              [~lhs ~rhs]
              (~multifn
               ~(if (= lhs-type ::CInt) lhs `(as-c-node ~lhs))
               ~(if (= rhs-type ::CInt) rhs `(as-c-node ~rhs))))))))

(define-c-comparison-op
  Eq/=
  (clj/fn [_type lhs rhs] (ir/icmp :eq lhs rhs {})))
(define-c-comparison-op
  Eq/!=
  (clj/fn [_type lhs rhs] (ir/icmp :ne lhs rhs {})))

(defmacro define-c-ordered-op
  [multifn signed-predicate unsigned-predicate]
  `(define-c-comparison-op ~multifn
     (clj/fn [type# lhs# rhs#]
       (ir/icmp (if (:signed? (meta type#))
                  ~signed-predicate
                  ~unsigned-predicate)
                lhs# rhs# {}))))

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
     (clj/fn [ctx]
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
  (clj/fn [_type lhs rhs] (ir/shl lhs rhs {})))

(define-c-shift-op Bitwise/bit-shift-right
  (clj/fn [type lhs rhs]
    (if (:signed? (meta type))
      (ir/ashr lhs rhs {})
      (ir/lshr lhs rhs {}))))

(defmethod Bitwise/bit-not [::CInt]
  [node]
  (c-unary-node node
                (clj/fn [node type]
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
  ;; `vary-meta` creates a distinct function object. Preserve identity when
  ;; there are no qualifiers so repeated function-type designators remain
  ;; compatible (notably for function-pointer parameters).
  (if (seq (:qualifiers (meta type)))
    (vary-meta type dissoc :qualifiers)
    type))

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
      (CPtr object-type))))

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
  ;; C context promotes pointer operands into the C pointer subtype before
  ;; selecting their composite type. This also makes explicit `c/conditional`
  ;; usable with an addressable core pointer produced outside a C body.
  (let [then-node (if (pointer-type? (o/type-of then-node))
                    (c-pointer-node then-node)
                    then-node)
        else-node (if (pointer-type? (o/type-of else-node))
                    (c-pointer-node else-node)
                    else-node)
        then-type (o/type-of then-node)
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
(define-c-conditional ::CPtr ::CPtr)
(define-c-conditional ::CPtr ::CInt)
(define-c-conditional ::CInt ::CPtr)
(define-c-conditional ::CPtr ::N/Int)
(define-c-conditional ::N/Int ::CPtr)

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
(define-c-conditioned-conditional ::CPtr)

(defn conditional
  "C's value-producing conditional expression.

  The ordinary Oben `if` form uses C semantics when its operands have C
  semantic types. This explicit entry point accepts a core pointer as a
  convenience and promotes it at the C boundary."
  [condition then-node else-node]
  (Conditional/select
   condition
   (if (pointer-type? (o/type-of then-node))
     (c-pointer-node then-node)
     then-node)
   (if (pointer-type? (o/type-of else-node))
     (c-pointer-node else-node)
     else-node)))

(defn comma
  "C's comma expression: evaluate `lhs`, then return `rhs`."
  [lhs rhs]
  (nodes/%do lhs rhs))

(defn- c-loop-label
  [prefix]
  (keyword (str "oben.c/" prefix "-" (gensym))))

(o/defmacro %break
  []
  (if-let [label (get-in &env [:oben.c/loop :break])]
    (list 'go label)
    (throw (ex-info "break used outside a C loop" {}))))

(o/defmacro %continue
  []
  (if-let [label (get-in &env [:oben.c/loop :continue])]
    (list 'go label)
    (throw (ex-info "continue used outside a C loop" {}))))

(def break %break)
(def continue %continue)

(o/defmacro %for
  [init test update & body]
  (let [head-label (c-loop-label "for-head")
        continue-label (c-loop-label "for-continue")
        break-label (c-loop-label "for-break")
        loop-env (assoc &env
                        :oben.c/loop {:break break-label
                                      :continue continue-label})
        init (if (nil? init) '(nop) init)
        test (if (nil? test) true test)
        update (if (nil? update) '(nop) update)
        body (if (seq body) body ['(nop)])]
    (o/parse
     `(do
        ~init
        (tagbody
          ~head-label
          (when (not ~test)
            (go ~break-label))
          (do ~@body)
          ~continue-label
          ~update
          (go ~head-label)
          ~break-label)
        (nop))
     loop-env)))

(o/defmacro %do-while
  [test & body]
  (let [body-label (c-loop-label "do-while-body")
        continue-label (c-loop-label "do-while-continue")
        break-label (c-loop-label "do-while-break")
        loop-env (assoc &env
                        :oben.c/loop {:break break-label
                                      :continue continue-label})
        body (if (seq body) body ['(nop)])]
    (o/parse
     `(tagbody
        ~body-label
        (do ~@body)
        ~continue-label
        (when ~test
          (go ~body-label))
        ~break-label)
     loop-env)))

(def do-while %do-while)

(o/defmacro %switch
  [control & clauses]
  (when-not (seq clauses)
    (throw (ex-info "C switch requires at least one clause" {})))
  (let [switch-value (gensym "switch-value")
        break-label (c-loop-label "switch-break")
        outer-loop (get &env :oben.c/loop {})
        switch-env (assoc &env
                           :oben.c/loop
                           (assoc outer-loop :break break-label))
        control-node (as-c-node (o/parse control &env))
        control-type (promoted-type (o/type-of control-node))
        control-node (o/cast control-type control-node false)
        clauses (mapv (clj/fn [clause]
                        (when-not (sequential? clause)
                          (throw (ex-info "invalid C switch clause"
                                          {:clause clause})))
                        (let [kind (first clause)]
                          (cond
                            (= kind :case)
                            (when (< (count clause) 2)
                              (throw (ex-info "C case requires a value"
                                              {:clause clause})))

                            (= kind :default)
                            clause

                            :else
                            (throw (ex-info "C switch clauses must start with :case or :default"
                                            {:clause clause})))
                          {:kind kind
                           :value (second clause)
                           :body (if (= kind :case)
                                   (nnext clause)
                                   (next clause))}))
                      clauses)
        default-clauses (filterv #(= :default (:kind %)) clauses)
        _ (when (> (count default-clauses) 1)
            (throw (ex-info "C switch may contain only one :default clause" {})))
        seen-values (atom #{})
        clauses (mapv (clj/fn [clause]
                        (let [label (c-loop-label "switch-case")]
                          (if (= :case (:kind clause))
                            (let [value-node (o/cast control-type
                                                     (as-c-node
                                                      (o/parse (:value clause) &env))
                                                     false)]
                              (when-not (o/constant-node? value-node)
                                (throw (ex-info "C case value must be an integer constant expression"
                                                {:value (:value clause)})))
                              (let [value (o/constant->value value-node)]
                                (when (contains? @seen-values value)
                                  (throw (ex-info "duplicate C switch case value"
                                                  {:value value})))
                                (swap! seen-values conj value)
                                (assoc clause :label label :value-node value-node)))
                            (assoc clause :label label))))
                      clauses)
        default-label (:label (first (filter #(= :default (:kind %)) clauses)))
        dispatch (concat
                  (map (clj/fn [{:keys [label value-node]}]
                         `(when (= ~switch-value ~value-node)
                            (go ~label)))
                       (filter #(= :case (:kind %)) clauses))
                  [`(go ~(or default-label break-label))])
        arms (mapcat (clj/fn [{:keys [label body]}]
                       [label (if (seq body)
                                `(do ~@body)
                                '(nop))])
                     clauses)]
    (o/parse
     `(let [~switch-value ~control-node]
        (tagbody
          ~@dispatch
          ~@arms
          ~break-label))
     switch-env)))

(def switch %switch)

(defn- c-pointer-offset
  [ptr offset]
  ;; `nodes/%gep` uses core integer indices.  Preserve C signedness during the
  ;; conversion so negative offsets remain negative at the pointer width.
  (c-pointer-node
   (nodes/%gep ptr [(o/cast (N/UInt (target/attr :address-size)) offset false)])))

(defmethod Algebra/+ [::CPtr ::CInt]
  [ptr offset]
  (c-pointer-offset ptr offset))

(defmethod Algebra/+ [::CInt ::CPtr]
  [offset ptr]
  (c-pointer-offset ptr offset))

(defmethod Algebra/+ [::N/Int ::CPtr]
  [offset ptr]
  (c-pointer-offset ptr offset))

(defmethod Algebra/- [::CPtr ::CInt]
  [ptr offset]
  (c-pointer-offset ptr (Algebra/- offset)))

;; C defines pointer ordering and subtraction only for pointers into the same
;; array object (including its one-past position).  We intentionally do not
;; track provenance here: raw LLVM address operations produce the required
;; result for defined cases, while cross-object uses have undefined behavior.
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
    (let [result-type (ptrdiff_t (target/current))
          lhs (o/cast result-type (Ptr/ptrtoint lhs) false)
          rhs (o/cast result-type (Ptr/ptrtoint rhs) false)
          element-size (o/sizeof (target/ctx) object-type)
          difference-node
          (o/make-node
           result-type
           (clj/fn [ctx]
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
         (clj/fn [ctx]
           (let [ctx (ctx/compile-type ctx result-type)
                 ctx (ctx/compile-node ctx difference-node)
                 instruction (ir/sdiv
                             (ctx/compiled-node ctx difference-node)
                             (ir/const (ctx/compiled-type ctx result-type)
                                       element-size)
                             {})]
             (ctx/compile-instruction ctx instruction)))
         {:class ::pointer-difference})))))

(defmethod Algebra/- [::CPtr ::CPtr]
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
     (defmethod ~multifn [::CPtr ::CInt]
       [ptr# integer#]
       (~multifn ptr# (c-null-pointer ptr# integer#)))
     (defmethod ~multifn [::CInt ::CPtr]
       [integer# ptr#]
       (~multifn (c-null-pointer ptr# integer#) ptr#))
     (defmethod ~multifn [::CPtr ::N/Int]
       [ptr# integer#]
       (~multifn ptr# (c-null-pointer ptr# integer#)))
     (defmethod ~multifn [::N/Int ::CPtr]
       [integer# ptr#]
       (~multifn (c-null-pointer ptr# integer#) ptr#))))

(define-c-null-pointer-comparison Eq/=)
(define-c-null-pointer-comparison Eq/!=)

(defmacro define-c-float-binary-op
  [multifn instruction-fn]
  `(do
     ~@(clj/for [dispatch# '([::CFloat ::CFloat]
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
     ~@(clj/for [dispatch# '([::CFloat ::CFloat]
                          [::CFloat ::CInt]
                          [::CInt ::CFloat]
                          [::CFloat ::N/Number]
                          [::N/Number ::CFloat]
                          [::CFloat ::Bool/Bool]
                          [::Bool/Bool ::CFloat])]
         `(defmethod ~multifn ~dispatch#
            [lhs# rhs#]
            (c-comparison-node lhs# rhs#
                               (clj/fn [_type# lhs# rhs#]
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
     ~@(clj/for [dispatch# '([::CInt ::CInt]
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
                         [::CPtr ::CPtr]
                         [::CPtr ::CInt]
                         [::CInt ::CPtr]
                         [::CPtr ::CFloat]
                         [::CFloat ::CPtr]
                         [::CPtr ::Bool/Bool]
                         [::Bool/Bool ::CPtr]
                         [::CPtr ::N/Number]
                         [::N/Number ::CPtr])]
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

(defmethod Logical/not [::CPtr]
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

(defn- c-default-argument-promotion
  "Applies C17's default argument promotions to one argument."
  [node]
  (let [node (Semantics/expression-value :c17 node)
        type (o/type-of node)]
    (cond
      (c-int-type? type)
      (o/cast (promoted-type type) node false)

      (bool-type? type)
      (let [c-node (as-c-node node)]
        (o/cast (promoted-type (o/type-of c-node)) c-node false))

      (isa? (o/tid-of-type type) ::N/Int)
      (let [c-node (as-c-node node)]
        (o/cast (promoted-type (o/type-of c-node)) c-node false))

      (c-float-type? type)
      (if (= 32 (:bits (meta type)))
        (o/cast (double (target/current)) node false)
        node)

      (isa? (o/tid-of-type type) ::N/FP)
      (o/cast (double (target/current)) node false)

      :else
      node)))

(defn c-local-declaration-type
  "Returns the C semantic type of a local object declaration.

   Unlike function parameters, arrays and functions are not adjusted here;
   only explicitly declared pointer objects acquire the C pointer subtype."
  [type]
  (if (pointer-type? type)
    (c-pointer-type type)
    type))

(defmethod Semantics/type-argument [:c17 ::Ptr/Ptr]
  [_ op index type]
  (if (and (= op nodes/%var) (zero? index))
    (c-local-declaration-type type)
    type))

(defmethod Semantics/type-argument [:c17 :oben/Any]
  [_ _ _ type]
  type)

;; C17 6.7.6.3 adjusts array parameters to pointers to their first element and
;; function parameters to pointers to function. Top-level qualifiers on a
;; parameter do not affect calls.
(defmethod Semantics/parameter-type [:c17 ::Array/Array]
  [_ type]
  (CPtr (:element-type (meta (unqualified-type type)))))

(defmethod Semantics/parameter-type [:c17 ::Fn/Fn]
  [_ type]
  (CPtr (unqualified-type type)))

(defmethod Semantics/parameter-type [:c17 ::Ptr/Ptr]
  [_ type]
  (c-pointer-type (unqualified-type type)))

(defmethod Semantics/parameter-type [:c17 :oben/Any]
  [_ type]
  (unqualified-type type))

(defn- invalid-c-return-type
  [type]
  (throw (ex-info "a C function cannot return an array or function type"
                  {:return-type type})))

(defmethod Semantics/return-type [:c17 ::Array/Array]
  [_ type]
  (invalid-c-return-type type))

(defmethod Semantics/return-type [:c17 ::Fn/Fn]
  [_ type]
  (invalid-c-return-type type))

(defmethod Semantics/return-type [:c17 ::Ptr/Ptr]
  [_ type]
  (c-pointer-type type))

(defmethod Semantics/return-type [:c17 :oben/Any]
  [_ type]
  type)

(defn- array-pointer?
  [type]
  ;; Addressable C arrays originate as core pointers to array storage. They
  ;; become C pointers only after the array-to-pointer conversion below.
  (and (pointer-type? type)
       (isa? (o/tid-of-type (:object-type (meta type))) ::Array/Array)))

(defn decay-array
  "Explicit C array-to-pointer conversion.

   `node` must designate an addressable array object. The result is a pointer
   to its first element; multidimensional arrays consequently decay by exactly
   one level. Core Oben deliberately does not apply this conversion itself."
  [node]
  (let [type (o/type-of node)]
    (when-not (array-pointer? type)
      (throw (ex-info "C array decay requires an addressable array object"
                      {:node node :type type})))
    ;; A pointer to an array object is how Oben represents addressable array
    ;; storage. `gep [0 0]` is its C decay to a pointer to element zero.
    (let [index-type (N/UInt (target/attr :address-size))
          zero (N/make-constant-number-node index-type 0)]
      ;; GEP normally denotes an addressable subobject. This particular GEP is
      ;; C's array value conversion, whose result is a pointer rvalue; retain
      ;; that distinction so a later conversion pass does not load element 0.
      (vary-meta (c-pointer-node (nodes/%gep node [zero zero]))
                 assoc :oben.c/value-category :rvalue))))

(defn decay-function
  "Explicit C function-to-pointer conversion.

   LLVM and Oben represent named function designators as `Ptr(Fn ...)`, so the
   conversion has no IR instruction to emit. It exists as an explicit C-level
   operation and validates that its operand is a function designator."
  [node]
  (if (o/fnode? node)
    (c-pointer-node node)
    (throw (ex-info "C function decay requires a function designator"
                    {:node node
                     :type (when (o/node? node) (o/type-of node))}))))

(declare c-compatible-call-type?)

(defn- c-compatible-function-type?
  [from-type to-type]
  (let [{from-return :return-type from-params :param-types
         from-prototype? :prototype? from-variadic? :variadic?} (meta from-type)
        {to-return :return-type to-params :param-types
         to-prototype? :prototype? to-variadic? :variadic?} (meta to-type)]
    (and (= from-prototype? to-prototype?)
         (= from-variadic? to-variadic?)
         (= (count from-params) (count to-params))
         (c-compatible-call-type? from-return to-return)
         (every? true? (map c-compatible-call-type? from-params to-params)))))

(defn- c-compatible-call-type?
  "C type compatibility used for function signatures.

   Function types can have different constructor options but equivalent call
   signatures. Compare those structurally; object types retain their semantic
   identity, independent of the metadata wrapper used for qualifiers."
  [from-type to-type]
  (let [from-type (unqualified-type from-type)
        to-type (unqualified-type to-type)]
    (cond
      (= (o/tid-of-type from-type) (o/tid-of-type to-type)) true
      (and (isa? (o/tid-of-type from-type) ::Fn/Fn)
           (isa? (o/tid-of-type to-type) ::Fn/Fn))
      (c-compatible-function-type? from-type to-type)
      (and (c-pointer-type? from-type) (c-pointer-type? to-type))
      ;; Top-level parameter qualifiers were removed by the parameter-type
      ;; semantics, but qualifiers on the pointed-to type remain part of a function
      ;; pointer's compatible signature.
      (and (= (o/qualifiers (:object-type (meta from-type)))
              (o/qualifiers (:object-type (meta to-type))))
           (c-compatible-call-type? (:object-type (meta from-type))
                                    (:object-type (meta to-type))))
      :else false)))

(defn- c-compatible-pointer-parameter?
  [from-type to-type]
  (let [from-object (unqualified-type (:object-type (meta from-type)))
        to-object (unqualified-type (:object-type (meta to-type)))
        from-qualifiers (o/qualifiers (:object-type (meta from-type)))
        to-qualifiers (o/qualifiers (:object-type (meta to-type)))]
    (and (or (c-compatible-call-type? from-object to-object)
             (and (not (isa? (o/tid-of-type from-object) ::Fn/Fn))
                  (not (isa? (o/tid-of-type to-object) ::Fn/Fn))
                  (or (void-object-type? from-object)
                      (void-object-type? to-object))))
         ;; A call may add pointed-to qualifiers but must not discard them.
         (set/subset? from-qualifiers to-qualifiers))))

(defn- c-object-place?
  "Whether `node` is one of Oben's address-producing C object expressions.

   Pointer values returned from calls are not included: treating every pointer
   as an lvalue would incorrectly dereference a pointer rvalue supplied to a
   scalar parameter."
  [node]
  (and (not= :rvalue (:oben.c/value-category (meta node)))
       (contains? #{:oben/var :oben/global :oben/gep}
                  (o/class-of-node node))))

(defn- c-pointer-rvalue
  "Keep the original node as a dependency rather than cloning its compiler.
   Cloning an alloca/global node with vary-meta would allocate a second object
   instead of returning the address of the first one."
  [node]
  (if (= :rvalue (:oben.c/value-category (meta node)))
    node
    (o/make-node (o/type-of node)
      (clj/fn [ctx]
        (let [ctx (ctx/compile-node ctx node)]
          (ctx/save-ir ctx (ctx/compiled-node ctx node))))
      {:class ::pointer-rvalue
       :oben.c/value-category :rvalue})))

(defmethod Semantics/expression-result [:c17 ::Ptr/Ptr]
  [_ op node]
  (if (contains? #{Place/address-of nodes/%gep Algebra/+ Algebra/-} op)
    (c-pointer-rvalue (c-pointer-node node))
    node))

(defmethod Semantics/expression-result [:c17 :oben/Any]
  [_ _ node]
  node)

(def ^:private c-place-operand-operators
  #{Place/address-of Place/load Place/store! Ptr/%deref
    Place/pre-update! Place/post-update! Place/update!
    Container/get Container/get-in Container/at Container/at-in
    Container/assoc! Container/assoc-in!
    nodes/%set! nodes/%gep
    pre-inc! post-inc! pre-dec! post-dec!
    Assignment/add-assign Assignment/sub-assign
    Assignment/mul-assign Assignment/div-assign
    Assignment/rem-assign Assignment/shift-left-assign
    Assignment/shift-right-assign Assignment/bit-and-assign
    Assignment/bit-xor-assign Assignment/bit-or-assign
    decay-array decay-function})

(defmethod Semantics/operand-context :c17
  [_ op index]
  (if (and (zero? index) (contains? c-place-operand-operators op))
    :place
    :value))

(defn- c-pointer-expression-value
  [node]
  (let [type (o/type-of node)]
    (cond
      ;; A pointer-to-array value is not an array designator. In particular,
      ;; &array, pointer parameters, and the result of a previous decay must
      ;; retain their pointer type on subsequent conversion passes.
      (and (c-object-place? node) (array-pointer? type))
      (decay-array node)

      (o/fnode? node)
      (decay-function node)

      ;; C local/global storage is represented by a core pointer. Loading it
      ;; produces either a scalar value or a C pointer rvalue.
      (c-object-place? node)
      (let [value (Place/load node)]
        (if (pointer-type? (o/type-of value))
          (c-pointer-node value)
          value))

      ;; Pointer rvalues crossing into C acquire the C pointer subtype.
      :else
      (c-pointer-node node))))

(defmethod Semantics/expression-value [:c17 ::Ptr/Ptr]
  [_ node]
  (c-pointer-expression-value node))

(defmethod Semantics/expression-value [:c17 :oben/Any]
  [_ node]
  node)

(defn- c-fixed-argument-conversion
  [parameter-type argument]
  (let [parameter-type (Semantics/parameter-type :c17 parameter-type)
        argument (Semantics/expression-value :c17 argument)
        argument-type (o/type-of argument)]
    (cond
      (and (c-pointer-type? parameter-type)
           (c-pointer-type? argument-type))
      (do
        (when-not (c-compatible-pointer-parameter? argument-type parameter-type)
          (throw (ex-info "incompatible pointer argument in C function call"
                          {:argument-type argument-type
                           :parameter-type parameter-type})))
        (o/cast parameter-type argument false))

      :else
      (o/cast (unqualified-type parameter-type) argument false))))

(defn c-function-type
  "Constructs a function type using C17 call semantics."
  ([return-type param-types]
   (c-function-type return-type param-types {}))
  ([return-type param-types opts]
   (let [opts (assoc opts :semantics :c17)]
     (when (and (:variadic? opts) (empty? param-types))
       (throw (ex-info "a C variadic function requires a named parameter before ..."
                       {:return-type return-type :param-types param-types})))
     (Fn/function-type return-type param-types opts))))

(defn make-extern
  "Creates a target-portable declaration for an external C function."
  [name return-type param-types lexical-bindings opts]
  (with-meta
   (memoize
    (clj/fn [target]
      (let [env (assoc lexical-bindings :oben/target target)
            return-type (o/parse return-type env)
            param-types (o/parse param-types env)]
        (nodes/make-external-function
         name
         (c-function-type return-type param-types opts)
         opts))))
   {:kind :oben/PORTABLE}))

(defn- c17-call-arguments
  [fnode args]
  (when-not (o/fnode? fnode)
    (throw (ex-info "cannot call a non-function value"
                    {:callee fnode
                     :type (when (o/node? fnode)
                             (o/type-of fnode))})))
  (let [ftype (-> fnode o/type-of meta :object-type)
        {:keys [param-types prototype? variadic?]} (meta ftype)
        fixed-count (count param-types)
        arg-count (count args)]
    (cond
      (and prototype? (not variadic?) (not= fixed-count arg-count))
      (throw (ex-info "invalid number of arguments in C function call"
                      {:expected fixed-count
                       :actual arg-count
                       :callee fnode}))

      (and prototype? variadic? (< arg-count fixed-count))
      (throw (ex-info "not enough arguments in C variadic function call"
                      {:expected-at-least fixed-count
                       :actual arg-count
                       :callee fnode})))
    (let [fixed-args (if prototype?
                       (mapv c-fixed-argument-conversion
                             param-types
                             (take fixed-count args))
                       [])
          variadic-args (if (and prototype? variadic?)
                          (drop fixed-count args)
                          (if prototype?
                            []
                            args))]
      (into fixed-args (map c-default-argument-promotion variadic-args)))))

(defmethod Callable/call :c17
  [fnode args]
  (nodes/make-funcall-node fnode (c17-call-arguments fnode args)))

(clj/defmacro fn
  "Defines an Oben function whose calls use C17 argument semantics.

   An optional map immediately after the parameter vector accepts `:variadic?`
   and LLVM declaration attributes. Variadic function bodies may use their
   fixed parameters; C varargs access (`va_list`) is not yet modeled."
  [& decl]
  (let [[signature body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta signature))
        [opts body] (if (map? (first body))
                      [(first body) (next body)]
                      [{} body])
        opts (assoc opts :semantics :c17)]
    (when (and (:variadic? opts) (empty? params))
      (throw (ex-info "a C variadic function requires a named parameter before ..."
                      {:options opts})))
    ;; Core owns function declaration parsing and option handling. C only
    ;; supplies its semantic mode and retains the C-specific variadic rule.
    `(oben/fn ~@signature ~opts ~@body)))

(clj/defmacro extern
  "Declares an external C function.

   Example: `(c/extern printf c/int [(* c/char)] {:variadic? true})`."
  [name return-type param-types & [opts]]
  `(oben/with-lexical-bindings bindings#
     (make-extern '~name '~return-type '~param-types bindings# ~(or opts {}))))

(clj/defmacro defextern
  "Defines a named target-portable external C declaration."
  [name return-type param-types & [opts]]
  `(def ~name (extern ~name ~return-type ~param-types ~@(when opts [opts]))))

;; Keep this alias after the macro definitions above so it does not shadow
;; clojure.core/for while this namespace is being compiled.
(def for %for)
