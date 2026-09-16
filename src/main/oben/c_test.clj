(ns oben.c-test
  (:require [midje.sweet :as m]
            [oben.c :as c]
            [oben.core :as oben]
            [oben.core.api :as o]
            [oben.core.compiler :as compiler]
            [oben.core.target :as target]
            [oben.core.types.Bool :as Bool]
            [oben.core.types.Number :as Number]))

(oben/with-target :inprocess
  (let [add (oben/fn ^c/int [^c/int lhs ^c/int rhs]
             (+ lhs rhs))
        constant-add (oben/fn ^c/int []
                       (+ (c/int 7) (c/int 5)))
        constant-float (oben/fn ^c/float []
                         (+ (c/float 1.5) (c/float 2.0)))
        exact-double (oben/fn ^c/double []
                       (c/double 16777217))
        wide-long (oben/fn ^c/long []
                    (c/long 4294967296.0))
        divide (oben/fn ^c/int [^c/int lhs ^c/int rhs]
                 (/ lhs rhs))
        divide-literal (oben/fn ^c/int [^c/int lhs]
                         (/ lhs 4))
        less (oben/fn ^bool [^c/int lhs ^c/int rhs]
               (< lhs rhs))
        mask (oben/fn ^c/uint [^c/uint lhs ^c/uint rhs]
               (bit-and lhs rhs))
        mixed (oben/fn ^c/uint [^c/int lhs ^c/uint rhs]
                (+ lhs rhs))
        promoted (oben/fn ^c/int [^c/short lhs ^c/short rhs]
                   (+ lhs rhs))]
    (m/fact "plain + dispatches to the C integer implementation"
            (add 19 23) => 42)
    (m/fact "C type names act as constructors"
            (constant-add) => 12)
    (m/fact "floating C type names act as constructors"
            (constant-float) => 3.5)
    (m/fact "C double constants preserve integer precision"
            (exact-double) => 16777217.0)
    (m/fact "C 64-bit integer constants accept wide floating values"
            (wide-long) => 4294967296)
    (m/fact "signed division uses the C signed operation"
            (divide 21 4) => 5)
    (m/fact "C operators accept ordinary integer literals"
            (divide-literal 21) => 5)
    (m/fact "comparisons dispatch from C integer types"
            (less 3 4) => 1)
    (m/fact "unsigned bitwise operations dispatch from C integer types"
            (mask 13 7) => 5)
    (m/fact "mixed signedness uses C common-type rules"
            (mixed 1 2) => 3)
    (m/fact "small C integers undergo integer promotion"
            (promoted 1 2) => 3)))

(oben/with-target :inprocess
  (let [left-shift (oben/fn ^c/int [^c/short value ^c/short count]
                    (bit-shift-left value count))
        signed-right-shift (oben/fn ^c/int [^c/int value ^c/short count]
                             (bit-shift-right value count))
        unsigned-right-shift (oben/fn ^c/uint [^c/uint value ^c/short count]
                               (bit-shift-right value count))
        left-result (o/parse '(bit-shift-left (c/short 1) (c/long 2)))]
    (m/fact "C left shift promotes the operands and returns the promoted lhs type"
            (o/type-of left-result) => (m/exactly (c/int (target/current))))
    (m/fact "C left shift uses the promoted lhs width"
            (left-shift 3 4) => 48)
    (m/fact "C signed right shift uses arithmetic shift"
            (signed-right-shift -16 2) => -4)
    (m/fact "C unsigned right shift uses logical shift"
            (unsigned-right-shift 0x80000000 2) => 0x20000000)))

(oben/with-target :inprocess
  (let [float-add (oben/fn ^c/float [^c/float x ^c/int n]
                    (+ x n))
        double-product (oben/fn ^c/double [^c/double x ^c/double y]
                         (* x y))
        float-bool-add (oben/fn ^c/float [^c/float x]
                         (+ x true))
        float-to-int (oben/fn ^c/int [^c/float x]
                       (cast c/int x))
        float-double-less (oben/fn ^bool []
                            (< (c/float 16777216.0) 16777217.0))
        nan-not-equal (oben/fn ^bool [^c/double x]
                        (!= x x))]
    (m/fact "C float promotes integer operands to float"
            (float-add 1.5 2) => 3.5)
    (m/fact "C double arithmetic uses double LLVM operations"
            (double-product 1.5 2.0) => 3.0)
    (m/fact "C float arithmetic promotes Bool through C int"
            (float-bool-add 1.5) => 2.5)
    (m/fact "C floating/integer conversions are type-directed"
            (float-to-int 3.75) => 3
            (float-to-int -3.75) => -3)
    (m/fact "C mixed float comparisons use the common floating type"
            (float-double-less) => 1)
    (m/fact "C floating != treats NaN as unequal"
            (nan-not-equal Double/NaN) => 1)))

(oben/with-target :dump
  (let [signed (oben/fn ^c/int [^c/double x]
                 (cast c/int x))
        unsigned (oben/fn ^c/uint [^c/double x]
                   (cast c/uint x))
        bit-not-short (oben/fn ^c/int [^c/short x]
                        (bit-not x))
        numeric-signed (oben/fn ^c/int [^Number/%f64 x]
                         (cast c/int x))
        numeric-unsigned (oben/fn ^c/uint [^Number/%f64 x]
                           (cast c/uint x))
        compile (fn [f]
                  (let [fnode ((:parse-for-target (meta f)) (target/current))]
                    (:source (compiler/compile-function (target/current)
                                                        (target/ctx)
                                                        fnode))))]
    (m/fact "C bit-not promotes its operand before lowering"
            (compile bit-not-short) => (m/contains "xor i32"))
    (m/fact "C float-to-signed-integer casts use fptosi"
            (compile signed) => (m/contains "fptosi double"))
    (m/fact "C float-to-unsigned-integer casts use fptoui"
            (compile unsigned) => (m/contains "fptoui double"))
    (m/fact "numeric float-to-signed-integer casts use fptosi"
            (compile numeric-signed) => (m/contains "fptosi double"))
    (m/fact "numeric float-to-unsigned-integer casts use fptoui"
            (compile numeric-unsigned) => (m/contains "fptoui double"))))

(oben/with-target :inprocess
  (let [logical-and (oben/fn ^c/int [^c/int lhs ^c/int rhs]
                       (and lhs rhs))
        logical-or (oben/fn ^c/int [^c/int lhs ^c/int rhs]
                      (or lhs rhs))
        logical-not (oben/fn ^c/int [^c/int value]
                       (not value))
        float-and (oben/fn ^c/int [^c/double lhs ^c/double rhs]
                    (and lhs rhs))
        bool-result (o/parse '(and true false))
        short-circuit (oben/fn ^c/int [^c/int divisor]
                        (and (!= divisor 0)
                             (/ divisor divisor)))]
    (m/fact "Oben bool logical operations return bool"
            (o/type-of bool-result) => (m/exactly Bool/%bool))
    (m/fact "C logical and returns integer zero or one"
            (logical-and 2 3) => 1
            (logical-and 2 0) => 0)
    (m/fact "C logical or returns integer zero or one"
            (logical-or 0 0) => 0
            (logical-or 0 2) => 1)
    (m/fact "C logical not returns integer zero or one"
            (logical-not 0) => 1
            (logical-not 2) => 0)
    (m/fact "C floating-point logical operands use nonzero truth"
            (float-and 2.0 Double/NaN) => 1
            (float-and 0.0 Double/NaN) => 0)
    (m/fact "C logical and short-circuits its right operand"
            (short-circuit 0) => 0
            (short-circuit 2) => 1)))

(oben/with-target {:type :inprocess
                   :attrs {:c-int-size 64
                           :c-long-size 64
                           :c-float-size 64}}
  (let [qualified-int (o/parse '(qualify c/int :const :volatile))]
    (m/fact "qualify resolves portable C types before adding qualifiers"
            (:bits (meta qualified-int)) => 64)
    (m/fact "portable C qualifiers are preserved"
            (:qualifiers (meta qualified-int)) => #{:const :volatile})))

(oben/with-target {:type :inprocess
                   :attrs {:c-int-size 64
                           :c-long-size 64
                           :c-float-size 64}}
  (let [add (oben/fn ^c/int [^c/int lhs ^c/int rhs]
             (+ lhs rhs))
        long-add (oben/fn ^c/long [^c/long lhs ^c/long rhs]
                   (+ lhs rhs))
        float-add (oben/fn ^c/float [^c/float lhs ^c/float rhs]
                    (+ lhs rhs))
        int-type (c/int (target/current))
        long-type (c/long (target/current))
        common-type (o/type-of (o/parse '(+ (c/int 1) (c/long 2))))]
    (m/fact "C int follows the target data model"
            (add 19 23) => 42)
    (m/fact "C long is also target-configurable"
            (long-add 19 23) => 42)
    (m/fact "C float follows the target data model"
            (float-add 1.5 2.0) => 3.5)
    (m/fact "same-width C int and long retain distinct identities"
            int-type =not=> (m/exactly long-type))
    (m/fact "C integer conversions select the higher-rank long type"
            common-type => (m/exactly long-type))
    (m/fact "C pointers to same-width but distinct integer types are incompatible"
            (o/parse '(c/conditional (c/int 1)
                                     (var c/int 0)
                                     (var c/long 0)))
            => (m/throws #"conditional pointer types are incompatible"))))

(oben/with-target :inprocess
  (let [mixed-float (oben/fn ^c/double [^c/int condition]
                       (if condition
                         (c/float 1.5)
                         2))
        float-bool-conditional (oben/fn ^c/float [^c/int condition]
                                 (if condition
                                   true
                                   (c/float 2.0)))
        contextual-integer (oben/fn ^c/int [^c/int condition]
                             (if condition 1 2))
        promoted-integer (oben/fn ^c/long [^c/int condition]
                           (if condition
                             (c/short 4)
                             (c/long 9)))
        lazy (oben/fn ^c/int [^c/int condition ^c/int divisor]
               (if condition
                 1
                 (/ divisor divisor)))
        explicit (oben/fn ^c/int [^c/int condition]
                   (c/conditional condition 7 (c/int 9)))
        pointer-arm (oben/fn ^c/int [^c/int condition]
                      (let [p (var c/int 7)]
                        (deref (if condition p (c/int 0)))))]
    (m/fact "C conditional expressions use the common floating type"
            (mixed-float 1) => 1.5
            (mixed-float 0) => 2.0)
    (m/fact "C float/Bool conditionals convert Bool through C int"
            (float-bool-conditional 1) => 1.0
            (float-bool-conditional 0) => 2.0)
    (m/fact "C-typed conditions give literal arms C integer semantics"
            (contextual-integer 1) => 1
            (contextual-integer 0) => 2)
    (m/fact "C conditional integer arms undergo integer promotion"
            (promoted-integer 1) => 4
            (promoted-integer 0) => 9)
    (m/fact "C conditional expressions evaluate only the selected arm"
            (lazy 1 0) => 1
            (lazy 0 2) => 1)
    (m/fact "C conditionals can also be constructed explicitly"
            (explicit 1) => 7
            (explicit 0) => 9)
    (m/fact "C conditional pointers accept integer constant zero"
            (pointer-arm 1) => 7)))

(oben/with-target :inprocess
  (let [arithmetic (oben/fn ^c/int []
                   (let [v (var c/int 10)]
                     (add= v 5)
                     (sub= v 3)
                     (mul= v 2)
                     (div= v 4)
                     @v))
        remainder (oben/fn ^c/int []
                    (let [v (var c/int 17)]
                      (rem= v 5)
                      @v))
        shifts (oben/fn ^c/uint []
                (let [v (var c/uint 3)]
                  (shift-left= v 4)
                  (shift-right= v 2)
                  @v))
        bitwise (oben/fn ^c/uint []
                 (let [v (var c/uint 13)]
                   (bit-and= v 7)
                   (bit-or= v 8)
                   (bit-xor= v 3)
                   @v))
        narrowing (oben/fn ^c/uint []
                   (let [v (var c/ushort 65530)]
                     (add= v 10)
                     @v))
        floating (oben/fn ^c/float [^c/int n]
                   (let [v (var c/float 1.5)]
                     (add= v n)
                     (mul= v (c/float 2.0))
                     @v))
        pointer-add (oben/fn ^c/int [^c/int offset]
                      (let [values (var (array c/int [4 7 9]))
                            pointer (var (* c/int) (gep values [0 0]))]
                        (add= pointer offset)
                        @@pointer))
        pointer-sub (oben/fn ^c/int [^c/short offset]
                      (let [values (var (array c/int [4 7 9]))
                            pointer (var (* c/int) (gep values [0 2]))]
                        (sub= pointer offset)
                        @@pointer))
        pointer-constant-add (oben/fn ^c/int []
                               (let [values (var (array c/int [4 7 9]))
                                     pointer (var (* c/int) (gep values [0 0]))]
                                 (add= pointer (c/int 1))
                                 @@pointer))
        shorthand (oben/fn ^c/int []
                    (let [v (var c/int 1)]
                      (+= v 1)
                      (-= v 1)
                      (*= v 3)
                      (%= v 2)
                      @v))]
    (m/fact "C compound arithmetic assignments update and return the place"
            (arithmetic) => 6
            (remainder) => 2)
    (m/fact "C compound shift assignments use C shift semantics"
            (shifts) => 12)
    (m/fact "C compound bitwise assignments use C bitwise semantics"
            (bitwise) => 14)
    (m/fact "C compound assignment converts back to the place type"
            (narrowing) => 4)
    (m/fact "C floating compound assignments use the usual arithmetic conversions"
            (floating 2) => 7.0)
    (m/fact "C += and -= support C integer offsets on pointer places"
            (pointer-add 1) => 7
            (pointer-sub 1) => 7
            (pointer-constant-add) => 7)
    (m/fact "readable shorthand aliases remain available"
            (shorthand) => 1)))
