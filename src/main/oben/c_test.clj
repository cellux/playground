(ns oben.c-test
  (:require [midje.sweet :as m]
            [oben.c :as c]
            [oben.core :as oben]
            [oben.core.api :as o]))

(oben/with-target :inprocess
  (let [add (oben/fn ^c/int [^c/int lhs ^c/int rhs]
             (+ lhs rhs))
        constant-add (oben/fn ^c/int []
                       (+ (c/int 7) (c/int 5)))
        constant-float (oben/fn ^c/float []
                         (+ (c/float 1.5) (c/float 2.0)))
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
  (let [float-add (oben/fn ^c/float [^c/float x ^c/int n]
                    (+ x n))
        double-product (oben/fn ^c/double [^c/double x ^c/double y]
                         (* x y))
        float-to-int (oben/fn ^c/int [^c/float x]
                       (cast c/int x))
        nan-not-equal (oben/fn ^bool [^c/double x]
                        (!= x x))]
    (m/fact "C float promotes integer operands to float"
            (float-add 1.5 2) => 3.5)
    (m/fact "C double arithmetic uses double LLVM operations"
            (double-product 1.5 2.0) => 3.0)
    (m/fact "C floating/integer conversions are type-directed"
            (float-to-int 3.75) => 3)
    (m/fact "C floating != treats NaN as unequal"
            (nan-not-equal Double/NaN) => 1)))

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
                    (+ lhs rhs))]
    (m/fact "C int follows the target data model"
            (add 19 23) => 42)
    (m/fact "C long is also target-configurable"
            (long-add 19 23) => 42)
    (m/fact "C float follows the target data model"
            (float-add 1.5 2.0) => 3.5)))
