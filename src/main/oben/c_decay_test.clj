(ns oben.c-decay-test
  (:require [midje.sweet :as m]
            [oben.c :as c]
            [oben.core :as oben]
            [oben.core.api :as o]
            [oben.core.compiler :as compiler]
            [oben.core.nodes :as nodes]
            [oben.core.target :as target]
            [oben.core.types.Array :as Array]
            [oben.core.types.Ptr :as Ptr]
            [oben.core.protocols.Place :as Place]
            [oben.core.protocols.Semantics :as Semantics]))

(defn- compile-source [f]
  (let [node ((:parse-for-target (meta f)) (target/current))]
    (:source (compiler/compile-function (target/current) (target/ctx) node))))

(c/with-target :inprocess
  (let [int-type (c/int (target/current))
        row (Array/Array int-type 3)
        matrix (Array/Array row 2)
        place (nodes/%var matrix nil)
        decayed (Semantics/expression-value :c17 place)
        address (Semantics/expression-result :c17 Place/address-of place)
        pointer (nodes/function-parameter 'p (Ptr/Ptr row))
        row-pointer (Semantics/parameter-type :c17 (Ptr/Ptr row))
        matrix-pointer (Semantics/parameter-type :c17 (Ptr/Ptr matrix))]
    (m/facts "array decay tags pointer rvalues with C semantics"
      (o/type-of decayed) => (m/exactly row-pointer)
      (Semantics/expression-value :c17 decayed) => (m/exactly decayed)
      (Semantics/expression-value :c17 address) => (m/exactly address)
      (o/type-of address) => (m/exactly matrix-pointer)
      (o/type-of (Semantics/expression-value :c17 pointer))
      => (m/exactly row-pointer))))

(c/with-target :inprocess
  (let [Row (oben/Array c/int 3)
        Matrix (oben/Array Row 2)
        last-in-second-row (c/fn c/int [(* Row) rows]
                             (load (gep rows [1 2])))
        via-whole-array (c/fn c/int [(* Matrix) matrix]
                          (load (gep matrix [0 1 2])))
        second-in-row (c/fn c/int [(* c/int) row] (get row 1))
        caller (c/fn c/int []
                 (let [a (var Matrix [[(c/int 1) (c/int 2) (c/int 3)]
                                      [(c/int 4) (c/int 5) (c/int 6)]])]
                   (+ (+ (last-in-second-row a)
                         (via-whole-array (address-of a)))
                      (+ (get-in a [1 2]) (second-in-row (at a 1))))))]
    (m/fact "multidimensional arguments decay once; &array does not decay"
      (caller) => 23)))

(c/with-target :inprocess
  (let [head (c/fn c/int [(* c/int) p] (load p))
        aliases (c/fn c/int []
                  (let [x (var c/int (c/int 1))
                        p (address-of x)]
                    (set! p (c/int 17))
                    @x))
        arithmetic (c/fn c/int []
                     (let [a (var (oben/Array c/int 3)
                                  [(c/int 7) (c/int 8) (c/int 9)])]
                       (head (+ a (c/int 1)))))
        assignments (c/fn c/int []
                      (let [x (var c/int (c/int 10))]
                        (add= x (c/int 4))
                        (sub= x (c/int 2))
                        (c/pre-dec! x)
                        (c/post-dec! x)
                        @x))
        explicit-places (c/fn c/int []
                          (let [a (var (oben/Array c/int 2)
                                       [(c/int 2) (c/int 3)])]
                            (set! (at a 1) (c/int 19))
                            (load (at a 1))))
        alias-source (compile-source aliases)]
    (m/facts "pointer results and explicit place operations survive C conversion"
      (aliases) => 17
      (arithmetic) => 8
      (assignments) => 10
      (explicit-places) => 19)
    (m/fact "taking an address does not clone the underlying alloca"
      (count (re-seq #"alloca i32" alias-source)) => 1)))

(c/with-target :inprocess
  (let [Pair (oben/Struct [c/int x c/int y])
        read-fields (c/fn c/int []
                      (let [p (var Pair [(c/int 4) (c/int 5)])]
                        (set! (at p :y) (c/int 8))
                        (+ (get p :x) (:y p))))
        core-read (oben/fn c/int []
                    (let [a (var (oben/Array c/int 2)
                                 [(c/int 11) (c/int 12)])]
                      (get a 1)))]
    (m/facts "aggregate access preserves its receiver in both languages"
      (read-fields) => 12
      (core-read) => 12)))

(c/with-target :inprocess
  (let [f (c/fn c/int [c/int x] x)
        node ((:parse-for-target (meta f)) (target/current))
        signature (:object-type (meta (o/type-of node)))
        int-type (c/int (target/current))]
    (m/fact "parser options are not part of function type identity"
      signature => (m/exactly (c/c-function-type int-type [int-type])))
    (m/fact "LLVM attributes are not part of function type identity"
      (c/c-function-type int-type [int-type] {:linkage :external})
      => (m/exactly signature))))

(c/with-target :inprocess
  (let [int-type (c/int (target/current))
        source-type (Ptr/Ptr (o/qualify int-type :const))
        dest-type (Ptr/Ptr (o/qualify int-type :const))
        ftype (c/c-function-type int-type [dest-type])
        callee (o/make-node (Ptr/Ptr ftype) identity)
        arg (o/make-node source-type identity)]
    (m/fact "equally qualified pointer arguments have compatible object types"
      (o/type-of (nodes/%funcall callee arg)) => (m/exactly int-type))))

(c/with-target :inprocess
  (let [outer (c/fn c/int [c/int unused] {:variadic? true}
                (let [inner (fn c/int [] (c/int 5))]
                  (inner)))
        source (compile-source outer)]
    (m/fact "a nested core function does not inherit C definition options"
      (count (re-seq #"\.\.\." source)) => 1)))

(c/with-target :inprocess
  (let [int-type (c/int (target/current))
        int-pointer (Ptr/Ptr int-type)
        const-int-pointer (Ptr/Ptr (o/qualify int-type :const))
        call-with (fn [parameter-type argument-type]
                    (nodes/%funcall
                     (o/make-node
                      (Ptr/Ptr (c/c-function-type int-type [parameter-type]))
                      identity)
                     (o/make-node argument-type identity)))]
    (m/fact "implicit calls cannot discard pointed-to const"
      (call-with int-pointer const-int-pointer)
      => (m/throws #"incompatible pointer argument"))
    (m/fact "function-pointer compatibility preserves parameter pointee qualifiers"
      (call-with
       (Ptr/Ptr (c/c-function-type int-type [int-pointer]))
       (Ptr/Ptr (c/c-function-type int-type [const-int-pointer])))
      => (m/throws #"incompatible pointer argument"))))

(c/with-target :inprocess
  (let [Row (oben/Array c/int 3)
        Matrix (oben/Array Row 2)
        old-style (c/extern old_style c/int [] {:prototype? false})
        caller (c/fn c/int []
                 (let [a (var Matrix)]
                   (old-style a)))]
    (m/fact "default argument promotions do not decay a matrix twice"
      (compile-source caller)
      => #(.contains % "@old_style([3 x i32]*"))))
