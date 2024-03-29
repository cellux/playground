(ns oben.core-test
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.walk :as walk]
   [midje.sweet :as m]
   [oben.core :as oben]
   [oben.core.api :as o]
   [oben.core.context :as ctx]
   [oben.core.target :as target]
   [oben.core.math :as math]
   [oben.core.types.Void :as Void]
   [oben.core.types.Unseen :as Unseen]
   [oben.core.types.Number :as Number]
   [oben.core.types.Ptr :as Ptr :refer [Ptr]]
   [oben.core.types.Aggregate :as Aggregate]
   [oben.core.types.Array :as Array :refer [Array]]
   [oben.core.types.Struct :as Struct :refer [Struct]]
   [oben.core.types.Fn :as Fn :refer [Fn]])
  (:use
   [midje.repl]))

(defn compile-type
  [t]
  (let [ctx (ctx/create)]
    (-> ctx
        (ctx/compile-type t)
        (ctx/compiled-type t))))

(m/facts
 "type constructors"
 (m/fact (compile-type (Void/%Void)) => :void)
 (m/fact (compile-type (Unseen/%Unseen)) => :void)
 (m/fact (compile-type (Number/UInt 32)) => [:integer 32])
 (m/fact (compile-type (Number/UInt 8)) => [:integer 8])
 (m/fact (compile-type (Number/SInt 16)) => [:integer 16])
 (m/fact (compile-type (Number/FP 32)) => :float)
 (m/fact (compile-type (Number/FP 64)) => :double)
 (m/fact (compile-type (Ptr (Number/SInt 16))) => [:ptr [:integer 16]])
 (m/fact (compile-type (Array (Number/SInt 16) 8)) => [:array [:integer 16] 8])
 (m/fact (compile-type (Struct [{:name 'x :type (Number/FP 32)}
                                {:name 'y :type (Number/FP 32)}
                                {:name 'z :type (Number/FP 32)}]
                               {:name 'Vec3}))
         => [:struct 'Vec3.0.0 [:float :float :float]])
 (m/fact (compile-type (Fn (Number/UInt 64)
                           [(Number/FP 64) (Number/FP 32)]))
         => [:fn [:integer 64] [:double :float]]))

(m/facts
 "primitive types"
 (m/fact (compile-type Void/%void) => :void)
 (m/fact (compile-type Unseen/%unseen) => :void)
 (m/fact (compile-type Number/%u1) => [:integer 1])
 (m/fact (compile-type Number/%u8) => [:integer 8])
 (m/fact (compile-type Number/%u16) => [:integer 16])
 (m/fact (compile-type Number/%u32) => [:integer 32])
 (m/fact (compile-type Number/%u64) => [:integer 64])
 (m/fact (compile-type Number/%s1) => [:integer 1])
 (m/fact (compile-type Number/%s8) => [:integer 8])
 (m/fact (compile-type Number/%s16) => [:integer 16])
 (m/fact (compile-type Number/%s32) => [:integer 32])
 (m/fact (compile-type Number/%s64) => [:integer 64])
 (m/fact (compile-type Number/%f32) => :float)
 (m/fact (compile-type Number/%f64) => :double))

(m/facts
 "type constructors memoize the types they return"
 (m/fact (Number/UInt 32) => (m/exactly (Number/UInt 32)))
 (let [ft (Fn (Number/UInt 64) [(Number/FP 64) (Number/FP 32)])
       t (Array (Ptr ft) 10)]
   (m/fact (Array (Ptr ft) 10) => (m/exactly t))))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [] (+ 5 2))]
    (m/fact
     "a type tag on the param vector defines the return type of the function"
     (f) => 7)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y] (+ x y))]
    (m/fact
     "a type tag on a parameter defines its type"
     (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

;; to denote the type of a parameter or return value we can use:
;;
;; 1. type tag => ^u32
;; 2. type symbol => u32
;; 3. type constructor => (Number/UInt 32)
;; 4. type value => Number/%u32
;; 5. metadata map => {:tag u32}

;; if the name of a type or type constructor is not one of the
;; builtins (u32, f64, Struct, Array, etc.), it should be either a
;; symbol bound locally via Clojure let or the name of a Clojure var

(oben/with-target :inprocess
  (let [f (oben/fn u32 [] (+ 5 2))]
    (m/fact (f) => 7))
  (let [f (oben/fn (Number/UInt 32) [] (+ 5 2))]
    (m/fact (f) => 7))
  (let [f (oben/fn u32 [u32 x u32 y] (+ x y))]
    (m/fact (f 5 3) => 8))
  (let [f (oben/fn u32 [f32 x f32 y] (+ x y))]
    (m/fact (f 5.7 3.2) => 8)
    (m/fact (f 5.7 3.5) => 9))
  (let [u32-type-alias-1 'u32
        u32-type-alias-2 {:tag (Number/UInt 32)}
        f (oben/fn ^u32 [u32 a
                         ^u32 b
                         (Number/UInt 32) c
                         Number/%u32 d
                         ^{:tag u32} e
                         {:tag Number/%u32} f
                         ^u32-type-alias-1 g
                         u32-type-alias-2 h]
            (+ a b c d e f g h))
        numbers [5 3 1 6 9 2 4 11]]
    (m/fact (apply f numbers)
            => (apply + numbers))))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u8 x]
            (bit-not x))]
    (m/fact (f 0) => 255))
  (let [f (oben/fn ^u32 [^u16 x]
            (bit-not x))]
    (m/fact (f 0) => 65535))
  (let [u8 'u16
        return-size 32
        f (oben/fn (Number/UInt return-size) [^u8 x]
            (bit-not x))]
    (m/fact
     "symbols inside type designators can be resolved via bindings in the local environment"
     (f 0) => 65535)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (+ x y)
            (+ 5 2))]
    (m/fact
     "functions without an explicit return form return the result of
     the last expression of their body"
     (f 1 2) => 7)
    (m/fact (f 5 3) => 7)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (return (+ x y))
            (+ 5 2))]
    (m/fact (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            1)]
    (m/fact (f) => 1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u8 []
            1)]
    (m/fact (f) => 1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x]
            x)]
    (m/fact (f 5) => 5)))

(oben/defn add
  ^u32 [^u32 x ^u32 y]
  (+ x y))

(m/facts
 (m/fact
  "Calling Oben functions from Clojure results in their compilation
  into the default target followed by invocation through the function
  pointer acquired from LLVM. The invoker provides automatic two-way
  conversion between Clojure values and LLVM types. Functions are
  compiled on their first invocation and then memoized by target so
  subsequent calls within the same target do not incur a compilation
  overhead."
  (add (add 1 2) (add 7 -2)) => 8))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u16 x ^u8 y] (+ x y))]
    (m/fact
     "automatic zero extension"
     (f 1 2) => 3)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (do)
            (do (+ x y))
            (do (+ x y)
                (+ 3 9 y)))]
    (m/fact (f 1 2) => 14)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u16 [^u32 x]
            (cast u16 x))]
    (m/fact
     "narrowing casts are rejected by default"
     (f 65535) => (m/throws #"rejected narrowing UInt->UInt conversion"))))

(oben/with-target :inprocess
  (let [f (oben/fn ^u16 [^u32 x]
            (cast! u16 x))]
    (m/fact
     "forced casts"
     (f 65535) => -1
     (f 65536) => 0
     (f 65537) => 1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^u32 x]
            (let [g (fn ^u32 [^f32 x] (/ x 4))]
              (g x)))]
    (m/fact
     "call to let-bound function"
     (f 60) => 15.0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (let [g (fn ^u32 [^u16 x ^u8 y] (+ x y))]
              (g (cast! u16 x) (cast! u8 y))))]
    (m/fact
     (f 6 3) => 9.0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (* x y))
        g (oben/fn ^f32 [^u32 x ^u32 y]
            (f (+ x y) (- x y)))]
    (m/fact
     "call to Oben function defined in Clojure lexical environment"
     (g 12 5) => 119.0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (* x y))
        g (oben/fn ^f32 [^s32 x ^s32 y]
            (let [f (fn ^f32 [^f32 x ^f32 y]
                      (/ x y))]
              (f (+ x y) (- x y))))]
    (m/fact
     "Oben function defined in Clojure lexical environment shadowed by local binding"
     (g 5 9) => -3.5)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (+ x y))]
    (m/fact (f 6 3) => 9))
  (let [f (oben/fn ^s32 [^u32 x ^s32 y]
            (+ x y))]
    (m/fact (f 6 -3) => 3))
  (let [f (oben/fn ^s32 [^s32 x ^u32 y]
            (+ x y))]
    (m/fact (f -3 6) => 3))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (+ x y))]
    (m/fact (f -3 -6) => -9)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (+ x y))]
    (m/fact (f 6.5 3.25) => 9.75))
  (let [f (oben/fn ^f32 [^f32 x ^u32 y]
            (+ x y))]
    (m/fact (f 6.5 3) => 9.5))
  (let [f (oben/fn ^f32 [^u32 x ^f32 y]
            (+ x y))]
    (m/fact (f 3 6.5) => 9.5))
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (+ x y))]
    (m/fact (f 3 6) => 9.0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (+ x y))]
    (m/fact (f 6.5 -1.25) => 5.25))
  (let [f (oben/fn ^f32 [^f32 x ^s32 y]
            (+ x y))]
    (m/fact (f 6.5 -3) => 3.5))
  (let [f (oben/fn ^f32 [^s32 x ^f32 y]
            (+ x y))]
    (m/fact (f -3 6.5) => 3.5))
  (let [f (oben/fn ^f32 [^s32 x ^s32 y]
            (+ x y))]
    (m/fact (f -3 -6) => -9.0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (+ x y))]
    (m/fact (f 6.5 -1.25) => 5.25))
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (- x y))]
    (m/fact (f 6.5 -1.25) => 7.75))
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (* x y))]
    (m/fact (f 6.5 -1.25) => -8.125))
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (/ x y))]
    (m/fact (f 6.5 -3.25) => -2.0)))

(m/fact (o/parse 'u8) => (m/exactly Number/%u8))
(m/fact (o/parse 's8) => (m/exactly Number/%s8))

(m/fact (o/type-of (o/parse '(u8 0))) => (m/exactly (Number/UInt 8)))
(m/fact (o/type-of (o/parse '(u8 0))) => (m/exactly Number/%u8))

(m/fact (o/type-of (o/parse '(s8 0))) => (m/exactly (Number/SInt 8)))
(m/fact (o/type-of (o/parse '(s8 0))) => (m/exactly Number/%s8))

(m/fact (o/type-of (o/parse 0)) => (m/exactly Number/%u1))
(m/fact (o/type-of (o/parse 1)) => (m/exactly Number/%u1))
(m/fact (o/type-of (o/parse -1)) => (m/exactly Number/%s8))
(m/fact (o/type-of (o/parse 5)) => (m/exactly Number/%u8))
(m/fact (o/type-of (o/parse -5)) => (m/exactly Number/%s8))

(m/fact (o/type-of (o/parse '(u8 -5))) => (m/exactly Number/%u8))
(m/fact
 "reinterpreting a signed value as unsigned does not change its bit pattern"
 (o/constant->value (o/parse '(u8 -5))) => -5)

(m/facts
 (m/fact
  "negating an UInt turns it into an SInt"
  (o/type-of (o/parse '(- 5))) => (m/exactly Number/%s8))
 (m/fact
  (o/type-of (o/parse '(- 200))) => (m/exactly Number/%s16)))

(oben/with-target :inprocess
  (let [f (oben/fn ^s32 [^u32 x]
            (- x))]
    (m/fact (f 5) => -5))
  (let [f (oben/fn ^s32 [^s32 x]
            (- x))]
    (m/fact (f -5) => 5))
  (let [f (oben/fn ^f32 [^f32 x]
            (- x))]
    (m/fact (f 5.25) => -5.25)
    (m/fact (f -5.25) => 5.25)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (% x y))]
    (m/fact (f 67 7) => 4))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (% x y))]
    (m/fact (f -17 5) => -2)
    (m/fact (f 17 -5) => 2))
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (% x y))]
    (m/fact (f -17 5) => -2.0)
    (m/fact (f 17 -5) => 2.0)
    (m/fact (f 7.25 3.5) => 0.25)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and x y))]
    (m/fact (f 0x1234 0xff) => 0x34)
    (m/fact (f 0x1234 0xff00) => 0x1200))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-and x y))]
    (m/fact (f -2 0xff) => 0xfe)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-or x y))]
    (m/fact (f 0x1234 0xff) => 0x12ff)
    (m/fact (f 0x1234 0xff00) => 0xff34))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-or x y))]
    (m/fact (f -2 0xff) => -1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-xor x y))]
    (m/fact (f 0x1234 0xff) => (+ 0x1200 (- 0xff 0x34)))
    (m/fact (f 0x1234 0xff00) => (+ 0x34 (- 0xff00 0x1200)))))

;; TODO bool should be a separate type (not an alias of u1)

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (= x y))]
    (m/fact (f -3 -5) => 0)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 3 3) => 1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (!= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (!= x y))]
    (m/fact (f -3 -5) => 1)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (!= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 3 3) => 0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (< x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (< x y))]
    (m/fact (f -3 5) => 1)
    (m/fact (f 5 -3) => 0)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (< x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 0)
    (m/fact (f 3.25 3.25) => 0)
    (m/fact (f 3.2 3.3) => 1)
    (m/fact (f 3.3 3.2) => 0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (<= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (<= x y))]
    (m/fact (f -3 5) => 1)
    (m/fact (f 5 -3) => 0)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (<= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 1)
    (m/fact (f 3.25 3.25) => 1)
    (m/fact (f 3.2 3.3) => 1)
    (m/fact (f 3.3 3.2) => 0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (>= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (>= x y))]
    (m/fact (f -3 5) => 0)
    (m/fact (f 5 -3) => 1)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (>= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 1)
    (m/fact (f 3.25 3.25) => 1)
    (m/fact (f 3.2 3.3) => 0)
    (m/fact (f 3.3 3.2) => 1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^bool [^u32 x ^u32 y]
            (> x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^bool [^s32 x ^s32 y]
            (> x y))]
    (m/fact (f -3 5) => 0)
    (m/fact (f 5 -3) => 1)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^bool [^f32 x ^f32 y]
            (> x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 0)
    (m/fact (f 3.25 3.25) => 0)
    (m/fact (f 3.2 3.3) => 0)
    (m/fact (f 3.3 3.2) => 1)))

(oben/with-target :inprocess
  (let [compare-s8 (oben/fn ^s8 [^u32 x ^u32 y]
                     (if (> x y) 1 -1))
        compare-s16 (oben/fn ^s16 [^u32 x ^u32 y]
                      (if (> x y) 1 -1))
        compare-s32 (oben/fn ^s32 [^u32 x ^u32 y]
                      (if (> x y) 1 -1))]
    (m/fact (compare-s8 3 5) => -1)
    (m/fact (compare-s8 5 3) => 1)
    (m/fact (compare-s16 3 5) => -1)
    (m/fact (compare-s16 5 3) => 1)
    (m/fact (compare-s32 3 5) => -1)
    (m/fact (compare-s32 5 3) => 1)))

(oben/with-target :inprocess
  (let [compare (oben/fn ^s8 [^u32 x ^u32 y]
                  (if (> x y) 1
                      (if (< x y) -1
                          0)))]
    (m/fact (compare 3 5) => -1)
    (m/fact (compare 5 3) => 1)
    (m/fact (compare 3 3) => 0)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v 5]
              3))]
    (m/fact (f) => 3)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v 5]
              v))]
    (m/fact (f) => 5)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8 (+ 5 3))]
              @v))]
    (m/fact
     "var compiles to alloca so v is a pointer to u8"
     (f) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)))]
    (m/fact
     "set! results the value it has written into the var"
     (f) => 5)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v 3)))]
    (m/fact (f) => 3)))

(oben/with-target :inprocess
  (let [f (oben/fn ^s32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v (- @v))))]
    (m/fact (f) => -5)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)))]
    (m/fact (f) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return (* @v @v))))]
    (m/fact (f) => 64)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)
              2))]
    (m/fact (f) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32 (+ 5 3))]
              (+ @v increment)))]
    (m/fact
     "vars with type and initializer"
     (f 7) => 15)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32)]
              (set! v (+ 5 3))
              (+ @v increment)))]
    (m/fact
     "vars with type only"
     (f 7) => 15)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 [^u32 increment]
            (let [v (var (+ 5.0 3.75))]
              (+ @v increment)))]
    (m/fact
     "vars with initializer only"
     (f 7) => 15.75)))

(oben/with-target :inprocess
  (let [f (oben/fn ^f32 []
            (let [v1 (var 5.0)
                  v2 (var 3.75)]
              (+ @v1 @v2)))]
    (m/fact
     (f) => 8.75)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (when (> @v 7)
                (set! v (+ @v 2))
                (set! v (+ @v 2)))
              (return @v)
              2))]
    (m/fact (f) => 12)))

(oben/with-target :inprocess
  (let [count-to (oben/fn ^u32 [^u32 limit]
                   (let [i (var u32 0)]
                     (tagbody
                      :loop
                      (when (>= @i limit)
                        (return @i))
                      (set! i (+ @i 1))
                      (go :loop))
                     7))]
    (m/fact (count-to 420) => 420)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u8 [^u32 x]
            (if (not (< x 5))
              8 3))]
    (m/fact (f 4) => 3)
    (m/fact (f 5) => 8)
    (m/fact (f 6) => 8)))

(oben/with-target :inprocess
  (let [count-to (oben/fn ^u32 [^u32 limit]
                   (let [i (var u32 0)]
                     (while (< @i limit)
                       (set! i (+ @i 1)))
                     @i))]
    (m/fact (count-to 420) => 420)))

(oben/with-target :inprocess
  (let [clamp (oben/fn ^u32 [^u32 x ^u32 lo ^u32 hi]
                (if (< x lo)
                  lo
                  (if (> x hi)
                    hi
                    x)))]
    (m/fact (clamp 4 7 10) => 7)))

(oben/with-target :inprocess
  (let [clamp (oben/fn ^u32 [^u32 x ^u32 lo ^u32 hi]
                (cond (< x lo) lo
                      (> x hi) hi
                      x))]
    (m/fact (clamp 6 7 10) => 7)
    (m/fact (clamp 7 7 10) => 7)
    (m/fact (clamp 8 7 10) => 8)
    (m/fact (clamp 9 7 10) => 9)
    (m/fact (clamp 10 7 10) => 10)
    (m/fact (clamp 11 7 10) => 10)))

(oben/with-target :inprocess
  (let [select (oben/fn ^s32 [^u32 x]
                 (condp = x
                   1 -2
                   2 -4
                   3 -6
                   10))]
    (m/fact (select 1) => -2)
    (m/fact (select 2) => -4)
    (m/fact (select 3) => -6)
    (m/fact (select 4) => 10)))

(oben/with-target :inprocess
  (let [select (oben/fn ^s32 [^u32 x]
                 (case x
                   1 -2
                   2 -4
                   3 -6
                   10))]
    (m/fact (select 1) => -2)
    (m/fact (select 2) => -4)
    (m/fact (select 3) => -6)
    (m/fact (select 4) => 10)))

(oben/with-target :inprocess
  (let [div-six (oben/fn ^bool [^u32 x]
                  (and (= (% x 2) 0)
                       (= (% x 3) 0)))]
    (m/fact (div-six 0) => 1)
    (m/fact (div-six 1) => 0)
    (m/fact (div-six 2) => 0)
    (m/fact (div-six 3) => 0)
    (m/fact (div-six 4) => 0)
    (m/fact (div-six 5) => 0)
    (m/fact (div-six 6) => 1)
    (m/fact (div-six 7) => 0)
    (m/fact (div-six 8) => 0)
    (m/fact (div-six 9) => 0)
    (m/fact (div-six 10) => 0)
    (m/fact (div-six 11) => 0)
    (m/fact (div-six 12) => 1)))

(oben/with-target :inprocess
  (let [div-2or3 (oben/fn ^bool [^u32 x]
                   (or (= (% x 2) 0)
                       (= (% x 3) 0)))]
    (m/fact (div-2or3 0) => 1)
    (m/fact (div-2or3 1) => 0)
    (m/fact (div-2or3 2) => 1)
    (m/fact (div-2or3 3) => 1)
    (m/fact (div-2or3 4) => 1)
    (m/fact (div-2or3 5) => 0)
    (m/fact (div-2or3 6) => 1)
    (m/fact (div-2or3 7) => 0)
    (m/fact (div-2or3 8) => 1)
    (m/fact (div-2or3 9) => 1)
    (m/fact (div-2or3 10) => 1)
    (m/fact (div-2or3 11) => 0)
    (m/fact (div-2or3 12) => 1)))

(oben/with-target :inprocess
  (let [neg (oben/fn ^u32 [^u32 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)))

(oben/with-target :inprocess
  (let [neg (oben/fn ^u64 [^u64 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)
    (m/fact (neg -9223372036854775808) => 9223372036854775807)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and-not x y))]
    (m/fact (f 0xff 0x40) => 0xbf)
    (m/fact (f 0xff 0x55) => 0xaa)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-left x y))]
    (m/fact (f 0xff 4) => 0xff0)
    (m/fact (f 0xff 1) => 0x1fe)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => 0x0f)
    (m/fact (f 0xff 1) => 0x7f))
  (let [f (oben/fn ^s8 [^s8 x ^u8 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => -1)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [a (array u32 [9 8 7 6 5 4 3 2 1 0])]
              (get a 3)))]
    (m/fact
     "array literal supports get with constant index"
     (f) => 6)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (array u32 [9 8 7 6 5 4 3 2 1 0]))]
              (get a (+ index 1))))]
    (m/fact
     "pointer to aggregate supports get with variable index"
     (f 3) => 5)))

(def u32*5 (oben/Array Number/%u32 5))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32*5 [9 8 7 6 5]))]
              (get a (+ index (u8 3)))))]
    (m/fact
     "types can be used as value constructors"
     (f 1) => 5))

  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32*5 [9 8 7 6 5]))]
              (get a (+ index (u1 3)))))]
    (m/fact
     "narrowing conversions are rejected by default"
     (f 1) => (m/throws #"rejected narrowing UInt->UInt conversion")))

  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32*5 [9 8 7 6 5]))]
              (get a (+ index (cast! u1 3)))))]
    (m/fact
     "narrowing conversions can be forced via cast!"
     (f 1) => 7)))

(oben/with-target :inprocess
  (let [f (oben/fn ^void [] 5)
        g (oben/fn ^void [] (do))]
    (m/facts
     "void functions return nil"
     (m/fact (f) => nil)
     (m/fact (g) => nil))))

(defmacro make-array-type
  [element-type size]
  `(o/parse (oben/Array ~element-type ~size)))

;; TODO this test knows too much about the implementation

(m/facts
 (let [actual (o/parse '(Array u64 10))
       expected (o/parse '(Array u64 10))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (o/parse '(Array u64 10))
       expected (o/parse '(Array Number/%u64 10))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (oben/make-array-type nil 'u64 10 nil)
       expected (oben/make-array-type nil 'u64 10 nil)]
   (m/fact actual =not=> (m/exactly expected)))
 (let [*actual (oben/make-array-type nil 'u64 10 nil)
       actual (o/parse-for-target (target/current) *actual)
       *expected (oben/make-array-type nil 'u64 10 nil)
       expected (o/parse-for-target (target/current) *expected)]
   (m/fact (meta actual) => (m/exactly (meta expected)))
   (m/fact actual => (m/exactly expected)))
 (let [actual (oben/make-array-type nil 'u64 10 nil)
       expected (oben/make-array-type nil 'u64 10 nil)]
   (m/fact actual =not=> (m/exactly expected)))
 (let [actual (o/parse (oben/make-array-type nil 'u64 10 nil))
       expected (o/parse (oben/make-array-type nil 'u64 10 nil))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (o/parse (oben/Array u64 10))
       expected (o/parse (oben/Array u64 10))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (o/parse (oben/Array u64 10))
       expected (o/parse (oben/Array Number/%u64 10))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (o/parse (make-array-type Number/%u64 10))
       expected (o/parse (make-array-type Number/%u64 10))]
   (m/fact actual => (m/exactly expected)))
 (let [actual (o/parse (make-array-type u64 10))
       expected (o/parse (make-array-type Number/%u64 10))]
   (m/fact actual => (m/exactly expected)))
 (m/fact (Ptr (make-array-type Number/%u64 10))
         => (m/exactly (Ptr (make-array-type Number/%u64 10))))
 (let [actual (o/parse '(Array u64 10))
       expected (make-array-type Number/%u64 10)]
   (m/fact actual => (m/exactly expected))))

(m/facts
 (let [result (o/parse (with-meta 'x {:tag '(* (Array u64 10))}))
       expected-type (Ptr (make-array-type Number/%u64 10))]
   (m/fact result => 'x)
   (m/fact (:tag (meta result)) => (m/exactly expected-type)))
 (let [result (o/parse (with-meta 'ret {:tag (list '* (list 'Array 'u64 10))}))
       expected-type (Ptr (make-array-type Number/%u64 10))]
   (m/fact result => 'ret)
   (m/fact (:tag (meta result)) => (m/exactly expected-type))))

(oben/with-target :inprocess
  (let [f (oben/fn ^void [(* (Array u64 10)) ret]
            (put! ret 0 (cast u1 0))
            (put! ret 1 (cast u1 1))
            (put! ret 2 (cast u8 0xff))
            (put! ret 3 (cast u16 0xffff))
            (put! ret 4 (cast u32 0xffffffff))
            (put! ret 5 (cast u64 0xffffffffffffffff)))]
    (let [a (long-array 10 0)]
      (f a)
      (m/fact (vec a) => [0 1 0xff 0xffff 0xffffffff -1 0 0 0 0]))))

(let [f (oben/fn ^void []
          (cast u1 2))]
  (m/fact
   (f) => (m/throws #"rejected narrowing UInt->UInt conversion")))

(oben/with-target :inprocess
  (let [a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(Ptr (Number/UInt 32)) a
                         ^u32 index]
            @(+ a index))]
    (m/fact (f a 3) => 6)))

(oben/with-target :inprocess
  (let [a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a
                         ^u32 index]
            @(+ a index))]
    (m/fact
     "(* type) is a shortcut for (Ptr type)"
     (f a 3) => 6)))

(oben/with-target :inprocess
  (let [a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a]
            @a)]
    (m/fact
     (f a) => 9)))

(oben/with-target :inprocess
  (let [a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a
                         ^u32 index
                         ^u32 value]
            (set! (+ a index) value))]
    (m/fact
     (aset a 3 6)
     (nth a 3) => 6
     (f a 3 21)
     (nth a 3) => 21)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 []
            (let [f* (fn ^u32 []
                       (+ 5 2))]
              (f*)))]
    (m/fact
     "a type tag on the param vector defines the return type of the function"
     (f) => 7)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (let [f* (fn ^u32 [^u32 x ^u32 y]
                       (+ x y))]
              (f* x y)))]
    (m/fact
     "type tags on params define their types"
     (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn (Number/UInt 32) []
            (let [f* (fn (Number/UInt 32) []
                       (+ 5 2))]
              (f*)))]
    (m/fact
     "a list before the param vector is evaluated and moved to the type tag"
     (f) => 7)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [u32 x u32 y]
            (let [f* (fn ^u32 [u32 x u32 y]
                       (+ x y))]
              (f* x y)))]
    (m/fact
     "a symbol before a param is parsed as a type designator"
     (f 5 3) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [u32 a
                         ^u32 b
                         (Number/UInt 32) c
                         Number/%u32 d]
            (let [f* (fn ^u32 [u32 a
                               ^u32 b
                               (Number/UInt 32) c
                               Number/%u32 d]
                       (+ a b c d))]
              (f* a b c d)))]
    (m/fact
     "type designators"
     (f 5 3 1 6) => 15)))

(oben/with-target :inprocess
  (let [f (oben/fn (Number/UInt 32) []
            (let [return-type (Number/UInt 32)
                  alias return-type
                  f1 (fn return-type []
                       (+ 5 2))
                  f2 (fn alias []
                       (- 7 3))
                  f3 (fn ^alias []
                       (cast! alias (* 1.2 2)))
                  f4 (fn (Number/UInt 32) []
                       8)]
              (+ (f1) (f2) (f3) (f4))))]
    (m/fact
     "let-bound types"
     (f) => 21)))

(defmacro parses-to-constant-value
  [form value]
  `(let [result# (o/parse '~form)]
     (m/fact (o/constant-node? result#) => m/truthy)
     (m/fact (o/constant->value result#) => ~value)))

(m/facts
 (parses-to-constant-value 0 0)
 (parses-to-constant-value 5 5)
 (parses-to-constant-value -5 -5)
 (parses-to-constant-value 0.5 0.5)
 (parses-to-constant-value -0.5 -0.5))

(m/facts
 (parses-to-constant-value (cast u32 5.0) 5)
 (parses-to-constant-value (cast s32 -5.0) -5)
 (parses-to-constant-value (cast f32 5) 5.0)
 (parses-to-constant-value (cast f32 -5) -5.0))

(m/facts
 (parses-to-constant-value (+ 3 5) 8)
 (parses-to-constant-value (+ 3.0 5.0) 8.0)
 (parses-to-constant-value (+ 3.0 5) 8.0)
 (parses-to-constant-value (+ 3 5.0) 8.0))

(m/facts
 (parses-to-constant-value (- 3 5) -2)
 (parses-to-constant-value (- 3.0 5.0) -2.0)
 (parses-to-constant-value (- 3.0 5) -2.0)
 (parses-to-constant-value (- 3 5.0) -2.0))

(m/facts
 (parses-to-constant-value (* 3 5) 15)
 (parses-to-constant-value (* 3.0 5.0) 15.0)
 (parses-to-constant-value (* 3.0 5) 15.0)
 (parses-to-constant-value (* 3 5.0) 15.0))

(m/facts
 (parses-to-constant-value (/ 9 4) 2)
 (parses-to-constant-value (/ 9.0 4.0) 2.25)
 (parses-to-constant-value (/ 9.0 4) 2.25)
 (parses-to-constant-value (/ 9 4.0) 2.25))

(defmacro oben-expr
  [return-type form]
  `(let [f# (oben/fn ~return-type [] ~form)]
     (f#)))

(m/facts
 (m/fact (oben-expr u32 (+ 250 10)) => 260)
 (m/fact (oben-expr u32 (+ 250 100000)) => 100250)
 (m/fact (oben-expr s16 (- -120 10)) => -130)
 (m/fact (oben-expr s16 (- -120 100)) => -220))

(m/facts
 (parses-to-constant-value (/ -9 4) -2)
 (parses-to-constant-value (/ -9.0 4.0) -2.25)
 (parses-to-constant-value (/ -9.0 4) -2.25)
 (parses-to-constant-value (/ -9 4.0) -2.25))

(oben/with-target :inprocess
  (m/fact (oben-expr s32 (/ -9 4)) => -2)
  (m/fact (oben-expr f32 (/ -9.0 4.0)) => -2.25)
  (m/fact (oben-expr f32 (/ -9.0 4)) => -2.25)
  (m/fact (oben-expr f32 (/ -9 4.0)) => -2.25))

(m/facts
 (parses-to-constant-value (% 11 4) 3)
 (parses-to-constant-value (% 11.0 4.0) 3.0)
 (parses-to-constant-value (% 11.0 4) 3.0)
 (parses-to-constant-value (% 11 4.0) 3.0))

(oben/with-target :inprocess
  (m/fact (oben-expr u32 (% 11 4)) => 3)
  (m/fact (oben-expr f32 (% 11.0 4.0)) => 3.0)
  (m/fact (oben-expr f32 (% 11.0 4)) => 3.0)
  (m/fact (oben-expr f32 (% 11 4.0)) => 3.0))

(m/facts
 (parses-to-constant-value (% -11 4) -3)
 (parses-to-constant-value (% -11.0 4.0) -3.0)
 (parses-to-constant-value (% -11.0 4) -3.0)
 (parses-to-constant-value (% -11 4.0) -3.0))

(oben/with-target :inprocess
  (m/fact (oben-expr s32 (% -11 4)) => -3)
  (m/fact (oben-expr f32 (% -11.0 4.0)) => -3.0)
  (m/fact (oben-expr f32 (% -11.0 4)) => -3.0)
  (m/fact (oben-expr f32 (% -11 4.0)) => -3.0))

(m/facts
 (parses-to-constant-value (% 11 -4) 3)
 (parses-to-constant-value (% 11.0 -4.0) 3.0)
 (parses-to-constant-value (% 11.0 -4) 3.0)
 (parses-to-constant-value (% 11 -4.0) 3.0))

(oben/with-target :inprocess
  (m/fact (oben-expr s32 (% 11 -4)) => 3)
  (m/fact (oben-expr f32 (% 11.0 -4.0)) => 3.0)
  (m/fact (oben-expr f32 (% 11.0 -4)) => 3.0)
  (m/fact (oben-expr f32 (% 11 -4.0)) => 3.0))

(m/facts
 (parses-to-constant-value (bit-and 0xface 0xff) 0xce)
 (parses-to-constant-value (bit-or 0xfa00 0xce) 0xface)
 (parses-to-constant-value (bit-xor 0xfa00 0xfafa) 0x00fa)
 (parses-to-constant-value (bit-shift-left 0xfa 4) 0xfa0)
 (parses-to-constant-value (bit-shift-left 0xfa 8) 0xfa00)
 (parses-to-constant-value (bit-shift-left 0xfa 12) 0xfa000)
 (parses-to-constant-value (bit-shift-right 0xf0 3) 0x1e))

(m/facts
 (m/fact (o/replace-stars-with-ptr '(* u32))
         => '(oben.core.types.Ptr/Ptr u32))
 (m/fact (o/replace-stars-with-ptr '(** u32))
         => '(oben.core.types.Ptr/Ptr (oben.core.types.Ptr/Ptr u32)))
 (m/fact (o/replace-stars-with-ptr '(UInt 32)) => '(UInt 32))
 (m/fact (o/replace-stars-with-ptr '[* u32]) => '[* u32])
 (m/fact (o/replace-stars-with-ptr '*) => '*)
 (m/fact (o/replace-stars-with-ptr 'foo) => 'foo)
 (m/fact (o/replace-stars-with-ptr "foo") => "foo"))

(defmacro same-values-and-tags?
  [actual expected]
  `(let [actual# ~actual
         actual-meta# (walk/postwalk (comp :tag meta) actual#)
         expected# ~expected
         expected-meta# (walk/postwalk (comp :tag meta) expected#)]
     (m/fact actual# => expected#)
     (m/fact actual-meta# => expected-meta#)))

(m/facts
 (same-values-and-tags?
  (o/move-types-to-meta '(u32 f))
  (list (with-meta 'f {:tag 'u32})))
 (same-values-and-tags?
  (o/move-types-to-meta '(u32 f f64 g))
  (list (with-meta 'f {:tag 'u32})
        (with-meta 'g {:tag 'f64})))
 (same-values-and-tags?
  (o/move-types-to-meta '(u8 arg0 u8 arg1))
  (list (with-meta 'arg0 {:tag 'u8})
        (with-meta 'arg1 {:tag 'u8})))
 (same-values-and-tags?
  (o/move-types-to-meta '(u8 arg0 u32 arg1))
  (list (with-meta 'arg0 {:tag 'u8})
        (with-meta 'arg1 {:tag 'u32})))
 (same-values-and-tags?
  (o/move-types-to-meta '(u8 arg0 u32 arg1 f64 arg2))
  (list (with-meta 'arg0 {:tag 'u8})
        (with-meta 'arg1 {:tag 'u32})
        (with-meta 'arg2 {:tag 'f64})))
 (same-values-and-tags?
  (o/move-types-to-meta '(u32 [^u32 x f32 y]))
  (list (with-meta (vector (with-meta 'x {:tag 'u32})
                           (with-meta 'y {:tag 'f32})) {:tag 'u32})))
 (same-values-and-tags?
  (o/move-types-to-meta '((Struct [^f32 x u16 y]) x))
  (list (with-meta 'x {:tag (list 'Struct (vector (with-meta 'x {:tag 'f32})
                                                  'u16 'y))}))))

(m/facts
 (m/fact (o/split-after keyword? [1 2 3 :foo 4 5]) => [[1 2 3 :foo] [4 5]]))

(o/defmulti determine-bit-size)

(o/defmethod determine-bit-size [Number/%u64] [_] 64)
(o/defmethod determine-bit-size [Number/%u32] [_] 32)
(o/defmethod determine-bit-size [Number/%u8] [_] 8)
(o/defmethod determine-bit-size [Number/UInt] [_] :uint)
(o/defmethod determine-bit-size [Number/FP] [_] :float)

(m/facts
 "dispatch by type or typeclass"
 (m/fact (determine-bit-size (o/parse 250)) => 8)
 (m/fact (determine-bit-size (o/parse 270)) => :uint)
 (m/fact (determine-bit-size (o/parse 65535)) => :uint)
 (m/fact (determine-bit-size (o/parse 65536)) => 32)
 (m/fact (determine-bit-size (o/parse Long/MAX_VALUE)) => 64)
 (m/fact (determine-bit-size (o/parse -5.0)) => :float))

(oben/with-target :inprocess
  (m/fact
   "cannot get address of LLVM intrinsics"
   (math/llvm.sqrt.f32 25.0)
   => (m/throws #"cannot get function address")))

(oben/with-target :inprocess
  (let [sqrt (oben/fn ^f32 [^f32 x]
               (math/sqrt x))]
    (m/fact (sqrt 25.0) => 5.0)))

(o/defportable c-long :arch
  :x86_64 Number/%s64
  :i386 Number/%s32)

(m/fact (o/portable? c-long))

(o/defportable c-long [:arch]
  :aarch64 Number/%s64)

(m/fact (o/resolve `c-long) => o/portable?)

(m/facts
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :x86_64}}
    (o/parse `c-long))
  => (m/exactly Number/%s64))
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :i386}}
    (o/parse `c-long))
  => (m/exactly Number/%s32))
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :aarch64}}
    (o/parse `c-long))
  => (m/exactly Number/%s64)))

(o/defportable timezone-delta
  [target]
  (case (target/attr* target :timezone)
    :cest 2
    :cet 1))

(m/fact (o/resolve `timezone-delta) => o/portable?)

(m/facts
 (m/fact
  (-> (oben/with-target
        {:type :inprocess
         :attrs {:timezone :cest}}
        (o/parse `timezone-delta))
      o/constant->value)
  => 2)
 (m/fact
  (-> (oben/with-target
        {:type :inprocess
         :attrs {:timezone :cet}}
        (o/parse `timezone-delta))
      o/constant->value)
  => 1))

(o/defportable add-or-mul :os
  :adding-os
  (fn [x y] (+ x y))
  :multiplying-os
  (fn [x y] (* x y)))

(m/facts
 (m/fact
  (let [add-or-mul
        (oben/with-target
          {:type :inprocess
           :attrs {:os :adding-os}}
          (o/parse `add-or-mul))]
    (add-or-mul 5 4)) => 9)
 (m/fact
  (let [add-or-mul
        (oben/with-target
          {:type :inprocess
           :attrs {:os :multiplying-os}}
          (o/parse `add-or-mul))]
    (add-or-mul 5 4)) => 20))

(oben/with-target :inprocess
  (let [s1 (o/parse '(Struct [^f32 x ^u8 y]))
        s2 (o/parse '(Struct [^f32 x ^u8 y]))]
    (m/fact (= s1 s2))
    (m/fact (= (meta s1) (meta s2)))))

(oben/with-target :inprocess
  (let [vec2 (o/parse '(Struct [^f32 x ^f32 y]))
        m (meta vec2)]
    (m/fact (:name->index m) => {:x 0 :y 1
                                 0 0 1 1})
    (m/fact (o/type? vec2))
    (m/fact (isa? (o/tid-of-type vec2) :oben.core.types.Struct/Struct) => m/truthy)
    (m/fact (:field-names m) => [:x :y])
    (m/fact (:field-types m) => [Number/%f32 Number/%f32])
    (m/fact (Aggregate/valid-key? vec2 :x) => m/truthy)
    (m/fact (Aggregate/valid-key? vec2 :y) => m/truthy)
    (m/fact (Aggregate/valid-key? vec2 :z) => m/falsey)
    (m/fact (Aggregate/valid-key? vec2 0) => m/truthy)
    (m/fact (Aggregate/valid-key? vec2 1) => m/truthy)
    (m/fact (Aggregate/valid-key? vec2 2) => m/falsey)
    (m/fact (Aggregate/get-element-type vec2 :x) => (m/exactly Number/%f32))
    (m/fact (Aggregate/get-element-type vec2 :y) => (m/exactly Number/%f32))
    (m/fact (Aggregate/get-element-type vec2 0) => (m/exactly Number/%f32))
    (m/fact (Aggregate/get-element-type vec2 1) => (m/exactly Number/%f32))
    (m/fact (o/constant->value (Aggregate/get-element-index vec2 :x)) => 0)
    (m/fact (o/constant->value (Aggregate/get-element-index vec2 :y)) => 1)
    (m/fact (o/constant->value (Aggregate/get-element-index vec2 0)) => 0)
    (m/fact (o/constant->value (Aggregate/get-element-index vec2 1)) => 1))
  (let [s (o/parse '(Struct [u8 x8 u16 x16 u32 x32 u64 x64
                             s8 y8 s16 y16 s32 y32 s64 y64
                             f32 z32 f64 z64]))
        m (meta s)]
    (m/fact (o/type? s))
    (m/fact (isa? (o/tid-of-type s) :oben.core.types.Struct/Struct) => m/truthy)
    (m/fact (:field-names m) => [:x8 :x16 :x32 :x64
                                 :y8 :y16 :y32 :y64
                                 :z32 :z64])
    (m/fact (:field-types m) => [Number/%u8 Number/%u16 Number/%u32 Number/%u64
                                 Number/%s8 Number/%s16 Number/%s32 Number/%s64
                                 Number/%f32 Number/%f64])
    (m/fact (Aggregate/get-element-type s :x8) => (m/exactly Number/%u8))
    (m/fact (Aggregate/get-element-type s :x16) => (m/exactly Number/%u16))
    (m/fact (Aggregate/get-element-type s :x32) => (m/exactly Number/%u32))
    (m/fact (Aggregate/get-element-type s :x64) => (m/exactly Number/%u64))
    (m/fact (Aggregate/get-element-type s :y8) => (m/exactly Number/%s8))
    (m/fact (Aggregate/get-element-type s :y16) => (m/exactly Number/%s16))
    (m/fact (Aggregate/get-element-type s :y32) => (m/exactly Number/%s32))
    (m/fact (Aggregate/get-element-type s :y64) => (m/exactly Number/%s64))
    (m/fact (Aggregate/get-element-type s :z32) => (m/exactly Number/%f32))
    (m/fact (Aggregate/get-element-type s :z64) => (m/exactly Number/%f64))
    (m/fact (o/constant->value (Aggregate/get-element-index s :x8)) => 0)
    (m/fact (o/constant->value (Aggregate/get-element-index s :y8)) => 4)
    (m/fact (o/constant->value (Aggregate/get-element-index s :z32)) => 8)
    (m/fact (o/constant->value (Aggregate/get-element-index s :z64)) => 9)))

(m/facts
 (m/fact (o/sizeof Number/%u1) => 1)
 (m/fact (o/sizeof Number/%u8) => 1)
 (m/fact (o/sizeof Number/%u16) => 2)
 (m/fact (o/sizeof Number/%u32) => 4)
 (m/fact (o/sizeof Number/%u64) => 8)
 (m/fact (o/sizeof Number/%f32) => 4)
 (m/fact (o/sizeof Number/%f64) => 8))

(m/facts
 (m/fact (o/alignof Number/%u1) => 1)
 (m/fact (o/alignof Number/%u8) => 1)
 (m/fact (o/alignof Number/%u16) => 2)
 (m/fact (o/alignof Number/%u32) => 4)
 (m/fact (o/alignof Number/%u64) => 8)
 (m/fact (o/alignof Number/%f32) => 4)
 (m/fact (o/alignof Number/%f64) => 8))

(oben/with-target
  {:type :inprocess
   :attrs {:address-size 32}}
  (m/fact (o/sizeof (o/parse 'usize)) => 4)
  (m/fact (o/sizeof (o/parse 'ssize)) => 4))

(oben/with-target
  {:type :inprocess
   :attrs {:address-size 64}}
  (m/fact (o/sizeof (o/parse 'usize)) => 8)
  (m/fact (o/sizeof (o/parse 'ssize)) => 8))

(defmacro make-struct-type*
  [field-types]
  `(oben/Struct
    ~(into [] (interleave field-types
                          (map #(symbol (str "arg" %))
                               (range (count field-types)))))))

(defmacro make-struct-type
  [field-types]
  `(o/parse (make-struct-type* ~field-types)))

(oben/with-target :inprocess
  (let [s (make-struct-type [u8 u32])]
    (m/fact (:field-types (meta s)) => [Number/%u8 Number/%u32]))
  (let [s (make-struct-type [u8 u8])]
    (m/fact (:field-types (meta s)) => [Number/%u8 Number/%u8])))

(defmacro sizeof-struct
  [field-types]
  `(let [s# (make-struct-type ~field-types)]
     (o/sizeof s#)))

(oben/with-target
  {:type :inprocess
   :attrs {:address-size 64}}
  (m/facts
   (m/fact (sizeof-struct [u8]) => 1)
   (m/fact (sizeof-struct [u8 u8]) => 2)
   (m/fact (sizeof-struct [u8 f32]) => 8)
   (m/fact (sizeof-struct [f32 u8]) => 8)
   (m/fact (sizeof-struct [f32 (Ptr f64) u8]) => 24)))

(oben/with-target
  {:type :inprocess
   :attrs {:address-size 32}}
  (m/facts
   (m/fact (sizeof-struct [u8]) => 1)
   (m/fact (sizeof-struct [u8 u8]) => 2)
   (m/fact (sizeof-struct [u8 f32]) => 8)
   (m/fact (sizeof-struct [f32 u8]) => 8)
   (m/fact (sizeof-struct [f32 (Ptr f64) u8]) => 12)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x]
            (let [v1 (var x)]
              (set! v1 (+ @v1 1))
              (let [v2 (var @v1)]
                @v2)))]
    (m/fact
     "vars are initialized at the point of their creation"
     (f 7) => 8)))

(oben/with-target :inprocess
  (let [f (oben/fn ^u32 [^u32 x]
            (let [v (var x)
                  p (set! v (+ @v 1))
                  sum (var u32 0)
                  i (var u32 0)]
              (while (< @i 10)
                (set! sum (+ @sum p))
                (set! i (+ @i 1)))
              @sum))]
    (m/fact
     "let expressions are evaluated only once when they are bound"
     (f 4) => 50)))

(o/defportable size_t
  [target]
  (Number/UInt (target/attr :address-size)))

(oben/defn malloc (* u8) [size_t size])
(oben/defn free void [(* u8) ptr])

(oben/with-target :inprocess
  (let [f (oben/fn ^u64 []
            (let [p (malloc 4096)
                  i (var u32 0)
                  sum (var u64 0)]
              (while (< @i 4096)
                (let [v (cast! u8 @i)]
                  (set! (+ p @i) v)
                  (set! sum (+ @sum v)))
                (set! i (+ @i 1)))
              (free p)
              @sum))]
    (m/fact (f) => (reduce + (map #(mod % 256) (range 4096))))))

(oben/with-target :inprocess
  (let [ret-g (oben/fn ^u64 []
                (let [g (global u64 1234)]
                  @g))]
    (m/fact (ret-g) => 1234)))

(oben/with-target :inprocess
  (let [g (oben/global u64 1234)
        ret-g (oben/fn ^u64 []
                @g)]
    (m/fact (ret-g) => 1234)))

(oben/with-target :inprocess
  (let [sum (oben/global u64 23)
        add (oben/fn ^void [^u64 x]
              (set! sum (+ @sum x)))
        ret-sum (oben/fn ^u64 []
                  @sum)]
    (add 17)
    (add 51)
    (m/fact (ret-sum) => (+ 23 51 17))))

(oben/defglobal sum u64 33)

(oben/with-target :inprocess
  (let [add (oben/fn ^void [^u64 x]
              (set! sum (+ @sum x)))
        ret-sum (oben/fn ^u64 [] @sum)]
    (add 17)
    (add 51)
    (m/fact (ret-sum) => (+ 33 51 17))))

(oben/with-target :inprocess
  (let [vec2 (oben/Struct [^f32 x ^f32 y])
        vec2-x (oben/fn ^f32 [vec2 v]
                 (:x v))
        f (oben/fn ^f32 []
            (vec2-x {:x 3 :y 4}))]
    (m/fact (f) => 3.0)))

(oben/with-target :inprocess
  (let [vec2 (oben/Struct [^f32 x ^f32 y])
        vec2-len (oben/fn ^f32 [vec2 v]
                   (let [x (:x v)
                         y (:y v)]
                     (math/sqrt (+ (* x x) (* y y)))))
        f (oben/fn ^f32 [^f32 x ^f32 y]
            (let [v {:x x :y y}]
              (vec2-len v)))]
    (m/fact (f 3 4) => 5.0)))

(oben/with-target :inprocess
  (let [F (o/parse (oben/Fn u16 [u32 f32 (* (Array s8 7))]))]
    (m/fact F => o/type?)
    (let [{:keys [return-type param-types]} (meta F)]
      (m/fact return-type => (m/exactly Number/%u16))
      (m/fact param-types => (m/exactly (o/parse '[u32 f32 (* (Array s8 7))]))))))

(oben/with-target :inprocess
  (let [OpFn (o/parse (oben/Fn u32 [u32]))
        op (o/parse (oben/fn u32 [u32 x] (+ x 1)))]
    (m/fact op => o/fnode?)
    (m/fact (o/type-of op) => (m/exactly (Ptr OpFn)))
    (m/fact (o/type-of op) => (m/exactly (o/parse (list '* OpFn))))))

(oben/with-target :inprocess
  (let [OpFn (oben/Fn u32 [u32])
        op (oben/fn u32 [u32 x] (+ x 1))
        f (oben/fn u32 [u32 x]
            (op x))]
    (m/fact (f 11) => 12)))

(oben/with-target :inprocess
  (let [OpFn (oben/Fn u32 [u32])
        op (oben/fn u32 [u32 x] (+ x 1))
        f (oben/fn u32 [u32 x]
            (let [p (var (* OpFn) op)]
              (@p x)))]
    (m/fact (f 11) => 12)))

(oben/with-target :inprocess
  (let [OpFn (oben/Fn u32 [u32])
        add-1 (oben/fn ^u32 [^u32 x] (+ x 1))
        mul-3 (oben/fn ^u32 [^u32 x] (* x 3))
        div-2 (oben/fn ^u32 [^u32 x] (/ x 2))
        f (oben/fn ^u32 [^u32 x (** OpFn) ops]
            (let [opfn-ptr (var (** OpFn) ops)
                  result (var u32 x)]
              (while @@opfn-ptr
                (set! result (@@opfn-ptr @result))
                (set! opfn-ptr (+ @opfn-ptr 1)))
              @result))
        g (oben/fn ^u32 [^u32 x]
            (let [ops (var (array (* OpFn) [add-1 mul-3 div-2 nil]))]
              (f x ops)))]
    (m/fact (g 11) => (-> 11 (+ 1) (* 3) (/ 2)))))
