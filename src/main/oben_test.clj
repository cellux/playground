(ns oben-test
  (:require [oben])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.target :as target])
  (:require [oben.core.math :as math])
  (:require [oben.core.types.Void :as Void])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Ptr :as Ptr])
  (:require [clojure.walk :as walk])
  (:require [midje.sweet :as m])
  (:use [midje.repl]))

(defn compile-type
  [t]
  (let [ctx (ctx/create)]
    (-> ctx
        (ctx/compile-type t)
        (ctx/compiled-type t))))

(m/facts
 "type constructors and named types"
 (m/fact (compile-type (Void/%Void)) => :void)
 (m/fact (compile-type Void/%void) => :void)
 (m/fact (compile-type (Number/UInt 32)) => [:integer 32])
 (m/fact (compile-type Number/%u32) => [:integer 32])
 (m/fact (compile-type (Number/UInt 8)) => [:integer 8])
 (m/fact (compile-type Number/%u8) => [:integer 8])
 (m/fact (compile-type (Number/SInt 16)) => [:integer 16])
 (m/fact (compile-type Number/%s16) => [:integer 16]))

(m/facts
 "type constructors memoize the types they return"
 (m/fact (Number/UInt 32) => (m/exactly (Number/UInt 32))))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (+ 5 2))]
    (m/fact
     "a type tag on the param vector defines the return type of the function"
     (f) => 7)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y] (+ x y))]
    (m/fact
     "type tags on params define their types"
     (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-temp-target
  (let [f (oben/fn (Number/UInt 32) []
            (+ 5 2))]
    (m/fact
     "a list before the param vector is evaluated and moved to the
     vector's type tag"
     (f) => 7)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [u32 x u32 y] (+ x y))]
    (m/fact
     "a symbol before a param is used as a type designator"
     (f 5 3) => 8)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [u32 a
                         ^u32 b
                         (Number/UInt 32) c
                         Number/%u32 d]
            (+ a b c d))]
    (m/fact
     "type designators"
     (f 5 3 1 6) => 15)))

(oben/with-temp-target
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

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (+ x y)
            (+ 5 2))]
    (m/fact (f 1 2) => 7)
    (m/fact (f 5 3) => 7)))

(oben/defn add
  ^u32 [^u32 x ^u32 y]
  (+ x y))

(m/facts
 (m/fact (add (add 1 2) (add 7 -2)) => 8))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u16 x ^u8 y] (+ x y))]
    (m/fact (f 1 2) => 3)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (do)
            (do (+ x y))
            (do
              (+ x y)
              (+ 3 9)))]
    (m/fact (f 1 2) => 12)))

(oben/with-temp-target
  (let [f (oben/fn ^u16 [^u32 x]
            (cast! u16 x))]
    (m/fact
     "forced casts"
     (f 65535) => -1
     (f 65536) => 0
     (f 65537) => 1)))

(oben/with-temp-target
  (let [f (oben/fn ^f32 [^u32 x]
            (let [g (fn ^u32 [^f32 x] x)]
              (g x)))]
    (m/fact
     "call to let-bound function"
     (f 60) => 60.0)))

(oben/with-temp-target
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (let [g (fn ^u32 [^u16 x ^u8 y] (+ x y))]
              (g (cast! u16 x) (cast! u8 y))))]
    (m/fact
     (f 6 3) => 9.0)))

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(m/fact (ast/parse 'u8) => (m/exactly Number/%u8))
(m/fact (ast/parse 's8) => (m/exactly Number/%s8))

(m/fact (o/type-of (ast/parse '(u8 0))) => (m/exactly (Number/UInt 8)))
(m/fact (o/type-of (ast/parse '(u8 0))) => (m/exactly Number/%u8))

(m/fact (o/type-of (ast/parse '(s8 0))) => (m/exactly (Number/SInt 8)))
(m/fact (o/type-of (ast/parse '(s8 0))) => (m/exactly Number/%s8))

(m/fact (o/type-of (ast/parse 0)) => (m/exactly Number/%u1))
(m/fact (o/type-of (ast/parse 1)) => (m/exactly Number/%u1))
(m/fact (o/type-of (ast/parse -1)) => (m/exactly Number/%s8))
(m/fact (o/type-of (ast/parse 5)) => (m/exactly Number/%u8))
(m/fact (o/type-of (ast/parse -5)) => (m/exactly Number/%s8))

(m/fact (o/type-of (ast/parse '(u8 -5))) => (m/exactly Number/%u8))
(m/fact
 "reinterpreting a signed value as unsigned does not change its bit pattern"
 (o/constant->value (ast/parse '(u8 -5))) => -5)

(m/fact
 "negating an UInt turns it into an SInt"
 (o/type-of (ast/parse '(- 5))) => (m/exactly Number/%s8))

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and x y))]
    (m/fact (f 0x1234 0xff) => 0x34)
    (m/fact (f 0x1234 0xff00) => 0x1200))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-and x y))]
    (m/fact (f -2 0xff) => 0xfe)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-or x y))]
    (m/fact (f 0x1234 0xff) => 0x12ff)
    (m/fact (f 0x1234 0xff00) => 0xff34))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-or x y))]
    (m/fact (f -2 0xff) => -1)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-xor x y))]
    (m/fact (f 0x1234 0xff) => (+ 0x1200 (- 0xff 0x34)))
    (m/fact (f 0x1234 0xff00) => (+ 0x34 (- 0xff00 0x1200)))))

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
  (let [compare (oben/fn ^s8 [^u32 x ^u32 y]
                  (if (> x y) 1
                      (if (< x y) -1
                          0)))]
    (m/fact (compare 3 5) => -1)
    (m/fact (compare 5 3) => 1)
    (m/fact (compare 3 3) => 0)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-target
  (let [f (oben/fn ^u8 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x]
            x)]
    (m/fact (f 5) => 5)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v 5]
              3))]
    (m/fact (f) => 3)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v 5]
              v))]
    (m/fact (f) => 5)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8 (+ 5 3))]
              @v))]
    (m/fact (f) => 8)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)))]
    (m/fact (f) => 5)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v 3)))]
    (m/fact (f) => 3)))

(oben/with-temp-target
  (let [f (oben/fn ^s32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v (- @v))))]
    (m/fact (f) => -5)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)))]
    (m/fact (f) => 8)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return (* @v @v))))]
    (m/fact (f) => 64)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)
              2))]
    (m/fact (f) => 8)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (when (> @v 7)
                (set! v (+ @v 2))
                (set! v (+ @v 2)))
              (return @v)
              2))]
    (m/fact (f) => 12)))

(oben/with-temp-target
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

(oben/with-temp-target
  (let [f (oben/fn ^u8 [^u32 x]
            (if (not (< x 5))
              8 3))]
    (m/fact (f 4) => 3)
    (m/fact (f 5) => 8)
    (m/fact (f 6) => 8)))

(oben/with-temp-target
  (let [count-to (oben/fn ^u32 [^u32 limit]
                   (let [i (var u32 0)]
                     (while (< @i limit)
                       (set! i (+ @i 1)))
                     @i))]
    (m/fact (count-to 420) => 420)))

(oben/with-temp-target
  (let [clamp (oben/fn ^u32 [^u32 x ^u32 lo ^u32 hi]
                (if (< x lo)
                  lo
                  (if (> x hi)
                    hi
                    x)))]
    (m/fact (clamp 4 7 10) => 7)))

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
  (let [neg (oben/fn ^u32 [^u32 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)))

(oben/with-temp-target
  (let [neg (oben/fn ^u64 [^u64 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)
    (m/fact (neg -9223372036854775808) => 9223372036854775807)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and-not x y))]
    (m/fact (f 0xff 0x40) => 0xbf)
    (m/fact (f 0xff 0x55) => 0xaa)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-left x y))]
    (m/fact (f 0xff 4) => 0xff0)
    (m/fact (f 0xff 1) => 0x1fe)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => 0x0f)
    (m/fact (f 0xff 1) => 0x7f))
  (let [f (oben/fn ^s8 [^s8 x ^u8 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => -1)))

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
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

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32 (+ 5 3))]
              (+ @v increment)))]
    (m/fact
     "vars with type and initializer"
     (f 7) => 15)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32)]
              (set! v (+ 5 3))
              (+ @v increment)))]
    (m/fact
     "vars with type only"
     (f 7) => 15)))

(oben/with-temp-target
  (let [f (oben/fn ^f32 [^u32 increment]
            (let [v (var (+ 5.0 3.75))]
              (+ @v increment)))]
    (m/fact
     "vars with initializer only"
     (f 7) => 15.75)))

(oben/with-temp-target
  (let [f (oben/fn ^f32 []
            (let [v1 (var 5.0)
                  v2 (var 3.75)]
              (+ @v1 @v2)))]
    (m/fact
     (f) => 8.75)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [a (array u32 [9 8 7 6 5 4 3 2 1 0])]
              (get a 3)))]
    (m/fact
     "array literal supports get with constant index"
     (f) => 6)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (array u32 [9 8 7 6 5 4 3 2 1 0]))]
              (get a (+ index 1))))]
    (m/fact
     "pointer to aggregate supports get with variable index"
     (f 3) => 5)))

(def u32array-3 (oben/Array Number/%u32 5))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32array-3 [9 8 7 6 5]))]
              (get a (+ index (u8 3)))))]
    (m/fact
     "types can be used as value constructors"
     (f 1) => 5))

  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32array-3 [9 8 7 6 5]))]
              (get a (+ index (u1 3)))))]
    (m/fact
     "narrowing conversions are rejected by default"
     (f 1) => (throws clojure.lang.ExceptionInfo #"rejected narrowing UInt->UInt conversion")))

  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32array-3 [9 8 7 6 5]))]
              (get a (+ index (cast! u1 3)))))]
    (m/fact
     "narrowing conversions can be forced via cast!"
     (f 1) => 7)))

(oben/with-temp-target
  (let [f (oben/fn ^void [] 5)]
    (m/fact
     "void functions return nil"
     (f) => nil)))

(m/facts
 (m/fact (Ptr/Ptr (oben/Array Number/%u64 10))
         => (m/exactly (Ptr/Ptr (oben/Array Number/%u64 10))))
 (let [actual (ast/parse '(Array u64 10))
       expected (oben/Array Number/%u64 10)]
   (m/fact actual => (m/exactly expected))))

(m/facts
 (let [result (ast/parse (with-meta 'x {:tag '(* (Array u64 10))}))
       expected-type (Ptr/Ptr (oben/Array Number/%u64 10))]
   (m/fact result => 'x)
   (m/fact (:tag (meta result)) => (m/exactly expected-type)))
 (let [result (ast/parse (with-meta 'ret {:tag (list '* (list 'Array 'u64 10))}))
       expected-type (Ptr/Ptr (oben/Array Number/%u64 10))]
   (m/fact result => 'ret)
   (m/fact (:tag (meta result)) => (m/exactly expected-type))))

(oben/with-temp-target
  (let [f (oben/fn ^void [(* (Array u64 10)) ret]
            (put ret 0 (cast u1 0))
            (put ret 1 (cast u1 1))
            (put ret 2 (cast u8 0xff))
            (put ret 3 (cast u16 0xffff))
            (put ret 4 (cast u32 0xffffffff))
            (put ret 5 (cast u64 0xffffffffffffffff)))]
    (let [a (long-array 10 0)]
      (f a)
      (m/fact (vec a) => [0 1 0xff 0xffff 0xffffffff -1 0 0 0 0]))))

(let [f (oben/fn ^void []
          (cast u1 2))]
  (m/fact
   (f) => (throws clojure.lang.ExceptionInfo #"rejected narrowing UInt->UInt conversion")))

(oben/with-temp-target
  (let [atype (oben/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(Ptr/Ptr (Number/UInt 32)) a
                         ^u32 index]
            @(+ a index))]
    (m/fact (f a 3) => 6)))

(oben/with-temp-target
  (let [atype (oben/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a
                         ^u32 index]
            @(+ a index))]
    (m/fact
     "(* type) is a shortcut for (Ptr/Ptr type)"
     (f a 3) => 6)))

(oben/with-temp-target
  (let [atype (oben/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a]
            @a)]
    (m/fact
     (f a) => 9)))

(oben/with-temp-target
  (let [atype (oben/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a
                         ^u32 index
                         ^u32 value]
            (set! (+ a index) value))]
    (m/fact
     (aset a 3 6)
     (nth a 3) => 6
     (f a 3 21)
     (nth a 3) => 21)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 []
            (let [f* (fn ^u32 []
                       (+ 5 2))]
              (f*)))]
    (m/fact
     "a type tag on the param vector defines the return type of the function"
     (f) => 7)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (let [f* (fn ^u32 [^u32 x ^u32 y]
                       (+ x y))]
              (f* x y)))]
    (m/fact
     "type tags on params define their types"
     (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-temp-target
  (let [f (oben/fn (Number/UInt 32) []
            (let [f* (fn (Number/UInt 32) []
                       (+ 5 2))]
              (f*)))]
    (m/fact
     "a list before the param vector is evaluated and moved to the type tag"
     (f) => 7)))

(oben/with-temp-target
  (let [f (oben/fn ^u32 [u32 x u32 y]
            (let [f* (fn ^u32 [u32 x u32 y]
                       (+ x y))]
              (f* x y)))]
    (m/fact
     "a symbol before a param is used as a type designator"
     (f 5 3) => 8)))

(oben/with-temp-target
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

(oben/with-temp-target
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
  `(let [result# (ast/parse '~form)]
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

(oben/with-temp-target
  (m/fact (oben-expr s32 (/ -9 4)) => -2)
  (m/fact (oben-expr f32 (/ -9.0 4.0)) => -2.25)
  (m/fact (oben-expr f32 (/ -9.0 4)) => -2.25)
  (m/fact (oben-expr f32 (/ -9 4.0)) => -2.25))

(m/facts
 (parses-to-constant-value (% 11 4) 3)
 (parses-to-constant-value (% 11.0 4.0) 3.0)
 (parses-to-constant-value (% 11.0 4) 3.0)
 (parses-to-constant-value (% 11 4.0) 3.0))

(oben/with-temp-target
  (m/fact (oben-expr u32 (% 11 4)) => 3)
  (m/fact (oben-expr f32 (% 11.0 4.0)) => 3.0)
  (m/fact (oben-expr f32 (% 11.0 4)) => 3.0)
  (m/fact (oben-expr f32 (% 11 4.0)) => 3.0))

(m/facts
 (parses-to-constant-value (% -11 4) -3)
 (parses-to-constant-value (% -11.0 4.0) -3.0)
 (parses-to-constant-value (% -11.0 4) -3.0)
 (parses-to-constant-value (% -11 4.0) -3.0))

(oben/with-temp-target
  (m/fact (oben-expr s32 (% -11 4)) => -3)
  (m/fact (oben-expr f32 (% -11.0 4.0)) => -3.0)
  (m/fact (oben-expr f32 (% -11.0 4)) => -3.0)
  (m/fact (oben-expr f32 (% -11 4.0)) => -3.0))

(m/facts
 (parses-to-constant-value (% 11 -4) 3)
 (parses-to-constant-value (% 11.0 -4.0) 3.0)
 (parses-to-constant-value (% 11.0 -4) 3.0)
 (parses-to-constant-value (% 11 -4.0) 3.0))

(oben/with-temp-target
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

(defn replace-with-non-empty-tag
  [x]
  (or (:tag (meta x)) x))

(defmacro check-equal-value-and-tag
  [actual expected]
  `(let [actual# ~actual
         actual-meta# (walk/postwalk replace-with-non-empty-tag actual#)
         expected# ~expected
         expected-meta# (walk/postwalk replace-with-non-empty-tag expected#)]
     (m/fact actual# => expected#)
     (m/fact actual-meta# => expected-meta#)))

(m/facts
 (check-equal-value-and-tag
  (o/move-types-to-meta '(u32 f))
  (list (with-meta 'f {:tag 'u32})))
 (check-equal-value-and-tag
  (o/move-types-to-meta '(u32 f f64 g))
  (list (with-meta 'f {:tag 'u32})
        (with-meta 'g {:tag 'f64})))
 (check-equal-value-and-tag
  (o/move-types-to-meta '(u32 [^u32 x f32 y]))
  (list (with-meta (vector (with-meta 'x {:tag 'u32})
                           (with-meta 'y {:tag 'f32})) {:tag 'u32})))
 (check-equal-value-and-tag
  (o/move-types-to-meta '((Struct [^f32 x u16 y]) x))
  (list (with-meta 'x {:tag (list 'Struct (vector (with-meta 'x {:tag 'f32})
                                                  'u16 'y))}))))

(defmacro quote-all-except-locals
  [form env]
  (list 'quote (o/quote-all-except-locals form (eval env))))

(m/facts
 (m/fact
  (quote-all-except-locals
   x
   {})
  => '(quote x))
 (m/fact
  (quote-all-except-locals
   x
   {'x 5})
  => 'x)
 (m/fact
  (quote-all-except-locals
   (fn u32 [^f32 x f32 y])
   {})
  => '(list 'fn 'u32 (vector (with-meta 'x {:tag 'f32}) 'f32 'y)))
 (m/fact
  (quote-all-except-locals
   (fn u32 [^f32 x f32 y])
   {'f32 :bound})
  => '(list 'fn 'u32 (vector (with-meta 'x {:tag f32}) f32 'y)))
 (m/fact
  (quote-all-except-locals
   (fn u32 [^{:tag (UInt 32)} x f32 y])
   {'f32 :bound})
  => '(list 'fn 'u32 (vector (with-meta 'x {:tag (list 'UInt 32)}) f32 'y)))
 (m/fact
  (quote-all-except-locals
   (fn f32 [^{:tag (UInt 32)} x f32 y])
   {'f32 :bound 'UInt :bound})
  => '(list 'fn f32 (vector (with-meta 'x {:tag (list UInt 32)}) f32 'y)))
 (m/fact
  "symbols with type tags are protected from evaluation"
  (quote-all-except-locals
   (fn f32 [^{:tag (UInt 32)} x f32 y])
   {'f32 :bound 'UInt :bound 'x :bound})
  => '(list 'fn f32 (vector (with-meta 'x {:tag (list UInt 32)}) f32 'y))))

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
 (m/fact (determine-bit-size (ast/parse 250)) => 8)
 (m/fact (determine-bit-size (ast/parse 270)) => :uint)
 (m/fact (determine-bit-size (ast/parse 65535)) => :uint)
 (m/fact (determine-bit-size (ast/parse 65536)) => 32)
 (m/fact (determine-bit-size (ast/parse Long/MAX_VALUE)) => 64)
 (m/fact (determine-bit-size (ast/parse -5.0)) => :float))

(oben/with-temp-target
  (m/fact
   "cannot get address of LLVM intrinsics"
   (math/llvm.sqrt.f32 25.0)
   => (throws clojure.lang.ExceptionInfo #"cannot get function address")))

(oben/with-temp-target
  (let [sqrt (oben/fn ^f32 [^f32 x]
               (math/sqrt x))]
    (m/fact (sqrt 25.0) => 5.0)))

(o/defportable c-long :arch
  :x86_64 (Number/%s64)
  :i386 (Number/%s32))

(m/fact (o/portable? c-long))

(o/defportable c-long [:arch]
  :aarch64 (Number/%s64))

(m/fact (o/resolve `c-long) => o/portable?)

(m/facts
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :x86_64}}
    (ast/parse `c-long))
  => (m/exactly Number/%s64))
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :i386}}
    (ast/parse `c-long))
  => (m/exactly Number/%s32))
 (m/fact
  (oben/with-target
    {:type :inprocess
     :attrs {:arch :aarch64}}
    (ast/parse `c-long))
  => (m/exactly Number/%s64)))

(o/defportable timezone-delta
  [target]
  (let [attrs (target/attrs target)]
    (case (:timezone attrs)
      :cest 2
      :cet 1)))

(m/fact (o/resolve `timezone-delta) => o/portable?)

(m/facts
 (m/fact
  (-> (oben/with-target
        {:type :inprocess
         :attrs {:timezone :cest}}
        (ast/parse `timezone-delta))
      o/constant->value)
  => 2)
 (m/fact
  (-> (oben/with-target
        {:type :inprocess
         :attrs {:timezone :cet}}
        (ast/parse `timezone-delta))
      o/constant->value)
  => 1))

;; (oben/with-temp-target
;;   (let [vec2 (oben/struct [^f32 x ^f32 y])
;;         vec2-len (oben/fn ^f32 [vec2 v]
;;                    (let [x (:x v)
;;                          y (:y v)]
;;                      (math/sqrt (+ (* x x) (* y y)))))]
;;     (m/fact
;;      (vec2-len {:x 3 :y 4}) => 5)))
