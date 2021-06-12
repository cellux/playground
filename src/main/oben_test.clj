(ns oben-test
  (:require [oben])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [oben.core.types.Number :as Number])
  (:require [oben.core.types.Array :as Array])
  (:require [oben.core.types.Ptr :as Ptr])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m])
  (:use [midje.repl]))

(defn get-ctx
  [f]
  (assert (= (:kind (meta f)) :oben/FN))
  (let [fnode (:fnode (meta f))]
    (-> oben/*ctx*
        (ctx/next-epoch)
        (ctx/forget-node fnode)
        (ctx/compile-node fnode))))

(defn get-m
  [f]
  (:m (get-ctx f)))

(defn get-ir
  [f]
  (ir/render-module (get-m f)))

(defn dump-ir
  [f]
  (println (get-ir f)))

(defn gen-fname
  [name return-type lhs-type rhs-type]
  (symbol (str
           name
           \:
           (first (str lhs-type))
           (first (str rhs-type))
           "->"
           (first (str return-type)))))

(defmacro gen-param-list
  [return-type lhs-type rhs-type]
  `(with-meta [(with-meta (symbol "x") {:tag ~lhs-type})
               (with-meta (symbol "y") {:tag ~rhs-type})]
     {:tag ~return-type}))

(defmacro with-operator-functions
  [ops opnames types & body]
  (let [combos (for [[op name] (map vector ops opnames)
                     return-type types
                     lhs-type types
                     rhs-type types]
                 [op name return-type lhs-type rhs-type])]
    `(let ~(into [] (mapcat
                     (fn [[op name return-type lhs-type rhs-type]]
                       [(gen-fname name return-type lhs-type rhs-type)
                        `(oben/fn
                           ~(gen-param-list return-type lhs-type rhs-type)
                           (~op ~'x ~'y))])
                     combos))
       ~@body)))

#_(oben/with-temp-context
    (with-operator-functions
      [+ - * /]
      [add sub mul div]
      [u32 s32 f32]
      (m/fact (add:ii->i 1 2) => 3)))

(m/facts
 "type constructors memoize the types they return"
 (m/fact (identical? (Number/UInt 32) (Number/UInt 32)))
 (m/fact (identical? (Number/UInt 32) Number/%u32)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (+ 5 2))]
    (m/fact
     "a type tag on the param vector defines the return type of the function"
     (f) => 7)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y] (+ x y))]
    (m/fact
     "type tags on params define their types"
     (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-temp-context
  (let [f (oben/fn (Number/UInt 32) []
            (+ 5 2))]
    (m/fact
     "a list before the param vector is evaluated and moved to the type tag"
     (f) => 7)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [u32 x u32 y] (+ x y))]
    (m/fact
     "a symbol before a param is used as a type designator"
     (f 5 3) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [u32 a
                         ^u32 b
                         (Number/UInt 32) c
                         Number/%u32 d]
            (+ a b c d))]
    (m/fact
     "type designators"
     (f 5 3 1 6) => 15)))

(oben/with-temp-context
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
     (f 0) => 65535))
  (let [f (oben/fn ^u32 [u8 ^u16 x]
            (bit-not x))]
    (m/fact
     "type designators override existing type tag within metadata"
     (f 0) => 255)))

(oben/with-temp-context
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

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u16 x ^u8 y] (+ x y))]
    (m/fact (f 1 2) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (do)
            (do (+ x y))
            (do
              (+ x y)
              (+ 3 9)))]
    (m/fact (f 1 2) => 12)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^u32 x ^u32 y]
            (let [g (fn ^u32 [^u16 x ^u8 y] (+ x y))]
              (g (cast! u16 x) (cast! u8 y))))]
    (m/fact
     "forced casts"
     (f 6 3) => 9.0)))

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(m/fact (ast/parse 'u8) => Number/%u8)
(m/fact (ast/parse 's8) => Number/%s8)

(m/fact (t/type-of (ast/parse '(u8 0))) => Number/%u8)
(m/fact (t/type-of (ast/parse '(s8 0))) => Number/%s8)

(m/fact (t/type-of (ast/parse 0)) => Number/%u1)
(m/fact (t/type-of (ast/parse 1)) => Number/%u1)
(m/fact (t/type-of (ast/parse -1)) => Number/%s8)
(m/fact (t/type-of (ast/parse 5)) => Number/%u8)
(m/fact (t/type-of (ast/parse -5)) => Number/%s8)

(m/fact (t/type-of (ast/parse '(u8 -5))) => Number/%u8)
(m/fact
 "reinterpreting a signed value as unsigned does not change its bit pattern"
 (ast/constant-value (ast/parse '(u8 -5))) => -5)

(m/fact
 "negating an UInt turns it into an SInt"
 (t/type-of (ast/parse '(- 5))) => Number/%s8)

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and x y))]
    (m/fact (f 0x1234 0xff) => 0x34)
    (m/fact (f 0x1234 0xff00) => 0x1200))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-and x y))]
    (m/fact (f -2 0xff) => 0xfe)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-or x y))]
    (m/fact (f 0x1234 0xff) => 0x12ff)
    (m/fact (f 0x1234 0xff00) => 0xff34))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-or x y))]
    (m/fact (f -2 0xff) => -1)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-xor x y))]
    (m/fact (f 0x1234 0xff) => (+ 0x1200 (- 0xff 0x34)))
    (m/fact (f 0x1234 0xff00) => (+ 0x34 (- 0xff00 0x1200)))))

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
  (let [compare (oben/fn ^s8 [^u32 x ^u32 y]
                  (if (> x y) 1
                      (if (< x y) -1
                          0)))]
    (m/fact (compare 3 5) => -1)
    (m/fact (compare 5 3) => 1)
    (m/fact (compare 3 3) => 0)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^u8 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x]
            x)]
    (m/fact (f 5) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v 5]
              3))]
    (m/fact (f) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v 5]
              v))]
    (m/fact (f) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8 (+ 5 3))]
              @v))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)))]
    (m/fact (f) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v 3)))]
    (m/fact (f) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^s32 []
            (let [v (var u8)]
              (set! v 5)
              (set! v (- @v))))]
    (m/fact (f) => -5)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return (* @v @v))))]
    (m/fact (f) => 64)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (return @v)
              2))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [v (var u8)]
              (set! v (+ 5 3))
              (when (> @v 7)
                (set! v (+ @v 2))
                (set! v (+ @v 2)))
              (return @v)
              2))]
    (m/fact (f) => 12)))

(oben/with-temp-context
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

(oben/with-temp-context
  (let [f (oben/fn ^u8 [^u32 x]
            (if (not (< x 5))
              8 3))]
    (m/fact (f 4) => 3)
    (m/fact (f 5) => 8)
    (m/fact (f 6) => 8)))

(oben/with-temp-context
  (let [count-to (oben/fn ^u32 [^u32 limit]
                   (let [i (var u32 0)]
                     (while (< @i limit)
                       (set! i (+ @i 1)))
                     @i))]
    (m/fact (count-to 420) => 420)))

(oben/with-temp-context
  (let [clamp (oben/fn ^u32 [^u32 x ^u32 lo ^u32 hi]
                (if (< x lo)
                  lo
                  (if (> x hi)
                    hi
                    x)))]
    (m/fact (clamp 4 7 10) => 7)))

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
  (let [neg (oben/fn ^u32 [^u32 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)))

(oben/with-temp-context
  (let [neg (oben/fn ^u64 [^u64 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)
    (m/fact (neg -9223372036854775808) => 9223372036854775807)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-and-not x y))]
    (m/fact (f 0xff 0x40) => 0xbf)
    (m/fact (f 0xff 0x55) => 0xaa)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-left x y))]
    (m/fact (f 0xff 4) => 0xff0)
    (m/fact (f 0xff 1) => 0x1fe)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 x ^u32 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => 0x0f)
    (m/fact (f 0xff 1) => 0x7f))
  (let [f (oben/fn ^s8 [^s8 x ^u8 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => -1)))

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
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

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32 (+ 5 3))]
              (+ @v increment)))]
    (m/fact
     "vars with type and initializer"
     (f 7) => 15)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 increment]
            (let [v (var u32)]
              (set! v (+ 5 3))
              (+ @v increment)))]
    (m/fact
     "vars with type only"
     (f 7) => 15)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^u32 increment]
            (let [v (var (+ 5.0 3.75))]
              (+ @v increment)))]
    (m/fact
     "vars with initializer only"
     (f 7) => 15.75)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 []
            (let [v1 (var 5.0)
                  v2 (var 3.75)]
              (+ @v1 @v2)))]
    (m/fact
     (f) => 8.75)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 []
            (let [a (array u32 [9 8 7 6 5 4 3 2 1 0])]
              (get a 3)))]
    (m/fact
     "array literal supports get with constant index"
     (f) => 6)))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (array u32 [9 8 7 6 5 4 3 2 1 0]))]
              (get a (+ index 1))))]
    (m/fact
     "pointer to aggregate supports get with variable index"
     (f 3) => 5)))

(def u32array-3 (Array/Array Number/%u32 5))

(oben/with-temp-context
  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32array-3 [9 8 7 6 5]))]
              (get a (+ index (u8 3)))))]
    (m/fact
     "types can be used as value constructors"
     (f 1) => 5))

  (m/fact
   "narrowing conversions are rejected by default"
   (oben/fn ^u32 [^u32 index]
     (let [a (var (u32array-3 [9 8 7 6 5]))]
       (get a (+ index (u1 3)))))
   => (throws clojure.lang.ExceptionInfo #"rejected narrowing UInt->UInt conversion"))

  (let [f (oben/fn ^u32 [^u32 index]
            (let [a (var (u32array-3 [9 8 7 6 5]))]
              (get a (+ index (cast! u1 3)))))]
    (m/fact
     "narrowing conversions can be forced via cast!"
     (f 1) => 7)))

(oben/with-temp-context
  (let [f (oben/fn ^void [] 5)]
    (m/fact
     "void functions return nil"
     (f) => nil)))

(oben/with-temp-context
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

(m/fact
 (oben/fn ^void []
   (cast u1 2))
 => (throws clojure.lang.ExceptionInfo #"rejected narrowing UInt->UInt conversion"))

(oben/with-temp-context
  (let [atype (Array/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(Ptr/Ptr (Number/UInt 32)) a
                         ^u32 index]
            @(+ a index))]
    (m/fact (f a 3) => 6)))

(oben/with-temp-context
  (let [atype (Array/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a
                         ^u32 index]
            @(+ a index))]
    (m/fact
     "(* type) is a shortcut for (Ptr/Ptr type)"
     (f a 3) => 6)))

(oben/with-temp-context
  (let [atype (Array/Array Number/%u32 10)
        a (into-array Integer/TYPE [9 8 7 6 5 4 3 2 1 0])
        f (oben/fn ^u32 [(* u32) a]
            @a)]
    (m/fact
     (f a) => 9)))

(oben/with-temp-context
  (let [atype (Array/Array Number/%u32 10)
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
