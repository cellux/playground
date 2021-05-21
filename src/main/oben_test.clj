(ns oben-test
  (:require [oben])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [oben.core.types :as t*])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m])
  (:use [midje.repl]))

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
      [i32 s32 f32]
      (m/fact (add:ii->i 1 2) => 3)))

(m/facts
 "type constructors memoize the types they return"
 (m/fact (identical? (t/Int 32) (t/Int 32)))
 (m/fact (identical? (t/Int 32) t/%i32)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (+ 5 2))]
    (m/fact (f) => 7)))

(oben/with-temp-context
  (let [f (oben/fn ^{:tag (t*/Int 32)} []
            (+ 5 2))]
    (m/fact
     "values can be tagged with types constructed on the spot"
     (f) => 7)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y] (+ x y))]
    (m/fact (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (+ x y)
            (+ 5 2))]
    (m/fact (f 1 2) => 7)
    (m/fact (f 5 3) => 7)))

(oben/defn add
  ^i32 [^i32 x ^i32 y]
  (+ x y))

(m/facts
 (m/fact (add (add 1 2) (add 7 -2)) => 8))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i16 x ^i8 y] (+ x y))]
    (m/fact (f 1 2) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (do)
            (do (+ x y))
            (do
              (+ x y)
              (+ 3 9)))]
    (m/fact (f 1 2) => 12)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
            (let [g (fn ^i32 [^i16 x ^i8 y] (+ x y))]
              (g (cast! i16 x) (cast! i8 y))))]
    (m/fact
     "forced casts"
     (f 6 3) => 9.0)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
            (let [g (fn ^i32 [^i16 x ^i8 y] (+ x y))]
              (g (i16 x) (i8 y))))]
    (m/fact
     "types in operator position can be also used for forced casts"
     (f 6 3) => 9.0)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (+ x y))]
    (m/fact (f 6 3) => 9))
  (let [f (oben/fn ^s32 [^i32 x ^s32 y]
            (+ x y))]
    (m/fact (f 6 -3) => 3))
  (let [f (oben/fn ^s32 [^s32 x ^i32 y]
            (+ x y))]
    (m/fact (f -3 6) => 3))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (+ x y))]
    (m/fact (f -3 -6) => -9)))

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^f32 x ^f32 y]
            (+ x y))]
    (m/fact (f 6.5 3.25) => 9.75))
  (let [f (oben/fn ^f32 [^f32 x ^i32 y]
            (+ x y))]
    (m/fact (f 6.5 3) => 9.5))
  (let [f (oben/fn ^f32 [^i32 x ^f32 y]
            (+ x y))]
    (m/fact (f 3 6.5) => 9.5))
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
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

(oben/with-temp-context
  (let [f (oben/fn ^s32 [^i32 x]
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
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
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
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-and x y))]
    (m/fact (f 0x1234 0xff) => 0x34)
    (m/fact (f 0x1234 0xff00) => 0x1200))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-and x y))]
    (m/fact (f -2 0xff) => 0xfe)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-or x y))]
    (m/fact (f 0x1234 0xff) => 0x12ff)
    (m/fact (f 0x1234 0xff00) => 0xff34))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (bit-or x y))]
    (m/fact (f -2 0xff) => -1)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-xor x y))]
    (m/fact (f 0x1234 0xff) => (+ 0x1200 (- 0xff 0x34)))
    (m/fact (f 0x1234 0xff00) => (+ 0x34 (- 0xff00 0x1200)))))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (= x y))]
    (m/fact (f -3 -5) => 0)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 3 3) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (!= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (!= x y))]
    (m/fact (f -3 -5) => 1)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (!= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 3 3) => 0)))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (< x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (< x y))]
    (m/fact (f -3 5) => 1)
    (m/fact (f 5 -3) => 0)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (< x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 0)
    (m/fact (f 3.25 3.25) => 0)
    (m/fact (f 3.2 3.3) => 1)
    (m/fact (f 3.3 3.2) => 0)))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (<= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (<= x y))]
    (m/fact (f -3 5) => 1)
    (m/fact (f 5 -3) => 0)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (<= x y))]
    (m/fact (f 3 5) => 1)
    (m/fact (f 5 3) => 0)
    (m/fact (f 3 3) => 1)
    (m/fact (f 3.25 3.25) => 1)
    (m/fact (f 3.2 3.3) => 1)
    (m/fact (f 3.3 3.2) => 0)))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (>= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 1))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (>= x y))]
    (m/fact (f -3 5) => 0)
    (m/fact (f 5 -3) => 1)
    (m/fact (f -3 -3) => 1))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (>= x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 1)
    (m/fact (f 3.25 3.25) => 1)
    (m/fact (f 3.2 3.3) => 0)
    (m/fact (f 3.3 3.2) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^i1 [^i32 x ^i32 y]
            (> x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 0))
  (let [f (oben/fn ^i1 [^s32 x ^s32 y]
            (> x y))]
    (m/fact (f -3 5) => 0)
    (m/fact (f 5 -3) => 1)
    (m/fact (f -3 -3) => 0))
  (let [f (oben/fn ^i1 [^f32 x ^f32 y]
            (> x y))]
    (m/fact (f 3 5) => 0)
    (m/fact (f 5 3) => 1)
    (m/fact (f 3 3) => 0)
    (m/fact (f 3.25 3.25) => 0)
    (m/fact (f 3.2 3.3) => 0)
    (m/fact (f 3.3 3.2) => 1)))

(oben/with-temp-context
  (let [compare-s8 (oben/fn ^s8 [^i32 x ^i32 y]
                     (if (> x y) 1 -1))
        compare-s16 (oben/fn ^s16 [^i32 x ^i32 y]
                      (if (> x y) 1 -1))
        compare-s32 (oben/fn ^s32 [^i32 x ^i32 y]
                      (if (> x y) 1 -1))]
    (m/fact (compare-s8 3 5) => -1)
    (m/fact (compare-s8 5 3) => 1)
    (m/fact (compare-s16 3 5) => -1)
    (m/fact (compare-s16 5 3) => 1)
    (m/fact (compare-s32 3 5) => -1)
    (m/fact (compare-s32 5 3) => 1)))

(oben/with-temp-context
  (let [compare (oben/fn ^s8 [^i32 x ^i32 y]
                  (if (> x y) 1
                      (if (< x y) -1
                          0)))]
    (m/fact (compare 3 5) => -1)
    (m/fact (compare 5 3) => 1)
    (m/fact (compare 3 3) => 0)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^i8 []
            1)]
    (m/fact (f) => 1)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x]
            x)]
    (m/fact (f 5) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v 5]
              3))]
    (m/fact (f) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v 5]
              v))]
    (m/fact (f) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8 (+ 5 3))]
              v))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v 5)))]
    (m/fact (f) => 5)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v 5)
              (set! v 3)))]
    (m/fact (f) => 3)))

(oben/with-temp-context
  (let [f (oben/fn ^s32 []
            (let [v (var i8)]
              (set! v 5)
              (set! v (- v))))]
    (m/fact (f) => -5)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v (+ 5 3))
              (return v)))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v (+ 5 3))
              (return (* v v))))]
    (m/fact (f) => 64)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v (+ 5 3))
              (return v)
              2))]
    (m/fact (f) => 8)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 []
            (let [v (var i8)]
              (set! v (+ 5 3))
              (when (> v 7)
                (set! v (+ v 2))
                (set! v (+ v 2)))
              (return v)
              2))]
    (m/fact (f) => 12)))

(m/fact
 (t/ubertype-of (t/Ptr (t/Int 32))
                 (t/Int 8)) => (t/Int 32))

(oben/with-temp-context
  (let [count-to (oben/fn ^i32 [^i32 limit]
                   (let [i (var i32 0)]
                     (tagbody
                      :loop
                      (when (>= i limit)
                        (return i))
                      (set! i (+ i 1))
                      (go :loop))
                     7))]
    (m/fact (count-to 420) => 420)))

(oben/with-temp-context
  (let [f (oben/fn ^i8 [^i32 x]
            (if (not (< x 5))
              8 3))]
    (m/fact (f 4) => 3)
    (m/fact (f 5) => 8)
    (m/fact (f 6) => 8)))

(oben/with-temp-context
  (let [count-to (oben/fn ^i32 [^i32 limit]
                   (let [i (var i32 0)]
                     (while (< i limit)
                       (set! i (+ i 1)))
                     i))]
    (m/fact (count-to 420) => 420)))

(oben/with-temp-context
  (let [clamp (oben/fn ^i32 [^i32 x ^i32 lo ^i32 hi]
                (if (< x lo)
                  lo
                  (if (> x hi)
                    hi
                    x)))]
    (m/fact (clamp 4 7 10) => 7)))

(oben/with-temp-context
  (let [div-six (oben/fn ^i1 [^i32 x]
                         (if (and (= (% x 2) 0)
                                  (= (% x 3) 0))
                           1 0))]
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
  (let [div-2or3 (oben/fn ^i1 [^i32 x]
                  (if (or (= (% x 2) 0)
                          (= (% x 3) 0))
                    1 0))]
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
  (let [neg (oben/fn ^i32 [^i32 x]
              (bit-not x))]
    (m/fact (neg 0) => -1)
    (m/fact (neg -1) => 0)
    (m/fact (neg -2) => 1)
    (m/fact (neg -128) => 127)
    (m/fact (neg -32768) => 32767)
    (m/fact (neg -2147483648) => 2147483647)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-and-not x y))]
    (m/fact (f 0xff 0x40) => 0xbf)
    (m/fact (f 0xff 0x55) => 0xaa)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-shift-left x y))]
    (m/fact (f 0xff 4) => 0xff0)
    (m/fact (f 0xff 1) => 0x1fe)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => 0x0f)
    (m/fact (f 0xff 1) => 0x7f))
  (let [f (oben/fn ^s8 [^s8 x ^i8 y]
            (bit-shift-right x y))]
    (m/fact (f 0xff 4) => -1)))

(oben/with-temp-context
  (let [clamp (oben/fn ^i32 [^i32 x ^i32 lo ^i32 hi]
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
  (let [select (oben/fn ^s32 [^i32 x]
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
  (let [select (oben/fn ^s32 [^i32 x]
                 (case x
                   1 -2
                   2 -4
                   3 -6
                   10))]
    (m/fact (select 1) => -2)
    (m/fact (select 2) => -4)
    (m/fact (select 3) => -6)
    (m/fact (select 4) => 10)))
