(ns oben-test
  (:require [oben])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types :as t])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y] (+ x y))]
    (m/fact (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))

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

;; forced casts
(oben/with-temp-context
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
            (let [g (fn ^i32 [^i16 x ^i8 y] (+ x y))]
              (g (cast! i16 x) (cast! i8 y))))]
    (m/fact (f 6 3) => 9.0)))

;; types in operator position can be also used for forced casts
(oben/with-temp-context
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
            (let [g (fn ^i32 [^i16 x ^i8 y] (+ x y))]
              (g (i16 x) (i8 y))))]
    (m/fact (f 6 3) => 9.0)))

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
            (and x y))]
    (m/fact (f 0x1234 0xff) => 0x34)
    (m/fact (f 0x1234 0xff00) => 0x1200))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (and x y))]
    (m/fact (f -2 0xff) => 0xfe)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (or x y))]
    (m/fact (f 0x1234 0xff) => 0x12ff)
    (m/fact (f 0x1234 0xff00) => 0xff34))
  (let [f (oben/fn ^s32 [^s32 x ^s32 y]
            (or x y))]
    (m/fact (f -2 0xff) => -1)))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y]
            (xor x y))]
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
