(ns oben-test
  (:require [oben])
  (:require [oben.lang.context :as ctx])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.types :as t])
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

(oben/with-temp-context
  (let [f (oben/fn ^f32 [^i32 x ^i32 y]
            (let [g (fn ^i32 [^i16 x ^i8 y] (+ x y))]
              (g (cast! i16 x) (cast! i8 y))))]
    (m/fact (f 6 3) => 9.0)))
