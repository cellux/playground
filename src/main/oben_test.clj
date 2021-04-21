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

(defn get-ir
  [f]
  (assert (= (:kind (meta f)) :oben/FN))
  (let [fnode (:fnode (meta f))
        ctx (-> oben/*ctx*
                (ctx/next-epoch)
                (ctx/forget-node fnode)
                (ctx/compile-node fnode))]
    (ir/render-module (:m ctx))))

(oben/defn add
  ^i32 [^i32 x ^i32 y]
  (+ x y))

(m/facts
 (m/fact (add (add 1 2) (add 7 -2)) => 8))

(oben/with-temp-context
  (let [f (oben/fn ^i32 [^i16 x ^i8 y] (+ x y))]
    (m/fact (f 1 2) => 3)))
