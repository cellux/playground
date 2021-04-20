(ns oben-test
  (:require [oben])
  (:require [oben.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [midje.sweet :as m]))

(defmacro with-temp-context
  [& body]
  `(let [ctx# (ctx/new)]
     (oben/with-context ctx#
       (let [result# (do ~@body)]
         (ctx/dispose ctx#)
         result#))))

(with-temp-context
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
