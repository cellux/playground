(ns oben.core
  (:refer-clojure :exclude [fn +])
  (:require [clojure.core :as clj])
  (:require [oben.types :as t])
  (:require [oben.ast :as ast])
  (:require [oben.util :as u])
  (:require [oben.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(def i1 (t/Int 1))
(def i8 (t/Int 8))
(def i16 (t/Int 16))
(def i32 (t/Int 32))
(def i64 (t/Int 64))

(def f32 (t/FP 32))
(def f64 (t/FP 64))

(defn fn
  [env params & body]
  (let [return-type (u/resolve-type-from-meta params)
        param-types (mapv u/resolve-type-from-meta params)
        param-names (mapv u/drop-meta params)
        params (mapv ast/function-parameter param-names param-types)
        env (into env (map vector param-names params))
        body-nodes (map #(ast/parse env %) body)]
    (t/with-type (t/Fn return-type param-types)
      (clj/fn [ctx]
        (let [return-type (t/compile return-type)
              ctx (reduce ctx/compile-node ctx params)
              f (ir/function (ctx/node-name ctx)
                             return-type
                             (map #(ctx/compiled ctx %) params))]
          (letfn [(compile-body-nodes [ctx]
                    (reduce ctx/compile-node ctx body-nodes))
                  (add-ret-if-needed [ctx]
                    (if-not (ir/terminator? (last (:bb ctx)))
                      (let [instr (ir/ret (:ir ctx))]
                        (-> ctx
                            (ctx/compile-instruction instr)
                            (assoc :ir instr)))
                      ctx))
                  (add-function-to-module [ctx]
                    (update ctx :m ir/add-function (:f ctx)))
                  (set-ir [ctx]
                    (assoc ctx :ir (:f ctx)))]
            (-> ctx
                (assoc :f f)
                compile-body-nodes
                add-ret-if-needed
                (ctx/flush-bb)
                add-function-to-module
                set-ir)))))))

(t/defmulti +)

(defmethod + [::t/Int ::t/Int]
  [x y]
  (t/with-type i32
    (clj/fn [ctx]
      (let [ctx (ctx/compile-nodes ctx x y)
            instr (ir/add (ctx/compiled ctx x)
                          (ctx/compiled ctx y)
                          {})]
        (-> ctx
            (ctx/compile-instruction instr)
            (assoc :ir instr))))))
