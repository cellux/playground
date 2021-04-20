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

(oben/defmacro fn
  [params & body]
  (let [return-type (u/resolve-type-from-meta params)
        param-types (mapv u/resolve-type-from-meta params)
        param-names (mapv u/drop-meta params)
        params (mapv ast/function-parameter param-names param-types)
        &env (into &env (map vector param-names params))
        &env (assoc &env :return-type return-type)
        body (let [last-item (last body)]
               (if (and (sequential? last-item)
                        (= 'return (first last-item)))
                 body
                 (concat (butlast body) [(list 'return last-item)])))
        body-nodes (map #(ast/parse &env %) body)]
    (ast/make-node (t/Fn return-type param-types)
      (clj/fn [ctx]
        (let [fname (ctx/node-name ctx)
              return-type (t/compile return-type)
              ctx (reduce ctx/compile-node ctx params)
              f (ir/function fname
                             return-type
                             (map #(ctx/compiled ctx %) params))]
          (letfn [(add-function-to-module [ctx]
                    (update ctx :m ir/add-function (:f ctx)))
                  (store-ir [ctx]
                    (ctx/store-ir ctx (:f ctx)))]
            (-> ctx
                (assoc :f f)
                (ctx/compile-nodes body-nodes)
                add-function-to-module
                store-ir)))))))

(oben/defmacro return
  ([form]
   (let [node (t/cast (:return-type &env) (ast/parse &env form))]
     (ast/make-node (t/Return)
       (clj/fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (let [ctx (ctx/compile-node ctx node)
               instr (ir/ret (ctx/compiled ctx node))]
           (ctx/compile-instruction ctx instr))))))
  ([]
   (if (= ::t/None (:return-type &env))
     (ast/make-node (t/Return)
       (clj/fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (ctx/compile-instruction ctx (ir/ret))))
     (throw (ex-info "returning void when scope expects non-void type"
                     {:return-type (:return-type &env)})))))

(defn zext
  [node size]
  (let [result-type (t/resize (t/type-of node) size)]
    (ast/make-node result-type
      (clj/fn [ctx]
        (let [ctx (ctx/compile-node ctx node)
              instr (ir/zext (ctx/compiled ctx node)
                             (t/compile result-type)
                             {})]
          (ctx/compile-instruction ctx instr))))))

(oben/defmulti +)

(defmethod + [::t/Int ::t/Int]
  [x y]
  (let [[s1 s2] (map (comp :size t/type-of) [x y])
        result-size (max s1 s2)
        result-type (t/Int result-size)
        x (if (< s1 result-size) (zext x result-size) x)
        y (if (< s2 result-size) (zext y result-size) y)]
    (ast/make-node result-type
      (clj/fn [ctx]
        (let [ctx (ctx/compile-nodes ctx [x y])
              instr (ir/add (ctx/compiled ctx x)
                            (ctx/compiled ctx y)
                            {})]
          (ctx/compile-instruction ctx instr))))))
