(ns oben.lang.core.macros
  (:require [oben.lang.core.types :refer [%void]])
  (:require [oben.lang.types :as t])
  (:require [oben.lang.ast :as ast])
  (:require [oben.lang.util :as u])
  (:require [oben.lang.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(oben/defmacro %let
  [[k v & rest] & body]
  (if rest
    `(let [~k ~v]
       (let [~@rest]
         ~@body))
    (ast/parse (assoc &env k (ast/parse &env v)) `(do ~@body))))

(oben/defmacro %fn
  [params & body]
  (let [return-type (u/resolve-type-from-meta params)
        param-types (mapv u/resolve-type-from-meta params)
        param-names (mapv u/drop-meta params)
        params (mapv ast/function-parameter param-names param-types)
        &env (into &env (map vector param-names params))
        &env (assoc &env :return-type return-type)
        body (if (= return-type %void)
               body
               (let [last-item (last body)]
                 (if (and (sequential? last-item)
                          (= 'return (first last-item)))
                   body
                   (concat (butlast body) [(list 'return last-item)]))))
        body-nodes (map #(ast/parse &env %) body)]
    (ast/make-node (t/Fn return-type param-types)
      (fn [ctx]
        (let [saved ctx
              fname (ctx/node-name ctx)
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
                (dissoc :bb)
                (ctx/compile-nodes body-nodes)
                add-function-to-module
                store-ir
                (merge (select-keys saved [:f :bb])))))))))

(oben/defmacro %return
  ([form]
   (let [node (ast/parse &env `(cast ~(:return-type &env) ~form))]
     (ast/make-node (t/Return)
       (fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (let [ctx (ctx/compile-node ctx node)
               ins (ir/ret (ctx/compiled ctx node))]
           (ctx/compile-instruction ctx ins))))))
  ([]
   (if (= %void (:return-type &env))
     (ast/make-node (t/Return)
       (fn [ctx]
         (when (seq (:bb ctx))
           (assert (not (ir/terminator? (last (:bb ctx))))))
         (ctx/compile-instruction ctx (ir/ret))))
     (throw (ex-info "returning void when scope expects non-void type"
                     {:return-type (:return-type &env)})))))
