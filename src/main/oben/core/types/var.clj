(ns oben.core.types.var
  (:require [oben.core.types :as t])
  (:require [oben.core.types.ptr :as ptr])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(defn %var
  ([type init-node]
   (let [init-node (when init-node (ast/parse `(cast ~type ~init-node)))]
     (ast/make-node (ptr/Ptr type)
       (fn [ctx]
         (let [ins (ir/alloca
                    (t/compile type)
                    {:name (keyword (ctx/get-assigned-name ctx))})
               compile-var (fn [ctx]
                             (ctx/compile-instruction ctx ins))
               compile-store (fn [ctx]
                               (ctx/compile-instruction
                                ctx (ir/store
                                     (ctx/compiled ctx init-node)
                                     ins
                                     {})))
               compile-init (fn [ctx]
                              (if init-node
                                (-> ctx
                                    (ctx/compile-node init-node)
                                    compile-store)
                                ctx))
               save-ir (fn [ctx]
                         (ctx/save-ir ctx ins))]
           (-> ctx
               (ctx/with-blockbin :entry compile-var)
               (ctx/with-blockbin :init compile-init)
               save-ir)))
       {:class :oben/var
        :children (when init-node #{init-node})})))
  ([type]
   (cond (t/type? type) (%var type nil)
         (ast/node? type) (let [init-node type
                                type (t/type-of init-node)]
                            (%var type init-node))
         :else (throw (ex-info "invalid var form")))))
