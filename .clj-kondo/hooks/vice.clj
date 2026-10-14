(ns hooks.vice
  (:require [clj-kondo.hooks-api :as api]))

(defn with-binding-vector
  "Binding macros whose first argument is a vector of the form
  `[bound-symbol expression ...]`.

  Only the leading symbol is bound; the remaining elements are expressions.
  This lets macro bodies that use the bound connection or runtime resolve
  without unresolved-symbol warnings."
  [{:keys [node]}]
  (let [[_ bindings & body] (:children node)
        [bound] (:children bindings)
        let-node (api/list-node
                  (vec (concat [(api/token-node 'let)
                                (api/vector-node [bound (api/token-node nil)])]
                               body)))]
    {:node let-node}))
