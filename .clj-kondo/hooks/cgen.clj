(ns hooks.cgen
  (:require [clj-kondo.hooks-api :as api]))

(defn defn
  "For linting, a cgen definition is a normal var definition.  Its body is an
  embedded C-like language and must not be resolved as Clojure."
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))

(clojure.core/defn anonymous-fn
  "A cgen/fn body is embedded cgen data, not Clojure code."
  [_]
  {:node (api/token-node nil)})
