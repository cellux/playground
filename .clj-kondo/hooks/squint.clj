(ns hooks.squint
  (:require [clj-kondo.hooks-api :as api]))

(defn defclass
  "Squint's defclass body is embedded JavaScript class data."
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))
