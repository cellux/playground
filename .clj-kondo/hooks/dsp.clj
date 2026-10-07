(ns hooks.dsp
  (:require [clj-kondo.hooks-api :as api]))

(defn defn
  "A DSP definition is a normal var definition; its body is embedded DSP data."
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))
