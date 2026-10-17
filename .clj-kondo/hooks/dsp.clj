(ns hooks.dsp
  (:require [clj-kondo.hooks-api :as api]))

(defn defn
  "A DSP definition is a normal var definition; its body is embedded DSP data."
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))

(clojure.core/defn dsp-defstruct
  "A DSP struct is a normal var definition; its fields are embedded data."
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))

(clojure.core/defn dsp-fn
  "An anonymous DSP definition is a value whose body is embedded data; clj-kondo
  must not read the body as Clojure."
  [{:keys [node]}]
  (let [[_ & args] (api/sexpr node)]
    (when (empty? args)
      (api/reg-finding! (assoc (meta node)
                               :message "dsp/fn requires a parameter vector"
                               :type :omkamra.dsp/invalid-definition)))
    {:node (api/token-node nil)}))
