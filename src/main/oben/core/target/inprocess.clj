(ns oben.core.target.inprocess
  (:require [oben.core.target :as target])
  (:require [oben.core.protocols.Target :as Target])
  (:require [oben.core.context :as ctx]))

(defrecord InProcessTarget [ctx]
  Target/protocol

  (platform [this]
    :TODO)

  (compile-function [this fnode]
    (let [ctx (ctx/compile-node (ctx/next-epoch ctx) fnode)
          ctx (ctx/assemble-module ctx)]
      (assoc this :ctx ctx)))

  (invoke-function [this fnode args]
    (let [f (ctx/compiled-node ctx fnode)
          invoker (ctx/invoker ctx f)]
      (apply invoker args)))

  (dispose [this]
    (let [ctx (ctx/dispose-llvm-execution-engine ctx)
          ctx (ctx/dispose-llvm-context ctx)]
      (assoc this :ctx ctx))))

(defn create
  [opts]
  (map->InProcessTarget
   {:ctx (ctx/create)}))
