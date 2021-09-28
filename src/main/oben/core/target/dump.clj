(ns oben.core.target.dump
  (:require [oben.core.target :as target])
  (:require [oben.core.protocols.Target :as Target])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir]))

(defrecord DumpTarget [ctx]
  Target/protocol

  (platform [this]
    :TODO)

  (compile-function [this fnode]
    (assoc this :ctx (-> ctx
                         (ctx/next-epoch)
                         (ctx/forget-node fnode)
                         (ctx/compile-node fnode))))

  (invoke-function [this fnode args]
    (-> (:m ctx)
        ir/render-module
        println))

  (dispose [this]
    this))

(defn create
  [opts]
  (map->DumpTarget
   {:ctx (ctx/create)}))
