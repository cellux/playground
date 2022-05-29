(ns oben.core.target.dump
  (:require [oben.core.target :as target])
  (:require [oben.core.protocols.Target :as Target])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [omkamra.llvm.platform :as platform]))

(defrecord DumpTarget [ctx attrs]
  Target/protocol

  (compile-function [this fnode]
    (assoc this :ctx (-> ctx
                         (ctx/next-epoch)
                         (ctx/forget-node fnode)
                         (ctx/compile-node fnode))))

  (invoke-function [this fnode args]
    (let [m (-> (:m ctx)
                (assoc :data-layout platform/data-layout
                       :target-triple platform/target-triple))]
      (if (-> this :attrs :print-m)
        (-> m clojure.pprint/pprint)
        (-> m ir/render-module println))))

  (dispose [this]
    this))

(defn create
  [{:keys [attrs] :as opts}]
  (map->DumpTarget
   {:ctx (ctx/create)
    :attrs (assoc attrs
                  :address-size platform/address-size)}))
