(ns oben.core.target.dump
  (:require
   [clojure.pprint :refer [pprint]]
   [oben.core.target :as target]
   [oben.core.protocols.Target :as Target]
   [oben.core.context :as ctx]
   [omkamra.llvm.ir :as ir]
   [omkamra.llvm.platform :as platform]))

(defrecord DumpTarget [ctx attrs]
  Target/protocol

  (compile-function [this fnode]
    (if (ctx/compiled-node ctx fnode)
      this
      (let [ctx (ctx/next-epoch ctx)
            ctx (ctx/compile-node ctx fnode)]
        (assoc this :ctx ctx))))

  (invoke-function [this fnode args]
    (let [m (assoc (:m ctx)
                   :data-layout platform/data-layout
                   :target-triple platform/target-triple)
          module-src (try
                       (ir/render-module m)
                       (catch Throwable _
                         m))]
      (if (string? module-src)
        (println module-src)
        (pprint module-src))))

  (dispose [this]
    this))

(def default-attrs
  {:address-size platform/address-size
   :align-min 1})

(defn create
  [{:keys [attrs] :as opts}]
  (map->DumpTarget
   {:ctx (ctx/create)
    :attrs (merge default-attrs attrs)}))
