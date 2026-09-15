(ns oben.core.target.dump
  (:require
   [clojure.pprint :refer [pprint]]
   [oben.core.target :as target]
   [oben.core.protocols.Target :as Target]
   [oben.core.context :as ctx]
   [oben.compiler :as compiler]
   [omkamra.llvm.platform :as platform]))

(defrecord DumpTarget [ctx attrs]
  Target/protocol

  (compile-function [this fnode]
    (let [{:keys [ctx source]} (compiler/compile-function this ctx fnode)]
      (assoc this :ctx ctx :module-source source)))

  (invoke-function [this fnode args]
    (let [module-src (:module-source this)]
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
