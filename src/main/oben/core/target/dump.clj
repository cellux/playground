(ns oben.core.target.dump
  (:require
   [clojure.pprint :refer [pprint]]
   [oben.core.target :as target]
   [oben.core.protocols.Target :as Target]
   [oben.core.context :as ctx]
   [oben.core.compiler :as compiler]
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
  (merge {:address-size platform/address-size
          :align-min 1}
         (target/common-lp64-c-attrs platform/address-size)))

(defn create
  [{:keys [attrs target-layout] :as _opts}]
  (let [attrs (merge default-attrs attrs)]
    (map->DumpTarget
     {:ctx (ctx/create {:target-attrs attrs :target-layout target-layout})
      :attrs attrs})))
