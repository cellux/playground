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
   :align-min 1
   ;; Defaults for the common LP64 C data model.
   :c-char-size 8
   :c-char-signed? true
   :c-short-size 16
   :c-int-size 32
   :c-long-size 64
   :c-float-size 32
   :c-double-size 64})

(defn create
  [{:keys [attrs target-layout] :as _opts}]
  (let [attrs (merge default-attrs attrs)]
    (map->DumpTarget
     {:ctx (ctx/create {:target-attrs attrs :target-layout target-layout})
      :attrs attrs})))
