(ns oben.core.target.llvm-server
  "An Oben target backed by the local llvm-http-server byte-buffer ABI."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [oben.core.compiler :as compiler]
            [oben.core.abi :as abi]
            [oben.core.context :as ctx]
            [oben.core.protocols.Target :as Target]
            [omkamra.llvm.platform :as platform])
  (:import [java.util UUID]))

(def default-url "http://127.0.0.1:18080")

(def default-attrs
  (merge {:address-size platform/address-size
          :align-min 1}
         (target/common-lp64-c-attrs platform/address-size)))

(defn- endpoint
  [url & segments]
  (str (str/replace url #"/+$" "") "/" (str/join "/" segments)))

(defn- request!
  [request]
  (let [{:keys [status body] :as response}
        (http/request (assoc request :throw-exceptions false))]
    (if (<= 200 status 299)
      response
      (throw (ex-info "llvm-server request failed"
                      {:status status :body body :url (:url request)})))))

(defn- load-module!
  [url namespace module source]
  (request! {:method :put
             :url (endpoint url namespace "modules" module)
             :content-type "text/plain"
             :body source}))

(defn- invoke!
  [url namespace {:keys [module] :as compiled} args]
  (let [response (request! {:method :post
                            :url (str (endpoint url namespace "modules" module
                                                "symbols" (:adapter compiled) "invoke")
                                      "?output_capacity=" (:output-size compiled))
                            :content-type "application/octet-stream"
                            :as :byte-array
                            :body (abi/encode-args compiled args)})
        return-code (some-> (get (:headers response) "x-llvm-return-code")
                             Long/parseLong)]
    (when-not (= return-code (long (:output-size compiled)))
      (throw (ex-info "llvm-server function invocation failed"
                      {:return-code return-code :module module
                       :adapter (:adapter compiled)})))
    (let [body ^bytes (:body response)]
      (abi/decode-result compiled body))))

(defrecord LLVMServerTarget [ctx attrs url namespace modules next-module-id]
  Target/protocol

  (compile-function [this fnode]
    (if (contains? modules fnode)
      this
      (let [{:keys [ctx source function]} (compiler/compile-function this ctx fnode)
            _ (when-not (abi/supported-function? fnode function)
                (throw (ex-info "llvm-server byte-buffer ABI does not support pointer values"
                                {:function function})))
            module-id (str "oben-" next-module-id)
            adapter-name (str "oben_entry_" next-module-id)
            compiled (abi/function-abi ctx fnode function adapter-name)
            source (compiler/verify-module-source!
                    (str source "\n" (abi/adapter-source compiled)))]
        (load-module! url namespace module-id source)
        (assoc this
               :ctx ctx
               :modules (assoc modules fnode
                               (assoc compiled :module module-id))
               :next-module-id (inc next-module-id)))))

  (invoke-function [_ fnode args]
    (if-let [compiled (get modules fnode)]
      (invoke! url namespace compiled args)
      (throw (ex-info "function was not compiled for llvm-server target"
                      {:function fnode}))))

  (dispose [this]
    (try
      (request! {:method :delete :url (endpoint url namespace)})
      (catch clojure.lang.ExceptionInfo e
        (when-not (= 404 (:status (ex-data e)))
          (throw e))))
    this))

(defn create
  [{:keys [attrs url namespace target-layout] :as _opts}]
  (let [attrs (merge default-attrs attrs)]
    (map->LLVMServerTarget
     {:ctx (ctx/create {:target-attrs attrs :target-layout target-layout})
      :attrs attrs
      :url (or url default-url)
      :namespace (or namespace (str "oben-" (UUID/randomUUID)))
      :modules {}
      :next-module-id 1})))
