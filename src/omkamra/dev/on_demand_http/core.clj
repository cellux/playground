(ns omkamra.dev.on-demand-http.core
  "Implementation of the convention-based Ring and Shadow page server."
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [omkamra.dev.frontend.shadow :as frontend]
            [omkamra.dev.systems :as systems]
            [org.httpkit.server :as http-kit]))

(def system
  "Portable Integrant configuration for the on-demand HTTP server."
  {:omkamra.dev.frontend/compiler {}
   :omkamra.dev.on-demand-http/server
   {:compiler (ig/ref :omkamra.dev.frontend/compiler)
    :host "127.0.0.1"
    :port 8080}})

(def ^:private valid-path-segment
  #"[-+*!?_A-Za-z][-+*!?_A-Za-z0-9'.]*")

(defn server-config
  "Return the declared server options without Integrant references." []
  (dissoc (get system :omkamra.dev.on-demand-http/server)
          :compiler))

(defn- endpoint-symbols
  [uri]
  (let [parts (-> (or uri "")
                  (str/split #"/"))
        parts (if (= [""] parts)
                []
                (vec (remove str/blank? parts)))]
    (when (and (<= 2 (count parts))
               (every? #(re-matches valid-path-segment %) parts))
      [[(symbol (str/join "." (butlast parts)))
        (symbol (last parts))]
       [(symbol (str/join "." parts))
        (symbol "index")]])))

(defn- resolve-handler-candidate
  [[namespace-name var-name]]
  (try
    (require namespace-name)
    (when-let [v (ns-resolve (find-ns namespace-name) var-name)]
      (let [metadata (meta v)]
        (when (and (= namespace-name (-> metadata :ns ns-name))
                   (not (:private metadata))
                   (ifn? @v))
          v)))
    (catch java.io.FileNotFoundException _
      nil)))

(defn resolve-handler
  "Return the Ring handler selected by `uri` as an agent-facing descriptor.

  The descriptor includes `:namespace`, `:var`, `:handler-var`, and a
  `:resolution` of `:direct` or `:index`. Namespace loading is lazy and Var
  lookup occurs on every call so nREPL redefinitions take effect immediately.
  Compilation errors are deliberately not swallowed."
  [uri]
  (some (fn [[resolution candidate]]
          (when-let [handler-var (resolve-handler-candidate candidate)]
            (let [[namespace-name var-name] candidate]
              {:uri uri
               :namespace namespace-name
               :var var-name
               :handler-var handler-var
               :resolution resolution})))
        (map vector [:direct :index] (endpoint-symbols uri))))

(defn- not-found
  [_request]
  {:status 404
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body "Not found"})

(defn- page-namespace
  [uri]
  ;; The second handler candidate for /a/b/c is a.b.c/index, so its namespace
  ;; is the browser app's page namespace.
  (some-> (endpoint-symbols uri) second first))

(defn- handler
  "Serve generated CLJS assets, Ring handlers, then registered CLJS pages."
  [compiler request]
  (or (frontend/asset-response compiler (:uri request))
      (when-let [descriptor (resolve-handler (:uri request))]
        (let [handler-var (:handler-var descriptor)]
          (@handler-var request)))
      (when-let [namespace-name (page-namespace (:uri request))]
        (frontend/page-response compiler namespace-name))
      (not-found request)))

(defmethod ig/init-key :omkamra.dev.on-demand-http/server
  [_ {:keys [compiler] :as options}]
  (let [options (merge (server-config) options)
        options (-> options
                    (assoc :ip (:host options))
                    (dissoc :host :compiler))]
    (http-kit/run-server (partial handler compiler) options)))

(defmethod ig/halt-key! :omkamra.dev.on-demand-http/server
  [_ server]
  (server :timeout 100))

(defn start!
  "Start the on-demand HTTP system." []
  (systems/start! :on-demand-http))

(defn stop!
  "Stop the on-demand HTTP system." []
  (systems/stop! :on-demand-http))

(defn restart!
  "Restart the on-demand HTTP system." []
  (systems/restart! :on-demand-http))
