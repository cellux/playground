(ns omkamra.dev.http
  "A convention-based Ring HTTP server for playground namespaces."
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [omkamra.dev.cljs :as cljs]
            [org.httpkit.server :as http-kit]))

(def system
  "Portable Integrant configuration for the development HTTP server."
  {:omkamra.dev.cljs/compiler {}
   :omkamra.dev.http/server
   {:compiler (ig/ref :omkamra.dev.cljs/compiler)
    :host "127.0.0.1"
    :port 8080}})

(def ^:private valid-path-segment
  #"[-+*!?_A-Za-z][-+*!?_A-Za-z0-9'.]*")

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

(defn- resolve-handler
  "Resolve the public Ring handler named by a request URI.

  `/a/b/c/x` first resolves `a.b.c/x`, then falls back to
  `a.b.c.x/index`. Namespace loading is lazy, while Var lookup happens for
  every request so redefining a handler through nREPL takes effect immediately.
  Namespace compilation errors are deliberately not swallowed."
  [uri]
  (some resolve-handler-candidate
        (endpoint-symbols uri)))

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
  (or (cljs/asset-response compiler (:uri request))
      (when-let [ring-handler (resolve-handler (:uri request))]
        (@ring-handler request))
      (when-let [namespace-name (page-namespace (:uri request))]
        (cljs/page-response compiler namespace-name))
      (not-found request)))

(defmethod ig/init-key :omkamra.dev.http/server
  [_ {:keys [compiler] :as options}]
  (let [options (merge {:host "127.0.0.1"
                        :port 8080}
                       options)
        options (-> options
                    (assoc :ip (:host options))
                    (dissoc :host :compiler))]
    (http-kit/run-server (partial handler compiler) options)))

(defmethod ig/halt-key! :omkamra.dev.http/server
  [_ server]
  (server :timeout 100))
