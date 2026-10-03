(ns omkamra.dev.http
  "A convention-based Ring HTTP server for playground namespaces."
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [org.httpkit.server :as http-kit]))

(def system
  "Portable Integrant configuration for the development HTTP server."
  {:omkamra.dev.http/server
   {:host "127.0.0.1"
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

(defn- handler
  "A Ring handler mapping `/a/b/c/x` to the public Var `a.b.c/x`."
  [request]
  (if-let [handler (resolve-handler (:uri request))]
    (@handler request)
    (not-found request)))

(defmethod ig/init-key :omkamra.dev.http/server
  [_ options]
  (let [options (merge {:host "127.0.0.1"
                        :port 8080}
                       options)
        options (-> options
                    (assoc :ip (:host options))
                    (dissoc :host))]
    (http-kit/run-server handler options)))

(defmethod ig/halt-key! :omkamra.dev.http/server
  [_ server]
  (server :timeout 100))
