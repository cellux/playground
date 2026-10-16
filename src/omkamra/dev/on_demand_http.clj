(ns omkamra.dev.on-demand-http
  "Serve arbitrary Ring handlers and registered Shadow pages on demand.

  Start here when an agent needs the development HTTP system, its discovered
  Ring endpoint convention, or its server configuration. The implementation
  lives in `omkamra.dev.on-demand-http.core`; frontend build and browser
  runtime operations live in `omkamra.dev.frontend`."
  (:require [omkamra.dev.on-demand-http.core :as core]))

(def system
  "Portable Integrant configuration for the `:on-demand-http` system."
  core/system)

(defn server-config
  "Return the declared effective HTTP server options." []
  (core/server-config))

(defn resolve-handler
  "Return an informative descriptor for the public Ring handler selected by a URI."
  [uri]
  (core/resolve-handler uri))

(defn start!
  "Start the `:on-demand-http` system." []
  (core/start!))

(defn stop!
  "Stop the `:on-demand-http` system." []
  (core/stop!))

(defn restart!
  "Restart the `:on-demand-http` system." []
  (core/restart!))
