(ns omkamra.dev.systems
  "Discover and control the generic Integrant systems listed in dev.edn.

  Start here when an agent needs to inspect available development systems or
  start, stop, restart, and shut them down. This namespace is intentionally a
  lightweight facade; lifecycle implementation details live in
  `omkamra.dev.systems.core`."
  (:require [omkamra.dev.systems.core :as core]))

(defn load-config!
  "Read and validate the development systems configuration from dev.edn."
  ([]
   (core/load-config!))
  ([path]
   (core/load-config! path)))

(defn list-systems
  "Return configured systems with compact current lifecycle status records." []
  (core/list-systems))

(defn status
  "Return the compact lifecycle status record for one system." [name]
  (core/status name))

(defn start!
  "Start the named system from dev.edn." [name]
  (core/start! name))

(defn stop!
  "Stop the named system if it is running." [name]
  (core/stop! name))

(defn restart!
  "Stop and start the named system." [name]
  (core/restart! name))

(defn shutdown!
  "Stop every currently running system." []
  (core/shutdown!))
