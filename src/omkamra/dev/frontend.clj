(ns omkamra.dev.frontend
  "Discover and operate the project's on-demand Shadow CLJS frontend.

  Start here when an agent needs to inspect browser builds, check connected
  runtimes, synchronously recompile a running build, or evaluate CLJS in a
  connected page. Build watchers are started automatically by
  `omkamra.dev.on-demand-http` when a compatible page or asset is requested;
  this namespace intentionally does not expose a separate watch operation.
  `cljs-eval` through the Clojure nREPL is the normal evaluation path. When
  evaluation specifically requires browser-context JavaScript, use browser
  tooling and the connected page's Shadow client.

  This namespace is a lightweight facade. Implementations live in
  `omkamra.dev.frontend.shadow` and `omkamra.dev.frontend.browser-repl`."
  (:require [omkamra.dev.frontend.browser-repl :as browser-repl]
            [omkamra.dev.frontend.shadow :as shadow]))

(defn list-builds
  "Return all compatible registered browser build descriptors." []
  (shadow/list-builds))

(defn worker-running?
  "Return whether Shadow is watching `build`."
  [build]
  (shadow/worker-running? build))

(defn recompile!
  "Synchronously recompile a running Shadow build."
  [build]
  (shadow/recompile! build))

(defn list-runtimes
  "List connected runtimes for all builds or one build."
  ([]
   (browser-repl/list-runtimes))
  ([build]
   (browser-repl/list-runtimes build)))

(defn cljs-eval
  "Evaluate CLJS source in a connected build/runtime."
  ([build source]
   (browser-repl/cljs-eval build source))
  ([build source options]
   (browser-repl/cljs-eval build source options)))
