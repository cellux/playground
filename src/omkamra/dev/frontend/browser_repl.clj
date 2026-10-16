(ns omkamra.dev.frontend.browser-repl
  "Implementation of connected-browser runtime discovery and CLJS evaluation."
  (:require [omkamra.dev.frontend.shadow :as shadow]
            [shadow.cljs.devtools.api :as shadow-api]))

(defn- build-id
  [build]
  (if (keyword? build)
    build
    (keyword (str build))))

(defn- runtimes-for
  [build-id]
  (try
    (vec (shadow-api/repl-runtimes build-id))
    (catch clojure.lang.ExceptionInfo throwable
      (if (re-find #"has not been started yet" (or (ex-message throwable) ""))
        []
        (throw throwable)))))

(defn list-runtimes
  "List connected runtimes for all builds or one build.

  Before the embedded Shadow server starts, the result is empty."
  ([]
   (into (sorted-map)
         (map (fn [{:keys [build-id]}]
                [build-id (runtimes-for build-id)]))
         (shadow/list-builds)))
  ([build]
   (runtimes-for (build-id build))))

(defn cljs-eval
  "Evaluate CLJS source in a connected build/runtime.

  An explicit `:runtime-id` is required when more than one runtime is
  connected to the selected build."
  ([build source]
   (cljs-eval build source {}))
  ([build source options]
   (let [build-id (build-id build)
         options (or options {})
         runtimes (list-runtimes build-id)]
     (when (and (> (count runtimes) 1)
                (nil? (:runtime-id options)))
       (throw (ex-info "Multiple browser runtimes are connected"
                       {:build-id build-id
                        :runtime-ids (mapv :runtime-id runtimes)})))
     (shadow-api/cljs-eval build-id source options))))
