(ns omkamra.dev.frontend.shadow
  "Implementation of Shadow CLJS build and generated-asset management."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [integrant.core :as ig]
            [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as shadow-server])
  (:import [java.nio.file Files]))

(defn- build-id
  [build-or-namespace]
  (if (keyword? build-or-namespace)
    build-or-namespace
    (keyword (str build-or-namespace))))

(defn- browser-build
  [build-or-namespace]
  (let [build-id (build-id build-or-namespace)
        build (get-in (shadow/get-config) [:builds build-id])]
    (when (= :browser (:target build))
      (let [module (or (:omkamra.dev.frontend/module build)
                       (when (= 1 (count (:modules build)))
                         (ffirst (:modules build))))]
        (when (and (:output-dir build)
                   (:asset-path build)
                   module)
          {:build-id build-id
           :namespace (symbol (name build-id))
           :output-dir (:output-dir build)
           :asset-path (str/replace (:asset-path build) #"/$" "")
           :module module})))))

(defn list-builds
  "Return all registered, compatible browser build descriptors." []
  (->> (keys (get-in (shadow/get-config) [:builds]))
       sort
       (keep browser-build)
       vec))

(defn- shadow-not-started?
  [throwable]
  (and (instance? clojure.lang.ExceptionInfo throwable)
       (re-find #"has not been started yet" (or (ex-message throwable) ""))))

(defn worker-running?
  "Return whether Shadow is watching `build-id`.

  Returns false before the embedded Shadow server has been started."
  [build]
  (try
    (shadow/worker-running? (build-id build))
    (catch clojure.lang.ExceptionInfo throwable
      (if (shadow-not-started? throwable)
        false
        (throw throwable)))))

(defn recompile!
  "Synchronously recompile a running Shadow build."
  [build]
  (shadow/watch-compile! (build-id build)))

(defn- ensure-shadow-runtime!
  [{:keys [lock shadow-owned? shadow-started?]}]
  (locking lock
    (when-not @shadow-started?
      (let [result (shadow-server/start!)]
        (reset! shadow-started? true)
        (reset! shadow-owned?
                (= result ::shadow-server/started))))))

(defn- ensure-watch!
  [{:keys [lock workers] :as compiler} {:keys [build-id] :as page}]
  (locking lock
    (ensure-shadow-runtime! compiler)
    (when-not (shadow/worker-running? build-id)
      (let [result (shadow/watch build-id {:autobuild true})]
        (when-not (#{:watching :already-watching} result)
          (throw (ex-info "Shadow CLJS watch failed"
                          {:build-id build-id
                           :result result})))
        (swap! workers conj build-id)))
    page))

(defn- content-type
  [file]
  (case (some-> (.getName file)
                (str/split #"\.")
                last
                str/lower-case)
    "css" "text/css; charset=utf-8"
    "html" "text/html; charset=utf-8"
    "js" "text/javascript; charset=utf-8"
    "json" "application/json; charset=utf-8"
    "map" "application/json; charset=utf-8"
    "svg" "image/svg+xml"
    "wasm" "application/wasm"
    "application/octet-stream"))

(defn- asset-file
  [{:keys [asset-path output-dir]} uri]
  (let [prefix (str asset-path "/")]
    (when (str/starts-with? uri prefix)
      (let [relative-path (subs uri (count prefix))
            root (.getCanonicalFile (io/file output-dir))
            file (.getCanonicalFile (io/file root relative-path))
            root-path (str (.getPath root) java.io.File/separator)]
        (when (and (not (str/blank? relative-path))
                   (str/starts-with? (.getPath file) root-path)
                   (.isFile file))
          file)))))

(defn asset-response
  "Watch and return a Ring response for a generated app asset, if any."
  [compiler uri]
  (some (fn [{:keys [asset-path] :as page}]
          (when (str/starts-with? uri (str asset-path "/"))
            (ensure-watch! compiler page)
            (when-let [file (asset-file page uri)]
              {:status 200
               :headers {"content-type" (content-type file)}
               :body (Files/readAllBytes (.toPath file))})))
        (list-builds)))

(defn page-response
  "Start watching the registered page on its first request and return its HTML."
  [compiler namespace-name]
  (when-let [{:keys [asset-path module] :as page} (browser-build namespace-name)]
    (ensure-watch! compiler page)
    {:status 200
     :headers {"content-type" "text/html; charset=utf-8"}
     :body (str "<!doctype html>\n"
                "<html>\n"
                "  <head><meta charset=\"utf-8\"></head>\n"
                "  <body>\n"
                "    <script src=\"" asset-path "/" (name module) ".js\"></script>\n"
                "  </body>\n"
                "</html>\n")}))

(defmethod ig/init-key :omkamra.dev.frontend/compiler
  [_ _options]
  {:lock (Object.)
   :shadow-owned? (atom false)
   :shadow-started? (atom false)
   :workers (atom #{})})

(defmethod ig/halt-key! :omkamra.dev.frontend/compiler
  [_ {:keys [lock shadow-owned? shadow-started? workers]}]
  (locking lock
    (doseq [build-id @workers]
      (when (shadow/worker-running? build-id)
        (shadow/stop-worker build-id)))
    (reset! workers #{})
    (when (and @shadow-started? @shadow-owned?)
      (shadow-server/stop!))))
