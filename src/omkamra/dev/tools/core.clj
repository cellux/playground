(ns omkamra.dev.tools.core
  "Implementation for the discoverable development-tool facade.

  The public entrypoint is `omkamra.dev.tools`; this namespace owns path
  handling and the lint/format tool adapters. Namespace reloading lives in
  `omkamra.dev.tools.reload`."
  (:require [clj-kondo.core :as kondo]
            [cljfmt.config :as fmt-config]
            [cljfmt.core :as fmt]
            [cljfmt.tool :as fmt-tool]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- path-value?
  [value]
  (or (string? value)
      (instance? java.io.File value)
      (instance? java.nio.file.Path value)))

(defn- path-inputs
  [paths]
  (let [paths (if (path-value? paths) [paths] (seq paths))]
    (when-not (seq paths)
      (throw (ex-info "At least one path is required." {:paths paths})))
    (vec paths)))

(defn- path-string
  [path]
  (.getPath (io/file path)))

(defn- validate-paths!
  [paths]
  (let [paths (path-inputs paths)
        files (mapv io/file paths)
        missing (remove #(.exists ^java.io.File %) files)]
    (when (seq missing)
      (throw
       (ex-info
        (str "Path does not exist: " (path-string (first missing)))
        {:paths (mapv path-string missing)})))
    {:inputs (mapv path-string paths)
     :files files}))

(defn- concise-message
  [throwable]
  (if throwable
    (let [message (some-> throwable .getMessage str/trim)]
      (if (seq message)
        message
        (.getName (class throwable))))
    "Unknown error"))

(defn- error-report
  [path throwable]
  {:path path :message (concise-message throwable)})

(defn- kondo-errors
  [findings]
  (->> findings
       (filter #(= :error (:level %)))
       (map #(select-keys % [:filename :row :col :message]))
       vec))

(defn- processed-report
  [report]
  (let [errors (kondo-errors (:findings report))]
    (assoc report
           :status (if (seq errors) :errors :processed)
           :errors errors)))

(defn- error-status
  [report errors]
  (assoc report :status :errors :errors (vec errors)))

(defn lint-files
  "Lint one or more files or directories with clj-kondo.

  Directories are processed recursively using clj-kondo's normal defaults.
  Relative and absolute paths are accepted. The return value contains the
  normal clj-kondo result data plus `:status` (`:processed` or `:errors`) and
  a concise `:errors` collection. Invalid paths throw before linting starts."
  [paths]
  (let [{:keys [inputs]} (validate-paths! paths)]
    (try
      (processed-report (kondo/run! {:lint inputs}))
      (catch Throwable throwable
        (error-status
         {:findings []
          :summary {}
          :config nil}
         (map #(error-report % throwable) inputs))))))

(defn- format-report
  [event context data]
  (case event
    :fix/initial (assoc context :tool-results [])
    :fix/file (update context :tool-results conj data)
    :fix/summary context
    context))

(defn- report-file-path
  [file]
  (path-string file))

(defn- format-result
  [{:keys [tool-results]}]
  (reduce
   (fn [report result]
     (let [path (report-file-path (:file result))]
       (cond
         (:exception result)
         (update report :errors conj (error-report path (:exception result)))

         (:reformatted result)
         (update report :formatted conj path)

         :else
         (update report :unchanged conj path))))
   {:formatted [] :unchanged [] :errors []}
   tool-results))

(defn format-files
  "Format one or more files or directories in place with cljfmt.

  Directories are processed recursively using cljfmt's normal file pattern and
  configuration discovery. The result contains `:formatted`, `:unchanged`,
  and concise per-file `:errors`, together with `:status` (`:processed` or
  `:errors`). Invalid paths throw before any file is changed."
  [paths]
  (let [{:keys [inputs]} (validate-paths! paths)]
    (try
      (let [report (fmt-tool/fix {:paths inputs :report format-report})
            result (format-result report)]
        (if (seq (:errors result))
          (assoc result :status :errors)
          (assoc result :status :processed)))
      (catch Throwable throwable
        (error-status
         {:formatted [] :unchanged []}
         (map #(error-report % throwable) inputs))))))

(defn format-string
  "Format source text with cljfmt and return the resulting string.

  The current working directory's cljfmt configuration is used when
  available; cljfmt defaults are used when no project configuration exists."
  [source]
  (fmt/reformat-string source (fmt-config/load-config)))

