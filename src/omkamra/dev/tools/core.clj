(ns omkamra.dev.tools.core
  "Implementation for the discoverable development-tool facade.

  The public entrypoint is `omkamra.dev.tools`; this namespace owns path
  handling, tool adapters, explicit clj-reload lifecycle, and managed aliases."
  (:require [clj-kondo.core :as kondo]
            [clj-reload.core :as reload]
            [cljfmt.config :as fmt-config]
            [cljfmt.core :as fmt]
            [cljfmt.tool :as fmt-tool]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defonce ^:private initialized? (atom false))
(defonce ^:private managed-aliases (atom {}))

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

(defn- init-options
  [directories]
  (if (nil? directories)
    {:output :quiet}
    (let [{:keys [files]} (validate-paths! directories)
          non-directories (remove #(.isDirectory ^java.io.File %) files)]
      (when (seq non-directories)
        (throw
         (ex-info
          (str "clj-reload init requires directories: "
               (path-string (first non-directories)))
          {:paths (mapv path-string non-directories)})))
      {:dirs (mapv path-string files)
       :output :quiet})))

(defn init-reload
  "Initialize clj-reload and return a compact namespace-count report.

  With no argument, clj-reload's classpath-directory defaults are used. With
  one directory or a collection of directories, scanning is scoped to those
  directories. Calling this again reinitializes clj-reload without clearing
  aliases registered through `create-alias!`."
  ([]
   (init-reload nil))
  ([directories]
   (reload/init (init-options directories))
   (reset! initialized? true)
   {:status :initialized
    :namespace-count (count (reload/find-namespaces))}))

(defn- reload-error
  [result]
  (when-let [failed (:failed result)]
    {:namespace failed
     :message (concise-message (:exception result))}))

(defn- failed-namespace-symbol
  [target]
  (symbol (str "omkamra.dev.tools.failed." (Math/abs (hash target)))))

(defn- failed-namespace!
  [target message]
  (let [namespace-symbol (failed-namespace-symbol target)
        namespace (or (find-ns namespace-symbol)
                      (create-ns namespace-symbol))
        error (fn [& _]
                (throw
                 (ex-info
                  (str "Managed alias target " target " is unavailable: " message)
                  {:target target})))]
    (alter-meta! namespace assoc :omkamra.dev.tools/failure
                 {:target target :message message})
    (intern namespace 'unavailable error)
    namespace-symbol))

(defn- bind-alias!
  [owner alias-name target]
  (when-let [namespace (find-ns owner)]
    (binding [*ns* namespace]
      (when (contains? (ns-aliases namespace) alias-name)
        (ns-unalias namespace alias-name))
      (clojure.core/alias alias-name target))))

(defn- repair-aliases!
  [{:keys [reloaded unloaded failed]}]
  (let [reloaded (set reloaded)
        unloaded (set unloaded)
        changed (into reloaded unloaded)
        failed-records (vec failed)
        failed (set (keep :namespace failed-records))]
    (doseq [[[owner alias] target] @managed-aliases
            :when (not (contains? changed owner))]
      (cond
        (contains? reloaded target)
        (when (find-ns target)
          (bind-alias! owner alias target))

        (or (contains? failed target)
            (and (contains? unloaded target)
                 (not (find-ns target))))
        (when (find-ns owner)
          (bind-alias! owner alias
                       (failed-namespace! target
                                          (or (some #(when (= target (:namespace %))
                                                       (:message %)) failed-records)
                                              "namespace did not load"))))))))

(defn reload
  "Reload changed namespaces and return a terse normalized report.

  `init-reload` must be called first. The result contains `:reloaded` and
  `:unloaded` namespace symbols. A no-op has `:status :unchanged`; a failed
  load has `:status :errors` and concise `:failed` records. Managed aliases
  are refreshed after the reload."
  []
  (when-not @initialized?
    (throw
     (IllegalStateException.
      "clj-reload is not initialized; call omkamra.dev.tools/init-reload first")))
  (let [result (try
                 (reload/reload {:only :changed
                                 :throw false
                                 :log-fn (fn [& _] nil)})
                 (catch Throwable throwable
                   {:unloaded []
                    :loaded []
                    :failed nil
                    :exception throwable}))
        failed (cond-> []
                 (:failed result) (conj (reload-error result))
                 (and (:exception result) (not (:failed result)))
                 (conj {:namespace nil
                        :message (concise-message (:exception result))}))
        report {:reloaded (vec (:loaded result))
                :unloaded (vec (:unloaded result))}
        report (if (seq failed) (assoc report :failed failed) report)]
    (repair-aliases! (assoc report :failed failed))
    (cond
      (seq failed) (assoc report :status :errors)
      (or (seq (:reloaded report)) (seq (:unloaded report)))
      (assoc report :status :reloaded)
      :else (assoc report :status :unchanged))))

(defn create-alias!
  "Create and register a reload-aware namespace alias.

  Accepts a map with `:in-ns`, `:alias`, and `:target` symbols. Both the owner
  and target namespaces must already exist. Identical mappings are idempotent;
  conflicting existing mappings throw. The registry survives reload and `init-reload`,
  but an alias owner that reloads loses its alias until its target reloads."
  [{:keys [in-ns alias target] :as options}]
  (when-not (and (symbol? in-ns) (symbol? alias) (symbol? target))
    (throw
     (ex-info
      "create-alias! requires symbol values for :in-ns, :alias, and :target"
      {:options options})))
  (when-not (find-ns in-ns)
    (throw (ex-info (str "Alias owner namespace does not exist: " in-ns)
                    {:namespace in-ns})))
  (when-not (find-ns target)
    (throw (ex-info (str "Alias target namespace does not exist: " target)
                    {:namespace target})))
  (let [key [in-ns alias]
        registered (get @managed-aliases key)
        existing (get (ns-aliases (find-ns in-ns)) alias)]
    (when (and registered (not= registered target))
      (throw (ex-info (str "Managed alias already targets " registered)
                      {:in-ns in-ns :alias alias :target registered})))
    (when (and existing (not= (ns-name existing) target))
      (throw (ex-info (str "Alias already targets " (ns-name existing))
                      {:in-ns in-ns :alias alias :target (ns-name existing)})))
    (bind-alias! in-ns alias target)
    (swap! managed-aliases assoc key target)
    {:in-ns in-ns :alias alias :target target}))
