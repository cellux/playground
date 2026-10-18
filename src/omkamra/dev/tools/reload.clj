(ns omkamra.dev.tools.reload
  "In-process namespace reloading designed for agent workflows.

  This is a third reload facility beside `clojure.core/require :reload` and
  `io.github.tonsky/clj-reload`, built to keep what each does well and drop what
  each does badly. It is deliberately **self-contained**: the state lives in
  in-memory atoms, there is no baseline file, and it does not depend on
  clj-reload.

  ## What it keeps

  From `require :reload`:

  * **Determinism without ceremony.** `(reload 'my.ns)` always reloads `my.ns`
    and whatever depends on it. It never consults time or a baseline, so it can
    never silently decide there is nothing to do.
  * **In-memory, non-destructive.** No init file, no global config; reload state
    comes and goes with the JVM.

  From clj-reload:

  * **Change detection.** `(reload)` finds what changed by **content**, not by
    modification time, and reloads it.
  * **Dependency awareness.** A changed namespace pulls in its transitive
    dependents, topologically ordered, so macros and inlined definitions are not
    left stale.
  * **Unload before load.** Reloading clears removed vars and gives `deftype` /
    `defrecord` fresh classes, while `defonce` values are preserved.

  ## What it drops

  * clj-reload's `:since`/mtime window, which silently misses an edit that
    predates `init`. This facility compares file **content digests**, and the
    first scan after a JVM restart additionally treats any file newer than the
    JVM start time as changed, which closes the edit-before-first-reload window.
  * The silent no-op. A reload with nothing to do reports `:status :unchanged`
    together with the number of files scanned and a note, so a miss is visible.
  * Stop-at-first-failure. Every namespace is attempted; **all** failures are
    reported, each with a concise message and, when available, a location.
  * The `init` that must precede the first edit. `reload` establishes its own
    baseline; a scope map is only needed to scan somewhere other than the
    classpath source directories.

  ## Reports

  Every result is compact and serializable: namespace symbols, path strings,
  keywords, numbers and plain maps. No `java.io.File` objects, no `Throwable`s.
  The reload result carries `:status` (`:reloaded`, `:unchanged`, or `:errors`),
  `:changed` (the roots: content changes or explicit targets), `:reloaded` and
  `:unloaded` (topological order), `:kept` (namespaces whose `defonce` state was
  preserved), `:failed`, `:deferred` (the tool's own namespaces, never unloaded
  in process), and `:scanned`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File PushbackReader]
           [java.lang.management ManagementFactory]
           [java.security MessageDigest]
           [java.util.concurrent.locks ReentrantLock]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(def ^:private ^ReentrantLock lock (ReentrantLock.))

(defmacro ^:private with-lock
  [& body]
  `(try
     (.lock lock)
     ~@body
     (finally (.unlock lock))))

(def ^:private jvm-start-ms
  (.getStartTime (ManagementFactory/getRuntimeMXBean)))

(defonce ^:private state
  (atom {:config nil
         ;; {absolute-path {:digest "<hex>" :parsed {…}}}
         :known {}
         :initialized? false}))

(defonce ^:private alias-registry (atom {}))

(def ^:private default-file-pattern #".*\.cljc?$")

(def ^:private protected-namespaces
  #{"omkamra.dev.tools" "omkamra.dev.tools.reload"})

;; ---------------------------------------------------------------------------
;; Paths
;; ---------------------------------------------------------------------------

(defn- path-value?
  [value]
  (or (string? value) (instance? File value) (instance? java.nio.file.Path value)))

(defn- canonical
  [path]
  (.getCanonicalPath (io/file path)))

(defn- classpath-dirs
  "The directories currently on the JVM classpath, canonicalized."
  []
  (->> (str/split (System/getProperty "java.class.path")
                  (re-pattern (java.util.regex.Pattern/quote File/pathSeparator)))
       (remove str/blank?)
       (filter #(.isDirectory (io/file ^String %)))
       (map canonical)
       (distinct)
       (sort)
       (vec)))

(defn- scan-files
  "Every source file under the configured directories, canonical and sorted."
  [{:keys [dirs pattern]}]
  (->> dirs
       (mapcat #(file-seq (io/file ^String %)))
       (filter #(.isFile ^File %))
       (filter #(re-matches pattern (.getName ^File %)))
       (map #(.getCanonicalPath ^File %))
       (distinct)
       (sort)
       (vec)))

(defn- digest
  [file]
  (let [message-digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream (io/file ^String file))]
      (let [buffer (byte-array 8192)]
        (loop []
          (let [read (.read in buffer)]
            (when (pos? read)
              (.update message-digest buffer 0 (int read))
              (recur))))))
    (format "%x" (BigInteger. 1 (.digest message-digest)))))

(defn- initial-digest
  "A file's digest, or `::changed` when it was modified after the JVM started.

  The sentinel closes the one gap a content baseline cannot see: an edit made
  after the JVM started but before the first reload. Once a real baseline exists
  it is never used again."
  [file]
  (if (> (.lastModified (io/file ^String file)) jvm-start-ms)
    ::changed
    (digest file)))

;; ---------------------------------------------------------------------------
;; Reading a file's shape without evaluating it
;; ---------------------------------------------------------------------------

(defn- read-forms
  "The top-level forms of `file`, tolerating a read error partway through."
  [file]
  (let [forms (volatile! [])]
    (try
      (with-open [reader (PushbackReader. (io/reader (io/file ^String file)
                                                     :encoding "UTF-8"))]
        (binding [*read-eval* false
                  *default-data-reader-fn* (fn [_ _] nil)]
          (loop []
            (let [form (read {:eof ::eof :read-cond :allow :features #{:clj}} reader)]
              (when-not (identical? ::eof form)
                (vswap! forms conj form)
                (recur))))))
      {:forms @forms}
      (catch Throwable throwable
        {:forms @forms :error (ex-message throwable)}))))

(defn- quoted-symbol
  [form]
  (cond
    (symbol? form) form
    (and (seq? form) (= 'quote (first form)) (symbol? (second form))) (second form)
    :else nil))

(defn- form-namespace
  "The namespace a top-level form declares, if it declares one."
  [form]
  (when (seq? form)
    (case (first form)
      ns (when (symbol? (second form)) (second form))
      in-ns (quoted-symbol (second form))
      nil)))

(defn- prefix-dependencies
  "The namespaces named by one `:require`/`:use` spec."
  [spec]
  (if (vector? spec)
    (let [[head & more] spec]
      (if (symbol? head)
        (cons head
              (keep (fn [entry]
                      (when (and (vector? entry) (symbol? (first entry)))
                        (symbol (str head "." (first entry)))))
                    (take-while (complement keyword?) more)))
        []))
    (if (symbol? spec) [spec] [])))

(defn- ns-requires
  [form]
  (into #{}
        (mapcat (fn [clause]
                  (when (and (seq? clause) (contains? #{'require 'use} (first clause)))
                    (mapcat prefix-dependencies (rest clause)))))
        (drop 2 form)))

(defn- form-keep
  "The name of a var whose value must survive a reload."
  [form]
  (when (and (seq? form) (symbol? (second form)))
    (case (first form)
      defonce (second form)
      def (when (:reload-preserve (meta (second form))) (second form))
      nil)))

(defn- parse-file
  "The reload-relevant shape of a file, cached in `state` by digest.

  * `:namespaces` — the namespaces the file declares;
  * `:requires` — the namespaces its `ns` forms require;
  * `:keeps` — the vars to preserve across a reload (`defonce`, or `def` with
    `^:reload-preserve`);
  * `:readable?` and `:error` — whether the whole file could be read."
  [file]
  (let [{:keys [forms error]} (read-forms file)
        ns-forms (filter #(and (seq? %) (= 'ns (first %))) forms)]
    {:readable? (nil? error)
     :error error
     :namespaces (set (keep form-namespace forms))
     :requires (into #{} (mapcat ns-requires) ns-forms)
     :keeps (into #{} (keep form-keep) forms)}))

;; ---------------------------------------------------------------------------
;; The live image
;; ---------------------------------------------------------------------------

(defn- live-file-map
  "Map of canonical source file to the loaded namespaces it holds."
  [{:keys [dirs]}]
  (let [resolved (atom {})
        resolve* (fn [relative]
                   (if (contains? @resolved relative)
                     (get @resolved relative)
                     (let [file (io/file ^String relative)
                           path (if (.isAbsolute file)
                                  (when (.isFile file) (.getCanonicalPath file))
                                  (some (fn [dir]
                                          (let [candidate (io/file dir ^String relative)]
                                            (when (.isFile candidate)
                                              (.getCanonicalPath candidate))))
                                        dirs))]
                       (swap! resolved assoc relative path)
                       path)))]
    (reduce (fn [result namespace]
              (let [relative (into #{} (keep #(some-> % meta :file))
                                   (vals (ns-interns namespace)))]
                (reduce (fn [result relative]
                          (if-let [path (resolve* relative)]
                            (update result path (fnil conj #{}) (ns-name namespace))
                            result))
                        result
                        relative)))
            {}
            (all-ns))))

(defn- invert
  [file->namespaces]
  (reduce (fn [result [file namespaces]]
            (reduce #(update %1 %2 (fnil conj #{}) file) result namespaces))
          {}
          file->namespaces))

(defn- live-dependencies
  [namespace]
  (when-let [object (find-ns namespace)]
    (into #{}
          (concat (map ns-name (vals (ns-aliases object)))
                  (keep #(some-> % meta :ns ns-name) (vals (ns-refers object)))))))

(defn- dependency-set
  "The namespaces `namespace` depends on, from the live image and from source."
  [namespace ns->files parsed]
  (into (or (live-dependencies namespace) #{})
        (mapcat #(get-in parsed [% :requires] #{}))
        (get ns->files namespace)))

(defn- topological-sort
  "Order `namespaces` so that dependencies precede dependents."
  [namespaces dependencies-of]
  (let [members (set namespaces)
        visited (atom #{})
        order (volatile! [])]
    (letfn [(visit [namespace]
              (when-not (contains? @visited namespace)
                (swap! visited conj namespace)
                (doseq [dependency (sort (dependencies-of namespace))]
                  (when (contains? members dependency)
                    (visit dependency)))
                (vswap! order conj namespace)))]
      (doseq [namespace (sort members)]
        (visit namespace))
      @order)))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(defn- single-line
  [text limit]
  (let [text (-> (str text) (str/replace #"\s+" " ") (str/trim))]
    (if (> (count text) limit) (str (subs text 0 limit) "…") text)))

(defn- root-cause
  [throwable]
  (if-let [cause (ex-cause throwable)] (recur cause) throwable))

(defn- failure
  [namespace throwable]
  (let [location (or (not-empty (select-keys (ex-data throwable) [:file :line :column]))
                     (when-let [match (re-find #"\(([^()]+):(\d+)(?::(\d+))?\)"
                                               (str (ex-message throwable)))]
                       (cond-> {:file (second match)
                                :line (Long/parseLong (nth match 2))}
                         (nth match 3) (assoc :column (Long/parseLong (nth match 3))))))]
    (cond-> {:namespace namespace
             :message (single-line (or (ex-message (root-cause throwable))
                                       (ex-message throwable)
                                       (str (class throwable)))
                                   300)}
      (:file location) (assoc :file (:file location))
      (:line location) (assoc :line (:line location))
      (:column location) (assoc :column (:column location)))))

;; ---------------------------------------------------------------------------
;; Unloading and loading
;; ---------------------------------------------------------------------------

(defn- unload!
  [namespace]
  (when (find-ns namespace)
    (dosync (alter @#'clojure.core/*loaded-libs* disj namespace))
    (remove-ns namespace)
    true))

(defn- interned
  [namespace name]
  (when-let [object (find-ns namespace)]
    (get (ns-interns object) name)))

(defn- capture-keeps
  [namespace keep-names]
  (into {}
        (keep (fn [name]
                (when-let [var (interned namespace name)]
                  [name @var])))
        keep-names))

(defn- restore-keeps!
  [namespace kept]
  (doseq [[name value] kept]
    (when-let [var (interned namespace name)]
      (alter-var-root var (constantly value)))))

(defn- load-all!
  [files]
  (doseq [file files]
    (load-file file)))

(defn- mark-loaded!
  "Register `namespace` as loaded, as `require` would, so a dependent's
  `:require` of it is a no-op instead of reading it from the classpath again."
  [namespace]
  (dosync (alter @#'clojure.core/*loaded-libs* conj namespace)))

;; ---------------------------------------------------------------------------
;; Managed (persistent) aliases
;; ---------------------------------------------------------------------------

(defn- bind-alias!
  [owner alias target]
  (when-let [object (find-ns owner)]
    (binding [*ns* object]
      (when (contains? (ns-aliases object) alias)
        (ns-unalias object alias))
      (clojure.core/alias alias target))))

(defn- failed-namespace!
  [target message]
  (let [name (symbol (str "omkamra.dev.tools.failed." (Math/abs (hash target))))
        object (or (find-ns name) (create-ns name))]
    (alter-meta! object assoc :omkamra.dev.tools/failure
                 {:target target :message message})
    (intern object 'unavailable
            (fn [& _]
              (throw (ex-info (str "Persistent alias target " target
                                   " is unavailable: " message)
                              {:target target}))))
    name))

(defn- repair-aliases!
  [{:keys [reloaded unloaded failed]}]
  (let [reloaded (set reloaded)
        unloaded (set unloaded)
        changed (into reloaded unloaded)
        failures (set (map :namespace failed))]
    (doseq [[[owner alias] target] @alias-registry
            :when (not (contains? changed owner))]
      (cond
        (contains? reloaded target)
        (when (find-ns target)
          (bind-alias! owner alias target))

        (or (contains? failures target)
            (and (contains? unloaded target) (not (find-ns target))))
        (when (find-ns owner)
          (bind-alias!
           owner alias
           (failed-namespace! target
                              (or (some #(when (= target (:namespace %)) (:message %))
                                        failed)
                                  "namespace did not load"))))))))

;; ---------------------------------------------------------------------------
;; Planning
;; ---------------------------------------------------------------------------

(defn- normalize-config
  [directories]
  (cond
    (nil? directories)
    {:dirs (classpath-dirs) :pattern default-file-pattern}

    (path-value? directories)
    {:dirs [(canonical directories)] :pattern default-file-pattern}

    (map? directories)
    (let [dirs (or (:dirs directories) (classpath-dirs))
          dirs (if (path-value? dirs) [dirs] dirs)]
      {:dirs (mapv canonical dirs)
       :pattern (or (:files directories) default-file-pattern)})

    (sequential? directories)
    {:dirs (mapv canonical directories) :pattern default-file-pattern}

    :else
    (throw (ex-info "reload directories must be paths, a collection, or options"
                    {:directories directories}))))

(defn- validate-config!
  [{:keys [dirs]}]
  (let [missing (remove #(.isDirectory (io/file ^String %)) dirs)]
    (when (seq missing)
      (throw (ex-info (str "reload directories must exist: " (first missing))
                      {:dirs (vec missing)})))
    dirs))

(defn- build-known
  "Parse and fingerprint every scanned file. Called once at init."
  [config]
  (into {}
        (map (fn [file]
               [file {:digest (initial-digest file) :parsed (parse-file file)}]))
        (scan-files config)))

(defn- scan
  [config known]
  (let [files (scan-files config)
        digests (into {} (map (fn [file] [file (digest file)])) files)]
    {:files files
     :digests digests
     :changed (set (filter #(not= (get-in known [% :digest]) (get digests %))
                           files))
     :missing (set (remove (set files) (keys known)))}))

(defn- parsed-for
  "Each scanned file's parsed shape, reusing the cache for unchanged files and
  re-reading changed or new ones."
  [known scan]
  (into {}
        (map (fn [file]
               (let [cached (get-in known [file :parsed])
                     changed? (contains? (:changed scan) file)]
                 [file (if (and cached (not changed?)) cached (parse-file file))])))
        (:files scan)))

(defn- file->namespaces
  [scan parsed live]
  (into {}
        (map (fn [file]
               [file (into (or (get live file) #{})
                           (get-in parsed [file :namespaces]))]))
        (:files scan)))

(defn- roots
  "The namespaces that must be (re)loaded: content changes (which includes brand
  new files), files that are gone (to unload), or explicitly named namespaces.

  A namespace whose unchanged file simply is not loaded yet is deliberately not
  a root: on a fresh JVM that would try to load the whole tree. Loading happens
  by editing, by an explicit target, or with `:all`."
  [scan file->ns known explicit]
  (cond
    (= :all explicit) (set (mapcat val file->ns))
    (seq explicit) (set explicit)
    :else
    (into (set (mapcat file->ns (:changed scan)))
          (mapcat #(get-in known [% :parsed :namespaces])
                  (:missing scan)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn- configure!
  "Establish or replace the reload baseline and scan scope.

  With no argument, every classpath source directory is scanned; with a path, a
  collection of paths, or an options map (`:dirs`, `:files` for a filename
  pattern), scanning is scoped accordingly. `reload` calls this itself on first
  use, so a caller only needs it to scope the scan deliberately."
  [directories]
  (let [config (normalize-config directories)]
    (validate-config! config)
    (let [known (build-known config)]
      (swap! state assoc :config config :known known :initialized? true)
      config)))

(defn- ensure-initialized!
  []
  (when-not (:initialized? @state)
    (configure! nil)))

(defn- run-reload!
  [explicit]
  (ensure-initialized!)
  (let [{:keys [config known]} @state
        scan (scan config known)
        parsed (parsed-for known scan)
        live (live-file-map config)
        file->ns (file->namespaces scan parsed live)
        ns->files (invert file->ns)
        root-set (roots scan file->ns known explicit)
        members (set (keys ns->files))
        dependencies-of (fn [namespace]
                          (dependency-set namespace ns->files parsed))
        ;; Reload a changed namespace *and everything that depends on it*, so
        ;; macros and inlined definitions in dependents are refreshed too.
        dependents (reduce (fn [result namespace]
                             (reduce (fn [result dependency]
                                       (update result dependency (fnil conj #{}) namespace))
                                     result
                                     (dependencies-of namespace)))
                           {}
                           members)
        closure (loop [result (set root-set)]
                  (let [next (into result
                                   (comp (mapcat #(get dependents % #{}))
                                         (remove result))
                                   result)]
                    (if (= next result) result (recur next))))
        order (topological-sort closure dependencies-of)
        outcome (reduce
                 (fn [result namespace]
                   (cond
                     (contains? protected-namespaces (str namespace))
                     (update result :deferred conj namespace)

                     (empty? (get ns->files namespace))
                     (if (unload! namespace)
                       (update result :unloaded conj namespace)
                       (update result :failed conj
                               {:namespace namespace
                                :message "no source file found for this namespace"}))

                     :else
                     (let [files (get ns->files namespace)
                           keep-names (into #{} (mapcat #(get-in parsed [% :keeps] #{}))
                                            files)
                           kept (capture-keeps namespace keep-names)]
                       (unload! namespace)
                       (let [throwable (try
                                         (load-all! files)
                                         (mark-loaded! namespace)
                                         (restore-keeps! namespace kept)
                                         nil
                                         (catch Throwable throwable
                                           (unload! namespace)
                                           throwable))]
                         (if throwable
                           (-> result
                               (update :unloaded conj namespace)
                               (update :failed conj (failure namespace throwable)))
                           (-> result
                               (update :reloaded conj namespace)
                               (update :unloaded conj namespace)
                               (update :kept assoc namespace (set (keys kept)))))))))
                 {:reloaded [] :unloaded [] :failed [] :deferred [] :kept {}}
                 order)
        failed (:failed outcome)
        failed-names (set (map :namespace failed))
        new-known (reduce
                   (fn [known file]
                     (let [namespaces (get file->ns file)]
                       (if (some failed-names namespaces)
                         known
                         (assoc known file {:digest (get-in scan [:digests file])
                                            :parsed (get parsed file)}))))
                   (apply dissoc known (:missing scan))
                   (:files scan))
        kept (vec (sort (keep (fn [[namespace names]]
                                (when (seq names) namespace))
                              (:kept outcome))))
        status (cond
                 (seq failed) :errors
                 (or (seq (:reloaded outcome)) (seq (:unloaded outcome))) :reloaded
                 :else :unchanged)]
    (swap! state assoc :known new-known)
    (repair-aliases! outcome)
    (cond-> {:status status
             :changed (vec (sort root-set))
             :reloaded (vec (:reloaded outcome))
             :unloaded (vec (:unloaded outcome))
             :kept kept
             :failed (vec failed)
             :deferred (vec (sort (:deferred outcome)))
             :scanned (count (:files scan))}
      (= :unchanged status)
      (assoc :note (str "no file content changed since the last reload; "
                        "pass a namespace to reload it explicitly")))))

(defn reload
  "Reload a namespace set and report compactly.

  `(reload)` reloads what changed since the baseline: file **content** changes
  are detected by digest (never by modification time), and each changed
  namespace pulls in its transitive dependents, topologically ordered. The
  namespaces are unloaded first, so removed vars disappear and `deftype` /
  `defrecord` get fresh classes, while `defonce` values are preserved.

  `(reload 'my.ns)`, `(reload '[a b])`, and `(reload :all)` ignore the baseline
  and reload exactly the named namespaces (and their dependents), which is the
  deterministic escape hatch when detection cannot know about an edit.

  `(reload {:dirs [...]})` sets the scan scope first (the classpath source
  directories by default; an optional `:files` pattern selects filenames) and
  then reloads what changed under it; the scope persists for later calls. There
  is no separate initialization step.

  Every failure is reported, not just the first."
  ([] (with-lock (run-reload! nil)))
  ([target-or-options]
   (with-lock
     (if (map? target-or-options)
       (do (configure! target-or-options)
           (run-reload! nil))
       (let [explicit (cond
                        (= :all target-or-options) :all
                        (symbol? target-or-options) [target-or-options]
                        (sequential? target-or-options) (vec target-or-options)
                        :else (throw (ex-info "reload expects a namespace symbol, a collection, :all, or a scope map"
                                              {:target target-or-options})))]
         (run-reload! explicit))))))

(defn create-persistent-alias!
  "Create and register a namespace alias that stays usable across reloads.

  Pass `{:in-ns 'user :alias 'tools :target 'omkamra.dev.tools}`. Both
  namespaces must exist; identical mappings are idempotent and conflicts throw.
  After a reload, an alias whose target was reloaded is rebound to the current
  namespace; one whose target failed becomes a diagnostic stub whose known uses
  fail with a concise message. If the owner namespace reloads, the alias is not
  silently recreated."
  [{:keys [in-ns alias target] :as options}]
  (when-not (and (symbol? in-ns) (symbol? alias) (symbol? target))
    (throw (ex-info "create-persistent-alias! requires symbol values for :in-ns, :alias, and :target"
                    {:options options})))
  (when-not (find-ns in-ns)
    (throw (ex-info (str "Alias owner namespace does not exist: " in-ns)
                    {:namespace in-ns})))
  (when-not (find-ns target)
    (throw (ex-info (str "Alias target namespace does not exist: " target)
                    {:namespace target})))
  (let [key [in-ns alias]
        registered (get @alias-registry key)
        existing (get (ns-aliases (find-ns in-ns)) alias)]
    (when (and registered (not= registered target))
      (throw (ex-info (str "Persistent alias already targets " registered)
                      {:in-ns in-ns :alias alias :target registered})))
    (when (and existing (not= (ns-name existing) target))
      (throw (ex-info (str "Alias already targets " (ns-name existing))
                      {:in-ns in-ns :alias alias :target (ns-name existing)})))
    (bind-alias! in-ns alias target)
    (swap! alias-registry assoc key target)
    {:in-ns in-ns :alias alias :target target}))
