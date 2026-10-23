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

(defonce ^:private session-namespaces
  ;; Namespaces this session has loaded at least once, whether or not they are
  ;; loaded now. A reload may only run code that has already run here: loading is
  ;; what makes it dangerous, and a namespace that ran once has already done
  ;; whatever it does. Keeping the names *past* an unload is also what lets a
  ;; reload recover a namespace that a failed reload took out of the image.
  (atom #{}))

(def ^:private default-file-pattern #".*\.cljc?$")

(def ^:private protected-namespaces
  #{"omkamra.dev.tools" "omkamra.dev.tools.reload"})

(def ^:private wholesale-reload-floor
  "The size at which a detection-driven reload is held to a share of the scope,
  and the number of namespaces rebuilding that is already too many.

  Calibrated against ordinary work rather than picked round: reloading everything
  that depends on `omkamra.dsp` in this repository touches about forty
  namespaces, so a scope below this floor never earns a refusal."
  50)

(def ^:private wholesale-reload-share
  "The share of a scope a detection-driven reload may rebuild and still be taken
  for an edit. Half: the guard speaks when a reload would touch more of the tree
  than it leaves."
  (/ 1 2))

(def ^:private not-loaded-sample-size
  "How many skipped namespaces a report names when there are more of them: enough
  to recognise what was left alone, not a list to read."
  5)

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

(def ^:private require-clause-heads
  "The `ns` clause heads that name another namespace.

  An `ns` form spells them as **keywords** — `(:require [a.b :as b])` — so a
  set of bare symbols silently matched nothing: every file's `:requires` came
  back empty, and the reload graph fell back to the live image alone. That
  masked itself in the common interactive case (a loaded namespace's aliases are
  also a live dependency) but lost exactly the case that matters: a namespace
  that is *not* loaded has no live image to ask, so it could never be pulled
  back in. The symbol spellings are accepted too, since a `(require ...)` clause
  written by hand is not a syntax error."
  #{:require :use 'require 'use})

(defn- ns-requires
  [form]
  (into #{}
        (mapcat (fn [clause]
                  (when (and (seq? clause)
                             (contains? require-clause-heads (first clause)))
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
;; Aliases left behind by unloading
;; ---------------------------------------------------------------------------

(defn- stale-alias-entries
  "Every alias, in every loaded namespace, that still points at an unloaded
  namespace object, as `[owner alias target]` names.

  `require` stores the namespace object it found. Unloading a namespace removes
  it from the registry but cannot reach into the namespaces that alias it, and
  those are usually *outside* the reload scope — `user` above all — so nothing
  else repairs them. A namespace inside the scope is rebuilt from its own `ns`
  form and repairs itself, so only genuinely stale entries appear here."
  []
  (into []
        (for [owner (all-ns)
              [alias target] (ns-aliases owner)
              :let [live (find-ns (ns-name target))]
              :when (and live (not (identical? target live)))]
          [(ns-name owner) alias (ns-name live)])))

(defn- refresh-stale-aliases!
  "Re-point stale aliases at the live namespace of the same name.

  A stale alias is not merely untidy: it resolves through the dead object to the
  namespace's *old* vars, so an alias in a REPL namespace would keep running
  code the reload replaced. Returns the repairs, for the caller to report;
  `bind-alias!` returns nothing, so the repair itself is the record."
  []
  (into []
        (keep (fn [[owner alias target]]
                (when (find-ns owner)
                  (bind-alias! owner alias target)
                  {:in-ns owner :alias alias :target target})))
        (stale-alias-entries)))

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

(defn- remember-loaded!
  "Record every namespace loaded so far and return the whole remembered set."
  []
  (swap! session-namespaces into (map ns-name (all-ns))))

(defn- select-roots
  "The namespaces a reload will (re)load, and the ones it was pointed at but will
  not, because this session has never loaded them.

  A reload refreshes the image; it does not grow it, and it runs no code that has
  not already run here. Loading is `require`'s job — deliberate, by name — and a
  file digest is not a reason to run a namespace for the first time: one scanned
  namespace in this repository ends in `(q/defsketch …)`, which launches a sketch
  as it loads and takes a headless JVM with it.

  Everything else is a root: a changed or brand new file, a file that is gone (to
  unload), or a namespace named explicitly — each only if the session has loaded
  it. That includes a namespace a *failed* reload left unloaded: it has run here,
  so a reload may bring it back."
  [scan file->ns known explicit remembered]
  (let [known? #(contains? remembered %)
        selected (cond
                   (seq explicit) (set explicit)
                   :else
                   (into (set (mapcat file->ns (:changed scan)))
                         (mapcat #(get-in known [% :parsed :namespaces])
                                 (:missing scan))))]
    {:root-set (into #{} (filter known?) selected)
     :not-loaded (into (sorted-set) (remove known?) selected)}))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn- wholesale-reload?
  "True when a detection-driven reload would rebuild the entire session.

  Detection is content-based, so a baseline that no longer describes the tree — a
  check-out, or anything that corrupted it — makes *every* file look changed, and
  acting on that verdict unloads and rebuilds every namespace in scope that the
  session has run. That is never an incremental edit: it is a reload nobody asked
  for, of everything the session is made of, through an unload-then-load sequence
  that a single failure leaves half-done. The remedy is a fresh process rather
  than a wider reload, which is why there is no option to ask for one: a baseline
  that no longer describes the tree cannot be trusted to describe the reload
  either.

  The threshold is a share of the session rather than all of it, so a check-out
  that rewrites most of the tree is caught too, and it is floored because
  refreshing all of a small session is ordinary — a widely used namespace in a
  small project really does reach every dependent."
  [session closure]
  (let [total (count session)]
    (and (>= total wholesale-reload-floor)
         (>= (count closure) (max wholesale-reload-floor
                                  (* wholesale-reload-share total))))))

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
        remembered (remember-loaded!)
        {:keys [root-set not-loaded]} (select-roots scan file->ns known explicit
                                                    remembered)
        ;; What detection alone would reload, whatever drove this call.
        ;;
        ;; A reload refreshes the image, and the one thing an explicitly *named*
        ;; target leaves unsaid is that it refreshed only what it named — which is
        ;; the point of the form (it is how you reload one namespace while another
        ;; file is mid-edit, and why it must not quietly adopt the rest). So the
        ;; report says what it left behind: "the image is current now" should
        ;; never be something the caller assumed. Computed on every call rather
        ;; than only the named ones, so the claim is checked instead of assumed;
        ;; for a detection-driven call the set below is empty by construction.
        detected-roots (:root-set (select-roots scan file->ns known nil remembered))
        stale (vec (sort (remove (set root-set) detected-roots)))
        members (set (keys ns->files))
        ;; The namespaces a reload may touch at all: remembered, and in scope.
        session (into #{} (filter remembered) members)
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
        ;; Dependents too, and for the same reason: a namespace this session has
        ;; never loaded has nothing to refresh, and a reload does not load it.
        closure (into #{}
                      (filter remembered)
                      (loop [result (set root-set)]
                        (let [next (into result
                                         (comp (mapcat #(get dependents % #{}))
                                               (remove result))
                                         result)]
                          (if (= next result) result (recur next)))))
        ;; A detection-driven reload that would rebuild the whole session is a
        ;; baseline that no longer describes the tree, not an edit. An explicit
        ;; target means the caller has already said what they want.
        wholesale? (and (nil? explicit) (wholesale-reload? session closure))
        order (if wholesale? [] (topological-sort closure dependencies-of))
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
        reloaded-names (set (:reloaded outcome))
        ;; Advance the baseline only for files whose namespaces were actually
        ;; (re)loaded. A file that was merely *scanned* keeps its old digest, so
        ;; an edit it carries stays detectable: otherwise any reload, even an
        ;; explicit one for an unrelated namespace, would silently absorb every
        ;; pending edit and the next `(reload)` would report `:unchanged`.
        new-known (reduce
                   (fn [known file]
                     (let [namespaces (get file->ns file)]
                       (cond
                         (some failed-names namespaces) known
                         (some reloaded-names namespaces)
                         (assoc known file {:digest (get-in scan [:digests file])
                                            :parsed (get parsed file)})
                         :else known)))
                   (apply dissoc known (:missing scan))
                   (:files scan))
        kept (vec (sort (keep (fn [[namespace names]]
                                (when (seq names) namespace))
                              (:kept outcome))))
        status (cond
                 wholesale? :refused
                 (seq failed) :errors
                 (or (seq (:reloaded outcome)) (seq (:unloaded outcome))) :reloaded
                 :else :unchanged)]
    (swap! state assoc :known new-known)
    (repair-aliases! outcome)
    (let [aliases-repaired (refresh-stale-aliases!)
          ;; Every remark the call owes the caller, in one place: they are
          ;; independent, and a report that quietly dropped one because another
          ;; was already written would be the report lying by omission. Each is
          ;; as short as it can be and still be read on its own: what happened,
          ;; and the one command that answers it. The reasoning lives in
          ;; `reload`'s docstring, where there is room for it.
          notes (cond-> []
                  (seq not-loaded)
                  (conj (str (count not-loaded) " namespace(s) never loaded in this "
                             "session: `require` them"))
                  wholesale?
                  (conj (str "the whole session changed (" (count closure) " of "
                             (count session) "), a replaced baseline rather than an "
                             "edit. Nothing was reloaded. Restart the dev server for a "
                             "fresh baseline, or (reload 'some.ns) to mean one"))
                  (and (= :unchanged status) (empty? not-loaded))
                  (conj "nothing changed; name a namespace to reload it")
                  (seq stale)
                  (conj (str (count stale) " namespace(s) changed and were not "
                             "reloaded: (reload) picks them up")))]
      (cond-> {:status status
               :changed (vec (sort root-set))
               :reloaded (vec (:reloaded outcome))
               :unloaded (vec (:unloaded outcome))
               :kept kept
               :failed (vec failed)
               :deferred (vec (sort (:deferred outcome)))
               :scanned (count (:files scan))}
        (seq aliases-repaired)
        (assoc :aliases-repaired aliases-repaired)
        (seq not-loaded)
        (assoc :not-loaded {:count (count not-loaded)
                            :sample (vec (take not-loaded-sample-size not-loaded))})
        wholesale?
        (assoc :would-reload (count closure))
        (seq stale)
        (assoc :stale {:count (count stale)
                       :sample (vec (take not-loaded-sample-size stale))})
        (seq notes)
        (assoc :note (str/join " " notes))))))

(defn reload
  "Reload a namespace set and report compactly.

  `(reload)` reloads what changed since the baseline: file **content** changes
  are detected by digest (never by modification time), and each changed
  namespace pulls in its transitive dependents, topologically ordered. The
  namespaces are unloaded first, so removed vars disappear and `deftype` /
  `defrecord` get fresh classes, while `defonce` values are preserved.

  A reload only concerns namespaces **this session has loaded at least once**,
  and among those the ones that changed or that depend on one that did, which is
  what the reload graph was always for. It refreshes the image; it does not grow
  it. A changed — or brand new — file whose namespace has never been loaded is
  left alone and summarised under `:not-loaded`: loading code runs it, and
  `require` is the verb that does that deliberately, by name. A namespace that a
  *failed* reload left unloaded is still the tool's business, because it has run
  here.

  `(reload 'my.ns)` and `(reload '[a b])` ignore the baseline and force the
  loaded namespaces among those named (and their loaded dependents), which is
  the deterministic escape hatch when detection cannot know about an edit. A
  namespace that is not loaded is still left alone.

  A *named* target reloads what it names and nothing else — deliberately, since
  that is what makes it usable while another file is mid-edit — so it is the one
  form that can leave the image stale. The report therefore says so, under
  `:stale`, naming the namespaces whose content changed and were not reloaded.
  Detection runs on every call for that reason, so `:stale` being absent is a
  claim the tool checked rather than one it assumed.

  `(reload {:dirs [...]})` sets the scan scope first (the classpath source
  directories by default; an optional `:files` pattern selects filenames) and
  then reloads what changed under it; the scope persists for later calls. There
  is no separate initialization step.

  An alias that a `require` made outside the scan — in `user`, say — is
  re-pointed at the reloaded namespace, so it keeps resolving to the current
  vars; the repairs are reported under `:aliases-repaired`.

  Detection can lose the tree: a baseline that no longer describes it, after a
  check-out or a corruption, makes *every* file look changed. A reload driven by
  detection then refuses rather than rebuild the whole live image, reporting
  `:status :refused`, what it saw in `:changed`, `:would-reload`, and a note
  saying what to do about it. There is deliberately **no option that reloads
  everything**: detection is what decides, and a session whose baseline no longer
  describes the tree needs a fresh process — restart the dev server — which is
  the one remedy that cannot half-succeed. Naming namespaces still works, and a
  change to a widely used namespace still cascades normally.

  Every failure is reported, not just the first."
  ([] (with-lock (run-reload! nil)))
  ([target-or-options]
   (with-lock
     (if (map? target-or-options)
       (do (configure! target-or-options)
           (run-reload! nil))
       (let [explicit (cond
                        (symbol? target-or-options) [target-or-options]
                        (sequential? target-or-options) (vec target-or-options)
                        :else (throw (ex-info "reload expects a namespace symbol, a collection, or a scope map"
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
