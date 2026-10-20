(ns omkamra.dev.tools-test
  "Tests for `omkamra.dev.tools` and the reload machinery behind it.

  The namespace under test is the facade an agent calls; the behaviour lives one
  level down in `omkamra.dev.tools.reload`. A reload exercises that machinery
  from the outside but cannot isolate its units, so the parts a whole reload
  cannot single out are read through their vars below — the same way
  `omkamra.dev.frontend-test` reaches into `omkamra.dev.frontend.shadow`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [omkamra.dev.tools :as tools]
            [omkamra.dev.tools.reload :as reload]))

;; `#'reload/...` reaches a private var without widening the API: these tests
;; need the unit, not a public surface.
(def ^:private stale-alias-entries #'reload/stale-alias-entries)
(def ^:private refresh-stale-aliases! #'reload/refresh-stale-aliases!)
(def ^:private failed-namespace! #'reload/failed-namespace!)
(def ^:private initial-digest #'reload/initial-digest)
(def ^:private wholesale-reload? #'reload/wholesale-reload?)
;; A threshold read as a value, so a test about it does not restate it.
(def ^:private wholesale-reload-floor @#'reload/wholesale-reload-floor)
;; The tool's memory of what this session has loaded, so a test can forget.
(def ^:private session-namespaces @#'reload/session-namespaces)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- temp-directory
  []
  (doto (java.io.File/createTempFile "omkamra-tools" "dir")
    (.delete)
    (.mkdirs)))

(defn- delete-tree!
  [file]
  (when (.exists ^java.io.File file)
    (doseq [child (reverse (file-seq file))]
      (.delete ^java.io.File child))))

(defn- write-file!
  "Write `content` and backdate the file, so a test can prove that reloading
  never depends on modification time."
  [file content]
  (spit file content)
  (.setLastModified (io/file file) (- (System/currentTimeMillis) 3600000))
  file)

(defn- forget!
  "Forget namespaces completely, as if this session had never loaded them."
  [& namespaces]
  (doseq [namespace namespaces]
    (when (find-ns namespace)
      (remove-ns namespace))
    (swap! session-namespaces disj namespace)))

(defn- load-fixture!
  "Load a fixture namespace, and file it under its own name as `require` does.

  Fixtures live outside the classpath, so a fixture that `:require`s another one
  needs both halves: `load-file` evaluates the file, and the registration is what
  the inner `require` finds instead of going looking on the classpath."
  [namespace file]
  (load-file (.getPath file))
  (dosync (alter @#'clojure.core/*loaded-libs* conj namespace)))

(defn- fixture-var
  "Read a var from a namespace the test loads at runtime, so the test namespace
  itself does not have to compile against it."
  [namespace name]
  (var-get (ns-resolve namespace name)))

;; The reload scope persists across calls by design, so a test that scopes it to
;; a temp directory would leave the tool pointed at a directory it then deletes.
;; Put the classpath scope back once, after the last test.
(use-fixtures :once
  (fn [run]
    (try (run)
         (finally (tools/reload {})))))

;; ---------------------------------------------------------------------------
;; Reload
;; ---------------------------------------------------------------------------

(deftest reload-with-a-scope-map-scopes-the-scan
  (let [directory (temp-directory)
        file (io/file directory "fixture_scope.clj")]
    (try
      (write-file! file "(ns fixture.scope)\n(def x 1)\n")
      (let [result (tools/reload {:dirs [directory]})]
        (is (= 1 (:scanned result)) "the map scoped the scan to one file"))
      (is (= 1 (:scanned (tools/reload)))
          "the scope persists for later calls")
      (finally
        (forget! 'fixture.scope)
        (delete-tree! directory)))))

(deftest reload-repoints-an-alias-made-outside-the-scan-scope
  (let [directory (temp-directory)
        file (io/file directory "fixture_alias_target.clj")
        owner 'fixture.alias-owner]
    (try
      ;; Loaded here, and not by a reload: a reload runs no code this session has
      ;; not run, and `require` below needs the namespace filed under its name.
      (write-file! file "(ns fixture.alias-target)\n(def marker :first)\n")
      (load-fixture! 'fixture.alias-target file)
      (tools/reload {:dirs [directory]})
      ;; An alias `require` made outside the scan. A namespace inside the scan is
      ;; rebuilt from its own `ns` form and repairs itself; one with no file (a
      ;; REPL namespace) is not, and would keep resolving through the namespace
      ;; object the reload threw away.
      (let [object (create-ns owner)]
        (binding [*ns* object]
          (clojure.core/require '[fixture.alias-target :as t]))
        (is (= :first @(ns-resolve (get (ns-aliases object) 't) 'marker)))
        (write-file! file "(ns fixture.alias-target)\n(def marker :second)\n")
        (let [result (tools/reload)]
          (is (= :reloaded (:status result)))
          (is (some #(= {:in-ns owner :alias 't :target 'fixture.alias-target} %)
                    (:aliases-repaired result))))
        (is (identical? (get (ns-aliases object) 't)
                        (find-ns 'fixture.alias-target))
            "the alias follows the live namespace object, not the dead one")
        (is (= :second @(ns-resolve (get (ns-aliases object) 't) 'marker))
            "and so resolves the reloaded var, not the one it aliased before"))
      (finally
        (forget! owner 'fixture.alias-target)
        (delete-tree! directory)))))

(deftest reload-with-no-changes-is-explicit
  (let [directory (temp-directory)
        file (io/file directory "fixture_quiet.clj")]
    (try
      (write-file! file "(ns fixture.quiet)\n(def x 1)\n")
      (load-file (.getPath file))
      (tools/reload {:dirs [directory]})
      (tools/reload)
      (let [result (tools/reload)]
        (is (= :unchanged (:status result)))
        (is (= [] (:changed result)))
        (is (= [] (:failed result)))
        (is (= 1 (:scanned result)))
        (is (string? (:note result)) "a no-op explains itself"))
      (finally
        (forget! 'fixture.quiet)
        (delete-tree! directory)))))

(deftest reload-detects-content-not-modification-time
  (let [directory (temp-directory)
        file (io/file directory "fixture_content.clj")]
    (try
      (write-file! file "(ns fixture.content)\n(def x 1)\n")
      (load-file (.getPath file))
      (tools/reload {:dirs [directory]})
      ;; `write-file!` backdates the file, so only a content digest can see this
      (write-file! file "(ns fixture.content)\n(def x 2)\n")
      (let [result (tools/reload)]
        (is (= :reloaded (:status result)))
        (is (= '[fixture.content] (:changed result)))
        (is (= 2 @(ns-resolve 'fixture.content 'x))))
      (finally
        (forget! 'fixture.content)
        (delete-tree! directory)))))

(deftest initial-digest-separates-a-fresh-file-from-a-stable-one
  ;; The boundary is this JVM's own start, so the margin cannot be a fixed age:
  ;; an hour of backdating is not "before startup" on a session that has been up
  ;; longer than that. A second either side also survives a coarse filesystem
  ;; timestamp.
  (let [directory (temp-directory)
        file (io/file directory "fixture_baseline.clj")
        start @#'reload/jvm-start-ms
        age! (fn [millis] (.setLastModified file millis))]
    (try
      (spit file "(ns fixture.baseline)\n(def x 1)\n")
      (age! (+ start 1000))
      (is (= ::reload/changed (initial-digest (.getPath file)))
          "a file written since the JVM started is flagged, not digested")
      (age! (- start 1000))
      (let [stable (initial-digest (.getPath file))]
        (is (string? stable) "an older file is digested")
        (is (= stable (initial-digest (.getPath file)))
            "the same content digests the same, so a reload sees no change")
        (spit file "(ns fixture.baseline)\n(def x 2)\n")
        (age! (- start 1000))
        (is (not= stable (initial-digest (.getPath file)))
            "and different content digests differently"))
      (finally
        (delete-tree! directory)))))

(deftest reload-clears-stale-vars-and-preserves-defonce
  (let [directory (temp-directory)
        file (io/file directory "fixture_state.clj")]
    (try
      (write-file! file (str "(ns fixture.state)\n(def x 1)\n(def gone 1)\n"
                             "(defonce counter (atom 0))\n"))
      (load-file (.getPath file))
      (tools/reload {:dirs [directory]})
      (tools/reload)
      (swap! (fixture-var 'fixture.state 'counter) inc)
      (write-file! file "(ns fixture.state)\n(def x 2)\n(defonce counter (atom 0))\n")
      (let [result (tools/reload)]
        (is (= :reloaded (:status result)))
        (is (= '[fixture.state] (:kept result))))
      (is (= 2 (fixture-var 'fixture.state 'x)))
      (is (nil? (ns-resolve 'fixture.state 'gone))
          "a removed def is gone, not a stale var")
      (is (= 1 @(fixture-var 'fixture.state 'counter))
          "defonce state survived the reload")
      (finally
        (forget! 'fixture.state)
        (delete-tree! directory)))))

(deftest reload-pulls-in-dependents
  (let [directory (temp-directory)
        base (io/file directory "fixture_base.clj")
        dep (io/file directory "fixture_dep.clj")]
    (try
      (write-file! base "(ns fixture.base)\n(def x 1)\n")
      (write-file! dep "(ns fixture.dep (:require [fixture.base :as b]))\n(def y (+ b/x 10))\n")
      (load-fixture! 'fixture.base base)
      (load-fixture! 'fixture.dep dep)
      (tools/reload {:dirs [directory]})
      (is (= 11 (fixture-var 'fixture.dep 'y)))
      (write-file! base "(ns fixture.base)\n(def x 2)\n")
      (let [result (tools/reload)]
        (is (= '[fixture.base] (:changed result)))
        (is (= '[fixture.base fixture.dep] (:reloaded result))
            "the dependent is reloaded, in dependency order"))
      (is (= 12 (fixture-var 'fixture.dep 'y))
          "the dependent recomputed against the new value")
      (finally
        (forget! 'fixture.dep 'fixture.base)
        (delete-tree! directory)))))

(deftest reload-pulls-in-an-unloaded-dependent-from-source
  ;; Dependencies are declared in a namespace's `ns` form, so the reload graph
  ;; must be built from source as well as from the live image. A namespace left
  ;; unloaded — say, because its dependency failed to load — has no live image
  ;; to ask, and must still come back when the dependency is fixed. Reading the
  ;; graph from the live image alone loses exactly that case, and the namespace
  ;; then stays missing while `reload` reports success.
  (let [directory (temp-directory)
        base (io/file directory "fixture_base.clj")
        dep (io/file directory "fixture_dep.clj")]
    (try
      (write-file! base "(ns fixture.base)\n(def x 1)\n")
      (write-file! dep "(ns fixture.dep (:require [fixture.base :as b]))\n(def y (+ b/x 10))\n")
      (load-fixture! 'fixture.base base)
      (load-fixture! 'fixture.dep dep)
      (tools/reload {:dirs [directory]})
      (is (= 11 (fixture-var 'fixture.dep 'y)))
      ;; Break the dependency: both fail, and the dependent is left unloaded.
      (write-file! base "(ns fixture.base)\n(defn broken [\n")
      (let [broken (tools/reload)]
        (is (= :errors (:status broken)))
        (is (= #{'fixture.base 'fixture.dep}
               (set (map :namespace (:failed broken))))
            "the dependent fails too, because its dependency did not load"))
      (is (nil? (find-ns 'fixture.dep))
          "and it is left unloaded")
      ;; Fix the dependency. The unloaded dependent must come back from source.
      (write-file! base "(ns fixture.base)\n(def x 2)\n")
      (let [fixed (tools/reload)]
        (is (= '[fixture.base] (:changed fixed)))
        (is (= '[fixture.base fixture.dep] (:reloaded fixed))
            "the unloaded dependent is pulled back in from its source require"))
      (is (= 12 (when (find-ns 'fixture.dep) (fixture-var 'fixture.dep 'y)))
          "and it recomputed against the new value")
      (finally
        (forget! 'fixture.dep 'fixture.base)
        (delete-tree! directory)))))

(deftest reload-refreshes-what-the-session-has-loaded
  (let [directory (temp-directory)
        file (io/file directory "fixture_lifecycle.clj")]
    (try
      (write-file! file "(ns fixture.lifecycle)\n(def x 1)\n")
      (tools/reload {:dirs [directory]})
      ;; A file this session never loaded is left alone: a reload runs no code
      ;; that has not already run here, and it says what it skipped instead.
      (write-file! file "(ns fixture.lifecycle)\n(def x 7)\n")
      (let [skipped (tools/reload)]
        (is (= :unchanged (:status skipped)))
        (is (= {:count 1 :sample '[fixture.lifecycle]} (:not-loaded skipped)))
        (is (nil? (find-ns 'fixture.lifecycle)) "and nothing was loaded"))
      ;; Once loaded, the same file is the tool's business.
      (load-fixture! 'fixture.lifecycle file)
      (write-file! file "(ns fixture.lifecycle)\n(def x 2)\n")
      (let [refreshed (tools/reload)]
        (is (= :reloaded (:status refreshed)))
        (is (= '[fixture.lifecycle] (:reloaded refreshed)))
        (is (= 2 (fixture-var 'fixture.lifecycle 'x))))
      (.delete file)
      (let [removed (tools/reload)]
        (is (= '[fixture.lifecycle] (:unloaded removed)))
        (is (nil? (find-ns 'fixture.lifecycle))))
      (finally
        (forget! 'fixture.lifecycle)
        (delete-tree! directory)))))

(deftest reload-reports-every-failure
  (let [directory (temp-directory)
        good (io/file directory "fixture_good.clj")
        bad-a (io/file directory "fixture_bad_a.clj")
        bad-b (io/file directory "fixture_bad_b.clj")]
    (try
      (write-file! good "(ns fixture.good)\n(def x 1)\n")
      (write-file! bad-a "(ns fixture.bad-a)\n(defn ok [x] x)\n")
      (write-file! bad-b "(ns fixture.bad-b)\n(defn ok [x] x)\n")
      (doseq [[namespace file] [['fixture.good good]
                                ['fixture.bad-a bad-a]
                                ['fixture.bad-b bad-b]]]
        (load-fixture! namespace file))
      (tools/reload {:dirs [directory]})
      ;; Broke them after they were loaded, which is the only way a reload comes
      ;; to look at them at all: it runs no code this session has not run.
      (write-file! good "(ns fixture.good)\n(def x 2)\n")
      (write-file! bad-a "(ns fixture.bad-a)\n(defn broken [x]\n")
      (write-file! bad-b "(ns fixture.bad-b)\n(defn broken [x]\n")
      (let [result (tools/reload)]
        (is (= :errors (:status result)))
        (is (= #{'fixture.bad-a 'fixture.bad-b}
               (set (map :namespace (:failed result))))
            "both failures are reported, not just the first")
        (is (every? #(string? (:message %)) (:failed result)))
        (is (contains? (set (:reloaded result)) 'fixture.good)
            "and the namespace that is fine still reloads")
        (is (= 2 (fixture-var 'fixture.good 'x))))
      (finally
        (forget! 'fixture.good 'fixture.bad-a 'fixture.bad-b)
        (delete-tree! directory)))))

(deftest reload-explicit-target-forces-without-a-baseline
  (let [directory (temp-directory)
        file (io/file directory "fixture_force.clj")]
    (try
      (write-file! file "(ns fixture.force)\n(def x 1)\n")
      (load-file (.getPath file))
      (tools/reload {:dirs [directory]})
      (tools/reload)
      (let [result (tools/reload 'fixture.force)]
        (is (= :reloaded (:status result)))
        (is (= '[fixture.force] (:changed result))))
      (finally
        (forget! 'fixture.force)
        (delete-tree! directory)))))

(deftest reload-does-not-absorb-an-unreloaded-edit
  ;; An explicit target reloads only the target's closure, so every other file's
  ;; baseline must stay where it was: an edit in an unrelated file has to stay
  ;; detectable. If the baseline advanced for *every* scanned file, the explicit
  ;; reload would silently absorb that edit and the next plain `reload` would
  ;; report `:unchanged`.
  (let [directory (temp-directory)
        file-a (io/file directory "fixture_a.clj")
        file-b (io/file directory "fixture_b.clj")]
    (try
      (write-file! file-a "(ns fixture.a)\n(def x 1)\n")
      (write-file! file-b "(ns fixture.b)\n(def x 1)\n")
      (load-file (.getPath file-a))
      (load-file (.getPath file-b))
      (tools/reload {:dirs [directory]})
      ;; Edit both files, then reload only `fixture.a`.
      (write-file! file-a "(ns fixture.a)\n(def x 2)\n")
      (write-file! file-b "(ns fixture.b)\n(def x 2)\n")
      (let [explicit (tools/reload 'fixture.a)]
        (is (= :reloaded (:status explicit)))
        (is (= '[fixture.a] (:reloaded explicit))
            "only the explicit target is reloaded")
        (is (= {:count 1 :sample '[fixture.b]} (:stale explicit))
            (str "and the report names the edit it left behind, so \"the image is "
                 "current now\" is never something the caller assumed"))
        (is (str/includes? (str (:note explicit)) "not reloaded")
            "said in words too"))
      (let [next-result (tools/reload)]
        (is (= :reloaded (:status next-result))
            "the unrelated edit is still detected, not absorbed")
        (is (= '[fixture.b] (:changed next-result)))
        (is (nil? (:stale next-result))
            (str "and a detection-driven reload leaves nothing stale, so the key "
                 "above is a claim the tool checked rather than one it assumed"))
        (is (= 2 @(ns-resolve 'fixture.b 'x))))
      (finally
        (forget! 'fixture.a 'fixture.b)
        (delete-tree! directory)))))

(deftest reload-report-is-serializable
  (let [directory (temp-directory)
        file (io/file directory "fixture_serial.clj")]
    (try
      (write-file! file "(ns fixture.serial)\n(def x 1)\n")
      (load-file (.getPath file))
      (tools/reload {:dirs [directory]})
      (let [report (tools/reload)
            printed (pr-str report)]
        (is (string? printed))
        (is (not (str/includes? printed "#object"))
            "no File or Throwable objects leak into the report")
        (is (every? #(not (instance? Throwable %)) (tree-seq coll? seq report))))
      (finally
        (forget! 'fixture.serial)
        (delete-tree! directory)))))

;; ---------------------------------------------------------------------------
;; Persistent aliases
;; ---------------------------------------------------------------------------

(deftest persistent-alias-creation-is-idempotent
  (let [owner (create-ns 'omkamra.tools-test-owner)
        target (create-ns 'omkamra.tools-test-target)
        alias-name 'target-api]
    (try
      (is (= {:in-ns 'omkamra.tools-test-owner
              :alias alias-name
              :target 'omkamra.tools-test-target}
             (tools/create-persistent-alias!
              {:in-ns 'omkamra.tools-test-owner
               :alias alias-name
               :target 'omkamra.tools-test-target})))
      (is (= target (get (ns-aliases owner) alias-name)))
      (is (= target
             (get (ns-aliases owner)
                  (:alias (tools/create-persistent-alias!
                           {:in-ns 'omkamra.tools-test-owner
                            :alias alias-name
                            :target 'omkamra.tools-test-target})))))
      (finally
        (ns-unalias owner alias-name)
        (remove-ns 'omkamra.tools-test-owner)
        (remove-ns 'omkamra.tools-test-target)))))

(deftest persistent-aliases-refresh-after-target-reload
  (let [directory (temp-directory)
        file (io/file directory "fixture_alias.clj")
        namespace-symbol 'fixture.alias]
    (try
      (write-file! file "(ns fixture.alias)\n(def value 1)\n")
      (tools/reload {:dirs [directory]})
      (load-file (.getPath file))
      (tools/create-persistent-alias!
       {:in-ns 'user :alias 'reload-fixture :target namespace-symbol})
      (write-file! file "(ns fixture.alias)\n(def value 2)\n")
      (let [result (tools/reload)
            alias-target (get (ns-aliases (find-ns 'user)) 'reload-fixture)]
        (is (= :reloaded (:status result)))
        (is (= namespace-symbol (ns-name alias-target)))
        (is (= 2 @(ns-resolve alias-target 'value))))
      (finally
        (ns-unalias (find-ns 'user) 'reload-fixture)
        (forget! namespace-symbol)
        (delete-tree! directory)))))

(deftest persistent-alias-target-failure-becomes-a-stub
  (let [directory (temp-directory)
        file (io/file directory "fixture_stub.clj")
        namespace-symbol 'fixture.stub]
    (try
      (write-file! file "(ns fixture.stub)\n(def value 1)\n")
      (tools/reload {:dirs [directory]})
      (load-file (.getPath file))
      (tools/create-persistent-alias!
       {:in-ns 'user :alias 'stub-fixture :target namespace-symbol})
      (write-file! file "(ns fixture.stub)\n(defn broken [x]\n")
      (let [result (tools/reload)
            alias-target (get (ns-aliases (find-ns 'user)) 'stub-fixture)]
        (is (= :errors (:status result)))
        (is (some #(= namespace-symbol (:namespace %)) (:failed result)))
        (is (not= namespace-symbol (ns-name alias-target))
            "the alias points at a diagnostic stub, not the broken namespace")
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"unavailable"
             ((ns-resolve alias-target 'unavailable)))))
      (finally
        (ns-unalias (find-ns 'user) 'stub-fixture)
        (forget! namespace-symbol)
        (delete-tree! directory)))))

;; ---------------------------------------------------------------------------
;; Alias repair
;; ---------------------------------------------------------------------------

(deftest stale-alias-sweep-repoints-only-a-replaced-target
  (let [owner-name 'fixture.sweep-owner
        target-name 'fixture.sweep-target
        owner (create-ns owner-name)
        mine (fn [] (filter #(= owner-name (first %)) (stale-alias-entries)))]
    (try
      (create-ns target-name)
      (binding [*ns* owner]
        (clojure.core/alias 't target-name))
      (is (empty? (mine)) "a live alias is not stale")
      ;; Unloading removes a namespace from the registry; the alias keeps the
      ;; object it was handed, which is what makes it stale.
      (remove-ns target-name)
      (is (empty? (mine))
          "a target that is gone entirely has no replacement to point at, so nothing is reported")
      (let [target (create-ns target-name)]
        (is (= [[owner-name 't target-name]] (vec (mine)))
            "the alias holds the object the reload threw away")
        (is (some #(= {:in-ns owner-name :alias 't :target target-name} %)
                  (refresh-stale-aliases!))
            "the sweep reports what it repaired")
        (is (identical? target (get (ns-aliases owner) 't))
            "the alias follows the live object")
        (is (empty? (mine)) "and the sweep is idempotent"))
      (finally
        (ns-unalias owner 't)
        (remove-ns owner-name)
        (forget! target-name)))))

(deftest failed-namespace-stub-is-addressable-and-explains-itself
  (let [target 'fixture.unloadable
        stub (failed-namespace! target "could not be read")
        other (failed-namespace! 'fixture.another-unloadable "other")
        unavailable (ns-resolve stub 'unavailable)]
    (try
      (is (symbol? stub))
      (is (= {:target target :message "could not be read"}
             (:omkamra.dev.tools/failure (meta (find-ns stub)))))
      (let [failure (try (unavailable)
                         nil
                         (catch clojure.lang.ExceptionInfo error error))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            "using the stub throws rather than returning something plausible")
        (is (re-find #"could not be read" (ex-message failure)))
        (is (= target (:target (ex-data failure)))
            "and names the namespace it stands for"))
      (is (= stub (failed-namespace! target "could not be read"))
          "the same target always gets the same stub")
      (is (not= stub other))
      (finally
        (remove-ns stub)
        (remove-ns other)))))

;; ---------------------------------------------------------------------------
;; Refusing a wholesale reload
;; ---------------------------------------------------------------------------

(deftest wholesale-reload-guard-fires-on-a-whole-tree-reload-only
  (let [below (into #{} (map #(symbol (str "fixture.small-" %)))
                    (range (dec wholesale-reload-floor)))
        at-floor (into #{} (map #(symbol (str "fixture.floor-" %)))
                       (range wholesale-reload-floor))
        big (into #{} (map #(symbol (str "fixture.big-" %)))
                  (range (* 2 wholesale-reload-floor)))
        half (quot (count big) 2)]
    (is (not (wholesale-reload? below below))
        "a scope below the floor may legitimately change wholesale")
    (is (wholesale-reload? at-floor at-floor)
        "at the floor, rebuilding the whole scope is a whole-tree reload")
    (is (wholesale-reload? big big)
        "and so is rebuilding a large one")
    (is (wholesale-reload? big (set (take half big)))
        "half of a large scope is enough")
    (is (not (wholesale-reload? big (set (take (dec half) big))))
        "just under half is an edit")
    (is (wholesale-reload? big (conj big 'fixture.gone))
        "a closure larger than the scope counts too, as mass deletion produces")
    (is (not (wholesale-reload? big #{}))
        "a reload with nothing to do is not wholesale")))

(deftest reload-refuses-to-rebuild-the-whole-scope
  (let [directory (temp-directory)
        files (mapv #(io/file directory (str "fixture_bulk_" % ".clj"))
                    (range wholesale-reload-floor))
        namespaces (mapv #(symbol (str "fixture.bulk-" %))
                         (range wholesale-reload-floor))
        write-all! (fn [base]
                     (doseq [[file index] (map vector files (range))]
                       (write-file! file (str "(ns fixture.bulk-" index ")\n"
                                              "(def value " (+ base index) ")\n"))))]
    (try
      (write-all! 0)
      ;; Loaded here rather than by a reload: a reload only concerns namespaces
      ;; this session has run, so the guard's population has to be in the image
      ;; before there is anything to refuse over.
      (doseq [[namespace file] (map vector namespaces files)]
        (load-fixture! namespace file))
      (is (= :unchanged (:status (tools/reload {:dirs [directory]})))
          "the scope is installed with the files as they are")
      ;; Every file changes at once: a replaced or corrupted baseline, and the
      ;; one situation detection cannot tell from a real edit.
      (write-all! 100)
      (let [result (tools/reload)]
        (is (= :refused (:status result)))
        (is (= [] (:reloaded result)))
        (is (= wholesale-reload-floor (:would-reload result)))
        (is (= wholesale-reload-floor (count (:changed result))))
        (is (re-find #"\(reload :all\)" (:note result))
            "the refusal spells out the call that means it")
        (is (re-find #"\(reload 'some\.ns\)" (:note result))
            "and the narrower one")
        (is (= 0 (fixture-var 'fixture.bulk-0 'value))
            "and nothing was reloaded, which is the point of refusing"))
      (is (= :reloaded (:status (tools/reload :all)))
          ":all is how to mean it")
      (is (= [101 102] [@(ns-resolve 'fixture.bulk-1 'value)
                        @(ns-resolve 'fixture.bulk-2 'value)])
          "and it refreshes what it was told to")
      (finally
        (apply forget! namespaces)
        (delete-tree! directory)))))
