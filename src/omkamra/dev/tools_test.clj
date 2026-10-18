(ns omkamra.dev.tools-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dev.tools :as tools]))

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
  [& namespaces]
  (doseq [namespace namespaces]
    (when (find-ns namespace)
      (remove-ns namespace))))

(defn- fixture-var
  "Read a var from a namespace the test loads at runtime, so the test namespace
  itself does not have to compile against it."
  [namespace name]
  (var-get (ns-resolve namespace name)))

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
      (tools/reload {:dirs [directory]})
      ;; `:all` is how a fresh namespace tree gets loaded initially; `load-file`
      ;; alone would leave `fixture.dep` unable to `:require` its source.
      (tools/reload :all)
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

(deftest reload-loads-new-and-unloads-deleted-namespaces
  (let [directory (temp-directory)
        file (io/file directory "fixture_lifecycle.clj")]
    (try
      (tools/reload {:dirs [directory]})
      (write-file! file "(ns fixture.lifecycle)\n(def x 1)\n")
      (let [added (tools/reload)]
        (is (= :reloaded (:status added)))
        (is (= '[fixture.lifecycle] (:reloaded added)))
        (is (= 1 (fixture-var 'fixture.lifecycle 'x))))
      (.delete file)
      (let [removed (tools/reload)]
        (is (= :reloaded (:status removed)))
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
      (tools/reload {:dirs [directory]})
      (write-file! good "(ns fixture.good)\n(def x 1)\n")
      (write-file! bad-a "(ns fixture.bad-a)\n(defn broken [x]\n")
      (write-file! bad-b "(ns fixture.bad-b)\n(defn broken [x]\n")
      (let [result (tools/reload)]
        (is (= :errors (:status result)))
        (is (= #{'fixture.bad-a 'fixture.bad-b}
               (set (map :namespace (:failed result))))
            "both failures are reported, not just the first")
        (is (every? #(string? (:message %)) (:failed result)))
        (is (contains? (set (:reloaded result)) 'fixture.good)
            "an unrelated namespace still loads"))
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
