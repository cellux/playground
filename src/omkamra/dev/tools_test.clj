(ns omkamra.dev.tools-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.dev.tools :as tools]))

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

(deftest lint-files-returns-standard-report
  (let [result (tools/lint-files "src/omkamra/dev/tools.clj")]
    (is (= :processed (:status result)))
    (is (= [] (:errors result)))
    (is (map? (:summary result)))
    (is (vector? (:findings result)))))

(deftest lint-reports-error-findings
  (let [file (java.io.File/createTempFile "omkamra-tools" ".clj")]
    (try
      (spit file "(defn broken [x]\n")
      (let [result (tools/lint-files (.getPath file))]
        (is (= :errors (:status result)))
        (is (seq (:errors result)))
        (is (= (:filename (first (:findings result)))
               (:filename (first (:errors result)))))
        (is (= :error (:level (first (:findings result))))))
      (finally
        (.delete file)))))

(deftest format-files-reports-changes
  (let [directory (temp-directory)
        file (java.io.File. directory "example.clj")]
    (try
      (spit file "(defn example[x] (+ x 1))\n")
      (let [result (tools/format-files (.getPath file))]
        (is (= :processed (:status result)))
        (is (= [(.getPath file)] (:formatted result)))
        (is (= [] (:unchanged result)))
        (is (= [] (:errors result)))
        (is (= "(defn example [x] (+ x 1))\n" (slurp file))))
      (finally
        (delete-tree! directory)))))

(deftest format-string-is-explicitly-non-writing
  (is (= "(defn example [x] (+ x 1))"
         (tools/format-string "(defn example[x] (+ x 1))"))))

(deftest invalid-paths-fail-before-processing
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Path does not exist"
       (tools/lint-files "/path/that/does/not/exist")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Path does not exist"
       (tools/format-files "/path/that/does/not/exist"))))

(deftest init-and-reload-are-explicit
  (let [directory (temp-directory)]
    (try
      (let [result (tools/init-reload [directory])]
        (is (= :initialized (:status result)))
        (is (number? (:namespace-count result)))
        (is (= {:reloaded [] :unloaded [] :status :unchanged}
               (tools/reload))))
      (finally
        (delete-tree! directory)))))

(deftest managed-alias-creation-is-idempotent
  (let [owner (create-ns 'omkamra.tools-test-owner)
        target (create-ns 'omkamra.tools-test-target)
        alias-name 'target-api]
    (try
      (is (= {:in-ns 'omkamra.tools-test-owner
              :alias alias-name
              :target 'omkamra.tools-test-target}
             (tools/create-alias!
              {:in-ns 'omkamra.tools-test-owner
               :alias alias-name
               :target 'omkamra.tools-test-target})))
      (is (= target (get (ns-aliases owner) alias-name)))
      (is (= target
             (get (ns-aliases owner)
                  (:alias (tools/create-alias!
                           {:in-ns 'omkamra.tools-test-owner
                            :alias alias-name
                            :target 'omkamra.tools-test-target})))))
      (finally
        (ns-unalias owner alias-name)
        (remove-ns 'omkamra.tools-test-owner)
        (remove-ns 'omkamra.tools-test-target)))))

(deftest managed-aliases-refresh-after-target-reload
  (let [directory (temp-directory)
        file (java.io.File. directory "fixture.clj")
        namespace-symbol 'omkamra.tools-reload-fixture
        alias-name 'reload-fixture]
    (try
      (spit file "(ns omkamra.tools-reload-fixture)\n(def value 1)\n")
      (tools/init-reload [directory])
      (load-file (.getPath file))
      (tools/create-alias!
       {:in-ns 'user :alias alias-name :target namespace-symbol})
      (spit file "(ns omkamra.tools-reload-fixture)\n(def value 2)\n")
      (.setLastModified file (+ (System/currentTimeMillis) 2000))
      (let [result (tools/reload)
            alias-target (get (ns-aliases (find-ns 'user)) alias-name)]
        (is (= :reloaded (:status result)))
        (is (= namespace-symbol (ns-name alias-target)))
        (is (= 2 @(ns-resolve alias-target 'value))))
      (finally
        (ns-unalias (find-ns 'user) alias-name)
        (when (find-ns namespace-symbol)
          (remove-ns namespace-symbol))
        (delete-tree! directory)))))
