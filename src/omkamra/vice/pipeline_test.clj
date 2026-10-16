(ns omkamra.vice.pipeline-test
  "Tests for the analysis dependency-graph scheduler and its transactional
  commit / reuse-integrity behaviour."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.analysis.core :as analysis]
            [omkamra.vice.pipeline :as pipeline]
            [omkamra.vice.store :as store]
            [omkamra.vice.synthetic-test-support :as synthetic])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest target-normalization
  (testing "a single stage, a collection, and :all"
    (is (= #{:segments} (pipeline/normalize-targets :segments)))
    (is (= #{:segments :assets} (pipeline/normalize-targets [:segments :assets])))
    (is (= (set (keys pipeline/dependencies)) (pipeline/normalize-targets :all))))
  (testing "unknown and malformed targets are rejected"
    (is (= :unknown-stage
           (:reason (ex-data (try (pipeline/normalize-targets :nope)
                                  (catch Exception e e))))))
    (is (= :invalid-analysis-target
           (:reason (ex-data (try (pipeline/normalize-targets 42)
                                  (catch Exception e e))))))))

(deftest closure-is-dependency-first
  (testing "a single target is its transitive dependency closure, deps first"
    (let [order (pipeline/closure (pipeline/normalize-targets :segments))
          position (into {} (map-indexed (fn [index stage] [stage index]) order))]
      (is (= [:dedupe :audio :features :classification :segments] order))
      (doseq [[stage deps] pipeline/dependencies
              dep deps
              :when (and (contains? position stage)
                         (contains? position dep))]
        (is (< (position dep) (position stage))
            (str (name dep) " must precede " (name stage))))))
  (testing "multiple targets merge into one dependency-first order"
    (let [order (pipeline/closure (pipeline/normalize-targets [:segments :assets]))
          index (fn [stage] (.indexOf ^java.util.List order stage))]
      (is (= (set order)
             #{:video :assets :dedupe :audio :features :classification :segments}))
      (is (< (index :video) (index :assets)))
      (is (< (index :dedupe) (index :features)))
      (is (< (index :features) (index :segments))))))

(deftest default-parallelism
  (is (pos-int? (pipeline/default-stage-parallelism))))

(deftest invalid-parallelism-is-rejected-before-opening-the-capture
  (is (= :invalid-stage-parallelism
         (:reason (ex-data (try (pipeline/build! "/nonexistent-dataset" :segments
                                                 {:stage-parallelism 0})
                                (catch Exception e e)))))))

(deftest reuse-decision-rules
  (testing "missing entry, version, signature, and forced builds all rebuild"
    (is (= :rebuild (pipeline/reuse-decision nil 1 "sig" {} false)))
    (is (= :rebuild (pipeline/reuse-decision {:version 2 :signature "sig" :provenance {}}
                                             1 "sig" {} false)))
    (is (= :rebuild (pipeline/reuse-decision {:version 1 :signature "other" :provenance {}}
                                             1 "sig" {} false)))
    (is (= :rebuild (pipeline/reuse-decision {:version 1 :signature "sig" :provenance {}}
                                             1 "sig" {} true))))
  (testing "legacy entries are reused once and get provenance backfilled"
    (is (= :backfill (pipeline/reuse-decision {:version 1 :signature "sig"}
                                              1 "sig" {} false))))
  (testing "matching provenance reuses"
    (is (= :reuse (pipeline/reuse-decision
                   {:version 1 :signature "sig" :provenance {:dedupe "g1"}}
                   1 "sig" {:dedupe "g1"} false))))
  (testing "a same-signature but different upstream generation invalidates reuse"
    (is (= :rebuild (pipeline/reuse-decision
                     {:version 1 :signature "sig" :provenance {:dedupe "g1"}}
                     1 "sig" {:dedupe "g2"} false)))))

(deftest transaction-defers-until-publish
  (let [root (str (Files/createTempDirectory "vice-tx-" (make-array FileAttribute 0)))
        tx (atom {})]
    (try
      (is (nil? (analysis/read-manifest root)) "nothing is on disk yet")
      (binding [analysis/*transaction* tx]
        (analysis/commit-stage! root :dedupe
                                {:generation "g1" :directory "analysis/dedupe/g1"
                                 :version 1})
        (is (= "g1" (get-in (analysis/read-manifest root) [:stages :dedupe :generation]))
            "the staged entry is visible inside the transaction"))
      (is (nil? (analysis/read-manifest root))
          "nothing is written until publish")
      (analysis/publish! root tx)
      (is (= "g1" (get-in (analysis/read-manifest root) [:stages :dedupe :generation]))
          "publish makes the staged entry durable")
      (finally
        (store/delete-tree! (io/file root))))))

(deftest manifest-lock-is-os-visible
  (let [root (str (Files/createTempDirectory "vice-lock-" (make-array FileAttribute 0)))]
    (try
      (analysis/with-manifest-lock
        root
        (fn []
          (let [lock-file (io/file root "analysis" ".lock")]
            (is (.isFile lock-file) "the lock file is created")
            (is (thrown? java.nio.channels.OverlappingFileLockException
                         (with-open [channel
                                     (java.nio.channels.FileChannel/open
                                      (.toPath lock-file)
                                      (into-array java.nio.file.OpenOption
                                                  [java.nio.file.StandardOpenOption/CREATE
                                                   java.nio.file.StandardOpenOption/WRITE]))]
                           (.tryLock channel)))
                "the OS lock is held while the lock is taken"))))
      ;; the lock is released afterwards, so it can be taken again
      (is (= :ok (analysis/with-manifest-lock root (fn [] :ok))))
      (finally
        (store/delete-tree! (io/file root))))))

(defn- generation-names
  [root stage]
  (let [directory (io/file root "analysis" (name stage))]
    (if (.isDirectory directory)
      (set (map #(.getName ^java.io.File %) (.listFiles directory)))
      #{})))

(deftest failed-invocation-publishes-nothing
  (let [fixture (synthetic/fixture)
        root (:root fixture)
        before (:stages (analysis/read-manifest root))
        before-generations (into {} (map (fn [stage] [stage (generation-names root stage)]))
                                 [:dedupe :audio :features])
        build-stage (ns-resolve 'omkamra.vice.pipeline 'build-stage!)
        original @build-stage]
    (with-redefs-fn
      {build-stage (fn [stage context configuration]
                     (if (= stage :classification)
                       (throw (ex-info "injected failure" {}))
                       (original stage context configuration)))}
      (fn []
        (is (thrown? Exception
                     (pipeline/build! root :segments
                                      {:force #{:dedupe :audio :features :classification}})))))
    (let [after (:stages (analysis/read-manifest root))
          pointers (fn [stages] (into {} (map (fn [[s e]] [s (:generation e)])) stages))]
      (is (= (set (keys before)) (set (keys after)))
          "no new stage pointer was published")
      (is (= (pointers before) (pointers after))
          "every committed pointer is unchanged"))
    (doseq [stage [:dedupe :audio :features]]
      (is (= (before-generations stage) (generation-names root stage))
          (str "generations created for " (name stage) " were removed")))))
