(ns omkamra.vice.analysis-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.decoder :as decoder]))

(defn- delete-tree!
  [file]
  (when (.isDirectory file)
    (doseq [child (.listFiles file)]
      (delete-tree! child)))
  (.delete file))

(defn- private
  [symbol]
  (var-get (ns-resolve 'omkamra.vice.decoder symbol)))

(defn- with-raw-capture
  [f]
  (let [directory (doto (java.io.File/createTempFile "omkamra-analysis-" "")
                    (.delete)
                    (.mkdirs))
        _ (.mkdirs (io/file directory "chunks"))
        options ((private 'normalized-chunk-options)
                 {:chunk-max-events 2 :chunk-queue-capacity 2})
        metadata {:capture-id "analysis-test" :input "/tmp/demo.prg"}
        manifest (atom ((private 'make-manifest) (.getPath directory)
                                                 metadata options))
        queue (java.util.concurrent.ArrayBlockingQueue. 2)
        writer-state (atom {:status :running :queue-depth 0 :high-water-mark 0
                            :chunks-written 0 :backpressure-count 0})
        writer ((private 'start-chunk-writer!) (.getPath directory) manifest
                                               queue writer-state)
        coordinator (atom {:open-chunk ((private 'open-chunk)
                                        1 0 (byte-array 65536) false)
                           :retain-samples? false
                           :writer-queue queue
                           :writer-state writer-state
                           :writer-thread writer})
        records [[0x1000 [0xea] 1 1 0 0 0 nil "........" nil]
                 [0x1001 [0x60] 1 2 0 0 0 nil "........" nil]
                 [0x1002 [0xea] 1 3 0 0 0 nil "........" nil]]]
    (try
      ((private 'write-manifest!) (.getPath directory) manifest)
      ((private 'ingest-chunked-batch!) coordinator metadata options records)
      ((private 'close-open-chunk!) coordinator metadata options
                                    {:kind :final :reason :stopped})
      ((private 'shutdown-chunk-writer!) coordinator 5000)
      (swap! manifest assoc :status :stopped :finalized? true)
      ((private 'write-manifest!) (.getPath directory) manifest)
      (f (.getPath directory))
      (finally
        (delete-tree! directory)))))

(deftest stages-expose-a-broadcast-reducer-contract
  (let [registry (var-get (ns-resolve 'omkamra.vice.analysis 'stage-registry))
        stage (first registry)]
    (is (= [:id :version :depends-on :init :step :close]
           (keys stage)))
    (is (every? fn? ((juxt :init :step :close) stage)))))

(deftest analysis-is-explicit-and-reuses-compatible-stage-output
  (with-raw-capture
    (fn [directory]
      (testing "raw capture finalization does not create derived output"
        (is (not (.exists (io/file directory "analysis"))))
        (is (= :not-run (:analysis-status (analysis/status directory)))))
      (testing "the requested stage writes a versioned replaceable result"
        (let [result (analysis/run! directory {:stage :structure})
              manifest (decoder/read-capture-manifest directory)
              analysis-manifest
              (read-string (slurp (io/file directory "analysis" "manifest.edn")))]
          (is (= [:structure] (:executed-stages result)))
          (is (= [] (:skipped-stages result)))
          (is (= :stopped (:status manifest)))
          (is (= true (:finalized? manifest)))
          (is (= :complete (get-in manifest [:analysis :status])))
          (is (= "analysis/manifest.edn" (get-in manifest [:analysis :manifest-file])))
          (is (= :complete (:status analysis-manifest)))
          (is (= "structure-v1"
                 (get-in analysis-manifest [:stages :structure :version])))
          (is (.isFile (io/file directory "analysis" "stages"
                                "structure" "index.edn")))))
      (testing "later stages can be requested without rerunning structure"
        (let [result (analysis/run! directory {:stage :all})]
          (is (= [:writes :video :assets] (:executed-stages result)))
          (is (= [:structure] (:skipped-stages result)))
          (is (.isFile (io/file directory "analysis" "stages"
                                "video" "index.edn")))
          (is (.isFile (io/file directory "analysis" "stages"
                                "assets" "index.edn"))))))))

(deftest named-targets-resolve-transitive-dependencies
  (with-raw-capture
    (fn [directory]
      (let [assets-result (analysis/run! directory {:stage :assets})]
        (is (= [:writes :video :assets] (:executed-stages assets-result)))
        (is (= [] (:skipped-stages assets-result))))
      (let [all-result (analysis/run! directory {:stage :all})]
        (is (= [:structure] (:executed-stages all-result)))
        (is (= [:writes :video :assets] (:skipped-stages all-result)))))))

(deftest broadcasts-each-raw-chunk-to-all-requested-stages-once
  (with-raw-capture
    (fn [directory]
      (let [read-chunk decoder/read-chunk
            read-stage-chunk (ns-resolve 'omkamra.vice.analysis
                                         'read-stage-chunk)
            raw-chunk-reads (atom 0)]
        (with-redefs-fn
          {#'decoder/read-chunk
           (fn [& args]
             (swap! raw-chunk-reads inc)
             (apply read-chunk args))
           read-stage-chunk
           (fn [& _]
             (throw (ex-info "broadcast dependencies must stay in memory" {})))
           #'decoder/expand-writes
           (fn [& _]
             (throw (ex-info "eager write expansion should not run" {})))}
          #(is (= [:writes :structure :video :assets]
                  (:executed-stages (analysis/run! directory {:stage :all})))))
        ;; The fixture has two chunks. Before broadcasting, each of the four
        ;; stages independently parsed both raw chunk EDN files. The throwing
        ;; replacement above also proves that assets receives video's in-flight
        ;; result rather than rereading its staged output. The eager
        ;; expansion path is also forbidden: workers consume compact records
        ;; through fresh streaming sources.
        (is (= 2 @raw-chunk-reads))))))

(deftest caches-one-committed-writes-dependency-per-chunk
  (with-raw-capture
    (fn [directory]
      (analysis/run! directory {:stage :writes :chunk-parallelism 1})
      (let [read-stage-chunk (ns-resolve 'omkamra.vice.analysis
                                         'read-stage-chunk)
            original-read-stage-chunk (var-get read-stage-chunk)
            stage-reads (atom [])]
        (with-redefs-fn
          {read-stage-chunk
           (fn [& args]
             (swap! stage-reads conj (nth args 2))
             (apply original-read-stage-chunk args))
           #'decoder/expand-writes
           (fn [& _]
             (throw (ex-info "eager write expansion should not run" {})))}
          #(is (= [:video :assets]
                  (:executed-stages
                   (analysis/run! directory {:stage :assets
                                             :chunk-parallelism 1})))))
        ;; Video and assets share the committed descriptor read for each raw
        ;; chunk and consume its compact source without eager expansion.
        (is (= [:writes :writes] @stage-reads))))))

(deftest a-failed-broadcast-branch-does-not-publish-other-staged-output
  (with-raw-capture
    (fn [directory]
      (with-redefs [decoder/derive-assets-chunk
                    (fn [& _]
                      (throw (ex-info "asset derivation failed" {})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"asset derivation failed"
                              (analysis/run! directory {:stage :all}))))
      ;; Writes, structure, and video completed their private staging trees,
      ;; but none becomes visible when a dependent worker fails.
      (is (not (.exists (io/file directory "analysis" "stages" "writes"))))
      (is (not (.exists (io/file directory "analysis" "stages" "structure"))))
      (is (not (.exists (io/file directory "analysis" "stages" "video"))))
      (is (= :failed (:analysis-status (analysis/status directory)))))))

(deftest analysis-failure-preserves-last-successful-stage-output
  (with-raw-capture
    (fn [directory]
      (analysis/run! directory {:stage :structure})
      (let [index-file (io/file directory "analysis" "stages"
                                "structure" "index.edn")
            before (slurp index-file)]
        (with-redefs [decoder/derive-structure-chunk
                      (fn [_]
                        (throw (ex-info "derived analysis failed" {})))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"derived analysis failed"
                                (analysis/run! directory {:stage :structure :force? true}))))
        (is (= before (slurp index-file)))
        (is (= :stopped (:status (decoder/read-capture-manifest directory))))
        (is (= :failed (:analysis-status (analysis/status directory))))
        ;; Existing compatible output is still usable, and a later request
        ;; clears the failed run state without rewriting that output.
        (is (= [:structure]
               (:skipped-stages (analysis/run! directory {:stage :structure}))))))))

(deftest analysis-recovers-stale-partial-output
  (with-raw-capture
    (fn [directory]
      (let [partial (io/file directory "analysis" "stages"
                             "structure.partial")]
        (.mkdirs partial)
        (spit (io/file partial "incomplete.edn") "incomplete")
        (analysis/run! directory {:stage :structure})
        (is (not (.exists partial)))
        (is (= :complete (:analysis-status (analysis/status directory))))))))
