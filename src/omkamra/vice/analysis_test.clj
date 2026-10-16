(ns omkamra.vice.analysis-test
  "Tests for the high-level finalized-dataset inspection API."
  (:require [clojure.test :refer [deftest is]]
            [omkamra.vice :as vice]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.store :as store]
            [omkamra.vice.synthetic-test-support :as synthetic])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-root []
  (str (Files/createTempDirectory "vice-analysis-api-"
                                  (make-array FileAttribute 0))))

(defn- with-temp-root [f]
  (let [root (temp-root)]
    (try
      (f root)
      (finally
        (store/delete-tree! root)))))

(deftest open-analysis-exposes-overview-and-handles
  (let [opened (analysis/open-analysis (synthetic/root))
        overview (:overview opened)]
    (is (= "synthetic" (:capture-id overview)))
    (is (pos? (:event-count overview)))
    (is (= 3 (:chunk-count overview)))
    (is (= #{:dedupe :video :assets :audio :features :classification
             :segments :disassembly :report}
           (:available-stages overview)))
    (is (empty? (:missing-stages overview)))
    (doseq [stage [:dedupe :video :assets :audio :features :classification
                   :segments :disassembly :report]]
      (is (map? (get opened stage)) (str "opened stage " stage)))))

(deftest list-and-get-segment-provide-compact-drill-down
  (let [opened (analysis/open-analysis (synthetic/root))
        listed (vice/list-segments opened)
        first-segment (first listed)
        selected (vice/get-segment opened (:segment-id first-segment))]
    (is (= (count listed) (get-in opened [:overview :stage-summaries :segments
                                          :summary :segment-count])))
    (is (= (select-keys first-segment [:segment-id :event-range])
           (select-keys selected [:segment-id :event-range])))
    (is (vector? (get-in selected [:references :raw-capture :event-range])))
    (is (= :capture (get-in selected [:references :raw-capture :handle-key])))
    (is (= "segments/segment-000.asm"
           (get-in (first (filter #(= 0 (:segment-id %)) listed))
                   [:references :disassembly :asm-file])))
    (is (not (contains? selected :events)))
    (is (not (contains? selected :assembly)))
    (is (not (contains? selected :screenshot-bytes)))))

(deftest open-analysis-reports-absent-stages
  (with-temp-root
    (fn [root]
      (synthetic/write-capture! root)
      (let [opened (analysis/open-analysis root)]
        (is (= #{:dedupe :video :assets :audio :features :classification
                 :segments :disassembly :report}
               (get-in opened [:overview :missing-stages])))
        (is (nil? (:segments opened)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"requires committed :segments"
                              (analysis/list-segments opened)))))))

(deftest build-pipeline-wrapper-reuses-committed-stages
  (let [root (synthetic/root)
        result (analysis/build-pipeline! root :segments {:stage-parallelism 1})]
    (is (some #{:segments} (:reused result)))
    (is (map? (get-in result [:images :segments])))))
