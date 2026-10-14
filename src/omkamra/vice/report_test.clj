(ns omkamra.vice.report-test
  "Tests for the `:report` projection: screenshot selection, relative links,
  escaping, and the committed generation."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.report :as report]
            [omkamra.vice.synthetic-test-support :as synthetic]))

;; Screenshot selection ------------------------------------------------------

(deftest target-indices-sample-the-interior-of-a-range
  (testing "three screenshots target 25%, 50% and 75% of the span"
    (is (= [25 50 75] (report/target-indices [0 100] 3))))
  (testing "the interior rule generalizes to any count"
    (is (= [50] (report/target-indices [0 100] 1)))
    (is (= [33 66] (report/target-indices [0 100] 2)))
    (is (= [20 40 60 80] (report/target-indices [0 100] 4)))
    (is (= [] (report/target-indices [0 100] 0))))
  (testing "targets are relative to the range start"
    (is (= [300 400 500] (report/target-indices [200 600] 3)))))

(def ^:private boundaries
  [{:chunk-number 1 :event-index 0}
   {:chunk-number 2 :event-index 100}
   {:chunk-number 3 :event-index 200}
   {:chunk-number 4 :event-index 300}
   {:chunk-number 5 :event-index 400}])

(deftest screenshots-are-nearest-to-the-target-percentages
  (testing "targets 25%, 50% and 75% pick the nearest boundaries"
    (is (= [{:chunk-number 2 :event-index 100 :target 0.25 :fallback? false}
            {:chunk-number 3 :event-index 200 :target 0.5 :fallback? false}
            {:chunk-number 4 :event-index 300 :target 0.75 :fallback? false}]
           (report/choose-boundaries boundaries [0 400] 3))))
  (testing "a target is relative to its own segment range"
    (is (= [{:chunk-number 2 :event-index 100 :target 0.5 :fallback? false}]
           (report/choose-boundaries boundaries [100 200] 1))))
  (testing "a target closer to a boundary outside the range keeps that frame"
    (is (= [{:chunk-number 2 :event-index 100 :target 0.5 :fallback? true}]
           (report/choose-boundaries boundaries [50 60] 1))))
  (testing "a range shorter than the chunk granularity is marked as such"
    (is (= [{:chunk-number 3 :event-index 200 :fallback? true}]
           (mapv #(dissoc % :target)
                 (report/choose-boundaries boundaries [150 180] 3)))))
  (testing "no screenshots requested or none available"
    (is (= [] (report/choose-boundaries boundaries [0 400] 0)))
    (is (= [] (report/choose-boundaries [] [0 400] 3)))))

(deftest duplicate-boundary-choices-are-dropped
  (testing "a short segment shows one frame rather than three copies"
    (is (= [{:chunk-number 2 :event-index 100 :target 0.25 :fallback? false}]
           (report/choose-boundaries boundaries [80 120] 3)))))

;; Rendering -----------------------------------------------------------------

(def ^:private index
  {:capture-id "synthetic"
   :capture-manifest-sha256 "deadbeefdeadbeefdeadbeefdeadbeef"
   :generation "1700000000000-abcdef12"
   :created-at "2026-10-11T00:00:00Z"
   :event-count 1000
   :timing-model {:clock-hz 985248 :cycles-per-line 63 :raster-lines 312}
   :upstream {:segments "analysis/segments/1700000000000-aaaaaaaa"
              :disassembly "analysis/disassembly/1700000000000-bbbbbbbb"}
   :disassembly-generation "1700000000000-bbbbbbbb"
   :summary {:segment-count 2 :screenshot-count 2}
   :audit {:uncovered [] :overlaps []}
   :role-files [{:role :loader :block-count 4 :file "roles/loader.asm"}]
   :segments [{:segment-id 0
               :kind :demopart
               :roles #{:raster}
               :event-range [0 600]
               :event-count 600
               :confidence 0.5
               :phases [{:role :raster :event-range [0 600]}]
               :cycles {:start 0 :end 6000 :duration 6000}
               :handler-contexts #{1}
               :context-ids #{2}
               :signals []
               :asm-file "segments/segment-000.asm"
               :disassembly-generation "1700000000000-bbbbbbbb"
               :screenshots [{:chunk-number 1
                              :event-index 0
                              :timing {:global-cycle 0 :raster-line 3}
                              :fallback? false
                              :file "../../../capture/chunks/chunk-000001.png"}
                             {:chunk-number 2
                              :event-index 500
                              :timing {:global-cycle 5000 :raster-line 7}
                              :fallback? true
                              :file "../../../capture/chunks/chunk-000002.png"}]}
              {:segment-id 1
               :kind :transition
               :roles #{}
               :event-range [600 1000]
               :event-count 400
               :confidence 0.25
               :phases []
               :cycles {:start 6000 :end 9000 :duration 3000}
               :handler-contexts #{}
               :context-ids #{}
               :signals ["loop"]
               :asm-file nil
               :disassembly-generation "1700000000000-bbbbbbbb"
               :screenshots []}]})

(deftest rendered-html-is-self-contained-and-linked
  (let [html (report/render-html index)]
    (testing "one page with embedded styling and no scripts"
      (is (str/starts-with? html "<!doctype html>"))
      (is (str/includes? html "<style>"))
      (is (not (str/includes? html "<script")))
      (is (not (str/includes? html "<link"))))
    (testing "the capture identity is shown"
      (is (str/includes? html "synthetic"))
      (is (str/includes? html "deadbeefdeadbeef")))
    (testing "every segment gets an anchored section and a summary row"
      (is (str/includes? html "id=\"segment-000\""))
      (is (str/includes? html "id=\"segment-001\""))
      (is (str/includes? html "id=\"segments\""))
      (is (str/includes? html "href=\"#segment-000\""))
      (is (str/includes? html "demopart")))
    (testing "screenshots are relative links into the capture"
      (is (str/includes? html "src=\"../../../capture/chunks/chunk-000001.png\""))
      (is (str/includes? html "href=\"../../../capture/chunks/chunk-000002.png\"")))
    (testing "the disassembly file and role files are relative links"
      (is (str/includes?
           html
           "href=\"../../disassembly/1700000000000-bbbbbbbb/segments/segment-000.asm\""))
      (is (str/includes?
           html
           "href=\"../../disassembly/1700000000000-bbbbbbbb/roles/loader.asm\"")))
    (testing "a fallback screenshot and a missing screenshot are called out"
      (is (str/includes? html "boundary outside the segment"))
      (is (str/includes? html "no committed chunk screenshot")))
    (testing "durations are rendered for a known timing model"
      (is (str/includes? html "0.01 s")))))

(deftest rendered-html-escapes-metadata
  (let [html (report/render-html
              (assoc-in index [:segments 1 :signals]
                        ["<script>alert(1)</script>"]))]
    (is (not (str/includes? html "<script>alert")))
    (is (str/includes? html "&lt;script&gt;"))))

;; Committed generation ------------------------------------------------------

(deftest the-report-stage-commits-a-generation
  (let [fixture (synthetic/fixture)
        root (:root fixture)
        image (:report fixture)
        index (:index image)]
    (is (= report/report-format (:format index)))
    (is (= report/report-version (:version index)))
    (is (= (:configuration index) report/default-configuration))
    (testing "every segment is reported, including its disassembly file"
      (is (= (get-in image [:index :summary :segment-count])
             (count (report/segments image))))
      (is (every? :event-range (report/segments image)))
      (is (every? :cycles (report/segments image))))
    (testing "the synthetic capture has no screenshots, and the report says so"
      (is (zero? (get-in index [:summary :screenshot-count])))
      (is (every? empty? (map report/screenshots (report/segments image)))))
    (testing "the page is committed with a size and checksum"
      (is (= "report.html" (get-in index [:files 0 :file])))
      (is (pos? (get-in index [:files 0 :size])))
      (is (= 64 (count (get-in index [:files 0 :sha256]))))
      (is (.isFile (report/html-file image))))
    (testing "the analysis manifest points at the committed generation"
      (is (= report/report-version (:version (report/status root)))))
    (testing "reopening the generation validates the format"
      (is (= report/report-format
             (get-in (report/open! root) [:index :format]))))))
