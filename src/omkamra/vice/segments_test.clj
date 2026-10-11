(ns omkamra.vice.segments-test
  "Tests for semantic segment assembly and the boundary audit.

  `assemble`/`boundary-audit` are exercised on hand-built classification spans."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.segments :as segments]))

(defn- span
  ([context-kind context-id event-range roles]
   (span context-kind context-id event-range roles
         (if (= :main context-kind) :mainline :irq)))
  ([context-kind context-id event-range roles effective-level]
   {:context-kind context-kind
    :effective-level effective-level
    :context-id context-id
    :frame-id context-id
    :event-range event-range
    :roles roles
    :confidence 0.5
    :signals []}))

(defn- demoparts [assembled]
  (filter #(= :demopart (:kind %)) assembled))

(deftest assemble-demoparts-from-handler-sets
  (testing "a change of display handlers starts a new demopart"
    (let [assembled (segments/assemble
                     [(span :irq 1 [0 400000] #{:raster :music})
                      (span :irq 2 [500000 900000] #{:raster :music})]
                     []
                     1000000)]
      (is (= [[0 500000] [500000 1000000]]
             (mapv :event-range (demoparts assembled))))
      (is (= [#{1} #{2}] (mapv :handler-contexts (demoparts assembled))))))
  (testing "a run with no display handler becomes a loader/decruncher region"
    (let [assembled (segments/assemble
                     [(span :irq 1 [0 250000] #{:raster})
                      (span :main 0 [250000 500000] #{:loader})
                      (span :irq 2 [500000 750000] #{:raster})]
                     []
                     750000)]
      (is (= [:demopart :transition :demopart] (mapv :kind assembled)))
      (is (= 2 (count (segments/group-files assembled))))))
  (testing "a mainline-only region with raster activity is still a demopart"
    (let [assembled (segments/assemble
                     [(span :main 0 [0 250000] #{:raster})]
                     []
                     1000000)]
      (is (= [:demopart] (mapv :kind assembled))))))

(deftest assemble-merges-transition-bridges
  (testing "a nested handler set bridge is one part, not three"
    ;; {1} for 4 buckets, {1 2} for one bucket, {2} for four buckets.
    (let [assembled (segments/assemble
                     [(span :irq 1 [0 1000000] #{:raster})
                      (span :irq 1 [1000000 1250000] #{:raster})
                      (span :irq 2 [1000000 1250000] #{:raster})
                      (span :irq 2 [1250000 2250000] #{:raster})]
                     []
                     2250000)]
      (is (= 1 (count (demoparts assembled))))
      (is (= #{1 2} (:handler-contexts (first (demoparts assembled))))))))

(deftest assemble-covers-the-whole-timeline
  (let [assembled (segments/assemble
                   [(span :irq 1 [0 300000] #{:raster})
                    (span :main 0 [1000000 1500000] #{:decruncher})]
                   []
                   2000000)]
    (testing "segments are ordered, contiguous, and address the full range"
      (is (= 0 (first (:event-range (first assembled)))))
      (is (= 2000000 (second (:event-range (last assembled)))))
      (is (every? (fn [[[_ b] [c _]]] (= b c))
                  (partition 2 1 (map :event-range assembled)))))))

(deftest group-files-closes-after-a-loader-run
  (let [assembled [{:segment-id 0 :kind :demopart :event-range [0 10] :roles #{:raster}}
                   {:segment-id 1 :kind :transition :event-range [10 20]
                    :roles #{:loader :music}}
                   {:segment-id 2 :kind :transition :event-range [20 30]
                    :roles #{:loader :music}}
                   {:segment-id 3 :kind :demopart :event-range [30 40] :roles #{:raster}}
                   {:segment-id 4 :kind :demopart :event-range [40 50] :roles #{:raster}}]]
    (is (= [[0 1 2] [3 4]]
           (mapv :segments (segments/group-files assembled))))))

(deftest boundary-audit
  (let [segments [{:segment-id 0 :kind :demopart :event-range [0 60]
                   :roles #{:raster} :event-count 60}
                  {:segment-id 1 :kind :transition :event-range [60 70]
                   :roles #{:loader :music} :event-count 10}]
        file-groups [{:file-id 0 :segments [0 1] :event-range [0 70]
                      :roles #{:raster :loader}}]
        audit (segments/boundary-audit segments file-groups 70)]
    (testing "coverage reports no gaps and no overlaps"
      (is (empty? (:uncovered audit)))
      (is (empty? (:overlaps audit))))
    (testing "handoff alignment is deliberately not reported"
      (is (not (contains? audit :hard-handoffs)))
      (is (not (contains? audit :unaligned-hard-handoff-count))))
    (testing "role evidence is aggregated"
      (is (pos? (get-in audit [:role-evidence :raster :event-count])))
      (is (pos? (get-in audit [:role-evidence :raster :segment-count]))))))
