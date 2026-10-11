(ns omkamra.vice.dedupe-test
  "Tests for the `:dedupe` structural image.

  A synthetic multi-chunk capture with a hardware interrupt verifies
  instruction identity and operand volatility, global block deduplication,
  mainline and IRQ contexts, context-qualified CFG edges, suspend/resume
  fragments that keep an interrupted mainline block whole, timing/port/switch
  channels, and window-independent range reads."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.event :as event]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Synthetic capture ---------------------------------------------------------

(defn- zero-vector [n] (vec (repeat n 0)))

(defn- snapshot-at [timing]
  {:cpu {:pc 0x1000 :a 0 :x 0 :y 0 :sp 0xfd :flags 0}
   :timing timing
   :registers {:vic (zero-vector 47) :sid (zero-vector 32)
               :cia1 (zero-vector 16) :cia2 (zero-vector 16)}
   :cpu-port [0x2f 0x37]
   :color-ram (zero-vector 1024)
   :semantics contract/register-semantics})

(defn- event-at [pc bytes a x y sp flags cycle]
  {:pc pc :instruction-length (count bytes) :instruction-bytes (vec bytes)
   :a a :x x :y y :sp sp :flags flags
   :global-cycle cycle :raster-line 10 :cpu-cycle (mod cycle 63)})

;; A mainline loop interrupted by one IRQ. Event 2 is the KERNAL handler entry
;; ($2000) reached through an unexplained three-byte stack drop; event 4 is the
;; RTI that resumes the suspended mainline block at event 5.
(def ^:private synthetic-chunk-1
  [(event-at 0x1000 [0xa9 0x01] 0x00 0 0 0xfd 0 100)
   (event-at 0x1002 [0x8d 0x00 0x02] 0x01 0 0 0xfd 0 102)
   (event-at 0x2000 [0xa9 0x00] 0x01 0 0 0xfa 0x04 104)
   (event-at 0x2002 [0x85 0x01] 0x00 0 0 0xfa 0x04 106)
   (event-at 0x2004 [0x40] 0x00 0 0 0xfa 0x04 108)
   (event-at 0x1004 [0xe8] 0x01 0 0 0xfd 0 110)])

(def ^:private synthetic-chunk-2
  [(event-at 0x1005 [0xd0 0xf9] 0x01 1 0 0xfd 0 112)
   (event-at 0x1000 [0xa9 0x02] 0x02 1 0 0xfd 0 114)
   (event-at 0x1002 [0x8d 0x00 0x02] 0x02 1 0 0xfd 0 116)
   (event-at 0x1004 [0xe8] 0x02 1 0 0xfd 0 118)
   (event-at 0x1005 [0xd0 0xf9] 0x02 2 0 0xfd 0 120)])

(defn- commit-chunk! [capture number events]
  (recorder/commit-chunk!
   capture
   {:event-count (count events)
    :event-writer (fn [out] (event/write-events! out events))
    :ram (byte-array contract/memory-image-size)
    :mem (byte-array contract/memory-image-size)
    :snapshot (snapshot-at {:global-cycle (* 100 number)
                            :raster-line 10 :cpu-cycle 37})
    :snapshot-timing {:global-cycle (* 100 number)
                      :raster-line 10 :cpu-cycle 37}}))

(defn- temp-dataset-root []
  (str (Files/createTempDirectory "vice-v2-dedupe-test"
                                  (make-array FileAttribute 0))))

(defn- write-synthetic-capture! [root]
  (let [capture (recorder/start-capture!
                 {:dataset-root root :capture-id "dedupe-synthetic"})]
    (commit-chunk! capture 1 synthetic-chunk-1)
    (commit-chunk! capture 2 synthetic-chunk-2)
    (recorder/finalize-capture! capture)))

(defn- with-synthetic-trace
  ([f] (with-synthetic-trace {} f))
  ([build-options f]
   (let [root (temp-dataset-root)]
     (try
       (write-synthetic-capture! root)
       (let [capture (reader/open root)]
         (dedupe/build! capture (merge {:window-events 4} build-options))
         (f (dedupe/open root)))
       (finally (store/delete-tree! root))))))

(defn- occurrence-shape [occurrence]
  [(:occurrence-id occurrence) (:context-id occurrence) (:block-id occurrence)
   (:fragments occurrence)])

;; Tests ---------------------------------------------------------------------

(deftest synthetic-structure
  (with-synthetic-trace
    (fn [trace]
      (testing "dictionaries are globally deduplicated"
        (is (= 2 (get-in trace [:index :counts :blocks])))
        (is (= 7 (get-in trace [:index :counts :instructions]))))
      (testing "the handler block and the mainline block are both interned"
        (let [handler (dedupe/read-block trace 0)
              mainline (dedupe/read-block trace 1)]
          (is (= [0x2000 0x2002 0x2004] (mapv :pc (:instructions handler))))
          (is (= [0x1000 0x1002 0x1004 0x1005]
                 (mapv :pc (:instructions mainline))))))
      (testing "interrupt entry and return create a second context"
        (is (= 2 (get-in trace [:index :counts :contexts])))
        (is (= {:kind :main :handler-pc nil} (select-keys (dedupe/read-context trace 0)
                                                          [:kind :handler-pc])))
        (is (= {:kind :irq :handler-pc 0x2000}
               (select-keys (dedupe/read-context trace 1) [:kind :handler-pc]))))
      (testing "the context-qualified CFG records the mainline self-loop"
        (is (some #(= {:from 1 :to 1 :count 1} %)
                  (:edges (dedupe/read-context trace 0))))
        (is (empty? (:edges (dedupe/read-context trace 1)))))
      (testing "an interrupt does not split the interrupted mainline block"
        (let [range (dedupe/read-range trace 0 11)
              shapes (mapv occurrence-shape (:occurrences range))]
         ;; One handler occurrence (block 0), then the interrupted mainline
         ;; occurrence (block 1) with two fragments, then the next iteration of
         ;; the same mainline block.
          (is (= [[1 1 0 [[2 5]]]
                  [0 0 1 [[0 2] [5 7]]]
                  [2 0 1 [[7 11]]]]
                 shapes))))
      (testing "context switches and the CPU-port timeline are explicit"
        (let [range (dedupe/read-range trace 0 11)]
          (is (= [{:event-index 2 :kind :irq :frame-id 1 :context-id 1}
                  {:event-index 4 :kind :return :frame-id 1 :context-id 1}]
                 (:context-switches range)))
          (is (= [{:event-index 3 :value 0}] (:port-timeline range)))))
      (testing "timing is reconstructed per occurrence"
        (is (= [[104 108] [100 112] [114 120]]
               (mapv (juxt :start-cycle :end-cycle)
                     (:occurrences (dedupe/read-range trace 0 11)))))))))

(deftest synthetic-volatility-and-windows
  (with-synthetic-trace
    (fn [trace]
      (testing "operand volatility marks the identity without duplicating it"
        (let [mainline (dedupe/read-block trace 1)
              lda (first (:instructions mainline))]
          (is (= 0x1000 (:pc lda)))
          (is (= [0x01 0x00] (:template lda)))
          (is (= 1 (:volatile-mask lda))))
        (is (= 1 (count (filter #(pos? (:volatile-mask %)) (:instructions trace))))))
      (testing "logical windows are independent of raw capture chunks"
        (let [whole (dedupe/read-range trace 0 11)
              split (concat (:occurrences (dedupe/read-range trace 0 7))
                            (:occurrences (dedupe/read-range trace 7 11)))]
          (is (= (mapv occurrence-shape (:occurrences whole))
                 (mapv occurrence-shape split))))))))

(deftest synthetic-transactional-failure
  (let [root (temp-dataset-root)]
    (try
      (write-synthetic-capture! root)
      (let [capture (reader/open root)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (dedupe/build! capture {:window-events 0})))
        (is (nil? (dedupe/status root))))
      (finally (store/delete-tree! root)))))

(deftest synthetic-frame-leveling
  (testing "a long-lived frame is leveled at an exact occurrence boundary"
    (with-synthetic-trace {:configuration {:leveling-threshold-events 1}}
      (fn [trace]
        (let [range (dedupe/read-range trace 0 11)
              leveling (first (:levelings trace))
              occurrences (:occurrences range)]
          (is (= 1 (count (:levelings trace))))
          (is (= {:frame-id 1 :context-id 1 :observed-kind :irq
                  :event-range [2 4] :level-start 3
                  :effective-level :mainline :reason :long-lived-frame
                  :threshold-events 1}
                 leveling))
          (testing "the IRQ prefix stays at level :irq"
            (is (= :irq (:effective-level
                         (first (filter #(= 1 (:context-id %)) occurrences))))))
          (testing "the continuation is placed on the mainline timeline"
            (is (= [:irq :mainline :mainline :mainline]
                   (mapv :effective-level occurrences))))
          (testing "leveled occurrences keep their observed IRQ context"
            (is (= 1 (:context-id (second occurrences))))
            (is (= :irq (:context-kind (second occurrences)))))
          (testing "the leveling annotation is exposed on the range"
            (is (= [leveling] (:levelings range)))))))))

(deftest synthetic-unleveled-frame-is-unchanged
  (with-synthetic-trace
    (fn [trace]
      (is (empty? (:levelings trace)))
      (is (every? #(not= :mainline (:effective-level %))
                  (filter #(= 1 (:context-id %))
                          (:occurrences (dedupe/read-range trace 0 11))))))))
