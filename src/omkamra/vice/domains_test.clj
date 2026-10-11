(ns omkamra.vice.domains-test
  "Tests for `:video`, `:assets`, and `:audio`.

  A synthetic two-chunk capture with three VIC frames (one blacklisted border
  write per frame and one layout change) and six SID writes verifies structural
  pattern deduplication, blacklist omission, incremental configuration points,
  replayed 64 KiB + 1 KiB asset snapshots, and dedupe-joined SID evidence."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.assets :as assets]
            [omkamra.vice.audio :as audio]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.event :as event]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.store :as store]
            [omkamra.vice.video :as video])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Synthetic capture ---------------------------------------------------------

(defn- zero-vector [n] (vec (repeat n 0)))

(defn- snapshot-at [timing]
  {:cpu {:pc 0x1000 :a 0 :x 0 :y 0 :sp 0xfd :flags 0}
   :timing timing
   :registers {:vic (assoc (assoc (assoc (zero-vector 47) 0x11 0x1b) 0x16 0x00)
                           0x18 0x14)
               :sid (zero-vector 32)
               :cia1 (zero-vector 16)
               :cia2 (assoc (zero-vector 16) 0 0x37)}
   :cpu-port [0x2f 0x37]
   :color-ram (zero-vector 1024)
   :semantics contract/register-semantics})

(defn- event-at [pc bytes a cycle raster]
  {:pc pc :instruction-length (count bytes) :instruction-bytes (vec bytes)
   :a a :x 0 :y 0 :sp 0xfd :flags 0
   :global-cycle cycle :raster-line raster :cpu-cycle (mod cycle 63)})

;; Three frames. `$d020` is in the capture's default value blacklist; frame 3
;; changes `$d018` from `$14` to `$16`, moving the charset base.
(def ^:private synthetic-chunk-1
  [(event-at 0x1000 [0x8d 0x11 0xd0] 0x1b 100 50)
   (event-at 0x1003 [0x8d 0x20 0xd0] 0x00 102 60)
   (event-at 0x1006 [0x8d 0x16 0xd0] 0x00 104 70)
   (event-at 0x1009 [0x8d 0x18 0xd0] 0x14 106 80)
   (event-at 0x100c [0x8d 0x00 0xd4] 0x10 108 85)
   (event-at 0x100f [0x8d 0x01 0xd4] 0x20 110 88)
   (event-at 0x1012 [0x4c 0x00 0x10] 0x20 112 90)
   (event-at 0x1000 [0x8d 0x11 0xd0] 0x1b 200 10)
   (event-at 0x1003 [0x8d 0x20 0xd0] 0x01 202 20)
   (event-at 0x1006 [0x8d 0x16 0xd0] 0x01 204 30)
   (event-at 0x1009 [0x8d 0x18 0xd0] 0x14 206 40)])

(def ^:private synthetic-chunk-2
  [(event-at 0x100c [0x8d 0x00 0xd4] 0x30 208 45)
   (event-at 0x100f [0x8d 0x01 0xd4] 0x40 210 48)
   (event-at 0x1012 [0x4c 0x00 0x10] 0x40 212 50)
   (event-at 0x1000 [0x8d 0x11 0xd0] 0x1b 300 10)
   (event-at 0x1003 [0x8d 0x20 0xd0] 0x02 302 20)
   (event-at 0x1006 [0x8d 0x16 0xd0] 0x02 304 30)
   (event-at 0x1009 [0x8d 0x18 0xd0] 0x16 306 40)
   (event-at 0x100c [0x8d 0x00 0xd4] 0x50 308 45)
   (event-at 0x100f [0x8d 0x01 0xd4] 0x60 310 48)
   (event-at 0x1012 [0x4c 0x00 0x10] 0x60 312 50)])

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
  (str (Files/createTempDirectory "vice-v2-domains-test"
                                  (make-array FileAttribute 0))))

(defn- write-synthetic-capture! [root]
  (let [capture (recorder/start-capture!
                 {:dataset-root root :capture-id "domains-synthetic"})]
    (commit-chunk! capture 1 synthetic-chunk-1)
    (commit-chunk! capture 2 synthetic-chunk-2)
    (recorder/finalize-capture! capture)))

(defn- with-synthetic [f]
  (let [root (temp-dataset-root)]
    (try
      (write-synthetic-capture! root)
      (f (reader/open root) root)
      (finally (store/delete-tree! root)))))

;; Video ---------------------------------------------------------------------

(deftest synthetic-video
  (with-synthetic
    (fn [reader root]
      (video/build! reader)
      (let [v (video/open root)]
        (testing "repeated write shapes intern to one structural pattern"
          (is (= 1 (count (:patterns v))))
          (is (= 3 (count (video/frames v)))))
        (testing "blacklisted values are omitted but the write remains"
          (let [writes (video/vic-writes v)]
            (is (= 12 (count writes)))
            (is (= [0x1b nil 0x00 0x14] (mapv :value (take 4 writes))))
            (is (false? (:value-known? (second writes))))))
        (testing "the initial and layout-change configurations are published"
          (is (= 2 (count (video/configurations v))))
          (is (= -1 (:event-index (first (video/configurations v)))))
          (is (= 17 (:event-index (second (video/configurations v)))))
          (is (= 0x1000 (get-in (first (video/configurations v)) [:config :charset-base])))
          (is (= 0x1800 (get-in (second (video/configurations v)) [:config :charset-base]))))))))

;; Assets --------------------------------------------------------------------

(deftest synthetic-assets
  (with-synthetic
    (fn [reader root]
      (video/build! reader)
      (assets/build! reader (video/open root))
      (let [a (assets/open root)
            snapshots (assets/snapshots a)]
        (testing "one snapshot per effective layout point"
          (is (= 2 (count snapshots))))
        (testing "each payload is 64 KiB RAM plus 1 KiB color RAM"
          (is (= assets/payload-size (alength (assets/read-payload a 0))))
          (is (= contract/memory-image-size (alength (assets/snapshot-ram a 0))))
          (is (= contract/color-ram-size (alength (assets/snapshot-color-ram a 0))))
          (is (= [0 assets/payload-size]
                 (mapv (comp :offset :payload) snapshots))))
        (testing "the index records the asset memory ranges"
          (let [snapshot (first snapshots)]
            (is (= 0x400 (get-in snapshot [:ranges :screen :address])))
            (is (= 0x1000 (get-in snapshot [:ranges :charset :address])))
            (is (= 0xd800 (get-in snapshot [:ranges :color-ram :address])))
            (is (= 0x7f8 (get-in snapshot [:ranges :sprite-pointers :address])))
            (is (= 8 (count (:sprites snapshot))))))))))

;; Audio ---------------------------------------------------------------------

(deftest synthetic-audio
  (with-synthetic
    (fn [reader root]
      (dedupe/build! reader {:window-events 5})
      (audio/build! reader (dedupe/open root)
                    {:configuration {:min-sid-writes-per-unit 2
                                     :min-sustained-units 2
                                     :candidate-min-writes 4}})
      (let [a (audio/open root)
            evidence (audio/evidence a)]
        (testing "SID writes are counted and joined with dedupe blocks"
          (is (= 6 (get-in evidence [:summary :sid-write-count])))
          (is (= {0 6} (:by-context evidence)))
          (is (= {0 6} (:by-block evidence)))
          (is (= {0 3 1 3} (:by-register evidence))))
        (testing "sustained SID activity produces music-role evidence"
          (is (= 1 (count (:sustained-runs evidence))))
          (is (<= 2 (get-in evidence [:summary :sustained-unit-count])))
          (is (pos? (get-in evidence [:summary :candidate-count])))
          (is (some #(= :block (:kind %)) (audio/music-candidates a))))))))
