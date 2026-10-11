(ns omkamra.vice.features-classification-test
  "Feature-evidence and classification-span tests.

  A synthetic capture with a VIC/SID mainline loop and one mixed IRQ handler
  verifies unit evidence and end-to-end spans. `classification/classify` is
  additionally exercised on hand-built units for hysteresis, role-set changes,
  and IRQ block-occurrence splitting."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.classification :as classification]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.event :as event]
            [omkamra.vice.features :as features]
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

(defn- event-at [pc bytes a sp cycle raster]
  {:pc pc :instruction-length (count bytes) :instruction-bytes (vec bytes)
   :a a :x 0 :y 0 :sp sp :flags 0
   :global-cycle cycle :raster-line raster :cpu-cycle (mod cycle 63)})

(def ^:private mainline-1
  [(event-at 0x1000 [0x8d 0x11 0xd0] 0x1b 0xfd 100 50)
   (event-at 0x1003 [0x8d 0x16 0xd0] 0x00 0xfd 102 55)
   (event-at 0x1006 [0x8d 0x18 0xd0] 0x14 0xfd 104 60)
   (event-at 0x1009 [0x8d 0x00 0xd4] 0x10 0xfd 106 65)
   (event-at 0x100c [0x8d 0x01 0xd4] 0x20 0xfd 108 70)
   (event-at 0x100f [0x4c 0x00 0x10] 0x20 0xfd 110 75)])

(def ^:private irq-handler
  [(event-at 0x3000 [0x8d 0x11 0xd0] 0x0a 0xfa 114 80)
   (event-at 0x3003 [0x8d 0x00 0xd4] 0x30 0xfa 116 85)
   (event-at 0x3006 [0x40] 0x30 0xfa 118 90)])

(def ^:private mainline-2
  [(event-at 0x1000 [0x8d 0x11 0xd0] 0x1b 0xfd 122 10)
   (event-at 0x1003 [0x8d 0x16 0xd0] 0x00 0xfd 124 15)
   (event-at 0x1006 [0x8d 0x18 0xd0] 0x14 0xfd 126 20)
   (event-at 0x1009 [0x8d 0x00 0xd4] 0x10 0xfd 128 25)
   (event-at 0x100c [0x8d 0x01 0xd4] 0x20 0xfd 130 30)
   (event-at 0x100f [0x4c 0x00 0x10] 0x20 0xfd 132 35)])

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
  (str (Files/createTempDirectory "vice-v2-features-classification-test"
                                  (make-array FileAttribute 0))))

(defn- write-synthetic-capture! [root]
  (let [capture (recorder/start-capture!
                 {:dataset-root root :capture-id "features-classification-synthetic"})]
    (commit-chunk! capture 1 (vec (concat mainline-1 irq-handler mainline-2)))
    (recorder/finalize-capture! capture)))

(defn- with-synthetic [f]
  (let [root (temp-dataset-root)]
    (try
      (write-synthetic-capture! root)
      (f (reader/open root) root)
      (finally (store/delete-tree! root)))))

;; Feature evidence ----------------------------------------------------------

(deftest synthetic-features
  (with-synthetic
    (fn [reader root]
      (dedupe/build! reader {:window-events 1000})
      (features/build! reader (dedupe/open root)
                       {:configuration {:window-events 6
                                        :destination-min-address 512}})
      (let [f (features/open root)
            units (features/units f)
            mainline (filter #(= :mainline-window (:kind %)) units)
            irq (filter #(= :irq-invocation (:kind %)) units)]
        (testing "mainline windows count only mainline events"
          (is (<= 2 (count mainline)))
          (is (every? #(= 0 (:context-id %)) mainline))
          (is (every? #(<= 6 (get-in % [:evidence :event-count])) mainline)))
        (testing "the IRQ handler keeps compact role-relevant occurrence evidence"
          (is (= 1 (count irq)))
          (is (= 1 (:context-id (first irq))))
          (is (sequential? (:occurrences (first irq))))
          (is (every? #(not (contains? (:evidence %) :domain-counts))
                      (:occurrences (first irq))))
          (is (every? #(not (contains? (:evidence %) :destination-page-words))
                      (:occurrences (first irq)))))
        (testing "VIC and SID writes are attributed to the units"
          (let [evidence (get-in (first mainline) [:evidence])]
            (is (= 3 (:vic-write-count evidence)))
            (is (= 2 (:sid-write-count evidence)))
            (is (pos? (:write-total evidence)))))
        (testing "an execution after a code write is a hard handoff"
          (is (vector? (features/handoffs f))))))))

(deftest features-parallel-determinism
  (let [run (fn [parallelism]
              (let [root (temp-dataset-root)]
                (try
                  (write-synthetic-capture! root)
                  (let [r (reader/open root)]
                    (dedupe/build! r {:window-events 1000})
                    (features/build! r (dedupe/open root)
                                     {:configuration {:window-events 6
                                                      :destination-min-address 512
                                                      :parallelism parallelism}})
                    (features/units (features/open root)))
                  (finally (store/delete-tree! root)))))]
    (is (= (run 1) (run 4)))))

;; Classification units ------------------------------------------------------

(defn- evidence [values]
  (merge {:event-count 10 :write-total 0 :domain-counts {}
          :vic-write-count 0 :sid-write-count 0 :ram-write-count 0
          :serial-write-count 0 :iec-write-count 0
          :code-overwrite-count 0 :kernal-iec-execution-count 0
          :destination-unique-address-count 0 :destination-page-count 0}
         values))

(defn- unit [kind context-id event-range evidence-values]
  {:kind kind :context-id context-id :frame-id context-id
   :context-kind (if (= :mainline-window kind) :main :irq)
   :effective-level (if (= :mainline-window kind) :mainline :irq)
   :event-range event-range
   :evidence (evidence evidence-values)})

(def ^:private role-config
  {:raster-write-threshold 1
   :music-write-threshold 1
   :loader-serial-threshold 1
   :decruncher-min-ram-writes 1
   :decruncher-min-unique-addresses 1
   :decruncher-min-page-count 1
   :idle-max-writes 0
   :enter-units 1
   :exit-units 1})

(deftest audio-candidates-require-a-matching-id
  (testing "a missing :block-id must not match an audio candidate whose block is nil"
    (let [mainline (unit :mainline-window 0 [0 10] {})]
      (is (not (contains? (classification/unit-roles
                           mainline classification/default-configuration
                           #{nil 5} #{})
                          :music)))
      (is (contains? (classification/unit-roles
                      mainline classification/default-configuration
                      #{} #{0})
                     :music))
      (is (not (contains? (classification/unit-roles
                           mainline classification/default-configuration
                           #{} #{nil})
                          :music))))))

(deftest classification-roles-and-hysteresis
  (testing "isolated serial/KERNAL noise does not classify a loader"
    (is (not (contains?
              (classification/unit-roles
               (unit :mainline-window 0 [0 10]
                     {:serial-read-count 1 :kernal-iec-execution-count 4})
               classification/default-configuration)
              :loader)))
    (is (contains?
         (classification/unit-roles
          (unit :mainline-window 0 [0 10] {:serial-read-count 32})
          classification/default-configuration)
         :loader)))
  (testing "roles come from the hardware write mix"
    (let [units [(unit :mainline-window 0 [0 10] {:vic-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [10 20] {:sid-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [20 30] {:ram-write-count 500
                                                   :destination-unique-address-count 3000
                                                   :destination-page-count 20
                                                   :write-total 500})]
          spans (classification/classify units role-config)]
      (is (= [#{:raster} #{:music} #{:decruncher}]
             (mapv :roles spans)))))
  (testing "hysteresis delays role entry until it is sustained"
    (let [units [(unit :mainline-window 0 [0 10] {:vic-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [10 20] {:vic-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [20 30] {:sid-write-count 5 :write-total 5})]
          with-enter-2 (classification/classify units (assoc role-config :enter-units 2))]
      (is (= 1 (count with-enter-2)))
      (is (= :raster (first (:roles (first with-enter-2)))))
      (is (= [10 20] (:event-range (first with-enter-2)))))))

(deftest classification-span-merging-and-irq-splitting
  (testing "mainline spans merge while the stable role set is unchanged"
    (let [units [(unit :mainline-window 0 [0 10] {:vic-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [10 20] {:vic-write-count 5 :write-total 5})]
          spans (classification/classify units role-config)]
      (is (= [[0 20]] (mapv :event-range spans)))
      (is (= [:main] (mapv :context-kind spans)))
      (is (= [:mainline] (mapv :effective-level spans)))))
  (testing "a change of the stable role set starts a new mainline span"
    (let [units [(unit :mainline-window 0 [0 10] {:vic-write-count 5 :write-total 5})
                 (unit :mainline-window 0 [10 20] {:sid-write-count 5 :write-total 5})]
          spans (classification/classify units role-config)]
      (is (= [[0 10] [10 20]] (mapv :event-range spans)))
      (is (= [#{:raster} #{:music}] (mapv :roles spans)))))
  (testing "IRQ occurrences split at block-occurrence role boundaries"
    (let [invocation {:kind :irq-invocation :context-id 1 :event-range [0 8]
                      :evidence (evidence {:vic-write-count 5 :sid-write-count 5
                                           :write-total 10})
                      :occurrences [{:event-range [0 2] :evidence (evidence {:vic-write-count 5 :write-total 5})}
                                    {:event-range [2 4] :evidence (evidence {:vic-write-count 5 :write-total 5})}
                                    {:event-range [4 6] :evidence (evidence {:sid-write-count 5 :write-total 5})}
                                    {:event-range [6 8] :evidence (evidence {:sid-write-count 5 :write-total 5})}]}
          spans (classification/classify [invocation] role-config)]
      (is (= [[0 4] [4 8]] (mapv :event-range spans)))
      (is (= [#{:raster} #{:music}] (mapv :roles spans))))))

;; End to end ----------------------------------------------------------------

(deftest synthetic-features-classification
  (with-synthetic
    (fn [reader root]
      (dedupe/build! reader {:window-events 1000})
      (video/build! reader)
      (features/build! reader (dedupe/open root)
                       {:configuration {:window-events 6
                                        :destination-min-address 512}})
      (classification/build! (features/open root) nil
                             {:configuration {:raster-write-threshold 1
                                              :music-write-threshold 1
                                              :enter-units 1 :exit-units 1}})
      (let [c (classification/open root)]
        (is (pos? (get-in c [:index :summary :span-count])))
        (is (contains? (get-in c [:index :summary :role-counts]) :raster))
        (is (contains? (get-in c [:index :summary :role-counts]) :music))
        (is (pos? (get-in c [:index :summary :irq-span-count])))))))

(deftest binary-occurrence-channel-round-trips
  (let [encode (var-get (ns-resolve 'omkamra.vice.features 'encode-occurrences))
        decode (var-get (ns-resolve 'omkamra.vice.features 'decode-occurrences))
        occurrences [{:occurrence-id 5 :block-id 10 :event-range [100 110]
                      :evidence {:vic-write-count 3 :write-total 4}}
                     {:occurrence-id 6 :block-id 10 :event-range [120 130]
                      :evidence {:vic-write-count 3 :write-total 4}}
                     {:occurrence-id 9 :block-id 4 :event-range [140 200]
                      :evidence {:sid-write-count 2 :write-total 2}}]]
    (is (= occurrences (decode (encode occurrences) 0)))))
