(ns omkamra.vice.reader-test
  "Tests for the 6510 instruction vocabulary, the shared capture reader,
  logical write inference, and exact cross-chunk replay continuity.

  Every test builds a small synthetic capture, so write inference is checked
  against known expected mutations with no dependency on a real capture."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.isa :as isa]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; ISA -----------------------------------------------------------------------

(deftest isa-vocabulary
  (testing "the full 6510 opcode table is present"
    (is (= 256 (count isa/opcode-table))))
  (testing "decode returns the instruction bytes used for operand resolution"
    (is (= {:pc 0x1000 :opcode 0x9d :bytes [0x9d 0xa8 0xd8]
            :mnemonic "STA" :mode :absx :width 3}
           (isa/decode 0x1000 [0x9d 0xa8 0xd8]))))
  (testing "instruction effects are classified"
    (is (= :store (isa/category "STA")))
    (is (= :rmw (isa/category "INC")))
    (is (= :push (isa/category "PHP")))
    (is (= :pull (isa/category "PLA")))
    (is (= :jsr (isa/category "JSR")))
    (is (= :brk (isa/category "BRK")))
    (is (= :rts (isa/category "RTS")))
    (is (= :rti (isa/category "RTI")))
    (is (= :jmp (isa/category "JMP")))
    (is (= :branch (isa/category "BNE")))
    (is (= :other (isa/category "LDA")))))

(deftest isa-values-and-stack-effects
  (testing "store values including unstable high-byte masking"
    (is (= 0x42 (isa/store-value "STA" {:a 0x42} 0x2000)))
    (is (= 0x03 (isa/store-value "SAX" {:a 0x0f :x 0x03} 0x2000)))
    (is (= 0x21 (isa/store-value "AHX" {:a 0xff :x 0xff} 0x20ff))))
  (testing "read-modify-write final values"
    (is (= 0x00 (isa/rmw-value "INC" 0xff false)))
    (is (= 0x02 (isa/rmw-value "ASL" 0x01 false)))
    (is (= 0x81 (isa/rmw-value "ROR" 0x03 true))))
  (testing "branch decisions use the instruction's own flags"
    (is (isa/branch-taken? "BNE" {:flags 0x00}))
    (is (not (isa/branch-taken? "BNE" {:flags 0x02})))
    (is (isa/branch-taken? "BEQ" {:flags 0x02}))
    (is (isa/branch-taken? "BCS" {:flags 0x01})))
  (testing "stack pointer effects"
    (is (= 0xfd (isa/sp-after "PHA" 0xfe 0)))
    (is (= 0xfd (isa/sp-after "JSR" 0xff 0)))
    (is (= 0xfc (isa/sp-after "BRK" 0xff 0)))
    (is (= 0x01 (isa/sp-after "RTS" 0xff 0)))
    (is (= 0x42 (isa/sp-after "TXS" 0xff 0x42)))))

(deftest isa-operand-resolution
  (let [read (fn [address] (case address 0x10 0x34 0x11 0x12 0x20 0x56 0x21 0x78 0))]
    (testing "indexed and absolute modes"
      (is (= 0x0200 (isa/resolve-address read {:mode :abs :bytes [0xad 0x00 0x02]} {})))
      (is (= 0x0203 (isa/resolve-address read {:mode :absx :bytes [0xbd 0x00 0x02]} {:x 3})))
      (is (= 0x02ff (isa/resolve-address read {:mode :absy :bytes [0xb9 0x00 0x02]} {:y 0xff}))))
    (testing "zero-page indirect modes read the replay memory"
      (is (= 0x1234 (isa/resolve-address read {:mode :indx :bytes [0xa1 0x10]} {:x 0})))
      (is (= 0x1235 (isa/resolve-address read {:mode :indy :bytes [0xb1 0x10]} {:y 1}))))
    (testing "JMP indirect wraps within the pointer page"
      (is (= 0x1234 (isa/jmp-indirect-target read [0x6c 0x10 0x00]))))))

;; Synthetic capture ---------------------------------------------------------

(def ^:private ram-size contract/memory-image-size)

(defn- zero-vector [n] (vec (repeat n 0)))

(defn- snapshot-at [timing]
  {:cpu {:pc 0x1000 :a 0 :x 0 :y 0 :sp 0xfd :flags 0}
   :timing timing
   :registers {:vic (zero-vector 47) :sid (zero-vector 32)
               :cia1 (zero-vector 16) :cia2 (zero-vector 16)}
   :cpu-port [0x2f 0x37]
   :color-ram (zero-vector 1024)
   :semantics contract/register-semantics})

(defn- image-with-vector []
  (let [image (byte-array ram-size)]
    (aset-byte image 0xfffe (unchecked-byte 0x48))
    (aset-byte image 0xffff (unchecked-byte 0xff))
    image))

(defn- event-at
  [pc bytes a x y sp flags cycle]
  {:pc pc
   :instruction-length (count bytes)
   :instruction-bytes (vec bytes)
   :a a :x x :y y :sp sp :flags flags
   :global-cycle cycle :raster-line 10 :cpu-cycle (mod cycle 63)})

(def ^:private synthetic-chunk-1
  [(event-at 0x1000 [0xa9 0x42] 0x00 0 0 0xfd 0x00 100)
   (event-at 0x1002 [0x8d 0x00 0x02] 0x42 0 0 0xfd 0x00 102)
   (event-at 0x1005 [0xa2 0x01] 0x42 0x00 0 0xfd 0x00 104)
   (event-at 0x1007 [0xee 0x00 0x02] 0x42 0x01 0 0xfd 0x00 106)
   (event-at 0x100a [0x48] 0x42 0x01 0 0xfd 0x00 108)])

(def ^:private synthetic-chunk-2
  [(event-at 0x100b [0x08] 0x42 0x01 0 0xfc 0x24 110)
   (event-at 0x100c [0x20 0x00 0x11] 0x42 0x01 0 0xfb 0x24 112)
   (event-at 0x1100 [0x60] 0x42 0x01 0 0xf9 0x24 114)
   (event-at 0xff48 [0x48] 0x42 0x01 0 0xf8 0x24 116)])

(defn- write-synthetic-capture!
  [dataset-root]
  (let [capture (recorder/start-capture!
                 {:dataset-root dataset-root :capture-id "synthetic"})]
    (recorder/commit-chunk!
     capture
     {:event-count (count synthetic-chunk-1)
      :event-writer (fn [out] (event/write-events! out synthetic-chunk-1))
      :ram (image-with-vector)
      :mem (image-with-vector)
      :snapshot (snapshot-at {:global-cycle 100 :raster-line 10 :cpu-cycle 37})
      :snapshot-timing {:global-cycle 100 :raster-line 10 :cpu-cycle 37}})
    (recorder/commit-chunk!
     capture
     {:event-count (count synthetic-chunk-2)
      :event-writer (fn [out] (event/write-events! out synthetic-chunk-2))
      :ram (image-with-vector)
      :mem (image-with-vector)
      :snapshot (snapshot-at {:global-cycle 110 :raster-line 10 :cpu-cycle 47})
      :snapshot-timing {:global-cycle 110 :raster-line 10 :cpu-cycle 47}})
    (recorder/finalize-capture! capture)))

(defn- temp-dataset-root []
  (str (Files/createTempDirectory "vice-v2-reader-test"
                                  (make-array FileAttribute 0))))

(defn- with-synthetic-reader [f]
  (let [root (temp-dataset-root)
        reader (try
                 (write-synthetic-capture! root)
                 (reader/open root)
                 (catch Throwable error
                   (is false (str "synthetic capture setup failed: " error))
                   nil))]
    (try
      (when reader (f reader))
      (finally (store/delete-tree! root)))))

(defn- observable [writes]
  (mapv #(select-keys % [:event-index :kind :address :value]) writes))

(def ^:private expected-writes
  [{:event-index 1 :kind :store :address 0x0200 :value 0x42}
   {:event-index 3 :kind :read-modify-write :address 0x0200 :value 0x43}
   {:event-index 4 :kind :stack-push :address 0x01fd :value 0x42}
   {:event-index 5 :kind :stack-push :address 0x01fc :value 0x34}
   {:event-index 6 :kind :stack-push :address 0x01fb :value 0x10}
   {:event-index 6 :kind :stack-push :address 0x01fa :value 0x0e}
   {:event-index 8 :kind :interrupt-entry :address 0x01fb :value 0x10}
   {:event-index 8 :kind :interrupt-entry :address 0x01fa :value 0x0f}
   {:event-index 8 :kind :interrupt-entry :address 0x01f9 :value 0x20}
   {:event-index 8 :kind :stack-push :address 0x01f8 :value 0x42}])

(deftest synthetic-write-inference
  (with-synthetic-reader
    (fn [reader]
      (testing "stores, RMW, PHA/PHP, JSR, and interrupt pushes are inferred"
        (is (= expected-writes (observable (reader/read-writes reader [0 9])))))
      (testing "unchanged writes are retained"
        (let [writes (reader/read-writes reader [0 9])]
          (is (every? #(contains? % :value) writes))))
      (testing "RTS pops are reads, not writes"
        (is (not-any? #(and (= 0x01fa (:address %))
                            (= :store (:kind %)))
                      (reader/read-writes reader [7 8]))))
      (testing "access scans expose the read and write of a read-modify-write"
        (is (= [[:read 0x0200] [:read-modify-write 0x0200]]
               (mapv (juxt :kind :address)
                     (reader/read-accesses reader [3 4]))))))))

(deftest synthetic-ranges-and-boundaries
  (with-synthetic-reader
    (fn [reader]
      (testing "event scanning returns the requested half-open range"
        (is (= 5 (count (reader/read-events reader [0 5]))))
        (is (= 4 (count (reader/read-events reader [5 9]))))
        (is (= 9 (count (reader/read-events reader [0 9])))))
      (testing "writes are identical whether scanned sequentially or split"
        (let [whole (reader/read-writes reader [1 8])
              split (vec (concat (reader/read-writes reader [1 5])
                                 (reader/read-writes reader [5 8])))]
          (is (= (observable whole) (observable split)))))
      (testing "writes are not lost or duplicated across a chunk boundary"
        (let [whole (reader/read-writes reader [4 9])
              split (vec (concat (reader/read-writes reader [4 5])
                                 (reader/read-writes reader [5 9])))]
          (is (= (observable whole) (observable split)))))
      (testing "boundary snapshots are addressable and non-boundaries rejected"
        (is (map? (reader/boundary-snapshot reader 0)))
        (is (map? (reader/boundary-snapshot reader 5)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (reader/boundary-snapshot reader 2)))))))

(deftest synthetic-replay-state
  (with-synthetic-reader
    (fn [reader]
      (testing "replay state carries the inferred RAM mutations"
        (let [state (reader/replay-state reader 9 0)
              ram ^bytes (:ram state)]
          (is (= 0x43 (bit-and (aget ram 0x0200) 0xff)))
          (is (= 0x42 (bit-and (aget ram 0x01fd) 0xff)))
          (is (= 0x34 (bit-and (aget ram 0x01fc) 0xff)))
          (is (= 0x10 (bit-and (aget ram 0x01fb) 0xff)))
          (is (= 0x0f (bit-and (aget ram 0x01fa) 0xff)))
          (is (= 0x20 (bit-and (aget ram 0x01f9) 0xff)))))
      (testing "replay state is explicit about opaque device capabilities"
        (let [state (reader/replay-state reader 9 0)]
          (is (= :exact (get-in state [:capabilities :ram])))
          (is (= :write-shadow (get-in state [:capabilities :vic])))
          (is (= :unmodeled (get-in state [:capabilities :opaque-device-state])))))
      (testing "ordered targets share replay without changing their states"
        (let [indexes [0 4 5 9]
              shared (reader/replay-states reader indexes)
              separate (mapv #(reader/replay-state reader %) indexes)]
          (is (= (mapv :event-index separate) (mapv :event-index shared)))
          (is (every? true?
                      (map (fn [a b]
                             (java.util.Arrays/equals ^bytes (:ram a)
                                                      ^bytes (:ram b)))
                           separate shared)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (reader/replay-states reader [5 4]))))))))

(deftest synthetic-device-domains-and-mirrors
  (with-synthetic-reader
    (fn [_reader]
     ;; A dedicated capture makes the VIC/SID domain and mirror assertions
     ;; independent of the program above.
      (let [root (temp-dataset-root)
            events [(event-at 0x2000 [0x8d 0x20 0xd0] 0x0e 0 0 0xfd 0 100)
                    (event-at 0x2003 [0x8d 0x40 0xd0] 0x02 0 0 0xfd 0 102)
                    (event-at 0x2006 [0x8d 0x00 0xd4] 0x0f 0 0 0xfd 0 104)
                    (event-at 0x2009 [0xad 0x00 0xdd] 0x00 0 0 0xfd 0 106)]]
        (try
          (let [capture (recorder/start-capture!
                         {:dataset-root root :capture-id "devices"})]
            (recorder/commit-chunk!
             capture
             {:event-count (count events)
              :event-writer (fn [out] (event/write-events! out events))
              :ram (byte-array ram-size)
              :mem (byte-array ram-size)
              :snapshot (snapshot-at {:global-cycle 100 :raster-line 1 :cpu-cycle 1})
              :snapshot-timing {:global-cycle 100 :raster-line 1 :cpu-cycle 1}})
            (recorder/finalize-capture! capture))
          (let [reader (reader/open root)
                writes (reader/read-writes reader [0 3])]
            (is (= [[:vic 0xd020 0x20] [:vic 0xd011 0x11] [:sid 0xd400 0x00]]
                   (mapv (fn [w] [(:domain w) (:canonical-address w)
                                  (:register w)]) writes)))
            (is (= [0x0e 0x02 0x0f] (mapv :value writes)))
            (is (= [0xd020 0xd011]
                   (mapv :canonical-address
                         (reader/scan-vic-writes reader [0 3] conj []))))
            (is (= [0xd400]
                   (mapv :canonical-address
                         (reader/scan-sid-writes reader [0 3] conj []))))
            (is (= [0xd011] (mapv :canonical-address
                                  (reader/read-writes reader [1 2]))))
            (testing "ordinary memory operands are exposed as read accesses"
              (is (= [[:read :cia2 0xdd00]]
                     (mapv (juxt :kind :domain :address)
                           (reader/read-accesses reader [3 4]))))))
          (finally (store/delete-tree! root)))))))
