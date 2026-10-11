(ns omkamra.vice.contract-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.contract :as contract]))

(deftest event-record-descriptor-is-exact
  (is (= 23 (:record-size contract/event-record-descriptor)))
  (is (= {:pc 0
          :instruction-length 2
          :instruction-bytes 3
          :a 6
          :x 7
          :y 8
          :sp 9
          :flags 10
          :global-cycle 11
          :raster-line 19
          :cpu-cycle 21}
         (into {} (map (juxt :key :offset))
               (:fields contract/event-record-descriptor))))
  (is (contract/event-record-descriptor-valid? contract/event-record-descriptor))
  (testing "malformed descriptors fail loudly"
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-event-record-descriptor!
                  (assoc contract/event-record-descriptor :record-size 26))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-event-record-descriptor!
                  (assoc-in contract/event-record-descriptor
                            [:fields 10 :offset] 20))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-event-record-descriptor!
                  (assoc contract/event-record-descriptor
                         :byte-order :big-endian))))))

(deftest little-endian-round-trips
  (is (= 0x1234 (contract/decode-u16 (contract/encode-u16 0x1234) 0)))
  (is (= 0x12345678 (contract/decode-u32 (contract/encode-u32 0x12345678) 0)))
  (is (= 0x123456789abcdef0
         (contract/decode-u64 (contract/encode-u64 0x123456789abcdef0) 0)))
  (is (= [0x34 0x12]
         (mapv contract/unsigned (contract/encode-u16 0x1234))))
  (is (= [0x78 0x56 0x34 0x12]
         (mapv contract/unsigned (contract/encode-u32 0x12345678)))))

(deftest trace-flags-conversion-is-exact
  ;; Recorded from stock VICE 3.9: the unused bit is always `-`.
  (is (= 0x85 (contract/trace-flags->status-byte "N.-..I.C")))
  (is (= 0x05 (contract/trace-flags->status-byte "..-..I.C")))
  (is (= 0x07 (contract/trace-flags->status-byte "..-..IZC")))
  (is (= "N.-..I.C" (contract/status-byte->trace-flags 0x85)))
  (is (= "..-....." (contract/status-byte->trace-flags 0)))
  (doseq [status [0x00 0x01 0x02 0x04 0x08 0x10 0x40 0x80 0x85 0xdf]]
    (is (= status
           (contract/trace-flags->status-byte
            (contract/status-byte->trace-flags status)))))
  (testing "the unused bit is rendered, not encoded"
    (is (= "..-....." (contract/status-byte->trace-flags 0x20)))
    (is (= "NV-BDIZC" (contract/status-byte->trace-flags 0xff))))
  (testing "malformed flags fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/trace-flags->status-byte "..-.I.C")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/trace-flags->status-byte "N.-..I.C ")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/trace-flags->status-byte "N.X..I.C")))))

(defn- valid-snapshot []
  {:cpu {:pc 0x0334 :a 0 :x 0 :y 0 :sp 0xff :flags 0x24}
   :timing {:global-cycle 1 :raster-line 0 :cpu-cycle 0}
   :registers {:vic (vec (repeat 47 0))
               :sid (vec (repeat 32 0))
               :cia1 (vec (repeat 16 0))
               :cia2 (vec (repeat 16 0))}
   :cpu-port [0x2f 0x37]
   :color-ram (vec (repeat 1024 0))
   :semantics contract/register-semantics})

(deftest snapshot-validation
  (is (= (valid-snapshot)
         (contract/validate-snapshot! (valid-snapshot))))
  (testing "malformed snapshots fail loudly"
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-snapshot!
                  (assoc-in (valid-snapshot) [:cpu :pc] 0x10000))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-snapshot!
                  (update-in (valid-snapshot) [:registers :vic] pop))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-snapshot!
                  (assoc (valid-snapshot) :color-ram [1 2 3]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (contract/validate-snapshot!
                  (assoc (valid-snapshot) :semantics {:vic :guess}))))))

(deftest canonical-windows
  (is (= 47 (get-in contract/register-windows [:vic :size])))
  (is (= 32 (get-in contract/register-windows [:sid :size])))
  (is (contract/validate-register-windows! contract/register-windows))
  (is (thrown? clojure.lang.ExceptionInfo
               (contract/validate-register-windows!
                (assoc contract/register-windows
                       :vic {:base 0xd000 :size 47 :reserved #{47}})))))

(deftest vic-blacklist-policy
  (is (contract/validate-vic-value-blacklist!
       contract/default-vic-value-blacklist))
  (testing "protected registers can never be blacklisted"
    (doseq [address contract/required-registers]
      (is (thrown? clojure.lang.ExceptionInfo
                   (contract/validate-vic-value-blacklist! #{address})))))
  (is (thrown? clojure.lang.ExceptionInfo
               (contract/validate-vic-value-blacklist! #{0x12345}))))

(deftest event-range-semantics
  (is (contract/event-range? [10 20]))
  (is (contract/event-range? [5 5]))
  (is (not (contract/event-range? [20 10])))
  (is (not (contract/event-range? [10])))
  (is (= 10 (contract/range-length [10 20])))
  (is (contract/range-contains? [10 20] 10))
  (is (not (contract/range-contains? [10 20] 20)))
  (is (contract/ranges-overlap? [10 20] [19 30]))
  (is (not (contract/ranges-overlap? [10 20] [20 30])))
  (is (contract/ranges-adjacent? [10 20] [20 30])))

(deftest annotation-vocabulary
  (is (= "; B:12 C:m P:04 S:07 E:1234 R:18"
         (contract/format-annotation
          {:block 12 :context "m" :predecessors 4 :successors 7
           :executions 1234 :repeats 18})))
  (is (<= (count (contract/format-annotation
                  {:block 12 :context "i8200" :predecessors 4 :successors 7
                   :executions 1234 :repeats 18}))
          contract/annotation-max-width))
  (is (thrown? clojure.lang.ExceptionInfo
               (contract/format-annotation {:block 1 :context "m"})))
  (is (thrown? clojure.lang.ExceptionInfo
               (contract/format-annotation
                {:block 1 :context "m" :predecessors 100 :successors 1
                 :executions 1 :repeats 1}))))

(deftest all-contracts-validate
  (is (contract/validate-contracts!)))
