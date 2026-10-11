(ns omkamra.vice.capture-live-test
  "On-demand live VICE integration test for the v2 capture adapter.

  This starts a real `x64sc`, captures the C64 boot plus a tiny looping PRG
  through several chunk boundaries, and validates the finalized capture. It is
  intentionally separate from the fast unit suite."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [omkamra.vice.capture :as capture]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dataset-root []
  (str (Files/createTempDirectory "vice-v2-live"
                                  (make-array FileAttribute 0))))

(defn- write-test-prg!
  "A `$0801` BASIC stub `10 SYS 2064` followed by `JMP $0810`."
  []
  (let [file (java.io.File/createTempFile "vice-v2-live" ".prg")]
    (with-open [out (io/output-stream file)]
      (.write out (byte-array (map unchecked-byte
                                   [0x01 0x08 0x0B 0x08 0x0A 0x00 0x9E
                                    0x32 0x30 0x36 0x34 0x00 0x00 0x00
                                    0xEA 0xEA 0xEA 0x4C 0x10 0x08]))))
    file))

(defn- await-events!
  [session minimum timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [status (capture/status session)]
        (cond
          (>= (or (:event-count status) 0) minimum) status
          (< (System/currentTimeMillis) deadline)
          (do (Thread/sleep 500) (recur))
          :else
          (throw (ex-info "Timed out waiting for live capture events"
                          {:minimum minimum :status status})))))))

(deftest ^:live live-capture-crosses-boundaries-and-validates
  (let [dataset-root (temp-dataset-root)
        prg (write-test-prg!)]
    (try
      (let [session (capture/start! {:input (.getPath prg)
                                     :dataset-root dataset-root
                                     :capture-id "live"
                                     :full-capture? true
                                     :warp? true
                                     :event-target 10000
                                     :record-queue-capacity 16384})]
        (try
          (let [status (await-events! session 25000 120000)]
            (is (>= (:chunk-count status) 2)))
          (let [result (capture/stop! session)
                opened (store/open-capture (:capture-directory result))
                manifest (:manifest opened)
                chunks (:chunks manifest)]
            (is (= :stopped (:status result)))
            (is (>= (:chunk-count result) 2))
            (is (= :finalized (:status manifest)))
            (is (pos? (:event-count manifest)))
            (is (= (:chunk-count manifest) (count chunks)))
            (is (= [0 (:event-count manifest)]
                   [(first (:event-range (first chunks)))
                    (second (:event-range (last chunks)))]))
            (let [first-chunk (store/read-edn
                               (io/file (:directory opened)
                                        (:file (first chunks))))
                  events (event/read-events-file
                          (io/file (:directory opened)
                                   (get-in first-chunk
                                           [:artifacts :events :file])))]
              (is (pos? (count events)))
              (is (apply < (map :global-cycle events)))
              (is (= contract/memory-image-size
                     (.length (io/file (:directory opened)
                                       (get-in first-chunk
                                               [:artifacts :ram :file])))))))
          (finally
            (try (capture/stop! session) (catch Throwable _ nil)))))
      (finally
        (store/delete-tree! (io/file dataset-root))
        (io/delete-file prg true)))))
