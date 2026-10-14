(ns omkamra.vice.capture-live-test
  "On-demand live VICE integration test for the v2 capture adapter.

  This starts a real `x64sc`, captures the C64 boot plus a tiny looping PRG
  through several chunk boundaries, and validates the finalized capture. It is
  intentionally separate from the fast unit suite."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [omkamra.vice :as vice]
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

(defn- await-worker!
  [session timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [status (capture/status session)]
        (cond
          (:worker-done? status) status
          (< (System/currentTimeMillis) deadline)
          (do (Thread/sleep 500) (recur))
          :else
          (throw (ex-info "Timed out waiting for the live capture to stop"
                          {:status status})))))))

(deftest ^:live live-full-capture-records-the-boot-sequence
  ;; A full capture powers on the machine and starts the trace at the reset
  ;; entry; it needs no input and records the boot up to the READY prompt.
  (let [dataset-root (temp-dataset-root)]
    (try
      (let [session (capture/capture! {:dataset-root dataset-root
                                       :capture-id "boot"
                                       :full-capture? true
                                       :max-events 20000
                                       :events-per-chunk 8000
                                       :record-queue-capacity 16384})]
        (try
          (let [status (await-worker! session 180000)]
            (is (true? (:worker-done? status)))
            (is (= 20000 (:event-count status))))
          (let [result (capture/stop! session)
                opened (store/open-capture! (:capture-directory result))
                manifest (:manifest opened)
                first-chunk (store/read-edn
                             (io/file (:directory opened)
                                      (:file (first (:chunks manifest)))))
                events (event/read-events-file
                        (io/file (:directory opened)
                                 (get-in first-chunk [:artifacts :events :file])))]
            (is (= :stopped (:status result)))
            (is (= 20000 (:event-count manifest)))
            ;; The boot trace starts at the CPU reset vector ($FCE2).
            (is (= 0xfce2 (:pc (first events)))))
          (finally
            (try (capture/stop! session) (catch Throwable _ nil)))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest ^:live live-program-only-capture-starts-at-sys-target
  ;; The default input path autostarts the PRG, waits for BASIC RUN at $E144,
  ;; then gates the backend on the stub's SYS target ($0810).
  (let [dataset-root (temp-dataset-root)
        prg (write-test-prg!)]
    (try
      (let [session (capture/capture! {:input (.getPath prg)
                                       :dataset-root dataset-root
                                       :capture-id "program"
                                       :events-per-chunk 3000
                                       :max-events 9000
                                       :record-queue-capacity 16384})]
        (try
          (let [status (await-worker! session 120000)]
            (is (true? (:worker-done? status)))
            (is (= 9000 (:event-count status))))
          (let [result (capture/stop! session)
                opened (store/open-capture! (:capture-directory result))
                manifest (:manifest opened)
                first-chunk (store/read-edn
                             (io/file (:directory opened)
                                      (:file (first (:chunks manifest)))))
                events (event/read-events-file
                        (io/file (:directory opened)
                                 (get-in first-chunk [:artifacts :events :file])))]
            (is (= :stopped (:status result)))
            (is (= 9000 (:event-count manifest)))
            ;; The trace starts on the program's first machine instruction.
            (is (= 0x0810 (:pc (first events)))))
          (finally
            (try (capture/stop! session) (catch Throwable _ nil)))))
      (finally
        (store/delete-tree! (io/file dataset-root))
        (io/delete-file prg true)))))

(deftest ^:live live-instance-capture-stops-at-max-events
  ;; Attach to an already running VICE, gate the trace on the KERNAL IRQ entry,
  ;; and let :max-events finalize the capture without an explicit stop.
  (let [dataset-root (temp-dataset-root)
        port (+ 20000 (rand-int 30000))
        instance (vice/start! {:port port})]
    (try
      (let [session (capture/capture! {:instance instance
                                       :dataset-root dataset-root
                                       :capture-id "instance"
                                       :from-pc 0xea31
                                       :max-events 8000
                                       :events-per-chunk 4000
                                       :record-queue-capacity 16384})]
        (try
          (let [status (await-worker! session 120000)]
            (is (true? (:worker-done? status)))
            (is (= 8000 (:event-count status))))
          (let [result (capture/stop! session)
                opened (store/open-capture! (:capture-directory result))
                manifest (:manifest opened)]
            (is (= :stopped (:status result)))
            (is (= 8000 (:event-count result)))
            (is (= :finalized (:status manifest)))
            (is (= 8000 (:event-count manifest)))
            ;; :max-events is a hard cap, so the final chunk ends exactly there.
            (is (= 8000 (second (:event-range (last (:chunks manifest)))))))
          (finally
            (try (capture/stop! session) (catch Throwable _ nil)))))
      (finally
        (vice/stop! instance)
        (store/delete-tree! (io/file dataset-root))))))

(deftest ^:live live-capture-crosses-boundaries-and-validates
  (let [dataset-root (temp-dataset-root)
        prg (write-test-prg!)]
    (try
      (let [session (capture/capture! {:input (.getPath prg)
                                       :dataset-root dataset-root
                                       :capture-id "live"
                                       :full-capture? true
                                       :warp? true
                                       :events-per-chunk 10000
                                       :record-queue-capacity 16384})]
        (try
          (let [status (await-events! session 25000 120000)]
            (is (>= (:chunk-count status) 2)))
          (let [result (capture/stop! session)
                opened (store/open-capture! (:capture-directory result))
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
