(ns omkamra.vice.capture-test
  "Tests for the event codec, trace adaptation, and transactional capture
  commitment with a synthetic FIFO trace."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.vice.trace :as trace]
            [omkamra.vice.capture :as capture]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.driver :as driver]
            [omkamra.vice.event :as event]
            [omkamra.vice.recorder :as recorder]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Arrays]
           [java.util.concurrent TimeUnit]
           [javax.imageio ImageIO]))

;; Fixtures -----------------------------------------------------------------

(def ^:private sample-events
  [{:pc 0x1000 :bytes [0xA9 0x01] :a 0x00 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 100 :raster 1 :cpu-cycle 5}
   {:pc 0x1002 :bytes [0x8D 0x00 0x20] :a 0x01 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 102 :raster 1 :cpu-cycle 7}
   {:pc 0x1005 :bytes [0xEA] :a 0x01 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 106 :raster 1 :cpu-cycle 11}
   {:pc 0x1006 :bytes [0x4C 0x00 0x10] :a 0x01 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 108 :raster 1 :cpu-cycle 13}
   {:pc 0x1000 :bytes [0xA9 0x01] :a 0x01 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 111 :raster 1 :cpu-cycle 16}
   {:pc 0x1002 :bytes [0x8D 0x00 0x20] :a 0x01 :x 0x00 :y 0x00 :sp 0xff
    :flags "..-..I.C" :global-cycle 113 :raster 1 :cpu-cycle 18}])

(def ^:private malformed-event
  {:pc 0x2000 :bytes [0xA9 0x01 0x02 0x03] :a 0x00 :x 0x00 :y 0x00 :sp 0xff
   :flags "..-..I.C" :global-cycle 500 :raster 1 :cpu-cycle 1})

(defn- trace-pair
  [{:keys [pc bytes a x y sp flags global-cycle raster cpu-cycle]}]
  (str (format "#1 (Trace EXEC %04x) %d/$%x, %d/$%x%n"
               pc raster raster cpu-cycle cpu-cycle)
       (format ".C:%04x %s - A:%02X X:%02X Y:%02X SP:%02X %s %d%n"
               pc (str/join " " (map #(format "%02X" %) bytes))
               a x y sp flags global-cycle)))

(defn- trace-text
  [events]
  (apply str (map trace-pair events)))

(defn- string-source
  [text]
  (fn [consume!]
    (trace/reduce-records! (java.io.StringReader. text) consume! true)))

(defn- temp-dataset-root []
  (str (Files/createTempDirectory "vice-v2-test"
                                  (make-array FileAttribute 0))))

(defn- snapshot-at
  [timing]
  {:cpu {:pc 0x1000 :a 0 :x 0 :y 0 :sp 0xff :flags 5}
   :timing timing
   :registers {:vic (vec (repeat 47 0))
               :sid (vec (repeat 32 0))
               :cia1 (vec (repeat 16 0))
               :cia2 (vec (repeat 16 0))}
   :cpu-port [0x2f 0x37]
   :color-ram (vec (repeat 1024 0))
   :semantics contract/register-semantics})

(defn- test-snapshot-provider
  [{:keys [first-event]}]
  (let [timing (select-keys first-event [:global-cycle :raster-line :cpu-cycle])]
    {:ram (byte-array contract/memory-image-size)
     :mem (byte-array contract/memory-image-size)
     :snapshot (snapshot-at timing)
     :snapshot-timing timing}))

(def ^:private test-frame
  {:debug-width 4
   :debug-height 2
   :x-offset 1
   :y-offset 0
   :inner-width 2
   :inner-height 1
   :bpp 8
   :buffer (byte-array (map unchecked-byte [0 1 2 3 0 1 2 3]))})

(def ^:private test-palette
  [[0 0 0] [255 0 0] [0 255 0] [0 0 255]])

(defn- screenshot-snapshot-provider
  [request]
  (assoc (test-snapshot-provider request)
         :screenshot {:frame test-frame :palette test-palette}))

(defn- expected-event
  [{:keys [pc bytes a x y sp global-cycle raster cpu-cycle]}]
  {:pc pc
   :instruction-length (count bytes)
   :instruction-bytes (vec bytes)
   :a a
   :x x
   :y y
   :sp sp
   :flags 5
   :global-cycle global-cycle
   :raster-line raster
   :cpu-cycle cpu-cycle})

(defn- record-event!
  ([dataset-root events] (record-event! dataset-root events 50000))
  ([dataset-root events target]
   (record-event! dataset-root events target test-snapshot-provider))
  ([dataset-root events target snapshot-provider]
   (let [capture (recorder/start-capture! {:dataset-root dataset-root
                                           :capture-id "synthetic"
                                           :options {:event-target target}})]
     (recorder/ingest-trace-records!
      capture
      (string-source (trace-text events))
      {:event-target target
       :snapshot-provider snapshot-provider})
     (recorder/finalize-capture! capture))))

;; Codec --------------------------------------------------------------------

(deftest event-record-offsets-are-exact
  (let [source {:pc 0x1234
                :instruction-length 2
                :instruction-bytes [0xAB 0xCD]
                :a 0x11
                :x 0x22
                :y 0x33
                :sp 0x44
                :flags 0x55
                :global-cycle 0x0102030405060708
                :raster-line 0x0abc
                :cpu-cycle 0x0def}
        buffer (event/encode-event source)
        slice (fn [start end]
                (mapv contract/unsigned (Arrays/copyOfRange buffer start end)))]
    (is (= 23 (alength buffer)))
    (is (= [0x34 0x12] (slice 0 2)))
    (is (= [0x02] (slice 2 3)))
    (is (= [0xab 0xcd 0x00] (slice 3 6)))
    (is (= [0x11] (slice 6 7)))
    (is (= [0x22] (slice 7 8)))
    (is (= [0x33] (slice 8 9)))
    (is (= [0x44] (slice 9 10)))
    (is (= [0x55] (slice 10 11)))
    (is (= [0x08 0x07 0x06 0x05 0x04 0x03 0x02 0x01] (slice 11 19)))
    (is (= [0xbc 0x0a] (slice 19 21)))
    (is (= [0xef 0x0d] (slice 21 23)))
    (is (= source (select-keys (event/decode-event buffer 0) (keys source))))))

(deftest event-codec-handles-high-addresses-and-raster-lines
  ;; VICE addresses above $7FFF and raster lines above 255 must round-trip;
  ;; Clojure's checked `short` would otherwise reject them.
  (let [source {:pc 0xfd74
                :instruction-length 1
                :instruction-bytes [0x60]
                :a 0xff
                :x 0xff
                :y 0xff
                :sp 0xff
                :flags 0xff
                :global-cycle 0x1ffffffff
                :raster-line 0x0137
                :cpu-cycle 0x0038}
        decoded (event/decode-event (event/encode-event source) 0)]
    (is (= source (select-keys decoded (keys source))))))

(deftest event-codec-rejects-malformed-records
  (testing "instruction length must match the byte vector"
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/encode-event
                  {:pc 0x1000 :instruction-length 2 :instruction-bytes [0xA9]
                   :a 0 :x 0 :y 0 :sp 0 :flags 0 :global-cycle 0
                   :raster-line 0 :cpu-cycle 0})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/encode-event
                  {:pc 0x1000 :instruction-length 4
                   :instruction-bytes [1 2 3 4]
                   :a 0 :x 0 :y 0 :sp 0 :flags 0 :global-cycle 0
                   :raster-line 0 :cpu-cycle 0}))))
  (testing "out-of-range fields fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/encode-event
                  {:pc 0x10000 :instruction-length 1 :instruction-bytes [0xEA]
                   :a 0 :x 0 :y 0 :sp 0 :flags 0 :global-cycle 0
                   :raster-line 0 :cpu-cycle 0}))))
  (testing "misaligned event bytes fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/decode-events (byte-array 5))))))

(deftest trace-records-convert-to-packed-events
  (is (= (expected-event (first sample-events))
         (event/trace-record->event
          [0x1000 [0xA9 0x01] 1 5 0x00 0x00 0x00 0xff "..-..I.C" 100])))
  (testing "unsupported records fail closed"
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/trace-record->event
                  [0x2000 [0xA9 0x01 0x02 0x03] 1 1 0 0 0 0xff "..-..I.C" 1])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (event/trace-record->event
                  [0x2000 [0xA9] 1 1 0 0 0 0xff "..-..I.C" nil])))))

;; Synthetic capture --------------------------------------------------------

(deftest synthetic-trace-produces-valid-binary-chunks
  (let [dataset-root (temp-dataset-root)]
    (try
      (let [result (record-event! dataset-root sample-events 4)
            opened (store/open-capture (io/file dataset-root "capture"))
            manifest (:manifest opened)
            chunks (:chunks manifest)
            chunk1 (store/read-edn (io/file (:directory opened)
                                            (:file (first chunks))))
            chunk2 (store/read-edn (io/file (:directory opened)
                                            (:file (second chunks))))
            events1 (event/read-events-file
                     (io/file (:directory opened)
                              (get-in chunk1 [:artifacts :events :file])))
            events2 (event/read-events-file
                     (io/file (:directory opened)
                              (get-in chunk2 [:artifacts :events :file])))]
        (is (= 2 (:chunk-count result)))
        (is (= 6 (:event-count result)))
        (is (= 2 (count chunks)))
        (is (= [0 4] (:event-range (first chunks))))
        (is (= [4 6] (:event-range (second chunks))))
        (is (= 6 (:event-count manifest)))
        (is (= (mapv expected-event sample-events)
               (into events1 events2)))
        (is (= 4 (count events1)))
        (is (= 2 (count events2)))
        (is (= contract/memory-image-size
               (.length (io/file (:directory opened)
                                 (get-in chunk1 [:artifacts :ram :file])))))
        (is (= contract/memory-image-size
               (.length (io/file (:directory opened)
                                 (get-in chunk1 [:artifacts :mem :file]))))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest chunk-screenshots-are-committed
  (let [dataset-root (temp-dataset-root)]
    (try
      (record-event! dataset-root sample-events 4 screenshot-snapshot-provider)
      (let [directory (io/file dataset-root "capture")
            opened (store/open-capture directory)
            manifest (:manifest opened)
            entry (first (:chunks manifest))
            chunk (store/read-edn (io/file (:directory opened) (:file entry)))
            artifact (get-in entry [:artifacts :screenshot])
            ^java.io.File png (io/file (:directory opened) (:file artifact))
            reader (reader/open dataset-root)]
        (testing "the PNG artifact is declared and checksummed"
          (is (= contract/screen-png-format
                 (get-in manifest [:formats :screenshot :format])))
          (is (= contract/screen-png-format
                 (get-in chunk [:schemas :screenshot])))
          (is (= (str "chunks/" (store/chunk-artifact-name 1 :screenshot))
                 (:file artifact)))
          (is (= (.getPath png)
                 (.getPath (reader/screenshot-file reader entry))))
          (is (= (:size artifact) (.length png)))
          (is (= (:sha256 artifact) (store/sha256-file png))))
        (testing "the metadata describes the full debug frame"
          (is (= {:file (str "chunks/" (store/chunk-artifact-name 1 :screenshot))
                  :width 4
                  :height 2
                  :inner-width 2
                  :inner-height 1
                  :x-offset 1
                  :y-offset 0
                  :bpp 8}
                 (:screenshot chunk)))
          (is (= (:screenshot chunk) (reader/chunk-screenshot reader entry)))
          (is (some? (reader/screenshot-bytes reader entry))))
        (testing "the committed PNG decodes to the frame geometry"
          (let [image (ImageIO/read png)]
            (is (= 4 (.getWidth image)))
            (is (= 2 (.getHeight image))))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest malformed-trace-record-destroys-the-capture
  (let [dataset-root (temp-dataset-root)
        capture (recorder/start-capture! {:dataset-root dataset-root
                                          :capture-id "malformed"})]
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (recorder/ingest-trace-records!
                    capture
                    (string-source (trace-text [malformed-event]))
                    {:snapshot-provider test-snapshot-provider})))
      (is (not (.exists (io/file dataset-root "capture"))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest capture-option-validation
  (testing ":from-pc must be a 16-bit address"
    (is (nil? (#'capture/from-pc-option {})))
    (is (= 0x1000 (#'capture/from-pc-option {:from-pc 0x1000})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/from-pc-option {:from-pc 0x10000})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/from-pc-option {:from-pc -1}))))
  (testing ":max-events must be a positive integer"
    (is (nil? (#'capture/max-events-option {})))
    (is (= 10 (#'capture/max-events-option {:max-events 10})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/max-events-option {:max-events 0})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/max-events-option {:max-events 1.5}))))
  (testing ":instance must describe an address and port"
    (is (= {:address "localhost" :port 6502}
           (#'capture/instance-option
            {:instance {:address "localhost" :port 6502}})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/instance-option {:instance {:port 6502}})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'capture/instance-option {:instance "localhost:6502"}))))
  (testing ":input and :instance are mutually exclusive"
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/start! {:input "program.prg"
                                  :instance {:address "localhost" :port 6502}}))))
  (testing "a full capture powers on its own VICE"
    (is (thrown? clojure.lang.ExceptionInfo
                 (capture/start! {:full-capture? true
                                  :instance {:address "localhost" :port 6502}})))))

(deftest basic-sys-stub-parsing
  (let [target (fn [text]
                 (#'driver/sys-target-in-text (byte-array text) 0 (count text)))]
    (testing "plain and parenthesized SYS operands are both accepted"
      (is (= 2070 (target [0x9e 0x20 0x32 0x30 0x37 0x30])))
      (is (= 2064 (target [0x9e 0x20 0x28 0x32 0x30 0x36 0x34 0x29])))
      (is (= 2049 (target [0x9e 0x32 0x30 0x34 0x39 0x3a 0x00]))))
    (testing "a line without a SYS token has no target"
      (is (nil? (target [0x20 0x41 0x42 0x43 0x00]))))))

(deftest session-registry-lifecycle
  (let [fake {:session-id "test-session"
              :capture-id "test-capture"
              :input "in" :dataset-root "root" :event-target 1
              :capture {:state (atom {})}
              :state (atom {:status :stopped})
              :metrics (atom nil)
              :queue (java.util.concurrent.ArrayBlockingQueue. 1)
              :reader-thread (atom nil)
              :worker (atom nil)
              :stop-requested (atom true)}]
    (#'capture/register-session! fake)
    (is (some #(= "test-session" (:session-id %)) (capture/live-sessions)))
    (testing "a registered session reports reader/worker liveness"
      (let [summary (first (filter #(= "test-session" (:session-id %))
                                   (capture/live-sessions)))]
        (is (contains? summary :reader-alive?))
        (is (not (:reader-alive? summary)))
        (is (true? (:stop-requested? summary)))
        (is (not (:worker-done? summary)))))
    (#'capture/unregister-session! fake)
    (is (not (some #(= "test-session" (:session-id %)) (capture/live-sessions))))))

(deftest release-fifo-open-unblocks-a-blocked-reader
  (let [directory (Files/createTempDirectory "vice-fifo-release-"
                                             (make-array FileAttribute 0))
        fifo (str directory "/trace.fifo")]
    (try
      (let [mkfifo (ProcessBuilder. ["mkfifo" fifo])]
        (is (.waitFor (.start mkfifo) 5 TimeUnit/SECONDS)))
      (let [opened (promise)
            reader (doto (Thread. (fn []
                                    (try
                                      (let [r (io/reader fifo)]
                                        (.close r)
                                        (deliver opened :opened))
                                      (catch Throwable error
                                        (deliver opened error)))))
                     (.setName "vice-fifo-release-reader")
                     (.setDaemon true)
                     (.start))]
        ;; The reader blocks in the FIFO open until a writer appears.
        (Thread/sleep 300)
        (is (not (realized? opened)))
        (#'capture/release-fifo-open! fifo)
        (is (= :opened (deref opened 3000 :timeout)))
        (.join reader 2000))
      (finally
        (store/delete-tree! (.toFile directory))))))

(deftest finalized-captures-are-not-overwritten
  (let [dataset-root (temp-dataset-root)]
    (try
      (record-event! dataset-root sample-events 4)
      (is (thrown? clojure.lang.ExceptionInfo
                   (recorder/start-capture! {:dataset-root dataset-root
                                             :capture-id "second"})))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest unfinalized-captures-are-not-openable
  (let [dataset-root (temp-dataset-root)
        capture (recorder/start-capture! {:dataset-root dataset-root
                                          :capture-id "partial"})]
    (try
      (recorder/ingest-trace-records!
       capture
       (string-source (trace-text sample-events))
       {:event-target 4 :snapshot-provider test-snapshot-provider})
      (is (thrown? clojure.lang.ExceptionInfo
                   (store/open-capture (io/file dataset-root "capture"))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))

(deftest tampered-artifacts-are-detected
  (let [dataset-root (temp-dataset-root)]
    (try
      (record-event! dataset-root sample-events 4)
      (let [directory (io/file dataset-root "capture")
            manifest (store/read-manifest directory)
            event-file (io/file directory
                                (get-in (first (:chunks manifest))
                                        [:artifacts :events :file]))
            bytes (Files/readAllBytes (.toPath event-file))]
        (aset-byte bytes 0 (unchecked-byte (bit-xor (aget bytes 0) 0xff)))
        (Files/write (.toPath event-file) bytes
                     (make-array java.nio.file.OpenOption 0))
        (is (thrown? clojure.lang.ExceptionInfo
                     (store/open-capture directory))))
      (finally
        (store/delete-tree! (io/file dataset-root))))))
