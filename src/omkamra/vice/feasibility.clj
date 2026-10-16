(ns omkamra.vice.feasibility
  "Executable probes for the pipeline-v2 VICE feasibility gate.

  Every probe runs against an unmodified stock VICE through the binary monitor
  and returns a report with `:status :passed` or `:failed`, the individual
  claims it checked, and the raw evidence it observed. A failing claim is a
  blocker: it is recorded, never weakened into a best-effort assumption.

  Memory access uses the VICE memory-space/bank contract discovered from
  `monitor_binary.c` and `mon_register6502.c`:

  * memspace 0 is the computer space and the `bank` field selects the bank;
  * bank 0 `cpu` is the `$01`-mapped CPU view;
  * bank 1 `ram` is the underlying 64 KiB RAM;
  * bank 2 `rom` is the ROM image;
  * bank 3 `io` is the mapped I/O view;
  * bank 4 `cart` is the cartridge space.

  `run!` starts one VICE instance, executes every probe, and returns (and
  optionally persists) the aggregate report."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [omkamra.vice :as vice]
            [omkamra.vice.binary-monitor :as bm]
            [omkamra.vice.trace :as trace]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.driver :as driver])
  (:import [java.net ServerSocket]
           [java.time Instant]
           [java.util.concurrent TimeUnit]))

(def ^:private computer-memspace 0)
(def ^:private cpu-bank 0)
(def ^:private ram-bank 1)
(def ^:private rom-bank 2)
(def ^:private io-bank 3)

(def ^:private trace-program-address 0x0334)
(def ^:private trace-program
  "LDA #$01 / LDA #$02 / NOP / JMP $0334. The two distinct pre-instruction
  A values make the trace's pre-instruction register semantics observable."
  [0xA9 0x01 0xA9 0x02 0xEA 0x4C 0x34 0x03])

(def ^:private trace-successors
  {0x0334 0x0336 0x0336 0x0338 0x0338 0x0339 0x0339 0x0334})

(def ^:private opcode-cycles
  "Cycle counts for the opcodes used by the trace probe program."
  {0xA9 2 0xEA 2 0x4C 3})

;; Low-level monitor helpers -------------------------------------------------

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- connect!
  [instance timeout-ms retry-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [conn (try
                   (let [conn (vice/connect! instance {:handle-event (fn [_ _] nil)})]
                     (bm/ping conn)
                     conn)
                   (catch Throwable _ nil))]
        (cond
          conn conn
          (< (System/currentTimeMillis) deadline)
          (do (Thread/sleep (long retry-ms)) (recur))
          :else
          (throw (ex-info "Timed out waiting for the VICE binary monitor" {})))))))

(defn- ub
  [^bytes bytes]
  (mapv contract/unsigned bytes))

(defn- read-block
  [conn bank address length]
  (ub (:memory (bm/mem-get conn {:start address
                                 :end (+ address (dec length))
                                 :memspace computer-memspace
                                 :bank bank}))))

(defn- write-block!
  [conn bank address values]
  (bm/mem-set! conn {:start address
                     :end (+ address (dec (count values)))
                     :memspace computer-memspace
                     :bank bank
                     :side-effects? true
                     :data (byte-array (map unchecked-byte values))}))

(defn- register-values
  [conn]
  (into {} (map (fn [[id {:keys [value]}]] [id value]))
        (bm/registers-get conn)))

;; FIFO trace helpers --------------------------------------------------------

(defn- mkfifo!
  [path]
  (let [^java.util.List command ["mkfifo" path]
        process (.start (ProcessBuilder. command))]
    (when-not (.waitFor process 5 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Timed out creating the trace FIFO" {:path path})))
    (when-not (zero? (.exitValue process))
      (throw (ex-info "Could not create the trace FIFO"
                      {:path path :exit-code (.exitValue process)})))))

(defn- open-line-reader!
  "Open a FIFO for line-oriented reading on a background thread.

  Returns the accumulated `:lines`, an `:eof` state that becomes `:eof` or an
  `:error` vector, and the reader `:thread`."
  [path]
  (let [lines (atom [])
        eof (atom :pending)
        thread (doto (Thread.
                      (fn []
                        (try
                          (with-open [reader (io/reader path)]
                            (loop []
                              (if-let [line (.readLine ^java.io.BufferedReader
                                             reader)]
                                (do (swap! lines conj line) (recur))
                                (reset! eof :eof))))
                          (catch Throwable error
                            (reset! eof [:error (.getMessage error)])))))
                 (.setDaemon true)
                 (.start))]
    {:lines lines :eof eof :thread thread}))

(defn- await-eof!
  "Wait for a FIFO reader to observe end of stream. Returns true only when the
  reader thread actually reached EOF."
  [{:keys [eof thread]} timeout-ms]
  (.join ^Thread thread (long timeout-ms))
  (and (not (.isAlive ^Thread thread)) (= :eof @eof)))

(defn- parse-records
  [lines]
  (into [] (trace/records-xf true) lines))

(defn- record-pc [record] (nth record 0))
(defn- record-bytes [record] (nth record 1))
(defn- record-a [record] (nth record 4))
(defn- record-x [record] (nth record 5))
(defn- record-y [record] (nth record 6))
(defn- record-sp [record] (nth record 7))
(defn- record-flags [record] (nth record 8))
(defn- record-global-cycle [record] (nth record 9))

;; Report shape --------------------------------------------------------------

(defn- probe-report
  [id claims evidence]
  (let [failed (vec (sort (keep (fn [[claim passed?]]
                                  (when-not passed? claim))
                                claims)))]
    {:probe id
     :status (if (empty? failed) :passed :failed)
     :claims claims
     :failed-claims failed
     :evidence evidence}))

(defn- error-report
  [id error]
  {:probe id
   :status :failed
   :claims {}
   :failed-claims [:probe-error]
   :evidence {:error (.getMessage ^Throwable error)}})

;; Probe 1: identity, memory banks, registers, and resources ----------------

(defn- probe-inventory
  [conn _]
  (let [bank-names (into {} (map (fn [[id {:keys [name]}]] [id name]))
                         (bm/banks-available conn))
        register-names (into {} (map (fn [[id {:keys [name]}]] [id name]))
                             (bm/registers-available conn))
        version-bytes (:main-version (bm/vice-info conn))
        log-name (:value (bm/resource-get conn {:name "MonitorLogFileName"}))
        log-enabled (:value (bm/resource-get conn
                                             {:name "MonitorLogEnabled"}))]
    (probe-report
     :vice-inventory
     {:bank-list-matches-source
      (= #{"cpu" "ram" "rom" "io" "cart"} (set (vals bank-names)))
      :cpu-registers-available
      (set/subset? #{"A" "X" "Y" "SP" "PC" "FL"} (set (vals register-names)))
      :snapshot-registers-available
      (set/subset? #{"00" "01" "LIN" "CYC"} (set (vals register-names)))
      :log-file-resource-present (string? log-name)
      :log-enabled-resource-readable (int? log-enabled)}
     {:vice-version (String. ^bytes version-bytes "US-ASCII")
      :vice-version-bytes (ub version-bytes)
      :banks bank-names
      :registers register-names
      :log-file-name log-name
      :log-enabled log-enabled})))

;; Probe 2: underlying RAM vs CPU-visible memory -----------------------------

(defn- probe-memory-views
  [conn _]
  (write-block! conn ram-bank 0xe000 [0xab])
  (write-block! conn ram-bank 0xd000 [0x5a])
  (write-block! conn cpu-bank 0xd800 [0x0f])
  (let [ram-e000 (first (read-block conn ram-bank 0xe000 1))
        cpu-e000 (first (read-block conn cpu-bank 0xe000 1))
        rom-e000 (first (read-block conn rom-bank 0xe000 1))
        ram-d000 (first (read-block conn ram-bank 0xd000 1))
        cpu-d000 (first (read-block conn cpu-bank 0xd000 1))
        io-d000 (first (read-block conn io-bank 0xd000 1))
        color (read-block conn cpu-bank 0xd800 16)
        mem-size (count (read-block conn cpu-bank 0 contract/memory-image-size))
        ram-size (count (read-block conn ram-bank 0 contract/memory-image-size))]
    (probe-report
     :memory-views
     {:underlying-ram-independent (= 0xab ram-e000)
      :cpu-view-shows-rom (= cpu-e000 rom-e000)
      :cpu-view-differs-from-underlying-ram (not= 0xab cpu-e000)
      :underlying-ram-under-io (= 0x5a ram-d000)
      :cpu-view-at-io-is-not-ram (not= 0x5a cpu-d000)
      :color-ram-is-nibble-addressed (and (= 0x0f (first color))
                                          (every? #(<= 0 % 0x0f) color))
      :both-images-are-64k (= contract/memory-image-size mem-size ram-size)}
     {:underlying-ram-at-e000 ram-e000
      :cpu-view-at-e000 cpu-e000
      :rom-bank-at-e000 rom-e000
      :underlying-ram-at-d000 ram-d000
      :cpu-view-at-d000 cpu-d000
      :io-view-at-d000 io-d000
      :color-ram (vec (take 16 color))
      :cpu-image-size mem-size
      :ram-image-size ram-size})))

;; Probe 3: canonical register acquisition semantics --------------------------

(defn- probe-register-semantics
  [conn _]
  (write-block! conn cpu-bank 0xd011 [0x1b])
  (write-block! conn cpu-bank 0xd400 [0x42])
  (write-block! conn cpu-bank 0xd401 [0x07])
  (write-block! conn cpu-bank 0xd418 [0x0f])
  (write-block! conn cpu-bank 0xdc00 [0x3f])
  (write-block! conn cpu-bank 0xdc0d [0x7f])
  (write-block! conn cpu-bank 0xdd00 [0x03])
  (let [vic (first (read-block conn cpu-bank 0xd011 1))
        sid-v1 (first (read-block conn cpu-bank 0xd400 1))
        sid-v2 (first (read-block conn cpu-bank 0xd401 1))
        sid-filter (first (read-block conn cpu-bank 0xd418 1))
        cia-pra (first (read-block conn cpu-bank 0xdc00 1))
        cia-icr (first (read-block conn cpu-bank 0xdc0d 1))
        cia2-pra (first (read-block conn cpu-bank 0xdd00 1))
        registers (register-values conn)]
    (probe-report
     :register-semantics
     {:vic-value-readable (= 0x1b (bit-and vic 0x7f))
      :sid-voice1-latched (= 0x42 sid-v1)
      :sid-voice2-latched (= 0x07 sid-v2)
      :sid-filter-latched (= 0x0f sid-filter)
      :cia-port-readable (= 0x3f cia-pra)
      :cia-interrupt-is-live-read (and (pos? (bit-and cia-icr 0x80))
                                       (not= 0x7f cia-icr))
      :cia2-port-readable (some? cia2-pra)
      :cpu-registers-present (every? #(contains? registers %)
                                     [0 1 2 3 4 5])
      :cpu-port-registers-present (every? #(contains? registers %)
                                          [53 54 55 56])}
     {:vic-d011 vic
      :sid-d400 sid-v1
      :sid-d401 sid-v2
      :sid-d418 sid-filter
      :cia1-dc00 cia-pra
      :cia1-dc0d cia-icr
      :cia2-dd00 cia2-pra
      :registers (select-keys registers [0 1 2 3 4 5 53 54 55 56])})))

;; Probe 4: trace semantics, flush barrier, pause, and boundary continuity ----

(defn- probe-trace-and-barrier
  [conn {:keys [work-dir sleep-ms] :or {sleep-ms 400}}]
  (let [fifo (str (io/file work-dir "trace.fifo"))
        _ (io/delete-file fifo true)
        _ (mkfifo! fifo)
        checkpoint (atom nil)]
    (try
      (write-block! conn cpu-bank trace-program-address trace-program)
      (bm/registers-set! conn {:register-values [[3 trace-program-address]]})
      (reset! checkpoint
              (bm/checkpoint-set! conn {:start 0 :end 0xffff :stop? false
                                        :enabled? true
                                        :op bm/MON_CHECKPOINT_OP_EXECUTE
                                        :temporary? false}))
      (bm/ignore-unsolicited-types!
       conn #{bm/MON_RESPONSE_CHECKPOINT_INFO})
      (let [reader1 (open-line-reader! fifo)]
        (bm/resource-set! conn {:name "MonitorLogFileName" :value fifo})
        (bm/resource-set! conn {:name "MonitorLogEnabled" :value 1})
        (bm/resume! conn)
        (Thread/sleep (long sleep-ms))
        (bm/resource-set! conn {:name "MonitorLogEnabled" :value 0})
        (let [barrier1 (await-eof! reader1 5000)
              pc-before (:value (get (bm/registers-get conn) 3))
              flags (:value (get (bm/registers-get conn) 5))
              _ (Thread/sleep 200)
              pc-after (:value (get (bm/registers-get conn) 3))
              lines1 @(:lines reader1)
              records1 (parse-records lines1)
              last-record (last records1)
              prompt-line (first (filter #(str/starts-with? % "(C:$") lines1))
              steady-flags (vec (drop 1 (map #(contract/trace-flags->status-byte
                                               (record-flags %))
                                             records1)))
              last-opcode (first (record-bytes last-record))
              expected-next-cycle (+ (record-global-cycle last-record)
                                     (get opcode-cycles last-opcode 0))
              record-at (fn [pc] (first (filter #(= pc (record-pc %))
                                                records1)))
              reader2 (open-line-reader! fifo)]
          (bm/resource-set! conn {:name "MonitorLogEnabled" :value 1})
          (bm/resume! conn)
          (Thread/sleep 200)
          (bm/resource-set! conn {:name "MonitorLogEnabled" :value 0})
          (let [barrier2 (await-eof! reader2 5000)
                records2 (parse-records @(:lines reader2))
                first-record2 (first records2)
                r334 (record-at 0x0334)
                r336 (record-at 0x0336)
                r338 (record-at 0x0338)]
            (probe-report
             :trace-and-boundary-barrier
             {:logging-disable-is-eof-barrier (true? barrier1)
              :vice-paused-after-barrier (= pc-before pc-after)
              :records-drained (pos? (count records1))
              :pre-instruction-registers
              (and (some? r336) (some? r338)
                   (= 1 (record-a r336))
                   (= 2 (record-a r338)))
              :pre-instruction-instruction-bytes
              (and (some? r334) (= [0xA9 0x01] (vec (record-bytes r334))))
              :trace-records-have-timing
              (every? #(and (int? (record-global-cycle %))
                            (int? (record-x %))
                            (int? (record-y %))
                            (int? (record-sp %)))
                      records1)
              :flags-conversion-matches-monitor
              (and (pos? (count steady-flags))
                   (apply = steady-flags)
                   (= flags (first steady-flags)))
              :pause-pc-is-next-untraced
              (and (some? last-record)
                   (= pc-before (get trace-successors (record-pc last-record))))
              :monitor-prompt-observed (some? prompt-line)
              :resume-continues-at-pause-pc
              (and (some? first-record2)
                   (= pc-before (record-pc first-record2)))
              :first-event-has-no-intervening-cycle
              (and (some? first-record2)
                   (= expected-next-cycle (record-global-cycle first-record2)))
              :second-barrier-is-eof-barrier (true? barrier2)}
             {:line-count-1 (count lines1)
              :record-count-1 (count records1)
              :line-count-2 (count @(:lines reader2))
              :record-count-2 (count records2)
              :pause-pc pc-before
              :last-record (vec last-record)
              :expected-next-cycle expected-next-cycle
              :first-record-2 (some-> first-record2 vec)
              :monitor-flags flags
              :steady-flags (vec (distinct steady-flags))
              :monitor-prompt prompt-line
              :first-line-1 (first lines1)
              :last-line-1 (last lines1)
              :first-line-2 (first @(:lines reader2))}))))
      (finally
        (when-let [number (:number @checkpoint)]
          (try (bm/checkpoint-delete! conn {:number number})
               (catch Throwable _ nil)))
        (io/delete-file fifo true)))))

;; Runner -------------------------------------------------------------------

(def ^:private probes
  [{:id :vice-inventory :run probe-inventory}
   {:id :memory-views :run probe-memory-views}
   {:id :register-semantics :run probe-register-semantics}
   {:id :trace-and-boundary-barrier :run probe-trace-and-barrier}])

(defn run!
  "Run the complete pipeline-v2 VICE feasibility gate.

  Options: `:executable`, `:port`, `:connect-timeout-ms`, `:connect-retry-ms`,
  `:work-dir`, `:sleep-ms`, and `:report-file`. Returns the aggregate report
  and writes it to `:report-file` when supplied."
  ([] (run! {}))
  ([{:keys [executable port connect-timeout-ms connect-retry-ms
            work-dir report-file sleep-ms]
     :or {connect-timeout-ms 30000 connect-retry-ms 100
          work-dir "/tmp/omkamra-vice-v2-feasibility" sleep-ms 400}}]
   (.mkdirs (io/file work-dir))
   (let [instance (vice/start! (cond-> {:port (or port (free-port))}
                                 executable (assoc :executable executable)))
         conn (try
                (connect! instance connect-timeout-ms connect-retry-ms)
                (catch Throwable error
                  (vice/stop! instance)
                  (throw error)))]
     (try
       (let [results (mapv (fn [{:keys [id run]}]
                             (try
                               (run conn {:work-dir work-dir
                                          :sleep-ms sleep-ms})
                               (catch Throwable error
                                 (error-report id error))))
                           probes)
             report {:format :omkamra.vice/v2-feasibility-report
                     :vice-version contract/supported-vice-version
                     :executable (or executable driver/default-executable)
                     :generated-at (str (Instant/now))
                     :probes results
                     :summary {:passed (count (filter #(= :passed (:status %))
                                                      results))
                               :failed (count (filter #(= :failed (:status %))
                                                      results))}
                     :blockers (vec (keep (fn [result]
                                            (when (= :failed (:status result))
                                              (:probe result)))
                                          results))}]
         (when report-file
           (with-open [writer (io/writer report-file)]
             (binding [*out* writer]
               (pr report)
               (newline))))
         report)
       (finally
         (try (bm/resource-set! conn {:name "MonitorLogEnabled" :value 0})
              (catch Throwable _ nil))
         (bm/close! conn)
         (vice/stop! instance))))))

(defn -main
  [& args]
  (let [report-file (or (first args)
                        (io/file "src" "omkamra" "vice" "docs"
                                 "v2-feasibility-report.edn"))
        report (run! {:report-file report-file})]
    (println "feasibility:" (pr-str (:summary report)))
    (println "blockers:" (pr-str (:blockers report)))
    (flush)))
