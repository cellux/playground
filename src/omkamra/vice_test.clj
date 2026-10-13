(ns omkamra.vice-test
  "Tests for the VICE runner lifecycle.

  Most tests use a binary-monitor test double so failure paths run without an
  emulator; one integration test runs against a real VICE when it is
  available."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.vice :as vice]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.binary-monitor :as bm]))

(defn- fake-state []
  (atom {:mem {}
         :closed? false
         :resumed? false
         :stopped nil
         :register-values nil}))

(defn- fake-instance []
  {:process nil :pid 42 :address "127.0.0.1" :port 6502})

(defn- fake-banks [_conn]
  {1 {:id 1 :name "ram"}})

(defn- fake-registers [_conn]
  {3 {:id 3 :name "PC" :size 2}})

(defn- fake-mem-set! [state _conn {:keys [start data]}]
  (swap! state update :mem
         (fn [mem]
           (reduce (fn [m i]
                     (assoc m (+ (long start) i)
                            (bit-and (int (aget ^bytes data i)) 0xff)))
                   mem
                   (range (alength ^bytes data))))))

(defn- fake-mem-get [state _conn {:keys [start end]}]
  {:memory (byte-array
            (map (fn [address] (get (:mem @state) address 0))
                 (range (long start) (inc (long end)))))})

(defn- fake-registers-set! [state _conn {:keys [register-values]}]
  (swap! state assoc :register-values register-values))

(defn- fake-resume! [state _conn]
  (swap! state assoc :resumed? true))

(defn- fake-close! [state _conn]
  (swap! state assoc :closed? true))

(defn- fake-redefs [state conn]
  {#'vice/start! (fn [params]
                   (swap! state assoc :started params)
                   (fake-instance))
   #'vice/connect! (fn [_ _] conn)
   #'vice/stop! (fn [x] (swap! state assoc :stopped x) nil)
   #'bm/banks-available fake-banks
   #'bm/registers-available fake-registers
   #'bm/mem-set (partial fake-mem-set! state)
   #'bm/mem-get (partial fake-mem-get state)
   #'bm/registers-set (partial fake-registers-set! state)
   #'bm/resume (partial fake-resume! state)
   #'bm/close (partial fake-close! state)})

(defn- with-fake-vice* [f]
  (let [state (fake-state)
        conn {:fake true}]
    (with-redefs-fn (fake-redefs state conn)
      (fn [] (f state conn)))))

(defn- simple-object []
  (assembler/assemble
   '((%entry :start)
     :start
     (lda 0x00)
     :loop
     (inc 0x0400)
     (jmp :loop))
   {:ns 'omkamra.vice-test}))

;; Runner behavior -----------------------------------------------------------

(deftest run!-injects-verifies-and-resumes
  (with-fake-vice*
    (fn [state conn]
      (let [artifact (assembler/link (simple-object))
            runtime (vice/run! artifact {:connect-timeout-ms 50})]
        (is (:resumed? @state))
        (is (= [[3 0x800]] (:register-values @state)))
        (is (= 0x800 (:entry-point runtime)))
        (is (= 1 (:bank runtime)))
        (is (= 3 (:pc-register runtime)))
        (is (:verified (:injection runtime)))
        (is (= conn (:connection runtime)))
        (is (= 8 (count (:mem @state)))
            "only the linked segment is injected")))))

(deftest run!-object-links-with-link-options
  (with-fake-vice*
    (fn [state _conn]
      (let [runtime (vice/run! (simple-object)
                               {:link-options {:ranges [[0x5000 0x6000]]}
                                :run-options {:connect-timeout-ms 50}})]
        (is (= 0x5000 (apply min (keys (:mem @state)))))
        (is (= {:ranges [[0x5000 0x6000]]} (:link-options runtime)))
        (is (= {:connect-timeout-ms 50} (:run-options runtime)))
        (is (= 0x5000 (:entry-point runtime)))))))

(deftest run!-artifact-options-are-run-options
  (with-fake-vice*
    (fn [_state _conn]
      (let [artifact (assembler/link (simple-object))
            runtime (vice/run! artifact {:connect-timeout-ms 50})]
        (is (= {:connect-timeout-ms 50} (:run-options runtime)))
        (is (= {} (:link-options runtime)))))))

(deftest run!-rejects-non-assembly-input
  (with-fake-vice*
    (fn [_state _conn]
      (is (thrown? IllegalArgumentException (vice/run! :not-assembly {}))))))

;; Failure paths -------------------------------------------------------------

(deftest run!-stops-process-when-connect-fails
  (with-fake-vice*
    (fn [state _conn]
      (with-redefs [vice/connect! (fn [_ _]
                                    (throw (ex-info "no monitor"
                                                    {:reason :no-monitor})))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (vice/run! (assembler/link (simple-object)) {})))
        (is (some? (:stopped @state)))
        (is (not (:closed? @state)))))))

(deftest run!-closes-connection-and-stops-on-injection-failure
  (with-fake-vice*
    (fn [state _conn]
      (with-redefs [bm/banks-available (fn [_] {})]
        (is (thrown? clojure.lang.ExceptionInfo
                     (vice/run! (assembler/link (simple-object)) {})))
        (is (:closed? @state))
        (is (some? (:stopped @state)))
        (is (not (:resumed? @state)))))))

(deftest run!-detects-non-visible-entry-bytes
  (with-fake-vice*
    (fn [state _conn]
      (with-redefs [bm/mem-get (fn [_ _] {:memory (byte-array [0 0 0])})]
        (is (= :entry-not-visible
               (try
                 (vice/run! (assembler/link (simple-object)) {})
                 (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (:closed? @state))
        (is (some? (:stopped @state)))))))

(deftest run!-rejects-missing-entry-point
  (with-fake-vice*
    (fn [state _conn]
      (let [artifact (assembler/link
                      (assembler/assemble '(:start (rts))
                                          {:ns 'omkamra.vice-test}))]
        (is (= :missing-entry-point
               (try
                 (vice/run! artifact {})
                 (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (:closed? @state))
        (is (some? (:stopped @state)))))))

;; stop! idempotence ---------------------------------------------------------

(deftest stop!-is-idempotent
  (let [state (fake-state)
        conn {:fake true}]
    (with-redefs [bm/close (partial fake-close! state)]
      (vice/stop! {:connection conn :process nil :pid nil})
      (is (:closed? @state))
      (vice/stop! nil)
      (vice/stop! {:connection nil :process nil :pid nil})
      (is true))))

;; Integration ---------------------------------------------------------------

(defn- vice-available? []
  (some (fn [dir]
          (let [file (io/file dir "x64sc")]
            (and (.isFile file) (.canExecute file))))
        (str/split (or (System/getenv "PATH") "") #":")))

(deftest ^:integration run!-against-vice
  (when (vice-available?)
    (let [runtime (vice/run! (simple-object)
                             {:connect-timeout-ms 15000
                              :connect-retry-ms 100})]
      (try
        (is (= 0x800 (:entry-point runtime)))
        (is (= "ram" (:name (get (bm/banks-available (:connection runtime))
                                 (:bank runtime)))))
        (is (:verified (:injection runtime)))
        (is (pos? (long (:pid runtime))))
        (finally
          (vice/stop! runtime))))))
