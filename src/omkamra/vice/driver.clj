(ns omkamra.vice.driver
  "VICE instance driver.

  Owns a running emulator and its binary-monitor connection: process launch
  and teardown, connecting when the monitor is ready, and injecting a linked
  artifact into RAM and starting it. The capture API and the examples' `run!`
  are clients of this namespace.

  VICE's binary monitor serves exactly one client at a time, so the connection
  is an exclusive resource: callers take it with `connect!` and hand it back
  with `close` (or `stop!`, which also terminates the process)."
  (:require [omkamra.vice.binary-monitor :as bm]))

(def default-executable "x64sc")
(def default-address bm/default-host)
(def default-port bm/default-port)

(def ^:private default-extra-args
  ["-remotemonitor"
   "-binarymonitor"
   "-sounddev"
   "dummy"])

(defn- validate-instance
  [instance]
  (when-not (map? instance)
    (throw (IllegalArgumentException.
            "VICE instance descriptor must be a map")))
  (doseq [[key value] [[:address (:address instance)]
                       [:port (:port instance)]]]
    (when (nil? value)
      (throw (IllegalArgumentException.
              (format "VICE instance descriptor must contain :%s" (name key))))))
  (when-not (string? (:address instance))
    (throw (IllegalArgumentException.
            "VICE instance descriptor :address must be a string")))
  (when-not (integer? (:port instance))
    (throw (IllegalArgumentException.
            "VICE instance descriptor :port must be an integer")))
  instance)

(defn- command-line
  [{:keys [executable address port extra-args options]
    :or {executable default-executable
         address default-address
         port default-port}}]
  (let [extra-args (or extra-args options [])]
    (when-not (sequential? extra-args)
      (throw (IllegalArgumentException.
              ":extra-args must be sequential")))
    (vec (concat [(str executable)]
                 default-extra-args
                 ["-binarymonitoraddress"
                  (format "ip4://%s:%d" address port)]
                 (map str extra-args)))))

(defn start
  "Start a VICE emulator asynchronously.

  `params` is a map containing the executable and command-line arguments. The
  executable defaults to `x64sc`; `:extra-args` (or `:options`) supplies
  additional VICE command-line arguments. The binary monitor is enabled by
  default and configured using `:address` and `:port`, which default to
  localhost and 6502. The dummy sound device is selected by default for
  captures; pass a different `-sounddev` option through `:extra-args` to
  override it.

  Returns a VICE instance descriptor suitable for `connect` and `stop`."
  ([]
   (start {}))
  ([params]
   (when-not (map? params)
     (throw (IllegalArgumentException. "VICE parameters must be a map")))
   (let [address (str (or (:address params) default-address))
         port (or (:port params) default-port)
         _ (when-not (integer? port)
             (throw (IllegalArgumentException. ":port must be an integer")))
         command (command-line (assoc params :address address :port port))
         process (.start (doto (ProcessBuilder. ^java.util.List command)
                           (.redirectOutput java.lang.ProcessBuilder$Redirect/INHERIT)
                           (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
     {:process process
      :pid (.pid process)
      :address address
      :port port
      :command command
      :params params})))

(defn start!
  "Canonical VICE process launcher. See `start`."
  ([] (start! {}))
  ([params] (start (or params {}))))

(defn- destroy-process!
  "Terminate a process and wait briefly for it to release its port."
  [^Process process]
  (when (.isAlive process)
    (.destroy process)
    (try
      (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)
      (catch Throwable _ nil))
    (when (.isAlive process)
      (.destroyForcibly process))))

(defn- destroy-pid!
  "Terminate a process identified only by pid and wait briefly."
  [pid]
  (let [handle (java.lang.ProcessHandle/of (long pid))]
    (when (.isPresent handle)
      (let [process (.get handle)]
        (when (.isAlive process)
          (.destroy process)
          (loop [waited 0]
            (when (and (.isAlive process) (< waited 5000))
              (Thread/sleep 20)
              (recur (+ waited 20))))
          (when (.isAlive process)
            (.destroyForcibly process)))))))

(defn stop!
  "Close the monitor connection and terminate a VICE process.

  Accepts either a plain instance descriptor or a runtime map returned by
  `run!`. Idempotent: a missing or already-dead process is not an error."
  [runtime-or-instance]
  (when runtime-or-instance
    (when-let [conn (:connection runtime-or-instance)]
      (try
        (bm/close conn)
        (catch Throwable _ nil)))
    (if-let [process (:process runtime-or-instance)]
      (destroy-process! process)
      (when-let [pid (:pid runtime-or-instance)]
        (destroy-pid! pid))))
  nil)

(defn stop
  "Compatibility wrapper around `stop!` for a plain VICE instance descriptor."
  [instance]
  (validate-instance instance)
  (stop! instance))

(defn free-port
  "An ephemeral localhost TCP port, so repeated runs never collide."
  []
  (with-open [socket (java.net.ServerSocket. 0)]
    (.getLocalPort socket)))

(defn connect
  "Connect to the binary monitor described by `instance`.

  Connecting to VICE enters the monitor and pauses emulation. Monitor
  requests also leave VICE paused; call
  `omkamra.vice.binary-monitor/resume` (or `exit`) as the final request when
  execution should resume.

  An optional event handler receives unsolicited binary-monitor events; it
  defaults to `prn`."
  ([instance]
   (connect instance prn))
  ([instance handle-event]
   (let [{:keys [address port]} (validate-instance instance)]
     (bm/connect address port (or handle-event prn)))))

(defn close
  [conn]
  (bm/close conn))

(defmacro with-conn
  [conn instance & body]
  `(let [~conn (connect ~instance)]
     (try
       ~@body
       (finally
         (bm/close ~conn)))))

;; Connecting when ready -----------------------------------------------------

(def default-connect-timeout-ms 10000)
(def default-connect-retry-ms 100)

(defn connect!
  "Connect to `instance`'s binary monitor, retrying until it is ready.

  Throws if the process exits first or the timeout elapses. Options:
  `:connect-timeout-ms`, `:connect-retry-ms`, and `:handle-event`."
  ([instance]
   (connect! instance {}))
  ([instance {:keys [connect-timeout-ms connect-retry-ms handle-event]
              :or {connect-timeout-ms default-connect-timeout-ms
                   connect-retry-ms default-connect-retry-ms}}]
   (let [deadline (+ (System/nanoTime) (* (long connect-timeout-ms) 1000000))]
     (loop []
       (when-let [process (:process instance)]
         (when-not (.isAlive ^Process process)
           (throw (ex-info "VICE exited before its binary monitor became ready"
                           {:reason :vice-exited :pid (:pid instance)}))))
       (let [conn (try
                    (let [conn (connect instance (or handle-event (fn [_ _] nil)))]
                      (bm/ping conn)
                      conn)
                    (catch Throwable _ nil))]
         (cond
           conn conn

           (< (System/nanoTime) deadline)
           (do (Thread/sleep (long connect-retry-ms))
               (recur))

           :else
           (throw (ex-info "Timed out waiting for the VICE binary monitor"
                           {:reason :connect-timeout
                            :pid (:pid instance)
                            :timeout-ms connect-timeout-ms}))))))))

;; Program injection ---------------------------------------------------------

(defn- named-bank
  [conn name]
  (or (some (fn [bank]
              (when (= name (:name bank)) bank))
            (vals (bm/banks-available conn)))
      (throw (ex-info (str "VICE has no bank named " name)
                      {:reason :missing-bank :bank name}))))

(defn- named-register
  [conn name]
  (or (some (fn [register]
              (when (= name (:name register)) register))
            (vals (bm/registers-available conn)))
      (throw (ex-info (str "VICE has no register named " name)
                      {:reason :missing-register :register name}))))

(def ^:private color-ram-start 0xd800)
(def ^:private color-ram-end 0xdc00)

(defn- write-segment!
  [conn bank {:keys [start-address bytes]}]
  (let [length (alength ^bytes bytes)]
    (when (pos? length)
      (bm/mem-set conn {:start start-address
                        :end (+ (long start-address) (dec length))
                        :bank bank
                        :data bytes}))))

(defn- write-color-ram!
  "Write the colour-RAM window of a segment through the CPU-visible bank.

  Colour RAM ($D800-$DBFF) is a separate 1 KiB chip that the RAM bank does not
  reach. This is the one target the injector treats specially, and a future
  PRG startup stub handles it the same way."
  [conn {:keys [start-address bytes]}]
  (let [start (long start-address)
        end (+ start (alength ^bytes bytes))
        from (max start color-ram-start)
        to (min end color-ram-end)]
    (when (< from to)
      (let [offset (- from start)
            data (java.util.Arrays/copyOfRange ^bytes bytes
                                               (int offset)
                                               (int (+ offset (- to from))))]
        (bm/mem-set conn {:start from :end (dec to) :bank 0 :data data})))))

(defn- inject-segments!
  [conn bank-id segments]
  (doseq [segment segments]
    (write-segment! conn bank-id segment)
    (write-color-ram! conn segment)))

(defn- verify-entry!
  [conn entry segments]
  (let [segment (some (fn [{:keys [start-address bytes]}]
                        (let [length (alength ^bytes bytes)]
                          (when (and (<= (long start-address) (long entry))
                                     (< (long entry) (+ (long start-address) length)))
                            {:start-address start-address :bytes bytes})))
                      segments)]
    (when-not segment
      (throw (ex-info "Entry point is not inside any linked segment"
                      {:reason :entry-not-in-segment :entry-point entry})))
    (let [offset (- (long entry) (long (:start-address segment)))
          bytes ^bytes (:bytes segment)
          length (min 8 (- (alength bytes) offset))
          response (bm/mem-get conn {:start entry
                                     :end (dec (+ (long entry) length))
                                     :bank 0})
          visible ^bytes (:memory response)]
      (dotimes [index length]
        (when-not (= (bit-and (int (aget visible index)) 0xff)
                     (bit-and (int (aget bytes (+ offset index))) 0xff))
          (throw (ex-info "Entry point bytes are not CPU-visible"
                          {:reason :entry-not-visible
                           :entry-point entry
                           :offset index
                           :expected (bit-and (int (aget bytes (+ offset index))) 0xff)
                           :actual (bit-and (int (aget visible index)) 0xff)}))))
      length)))

(defn inject!
  "Inject `artifact`'s segments into RAM, leaving the PC untouched and VICE
  paused so a capture can arm its trace before the entry point is entered.

  Returns the entry point, the RAM bank id, the PC register id, and injection
  metadata. The caller owns `conn`; this does not connect, resume, or close."
  [conn artifact]
  (let [bank (named-bank conn "ram")
        pc (named-register conn "PC")
        segments (:segments artifact)
        entry (:entry-point artifact)]
    (when (nil? entry)
      (throw (ex-info "The linked artifact has no entry point"
                      {:reason :missing-entry-point})))
    (inject-segments! conn (:id bank) segments)
    (verify-entry! conn entry segments)
    {:entry-point entry
     :bank (:id bank)
     :pc-register (:id pc)
     :injection {:bank-id (:id bank)
                 :pc-register-id (:id pc)
                 :entry-point entry
                 :segments (mapv (fn [{:keys [start-address bytes]}]
                                   {:start-address start-address
                                    :length (alength ^bytes bytes)})
                                 segments)
                 :verified true}}))

(defn set-pc!
  "Set the program counter to `address`."
  [conn address]
  (let [pc (named-register conn "PC")]
    (bm/registers-set conn {:register-values [[(:id pc) address]]})))

(defn resume!
  "Resume a paused instance."
  [conn]
  (bm/resume conn))

(defn inject-and-resume!
  "Inject `artifact` and start it at its entry point."
  [conn artifact]
  (let [result (inject! conn artifact)]
    (set-pc! conn (:entry-point artifact))
    (resume! conn)
    result))

;; Loading and start policy --------------------------------------------------

(def program-entry-address
  "Address BASIC reaches when the autostarted program issues RUN."
  0xe144)

(def ^:private basic-program-address 0x0801)
(def ^:private basic-sys-token 0x9e)
(def ^:private basic-probe-bytes 128)
(def ^:private basic-max-lines 8)

(def default-autostart-timeout-ms 5000)
(def default-program-start-timeout-ms 30000)

(defn- fail
  [message data]
  (throw (ex-info message data)))

(defn- monitor-bytes
  "Read `[start end]` (inclusive) from CPU-visible memory through the monitor."
  ^bytes [conn start end]
  (:memory (bm/mem-get conn {:start start :end end :memspace 0})))

(defn- unsigned-byte
  [^bytes bytes index]
  (bit-and (int (aget bytes index)) 0xff))

(defn- little-endian-u16
  [^bytes bytes index]
  (bit-or (unsigned-byte bytes index)
          (bit-shift-left (unsigned-byte bytes (inc index)) 8)))

(defn- decimal-after
  "Parse the decimal operand at or after `start`.

  Skips spaces, and a parenthesized operand's opening `(` (BASIC accepts
  `SYS (2064)`). Returns nil when no digit is present before `limit`."
  [^bytes bytes start limit]
  (loop [index (long start)
         value nil]
    (if (< index (long limit))
      (let [byte (unsigned-byte bytes index)]
        (cond
          (or (= byte 0x20)
              (and (nil? value) (= byte 0x28))) (recur (inc index) value)
          (<= 0x30 byte 0x39)
          (recur (inc index) (+ (* (long (or value 0)) 10) (- byte 0x30)))
          :else value))
      value)))

(defn sys-target-in-text
  "Decimal operand of the first BASIC SYS token in `[start limit)`, or nil."
  [^bytes bytes start limit]
  (loop [index (long start)]
    (when (< index (long limit))
      (if (= basic-sys-token (unsigned-byte bytes index))
        (decimal-after bytes (inc index) limit)
        (recur (inc index))))))

(defn basic-sys-target
  "Return the machine-language entry of the autostarted BASIC stub, or nil.

  A PRG autostarted through VICE is usually a tokenized `SYS <address>` stub at
  `$0801`. Walking the program's line links and parsing the operand of the first
  SYS token yields the address of the program's first machine instruction."
  [conn]
  (let [bytes (monitor-bytes conn basic-program-address
                             (+ basic-program-address (dec basic-probe-bytes)))]
    (loop [offset 0
           lines 0]
      (if (or (> (+ offset 4) (alength bytes))
              (>= lines basic-max-lines))
        nil
        (let [link (little-endian-u16 bytes offset)]
          (if (zero? link)
            nil
            (let [text-start (+ offset 4)
                  next-offset (- link basic-program-address)
                  limit (if (and (>= next-offset text-start)
                                 (<= next-offset (alength bytes)))
                          next-offset
                          (alength bytes))]
              (or (sys-target-in-text bytes text-start limit)
                  (recur (long next-offset) (inc lines))))))))))

(defn- set-execute-checkpoint!
  [conn address]
  (bm/checkpoint-set conn {:start address
                           :end address
                           :stop? true
                           :enabled? true
                           :op bm/MON_CHECKPOINT_OP_EXECUTE
                           :temporary? false}))

(defn- wait-for-stop-at!
  [conn checkpoint-number address timeout-ms]
  (or (bm/await-event
       conn
       (fn [{:keys [response-type response]}]
         (and (= bm/MON_RESPONSE_CHECKPOINT_INFO response-type)
              (= checkpoint-number (:number response))
              (:hit? response)))
       timeout-ms)
      (fail "VICE did not reach the requested execution checkpoint"
            {:checkpoint-number checkpoint-number
             :address address}))
  (or (bm/await-event
       conn
       (fn [{:keys [response-type response]}]
         (and (= bm/MON_RESPONSE_STOPPED response-type)
              (= address (:pc response))))
       timeout-ms)
      (fail "VICE did not stop at the requested execution checkpoint"
            {:address address})))

(defn await-pc!
  "Install a temporary execute checkpoint at `address`, resume execution, and
  wait for the CPU to stop there, leaving VICE paused at `address`. VICE removes
  the temporary checkpoint on the hit; `checkpoint` records it so a caller can
  delete it when the wait fails beforehand."
  [conn address checkpoint timeout-ms]
  (let [installed (bm/checkpoint-set
                   conn
                   {:start address
                    :end address
                    :stop? true
                    :enabled? true
                    :op bm/MON_CHECKPOINT_OP_EXECUTE
                    :temporary? true})]
    (reset! checkpoint installed)
    (bm/resume conn)
    (wait-for-stop-at! conn (:number installed) address timeout-ms)
    (reset! checkpoint nil)
    address))

(defn reset-vector
  "The CPU reset entry read from the 6502 reset vector at `$FFFC`."
  [conn]
  (little-endian-u16 (monitor-bytes conn 0xfffc 0xfffd) 0))

(defn reset-to-pc!
  "Power-cycle the machine and stop the CPU at `address`.

  Install a temporary execute checkpoint, trigger a power cycle, and wait for
  the CPU to reach `address` so the initial snapshot is taken at the very start
  of the boot sequence. VICE leaves the monitor after a reset, so no explicit
  resume is needed; the checkpoint pauses the freshly reset machine."
  [conn address checkpoint timeout-ms]
  (let [installed (bm/checkpoint-set
                   conn
                   {:start address
                    :end address
                    :stop? true
                    :enabled? true
                    :op bm/MON_CHECKPOINT_OP_EXECUTE
                    :temporary? true})]
    (reset! checkpoint installed)
    (bm/reset conn {:what 1})
    (wait-for-stop-at! conn (:number installed) address timeout-ms)
    (reset! checkpoint nil)
    address))

(defn autostart!
  "Autostart `input` (a PRG/D64 path) and return when RUN is under way."
  [conn input options]
  (bm/autostart conn {:run-after-load? (if (contains? options :run-after-load?)
                                         (:run-after-load? options)
                                         true)
                      :file-index 0
                      :filename (str input)
                      :timeout-ms (or (:autostart-timeout-ms options)
                                      default-autostart-timeout-ms)}))

(defn prepare-program-start!
  "Autostart `input`, wait for the BASIC RUN stop at `$E144`, and return the
  backend start address: `:from-pc` when given, else the stub's `SYS` target,
  else `$E144` (already at the entry, so return nil).

  The autostart checkpoint is deleted on the way out. Throws when
  `:run-after-load?` is false, which the full-capture path needs."
  [conn input options]
  (when (false? (:run-after-load? options))
    (fail ":run-after-load? false requires :full-capture? true" {}))
  (let [checkpoint (atom nil)]
    (try
      (reset! checkpoint (set-execute-checkpoint! conn program-entry-address))
      (autostart! conn input options)
      (wait-for-stop-at! conn (:number @checkpoint) program-entry-address
                         (or (:program-start-timeout-ms options)
                             default-program-start-timeout-ms))
      (let [target (or (:from-pc options)
                       (basic-sys-target conn)
                       program-entry-address)]
        (when-not (= target program-entry-address)
          target))
      (finally
        (when-let [number (:number @checkpoint)]
          (try (bm/checkpoint-delete conn {:number number})
               (catch Throwable _ nil)))))))
