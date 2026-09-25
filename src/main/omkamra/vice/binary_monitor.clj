(ns omkamra.vice.binary-monitor
  (:require [clojure.java.io :as io])
  (:import
   [java.awt.image BufferedImage]
   [java.io ByteArrayInputStream InputStream OutputStream]
   [java.net Socket SocketException]
   [java.nio.charset StandardCharsets]
   [java.util.concurrent LinkedBlockingQueue TimeUnit]
   [javax.imageio ImageIO]))

(def vice-api-version 0x02)

(def default-host "localhost")
(def default-port 6502)

;; monitor commands

(def MON_CMD_MEM_GET 0x01)
(def MON_CMD_MEM_SET 0x02)

(def MON_CMD_CHECKPOINT_GET 0x11)
(def MON_CMD_CHECKPOINT_SET 0x12)
(def MON_CMD_CHECKPOINT_DELETE 0x13)
(def MON_CMD_CHECKPOINT_LIST 0x14)
(def MON_CMD_CHECKPOINT_TOGGLE 0x15)

(def MON_CMD_CONDITION_SET 0x22)

(def MON_CMD_REGISTERS_GET 0x31)
(def MON_CMD_REGISTERS_SET 0x32)

(def MON_CMD_DUMP 0x41)
(def MON_CMD_UNDUMP 0x42)

(def MON_CMD_RESOURCE_GET 0x51)
(def MON_CMD_RESOURCE_SET 0x52)

(def MON_CMD_ADVANCE_INSTRUCTIONS 0x71)
(def MON_CMD_KEYBOARD_FEED 0x72)
(def MON_CMD_EXECUTE_UNTIL_RETURN 0x73)

(def MON_CMD_PING 0x81)
(def MON_CMD_BANKS_AVAILABLE 0x82)
(def MON_CMD_REGISTERS_AVAILABLE 0x83)
(def MON_CMD_DISPLAY_GET 0x84)
(def MON_CMD_VICE_INFO 0x85)
(def MON_CMD_CPUHISTORY_GET 0x86)

(def MON_CMD_PALETTE_GET 0x91)

(def MON_CMD_JOYPORT_SET 0xa2)

(def MON_CMD_USERPORT_SET 0xb2)

(def MON_CMD_EXIT 0xaa)
(def MON_CMD_QUIT 0xbb)
(def MON_CMD_RESET 0xcc)
(def MON_CMD_AUTOSTART 0xdd)

;; monitor command responses

(def MON_RESPONSE_MEM_GET 0x01)
(def MON_RESPONSE_MEM_SET 0x02)

(def MON_RESPONSE_CHECKPOINT_INFO 0x11)

(def MON_RESPONSE_CHECKPOINT_DELETE 0x13)
(def MON_RESPONSE_CHECKPOINT_LIST 0x14)
(def MON_RESPONSE_CHECKPOINT_TOGGLE 0x15)

(def MON_RESPONSE_CONDITION_SET 0x22)

(def MON_RESPONSE_REGISTER_INFO 0x31)

(def MON_RESPONSE_DUMP 0x41)
(def MON_RESPONSE_UNDUMP 0x42)

(def MON_RESPONSE_RESOURCE_GET 0x51)
(def MON_RESPONSE_RESOURCE_SET 0x52)

(def MON_RESPONSE_JAM 0x61)
(def MON_RESPONSE_STOPPED 0x62)
(def MON_RESPONSE_RESUMED 0x63)

(def MON_RESPONSE_ADVANCE_INSTRUCTIONS 0x71)
(def MON_RESPONSE_KEYBOARD_FEED 0x72)
(def MON_RESPONSE_EXECUTE_UNTIL_RETURN 0x73)

(def MON_RESPONSE_PING 0x81)
(def MON_RESPONSE_BANKS_AVAILABLE 0x82)
(def MON_RESPONSE_REGISTERS_AVAILABLE 0x83)
(def MON_RESPONSE_DISPLAY_GET 0x84)
(def MON_RESPONSE_VICE_INFO 0x85)
(def MON_RESPONSE_CPUHISTORY_GET 0x86)

(def MON_RESPONSE_PALETTE_GET 0x91)

(def MON_RESPONSE_JOYPORT_SET 0xa2)

(def MON_RESPONSE_USERPORT_SET 0xb2)

(def MON_RESPONSE_EXIT 0xaa)
(def MON_RESPONSE_QUIT 0xbb)
(def MON_RESPONSE_RESET 0xcc)
(def MON_RESPONSE_AUTOSTART 0xdd)

(defmulti read-response
  (fn [response-type in] response-type))

(defn read-byte
  [^InputStream in]
  (let [value (.read in)]
    (if (neg? value)
      (throw (ex-info "Unexpected end of VICE monitor stream" {}))
      value)))

(defn read-short
  [in]
  (let [lo (read-byte in)
        hi (read-byte in)]
    (+ (bit-shift-left hi 8) lo)))

(defn read-int
  [in]
  (let [lo (read-short in)
        hi (read-short in)]
    (+ (bit-shift-left hi 16) lo)))

(defn read-long
  [in]
  (let [lo (long (read-int in))
        hi (long (read-int in))]
    (bit-or lo (bit-shift-left hi 32))))

(defn read-bytes
  [^InputStream in length]
  (let [buf (byte-array length)]
    (loop [offset 0]
      (when (< offset length)
        (let [read (.read in buf offset (- length offset))]
          (when (neg? read)
            (throw (ex-info "Unexpected end of VICE monitor stream"
                            {:expected length
                             :read offset})))
          (recur (+ offset read)))))
    buf))

(defn write-byte
  [^OutputStream out val]
  (.write out val))

(defn write-short
  [out val]
  (.write out (bit-and val 0xff))
  (.write out (bit-and (bit-shift-right val 8) 0xff)))

(defn write-int
  [out val]
  (write-short out (bit-and val 0xffff))
  (write-short out (bit-and (bit-shift-right val 16) 0xffff)))

(defn connect
  "Connect to VICE's binary monitor.

  `opts/:ignored-unsolicited-types` suppresses selected high-volume event
  types before body decoding and queueing. Request responses of the same type
  are never suppressed. The set can be changed through
  `ignore-unsolicited-types!` on the returned connection."
  ([host port handle-event]
   (connect host port handle-event {}))
  ([host port handle-event {:keys [ignored-unsolicited-types]
                            :or {ignored-unsolicited-types #{}}}]
  (let [socket (Socket. host port)
        in (.getInputStream socket)
        out (.getOutputStream socket)
        pending-requests (atom {})
        write-lock (Object.)
        events (LinkedBlockingQueue.)
        ignored-unsolicited-types (atom (set ignored-unsolicited-types))
        response-reader
        (future
          (try
            (while (not (.isInputShutdown socket))
              (let [stx (read-byte in)
                    _ (assert (= stx 0x02) "bad response")
                    api-version (read-byte in)
                    len (read-int in)
                    response-type (read-byte in)
                    error-code (read-byte in)
                    request-id (read-int in)
                    ;; _ (println (format "request_id: %x got response of type: %02x length: %d" request-id response-type len))
                    body (read-bytes in len)
                    unsolicited? (= request-id 0xffffffff)
                    ignored? (and unsolicited?
                                  (contains? @ignored-unsolicited-types
                                             response-type))]
                (when-not ignored?
                  (let [response (if (zero? error-code)
                                   (read-response response-type
                                                  (ByteArrayInputStream. body))
                                   {:error-code error-code
                                    :response-type response-type})]
                    (if unsolicited?
                      (let [event {:response-type response-type
                                   :response response}]
                        (.offer events event)
                        (handle-event response-type response))
                      (let [request-promise (get @pending-requests request-id)]
                        (if request-promise
                          (do
                            (swap! pending-requests dissoc request-id)
                            (deliver request-promise response))
                          (.offer events {:response-type response-type
                                          :response response
                                          :request-id request-id}))))))))
            (catch SocketException _)
            (catch Throwable t
              (.close socket)
              (println "caught throwable:" t))))]
    {:host host
     :port port
     :socket socket
     :in in
     :out out
     :pending-requests pending-requests
     :write-lock write-lock
     :events events
     :ignored-unsolicited-types ignored-unsolicited-types
     :response-reader response-reader
     :next-request-id (atom 0)})))

(defn ignore-unsolicited-types!
  "Replace the set of unsolicited response types discarded by `connect`.

  Suppression happens before response-body decoding and queueing, making this
  suitable for checkpoint-hit floods produced by non-stopping tracepoints."
  [conn response-types]
  (reset! (:ignored-unsolicited-types conn) (set response-types)))

(defn ignored-unsolicited-types
  [conn]
  @(:ignored-unsolicited-types conn))

(defn close
  [conn]
  (.close (:socket conn))
  (reset! (:pending-requests conn) {})
  (reset! (:next-request-id conn) 0)
  (some-> (:events conn) .clear)
  nil)

(defn await-event
  "Wait up to `timeout-ms` for an unsolicited monitor event matching `pred`.

  Events not matching `pred` are retained in encounter order. Returns the
  event map (`:response-type`, `:response`) or nil after the timeout."
  ([conn timeout-ms]
   (await-event conn (constantly true) timeout-ms))
  ([{:keys [events]} pred timeout-ms]
   (let [deadline (+ (System/nanoTime) (* (long timeout-ms) 1000000))
         skipped (transient [])]
     (try
       (loop []
         (let [remaining (quot (- deadline (System/nanoTime)) 1000000)]
           (when (not (neg? remaining))
             (when-let [event (.poll ^LinkedBlockingQueue events
                                     (long remaining)
                                     TimeUnit/MILLISECONDS)]
               (if (pred event)
                 event
                 (do (conj! skipped event)
                     (recur)))))))
       (finally
         (doseq [event (persistent! skipped)]
           (.offer ^LinkedBlockingQueue events event)))))))

(defn drain-events
  "Remove and return all queued unsolicited monitor events."
  [{:keys [events]}]
  (loop [result []]
    (if-let [event (.poll ^LinkedBlockingQueue events)]
      (recur (conj result event))
      result)))

(defmethod read-response :default
  [_ in]
  {})

(defn body-length
  [sig args]
  (reduce + (map (fn [[code arg]]
                   (case code
                     \1 1
                     \2 2
                     \4 4
                     \b (count arg)))
                 (map vector sig args))))

(defn send-request
  "Send one monitor request.

  VICE enters the monitor while handling requests, so callers that want the
  emulator to continue must make `exit`/`resume` the final request. The
  optional fifth argument controls how long to wait for a response."
  ([conn cmd body-sig body-args]
   (send-request conn cmd body-sig body-args 1000))
  ([conn cmd body-sig body-args response-timeout-ms]
   (assert (= (count body-sig) (count body-args)))
   (let [{:keys [out next-request-id pending-requests write-lock]} conn
        request-id (swap! next-request-id (comp #(mod % 0x100000000) inc))
        request-promise (promise)]
    (swap! pending-requests assoc request-id request-promise)
    (locking write-lock
      (write-byte out 0x02)
      (write-byte out vice-api-version)
      (write-int out (body-length body-sig body-args))
      (write-int out request-id)
      (write-byte out cmd)
      (doseq [[code arg] (map vector body-sig body-args)]
        (case code
          \1 (write-byte out (cond (nil? arg) 0
                                   (boolean? arg) (if arg 1 0)
                                   :else arg))
          \2 (write-short out arg)
          \4 (write-int out arg)
          \b (.write out arg 0 (alength ^bytes arg))))
      (.flush out))
    (let [rv (deref request-promise response-timeout-ms nil)]
      (when (nil? rv)
        (swap! pending-requests dissoc request-id))
      (if (and (map? rv) (:error-code rv))
        (throw (ex-info "VICE monitor command failed" rv))
        rv)))))

(defn mem-get
  [conn {:keys [side-effects? start end memspace bank]}]
  (send-request conn MON_CMD_MEM_GET
                "12212"
                [(if side-effects? 1 0)
                 start
                 end
                 (or memspace 0)
                 (or bank 0)]))

(defmethod read-response MON_RESPONSE_MEM_GET
  [_ in]
  (let [length (read-short in)
        length (if (zero? length) 65536 length)
        memory (read-bytes in length)]
    {:length length
     :memory memory}))

(defn mem-set
  [conn {:keys [side-effects? start end memspace bank data]}]
  (send-request conn MON_CMD_MEM_SET
                "12212b"
                [side-effects?
                 start
                 end
                 (or memspace 0)
                 (or bank 0)
                 data]))

(defn checkpoint-get
  [conn {:keys [number]}]
  (send-request conn MON_CMD_CHECKPOINT_GET "4" [number]))

(defn checkpoint-set
  [conn {:keys [start end stop? enabled? op temporary? memspace]}]
  (send-request conn MON_CMD_CHECKPOINT_SET
                "2211111"
                [start
                 end
                 stop?
                 enabled?
                 op
                 temporary?
                 (or memspace 0)]))

(defn checkpoint-delete
  [conn {:keys [number]}]
  (send-request conn MON_CMD_CHECKPOINT_DELETE "4" [number]))

(defn checkpoint-list
  "Return all checkpoints and their current monitor metadata.

  VICE sends one unsolicited CHECKPOINT_INFO response per checkpoint before
  the final CHECKPOINT_LIST response, so this function collects both parts."
  [conn]
  (drain-events conn)
  (let [first-response (send-request conn MON_CMD_CHECKPOINT_LIST "" [])
        queued (drain-events conn)
        summary (or (when (contains? first-response :count)
                      first-response)
                    (some #(when (= MON_RESPONSE_CHECKPOINT_LIST
                                    (:response-type %))
                             (:response %))
                          queued)
                    {:count 0})
        first-info (when-not (contains? first-response :count)
                     [first-response])
        checkpoints (->> (concat first-info queued)
                         (filter #(= MON_RESPONSE_CHECKPOINT_INFO
                                      (:response-type %)))
                         (map :response)
                         vec)]
    (assoc summary :checkpoints checkpoints)))

(defn checkpoint-delete-all
  "Delete every checkpoint currently known to VICE and return its metadata.

  This is intentionally explicit because non-stopping tracepoints continue
  generating monitor events and can otherwise survive a failed capture."
  [conn]
  (let [checkpoints (:checkpoints (checkpoint-list conn))]
    (doseq [{:keys [number]} checkpoints]
      (checkpoint-delete conn {:number number}))
    checkpoints))

(defmethod read-response MON_RESPONSE_CHECKPOINT_INFO
  [_ in]
  ;; Bind sequentially: reading a binary response directly inside a map
  ;; literal does not guarantee evaluation order, which would desynchronize
  ;; this stream parser.
  (let [number (read-int in)
        hit? (not (zero? (read-byte in)))
        start (read-short in)
        end (read-short in)
        stop? (not (zero? (read-byte in)))
        enabled? (not (zero? (read-byte in)))
        op (read-byte in)
        temporary? (not (zero? (read-byte in)))
        hit-count (read-int in)
        ignore-count (read-int in)
        has-condition? (not (zero? (read-byte in)))
        memspace (read-byte in)]
    {:number number
     :hit? hit?
     :start start
     :end end
     :stop? stop?
     :enabled? enabled?
     :op op
     :temporary? temporary?
     :hit-count hit-count
     :ignore-count ignore-count
     :has-condition? has-condition?
     :memspace memspace}))

(defmethod read-response MON_RESPONSE_CHECKPOINT_LIST
  [_ in]
  {:count (read-int in)})

(defn checkpoint-toggle
  [conn {:keys [number enabled?]}]
  (send-request conn MON_CMD_CHECKPOINT_TOGGLE "41" [number enabled?]))

(defn ->bytes
  [x]
  (if (bytes? x)
    x
    (.getBytes x StandardCharsets/US_ASCII)))

(defn condition-set
  [conn {:keys [number condition-str]}]
  (let [bytes (->bytes condition-str)]
    (assert (< (count bytes) 256))
    (send-request conn MON_CMD_CONDITION_SET "41b" [number (count bytes) bytes])))

(defn registers-get
  ([conn {:keys [memspace]}]
   (send-request conn MON_CMD_REGISTERS_GET "1" [(or memspace 0)]))
  ([conn]
   (registers-get conn {:memspace 0})))

(defmethod read-response MON_RESPONSE_REGISTER_INFO
  [_ in]
  (let [register-count (read-short in)]
    (loop [remaining register-count
           result {}]
      (if (zero? remaining)
        result
        (let [size (read-byte in)
              register-id (read-byte in)
              register-value (read-short in)]
          (assert (= size 3))
          (recur (dec remaining)
                 (assoc result register-id
                        {:id register-id
                         :value register-value})))))))

(defn registers-set
  [conn {:keys [memspace register-values]}]
  (let [data (byte-array (* 4 (count register-values)))]
    (doseq [[index [id value]] (map-indexed vector register-values)
            :let [offset (* index 4)]]
      (aset-byte data offset (unchecked-byte 3))
      (aset-byte data (inc offset) (unchecked-byte id))
      (aset-byte data (+ offset 2) (unchecked-byte value))
      (aset-byte data (+ offset 3)
                 (unchecked-byte (bit-shift-right value 8))))
    (send-request conn MON_CMD_REGISTERS_SET "12b"
                  [(or memspace 0) (count register-values) data])))

(defn dump
  [conn {:keys [save-roms? save-disks? filename]}]
  (let [filename-bytes (->bytes filename)]
    (assert (< (count filename-bytes) 256))
    (send-request conn MON_CMD_DUMP "111b"
                  [save-roms?
                   save-disks?
                   (count filename-bytes)
                   filename-bytes])))

(defn undump
  [conn {:keys [filename]}]
  (let [filename-bytes (->bytes filename)]
    (assert (< (count filename-bytes) 256))
    (send-request conn MON_CMD_UNDUMP "1b"
                  [(count filename-bytes)
                   filename-bytes])))

(defmethod read-response MON_RESPONSE_UNDUMP
  [_ in]
  {:pc (read-short in)})

(defn resource-get
  [conn {:keys [name]}]
  (let [name-bytes (->bytes name)]
    (assert (< (count name-bytes) 256))
    (send-request conn MON_CMD_RESOURCE_GET "1b"
                  [(count name-bytes)
                   name-bytes])))

(defn resource-set
  [conn {:keys [name value]}]
  (let [name-bytes (->bytes name)
        [value-type value-bytes]
        (cond
          (string? value)
          [0 (->bytes value)]

          (integer? value)
          (let [bytes (byte-array 4)]
            (dotimes [offset 4]
              (aset-byte bytes offset
                         (unchecked-byte
                          (bit-shift-right (long value) (* offset 8)))))
            [1 bytes])

          :else
          (throw (IllegalArgumentException.
                  ":value must be a string or integer")))]
    (when-not (< (count name-bytes) 256)
      (throw (IllegalArgumentException. ":name is too long")))
    (when-not (< (count value-bytes) 256)
      (throw (IllegalArgumentException. ":value is too long")))
    (send-request conn MON_CMD_RESOURCE_SET "11b1b"
                  [value-type
                   (count name-bytes)
                   name-bytes
                   (count value-bytes)
                   value-bytes])))

(defn read-sized-bytes
  [in]
  (let [length (read-byte in)]
    (read-bytes in length)))

(defn read-sized-string
  [in]
  (String. (read-sized-bytes in) StandardCharsets/US_ASCII))

(defn read-sized-integer
  [in]
  (let [length (read-byte in)]
    (case length
      1 (read-byte in)
      2 (read-short in)
      4 (read-int in))))

(defmethod read-response MON_RESPONSE_RESOURCE_GET
  [_ in]
  (let [type (read-byte in)]
    (case type
      0 {:value (read-sized-string in)}
      1 {:value (read-sized-integer in)})))

(defn advance-instructions
  [conn {:keys [step-over? count]}]
  (send-request conn MON_CMD_ADVANCE_INSTRUCTIONS "12" [step-over? count]))

(defn advance-and-wait
  "Advance instructions from a paused monitor and wait for its next stop event.

  `opts` accepts `:count`, `:step-over?`, and optional `:timeout-ms` (default
  1000). It returns the stopped-event response or throws on timeout."
  [conn {:keys [timeout-ms] :as opts}]
  (advance-instructions conn opts)
  (let [event (await-event conn #(= MON_RESPONSE_STOPPED (:response-type %))
                           (or timeout-ms 1000))]
    (or (:response event)
        (throw (ex-info "VICE did not stop after advancing instructions"
                        {:timeout-ms (or timeout-ms 1000)
                         :options (dissoc opts :timeout-ms)})))))

(defn keyboard-feed
  [conn {:keys [text]}]
  (let [text-bytes (->bytes text)]
    (assert (< (count text-bytes) 256))
    (send-request conn MON_CMD_KEYBOARD_FEED "1b"
                  [(count text-bytes)
                   text-bytes])))

(defn execute-until-return
  [conn]
  (send-request conn MON_CMD_EXECUTE_UNTIL_RETURN "" []))

(defn ping
  [conn]
  (send-request conn MON_CMD_PING "" []))

(defn banks-available
  [conn]
  (send-request conn MON_CMD_BANKS_AVAILABLE "" []))

(defmethod read-response MON_RESPONSE_BANKS_AVAILABLE
  [_ in]
  (let [bank-count (read-short in)]
    (loop [remaining bank-count
           result {}]
      (if (zero? remaining)
        result
        (let [size (read-byte in)
              bank-id (read-short in)
              name (read-sized-string in)]
          (assert (= size (+ 3 (count name))))
          (recur (dec remaining)
                 (assoc result bank-id
                        {:id bank-id
                         :name name})))))))

(defn registers-available
  ([conn {:keys [memspace]}]
   (send-request conn MON_CMD_REGISTERS_AVAILABLE "1" [(or memspace 0)]))
  ([conn]
   (registers-available conn {:memspace 0})))

(defmethod read-response MON_RESPONSE_REGISTERS_AVAILABLE
  [_ in]
  (let [register-count (read-short in)]
    (loop [remaining register-count
           result {}]
      (if (zero? remaining)
        result
        (let [size (read-byte in)
              register-id (read-byte in)
              register-size (read-byte in)
              name (read-sized-string in)]
          (assert (= size (+ 3 (count name))))
          (recur (dec remaining)
                 (assoc result register-id
                        {:id register-id
                         :size register-size
                         :name name})))))))

(defn display-get
  [conn {:keys [use-vic-ii? format]}]
  (send-request conn MON_CMD_DISPLAY_GET "11" [use-vic-ii? format]))

(defmethod read-response MON_RESPONSE_DISPLAY_GET
  [_ in]
  (let [header-length (read-int in)
        debug-width (read-short in)
        debug-height (read-short in)
        x-offset (read-short in)
        y-offset (read-short in)
        inner-width (read-short in)
        inner-height (read-short in)
        bpp (read-byte in)
        buffer-length (read-int in)
        available (.available in)
        raw-buffer (read-bytes in available)
        buffer (byte-array buffer-length)]
    (System/arraycopy raw-buffer 0 buffer 0
                      (min buffer-length (alength ^bytes raw-buffer)))
    {:header-length header-length
     :debug-width debug-width
     :debug-height debug-height
     :x-offset x-offset
     :y-offset y-offset
     :inner-width inner-width
     :inner-height inner-height
     :bpp bpp
     :buffer-length buffer-length
     :buffer buffer}))

(defn vice-info
  [conn]
  (send-request conn MON_CMD_VICE_INFO "" []))

(defmethod read-response MON_RESPONSE_VICE_INFO
  [_ in]
  (let [main-version (read-sized-bytes in)
        svn-revision (read-sized-integer in)]
    {:main-version main-version
     :svn-revision svn-revision}))

(defmethod read-response MON_RESPONSE_CPUHISTORY_GET
  [_ in]
  (let [count (read-int in)]
    {:entries
     (loop [remaining count
            entries []]
       (if (zero? remaining)
         entries
         (let [_item-size (read-byte in)
               register-count (read-short in)
               registers (loop [registers-left register-count
                                result {}]
                           (if (zero? registers-left)
                             result
                             (let [_register-size (read-byte in)
                                   id (read-byte in)
                                   value (read-short in)]
                               (recur (dec registers-left)
                                      (assoc result id {:id id :value value})))))]
           (let [cycle (read-long in)
                 instruction-length (read-byte in)
                 bytes (read-bytes in instruction-length)
                 entry {:registers registers
                        :cycle cycle
                        :bytes (mapv #(bit-and (int %) 0xff) bytes)}]
             (recur (dec remaining) (conj entries entry))))))}))

(defn cpuhistory-get
  "Return recent CPU-history entries, oldest first.

  VICE limits the result to the history buffer currently configured in the
  emulator (8192 entries by default in this build)."
  [conn count-or-options]
  (let [{:keys [count memspace]}
        (if (map? count-or-options)
          count-or-options
          {:count count-or-options})]
    (when-not (pos-int? count)
      (throw (IllegalArgumentException. ":count must be positive")))
    (send-request conn MON_CMD_CPUHISTORY_GET "14"
                  [(or memspace 0) count])))

(defn palette-get
  [conn {:keys [use-vic-ii?]}]
  (send-request conn MON_CMD_PALETTE_GET "1" [use-vic-ii?]))

(defmethod read-response MON_RESPONSE_PALETTE_GET
  [_ in]
  (let [num-items (read-short in)]
    (loop [remaining num-items
           result (vector)]
      (if (zero? remaining)
        result
        (recur (dec remaining)
               (let [item-size (read-byte in)]
                 (assert (= item-size 3))
                 (let [r (read-byte in)
                       g (read-byte in)
                       b (read-byte in)]
                   (conj result [r g b]))))))))

(declare resume)

(defn screenshot!
  "Capture the VICE display as a PNG and return `output-file`.

  The monitor is paused while the framebuffer and palette are read. By
  default, `:resume? true` resumes emulation after the file is written.
  Options are `:use-vic-ii?` and `:resume?`."
  ([conn output-file]
   (screenshot! conn output-file {}))
  ([conn output-file {:keys [use-vic-ii? resume?]
                      :or {use-vic-ii? true
                           resume? true}}]
   (try
     (let [{:keys [debug-width debug-height buffer]}
           (display-get conn {:use-vic-ii? use-vic-ii? :format 0})
           palette (palette-get conn {:use-vic-ii? use-vic-ii?})
           width (int debug-width)
           height (int debug-height)
           expected (* width height)]
       (when-not (and (pos? width) (pos? height))
         (throw (ex-info "VICE returned an invalid display size"
                         {:width width :height height})))
       (when (< (alength ^bytes buffer) expected)
         (throw (ex-info "VICE returned a short display buffer"
                         {:expected expected
                          :actual (alength ^bytes buffer)})))
       (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
         (dotimes [y height]
           (dotimes [x width]
             (let [index (bit-and (int (aget ^bytes buffer (+ x (* y width)))) 0xff)
                   [r g b] (get palette index [0 0 0])
                   rgb (bit-or (bit-shift-left (int r) 16)
                               (bit-shift-left (int g) 8)
                               (int b))]
               (.setRGB image x y rgb))))
         (when-not (ImageIO/write image "png" (io/file output-file))
           (throw (ex-info "No PNG writer is available" {:output-file output-file}))))
       output-file)
     (finally
       (when resume?
         (resume conn))))))

(defn joyport-set
  [conn {:keys [port value]}]
  (send-request conn MON_CMD_JOYPORT_SET "22" [port value]))

(defn userport-set
  [conn {:keys [value]}]
  (send-request conn MON_CMD_USERPORT_SET "2" [value]))

(defn exit
  "Resume emulation; this should normally be the final monitor request."
  [conn]
  (send-request conn MON_CMD_EXIT "" []))

(defn resume
  "Alias for `exit`, named for the effect on the emulator."
  [conn]
  (exit conn))

(defn quit
  [conn]
  (send-request conn MON_CMD_QUIT "" []))

(defn reset
  ([conn]
   (reset conn {:what 1}))
  ([conn {:keys [what]}]
   (send-request conn MON_CMD_RESET "1" [what])))

(defn autostart
  "Autostart a file and wait for VICE to leave the monitor.

  VICE's AUTOSTART command is special: it schedules the load/run operation
  and resumes emulation itself. Callers must not follow this with `resume` or
  `exit`. Waiting for the unsolicited RESUMED event avoids racing that
  transition, which can otherwise cancel or corrupt an autostart."
  [conn {:keys [run-after-load? file-index filename timeout-ms]
         :or {file-index 0
              timeout-ms 5000}}]
  (let [filename-bytes (->bytes filename)]
    ;; Discard state-transition events from the preceding monitor session so
    ;; the RESUMED event below belongs to this autostart request.
    (drain-events conn)
    (let [response (send-request conn MON_CMD_AUTOSTART "121b"
                                 [run-after-load?
                                  file-index
                                  (count filename-bytes)
                                  filename-bytes]
                                 timeout-ms)]
      (when-not (await-event conn
                             #(= MON_RESPONSE_RESUMED (:response-type %))
                             timeout-ms)
        (throw (ex-info "VICE did not resume after autostart"
                        {:filename filename
                         :timeout-ms timeout-ms})))
      response)))

(defmethod read-response MON_RESPONSE_JAM
  [_ in]
  {:pc (read-short in)})

(defmethod read-response MON_RESPONSE_STOPPED
  [_ in]
  {:pc (read-short in)})

(defmethod read-response MON_RESPONSE_RESUMED
  [_ in]
  {:pc (read-short in)})
