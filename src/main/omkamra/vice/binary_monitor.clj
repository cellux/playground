(ns omkamra.vice.binary-monitor
  (:import
   [java.net Socket SocketException]
   [java.nio ByteBuffer]
   [java.nio.charset StandardCharsets]))

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
  [in]
  (.read in))

(defn read-short
  [in]
  (let [lo (.read in)
        hi (.read in)]
    (+ (bit-shift-left hi 8) lo)))

(defn read-int
  [in]
  (let [lo (read-short in)
        hi (read-short in)]
    (+ (bit-shift-left hi 16) lo)))

(defn read-bytes
  [in length]
  (let [buf (byte-array length)]
    (.readNBytes in buf 0 length)
    buf))

(defn write-byte
  [out val]
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
  [host port handle-event]
  (let [socket (Socket. host port)
        in (.getInputStream socket)
        out (.getOutputStream socket)
        pending-requests (atom {})
        response-reader (future
                          (try
                            (while (not (.isInputShutdown socket))
                              (let [stx (read-byte in)
                                    _ (assert (= stx 0x02) "bad response")
                                    api-version (read-byte in)
                                    len (read-int in)
                                    response-type (read-byte in)
                                    error-code (read-byte in)
                                    _ (assert (zero? error-code) (format "error %d" error-code))
                                    request-id (read-int in)
                                    ;; _ (println (format "request_id: %x got response of type: %02x length: %d" request-id response-type len))
                                    response (read-response response-type in)]
                                (if (= request-id 0xffffffff)
                                  (handle-event response-type response)
                                  (let [request-promise (get @pending-requests request-id)]
                                    (assert (and request-promise
                                                 (not (realized? request-promise)))
                                            "got response for unknown request")
                                    (swap! pending-requests dissoc request-id)
                                    (deliver request-promise response)))))
                            (catch SocketException e)
                            (catch Throwable t
                              (.close socket)
                              (println "caught throwable:" t))))]
    {:host host
     :port port
     :socket socket
     :in in
     :out out
     :pending-requests pending-requests
     :response-reader response-reader
     :next-request-id (atom 0)}))

(defn close
  [conn]
  (.close (:socket conn))
  (reset! (:pending-requests conn) {})
  (reset! (:next-request-id conn) 0)
  nil)

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
  [conn cmd body-sig body-args]
  (assert (= (count body-sig) (count body-args)))
  (let [{:keys [out next-request-id pending-requests]} conn
        request-id (swap! next-request-id (comp #(mod % 0x100000000) inc))
        request-promise (promise)]
    (swap! pending-requests assoc request-id request-promise)
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
        \b (.write out arg 0 (count arg))))
    (.flush out)
    @request-promise))

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
  [conn]
  (throw (ex-info "not implemented" {:command MON_CMD_CHECKPOINT_LIST})))

(defmethod read-response MON_RESPONSE_CHECKPOINT_INFO
  [_ in]
  {:number (read-int in)
   :hit? (not (zero? (read-byte in)))
   :start (read-short in)
   :end (read-short in)
   :stop? (not (zero? (read-byte in)))
   :enabled? (not (zero? (read-byte in)))
   :op (read-byte in)
   :temporary? (not (zero? (read-byte in)))
   :hit-count (read-int in)
   :ignore-count (read-int in)
   :has-condition? (not (zero? (read-byte in)))
   :memspace (read-byte in)})

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
  (let [buf (ByteBuffer/allocate (* 4 (count register-values)))]
    (doseq [[id value] register-values]
      (.put buf 3)
      (.put buf id)
      (.putShort buf value))
    (send-request conn MON_CMD_REGISTERS_SET "12b" [(or memspace 0) (count register-values) buf])))

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
  (throw (ex-info "not implemented" {:command MON_CMD_RESOURCE_SET})))

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
  (throw (ex-info "not implemented" {:command MON_CMD_DISPLAY_GET})))

(defmethod read-response MON_RESPONSE_DISPLAY_GET
  [_ in]
  (throw (ex-info "unimplemented response type"
                  {:response-type MON_RESPONSE_DISPLAY_GET})))

(defn vice-info
  [conn]
  (send-request conn MON_CMD_VICE_INFO "" []))

(defmethod read-response MON_RESPONSE_VICE_INFO
  [_ in]
  (let [main-version (read-sized-bytes in)
        svn-revision (read-sized-integer in)]
    {:main-version main-version
     :svn-revision svn-revision}))

(defn palette-get
  [conn {:keys [use-vic-ii?]}]
  (throw (ex-info "not implemented" {:command MON_CMD_PALETTE_GET})))

(defmethod read-response MON_RESPONSE_PALETTE_GET
  [_ in]
  (throw (ex-info "unimplemented response type"
                  {:response-type MON_RESPONSE_PALETTE_GET})))

(defn joyport-set
  [conn {:keys [port value]}]
  (send-request conn MON_CMD_JOYPORT_SET "22" [port value]))

(defn userport-set
  [conn {:keys [value]}]
  (send-request conn MON_CMD_USERPORT_SET "2" [value]))

(defn exit
  [conn]
  (send-request conn MON_CMD_EXIT "" []))

(defn quit
  [conn]
  (send-request conn MON_CMD_QUIT "" []))

(defn reset
  [conn {:keys [what]}]
  (send-request conn MON_CMD_RESET "1" [(or what 1)]))

(defn autostart
  [conn {:keys [run-after-load? file-index filename]}]
  (let [filename-bytes (-> bytes filename)]
    (send-request conn MON_CMD_AUTOSTART "121b"
                  [run-after-load?
                   file-index
                   (count filename-bytes)
                   filename-bytes])))

(defmethod read-response MON_RESPONSE_JAM
  [_ in]
  {:pc (read-short in)})

(defmethod read-response MON_RESPONSE_STOPPED
  [_ in]
  {:pc (read-short in)})

(defmethod read-response MON_RESPONSE_RESUMED
  [_ in]
  {:pc (read-short in)})
