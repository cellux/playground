(ns omkamra.vice
  (:refer-clojure :exclude [run!])
  (:require
   [omkamra.vice.assembler :as assembler]
   [omkamra.vice.binary-monitor :as bm]))

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
    (let [{:keys [process pid]} runtime-or-instance]
      (cond
        process (when (.isAlive ^Process process)
                  (.destroy ^Process process))
        pid (let [handle (java.lang.ProcessHandle/of (long pid))]
              (when (.isPresent handle)
                (let [p (.get handle)]
                  (when (.isAlive p)
                    (.destroy p))))))))
  nil)

(defn stop
  "Compatibility wrapper around `stop!` for a plain VICE instance descriptor."
  [instance]
  (validate-instance instance)
  (stop! instance))

(defn start!
  "Canonical VICE process launcher. See `start`."
  ([] (start! {}))
  ([params] (start (or params {}))))

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

;; Runner --------------------------------------------------------------------

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

(defn- inject-segments!
  [conn bank-id segments]
  (doseq [{:keys [start-address bytes]} segments]
    (let [length (alength ^bytes bytes)]
      (when (pos? length)
        (bm/mem-set conn {:start start-address
                          :end (+ (long start-address) (dec length))
                          :bank bank-id
                          :data bytes})))))

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

(defn- inject-and-resume!
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
    (bm/registers-set conn {:register-values [[(:id pc) entry]]})
    (bm/resume conn)
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

(defn- run-connected!
  [instance artifact link-options run-options]
  (let [conn (connect! instance run-options)]
    (try
      (merge instance
             {:artifact artifact
              :link-options link-options
              :run-options run-options}
             (inject-and-resume! conn artifact)
             {:connection conn})
      (catch Throwable error
        (try
          (bm/close conn)
          (catch Throwable _ nil))
        (throw error)))))

(defn run!
  "Link (if needed), inject, and start a program on a fresh VICE instance.

  Given an artifact, it runs directly with `options` as run options. Given a
  relocatable object, it links it first using `:link-options` and then runs it
  using `:run-options`. Returns a runtime map extending the instance
  descriptor with the monitor connection, linked artifact, entry point, bank
  and register discovery, and injection metadata.

  On any startup, injection, or execution-setup failure the monitor connection
  is closed and the process is terminated."
  ([target]
   (run! target {}))
  ([target options]
   (let [object? (assembler/object? target)
         link-options (if object? (:link-options options) {})
         run-options (if object? (:run-options options) options)
         artifact (if object?
                    (assembler/link target link-options)
                    target)]
     (when-not (assembler/artifact? artifact)
       (throw (IllegalArgumentException.
               "run! expects a relocatable object or a linked artifact")))
     (let [instance (start! (select-keys run-options
                                         [:executable :address :port
                                          :extra-args :options]))]
       (try
         (run-connected! instance artifact link-options run-options)
         (catch Throwable error
           (stop! instance)
           (throw error)))))))
