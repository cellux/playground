(ns omkamra.vice
  (:require
   [omkamra.vice.binary-monitor :as bm]))

(def default-executable "x64sc")
(def default-address bm/default-host)
(def default-port bm/default-port)

(def ^:private default-extra-args
  ["-remotemonitor"
   "-binarymonitor"
   "-sounddev"
   "alsa"])

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
  localhost and 6502. ALSA is selected as the default sound device; pass a
  different `-sounddev` option through `:extra-args` to override it.

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

(defn stop
  "Stop the subprocess referenced by a VICE instance descriptor."
  [{:keys [process pid] :as instance}]
  (validate-instance instance)
  (cond
    process (.destroy ^Process process)
    pid (let [handle (java.lang.ProcessHandle/of (long pid))]
          (when (.isPresent handle)
            (.destroy ^java.lang.ProcessHandle (.get handle))))
    :else (throw (IllegalArgumentException.
                  "VICE instance descriptor must contain :process or :pid")))
  nil)

(defn connect
  "Connect to the binary monitor described by `instance`.

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
