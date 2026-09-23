(ns omkamra.supercollider.synth
  (:require [omkamra.osc :as osc])
  (:refer-clojure :exclude [sync]))

(def ^:private scsynth-options
  [[:udp-port "-u" :value]
   [:tcp-port "-t" :value]
   [:bind-address "-B" :value]
   [:control-bus-channels "-c" :value]
   [:audio-bus-channels "-a" :value]
   [:input-bus-channels "-i" :value]
   [:output-bus-channels "-o" :value]
   [:block-size "-z" :value]
   [:hardware-buffer-size "-Z" :value]
   [:hardware-sample-rate "-S" :value]
   [:sample-buffers "-b" :value]
   [:max-nodes "-n" :value]
   [:max-synthdefs "-d" :value]
   [:real-time-memory-size "-m" :value]
   [:wire-buffers "-w" :value]
   [:random-seeds "-r" :value]
   [:load-synthdefs? "-D" :boolean]
   [:rendezvous? "-R" :boolean]
   [:max-logins "-l" :value]
   [:session-password "-p" :value]
   [:memory-locking? "-L" :flag]
   [:hardware-device-name "-H" :value]
   [:verbosity "-V" :value]
   [:ugen-plugins-path "-U" :value]
   [:restricted-path "-P" :value]])

(def ^:private scsynth-option-keys
  (set (map first scsynth-options)))

(defn- option-args
  [params [key option kind]]
  (when (contains? params key)
    (let [value (get params key)]
      (when (some? value)
        (case kind
          :flag (when value [option])
          :boolean [option (if value "1" "0")]
          :value [option (str value)])))))

(defn- scsynth-args
  [params]
  (let [unknown-keys (seq (remove (into scsynth-option-keys
                                        #{:executable :extra-args})
                                  (keys params)))
        extra-args (or (:extra-args params) [])]
    (when unknown-keys
      (throw (ex-info "unknown scsynth parameter"
                      {:keys (vec unknown-keys)})))
    (when-not (sequential? extra-args)
      (throw (IllegalArgumentException. ":extra-args must be sequential")))
    (into (vec (mapcat #(or (option-args params %) []) scsynth-options))
          (map str extra-args))))

(defn start
  "Start scsynth asynchronously and return a description of the process.

  The supported keys correspond to scsynth's command-line options, for example
  `:udp-port`, `:input-bus-channels`, and `:load-synthdefs?`. `:executable`
  may be used to select a scsynth executable, and `:extra-args` may be used for
  options not represented here."
  ([]
   (start {}))
  ([params]
   (when-not (map? params)
     (throw (IllegalArgumentException. "scsynth parameters must be a map")))
   (let [executable (or (:executable params) "scsynth")
         command (into [(str executable)] (scsynth-args params))
         process (.start (doto (ProcessBuilder. ^java.util.List command)
                           (.redirectOutput java.lang.ProcessBuilder$Redirect/INHERIT)
                           (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
     {:pid (.pid process)
      :command command
      :params (dissoc params :executable :extra-args)})))

(defn stop
  "Stop the process described by `process` using its stored PID.

  Returns true when a termination request was sent, and false when the PID no
  longer refers to a process or the process rejected the request."
  [{:keys [pid]}]
  (when-not (some? pid)
    (throw (IllegalArgumentException. "process description must contain :pid")))
  (let [handle (java.lang.ProcessHandle/of (long pid))]
    (if (.isPresent handle)
      (.destroy ^java.lang.ProcessHandle (.get handle))
      false)))

(defn- synth-instance?
  [x]
  (and (map? x)
       (contains? x :pid)
       (map? (:params x))))

(defn- synth-connect-args
  [{:keys [params]}]
  (let [[scheme port] (if-let [tcp-port (:tcp-port params)]
                        ["tcp" tcp-port]
                        ["udp" (or (:udp-port params) 57110)])
        host (:bind-address params)]
    [(str scheme "://"
          (if (or (nil? host)
                  (= host "0.0.0.0")
                  (= host "::"))
            "127.0.0.1"
            host)
          ":"
          port)]))

(defn connect
  "Connect to a synth instance or pass arguments through to `osc/connect`.

  A synth instance is connected using its `:tcp-port`, `:udp-port`, or the
  default scsynth UDP port. Additional arguments after a synth instance are
  passed to `osc/connect` instead, allowing an explicit OSC URI to be supplied."
  [target & args]
  (if (synth-instance? target)
    (apply osc/connect (if (seq args)
                         args
                         (synth-connect-args target)))
    (apply osc/connect target args)))

(defn close
  [conn]
  (osc/close conn))

(defn- default-fail-handler
  [[_ _ & [error data]]]
  {:error error
   :data data})

(defn- async-req
  ([conn msg done fail]
   (let [command (first msg)
         check (fn [reply]
                 (and (#{"/done" "/fail"} (first reply))
                      (= command (second reply))))
         reply @(osc/send conn msg check)]
     (if (= (first reply) "/done")
       (done reply)
       (fail reply))))
  ([conn msg done]
   (async-req conn msg done default-fail-handler))
  ([conn msg]
   (async-req conn msg identity default-fail-handler)))

(defn quit
  [conn]
  (async-req conn ["/quit"]
             (fn done [_] (close conn))))

(defn notify
  [conn notify? & [client-id]]
  (let [state (cond
                (boolean? notify?) (if notify? 1 0)
                (int? notify?) (if (pos? notify?) 1 0))
        msg (if client-id
              ["/notify" (int state) (int client-id)]
              ["/notify" (int state)])]
    (async-req conn msg
         (fn done [[_ _ & [client-id max-logins]]]
           {:client-id client-id
            :max-logins max-logins}))))

(defn status
  [conn]
  (let [[_ ugens synths groups synthdefs
         avg-cpu peak-cpu
         nominal-sample-rate actual-sample-rate]
        @(osc/send conn ["/status"] "/status.reply")]
    {:ugens ugens
     :synths synths
     :groups groups
     :synthdefs synthdefs
     :avg-cpu avg-cpu
     :peak-cpu peak-cpu
     :nominal-sample-rate nominal-sample-rate
     :actual-sample-rate actual-sample-rate}))

(defn cmd
  [conn & msg]
  (osc/send conn (vec msg)))

(defn dumpOSC
  [conn code]
  (osc/send conn ["/dumpOSC" (int code)]))

(let [sync-counter (atom 0)]
  (defn sync
    ([conn id]
     (let [msg ["/sync" (int id)]
           check (fn [reply]
                   (and (= (first reply) "/synced")
                        (== (second reply) id)))]
       @(osc/send conn msg check)))
    ([conn]
     (sync conn (swap! sync-counter inc)))))

(defn clearSched
  [conn]
  (osc/send conn ["/clearSched"]))

(defn error
  [conn mode]
  (osc/send conn ["/error" (int mode)]))

(defn version
  [conn]
  (let [[_ program major-version minor-version patch-version
         git-branch commit-hash]
        @(osc/send conn ["/version"] "/version.reply")]
    {:program program
     :major-version major-version
     :minor-version minor-version
     :patch-version patch-version
     :git-branch git-branch
     :commit-hash commit-hash}))

(defn d_recv
  ([conn synthdef]
   (async-req conn ["/d_recv" synthdef]))
  ([conn synthdef completion-message]
   (async-req conn ["/d_recv" synthdef completion-message])))

(defn d_load
  ([conn path]
   (async-req conn ["/d_load" path]))
  ([conn path completion-message]
   (async-req conn ["/d_load" path completion-message])))

(defn d_loadDir
  ([conn path]
   (async-req conn ["/d_loadDir" path]))
  ([conn path completion-message]
   (async-req conn ["/d_loadDir" path completion-message])))

(defn d_free
  [conn & names]
  (osc/send conn (apply vector "/d_free" names)))

(defn n_free
  [conn & ids]
  (osc/send conn (apply vector "/n_free" (map int ids))))

(defn- run-flag
  [x]
  (int (if (boolean? x) (if x 1 0) x)))

(defn n_run
  [conn & node-ids-and-flags]
  (let [args (reduce
              (fn [result [node-id flag]]
                (conj result (int node-id) (run-flag flag)))
              [] (partition 2 node-ids-and-flags))]
    (osc/send conn (apply vector "/n_run" args))))

(defn- control-index
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (integer? x) (int x)
    :else (throw (IllegalArgumentException.
                  (str "control index must be a name or integer: "
                       (pr-str x))))))

(defn- bus-ref
  [x]
  (if (re-matches #"^[ac][0-9]+$" x)
    x
    (throw (ex-info "invalid bus reference" {:ref x}))))

(defn- control-value
  [x]
  (cond
    (number? x) (float x)
    (vector? x) (mapv control-value x)
    (symbol? x) (bus-ref (name x))
    (string? x) (bus-ref x)
    :else (throw (IllegalArgumentException.
                  (str "control value must be numeric, a bus reference, or a vector: "
                       (pr-str x))))))

(defn n_set
  [conn node-id & control-indices-and-values]
  (let [args (reduce
              (fn [result [index value]]
                (conj result
                      (control-index index)
                      (control-value value)))
              [] (partition 2 control-indices-and-values))]
    (osc/send conn (apply vector "/n_set" (int node-id) args))))

(defn n_setn
  [conn node-id & control-indices-and-values]
  (let [args (reduce
              (fn [result [index values]]
                (conj result
                      (control-index index)
                      (count values)
                      (mapv control-value values)))
              [] (partition 2 control-indices-and-values))]
    (osc/send conn (apply vector "/n_setn" (int node-id) args))))

(defn n_fill
  [conn node-id & control-indices-and-values]
  (let [args (reduce
              (fn [result [index n value]]
                (conj result
                      (control-index index)
                      n
                      (control-value value)))
              [] (partition 3 control-indices-and-values))]
    (osc/send conn (apply vector "/n_fill" (int node-id) args))))

(defn n_map
  [conn node-id & control-and-bus-indices]
  (let [args (reduce
              (fn [result [c b]]
                (conj result
                      (control-index c)
                      (int b)))
              [] (partition 2 control-and-bus-indices))]
    (osc/send conn (apply vector "/n_map" (int node-id) args))))

(defn n_mapn
  [conn node-id & control-and-bus-indices]
  (let [args (reduce
              (fn [result [c b n]]
                (conj result
                      (control-index c)
                      (int b)
                      (int n)))
              [] (partition 3 control-and-bus-indices))]
    (osc/send conn (apply vector "/n_mapn" (int node-id) args))))

(defn n_mapa
  [conn node-id & control-and-bus-indices]
  (let [args (reduce
              (fn [result [c b]]
                (conj result
                      (control-index c)
                      (int b)))
              [] (partition 2 control-and-bus-indices))]
    (osc/send conn (apply vector "/n_mapa" (int node-id) args))))

(defn n_mapan
  [conn node-id & control-and-bus-indices]
  (let [args (reduce
              (fn [result [c b n]]
                (conj result
                      (control-index c)
                      (int b)
                      (int n)))
              [] (partition 3 control-and-bus-indices))]
    (osc/send conn (apply vector "/n_mapan" (int node-id) args))))

(defn n_before
  [conn & placements]
  (let [args (reduce
              (fn [result [a b]]
                (conj result (int a) (int b)))
              [] (partition 2 placements))]
    (osc/send conn (apply vector "/n_before" args))))

(defn n_after
  [conn & placements]
  (let [args (reduce
              (fn [result [a b]]
                (conj result (int a) (int b)))
              [] (partition 2 placements))]
    (osc/send conn (apply vector "/n_after" args))))

(defn n_query
  [conn & ids]
  (osc/send conn (apply vector "/n_query" (map int ids))))

(defn n_trace
  [conn & ids]
  (osc/send conn (apply vector "/n_trace" (map int ids))))

(defn- add-action
  [x]
  (if (int? x) x
      (case x
        :head 0
        :tail 1
        :before 2
        :after 3
        :replace 4)))

(defn n_order
  [conn action target-id & ids]
  (osc/send conn (apply vector "/n_order"
                          (add-action action)
                          (int target-id)
                          (map int ids))))

(defn s-new-message
  "Create a `/s_new` OSC message.

  Control names may be strings, keywords, or symbols. Numeric values are
  encoded as OSC float values so fractional controls are never truncated."
  [synthdef-name synth-id action target-id & control-indices-and-values]
  (when (odd? (count control-indices-and-values))
    (throw (IllegalArgumentException.
            "s_new controls must be supplied as index/value pairs")))
  (let [args (reduce
              (fn [result [c v]]
                (conj result
                      (control-index c)
                      (control-value v)))
              [] (partition 2 control-indices-and-values))]
    (apply vector "/s_new"
           (str synthdef-name)
           (int synth-id)
           (add-action action)
           (int target-id)
           args)))

(defn s_new
  [conn synthdef-name synth-id action target-id & control-indices-and-values]
  (osc/send conn
            (apply s-new-message synthdef-name synth-id action target-id
                   control-indices-and-values)))
