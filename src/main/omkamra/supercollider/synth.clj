(ns omkamra.supercollider.synth
  (:require [omkamra.osc :as osc])
  (:refer-clojure :exclude [sync]))

(defn connect
  [& args]
  (apply osc/connect args))

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
  (if (string? x) x (int x)))

(defn- bus-ref
  [x]
  (if (re-matches #"^[ac][0-9]+$" x)
    x
    (throw (ex-info "invalid bus reference" {:ref x}))))

(defn- control-value
  [x]
  (cond (float? x) (float x)
        (int? x) (int x)
        (vector? x) (mapv control-value x)
        (symbol? x) (bus-ref (name x))
        (string? x) (bus-ref x)
        :else (int x)))

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

(defn s_new
  [conn
   synthdef-name synth-id
   action target-id
   & control-indices-and-values]
  (let [args (reduce
              (fn [result [c v]]
                (conj result
                      (control-index c)
                      (control-value v)))
              [] (partition 2 control-indices-and-values))]
    (osc/send conn (apply vector "/s_new"
                          (str synthdef-name)
                          (int synth-id)
                          (add-action action)
                          (int target-id)
                          args))))
