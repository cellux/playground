(ns omkamra.decode64
  (:require [cljfx.api :as fx]
            [cljfx.prop :as fx.prop]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle]
            [clojure.java.shell :refer [sh]]
            [clojure.core.cache :as cache]
            [clojure.string :as str]
            [omkamra.vice :as vice]
            [omkamra.vice.binary-monitor :as vice.bm]))

(def *context
  (atom
   (fx/create-context
    {:vice {:options {:rm-host "127.0.0.1"
                      :rm-port 6510
                      :bm-host "127.0.0.1"
                      :bm-port 6502}
            :process nil                ; vice emulator process
            :rm-conn nil                ; remote monitor connection
            :bm-conn nil                ; binary monitor connection
            }
     :log-lines []}
    cache/lru-cache-factory)))

(defmulti handle-event :event/type)

(defn lookup [ctx ks]
  (fx/sub-val ctx get-in ks))

(defn app-log-lines [ctx]
  (fx/sub-val ctx :log-lines))

(defn app-log-size [ctx]
  (count (fx/sub-val ctx :log-lines)))

(defn trim-log
  [log-lines n]
  (if (> (count log-lines) n)
    (let [keep-from (- (count log-lines) n)]
      (vec (nthnext log-lines keep-from)))
    log-lines))

(defmethod handle-event :log [{:keys [fx/context message]}]
  {:context (fx/swap-context
             context
             #(-> %
                  (update :log-lines conj message)
                  (update :log-lines trim-log 1000)))})

(defmethod handle-event :log/clear [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :log-lines [])})

(defn log-message [level message]
  {:event/type :log :level level :message message})

(defn log-info [message]
  (log-message :info message))

(defn log-debug [message]
  (log-message :debug message))

(defn log-error [message]
  (log-message :error message))

(defn vice-options [ctx]
  (lookup ctx [:vice :options]))

(defn vice-process [ctx]
  (lookup ctx [:vice :process]))

(defn vice-rm-conn [ctx]
  (lookup ctx [:vice :rm-conn]))

(defn vice-bm-conn [ctx]
  (lookup ctx [:vice :bm-conn]))

(defn vice-running? [ctx]
  (some? (vice-process ctx)))

(defn vice-connected? [ctx]
  (some? (vice-bm-conn ctx)))

(defmethod handle-event :default [e]
  {:dispatch (log-debug (format "unhandled event: %s" (dissoc e :fx/context)))})

(defn process-start
  [{:keys [command on-start on-exit on-error] :as v} dispatch!]
  (let [runtime (Runtime/getRuntime)
        process (.exec runtime (into-array String command))]
    (future
      (try
        (dispatch! (log-info (format "[%d] Starting process: %s" (.pid process) command)))
        (dispatch! (assoc on-start :process process))
        (let [exit-code (.waitFor process)]
          (dispatch! (log-info (format "[%d] Process exited with code: %d" (.pid process) exit-code)))
          (dispatch! (assoc on-exit :exit-code exit-code)))
        (catch Throwable t
          (dispatch! (log-error (format "[%d] Process start failed: %s" t)))
          (dispatch! (assoc on-error :error t)))))))

(defn process-stop
  [{:keys [process]} dispatch!]
  (when process
    (dispatch! (log-info (format "[%d] Killing process" (.pid process))))
    (.destroy process)))

(defn vice-connect
  [{:keys [bm-host bm-port]} dispatch!]
  (future
    (let [event-handler (fn [response-type response]
                          (dispatch! {:event/type :vice/response
                                      :response-type response-type
                                      :response response}))
          connect (fn []
                    (try
                      (dispatch! (log-info (format "Attempting to connect to VICE binary monitor at %s:%d"
                                                   bm-host bm-port)))
                      (vice/connect bm-host bm-port event-handler)
                      (catch Throwable t
                        (dispatch! (log-error (format "Connection to binary monitor failed: %s" t)))
                        nil)))
          max-retries 10
          bm-conn (loop [conn (connect)
                         current-retry 1]
                    (or conn
                        (if (= current-retry max-retries)
                          nil
                          (do
                            (dispatch! (log-info (format "Retrying [%d/%d]"
                                                         current-retry
                                                         max-retries)))
                            (Thread/sleep 1000)
                            (recur (connect)
                                   (inc current-retry))))))]
      (if bm-conn
        (do
          (dispatch! (log-info "Successfully connected to VICE monitor."))
          (dispatch! {:event/type :vice/bm-connected
                      :bm-conn bm-conn}))
        (dispatch! (log-error "Connection to VICE monitor failed."))))))

(defn vice-disconnect
  [{:keys [bm-conn]} dispatch!]
  (dispatch! (log-info "Closing connection to VICE monitor."))
  (vice/close bm-conn)
  (dispatch! {:event/type :vice/bm-disconnected}))

(defmulti vice-send-request
  (fn [opts dispatch!]
    (:command opts)))

(defmethod vice-send-request :reset
  [{:keys [bm-conn]} dispatch!]
  (future
    (dispatch! (log-debug "Sending RESET to VICE."))
    (vice.bm/reset bm-conn)))

(defmethod handle-event :vice/start-request [{:keys [fx/context]}]
  {:process/start {:command ["x64sc",
                             "-pal",
                             "-remotemonitor",
                             "-binarymonitor",
                             "+confirmonexit"
                             "+fullscreen"]
                   :on-start {:event/type :vice/started}
                   :on-exit {:event/type :vice/exited}}})

(defmethod handle-event :vice/started [{:keys [fx/context process]}]
  {:context (fx/swap-context context assoc-in [:vice :process] process)
   :dispatch (log-info "VICE started")
   :vice/connect (lookup context [:vice :options])})

(defmethod handle-event :vice/bm-connected [{:keys [fx/context bm-conn]}]
  {:context (fx/swap-context context assoc-in [:vice :bm-conn] bm-conn)})

(defmethod handle-event :vice/bm-disconnected [{:keys [fx/context]}]
  {:context (fx/swap-context context update :vice dissoc :bm-conn)})

(defmethod handle-event :vice/exited [{:keys [fx/context exit-code]}]
  {:context (fx/swap-context context update :vice dissoc :process)})

(defmethod handle-event :vice/stop-request [{:keys [fx/context]}]
  (when-let [process (vice-process context)]
    [[:vice/disconnect {:bm-conn (vice-bm-conn context)}]
     [:process/stop {:process process}]]))

(defmethod handle-event :vice/reset-request [{:keys [fx/context]}]
  {:vice/send-request {:command :reset
                       :bm-conn (vice-bm-conn context)}})

(def log-viewer
  (fx/make-ext-with-props
   {:log-lines (fx.prop/make
                (fx.mutator/setter (fn [self lines]
                                     (doto self
                                       (.setText (str/join "\n" lines))
                                       (some-> .getParent .layout)
                                       (.setScrollTop ##Inf))))
                fx.lifecycle/scalar
                :default [])}))

(def app
  (fx/create-app
   *context
   :event-handler handle-event
   :effects {:process/start process-start
             :process/stop process-stop
             :vice/connect vice-connect
             :vice/disconnect vice-disconnect
             :vice/send-request vice-send-request}
   :desc-fn (fn [ctx]
              {:fx/type :stage
               :showing true
               :scene
               {:fx/type :scene
                :root
                {:fx/type :v-box
                 :children
                 [{:fx/type :h-box
                   :alignment :baseline-left
                   :padding 8
                   :spacing 8
                   :children
                   [{:fx/type :label
                     :text "VICE:"}
                    {:fx/type :button
                     :text "Start"
                     :disable (fx/sub-ctx ctx vice-running?)
                     :on-action {:event/type :vice/start-request}}
                    {:fx/type :button
                     :text "Stop"
                     :disable (not (fx/sub-ctx ctx vice-running?))
                     :on-action {:event/type :vice/stop-request}}
                    {:fx/type :button
                     :text "Reset"
                     :disable (not (fx/sub-ctx ctx vice-running?))
                     :on-action {:event/type :vice/reset-request}}
                    {:fx/type :check-box
                     :text "Connected"
                     :selected (fx/sub-ctx ctx vice-connected?)}
                    {:fx/type :button
                     :text "Clear log"
                     :on-action {:event/type :log/clear}}]}
                  {:fx/type log-viewer
                   :props {:log-lines (fx/sub-ctx ctx app-log-lines)}
                   :desc {:fx/type :text-area
                          :editable false}}]}}})))

;; ((:renderer app))