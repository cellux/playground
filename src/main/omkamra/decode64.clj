(ns omkamra.decode64
  (:require
   [cljfx.api :as fx]
   [cljfx.prop :as fx.prop]
   [cljfx.mutator :as fx.mutator]
   [cljfx.lifecycle :as fx.lifecycle]
   [clojure.java.io :as jio]
   [clojure.java.shell :refer [sh]]
   [clojure.core.cache :as cache]
   [clojure.string :as str]
   [clojure.stacktrace]
   [omkamra.vice :as vice]
   [omkamra.vice.binary-monitor :as vice.bm])
  (:import
   [javafx.stage FileChooser]
   [de.codecentric.centerdevice.javafxsvg SvgImageLoaderFactory]
   [de.codecentric.centerdevice.javafxsvg.dimension PrimitiveDimensionProvider]))

(SvgImageLoaderFactory/install (PrimitiveDimensionProvider.))

(def *context
  (atom
   (fx/create-context
    {:vice {:options {:bm-host "127.0.0.1"
                      :bm-port 6502}
            :process nil                ; vice emulator process
            :bm-conn nil                ; binary monitor connection
            :paused? false
            :registers nil
            :reg/name->id nil
            :reg/id->name nil
            :reg/id->size nil
            :bank/name->id nil
            :bank/id->name nil
            }
     :log-lines []}
    cache/lru-cache-factory)))

(defmulti handle-event :event/type)

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

(defn log-message
  [level fmt & args]
  {:event/type :log
   :level level
   :message (if (seq args)
              (apply format fmt args)
              fmt)})

(defn log-info [fmt & args]
  (apply log-message :info fmt args))

(defn log-debug [fmt & args]
  (apply log-message :debug fmt args))

(defn log-error [fmt & args]
  (apply log-message :error fmt args))

(defmethod handle-event :default [e]
  {:dispatch (log-debug "unhandled event: %s" (dissoc e :fx/context))})

(defn vice-running? [ctx]
  (some? (fx/sub-val ctx get-in [:vice :process])))

(defn vice-paused? [ctx]
  (fx/sub-val ctx get-in [:vice :paused?]))

(defn vice-connected? [ctx]
  (some? (fx/sub-val ctx get-in [:vice :bm-conn])))

(defn process-start
  [{:keys [command on-start on-exit on-error] :as v} dispatch!]
  (let [runtime (Runtime/getRuntime)
        process (.exec runtime (into-array String command))]
    (future
      (try
        (dispatch! (log-info "[%d] Starting process: %s" (.pid process) command))
        (dispatch! (assoc on-start :process process))
        (let [exit-code (.waitFor process)]
          (dispatch! (log-info "[%d] Process exited with code: %d" (.pid process) exit-code))
          (dispatch! (assoc on-exit :exit-code exit-code)))
        (catch Throwable t
          (dispatch! (log-error "[%d] Process start failed: %s" t))
          (dispatch! (assoc on-error :error t)))))))

(defn process-stop
  [{:keys [process]} dispatch!]
  (when process
    (dispatch! (log-info "[%d] Killing process" (.pid process)))
    (.destroy process)))

(defmethod handle-event :vice/response
  [{:keys [fx/context response-type response]}]
  (condp = response-type
    vice.bm/MON_RESPONSE_STOPPED {:context (fx/swap-context context assoc-in [:vice :paused?] true)}
    vice.bm/MON_RESPONSE_RESUMED {:context (fx/swap-context context assoc-in [:vice :paused?] false)}
    vice.bm/MON_RESPONSE_REGISTER_INFO {:context (fx/swap-context context assoc-in [:vice :registers] response)}
    {:dispatch (log-debug "unhandled VICE response: 0x%02x %s" response-type response)}))

(defmethod handle-event :vice/autostart
  [{:keys [fx/context file]}]
  {:vice/send-request {:command :autostart
                       :bm-conn (fx/sub-val context get-in [:vice :bm-conn])
                       :file file}})

(defn vice-connect
  [{:keys [bm-host bm-port]} dispatch!]
  (future
    (let [event-handler (fn [response-type response]
                          (dispatch! {:event/type :vice/response
                                      :response-type response-type
                                      :response response}))
          connect (fn []
                    (try
                      (dispatch! (log-info "Attempting to connect to VICE binary monitor at %s:%d"
                                           bm-host bm-port))
                      (vice/connect bm-host bm-port event-handler)
                      (catch Throwable t nil
                        (do
                          (dispatch! (log-error "Connection to binary monitor failed: %s" t))
                          nil))))
          with-max-retries (fn [max-retries f]
                             (loop [result (f)
                                    current-retry 1]
                               (or result
                                   (if (= current-retry max-retries)
                                     nil
                                     (do
                                       (dispatch! (log-info "Retrying [%d/%d]"
                                                            current-retry max-retries))
                                       (Thread/sleep 1000)
                                       (recur (f)
                                              (inc current-retry)))))))
          bm-conn (with-max-retries 10 connect)]
      (if-not bm-conn
        (dispatch! (log-error "Connection to VICE monitor failed."))
        (do (dispatch! (log-info "Successfully connected to VICE monitor."))
            (let [fetch-info (fn [what fetch]
                               (dispatch! (log-debug "Fetching available %s." what))
                               (if-let [result (with-max-retries 3 fetch)]
                                 result
                                 (do
                                   (dispatch! (log-error "Cannot fetch available %s from VICE." what))
                                   nil)))
                  fetch-banks (fn []
                                (when-let [banks (fetch-info "banks"
                                                             #(vice.bm/banks-available bm-conn))]
                                  {:bank/name->id (into {} (map #(vector (:name %) (:id %))
                                                                (vals banks)))
                                   :bank/id->name (into {} (map #(vector (:id %) (:name %))
                                                                (vals banks)))}))
                  fetch-regs (fn []
                               (when-let [regs (fetch-info "registers"
                                                           #(vice.bm/registers-available bm-conn))]
                                 {:reg/name->id (into {} (map #(vector (:name %) (:id %))
                                                              (vals regs)))
                                  :reg/id->name (into {} (map #(vector (:id %) (:name %))
                                                              (vals regs)))
                                  :reg/id->size (into {} (map #(vector (:id %) (:size %))
                                                              (vals regs)))}))
                  vice-data (reduce (fn [data f]
                                      (if-let [result (f)]
                                        (merge data result)
                                        (update data ::failed conj f)))
                                    {::failed #{}}
                                    [fetch-banks fetch-regs])]
              (when (empty? (::failed vice-data))
                (vice.bm/exit bm-conn)  ; resume execution
                (dispatch! {:event/type :vice/connected
                            :vice/data (-> vice-data
                                           (assoc :bm-conn bm-conn)
                                           (dissoc ::failed))}))))))))

(defn vice-disconnect
  [{:keys [bm-conn]} dispatch!]
  (dispatch! (log-info "Closing connection to VICE monitor."))
  (vice/close bm-conn)
  (dispatch! {:event/type :vice/disconnected}))

(defmulti vice-send-request
  (fn [opts dispatch!]
    (:command opts)))

(defmethod vice-send-request :reset
  [{:keys [bm-conn]} dispatch!]
  (future
    (dispatch! (log-debug "Sending RESET to VICE."))
    (vice.bm/reset bm-conn)))

(defmethod vice-send-request :pause
  [{:keys [bm-conn]} dispatch!]
  (future
    (dispatch! (log-debug "Pausing VICE."))
    (vice.bm/ping bm-conn)))

(defmethod vice-send-request :resume
  [{:keys [bm-conn]} dispatch!]
  (future
    (dispatch! (log-debug "Resuming VICE."))
    (vice.bm/exit bm-conn)))

(defmethod vice-send-request :advance
  [{:keys [bm-conn step-over? count]} dispatch!]
  (vice.bm/advance-instructions bm-conn {:step-over? (or step-over? false)
                                         :count (or count 1)}))

(defmethod vice-send-request :autostart
  [{:keys [bm-conn file]} dispatch!]
  (future
    (let [filename (.getPath file)]
      (dispatch! (log-debug "Autostarting file: %s" filename))
      (vice.bm/autostart bm-conn {:run-after-load? true
                                  :file-index 0
                                  :filename filename}))))

(defn file-chooser-show
  [{:keys [window title on-file-selected]} dispatch!]
  (fx/run-later
   (let [chooser (doto (FileChooser.)
                   (.setTitle title))]
     (when-let [file (.showOpenDialog chooser window)]
       (dispatch! (assoc on-file-selected :file file))))))

(defn decode64
  [{:keys [bm-conn]} dispatch!]
  (dispatch! (log-info "decode64")))

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
   :vice/connect (fx/sub-val context get-in [:vice :options])})

(defmethod handle-event :vice/connected [{:keys [fx/context vice/data]}]
  {:context (fx/swap-context context update :vice merge data)})

(defmethod handle-event :vice/disconnected [{:keys [fx/context]}]
  {:context (fx/swap-context context update :vice dissoc :bm-conn)})

(defmethod handle-event :vice/exited [{:keys [fx/context]}]
  {:context (fx/swap-context context update :vice dissoc :process)})

(defmethod handle-event :vice/stop-request [{:keys [fx/context]}]
  (when-let [process (fx/sub-val context get-in [:vice :process])]
    [[:vice/disconnect {:bm-conn (fx/sub-val context get-in [:vice :bm-conn])}]
     [:process/stop {:process process}]]))

(defmethod handle-event :vice/reset-request [{:keys [fx/context]}]
  {:vice/send-request {:command :reset
                       :bm-conn (fx/sub-val context get-in [:vice :bm-conn])}})

(defmethod handle-event :vice/pause-request [{:keys [fx/context]}]
  {:vice/send-request {:command :pause
                       :bm-conn (fx/sub-val context get-in [:vice :bm-conn])}})

(defmethod handle-event :vice/resume-request [{:keys [fx/context]}]
  {:vice/send-request {:command :resume
                       :bm-conn (fx/sub-val context get-in [:vice :bm-conn])}})

(defmethod handle-event :vice/advance-request [{:keys [fx/context]}]
  {:vice/send-request {:command :advance
                       :count 1
                       :bm-conn (fx/sub-val context get-in [:vice :bm-conn])}})

(defmethod handle-event :decode64-request [{:keys [fx/context]}]
  {:decode64 {:vice (fx/sub-val context :vice)}})

(defmethod handle-event :vice/autostart-request [{:keys [fx/context fx/event]}]
  {:file-chooser/show {:window (-> event .getTarget .getScene .getWindow)
                       :title "Choose a file to autostart in VICE"
                       :on-file-selected {:event/type :vice/autostart}}})

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

(defn svg-button
  [{:keys [filename tooltip disable on-action]}]
  {:fx/type :button
   :graphic {:fx/type :image-view
             :image {:url (str (jio/resource (str "omkamra/decode64/" filename)))
                     :requested-width 16
                     :requested-height 16}}
   :tooltip {:fx/type :tooltip :text tooltip}
   :disable (or disable false)
   :on-action on-action})

(defn pause-resume-button
  [{:keys [fx/context]}]
  (let [running? (vice-running? context)
        paused? (vice-paused? context)]
    (if paused?
      {:fx/type svg-button
       :filename "angles-right.svg"
       :text "Resume"
       :disable (not running?)
       :on-action {:event/type :vice/resume-request}}
      {:fx/type svg-button
       :filename "pause.svg"
       :text "Pause"
       :disable (not running?)
       :on-action {:event/type :vice/pause-request}})))

(defn vice-register-view
  [{:keys [fx/context]}]
  (let [registers (fx/sub-val context get-in [:vice :registers])
        name->id (fx/sub-val context get-in [:vice :reg/name->id])
        id->size (fx/sub-val context get-in [:vice :reg/id->size])]
    (if (or (nil? registers) (nil? name->id))
      {:fx/type :label
       :text "-"}
      {:fx/type :v-box
       :children
       (for [[name id] name->id]
         {:fx/type :h-box
          :alignment :baseline-left
          :children
          [{:fx/type :label
            :text (str name ":")
            :min-width 30}
           {:fx/type :text-field
            :text (format (str "%0" (/ (id->size id) 4) "X")
                          (-> registers (get id) :value))
            :editable false}]})})))

(def app
  (fx/create-app
   *context
   :event-handler handle-event
   :effects {:process/start process-start
             :process/stop process-stop
             :vice/connect vice-connect
             :vice/disconnect vice-disconnect
             :vice/send-request vice-send-request
             :file-chooser/show file-chooser-show
             :decode64 #'decode64}
   :desc-fn (fn [ctx]
              {:fx/type :stage
               :showing true
               :scene
               {:fx/type :scene
                :stylesheets #{(str (jio/resource "omkamra/decode64.css"))}
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
                    {:fx/type svg-button
                     :filename "play.svg"
                     :tooltip "Start"
                     :disable (vice-running? ctx)
                     :on-action {:event/type :vice/start-request}}
                    {:fx/type svg-button
                     :filename "stop.svg"
                     :tooltip "Stop"
                     :disable (not (vice-running? ctx))
                     :on-action {:event/type :vice/stop-request}}
                    {:fx/type svg-button
                     :filename "arrows-rotate.svg"
                     :tooltip "Reset"
                     :disable (not (vice-running? ctx))
                     :on-action {:event/type :vice/reset-request}}
                    {:fx/type svg-button
                     :filename "folder-open.svg"
                     :tooltip "Autostart"
                     :disable (not (vice-running? ctx))
                     :on-action {:event/type :vice/autostart-request}}
                    {:fx/type pause-resume-button}
                    {:fx/type svg-button
                     :filename "1.svg"
                     :tooltip "Advance"
                     :disable (or (not (vice-running? ctx))
                                  (not (vice-paused? ctx)))
                     :on-action {:event/type :vice/advance-request}}
                    {:fx/type :button
                     :text "Decode"
                     :disable (not (vice-running? ctx))
                     :on-action {:event/type :decode64-request}}
                    {:fx/type :check-box
                     :text "Connected"
                     :selected (vice-connected? ctx)}
                    {:fx/type :button
                     :text "Clear log"
                     :on-action {:event/type :log/clear}}]}
                  {:fx/type :h-box
                   :alignment :baseline-left
                   :padding 8
                   :spacing 8
                   :children
                   [{:fx/type vice-register-view}]}
                  {:fx/type log-viewer
                   :v-box/vgrow :always
                   :props {:log-lines (fx/sub-val ctx :log-lines)}
                   :desc {:fx/type :text-area
                          :editable false}}]}}})))

;; ((:renderer app))
