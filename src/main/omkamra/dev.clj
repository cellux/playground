(ns omkamra.dev
  "Development-time lifecycle control."
  (:require [clojure.java.io :as io]
            [integrant.core :as ig]))

(def ^:private default-config-path "dev.edn")
(def ^:private lifecycle-lock (Object.))
(defonce ^:private state
  (atom {:config nil
         :config-path nil
         :running {}}))

(defn- read-config-file
  [path]
  (let [file (io/file path)]
    (when-not (.isFile file)
      (throw (ex-info "Development configuration file does not exist"
                      {:path (.getPath file)})))
    (ig/read-string (slurp file))))

(defn load-config!
  "Load the Integrant configurations from dev.edn.

  Loading configuration does not start any systems. If systems are already
  running, their loaded configuration is retained for stopping them later."
  ([]
   (load-config! default-config-path))
  ([path]
   (let [config (read-config-file path)]
     (when-not (map? (:systems config))
       (throw (ex-info "Development configuration must contain a :systems map"
                       {:path path
                        :config-keys (keys config)})))
     (swap! state assoc
            :config config
            :config-path path)
     config)))

(defn- current-config
  []
  (or (:config @state)
      (load-config!)))

(defn- configured-systems
  []
  (:systems (current-config)))

(defn list-systems
  "Return the names of systems available in dev.edn." []
  (-> (configured-systems) keys sort vec))

(defn- require-system
  [name]
  (or (get (configured-systems) name)
      (throw (ex-info "Unknown development system"
                      {:system name
                       :available (list-systems)}))))

(defn- resolve-system-config
  [name system-link]
  (let [config (if (symbol? system-link)
                 (let [v (requiring-resolve system-link)]
                   (when (:private (meta v))
                     (throw (ex-info "Development system link must resolve to a public Var"
                                     {:system name
                                      :link system-link})))
                   @v)
                 system-link)]
    (when-not (map? config)
      (throw (ex-info "Development system link must resolve to an Integrant config map"
                      {:system name
                       :link system-link
                       :resolved-value config})))
    config))

(defn- running-entry
  [name]
  (get-in @state [:running name]))

(defn system-status
  "Return a small, printable status map for one configured system."
  [name]
  {:system name
   :configured? (contains? (configured-systems) name)
   :running? (boolean (running-entry name))})

(defn status
  "Return the available systems and their lifecycle status." []
  (let [config (current-config)
        names (into (set (keys (:systems config)))
                    (keys (:running @state)))]
    {:config-path (:config-path @state)
     :systems (into (sorted-map)
                    (map (fn [name]
                           [name (system-status name)]))
                    names)}))

(defn- init-system
  [config]
  ;; This is intentionally done at start time. A config can mention arbitrary
  ;; Integrant components, and the control plane should not load all of them
  ;; merely because it was required.
  (ig/load-namespaces config)
  (ig/init config))

(defn start!
  "Start the named Integrant system from dev.edn.

  Starting an already-running system is idempotent. The return value is a
  compact status map rather than the potentially large Integrant state map."
  [name]
  (locking lifecycle-lock
    (let [system-link (require-system name)]
      (if (running-entry name)
        (system-status name)
        (let [config (resolve-system-config name system-link)
              system (init-system config)]
          (swap! state assoc-in [:running name]
                 {:config config
                  :system system})
          (system-status name))))))

(defn stop!
  "Stop the named system if it is running."
  [name]
  (locking lifecycle-lock
    ;; A config reload may remove a system while its old instance is still
    ;; running. In that case stopping the old instance must remain possible.
    (when-not (running-entry name)
      (require-system name))
    (when-let [{:keys [system]} (running-entry name)]
      (ig/halt! system)
      (swap! state update :running dissoc name))
    (system-status name)))

(defn restart!
  "Stop and start the named system."
  [name]
  (locking lifecycle-lock
    (stop! name)
    (start! name)))

(defn stop-all!
  "Stop every currently-running system." []
  (locking lifecycle-lock
    (doseq [name (keys (:running @state))]
      (when-let [{:keys [system]} (running-entry name)]
        (ig/halt! system)))
    (swap! state assoc :running {})
    (status)))

