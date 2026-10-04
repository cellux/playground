(ns omkamra.entfalter.core
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [omkamra.entfalter.helpers :as helpers]
            [omkamra.entfalter.schema :as schema]
            [omkamra.pygen.core :as py])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.nio.charset StandardCharsets)
           (java.util EnumSet)
           (org.apache.sshd.client SshClient)
           (org.apache.sshd.client.channel ClientChannelEvent)
           (org.apache.sshd.client.keyverifier AcceptAllServerKeyVerifier)))

(defn- resolve-plugin-var
  [plugin-key]
  (when-not (schema/namespaced-keyword? plugin-key)
    (throw (ex-info "plugin key must be a namespaced keyword"
                    {:plugin plugin-key})))
  (let [sym (symbol (namespace plugin-key) (name plugin-key))
        resolved (try
                   (requiring-resolve sym)
                   (catch Throwable t
                     (throw (ex-info "unable to resolve plugin keyword"
                                     {:plugin plugin-key :symbol sym}
                                     t))))]
    (when-not (var? resolved)
      (throw (ex-info "plugin keyword did not resolve to a var"
                      {:plugin plugin-key :symbol sym :resolved resolved})))
    resolved))

(defn- validate-plugin-spec
  [plugin-key spec]
  (schema/validate! schema/PluginSpec
                    spec
                    "invalid plugin spec"
                    {:plugin plugin-key}))

(defn- resolve-plugin-spec
  [plugin-key]
  (->> plugin-key
       resolve-plugin-var
       var-get
       (validate-plugin-spec plugin-key)))

(defn- plugin-entries
  [config]
  (let [plugins (:plugins config)]
    (schema/validate! schema/PluginsMap
                      plugins
                      "config :plugins must be a map keyed by namespaced keywords"
                      {:plugins plugins})
    (->> plugins
         (map (fn [[plugin-key plugin-config]]
                {:plugin-key plugin-key
                 :plugin-config plugin-config
                 :spec (resolve-plugin-spec plugin-key)}))
         (sort-by (comp str :plugin-key))
         vec)))

(defn- required-fact-dependencies
  [entries]
  (->> entries
       (mapcat #(get-in % [:spec :fact-dependencies]))
       set
       (sort-by str)))

(defn- resolve-fact-var
  [fact-dependency]
  (when-not (schema/namespaced-keyword? fact-dependency)
    (throw (ex-info "fact dependency must be a namespaced keyword"
                    {:fact fact-dependency})))
  (let [sym (symbol (namespace fact-dependency) (name fact-dependency))
        resolved (try
                   (requiring-resolve sym)
                   (catch Throwable t
                     (throw (ex-info "unable to resolve fact dependency keyword"
                                     {:fact fact-dependency :symbol sym}
                                     t))))]
    (when-not (var? resolved)
      (throw (ex-info "fact dependency keyword did not resolve to a var"
                      {:fact fact-dependency :symbol sym :resolved resolved})))
    resolved))

(defn- validate-fact-spec
  [fact-dependency fact-spec]
  (schema/validate! schema/FactSpec
                    fact-spec
                    "invalid fact spec"
                    {:fact fact-dependency}))

(defn- resolve-fact-spec
  [fact-dependency]
  (->> fact-dependency
       resolve-fact-var
       var-get
       (validate-fact-spec fact-dependency)))

(defn- collect-fact-collector-specs
  [fact-dependencies]
  (reduce
   (fn [acc fact-dependency]
     (let [fact-spec (resolve-fact-spec fact-dependency)
           fact-key (:fact-key fact-spec)]
       (when-let [existing-by-fact-key (some (fn [[_ dep-spec]]
                                               (when (= (:fact-key dep-spec) fact-key)
                                                 dep-spec))
                                             acc)]
         (when-not (= existing-by-fact-key fact-spec)
           (throw (ex-info "conflicting fact definitions for fact key"
                           {:fact-key fact-key
                            :existing existing-by-fact-key
                            :incoming fact-spec}))))
       (assoc acc fact-dependency fact-spec)))
   {}
   fact-dependencies))

(defn- emit-fact-assignment-form
  [fact-collector-specs fact-dependency]
  (let [{:keys [fact-key collector-ref]} (get fact-collector-specs fact-dependency)]
    (when-not (and fact-key collector-ref)
      (throw (ex-info "missing fact collector spec"
                      {:fact-dependency fact-dependency})))
    (list 'assign!
          (list 'py-at 'facts fact-key)
          (list collector-ref))))

(defn emit-fact-collector-script
  [config]
  (let [entries (plugin-entries config)
        fact-dependencies (required-fact-dependencies entries)
        fact-collector-specs (collect-fact-collector-specs fact-dependencies)
        collector-forms (map #(emit-fact-assignment-form fact-collector-specs %)
                             fact-dependencies)
        collect-facts-def (apply list
                                 'def
                                 'collect-facts
                                 []
                                 (concat [(list 'assign! 'facts {})]
                                         collector-forms
                                         [(list 'return 'facts)]))
        main-def (list 'def
                       'main
                       []
                       (list ::helpers/print-json (list 'collect-facts)))
        module-forms [collect-facts-def
                      main-def
                      (list 'main)]]
    (py/transpile module-forms)))

(defn- plugin-config->py-literal
  [{:keys [plugin-key plugin-config spec]}]
  (try
    (let [config-schema (:config-schema spec)]
      (schema/validate! config-schema
                        plugin-config
                        "invalid plugin config"
                        {:plugin plugin-key
                         :plugin-config plugin-config})
      (schema/normalize-for-py config-schema plugin-config))
    (catch Throwable t
      (throw (ex-info "plugin config validation/normalization failed"
                      {:plugin plugin-key :plugin-config plugin-config}
                      t)))))

(defn- config-apply-form
  [entry]
  (let [plugin-config (plugin-config->py-literal entry)]
    (list (get-in entry [:spec :apply-ref]) plugin-config 'facts)))

(defn emit-configuration-script
  [config facts]
  (let [entries (plugin-entries config)
        apply-forms (map config-apply-form entries)
        main-def (apply list
                        'def
                        'main
                        []
                        (concat [(list 'assign! 'facts facts)]
                                apply-forms))
        module-forms [main-def
                      (list 'main)]]
    (py/transpile module-forms)))

(defn- as-utf8-bytes
  [^String s]
  (.getBytes s StandardCharsets/UTF_8))

(defn run-python-script-over-ssh
  [{:keys [host port user username password timeout-ms verify-host-key? python-command]
    :or {port 22
         timeout-ms 60000
         verify-host-key? false
         python-command "python3 -"}
    :as connection}
   script]
  (schema/validate! schema/SSHConnection
                    connection
                    "invalid SSH connection map"
                    {:connection connection})
  (let [effective-user (or user username (System/getProperty "user.name"))
        client (doto (SshClient/setUpDefaultClient)
                 (#(when-not verify-host-key?
                     (.setServerKeyVerifier % AcceptAllServerKeyVerifier/INSTANCE))))]
    (.start client)
    (try
      (with-open [session (-> client
                              (.connect effective-user host (int port))
                              (.verify timeout-ms)
                              (.getSession))]
        (when (string? password)
          (.addPasswordIdentity session password))
        (-> (.auth session) (.verify timeout-ms))
        (with-open [stdout (ByteArrayOutputStream.)
                    stderr (ByteArrayOutputStream.)
                    channel (.createExecChannel session python-command)]
          (.setOut channel stdout)
          (.setErr channel stderr)
          (.setIn channel (ByteArrayInputStream. (as-utf8-bytes script)))
          (-> (.open channel) (.verify timeout-ms))
          (.waitFor channel (EnumSet/of ClientChannelEvent/CLOSED) timeout-ms)
          {:stdout (.toString stdout "UTF-8")
           :stderr (.toString stderr "UTF-8")
           :exit-status (or (.getExitStatus channel) -1)}))
      (finally
        (.stop client)))))

(defn- parse-json
  [s]
  (json/parse-string s false))

(defn- parse-json-lines
  [s]
  (->> (str/split-lines (or s ""))
       (map str/trim)
       (remove str/blank?)
       (mapv (fn [line]
               (try
                 (parse-json line)
                 (catch Exception _
                   {"raw" line "parse_error" true}))))))

(defn- parse-facts-stdout
  [stdout]
  (let [trimmed (str/trim (or stdout ""))]
    (when (str/blank? trimmed)
      (throw (ex-info "fact collector produced empty stdout" {})))
    (parse-json trimmed)))

(defn reconcile
  [config]
  (schema/validate! schema/ReconcileConfig
                    config
                    "invalid reconcile config"
                    {:config config})
  (when-not (= ::linux-server (:type config))
    (throw (ex-info "unsupported reconcile target type"
                    {:type (:type config)})))
  (let [connection (:connection config)
        fact-script (emit-fact-collector-script config)
        fact-result (run-python-script-over-ssh connection fact-script)]
    (when-not (zero? (:exit-status fact-result))
      (throw (ex-info "fact collector script failed"
                      {:result fact-result})))
    (let [facts (parse-facts-stdout (:stdout fact-result))
          config-script (emit-configuration-script config facts)
          config-result (run-python-script-over-ssh connection config-script)
          events (parse-json-lines (:stdout config-result))
          status (if (zero? (:exit-status config-result)) :ok :error)]
      {:status status
       :facts facts
       :events events
       :fact-result fact-result
       :configuration-result config-result})))
