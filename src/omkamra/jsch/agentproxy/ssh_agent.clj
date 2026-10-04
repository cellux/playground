(ns omkamra.jsch.agentproxy.ssh-agent
  (:require
   [omkamra.jsch.agentproxy.core :as core])
  (:import
   (com.jcraft.jsch.agentproxy.connector SSHAgentConnector)
   (com.jcraft.jsch.agentproxy.usocket JNAUSocketFactory)))

(defn make-connector
  []
  (let [udsf (JNAUSocketFactory.)]
    (SSHAgentConnector. udsf)))

(defn get-identity-repository
  []
  (let [connector (make-connector)]
    (core/get-identity-repository connector)))

(defn get-identities
  []
  (let [repo (get-identity-repository)]
    (core/get-identities repo)))

(defn get-identity-by-name
  [name]
  (let [repo (get-identity-repository)]
    (core/get-identity-by-name name repo)))
