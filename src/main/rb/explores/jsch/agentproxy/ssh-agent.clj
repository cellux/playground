(ns rb.explores.jsch.agentproxy.ssh-agent
  (:require
   [rb.explores.jsch.agentproxy.core :as core])
  (:import
   (com.jcraft.jsch.agentproxy.connector SSHAgentConnector)
   (com.jcraft.jsch.agentproxy.usocket JNAUSocketFactory)))

(defn make-connector
  []
  (let [udsf (JNAUSocketFactory.)]
    (SSHAgentConnector. udsf)))

(defn get-repo
  []
  (let [connector (make-connector)]
    (core/get-repo connector)))

(defn get-identities
  []
  (let [repo (get-repo)]
    (core/get-identities repo)))

(defn get-identity-by-name
  [name]
  (let [repo (get-repo)]
    (core/get-identity-by-name name repo)))
