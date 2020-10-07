(ns rb.explores.jsch.client
  (:require
   [clojure.java.io :as jio]
   [rb.explores.jsch.agentproxy.ssh-agent :as ssh-agent]
   [rb.explores.jsch.session :as session])
  (:import
   (com.jcraft.jsch JSch ConfigRepository OpenSSHConfig)))

(defn get-ssh-agent-identity-repository
  []
  (ssh-agent/get-repo))

(defn get-openssh-config-repository
  []
  (let [user-home (System/getenv "HOME")
        user-ssh-config-file (jio/file user-home ".ssh/config")]
    (when (.exists user-ssh-config-file)
      (OpenSSHConfig/parseFile (.getPath user-ssh-config-file)))))

(defn new
  ([config]
   (let [config-repository (or (:config-repository config)
                               (get-openssh-config-repository)
                               (ConfigRepository/nullConfig))
         identity-repository (or (:identity-repository config)
                                 (ssh-agent/get-repo))]
     {:config config
      :config-repository config-repository
      :identity-repository identity-repository
      :jsch (doto (JSch.)
              (.setConfigRepository config-repository)
              (.setIdentityRepository identity-repository))}))
  ([]
   (rb.explores.jsch.client/new {})))

(defn get-identity-names
  [client]
  (-> (:jsch client)
      (.getIdentityNames)))

(defn exec
  [client host command]
  (let [session (session/new client host)]
    (try
      (session/exec session command)
      (finally
        (session/disconnect session)))))
