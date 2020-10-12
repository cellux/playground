(ns omkamra.jsch.client
  (:require
   [clojure.java.io :as jio]
   [omkamra.jsch.agentproxy.ssh-agent :as ssh-agent]
   [omkamra.jsch.session :as session])
  (:import
   (com.jcraft.jsch JSch ConfigRepository OpenSSHConfig)))

(defn get-ssh-agent-identity-repository
  []
  (ssh-agent/get-identity-repository))

(defn resolve-ssh-config-file
  [filename]
  (let [user-home (System/getenv "HOME")
        config-file (jio/file user-home ".ssh" filename)]
    (when (.exists config-file)
      (.getPath config-file))))

(defn get-openssh-config-repository
  []
  (when-let [config-file (resolve-ssh-config-file "config")]
    (OpenSSHConfig/parseFile config-file)))

(defn set-known-hosts-if-exists
  [jsch hosts-file]
  (when hosts-file
    (.setKnownHosts jsch hosts-file)))

(defn new
  ([config]
   (let [config-repository (or (:config-repository config)
                               (get-openssh-config-repository)
                               (ConfigRepository/nullConfig))
         identity-repository (or (:identity-repository config)
                                 (ssh-agent/get-identity-repository))
         known-hosts-file (resolve-ssh-config-file "known_hosts")]
     {:config config
      :config-repository config-repository
      :identity-repository identity-repository
      :known-hosts-file known-hosts-file
      :jsch (doto (JSch.)
              (.setConfigRepository config-repository)
              (.setIdentityRepository identity-repository)
              (set-known-hosts-if-exists known-hosts-file))}))
  ([]
   (omkamra.jsch.client/new {})))

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
