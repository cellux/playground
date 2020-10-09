(ns rb.explores.jsch.agentproxy.core
  (:import
   (com.jcraft.jsch.agentproxy RemoteIdentityRepository)))

(defn get-repo
  [connector]
  (RemoteIdentityRepository. connector))

(defn get-identities
  ([repo]
   (.getIdentities repo)))

(defn get-identity-by-name
  ([name repo]
   (->> (get-identities repo)
        (filter #(= (.getName %) name))
        first)))
