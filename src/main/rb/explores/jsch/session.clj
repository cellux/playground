(ns rb.explores.jsch.session
  (:require
   [clojure.java.io :as jio]
   [rb.explores.jsch.channel :as channel]))

(defn new
  [{:keys [jsch] :as client} host]
  (let [session (.getSession jsch host)]
    (.connect session)
    session))

(defn disconnect
  [session]
  (.disconnect session))

(defn connected?
  [session]
  (.isConnected session))

(defn read-streams-in-parallel
  [streams]
  (let [futures (for [s streams] (future (slurp s)))]
    (map deref futures)))

(defn exec
  [session command]
  (let [channel (channel/new session :exec)]
    (.setCommand channel command)
    (.connect channel)
    (try
      (let [stdout-stream (.getInputStream channel)
            stderr-stream (.getErrStream channel)
            [stdout stderr] (read-streams-in-parallel [stdout-stream stderr-stream])
            exit-status (.getExitStatus channel)]
        {:stdout stdout
         :stderr stderr
         :exit-status exit-status})
      (finally
        (.disconnect channel)))))
