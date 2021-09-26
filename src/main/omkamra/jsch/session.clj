(ns omkamra.jsch.session
  (:require
   [clojure.java.io :as jio]
   [omkamra.jsch.channel :as channel]))

(defn create
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

(defn exec
  [session command]
  (let [channel (channel/create session :exec)]
    (.setCommand channel command)
    (let [stdout-reader (future (slurp (.getInputStream channel)))
          stderr-reader (future (slurp (.getErrStream channel)))]
      (.connect channel)
      (try
        (let [stdout @stdout-reader
              stderr @stderr-reader
              exit-status (.getExitStatus channel)]
          {:stdout stdout
           :stderr stderr
           :exit-status exit-status})
        (finally
          (.disconnect channel))))))
