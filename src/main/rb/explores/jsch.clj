(ns rb.explores.jsch
  (:require [rb.explores.jsch.client :as client]))

(def version JSch/VERSION)

(defn exec
  [host command]
  (let [client (client/new)]
    (client/exec client host command)))
