(ns omkamra.jsch
  (:import (com.jcraft.jsch JSch))
  (:require [omkamra.jsch.client :as client]))

(def version JSch/VERSION)

(defn exec
  [host command]
  (let [client (client/new)]
    (client/exec client host command)))
