(ns rb.explores.jsch
  (:require
   [rb.explores.jsch.agentproxy.ssh-agent :as ssh-agent])
  (:import
   (com.jcraft.jsch JSch)))

(def version JSch/VERSION)
