(ns omkamra.entfalter.facts.systemd
  (:require [omkamra.pygen.core :as py]))

(py/define (collect-systemd-facts)
  {:imports [shutil]}
  (return {"hostnamectl" (shutil.which "hostnamectl")}))

(def fact
  {:fact-key "systemd"
   :collector-ref ::collect-systemd-facts})
