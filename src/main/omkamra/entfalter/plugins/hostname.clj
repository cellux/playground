(ns omkamra.entfalter.plugins.hostname
  (:require [omkamra.entfalter.facts.systemd :as systemd]
            [omkamra.entfalter.helpers :as helpers]
            [omkamra.pygen.core :as py]))

(def HostnamePluginConfig
  [:map
   [:hostname string?]
   [:fqdn string?]])

(py/define (ensure-hosts-entry fqdn hostname)
  (assign! desired (str "127.0.1.1 " fqdn " " hostname))
  (assign! original-lines [])
  (with [(open "/etc/hosts" "r" :encoding "utf-8") f]
    (for line f
      (original-lines.append (line.rstrip "\n"))))
  (assign! new-lines [])
  (assign! replaced false)
  (for line original-lines
    (if (and (not replaced)
             (line.startswith "127.0.1.1 "))
      [(new-lines.append desired)
       (assign! replaced true)]
      (new-lines.append line)))
  (if (not replaced)
    (new-lines.append desired))
  (if (!= new-lines original-lines)
    [(::helpers/write-file-atomic "/etc/hosts" new-lines)
     (return true)])
  (return false))

(py/define (apply-hostname-plugin plugin-config facts)
  (assign! desired-hostname (py-at plugin-config "hostname"))
  (assign! desired-fqdn (py-at plugin-config "fqdn"))
  (assign! systemd-facts (or (facts.get "systemd") {}))
  (assign! has-hostnamectl (systemd-facts.get "hostnamectl"))
  (if (not has-hostnamectl)
    (raise (RuntimeError "hostnamectl is required but not available")))
  (assign! changed false)
  (assign! current-hostname (::helpers/run-command-checked ["hostname"]))
  (if (!= current-hostname desired-hostname)
    [(::helpers/run-command-checked ["hostnamectl" "set-hostname" desired-hostname])
     (assign! changed true)])
  (if (::ensure-hosts-entry desired-fqdn desired-hostname)
    (assign! changed true))
  (assign! current-hostname (::helpers/run-command-checked ["hostname"]))
  (if (!= current-hostname desired-hostname)
    (raise (RuntimeError
            (str "hostname verification failed: expected "
                 desired-hostname
                 ", got "
                 current-hostname))))
  (assign! current-fqdn (::helpers/run-command-checked ["hostname" "--fqdn"]))
  (if (!= current-fqdn desired-fqdn)
    (raise (RuntimeError
            (str "fqdn verification failed: expected "
                 desired-fqdn
                 ", got "
                 current-fqdn))))
  (::helpers/emit-event "hostname"
                        (if-else changed "changed" "unchanged")
                        "hostname reconciled"))

(def plugin
  {:fact-dependencies #{::systemd/fact}
   :config-schema HostnamePluginConfig
   :apply-ref ::apply-hostname-plugin})
