(ns omkamra.k8s.kubeconfig
  (:require [clj-yaml.core :as yaml]
            [omkamra.crypto.pem :as pem])
  (:import (java.util Base64)))

(defn path
  []
  (or (System/getenv "KUBECONFIG")
      (format "%s/%s"
              (System/getenv "HOME")
              ".kube/config")))

(defn load
  ([path]
   (-> (slurp path)
       (yaml/parse-string)))
  ([]
   (load (path))))

(defn- pluralize
  [kw]
  (keyword (str (name kw) "s")))

(defn- find-entry
  [config kind entry-name]
  {:pre [(keyword? kind) (string? entry-name)]}
  (if-let [entry (->> (get config (pluralize kind))
                      (filter #(= (:name %) entry-name))
                      first
                      kind)]
    entry
    (throw (ex-info "cannot find kube config entry"
                    {:config config :kind kind :entry-name entry-name}))))

(defn context
  ([config context-name]
   (let [context (find-entry config :context context-name)
         cluster (find-entry config :cluster (:cluster context))
         user (find-entry config :user (:user context))
         b64 (Base64/getDecoder)
         parse-pem #(-> (.decode b64 %) pem/parse)]
     {:cluster (-> cluster
                   (update :certificate-authority-data parse-pem))
      :user (-> user
                (update :client-certificate-data parse-pem)
                (update :client-key-data parse-pem))}))
  ([context-name]
   (let [config (load)]
     (context config context-name))))
