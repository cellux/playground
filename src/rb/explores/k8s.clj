(ns rb.explores.k8s
  (:require [clojure.java.io :as jio]
            [clojure.java.data :as jdata]
            [clojure.string :as str])
  (:import (io.kubernetes.client.util KubeConfig ClientBuilder)
           (io.kubernetes.client.openapi Configuration)
           (io.kubernetes.client.openapi.apis CoreV1Api)))

(def kube-config-path
  (format "%s/%s"
          (System/getenv "HOME")
          ".kube/config"))

(defn read-kube-config
  [path]
  (-> (jio/reader kube-config-path)
      (KubeConfig/loadKubeConfig)))

(defn build-client
  [kube-config]
  (.build (ClientBuilder/kubeconfig kube-config)))

(defn setup-default-api-client
  []
  (-> kube-config-path
      read-kube-config
      build-client
      (Configuration/setDefaultApiClient))
  :done)

(defn get-api-resources
  []
  (let [api (CoreV1Api.)]
    (jdata/from-java-deep (.getAPIResources api) {})))

(defn lower-case-first-char
  [s]
  (str/replace-first s #"\w" str/lower-case))

(defn camel-case
  [sym]
  (->> (str/split (name sym) #"-")
       (map str/capitalize)
       (apply str)))

(defmacro def-api-generator
  [name name-expr args options]
  `(defmacro ~name
     [~'name]
     (let [~'full-name (symbol ~name-expr)
           ~'camel-name (camel-case ~'full-name)
           ~'method-name (lower-case-first-char ~'camel-name)]
       `(defn ~~'full-name
          [~@'~args & {:keys ~'~options}]
          (let [~'~'api (CoreV1Api.)]
            (jdata/from-java-deep
             (~(symbol (str "." ~'method-name)) ~'~'api ~@'~args ~@'~options)
             {:omit #{:uncachedZone}}))))))

;; CREATE

(def-api-generator def-create-api
  (str "create-" name)
  [body]
  [pretty dry-run field-manager])

(def-create-api namespace)
(def-create-api node)
(def-create-api persistent-volume)

(def-api-generator def-create-namespaced-api-v1
  (str "create-namespaced-" name)
  [ns body]
  [dry-run field-manager pretty])

(def-create-namespaced-api-v1 binding)

(def-api-generator def-create-namespaced-api-v2
  (str "create-namespaced-" name)
  [ns body]
  [pretty dry-run field-manager])

(def-create-namespaced-api-v2 config-map)
(def-create-namespaced-api-v2 endpoints)
(def-create-namespaced-api-v2 event)
(def-create-namespaced-api-v2 limit-range)
(def-create-namespaced-api-v2 persistent-volume-claim)
(def-create-namespaced-api-v2 pod)
(def-create-namespaced-api-v2 pod-template)
(def-create-namespaced-api-v2 replication-controller)
(def-create-namespaced-api-v2 resource-quota)
(def-create-namespaced-api-v2 secret)
(def-create-namespaced-api-v2 service)
(def-create-namespaced-api-v2 service-account)

(def-api-generator def-create-namespaced-api-v3
  (str "create-namespaced-" name)
  [name ns body]
  [dry-run field-manager pretty])

(def-create-namespaced-api-v3 pod-binding)
(def-create-namespaced-api-v3 pod-eviction)
(def-create-namespaced-api-v3 service-account-token)

;; LIST

(def-api-generator def-list-api-v1
  (str "list-" name)
  []
  [pretty allow-watch-bookmarks continue field-selector label-selector
   limit resource-version timeout-seconds watch])

(def-list-api-v1 namespace)
(def-list-api-v1 node)
(def-list-api-v1 persistent-volume)

(def-api-generator def-list-api-v2
  (str "list-" name)
  []
  [allow-watch-bookmarks continue field-selector label-selector
   limit pretty resource-version timeout-seconds watch])

(def-list-api-v2 component-status)

(def-api-generator def-list-for-all-namespaces-api
  (str "list-" name "-for-all-namespaces")
  []
  [allow-watch-bookmarks continue field-selector label-selector
   limit pretty resource-version timeout-seconds watch])

(def-list-for-all-namespaces-api config-map)
(def-list-for-all-namespaces-api endpoints)
(def-list-for-all-namespaces-api event)
(def-list-for-all-namespaces-api limit-range)
(def-list-for-all-namespaces-api persistent-volume-claim)
(def-list-for-all-namespaces-api pod)
(def-list-for-all-namespaces-api pod-template)
(def-list-for-all-namespaces-api replication-controller)
(def-list-for-all-namespaces-api resource-quota)
(def-list-for-all-namespaces-api secret)
(def-list-for-all-namespaces-api service-account)
(def-list-for-all-namespaces-api service)

(def-api-generator def-list-namespaced-api
  (str "list-namespaced-" name)
  [ns]
  [pretty allow-watch-bookmarks continue field-selector label-selector
   limit resource-version timeout-seconds watch])

(def-list-namespaced-api config-map)
(def-list-namespaced-api endpoints)
(def-list-namespaced-api event)
(def-list-namespaced-api limit-range)
(def-list-namespaced-api persistent-volume-claim)
(def-list-namespaced-api pod)
(def-list-namespaced-api pod-template)
(def-list-namespaced-api replication-controller)
(def-list-namespaced-api resource-quota)
(def-list-namespaced-api secret)
(def-list-namespaced-api service)
(def-list-namespaced-api service-account)

;; PATCH

(def-api-generator def-patch-api
  (str "patch-" name)
  [name body]
  [pretty dry-run field-manager force])

(def-patch-api namespace)
(def-patch-api namespace-status)
(def-patch-api node)
(def-patch-api node-status)
(def-patch-api persistent-volume)
(def-patch-api persistent-volume-status)

(def-api-generator def-patch-namespaced-api
  (str "patch-namespaced-" name)
  [name ns body]
  [pretty dry-run field-manager force])

(def-patch-namespaced-api config-map)
(def-patch-namespaced-api endpoints)
(def-patch-namespaced-api event)
(def-patch-namespaced-api limit-range)
(def-patch-namespaced-api persistent-volume-claim)
(def-patch-namespaced-api persistent-volume-claim-status)
(def-patch-namespaced-api pod)
(def-patch-namespaced-api pod-status)
(def-patch-namespaced-api pod-template)
(def-patch-namespaced-api replication-controller)
(def-patch-namespaced-api replication-controller-scale)
(def-patch-namespaced-api replication-controller-status)
(def-patch-namespaced-api resource-quota)
(def-patch-namespaced-api resource-quota-status)
(def-patch-namespaced-api secret)
(def-patch-namespaced-api service)
(def-patch-namespaced-api service-account)
(def-patch-namespaced-api service-status)

;; READ

(def-api-generator def-read-api-v1
  (str "read-" name)
  [name]
  [pretty])

(def-read-api-v1 component-status)
(def-read-api-v1 namespace-status)
(def-read-api-v1 node-status)
(def-read-api-v1 persistent-volume-status)

(def-api-generator def-read-api-v2
  (str "read-" name)
  [name]
  [pretty exact export])

(def-read-api-v2 namespace)
(def-read-api-v2 node)
(def-read-api-v2 persistent-volume)

(def-api-generator def-read-namespaced-api-v1
  (str "read-namespaced-" name)
  [name ns]
  [pretty exact export])

(def-read-namespaced-api-v1 config-map)
(def-read-namespaced-api-v1 endpoints)
(def-read-namespaced-api-v1 event)
(def-read-namespaced-api-v1 limit-range)
(def-read-namespaced-api-v1 persistent-volume-claim)
(def-read-namespaced-api-v1 pod)
(def-read-namespaced-api-v1 pod-template)
(def-read-namespaced-api-v1 replication-controller)
(def-read-namespaced-api-v1 resource-quota)
(def-read-namespaced-api-v1 secret)
(def-read-namespaced-api-v1 service)
(def-read-namespaced-api-v1 service-account)

(def-api-generator def-read-namespaced-api-v2
  (str "read-namespaced-" name)
  [name ns]
  [pretty])

(def-read-namespaced-api-v2 persistent-volume-claim-status)
(def-read-namespaced-api-v2 pod-status)
(def-read-namespaced-api-v2 replication-controller-scale)
(def-read-namespaced-api-v2 replication-controller-status)
(def-read-namespaced-api-v2 resource-quota-status)
(def-read-namespaced-api-v2 service-status)

(def-api-generator def-read-namespaced-api-v3
  (str "read-namespaced-" name)
  [name ns container]
  [follow insecure-skip-tls-verify-backend limit-bytes pretty previous since-seconds tail-lines timestamps])

(def-read-namespaced-api-v3 podlog)

;; REPLACE

(def-api-generator def-replace-api
  (str "replace-" name)
  [name body]
  [pretty dry-run field-manager])

(def-replace-api namespace)
(def-replace-api namespace-finalize)
(def-replace-api namespace-status)
(def-replace-api node)
(def-replace-api node-status)
(def-replace-api persistent-volume)
(def-replace-api persistent-volume-status)

(def-api-generator def-replace-namespaced-api
  (str "replace-namespaced-" name)
  [name ns body]
  [pretty dry-run field-manager])

(def-replace-namespaced-api config-map)
(def-replace-namespaced-api endpoints)
(def-replace-namespaced-api event)
(def-replace-namespaced-api limit-range)
(def-replace-namespaced-api persistent-volume-claim)
(def-replace-namespaced-api persistent-volume-claim-status)
(def-replace-namespaced-api pod)
(def-replace-namespaced-api pod-status)
(def-replace-namespaced-api pod-template)
(def-replace-namespaced-api replication-controller)
(def-replace-namespaced-api replication-controller-scale)
(def-replace-namespaced-api replication-controller-status)
(def-replace-namespaced-api resource-quota)
(def-replace-namespaced-api resource-quota-status)
(def-replace-namespaced-api secret)
(def-replace-namespaced-api service)
(def-replace-namespaced-api service-account)
(def-replace-namespaced-api service-status)
