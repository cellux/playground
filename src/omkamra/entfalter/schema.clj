(ns omkamra.entfalter.schema
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]))

(defn namespaced-keyword?
  [x]
  (and (keyword? x) (namespace x)))

(def NamespacedKeyword
  [:fn {:error/message "must be a namespaced keyword"} namespaced-keyword?])

(defn malli-schema?
  [x]
  (try
    (m/schema x)
    true
    (catch Throwable _
      false)))

(def MalliSchema
  [:fn {:error/message "must be a valid malli schema"} malli-schema?])

(def PluginSpec
  [:map
   [:fact-dependencies [:set NamespacedKeyword]]
   [:config-schema MalliSchema]
   [:apply-ref NamespacedKeyword]])

(def FactSpec
  [:map
   [:fact-key string?]
   [:collector-ref NamespacedKeyword]])

(def PluginsMap
  [:map-of NamespacedKeyword any?])

(def SSHConnection
  [:map
   [:host string?]
   [:port {:optional true} int?]
   [:user {:optional true} string?]
   [:username {:optional true} string?]
   [:password {:optional true} string?]
   [:timeout-ms {:optional true} int?]
   [:verify-host-key? {:optional true} boolean?]
   [:python-command {:optional true} string?]])

(def ReconcileConfig
  [:map
   [:type keyword?]
   [:connection SSHConnection]
   [:plugins PluginsMap]])

(def entfalter-transformer
  (mt/transformer
   (mt/key-transformer {:encode name
                        :decode keyword})
   {:name :entfalter}))

(defn validate!
  ([schema value message]
   (validate! schema value message {}))
  ([schema value message data]
   (if (m/validate schema value)
     value
     (throw (ex-info message
                     (merge data
                            {:value value
                             :errors (-> (m/explain schema value)
                                         me/humanize)}))))))

(defn normalize-for-py
  [schema value]
  (let [decoded (m/decode schema value entfalter-transformer)]
    (m/encode schema decoded entfalter-transformer)))
