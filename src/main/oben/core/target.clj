(ns oben.core.target
  (:require [clojure.string :as str])
  (:require [oben.core.protocols.Target :as Target]))

(def ^:dynamic *current-target* nil)

(defn get-ns-binding
  [ns child sym]
  (let [child-ns (symbol (str/join \. (map name [ns child])))]
    (require child-ns)
    (when-let [v (ns-resolve child-ns sym)]
      (var-get v))))

(defn create
  [{:keys [type] :as opts}]
  (if-let [constructor (get-ns-binding 'oben.core.target type 'create)]
    (atom (constructor opts))
    (throw (ex-info "unknown target type" {:type type}))))

(defn ctx
  [this]
  (:ctx @this))

(defn platform
  [this]
  (Target/platform @this))

(defn invoke-function
  [this fnode args]
  (swap! this Target/compile-function fnode)
  (Target/invoke-function @this fnode args))

(defn dispose
  [this]
  (Target/dispose @this)
  (reset! this nil))
