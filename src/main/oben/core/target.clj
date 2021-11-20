(ns oben.core.target
  (:require [clojure.string :as str])
  (:require [oben.core.protocols.Target :as Target]))

(defn get-constructor-by-type
  [type]
  (let [child-ns (symbol (str "oben.core.target." (name type)))]
    (require child-ns)
    (when-let [v (ns-resolve child-ns 'create)]
      (var-get v))))

(defn create
  [{:keys [type] :as opts}]
  (if-let [constructor (get-constructor-by-type type)]
    (atom (constructor opts))
    (throw (ex-info "unknown target type" {:type type}))))

(def ^:dynamic *current-target* nil)

(defn current
  []
  (when (nil? *current-target*)
    (let [default-target (create {:type :inprocess})]
      (alter-var-root #'*current-target* (constantly default-target))))
  *current-target*)

(defn ctx
  [this]
  (:ctx @this))

(defn attrs
  [this]
  (:attrs @this))

(defn getattr
  [this name]
  (let [attr-map (attrs this)]
    (if (contains? attr-map name)
      (get attr-map name)
      (throw (ex-info "missing target attribute" {:name name})))))

(defn invoke-function
  [this fnode args]
  (swap! this Target/compile-function fnode)
  (Target/invoke-function @this fnode args))

(defn dispose
  [this]
  (Target/dispose @this)
  (reset! this nil))

(defmacro with-target
  [t & body]
  (cond (keyword? t) `(with-target {:type ~t} ~@body)
        (map? t) `(with-target (create ~t) ~@body)
        :else `(binding [*current-target* ~t] ~@body)))
