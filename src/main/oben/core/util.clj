(ns oben.core.util
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core :as clj]))

(defn drop-meta
  [x]
  (with-meta x nil))

(defn- oben-fn?
  [x]
  (and (fn? x) (= :oben/FN (:kind (meta x)))))

(defn- find-oben-var
  [sym]
  (let [sym-without-ns (symbol (name sym))]
    (ns-resolve (the-ns 'oben.core) sym-without-ns)))

(defn- find-clojure-var
  [sym]
  (clj/resolve sym))

(defn resolve
  ([sym env]
   (or (get env sym)
       (when-let [v (or (find-oben-var sym)
                        (find-clojure-var sym))]
         (let [value (var-get v)]
           (if (oben-fn? value)
             (:fnode (meta value))
             value)))
       (throw (ex-info "cannot resolve symbol" {:sym sym}))))
  ([sym]
   (resolve sym {})))

(defn resolve-type-from-meta
  [x]
  (if-let [m (meta x)]
    (if-let [tag (:tag m)]
      (if (symbol? tag)
        (resolve tag)
        (eval tag))
      (throw (ex-info "no tag field in metadata of value"
                      {:value x :meta m})))
    (throw (ex-info "no metadata on value"
                    {:value x}))))
