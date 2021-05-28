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
  (let [sym-without-ns (symbol (name sym))
        v (ns-resolve (the-ns 'oben.core) sym-without-ns)]
    (if (= (:ns (meta v)) (the-ns 'oben.core))
      v nil)))

(defn- find-clojure-var
  [sym]
  (let [v (clj/resolve sym)]
    (if (= (:ns (meta v)) (the-ns 'clojure.core))
      (throw (ex-info "oben symbol resolves to a var in clojure.core" {:sym sym}))
      v)))

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
