(ns oben.util
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core :as clj]))

(defn drop-meta
  [x]
  (with-meta x nil))

(defn resolve
  ([sym env]
   (or (get env sym)
       (when-let [v (or (let [sym-without-ns (symbol (name sym))]
                          (ns-resolve (the-ns 'oben.core)
                                      sym-without-ns))
                        (clj/resolve sym))]
         (var-get v))
       (throw (ex-info "cannot resolve symbol" {:sym sym}))))
  ([sym]
   (resolve sym {})))

(defn resolve-type-from-meta
  [x]
  (if-let [m (meta x)]
    (if-let [tag (:tag m)]
      (resolve tag)
      (throw (ex-info "no tag field in metadata of value"
                      {:value x
                       :meta m})))
    (throw (ex-info "no metadata on value"
                    {:value x}))))
