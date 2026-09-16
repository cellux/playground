(ns oben.core.protocols.Container
  (:refer-clojure :exclude [get-in get load
                            assoc assoc-in assoc!])
  (:require [oben.core.api :as o]))

(o/defmulti get-in)
(o/defmulti get)

;; Addressable access. `get` reads a value; `at` returns a place that can
;; subsequently be used by generic mutation operations such as `set!`.
(o/defmulti at-in)
(o/defmulti at)

(defn- non-addressable!
  [operation value path]
  (throw (ex-info (str operation " requires an addressable value")
                  {:operation operation
                   :value value
                   :path path})))

(defmethod at-in :default
  [value path]
  (non-addressable! 'at-in value path))

(defmethod at :default
  [value key]
  (non-addressable! 'at value [key]))

(o/defmulti assoc-in!)
(o/defmulti assoc!)

;; Compatibility aliases for the former mutation names.
(def put-in! assoc-in!)
(def put! assoc!)

(o/defmulti assoc-in)
(o/defmulti assoc)
