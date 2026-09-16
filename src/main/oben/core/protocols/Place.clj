(ns oben.core.protocols.Place
  (:refer-clojure :exclude [load volatile?])
  (:require [oben.core.api :as o]))

;; A Place represents addressable storage. Containers expose access paths via
;; `at`; places provide the primitive read/write operations for those paths.
(derive :oben/Place :oben/Any)

(defn place?
  "Returns true when value is an addressable Oben place."
  [value]
  (and (o/node? value)
       (isa? (o/tid-of-node value) :oben/Place)))

(o/defmulti address-of)
(o/defmulti load)
(o/defmulti store!)
(o/defmulti writable?)
(o/defmulti volatile?)

(defn update!
  "Loads `place`, applies `update-fn`, stores the result, and returns it.

  The same place node is used for the load and store, so an address-producing
  expression is represented once in the generated Oben expression."
  [place update-fn]
  (store! place (update-fn (load place))))

(defmethod address-of :default
  [value]
  (throw (ex-info "value is not an addressable place"
                  {:value value})))

(defmethod writable? :default
  [_]
  false)

(defmethod volatile? :default
  [_]
  false)
