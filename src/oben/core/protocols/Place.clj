(ns oben.core.protocols.Place
  (:refer-clojure :exclude [load volatile?])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx]))

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

(defn- update-and-return
  "Builds a single-evaluation read/modify/write expression.

  `return-old?` selects whether the expression produces the value loaded before
  the update or the value stored after it.  The place node is shared by the
  load and store, so an address-producing expression is evaluated once."
  [place update-fn return-old?]
  (when-not (place? place)
    (throw (ex-info "update requires an addressable place"
                    {:place place})))
  (when-not (writable? place)
    (throw (ex-info "update requires a writable place"
                    {:place place})))
  (let [old-node (load place)
        new-node (update-fn old-node)
        store-node (store! place new-node)
        result-node (if return-old? old-node store-node)
        result-type (o/type-of old-node)]
    (o/make-node
     result-type
     (fn [ctx]
       (let [ctx (ctx/compile-node ctx store-node)]
         (ctx/save-ir ctx (ctx/compiled-node ctx result-node))))
     {:class :oben/place-update
      :return (if return-old? :old :new)})))

(defn pre-update!
  "Updates `place` and returns the new value.

  This is the value-producing form used by prefix update operators."
  [place update-fn]
  (update-and-return place update-fn false))

(defn post-update!
  "Updates `place` and returns the old value.

  This is the value-producing form used by postfix update operators."
  [place update-fn]
  (update-and-return place update-fn true))

(defn update!
  "Loads `place`, applies `update-fn`, stores the result, and returns it.

  Kept as the traditional alias for `pre-update!`; existing compound
  assignments therefore continue to return the stored value."
  [place update-fn]
  (pre-update! place update-fn))

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
