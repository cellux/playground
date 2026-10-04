(ns oben.core.protocols.Callable
  "Semantic dispatch for Oben function calls.

   Callers pass the callee and a vector of already-parsed argument nodes.
   Dispatch is based on the callable's function-type metadata rather than the
   JVM class of the node, since Oben nodes are represented by function values."
  (:require [clojure.core :as clj]
            [oben.core.api :as o]))

(defn semantics
  "Returns the language-semantics tag for a callable node.

   Function nodes are pointers to `Fn` types, so the metadata describing their
   semantics lives on the pointed-to function type."
  [callee]
  (let [type (o/type-of callee)
        object-type (:object-type (meta type))]
    (or (:semantics (meta object-type))
        :oben)))

(clj/defmulti call
  (fn [callee _args]
    (semantics callee)))
