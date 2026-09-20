(ns oben.core.protocols.Callable
  "Semantic dispatch for Oben function calls.

   Callers pass the callee and a vector of already-parsed argument nodes.
   Dispatch is based on the callable's function-type metadata rather than the
   JVM class of the node, since Oben nodes are represented by function values."
  (:require [clojure.core :as clj]
            [oben.core.api :as o]))

(defn call-semantics
  "Returns the call-semantics tag for a callable node.

   Function nodes are pointers to `Fn` types, so the metadata describing call
   semantics lives on the pointed-to function type rather than on the pointer
   type itself."
  [callee]
  (let [type (o/type-of callee)
        object-type (:object-type (meta type))]
    (or (:call-semantics (meta object-type))
        :oben)))

(clj/defmulti call
  (fn [callee _args]
    (call-semantics callee)))
