(ns hooks.assembler
  (:require [clj-kondo.hooks-api :as api]))

(defn- define-var
  [{:keys [node]}]
  (let [[_ name] (api/sexpr node)]
    {:node (api/list-node [(api/token-node 'def)
                           (api/token-node name)
                           (api/token-node nil)])}))

(defn define-asm
  "`define-asm` binds a Var; its body is quoted assembly data, not code, so it
  must not be linted as ordinary Clojure."
  [context]
  (define-var context))

(defn define-resource
  "`define-resource` binds a Var; its path and metadata are asset data."
  [context]
  (define-var context))

(defn asm
  "`asm` returns deferred assembly data; its body is not code to lint."
  [context]
  {:node (api/token-node nil)})
