(ns oben
  (:refer-clojure :exclude [fn defn defmulti defmethod defmacro])
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.engine :as llvm-engine]))

(def
  ^{:dynamic true}
  *ctx* (ctx/new))

(clj/defn set-*ctx*!
  [new-ctx]
  (if (thread-bound? #'*ctx*)
    (set! *ctx* new-ctx)
    (intern 'oben '*ctx* new-ctx)))

(clj/defmacro with-context
  [ctx & body]
  `(binding [*ctx* ~ctx]
     (let [had-llvm-context# (ctx/get-llvm-context *ctx*)
           had-llvm-ee# (ctx/get-llvm-execution-engine *ctx*)
           result# (do ~@body)]
       (when-not had-llvm-ee#
         (when-let [llvm-ee# (ctx/get-llvm-execution-engine *ctx*)]
           (llvm-engine/dispose llvm-ee#)))
       (when-not had-llvm-context#
         (when-let [llvm-context# (ctx/get-llvm-context *ctx*)]
           (llvm-context/dispose llvm-context#)))
       result#)))

(clj/defmacro with-temp-context
  [& body]
  `(with-context (ctx/new)
     ~@body))

(clj/defn make-fn
  [name params body]
  (let [fnode (-> (ast/parse `(fn ~params ~@body))
                  (vary-meta assoc :name name))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [ctx (ctx/compile-node (ctx/next-epoch *ctx*) fnode)
              f (ctx/compiled-node ctx fnode)
              ctx (ctx/assemble-module ctx)
              invoker (ctx/invoker ctx f)
              result (apply invoker args)]
          (set-*ctx*! ctx)
          result))
      {:kind :oben/FN :fnode fnode})))

(clj/defn build-make-fn-args
  [decl env]
  (let [[params body] (o/sanitize-typed-forms decl env vector? false false)]
    (vector
     `(with-meta
        (vector ~@(o/sanitize-typed-forms params env symbol? true true))
        ~(meta params))
     `(quote ~body))))

(clj/defmacro fn
  [& decl]
  (let [[params body] (build-make-fn-args decl &env)]
    `(make-fn nil ~params ~body)))

(clj/defmacro defn
  [name & decl]
  (let [[params body] (build-make-fn-args decl &env)]
    `(def ~name (make-fn '~name ~params ~body))))

(clj/defmacro defmacro
  [& args]
  `(let [m# (clj/defmacro ~@args)]
     (alter-meta! m# dissoc :macro)
     (alter-var-root m# vary-meta assoc :kind :oben/MACRO)
     m#))

(def params->tids (comp (partial mapv o/tid-of-value) vector))

(clj/defmacro defmulti
  [name]
  `(clj/defmulti ~name params->tids))

(clj/defmacro defmethod
  [multifn dispatch-val & fn-tail]
  (assert (vector? dispatch-val))
  `(clj/defmethod ~multifn
     ~(mapv (comp o/tid-of-type-or-typeclass eval) dispatch-val)
     ~@fn-tail))

(require 'oben.core)
