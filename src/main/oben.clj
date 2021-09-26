(ns oben
  (:refer-clojure :exclude [fn fn? defn defmacro defmulti defmethod])
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types.Array])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.engine :as llvm-engine]))

(def ^:dynamic *ctx* (ctx/create))

(clj/defn set-ctx!
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
  `(with-context (ctx/create)
     ~@body))

(def Array oben.core.types.Array/Array)

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
          (set-ctx! ctx)
          result))
      {:kind :oben/FN :oben/node fnode})))

(clj/defn fn?
  "Returns true if the argument is a Clojure wrapper around an Oben function."
  [x]
  (and (clj/fn? x) (o/has-kind? :oben/FN x)))

(clj/defmacro fn
  [& decl]
  (let [[params body] (o/split-after vector? decl)
        params (first (o/move-types-to-tags params))
        _ (assert (vector? params))
        params (o/quote-all-except-locals params &env)]
    `(make-fn nil ~params '~body)))

(clj/defmacro defn
  [name & decl]
  (let [[params body] (o/split-after vector? decl)
        params (first (o/move-types-to-tags params))
        _ (assert (vector? params))
        params (o/quote-all-except-locals params &env)]
    `(def ~name (make-fn '~name ~params '~body))))

(clj/defmacro define-typeclass
  [& args]
  `(o/define-typeclass ~@args))

(clj/defmacro defmacro
  [& args]
  `(o/defmacro ~@args))

(clj/defmacro defmulti
  [& args]
  `(o/defmulti ~@args))

(clj/defmacro defmethod
  [& args]
  `(o/defmethod ~@args))

(require 'oben.core)
