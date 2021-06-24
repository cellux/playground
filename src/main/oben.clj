(ns oben
  (:refer-clojure :exclude [fn defn defmacro defmulti defmethod])
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.types.Array :as Array])
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

(def Array Array/Array)

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
  (let [quote-preserving-meta (clj/fn [form]
                                `(with-meta '~form ~(meta form)))
        quote-tag-if-unbound-symbol (clj/fn [form]
                                      (let [m (meta form)]
                                        (if (and (symbol? (:tag m))
                                                 (not (contains? env (:tag m))))
                                          (vary-meta form update :tag #(list 'quote %))
                                          form)))
        [params body] (o/sanitize-typed-forms decl env vector? false
                                              quote-tag-if-unbound-symbol)]
    (vector
     `(with-meta
        (vector ~@(o/sanitize-typed-forms params env symbol? true
                                          (comp quote-preserving-meta
                                                quote-tag-if-unbound-symbol)))
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
