(ns oben
  (:refer-clojure :exclude [fn defn])
  (:require [clojure.core :as clj])
  (:require [oben.context :as ctx])
  (:require [oben.ast :as ast])
  (:require [oben.core]))

(def
  ^{:dynamic true}
  *ctx* (ctx/new))

(defmacro with-context
  [ctx & body]
  `(binding [*ctx* ~ctx]
     ~@body))

(clj/defn make-fn
  [params body]
  (let [fnode (ast/parse (list* 'fn params body))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [ctx (ctx/compile-node (ctx/next-epoch *ctx*) fnode)
              f (ctx/compiled ctx fnode)
              ctx (ctx/assemble-module ctx)
              invoker (ctx/invoker ctx f)
              result (apply invoker args)]
          (if (thread-bound? #'*ctx*)
            (set! *ctx* ctx)
            (intern 'oben '*ctx* ctx))
          result))
      {:oben/FNODE fnode})))

(defmacro fn
  [params & body]
  `(make-fn '~params '~body))

(defmacro defn
  [name params & body]
  `(def ~name (make-fn '~params '~body)))
