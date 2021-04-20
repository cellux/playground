(ns oben
  (:refer-clojure :exclude [fn defn defmulti defmacro])
  (:require [clojure.core :as clj])
  (:require [oben.context :as ctx])
  (:require [oben.types :as t])
  (:require [oben.ast :as ast]))

(def
  ^{:dynamic true}
  *ctx* (ctx/new))

(clj/defn alter-*ctx*!
  [new-ctx]
  (if (thread-bound? #'*ctx*)
    (set! *ctx* new-ctx)
    (intern 'oben '*ctx* new-ctx)))

(clj/defmacro with-context
  [ctx & body]
  `(binding [*ctx* ~ctx]
     ~@body))

(clj/defn make-fn
  [name params body]
  (let [fnode (-> (ast/parse {} (list* 'fn params body))
                  (vary-meta assoc :name name))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [ctx (ctx/compile-node (ctx/next-epoch *ctx*) fnode)
              f (ctx/compiled ctx fnode)
              ctx (ctx/assemble-module ctx)
              invoker (ctx/invoker ctx f)
              result (apply invoker args)]
          (alter-*ctx*! ctx)
          result))
      {:kind :oben/FN :fnode fnode})))

(clj/defmacro fn
  [params & body]
  `(make-fn nil '~params '~body))

(clj/defmacro defn
  [name params & body]
  `(def ~name (make-fn '~name '~params '~body)))

(clj/defmacro defmacro
  [& args]
  `(let [m# (clj/defmacro ~@args)]
     (alter-meta! m# dissoc :macro)
     (alter-var-root m# vary-meta assoc :kind :oben/MACRO)
     m#))

(clj/defmacro defmulti
  [name]
  `(clj/defmulti ~name t/params->typeclasses))

(require '(oben core))
