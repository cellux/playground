(ns oben
  (:refer-clojure :exclude [fn defn])
  (:require [clojure.core :as clj])
  (:require [oben.context :as ctx])
  (:require [oben.ast :as ast])
  (:require [oben.core])
  (:require [midje.sweet :as m]))

(def
  ^{:dynamic true}
  *ctx* (ctx/new))

(defmacro with-context
  [ctx & body]
  `(binding [*ctx* ~ctx]
     ~@body))

(defmacro with-temp-context
  [& body]
  `(let [ctx# (ctx/new)]
     (binding [*ctx* ctx#]
       (let [result# (do ~@body)]
         (ctx/dispose ctx#)
         result#))))

(clj/defn make-fn
  [params body]
  (let [fnode (ast/parse (list* 'fn params body))]
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
        result))))

(defmacro fn
  [params & body]
  `(make-fn '~params '~body))

(with-temp-context
  (let [f (oben/fn ^i32 [^i32 x ^i32 y] (+ x y))]
    (m/fact (f 1 2) => 3)
    (m/fact (f 5 3) => 8)))
