(ns oben
  (:refer-clojure :exclude [fn fn? defn defmacro defmulti defmethod])
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.ast :as ast])
  (:require [oben.core.target :as target])
  (:require [oben.core.types.Array])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.engine :as llvm-engine]))

(clj/defn create-inprocess-target
  []
  (target/create {:type :inprocess}))

(when (nil? oben.core.target/*current-target*)
  (intern 'oben.core.target '*current-target* (create-inprocess-target)))

(clj/defmacro with-target
  [t & body]
  `(binding [oben.core.target/*current-target* ~t]
     ~@body))

(clj/defmacro with-temp-target
  [& body]
  `(let [t# (create-inprocess-target)
         result# (with-target t# ~@body)]
     (target/dispose t#)
     result#))

(def Array oben.core.types.Array/Array)

(clj/defn make-fn
  [name params body]
  (let [meta {:kind :oben/FN
              :platform->fnode (atom {})}]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [platform (target/platform oben.core.target/*current-target*)
              fnodes (:platform->fnode meta)
              fnode (or (get @fnodes platform)
                        (let [fnode (-> (ast/parse `(fn ~params ~@body))
                                        (vary-meta assoc :name name))]
                          (swap! fnodes assoc platform fnode)
                          fnode))]
          (target/invoke-function oben.core.target/*current-target* fnode args)))
      meta)))

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
