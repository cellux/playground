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

(def Array oben.core.types.Array/Array)

(clj/defmacro with-target
  [& args]
  `(target/with-target ~@args))

(clj/defmacro with-temp-target-of-type
  [type & body]
  `(let [t# (target/create {:type ~type})
         result# (target/with-target t# ~@body)]
     (target/dispose t#)
     result#))

(clj/defmacro with-temp-target
  [& body]
  `(with-temp-target-of-type :inprocess ~@body))

(clj/defmacro with-dump-target
  [& body]
  `(with-temp-target-of-type :dump ~@body))

(clj/defn fn?
  "Returns true if the argument is a Clojure wrapper around an Oben function."
  [x]
  (and (clj/fn? x) (o/has-kind? :oben/FN x)))

(clj/defn make-fn
  [name params body]
  (let [parse-fn (memoize
                  (clj/fn [target]
                    (-> (ast/parse `(~'fn ~params ~@body))
                        (vary-meta assoc :name name))))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [target (target/current)
              fnode (parse-fn target)]
          (target/invoke-function target fnode args)))
      {:kind :oben/FN
       :parse-fn parse-fn})))

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
