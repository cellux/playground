(ns oben
  (:refer-clojure :exclude [fn fn? defn
                            struct defstruct
                            defmacro defmulti defmethod])
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
  (:require [oben.core.context :as ctx])
  (:require [oben.core.target :as target])
  (:require [oben.core.types.Array])
  (:require [oben.core.types.Struct])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.engine :as llvm-engine]))

(def Array oben.core.types.Array/Array)
(def Struct oben.core.types.Struct/Struct)

(clj/defmacro with-target
  [t & body]
  `(target/with-target ~t ~@body))

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
  (let [parse-for-target (memoize
                          (clj/fn [target]
                            (-> (o/parse (list* 'fn params body))
                                (vary-meta assoc :name name))))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params)))
        (let [target (target/current)
              fnode (parse-for-target target)]
          (target/invoke-function target fnode args)))
      {:kind :oben/FN
       :parse-for-target parse-for-target})))

(clj/defmacro fn
  [& decl]
  (let [[params body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta params))
        _ (assert (vector? params))
        params (o/quote-all-except-locals params &env)]
    `(make-fn nil ~params '~body)))

(clj/defmacro defn
  [name & decl]
  (let [[params body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta params))
        _ (assert (vector? params))
        params (o/quote-all-except-locals params &env)]
    `(def ~name (make-fn '~name ~params '~body))))

(clj/defn make-struct
  [name fields]
  (let [parse-for-target (memoize
                          (clj/fn [target]
                            (-> (o/parse (list 'oben.core.types.Struct/Struct fields))
                                (vary-meta assoc :name name))))]
    (with-meta
      {}
      {:kind :oben/STRUCT
       :parse-for-target parse-for-target})))

(clj/defmacro struct
  [fields]
  (let [fields (o/move-types-to-meta fields)
        _ (assert (vector? fields))
        fields (o/quote-all-except-locals fields &env)]
    `(make-struct nil ~fields)))

(clj/defmacro defstruct
  [name fields]
  (let [fields (o/move-types-to-meta fields)
        _ (assert (vector? fields))
        fields (o/quote-all-except-locals fields &env)]
    `(def ~name (make-struct '~name ~fields))))

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
