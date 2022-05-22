(ns oben.core
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

(clj/defmacro with-target
  [t & body]
  `(target/with-target ~t ~@body))

(clj/defmacro with-lexical-bindings
  [sym & body]
  `(let [~sym ~(into {} (for [k (keys &env)]
                          [`(quote ~k) k]))]
     ~@body))

(clj/defn fn?
  "Returns true if the argument is a Clojure wrapper around an Oben function."
  [x]
  (and (clj/fn? x) (o/has-kind? :oben/FN x)))

(clj/defn make-fn
  [name params body lexical-bindings]
  (let [parse-for-target (memoize
                          (clj/fn [target]
                            (-> (o/parse (list* 'fn params body)
                                         lexical-bindings)
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
        _ (assert (vector? params))]
    `(with-lexical-bindings bindings#
       (make-fn nil '~params '~body bindings#))))

(clj/defmacro defn
  [name & decl]
  (let [[params body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta params))
        _ (assert (vector? params))]
    `(with-lexical-bindings bindings#
       (def ~name (make-fn '~name '~params '~body bindings#)))))

(clj/defn make-struct-type
  [name fields lexical-bindings]
  (let [opts (if name {:name name} nil)
        parse-for-target (memoize
                          (clj/fn [target]
                            (o/parse (list 'Struct fields opts) lexical-bindings)))]
    (with-meta
      #(parse-for-target (target/current))
      {:kind :oben/STRUCT
       :parse-for-target parse-for-target})))

(clj/defmacro Struct
  [fields]
  (let [fields (o/move-types-to-meta fields)
        _ (assert (vector? fields))]
    `(with-lexical-bindings bindings#
       (make-struct-type nil '~fields bindings#))))

(clj/defmacro defstruct
  [name fields]
  (let [fields (o/move-types-to-meta fields)
        _ (assert (vector? fields))]
    `(with-lexical-bindings bindings#
       (def ~name (make-struct-type '~name '~fields bindings#)))))

(clj/defn make-array-type
  [name element-type size lexical-bindings]
  (let [opts (if name {:name name} nil)
        parse-for-target (memoize
                          (clj/fn [target]
                            (o/parse (list 'Array element-type size opts) lexical-bindings)))]
    (with-meta
      #(parse-for-target (target/current))
      {:kind :oben/ARRAY
       :parse-for-target parse-for-target})))

(clj/defmacro Array
  [element-type size]
  `(with-lexical-bindings bindings#
     (make-array-type nil '~element-type '~size bindings#)))

;; defarray was here but it felt awful
;;
;; I wonder why defstruct doesn't evoke the same feelings

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

(require 'oben.core.keywords)
