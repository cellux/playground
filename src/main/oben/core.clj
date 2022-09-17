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

(clj/defn candidate-library-filenames
  [library-name]
  (vector (str "lib" library-name ".so")))

(clj/defn load-library
  [library-name]
  (loop [candidate-filenames (candidate-library-filenames library-name)]
    (if-let [filename (first candidate-filenames)]
      (let [result (llvm-engine/load-library filename)]
        (when-not (zero? result)
          (recur (next candidate-filenames))))
      (throw (ex-info "failed to load dynamic library" {:library-name library-name})))))

(clj/defmacro with-lexical-bindings
  "Evaluates body with sym bound to a map of current lexical bindings."
  [sym & body]
  `(let [~sym ~(into {} (for [k (keys &env)]
                          [`(quote ~k) k]))]
     ~@body))

;; Fn/fn/defn

(clj/defn make-fn-type
  [return-type param-types lexical-bindings]
  (with-meta
    (memoize
     (clj/fn [target]
       (o/parse (list 'Fn return-type param-types) lexical-bindings)))
    {:kind :oben/PORTABLE}))

(clj/defmacro Fn
  [return-type param-types]
  `(with-lexical-bindings bindings#
     (make-fn-type '~return-type '~param-types bindings#)))

(clj/defn make-fn
  [name params body lexical-bindings]
  (assert (vector? params) "invalid fn")
  (assert (:tag (meta params)) "fn without return type")
  (let [parse-for-target (memoize
                          (clj/fn [target]
                            (-> (o/parse (list* 'fn params body)
                                         lexical-bindings)
                                (vary-meta assoc :name name))))]
    (with-meta
      (clj/fn [& args]
        (assert (= (count args) (count params))
                (format "invalid number of arguments in function call: expected %d, got %d"
                        (count params) (count args)))
        (let [target (target/current)
              fnode (parse-for-target target)]
          (target/invoke-function target fnode args)))
      {:kind :oben/PORTABLE
       :parse-for-target parse-for-target})))

(clj/defmacro fn
  [& decl]
  (let [[signature body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta signature))]
    `(with-lexical-bindings bindings#
       (make-fn nil '~params '~body bindings#))))

(clj/defmacro defn
  [name & decl]
  (let [[signature body] (o/split-after vector? decl)
        params (first (o/move-types-to-meta signature))]
    `(with-lexical-bindings bindings#
       (def ~name (make-fn '~name '~params '~body bindings#)))))

;; Struct/defstruct

(clj/defn make-struct-type
  [name fields lexical-bindings]
  (assert (vector? fields) "invalid struct")
  (let [opts (if name {:name name} nil)]
    (with-meta
      (memoize
       (clj/fn [target]
         (o/parse (list 'Struct fields opts) lexical-bindings)))
      {:kind :oben/PORTABLE})))

(clj/defmacro Struct
  [fields]
  (let [fields (o/move-types-to-meta fields)]
    `(with-lexical-bindings bindings#
       (make-struct-type nil '~fields bindings#))))

(clj/defmacro defstruct
  [name fields]
  (let [fields (o/move-types-to-meta fields)]
    `(with-lexical-bindings bindings#
       (def ~name (make-struct-type '~name '~fields bindings#)))))

;; Array

(clj/defn make-array-type
  [name element-type size lexical-bindings]
  (let [opts (if name {:name name} nil)]
    (with-meta
      (memoize
       (clj/fn [target]
         (o/parse (list 'Array element-type size opts) lexical-bindings)))
      {:kind :oben/PORTABLE})))

(clj/defmacro Array
  [element-type size]
  `(with-lexical-bindings bindings#
     (make-array-type nil '~element-type '~size bindings#)))

;; global/defglobal

(clj/defn make-global
  [name opts initializer lexical-bindings]
  (let [opts (if (map? opts) opts {:tag opts})
        opts (if name (assoc opts :name name) opts)]
    (with-meta
      (memoize
       (clj/fn [target]
         (o/parse (list 'global opts initializer) lexical-bindings)))
      {:kind :oben/PORTABLE})))

(clj/defmacro global
  [opts initializer]
  `(with-lexical-bindings bindings#
     (make-global nil '~opts '~initializer bindings#)))

(clj/defmacro defglobal
  [name opts initializer]
  `(with-lexical-bindings bindings#
     (def ~name (make-global '~name '~opts '~initializer bindings#))))

;; proxies

(clj/defmacro with-target
  [t & body]
  `(target/with-target ~t ~@body))

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
