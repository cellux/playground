(ns omkamra.cgen.core
  "Define, transpile, and execute a small C-like language embedded in Clojure.

  `defn` creates an ordinary Clojure callable. Its body is not evaluated as
  Clojure: it is retained as cgen data and compiled only when called. Qualified
  keywords (for example `::helper`) name other cgen definitions."
  (:refer-clojure :exclude [defn fn])
  (:require [omkamra.cgen.emit :as emit]
            [omkamra.cgen.parser :as parser]
            [omkamra.cgen.runtime :as runtime]))

(defn- normalize-type [type default]
  (let [type (or type default)]
    (case type
      (:double double java.lang.Double) :double
      (:float float java.lang.Float) :double
      (:int64 long java.lang.Long :long) :int64
      (:uint64 :ulong) :int64
      (throw (ex-info "unsupported cgen type hint"
                      {:type type :supported #{:int64 :double}})))))

(defn- type-hint [x default]
  (normalize-type (:tag (meta x)) default))

(defn- param-types [params]
  (mapv #(type-hint % :int64) params))

(defmacro defn
  "Defines a C function callable from Clojure.

  Type hints select the native ABI type.  Without hints, parameters and the
  return value are signed 64-bit integers.  Use `^double` for DSP functions,
  e.g. `(c/defn ^double sine [^double t ^double frequency] ...)`."
  [name params & body]
  (when-not (symbol? name)
    (throw (ex-info "cgen/defn name must be a symbol" {:name name})))
  (when-not (vector? params)
    (throw (ex-info "cgen/defn params must be a vector" {:params params})))
  (let [return-type (type-hint name :int64)
        param-types (param-types params)
        definition {:cgen/kind :function
                    :params params
                    :param-types param-types
                    :return-type return-type
                    :body body}]
    `(do
       (def ~name
         (with-meta
           (clojure.core/fn [& arguments#]
             (runtime/invoke (var ~name) arguments#))
           {:cgen/var (var ~name)}))
       (alter-meta! (var ~name) assoc :cgen/definition '~definition)
       ~name)))

(defmacro fn
  "Construct an anonymous cgen function definition.

  Syntax is `(cgen/fn ^double [^double x] (return x))`. The result is a
  definition descriptor, not a Clojure callable; pass it to `invoke` or
  `invoke-binary`. Without a return hint, the return type defaults to int64."
  [& forms]
  (let [first-form (first forms)
        [return-tag params body]
        (if (vector? first-form)
          [(:tag (meta first-form)) first-form (next forms)]
          [(:tag (meta first-form)) (second forms) (nnext forms)])
        return-type (normalize-type return-tag :int64)]
    (when-not (vector? params)
      (throw (ex-info "cgen/fn params must be a vector" {:params params})))
    `(function-definition
      '~(gensym "cgen")
      '~params
      ~return-type
      '~body)))

(clojure.core/defn function-definition
  "Construct an anonymous cgen function definition.

  Anonymous definitions can be passed directly to `source-for`, `describe`,
  `invoke`, or `invoke-binary` without creating a Clojure Var. Parameters may
  carry `^double`/`^int64` type metadata."
  ([params return-type body]
   (function-definition (gensym "cgen") params return-type body))
  ([name params return-type body]
   (when-not (symbol? name)
     (throw (ex-info "cgen function name must be a symbol" {:name name})))
   (when-not (vector? params)
     (throw (ex-info "cgen function params must be a vector" {:params params})))
   (when-not (every? symbol? params)
     (throw (ex-info "cgen function params must be symbols" {:params params})))
   (when-not (contains? #{:int64 :double} return-type)
     (throw (ex-info "unsupported cgen return type"
                     {:return-type return-type
                      :supported #{:int64 :double}})))
   (when-not (sequential? body)
     (throw (ex-info "cgen function body must be sequential"
                     {:body body})))
   {:cgen/kind :function
    :name name
    :params params
    :param-types (param-types params)
    :return-type return-type
    :body (vec body)}))

(clojure.core/defn transpile
  "Lowers explicit cgen `(def ...)` forms to C17 source.

  For executable definitions created by `cgen/defn`, use `source-for` or call
  the generated Clojure function directly."
  [forms]
  (emit/emit-translation-unit (parser/parse forms)))

(clojure.core/defn source-for
  "Returns complete C17 source, including a command-line launcher, for a
  cgen Var, callable value, or anonymous function definition."
  [entry-var]
  (:source (runtime/source-for-entry entry-var)))

(clojure.core/defn invoke
  "Explicitly invoke a named or anonymous cgen function."
  [entry-var & arguments]
  (runtime/invoke entry-var arguments))

(clojure.core/defn describe
  "Return the native parameter and return types of a named or anonymous cgen function."
  [entry-var]
  (runtime/describe entry-var))

(clojure.core/defn invoke-binary
  "Execute a named or anonymous cgen function over raw fixed-width binary records."
  [entry-var options]
  (runtime/invoke-binary entry-var options))
