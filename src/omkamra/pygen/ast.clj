(ns omkamra.pygen.ast
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [alias compare]))

(defn module []
  {:type :module
   :body []})

(defn module-add [module node]
  (update module :body conj node))

(defn arg [name]
  {:type :arg
   :arg name})

(defn arguments
  ([] (arguments [] [] nil [] [] nil []))
  ([posonlyargs args vararg kwonlyargs kw-defaults kwarg defaults]
   {:type :arguments
    :posonlyargs posonlyargs
    :args args
    :vararg vararg
    :kwonlyargs kwonlyargs
    :kw-defaults kw-defaults
    :kwarg kwarg
    :defaults defaults}))

(def kw-required ::kw-required)

(defn- prefixed-symbol? [x prefix]
  (and (symbol? x)
       (str/starts-with? (name x) prefix)))

(defn- vararg-symbol? [x]
  (and (prefixed-symbol? x "*")
       (not (prefixed-symbol? x "**"))))

(defn- kwarg-symbol? [x]
  (prefixed-symbol? x "**"))

(defn- strip-prefix-symbol [sym prefix]
  (let [raw (subs (name sym) (count prefix))]
    (when (str/blank? raw)
      (throw (ex-info "prefixed parameter name must not be empty"
                      {:param sym :prefix prefix})))
    (when (str/starts-with? raw "*")
      (throw (ex-info "invalid prefixed parameter name"
                      {:param sym :prefix prefix})))
    (symbol raw)))

(defn params->arguments [params]
  (when-not (vector? params)
    (throw (ex-info "params must be a vector"
                    {:params params})))
  (when-not (every? symbol? params)
    (throw (ex-info "params must all be symbols"
                    {:params params})))
  (loop [remaining params
         positional []
         vararg nil
         kwarg nil]
    (if (empty? remaining)
      (arguments [] positional vararg [] [] kwarg [])
      (let [param (first remaining)
            more (rest remaining)]
        (cond
          (kwarg-symbol? param)
          (do
            (when kwarg
              (throw (ex-info "params may contain at most one **kwargs parameter"
                              {:params params :param param})))
            (when (seq more)
              (throw (ex-info "**kwargs parameter must be last"
                              {:params params :param param})))
            (recur more
                   positional
                   vararg
                   (arg (strip-prefix-symbol param "**"))))

          (vararg-symbol? param)
          (do
            (when vararg
              (throw (ex-info "params may contain at most one *args parameter"
                              {:params params :param param})))
            (when kwarg
              (throw (ex-info "*args parameter cannot appear after **kwargs"
                              {:params params :param param})))
            (recur more
                   positional
                   (arg (strip-prefix-symbol param "*"))
                   kwarg))

          :else
          (do
            (when kwarg
              (throw (ex-info "normal parameter cannot appear after **kwargs"
                              {:params params :param param})))
            (recur more
                   (conj positional (arg param))
                   vararg
                   kwarg)))))))

(defn- coerce-arguments [x]
  (cond
    (and (map? x) (= :arguments (:type x))) x
    (vector? x) (params->arguments x)
    :else (throw (ex-info "arguments must be a params vector or :arguments node"
                          {:arguments x}))))

(defn alias
  ([name]
   (alias name nil))
  ([name asname]
   {:type :alias
    :name name
    :asname asname}))

(defn import-stmt [names]
  {:type :import-stmt
   :names names})

(defn import-from-stmt [module names]
  {:type :import-from-stmt
   :module module
   :names names})

(defn class-def [name bases body]
  {:type :class-def
   :name name
   :bases bases
   :body body})

(defn- make-function-def [type name params body]
  {:type type
   :name name
   :args (coerce-arguments params)
   :body body})

(defn function-def [name params body]
  (make-function-def :function-def name params body))

(defn async-function-def [name params body]
  (make-function-def :async-function-def name params body))

(defn return [expr]
  {:type :return
   :expr expr})

(defn expr-stmt [expr]
  {:type :expr-stmt
   :expr expr})

(defn assign [target expr]
  {:type :assign
   :target target
   :expr expr})

(defn augassign [target op expr]
  {:type :augassign
   :target target
   :op op
   :expr expr})

(defn if-stmt
  ([condition then-body]
   (if-stmt condition then-body nil))
  ([condition then-body else-body]
   {:type :if-stmt
    :condition condition
    :then-body then-body
    :else-body else-body}))

(defn while-stmt
  ([condition body]
   (while-stmt condition body nil))
  ([condition body orelse]
   {:type :while-stmt
    :condition condition
    :body body
    :orelse orelse}))

(defn for-stmt
  ([target iter body]
   (for-stmt target iter body nil))
  ([target iter body orelse]
   {:type :for-stmt
    :target target
    :iter iter
    :body body
    :orelse orelse}))

(defn async-for-stmt
  ([target iter body]
   (async-for-stmt target iter body nil))
  ([target iter body orelse]
   {:type :async-for-stmt
    :target target
    :iter iter
    :body body
    :orelse orelse}))

(defn break-stmt []
  {:type :break-stmt})

(defn continue-stmt []
  {:type :continue-stmt})

(defn pass-stmt []
  {:type :pass-stmt})

(defn raise-stmt
  ([] (raise-stmt nil))
  ([expr]
   (raise-stmt expr nil))
  ([expr cause]
   {:type :raise-stmt
    :expr expr
    :cause cause}))

(defn assert-stmt
  ([test]
   (assert-stmt test nil))
  ([test msg]
   {:type :assert-stmt
    :test test
    :msg msg}))

(defn delete-stmt [targets]
  {:type :delete-stmt
   :targets targets})

(defn global-stmt [names]
  {:type :global-stmt
   :names names})

(defn nonlocal-stmt [names]
  {:type :nonlocal-stmt
   :names names})

(defn match-stmt [subject cases]
  {:type :match-stmt
   :subject subject
   :cases cases})

(defn match-case
  ([pattern body]
   (match-case pattern nil body))
  ([pattern guard body]
   {:type :match-case
    :pattern pattern
    :guard guard
    :body body}))

(defn match-value [value]
  {:type :match-value
   :value value})

(defn match-singleton [value]
  {:type :match-singleton
   :value value})

(defn match-sequence [patterns]
  {:type :match-sequence
   :patterns patterns})

(defn match-mapping [keys patterns rest-name]
  {:type :match-mapping
   :keys keys
   :patterns patterns
   :rest rest-name})

(defn match-class [cls patterns kwd-attrs kwd-patterns]
  {:type :match-class
   :cls cls
   :patterns patterns
   :kwd-attrs kwd-attrs
   :kwd-patterns kwd-patterns})

(defn match-star [name]
  {:type :match-star
   :name name})

(defn match-as [pattern name]
  {:type :match-as
   :pattern pattern
   :name name})

(defn match-or [patterns]
  {:type :match-or
   :patterns patterns})

(defn except-handler [exception-types name body]
  {:type :except-handler
   :exception-types exception-types
   :name name
   :body body})

(defn try-stmt [body handlers else-body finally-body]
  {:type :try-stmt
   :body body
   :handlers handlers
   :else-body else-body
   :finally-body finally-body})

(defn with-item [context-expr optional-vars]
  {:type :with-item
   :context-expr context-expr
   :optional-vars optional-vars})

(defn with-stmt [items body]
  {:type :with-stmt
   :items items
   :body body})

(defn async-with-stmt [items body]
  {:type :async-with-stmt
   :items items
   :body body})

(defn list-expr [elements]
  {:type :list-expr
   :elements elements})

(defn comprehension [target iter ifs]
  {:type :comprehension
   :target target
   :iter iter
   :ifs ifs})

(defn list-comp [elt generators]
  {:type :list-comp
   :elt elt
   :generators generators})

(defn set-comp [elt generators]
  {:type :set-comp
   :elt elt
   :generators generators})

(defn dict-comp [key value generators]
  {:type :dict-comp
   :key key
   :value value
   :generators generators})

(defn generator-exp [elt generators]
  {:type :generator-exp
   :elt elt
   :generators generators})

(defn if-exp [test body orelse]
  {:type :if-exp
   :test test
   :body body
   :orelse orelse})

(defn lambda-expr [params body]
  {:type :lambda
   :args (coerce-arguments params)
   :body body})

(defn yield-expr [value]
  {:type :yield
   :value value})

(defn yield-from-expr [value]
  {:type :yield-from
   :value value})

(defn named-expr [target value]
  {:type :named-expr
   :target target
   :value value})

(defn await-expr [value]
  {:type :await
   :value value})

(defn binop [op lhs rhs]
  {:type :binop
   :op op
   :lhs lhs
   :rhs rhs})

(defn compare [left ops comparators]
  {:type :compare
   :left left
   :ops ops
   :comparators comparators})

(defn boolop [op values]
  {:type :boolop
   :op op
   :values values})

(defn unaryop [op operand]
  {:type :unaryop
   :op op
   :operand operand})

(defn attribute [value attr]
  {:type :attribute
   :value value
   :attr attr})

(defn subscript [value slice]
  {:type :subscript
   :value value
   :slice slice})

(defn slice-expr [lower upper step]
  {:type :slice
   :lower lower
   :upper upper
   :step step})

(defn tuple [elements]
  {:type :tuple
   :elements elements})

(defn dict [keys values]
  {:type :dict
   :keys keys
   :values values})

(defn set-expr [elements]
  {:type :set
   :elements elements})

(defn starred [value]
  {:type :starred
   :value value})

(defn keyword-arg [arg value]
  {:type :keyword-arg
   :arg arg
   :value value})

(defn call
  ([f args]
   (call f args []))
  ([f args keywords]
   {:type :call
    :f f
    :args args
    :keywords keywords}))
