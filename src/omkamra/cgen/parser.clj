(ns omkamra.cgen.parser
  "Parser for cgen's deliberately small, integer-only C-like DSL."
  (:require [omkamra.cgen.ast :as ast]))

(def ^:private binary-ops
  {'+ "+" '- "-" '* "*" '/ "/" '% "%"
   '= "==" '== "==" '!= "!=" '< "<" '<= "<=" '> ">" '>= ">="
   'bit-and "&" 'bit-or "|" 'bit-xor "^" 'bit-shift-left "<<" 'bit-shift-right ">>"
   'and "&&" 'or "||"})

(def ^:private unary-ops
  {'- "-" '+ "+" 'not "!" 'bit-not "~"})

(def ^:private dsl-symbols
  (into #{'return 'let 'assign! 'if 'while 'break 'continue 'do}
        (concat (keys binary-ops) (keys unary-ops))))

(defn- canonical-dsl-symbol [x]
  (if (and (symbol? x)
           (contains? dsl-symbols (symbol (name x))))
    (symbol (name x))
    x))

(declare parse-expr parse-stmt)

(defn- fail [message form]
  (throw (ex-info message {:form form})))

(defn- parse-block [form]
  (cond
    (vector? form) (mapv parse-stmt form)
    (and (seq? form) (= 'do (canonical-dsl-symbol (first form))))
    (mapv parse-stmt (rest form))
    :else [(parse-stmt form)]))

(defn- parse-bindings [bindings form]
  (when-not (vector? bindings)
    (fail "let bindings must be a vector" form))
  (when (odd? (count bindings))
    (fail "let bindings must contain name/value pairs" form))
  (mapv (fn [[name value]]
          (when-not (symbol? name)
            (fail "let binding names must be symbols" form))
          [name (parse-expr value)])
        (partition 2 bindings)))

(defn- parse-let [[_ bindings & body :as form]]
  (when (empty? body)
    (fail "let requires at least one body statement" form))
  (ast/let-stmt (parse-bindings bindings form) (mapv parse-stmt body)))

(defn- parse-if [[_ test then else & extra :as form]]
  (when (or (nil? test) (nil? then) (seq extra))
    (fail "if expects a test, then block, and optional else block" form))
  (ast/if-stmt (parse-expr test)
               (parse-block then)
               (when (some? else) (parse-block else))))

(defn- parse-while [[_ test & body :as form]]
  (when (or (nil? test) (empty? body))
    (fail "while requires a test and at least one body statement" form))
  (ast/while-stmt (parse-expr test) (mapv parse-stmt body)))

(defn- parse-assign [[_ name expr & extra :as form]]
  (when (or (nil? name) (nil? expr) (seq extra) (not (symbol? name)))
    (fail "assign! expects a symbol and an expression" form))
  (ast/assign name (parse-expr expr)))

(defn- parse-stmt [form]
  (if (seq? form)
    (case (canonical-dsl-symbol (first form))
      return (let [[_ expr & extra] form]
               (when (or (nil? expr) (seq extra))
                 (fail "return expects exactly one expression" form))
               (ast/return-stmt (parse-expr expr)))
      let (parse-let form)
      assign! (parse-assign form)
      if (parse-if form)
      while (parse-while form)
      break (do (when-not (= 1 (count form)) (fail "break expects no arguments" form))
                (ast/break-stmt))
      continue (do (when-not (= 1 (count form)) (fail "continue expects no arguments" form))
                   (ast/continue-stmt))
      do (fail "do is only valid where a block is expected" form)
      (ast/expr-stmt (parse-expr form)))
    (ast/expr-stmt (parse-expr form))))

(defn- parse-op [op args form]
  (let [op (canonical-dsl-symbol op)]
    (cond
      (and (contains? unary-ops op) (= 1 (count args)))
      (ast/unaryop (get unary-ops op) (parse-expr (first args)))

      (contains? binary-ops op)
      (do
        (when (< (count args) 2)
          (fail "binary operator expects at least two operands" form))
        (reduce (fn [lhs rhs]
                  (ast/binop (get binary-ops op) lhs (parse-expr rhs)))
                (parse-expr (first args))
                (rest args)))

      :else nil)))

(defn- parse-expr [form]
  (cond
    (symbol? form) form
    (number? form) form
    (true? form) true
    (false? form) false
    (seq? form) (let [op (canonical-dsl-symbol (first form))
                      args (rest form)]
                  (or (and (symbol? op) (parse-op op args form))
                      (ast/call (parse-expr op) (mapv parse-expr args))))
    :else (fail "C expressions must be symbols, numbers, booleans, or forms" form)))

(defn- normalize-type [type default]
  (let [type (or type default)]
    (cond
      (contains? #{:double 'double double java.lang.Double
                   :float 'float float java.lang.Float} type) :double
      (contains? #{:int64 'long long java.lang.Long :long} type) :int64
      :else (throw (ex-info "unsupported cgen type hint"
                            {:type type :supported #{:int64 :double}})))))

(defn- type-hint [x default]
  (normalize-type (:tag (meta x)) default))

(defn- parse-function [[_ name params & body :as form]]
  (when-not (symbol? name)
    (fail "def function name must be a symbol" form))
  (when-not (vector? params)
    (fail "def params must be a vector" form))
  (when-not (every? symbol? params)
    (fail "def params must all be symbols" form))
  (when (empty? body)
    (fail "def requires at least one body statement" form))
  (ast/function name params (mapv parse-stmt body)
                (type-hint name :int64)))

(defn parse
  "Parses top-level `(def name [params...] ...)` forms into a translation unit."
  [forms]
  (when-not (and (sequential? forms) (every? seq? forms))
    (fail "cgen expects a sequence of top-level def forms" forms))
  (ast/translation-unit
   (mapv (fn [form]
           (when-not (= 'def (canonical-dsl-symbol (first form)))
             (fail "only def is allowed at cgen translation-unit level" form))
           (parse-function form))
         forms)))
