(ns omkamra.cgen.emit
  (:require [clojure.string :as str]))

(defn- ident [x]
  (when-not (symbol? x)
    (throw (ex-info "C identifier must be a symbol" {:identifier x})))
  (let [s (name x)]
    (cond
      (= s "PI") "M_PI"
      (= s "E") "M_E"
      :else
      (let [s (str/replace s "-" "_")]
        (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" s)
          (throw (ex-info "invalid C identifier" {:identifier x})))
        s))))

(defn- indent [level text]
  (str (apply str (repeat (* 2 level) " ")) text))

(defn- type-name [type]
  (case type
    :double "double"
    :int64 "int64_t"
    (throw (ex-info "unsupported C type" {:type type}))))

(defn- double-type? [type]
  (contains? #{:double 'double double java.lang.Double
               :float 'float float java.lang.Float}
             type))

(defn param-type [param]
  (if (double-type? (:tag (meta param))) :double :int64))

(defn- literal-code [value]
  (cond
    (integer? value)
    (cond
      (= value Long/MIN_VALUE) "INT64_MIN"
      (or (< value Long/MIN_VALUE) (> value Long/MAX_VALUE))
      (throw (ex-info "C integer literals must fit signed 64-bit range"
                      {:literal value}))
      :else (str value "LL"))

    (float? value) (str (double value) "f")
    (double? value) (let [s (str value)]
                      (if (re-find #"[.eE]" s) s (str s ".0")))
    (ratio? value) (str "(" (double value) ")")
    :else (throw (ex-info "unsupported C numeric literal" {:literal value}))))

(defn- merge-type [left right]
  (if (or (= :double left) (= :double right)) :double :int64))

(defn- expr-type [expr env function-types]
  (cond
    (symbol? expr) (get env expr :int64)
    (integer? expr) :int64
    (number? expr) :double
    (or (true? expr) (false? expr)) :int64
    (map? expr)
    (case (:type expr)
      :binop (if (contains? #{"==" "!=" "<" "<=" ">" ">=" "&&" "||"} (:op expr))
               :int64
               (merge-type (expr-type (:lhs expr) env function-types)
                           (expr-type (:rhs expr) env function-types)))
      :unaryop (if (= "!" (:op expr))
                 :int64
                 (expr-type (:operand expr) env function-types))
      :call (get function-types (:f expr) :double)
      :int64)
    :else :int64))

(declare emit-expr emit-stmt-lines)

(defn- emit-expr [expr env function-types]
  (cond
    (symbol? expr) (ident expr)
    (number? expr) (literal-code expr)
    (true? expr) "1"
    (false? expr) "0"
    (map? expr)
    (case (:type expr)
      :binop (str "(" (emit-expr (:lhs expr) env function-types)
                  " " (:op expr) " "
                  (emit-expr (:rhs expr) env function-types) ")")
      :unaryop (str "(" (:op expr)
                    (emit-expr (:operand expr) env function-types) ")")
      :call (str (emit-expr (:f expr) env function-types) "("
                 (str/join ", " (map #(emit-expr % env function-types)
                                     (:args expr))) ")")
      (throw (ex-info "unsupported C expression node" {:expr expr})))
    :else (throw (ex-info "unsupported C expression" {:expr expr}))))

(defn- emit-body-lines [body level env function-types]
  (mapcat #(emit-stmt-lines % level env function-types) body))

(defn- emit-stmt-lines [stmt level env function-types]
  (case (:type stmt)
    :return [(indent level (str "return "
                                (emit-expr (:expr stmt) env function-types)
                                ";"))]
    :expr-stmt [(indent level (str (emit-expr (:expr stmt) env function-types)
                                   ";"))]
    :assign [(indent level (str (ident (:name stmt)) " = "
                                (emit-expr (:expr stmt) env function-types)
                                ";"))]
    :break [(indent level "break;")]
    :continue [(indent level "continue;")]
    :let (let [bindings (:bindings stmt)
               binding-types (into {} (map (fn [[name value]]
                                             [name (if (double-type? (:tag (meta name)))
                                                     :double
                                                     (expr-type value env function-types))])
                                           bindings))
               body-env (merge env binding-types)]
           (concat [(indent level "{")]
                   (map (fn [[name value]]
                          (indent (inc level)
                                  (str (type-name (get binding-types name)) " "
                                       (ident name) " = "
                                       (emit-expr value env function-types) ";")))
                        bindings)
                   (emit-body-lines (:body stmt) (inc level) body-env function-types)
                   [(indent level "}")]))
    :if (concat [(indent level (str "if ("
                                    (emit-expr (:test stmt) env function-types)
                                    ") {"))]
                (emit-body-lines (:then-body stmt) (inc level) env function-types)
                [(indent level "}")]
                (when (some? (:else-body stmt))
                  (concat [(indent level "else {")]
                          (emit-body-lines (:else-body stmt) (inc level) env function-types)
                          [(indent level "}")])))
    :while (concat [(indent level (str "while ("
                                       (emit-expr (:test stmt) env function-types)
                                       ") {"))]
                   (emit-body-lines (:body stmt) (inc level) env function-types)
                   [(indent level "}")])
    (throw (ex-info "unsupported C statement node" {:statement stmt}))))

(defn- emit-function-signature [{:keys [name params return-type]}]
  (str (type-name return-type) " " (ident name) "("
       (if (seq params)
         (str/join ", " (map #(str (type-name (param-type %)) " " (ident %)) params))
         "void")
       ")"))

(defn emit-translation-unit
  "Lowers a cgen translation unit to C17 source with integer and double types."
  [{:keys [functions] :as unit}]
  (when-not (= :translation-unit (:type unit))
    (throw (ex-info "translation unit expected" {:unit unit})))
  (let [function-types (into {} (map (fn [{:keys [name return-type]}]
                                       [name return-type])
                                     functions))
        prototypes (map #(str (emit-function-signature %) ";") functions)
        definitions (map (fn [{:keys [params body] :as function}]
                           (let [env (zipmap params (map param-type params))]
                             (str (emit-function-signature function) " {\n"
                                  (str/join "\n"
                                            (emit-body-lines body 1 env function-types))
                                  "\n}")))
                         functions)]
    (str "#include <stdint.h>\n"
         "#include <math.h>\n"
         "#ifndef M_PI\n#define M_PI 3.141592653589793238462643383279502884\n#endif\n"
         "#ifndef M_E\n#define M_E 2.7182818284590452353602874713526625\n#endif\n\n"
         (str/join "\n" prototypes)
         "\n\n"
         (str/join "\n\n" definitions)
         "\n")))