(ns omkamra.pygen.emit
  (:require [clojure.string :as str]
            [omkamra.pygen.ast :as ast]))

(defn- emit-ident [id]
  (if (symbol? id)
    (-> (clojure.core/name id)
        (str/replace "-" "_"))
    (throw (ex-info "Identifier must be a symbol" {:id id}))))

(defn- emit-op [op]
  (case op
    :+ "+"
    :- "-"
    :* "*"
    :/ "/"
    :% "%"
    :> ">"
    :< "<"
    :>= ">="
    :<= "<="
    :== "=="
    :!= "!="
    :in "in"
    :not-in "not in"
    :is "is"
    :is-not "is not"
    (throw (ex-info (str "Unsupported binary operator: " op) {:op op}))))

(defn- emit-boolop-op [op]
  (case op
    :and "and"
    :or "or"
    (throw (ex-info (str "Unsupported boolean operator: " op) {:op op}))))

(defn- emit-unaryop-op [op]
  (case op
    :not "not"
    :uadd "+"
    :usub "-"
    :invert "~"
    (throw (ex-info (str "Unsupported unary operator: " op) {:op op}))))

(defn- emit-augassign-op [op]
  (case op
    :+ "+"
    :- "-"
    :* "*"
    :/ "/"
    :% "%"
    (throw (ex-info (str "Unsupported augmented assignment operator: " op) {:op op}))))

(declare emit-expr
         emit-pattern
         emit-comprehension
         emit-stmt-lines
         emit-function-def-lines
         emit-class-def-lines
         emit-except-handler-lines
         emit-with-item)

(defn- emit-arg [arg-node]
  (cond
    (symbol? arg-node) (emit-ident arg-node)
    (and (map? arg-node) (= :arg (:type arg-node)))
    (let [name (:arg arg-node)]
      (when-not (symbol? name)
        (throw (ex-info "arg name must be a symbol" {:arg arg-node})))
      (emit-ident name))
    :else (throw (ex-info "arg node expected" {:arg arg-node}))))

(defn- emit-arguments [args-node]
  (cond
    (vector? args-node)
    (str/join ", " (map emit-ident args-node))

    (and (map? args-node) (= :arguments (:type args-node)))
    (let [posonlyargs (vec (or (:posonlyargs args-node) []))
          args (vec (or (:args args-node) []))
          vararg (:vararg args-node)
          kwonlyargs (vec (or (:kwonlyargs args-node) []))
          kw-defaults (vec (or (:kw-defaults args-node) []))
          kwarg (:kwarg args-node)
          defaults (vec (or (:defaults args-node) []))
          all-positional (vec (concat posonlyargs args))
          total-positional (count all-positional)
          num-defaults (count defaults)
          first-default-idx (- total-positional num-defaults)]
      (when (> num-defaults total-positional)
        (throw (ex-info "too many defaults for positional arguments"
                        {:arguments args-node})))
      (when-not (= (count kw-defaults) (count kwonlyargs))
        (throw (ex-info "kw-only defaults must match kw-only args count"
                        {:arguments args-node})))
      (let [positional-coded
            (map-indexed
             (fn [idx a]
               (let [base (emit-arg a)]
                 (if (>= idx first-default-idx)
                   (str base "=" (emit-expr (nth defaults (- idx first-default-idx))))
                   base)))
             all-positional)
            posonly-count (count posonlyargs)
            posonly-coded (take posonly-count positional-coded)
            args-coded (drop posonly-count positional-coded)
            kwonly-coded
            (map-indexed
             (fn [idx a]
                 (let [base (emit-arg a)
                      default (nth kw-defaults idx)]
                 ;; ast/kw-required marks required kw-only args; nil is a valid Python default (None).
                 (if (= default ast/kw-required)
                   base
                   (str base "=" (emit-expr default)))))
             kwonlyargs)
            head (concat posonly-coded
                         (when (seq posonly-coded) ["/"])
                         args-coded)
            star (cond
                   (some? vararg) [(str "*" (emit-arg vararg))]
                   (seq kwonly-coded) ["*"]
                   :else [])
            tail (concat star
                         kwonly-coded
                         (when (some? kwarg) [(str "**" (emit-arg kwarg))]))]
        (str/join ", " (concat head tail))))

    :else
    (throw (ex-info "arguments node expected" {:arguments args-node}))))

(defn- needs-call-arg-parens? [expr]
  (and (map? expr)
       (contains? #{:yield :yield-from :named-expr} (:type expr))))

(defn- emit-call-arg-expr [expr]
  (let [code (emit-expr expr)]
    (if (needs-call-arg-parens? expr)
      (str "(" code ")")
      code)))

(defn- emit-keyword-arg [k]
  (when-not (and (map? k) (= :keyword-arg (:type k)))
    (throw (ex-info "keyword arg node expected" {:keyword-arg k})))
  (let [arg (:arg k)
        value (:value k)]
    (if (nil? arg)
      (str "**" (emit-call-arg-expr value))
      (do
        (when-not (symbol? arg)
          (throw (ex-info "keyword arg name must be a symbol" {:keyword-arg k})))
        (str (emit-ident arg) "=" (emit-call-arg-expr value))))))

(defn- indent [level line]
  (str (apply str (repeat (* 4 level) " ")) line))

(defn- emit-lit [value]
  (cond
    (true? value) "True"
    (false? value) "False"
    (nil? value) "None"
    :else (pr-str value)))

(defn- emit-alias [a]
  (when-not (and (map? a) (= :alias (:type a)))
    (throw (ex-info "alias node expected" {:alias a})))
  (let [name (:name a)
        asname (:asname a)]
    (when-not (symbol? name)
      (throw (ex-info "alias name must be a symbol" {:alias a})))
    (when (some? asname)
      (when-not (symbol? asname)
        (throw (ex-info "alias asname must be a symbol" {:alias a}))))
    (str (emit-ident name)
         (when asname
           (str " as " (emit-ident asname))))))

;; Mirrors CPython's _ast_unparse._Precedence levels 1:1.
(def ^:private precedence-named-expr 1)
(def ^:private precedence-tuple 2)
(def ^:private precedence-yield 3)
(def ^:private precedence-test 4)
(def ^:private precedence-or 5)
(def ^:private precedence-and 6)
(def ^:private precedence-not 7)
(def ^:private precedence-cmp 8)
(def ^:private precedence-expr 9)
(def ^:private precedence-bor precedence-expr)
(def ^:private precedence-bxor 10)
(def ^:private precedence-band 11)
(def ^:private precedence-shift 12)
(def ^:private precedence-arith 13)
(def ^:private precedence-term 14)
(def ^:private precedence-factor 15)
(def ^:private precedence-power 16)
(def ^:private precedence-await 17)
(def ^:private precedence-atom 18)
(def ^:private precedence-none 0)

(defn- op-precedence [op]
  (case op
    :* precedence-term
    :/ precedence-term
    :% precedence-term
    :+ precedence-arith
    :- precedence-arith
    (throw (ex-info (str "Unsupported binary operator: " op) {:op op}))))

(defn- boolop-precedence [op]
  (case op
    :and precedence-and
    :or precedence-or
    (throw (ex-info (str "Unsupported boolean operator: " op) {:op op}))))

(defn- unaryop-precedence [op]
  (case op
    :not precedence-not
    :uadd precedence-factor
    :usub precedence-factor
    :invert precedence-factor
    (throw (ex-info (str "Unsupported unary operator: " op) {:op op}))))

(defn- maybe-parenthesize [code precedence parent-precedence side]
  (if (or (< precedence parent-precedence)
          (and (= side :right)
               (= precedence parent-precedence)))
    (str "(" code ")")
    code))

(defn- emit-expr
  ([expr]
   (emit-expr expr precedence-none nil))
  ([expr parent-precedence side]
   (cond
     (symbol? expr) (emit-ident expr)
     (or (string? expr)
         (number? expr)
         (boolean? expr)
         (nil? expr)) (emit-lit expr)
     (map? expr) (case (:type expr)
                   :list-expr (str "["
                                   (str/join ", " (map emit-expr (:elements expr)))
                                   "]")
                   :list-comp (let [elt (:elt expr)
                                    generators (:generators expr)]
                                (when (empty? generators)
                                  (throw (ex-info "list comprehension requires at least one generator"
                                                  {:expr expr})))
                                (str "["
                                     (emit-expr elt)
                                     " "
                                     (str/join " " (map emit-comprehension generators))
                                     "]"))
                   :set-comp (let [elt (:elt expr)
                                   generators (:generators expr)]
                               (when (empty? generators)
                                 (throw (ex-info "set comprehension requires at least one generator"
                                                 {:expr expr})))
                               (str "{"
                                    (emit-expr elt)
                                    " "
                                    (str/join " " (map emit-comprehension generators))
                                    "}"))
                   :dict-comp (let [key (:key expr)
                                    value (:value expr)
                                    generators (:generators expr)]
                                (when (empty? generators)
                                  (throw (ex-info "dict comprehension requires at least one generator"
                                                  {:expr expr})))
                                (str "{"
                                     (emit-expr key)
                                     ": "
                                     (emit-expr value)
                                     " "
                                     (str/join " " (map emit-comprehension generators))
                                     "}"))
                   :generator-exp (let [elt (:elt expr)
                                        generators (:generators expr)]
                                    (when (empty? generators)
                                      (throw (ex-info "generator expression requires at least one generator"
                                                      {:expr expr})))
                                    (str "("
                                         (emit-expr elt)
                                         " "
                                         (str/join " " (map emit-comprehension generators))
                                         ")"))
                   :binop (let [op (:op expr)
                                precedence (op-precedence op)
                                code (str (emit-expr (:lhs expr) precedence :left)
                                          " "
                                          (emit-op op)
                                          " "
                                          (emit-expr (:rhs expr) precedence :right))]
                            (maybe-parenthesize code precedence parent-precedence side))
                   :compare (let [precedence precedence-cmp
                                  left-code (emit-expr (:left expr) precedence :left)
                                  ops (:ops expr)
                                  comparators (:comparators expr)
                                  _ (when-not (= (count ops) (count comparators))
                                      (throw (ex-info "compare ops and comparators must have matching lengths"
                                                      {:expr expr})))
                                  code (reduce (fn [acc [op comparator]]
                                                 (str acc
                                                 " "
                                                 (emit-op op)
                                                 " "
                                                 (emit-expr comparator precedence :right)))
                                               left-code
                                               (map vector ops comparators))]
                              (maybe-parenthesize code precedence parent-precedence side))
                   :if-exp (let [precedence precedence-test
                                 code (str (emit-expr (:body expr) (inc precedence) nil)
                                           " if "
                                           (emit-expr (:test expr) (inc precedence) nil)
                                           " else "
                                           (emit-expr (:orelse expr) precedence nil))]
                             (maybe-parenthesize code precedence parent-precedence side))
                   :lambda (let [precedence precedence-test
                                 args-node (or (:args expr) (:params expr))
                                 params-code (emit-arguments args-node)
                                 code (str "lambda"
                                           (when (seq params-code)
                                             (str " " params-code))
                                           ": "
                                           (emit-expr (:body expr)))]
                             (maybe-parenthesize code precedence parent-precedence side))
                   :yield (let [precedence precedence-yield
                                value (:value expr)
                                code (if (nil? value)
                                       "yield"
                                       (str "yield " (emit-expr value precedence nil)))]
                            (maybe-parenthesize code precedence parent-precedence side))
                   :yield-from (let [precedence precedence-yield
                                     value (:value expr)
                                     _ (when (nil? value)
                                         (throw (ex-info "yield-from requires a value"
                                                         {:expr expr})))
                                     code (str "yield from " (emit-expr value precedence nil))]
                                 (maybe-parenthesize code precedence parent-precedence side))
                   :await (let [precedence precedence-await
                                value (:value expr)
                                _ (when (nil? value)
                                    (throw (ex-info "await requires a value"
                                                    {:expr expr})))
                                code (str "await " (emit-expr value precedence nil))]
                            (maybe-parenthesize code precedence parent-precedence side))
                   :named-expr (let [precedence precedence-named-expr
                                     target (:target expr)
                                     _ (when-not (symbol? target)
                                         (throw (ex-info "named expression target must be a symbol"
                                                         {:expr expr :target target})))
                                     code (str (emit-ident target)
                                               " := "
                                               (emit-expr (:value expr) precedence :right))]
                                 (maybe-parenthesize code precedence parent-precedence side))
                   :boolop (let [op (:op expr)
                                 precedence (boolop-precedence op)
                                 values (:values expr)
                                 _ (when (< (count values) 2)
                                     (throw (ex-info "boolop expects at least two values"
                                                     {:expr expr})))
                                 code (str/join (str " " (emit-boolop-op op) " ")
                                                (map #(emit-expr % precedence nil) values))]
                             (maybe-parenthesize code precedence parent-precedence side))
                   :unaryop (let [op (:op expr)
                                  precedence (unaryop-precedence op)
                                  operand (:operand expr)
                                  code (str (emit-unaryop-op op)
                                            (if (= :not op) " " "")
                                            (emit-expr operand precedence nil))]
                              (maybe-parenthesize code precedence parent-precedence side))
                   :attribute (let [precedence precedence-atom
                                    value (:value expr)
                                    attr (:attr expr)
                                    _ (when-not (symbol? attr)
                                        (throw (ex-info "attribute name must be a symbol"
                                                        {:expr expr :attr attr})))
                                    code (str (emit-expr value precedence nil)
                                              "."
                                              (emit-ident attr))]
                                (maybe-parenthesize code precedence parent-precedence side))
                   :subscript (let [precedence precedence-atom
                                    value (:value expr)
                                    slice-expr (:slice expr)
                                    code (str (emit-expr value precedence nil)
                                              "["
                                              (emit-expr slice-expr)
                                              "]")]
                                (maybe-parenthesize code precedence parent-precedence side))
                   :slice (let [lower (:lower expr)
                                upper (:upper expr)
                                step (:step expr)
                                lower-code (when (some? lower) (emit-expr lower))
                                upper-code (when (some? upper) (emit-expr upper))
                                step-code (when (some? step) (emit-expr step))]
                            (if (some? step)
                              (str (or lower-code "")
                                   ":"
                                   (or upper-code "")
                                   ":"
                                   (or step-code ""))
                              (str (or lower-code "")
                                   ":"
                                   (or upper-code ""))))
                   :tuple (let [elements (:elements expr)]
                            (case (count elements)
                              0 "()"
                              1 (str "(" (emit-expr (first elements)) ",)")
                              (str "("
                                   (str/join ", " (map emit-expr elements))
                                   ")")))
                   :dict (let [keys (:keys expr)
                               values (:values expr)
                               _ (when-not (= (count keys) (count values))
                                   (throw (ex-info "dict keys and values must have matching lengths"
                                                   {:expr expr})))
                               items (map (fn [k v]
                                            (str (emit-expr k) ": " (emit-expr v)))
                                          keys
                                          values)]
                           (str "{"
                                (str/join ", " items)
                                "}"))
                   :set (let [elements (:elements expr)]
                          (if (empty? elements)
                            "set()"
                            (str "{"
                                 (str/join ", " (map emit-expr elements))
                                 "}")))
                   :starred (str "*"
                                 (emit-expr (:value expr)))
                   :call (let [positional (map emit-call-arg-expr (:args expr))
                               keywords (map emit-keyword-arg (or (:keywords expr) []))
                               args-code (str/join ", " (concat positional keywords))
                               precedence precedence-atom
                               code (str (emit-expr (:f expr) precedence :left)
                                         "("
                                         args-code
                                         ")")]
                           (maybe-parenthesize code precedence parent-precedence side))
                   (throw (ex-info (str "Unsupported expression type: " (:type expr))
                                   {:expr expr})))
     :else (throw (ex-info (str "Unsupported expression: " (pr-str expr))
                           {:expr expr})))))

(defn- emit-assign-target [target]
  (cond
    (symbol? target) (emit-ident target)
    (and (map? target) (contains? #{:attribute :subscript} (:type target))) (emit-expr target)
    :else (throw (ex-info (str "Unsupported assignment target: " (pr-str target))
                          {:target target}))))

(defn- emit-for-target [target]
  (if (symbol? target)
    (emit-ident target)
    (throw (ex-info (str "Unsupported for target: " (pr-str target))
                    {:target target}))))

(defn- emit-comprehension [g]
  (when-not (and (map? g) (= :comprehension (:type g)))
    (throw (ex-info "comprehension node expected" {:comprehension g})))
  (let [target (:target g)
        iter-expr (:iter g)
        ifs (:ifs g)]
    (when-not (symbol? target)
      (throw (ex-info "comprehension target must be a symbol" {:comprehension g})))
    (str "for "
         (emit-ident target)
         " in "
         (emit-expr iter-expr)
         (apply str (map #(str " if " (emit-expr %)) ifs)))))

(defn- emit-body-lines [body indent-level]
  (if (seq body)
    (mapcat #(emit-stmt-lines % indent-level) body)
    [(indent indent-level "pass")]))

(defn- emit-except-handler-lines [handler indent-level]
  (when-not (and (map? handler) (= :except-handler (:type handler)))
    (throw (ex-info "except handler node expected" {:handler handler})))
  (let [exception-types (:exception-types handler)
        name (:name handler)
        body (:body handler)
        header (if (seq exception-types)
                 (let [types-code (if (= 1 (count exception-types))
                                    (emit-expr (first exception-types))
                                    (str "("
                                         (str/join ", " (map emit-expr exception-types))
                                         ")"))]
                   (str "except "
                        types-code
                        (when name
                          (str " as " (emit-ident name)))
                        ":"))
                 (do
                   (when name
                     (throw (ex-info "bare except must not specify a name"
                                     {:handler handler})))
                   "except:"))]
    (concat [(indent indent-level header)]
            (emit-body-lines body (inc indent-level)))))

(defn- emit-with-item [item]
  (when-not (and (map? item) (= :with-item (:type item)))
    (throw (ex-info "with-item node expected" {:item item})))
  (let [context-expr (:context-expr item)
        optional-vars (:optional-vars item)]
    (str (emit-expr context-expr)
         (when (some? optional-vars)
           (str " as " (emit-assign-target optional-vars))))))

(defn- emit-pattern [pattern]
  (when-not (map? pattern)
    (throw (ex-info "pattern node expected" {:pattern pattern})))
  (case (:type pattern)
    :match-value (emit-expr (:value pattern))
    :match-singleton (let [value (:value pattern)]
                       (cond
                         (true? value) "True"
                         (false? value) "False"
                         (nil? value) "None"
                         :else (throw (ex-info "match-singleton value must be true/false/nil"
                                               {:pattern pattern}))))
    :match-sequence (str "[" (str/join ", " (map emit-pattern (:patterns pattern))) "]")
    :match-mapping (let [keys (:keys pattern)
                         patterns (:patterns pattern)
                         rest-name (:rest pattern)]
                     (when-not (= (count keys) (count patterns))
                       (throw (ex-info "match-mapping keys and patterns must have matching lengths"
                                       {:pattern pattern})))
                     (when (some? rest-name)
                       (when-not (symbol? rest-name)
                         (throw (ex-info "match-mapping rest must be a symbol"
                                         {:pattern pattern :rest rest-name}))))
                     (let [items (map (fn [k p]
                                        (str (emit-expr k) ": " (emit-pattern p)))
                                      keys
                                      patterns)
                           all-items (if (some? rest-name)
                                       (concat items [(str "**" (emit-ident rest-name))])
                                       items)]
                       (str "{" (str/join ", " all-items) "}")))
    :match-class (let [cls (:cls pattern)
                       patterns (:patterns pattern)
                       kwd-attrs (:kwd-attrs pattern)
                       kwd-patterns (:kwd-patterns pattern)]
                   (when-not (= (count kwd-attrs) (count kwd-patterns))
                     (throw (ex-info "match-class kwd-attrs and kwd-patterns must have matching lengths"
                                     {:pattern pattern})))
                   (let [positional (map emit-pattern patterns)
                         keyworded (map (fn [attr pat]
                                          (when-not (symbol? attr)
                                            (throw (ex-info "match-class keyword attribute must be a symbol"
                                                            {:pattern pattern :attr attr})))
                                          (str (emit-ident attr) "=" (emit-pattern pat)))
                                        kwd-attrs
                                        kwd-patterns)
                         args-code (str/join ", " (concat positional keyworded))]
                     (str (emit-expr cls) "(" args-code ")")))
    :match-star (let [name (:name pattern)]
                  (if (nil? name)
                    "*_"
                    (do
                      (when-not (symbol? name)
                        (throw (ex-info "match-star name must be a symbol or nil"
                                        {:pattern pattern :name name})))
                      (str "*" (emit-ident name)))))
    :match-as (let [sub-pattern (:pattern pattern)
                    name (:name pattern)]
                (cond
                  (and (nil? sub-pattern) (nil? name)) "_"
                  (nil? sub-pattern) (do
                                       (when-not (symbol? name)
                                         (throw (ex-info "match-as name must be a symbol when pattern is nil"
                                                         {:pattern pattern :name name})))
                                       (emit-ident name))
                  (nil? name) (emit-pattern sub-pattern)
                  :else (do
                          (when-not (symbol? name)
                            (throw (ex-info "match-as name must be a symbol"
                                            {:pattern pattern :name name})))
                          (str (emit-pattern sub-pattern) " as " (emit-ident name)))))
    :match-or (let [patterns (:patterns pattern)]
                (when (< (count patterns) 2)
                  (throw (ex-info "match-or requires at least two patterns"
                                  {:pattern pattern})))
                (str/join " | " (map emit-pattern patterns)))
    (throw (ex-info (str "Unsupported pattern type: " (:type pattern))
                    {:pattern pattern}))))

(defn- emit-match-case-lines [match-case indent-level]
  (when-not (and (map? match-case) (= :match-case (:type match-case)))
    (throw (ex-info "match-case node expected" {:match-case match-case})))
  (let [header (str "case "
                    (emit-pattern (:pattern match-case))
                    (when (some? (:guard match-case))
                      (str " if " (emit-expr (:guard match-case))))
                    ":")]
    (concat [(indent indent-level header)]
            (emit-body-lines (:body match-case) (inc indent-level)))))

(defn- emit-function-def-lines [function-def indent-level]
  (let [params (emit-arguments (or (:args function-def) (:params function-def)))
        body (:body function-def)
        keyword (if (= :async-function-def (:type function-def))
                  "async def "
                  "def ")]
    (concat [(indent indent-level
                     (str keyword (emit-ident (:name function-def)) "(" params "):"))]
            (emit-body-lines body (inc indent-level)))))

(defn- emit-class-def-lines [class-def indent-level]
  (let [name (emit-ident (:name class-def))
        bases (:bases class-def)
        bases-code (if (seq bases)
                     (str "(" (str/join ", " (map emit-expr bases)) ")")
                     "")
        body (:body class-def)]
    (concat [(indent indent-level (str "class " name bases-code ":"))]
            (emit-body-lines body (inc indent-level)))))

(defn- emit-for-stmt-lines [stmt indent-level prefix]
  (let [target (:target stmt)
        iter-expr (:iter stmt)
        body (:body stmt)
        orelse (:orelse stmt)]
    (concat [(indent indent-level
                     (str prefix
                          (emit-for-target target)
                          " in "
                          (emit-expr iter-expr)
                          ":"))]
            (emit-body-lines body (inc indent-level))
            (if (some? orelse)
              (concat [(indent indent-level "else:")]
                      (emit-body-lines orelse (inc indent-level)))
              []))))

(defn- emit-with-stmt-lines [stmt indent-level prefix empty-items-msg]
  (let [items (:items stmt)
        body (:body stmt)]
    (when (empty? items)
      (throw (ex-info empty-items-msg {:stmt stmt})))
    (concat [(indent indent-level
                     (str prefix
                          (str/join ", " (map emit-with-item items))
                          ":"))]
            (emit-body-lines body (inc indent-level)))))

(defn- emit-import-stmt-lines [stmt indent-level]
  [(indent indent-level
           (str "import "
                (str/join ", " (map emit-alias (:names stmt)))))])

(defn- emit-import-from-stmt-lines [stmt indent-level]
  [(indent indent-level
           (str "from "
                (emit-ident (:module stmt))
                " import "
                (str/join ", " (map emit-alias (:names stmt)))))])

(defn- emit-return-stmt-lines [stmt indent-level]
  [(indent indent-level (str "return " (emit-expr (:expr stmt))))])

(defn- emit-expr-stmt-lines [stmt indent-level]
  (let [expr (:expr stmt)
        expr-code (emit-expr expr)]
    [(indent indent-level
             (if (and (map? expr) (= :named-expr (:type expr)))
               (str "(" expr-code ")")
               expr-code))]))

(defn- emit-assign-stmt-lines [stmt indent-level]
  [(indent indent-level
           (str (emit-assign-target (:target stmt))
                " = "
                (emit-expr (:expr stmt))))])

(defn- emit-augassign-stmt-lines [stmt indent-level]
  [(indent indent-level
           (str (emit-assign-target (:target stmt))
                " "
                (emit-augassign-op (:op stmt))
                "= "
                (emit-expr (:expr stmt))))])

(defn- emit-if-stmt-lines [stmt indent-level]
  (let [condition (:condition stmt)
        then-body (:then-body stmt)
        else-body (:else-body stmt)]
    (concat [(indent indent-level (str "if " (emit-expr condition) ":"))]
            (emit-body-lines then-body (inc indent-level))
            (if (nil? else-body)
              []
              (concat [(indent indent-level "else:")]
                      (emit-body-lines else-body (inc indent-level)))))))

(defn- emit-while-stmt-lines [stmt indent-level]
  (let [condition (:condition stmt)
        body (:body stmt)
        orelse (:orelse stmt)]
    (concat [(indent indent-level (str "while " (emit-expr condition) ":"))]
            (emit-body-lines body (inc indent-level))
            (if (some? orelse)
              (concat [(indent indent-level "else:")]
                      (emit-body-lines orelse (inc indent-level)))
              []))))

(defn- emit-try-stmt-lines [stmt indent-level]
  (let [body (:body stmt)
        handlers (:handlers stmt)
        else-body (:else-body stmt)
        finally-body (:finally-body stmt)]
    (when (and (empty? handlers) (nil? finally-body))
      (throw (ex-info "try statement must have at least one except or finally clause"
                      {:stmt stmt})))
    (when (and (some? else-body) (empty? handlers))
      (throw (ex-info "try statement else clause requires at least one except clause"
                      {:stmt stmt})))
    (concat [(indent indent-level "try:")]
            (emit-body-lines body (inc indent-level))
            (mapcat #(emit-except-handler-lines % indent-level) handlers)
            (if (some? else-body)
              (concat [(indent indent-level "else:")]
                      (emit-body-lines else-body (inc indent-level)))
              [])
            (if (some? finally-body)
              (concat [(indent indent-level "finally:")]
                      (emit-body-lines finally-body (inc indent-level)))
              []))))

(defn- emit-match-stmt-lines [stmt indent-level]
  (let [cases (:cases stmt)]
    (when (empty? cases)
      (throw (ex-info "match statement requires at least one case"
                      {:stmt stmt})))
    (concat [(indent indent-level (str "match " (emit-expr (:subject stmt)) ":"))]
            (mapcat #(emit-match-case-lines % (inc indent-level)) cases))))

(defn- emit-raise-stmt-lines [stmt indent-level]
  (let [expr (:expr stmt)
        cause (:cause stmt)]
    (when (and (some? cause) (nil? expr))
      (throw (ex-info "raise with cause requires an exception expression"
                      {:stmt stmt})))
    [(indent indent-level
             (cond
               (nil? expr) "raise"
               (some? cause) (str "raise " (emit-expr expr)
                                  " from " (emit-expr cause))
               :else (str "raise " (emit-expr expr))))]))

(defn- emit-assert-stmt-lines [stmt indent-level]
  [(indent indent-level
           (str "assert "
                (emit-expr (:test stmt))
                (when (some? (:msg stmt))
                  (str ", " (emit-expr (:msg stmt))))))])

(defn- emit-delete-stmt-lines [stmt indent-level]
  (let [targets (:targets stmt)]
    (when (empty? targets)
      (throw (ex-info "delete statement requires at least one target"
                      {:stmt stmt})))
    [(indent indent-level
             (str "del " (str/join ", " (map emit-assign-target targets))))]))

(defn- emit-global-stmt-lines [stmt indent-level]
  (let [names (:names stmt)]
    (when (empty? names)
      (throw (ex-info "global statement requires at least one name"
                      {:stmt stmt})))
    [(indent indent-level
             (str "global " (str/join ", " (map emit-ident names))))]))

(defn- emit-nonlocal-stmt-lines [stmt indent-level]
  (let [names (:names stmt)]
    (when (empty? names)
      (throw (ex-info "nonlocal statement requires at least one name"
                      {:stmt stmt})))
    [(indent indent-level
             (str "nonlocal " (str/join ", " (map emit-ident names))))]))

(def ^:private stmt-type->emitter
  {:function-def emit-function-def-lines
   :async-function-def emit-function-def-lines
   :class-def emit-class-def-lines
   :import-stmt emit-import-stmt-lines
   :import-from-stmt emit-import-from-stmt-lines
   :return emit-return-stmt-lines
   :expr-stmt emit-expr-stmt-lines
   :assign emit-assign-stmt-lines
   :augassign emit-augassign-stmt-lines
   :if-stmt emit-if-stmt-lines
   :while-stmt emit-while-stmt-lines
   :for-stmt #(emit-for-stmt-lines %1 %2 "for ")
   :async-for-stmt #(emit-for-stmt-lines %1 %2 "async for ")
   :with-stmt #(emit-with-stmt-lines %1
                                     %2
                                     "with "
                                     "with statement requires at least one with-item")
   :async-with-stmt #(emit-with-stmt-lines %1
                                           %2
                                           "async with "
                                           "async with statement requires at least one with-item")
   :try-stmt emit-try-stmt-lines
   :match-stmt emit-match-stmt-lines
   :break-stmt (fn [_ indent-level] [(indent indent-level "break")])
   :continue-stmt (fn [_ indent-level] [(indent indent-level "continue")])
   :pass-stmt (fn [_ indent-level] [(indent indent-level "pass")])
   :raise-stmt emit-raise-stmt-lines
   :assert-stmt emit-assert-stmt-lines
   :delete-stmt emit-delete-stmt-lines
   :global-stmt emit-global-stmt-lines
   :nonlocal-stmt emit-nonlocal-stmt-lines})

(defn- emit-stmt-lines [stmt indent-level]
  (if-let [emitter (get stmt-type->emitter (:type stmt))]
    (emitter stmt indent-level)
    (throw (ex-info (str "Unsupported statement type: " (:type stmt))
                    {:stmt stmt}))))

(defn- emit-top-level-stmt [stmt]
  (str/join "\n" (emit-stmt-lines stmt 0)))

(defn emit-module [module]
  (let [body (:body module)]
    (if (seq body)
      (str (str/join "\n\n" (map emit-top-level-stmt body)) "\n")
      "")))
