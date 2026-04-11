(ns omkamra.pygen.parser
  (:require [clojure.string :as str]
            [omkamra.pygen.ast :as ast]))

(def ^:private arith-binop-symbol->kw
  {'+ :+
   '- :-
   '* :*
   '/ :/
   '% :%})

(def ^:private compare-symbol->kw
  {'> :>
   '< :<
   '>= :>=
   '<= :<=
   '= :==
   '== :==
   '!= :!=
   'in :in
   'not-in :not-in
   'is :is
   'is-not :is-not})

(def ^:private boolop-symbol->kw
  {'and :and
   'or :or})

(def ^:private unaryop-symbol->kw
  {'not :not
   '+ :uadd
   '- :usub
   'bit-not :invert})

(defn- literal? [x]
  (or (string? x)
      (number? x)
      (true? x)
      (false? x)
      (nil? x)))

(defn- parse-symbol-expr [sym]
  (let [parts (str/split (name sym) #"\.")]
    (when (some str/blank? parts)
      (throw (ex-info "invalid dotted symbol"
                      {:symbol sym :parts parts})))
    (if (= 1 (count parts))
      sym
      (reduce (fn [value attr-part]
                (ast/attribute value (symbol attr-part)))
              (symbol (first parts))
              (rest parts)))))

(defn- prefixed-symbol? [x prefix]
  (and (symbol? x)
       (str/starts-with? (name x) prefix)))

(defn- vararg-symbol? [x]
  (and (prefixed-symbol? x "*")
       (not (prefixed-symbol? x "**"))))

(defn- kwarg-symbol? [x]
  (prefixed-symbol? x "**"))

(defn- strip-prefix-symbol [sym prefix form]
  (let [raw (subs (name sym) (count prefix))]
    (when (str/blank? raw)
      (throw (ex-info "prefixed argument/parameter name must not be empty"
                      {:form form :symbol sym :prefix prefix})))
    (when (str/starts-with? raw "*")
      (throw (ex-info "invalid prefixed argument/parameter name"
                      {:form form :symbol sym :prefix prefix})))
    (symbol raw)))

(declare parse-expr parse-pattern)

(def ^:private no-default ::no-default)

(defn- parse-param-spec [spec form]
  (cond
    (symbol? spec)
    {:name spec :default no-default}

    (and (vector? spec)
         (= 2 (count spec))
         (symbol? (first spec)))
    {:name (first spec)
     :default (parse-expr (second spec))}

    :else
    (throw (ex-info "parameter must be symbol or [name default]"
                    {:form form :param spec}))))

(defn- parse-params [params form]
  (when-not (vector? params)
    (throw (ex-info "params must be a vector" {:form form :params params})))
  (loop [remaining params
         positional []
         posonly-count nil
         vararg nil
         kwonly []
         kwarg nil
         kwonly-mode? false
         bare-star? false]
    (if (empty? remaining)
      (do
        (when (and bare-star? (nil? vararg) (empty? kwonly) (nil? kwarg))
          (throw (ex-info "bare * must be followed by at least one keyword-only parameter or **kwargs"
                          {:form form :params params})))
        (let [pos-default-start (or (first (keep-indexed
                                            (fn [idx p]
                                              (when (not= no-default (:default p)) idx))
                                            positional))
                                    (count positional))]
          (when (some #(= no-default (:default %)) (drop pos-default-start positional))
            (throw (ex-info "non-default positional parameter follows default parameter"
                            {:form form :params params})))
          (let [posonly-n (or posonly-count 0)
                posonly-entries (subvec (vec positional) 0 posonly-n)
                args-entries (subvec (vec positional) posonly-n)
                defaults (mapv :default (drop pos-default-start positional))
                kw-defaults (mapv (fn [p]
                                    (if (= no-default (:default p))
                                      ast/kw-required
                                      (:default p)))
                                  kwonly)]
            (ast/arguments
             (mapv #(ast/arg (:name %)) posonly-entries)
             (mapv #(ast/arg (:name %)) args-entries)
             vararg
             (mapv #(ast/arg (:name %)) kwonly)
             kw-defaults
             kwarg
             defaults))))
      (let [param (first remaining)
            more (rest remaining)]
        (cond
          (= '/ param)
          (do
            (when (some? posonly-count)
              (throw (ex-info "params may contain at most one / marker"
                              {:form form :params params})))
            (when (or kwonly-mode? (some? vararg) (some? kwarg))
              (throw (ex-info "/ marker must appear before * / *args / **kwargs"
                              {:form form :params params})))
            (when (empty? positional)
              (throw (ex-info "/ marker requires at least one positional parameter before it"
                              {:form form :params params})))
            (recur more positional (count positional) vararg kwonly kwarg kwonly-mode? bare-star?))

          (= '* param)
          (do
            (when (or kwonly-mode? bare-star?)
              (throw (ex-info "params may contain at most one bare * marker"
                              {:form form :params params})))
            (when (some? vararg)
              (throw (ex-info "bare * marker cannot appear after *args"
                              {:form form :params params})))
            (when (some? kwarg)
              (throw (ex-info "bare * marker cannot appear after **kwargs"
                              {:form form :params params})))
            (recur more positional posonly-count vararg kwonly kwarg true true))

          (kwarg-symbol? param)
          (let [arg-name (strip-prefix-symbol param "**" form)]
            (when (some? kwarg)
              (throw (ex-info "params may contain at most one **kwargs parameter"
                              {:form form :params params :param param})))
            (when (seq more)
              (throw (ex-info "**kwargs parameter must be last"
                              {:form form :params params :param param})))
            (recur more
                   positional
                   posonly-count
                   vararg
                   kwonly
                   (ast/arg arg-name)
                   kwonly-mode?
                   bare-star?))

          (vararg-symbol? param)
          (let [arg-name (strip-prefix-symbol param "*" form)]
            (when bare-star?
              (throw (ex-info "*args parameter cannot appear after bare * marker"
                              {:form form :params params :param param})))
            (when (some? vararg)
              (throw (ex-info "params may contain at most one *args parameter"
                              {:form form :params params :param param})))
            (when (some? kwarg)
              (throw (ex-info "*args parameter cannot appear after **kwargs"
                              {:form form :params params :param param})))
            (recur more
                   positional
                   posonly-count
                   (ast/arg arg-name)
                   kwonly
                   kwarg
                   true
                   bare-star?))

          :else
          (let [{:keys [name default] :as p} (parse-param-spec param form)]
            (when-not (symbol? name)
              (throw (ex-info "parameter name must be a symbol"
                              {:form form :params params :param param})))
            (when (contains? #{'/ '*} name)
              (throw (ex-info "parameter name must not be / or *"
                              {:form form :params params :param param})))
            (when (or (vararg-symbol? name) (kwarg-symbol? name))
              (throw (ex-info "parameter name must not be prefixed with * or **"
                              {:form form :params params :param param})))
            (if kwonly-mode?
              (recur more positional posonly-count vararg (conj kwonly p) kwarg kwonly-mode? bare-star?)
              (recur more (conj positional p) posonly-count vararg kwonly kwarg kwonly-mode? bare-star?))))))))

(declare parse-expr parse-stmt parse-top-level parse-function-def parse-async-function-def parse-else-clause)

(defn- expect-form-count [form expected message]
  (when-not (= expected (count form))
    (throw (ex-info message {:form form}))))

(defn- expect-form-counts [form expected-counts message]
  (when-not (contains? expected-counts (count form))
    (throw (ex-info message {:form form}))))

(defn- expect-form-max-count [form max-count message]
  (when (> (count form) max-count)
    (throw (ex-info message {:form form}))))

(defn- expect-non-empty [xs message ex-data]
  (when (empty? xs)
    (throw (ex-info message ex-data))))

(defn- parse-block [form]
  (cond
    (nil? form) []
    (vector? form) (mapv parse-stmt form)
    :else [(parse-stmt form)]))

(defn- parse-one-expr-arg [form message]
  (expect-form-count form 2 message)
  (parse-expr (second form)))

(defn- parse-zero-or-one-expr-arg [form message]
  (expect-form-max-count form 2 message)
  (when (= 2 (count form))
    (parse-expr (second form))))

(defn- parse-return [[_ expr :as form]]
  (ast/return (parse-one-expr-arg form "return expects exactly one argument")))

(defn- parse-alias-spec [spec form]
  (cond
    (symbol? spec) (ast/alias spec)
    (and (vector? spec)
         (= 2 (count spec))
         (every? symbol? spec))
    (ast/alias (first spec) (second spec))
    :else (throw (ex-info "alias must be symbol or [name asname]"
                          {:form form :alias spec}))))

(defn- parse-alias-list [kind names form]
  (expect-non-empty names
                    (str kind " expects at least one name")
                    {:form form})
  (mapv #(parse-alias-spec % form) names))

(defn- parse-import-form [[_ & names :as form]]
  (ast/import-stmt (parse-alias-list "import" names form)))

(defn- parse-from-import-form [[_ module import-kw & names :as form]]
  (when-not (= 'import import-kw)
    (throw (ex-info "from form expects literal 'import' keyword"
                    {:form form :import-keyword import-kw})))
  (when-not (symbol? module)
    (throw (ex-info "from module must be a symbol" {:form form :module module})))
  (ast/import-from-stmt module (parse-alias-list "from ... import" names form)))

(defn- assign-target? [target]
  (or (symbol? target)
      (and (map? target)
           (contains? #{:attribute :subscript} (:type target)))))

(defn- parse-assign-target [target-form]
  (let [target (parse-expr target-form)]
    (when-not (assign-target? target)
      (throw (ex-info "assignment target must be a symbol, attribute, or subscript"
                      {:target target-form :parsed target})))
    target))

(defn- parse-assign! [[_ target expr :as form]]
  (expect-form-count form 3 "assign! expects target and expression")
  (ast/assign (parse-assign-target target) (parse-expr expr)))

(defn- parse-del [[_ & targets :as form]]
  (expect-non-empty targets "del expects at least one target" {:form form})
  (ast/delete-stmt (mapv parse-assign-target targets)))

(defn- parse-name-list-stmt [kind names form]
  (expect-non-empty names
                    (str kind " expects at least one name")
                    {:form form :kind kind})
  (when-not (every? symbol? names)
    (throw (ex-info (str kind " names must all be symbols")
                    {:form form :kind kind :names names})))
  names)

(defn- parse-global [[_ & names :as form]]
  (ast/global-stmt (parse-name-list-stmt "global" names form)))

(defn- parse-nonlocal [[_ & names :as form]]
  (ast/nonlocal-stmt (parse-name-list-stmt "nonlocal" names form)))

(defn- parse-update! [[_ target op expr :as form]]
  (expect-form-count form 4 "update! expects target, operator, and expression")
  (let [op-kw (get arith-binop-symbol->kw op)]
    (when-not op-kw
      (throw (ex-info "Unsupported update! operator"
                      {:form form :op op :supported (keys arith-binop-symbol->kw)})))
    (ast/augassign (parse-assign-target target) op-kw (parse-expr expr))))

(defn- parse-if-form [[_ condition then-form else-form :as form]]
  (expect-form-counts form #{3 4} "if expects (if cond then) or (if cond then else)")
  (if (nil? else-form)
    (ast/if-stmt (parse-expr condition)
                 (parse-block then-form))
    (ast/if-stmt (parse-expr condition)
                 (parse-block then-form)
                 (parse-block else-form))))

(defn- split-loop-body-and-else [body-forms form loop-kind]
  (let [else-forms (filter #(and (seq? %) (= 'else (first %))) body-forms)]
    (when (> (count else-forms) 1)
      (throw (ex-info (str loop-kind " may contain at most one else clause")
                      {:form form :loop-kind loop-kind :body-forms body-forms})))
    (when (some #(and (seq? %) (= 'else (first %))) (butlast body-forms))
      (throw (ex-info (str loop-kind " else clause must be last")
                      {:form form :loop-kind loop-kind :body-forms body-forms})))
    (if (and (seq body-forms)
             (seq? (last body-forms))
             (= 'else (first (last body-forms))))
      [(vec (butlast body-forms))
       (parse-else-clause (last body-forms))]
      [body-forms nil])))

(defn- parse-while-form [[_ condition & body-forms :as form]]
  (when (empty? body-forms)
    (throw (ex-info "while expects condition and at least one body form" {:form form})))
  (let [[body orelse] (split-loop-body-and-else body-forms form "while")]
    (when (empty? body)
      (throw (ex-info "while expects condition and at least one body form"
                      {:form form})))
    (ast/while-stmt
     (parse-expr condition)
     (mapv parse-stmt body)
     orelse)))

(defn- parse-for-like-form [[_ target iter-form & body-forms :as form] loop-op ctor]
  (let [loop-kind (name loop-op)
        body-msg (str loop-kind " expects target, iterator expression, and at least one body form")]
    (when-not (symbol? target)
      (throw (ex-info (str loop-kind " target must be a symbol")
                      {:form form :target target})))
    (when (empty? body-forms)
      (throw (ex-info body-msg {:form form})))
    (let [[body orelse] (split-loop-body-and-else body-forms form loop-kind)]
      (when (empty? body)
        (throw (ex-info body-msg {:form form})))
      (ctor target
            (parse-expr iter-form)
            (mapv parse-stmt body)
            orelse))))

(defn- parse-for-form [form]
  (parse-for-like-form form 'for ast/for-stmt))

(defn- parse-async-for-form [form]
  (parse-for-like-form form 'async-for ast/async-for-stmt))

(defn- parse-with-bindings [bindings form]
  (when-not (vector? bindings)
    (throw (ex-info "with expects a vector of context/as-target pairs"
                    {:form form :bindings bindings})))
  (expect-non-empty bindings
                    "with expects at least one context/as-target pair"
                    {:form form :bindings bindings})
  (when (odd? (count bindings))
    (throw (ex-info "with binding vector must contain an even number of forms"
                    {:form form :bindings bindings})))
  (mapv (fn [[context-form target-form]]
          (ast/with-item (parse-expr context-form)
                         (when (some? target-form)
                           (parse-assign-target target-form))))
        (partition 2 bindings)))

(defn- parse-with-like-form [[_ bindings & body-forms :as form] with-op ctor]
  (let [with-kind (name with-op)]
    (when (empty? body-forms)
      (throw (ex-info (str with-kind " expects at least one body form")
                      {:form form})))
    (ctor (parse-with-bindings bindings form)
          (mapv parse-stmt body-forms))))

(defn- parse-with-form [form]
  (parse-with-like-form form 'with ast/with-stmt))

(defn- parse-async-with-form [form]
  (parse-with-like-form form 'async-with ast/async-with-stmt))

(defn- parse-class-def [[_ name bases & body-forms :as form]]
  (when-not (symbol? name)
    (throw (ex-info "class name must be a symbol" {:form form :name name})))
  (when-not (vector? bases)
    (throw (ex-info "class bases must be a vector" {:form form :bases bases})))
  (ast/class-def name
                 (mapv parse-expr bases)
                 (mapv parse-stmt body-forms)))

(defn- parse-argless-stmt [form kind ctor]
  (let [[_ & args] form]
    (if (seq args)
      (throw (ex-info (str kind " expects no arguments") {:form form}))
      (ctor))))

(defn- parse-break [form]
  (parse-argless-stmt form "break" ast/break-stmt))

(defn- parse-continue [form]
  (parse-argless-stmt form "continue" ast/continue-stmt))

(defn- parse-pass [form]
  (parse-argless-stmt form "pass" ast/pass-stmt))

(defn- parse-raise [[_ & args :as form]]
  (expect-form-max-count form 3 "raise expects zero, one, or two arguments")
  (let [[exc-expr cause-expr] args]
    (ast/raise-stmt (when (some? exc-expr) (parse-expr exc-expr))
                    (when (some? cause-expr) (parse-expr cause-expr)))))

(defn- parse-assert [[_ test-expr msg-expr :as form]]
  (expect-form-counts form
                      #{2 3}
                      "assert expects (assert test) or (assert test msg)")
  (ast/assert-stmt (parse-expr test-expr)
                   (when (= 3 (count form))
                     (parse-expr msg-expr))))

(defn- simple-ident-symbol? [sym]
  (and (symbol? sym)
       (not (str/includes? (name sym) "."))))

(defn- parse-pattern-capture-name [name-form form kind]
  (when-not (symbol? name-form)
    (throw (ex-info (str kind " name must be a symbol")
                    {:form form :name name-form :kind kind})))
  (when (not (simple-ident-symbol? name-form))
    (throw (ex-info (str kind " name must be a simple symbol")
                    {:form form :name name-form :kind kind})))
  (when (= '& name-form)
    (throw (ex-info (str kind " name must not be &")
                    {:form form :name name-form :kind kind})))
  name-form)

(defn- parse-match-symbol-pattern [sym]
  (cond
    (= '_ sym) (ast/match-as nil nil)
    (str/includes? (name sym) ".") (ast/match-value (parse-symbol-expr sym))
    :else (ast/match-as nil (parse-pattern-capture-name sym sym "capture"))))

(defn- parse-vector-pattern [v form]
  (loop [remaining (seq v)
         patterns []]
    (if (empty? remaining)
      (ast/match-sequence patterns)
      (let [x (first remaining)
            more (rest remaining)]
        (if (= '& x)
          (do
            (when (empty? more)
              (throw (ex-info "vector pattern rest marker & requires a following name"
                              {:form form :pattern v})))
            (when (seq (rest more))
              (throw (ex-info "vector pattern may contain at most one rest capture"
                              {:form form :pattern v})))
            (let [rest-name (first more)
                  parsed-rest-name (if (= '_ rest-name)
                                     nil
                                     (parse-pattern-capture-name rest-name form "vector rest"))]
              (ast/match-sequence (conj patterns (ast/match-star parsed-rest-name))))
            )
          (recur more (conj patterns (parse-pattern x))))))))

(defn- parse-map-pattern [m form]
  (let [rest-form (get m '& ::no-rest)
        entries (if (= ::no-rest rest-form) m (dissoc m '&))
        parsed-rest (when-not (= ::no-rest rest-form)
                      (parse-pattern-capture-name rest-form form "mapping rest"))
        keys (mapv (fn [[k _]] (parse-expr k)) entries)
        patterns (mapv (fn [[_ v]] (parse-pattern v)) entries)]
    (when (= '_ parsed-rest)
      (throw (ex-info "mapping rest name must not be _"
                      {:form form :name rest-form})))
    (ast/match-mapping keys patterns parsed-rest)))

(defn- parse-eq-pattern [[_ expr & extra :as form]]
  (when (or (nil? expr) (seq extra))
    (throw (ex-info "= pattern expects exactly one expression argument"
                    {:form form})))
  (ast/match-value (parse-expr expr)))

(defn- parse-match-or-pattern [[_ & pattern-forms :as form]]
  (when (< (count pattern-forms) 2)
    (throw (ex-info "or pattern expects at least two alternatives"
                    {:form form})))
  (ast/match-or (mapv parse-pattern pattern-forms)))

(defn- parse-match-as-pattern [[_ pattern-form name-form & extra :as form]]
  (when (or (nil? pattern-form) (nil? name-form) (seq extra))
    (throw (ex-info "as pattern expects exactly two arguments: pattern and name"
                    {:form form})))
  (let [name (parse-pattern-capture-name name-form form "as")]
    (when (= '_ name)
      (throw (ex-info "as pattern name must not be _"
                      {:form form :name name-form})))
    (ast/match-as (parse-pattern pattern-form) name)))

(defn- keyword->attr-symbol [kw form]
  (when-not (keyword? kw)
    (throw (ex-info "class pattern keyword must be a keyword"
                    {:form form :keyword kw})))
  (when (namespace kw)
    (throw (ex-info "class pattern keyword must not be namespaced"
                    {:form form :keyword kw})))
  (symbol (name kw)))

(defn- parse-match-class-pattern [[cls-form & args :as form]]
  (loop [remaining args
         positional []
         kwd-attrs []
         kwd-patterns []
         seen-keyword? false]
    (if (empty? remaining)
      (ast/match-class (parse-expr cls-form) positional kwd-attrs kwd-patterns)
      (let [x (first remaining)]
        (if (keyword? x)
          (let [value-form (second remaining)]
            (when (< (count remaining) 2)
              (throw (ex-info "class pattern keyword arguments must be :name pattern pairs"
                              {:form form :keyword x})))
            (recur (nnext remaining)
                   positional
                   (conj kwd-attrs (keyword->attr-symbol x form))
                   (conj kwd-patterns (parse-pattern value-form))
                   true))
          (do
            (when seen-keyword?
              (throw (ex-info "class pattern positional patterns must not follow keyword patterns"
                              {:form form :argument x})))
            (recur (rest remaining)
                   (conj positional (parse-pattern x))
                   kwd-attrs
                   kwd-patterns
                   seen-keyword?)))))))

(defn- parse-pattern [form]
  (cond
    (symbol? form) (parse-match-symbol-pattern form)
    (or (true? form) (false? form) (nil? form)) (ast/match-singleton form)
    (or (string? form) (number? form)) (ast/match-value form)
    (vector? form) (parse-vector-pattern form form)
    (map? form) (parse-map-pattern form form)
    (seq? form) (case (first form)
                  = (parse-eq-pattern form)
                  or (parse-match-or-pattern form)
                  as (parse-match-as-pattern form)
                  (parse-match-class-pattern form))
    :else (throw (ex-info "Unsupported match pattern form"
                          {:form form}))))

(defn- parse-match-case-form [[case-op pattern-form & remaining :as form]]
  (when-not (= 'case case-op)
    (throw (ex-info "match case must start with case"
                    {:form form :case-op case-op})))
  (let [[guard-form body-forms]
        (if (and (seq remaining) (= :if (first remaining)))
          (let [[_ guard & body] remaining]
            (when (nil? guard)
              (throw (ex-info "case :if clause requires a guard expression"
                              {:form form})))
            [guard body])
          [nil remaining])]
    (expect-non-empty body-forms "case expects at least one body form" {:form form})
    (ast/match-case (parse-pattern pattern-form)
                    (when (some? guard-form) (parse-expr guard-form))
                    (mapv parse-stmt body-forms))))

(defn- parse-match-form [[_ subject-form & case-forms :as form]]
  (when (nil? subject-form)
    (throw (ex-info "match expects subject expression and at least one case"
                    {:form form})))
  (expect-non-empty case-forms
                    "match expects subject expression and at least one case"
                    {:form form})
  (when-not (every? (fn [x] (and (seq? x) (= 'case (first x)))) case-forms)
    (throw (ex-info "match expects only case forms after the subject"
                    {:form form :cases case-forms})))
  (ast/match-stmt (parse-expr subject-form)
                  (mapv parse-match-case-form case-forms)))

(def ^:private try-clause-heads
  '#{except else finally})

(defn- try-clause-form? [form]
  (and (seq? form)
       (contains? try-clause-heads (first form))))

(defn- parse-except-spec [spec form]
  (when-not (vector? spec)
    (throw (ex-info "except spec must be a vector"
                    {:form form :spec spec})))
  (case (count spec)
    0 {:exception-types nil
       :name nil}
    1 (let [exc-form (first spec)
            exception-types (if (vector? exc-form)
                              (do
                                (when (empty? exc-form)
                                  (throw (ex-info "except type vector must not be empty"
                                                  {:form form :spec spec})))
                                (mapv parse-expr exc-form))
                              [(parse-expr exc-form)])]
        {:exception-types exception-types
         :name nil})
    2 (let [[exc-form name] spec
            exception-types (if (vector? exc-form)
                              (do
                                (when (empty? exc-form)
                                  (throw (ex-info "except type vector must not be empty"
                                                  {:form form :spec spec})))
                                (mapv parse-expr exc-form))
                              [(parse-expr exc-form)])]
        (when-not (symbol? name)
          (throw (ex-info "except name must be a symbol"
                          {:form form :spec spec :name name})))
        {:exception-types exception-types
         :name name})
    (throw (ex-info "except spec must be [], [type], [type name], [[type ...]], or [[type ...] name]"
                    {:form form :spec spec}))))

(defn- parse-except-clause [[_ spec & body-forms :as form]]
  (when (empty? body-forms)
    (throw (ex-info "except expects at least one body form" {:form form})))
  (let [{:keys [exception-types name]} (parse-except-spec spec form)]
    (ast/except-handler exception-types
                        name
                        (mapv parse-stmt body-forms))))

(defn- parse-else-clause [[_ & body-forms :as form]]
  (when (empty? body-forms)
    (throw (ex-info "else expects at least one body form" {:form form})))
  (mapv parse-stmt body-forms))

(defn- parse-finally-clause [[_ & body-forms :as form]]
  (when (empty? body-forms)
    (throw (ex-info "finally expects at least one body form" {:form form})))
  (mapv parse-stmt body-forms))

(defn- split-try-body-and-clauses [forms try-form]
  (loop [remaining forms
         body-forms []
         clause-forms []
         seen-clause? false]
    (if (empty? remaining)
      [body-forms clause-forms]
      (let [x (first remaining)]
        (if (try-clause-form? x)
          (recur (rest remaining) body-forms (conj clause-forms x) true)
          (do
            (when seen-clause?
              (throw (ex-info "try body forms must come before except/else/finally clauses"
                              {:form try-form :offending-form x})))
            (recur (rest remaining) (conj body-forms x) clause-forms false)))))))

(defn- parse-try-form [[_ & forms :as form]]
  (when (empty? forms)
    (throw (ex-info "try expects body forms and at least one clause" {:form form})))
  (let [[body-forms clause-forms] (split-try-body-and-clauses forms form)]
    (when (empty? body-forms)
      (throw (ex-info "try expects at least one body form" {:form form})))
    (let [body (mapv parse-stmt body-forms)]
      (loop [remaining clause-forms
             handlers []
             else-body nil
             finally-body nil]
        (if (empty? remaining)
          (do
            (when (and (empty? handlers) (nil? finally-body))
              (throw (ex-info "try expects at least one except clause or a finally clause"
                              {:form form})))
            (ast/try-stmt body handlers else-body finally-body))
          (let [clause (first remaining)
                head (first clause)]
            (case head
              except (do
                       (when (some? else-body)
                         (throw (ex-info "except clauses must appear before else"
                                         {:form form :clause clause})))
                       (when (some? finally-body)
                         (throw (ex-info "except clauses must appear before finally"
                                         {:form form :clause clause})))
                       (recur (rest remaining)
                              (conj handlers (parse-except-clause clause))
                              else-body
                              finally-body))
              else (do
                     (when (some? else-body)
                       (throw (ex-info "try may contain at most one else clause"
                                       {:form form :clause clause})))
                     (when (empty? handlers)
                       (throw (ex-info "else clause requires at least one except clause"
                                       {:form form :clause clause})))
                     (when (some? finally-body)
                       (throw (ex-info "else clause must appear before finally"
                                       {:form form :clause clause})))
                     (recur (rest remaining)
                            handlers
                            (parse-else-clause clause)
                            finally-body))
              finally (do
                        (when (some? finally-body)
                          (throw (ex-info "try may contain at most one finally clause"
                                          {:form form :clause clause})))
                        (when (seq (rest remaining))
                          (throw (ex-info "finally clause must be the last try clause"
                                          {:form form :clause clause
                                           :remaining (rest remaining)})))
                        (recur (rest remaining)
                               handlers
                               else-body
                               (parse-finally-clause clause)))
              (throw (ex-info "unsupported try clause"
                              {:form form :clause clause})))))))))

(defn- parse-binop-form [[op lhs rhs :as form]]
  (expect-form-count form 3 "binary operator expects exactly two arguments")
  (let [op-kw (get arith-binop-symbol->kw op)]
    (when-not op-kw
      (throw (ex-info "Unsupported binary operator"
                      {:form form :op op :supported (keys arith-binop-symbol->kw)})))
    (ast/binop op-kw
               (parse-expr lhs)
               (parse-expr rhs))))

(defn- parse-compare-form [[op & operands :as form]]
  (when (< (count operands) 2)
    (throw (ex-info "comparison expects at least two operands" {:form form :op op})))
  (let [op-kw (get compare-symbol->kw op)]
    (when-not op-kw
      (throw (ex-info "Unsupported comparison operator"
                      {:form form :op op :supported (keys compare-symbol->kw)})))
    (ast/compare (parse-expr (first operands))
                 (vec (repeat (dec (count operands)) op-kw))
                 (mapv parse-expr (rest operands)))))

(defn- parse-py-compare-form [[_ left & op-and-comparators :as form]]
  (when (< (count op-and-comparators) 2)
    (throw (ex-info "py-compare expects at least one operator/comparator pair"
                    {:form form})))
  (when (odd? (count op-and-comparators))
    (throw (ex-info "py-compare expects alternating op comparator pairs"
                    {:form form :pairs op-and-comparators})))
  (let [pairs (partition 2 op-and-comparators)
        op-forms (mapv first pairs)
        comparators (mapv second pairs)]
    (when-not (every? symbol? op-forms)
      (throw (ex-info "py-compare operators must be symbols"
                      {:form form :ops op-forms})))
    (let [ops (mapv #(get compare-symbol->kw %) op-forms)]
      (when (some nil? ops)
        (throw (ex-info "Unsupported py-compare operator"
                        {:form form
                         :ops op-forms
                         :supported (keys compare-symbol->kw)})))
      (ast/compare (parse-expr left)
                   ops
                   (mapv parse-expr comparators)))))

(defn- parse-boolop-form [[op & operands :as form]]
  (when (< (count operands) 2)
    (throw (ex-info "boolean operator expects at least two operands" {:form form :op op})))
  (let [op-kw (get boolop-symbol->kw op)]
    (when-not op-kw
      (throw (ex-info "Unsupported boolean operator"
                      {:form form :op op :supported (keys boolop-symbol->kw)})))
    (ast/boolop op-kw (mapv parse-expr operands))))

(defn- parse-unaryop-form [[op operand & extra :as form]]
  (when (or (nil? operand) (seq extra))
    (throw (ex-info "unary operator expects exactly one operand" {:form form :op op})))
  (let [op-kw (get unaryop-symbol->kw op)]
    (when-not op-kw
      (throw (ex-info "Unsupported unary operator"
                      {:form form :op op :supported (keys unaryop-symbol->kw)})))
    (ast/unaryop op-kw (parse-expr operand))))

(defn- parse-if-else-form [[_ test-expr body-expr orelse-expr & extra :as form]]
  (when (or (nil? test-expr) (nil? body-expr) (nil? orelse-expr) (seq extra))
    (throw (ex-info "if-else expects exactly three arguments: test, body, orelse"
                    {:form form})))
  (ast/if-exp (parse-expr test-expr)
              (parse-expr body-expr)
              (parse-expr orelse-expr)))

(defn- parse-lambda-form [[_ params body-expr & extra :as form]]
  (when (or (nil? params) (nil? body-expr) (seq extra))
    (throw (ex-info "lambda expects exactly two arguments: params vector and body expression"
                    {:form form})))
  (when-not (vector? params)
    (throw (ex-info "lambda params must be a vector"
                    {:form form :params params})))
  (ast/lambda-expr (parse-params params form) (parse-expr body-expr)))

(defn- parse-yield-form [form]
  (ast/yield-expr (parse-zero-or-one-expr-arg form
                                              "yield expects zero or one argument")))

(defn- parse-yield-from-form [form]
  (ast/yield-from-expr (parse-one-expr-arg form
                                           "yield-from expects exactly one argument")))

(defn- parse-await-form [form]
  (ast/await-expr (parse-one-expr-arg form
                                      "await expects exactly one argument")))

(defn- parse-py-named-form [[_ target value :as form]]
  (expect-form-count form 3 "py-named expects target symbol and value expression")
  (when-not (symbol? target)
    (throw (ex-info "py-named target must be a symbol"
                    {:form form :target target})))
  (let [parsed-target (parse-symbol-expr target)]
    (when-not (symbol? parsed-target)
      (throw (ex-info "py-named target must be a simple symbol"
                      {:form form :target target})))
    (ast/named-expr parsed-target (parse-expr value))))

(defn- parse-attribute-form [[_ value attr & extra :as form]]
  (when (or (nil? value) (nil? attr) (seq extra))
    (throw (ex-info "attribute expects value and attribute name" {:form form})))
  (when-not (symbol? attr)
    (throw (ex-info "attribute name must be a symbol" {:form form :attr attr})))
  (ast/attribute (parse-expr value) attr))

(defn- parse-subscript-form [[_ value slice & extra :as form]]
  (when (or (nil? value) (nil? slice) (seq extra))
    (throw (ex-info "py-get expects value and index/slice expression" {:form form})))
  (ast/subscript (parse-expr value) (parse-expr slice)))

(defn- parse-py-slice-form [[_ value start stop step :as form]]
  (expect-form-counts form
                      #{4 5}
                      "py-slice expects value, start, stop, and optional step")
  (let [step-form (if (= 5 (count form)) step nil)]
    (ast/subscript
     (parse-expr value)
     (ast/slice-expr (parse-expr start)
                     (parse-expr stop)
                     (parse-expr step-form)))))

(defn- parse-tuple-form [[_ & elements]]
  (ast/tuple (mapv parse-expr elements)))

(defn- parse-dict-literal [m]
  (let [entries (seq m)]
    (ast/dict (mapv (fn [[k _]] (parse-expr k)) entries)
              (mapv (fn [[_ v]] (parse-expr v)) entries))))

(defn- parse-set-literal [s]
  (ast/set-expr (mapv parse-expr (seq s))))

(defn- parse-comprehension-for-binding [binding form]
  (when-not (and (vector? binding) (= 2 (count binding)))
    (throw (ex-info ":for clause must be [target iterable]"
                    {:form form :for binding})))
  (let [[target iter-form] binding]
    (when-not (symbol? target)
      (throw (ex-info "comprehension target must be a symbol"
                      {:form form :target target})))
    (ast/comprehension target (parse-expr iter-form) [])))

(defn- parse-comprehension-clauses [clauses form]
  (when (empty? clauses)
    (throw (ex-info "comprehension requires at least one clause" {:form form})))
  (when (odd? (count clauses))
    (throw (ex-info "comprehension clauses must be :clause value pairs"
                    {:form form :clauses clauses})))
  (loop [remaining clauses
         current nil
         generators []]
    (if (empty? remaining)
      (let [result (if current (conj generators current) generators)]
        (when (empty? result)
          (throw (ex-info "comprehension requires at least one :for clause"
                          {:form form :clauses clauses})))
        result)
      (let [[clause value & more] remaining]
        (case clause
          :for (let [next-current (parse-comprehension-for-binding value form)
                     next-generators (if current (conj generators current) generators)]
                 (recur more next-current next-generators))
          :if (do
                (when-not current
                  (throw (ex-info ":if clause must follow a :for clause"
                                  {:form form :clauses clauses})))
                (recur more
                       (update current :ifs conj (parse-expr value))
                       generators))
          (throw (ex-info "unsupported comprehension clause"
                          {:form form :clause clause :value value})))))))

(defn- parse-list-form [form]
  (if (and (>= (count form) 3)
           (= :for (nth form 1)))
    (let [elt (parse-expr (nth form 0))
          clauses (subvec form 1)]
      (ast/list-comp elt (parse-comprehension-clauses clauses form)))
    (ast/list-expr (mapv parse-expr form))))

(defn- parse-comprehension-generator-clauses [kind clauses form]
  (when (empty? clauses)
    (throw (ex-info (str kind " expects at least one comprehension clause")
                    {:form form})))
  (when-not (= :for (first clauses))
    (throw (ex-info (str kind " clauses must start with :for")
                    {:form form :clauses clauses})))
  (parse-comprehension-clauses clauses form))

(defn- parse-list-comp-form [[_ elt & clauses :as form]]
  (ast/list-comp (parse-expr elt)
                 (parse-comprehension-generator-clauses "list-comp" clauses form)))

(defn- parse-set-comp-form [[_ elt & clauses :as form]]
  (ast/set-comp (parse-expr elt)
                (parse-comprehension-generator-clauses "set-comp" clauses form)))

(defn- parse-dict-comp-form [[_ key-expr value-expr & clauses :as form]]
  (ast/dict-comp (parse-expr key-expr)
                 (parse-expr value-expr)
                 (parse-comprehension-generator-clauses "dict-comp" clauses form)))

(defn- parse-gen-comp-form [[_ elt & clauses :as form]]
  (ast/generator-exp (parse-expr elt)
                     (parse-comprehension-generator-clauses "gen-comp" clauses form)))

(defn- keyword->arg-symbol [kw form]
  (when-not (keyword? kw)
    (throw (ex-info "call keyword argument name must be a keyword"
                    {:form form :keyword kw})))
  (when (namespace kw)
    (throw (ex-info "namespaced keyword arguments are not supported"
                    {:form form :keyword kw})))
  (symbol (name kw)))

(defn- parse-call-form [[f & args]]
  (let [form (cons f args)]
    (loop [remaining args
           positional []
           keywords []
           seen-keyword? false
           seen-kw-unpack? false]
      (if (empty? remaining)
        (ast/call (parse-expr f) positional keywords)
        (let [arg (first remaining)]
          (cond
            (keyword? arg)
            (let [value (second remaining)]
              (when (< (count remaining) 2)
                (throw (ex-info "keyword arguments must be provided as :name value pairs"
                                {:form form :keyword arg})))
              (recur (nnext remaining)
                     positional
                     (conj keywords
                           (ast/keyword-arg (keyword->arg-symbol arg form)
                                            (parse-expr value)))
                     true
                     seen-kw-unpack?))

            (kwarg-symbol? arg)
            (recur (rest remaining)
                   positional
                   (conj keywords
                         (ast/keyword-arg nil
                                          (parse-expr (strip-prefix-symbol arg "**" form))))
                   true
                   true)

            (vararg-symbol? arg)
            (do
              (when seen-kw-unpack?
                (throw (ex-info "iterable argument unpacking follows keyword argument unpacking"
                                {:form form :arg arg})))
              (recur (rest remaining)
                     (conj positional
                           (ast/starred
                            (parse-expr (strip-prefix-symbol arg "*" form))))
                     keywords
                     seen-keyword?
                     seen-kw-unpack?))

            :else
            (do
              (when seen-keyword?
                (throw (ex-info "positional argument follows keyword argument"
                                {:form form :arg arg})))
              (recur (rest remaining)
                     (conj positional (parse-expr arg))
                     keywords
                     seen-keyword?
                     seen-kw-unpack?))))))))

(defn- parse-renamed-op [form old-op new-op]
  (throw (ex-info (str old-op " has been renamed to " new-op)
                  {:form form :old-op old-op :new-op new-op})))

(def ^:private direct-expr-op->parser
  {'. parse-attribute-form
   'py-get parse-subscript-form
   'py-slice parse-py-slice-form
   'get-item #(parse-renamed-op % 'get-item 'py-get)
   'get-slice #(parse-renamed-op % 'get-slice 'py-slice)
   'py-tuple parse-tuple-form
   'list-comp parse-list-comp-form
   'set-comp parse-set-comp-form
   'dict-comp parse-dict-comp-form
   'gen-comp parse-gen-comp-form
   'py-compare parse-py-compare-form
   'if-else parse-if-else-form
   'lambda parse-lambda-form
   'yield parse-yield-form
   'yield-from parse-yield-from-form
   'await parse-await-form
   'py-named parse-py-named-form})

(defn- parse-expr [form]
  (cond
    (symbol? form) (parse-symbol-expr form)
    (literal? form) form
    (map? form) (parse-dict-literal form)
    (set? form) (parse-set-literal form)
    (vector? form) (parse-list-form form)
    (seq? form) (let [op (first form)]
                  (if-let [parser (get direct-expr-op->parser op)]
                    (parser form)
                    (cond
                      (and (contains? unaryop-symbol->kw op)
                           (= 2 (count form)))
                      (parse-unaryop-form form)
                      (contains? arith-binop-symbol->kw op)
                      (parse-binop-form form)
                      (contains? compare-symbol->kw op)
                      (parse-compare-form form)
                      (contains? boolop-symbol->kw op)
                      (parse-boolop-form form)
                      (contains? unaryop-symbol->kw op)
                      (parse-unaryop-form form)
                      :else
                      (parse-call-form form))))
    :else (throw (ex-info "Unsupported expression form" {:form form}))))

(defn- parse-stmt [form]
  (if (seq? form)
    (case (first form)
      class (parse-class-def form)
      def (parse-function-def form)
      async-def (parse-async-function-def form)
      import (parse-import-form form)
      from (parse-from-import-form form)
      return (parse-return form)
      assign! (parse-assign! form)
      update! (parse-update! form)
      if (parse-if-form form)
      while (parse-while-form form)
      for (parse-for-form form)
      async-for (parse-async-for-form form)
      with (parse-with-form form)
      async-with (parse-async-with-form form)
      try (parse-try-form form)
      match (parse-match-form form)
      break (parse-break form)
      continue (parse-continue form)
      pass (parse-pass form)
      raise (parse-raise form)
      assert (parse-assert form)
      del (parse-del form)
      global (parse-global form)
      nonlocal (parse-nonlocal form)
      (ast/expr-stmt (parse-expr form)))
    (ast/expr-stmt (parse-expr form))))

(defn- parse-function-like-def [[_ name params & body-forms :as form] def-op ctor]
  (let [def-kind (name def-op)]
    (when-not (symbol? name)
      (throw (ex-info (str def-kind " name must be a symbol")
                      {:form form :name name})))
    (when-not (vector? params)
      (throw (ex-info (str def-kind " params must be a vector")
                      {:form form :params params})))
    (ctor name (parse-params params form) (mapv parse-stmt body-forms))))

(defn- parse-function-def [form]
  (parse-function-like-def form 'def ast/function-def))

(defn- parse-async-function-def [form]
  (parse-function-like-def form 'async-def ast/async-function-def))

(def ^:private disallowed-module-stmt-types
  #{:return :break-stmt :continue-stmt :async-for-stmt :async-with-stmt})

(defn- disallowed-module-stmt? [stmt]
  (or (contains? disallowed-module-stmt-types (:type stmt))
      (and (= :expr-stmt (:type stmt))
           (contains? #{:yield :yield-from :await}
                      (:type (:expr stmt))))))

(defn- module-forms? [form]
  (and (sequential? form)
       (every? seq? form)))

(defn- parse-top-level [form]
  (let [stmt (parse-stmt form)]
    (when (disallowed-module-stmt? stmt)
      (throw (ex-info "statement is not allowed at module level"
                      {:form form :statement-type (:type stmt)})))
    stmt))

(defn- parse-module [forms]
  (reduce ast/module-add
          (ast/module)
          (map parse-top-level forms)))

(defn parse [form]
  (cond
    (and (map? form) (:type form)) form
    (module-forms? form) (parse-module form)
    (and (seq? form) (= 'def (first form))) (parse-function-def form)
    (and (seq? form) (= 'async-def (first form))) (parse-async-function-def form)
    (seq? form) (parse-stmt form)
    :else (parse-expr form)))
