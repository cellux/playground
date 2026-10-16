(ns omkamra.dsp.builtins
  "Single source of truth for DSP builtin operations (P2, decisions D4/D11/D13).

  A builtin is a stable portable-IR operation, not a host call.  Each registry
  entry declares:

  - `:operator`  the portable-IR operator keyword;
  - `:kind`      `:primitive` (lowers to a backend instruction/sequence) or
                 `:derived` (expands to other builtins during IR lowering);
  - `:signatures` a vector of `{:params [types...] :return type}` overloads;
  - `:effects`   the effect set (empty for every current builtin);
  - `:expand`    for derived operations, a function from argument symbols to a
                 source form using other builtins and language forms.

  The linker, descriptor type inference, and IR lowering all read this table so
  they cannot drift.  It is deliberately data plus small pure functions so the
  browser compiler (P13) can port it to `.cljc` unchanged.")

(defn- form
  [op & args]
  (list* op args))

(def builtins
  "Registry keyed by source operator symbol."
  {'abs     {:operator :abs
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float] :return :float}
                          {:params [:int] :return :int}]}
   'min     {:operator :min
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float :float] :return :float}
                          {:params [:int :int] :return :int}]}
   'max     {:operator :max
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float :float] :return :float}
                          {:params [:int :int] :return :int}]}
   'floor   {:operator :floor
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float] :return :float}]}
   'ceil    {:operator :ceil
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float] :return :float}]}
   'trunc   {:operator :trunc
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float] :return :float}]}
   'sqrt    {:operator :sqrt
             :kind :primitive
             :effects #{}
             :signatures [{:params [:float] :return :float}]}
   'copysign {:operator :copysign
              :kind :primitive
              :effects #{}
              :signatures [{:params [:float :float] :return :float}]}
   'rem     {:operator :rem
             :kind :primitive
             :effects #{}
             :signatures [{:params [:int :int] :return :int}]}

   'mod     {:operator :mod
             :kind :derived
             :effects #{}
             :signatures [{:params [:int :int] :return :int}]
             ;; Floored modulo.  The adjustment only ever runs when `r` and `b`
             ;; have opposite signs, where `r + b` cannot overflow because
             ;; `|r| < |b|`.  The remainder itself is total and wrapping (D12).
             :expand (fn [[a b]]
                       (let [r (gensym "mod_remainder")]
                         (form 'let [r (form 'rem a b)]
                               (form 'if (form 'and
                                               (form 'not= r 0)
                                               (form 'not= (form '< r 0)
                                                     (form '< b 0)))
                                     (form '+ r b)
                                     r))))}

   'clamp   {:operator :clamp
             :kind :derived
             :effects #{}
             :signatures [{:params [:float :float :float] :return :float}]
             :expand (fn [[x lo hi]] (form 'min (form 'max x lo) hi))}

   'fract   {:operator :fract
             :kind :derived
             :effects #{}
             :signatures [{:params [:float] :return :float}]
             :expand (fn [[x]] (form '- x (form 'floor x)))}

   'sign    {:operator :sign
             :kind :derived
             :effects #{}
             :signatures [{:params [:float] :return :float}]
             ;; IEEE signum: +0/-0 for zero, +/-1 elsewhere, NaN for NaN.
             :expand (fn [[x]]
                       (form 'if (form '> x 0.0)
                             1.0
                             (form 'if (form '< x 0.0) -1.0 x)))}

   'round   {:operator :round
             :kind :derived
             :effects #{}
             :signatures [{:params [:float] :return :float}]
             ;; Half away from zero (D10).
             :expand (fn [[x]]
                       (form 'copysign
                             (form 'floor (form '+ (form 'abs x) 0.5))
                             x))}})

(def operators
  "Source symbols recognized as builtins."
  (set (keys builtins)))

(defn builtin?
  [symbol]
  (contains? builtins symbol))

(defn entry
  [symbol]
  (get builtins symbol))

(defn signatures
  [symbol-or-entry]
  (:signatures
   (cond
     (map? symbol-or-entry) symbol-or-entry
     (symbol? symbol-or-entry) (entry symbol-or-entry)
     (keyword? symbol-or-entry) (some #(when (= symbol-or-entry (:operator %))
                                         %)
                                      (vals builtins)))))

(defn arities
  [symbol-or-entry]
  (into #{} (map (comp count :params) (signatures symbol-or-entry))))

(defn accepts-arity?
  [symbol arity]
  (contains? (arities symbol) arity))

(defn resolve-signature
  "Return the signature overload matching `param-types`, or nil.

  `symbol-or-entry` may be a source symbol, a registry entry map, or an
  operator keyword."
  [symbol-or-entry param-types]
  (let [param-types (vec param-types)]
    (some #(when (= param-types (:params %)) %)
          (signatures symbol-or-entry))))

(defn infer-return
  "Return the result logical type applied to `param-types`, or nil."
  [symbol-or-entry param-types]
  (some-> (resolve-signature symbol-or-entry param-types) :return))
