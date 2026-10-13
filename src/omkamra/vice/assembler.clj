(ns omkamra.vice.assembler
  "Data-driven relocatable 6510 assembler.

  A top-level assembly source is quoted Clojure data. `assemble` normalizes it
  into canonical assembly data and wraps it in a relocatable object; `link`
  (later) resolves placement and relocations into an artifact. `define-asm`
  binds a named object and registers it for transitive dependency discovery.

  Normalized assembly data is plain Clojure forms: the DSL surface after macro
  expansion and symbol resolution. Labels are plain keywords; external labels
  are namespace-qualified keywords; memory operands are bracketed vectors."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [omkamra.vice.isa :as isa]
   [sci.core :as sci]))

;; Asset descriptor ---------------------------------------------------------
;;
;; `define-asset` (later) binds an `Asset` to a Var. Resolution only needs the
;; record type and predicate to tell an asset reference apart from an ordinary
;; constant.

(defrecord Asset [name data constraints resource])

(defn asset?
  "True when `x` is an asset descriptor."
  [x]
  (instance? Asset x))

(defn- normalize-asset-bytes
  [values]
  (byte-array
   (mapv (fn [value]
           (when-not (integer? value)
             (throw (ex-info "Asset bytes must be integers"
                             {:reason :invalid-asset-byte :value value})))
           (let [byte (long value)]
             (when-not (<= 0 byte 255)
               (throw (ex-info "Asset byte out of range"
                               {:reason :asset-byte-out-of-range
                                :value value})))
             (unchecked-byte byte)))
         values)))

(defn- read-asset-resource
  [asset path]
  (let [url (io/resource path)]
    (when-not url
      (throw (ex-info (str "Asset resource not found: " path)
                      {:reason :asset-resource-not-found
                       :asset asset :resource path})))
    (with-open [in (io/input-stream url)]
      (.readAllBytes in))))

(defn build-asset
  "Normalize an asset specification into an `Asset` descriptor.

  Assets are kept with the source and read as classpath resources, so a file
  is found whether it lives under `src/` or `resources/`. `spec` is either a
  sequence of byte-sized integers, or a map with `:data` or a
  classpath-root-relative `:resource`, plus optional `:constraints`."
  [asset spec]
  (cond
    (sequential? spec)
    (->Asset asset (normalize-asset-bytes spec) nil nil)

    (map? spec)
    (let [{:keys [data resource constraints]} spec]
      (cond
        (sequential? data)
        (->Asset asset (normalize-asset-bytes data) constraints nil)

        resource
        (->Asset asset (read-asset-resource asset resource) constraints resource)

        :else
        (throw (ex-info "An asset map requires :data bytes or a :resource"
                        {:reason :invalid-asset :asset asset :spec spec}))))

    :else
    (throw (ex-info "Invalid asset specification"
                    {:reason :invalid-asset :asset asset :spec spec}))))

(defn object?
  "True when `x` is a relocatable assembler object."
  [x]
  (and (map? x) (= :object (:kind x))))

(defn artifact?
  "True when `x` is a linked assembler artifact."
  [x]
  (and (map? x) (= :artifact (:kind x))))

;; Errors and source locations ----------------------------------------------

(defn- form-location
  [form index]
  (let [m (meta form)]
    (cond-> {:index index}
      (:file m) (assoc :file (:file m))
      (:line m) (assoc :line (:line m))
      (:column m) (assoc :column (:column m)))))

(defn- fail!
  [reason message data form index]
  (throw (ex-info message (merge {:reason reason
                                  :location (form-location form index)}
                                 data))))

;; Builtin recognition ------------------------------------------------------

(def ^:private directive-names
  #{"%entry" "%label" "%db" "%dw" "%align" "%region" "%org" "%meta"
    "%volatile" "%opcode"})

(defn- directive-symbol? [x]
  (and (symbol? x) (contains? directive-names (name x))))

(def ^:private known-mnemonics
  (into #{} (map (comp first key)) isa/mnemonic-mode-opcodes))

(defn parse-mnemonic
  "Parse a mnemonic symbol/string into `{:mnemonic \"LDA\" :forced-opcode nil}`.

  A dotted mnemonic such as `nop.3a` forces the exact opcode `$3A`."
  [x]
  (let [raw (if (symbol? x) (name x) (str x))
        dot (str/index-of raw ".")
        [base hex] (if dot
                     [(subs raw 0 dot) (subs raw (inc dot))]
                     [raw nil])]
    {:mnemonic (str/upper-case base)
     :dotted? (boolean dot)
     :forced-opcode (when hex
                      (try
                        (Integer/parseInt hex 16)
                        (catch NumberFormatException _
                          nil)))}))

(defn- valid-mnemonic? [{:keys [mnemonic dotted? forced-opcode]}]
  (and (contains? known-mnemonics mnemonic)
       (if dotted?
         (and forced-opcode (isa/valid-opcode? mnemonic forced-opcode))
         true)))

(defn- mnemonic-symbol? [x]
  (and (symbol? x) (valid-mnemonic? (parse-mnemonic x))))

(defn- register-symbol? [x]
  (contains? #{'x 'y} x))

;; Curated SCI environment ---------------------------------------------------

(defn- forbidden
  [sym]
  (fn [& _]
    (throw (ex-info (str sym " is not available in the assembly environment")
                    {:reason :sci-forbidden :symbol sym}))))

(def ^:private byte-helpers
  "Byte-construction helpers exposed to assembly expressions."
  {'u8 (fn [x] (bit-and (int x) 0xff))
   'lo (fn [x] (bit-and (int x) 0xff))
   'hi (fn [x] (bit-and (bit-shift-right (int x) 8) 0xff))
   'word (fn [low high]
           (bit-or (bit-and (int low) 0xff)
                   (bit-shift-left (bit-and (int high) 0xff) 8)))
   'bytes (fn [& xs] (mapv (fn [x] (bit-and (int x) 0xff)) xs))})

(def ^:private blocked-core
  "SCI escape hatches replaced by throwing stubs: the assembly expression
  environment must not be able to load or evaluate arbitrary code."
  (into {}
        (map (fn [sym] [sym (forbidden sym)]))
        '[eval require load-string load-file intern create-ns add-namespace!
          add-class! add-import! enable-unrestricted-access! alter-var-root]))

(defn- namespace-bindings
  [ns-obj]
  (into {} (map (fn [[name v]] [name (deref v)])) (ns-publics ns-obj)))

(defn- make-sci-context
  "Build the curated SCI context used to evaluate assembly expressions.

  The captured namespace's publics and its aliases' publics are exposed as
  explicitly granted trusted capabilities; JVM interop and unrelated
  namespaces remain unavailable."
  [ns-obj]
  (try
    (let [locals (when ns-obj (namespace-bindings ns-obj))
          core (merge byte-helpers blocked-core locals)
          aliases (when ns-obj
                    (into {}
                          (map (fn [[alias target]]
                                 [alias (namespace-bindings target)]))
                          (ns-aliases ns-obj)))
          namespaces (cond-> {'clojure.core core}
                       ns-obj (assoc (ns-name ns-obj) locals)
                       (seq aliases) (merge aliases))]
      (sci/init {:namespaces namespaces}))
    (catch Throwable _ nil)))

;; Context and symbol resolution --------------------------------------------

(defn- make-context [options]
  (let [ns-arg (:ns options)
        ns-obj (cond
                 (instance? clojure.lang.Namespace ns-arg) ns-arg
                 (symbol? ns-arg) (or (find-ns ns-arg)
                                      (fail! :unknown-namespace
                                             (str "Unknown namespace " ns-arg)
                                             {:namespace ns-arg}
                                             ns-arg -1))
                 (nil? ns-arg) nil
                 :else (fail! :invalid-namespace
                              ":ns must be a namespace or namespace symbol"
                              {:namespace ns-arg}
                              ns-arg -1))]
    {:namespace ns-obj
     :namespace-symbol (when ns-obj (ns-name ns-obj))
     :sci (make-sci-context ns-obj)
     :options options}))

(defn- resolve-var [ctx sym]
  (when-let [ns (:namespace ctx)]
    (ns-resolve ns sym)))

(defn- qualify-var [v]
  (let [m (meta v)]
    (symbol (str (ns-name (:ns m))) (str (:name m)))))

(defn- eval-expression
  [ctx form index]
  (let [sci-ctx (:sci ctx)]
    (if (nil? sci-ctx)
      (fail! :expression-error
             "No namespace context is available for expression evaluation"
             {:form form} form index)
      (try
        (sci/eval-form sci-ctx form)
        (catch Throwable error
          (fail! :expression-error
                 (str "Failed to evaluate assembly expression: "
                      (ex-message error))
                 {:form form :cause (ex-message error)} form index))))))

;; Source coercion -----------------------------------------------------------

(defn- builtin-head? [x]
  (and (symbol? x) (or (directive-symbol? x) (mnemonic-symbol? x))))

(defn- coerce-source [source]
  (cond
    (nil? source) []
    (vector? source) source
    (seq? source) (if (builtin-head? (first source))
                    [source]
                    (vec source))
    :else (fail! :invalid-source
                 "Assembly source must be a sequence of forms"
                 {:source source} source -1)))

;; Operand normalization -----------------------------------------------------

(declare normalize-operand normalize-statement)

(defn- normalize-memory
  [ctx contents index]
  (mapv (fn [element]
          (cond
            (= element '%zp) '%zp
            (register-symbol? element) element
            (vector? element) (normalize-memory ctx element index)
            :else (normalize-operand ctx element index)))
        contents))

(defn- parse-meta-args
  [args form index]
  (cond
    (and (= 2 (count args)) (map? (first args)))
    [(first args) (second args)]

    (and (odd? (count args)) (>= (count args) 3))
    [(reduce (fn [m [k v]] (assoc m k v))
             {}
             (partition 2 (butlast args)))
     (last args)]

    :else
    (fail! :invalid-meta
           "(%meta ...) needs a map and a value, or key/value pairs and a value"
           {:form form} form index)))

(defn- normalize-annotated
  [ctx target index]
  (if (and (seq? target) (builtin-head? (first target)))
    (first (normalize-statement ctx target index))
    (normalize-operand ctx target index)))

(defn- normalize-operand-list
  [ctx form index]
  (let [[head & args] form]
    (cond
      (and (symbol? head) (contains? #{"<" ">"} (name head)))
      (if (= 1 (count args))
        (list head (normalize-operand ctx (first args) index))
        ;; Two or more operands is an ordinary SCI comparison.
        (eval-expression ctx form index))

      (= head '%volatile)
      (do
        (when-not (= 1 (count args))
          (fail! :invalid-volatile
                 "(%volatile ...) takes exactly one value" {:form form} form index))
        (list '%volatile (normalize-operand ctx (first args) index)))

      (= head '%meta)
      (let [[m target] (parse-meta-args args form index)]
        (list '%meta m (normalize-annotated ctx target index)))

      :else
      (eval-expression ctx form index))))

(defn- normalize-operand-symbol
  [ctx sym index]
  (let [n (name sym)]
    (cond
      (= sym 'a) 'a
      (register-symbol? sym) sym

      (and (or (str/starts-with? n "<") (str/starts-with? n ">"))
           (> (count n) 1))
      (let [op (symbol (subs n 0 1))
            suffix (subs n 1)]
        (if (str/starts-with? suffix ":")
          (list op (keyword (subs suffix 1)))
          (list op (normalize-operand-symbol ctx (symbol suffix) index))))

      :else
      (let [v (resolve-var ctx sym)]
        (cond
          (nil? v)
          (fail! :unresolved-symbol
                 (str "Unable to resolve assembly symbol " sym)
                 {:symbol sym} sym index)

          (asset? (deref v))
          (qualify-var v)

          (fn? (deref v))
          (fail! :function-as-operand
                 (str "Function " sym " cannot be used as an assembly operand")
                 {:symbol sym} sym index)

          :else (deref v))))))

(defn- normalize-operand
  [ctx x index]
  (cond
    (nil? x) nil
    (vector? x) (normalize-memory ctx x index)
    (seq? x) (normalize-operand-list ctx x index)
    (symbol? x) (normalize-operand-symbol ctx x index)
    (keyword? x) x
    (number? x) x
    (string? x) x
    (map? x) x
    :else x))

;; Statement normalization ---------------------------------------------------

(defn- normalize-macro
  [ctx form index]
  (let [head (first form)
        v (resolve-var ctx head)]
    (cond
      (nil? v)
      (fail! :unresolved-symbol
             (str "Unable to resolve assembly macro " head)
             {:symbol head} form index)

      (not (fn? (deref v)))
      (fail! :not-a-macro
             (str head " does not resolve to an assembly macro")
             {:symbol head} form index)

      :else
      (let [result (apply (deref v) (rest form))]
        (cond
          (nil? result) []
          (vector? result) (mapcat #(normalize-statement ctx % index) result)
          :else (normalize-statement ctx result index))))))

(defn- with-form-meta
  [result form]
  (if (instance? clojure.lang.IObj result)
    (with-meta result (meta form))
    result))

(defn- normalize-directive*
  [ctx form index]
  (let [[head & args] form]
    (case (name head)
      "%entry"
      (let [target (first args)]
        (when-not (and (keyword? target) (nil? (namespace target)))
          (fail! :invalid-entry
                 "(%entry ...) expects a local label keyword"
                 {:form form} form index))
        [(list '%entry target)])

      "%label"
      (let [target (first args)]
        (when-not (and (keyword? target) (nil? (namespace target)))
          (fail! :invalid-label
                 "(%label ...) expects a local label keyword"
                 {:form form} form index))
        [target])

      "%db"
      (let [values (if (and (= 1 (count args)) (vector? (first args)))
                     (first args)
                     args)]
        [(list '%db (mapv #(normalize-operand ctx % index) values))])

      "%dw"
      (let [values (if (and (= 1 (count args)) (vector? (first args)))
                     (first args)
                     args)]
        [(list '%dw (mapv #(normalize-operand ctx % index) values))])

      "%align"
      [(list '%align (normalize-operand ctx (first args) index))]

      "%region"
      [(list '%region (normalize-operand ctx (first args) index))]

      "%org"
      [(list '%org (normalize-operand ctx (first args) index))]

      "%opcode"
      [(list '%opcode (normalize-operand ctx (first args) index))]

      "%meta"
      (let [[m target] (parse-meta-args args form index)]
        [(list '%meta m (normalize-annotated ctx target index))])

      "%volatile"
      [(list '%volatile (normalize-operand ctx (first args) index))])))

(defn- normalize-directive
  [ctx form index]
  (mapv #(with-form-meta % form) (normalize-directive* ctx form index)))

(defn- normalize-instruction
  [ctx form index]
  (let [[head & operands] form]
    (when (> (count operands) 1)
      (fail! :invalid-instruction
             (str head " takes at most one operand")
             {:form form} form index))
    (with-form-meta
      (if (seq operands)
        (list head (normalize-operand ctx (first operands) index))
        (list head))
      form)))

(defn- normalize-statement
  [ctx form index]
  (cond
    (keyword? form)
    (if (namespace form)
      (fail! :invalid-label
             "A top-level label must be an unqualified keyword"
             {:form form} form index)
      [form])

    (symbol? form)
    (normalize-macro ctx (list form) index)

    (seq? form)
    (let [head (first form)]
      (cond
        (and (symbol? head) (directive-symbol? head))
        (normalize-directive ctx form index)

        (and (symbol? head) (mnemonic-symbol? head))
        [(normalize-instruction ctx form index)]

        (symbol? head)
        (normalize-macro ctx form index)

        :else
        (fail! :invalid-statement
               "Unrecognized assembly statement"
               {:form form} form index)))

    (nil? form) []

    :else
    (fail! :invalid-statement
           "Unrecognized assembly statement"
           {:form form} form index)))

(defn- normalize-source
  [ctx source]
  (let [index (volatile! -1)]
    (into []
          (mapcat (fn [form]
                    (vswap! index inc)
                    (normalize-statement ctx form @index)))
          (coerce-source source))))

;; Mode inference ------------------------------------------------------------

(defn- strip-annotations
  "Remove transparent `%meta`/`%volatile` wrappers from an operand."
  [x]
  (cond
    (and (seq? x) (= '%volatile (first x))) (recur (second x))
    (and (seq? x) (= '%meta (first x))) (recur (nth x 2 nil))
    :else x))

(defn- operand-small-address? [value]
  (and (integer? value) (<= 0 (long value) 0xff)))

(defn- memory-shape
  "Decompose a bracketed memory operand into force-zp, nested, index, value."
  [contents]
  (let [force-zp? (boolean (some (fn [element] (= element '%zp)) contents))
        elements (remove (fn [element] (= element '%zp)) contents)
        nested (first (filter vector? elements))
        index-sym (first (filter register-symbol? elements))
        index (when index-sym (keyword (name index-sym)))
        value (first (remove #(or (vector? %) (register-symbol? %)) elements))]
    {:force-zp? force-zp?
     :nested nested
     :index index
     :value value}))

(defn- supported!
  [modes mode form]
  (when-not (contains? modes mode)
    (fail! :unsupported-addressing-mode
           (str "Addressing mode " (name mode) " is not supported here")
           {:mode mode :form form} form -1))
  mode)

(defn- infer-memory-mode
  [mnemonic modes contents form]
  (let [{:keys [force-zp? nested index value]} (memory-shape contents)]
    (cond
      nested
      (let [{inner-index :index} (memory-shape nested)
            mode (cond
                   (and (= :x inner-index) (nil? index)) :indx
                   (and (nil? inner-index) (= :y index)) :indy)]
        (supported! modes (or mode :indx) form))

      index
      (let [zero-page? (or force-zp? (operand-small-address? value))
            mode (case [index zero-page?]
                   [:x true] :zpx
                   [:x false] :absx
                   [:y true] :zpy
                   [:y false] :absy)]
        (supported! modes mode form))

      :else
      (cond
        force-zp? (supported! modes :zp form)
        (and (operand-small-address? value) (contains? modes :zp))
        (supported! modes :zp form)
        (contains? modes :ind) (supported! modes :ind form)
        (contains? modes :abs) (supported! modes :abs form)
        (contains? modes :zp) (supported! modes :zp form)
        :else (fail! :unsupported-addressing-mode
                     (str mnemonic " does not support a memory operand")
                     {:mnemonic mnemonic :form form} form -1)))))

(defn infer-mode
  "Infer the addressing mode for a mnemonic and normalized operand.

  `mnemonic` is a symbol or string, possibly dotted (`nop.3a`). Throws when
  the mnemonic does not support the shape of `operand`."
  [mnemonic operand]
  (let [operand (strip-annotations operand)
        {:keys [mnemonic forced-opcode]} (parse-mnemonic mnemonic)
        modes (isa/modes-for mnemonic)]
    (cond
      forced-opcode (aget ^objects isa/mode-by-opcode forced-opcode)

      (nil? operand) (supported! modes :imp mnemonic)

      (= operand 'a) (supported! modes :acc mnemonic)

      (vector? operand) (infer-memory-mode mnemonic modes operand mnemonic)

      :else
      (cond
        (contains? modes :imm) :imm
        (contains? modes :rel) :rel
        (contains? modes :abs) :abs
        :else (fail! :unsupported-addressing-mode
                     (str mnemonic " does not support an immediate operand")
                     {:mnemonic mnemonic} mnemonic -1)))))

;; Analysis of normalized data ----------------------------------------------

(defn- statement-head [form]
  (when (seq? form) (first form)))

(defn- defined-labels [forms]
  (into [] (filter #(and (keyword? %) (nil? (namespace %)))) forms))

(defn- entry-declarations [forms]
  (into []
        (keep (fn [form]
                (when (= '%entry (statement-head form))
                  (second form))))
        forms))

(defn- collect-operand-refs [acc x index form]
  (cond
    (keyword? x)
    (if (namespace x)
      (update acc :external conj x)
      (update acc :labels (fn [m] (if (contains? m x) m (assoc m x [index form])))))

    (and (symbol? x) (namespace x))
    (update acc :assets conj x)

    (vector? x)
    (reduce (fn [a e] (collect-operand-refs a e index form)) acc x)

    (seq? x)
    (case (name (first x))
      ("<" ">" "%volatile") (collect-operand-refs acc (second x) index form)
      "%meta" (collect-operand-refs acc (nth x 2 nil) index form)
      ;; Remaining lists are unevaluated expressions; walk their arguments.
      (reduce (fn [a e] (collect-operand-refs a e index form)) acc (rest x)))

    :else acc))

(defn- collect-refs [forms]
  (reduce
   (fn [acc [index form]]
     (let [head (statement-head form)]
       (cond
         (nil? head) acc
         (= '%entry head)
         (let [label (second form)]
           (if (namespace label)
             (update acc :external conj label)
             (update acc :labels assoc label [index form])))
         (contains? #{"%db" "%dw"} (name head))
         (reduce (fn [a e] (collect-operand-refs a e index form))
                 acc (second form))
         (= '%meta head) (collect-operand-refs acc (nth form 2 nil) index form)
         (contains? #{"%align" "%region" "%org" "%opcode"} (name head)) acc
         ;; instruction form, or a bare label keyword
         :else (collect-operand-refs acc (second form) index form))))
   {:labels {} :external #{} :assets #{}}
   (map-indexed vector forms)))

;; Code generation -----------------------------------------------------------

(defn- lcm
  ^long [^long a ^long b]
  (if (or (zero? a) (zero? b))
    0
    (/ (* a b) (long (.gcd (biginteger a) (biginteger b))))))

(defn- value->target
  [value form index]
  (cond
    (integer? value) {:type :constant :value (long value)}
    (and (keyword? value) (nil? (namespace value))) {:type :label :name value}
    (keyword? value) {:type :external :name value}
    (and (symbol? value) (namespace value)) {:type :asset :name value}
    :else (fail! :invalid-operand
                 (str "Cannot use " (pr-str value) " as an operand")
                 {:value value} form index)))

(defn- unwrap-volatile
  [value]
  (cond
    (and (seq? value) (= '%meta (first value)))
    (recur (nth value 2 nil))

    (and (seq? value) (= '%volatile (first value)))
    [true (second value)]

    :else [false value]))

(defn- classify-value
  [value form index]
  (if (and (seq? value)
           (symbol? (first value))
           (contains? #{"<" ">"} (name (first value)))
           (= 2 (count value)))
    {:modifier (if (= "<" (name (first value))) :low :high)
     :target (value->target (second value) form index)}
    {:modifier nil
     :target (value->target value form index)}))

(defn- little-endian
  [value width]
  (mapv (fn [i] (bit-and (bit-shift-right (long value) (* 8 (long i))) 0xff))
        (range width)))

(defn- check-constant-range
  [value width form index]
  (let [value (long value)
        max (dec (bit-shift-left 1 (* 8 (long width))))]
    (when-not (<= 0 value max)
      (fail! :operand-out-of-range
             (str "Value " value " does not fit in " width " byte(s)")
             {:value value :width width} form index))
    value))

(defn- operand-value
  "The address/value wrapped by a normalized memory operand vector."
  [mode operand]
  (if (contains? isa/memory-modes mode)
    (let [{:keys [value nested]} (memory-shape operand)]
      (or value (:value (memory-shape nested))))
    operand))

(defn- encode-value
  [mode operand form index]
  (let [width (long (isa/mode-operand-width mode))
        [volatile? raw] (unwrap-volatile operand)
        value (if (and (contains? isa/memory-modes mode) (vector? raw))
                (operand-value mode raw)
                raw)]
    (cond
      (zero? width)
      {:bytes [] :volatile? volatile?}

      (= mode :rel)
      (let [{:keys [modifier target]} (classify-value value form index)]
        (when modifier
          (fail! :invalid-relative
                 "A relative branch cannot use a < or > relocation"
                 {:value value} form index))
        {:bytes [0]
         :volatile? volatile?
         :relocation {:kind :relative :width 1 :target target}})

      :else
      (let [{:keys [modifier target]} (classify-value value form index)]
        (cond
          modifier
          {:bytes [0]
           :volatile? volatile?
           :relocation {:kind modifier :width 1 :target target}}

          (= :constant (:type target))
          {:bytes (little-endian
                   (check-constant-range (:value target) width form index)
                   width)
           :volatile? volatile?}

          :else
          {:bytes (vec (repeat width 0))
           :volatile? volatile?
           :relocation {:kind :address :width width :target target}})))))

(defn- instruction-item
  [form index meta]
  (let [[head & operands] form
        operand (first operands)
        {:keys [mnemonic forced-opcode]} (parse-mnemonic head)
        mode (infer-mode head operand)
        opcode (or forced-opcode (isa/preferred-opcode mnemonic mode))]
    (when (nil? opcode)
      (fail! :unsupported-instruction
             (str "No opcode for " mnemonic " " (name mode))
             {:mnemonic mnemonic :mode mode} form index))
    (let [encoded (encode-value mode operand form index)
          [_ raw] (unwrap-volatile operand)
          operand-value (when (contains? isa/memory-modes mode)
                          (if (vector? raw)
                            (operand-value mode raw)
                            raw))
          operand-address (when (integer? operand-value)
                            (long operand-value))]
      {:type :instruction
       :mnemonic mnemonic
       :mode mode
       :opcode opcode
       :operand operand
       :operand-address operand-address
       :volatile? (:volatile? encoded)
       :bytes (into [opcode] (:bytes encoded))
       :size (inc (count (:bytes encoded)))
       :meta meta
       :relocations (when-let [relocation (:relocation encoded)]
                      [(assoc relocation :in-item 1)])})))

(defn- db-item
  [values form index meta]
  (let [{:keys [bytes relocations]}
        (reduce (fn [{:keys [bytes] :as acc} value]
                  (let [[volatile? v] (unwrap-volatile value)
                        {:keys [modifier target]} (classify-value v form index)]
                    (if (and (nil? modifier) (= :constant (:type target)))
                      (update acc :bytes conj
                              (check-constant-range (:value target) 1 form index))
                      (-> acc
                          (update :bytes conj 0)
                          (update :relocations conj
                                  {:in-item (count bytes)
                                   :kind (or modifier :address)
                                   :width 1
                                   :target target
                                   :volatile? volatile?})))))
                {:bytes [] :relocations []}
                values)]
    {:type :bytes
     :bytes bytes
     :size (count bytes)
     :meta meta
     :relocations relocations}))

(defn- dw-item
  [values form index meta]
  (let [{:keys [bytes relocations]}
        (reduce (fn [acc value]
                  (let [acc-bytes (:bytes acc)
                        [volatile? v] (unwrap-volatile value)
                        {:keys [modifier target]} (classify-value v form index)]
                    (if (and (nil? modifier) (= :constant (:type target)))
                      (update acc :bytes into
                              (little-endian
                               (check-constant-range (:value target) 2 form index)
                               2))
                      (-> acc
                          (update :bytes into [0 0])
                          (update :relocations conj
                                  {:in-item (count acc-bytes)
                                   :kind (or modifier :address)
                                   :width (if modifier 1 2)
                                   :target target
                                   :volatile? volatile?})))))
                {:bytes [] :relocations []}
                values)]
    {:type :words
     :bytes bytes
     :size (count bytes)
     :meta meta
     :relocations relocations}))

(defn- opcode-item
  [value form index meta]
  (let [byte (check-constant-range value 1 form index)]
    {:type :bytes :bytes [byte] :size 1 :meta meta :relocations []}))

;; Section builder -----------------------------------------------------------

(defn- new-section
  ([] (new-section :object))
  ([base] {:base base :alignment 1 :size 0 :items [] :labels {} :relocations []}))

(defn- empty-state []
  {:sections [(new-section)]
   :constraints {}})

(defn- current-section [state] (peek (:sections state)))

(defn- append-item [state item]
  (let [section (current-section state)
        offset (:size section)
        relocations (mapv (fn [relocation]
                            (-> relocation
                                (assoc :offset (+ offset (:in-item relocation)))
                                (dissoc :in-item)))
                          (:relocations item))
        section (-> section
                    (update :size + (:size item))
                    (update :items conj (assoc item :offset offset))
                    (update :relocations into relocations))]
    (assoc-in state [:sections (dec (count (:sections state)))] section)))

(defn- add-label [state label]
  (let [section (current-section state)]
    (assoc-in state [:sections (dec (count (:sections state))) :labels]
              (assoc (:labels section) label (:size section)))))

(defn- pad-count [base offset alignment]
  (let [address (if (integer? base) (+ (long base) (long offset)) (long offset))
        remainder (mod address (long alignment))]
    (if (zero? remainder) 0 (- (long alignment) remainder))))

(defn- apply-align [state alignment form index]
  (when-not (and (integer? alignment) (pos? (long alignment)))
    (fail! :invalid-alignment
           "(%align ...) expects a positive integer"
           {:alignment alignment} form index))
  (let [alignment (long alignment)
        section (current-section state)
        base (:base section)
        pad (pad-count base (:size section) alignment)
        section (if (integer? base)
                  section
                  (update section :alignment lcm alignment))
        state (assoc-in state [:sections (dec (count (:sections state)))] section)]
    (if (zero? pad)
      state
      (append-item state {:type :padding
                          :bytes (vec (repeat pad 0))
                          :size pad
                          :alignment alignment
                          :relocations []}))))

(defn- start-org [state address form index]
  (when-not (and (integer? address) (<= 0 (long address) 0xffff))
    (fail! :invalid-org
           "(%org ...) expects a 16-bit address"
           {:address address} form index))
  (let [section (current-section state)
        empty? (and (= :object (:base section))
                    (zero? (:size section))
                    (empty? (:labels section)))]
    (if empty?
      (assoc-in state [:sections (dec (count (:sections state)))]
                (assoc section :base (long address)))
      (update state :sections conj (new-section (long address))))))

(defn- add-region [state value form index]
  (let [region (cond
                 (integer? value)
                 {:start (long value)}

                 (and (vector? value) (= 2 (count value))
                      (every? integer? value))
                 {:start (long (first value)) :end (long (second value))}

                 :else
                 (fail! :invalid-region
                        "(%region ...) expects an address or [start end]"
                        {:value value} form index))]
    (update state :constraints update :regions (fnil conj []) region)))

(defn- build-form
  [state form meta]
  (let [head (statement-head form)]
    (cond
      (and (keyword? form) (nil? (namespace form))) (add-label state form)
      (nil? head) state

      (= '%meta head)
      (let [[_ m target] form]
        (build-form state target (if (seq m) (merge meta m) meta)))

      (= '%entry head) state
      (= '%org head) (start-org state (second form) form -1)
      (= '%align head) (apply-align state (second form) form -1)
      (= '%region head) (add-region state (second form) form -1)
      (= '%db head) (append-item state (db-item (second form) form -1 meta))
      (= '%dw head) (append-item state (dw-item (second form) form -1 meta))
      (= '%opcode head) (append-item state (opcode-item (second form) form -1 meta))
      (= '%volatile head) (build-form state (second form) meta)

      :else (append-item state (instruction-item form -1 meta)))))

(defn- build-program [forms]
  (reduce (fn [state form] (build-form state form nil)) (empty-state) forms))

(defn- finalize-sections [sections]
  (let [kept (->> sections
                  (filter (fn [section]
                            (or (pos? (long (:size section)))
                                (seq (:labels section))
                                (seq (:relocations section)))))
                  vec)]
    (mapv (fn [index section]
            (-> section
                (assoc :index index)
                (update :relocations
                        (fn [relocations]
                          (mapv #(assoc % :section index) relocations)))))
          (range)
          kept)))

;; Serialization -------------------------------------------------------------

(declare render-form)

(defn- render-map [m]
  (let [entries (sort-by (fn [[k _]] (pr-str k)) m)]
    (str "{"
         (str/join ", "
                   (map (fn [[k v]] (str (render-form k) " " (render-form v)))
                        entries))
         "}")))

(defn- render-form
  [x]
  (cond
    (map? x) (render-map x)
    (vector? x) (str "[" (str/join " " (map render-form x)) "]")
    (seq? x) (str "(" (str/join " " (map render-form x)) ")")
    :else (pr-str x)))

(defn- source-forms [x]
  (cond
    (object? x) (:source x)
    (nil? x) []
    (sequential? x) (vec x)
    :else (throw (ex-info "Cannot serialize assembly data"
                          {:reason :not-assembly-data :value x}))))

(defn serialize-clj
  "Render normalized assembly data or an object as canonical Clojure text.

  The result is a single vector form, so it can be read back with
  `read-string` and passed to `assemble`. When `x` is an object, its
  `:form-comments` metadata is printed as `;;` remark lines before the forms
  they describe."
  [x]
  (let [forms (source-forms x)
        comments (when (object? x) (:form-comments x))]
    (if (seq forms)
      (str "["
           (str/join "\n "
                     (map-indexed
                      (fn [index form]
                        (str (when-let [comment (nth comments index nil)]
                               (str ";; " comment "\n "))
                             (render-form form)))
                      forms))
           "]\n")
      "[]\n")))

;; Object model and assembly ------------------------------------------------

(defonce ^{:doc "Named objects registered by `define-asm`, keyed by object id."}
  object-registry
  (atom {}))

(defn register-object!
  "Register a named object for transitive dependency discovery."
  [obj]
  (when-let [id (:object-id obj)]
    (swap! object-registry assoc id obj))
  obj)

(defn registered-objects
  "Return every registered named object."
  []
  (vals @object-registry))

(defn reset-registry!
  "Forget every registered object. Intended for tests and REPL reloads."
  []
  (reset! object-registry {})
  nil)

(defn- duplicate-labels [labels]
  (->> labels
       frequencies
       (keep (fn [[label n]] (when (> (long n) 1) label)))
       vec))

(defn assemble
  "Assemble quoted source data into a relocatable object.

  Options:

  - `:ns` — the Clojure namespace (object or symbol) used to resolve symbols,
    aliases, macros, and asset references;
  - `:entry` — an explicit entry label overriding `%entry`;
  - `:object-id` — a namespaced keyword identity for the produced object."
  ([source]
   (assemble source {}))
  ([source options]
   (let [ctx (make-context options)
         forms (normalize-source ctx source)
         labels (defined-labels forms)
         duplicates (duplicate-labels labels)
         _ (when (seq duplicates)
             (fail! :duplicate-label
                    (str "Duplicate label(s): " (str/join ", " duplicates))
                    {:labels duplicates} source -1))
         refs (collect-refs forms)
         unresolved (remove (set labels) (keys (:labels refs)))
         _ (when (seq unresolved)
             (let [label (first (sort unresolved))
                   [index form] (get (:labels refs) label)]
               (fail! :unresolved-label
                      (str "Unresolved label(s): "
                           (str/join ", " (sort unresolved)))
                      {:labels (vec (sort unresolved))}
                      (or form source) index)))
         entries (entry-declarations forms)
         entry (or (:entry options)
                   (when (= 1 (count entries)) (first entries)))
         program (build-program forms)
         sections (finalize-sections (:sections program))
         label-map (into {}
                         (for [section sections
                               [label offset] (:labels section)]
                           [label {:section (:index section) :offset offset}]))
         relocations (vec (mapcat :relocations sections))]
     {:kind :object
      :object-id (:object-id options)
      :namespace (:namespace-symbol ctx)
      :source forms
      :sections sections
      :labels label-map
      :relocations relocations
      :constraints (:constraints program)
      :entries (vec entries)
      :entry entry
      :dependencies {:objects (vec (sort (:external refs)))
                     :assets (vec (sort (:assets refs)))}
      :options (dissoc options :ns)})))

;; Definition macros ---------------------------------------------------------

(defmacro define-asm
  "Assemble `body` immediately into a relocatable object.

  Binds the object to `name`, and assigns it a namespaced-keyword `:object-id`
  derived from the fully qualified Var name."
  [name & body]
  (let [ns-name (str *ns*)
        object-id (keyword ns-name (str name))]
    `(do
       (def ~name
         (assemble '~body {:ns '~(symbol ns-name)
                           :object-id ~object-id}))
       (register-object! ~name)
       ~name)))

(defmacro define-asset
  "Bind an asset descriptor to `name`.

  `spec` is a sequence of byte-sized integers, or a map with inline `:data`
  bytes or a classpath-root-relative `:resource`, plus optional
  `:constraints`. Assets are kept with the source and read as classpath
  resources."
  [name spec]
  (let [ns-name (str *ns*)
        qualified (symbol ns-name (str name))]
    `(do
       (def ~name (build-asset '~qualified ~spec))
       ~name)))

;; Human-oriented assembly rendering -----------------------------------------

(defn- asm-value-text
  ([value width] (asm-value-text value width {}))
  ([value width symbols]
   (cond
     (integer? value)
     (or (get symbols (long value))
         (if (= (long width) 1)
           (format "$%02X" (bit-and (long value) 0xff))
           (format "$%04X" (bit-and (long value) 0xffff))))

     (keyword? value)
     (subs (str value) 1)

     (symbol? value)
     (str value)

     (and (seq? value) (symbol? (first value))
          (contains? #{"<" ">"} (name (first value))))
     (str (first value) (asm-value-text (second value) width))

     (and (seq? value) (= '%volatile (first value)))
     "??"

     :else (pr-str value))))

(defn- asm-operand
  [{:keys [mode operand volatile?]} {:keys [symbols]}]
  (let [width (case mode
                (:imm :zp :zpx :zpy :indx :indy) 1
                2)
        [_ raw] (unwrap-volatile operand)
        value (if (and (contains? isa/memory-modes mode) (vector? raw))
                (operand-value mode raw)
                raw)
        text (if volatile?
               "??"
               (asm-value-text value width
                               (if (= mode :imm) {} symbols)))]
    (case mode
      :imp ""
      :acc "A"
      :imm (str "#" text)
      :rel text
      :zp text
      :zpx (str text ",X")
      :zpy (str text ",Y")
      :abs text
      :absx (str text ",X")
      :absy (str text ",Y")
      :ind (str "(" text ")")
      :indx (str "(" text ",X)")
      :indy (str "(" text "),Y"))))

(defn- asm-mnemonic
  [{:keys [mnemonic mode opcode]}]
  (if (= opcode (isa/preferred-opcode mnemonic mode))
    mnemonic
    (str mnemonic "." (format "%02x" (bit-and (long opcode) 0xff)))))

(defn- format-meta
  "Render an item's metadata as a comment, preferring human fields."
  [meta]
  (let [remark (:remark meta)
        note (:note meta)]
    (cond
      (and remark note) (str remark "  " note)
      remark remark
      note note
      :else (pr-str meta))))

(defn- item-label
  "The label operand of an instruction, if it has one."
  [{:keys [mode operand]}]
  (let [[_ raw] (unwrap-volatile operand)]
    (cond
      (and (contains? isa/memory-modes mode) (vector? raw))
      (some (fn [element] (when (keyword? element) element)) raw)

      (keyword? raw) raw
      :else nil)))

(defn- render-asm-item
  [address item {:keys [comments label-addresses] :as options}]
  (let [prefix (format "%04X  " (bit-and (long address) 0xffff))
        meta-comment (when (seq (:meta item)) (format-meta (:meta item)))
        operand-address (or (:operand-address item)
                            (when-let [label (item-label item)]
                              (get label-addresses label)))
        operand-comment (when operand-address
                          (get comments (long operand-address)))
        comment (cond
                  (and meta-comment operand-comment)
                  (str meta-comment "  " operand-comment)

                  meta-comment meta-comment
                  operand-comment operand-comment)
        body (case (:type item)
               :instruction
               (let [operand (asm-operand item options)
                     mnemonic (asm-mnemonic item)]
                 (if (str/blank? operand) mnemonic (str mnemonic " " operand)))

               :padding
               (str ";; align " (:alignment item) " (" (:size item)
                    " bytes of padding)")

               :bytes
               (str ".byte "
                    (str/join ", "
                              (map (fn [b] (format "$%02X" (bit-and (long b) 0xff)))
                                   (:bytes item))))

               :words
               (str ".word "
                    (str/join ", "
                              (map (fn [[lo hi]]
                                     (format "$%02X%02X"
                                             (bit-and (long hi) 0xff)
                                             (bit-and (long lo) 0xff)))
                                   (partition 2 (:bytes item)))))

               (pr-str (:type item)))]
    (str prefix body (when comment (str "  ; " comment)))))

(defn- render-section
  [section options]
  (let [base (:base section)
        labels (reduce-kv (fn [m label offset]
                            (update m offset (fnil conj []) (name label)))
                          {}
                          (:labels section))
        emit-labels (fn [offset]
                      (mapv (fn [label] (str label ":")) (get labels offset)))]
    (loop [items (:items section)
           lines []]
      (if-let [item (first items)]
        (let [offset (:offset item)
              address (if (integer? base) (+ (long base) (long offset)) offset)]
          (recur (rest items)
                 (into lines
                       (concat (emit-labels offset)
                               [(render-asm-item address item options)]))))
        (into lines (emit-labels (:size section)))))))

(defn serialize-asm
  "Render normalized assembly data or an object as conventional 6502 assembly.

  There is no round-trip guarantee. Volatile operands render as `??` and
  metadata renders as comments. Accepts an object or normalized forms.

  Options:

  - `:symbols` — a map from an absolute address to a name (for example the
    KERNAL jump table); memory operands render the name;
  - `:comments` — a map from an absolute address to a trailing comment (for
    example VIC/SID/CIA register descriptions)."
  ([x]
   (serialize-asm x {}))
  ([x options]
   (let [object (if (object? x) x (assemble x {}))
         label-addresses (into {}
                               (for [section (:sections object)
                                     :when (integer? (:base section))
                                     [label offset] (:labels section)]
                                 [label (+ (long (:base section)) (long offset))]))
         options {:symbols (or (:symbols options) (:symbols object) {})
                  :comments (or (:comments options) (:comments object) {})
                  :label-addresses label-addresses}
         section-lines (fn [section]
                         (let [base (:base section)]
                           (concat
                            (when (integer? base)
                              [(format "        .org $%04X"
                                       (bit-and (long base) 0xffff))])
                            (when-not (integer? base)
                              ["; unplaced object section"])
                            (render-section section options))))
         blocks (mapv section-lines (:sections object))
         lines (mapcat identity (interpose [""] blocks))]
     (str (str/join "\n"
                    (concat [(str "; " (or (:comment object)
                                           (:object-id object)
                                           "anonymous object"))]
                            lines))
          "\n"))))

;; Linker --------------------------------------------------------------------

(def default-placement-ranges
  "First-fit placement ranges used when an object has no `%region`."
  [[0x0800 0xa000]
   [0xc000 0xd000]
   [0xa000 0xc000]
   [0xe000 0x10000]])

(defn- link-fail!
  [reason message data]
  (throw (ex-info message (assoc data :reason reason))))

(defn- option-registry
  [options]
  (let [extra (:objects options)]
    (cond
      (map? extra) extra
      (sequential? extra)
      (into {} (keep (fn [object]
                       (when-let [id (:object-id object)] [id object])))
            extra)
      :else {})))

(defn- label-owner
  [registry namespace-symbol label]
  (filterv (fn [object]
             (and (= namespace-symbol (:namespace object))
                  (contains? (:labels object) label)))
           (vals registry)))

(defn- external-target
  [kw]
  {:namespace (symbol (namespace kw)) :label (keyword (name kw))})

(defn- discover-objects
  "Breadth-first discovery of the primary object and its external dependencies."
  [primary options]
  (let [registry (merge @object-registry (option-registry options))
        registry (if-let [id (:object-id primary)]
                   (assoc registry id primary)
                   registry)]
    (loop [queue [primary]
           seen {}
           order []
           external-owners {}]
      (if-let [object (first queue)]
        (let [id (:object-id object)
              _ (when (and id
                           (contains? seen id)
                           (not= (get seen id) object))
                  (link-fail! :duplicate-object-id
                              (str "Inconsistent object id " id)
                              {:object-id id}))
              seen (if id (assoc seen id object) seen)
              order (conj order object)
              [owners owners-map]
              (reduce (fn [[owners owners-map] kw]
                        (let [{:keys [namespace label]} (external-target kw)
                              candidates (label-owner registry namespace label)]
                          (cond
                            (empty? candidates)
                            (link-fail! :unresolved-external
                                        (str "No object in " namespace
                                             " defines " label)
                                        {:label kw})

                            (> (count candidates) 1)
                            (link-fail! :ambiguous-external
                                        (str "Multiple objects in " namespace
                                             " define " label)
                                        {:label kw
                                         :objects (mapv :object-id candidates)})

                            :else
                            [(conj owners (first candidates))
                             (assoc owners-map kw (first candidates))])))
                      [[] external-owners]
                      (:objects (:dependencies object)))
              queued (set (map :object-id (concat order queue)))
              new (remove (fn [owner]
                            (and (:object-id owner)
                                 (contains? queued (:object-id owner))))
                          owners)]
          (recur (into (vec (rest queue)) new) seen order owners-map))
        {:objects order :external-owners external-owners}))))

(defn- resolve-asset-var
  [options sym]
  (or (get (:assets options) sym)
      (some-> (try
                (requiring-resolve sym)
                (catch Throwable _ nil))
              deref)))

(defn- discover-assets
  [objects options]
  (->> objects
       (mapcat #(get-in % [:dependencies :assets]))
       distinct
       (keep (fn [sym]
               (let [asset (resolve-asset-var options sym)]
                 (when-not (asset? asset)
                   (link-fail! :unresolved-asset
                               (str "Cannot resolve asset " sym)
                               {:asset sym}))
                 asset)))
       (reduce (fn [acc asset]
                 (if (some #(= (:name %) (:name asset)) acc)
                   acc
                   (conj acc asset)))
               [])
       vec))

;; Placement -----------------------------------------------------------------

(defn- round-up ^long [^long value ^long alignment]
  (let [remainder (mod value alignment)]
    (if (zero? remainder)
      value
      (+ value (- alignment remainder)))))

(defn- overlaps?
  [occupied start size]
  (some (fn [[occupied-start occupied-end]]
          (and (< (long start) (long occupied-end))
               (< (long occupied-start) (+ (long start) (long size)))))
        occupied))

(defn- first-fit
  [occupied alignment size ranges]
  (some (fn [[start end]]
          (loop [candidate (long start)]
            (let [candidate (round-up candidate (long alignment))]
              (cond
                (> (+ candidate (long size)) (long end)) nil
                (overlaps? occupied candidate size) (recur (inc candidate))
                :else candidate))))
        ranges))

(defn- section-bytes
  [section]
  (vec (mapcat :bytes (:items section))))

(defn- object-section-specs
  [object regions-override]
  (mapv (fn [section]
          {:kind :object
           :key [object (:index section)]
           :object object
           :section section
           :index (:index section)
           :base (:base section)
           :alignment (long (or (:alignment section) 1))
           :size (long (:size section))
           :bytes (section-bytes section)
           :regions (or regions-override
                        (:regions (:constraints object)))})
        (:sections object)))

(defn- asset-section-spec
  [asset]
  (let [data (:data asset)
        bytes (when data (vec data))]
    {:kind :asset
     :key (:name asset)
     :asset asset
     :base :object
     :alignment (long (or (get-in asset [:constraints :alignment]) 1))
     :size (count bytes)
     :bytes (or bytes [])
     :regions (:regions (:constraints asset))}))

(defn- place-section
  [occupied spec ranges]
  (let [alignment (long (:alignment spec))
        size (long (:size spec))]
    (if (integer? (:base spec))
      (let [base (long (:base spec))]
        (when-not (zero? (mod base alignment))
          (link-fail! :unaligned-org
                      (str "Fixed address " base " breaks alignment " alignment)
                      {:address base :alignment alignment}))
        (when (overlaps? occupied base size)
          (link-fail! :overlap
                      (str "Section at " base " overlaps an earlier section")
                      {:address base :size size}))
        [base (conj occupied [base (+ base size)])])

      (let [regions (:regions spec)
            exact (first (filter #(nil? (:end %)) regions))
            intervals (if (seq regions)
                        (mapv (fn [region]
                                [(long (:start region))
                                 (long (or (:end region)
                                           (+ (:start region) size)))])
                              regions)
                        ranges)]
        (if exact
          (let [base (long (:start exact))]
            (when-not (zero? (mod base alignment))
              (link-fail! :unaligned-region
                          (str "Region start " base " breaks alignment "
                               alignment)
                          {:address base :alignment alignment}))
            (when (overlaps? occupied base size)
              (link-fail! :overlap
                          (str "Section at " base " overlaps an earlier section")
                          {:address base :size size}))
            [base (conj occupied [base (+ base size)])])
          (if-let [base (first-fit occupied alignment size intervals)]
            [base (conj occupied [base (+ base size)])]
            (link-fail! :unsatisfiable-placement
                        "No free range satisfies an object's constraints"
                        {:alignment alignment
                         :size size
                         :ranges intervals})))))))

(defn- place-all
  [specs ranges]
  (loop [specs specs
         occupied []
         placement {}]
    (if-let [spec (first specs)]
      (let [[base occupied] (place-section occupied spec ranges)]
        (recur (rest specs) occupied (assoc placement (:key spec) base)))
      placement)))

;; Relocation resolution -----------------------------------------------------

(defn- label-addresses
  [object placement]
  (into {}
        (for [section (:sections object)
              [label offset] (:labels section)]
          [label (+ (long (get placement [object (:index section)]))
                    (long offset))])))

(defn- relocation-value
  [relocation object-labels external-owners placement asset-placement]
  (let [target (:target relocation)
        type (:type target)]
    (case type
      :constant (:value target)
      :label (get object-labels (:name target))
      :external
      (let [owner (get external-owners (:name target))]
        (get (label-addresses owner placement)
             (keyword (name (:name target)))))
      :asset (get asset-placement (:name target)))))

(defn- write-relocation!
  [bytes relocation address section-base]
  (let [offset (long (:offset relocation))
        site (+ (long section-base) offset)
        width (long (:width relocation))]
    (case (:kind relocation)
      :address
      (if (= width 2)
        (do
          (when-not (<= 0 (long address) 0xffff)
            (link-fail! :address-overflow
                        (str "Address " address " does not fit in 16 bits")
                        {:address address}))
          (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))
          (aset-byte bytes (inc offset)
                     (unchecked-byte (bit-and (bit-shift-right address 8) 0xff))))
        (do
          (when-not (<= 0 (long address) 0xff)
            (link-fail! :zero-page-overflow
                        (str "Address " address " is not zero-page")
                        {:address address}))
          (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))))

      :low
      (aset-byte bytes offset (unchecked-byte (bit-and address 0xff)))

      :high
      (aset-byte bytes offset
                 (unchecked-byte (bit-and (bit-shift-right address 8) 0xff)))

      :relative
      (let [displacement (- (long address) (inc site))]
        (when-not (<= -128 displacement 127)
          (link-fail! :branch-out-of-range
                      (str "Branch displacement " displacement
                           " is outside -128..127")
                      {:displacement displacement
                       :target address
                       :site site}))
        (aset-byte bytes offset (unchecked-byte (bit-and displacement 0xff)))))))

;; Segments ------------------------------------------------------------------

(defn- build-segments
  [placed-sections]
  (let [sorted (sort-by first placed-sections)
        merged (reduce (fn [acc [base bytes]]
                         (if-let [[previous-base previous-bytes] (peek acc)]
                           (if (= (+ (long previous-base) (count previous-bytes))
                                  (long base))
                             (conj (pop acc) [previous-base
                                              (into previous-bytes bytes)])
                             (conj acc [base bytes]))
                           (conj acc [base bytes])))
                       []
                       sorted)]
    (mapv (fn [[base bytes]]
            {:start-address (long base)
             :bytes (byte-array (map unchecked-byte bytes))})
          merged)))

;; Link ----------------------------------------------------------------------

(defn- entry-point
  [primary options label-addresses]
  (let [declarations (:entries primary)
        explicit (:entry options)]
    (when (and (nil? explicit) (> (count declarations) 1))
      (link-fail! :multiple-entries
                  "Object declares multiple entry points; select one"
                  {:entries declarations}))
    (let [label (or explicit (:entry primary))]
      (if (nil? label)
        nil
        (or (get label-addresses label)
            (link-fail! :unresolved-entry
                        (str "Entry label " label " is not defined")
                        {:entry label}))))))

(defn link
  "Link a primary object and its transitive dependencies into an artifact.

  Options:

  - `:entry` — select the entry label, overriding object `%entry`;
  - `:objects` — additional named objects (map or collection) for resolution;
  - `:assets` — asset descriptors (map of qualified symbol to asset);
  - `:regions` — override every object's `%region` constraints;
  - `:ranges` — override the default first-fit ranges.

  Passing an artifact returns it unchanged (linking is idempotent)."
  ([primary]
   (link primary {}))
  ([primary options]
   (if (artifact? primary)
     primary
     (do
       (when-not (object? primary)
         (link-fail! :not-an-object
                     "link expects a relocatable object"
                     {:value primary}))
       (let [{:keys [objects external-owners]} (discover-objects primary options)
             assets (discover-assets objects options)
             ranges (or (:ranges options) default-placement-ranges)
             object-specs (mapcat #(object-section-specs % (:regions options))
                                  objects)
             asset-specs (mapv asset-section-spec assets)
             placement (place-all (concat object-specs asset-specs) ranges)
             asset-placement (into {} (map (fn [asset] [(:name asset)
                                                        (get placement (:name asset))])
                                           assets))
           ;; Resolve relocations into mutable section byte arrays.
             resolved
             (mapv (fn [spec]
                     (let [base (long (get placement (:key spec)))
                           bytes (byte-array (map unchecked-byte (:bytes spec)))
                           labels (label-addresses (:object spec) placement)]
                       (doseq [relocation (:relocations (:section spec))]
                         (let [address (relocation-value relocation labels
                                                         external-owners placement
                                                         asset-placement)]
                           (when (nil? address)
                             (link-fail! :unresolved-relocation
                                         "Relocation target could not be resolved"
                                         {:relocation relocation}))
                           (write-relocation! bytes relocation address base)))
                       {:kind :object
                        :key (:key spec)
                        :base base
                        :object (:object spec)
                        :index (:index spec)
                        :bytes bytes}))
                   object-specs)
             resolved-assets
             (mapv (fn [spec]
                     (let [base (long (get placement (:key spec)))]
                       {:kind :asset
                        :key (:key spec)
                        :base base
                        :asset (:asset spec)
                        :bytes (byte-array (map unchecked-byte (:bytes spec)))}))
                   asset-specs)
             placed (concat resolved resolved-assets)
             primary-labels (label-addresses primary placement)
             entry (entry-point primary options primary-labels)
             segment-source (mapv (fn [{:keys [base bytes]}]
                                    [base (vec bytes)])
                                  placed)]
         {:kind :artifact
          :segments (build-segments segment-source)
          :entry-point entry
          :objects (mapv (fn [object]
                           {:object-id (:object-id object)
                            :namespace (:namespace object)
                            :entry (:entry object)
                            :labels (label-addresses object placement)
                            :placement {:sections
                                        (mapv (fn [section]
                                                {:index (:index section)
                                                 :base (get placement
                                                            [object (:index section)])
                                                 :size (:size section)})
                                              (:sections object))}})
                         objects)
          :assets (mapv (fn [asset]
                          {:name (:name asset)
                           :start-address (get asset-placement (:name asset))
                           :constraints (:constraints asset)})
                        assets)
          :constraints {:ranges ranges}})))))
