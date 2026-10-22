(ns omkamra.dsp.ir
  "Typed, target-neutral IR for omkamra.dsp.

  Source expressions lower to pure value nodes plus explicit statements. Buffer
  declarations live on process roots; memory operations refer to those buffers
  by ID. Effects are attached only to inherently effectful nodes and summarized
  on function/process roots."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [omkamra.dsp.builtins :as builtins]
            [omkamra.dsp.literals :as literals]
            [omkamra.dsp.names :as names]
            [omkamra.dsp.descriptor :as descriptor]))

(def ^:private arithmetic-operators
  {'+ :+ '- :- '* :* '/ :/})

(def ^:private comparison-operators
  {'= := 'not= :not= '< :< '<= :<= '> :> '>= :>=})

(def ^:private logical-operators
  {'and :and 'or :or 'not :not})

;; Integer bitwise operations mirror `clojure.core` names and 32-bit
;; two's-complement semantics. `:bit-shift-right` is arithmetic and
;; `:unsigned-bit-shift-right` is logical; shift distances are masked modulo
;; 32 by every target.
(def ^:private bitwise-operators
  {'bit-and :bit-and
   'bit-or :bit-or
   'bit-xor :bit-xor
   'bit-not :bit-not
   'bit-shift-left :bit-shift-left
   'bit-shift-right :bit-shift-right
   'unsigned-bit-shift-right :unsigned-bit-shift-right})

(def ^:private unary-bitwise-operators #{:bit-not})

;; The bitwise operators that `clojure.core` takes any number of arguments for.
;; The shifts are deliberately not here: `(bit-shift-left a b c)` has no
;; folding meaning, so it stays an arity error rather than becoming left-nested.
(def ^:private variadic-bitwise-operators #{:bit-and :bit-or :bit-xor})

(def ^:private value-types #{:float :int :boolean})
(def ^:private numeric-types #{:float :int})

(def effect-types
  #{:read-state :write-state :read-memory :write-memory :write-local :call})

(defn- fail
  [message data]
  (throw (ex-info message (assoc data :dsp/error :invalid-definition))))

(defn- valid-name?
  [x]
  (and (symbol? x) (not (str/blank? (name x)))))

(defn- environment
  ([bindings functions options]
   (environment bindings functions options {}))
  ([bindings functions options buffers]
   (environment bindings functions options buffers {}))
  ([bindings functions options buffers modules]
   {:bindings bindings
    :functions functions
    :options options
    :buffers buffers
    ;; Linked callees available for composition: id -> lowered function IR.
    :modules modules
    :loop-depth 0
    :locals (atom [])
    ;; Fixed-size local arrays ("scratch"): a distinct, preallocated instance
    ;; region whose logical offsets continue after persistent state, so every
    ;; backend can realize state and scratch in one arena.
    :scratch (atom {:next 0 :entries []})
    ;; Per-call-site state partitions (D19). `:call-offset` is the running
    ;; offset, seeded after this module's own state; `:state-more` and
    ;; `:records-more` collect the rebased callee slots and record metadata that
    ;; extend this module's arena.
    :call-offset (atom 0)
    :state-more (atom [])
    :records-more (atom [])
    ;; One entry per call site that owns a partition, in *evaluation* order (a
    ;; call's arguments are lowered before the call, D19). A target that holds
    ;; several call sites in one instance needs to know where each one's state
    ;; begins, or it cannot reset one without disturbing the others (M6a).
    :call-partitions (atom [])
    ;; The per-call-site regions a `with-buffers` form introduces (F2, D57):
    ;; caller region name -> the call partition state slot that backs it. A region
    ;; is *state*, not a buffer, so it is addressed like any other partition slot;
    ;; this cell exists because `with-buffers` runs during body lowering and a
    ;; later `buffer-load` of a region name must see the binding.
    :region-aliases (atom {})
    :counter (atom 0)}))

(defn- fresh-local!
  [env prefix type]
  (let [name (symbol (str prefix "__" (swap! (:counter env) inc)))
        local {:name name :type type}]
    (swap! (:locals env) conj local)
    local))

(defn- unique-local-name
  "The IR name for a source local.

  A local keeps its own name unless that name is already used elsewhere in the
  compilation unit — for example by a sibling `if` branch, which is a separate
  scope. The flat local list has to stay unambiguous, so a name reused in a
  sibling scope gets a fresh one rather than a collision error."
  [env name]
  (if (some #(= name (:name %)) @(:locals env))
    (symbol (str name "__" (swap! (:counter env) inc)))
    name))

(defn- fragment
  ([value] (fragment [] value))
  ([statements value]
   {:statements (vec statements)
    :value value
    :type (:type value)}))

(defn- void-fragment
  [statements]
  {:statements (vec statements) :value nil :type :void})

(defn- expect-type!
  [expected expression form]
  (when (= :array (:type expression))
    (fail "DSP array places must be indexed (aget/aset) or bound in a let"
          {:form form :type :array}))
  (when (= :record (:type expression))
    (fail "DSP record values must be projected with a field access"
          {:form form :type :record}))
  (when-not (= expected (:type expression))
    (fail "DSP expression has the wrong type"
          {:form form :expected expected :actual (:type expression)}))
  expression)

(defn- combine-fragments
  [fragments value]
  (fragment (mapcat :statements fragments) value))

(defn- constant
  [form]
  (cond
    (literals/float-literal? form)
    {:op :const :type :float :value (double (literals/float-literal-value form))}

    (integer? form) {:op :const :type :int :value (int form)}
    ;; Keep the full double. Narrowing happens per precision at use: the
    ;; interpreter's `:const` and each backend's constant emission. Narrowing
    ;; here would lose f64 precision and, because a Clojure `Float` prints a
    ;; short decimal, would not round-trip through the EDN served to the
    ;; browser.
    ;;
    ;; An unboxed number is typed by its value, which is what the browser can
    ;; see: one that is not an integer is a float there too (D78). A float whose
    ;; value *is* integral has to arrive boxed, or the browser would read it as
    ;; the integer it looks like.
    (number? form) {:op :const :type :float :value (double form)}
    (boolean? form) {:op :const :type :boolean :value form}
    :else nil))

(declare lower-value lower-form lower-forms array-form? array-declaration
         lower-place-store! reject-constant-store! rebase-slots rebase-records
         record-leaves lower-record-value
         process-buffers process-input? audio-channels-in annotate-buffer-channeling
         lane-frame-capacity)

(defn- frame-index
  "Lower a source buffer offset to an absolute per-frame index.

  Literal offsets retain the compact `:frame-offset` representation. Dynamic
  offsets are explicit typed expressions so the interpreter can check them at
  the access site and target backends can either lower or reject them honestly."
  [form offset env]
  (let [offset (lower-value offset env)]
    (expect-type! :int (:value offset) form)
    (let [value (:value offset)]
      (when (and (= :const (:op value)) (neg? (:value value)))
        (fail "DSP buffer offsets must not be negative"
              {:form form :offset (:value value)}))
      {:statements (:statements offset)
       :value (if (= :const (:op value))
                {:op :frame-offset :offset (:value value) :type :int}
                {:op :frame-plus :offset value :type :int})})))

(defn- channel-and-offset
  [form args]
  (case (count args)
    1 [0 (first args)]
    2 [(first args) (second args)]
    (fail "DSP buffer access requires an offset or channel and offset"
          {:form form})))

(defn- buffer-access!
  [form env buffer-id direction]
  (let [buffer (get-in env [:buffers buffer-id])]
    (when-not buffer
      (fail "DSP buffer access refers to an unknown process buffer"
            {:form form :buffer-id buffer-id
             :buffers (vec (sort (keys (:buffers env))))}))
    (when-not (contains? direction (:direction buffer))
      (fail "DSP buffer access is not permitted for this buffer direction"
            {:form form :buffer-id buffer-id :direction (:direction buffer)
             :required-direction direction}))
    buffer))

(defn- channel-expression
  [form channel env buffer]
  (let [channel (lower-value channel env)]
    (expect-type! :int (:value channel) form)
    (let [value (:value channel)]
      (when (and (= :const (:op value))
                 (or (neg? (:value value))
                     (>= (:value value) (:channels buffer))))
        (fail "DSP buffer channel is outside the declared channel range"
              {:form form :channel (:value value) :buffer-id (:id buffer)
               :channels (:channels buffer)}))
      {:statements (:statements channel)
       :value (if (= :const (:op value)) (:value value) value)})))

(defn- record-array-ref
  "The `:array-ref` for a scalar record's single array field, or nil.

  An array of records is not one array place: its rows are strided, so
  `(aget voices i k)` resolves through the record accessors instead."
  [binding]
  (when (= 1 (or (:size binding) 1))
    (let [buffers (filterv #(> (:size %) 1) (:leaves binding))]
      (when (= 1 (count buffers))
        (let [leaf (first buffers)]
          {:op :array-ref :type :array :region :state
           :state-id (:name binding)
           :base (:offset leaf) :element (:type leaf) :size (:size leaf)
           :constant (boolean (:constant binding))})))))

(defn- binding-expression
  [binding]
  (case (:kind binding)
    :state (if (> (or (:size binding) 1) 1)
             ;; A state array is an addressable place, like a record's buffer.
             {:op :array-ref :type :array :region :state
              :state-id (:name binding)
              :base (:index binding) :element (:type binding) :size (:size binding)
              :constant (boolean (:constant binding))}
             {:op :state-load
              :state-id (:name binding)
              :index (:index binding)
              :type (:type binding)
              :effects #{:read-state}})
    :buffer (:expression binding)
    :array {:op :array-ref :type :array :region :scratch
            :base (:index binding) :element (:type binding) :size (:size binding)
            :constant false}
    :place {:op :array-ref :type :array :region (:region binding)
            :state-id (:state-id binding)
            :base (:base binding) :element (:element binding) :size (:size binding)
            :constant (:constant binding)}
    :record-value {:op :local :name (:name binding) :type :record}
    :record (or (record-array-ref binding)
                (fail "DSP state record does not expose a single array place"
                      {:record (:name binding) :size (:size binding)
                       :array-fields (mapv :path
                                           (filterv #(> (:size %) 1) (:leaves binding)))}))
    :frames {:op :frames :type :int}
    :sample-rate {:op :sample-rate :type :float}
    {:op :local :name (:name binding) :type (:type binding)}))

(defn- lower-binary-node
  "One two-operand IR node from two lowered operands.

  Shared by the binary comparison path and the left folds below, so the operand
  type rules are stated once."
  [form operator comparison? left right]
  (let [left-value (:value left)
        right-value (:value right)
        type (:type left-value)]
    (when-not (= type (:type right-value))
      (fail "DSP binary operands have the wrong type"
            {:form form :left-type type :right-type (:type right-value)}))
    (when-not (if comparison?
                (if (#{:= :not=} operator)
                  (contains? value-types type)
                  (contains? numeric-types type))
                (contains? numeric-types type))
      (fail "DSP binary operator does not support these operand types"
            {:form form :operator (first form) :type type}))
    (combine-fragments
     [left right]
     {:op (if comparison? :compare :binary)
      :operator operator
      :left left-value
      :right right-value
      :type (if comparison? :boolean type)})))

(defn- lower-arithmetic
  "Lower an arithmetic operator form, folding three or more operands left.

  `(+ a b c)` lowers exactly as `(+ (+ a b) c)` — each operand is lowered once,
  in source order, and the IR keeps its two-operand nodes, so folding costs no
  backend anything (D48)."
  [form env operator]
  (let [operands (vec (rest form))]
    (when (< (count operands) 2)
      (fail "DSP arithmetic operators require at least two operands"
            {:form form :operator (first form) :actual (count operands)}))
    (reduce (fn [left operand]
              (lower-binary-node form operator false
                                 left (lower-value operand env)))
            (lower-value (first operands) env)
            (rest operands))))

(defn- lower-comparison
  "Lower a comparison of exactly two operands.

  Chaining — `(< a b c)`, which `clojure.core` reads as `(and (< a b) (< b c))` —
  is refused rather than folded: the middle operand appears twice in the nested
  form, so reading it once would mean hoisting it into a local first (D48)."
  [form env operator]
  (let [[_ left-form right-form & extra] form]
    (when (or (nil? left-form) (nil? right-form) extra)
      (fail "DSP comparison operators require exactly two operands"
            {:form form :operator (first form)}))
    (lower-binary-node form operator true
                       (lower-value left-form env)
                       (lower-value right-form env))))

(defn- lower-logical
  "Lower `not`, and `and`/`or`, which fold three or more operands left."
  [form env operator]
  (let [operands (vec (rest form))]
    (if (= :not operator)
      (do
        (when-not (= 1 (count operands))
          (fail "DSP logical operators have the wrong arity"
                {:form form :operator (first form)
                 :expected 1 :actual (count operands)}))
        (let [arg (lower-value (first operands) env)]
          (expect-type! :boolean (:value arg) form)
          (combine-fragments [arg]
                             {:op :logical
                              :operator operator
                              :args [(:value arg)]
                              :type :boolean})))
      (do
        (when (< (count operands) 2)
          (fail "DSP logical operators require at least two operands"
                {:form form :operator (first form) :actual (count operands)}))
        (reduce (fn [left operand]
                  (let [right (lower-value operand env)]
                    (expect-type! :boolean (:value right) form)
                    (combine-fragments [left right]
                                       {:op :logical
                                        :operator operator
                                        :args [(:value left) (:value right)]
                                        :type :boolean})))
                (let [first-arg (lower-value (first operands) env)]
                  (expect-type! :boolean (:value first-arg) form)
                  first-arg)
                (rest operands))))))

(defn- lower-bitwise
  "Lower `bit-not`, the shifts, and `bit-and`/`bit-or`/`bit-xor`.

  The last three fold three or more operands left, as `clojure.core` does; the
  shifts keep exactly two."
  [form env operator]
  (let [operands (vec (rest form))]
    (cond
      (contains? unary-bitwise-operators operator)
      (do
        (when-not (= 1 (count operands))
          (fail "DSP bitwise operators have the wrong arity"
                {:form form :operator (first form)
                 :expected 1 :actual (count operands)}))
        (let [arg (lower-value (first operands) env)]
          (expect-type! :int (:value arg) form)
          (combine-fragments [arg]
                             {:op :bitwise
                              :operator operator
                              :args [(:value arg)]
                              :type :int})))

      (contains? variadic-bitwise-operators operator)
      (do
        (when (< (count operands) 2)
          (fail "DSP bitwise operators require at least two operands"
                {:form form :operator (first form) :actual (count operands)}))
        (reduce (fn [left operand]
                  (let [right (lower-value operand env)]
                    (expect-type! :int (:value right) form)
                    (combine-fragments [left right]
                                       {:op :bitwise
                                        :operator operator
                                        :args [(:value left) (:value right)]
                                        :type :int})))
                (let [first-arg (lower-value (first operands) env)]
                  (expect-type! :int (:value first-arg) form)
                  first-arg)
                (rest operands)))

      :else
      (do
        (when-not (= 2 (count operands))
          (fail "DSP bitwise operators have the wrong arity"
                {:form form :operator (first form)
                 :expected 2 :actual (count operands)}))
        (let [args (mapv #(lower-value % env) operands)]
          (doseq [arg args]
            (expect-type! :int (:value arg) form))
          (combine-fragments args
                             {:op :bitwise
                              :operator operator
                              :args (mapv :value args)
                              :type :int}))))))

(defn- lower-builtin-primitive
  [form env entry]
  (let [operator (:operator entry)
        args (mapv #(lower-value % env) (rest form))
        param-types (mapv #(get-in % [:value :type]) args)
        signature (builtins/resolve-signature operator param-types)]
    (when-not signature
      (fail "DSP builtin does not support these operand types"
            {:form form :operator (first form)
             :types param-types
             :supported (mapv :params (builtins/signatures operator))}))
    (combine-fragments args
                       {:op :builtin
                        :builtin operator
                        :args (mapv :value args)
                        :type (:return signature)
                        :effects (:effects entry)})))

(defn- lower-builtin-derived
  "Expand a derived builtin.

  Each argument is bound to a fresh local before the expansion is lowered, so
  an argument that is a call or buffer load is evaluated exactly once even when
  the expansion references it more than once (D13)."
  [form env entry]
  (let [{:keys [params return]} (first (:signatures entry))
        expand (:expand entry)
        arg-forms (vec (rest form))]
    (loop [arg-forms arg-forms
           params params
           env env
           statements []
           names []]
      (if-let [arg-form (first arg-forms)]
        (let [lowered (lower-value arg-form env)]
          (expect-type! (first params) (:value lowered) arg-form)
          (let [local (fresh-local! env (str (name (:operator entry)) "_arg")
                                    (:type lowered))]
            (recur (vec (rest arg-forms))
                   (vec (rest params))
                   (assoc-in env [:bindings (:name local)]
                             {:name (:name local) :type (:type local) :kind :local})
                   (into statements
                         (concat (:statements lowered)
                                 [{:op :declare
                                   :name (:name local)
                                   :type (:type local)
                                   :init (:value lowered)}]))
                   (conj names (:name local)))))
        (let [expanded (lower-value (expand names) env)]
          (expect-type! return (:value expanded) form)
          {:statements (into statements (:statements expanded))
           :value (:value expanded)
           :type (:type expanded)})))))

(defn- lower-builtin
  [form env operator]
  (let [entry (builtins/entry operator)
        arg-forms (vec (rest form))]
    (when-not (builtins/accepts-arity? operator (count arg-forms))
      (fail "DSP builtin has the wrong number of arguments"
            {:form form :operator (first form)
             :expected (builtins/arities operator)
             :actual (count arg-forms)}))
    (case (:kind entry)
      :primitive (lower-builtin-primitive form env entry)
      :derived (lower-builtin-derived form env entry))))

(defn- branch-type
  [form then-fragment else-fragment]
  (let [then-type (:type then-fragment)
        else-type (:type else-fragment)]
    (cond
      (= then-type else-type) then-type
      (= :never then-type) else-type
      (= :never else-type) then-type
      :else (fail "DSP if branches must have the same type"
                  {:form form :then-type then-type :else-type else-type}))))

(defn- block
  [statements]
  {:op :block :statements (vec statements)})

(defn- branch-statements
  [fragment result-local]
  (cond-> (:statements fragment)
    (and result-local (:value fragment))
    (conj {:op :assign
           :name (:name result-local)
           :type (:type result-local)
           :value (:value fragment)
           :effects #{:write-local}})))

(defn- lower-if
  [form env]
  (let [[_ condition-form then-form else-form & extra] form]
    (when (or (nil? condition-form) (nil? then-form) (nil? else-form) extra)
      (fail "DSP if expressions require a condition, then branch, and else branch"
            {:form form}))
    (let [condition (lower-value condition-form env)
          _ (expect-type! :boolean (:value condition) form)
          then-fragment (lower-form then-form env)
          else-fragment (lower-form else-form env)
          type (branch-type form then-fragment else-fragment)
          result-local (when (contains? value-types type)
                         (fresh-local! env "if_result" type))
          declaration (when result-local
                        {:op :declare
                         :name (:name result-local)
                         :type type})
          statement {:op :if
                     :condition (:value condition)
                     :then (block (branch-statements then-fragment result-local))
                     :else (block (branch-statements else-fragment result-local))}
          statements (concat (:statements condition)
                             (when declaration [declaration])
                             [statement])]
      (case type
        :never {:statements (vec statements) :value nil :type :never}
        :void (void-fragment statements)
        ;; A record value is a set of locals, so it must come straight from a
        ;; module call bound in a let, not be selected between by an `if`.
        :record (fail "DSP record values cannot be produced by an if"
                      {:form form})
        (fragment statements
                  {:op :local :name (:name result-local) :type type})))))

(defn- lower-let
  [form env]
  (let [[_ bindings & body] form]
    (when-not (vector? bindings)
      (fail "DSP let bindings must be a vector" {:form form}))
    (when (odd? (count bindings))
      (fail "DSP let bindings require name/value pairs" {:form form}))
    (when-not (seq body)
      (fail "DSP let requires a body" {:form form}))
    (loop [pairs (partition 2 bindings)
           env env
           statements []]
      (if-let [[[name init-form] & remaining] (seq pairs)]
        (do
          (when-not (valid-name? name)
            (fail "DSP local name must be a symbol" {:form form :name name}))
          (when (contains? (:bindings env) name)
            (fail "DSP local names cannot shadow an in-scope binding"
                  {:form form :name name}))
          (if (array-form? init-form)
            (let [binding (array-declaration env name init-form)]
              (recur remaining
                     (assoc-in env [:bindings name] binding)
                     statements))
            (let [init (lower-value init-form env)]
              (cond
                (= :array-ref (:op (:value init)))
                ;; A place alias: a compile-time name for an existing array.
                (let [ref (:value init)
                      local {:name name :kind :place
                             :region (:region ref) :base (:base ref)
                             :element (:element ref) :size (:size ref)
                             :type :array}]
                  (recur remaining
                         (assoc-in env [:bindings name] local)
                         (into statements (:statements init))))

                (= :record (:type (:value init)))
                ;; A record value (multi-output) is one local per leaf: the
                ;; interpreter holds a map, the backends split it when inlining.
                (let [value (:value init)
                      ir-name (unique-local-name env name)
                      local {:name ir-name :type :record :kind :record-value
                             :record-leaves (:record-leaves value)}
                      declaration {:op :declare
                                   :name ir-name
                                   :type :record
                                   :init value}]
                  (swap! (:locals env) conj (select-keys local [:name :type]))
                  (recur remaining
                         (assoc-in env [:bindings name] local)
                         (into statements (concat (:statements init) [declaration]))))

                :else
                (let [ir-name (unique-local-name env name)
                      local {:name ir-name :type (:type init) :kind :local}
                      declaration {:op :declare
                                   :name ir-name
                                   :type (:type init)
                                   :init (:value init)}]
                  (swap! (:locals env) conj (select-keys local [:name :type]))
                  (recur remaining
                         (assoc-in env [:bindings name] local)
                         (into statements (concat (:statements init) [declaration]))))))))
        (let [body-fragment (lower-forms body env)]
          (assoc body-fragment
                 :statements (into (vec statements)
                                   (:statements body-fragment))))))))

(defn- field-access
  "Recognize Clojure-style field access.

  `(:field record)` reads a field the way `(:key map)` reads a map entry, and
  `(record :field)` uses the record name in call position. Either way it is a
  two-element list whose head is a keyword and whose argument is a symbol, or
  the reverse. Returns `[record-name path]`, or nil when it is not field
  access."
  [form]
  (when (and (seq? form) (= 2 (count form)))
    (let [[head argument] form]
      (cond
        (and (keyword? head) (symbol? argument)) [argument [head]]
        (and (symbol? head) (keyword? argument)) [head [argument]]
        :else nil))))

(defn- lower-set!
  [form env]
  (let [[_ target value-form & extra] form]
    (when (or extra (nil? value-form))
      (fail "DSP set! requires a target and one value" {:form form}))
    (if-let [[record-name path] (field-access target)]
      (lower-place-store! form record-name path value-form env)
      (let [binding (get-in env [:bindings target])]
        (cond
          ;; A scalar state slot is writable from the body, the same way an
          ;; aggregate slot is written with `state-store`: it is one slot in the
          ;; same state arena. This is how a voice can publish a value the host
          ;; reads back (the builtin `activity` slot is one of these), rather
          ;; than only computing state through `:next`.
          (= :state (:kind binding))
          (do
            (when (> (or (:size binding) 1) 1)
              (fail "DSP set! on aggregate state must use aset"
                    {:form form :name target :size (:size binding)}))
            (reject-constant-store! form binding)
            (let [value (lower-value value-form env)]
              (expect-type! (:type binding) (:value value) form)
              (void-fragment
               (concat (:statements value)
                       [{:op :state-store
                         :state-id (:name binding)
                         :index (:index binding)
                         :size 1
                         :type (:type binding)
                         :value (:value value)
                         :effects #{:write-state}}]))))

          (not (valid-name? target))
          (fail "DSP set! requires a local name or a field place" {:form form})

          (not= :local (:kind binding))
          (fail "DSP set! can only update mutable locals" {:form form :name target})

          :else
          (let [value (lower-value value-form env)]
            (expect-type! (:type binding) (:value value) form)
            (fragment (conj (:statements value)
                            {:op :assign
                             :name target
                             :type (:type binding)
                             :value (:value value)
                             :effects #{:write-local}})
                      {:op :local :name target :type (:type binding)})))))))

(defn- loop-bound
  [form value source]
  (when-not (and (integer? value) (pos? value))
    (fail "DSP loop bounds must be positive integer constants"
          {:form form :value value :source source}))
  (int value))

(defn- lower-while
  [form env]
  (let [[_ condition-form & forms] form
        options (if (map? (last forms)) (last forms) {})
        body-forms (if (map? (last forms)) (butlast forms) forms)
        unknown-options (seq (remove #{:max-iterations} (keys options)))
        bound (cond
                unknown-options
                (fail "DSP while received unknown options"
                      {:form form :options options
                       :unknown-options (vec unknown-options)})
                (contains? options :max-iterations)
                (loop-bound form (:max-iterations options) :explicit)
                (some? (get-in env [:options :loop-bound]))
                (loop-bound form (get-in env [:options :loop-bound]) :option)
                :else nil)]
    (when (or (nil? condition-form) (empty? body-forms))
      (fail "DSP while requires a condition and body" {:form form}))
    (let [condition (lower-value condition-form env)]
      (when (seq (:statements condition))
        (fail "DSP while conditions cannot contain statements" {:form form}))
      (expect-type! :boolean (:value condition) form)
      (let [body (lower-forms body-forms (update env :loop-depth inc))]
        (void-fragment
         [{:op :while
           :condition (:value condition)
           :body (block (:statements body))
           :bound bound}])))))

(defn- lower-loop-control
  [form env operator]
  (when-not (and (= 1 (count form)) (pos? (:loop-depth env)))
    (fail (if (pos? (:loop-depth env))
            "DSP loop control forms take no arguments"
            "DSP loop control forms are only valid inside while loops")
          {:form form :operator operator}))
  {:statements [{:op operator}] :value nil :type :never})

(defn- int-binary
  [operator left right]
  {:op :binary :operator operator :type :int :left left :right right})

(defn- const-int
  [value]
  {:op :const :type :int :value (long value)})

(defn- region-element-index
  "The flat index of one sample inside a region: `channel*stride + frame + offset`.

  A region is a call-partition state slot (D57), so its addressing is state
  addressing with a channel stride and the current frame — the same shape the lane
  and shared regions use, but per call site. `channel` and `offset` are IR int
  nodes or raw constants; a constant channel (the usual case) is one."
  [channel offset stride]
  (let [int-value (fn [x] (if (map? x) x (const-int x)))]
    (int-binary :+ (int-binary :+ (int-binary :* (int-value channel) (const-int stride))
                               (int-value offset))
                {:op :frame :type :int})))

(defn- region-slot
  "The region backing a buffer id at this point in lowering, or nil.

  A process-shaped module's own buffers are regions (F2), recorded among its
  lowered buffers; a `with-buffers` region name is recorded in `:region-aliases`.
  Both resolve to the same thing: a call-partition state slot with a channel
  stride."
  [env buffer-id]
  (or (get @(:region-aliases env) buffer-id)
      (let [buffer (get-in env [:buffers buffer-id])]
        (when (:region buffer) buffer))))

(defn- lower-int
  [form value env]
  (let [lowered (lower-value value env)]
    (expect-type! :int (:value lowered) form)
    lowered))

(defn- lower-buffer-load
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset] (channel-and-offset form args)]
    (if-let [region (region-slot env buffer-id)]
      ;; A region's samples live in a call-partition state slot, so a region read
      ;; is a state read with the channel folded into the index (F2, D57).
      (let [channel (channel-expression form channel env region)
            offset (lower-int form offset env)]
        (combine-fragments
         [channel offset]
         {:op :state-load
          :state-id (:state-id region)
          :index (:index region)
          :element-index (region-element-index (:value channel) (:value offset)
                                               (:stride region))
          :size (:size region)
          :type :float
          :effects #{:read-state}}))
      (let [buffer (buffer-access! form env buffer-id #{:input :output})
            channel (channel-expression form channel env buffer)
            index (frame-index form offset env)]
        (when-not (= :float (:type buffer))
          (fail "DSP buffer-load currently requires a :float buffer"
                {:form form :buffer-id buffer-id :type (:type buffer)}))
        (combine-fragments
         [channel index]
         {:op :buffer-load
          :buffer-id buffer-id
          :channel (:value channel)
          :index (:value index)
          :type :float
          :effects #{:read-memory}})))))

(defn- lower-buffer-store
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset value-form]
        (case (count args)
          2 [0 (first args) (second args)]
          3 [(first args) (second args) (nth args 2)]
          (fail "DSP buffer-store requires offset, value, or channel, offset, value"
                {:form form}))]
    (if-let [region (region-slot env buffer-id)]
      (let [channel (channel-expression form channel env region)
            offset (lower-int form offset env)
            value (lower-value value-form env)]
        (expect-type! :float (:value value) form)
        (void-fragment
         (concat (:statements channel)
                 (:statements offset)
                 (:statements value)
                 [{:op :state-store
                   :state-id (:state-id region)
                   :index (:index region)
                   :element-index (region-element-index (:value channel)
                                                        (:value offset)
                                                        (:stride region))
                   :size (:size region)
                   :type :float
                   :value (:value value)
                   :effects #{:write-state}}])))
      (let [buffer (buffer-access! form env buffer-id #{:output})
            channel (channel-expression form channel env buffer)
            index (frame-index form offset env)
            value (lower-value value-form env)]
        (when-not (= :float (:type buffer))
          (fail "DSP buffer-store currently requires a :float buffer"
                {:form form :buffer-id buffer-id :type (:type buffer)}))
        (expect-type! :float (:value value) form)
        (void-fragment
         (concat (:statements channel)
                 (:statements index)
                 (:statements value)
                 [{:op :buffer-store
                   :buffer-id buffer-id
                   :channel (:value channel)
                   :index (:value index)
                   :value (:value value)
                   :effects #{:write-memory}}]))))))

(defn- buffer-access-form?
  "True when a body form is, or contains, a process-buffer access."
  [form]
  (cond
    (seq? form) (or (contains? #{'buffer-load 'buffer-store} (first form))
                    (some buffer-access-form? form))
    (vector? form) (some buffer-access-form? form)
    (map? form) (some buffer-access-form? (vals form))
    (set? form) (some buffer-access-form? form)
    :else false))

(defn- lane-access-form?
  "True when a body form reads the lane region: with the `(lane control)`
  accessor, or by naming a control that exists only as a lane, whose name *is*
  the read.

  A definition that does this needs the lane region handed to it: its own body
  has no such region, and the offsets in it belong to the artifact's entry
  process (F3, F4). The scan is on the *source* body because the region is an ABI
  parameter of the definition, so it has to be known before lowering the body
  that would discover it."
  [form lane-names]
  (cond
    (seq? form) (or (= 'lane (first form)) (some #(lane-access-form? % lane-names) form))
    (vector? form) (some #(lane-access-form? % lane-names) form)
    (map? form) (some #(lane-access-form? % lane-names) (vals form))
    (set? form) (some #(lane-access-form? % lane-names) form)
    (symbol? form) (contains? lane-names form)
    :else false))

(defn- resolve-with-buffers
  "Resolve a `with-buffers` form against the linked modules.

  Returns `{:function-id id :regions {callee-buffer-id caller-buffer-id}}`. The
  bare-keyword sugar names a callee's single output buffer; a map form must name
  every output buffer, so a region is never silently left unbound (D57)."
  [form env]
  (let [[_ bindings call-form] form]
    (when-not (and (seq? call-form) (= 'dsp/call (first call-form)))
      (fail "with-buffers must wrap a resolved call" {:form form}))
    (let [function-id (second call-form)
          callee (get (:modules env) function-id)]
      (when-not callee
        (fail "with-buffers refers to an unknown linked function"
              {:form form :function-id function-id}))
      (let [outputs (filterv #(= :output (:direction %)) (:buffers callee))
            output-ids (mapv :id outputs)]
        (cond
          (keyword? bindings)
          (do (when-not (= 1 (count outputs))
                (fail (str "with-buffers sugar requires a callee with exactly one "
                           "output buffer; use a map of buffer names")
                      {:form form :function-id function-id
                       :output-buffers output-ids}))
              {:function-id function-id
               :regions {(first output-ids) bindings}})

          (map? bindings)
          (do (when-not (= (set (keys bindings)) (set output-ids))
                (fail "with-buffers must bind exactly the callee's output buffers"
                      {:form form :function-id function-id
                       :output-buffers output-ids :bindings (vec (keys bindings))}))
              {:function-id function-id :regions bindings})

          :else
          (fail "with-buffers requires a buffer name or a map of buffer names"
                {:form form :bindings bindings}))))))

(defn- lower-with-buffers
  "Lower `(with-buffers <binding> <call>)`.

  The call allocates the callee's state partition (D19), and the callee's output
  buffers are regions inside it (F2, D57). Binding them is therefore pure
  bookkeeping: each region name becomes an alias for the callee's rebased state
  slot, which a later `buffer-load` resolves. Nothing is passed to the call --
  the region *is* the callee's own partition state."
  [form env]
  (let [call-form (nth form 2)
        {:keys [function-id regions]} (resolve-with-buffers form env)
        call (lower-value call-form env)
        base (long (or (:state-base (:value call)) 0))
        slots (:region-slots (get (:modules env) function-id))]
    (doseq [[callee-buffer region-name] regions]
      (let [slot (get slots callee-buffer)]
        (when-not slot
          (fail "with-buffers names a buffer the callee did not lower as a region"
                {:form form :buffer callee-buffer}))
        (when (contains? @(:region-aliases env) region-name)
          (fail "with-buffers binds a region name twice"
                {:form form :region region-name}))
        (swap! (:region-aliases env) assoc region-name
               (assoc slot :state-id callee-buffer :id region-name
                      :index (+ base (long (:index slot)))))))
    call))

(defn- reject-constant-store!
  "Constant tables and records are materialized at initialization only."
  [form binding]
  (when (:constant binding)
    (fail "DSP constant tables and records are read-only"
          {:form form :state (:name binding)})))

(defn- aggregate-state-binding!
  [form env state-id]
  (let [binding (get-in env [:bindings state-id])]
    (when-not (= :state (:kind binding))
      (fail "DSP state access requires a declared state name"
            {:form form :state state-id}))
    (when-not (> (or (:size binding) 1) 1)
      (fail "indexed state access requires aggregate state"
            {:form form :state state-id :size (:size binding)}))
    binding))

(defn- aggregate-state-index
  [form env binding index-form]
  (let [index (lower-value index-form env)]
    (expect-type! :int (:value index) form)
    (when (and (= :const (get-in index [:value :op]))
               (or (neg? (get-in index [:value :value]))
                   (>= (get-in index [:value :value]) (:size binding))))
      (fail "DSP aggregate state index is outside the declared range"
            {:form form :state (:name binding) :size (:size binding)
             :index (get-in index [:value :value])}))
    index))

(defn- lower-state-load
  [form env]
  (let [[_ state-id index-form & extra] form]
    (when (or (not (symbol? state-id)) (nil? index-form) extra)
      (fail "state-load requires an aggregate state name and one integer index"
            {:form form}))
    (let [binding (aggregate-state-binding! form env state-id)
          index (aggregate-state-index form env binding index-form)]
      (fragment (:statements index)
                {:op :state-load
                 :state-id (:name binding)
                 :index (:index binding)
                 :element-index (:value index)
                 :size (:size binding)
                 :type (:type binding)
                 :effects #{:read-state}}))))

(defn- lower-state-store
  [form env]
  (let [[_ state-id index-form value-form & extra] form]
    (when (or (not (symbol? state-id)) (nil? index-form) (nil? value-form) extra)
      (fail "state-store requires an aggregate state name, integer index, and value"
            {:form form}))
    (let [binding (aggregate-state-binding! form env state-id)
          index (aggregate-state-index form env binding index-form)
          value (lower-value value-form env)]
      (reject-constant-store! form binding)
      (expect-type! (:type binding) (:value value) form)
      (void-fragment
       (concat (:statements index) (:statements value)
               [{:op :state-store
                 :state-id (:name binding)
                 :index (:index binding)
                 :element-index (:value index)
                 :size (:size binding)
                 :type (:type binding)
                 :value (:value value)
                 :effects #{:write-state}}])))))

(defn- lower-store-special
  "Store a builtin, host-readable slot: `(store-special :activity value)`.

  `effective-state` already added the slot to the definition's state, so this is
  an ordinary scalar store. A module that writes a special slot gets its own,
  private to its call site; only the process entry's are exposed in the ABI."
  [form env]
  (let [[_ id value-form & extra] form]
    (when (or (not (keyword? id)) (nil? value-form) extra)
      (fail "store-special requires a special slot id and one value"
            {:form form :special id}))
    (let [name (symbol (name id))
          binding (get-in env [:bindings name])]
      (when-not (and (= :state (:kind binding)) (= id (:special binding)))
        (fail "store-special is only available for a declared special slot"
              {:form form :special id}))
      (reject-constant-store! form binding)
      (let [value (lower-value value-form env)]
        (expect-type! (:type binding) (:value value) form)
        (void-fragment
         (concat (:statements value)
                 [{:op :state-store
                   :state-id (:name binding)
                   :index (:index binding)
                   :size 1
                   :type (:type binding)
                   :value (:value value)
                   :effects #{:write-state}}]))))))

(defn- array-form?
  [form]
  (and (seq? form) (= 'array (first form))))

(defn- array-declaration
  "Declare a fixed-size local array in the instance's scratch region.

  Arrays get distinct logical slots and their offsets continue after persistent
  state. The declaration emits no runtime statement: the region is zeroed when
  the instance is created and retains its contents between frames, so a scratch
  array must be written before it is read within a frame."
  [env name init-form]
  (let [[_ element size & extra] init-form]
    (when (or extra
              (not (contains? #{:float :int} element))
              (not (and (integer? size) (pos? size))))
      (fail "array requires a :float or :int element type and a positive constant size"
            {:form init-form :name name :element element :size size}))
    (let [{:keys [next]} @(:scratch env)
          entry {:name name :type element :size size :index next}]
      (swap! (:scratch env)
             (fn [scratch]
               (-> scratch
                   (assoc :next (+ next size))
                   (update :entries conj entry))))
      (assoc entry :kind :array))))

(defn- reject-constant-place!
  [form ref]
  (when (:constant ref)
    (fail "DSP constant tables and records are read-only" {:form form})))

(defn- place-index
  "Lower an array index and check a constant one against the place's length."
  [form ref index-form env]
  (let [index (lower-value index-form env)]
    (expect-type! :int (:value index) form)
    (when (and (= :const (get-in index [:value :op]))
               (or (neg? (get-in index [:value :value]))
                   (>= (get-in index [:value :value]) (:size ref))))
      (fail "DSP array index is outside the declared range"
            {:form form :size (:size ref)
             :index (get-in index [:value :value])}))
    index))

(defn- array-place
  "Lower an `aget`/`aset` target to its `:array-ref` fragment."
  [form env target]
  (let [value (lower-value target env)]
    (when-not (= :array-ref (:op (:value value)))
      (fail "DSP array access requires an array place"
            {:form form :target target}))
    value))

(declare field-element-count field-leaf! leaf-load leaf-store)

(defn- record-array-binding
  "The `:record` binding named `name` when it is an array of records, else nil.

  Such a binding is not one array place (`record-array-ref` declines it), so
  `aget`/`aset` resolve the two indices through the record accessors."
  [env name]
  (when (symbol? name)
    (let [binding (get-in env [:bindings name])]
      (when (and (= :record (:kind binding)) (> (or (:size binding) 1) 1))
        binding))))

(defn- record-array-field!
  "The single array field of a record array, or fail naming the candidates."
  [form binding]
  (let [fields (filterv #(> (field-element-count true %) 1) (:leaves binding))]
    (when-not (= 1 (count fields))
      (fail "a record array's array access needs exactly one array field; name one with field"
            {:form form :state (:name binding)
             :array-fields (mapv :path fields)}))
    (first fields)))

(defn- lower-array-load
  [form env]
  (let [[_ target & arguments] form]
    (when (nil? target)
      (fail (str (first form) " requires an array place and one integer index")
            {:form form}))
    (if-let [binding (record-array-binding env target)]
      ;; `(aget voices i k)` — the terse spelling of `(field voices i :field k)`.
      (let [[record-index element-index & extra] arguments]
        (when (or (nil? element-index) extra)
          (fail "a record array's array access takes a record index and an element index"
                {:form form :state (:name binding)}))
        (leaf-load binding (field-leaf! form env binding
                                        {:record-index record-index
                                         :path (:path (record-array-field! form binding))
                                         :element-index element-index})))
      (let [[index-form & extra] arguments]
        (when (or (nil? index-form) extra)
          (fail (str (first form) " requires an array place and one integer index")
                {:form form}))
        (let [place (array-place form env target)
              ref (:value place)
              index (place-index form ref index-form env)]
          (fragment (concat (:statements place) (:statements index))
                    (if (= :scratch (:region ref))
                      {:op :array-load
                       :index (:base ref)
                       :element-index (:value index)
                       :size (:size ref)
                       :type (:element ref)
                       :effects #{:read-memory}}
                      {:op :state-load
                       :state-id (:state-id ref)
                       :index (:base ref)
                       :element-index (:value index)
                       :size (:size ref)
                       :type (:element ref)
                       :effects #{:read-state}})))))))

(defn- lower-array-store
  [form env]
  (let [[_ target & arguments] form]
    (when (nil? target)
      (fail (str (first form) " requires an array place, integer index, and value")
            {:form form}))
    (if-let [binding (record-array-binding env target)]
      ;; `(aset voices i k v)` — the terse spelling of `(set-field! ...)`.
      (let [[record-index element-index value-form & extra] arguments]
        (when (or (nil? value-form) extra)
          (fail (str "a record array's array access takes a record index, an"
                     " element index, and a value")
                {:form form :state (:name binding)}))
        (let [resolved (field-leaf! form env binding
                                    {:record-index record-index
                                     :path (:path (record-array-field! form binding))
                                     :element-index element-index})
              value (lower-value value-form env)]
          (reject-constant-store! form binding)
          (expect-type! (:type (:leaf resolved)) (:value value) form)
          (leaf-store binding resolved value)))
      (let [[index-form value-form & extra] arguments]
        (when (or (nil? index-form) (nil? value-form) extra)
          (fail (str (first form) " requires an array place, integer index, and value")
                {:form form}))
        (let [place (array-place form env target)
              ref (:value place)
              index (place-index form ref index-form env)
              value (lower-value value-form env)]
          (reject-constant-place! form ref)
          (expect-type! (:element ref) (:value value) form)
          (void-fragment
           (concat (:statements place) (:statements index) (:statements value)
                   (if (= :scratch (:region ref))
                     [{:op :array-store
                       :index (:base ref)
                       :element-index (:value index)
                       :size (:size ref)
                       :type (:element ref)
                       :value (:value value)
                       :effects #{:write-memory}}]
                     [{:op :state-store
                       :state-id (:state-id ref)
                       :index (:base ref)
                       :element-index (:value index)
                       :size (:size ref)
                       :type (:element ref)
                       :value (:value value)
                       :effects #{:write-state}}]))))))))

(defn- record-binding!
  [form env name]
  (let [binding (get-in env [:bindings name])]
    (when-not (= :record (:kind binding))
      (fail "DSP field access requires a declared state record"
            {:form form :state name}))
    binding))

(defn- value-record-binding
  "The `:record-value` binding named `name`, if any (a multi-output result)."
  [env name]
  (let [binding (get-in env [:bindings name])]
    (when (= :record-value (:kind binding)) binding)))

(defn- project-record-value
  [form binding path]
  (let [leaf (some #(when (= (vec (:path %)) (vec path)) %) (:record-leaves binding))]
    (when-not leaf
      (fail "DSP record value has no such field"
            {:form form :record (:name binding) :field (vec path)
             :fields (mapv :path (:record-leaves binding))}))
    (fragment []
              {:op :project
               :value {:op :local :name (:name binding) :type :record}
               :path (:path leaf)
               :type (:type leaf)})))

(defn- field-selectors
  "Split field selectors into an optional record index, a field path, and an
  optional element index.

  Selectors are `[record-index?] field-name+ [element-index?]`; a leading or
  trailing non-keyword selector is the record index or the element index."
  [form selectors]
  (let [selectors (vec selectors)
        [record-index selectors] (if (and (seq selectors)
                                          (not (keyword? (first selectors))))
                                   [(first selectors) (subvec selectors 1)]
                                   [nil selectors])
        [element-index selectors] (if (and (seq selectors)
                                           (not (keyword? (peek selectors))))
                                    [(peek selectors) (pop selectors)]
                                    [nil selectors])]
    (when (or (empty? selectors) (not (every? keyword? selectors)))
      (fail "DSP field access requires field names and optional indices"
            {:form form :selectors selectors}))
    {:record-index record-index :path selectors :element-index element-index}))

(defn- field-element-count
  "The number of elements one record owns of `leaf`'s field.

  An array of records that owns an array field carries its per-record element
  count as `:stride` (the leaf's own size is then the whole flat block);
  otherwise the leaf's size is the element count (a scalar record's array field)
  or 1."
  [record-array? leaf]
  (if record-array?
    (or (:stride leaf) 1)
    (or (:size leaf) 1)))

(defn- lower-index
  "Lower a field index and check that it is an integer."
  [form env index-form]
  (let [index (lower-value index-form env)]
    (expect-type! :int (:value index) form)
    index))

(defn- constant-index
  "The integer value of a lowered index when it is constant, else nil."
  [index]
  (when (= :const (get-in index [:value :op]))
    (get-in index [:value :value])))

(defn- strided-index
  "Fold a record index and an element index into one flat element index.

  An array of records that owns an array field is laid out strided: record `i`
  owns `stride` slots, so element `k` of record `i` sits at `i * stride + k`.
  Folding here is what keeps the backends unchanged — a leaf still has one
  constant base offset and one element index. Both dimensions are checked when
  they are constant; a dynamic one is the interpreter's development-time check."
  [form env binding leaf record-index element-index]
  (let [stride (long (field-element-count true leaf))
        records (long (:size binding))
        record (lower-index form env record-index)
        element (lower-index form env element-index)
        record-value (constant-index record)
        element-value (constant-index element)
        statements (vec (concat (:statements record) (:statements element)))]
    (when (and record-value (or (neg? record-value) (>= record-value records)))
      (fail "DSP record index is outside the declared range"
            {:form form :state (:name binding) :record-index record-value
             :records records}))
    (when (and element-value (or (neg? element-value) (>= element-value stride)))
      (fail "DSP array index is outside the declared range"
            {:form form :state (:name binding) :element-index element-value
             :size stride}))
    (fragment statements
              (if (and record-value element-value)
                ;; A fully constant access stays constant, so the host sees an
                ;; index and the access never reports a dynamic diagnostic.
                {:op :const :type :int
                 :value (+ (* record-value stride) element-value)}
                {:op :binary :operator :+
                 :left {:op :binary :operator :*
                        :left (:value record)
                        :right {:op :const :type :int :value stride}
                        :type :int}
                 :right (:value element)
                 :type :int}))))

(defn- field-leaf!
  "Resolve a parsed field access.

  Selectors are `[record-index?] field-name+ [element-index?]`. For a scalar
  record owning an array field the element index is the flat index and no element
  index is an array place (see `leaf-array-ref`); for an array of records the
  record index selects the record, and an array field takes both indices, folded
  by `strided-index`. Returns `{:leaf ... :index ...}`, or `{:place? true}` for an
  array place."
  [form env binding parsed]
  (let [path (:path parsed)
        leaf (get (:leaf-index binding) path)]
    (when-not leaf
      (fail "DSP field path does not exist in the record"
            {:form form :state (:name binding) :path path}))
    (let [record-array? (> (or (:size binding) 1) 1)
          array-field? (> (field-element-count record-array? leaf) 1)
          record-index (:record-index parsed)
          element-index (:element-index parsed)]
      (when (and (not record-array?) record-index)
        (fail "a scalar record has no record index"
              {:form form :state (:name binding) :path path}))
      (when (and element-index (not array-field?))
        (fail "scalar field access does not take an element index"
              {:form form :state (:name binding) :path path}))
      (when (and record-array? (nil? record-index))
        (fail "a record array's field needs a record index"
              {:form form :state (:name binding) :path path}))
      (when (and record-array? array-field? (nil? element-index))
        (fail "a record array's array field needs an element index"
              {:form form :state (:name binding) :path path}))
      (cond
        ;; An array of records owning an array field: both indices, folded.
        (and record-array? array-field?)
        {:leaf leaf :index (strided-index form env binding leaf
                                          record-index element-index)}

        ;; A scalar record owning an array field: no element index is the whole
        ;; field as a place.
        (and (not record-array?) array-field?)
        (if (nil? element-index)
          {:leaf leaf :place? true}
          {:leaf leaf :index (lower-index form env element-index)})

        ;; An array of records with a scalar field: the record index is flat.
        record-array?
        {:leaf leaf :index (lower-index form env record-index)}

        ;; A scalar field: there is no index to speak of.
        :else {:leaf leaf}))))

(defn- leaf-load
  "A `:state-load` for a resolved field access."
  [binding {:keys [leaf index]}]
  (fragment (:statements index)
            {:op :state-load
             :state-id (:name binding)
             :index (:offset leaf)
             :element-index (:value index)
             :size (:size leaf)
             :type (:type leaf)
             :effects #{:read-state}}))

(defn- leaf-store
  "A `:state-store` for a resolved field access and a lowered value."
  [binding {:keys [leaf index]} value]
  (void-fragment
   (concat (:statements index) (:statements value)
           [{:op :state-store
             :state-id (:name binding)
             :index (:offset leaf)
             :element-index (:value index)
             :size (:size leaf)
             :type (:type leaf)
             :value (:value value)
             :effects #{:write-state}}])))

(defn- leaf-array-ref
  "An `:array-ref` place for a record's array field."
  [binding leaf]
  {:op :array-ref :type :array :region :state
   :state-id (:name binding)
   :base (:offset leaf) :element (:type leaf) :size (:size leaf)
   :constant (boolean (:constant binding))})

(defn- lower-field
  [form env]
  (let [[_ name & selectors] form]
    (when-not (symbol? name)
      (fail "field requires a state record name" {:form form}))
    (if-let [binding (value-record-binding env name)]
      (let [parsed (field-selectors form selectors)]
        (when (or (:record-index parsed) (:element-index parsed))
          (fail "DSP record values have no record or element index" {:form form}))
        (project-record-value form binding (:path parsed)))
      (let [binding (record-binding! form env name)
            resolved (field-leaf! form env binding
                                  (field-selectors form selectors))]
        (if (:place? resolved)
          (fragment [] (leaf-array-ref binding (:leaf resolved)))
          (leaf-load binding resolved))))))

(defn- lower-set-field!
  [form env]
  (let [[_ name & arguments] form
        value-form (last arguments)
        selectors (butlast arguments)]
    (when-not (and (symbol? name) (seq selectors) value-form)
      (fail "set-field! requires a state record name, field path, and value"
            {:form form}))
    (when (value-record-binding env name)
      (fail "DSP record values are immutable" {:form form :record name}))
    (let [binding (record-binding! form env name)
          parsed (field-selectors form selectors)
          resolved (field-leaf! form env binding parsed)
          value (lower-value value-form env)]
      (when (:place? resolved)
        (fail "use aset to write an array field element"
              {:form form :state (:name binding) :path (:path parsed)}))
      (reject-constant-store! form binding)
      (expect-type! (:type (:leaf resolved)) (:value value) form)
      (leaf-store binding resolved value))))

(defn- lower-field-read
  "Clojure-style field read for `(:field record)` and `(record :field)`."
  [form env record-name path]
  (if-let [binding (value-record-binding env record-name)]
    (project-record-value form binding path)
    (let [binding (record-binding! form env record-name)
          resolved (field-leaf! form env binding {:path path})]
      (if (:place? resolved)
        (fragment [] (leaf-array-ref binding (:leaf resolved)))
        (leaf-load binding resolved)))))

(defn- lower-place-store!
  "Field write for a `set!` place: `(set! (:field record) value)`."
  [form record-name path value-form env]
  (when (value-record-binding env record-name)
    (fail "DSP record values are immutable"
          {:form form :record record-name}))
  (let [binding (record-binding! form env record-name)
        resolved (field-leaf! form env binding {:path path})]
    (when (:place? resolved)
      (fail "use aset to write an array field element"
            {:form form :state (:name binding) :path path}))
    (let [value (lower-value value-form env)]
      (reject-constant-store! form binding)
      (expect-type! (:type (:leaf resolved)) (:value value) form)
      (leaf-store binding resolved value))))

(defn- lower-call
  [form env]
  (let [[_ function-id & arg-forms] form
        signature (get-in env [:functions function-id])]
    (when-not (symbol? function-id)
      (fail "DSP call requires a resolved function ID" {:form form}))
    (when-not signature
      (fail "DSP call refers to an unknown linked function"
            {:form form :function-id function-id}))
    (when-not (= (count (:params signature)) (count arg-forms))
      (fail "DSP call has the wrong number of arguments"
            {:form form :function-id function-id
             :expected (count (:params signature)) :actual (count arg-forms)}))
    (let [args (mapv #(lower-value %1 env) arg-forms)]
      (doseq [[arg parameter] (map vector args (:params signature))]
        (expect-type! (:type parameter) (:value arg) form))
      ;; A callee with state gets a private partition at this call site (D19).
      ;; The callee IR keeps zero-based state indices; the partition base is
      ;; resolved relative to the enclosing module, and the rebased slots extend
      ;; the enclosing arena.
      (let [callee (get (:modules env) function-id)
            partition-size (or (:state-size callee) 0)
            ;; Every call site owns a partition, even when its callee has no state:
            ;; the partition *is* the site's identity — the slot a fused host
            ;; resets and reads a special from, and the slice a per-voice lane
            ;; belongs to (D70). A stateless site's range is empty, which every
            ;; host's reset already treats as a no-op, and the ordinal stays the
            ;; *call-site* ordinal, so slot k is the k-th call site whether or not
            ;; it carries state.
            slot (count @(:call-partitions env))
            partition-base @(:call-offset env)
            _ (swap! (:call-offset env) + partition-size)
            _ (swap! (:call-partitions env) conj
                     {:function-id function-id
                      :base partition-base :size partition-size})
            _ (when (pos? partition-size)
                (swap! (:state-more env) into
                       (rebase-slots partition-base (:state callee)))
                (swap! (:records-more env) into
                       (rebase-records partition-base (:records callee))))
            ;; A fused entry renders every voice in one call, so the lane region
            ;; it holds has one slice per voice and a lane-reading callee has to
            ;; be told which one is *this* call site's (F4). The slot is the call
            ;; partition the site owns -- the same one a host resets a voice by
            ;; -- so the base is a compile-time constant here, while a callee that
            ;; merely forwards the region passes on the base it was handed.
            lane-base
            (when (:lane-region? callee)
              (cond
                (:lane-slots env)
                {:op :const :type :int
                 :value (* (long slot)
                           (long (count (:lanes env)))
                           (long (:lane-frame-capacity env)))}

                (:lane-base? env) {:op :lane-base :type :int}))
            [argument-statements argument-values]
            (reduce (fn [[statements values] arg]
                      (let [value (:value arg)]
                        (if (contains? #{:local :const} (:op value))
                          [(into statements (:statements arg))
                           (conj values value)]
                            ;; Hoist a computed argument into a local. Inlining
                            ;; substitutes a parameter into the callee body once
                            ;; per use, so a computed argument left in place is
                            ;; duplicated by a callee that uses it more than
                            ;; once — and a stateful argument would then run
                            ;; twice against one partition.
                          (let [local (fresh-local! env "argument" (:type value))]
                            [(into statements
                                   (concat (:statements arg)
                                           [{:op :declare
                                             :name (:name local)
                                             :type (:type local)
                                             :init value}]))
                             (conj values {:op :local
                                           :name (:name local)
                                           :type (:type local)})]))))
                    [[] []]
                    args)]
        (fragment
         argument-statements
         {:op :call
          :function-id function-id
          :args argument-values
          :type (:return-type signature)
          :record-leaves (when (= :record (:return-type signature))
                           (:return-fields signature))
          :state-base partition-base
          ;; A callee that reads a lane is handed this call site's lane region
          ;; (F3). The region is not partitioned — there is one, laid out by the
          ;; entry — so the call site only has to say that the callee wants it,
          ;; and each backend passes the region it already holds.
          :lanes (boolean (:lane-region? callee))
          ;; And, in a fused entry, the base of the slice of the lane region this
          ;; call site reads: zero-based lane offsets plus a base, exactly as a
          ;; callee's state indices are resolved against a partition base (D19).
          :lane-base lane-base
          ;; The callee's inlining request travels with the call site (D47), so
          ;; a target that can call still copies what it was asked to copy.
          :inline (boolean (:inline signature))
          :effects (if partition-base
                     #{:call :read-state :write-state}
                     #{:call})})))))

(defn- lower-conversion
  [form env operator from-type to-type]
  (let [[_ value-form & extra] form]
    (when (or (nil? value-form) extra)
      (fail "DSP conversions require one operand" {:form form :operator operator}))
    (let [value (lower-value value-form env)]
      (expect-type! from-type (:value value) form)
      (fragment (:statements value)
                {:op :convert :operator operator
                 :value (:value value) :type to-type}))))

(defn- shared-binding
  "One shared control's read from the process's flat `:shared` region.

  Deliberately the same shape as `lane-binding`: the region has the same
  compile-time stride, so a shared control is an ordinary `:frame-offset`
  `:buffer-load` — the only difference is *who* writes the region, and that it is
  one region for the whole pool rather than one per voice (M5). A shared value is
  block-rate, so the host broadcasts it across the slice; a host that wants a
  per-frame global curve can write one, and reads as automation."
  [name index frame-capacity]
  {:name name
   :type :float
   :kind :buffer
   :required? true
   :expression {:op :buffer-load
                :buffer-id :shared
                :channel 0
                :index {:op :frame-offset
                        :offset (* (long index) (long frame-capacity))
                        :type :int}
                :type :float
                :effects #{:read-memory}}})

(defn- lane-binding
  "One lane's per-frame read from the process's flat `:lanes` region: lane *i*
  occupies `[i * frame-capacity, ...)`, a compile-time offset *inside the slice
  the read resolves against* (D28).

  `slice?` says the read sits in a definition that is handed a slice of the
  region rather than the whole of it, which is every callee of a fused entry
  (F4): its offset is added to the base the call site supplies rather than to the
  region's."
  [name index frame-capacity slice?]
  {:name name
   :type :float
   :kind :buffer
   :expression {:op :buffer-load
                :buffer-id :lanes
                :channel 0
                :index {:op (if slice? :lane-frame-offset :frame-offset)
                        :offset (* (long index) (long frame-capacity))
                        :type :int}
                :type :float
                :effects #{:read-memory}}})

(defn- lower-lane
  "Resolve the `(lane control)` accessor to a per-frame read from the lane region.

  The control must be one the artifact's *entry process* declares a lane for: the
  region is laid out once by the entry (D28), so its slices are compile-time
  offsets there however deep in the call graph the read sits (F3). A definition
  reached from such a process may read a lane — it is handed the region, and
  `:buffer-load :lanes` is the same node the entry itself uses, because for a
  callee the region is one of its ABI parameters rather than one of its buffers.
  A definition not reached from a process has no region to read, and says so."
  [form env]
  (let [[_ control & extra] form]
    (when (or (not (symbol? control)) extra)
      (fail "lane requires exactly one control name" {:form form}))
    (when-not (:lane-frame-capacity env)
      (fail "lane is only available where a lane region is"
            {:form form :control control}))
    ;; A fused entry owns one region with one slice per voice, and each slice is
    ;; the call site that was handed it. The entry is nobody's call site, so it
    ;; has no slice of its own: refusing keeps a read there from silently being
    ;; given one voice's automation (F4).
    (when (:lane-slots env)
      (fail (str "a fused entry cannot read a lane itself: its lanes belong to the"
                 " call sites, one slice each")
            {:form form :control control :lane-slots (:lane-slots env)}))
    (let [lanes (:lanes env)
          index (first (keep-indexed (fn [index lane]
                                       (when (= control (:name lane)) index))
                                     lanes))]
      (when-not index
        (fail "lane names a control of this process that carries no lane"
              {:form form :control control
               :lanes (mapv :name lanes)}))
      (fragment (get-in (lane-binding control index (:lane-frame-capacity env)
                                      (boolean (:lane-base? env)))
                        [:expression])))))

(defn- lower-value
  [form env]
  (cond
    (constant form) (fragment (constant form))

    (symbol? form)
    (if-let [binding (get-in env [:bindings form])]
      (fragment (binding-expression binding))
      (fail "DSP expression refers to an unknown local"
            {:form form :locals (keys (:bindings env))}))

    (seq? form)
    (let [op (first form)]
      (cond
        (field-access form)
        (let [[record-name path] (field-access form)]
          (lower-field-read form env record-name path))
        (= 'dsp/call op) (lower-call form env)
        (= 'if op) (lower-if form env)
        (= 'let op) (lower-let form env)
        (= 'do op) (lower-forms (rest form) env)
        (= 'set! op) (lower-set! form env)
        (= 'while op) (lower-while form env)
        (= 'break op) (lower-loop-control form env :break)
        (= 'continue op) (lower-loop-control form env :continue)
        (= 'buffer-load op) (lower-buffer-load form env)
        (= 'with-buffers op) (lower-with-buffers form env)
        (= 'lane op) (lower-lane form env)
        (= 'buffer-store op) (lower-buffer-store form env)
        (= 'state-load op) (lower-state-load form env)
        (= 'state-store op) (lower-state-store form env)
        (= 'store-special op) (lower-store-special form env)
        (= 'array op) (fail "array must be bound by a let binding" {:form form})
        (= 'array-load op) (lower-array-load form env)
        (= 'array-store op) (lower-array-store form env)
        (= 'aget op) (lower-array-load form env)
        (= 'aset op) (lower-array-store form env)
        (= 'field op) (lower-field form env)
        (= 'set-field! op) (lower-set-field! form env)
        (= 'int->float op) (lower-conversion form env :int->float :int :float)
        (= 'float->int op) (lower-conversion form env :float->int :float :int)
        (contains? logical-operators op)
        (lower-logical form env (get logical-operators op))
        (contains? bitwise-operators op)
        (lower-bitwise form env (get bitwise-operators op))
        (contains? arithmetic-operators op)
        (lower-arithmetic form env (get arithmetic-operators op))
        (contains? comparison-operators op)
        (lower-comparison form env (get comparison-operators op))
        (builtins/builtin? op)
        (lower-builtin form env op)
        :else (fail "unsupported DSP expression" {:form form :operator op})))

    ;; A record value: a map matching the struct carried by the environment. It
    ;; is erased at lowering, so `let`/`do` around it work as ordinary forms.
    (map? form)
    (if-let [record (:return-record env)]
      (lower-record-value form (:fields record) env)
      (fail "DSP record values may only be returned from a module"
            {:form form}))

    :else (fail "unsupported DSP expression value" {:form form})))

(defn- lower-form
  [form env]
  (lower-value form env))

(defn- lower-forms
  [forms env]
  (when-not (seq forms)
    (fail "DSP blocks must contain at least one form" {:forms forms}))
  (loop [remaining (seq forms)
         statements []]
    (let [current (lower-form (first remaining) env)
          statements (into statements (:statements current))]
      (if-let [remaining (next remaining)]
        (recur remaining
               (cond-> statements
                 (= :call (get-in current [:value :op]))
                 (conj {:op :expression :value (:value current)})))
        (assoc current :statements (vec statements))))))

(defn- collect-effects
  [value]
  (letfn [(walk [x]
            (cond
              (map? x) (into (or (:effects x) #{})
                             (mapcat walk (vals (dissoc x :effects))))
              (sequential? x) (into #{} (mapcat walk x))
              :else #{}))]
    (walk value)))

(defn- normalized-definition!
  [definition]
  (when-not (and (map? definition)
                 (= :function (:dsp/kind definition))
                 (every? map? (:params definition))
                 (every? map? (:state definition)))
    (fail "DSP IR lowering requires a normalized descriptor"
          {:definition definition}))
  definition)

(defn- signatures
  [definitions]
  (into {}
        (map (fn [[id definition]]
               [id {:params (:params definition)
                    :return-type (:return-type definition)
                    ;; The callee asks to be copied into its call sites (D47).
                    :inline (boolean (:inline definition))
                    ;; A record return carries its flat field list, so a call site
                    ;; can bind one local per output (multi-output).
                    :return-fields (when-let [record (:return-record definition)]
                                     (vec (record-leaves (:fields record) [])))}])
             definitions)))

(defn- record-leaves
  "Flatten a record's field tree into leaves carrying their path, element type,
  and (for an array field) its slot count."
  [fields prefix]
  (mapcat (fn [field]
            (let [path (conj prefix (:name field))]
              (if (contains? field :fields)
                (record-leaves (:fields field) path)
                [{:path path
                  :type (:type field)
                  :size (or (:size field) 1)
                  :init (:init field)}])))
          fields))

(defn- record-leaf-name
  [record-name path]
  (symbol (str (name record-name) "." (str/join "." (map name path)))))

(defn- repeat-field-init
  "The flat initializers of one field of an array of `n` records: the field's own
  initializers, repeated once per record."
  [n init]
  (vec (mapcat (fn [_] (if (vector? init) init [init])) (range n))))

(defn- expand-record
  "Expand one record entry into flat leaf slots from `offset`.

  An array of records (`n > 1`) is laid out struct-of-arrays: each leaf is one
  flat state entry of `:size n`. A scalar record (`n = 1`) may instead own array
  fields, in which case each field occupies its own contiguous block. An array of
  records may own an array field too: the leaf is then one flat block of `n * m`
  slots, `m` per record, and carries `:stride m` so an accessor can fold a record
  index and an element index into one flat index (`record * m + element`). Either
  way a leaf is an ordinary state slot with a constant base offset and a single
  element index, so the backends are unchanged. Returns
  `[slots offset' binding]`."
  [state offset]
  (let [n (or (:size state) 1)
        scalar? (= n 1)
        leaves (vec (record-leaves (:fields state) []))
        [slots offset' leaf-list]
        (reduce (fn [[slots offset result] leaf]
                  (let [;; `m` slots per record: one for a scalar field, the
                        ;; field's own element count for an array field.
                        elements (or (:size leaf) 1)
                        size (if scalar? elements (* n elements))
                        stride (when (and (not scalar?) (> elements 1)) elements)
                        entry (cond-> {:name (record-leaf-name (:name state) (:path leaf))
                                       :type (:type leaf)
                                       :index offset}
                                (> size 1) (assoc :size size
                                                  :init (if scalar?
                                                          (:init leaf)
                                                          (repeat-field-init
                                                           n (:init leaf))))
                                (= size 1) (assoc :init (:init leaf)))]
                    [(conj slots entry)
                     (+ offset size)
                     (conj result (cond-> {:path (:path leaf)
                                           :type (:type leaf)
                                           :offset offset
                                           :size size}
                                    stride (assoc :stride stride)))]))
                [[] offset []]
                leaves)
        leaf-index (into {} (map (fn [leaf] [(:path leaf) leaf])) leaf-list)
        binding (cond-> {:name (:name state)
                         :kind :record
                         :size n
                         :leaves leaf-list
                         :leaf-index leaf-index}
                  (:constant state) (assoc :constant true)
                  (:struct-name state) (assoc :struct-name (:struct-name state)))]
    [slots offset' binding]))

(defn- expand-state
  "Expand normalized state (including records) into flat logical slots plus
  binding and layout descriptions.

  Returns `{:slots [...] :bindings {name binding} :records [...]}`. Slots are
  what the backends store; a record binding maps a field path to the slot that
  holds it."
  [states]
  (loop [remaining states
         offset 0
         slots []
         bindings {}
         records []]
    (if-let [state (first remaining)]
      (if (contains? state :fields)
        (let [[record-slots offset' binding] (expand-record state offset)]
          (recur (rest remaining) offset'
                 (into slots record-slots)
                 (assoc bindings (:name state) binding)
                 (conj records (select-keys binding
                                            [:name :size :constant :struct-name :leaves]))))
        (let [size (or (:size state) 1)
              entry (assoc state :index offset)]
          (recur (rest remaining) (+ offset size)
                 (conj slots (select-keys entry
                                          [:name :type :size :init :index :next
                                           :constant :special :special-bit]))
                 (assoc bindings (:name state) (assoc entry :kind :state))
                 records)))
      {:slots slots :bindings bindings :records records})))

(def ^:private special-slots
  "Builtin, host-readable slots a definition can publish with `store-special`.

  The key is the id written in the form; the value gives the slot's fixed bit,
  type, and initial value. The bit is the ABI contract a host uses to address a
  special (`:activity` is bit 0): a definition's specials occupy its first state
  slots in ascending-bit order, so a host derives each slot's index from the
  mask. `:activity` is the voice's finish signal (D39): a released voice is
  freed when it reads <= 0. A voice that never writes it keeps its initial
  value, so it stays until stolen."
  {:activity {:bit 0 :type :int :init 1}})

(defn- used-special-ids
  "The special-slot ids a definition writes in its body."
  [forms]
  (let [found (volatile! #{})]
    (letfn [(walk [form]
              (cond
                (seq? form)
                (do (when (and (= 'store-special (first form))
                               (keyword? (second form)))
                      (vswap! found conj (second form)))
                    (run! walk form))
                (vector? form) (run! walk form)
                (map? form) (run! (fn [[key value]] (walk key) (walk value)) form)
                :else nil))]
      (run! walk forms)
      @found)))

(defn- special-bits
  "The fixed bits of the special slots a definition writes, ascending."
  [definition]
  (sort (map #(get-in special-slots [% :bit]) (used-special-ids (:body definition)))))

(defn- special-mask
  "The mask of builtin special slots a definition writes (D39)."
  [definition]
  (reduce (fn [mask bit] (bit-or mask (bit-shift-left 1 (long bit))))
          0 (special-bits definition)))

(defn- effective-state
  "A definition's builtin special slots, then its declared state. The specials
  come first, in ascending-bit order, so their logical indices are exactly the
  low slots of the entry's state and a host can derive them from the mask (D39).
  They are added before `expand-state` numbers the slots, so each is an ordinary
  slot with an ordinary index."
  [definition]
  (let [states (vec (or (:state definition) []))
        declared (set (map :name states))
        special-states
        (mapv (fn [id]
                (let [slot (get special-slots id)
                      name (symbol (name id))]
                  (when-not slot
                    (fail "unknown special slot"
                          {:special id :supported (vec (sort (keys special-slots)))}))
                  (when (contains? declared name)
                    (fail "a special slot name collides with a declared state"
                          {:special id :state name}))
                  {:name name :type (:type slot) :init (:init slot)
                   :special id :special-bit (:bit slot)}))
              (sort-by #(get-in special-slots [% :bit])
                       (used-special-ids (:body definition))))]
    (into special-states states)))

(defn- state-total
  [states]
  (reduce + 0 (map #(or (:size %) 1) states)))

(defn- rebase-slots
  "Shift a module's relative state slots to a partition base (D19)."
  [base slots]
  (mapv #(update % :index + base) slots))

(defn- rebase-records
  "Shift a module's relative record leaf offsets to a partition base."
  [base records]
  (mapv (fn [record]
          (update record :leaves
                  #(mapv (fn [leaf] (update leaf :offset + base)) %)))
        records))

(defn- partition-specials
  "The builtin special slots a call site's partition contains, one per bit, in
  ascending-bit order, each with its logical index.

  A partition covers its whole subtree, so a callee's special -- a voice's
  `:activity` finish signal, say -- is found here even when it is nested behind
  the definition the call site names. That is what lets a fused host read one
  slot's special (D39, M6 seam 4): the entry's own `:special-slots` are its
  contract, and these are each partition's."
  [state-more base size]
  (let [in-range (filter (fn [slot]
                           (let [index (long (:index slot))]
                             (and (:special slot)
                                  (>= index (long base))
                                  (< index (+ (long base) (long size))))))
                         state-more)]
    (->> (reduce (fn [by-bit slot]
                   (let [bit (long (:special-bit slot))]
                     ;; One slot per bit, the outermost (lowest index) wins: a
                     ;; partition is a subtree and the contract is per bit.
                     (if (contains? by-bit bit)
                       by-bit
                       (assoc by-bit bit slot))))
                 {}
                 (sort-by (fn [slot] (long (:index slot))) in-range))
         vals
         (sort-by :special-bit)
         (mapv (fn [slot]
                 {:special (:special slot)
                  :bit (long (:special-bit slot))
                  :type (:type slot)
                  :size (long (or (:size slot) 1))
                  :index (long (:index slot))})))))

(defn- special-mask-of
  [specials]
  (reduce (fn [mask {:keys [bit]}]
            (bit-or (long mask) (bit-shift-left 1 (long bit))))
          0
          specials))

(defn- definition-calls
  "Every resolved `dsp/call` form in a descriptor's body and state transitions."
  [definition]
  (letfn [(walk [form]
            (cond
              (seq? form) (if (= 'dsp/call (first form))
                            (cons form (mapcat walk (nnext form)))
                            (mapcat walk form))
              (vector? form) (mapcat walk form)
              (map? form) (mapcat walk (vals form))
              :else nil))]
    (concat (mapcat walk (:body definition))
            (mapcat (fn [state] (when-let [next-form (:next state)]
                                  (walk next-form)))
                    (:state definition)))))

(defn- dependency-order
  "The linker's dependency order, which lowering must iterate explicitly.

  `:definitions` is only insertion-ordered below Clojure's array-map threshold;
  past eight definitions it is a hash-map, and iterating it would lower or size a
  callee *after* its caller — so the call site would allocate a zero-size
  partition and the arena would be undersized."
  [unit]
  (or (seq (:order unit)) (keys (:definitions unit))))

(defn- module-state-sizes
  "Partition size of every definition: its own expanded state plus one
  partition per static call site, recursively (D19). `order` is dependency order,
  so a callee's size is known before its caller's."
  [definitions order]
  (reduce (fn [sizes id]
            (let [definition (get definitions id)]
              (assoc sizes id
                     (+ (state-total (:slots (expand-state (effective-state definition))))
                        (reduce + 0 (map (fn [call] (get sizes (second call) 0))
                                         (definition-calls definition)))))))
          {}
          order))

(defn- lower-record-fields
  "Flatten a record value's map form into leaf entries.

  The form mirrors the struct's field tree, so a nested field takes a nested map.
  Returns a flat sequence of `{:path ... :type ... :fragment ...}` in field order."
  [form fields env path]
  (when-not (map? form)
    (fail "DSP record value must be a map matching the declared struct"
          {:form form :path (vec path)}))
  (let [expected (set (map :name fields))
        actual (set (keys form))]
    (when-not (= expected actual)
      (fail "DSP record value fields do not match the declared struct"
            {:form form :path (vec path)
             :expected (vec (sort expected)) :actual (vec (sort actual))})))
  (mapcat (fn [field]
            (let [value (get form (:name field))
                  field-path (conj path (:name field))]
              (if (contains? field :fields)
                (lower-record-fields value (:fields field) env field-path)
                (let [lowered (lower-value value env)]
                  (expect-type! (:type field) (:value lowered) value)
                  [{:path field-path :type (:type field) :fragment lowered}]))))
          fields))

(defn- lower-record-value
  "Lower a record value's map form: one flat leaf per scalar field.

  The form mirrors the struct's field tree, so a nested field takes a nested map."
  [form fields env]
  (let [entries (vec (lower-record-fields form fields env []))]
    (fragment (mapcat (comp :statements :fragment) entries)
              {:op :record
               :type :record
               :record-leaves (mapv #(select-keys % [:path :type]) entries)
               :values (mapv (comp :value :fragment) entries)})))

(defn- context-bindings
  "The per-instance context values every definition may reference, whether it is
  lowered as an entry or as a module: the frame count and the sample rate."
  [definition]
  (let [process (:process definition)
        frames (or (get-in process [:frames]) {:name 'frames :type :int})
        sample-rate (or (get-in process [:sample-rate])
                        {:name 'sample-rate :type :float})]
    (into {}
          [[(:name frames) (assoc frames :kind :frames)]
           [(:name sample-rate) (assoc sample-rate :kind :sample-rate)]])))

(defn- lane-name-bindings
  "A definition's `:lane` controls as reads of the entry process's lane region.

  A control that exists only as a lane has no scalar, so its *name* is its lane.
  In the entry that is how `process-bindings` binds it; in a callee it is the same
  read, of the region the callee was handed — which in a fused artifact is the
  slice its call site is (F4). A callee is also *passed* the caller's value for
  such a control, and this binding overrides it: the caller's slice is not the
  callee's, and wherever the artifact has one slice the two are the same read
  anyway. A lane the entry does not lay out keeps the parameter, as before."
  [definition lanes lane-capacity slice?]
  (into {}
        (keep (fn [control]
                (when (:lane control)
                  (when-let [index (first (keep-indexed
                                           (fn [index lane]
                                             (when (= (:name control) (:name lane))
                                               index))
                                           lanes))]
                    [(:name control)
                     (lane-binding (:name control) index lane-capacity slice?)]))))
        (get-in definition [:process :controls])))

(defn- base-bindings
  [definition state-bindings]
  (merge (into {}
               (concat
                (map (fn [parameter]
                       [(:name parameter) (assoc parameter :kind :param)])
                     (:params definition))
                state-bindings))
         (context-bindings definition)))

(defn- lower*
  "Lower one normalized descriptor to explicit function IR. The public entry
  point of this name binds the counter its generated names come from (D79).

  `:lanes` and `:lane-capacity` are the *entry process's* lane layout, not
  this definition's: every `(lane control)` in an artifact resolves against it,
  because that is the one region there is (F3). A definition whose body reads a
  lane, or that calls a definition which does, is marked `:lane-region?` — it is
  handed the region as an ABI parameter.

  `:lane-base?` says this definition is a *callee* of an artifact that renders
  several voices in one call (`poly`, F4), so the region it is handed is one
  voice's slice of it and its lane reads resolve against the base the call site
  supplies (`:lane-frame-offset`). The entry's own env instead carries
  `:lane-slots`, which is the fact that its slices belong to its call sites."
  ([definition] (lower* definition {}))
  ([definition {:keys [functions definition-id options modules
                       lanes lane-capacity lane-base?]}]
   (normalized-definition! definition)
   (let [return-type (:return-type definition)
         record-return (:return-record definition)
         stateful? (boolean (seq (:state definition)))
         ;; A module called through `with-buffers` writes the caller's regions,
         ;; not the caller's process ports (F2, D57). The linker only admits a
         ;; buffer access through that form, so any buffer access here is a region
         ;; write: the module's output buffers become *state* in its own call
         ;; partition, so the call site that owns the partition owns the region.
         ;; The width is the module's own: its `:process` declaration counts
         ;; (D40), otherwise the body is scanned. Only a *forced* top-level width
         ;; is taken from `options` -- `normalize-options` always injects a default
         ;; `:channels`, which would pin a module to one channel.
         module-buffers
         (when (and (:process definition)
                    (some buffer-access-form? (:body definition)))
           (let [{:keys [max-channel dynamic?]} (audio-channels-in (:body definition))
                 channels (cond
                            (some? (:channels options)) (:channels options)
                            (:channels-declared? (:process definition))
                            (get-in definition [:process :channels])
                            dynamic? (fail (str "a dynamic audio-buffer channel requires "
                                                "an explicit :channels")
                                           {:name (:name definition)})
                            :else (clojure.core/max 1 (inc (long max-channel))))]
             (mapv #(assoc % :type :float) (process-buffers definition channels))))
         ;; One region per output buffer: `channels` frame-planes of `stride`
         ;; frames. The stride is the lane/shared region stride, a compile-time
         ;; constant every backend shares, so a region is one flat state slot.
         ;; The region stride *is* the declared capacity: every per-frame region
         ;; is a whole number of blocks, and no host may render a longer one.
         region-stride (long (or (:frame-capacity options)
                                 descriptor/default-frame-capacity))
         region-states (mapv (fn [{:keys [id channels]}]
                               {:name id :type :float
                                :size (* (long channels) region-stride)
                                :init (vec (repeat (* (long channels) region-stride) 0.0))})
                             module-buffers)
         expanded (expand-state (into (effective-state definition) region-states))
         states (:slots expanded)
         ;; The module's region buffers, keyed by buffer id: a call-partition
         ;; state slot plus the channel stride an access folds in.
         region-buffers
         (into {}
               (map (fn [{:keys [id channels]}]
                      (let [binding (get (:bindings expanded) id)]
                        [id {:id id :region true :channels (long channels)
                             :state-id id :index (long (:index binding))
                             :size (long (:size binding)) :stride region-stride}])))
               module-buffers)
         env (assoc (environment (merge (base-bindings definition (:bindings expanded))
                                        ;; A lane-only control is a read of the
                                        ;; lane region wherever it is named, not
                                        ;; just in the entry (F4).
                                        (lane-name-bindings definition lanes lane-capacity
                                                            (boolean lane-base?)))
                                 (or functions {}) (or options {})
                                 region-buffers
                                 (or modules {}))
                    :return-record record-return
                    ;; The lane accessor resolves here too, against the entry's
                    ;; layout: a definition reached from a process reads the one
                    ;; lane region that process laid out (F3), or -- when that
                    ;; process is fused -- the slice of it this call site was
                    ;; handed (F4).
                    :lanes lanes
                    :lane-frame-capacity lane-capacity
                    :lane-base? (boolean lane-base?))
         ;; Call-site partitions are allocated after this module's own state.
         _ (reset! (:call-offset env) (state-total states))
         _ (reset! (:records-more env) (vec (:records expanded)))
         ;; The return is a scalar expression, or a map producing a record value;
         ;; `let`/`do` around it lower as ordinary forms. A process-shaped module
         ;; (`:return-type :void`) instead writes its own region buffers and
         ;; returns nothing (F2, D57).
         returned (lower-forms (:body definition) env)
         void? (= :void return-type)
         _ (when (and void? (:value returned)
                      (not= :void (:type (:value returned))))
             (fail "a void DSP definition must not produce a value"
                   {:body (:body definition) :type (:type (:value returned))}))
         _ (when (and (not void?) (not (:value returned)))
             (fail "DSP function body does not produce a value"
                   {:body (:body definition)}))
         _ (when-not void?
             (if record-return
               (when-not (= :record (:type (:value returned)))
                 (fail "DSP record-returning body must produce a record value"
                       {:body (:body definition) :type (:type (:value returned))}))
               (expect-type! return-type (:value returned) (:body definition))))
         ;; Snapshot the return before this module's transitions can clobber a
         ;; state slot the returning expression already read.
         [result-declarations return-value]
         (cond
           void? [[] nil]
           (not stateful?) [[] (:value returned)]
           :else (if record-return
                   (let [leaves (:record-leaves (:value returned))
                         pairs (mapv (fn [leaf expr]
                                       {:local (fresh-local!
                                                env
                                                (symbol (str "out_"
                                                             (str/join "_" (map name (:path leaf)))))
                                                (:type leaf))
                                        :expr expr})
                                     leaves (:values (:value returned)))]
                     [(mapv (fn [{:keys [local expr]}]
                              {:op :declare :name (:name local)
                               :type (:type local) :init expr})
                            pairs)
                      {:op :record :type :record :record-leaves leaves
                       :values (mapv (fn [{:keys [local]}]
                                       {:op :local :name (:name local) :type (:type local)})
                                     pairs)}])
                   (let [local (fresh-local! env "result" return-type)]
                     [[{:op :declare :name (:name local)
                        :type return-type :init (:value returned)}]
                      {:op :local :name (:name local) :type return-type}])))
         ;; A module applies its own scalar state transition at the end of its
         ;; body, so consecutive calls in a frame observe each other (D19).
         state-results
         (mapv (fn [state]
                 (when-let [next-form (:next state)]
                   (let [next (lower-value next-form env)
                         _ (expect-type! (:type state) (:value next) next-form)
                         local (fresh-local! env
                                             (str (name (:name state)) "_next")
                                             (:type state))]
                     {:state state
                      :statements (conj (:statements next)
                                        {:op :declare
                                         :name (:name local)
                                         :type (:type local)
                                         :init (:value next)})
                      :local local})))
               states)
         transitions (vec (concat (mapcat :statements (remove nil? state-results))
                                  (map (fn [{:keys [state local]}]
                                         {:op :state-store
                                          :state-id (:name state)
                                          :index (:index state)
                                          :type (:type state)
                                          :value {:op :local
                                                  :name (:name local)
                                                  :type (:type local)}
                                          :effects #{:write-state}})
                                       (remove nil? state-results))))
         body (block
               (vec
                (concat
                 (:statements returned)
                 result-declarations
                 transitions
                 (when return-value [{:op :return :value return-value}]))))
         ir {:op :function
             :name (:name definition)
             :id (or definition-id (:name definition))
             :params (:params definition)
             :return-type return-type
             :return-record record-return
             ;; The callee's own request to be copied into its call sites (D47),
             ;; carried so a target that can call can still honour it.
             :inline (boolean (:inline definition))
             :locals @(:locals env)
             ;; The arena is this module's own slots followed by one partition
             ;; per call site, all relative to this module's base.
             :state (into (mapv #(select-keys % [:name :type :size :init
                                                 :index :constant :special
                                                 :special-bit])
                                states)
                          @(:state-more env))
             :state-size @(:call-offset env)
             :call-partitions (vec @(:call-partitions env))
             :special-mask (special-mask definition)
             :special-count (count (special-bits definition))
             ;; The module's output buffers as regions (F2): read by a call site's
             ;; `with-buffers` to know what to bind. The region itself is *state*,
             ;; recorded below, so every backend addresses it already.
             :buffers (vec module-buffers)
             :region-slots region-buffers
             ;; Does this definition's own body read a lane? A definition that only
             ;; *calls* one that does is marked in `lower-definitions`, once the
             ;; callee is lowered and its answer is known. Naming a lane-only
             ;; control reads the region too: such a control has no scalar, so its
             ;; name *is* its lane (F4).
             :lane-access? (boolean (lane-access-form?
                                     (:body definition)
                                     (into #{} (comp (filter :lane) (map :name))
                                           (get-in definition [:process :controls]))))
             :records (vec @(:records-more env))
             :scratch (vec (:entries @(:scratch env)))
             :precision (:precision options)
             :options (or options {})
             :body body}]
     (assoc ir :effects (collect-effects body)))))

(defn lower
  "Lower one normalized descriptor, with deterministic generated names (D79)."
  ([definition] (lower* definition))
  ([definition opts] (names/with-fresh-names (fn [] (lower* definition opts)))))

(defn- called-ids
  "Every definition id a lowered body calls, at any depth in the IR."
  [ir]
  (let [found (volatile! #{})]
    (letfn [(walk [value]
              (cond
                (map? value)
                (do (when (= :call (:op value))
                      (vswap! found conj (:function-id value)))
                    (run! walk (vals value)))
                (sequential? value) (run! walk value)
                :else nil))]
      (walk (:body ir)))
    @found))

(defn- lower-definitions
  "Lower definitions in `order`, exposing each lowered callee to the ones that
  follow so call sites can allocate their state partitions.

  `lanes-context` is the entry process's lane layout, handed to every definition
  so a `(lane control)` read resolves to the one region's offsets (F3). When the
  entry is fused, that region is sliced one voice per call site: every callee that
  reads a lane is then handed the base of the slice its call site passes, so it
  is marked `:lane-base?` (F4) and each backend gives it one more ABI parameter."
  [definitions order options lanes-context]
  (let [fused? (> (long (or (:lane-slots lanes-context) 1)) 1)
        context (dissoc lanes-context :lane-slots)]
    (reduce (fn [lowered id]
              (let [ir (lower (get definitions id)
                              (merge {:functions (signatures definitions)
                                      :definition-id id
                                      :options options
                                      :modules lowered}
                                     context
                                     ;; Every definition reached from a fused
                                     ;; entry is handed the slice its call site
                                     ;; reads, so its lane reads resolve
                                     ;; against a base from the start (F4) --
                                     ;; whether it turns out to read one is what
                                     ;; the flags below settle afterwards.
                                     (when fused? {:lane-base? true})))
                    ;; A definition that calls one reading a lane hands that callee
                    ;; its own lane region, so it needs one too (F3).
                    lane-region? (or (:lane-access? ir)
                                     (boolean (some #(get-in lowered [% :lane-region?])
                                                    (called-ids ir))))]
                (assoc lowered id (cond-> ir
                                    lane-region? (assoc :lane-region? true)
                                    (and fused? lane-region?)
                                    (assoc :lane-base? true)))))
            (array-map)
            order)))

(defn lower-unit
  "Lower every descriptor in a linked compilation unit."
  ([unit] (lower-unit unit {}))
  ([unit {:keys [options]}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP IR lowering expects a linked compilation unit" {:unit unit}))
   (names/with-fresh-names
     (fn []
       (let [definitions (:definitions unit)
             lowered (lower-definitions definitions (dependency-order unit) options nil)
             entry (:entry unit)]
         (assoc (get lowered entry)
                :definitions lowered
                :entry-id entry
                :graph (:graph unit)))))))

(defn- memory-requirements
  "Summarize frame-relative buffer accesses for ABI validation.

  Constant offsets give callers an up-front minimum buffer length; dynamic
  offsets remain legal in portable IR and are checked by the interpreter at
  each access in development mode. Physical width is part of the ABI because
  f32 and f64 hosts require different typed views and alignment."
  [buffers body precision]
  (let [element-bytes (if (= :f64 precision) 8 4)
        initial (into {}
                      (map (fn [{:keys [id]}]
                             [id {:max-frame-offset 0
                                  :dynamic-index? false
                                  :element-bytes element-bytes
                                  :alignment element-bytes}])
                           buffers))]
    (letfn [(access [requirements {:keys [buffer-id index]}]
              (let [static-offset (case (:op index)
                                    :frame 0
                                    :frame-offset (:offset index)
                                    nil)]
                (if (some? static-offset)
                  (update-in requirements [buffer-id :max-frame-offset]
                             max static-offset)
                  (assoc-in requirements [buffer-id :dynamic-index?] true))))
            (walk [requirements value]
              (cond
                (map? value)
                (let [requirements (if (#{:buffer-load :buffer-store} (:op value))
                                     (access requirements value)
                                     requirements)]
                  (reduce walk requirements (vals value)))
                (sequential? value) (reduce walk requirements value)
                :else requirements))]
      (walk initial body))))

;; The byte distance between two lanes in the shared `:lanes` region. It is the
;; same stride the Wasm layout gives a channel, so a lane's base offset is a
(defn- process-input?
  "True when a definition's process has a sample input port.

  A definition with no `:process` map at all is still an ordinary block
  processor whose first parameter is the sample. A `:process` map whose `:input`
  normalized to nil is an explicit `:input false` generator (D23), which has no
  input parameter, no input buffer, and no input binding."
  [definition]
  (let [process (:process definition)]
    (if (nil? process)
      true
      (some? (:input process)))))

(defn- process-buffers
  [definition channels]
  (let [declared (get-in definition [:process :buffers])
        default (vec (concat (when (process-input? definition)
                               [{:name :input :direction :input :type :float
                                 :channels channels}])
                             [{:name :output :direction :output :type :float
                               :channels channels}]))]
    (mapv (fn [{:keys [name direction type] buffer-channels :channels}]
            {:id name
             :direction direction
             :type type
             ;; The audio ports are per-channel *by definition*, whatever the
             ;; descriptor happened to record: descriptor normalization runs
             ;; before a channel count is known, so a declared input or output
             ;; carries a placeholder. Any other buffer keeps its own channel
             ;; count, which is what makes a one-channel buffer a broadcast
             ;; buffer in a multi-channel process.
             :channels (if (contains? #{:input :output} direction)
                         channels
                         (or buffer-channels 1))})
          (or declared default))))

(defn- process-bindings
  [definition state-bindings frame-capacity]
  (let [process (:process definition)
        input? (process-input? definition)
        sample-name (when input?
                      (or (get-in process [:input :name])
                          (get-in definition [:params 0 :name])))
        sample-param (some #(when (= sample-name (:name %)) %) (:params definition))
        ;; A lane control is bound to a per-frame read from the shared `:lanes`
        ;; region, not to a block-rate scalar, so it leaves the scalar control
        ;; list entirely: the ABI reports it under `:lanes` instead. Lane *i*
        ;; occupies `[i * frame-capacity, ...)`, a compile-time base, which is
        ;; why this needs no new IR op. A lane belongs to a *control*, so a fused
        ;; entry that expanded a control per slot still has one lane for it: the
        ;; slots read it from different slices of the region (F4), which is what
        ;; `:lane-name` names.
        lane-controls (if process
                        (reduce (fn [lanes control]
                                  (let [name (:lane-name control (:name control))]
                                    (if (some #(= name (:name %)) lanes)
                                      lanes
                                      (conj lanes (assoc control :name name)))))
                                []
                                (filter #(or (:lane %) (:has-lane %))
                                        (:controls process)))
                        [])
        ;; A `:shared` control is read from the pool-wide `:shared` region rather
        ;; than from this instance's control storage, so it leaves the scalar
        ;; list the same way a `:lane` control does (M5). `:shared` and a lane
        ;; are mutually exclusive, so a shared control never has a scalar
        ;; beside it.
        shared-controls (if process
                          (filterv #(:shared %) (:controls process))
                          [])
        ;; A `:lane` control leaves the scalar list, so its lane *is* its name
        ;; and needs a binding. A `:has-lane` control keeps its scalar, so its
        ;; lane is reachable only through the `(lane control)` accessor, which
        ;; `lower-lane` resolves from the same lane list and indices.
        lane-bindings (into []
                            (keep-indexed
                             (fn [index control]
                               (when (:lane control)
                                 [(:name control)
                                  (lane-binding (:name control) index
                                                frame-capacity false)])))
                            lane-controls)
        ;; A shared control's name *is* a read of the shared region, exactly as a
        ;; lane-only control's name is a read of the lane region.
        shared-bindings (into []
                              (keep-indexed
                               (fn [index control]
                                 (when (:shared control)
                                   [(:name control)
                                    (shared-binding (:name control) index
                                                    frame-capacity)])))
                              shared-controls)
        controls (if process
                   (mapv (fn [{:keys [name type] :as control}]
                           (let [param (some #(when (= name (:name %)) %)
                                             (:params definition))]
                             ;; `:default` is exposed so a host can infer a
                             ;; parameter's default from the control's.
                             (cond-> (merge param (select-keys control [:smooth :slew :default]))
                               ;; A control port that declares a non-default type
                               ;; (e.g. :int) types the local, since the shorthand
                               ;; parameter list can only express :float.
                               (and type (not= :float type)) (assoc :type type))))
                         (remove #(or (:lane %) (:shared %)) (:controls process)))
                   (vec (rest (:params definition))))
        frames (or (get-in process [:frames]) {:name 'frames :type :int})
        sample-rate (or (get-in process [:sample-rate])
                        {:name 'sample-rate :type :float})
        ;; One evaluation per frame (D40): there is no channel loop, so the
        ;; implicit channel is always 0. A multi-channel process indexes the
        ;; output buffer explicitly.
        channel 0
        index {:op :frame :type :int}
        sample-expression {:op :buffer-load
                           :buffer-id :input
                           :channel channel
                           :index index
                           :type :float
                           :effects #{:read-memory}}]
    (when (and input? (not sample-param))
      (fail "a DSP process requires a sample input parameter"
            {:name (:name definition) :input sample-name}))
    (when (some nil? controls)
      (fail "DSP process controls must refer to parameters"
            {:name (:name definition) :controls (:controls process)}))
    (when (and input? (not= :float (:type sample-param)))
      (fail "DSP process sample input must have type :float"
            {:name (:name definition) :input sample-name :type (:type sample-param)}))
    {:sample-name sample-name
     :channel channel
     :controls controls
     :frames frames
     :sample-rate sample-rate
     ;; Only a `:lane` region is mandatory; a `:has-lane` lane the host never
     ;; fills reads zero, so a process made only of those is valid unlaned. A
     ;; `:shared` region is mandatory whenever the process declares a shared
     ;; control: unlike a lane, the region *is* the control's value.
     :lanes (mapv (fn [control]
                    (assoc control :required? (boolean (:lane control))))
                  lane-controls)
     :shared (mapv (fn [control] (assoc control :required? true))
                   shared-controls)
     :bindings (into
                (if input?
                  {sample-name {:name sample-name
                                :type :float
                                :kind :buffer
                                :expression sample-expression}}
                  {})
                (concat
                 (context-bindings definition)
                 (map (fn [control]
                        [(:name control) (assoc control :kind :control)])
                      controls)
                 lane-bindings
                 shared-bindings
                 state-bindings))}))

(defn- process-diagnostics
  "Return portable safety diagnostics for a process body.

  These are intentionally data, not backend exception strings. Dynamic indices
  remain explicit because their bounds are host/runtime responsibilities; the
  interpreter validates accesses at runtime in development mode."
  [frame-body memory]
  (letfn [(walk [value path diagnostics]
            (cond
              (map? value)
              (let [dynamic-index? (let [index (:element-index value)]
                                     (and index (not= :const (:op index))))
                    dynamic-array? (and (#{:array-load :array-store} (:op value))
                                        dynamic-index?)
                    dynamic-state? (and (#{:state-load :state-store} (:op value))
                                        (some? (:state-id value))
                                        dynamic-index?)
                    diagnostics (cond-> diagnostics
                                  (and (= :while (:op value))
                                       (nil? (:bound value)))
                                  (conj {:dsp/warning :unbounded-loop
                                         :dsp/path path})
                                  dynamic-array?
                                  (conj {:dsp/warning :dynamic-array-index
                                         :array-id (:array-id value)
                                         :dsp/path path})
                                  dynamic-state?
                                  (conj {:dsp/warning :dynamic-state-index
                                         :state-id (:state-id value)
                                         :dsp/path path}))]
                (reduce-kv (fn [diagnostics key child]
                             (walk child (conj path key) diagnostics))
                           diagnostics value))

              (sequential? value)
              (reduce-kv (fn [diagnostics index child]
                           (walk child (conj path index) diagnostics))
                         diagnostics (vec value))

              :else diagnostics))]
    (into (vec (walk frame-body [:frame-body] []))
          (keep (fn [[buffer-id {:keys [dynamic-index?]}]]
                  (when dynamic-index?
                    {:dsp/warning :dynamic-buffer-index
                     :buffer-id buffer-id
                     :dsp/path [:memory buffer-id]}))
                memory))))

(defn- annotate-buffer-channeling
  "Mark each buffer access with whether it is subscripted by channel.

  A buffer is per-channel only if it declares more than one channel, and the
  audio ports always take the process channel count. Recording this on the
  access itself means no backend has to re-derive it — and a lane region, which
  is always flat, is never subscripted even in a multi-channel process."
  [frame-body per-channel-buffers]
  (walk/postwalk
   (fn [value]
     (if (and (map? value)
              (contains? #{:buffer-load :buffer-store} (:op value)))
       (assoc value :per-channel? (contains? per-channel-buffers (:buffer-id value)))
       value))
   frame-body))

(defn- audio-buffer?
  [id]
  (contains? #{:input :output} id))

(defn- call-channel
  "The channel form of a `buffer-load`/`buffer-store` call; nil means channel 0
  (the one-offset shorthand omits it)."
  [op args]
  (case op
    buffer-load (when (= 2 (count args)) (first args))
    buffer-store (when (= 3 (count args)) (first args))
    nil))

(defn- audio-channels-in
  "The highest constant channel a body addresses on an `:input`/`:output` buffer,
  and whether any such channel is dynamic (so the width cannot be counted).

  Only the entry body addresses the audio buffers — a callee is a function with
  no buffer namespace — so this syntactic scan is the whole answer (D40)."
  [form]
  (let [max-channel (volatile! -1)
        dynamic? (volatile! false)]
    (letfn [(walk [form]
              (if (seq? form)
                (do
                  (let [op (first form)]
                    (when (and (contains? '#{buffer-load buffer-store} op)
                               (audio-buffer? (second form)))
                      (let [channel (call-channel op (nthrest form 2))]
                        (cond
                          (nil? channel) (vswap! max-channel max 0)
                          (number? channel) (vswap! max-channel max (long channel))
                          :else (vreset! dynamic? true)))))
                  (run! walk form))
                (cond
                  (vector? form) (run! walk form)
                  (map? form) (run! (fn [[key value]] (walk key) (walk value)) form)
                  :else nil)))]
      (walk form)
      {:max-channel @max-channel :dynamic? @dynamic?})))

(defn- lower-process*
  "Lower the entry descriptor directly to explicit process IR. The public entry
  point of this name binds the counter its generated names come from (D79).

  The sample parameter is bound to a canonical input-buffer load while controls
  and state retain their own binding kinds. No scalar IR rewriting is involved.

  The audio width is the declared `:channels`, or otherwise **counted** from the
  body's `:input`/`:output` accesses (highest channel + 1), so a stereo body
  needs no declaration (D40)."
  ([unit] (lower-process* unit {}))
  ([unit {:keys [channels options]}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP process lowering expects a linked compilation unit" {:unit unit}))
   (let [definitions (:definitions unit)
         order (dependency-order unit)
         entry-id (:entry unit)
         definition (get definitions entry-id)
         _ (normalized-definition! definition)
         ;; Count the audio width from the body when the caller did not declare
         ;; it: one more than the highest channel an `:input`/`:output` access
         ;; names, or 1 for a scalar body (D40).
         {:keys [max-channel dynamic?]} (audio-channels-in (:body definition))
         channels (cond
                    (some? channels) channels
                    dynamic? (fail "a dynamic audio-buffer channel requires an explicit :channels"
                                   {:name (:name definition)})
                    :else (clojure.core/max 1 (inc (long max-channel))))
         _ (when-not (and (integer? channels) (pos? channels))
             (fail "DSP process channels must be a positive integer"
                   {:channels channels}))
         expanded (expand-state (effective-state definition))
         states (:slots expanded)
         frame-capacity (long (or (:frame-capacity options)
                                  descriptor/default-frame-capacity))
         ;; A fused entry renders `poly`'s voices in one call, so the one lane
         ;; region it owns holds one slice per voice and each call site reads its
         ;; own (F4). The fact is the definition's, marked where the fusion
         ;; happens; a plain artifact has exactly one slice.
         lane-slots (long (or (:dsp/lane-slots definition) 1))
         {:keys [bindings controls lanes shared frames sample-rate]}
         (process-bindings definition (:bindings expanded) frame-capacity)
         ;; Callees are lowered first, in dependency order, so every call site
         ;; can allocate its state partition (D19). The entry's lane layout goes
         ;; with them: the region is laid out once, here, so a callee's
         ;; `(lane control)` reads an offset this process fixed (F3), inside the
         ;; slice this process's call site hands it (F4).
         modules (lower-definitions definitions (remove #(= entry-id %) order)
                                    options
                                    {:lanes lanes :lane-capacity frame-capacity
                                     :lane-slots lane-slots})
         buffers (process-buffers definition channels)
         ;; A buffer is subscripted by channel only when it is per-channel.
         ;; Input and output are (they are channel-strided); a lane region is
         ;; not, and a one-channel buffer in a multi-channel process is a
         ;; *broadcast* buffer — one lane can drive both channels. Recording the
         ;; fact here keeps the four backends from each guessing at it.
         per-channel-buffers (set (keep #(when (> (:channels %) 1) (:id %)) buffers))
         ;; The entry arena is its own state, then one partition per call site,
         ;; then local-array scratch. Partition sizes are static, so scratch
         ;; starts after the whole state region.
         total-state (get (module-state-sizes definitions order) entry-id)
         env (-> (environment bindings (signatures definitions) (or options {})
                              (into {} (map (juxt :id identity) buffers))
                              modules)
                 ;; The lane accessor resolves here, where the region's offsets
                 ;; are known. The same layout reaches every callee (F3), so a
                 ;; `(lane c)` means the same slice wherever it is read -- and in
                 ;; a fused entry the slice a read lands in is the call site's,
                 ;; so the entry's own body has none (F4).
                 (assoc :lanes lanes :lane-frame-capacity frame-capacity)
                 (cond-> (> lane-slots 1) (assoc :lane-slots lane-slots)))
         _ (reset! (:call-offset env) (state-total states))
         _ (reset! (:records-more env) (vec (:records expanded)))
         _ (reset! (:scratch env) {:next total-state :entries []})
         output (lower-forms (:body definition) env)
         ;; A `:void` value is no value: a body that ends in a process-shaped
         ;; call has written its buffers and has nothing to store (F2).
         scalar-output? (and (:value output) (not= :void (:type (:value output))))
         _ (when scalar-output?
             (expect-type! :float (:value output) (:body definition)))
         ;; A scalar body is the one-channel sugar: lowering stores it to channel
         ;; 0. A `:void` body has written `:output` itself, per channel (D40).
         output-local (when scalar-output? (fresh-local! env "output" :float))
         output-declaration (when output-local
                              {:op :declare
                               :name (:name output-local)
                               :type :float
                               :init (:value output)})
         output-store (when output-local
                        {:op :buffer-store
                         :buffer-id :output
                         :channel 0
                         :index {:op :frame :type :int}
                         :value {:op :local :name (:name output-local) :type :float}
                         :effects #{:write-memory}})
         state-results
         (mapv (fn [state]
                 (when-let [next-form (:next state)]
                   (let [next (lower-value next-form env)
                         _ (expect-type! (:type state) (:value next) next-form)
                         local (fresh-local! env
                                             (str (name (:name state)) "_next")
                                             (:type state))]
                     {:state state
                      :statements (conj (:statements next)
                                        {:op :declare
                                         :name (:name local)
                                         :type (:type local)
                                         :init (:value next)})
                      :local local})))
               states)
         frame-statements
         (vec
          (concat
           (:statements output)
           (when output-declaration [output-declaration])
           (mapcat :statements (remove nil? state-results))
           (when output-store [output-store])
           (map (fn [{:keys [state local]}]
                  {:op :state-store
                   :state-id (:name state)
                   :index (:index state)
                   :type (:type state)
                   :value {:op :local :name (:name local) :type (:type local)}
                   :effects #{:write-state}})
                (remove nil? state-results))))
         frame-body (annotate-buffer-channeling (block frame-statements)
                                                per-channel-buffers)
         memory (memory-requirements buffers frame-body (:precision options))
         diagnostics (process-diagnostics frame-body memory)
         ;; A callee may publish a special of its own (M6, seam 4): a voice's
         ;; `:activity` finish signal lives in the call partition the fused slot
         ;; resets. The partition covers its whole subtree, so the entry records
         ;; which specials each call site owns, one per bit, and a fused host can
         ;; read a slot's activity. The entry's own specials stay the entry's
         ;; contract (`:special-mask`/`:special-count` above).
         call-partitions
         (mapv (fn [{:keys [base size] :as partition}]
                 (let [specials (partition-specials @(:state-more env) base size)]
                   (cond-> partition
                     (seq specials) (assoc :specials specials
                                           :special-mask
                                           (special-mask-of specials)))))
               @(:call-partitions env))
         ir {:op :process
             :name (:name definition)
             :id entry-id
             :channels channels
             :buffers buffers
             :memory memory
             :diagnostics diagnostics
             :frames frames
             :sample-rate sample-rate
             :controls controls
             :lanes lanes
             :lane-slots lane-slots
             :shared shared
             :frame-capacity frame-capacity
             :locals @(:locals env)
             :state (into (mapv #(select-keys % [:name :type :size :init :index :constant
                                                 :special :special-bit])
                                states)
                          @(:state-more env))
             :state-size @(:call-offset env)
             :call-partitions call-partitions
             :special-mask (special-mask definition)
             :special-count (count (special-bits definition))
             :records (vec @(:records-more env))
             :scratch (vec (:entries @(:scratch env)))
             :precision (:precision options)
             :lifecycle (merge {:init true :reset true}
                               (get-in definition [:process :lifecycle]))
             :options (or options {})
             :definitions modules
             :entry-id entry-id
             :graph (:graph unit)
             :frame-body frame-body}]
     (assoc ir :effects (collect-effects frame-body)))))

(defn lower-process
  "Lower the entry descriptor directly, with deterministic generated names."
  ([unit] (lower-process* unit))
  ([unit opts] (names/with-fresh-names (fn [] (lower-process* unit opts)))))

(def ^:private inlining-statement-limit
  "Conservative bound on the statements an expansion may add. Composition is
  inlined, not called, and recursion is rejected at link time, so growth is
  finite; this budget turns a pathological graph into a clear diagnostic rather
  than a runaway artifact."
  20000)

(declare inline-call inline-statement)

(defn- expression-body?
  [statements]
  (and (= 1 (count statements))
       (= :return (:op (first statements)))))

(defn- rename-local
  [renames name]
  (get renames name name))

(defn- rebase-expression
  "Rename a module's locals, substitute its params, and rebase its state.

  Parameter references become the call site's already-hoisted argument values,
  which are pure, so duplicating them is safe. State indices move into the call
  site's partition, and a nested call's base is shifted with them."
  [expr renames substitutions base]
  (case (:op expr)
    :local (or (get substitutions (:name expr))
               (assoc expr :name (rename-local renames (:name expr))))
    :state-load (cond-> (update expr :index + base)
                  (:element-index expr)
                  (update :element-index
                          rebase-expression renames substitutions base))
    :binary (assoc expr
                   :left (rebase-expression (:left expr) renames substitutions base)
                   :right (rebase-expression (:right expr) renames substitutions base))
    :compare (assoc expr
                    :left (rebase-expression (:left expr) renames substitutions base)
                    :right (rebase-expression (:right expr) renames substitutions base))
    :logical (update expr :args
                     #(mapv (fn [a] (rebase-expression a renames substitutions base)) %))
    :bitwise (update expr :args
                     #(mapv (fn [a] (rebase-expression a renames substitutions base)) %))
    :builtin (update expr :args
                     #(mapv (fn [a] (rebase-expression a renames substitutions base)) %))
    :convert (update expr :value rebase-expression renames substitutions base)
    :frame-plus (update expr :offset rebase-expression renames substitutions base)
    :project (update expr :value rebase-expression renames substitutions base)
    :record (update expr :values
                    #(mapv (fn [v] (rebase-expression v renames substitutions base)) %))
    ;; A scratch (`:array-*`) offset is arena-absolute, so only its element
    ;; index can hold locals; a state offset moves into the partition.
    :array-load (cond-> expr
                  (:element-index expr)
                  (update :element-index
                          rebase-expression renames substitutions base))
    :buffer-load (update expr :index rebase-expression renames substitutions base)
    :call (cond-> (update expr :args
                          #(mapv (fn [a] (rebase-expression a renames substitutions base)) %))
            (some? (:state-base expr)) (update :state-base + base))
    expr))

(defn- rebase-statement
  [statement renames substitutions base]
  (let [expr #(rebase-expression % renames substitutions base)
        stmt #(rebase-statement % renames substitutions base)]
    (case (:op statement)
      :block (update statement :statements #(mapv stmt %))
      :declare (cond-> (assoc statement :name (rename-local renames (:name statement)))
                 (:init statement) (update :init expr))
      :assign (-> statement
                  (assoc :name (rename-local renames (:name statement)))
                  (update :value expr))
      :expression (update statement :value expr)
      :return (update statement :value expr)
      :buffer-store (update statement :value expr)
      :state-store (-> statement
                       (update :index + base)
                       (cond-> (:element-index statement)
                         (update :element-index expr))
                       (update :value expr))
      :array-store (-> statement
                       (cond-> (:element-index statement)
                         (update :element-index expr))
                       (update :value expr))
      :if (-> statement
              (update :condition expr)
              (update :then stmt)
              (update :else stmt))
      :while (-> statement
                 (update :condition expr)
                 (update :body stmt))
      statement)))

(defn- inline-value
  "Inline every call in an expression, hoisting the side effects it introduces."
  [expr definitions state]
  (case (:op expr)
    :call (if (contains? (:inline-ids @state) (:function-id expr))
            (inline-call expr definitions state)
            ;; The callee keeps being called (D47). Its arguments still belong
            ;; to the caller, so they are inlined like any other expression of
            ;; the caller's body; the callee receives values.
            (let [args (mapv #(inline-value % definitions state) (:args expr))]
              {:statements (vec (mapcat :statements args))
               :value (assoc expr :args (mapv :value args))}))
    :binary (let [left (inline-value (:left expr) definitions state)
                  right (inline-value (:right expr) definitions state)]
              {:statements (into (:statements left) (:statements right))
               :value (assoc expr :left (:value left) :right (:value right))})
    :compare (let [left (inline-value (:left expr) definitions state)
                   right (inline-value (:right expr) definitions state)]
               {:statements (into (:statements left) (:statements right))
                :value (assoc expr :left (:value left) :right (:value right))})
    :logical (let [args (mapv #(inline-value % definitions state) (:args expr))]
               {:statements (vec (mapcat :statements args))
                :value (assoc expr :args (mapv :value args))})
    :bitwise (let [args (mapv #(inline-value % definitions state) (:args expr))]
               {:statements (vec (mapcat :statements args))
                :value (assoc expr :args (mapv :value args))})
    :builtin (let [args (mapv #(inline-value % definitions state) (:args expr))]
               {:statements (vec (mapcat :statements args))
                :value (assoc expr :args (mapv :value args))})
    :convert (let [value (inline-value (:value expr) definitions state)]
               {:statements (:statements value)
                :value (assoc expr :value (:value value))})
    :project (if (and (= :local (:op (:value expr)))
                      (get-in @state [:records (:name (:value expr))]))
               (if-let [local (get-in @state [:records (:name (:value expr)) (:path expr)])]
                 {:statements []
                  :value {:op :local :name local :type (:type expr)}}
                 (fail "DSP record value has no such field" {:path (:path expr)}))
               (let [value (inline-value (:value expr) definitions state)]
                 {:statements (:statements value)
                  :value (assoc expr :value (:value value))}))
    :record (let [values (mapv #(inline-value % definitions state) (:values expr))]
              {:statements (vec (mapcat :statements values))
               :value (assoc expr :values (mapv :value values))})
    :frame-plus (let [offset (inline-value (:offset expr) definitions state)]
                  {:statements (:statements offset)
                   :value (assoc expr :offset (:value offset))})
    {:statements [] :value expr}))

(defn- inline-call
  [expr definitions state]
  (let [callee (get definitions (:function-id expr))]
    (when-not callee
      (fail "DSP call refers to an unlinked function"
            {:function-id (:function-id expr)}))
    (let [args (mapv #(inline-value % definitions state) (:args expr))
          arg-statements (vec (mapcat :statements args))
          arg-values (mapv :value args)
          substitutions (into {}
                              (map (fn [parameter argument]
                                     [(:name parameter) argument])
                                   (:params callee) arg-values))
          statements (get-in callee [:body :statements])]
      (if (expression-body? statements)
        ;; State-free callee: substitute its return expression. The partition
        ;; base is still applied, because the expression may call stateful
        ;; modules whose own partitions are relative to this module's base.
        (let [base (or (:state-base expr) 0)
              value (rebase-expression (get-in statements [0 :value]) {}
                                       substitutions base)
              result (inline-value value definitions state)]
          {:statements (into arg-statements (:statements result))
           :value (:value result)})
        ;; A stateful module: rename its locals, rebase its state into this call
        ;; site's partition, and splice its statements in place (D19).
        (let [base (or (:state-base expr) 0)
              renames (into {}
                            (map (fn [local]
                                   [(:name local)
                                    (symbol (str "inl__"
                                                 (:names (swap! state update :names inc))
                                                 "__" (name (:name local))))]))
                            (:locals callee))
              body-nodes (pop statements)
              return-node (peek statements)
              rewritten (vec (mapcat (fn [node]
                                       (inline-statement
                                        (rebase-statement node renames substitutions base)
                                        definitions state))
                                     body-nodes))
              return-value (rebase-expression (:value return-node)
                                              renames substitutions base)
              result (inline-value return-value definitions state)
              _ (swap! state update :statements + (count rewritten))
              _ (when (> (:statements @state) inlining-statement-limit)
                  (fail "DSP inlining exceeded the code-size budget"
                        {:limit inlining-statement-limit
                         :function-id (:function-id expr)}))]
          {:statements (into arg-statements (into rewritten (:statements result)))
           :value (:value result)})))))

(defn- inline-statement
  "Inline a statement, returning the hoisted statement vector that replaces it."
  [statement definitions state]
  (let [expr #(inline-value % definitions state)]
    (case (:op statement)
      :block [(assoc statement
                     :statements (vec (mapcat #(inline-statement % definitions state)
                                              (:statements statement))))]
      :declare (if-let [init (:init statement)]
                 (let [value (expr init)]
                   (if (= :record (:op (:value value)))
                     ;; A multi-output result: one local per leaf, plus a mapping
                     ;; from the bound name to those locals for projections.
                     (let [record (:value value)
                           base-name (:name statement)
                           locals (mapv (fn [leaf]
                                          {:path (:path leaf)
                                           :name (symbol (str base-name "__"
                                                              (str/join "_"
                                                                        (map name (:path leaf)))))
                                           :type (:type leaf)})
                                        (:record-leaves record))]
                       (swap! state assoc-in [:records base-name]
                              (into {} (map (fn [l] [(:path l) (:name l)])) locals))
                       (into (vec (:statements value))
                             (mapv (fn [local leaf-value]
                                     {:op :declare :name (:name local)
                                      :type (:type local) :init leaf-value})
                                   locals (:values record))))
                     (conj (:statements value) (assoc statement :init (:value value)))))
                 [statement])
      :assign (let [value (expr (:value statement))]
                (conj (:statements value) (assoc statement :value (:value value))))
      :expression (let [value (expr (:value statement))]
                    (conj (:statements value) (assoc statement :value (:value value))))
      :return (let [value (expr (:value statement))]
                (conj (:statements value) (assoc statement :value (:value value))))
      :buffer-store (let [value (expr (:value statement))]
                      (conj (:statements value) (assoc statement :value (:value value))))
      :state-store (let [value (expr (:value statement))]
                     (conj (:statements value) (assoc statement :value (:value value))))
      :if (let [condition (expr (:condition statement))]
            (conj (:statements condition)
                  (assoc statement
                         :condition (:value condition)
                         :then (first (inline-statement (:then statement)
                                                        definitions state))
                         :else (when-let [else (:else statement)]
                                 (first (inline-statement else definitions state))))))
      :while (let [condition (expr (:condition statement))]
               (conj (:statements condition)
                     (assoc statement
                            :condition (:value condition)
                            :body (first (inline-statement (:body statement)
                                                           definitions state)))))
      [statement])))

(defn- declared-locals
  "Every local a statement tree declares, in walk order. Inlining introduces
  locals the enclosing descriptor never saw, so the arena's local layout is
  rebuilt from the expanded body."
  [statement]
  (case (:op statement)
    :block (mapcat declared-locals (:statements statement))
    :declare [{:name (:name statement) :type (:type statement)}]
    :if (concat (declared-locals (:then statement))
                (when (:else statement) (declared-locals (:else statement))))
    :while (declared-locals (:body statement))
    nil))

(defn- inline-definition
  "Inline the requested callees inside a definition that stays a function.

  A definition the entry keeps is still emitted, and it may itself call one that
  was inlined, so its body gets the same treatment (D47). Record bindings are
  per body: a local name never crosses between definitions."
  [definition definitions state]
  (swap! state assoc :records {})
  (let [body (first (inline-statement (:body definition) definitions state))]
    (assoc definition
           :body body
           :locals (vec (distinct (declared-locals body)))
           :effects (collect-effects body))))

(defn- uncallable-definition?
  "Whether the calling convention cannot take this definition (D47).

  A call produces one value, so a **record return** cannot be a call. A
  definition with **scratch arrays** cannot either: `lower-process` seeds the
  entry's scratch offsets after the whole state region, but the function path
  never resets them, so a callee's arrays start at 0 and overlap its own state —
  and a call partition covers state only. Both stay inlined at every call site."
  [definition]
  (or (= :record (:return-type definition))
      (boolean (seq (:scratch definition)))))

(defn uncallable-definition-ids
  "The definitions a backend that emits calls must still inline (D47).

  `dsp/compile` adds these to whatever the definitions asked for, so the set a
  target copies is `requested ∪ uncallable`."
  [ir]
  (into #{}
        (keep (fn [[id definition]]
                (when (uncallable-definition? definition) id)))
        (:definitions ir)))

(defn inline-calls
  "Inline the linked callees named in `inline-ids` for the source-emitting
  backends.

  Pure callees are substituted. A stateful module is expanded with its locals
  renamed and its state rebased into the call site's partition (D19), so the
  JVM, JavaScript, and Wasm backends share one composition implementation.

  A definition outside `inline-ids` keeps being called and stays in
  `:definitions`, so a target that can call does not copy a definition it was
  not asked to copy (D47). Its body is still scanned, because it may call a
  definition that *is* inlined. With one argument every definition is inlined,
  which is what a target that cannot call needs."
  ([ir] (inline-calls ir (set (keys (:definitions ir)))))
  ([ir inline-ids]
   (let [definitions (:definitions ir)
         body-key (if (= :process (:op ir)) :frame-body :body)
         ;; The entry is compiled on its own, so it is never also a callee. A
         ;; process IR already excludes it; a function IR does not.
         entry-id (:entry-id ir)
         inline-ids (set inline-ids)
         state (atom {:names 0 :statements 0 :records {}
                      :inline-ids inline-ids})
         body (first (inline-statement (get ir body-key) definitions state))
         retained (reduce-kv (fn [kept id definition]
                               (if (or (contains? inline-ids id)
                                       (= id entry-id))
                                 kept
                                 (assoc kept id (inline-definition definition
                                                                   definitions
                                                                   state))))
                             (array-map)
                             definitions)
         ir (assoc ir body-key body
                   :definitions retained
                   :locals (vec (distinct (declared-locals body))))]
     (assoc ir :effects (collect-effects body)))))

(defn validate-runtime-policy!
  "Validate effect annotations and return static real-time policy metadata."
  [ir]
  (let [maps (filter map? (tree-seq coll? seq ir))
        unknown (into #{} (mapcat #(remove effect-types (or (:effects %) #{})) maps))]
    (when (seq unknown)
      (throw (ex-info "DSP IR contains unknown effects"
                      {:dsp/error :invalid-ir
                       :unknown-effects unknown})))
    {:allocations? false
     :locks? false
     :reflection? false
     :io? false
     :effects (:effects ir)}))

(defn expression-operators
  []
  (into (set (keys arithmetic-operators))
        (concat (keys comparison-operators)
                (keys logical-operators)
                (keys bitwise-operators)
                builtins/operators
                ['if 'let 'do 'set! 'while 'break 'continue
                 'buffer-load 'buffer-store 'state-load 'state-store 'store-special
                 'int->float 'float->int])))
