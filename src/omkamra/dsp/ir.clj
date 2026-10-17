(ns omkamra.dsp.ir
  "Typed, target-neutral IR for omkamra.dsp.

  Source expressions lower to pure value nodes plus explicit statements. Buffer
  declarations live on process roots; memory operations refer to those buffers
  by ID. Effects are attached only to inherently effectful nodes and summarized
  on function/process roots."
  (:require [clojure.string :as str]
            [omkamra.dsp.builtins :as builtins]))

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
    (integer? form) {:op :const :type :int :value (int form)}
    ;; Keep the full double. Narrowing happens per precision at use: the
    ;; interpreter's `:const` and each backend's constant emission. Narrowing
    ;; here would lose f64 precision and, because a Clojure `Float` prints a
    ;; short decimal, would not round-trip through the EDN served to the
    ;; browser.
    (number? form) {:op :const :type :float :value (double form)}
    (boolean? form) {:op :const :type :boolean :value form}
    :else nil))

(declare lower-value lower-form lower-forms array-form? array-declaration
         lower-place-store! rebase-slots rebase-records record-leaves
         lower-record-value)

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
  "The `:array-ref` for a record's single array field, or nil."
  [binding]
  (let [buffers (filterv #(> (:size %) 1) (:leaves binding))]
    (when (= 1 (count buffers))
      (let [leaf (first buffers)]
        {:op :array-ref :type :array :region :state
         :state-id (:name binding)
         :base (:offset leaf) :element (:type leaf) :size (:size leaf)
         :constant (boolean (:constant binding))}))))

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

(defn- lower-binary
  [form env operator comparison?]
  (let [[_ left-form right-form & extra] form]
    (when (or (nil? left-form) (nil? right-form) extra)
      (fail "DSP binary operators require two operands"
            {:form form :operator (first form)}))
    (let [left (lower-value left-form env)
          right (lower-value right-form env)
          left-value (:value left)
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
        :type (if comparison? :boolean type)}))))

(defn- lower-logical
  [form env operator]
  (let [forms (vec (rest form))
        expected (if (= :not operator) 1 2)]
    (when-not (= expected (count forms))
      (fail "DSP logical operators have the wrong arity"
            {:form form :operator (first form)}))
    (let [args (mapv #(lower-value % env) forms)]
      (doseq [arg args]
        (expect-type! :boolean (:value arg) form))
      (combine-fragments args
                         {:op :logical
                          :operator operator
                          :args (mapv :value args)
                          :type :boolean}))))

(defn- lower-bitwise
  [form env operator]
  (let [forms (vec (rest form))
        expected (if (contains? unary-bitwise-operators operator) 1 2)]
    (when-not (= expected (count forms))
      (fail "DSP bitwise operators have the wrong arity"
            {:form form :operator (first form)
             :expected expected :actual (count forms)}))
    (let [args (mapv #(lower-value % env) forms)]
      (doseq [arg args]
        (expect-type! :int (:value arg) form))
      (combine-fragments args
                         {:op :bitwise
                          :operator operator
                          :args (mapv :value args)
                          :type :int}))))

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
        (when-not (valid-name? target)
          (fail "DSP set! requires a local name or a field place" {:form form}))
        (when-not (= :local (:kind binding))
          (fail "DSP set! can only update mutable locals" {:form form :name target}))
        (let [value (lower-value value-form env)]
          (expect-type! (:type binding) (:value value) form)
          (fragment (conj (:statements value)
                          {:op :assign
                           :name target
                           :type (:type binding)
                           :value (:value value)
                           :effects #{:write-local}})
                    {:op :local :name target :type (:type binding)}))))))

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

(defn- lower-buffer-load
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset] (channel-and-offset form args)
        buffer (buffer-access! form env buffer-id #{:input :output})
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
      :effects #{:read-memory}})))

(defn- lower-buffer-store
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset value-form]
        (case (count args)
          2 [0 (first args) (second args)]
          3 [(first args) (second args) (nth args 2)]
          (fail "DSP buffer-store requires offset, value, or channel, offset, value"
                {:form form}))
        buffer (buffer-access! form env buffer-id #{:output})
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
               :effects #{:write-memory}}]))))

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

(defn- lower-array-load
  [form env]
  (let [[_ target index-form & extra] form]
    (when (or (nil? target) (nil? index-form) extra)
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
                   :effects #{:read-state}})))))

(defn- lower-array-store
  [form env]
  (let [[_ target index-form value-form & extra] form]
    (when (or (nil? target) (nil? index-form) (nil? value-form) extra)
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
                   :effects #{:write-state}}]))))))

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

(defn- field-leaf!
  "Resolve a parsed field access to `{:leaf ... :index ...}`.

  For an array of records the index is the record index; for a scalar record
  owning an array field it is the field's element index. An array field with no
  element index is an array place; see `leaf-array-ref`."
  [form env binding parsed]
  (let [path (:path parsed)
        leaf (get (:leaf-index binding) path)]
    (when-not leaf
      (fail "DSP field path does not exist in the record"
            {:form form :state (:name binding) :path path}))
    (let [record-array? (> (:size binding) 1)
          array-field? (> (:size leaf) 1)
          record-index (:record-index parsed)
          element-index (:element-index parsed)]
      (when (and (not record-array?) record-index)
        (fail "a scalar record has no record index"
              {:form form :state (:name binding) :path path}))
      (when (and record-array? element-index)
        (fail "an array of records has no per-field element index"
              {:form form :state (:name binding) :path path}))
      (when (and (not array-field?) element-index)
        (fail "scalar field access does not take an element index"
              {:form form :state (:name binding) :path path}))
      (let [index-form (if record-array? record-index element-index)
            index (when index-form (lower-value index-form env))]
        (when index (expect-type! :int (:value index) form))
        {:leaf leaf :index index}))))

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
            {:keys [leaf index]} (field-leaf! form env binding
                                              (field-selectors form selectors))]
        (if (and (> (:size leaf) 1) (nil? index))
          (fragment [] (leaf-array-ref binding leaf))
          (fragment (:statements index)
                    {:op :state-load
                     :state-id (:name binding)
                     :index (:offset leaf)
                     :element-index (:value index)
                     :size (:size leaf)
                     :type (:type leaf)
                     :effects #{:read-state}}))))))

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
          {:keys [leaf index]} (field-leaf! form env binding parsed)
          value (lower-value value-form env)]
      (when (and (> (:size leaf) 1) (nil? index))
        (fail "use aset to write an array field element"
              {:form form :state (:name binding) :path (:path parsed)}))
      (reject-constant-store! form binding)
      (expect-type! (:type leaf) (:value value) form)
      (void-fragment
       (concat (:statements index) (:statements value)
               [{:op :state-store
                 :state-id (:name binding)
                 :index (:offset leaf)
                 :element-index (:value index)
                 :size (:size leaf)
                 :type (:type leaf)
                 :value (:value value)
                 :effects #{:write-state}}])))))

(defn- lower-field-read
  "Clojure-style field read for `(:field record)` and `(record :field)`."
  [form env record-name path]
  (if-let [binding (value-record-binding env record-name)]
    (project-record-value form binding path)
    (let [binding (record-binding! form env record-name)
          {:keys [leaf index]} (field-leaf! form env binding {:path path})]
      (if (and (> (:size leaf) 1) (nil? index))
        (fragment [] (leaf-array-ref binding leaf))
        (fragment (:statements index)
                  {:op :state-load
                   :state-id (:name binding)
                   :index (:offset leaf)
                   :element-index (:value index)
                   :size (:size leaf)
                   :type (:type leaf)
                   :effects #{:read-state}})))))

(defn- lower-place-store!
  "Field write for a `set!` place: `(set! (:field record) value)`."
  [form record-name path value-form env]
  (when (value-record-binding env record-name)
    (fail "DSP record values are immutable"
          {:form form :record record-name}))
  (let [binding (record-binding! form env record-name)
        {:keys [leaf index]} (field-leaf! form env binding {:path path})]
    (when (> (:size leaf) 1)
      (fail "use aset to write an array field element"
            {:form form :state (:name binding) :path path}))
    (let [value (lower-value value-form env)]
      (reject-constant-store! form binding)
      (expect-type! (:type leaf) (:value value) form)
      (void-fragment
       (concat (:statements index) (:statements value)
               [{:op :state-store
                 :state-id (:name binding)
                 :index (:offset leaf)
                 :element-index (:value index)
                 :size (:size leaf)
                 :type (:type leaf)
                 :value (:value value)
                 :effects #{:write-state}}])))))

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
            partition-base (when (pos? partition-size)
                             (let [base @(:call-offset env)]
                               (swap! (:call-offset env) + partition-size)
                               (swap! (:state-more env) into
                                      (rebase-slots base (:state callee)))
                               (swap! (:records-more env) into
                                      (rebase-records base (:records callee)))
                               base))]
        (combine-fragments
         args
         {:op :call
          :function-id function-id
          :args (mapv :value args)
          :type (:return-type signature)
          :record-leaves (when (= :record (:return-type signature))
                           (:return-fields signature))
          :state-base partition-base
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
        (= 'buffer-store op) (lower-buffer-store form env)
        (= 'state-load op) (lower-state-load form env)
        (= 'state-store op) (lower-state-store form env)
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
        (lower-binary form env (get arithmetic-operators op) false)
        (contains? comparison-operators op)
        (lower-binary form env (get comparison-operators op) true)
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

(defn- expand-record
  "Expand one record entry into flat leaf slots from `offset`.

  An array of records (`n > 1`) is laid out struct-of-arrays: each leaf is one
  flat state entry of `:size n`. A scalar record (`n = 1`) may instead own array
  fields, in which case each field occupies its own contiguous block. Either way
  a leaf is an ordinary state slot with a constant base offset, so the backends
  are unchanged. Returns `[slots offset' binding]`."
  [state offset]
  (let [n (or (:size state) 1)
        scalar? (= n 1)
        leaves (vec (record-leaves (:fields state) []))
        [slots offset' leaf-list]
        (reduce (fn [[slots offset result] leaf]
                  (let [size (if scalar? (:size leaf) n)
                        entry (cond-> {:name (record-leaf-name (:name state) (:path leaf))
                                       :type (:type leaf)
                                       :index offset}
                                (> size 1) (assoc :size size
                                                  :init (if scalar?
                                                          (:init leaf)
                                                          (vec (repeat n (:init leaf)))))
                                (= size 1) (assoc :init (:init leaf)))]
                    [(conj slots entry)
                     (+ offset size)
                     (conj result {:path (:path leaf)
                                   :type (:type leaf)
                                   :offset offset
                                   :size size})]))
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
                                          [:name :type :size :init :index :next :constant]))
                 (assoc bindings (:name state) (assoc entry :kind :state))
                 records)))
      {:slots slots :bindings bindings :records records})))

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

(defn- module-state-sizes
  "Partition size of every definition: its own expanded state plus one
  partition per static call site, recursively (D19). Definitions are dependency
  ordered, so a callee's size is known before its caller's."
  [definitions]
  (reduce (fn [sizes [id definition]]
            (assoc sizes id
                   (+ (state-total (:slots (expand-state (:state definition))))
                      (reduce + 0 (map (fn [call] (get sizes (second call) 0))
                                       (definition-calls definition))))))
          {}
          definitions))

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

(defn- base-bindings
  [definition state-bindings]
  (merge (into {}
               (concat
                (map (fn [parameter]
                       [(:name parameter) (assoc parameter :kind :param)])
                     (:params definition))
                state-bindings))
         (context-bindings definition)))

(defn lower
  "Lower one normalized descriptor to explicit function IR."
  ([definition] (lower definition {}))
  ([definition {:keys [functions definition-id options modules]}]
   (normalized-definition! definition)
   (let [return-type (:return-type definition)
         record-return (:return-record definition)
         expanded (expand-state (:state definition))
         states (:slots expanded)
         stateful? (boolean (seq (:state definition)))
         env (assoc (environment (base-bindings definition (:bindings expanded))
                                 (or functions {}) (or options {}) {} (or modules {}))
                    :return-record record-return)
         ;; Call-site partitions are allocated after this module's own state.
         _ (reset! (:call-offset env) (state-total states))
         _ (reset! (:records-more env) (vec (:records expanded)))
         ;; The return is a scalar expression, or a map producing a record value;
         ;; `let`/`do` around it lower as ordinary forms.
         returned (lower-forms (:body definition) env)
         _ (when-not (:value returned)
             (fail "DSP function body does not produce a value"
                   {:body (:body definition)}))
         _ (if record-return
             (when-not (= :record (:type (:value returned)))
               (fail "DSP record-returning body must produce a record value"
                     {:body (:body definition) :type (:type (:value returned))}))
             (expect-type! return-type (:value returned) (:body definition)))
         ;; Snapshot the return before this module's transitions can clobber a
         ;; state slot the returning expression already read.
         [result-declarations return-value]
         (if-not stateful?
           [[] (:value returned)]
           (if record-return
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
                 [{:op :return :value return-value}])))
         ir {:op :function
             :name (:name definition)
             :id (or definition-id (:name definition))
             :params (:params definition)
             :return-type return-type
             :return-record record-return
             :locals @(:locals env)
             ;; The arena is this module's own slots followed by one partition
             ;; per call site, all relative to this module's base.
             :state (into (mapv #(select-keys % [:name :type :size :init
                                                 :index :constant])
                                states)
                          @(:state-more env))
             :state-size @(:call-offset env)
             :records (vec @(:records-more env))
             :scratch (vec (:entries @(:scratch env)))
             :precision (:precision options)
             :options (or options {})
             :body body}]
     (assoc ir :effects (collect-effects body)))))

(defn- lower-definitions
  "Lower every definition in dependency order, exposing each lowered callee to
  the ones that follow so call sites can allocate their state partitions."
  [definitions options]
  (reduce (fn [lowered [id definition]]
            (assoc lowered id
                   (lower definition {:functions (signatures definitions)
                                      :definition-id id
                                      :options options
                                      :modules lowered})))
          (array-map)
          definitions))

(defn lower-unit
  "Lower every descriptor in a linked compilation unit."
  ([unit] (lower-unit unit {}))
  ([unit {:keys [options]}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP IR lowering expects a linked compilation unit" {:unit unit}))
   (let [definitions (:definitions unit)
         lowered (lower-definitions definitions options)
         entry (:entry unit)]
     (assoc (get lowered entry)
            :definitions lowered
            :entry-id entry
            :graph (:graph unit)))))

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
    (mapv (fn [{:keys [name direction type channels]}]
            {:id name
             :direction direction
             :type type
             :channels channels})
          (or declared default))))

(defn- process-bindings
  [definition state-bindings channels]
  (let [process (:process definition)
        input? (process-input? definition)
        sample-name (when input?
                      (or (get-in process [:input :name])
                          (get-in definition [:params 0 :name])))
        sample-param (some #(when (= sample-name (:name %)) %) (:params definition))
        controls (if process
                   (mapv (fn [{:keys [name type]}]
                           (let [param (some #(when (= name (:name %)) %)
                                             (:params definition))]
                             ;; A control port that declares a non-default type
                             ;; (e.g. :int) types the local, since the shorthand
                             ;; parameter list can only express :float.
                             (cond-> param
                               (and type (not= :float type)) (assoc :type type))))
                         (:controls process))
                   (vec (rest (:params definition))))
        frames (or (get-in process [:frames]) {:name 'frames :type :int})
        sample-rate (or (get-in process [:sample-rate])
                        {:name 'sample-rate :type :float})
        channel (if (= 1 channels) 0 {:op :channel :type :int})
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

(defn lower-process
  "Lower the entry descriptor directly to explicit process IR.

  The sample parameter is bound to a canonical input-buffer load while controls
  and state retain their own binding kinds. No scalar IR rewriting is involved."
  ([unit] (lower-process unit {}))
  ([unit {:keys [channels options] :or {channels 1}}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP process lowering expects a linked compilation unit" {:unit unit}))
   (when-not (and (integer? channels) (pos? channels))
     (fail "DSP process channels must be a positive integer" {:channels channels}))
   (let [definitions (:definitions unit)
         entry-id (:entry unit)
         definition (get definitions entry-id)
         _ (normalized-definition! definition)
         expanded (expand-state (:state definition))
         states (:slots expanded)
         buffers (process-buffers definition channels)
         {:keys [bindings channel controls frames sample-rate]} (process-bindings definition (:bindings expanded) channels)
         ;; Callees are lowered first, in dependency order, so every call site
         ;; can allocate its state partition (D19).
         modules (lower-definitions (dissoc definitions entry-id) options)
         ;; The entry arena is its own state, then one partition per call site,
         ;; then local-array scratch. Partition sizes are static, so scratch
         ;; starts after the whole state region.
         total-state (get (module-state-sizes definitions) entry-id)
         _ (when (and (> channels 1) (pos? total-state))
             (fail "stateful multi-channel DSP processes are not supported yet"
                   {:name (:name definition) :channels channels}))
         env (environment bindings (signatures definitions) (or options {})
                          (into {} (map (juxt :id identity) buffers))
                          modules)
         _ (reset! (:call-offset env) (state-total states))
         _ (reset! (:records-more env) (vec (:records expanded)))
         _ (reset! (:scratch env) {:next total-state :entries []})
         output (lower-forms (:body definition) env)
         _ (when-not (:value output)
             (fail "DSP process body does not produce an output value"
                   {:name (:name definition)}))
         _ (expect-type! :float (:value output) (:body definition))
         output-local (fresh-local! env "output" :float)
         output-declaration {:op :declare
                             :name (:name output-local)
                             :type :float
                             :init (:value output)}
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
           [output-declaration]
           (mapcat :statements (remove nil? state-results))
           [{:op :buffer-store
             :buffer-id :output
             :channel channel
             :index {:op :frame :type :int}
             :value {:op :local :name (:name output-local) :type :float}
             :effects #{:write-memory}}]
           (map (fn [{:keys [state local]}]
                  {:op :state-store
                   :state-id (:name state)
                   :index (:index state)
                   :type (:type state)
                   :value {:op :local :name (:name local) :type (:type local)}
                   :effects #{:write-state}})
                (remove nil? state-results))))
         frame-body (block frame-statements)
         memory (memory-requirements buffers frame-body (:precision options))
         diagnostics (process-diagnostics frame-body memory)
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
             :locals @(:locals env)
             :state (into (mapv #(select-keys % [:name :type :size :init :index :constant])
                                states)
                          @(:state-more env))
             :state-size @(:call-offset env)
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
    :call (inline-call expr definitions state)
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

(defn inline-calls
  "Inline linked callees for the source-emitting backends.

  Pure callees are substituted. A stateful module is expanded with its locals
  renamed and its state rebased into the call site's partition (D19), so the
  JVM, JavaScript, and Wasm backends share one composition implementation."
  [ir]
  (let [definitions (:definitions ir)
        body-key (if (= :process (:op ir)) :frame-body :body)
        state (atom {:names 0 :statements 0 :records {}})
        body (first (inline-statement (get ir body-key) definitions state))
        ir (assoc ir body-key body :definitions {}
                  :locals (vec (distinct (declared-locals body))))]
    (assoc ir :effects (collect-effects body))))

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
                 'buffer-load 'buffer-store 'state-load 'state-store
                 'int->float 'float->int])))
