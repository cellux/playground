(ns omkamra.dsp.descriptor
  "Normalization and validation for omkamra.dsp source descriptors.

  This namespace deliberately knows nothing about target backends.  It turns
  the two source-level descriptor forms into one stable descriptor shape and
  reports source-aware errors before IR lowering begins."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [omkamra.dsp.builtins :as builtins]))

(def logical-types #{:float :int :boolean :void})
(def value-types #{:float :int :boolean})

(def default-options
  {:target :interpreter
   :entry :function
   :precision :f32
   :channels 1
   :checks :development
   :optimize false
   :loop-bound nil
   :wasm-math :polynomial})

(def ^:private source-keys #{:namespace :file :line :column})

(defn- error-data
  [source path data]
  (merge {:dsp/error :invalid-definition
          :dsp/source source
          :dsp/path path}
         data))

(defn- fail
  ([message source path data]
   (throw (ex-info message (error-data source path data))))
  ([message source data]
   (fail message source nil data)))

(defn- schema-path
  [explanation]
  (or (some-> explanation :errors first :path)
      []))

(defn- validate-schema!
  [schema value source path]
  (when-let [explanation (m/explain schema value)]
    (fail "DSP descriptor has an invalid shape"
          source
          (into (vec path) (schema-path explanation))
          {:value value
           :errors (me/humanize explanation)
           :malli/explanation explanation}))
  value)

(defn- valid-name?
  [value]
  (and (symbol? value)
       (not (str/blank? (name value)))))

(defn- quoted-symbol?
  [value]
  (and (seq? value)
       (= 'quote (first value))
       (= 2 (count value))
       (symbol? (second value))))

(def ^:private LogicalType
  [:enum :float :int :boolean :void])

(def ^:private ValueType
  [:enum :float :int :boolean])

(def ^:private Name
  [:fn {:error/message "must be a non-blank symbol"} valid-name?])

(def ^:private Reference
  [:or symbol? keyword? [:fn quoted-symbol?]])

;; A return type is either a logical scalar or a reference to a `defstruct`
;; shape, resolved by the linker into a record return (multi-output).
(def ^:private ReturnType
  [:or LogicalType Reference])

(def ^:private ParameterSource
  [:or
   symbol?
   [:map {:closed true}
    [:name Name]
    [:type {:optional true} ValueType]]])

(def ^:private StateSource
  [:or
   symbol?
   [:map {:closed true}
    [:name Name]
    [:type {:optional true} ValueType]
    ;; A `:size` greater than one declares fixed-size persistent state.  Its
    ;; elements retain the declared logical `:type`; it is not a new physical
    ;; host-array type.
    [:size {:optional true} pos-int?]
    [:init {:optional true} any?]
    [:next {:optional true} any?]
    ;; A record: typed named fields, possibly nested.  Combined with `:size` it
    ;; is an array of records.  Validated recursively by `normalize-record`.
    [:fields {:optional true} any?]
    ;; A constant table is materialized at initialization and never written.
    [:constant {:optional true} boolean?]
    ;; Set by the linker when a `:struct` reference is inlined.
    [:struct-name {:optional true} symbol?]]])

(def ^:private ProcessPort
  [:map {:closed true}
   [:name [:or symbol? keyword?]]
   [:type {:optional true} ValueType]
   [:channels {:optional true} int?]])

(def ^:private ProcessPortSource
  [:or symbol? keyword? ProcessPort])

(def ^:private ControlSource
  [:or
   Reference
   [:map {:closed true}
    [:name [:or symbol? keyword?]]
    [:type {:optional true} ValueType]
    [:default {:optional true} any?]
    ;; Declared smoothing modes, both in the control's own units *per frame*: a
    ;; one-pole coefficient, or a slew limit. Per frame and not per block,
    ;; because a block-rate amount would make rendering depend on the host's
    ;; block size (P9's invariance requirement).
    [:smooth {:optional true} number?]
    [:slew {:optional true} number?]
    ;; A lane's value is supplied by the host one float *per frame*, from the
    ;; process's `:lanes` region, instead of as one block-rate scalar. It is the
    ;; host's mechanism for arbitrary automation curves; `:smooth` and `:slew`
    ;; compose with it, since they only change what the lane's target is.
    [:lane {:optional true} boolean?]]])

(def ^:private BufferSource
  [:map {:closed true}
   [:name keyword?]
   [:direction [:enum :input :output :control :state]]
   [:channels {:optional true} int?]
   [:type {:optional true} ValueType]])

(def ^:private Lifecycle
  [:map {:closed true}
   [:init {:optional true} boolean?]
   [:reset {:optional true} boolean?]])

(def ^:private ProcessSource
  [:map {:closed true}
   [:input {:optional true} [:or false? ProcessPortSource]]
   [:output {:optional true} ProcessPortSource]
   [:frames {:optional true}
    [:map {:closed true}
     [:name {:optional true} [:or symbol? keyword?]]
     [:type {:optional true} [:enum :int]]]]
   [:sample-rate {:optional true}
    [:map {:closed true}
     [:name {:optional true} [:or symbol? keyword?]]
     [:type {:optional true} ValueType]]]
   [:controls {:optional true} [:vector ControlSource]]
   [:channels {:optional true} int?]
   [:buffers {:optional true} [:vector BufferSource]]
   [:lifecycle {:optional true} Lifecycle]])

(def ^:private CompilerOptions
  [:map {:closed true}
   [:target {:optional true} [:enum :interpreter :jvm :js :wasm]]
   [:entry {:optional true} [:enum :function :process]]
   [:precision {:optional true} [:enum :f32 :f64]]
   [:channels {:optional true} int?]
   [:checks {:optional true} [:enum :development :none]]
   [:optimize {:optional true} boolean?]
   [:loop-bound {:optional true} [:or nil? int?]]
   ;; Selects the Wasm transcendental emulation. Only `:polynomial` is
   ;; implemented; `:table` is reserved for the lookup-table lowering.
   [:wasm-math {:optional true} [:enum :polynomial :table]]])

(def ^:private SourceMetadata
  [:map {:closed true}
   [:namespace {:optional true} [:or symbol? string?]]
   [:ns {:optional true} [:or symbol? string?]]
   [:file {:optional true} string?]
   [:line {:optional true} int?]
   [:column {:optional true} int?]])

(def ^:private ResolutionContext
  [:map {:closed true}
   [:aliases {:optional true} [:map-of symbol? symbol?]]
   [:refers {:optional true} [:map-of symbol? symbol?]]])

(def ^:private DefinitionSource
  [:map {:closed true}
   [:dsp/kind [:= :function]]
   [:name Name]
   [:params [:vector ParameterSource]]
   [:return-type {:optional true} ReturnType]
   ;; Attached by the linker when a struct-typed `:return-type` is resolved.
   [:return-record {:optional true} [:or nil? [:map]]]
   [:doc {:optional true} [:or nil? string?]]
   [:state {:optional true} [:or nil? [:vector StateSource]]]
   [:body [:vector any?]]
   [:source {:optional true} [:or nil? SourceMetadata]]
   [:resolution {:optional true} [:or nil? ResolutionContext]]
   [:process {:optional true} [:or nil? ProcessSource]]
   [:options {:optional true} [:or nil? CompilerOptions]]
   [:compiler-options {:optional true} [:or nil? CompilerOptions]]])

(def ^:private SourceForm
  [:schema {:registry {::source-form
                       [:or symbol?
                        keyword?
                        number?
                        boolean?
                        [:map-of keyword? [:ref ::source-form]]
                        [:sequential [:ref ::source-form]]]}}
   [:ref ::source-form]])

(defn- source-location
  [source]
  (let [source (if (map? source) source {})]
    (merge (select-keys source source-keys)
           (when (contains? source :ns)
             {:namespace (:ns source)}))))

(defn- integer-value?
  [value]
  (and (number? value)
       (== (double value) (double (long value)))))

(defn- default-value
  [type]
  (case type
    :float 0.0
    :int 0
    :boolean false
    (fail "DSP declarations cannot use :void as a value type"
          {} nil {:type type})))

(defn- normalize-initializer
  [value type source path]
  (case type
    :float
    (if (number? value)
      ;; Keep the full double. Narrowing happens per precision at use, so an
      ;; f64 table's values survive the round trip to the browser as EDN.
      (double value)
      (fail "DSP :float initializers must be numeric"
            source path {:value value :type type}))

    :int
    (if (integer-value? value)
      (long value)
      (fail "DSP :int initializers must be integer-valued"
            source path {:value value :type type}))

    :boolean
    (if (boolean? value)
      value
      (fail "DSP :boolean initializers must be boolean"
            source path {:value value :type type}))

    (fail "DSP declarations cannot use :void as a value type"
          source path {:type type})))

(defn- validate-source-form!
  [value source path]
  (when-let [explanation (m/explain SourceForm value)]
    (fail "DSP source contains an unsupported host value"
          source path
          {:form value
           :errors (me/humanize explanation)
           :malli/explanation explanation}))
  value)

(defn- normalize-param
  [param _source _path]
  (let [[name type] (if (symbol? param)
                      [param :float]
                      [(:name param) (or (:type param) :float)])]
    {:name name :type type}))

(defn- normalize-params
  [params source]
  (let [params (mapv #(normalize-param % source [:params %2]) params (range))
        names (map :name params)]
    (when-not (= (count names) (count (set names)))
      (fail "DSP parameter names must be unique"
            source [:params] {:params params}))
    params))

(defn- normalize-field
  [field source path]
  (let [field (if (keyword? field) {:name field} field)]
    (when-not (and (map? field) (:name field))
      (fail "DSP record fields require a name" source path {:field field}))
    (let [field-name (keyword (name (:name field)))]
      (when (contains? field :next)
        (fail "DSP record fields do not take :next" source (conj path :next)))
      (if (contains? field :fields)
        (do
          (when (contains? field :size)
            (fail "DSP nested record fields cannot declare :size"
                  source (conj path :size) {:field field-name}))
          {:name field-name
           :fields (mapv (fn [child index]
                           (normalize-field child source (conj path :fields index)))
                         (:fields field) (range))})
        (let [type (or (:type field) :float)
              size (or (:size field) 1)]
          (when-not (and (integer? size) (pos? size))
            (fail "DSP record field :size must be a positive integer"
                  source (conj path :size) {:field field-name :size size}))
          (if (= 1 size)
            {:name field-name
             :type type
             :init (normalize-initializer
                    (if (contains? field :init) (:init field) (default-value type))
                    type source (conj path :init))}
            (let [init (if (contains? field :init)
                         (:init field)
                         (vec (repeat size (default-value type))))]
              (when-not (and (vector? init) (= size (count init)))
                (fail "DSP array field initializers must be vectors of the declared size"
                      source (conj path :init)
                      {:field field-name :size size :init init}))
              {:name field-name
               :type type
               :size size
               :init (mapv (fn [value index]
                             (normalize-initializer value type source
                                                    (conj path :init index)))
                           init (range))})))))))

(defn- normalize-record
  [state source path]
  (let [size (or (:size state) 1)
        fields (mapv (fn [field index]
                       (normalize-field field source (conj path :fields index)))
                     (:fields state) (range))]
    (when (contains? state :next)
      (fail "DSP record state uses explicit field access, not :next"
            source (conj path :next) {:state (:name state)}))
    (when (contains? state :init)
      (fail "DSP record state does not take a top-level :init"
            source (conj path :init) {:state (:name state)}))
    (when (and (> size 1) (some #(> (or (:size %) 1) 1) fields))
      (fail "DSP arrays of records cannot contain array fields yet"
            source (conj path :fields) {:state (:name state)}))
    (cond-> {:name (:name state) :fields fields}
      (> size 1) (assoc :size size)
      (:constant state) (assoc :constant true)
      (:struct-name state) (assoc :struct-name (:struct-name state)))))

(defn- normalize-state
  [state source path]
  (let [state (if (symbol? state) {:name state} state)]
    (if (contains? state :fields)
      (normalize-record state source path)
      (let [type (or (:type state) :float)
            size (or (:size state) 1)
            supplied-init? (contains? state :init)
            init (if supplied-init?
                   (:init state)
                   (if (= 1 size)
                     (default-value type)
                     (vec (repeat size (default-value type)))))]
        (when (contains? state :next)
          (validate-source-form! (:next state) source (conj path :next)))
        (when (and (> size 1) (contains? state :next))
          (fail "DSP aggregate state uses explicit state-store, not :next"
                source (conj path :next)
                {:state (:name state) :size size}))
        (when (and (:constant state) (= 1 size))
          (fail "DSP :constant requires an aggregate state table"
                source (conj path :constant) {:state (:name state)}))
        (when (and (:constant state) (contains? state :next))
          (fail "DSP constant state has no :next"
                source (conj path :next) {:state (:name state)}))
        (let [init (if (= 1 size)
                     (normalize-initializer init type source (conj path :init))
                     (do
                       (when-not (and (vector? init) (= size (count init)))
                         (fail "DSP aggregate state initializers must be vectors of the declared size"
                               source (conj path :init)
                               {:state (:name state) :size size :init init}))
                       (mapv (fn [value index]
                               (normalize-initializer value type source
                                                      (conj path :init index)))
                             init (range))))]
          (cond-> {:name (:name state)
                   :type type
                   :init init
                   :next (:next state)}
            (> size 1) (assoc :size size)
            (:constant state) (assoc :constant true)))))))

(defn- normalize-states
  [states source]
  (let [states (mapv (fn [state index]
                       (normalize-state state source [:state index]))
                     (or states []) (range))
        names (map :name states)]
    (when-not (= (count names) (count (set names)))
      (fail "DSP state names must be unique"
            source [:state] {:states states}))
    states))

(defn normalize-struct
  "Validate and normalize a struct (state-record shape) description.

  Struct descriptions are bound to Vars by `omkamra.dsp/defstruct` and resolved
  and inlined by the linker."
  [value]
  (when-not (and (map? value) (= :struct (:dsp/kind value)))
    (fail "DSP struct reference must resolve to a value defined with dsp/defstruct"
          (:source value) [:struct] {:value value}))
  (let [source (:source value)
        fields (:fields value)]
    (when-not (vector? fields)
      (fail "DSP struct fields must be a vector"
            source [:fields] {:fields fields}))
    (cond-> {:dsp/kind :struct
             :name (:name value)
             :fields (mapv (fn [field index]
                             (normalize-field field source [:fields index]))
                           fields (range))}
      ;; Struct options are defaults for the state slots that reference it.
      (:constant value) (assoc :constant true))))

(defn- expression-type
  "Infer the type of the small source language for declaration checks.

  This is intentionally conservative.  Full expression lowering remains the
  responsibility of `omkamra.dsp.ir`; returning nil here means that lowering
  will provide the more specific unsupported-expression diagnostic."
  [form env source path]
  (cond
    (number? form) (if (integer? form) :int :float)
    (boolean? form) :boolean
    (symbol? form) (get env form)
    (seq? form)
    (let [op (first form)
          args (next form)
          arithmetic '#{+ - * /}
          comparisons '#{= not= < <= > >=}
          logical '#{and or not}]
      (cond
        (contains? arithmetic op)
        (when (= 2 (count args))
          (let [types (mapv #(expression-type % env source path) args)]
            (when (and (= 1 (count (set types)))
                       (contains? #{:float :int} (first types)))
              (first types))))

        (contains? comparisons op)
        (when (= 2 (count args))
          (let [types (mapv #(expression-type % env source path) args)
                type (first types)]
            (when (and (= 1 (count (set types)))
                       (if (#{'= 'not=} op)
                         (contains? #{:float :int :boolean} type)
                         (contains? #{:float :int} type)))
              :boolean)))

        (contains? logical op)
        (let [expected (if (= op 'not) 1 2)]
          (when (= expected (count args))
            (when (every? #{:boolean} (map #(expression-type % env source path) args))
              :boolean)))

        (contains? '#{bit-and bit-or bit-xor bit-not
                      bit-shift-left bit-shift-right unsigned-bit-shift-right} op)
        (let [expected (if (= op 'bit-not) 1 2)]
          (when (= expected (count args))
            (when (every? #{:int} (map #(expression-type % env source path) args))
              :int)))

        (builtins/builtin? op)
        (when (builtins/accepts-arity? op (count args))
          (let [types (mapv #(expression-type % env source path) args)]
            (when (every? some? types)
              (builtins/infer-return op types))))

        (= op 'if)
        (let [[condition then else] args]
          (when (and (= 3 (count args))
                     (= :boolean (expression-type condition env source path)))
            (let [then-type (expression-type then env source path)
                  else-type (expression-type else env source path)]
              (when (= then-type else-type) then-type))))

        (= op 'let)
        (let [[bindings & body] args]
          (when (and (vector? bindings)
                     (even? (count bindings))
                     (seq body))
            (loop [pairs (partition 2 bindings)
                   env env]
              (if-let [[[name init] & remaining] (seq pairs)]
                (let [init-type (expression-type init env source path)]
                  (when (and (valid-name? name) init-type)
                    (recur remaining (assoc env name init-type))))
                (expression-type (if (= 1 (count body))
                                   (first body)
                                   (cons 'do body))
                                 env source path)))))

        (= op 'do)
        (when (seq args)
          (expression-type (last args) env source path))

        (= op 'set!)
        (let [[name value] args]
          (when (and (= 2 (count args)) (contains? env name))
            (let [value-type (expression-type value env source path)]
              (when (= value-type (get env name)) value-type))))

        (= op 'int->float)
        (when (and (= 1 (count args))
                   (= :int (expression-type (first args) env source path)))
          :float)
        (= op 'float->int)
        (when (and (= 1 (count args))
                   (= :float (expression-type (first args) env source path)))
          :int)
        (= op 'break) (when (empty? args) :void)
        (= op 'continue) (when (empty? args) :void)
        (= op 'while) :void
        (= op 'buffer-load) :float
        (= op 'buffer-store) :void
        ;; Indexed aggregate-state operations are fully type-checked during
        ;; portable IR lowering, where the state size is available.
        (= op 'state-load) nil
        (= op 'state-store) :void
        :else nil))
    :else nil))

(defn- validate-expression-types!
  [body params states return-type source]
  (let [env (into {}
                  (concat (map (juxt :name :type) params)
                          (map (juxt :name :type) states)))
        actual (expression-type body env source [:body 0])]
    (when (and actual (not= actual return-type))
      (fail "DSP expression does not match the declared return type"
            source [:body 0] {:expected return-type :actual actual :form body}))
    (doseq [[index {:keys [name type next]}]
            (keep-indexed (fn [index state]
                            (when (some? (:next state)) [index state]))
                          states)]
      (let [actual (expression-type next env source [:state index :next])]
        (when (and actual (not= actual type))
          (fail "DSP state transition does not match its declared type"
                source [:state index :next]
                {:state name :expected type :actual actual :form next}))))
    actual))

(defn- process-name
  [value source path]
  (let [value (if (and (seq? value)
                       (= 'quote (first value))
                       (= 2 (count value)))
                (second value)
                value)
        value (if (keyword? value) (symbol (name value)) value)]
    (when-not (valid-name? value)
      (fail "DSP process references must name a symbol or keyword"
            source path {:value value}))
    value))

(defn- normalize-process-port
  [port default-name default-type default-channels source path params]
  (let [[name type channels]
        (cond
          (nil? port) [default-name default-type default-channels]
          (or (symbol? port) (keyword? port))
          [(process-name port source path) default-type default-channels]
          (map? port)
          [(:name port)
           (or (:type port) default-type)
           (or (:channels port) default-channels)]
          :else
          (fail "DSP process ports must be names or descriptor maps"
                source path {:port port}))
        name (process-name name source (conj path :name))]
    (when-not (= :float type)
      (fail "DSP process input and output ports must have type :float"
            source (conj path :type) {:type type}))
    (when-not (and (integer? channels) (pos? channels))
      (fail "DSP process channel counts must be positive integers"
            source (conj path :channels) {:channels channels}))
    (when (and (seq params) (not (some #(= name (:name %)) params)))
      (fail "DSP process port refers to an unknown parameter"
            source path {:name name :parameters (mapv :name params)}))
    {:name name :type type :channels channels}))

(defn- normalize-control
  [control source path params]
  (let [[name type]
        (cond
          (or (symbol? control)
              (keyword? control)
              (and (seq? control) (= 'quote (first control))))
          [(process-name control source path) :float]
          (map? control)
          [(process-name (:name control) source path)
           (or (:type control) :float)]
          :else
          (fail "DSP process controls must be names or descriptor maps"
                source path {:control control}))]
    (when-not (some #(= name (:name %)) params)
      (fail "DSP process control refers to an unknown parameter"
            source path {:name name :parameters (mapv :name params)}))
    (when (and (map? control) (contains? control :smooth) (contains? control :slew))
      (fail "DSP control smoothing and slew limiting are alternatives"
            source path {:control control}))
    ;; Shorthand controls arrive as symbols or keywords, so every mode check
    ;; must first establish that there is a control map at all.
    (when (and (map? control) (contains? control :smooth) (not= :float type))
      (fail "DSP smoothed controls must have type :float"
            source path {:name name :type type}))
    (when (and (map? control) (contains? control :slew) (not= :float type))
      (fail "DSP slewed controls must have type :float"
            source path {:name name :type type}))
    (when (and (map? control) (:lane control) (not= :float type))
      (fail "DSP per-frame lane controls must have type :float"
            source path {:name name :type type}))
    (cond-> {:name name :type type}
      (and (map? control) (contains? control :default))
      (assoc :default
             (do
               (validate-source-form! (:default control) source
                                      (conj path :default))
               (normalize-initializer (:default control) type source
                                      (conj path :default))))
      (map? control)
      (merge (select-keys control [:smooth :slew])
             (when (:lane control) {:lane true})))))

(defn- normalize-buffer-name
  [name source path]
  (when (or (not (keyword? name))
            (str/blank? (clojure.core/name name)))
    (fail "DSP process buffer names must be non-blank keywords"
          source path {:name name}))
  name)

(defn- normalize-buffers
  [buffers channels source input?]
  (let [buffers (if (nil? buffers)
                  ;; A generator (`:input false`, D23) has no input buffer at
                  ;; all, so its body cannot load a sample and its effects stay
                  ;; free of a spurious `:read-memory`.
                  (cond-> [{:name :output :direction :output
                            :channels channels :type :float}]
                    input? (conj {:name :input :direction :input
                                  :channels channels :type :float}))
                  (mapv (fn [buffer]
                          (assoc buffer
                                 :name (normalize-buffer-name
                                        (:name buffer) source
                                        [:process :buffers :name])
                                 :channels (or (:channels buffer) channels)
                                 :type (or (:type buffer) :float)))
                        buffers))]
    (let [buffer-names (map :name buffers)]
      (when-not (= (count buffer-names) (count (set buffer-names)))
        (fail "DSP process buffer names must be unique"
              source [:process :buffers] {:buffers buffers})))
    (doseq [[index buffer] (map-indexed vector buffers)]
      (when-not (and (integer? (:channels buffer))
                     (pos? (:channels buffer)))
        (fail "DSP process buffer channel counts must be positive integers"
              source [:process :buffers index :channels]
              {:channels (:channels buffer)})))
    (when (some #(and (#{:input :output} (:direction %))
                      (not= channels (:channels %)))
                buffers)
      (fail "DSP process input and output buffers must match process channels"
            source [:process :buffers]
            {:channels channels :buffers buffers}))
    (when (some #(and (#{:input :output} (:direction %))
                      (not= :float (:type %)))
                buffers)
      (fail "DSP process input and output buffers must have type :float"
            source [:process :buffers] {:buffers buffers}))
    ;; A process must always declare an output; only a generator (whose
    ;; `:input` is false) may have no input buffer at all.
    (when-not (every? (set (map :direction buffers))
                      (if input? [:input :output] [:output]))
      (fail "DSP process buffers must declare output, and input unless the process is a generator"
            source [:process :buffers] {:buffers buffers :input? input?}))
    buffers))

(defn- normalize-process
  [process params source]
  (let [channels (or (:channels process) 1)
        ;; `:input false` declares a generator: it has no sample parameter, no
        ;; input buffer, and every parameter is a control. An absent `:input`
        ;; still means the default port, so `false` is the only way to say
        ;; "none".
        input? (not (false? (:input process)))
        input (when input?
                (normalize-process-port (:input process)
                                        (:name (first params)) :float channels
                                        source [:process :input] params))
        output (normalize-process-port (:output process)
                                       :output :float channels
                                       source [:process :output] nil)
        frames (or (:frames process) {:name :frames :type :int})
        sample-rate (or (:sample-rate process)
                        {:name :sample-rate :type :float})
        controls (if (contains? process :controls)
                   (:controls process)
                   (if input?
                     (mapv :name (rest params))
                     (mapv :name params)))
        buffers (normalize-buffers (:buffers process) channels source input?)
        ;; Lane controls share one flat, host-written region: one slice of
        ;; `:frame-capacity` frames per lane. The slicing is an addressing
        ;; detail of the kernel; the host sees one named lane per control.
        lane-controls (filterv #(and (map? %) (:lane %)) controls)
        buffers (if (seq lane-controls)
                  (do
                    (when (some #(= :lanes (:name %)) buffers)
                      (fail "DSP process buffers cannot be named :lanes"
                            source [:process :buffers] {}))
                    (conj buffers {:name :lanes :direction :control
                                   :channels 1 :type :float}))
                  buffers)]
    (when-not (and (integer? channels) (pos? channels))
      (fail "DSP process channels must be a positive integer"
            source [:process :channels] {:channels channels}))
    (when-not (= :float (or (:type sample-rate) :float))
      (fail "DSP process sample-rate metadata must have type :float"
            source [:process :sample-rate] {:sample-rate sample-rate}))
    (let [frames {:name (process-name (or (:name frames) :frames)
                                      source [:process :frames])
                  :type :int}
          sample-rate {:name (process-name (or (:name sample-rate) :sample-rate)
                                           source [:process :sample-rate])
                       :type :float}
          controls (mapv #(normalize-control % source
                                             [:process :controls %2]
                                             params)
                         controls (range))
          control-names (map :name controls)
          signature-names (concat (when input [(:name input)])
                                  [(:name output)
                                   (:name frames)
                                   (:name sample-rate)]
                                  control-names)
          duplicate-signature-names (->> (frequencies signature-names)
                                         (filter (fn [[_ count]] (> count 1)))
                                         (map first)
                                         sort
                                         vec)
          lifecycle (merge {:init true :reset true} (:lifecycle process))]
      (when (and input? (not= channels (:channels input) (:channels output)))
        (fail "DSP process input and output channels must match"
              source [:process] {:channels channels
                                 :input (:channels input)
                                 :output (:channels output)}))
      (when (seq duplicate-signature-names)
        (fail "DSP process signature names must be unique"
              source [:process]
              {:names duplicate-signature-names}))
      (when (and input? (some #(= (:name input) (:name %)) controls))
        (fail "DSP process input cannot also be a control"
              source [:process :controls] {:input (:name input)
                                           :controls control-names}))
      {:input input
       :output output
       :frames frames
       :sample-rate sample-rate
       :controls controls
       :channels channels
       :buffers buffers
       :lifecycle lifecycle})))

(defn validate-options
  "Validate compiler options and return only explicitly supplied options."
  [options source path]
  (validate-schema! CompilerOptions options source path)
  (when (contains? options :channels)
    (when-not (pos? (:channels options))
      (fail "DSP compiler channels must be a positive integer"
            source (conj path :channels) {:channels (:channels options)})))
  (when (and (contains? options :loop-bound)
             (some? (:loop-bound options))
             (not (pos? (:loop-bound options))))
    (fail "DSP compiler loop-bound must be a positive integer"
          source (conj path :loop-bound) {:loop-bound (:loop-bound options)}))
  options)

(defn normalize-options
  "Return complete compiler options with visible defaults."
  ([options]
   (normalize-options options {} nil))
  ([options source path]
   (merge default-options (validate-options (or options {}) source path))))

(defn- normalize-resolution
  [resolution source]
  (let [resolution (or resolution {})]
    (validate-schema! ResolutionContext resolution source [:resolution])
    {:aliases (or (:aliases resolution) {})
     :refers (or (:refers resolution) {})}))

(defn- annotate-control-types
  "A control that declares a non-default type types the parameter it names.

  The parameter shorthand can only express `:float`, so without this a parameter
  would be `:int` when the definition is compiled as a process entry but
  `:float` when the same definition is called as a module. Only a non-`:float`
  control type overrides, so an explicitly typed parameter is left alone (and a
  control that declares nothing keeps the parameter's own type)."
  [params process]
  (let [typed (into {}
                    (keep (fn [control]
                            (let [type (:type control)]
                              (when (and type (not= :float type))
                                [(:name control) type])))
                          (:controls process)))]
    (if (seq typed)
      (mapv (fn [parameter]
              (if-let [type (get typed (:name parameter))]
                (assoc parameter :type type)
                parameter))
            params)
      params)))

(def ^:private ramp-modes [:smooth :slew])

(defn- ramp-mode
  "The declared smoothing mode of a control, as `[mode amount]`, or nil."
  [control]
  (some (fn [mode] (when (contains? control mode) [mode (get control mode)]))
        ramp-modes))

(defn- ramp-state-name
  ;; The parameter must not be called `name`: it would shadow `clojure.core/name`
  ;; and `(name name)` would silently become a map lookup returning nil.
  [control-name]
  (symbol (str (clojure.core/name control-name) "-ramped")))

(defn- replace-symbol
  "Replace every occurrence of the symbol `from` with `to` in a source form.

  Safe as a plain tree walk because the language rejects shadowing an in-scope
  binding (P7c), so a name inside a `let` cannot mean something else. Keywords,
  which is how record fields and selectors are spelled, are untouched."
  [form from to]
  (cond
    (= form from) to
    (seq? form) (apply list (map #(replace-symbol % from to) form))
    (vector? form) (mapv #(replace-symbol % from to) form)
    (map? form) (reduce-kv (fn [result key value]
                             (assoc result key (replace-symbol value from to)))
                           form form)
    :else form))

(defn- ramp-update
  "The per-frame update that moves a ramped control's own value one step toward
  the raw control.

  Both forms are deliberately per *frame*: a block-rate smoothing would make the
  result depend on how the host partitioned its blocks, which P9's invariance
  requirement forbids. The raw control is still the target, so a host sets it by
  the ordinary control mechanism — mid-block events included."
  [mode state-name control-name amount]
  (case mode
    :smooth (list '+ state-name
                  (list '* (list '- control-name state-name) amount))
    :slew (list '+ state-name
                (list 'max
                      (list 'min (list '- control-name state-name) amount)
                      (list '- 0.0 amount)))))

(defn- ramp-states
  "Turn each ramped control into a float state slot plus the `:next` update that
  carries its smoothed value, one step per frame."
  [controls states source]
  (let [taken (set (map :name states))]
    (vec (keep (fn [control]
                 (when-let [[mode amount] (ramp-mode control)]
                   (let [name (:name control)
                         state-name (ramp-state-name name)]
                     (when (taken state-name)
                       (fail "DSP ramped control needs a free state name"
                             source [:process :controls]
                             {:control name :state state-name}))
                     {:control name
                      :replaced state-name
                      :state {:name state-name
                              :type :float
                              :init (or (:default control) 0.0)
                              :next (ramp-update mode state-name name amount)}})))
               controls))))

(defn normalize
  "Normalize and validate a source descriptor before IR lowering."
  [definition]
  (let [source (source-location (when (map? definition)
                                  (:source definition)))]
    (validate-schema! DefinitionSource definition source [])
    (let [params (normalize-params (:params definition) source)
          return-type (or (:return-type definition) :float)
          states (normalize-states (:state definition) source)
          body (:body definition)
          collisions (seq (filter (set (map :name params))
                                  (map :name states)))
          compiler-options (cond
                             (and (contains? definition :options)
                                  (contains? definition :compiler-options))
                             (fail "DSP definitions cannot specify both :options and :compiler-options"
                                   source nil {})
                             (contains? definition :compiler-options)
                             (:compiler-options definition)
                             :else (:options definition))]
      (when collisions
        (fail "DSP parameter and state names must be unique"
              source [:state]
              {:names (vec collisions)
               :parameters (mapv :name params)
               :states (mapv :name states)}))
      (when (= :void return-type)
        (fail "DSP function return type :void is not supported by the initial expression entry"
              source [:return-type] {}))
      (when-not (= 1 (count body))
        (fail "DSP definitions require exactly one body expression"
              source [:body] {:body body}))
      (validate-source-form! (first body) source [:body 0])
      (let [process (when (contains? definition :process)
                      (normalize-process (:process definition) params source))
            ;; A control's declared type types its parameter, so a module call
            ;; and a process entry agree on the parameter's type.
            params (annotate-control-types params process)
            ;; A ramped control reads its own smoothed value instead of the raw
            ;; control; the raw control is still the target the update moves
            ;; toward. The rewrite covers the body and every *user* `:next`, and
            ;; must happen before the synthesized updates are added — rewriting
            ;; those would turn their target into the smoothed value itself.
            ramps (ramp-states (:controls process) states source)
            rewrite (fn [form]
                      (reduce (fn [form {:keys [control replaced]}]
                                (replace-symbol form control replaced))
                              form ramps))
            body (mapv rewrite body)
            states (mapv (fn [state]
                           (cond-> state
                             (:next state) (update :next rewrite)))
                         states)
            states (into states (map :state ramps))
            normalized {:dsp/kind :function
                        :name (:name definition)
                        :params params
                        :return-type return-type
                        :return-record (:return-record definition)
                        :doc (:doc definition)
                        :state states
                        :process process
                        :body (vec body)
                        :source source
                        :resolution (normalize-resolution (:resolution definition)
                                                          source)
                        :options (validate-options (or compiler-options {})
                                                   source [:options])}]
        (validate-expression-types! (first body) params states return-type source)
        normalized))))
