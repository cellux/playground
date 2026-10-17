(ns omkamra.dsp
  "A small C-like DSP language embedded in Clojure data.

  The initial slice supports single-expression functions over the logical
  `:float` type, mutable locals, `do`, `set!`, `while`, float comparisons,
  boolean `and`/`or`/`not`, conditional expressions, explicit mono buffer
  loads/stores, and a channel-configurable block-processing ABI.
  Definitions are data; their bodies are never run as ordinary Clojure
  expressions."
  (:refer-clojure :exclude [compile defn defstruct reset!])
  (:require [clojure.string :as str]
            [omkamra.dsp.targets.capability :as capability]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.targets.interpreter :as interpreter]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.targets.js :as js-backend]
            [omkamra.dsp.targets.jvm :as jvm]
            [omkamra.dsp.targets.wasm :as wasm-backend]))

(clojure.core/defn- valid-definition-name?
  [name]
  (and (symbol? name)
       (not (str/blank? (clojure.core/name name)))))

(clojure.core/defn- macro-source
  [form]
  (merge (select-keys (meta form) [:file :line :column])
         {:namespace (ns-name *ns*)}))

(clojure.core/defn- var-symbol
  [v]
  (symbol (str (-> v meta :ns ns-name)) (str (-> v meta :name))))

(clojure.core/defn- macro-reference-symbols
  [forms]
  (letfn [(walk [form]
            (cond
              (seq? form) (concat (when (symbol? (first form)) [(first form)])
                                  (mapcat walk (rest form)))
              (coll? form) (mapcat walk form)
              :else []))]
    (set (mapcat walk forms))))

(clojure.core/defn- macro-resolution
  "Capture only namespace mappings used by ordinary call forms at the
  definition site. This is a compact, stable snapshot of Clojure resolution."
  [ns forms]
  (let [references (macro-reference-symbols forms)
        aliases (ns-aliases ns)
        refers (ns-refers ns)]
    {:aliases (into {}
                    (keep (fn [reference]
                            (when-let [target (get aliases
                                                   (some-> reference namespace symbol))]
                              [(symbol (namespace reference)) (ns-name target)])))
                    references)
     :refers (into {}
                   (keep (fn [reference]
                           (when-let [v (and (nil? (namespace reference))
                                             (get refers reference))]
                             (when-not (= 'clojure.core (-> v meta :ns ns-name))
                               [reference (var-symbol v)]))))
                   references)}))

(clojure.core/defn- macro-fail
  [message form path data]
  (throw (ex-info message
                  (merge {:dsp/error :invalid-definition
                          :dsp/source (macro-source form)
                          :dsp/path path
                          :dsp/form form}
                         data))))

(clojure.core/defmacro defn
  "Capture a DSP function definition as a descriptor.

  The shorthand form is:

    (dsp/defn gain [sample amount] (* sample amount))

  An explicit descriptor may be packed into the parameter vector:

    (dsp/defn gain
      [{:params [{:name sample :type :float}]
        :return-type :float}
       sample])

  Both forms capture source data; DSP bodies are never executed as ordinary
  Clojure expressions during definition or compilation."
  [name & args]
  (when-not (valid-definition-name? name)
    (macro-fail "dsp/defn name must be a non-blank symbol"
                &form [:name] {:name name}))
  (let [[doc args] (if (string? (first args))
                     [(first args) (next args)]
                     [nil args])
        [params & body] args
        explicit-spec? (or (map? params)
                           (and (vector? params)
                                (= 2 (count params))
                                (map? (first params))
                                (contains? (first params) :params)))
        [spec body]
        (cond
          (and (vector? params)
               (= 2 (count params))
               (map? (first params))
               (contains? (first params) :params))
          [(first params) [(second params)]]

          (map? params)
          [params body]

          :else
          [{:params params} body])
        explicit-params? explicit-spec?
        params (:params spec)
        [options body] (if (and (not explicit-params?)
                                (seq body)
                                (map? (first body)))
                         [(first body) (next body)]
                         [{} body])
        spec (merge spec options)]
    (when-not (vector? params)
      (macro-fail "dsp/defn params must be a vector"
                  &form [:params] {:name name :params params}))
    (when (and (not explicit-params?) (not (every? symbol? params)))
      (macro-fail "dsp/defn parameters must be symbols"
                  &form [:params] {:name name :params params}))
    (when-not (= (count params)
                 (count (set (map #(if (symbol? %) % (:name %)) params))))
      (macro-fail "dsp/defn parameter names must be unique"
                  &form [:params] {:name name :params params}))
    (when-not (= 1 (count body))
      (macro-fail "dsp/defn requires exactly one body expression"
                  &form [:body] {:name name :body body}))
    (when (contains? spec :dependencies)
      (macro-fail "dsp/defn infers dependencies from ordinary DSP calls"
                  &form [:dependencies] {:name name}))
    (let [source (merge
                  (select-keys (meta &form) [:file :line :column])
                  {:namespace (ns-name *ns*)})
          captured (cond-> {:dsp/kind :function
                            :name name
                            :params params
                            :return-type (or (:return-type spec) :float)
                            :state (:state spec)
                            :body (vec body)
                            :source source
                            :resolution (macro-resolution *ns* body)}
                     (some? doc) (assoc :doc doc)
                     (contains? spec :process)
                     (assoc :process (:process spec))
                     (contains? spec :options)
                     (assoc :options (:options spec))
                     (contains? spec :compiler-options)
                     (assoc :compiler-options (:compiler-options spec)))]
      (list 'def name (list 'quote captured)))))

(clojure.core/defmacro defstruct
  "Capture a DSP state-record shape as data bound to a Var.

  Like `dsp/defn`, the shape takes a name followed by a body whose definitions
  and options are embedded DSP data rather than executable Clojure:

    (dsp/defstruct biquad-state
      [x1 x2 y1 y2])                     ; shorthand: :float fields, 0.0

    (dsp/defstruct biquad-state
      [{:name x1 :type :float :init 0.0} ; explicit field descriptors
       {:name taps :fields [{:name a} {:name b}]}]
      {:constant true})                  ; optional options map

  A process state declaration references the Var with `{:name z :struct
  biquad-state}`; the linker resolves the reference using the declaring
  namespace and inlines the fields. The description is portable data so a
  browser compiler can read the same forms."
  [name params & body]
  (when-not (valid-definition-name? name)
    (macro-fail "dsp/defstruct name must be a non-blank symbol"
                &form [:name] {:name name}))
  (when-not (vector? params)
    (macro-fail "dsp/defstruct requires a vector of field definitions"
                &form [:params] {:params params}))
  (let [[options body] (if (and (seq body) (map? (first body)))
                         [(first body) (next body)]
                         [{} body])]
    (when (seq body)
      (macro-fail "dsp/defstruct takes field definitions and one optional options map"
                  &form [:body] {:body body}))
    (let [fields (mapv (fn [field]
                         (if (symbol? field)
                           {:name field :type :float :init 0.0}
                           field))
                       params)
          source (merge (select-keys (meta &form) [:file :line :column])
                        {:namespace (ns-name *ns*)})]
      (list 'def name
            (list 'quote (merge {:dsp/kind :struct
                                 :name name
                                 :fields fields
                                 :source source}
                                options))))))

(clojure.core/defn link
  "Link a DSP descriptor or Var into a compilation unit.

  A linked unit resolves ordinary DSP calls using the defining namespace's
  Clojure alias/refer mappings. Anonymous descriptors can provide reachable
  definitions with `{:definitions {qualified-id descriptor-or-var}}`."
  ([definition]
   (linker/link definition))
  ([definition options]
   (linker/link definition options)))

(clojure.core/defn- linked-unit?
  [value]
  (= :compilation-unit (:dsp/kind value)))

(clojure.core/defn- physical-type-policy
  [ir]
  (let [f64? (= :f64 (:precision ir))
        element-type (if f64? :f64 :f32)]
    {:precision (:precision ir)
     :logical-type :float
     :element-type element-type
     :element-bytes (if f64? 8 4)
     :alignment (if f64? 8 4)
     :views {:jvm (if f64? :double-array :float-array)
             :js (if f64? :float64-array :float32-array)
             :wasm element-type}}))

(clojure.core/defn- artifact
  [target entry ir abi executable options]
  {:target target
   :entry entry
   :ir ir
   :abi abi
   :physical-type (physical-type-policy ir)
   :realtime (ir/validate-runtime-policy! ir)
   :options options
   :invoke executable})

(clojure.core/defn- function-abi
  [ir]
  {:kind :function
   :name (:name ir)
   :params (mapv :type (:params ir))
   :return-type (:return-type ir)
   :effects (:effects ir)})

(clojure.core/defn- process-abi
  [ir]
  (let [buffers (:buffers ir)
        inputs (filterv #(= :input (:direction %)) buffers)
        outputs (filterv #(= :output (:direction %)) buffers)
        ;; The declared smoothing mode is part of the ABI: it tells a host that
        ;; this control's value is carried across frames by the kernel, so
        ;; setting it is a target, not an instantaneous jump.
        controls (mapv #(select-keys % [:name :type :default :smooth :slew])
                       (:controls ir))
        state (mapv #(select-keys % [:name :type :size :init :constant]) (:state ir))
        scratch (mapv #(select-keys % [:name :type :size :index]) (:scratch ir))
        scratch-values (mapv (fn [value] (assoc value :persistent? false))
                             scratch)
        records (mapv (fn [record]
                        {:name (:name record)
                         :struct (:struct-name record)
                         :size (:size record)
                         :constant (boolean (:constant record))
                         :persistent? true
                         :ownership :instance
                         :fields (:leaves record)})
                      (:records ir))
        state-values (mapv (fn [index value]
                             (assoc value
                                    :index index
                                    :persistent? true))
                           (range)
                           state)
        physical-type (physical-type-policy ir)
        element-type (:element-type physical-type)
        element-bytes (:element-bytes physical-type)
        state-slot-count (reduce + 0 (map #(or (:size %) 1) (:state ir)))
        scratch-slot-count (reduce + 0 (map #(or (:size %) 1) (:scratch ir)))]
    {:abi-version 1
     :kind :process
     :name (:name ir)
     :entry :process
     :precision (:precision ir)
     :physical-type physical-type
     :channels {:count (:channels ir)
                :binding (if (> (:channels ir) 1) :loop :constant)
                :type :int}
     :inputs inputs
     :outputs outputs
     :buffers buffers
     :memory (:memory ir)
     :memory-layout {:physical-type physical-type
                     :element-type element-type
                     :element-bytes element-bytes
                     :alignment element-bytes
                     :buffers (mapv (fn [buffer]
                                      (merge (select-keys buffer [:id :direction
                                                                  :type :channels])
                                             (get-in ir [:memory (:id buffer)])))
                                    buffers)}
     ;; Argument indices are derived from the buffer list instead of hardcoded,
     ;; so a generator (`:input false`, D23) shifts every later argument down by
     ;; one and the ABI stays self-describing for both shapes.
     :bindings (let [has-input? (boolean (seq inputs))
                     base (if has-input? 1 0)]
                 (cond-> {:output {:role :output-buffer
                                   :id :output
                                   :argument-index base}
                          :frames (assoc (:frames ir)
                                         :role :frame-count
                                         :argument-index (inc base))
                          :sample-rate (assoc (:sample-rate ir)
                                              :role :sample-rate
                                              :argument-index (+ base 2))
                          :channel {:role :channel-index
                                    :type :int
                                    :binding (if (> (:channels ir) 1) :loop :constant)}
                          :controls (mapv (fn [index control]
                                            (assoc control
                                                   :role :control
                                                   :argument-index (+ base 3 index)))
                                          (range)
                                          controls)}
                   has-input? (assoc :input {:role :input-buffer
                                             :id :input
                                             :argument-index 0})))
     :frames (:frames ir)
     :sample-rate (:sample-rate ir)
     :controls controls
     ;; Host-supplied per-frame automation. Each lane is one slice of the flat
     ;; `:lanes` region, `:lane-stride` frames apart; the host addresses a lane
     ;; by name, and only a host writing memory directly needs the offset.
     :lane-stride (:frame-capacity ir)
     :lanes (mapv (fn [index lane]
                    (assoc (select-keys lane [:name :type :smooth :slew])
                           :role :lane
                           :index index
                           :buffer :lanes
                           :offset (* index (:frame-capacity ir))))
                  (range)
                  (:lanes ir))
     :state state
     :state-layout {:ownership :instance
                    :persistent? true
                    :values state-values}
     :scratch scratch
     :scratch-layout {:ownership :instance
                      :persistent? false
                      :region :scratch
                      :values scratch-values}
     :records records
     ;; Explicit logical layout of the one instance arena: persistent state
     ;; first, then per-invocation local-array scratch.
     :arena {:ownership :instance
             :element-type element-type
             :element-bytes element-bytes
             :alignment element-bytes
             :regions {:state {:role :state
                               :offset 0
                               :count state-slot-count
                               :persistent? true}
                       :scratch {:role :scratch
                                 :offset state-slot-count
                                 :count scratch-slot-count
                                 :persistent? false}}}
     :lifecycle (:lifecycle ir)
     :lifecycle-operations (->> [:init :reset]
                                (filter #(get (:lifecycle ir) %))
                                vec)
     :program {:ownership :artifact
               :mutable? false
               :shareable? true}
     :instance {:ownership :instance
                :state :persistent
                :allocation :per-instance
                :factory :create-instance
                :lifetime :explicit
                :handle {:kind :state-region
                         :regions [:state :scratch]
                         :shared? false}
                :operations {:create :create-instance
                             :reset :reset!
                             :destroy :destroy!}
                :instances :unbounded}
     :effects (:effects ir)
     :diagnostics (:diagnostics ir)}))

(declare compile-js-target)

(clojure.core/defn- compile-function-target
  [target lowered options]
  (when (or (seq (:state lowered)) (seq (:scratch lowered)))
    (throw (ex-info "stateful or local-array DSP definitions require the :process entry"
                    {:name (:name lowered)})))
  (case target
    :interpreter
    (artifact :interpreter :function lowered (function-abi lowered)
              (fn [& args]
                (interpreter/invoke lowered args))
              options)

    :jvm
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile (assoc lowered :options options))]
      (merge (artifact :jvm :function lowered (function-abi lowered) invoke
                       options)
             jvm-artifact))

    :js (compile-js-target :function lowered (function-abi lowered) options)

    (throw (ex-info "unsupported DSP compilation target"
                    {:target target
                     :supported #{:interpreter :jvm :js}}))))

(clojure.core/defn- target-instance
  "Describe a target's instance lifecycle in the target-neutral vocabulary.

  Every target creates, resets, and releases an instance; only the entry-point
  names differ. The abstract handle kind is what portable IR sees, so no
  target-specific object (a JVM class, a JS closure, a Wasm instance) leaks into
  the IR itself."
  [target abi]
  (let [base (:instance abi)]
    (case target
      :interpreter base
      :jvm base
      :js (assoc base
                 :factory :createKernel
                 :handle {:kind :kernel-handle :shared? false}
                 :operations {:create :createKernel
                              :reset :reset
                              :destroy :destroy})
      :wasm (assoc base
                   :factory :instantiate
                   :handle {:kind :wasm-instance
                            :region :state
                            :shared? false}
                   :operations {:create :instantiate
                                :reset :reset
                                ;; A Wasm instance owns its linear memory, so
                                ;; releasing it is dropping the instance.
                                :destroy :release})
      base)))

(clojure.core/defn- compile-js-target
  [entry lowered abi options]
  (let [js-ir (js-backend/lower (assoc lowered :options options))]
    {:target :js
     :entry entry
     :ir lowered
     :js-ir js-ir
     :abi (assoc abi :instance (target-instance :js abi))
     :physical-type (physical-type-policy lowered)
     :realtime (ir/validate-runtime-policy! lowered)
     :options options
     :module {:format :es-module
              :exports (if (= :process entry)
                         [:createKernel]
                         [:invoke])}
     :source (js-backend/emit-module js-ir)}))

(clojure.core/defn- compile-process-target
  [target lowered options]
  (case target
    :interpreter
    (let [create-instance
          (fn []
            (let [state (interpreter/create-state lowered)]
              {:state state
               :reset #(interpreter/reset-state! lowered state)
               :destroy #(interpreter/reset-state! lowered state)
               :invoke (fn [input output frames & controls]
                         (interpreter/process lowered input output frames
                                              controls state))
               :invoke-context
               (fn [context]
                 (interpreter/process-context lowered
                                              (assoc context :state state)))}))
          instance (create-instance)]
      (merge
       (artifact :interpreter :process lowered (process-abi lowered)
                 (:invoke instance)
                 options)
       instance
       {:initialize (:reset instance)
        :create-instance create-instance}))

    :jvm
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile-process (assoc lowered :options options))
          compiled (merge (artifact :jvm :process lowered (process-abi lowered) invoke
                                    options)
                          jvm-artifact)]
      (assoc compiled :initialize (:reset compiled)))

    :js (compile-js-target :process lowered (process-abi lowered) options)

    :wasm
    (let [artifact (wasm-backend/lower lowered)
          physical-type (physical-type-policy lowered)
          abi (let [abi (process-abi lowered)]
                (assoc abi :instance (target-instance :wasm abi)))]
      {:target :wasm
       :entry :process
       :ir lowered
       :abi abi
       :physical-type physical-type
       :realtime (ir/validate-runtime-policy! lowered)
       :options options
       :module {:format :wat
                :exports (:exports artifact)
                :memory (:memory artifact)
                :abi (assoc (:abi artifact) :physical-type physical-type
                            :instance (:instance abi))}
       :wat (:source artifact)
       :memory (:memory artifact)
       :metadata {:abi (assoc (:abi artifact) :physical-type physical-type
                              :instance (:instance abi))
                  :memory (:memory artifact)
                  :physical-type physical-type
                  :exports (:exports artifact)}
       :wasm-abi (assoc (:abi artifact) :physical-type physical-type
                        :instance (:instance abi))
       :binary {:format :wasm
                :source :wat
                :generator :browser-wabt
                :bytes nil}})

    (throw (ex-info "unsupported DSP compilation target"
                    {:target target
                     :supported #{:interpreter :jvm :js :wasm}}))))

(clojure.core/defn- check-capabilities!
  "Reject options this target cannot serve, naming what it does serve.

  This runs before any lowering, so a target that cannot honour a request fails
  with one legible error instead of emitting code that silently ignores it or
  substitutes something else."
  [target entry precision options]
  (let [problems (capability/problems {:target target
                                       :entry entry
                                       :precision precision
                                       :checks (:checks options)
                                       :wasm-math (:wasm-math options)})]
    (when (seq problems)
      (throw (ex-info (capability/describe problems target)
                      {:dsp/error :unsupported-capability
                       :target target
                       :entry entry
                       :precision precision
                       :capabilities problems}))))
  options)

(clojure.core/defn compile
  "Compile a DSP descriptor for `:interpreter`, `:jvm`, `:js`, or `:wasm`.

  The Wasm target currently returns WAT plus its linear-memory ABI; the
  browser-side WABT adapter turns that WAT into a binary module.

  The default `:entry` is `:function`, a scalar expression.  Use
  `{:entry :process}` to compile the same definition as a single-channel block
  processor: the first parameter is the input sample and remaining parameters
  are block-wide controls."
  ([definition]
   (compile definition {}))
  ([definition compile-options]
   (let [unit (if (linked-unit? definition)
                definition
                (link definition))
         definition (get-in unit [:definitions (:entry unit)])
         compile-options (or compile-options {})
         options (do
                   (when-not (map? compile-options)
                     (throw (ex-info "DSP compile options must be a map"
                                     {:dsp/error :invalid-definition
                                      :dsp/source (:source definition)
                                      :dsp/path [:options]
                                      :dsp/form compile-options
                                      :options compile-options})))
                   (descriptor/validate-options compile-options
                                                (:source definition)
                                                [:options])
                   (descriptor/normalize-options
                    (merge (:options definition) compile-options)
                    (:source definition)
                    [:options]))
         target (:target options)
         entry (:entry options)
         ;; Runtime validation is a capability, and only the interpreter
         ;; implements it, so its default has to follow the target. Otherwise
         ;; every generated-target compile would carry `:development` in its
         ;; effective options while honouring nothing, and an unsupported value
         ;; could not be told apart from the default.
         checks (if (or (contains? compile-options :checks)
                        (contains? (:options definition) :checks))
                  (:checks options)
                  (capability/checks-default target))
         options (assoc options :checks checks)
         _ (check-capabilities! target entry (:precision options) options)
         channels (if (or (contains? compile-options :channels)
                          (contains? (:options definition) :channels))
                    (:channels options)
                    (or (get-in definition [:process :channels])
                        (:channels options)))]
     (when-not (contains? #{:f32 :f64} (:precision options))
       (throw (ex-info "DSP precision must be :f32 or :f64"
                       {:dsp/error :unsupported-option
                        :option :precision
                        :precision (:precision options)
                        :target target})))
     (case entry
       :function (let [lowered (ir/lower-unit unit {:options options})]
                   (compile-function-target target
                                            (if (= :interpreter target)
                                              lowered
                                              (ir/inline-calls lowered))
                                            options))
       :process (let [lowered (ir/lower-process unit {:channels channels
                                                      :options options})]
                  (compile-process-target target
                                          (if (= :interpreter target)
                                            lowered
                                            (ir/inline-calls lowered))
                                          options))
       (throw (ex-info "unsupported DSP compilation entry"
                       {:entry entry
                        :supported #{:function :process}}))))))

(clojure.core/defn- check-live!
  "Reject use of a process instance that has been destroyed."
  [holder]
  (when (and (:live? holder) (not (deref (:live? holder))))
    (throw (ex-info "this DSP process instance has been destroyed"
                    {:target (:target holder)})))
  holder)

(clojure.core/defn process-instance?
  "True when `value` is an independent instance from `create-instance`."
  [value]
  (boolean (:dsp/instance value)))

(clojure.core/defn create-instance
  "Create an independent process instance from a compiled process artifact.

  The artifact is an immutable program shared by every instance; an instance
  owns its persistent state and its scratch arena. `:interpreter` and `:jvm`
  artifacts reuse one compiled code object and allocate a fresh state region
  per instance, so instantiating never recompiles. Call `destroy!` when the
  instance is no longer needed; the artifact remains usable either way."
  [artifact]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts can be instantiated"
                    {:entry (:entry artifact)})))
  (if-let [factory (:create-instance artifact)]
    (assoc (factory)
           :dsp/instance true
           :artifact artifact
           :target (:target artifact)
           :live? (atom true))
    (throw (ex-info "this DSP target does not support explicit process instances"
                    {:target (:target artifact)}))))

(clojure.core/defn destroy!
  "Release the persistent state owned by a process instance.

  The instance is unusable afterwards and further calls through it throw. A
  plain artifact owns its state for the artifact's lifetime and cannot be
  destroyed."
  [holder]
  (when-not (process-instance? holder)
    (throw (ex-info "destroy! expects an instance created by create-instance"
                    {:target (:target holder)})))
  (when (deref (:live? holder))
    (when-let [destroy (:destroy holder)]
      (destroy))
    (clojure.core/reset! (:live? holder) false))
  nil)

(clojure.core/defn invoke
  "Invoke a compiled artifact or instance through its target-specific adapter."
  [holder & args]
  (check-live! holder)
  (apply (:invoke holder) args))

(clojure.core/defn- lane-region
  "Flatten a `{:lane-name values}` context map into the ABI's single lane region.

  The kernel sees one flat region with compile-time offsets; the host supplies
  one named sequence per lane. This is the whole of the impedance mismatch, and
  it costs one copy per call — a real-time host writes the region directly and
  skips this path."
  [artifact context]
  (let [abi (:abi artifact)
        lanes (:lanes abi)]
    (when (and (seq lanes) (nil? (:lanes context)))
      ;; A lane process has no valid block without its region, so say that here
      ;; rather than letting a backend report an unknown buffer.
      (throw (ex-info "this DSP process declares per-frame lanes"
                      {:lanes (mapv :name lanes)})))
    (if (empty? lanes)
      context
      (let [stride (long (:lane-stride abi))
            supplied (:lanes context)
            f64? (= :f64 (get-in abi [:precision]))
            region (if f64?
                     (double-array (* stride (count lanes)))
                     (float-array (* stride (count lanes))))]
        (doseq [{:keys [name offset]} lanes]
          ;; The ABI names a lane with a symbol, as it does a control, but accept
          ;; the keyword spelling too: a key that silently missed would publish
          ;; zeros, which is the worst possible failure for automation.
          (let [values (or (get supplied name) (get supplied (keyword name)))]
            (when-not values
              (throw (ex-info "a DSP lane has no values for this block"
                              {:lane name :supplied (vec (keys supplied))})))
            (let [count (min (count values) stride)]
              (if f64?
                (let [^doubles target region]
                  (dotimes [index count]
                    (aset target (int (+ offset index)) (double (nth values index)))))
                (let [^floats target region]
                  (dotimes [index count]
                    (aset target (int (+ offset index)) (float (nth values index)))))))))
        (assoc context :lanes region)))))

(clojure.core/defn process!
  "Execute a process artifact or instance with its explicit ABI context.

  The context supplies `:output`, `:frames`, `:sample-rate`, and a vector of
  `:controls` in ABI order, plus `:input` when the process declares an input
  port. Interpreter and JVM artifacts and instances expose this checked context
  adapter; generated JS/Wasm modules expose their target-native entry points
  through the same lifecycle vocabulary."
  [holder context]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have a process ABI"
                      {:entry (:entry artifact)})))
    (check-live! holder)
    (if-let [invoke-context (:invoke-context holder)]
      (invoke-context (lane-region artifact context))
      (throw (ex-info "this DSP target does not expose an explicit process context adapter"
                      {:target (:target artifact)})))))

(clojure.core/defn process-checked!
  "Execute a JVM process artifact or instance through its checked raw ABI.

  Unlike `process!`, controls are supplied in their physical reusable arrays:
  `float-controls` and `int-controls`. It verifies primitive array classes,
  buffer shape/length, control storage, and the holder's own state before
  processing."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw checked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (check-live! holder)
     (jvm/process-checked! holder input output frames sample-rate
                           float-controls int-controls lanes))))

(clojure.core/defn process-unchecked!
  "Execute a JVM process artifact or instance through its raw primitive ABI.

  This deliberately performs no validation. Callers must provide buffers and
  physical f32/f64 control arrays matching the artifact ABI. Persistent state
  is owned by the holder: a plain artifact owns one state region, and each
  `create-instance` handle owns its own. Use `process-checked!` at
  setup/development boundaries; retain `:unchecked` and its matching
  `Dsp*Process` interface in a real-time host when direct dispatch is required."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw unchecked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (check-live! holder)
     (jvm/process-unchecked! holder input output frames sample-rate
                             float-controls int-controls lanes))))

(clojure.core/defn initialize!
  "Initialize a process artifact or instance according to its lifecycle ABI."
  [holder]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have a process lifecycle"
                      {:entry (:entry artifact)})))
    (when-not (get-in artifact [:abi :lifecycle :init])
      (throw (ex-info "this DSP instance does not support initialization"
                      {:target (:target artifact)})))
    (check-live! holder)
    (if-let [initialize (:initialize holder)]
      (initialize)
      (throw (ex-info "this DSP target does not expose initialization"
                      {:target (:target artifact)})))))

(clojure.core/defn reset!
  "Reset the persistent state of a process artifact or instance.

  Each instance resets only its own state; resetting one instance never
  disturbs another."
  [holder]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have persistent state"
                      {:entry (:entry artifact)})))
    (when-not (get-in artifact [:abi :lifecycle :reset])
      (throw (ex-info "this DSP instance does not support reset"
                      {:target (:target artifact)})))
    (check-live! holder)
    (if-let [reset (:reset holder)]
      (reset)
      (throw (ex-info "this DSP target does not expose reset"
                      {:target (:target artifact)})))))
