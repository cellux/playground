(ns omkamra.dsp
  "A small C-like DSP language embedded in Clojure data.

  The initial slice supports single-expression functions over the logical
  `:float` type, mutable locals, `do`, `set!`, `while`, float comparisons,
  boolean `and`/`or`/`not`, conditional expressions, explicit mono buffer
  loads/stores, and a channel-configurable block-processing ABI.
  Definitions are data; their bodies are never run as ordinary Clojure
  expressions."
  (:refer-clojure :exclude [compile defn reset!])
  (:require [clojure.string :as str]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.interpreter :as interpreter]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.js :as js-backend]
            [omkamra.dsp.jvm :as jvm]
            [omkamra.dsp.wasm :as wasm-backend]))

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
  [name params & body]
  (when-not (valid-definition-name? name)
    (macro-fail "dsp/defn name must be a non-blank symbol"
                &form [:name] {:name name}))
  (let [explicit-spec? (or (map? params)
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
                     (contains? spec :process)
                     (assoc :process (:process spec))
                     (contains? spec :options)
                     (assoc :options (:options spec))
                     (contains? spec :compiler-options)
                     (assoc :compiler-options (:compiler-options spec)))]
      (list 'def name (list 'quote captured)))))

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
        controls (mapv #(select-keys % [:name :type :default]) (:controls ir))
        state (mapv #(select-keys % [:name :type :init]) (:state ir))
        state-values (mapv (fn [index value]
                             (assoc value
                                    :index index
                                    :persistent? true))
                           (range)
                           state)
        physical-type (physical-type-policy ir)
        element-type (:element-type physical-type)
        element-bytes (:element-bytes physical-type)]
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
     :bindings {:input {:role :input-buffer
                        :id :input
                        :argument-index 0}
                :output {:role :output-buffer
                         :id :output
                         :argument-index 1}
                :frames (assoc (:frames ir)
                               :role :frame-count
                               :argument-index 2)
                :sample-rate (assoc (:sample-rate ir)
                                    :role :sample-rate
                                    :argument-index 3)
                :channel {:role :channel-index
                          :type :int
                          :binding (if (> (:channels ir) 1) :loop :constant)}
                :controls (mapv (fn [index control]
                                  (assoc control
                                         :role :control
                                         :argument-index (+ 4 index)))
                                (range)
                                controls)}
     :frames (:frames ir)
     :sample-rate (:sample-rate ir)
     :controls controls
     :state state
     :state-layout {:ownership :instance
                    :persistent? true
                    :values state-values}
     :lifecycle (:lifecycle ir)
     :lifecycle-operations (->> [:init :reset]
                                (filter #(get (:lifecycle ir) %))
                                vec)
     :instance {:ownership :artifact
                :state :persistent
                :allocation :compile-time
                :reset :explicit}
     :effects (:effects ir)
     :diagnostics (:diagnostics ir)}))

(declare compile-js-target)

(clojure.core/defn- compile-function-target
  [target lowered options]
  (when (seq (:state lowered))
    (throw (ex-info "stateful DSP definitions require the :process entry"
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

(clojure.core/defn- compile-js-target
  [entry lowered abi options]
  (let [js-ir (js-backend/lower (assoc lowered :options options))]
    {:target :js
     :entry entry
     :ir lowered
     :js-ir js-ir
     :abi abi
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
    (let [state (interpreter/create-state lowered)]
      (merge
       (artifact :interpreter :process lowered (process-abi lowered)
                 (fn [input output frames & controls]
                   (interpreter/process lowered input output frames controls state))
                 options)
       {:state state
        :initialize #(interpreter/reset-state! lowered state)
        :reset #(interpreter/reset-state! lowered state)
        :invoke-context (fn [context]
                          (interpreter/process-context lowered
                                                       (assoc context :state state)))}))

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
          physical-type (physical-type-policy lowered)]
      {:target :wasm
       :entry :process
       :ir lowered
       :abi (process-abi lowered)
       :physical-type physical-type
       :realtime (ir/validate-runtime-policy! lowered)
       :options options
       :module {:format :wat
                :exports (:exports artifact)
                :memory (:memory artifact)
                :abi (assoc (:abi artifact) :physical-type physical-type)}
       :wat (:source artifact)
       :memory (:memory artifact)
       :metadata {:abi (assoc (:abi artifact) :physical-type physical-type)
                  :memory (:memory artifact)
                  :physical-type physical-type
                  :exports (:exports artifact)}
       :wasm-abi (assoc (:abi artifact) :physical-type physical-type)
       :binary {:format :wasm
                :source :wat
                :generator :browser-wabt
                :bytes nil}})

    (throw (ex-info "unsupported DSP compilation target"
                    {:target target
                     :supported #{:interpreter :jvm :js :wasm}}))))

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

(clojure.core/defn invoke
  "Invoke a compiled artifact through its target-specific adapter."
  [artifact & args]
  (apply (:invoke artifact) args))

(clojure.core/defn process!
  "Execute an interpreter process artifact with its explicit ABI context.

  The context supplies `:input`, `:output`, `:frames`, `:sample-rate`, and a
  vector of `:controls` in ABI order. The artifact owns persistent state.
  Interpreter and JVM artifacts expose this checked context adapter; generated
  JS/Wasm modules expose their target-native process entry points."
  [artifact context]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts have a process ABI"
                    {:entry (:entry artifact)})))
  (if-let [invoke-context (:invoke-context artifact)]
    (invoke-context context)
    (throw (ex-info "this DSP target does not expose an explicit process context adapter"
                    {:target (:target artifact)}))))

(clojure.core/defn process-checked!
  "Execute a JVM process through its checked raw primitive ABI.

  Unlike `process!`, controls are supplied in their physical reusable arrays:
  `float-controls` and `int-controls`. It verifies primitive array classes,
  buffer shape/length, control storage, and artifact-owned state before
  processing."
  [artifact input output frames sample-rate float-controls int-controls]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts have a process ABI"
                    {:entry (:entry artifact)})))
  (when-not (= :jvm (:target artifact))
    (throw (ex-info "the raw checked process ABI is currently JVM-specific"
                    {:target (:target artifact)})))
  (jvm/process-checked! artifact input output frames sample-rate
                        float-controls int-controls))

(clojure.core/defn process-unchecked!
  "Execute a JVM process through its allocation-free raw primitive ABI.

  This deliberately performs no validation. Callers must provide buffers and
  physical f32/f64 control arrays matching the artifact ABI. Persistent state
  is owned by the artifact. Use `process-checked!` at setup/development
  boundaries; retain `:unchecked` and its matching `Dsp*Process` interface in
  a real-time host when direct dispatch is required."
  [artifact input output frames sample-rate float-controls int-controls]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts have a process ABI"
                    {:entry (:entry artifact)})))
  (when-not (= :jvm (:target artifact))
    (throw (ex-info "the raw unchecked process ABI is currently JVM-specific"
                    {:target (:target artifact)})))
  (jvm/process-unchecked! artifact input output frames sample-rate
                          float-controls int-controls))

(clojure.core/defn initialize!
  "Initialize a process instance according to its lifecycle ABI."
  [artifact]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts have a process lifecycle"
                    {:entry (:entry artifact)})))
  (when-not (get-in artifact [:abi :lifecycle :init])
    (throw (ex-info "this DSP instance does not support initialization"
                    {:target (:target artifact)})))
  (if-let [initialize (:initialize artifact)]
    (initialize)
    (throw (ex-info "this DSP target does not expose initialization"
                    {:target (:target artifact)}))))

(clojure.core/defn reset!
  "Reset the persistent state of a process artifact."
  [artifact]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts have persistent state"
                    {:entry (:entry artifact)})))
  (when-not (get-in artifact [:abi :lifecycle :reset])
    (throw (ex-info "this DSP instance does not support reset"
                    {:target (:target artifact)})))
  (if-let [reset (:reset artifact)]
    (reset)
    (throw (ex-info "this DSP target does not expose reset"
                    {:target (:target artifact)}))))
