(ns omkamra.dsp
  "A small C-like DSP language embedded in Clojure data.

  The initial slice supports single-expression functions over the logical
  `:float` type, mutable locals, `do`, `set!`, `while`, float comparisons,
  boolean `and`/`or`/`not`, conditional expressions, explicit mono buffer
  loads/stores, and a channel-configurable block-processing ABI.
  Definitions are data; their bodies are never run as ordinary Clojure
  expressions."
  (:refer-clojure :exclude [compile defn defstruct fn reset!])
  (:require [clojure.string :as str]
            [omkamra.dsp.compiler :as compiler]
            [omkamra.dsp.literals :as literals]
            [omkamra.dsp.readers]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.targets.jvm :as jvm]))

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

(clojure.core/defn- box-state-literals
  "Box the float literals of every `:next` state transition, and of nothing else
  in the state declaration: an `:init` is a declared value, typed by the slot it
  initializes, and needs no box."
  [states]
  (when states
    (mapv (clojure.core/fn [state]
            (if (contains? state :next)
              (update state :next literals/box-float-literals)
              state))
          states)))

(clojure.core/defn- macro-resolution
  "Capture only namespace mappings used by ordinary call forms at the
  definition site. This is a compact, stable snapshot of Clojure resolution."
  [ns forms]
  (let [references (macro-reference-symbols forms)
        aliases (ns-aliases ns)
        refers (ns-refers ns)]
    {:aliases (into {}
                    (keep (clojure.core/fn [reference]
                            (when-let [target (get aliases
                                                   (some-> reference namespace symbol))]
                              [(symbol (namespace reference)) (ns-name target)])))
                    references)
     :refers (into {}
                   (keep (clojure.core/fn [reference]
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

(clojure.core/defn- capture-definition
  "Build a `:function` descriptor from `dsp/defn`/`dsp/fn` arguments.

  `label` names the form for diagnostics, `name` is the definition's name (or a
  generated one for an anonymous `dsp/fn`), and `form` is the macro's `&form`, for
  the source location."
  [label name form args]
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
        spec (merge spec options)
        ;; A callee asks to be copied into its call sites rather than called,
        ;; with `^:inline` metadata (on the name for `dsp/defn`, on the form for
        ;; `dsp/fn`) or an `:inline true` spec key (D47).
        inline? (boolean (or (:inline spec)
                             (:inline (meta name))
                             (:inline (meta form))))]
    (when-not (vector? params)
      (macro-fail (str label " params must be a vector")
                  form [:params] {:name name :params params}))
    (when (and (not explicit-params?) (not (every? symbol? params)))
      (macro-fail (str label " parameters must be symbols")
                  form [:params] {:name name :params params}))
    (when-not (= (count params)
                 (count (set (map #(if (symbol? %) % (:name %)) params))))
      (macro-fail (str label " parameter names must be unique")
                  form [:params] {:name name :params params}))
    (when-not (pos? (count body))
      (macro-fail (str label " requires at least one body expression")
                  form [:body] {:name name :body body}))
    (when (contains? spec :dependencies)
      (macro-fail (str label " infers dependencies from ordinary DSP calls")
                  form [:dependencies] {:name name}))
    (let [source (merge
                  (select-keys (meta form) [:file :line :column])
                  {:namespace (ns-name *ns*)})]
      (cond-> {:dsp/kind :function
               :name name
               :params params
               :return-type (or (:return-type spec) :float)
               :state (box-state-literals (:state spec))
               ;; Like `clojure.core/defn`, several body forms are an implicit
               ;; `(do ...)`. A single form is left exactly as written, so a
               ;; captured descriptor's `[:body 0]` is still the author's form.
               :body (mapv literals/box-float-literals
                           (if (= 1 (count body)) body [(cons 'do body)]))
               :source source
               :resolution (macro-resolution *ns* body)}
        (some? doc) (assoc :doc doc)
        inline? (assoc :inline true)
        (contains? spec :process)
        (assoc :process (:process spec))
        (contains? spec :options)
        (assoc :options (:options spec))
        (contains? spec :compiler-options)
        (assoc :compiler-options (:compiler-options spec))))))

(clojure.core/defmacro defn
  "Capture a DSP function definition as a descriptor.

  The shorthand form is:

    (dsp/defn gain [sample amount] (* sample amount))

  As in `defn`, several body expressions are an implicit `(do ...)`:

    (dsp/defn stereo-cross [sample]
      (buffer-store :output 0 0 sample)
      (buffer-store :output 1 0 sample))

  An explicit descriptor may be packed into the parameter vector:

    (dsp/defn gain
      [{:params [{:name sample :type :float}]
        :return-type :float}
       sample])

  A definition asks to be **copied into its call sites** rather than called with
  `^:inline` on its name, or with `:inline true` in the descriptor. Inlining is
  the exception, not the default (D47), because a target may not be able to call:

    (dsp/defn ^:inline clamp01 [x] (max 0.0 (min 1.0 x)))

  Both forms capture source data; DSP bodies are never executed as ordinary
  Clojure expressions during definition or compilation."
  [name & args]
  (when-not (valid-definition-name? name)
    (macro-fail "dsp/defn name must be a non-blank symbol"
                &form [:name] {:name name}))
  (list 'def name (list 'quote (capture-definition "dsp/defn" name &form args))))

(clojure.core/defmacro fn
  "Capture an anonymous DSP definition as a value.

  `dsp/fn` is to `dsp/defn` what `clojure.core/fn` is to `defn`: it captures the
  same descriptor but returns it as a value instead of binding a Var, so a
  definition composes with `->` and with transformers:

    (def voice
      (-> (dsp/fn [sample amount] {:process {:controls [{:name amount}]}}
            (* sample amount))
          (unison {:count 3})))

  The descriptor carries a generated internal name and the defining namespace's
  source, so it links directly; `dsp/def` replaces the name when the value is
  bound to a Var.

  As in `dsp/defn`, an anonymous definition asks to be copied into its call
  sites rather than called, with `:inline true` in its descriptor — or with
  `^:inline` on the whole form:

    (def clipper
      (-> (dsp/fn [sample] {:inline true} (max -1.0 (min 1.0 sample)))
          (unison {:count 3})))"
  [& args]
  (list 'quote (capture-definition "dsp/fn" (gensym "dsp-fn-") &form args)))

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
    (let [fields (mapv (clojure.core/fn [field]
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

(clojure.core/defmacro def
  "Bind `name` to a DSP definition produced by a transformer.

  `dsp/defn` captures a definition from a source form; `dsp/def` takes an
  ordinary Clojure expression whose *value* is already a definition, so derived
  voices compose with `->`:

    (dsp/def unison-lead
      (-> lead-voice
          (unison {:count 3 :detune 15})))

  The Var holds the descriptor and is resolvable by the linker exactly like a
  `dsp/defn` Var. `:name` and `:source` are stamped here, so a transformer does
  not need to know where it is being defined."
  [name definition]
  (when-not (valid-definition-name? name)
    (macro-fail "dsp/def name must be a non-blank symbol"
                &form [:name] {:name name}))
  (let [source (merge (select-keys (meta &form) [:file :line :column])
                      {:namespace (ns-name *ns*)})]
    `(~'def ~name
            (let [definition# ~definition]
              (when-not (and (map? definition#) (= :function (:dsp/kind definition#)))
                (throw (ex-info "dsp/def expects a DSP definition value"
                                {:name '~name :definition definition#})))
              (assoc definition# :name '~name :source (quote ~source))))))

(clojure.core/defn- carried-definitions
  "Collect the definitions reachable from `entries` through `:dsp/dependencies`.

  A call-emitting transformer returns a definition whose body calls the original,
  which for an anonymous `dsp/fn` value has no Var to resolve. The transformer
  records the original in `:dsp/dependencies`; here those become supplied
  definitions, transitively, so a chain of transforms links."
  [entries]
  (loop [pending (vec entries)
         found {}]
    (if (empty? pending)
      found
      (let [entry (peek pending)
            value (if (var? entry) @entry entry)
            dependencies (when (map? value) (:dsp/dependencies value))
            fresh (when (map? dependencies)
                    (reduce-kv (clojure.core/fn [m id dependency]
                                 (if (contains? found id)
                                   m
                                   (assoc m id dependency)))
                               {}
                               dependencies))]
        (recur (into (pop pending) (vals fresh))
               (merge found fresh))))))

(clojure.core/defn link
  "Link a DSP descriptor or Var into a compilation unit.

  A linked unit resolves ordinary DSP calls using the defining namespace's
  Clojure alias/refer mappings. Anonymous descriptors can provide reachable
  definitions with `{:definitions {qualified-id descriptor-or-var}}`.

  A definition produced by a call-emitting transformer carries the definitions it
  calls in `:dsp/dependencies`; they are supplied here automatically, so a
  transform of an anonymous `dsp/fn` value links like one of a named voice."
  ([definition]
   (link definition {}))
  ([definition options]
   (let [options (or options {})
         supplied (:definitions options)
         carried (carried-definitions (cons definition (vals supplied)))]
     (linker/link definition
                  (assoc options :definitions (merge carried supplied))))))

(clojure.core/defn- linked-unit?
  [value]
  (= :compilation-unit (:dsp/kind value)))

(clojure.core/defn- jvm-target-compiler
  "The one target the portable pipeline cannot hold: `insn` is a JVM library, so
  the JVM backend is handed to `compiler/compile` from this side (P13c, D77).

  The pipeline is the same one the browser runs; only the final target step is
  JVM-only, and this is that step."
  [{:keys [entry lowered abi options]}]
  (case entry
    :function
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile (assoc lowered :options options))]
      (merge (compiler/artifact :jvm :function lowered abi invoke options)
             jvm-artifact))

    :process
    (let [{:keys [invoke] :as jvm-artifact}
          (jvm/compile-process (assoc lowered :options options))
          compiled (merge (compiler/artifact :jvm :process lowered abi invoke options)
                          jvm-artifact)]
      (assoc compiled :initialize (:reset compiled)))))

(clojure.core/defn compile
  "Compile a DSP descriptor for `:interpreter`, `:jvm`, `:js`, or `:wasm`.

  The pipeline itself is portable and lives in `omkamra.dsp.compiler` (P13c), so
  the browser compiles DSP source with the same code. This entry point adds the
  two things that are the JVM's: the JVM backend, whose bytecode generator
  (`insn`) is a JVM library, and `dsp/link`, because a definition produced by a
  call-emitting transformer carries the definitions it calls by Var.

  The Wasm target returns WAT plus its linear-memory ABI; the browser-side WABT
  assembler turns that WAT into a binary module. The default `:entry` is
  `:function`, a scalar expression.  Use `{:entry :process}` to compile the same
  definition as a single-channel block processor: the first parameter is the
  input sample and remaining parameters are block-wide controls."
  ([definition]
   (compile definition {}))
  ([definition compile-options]
   (let [unit (if (linked-unit? definition)
                definition
                (link definition))]
     (compiler/compile unit compile-options {:jvm jvm-target-compiler}))))

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

(clojure.core/defn- real-region?
  "True for a primitive float/double array: a region the host built itself.

  A real-time host writes its regions in place, so the context may carry the
  region *itself* rather than the named values this adapter would flatten into
  one. Both are accepted; the difference is who owns the copy."
  [value]
  (or (instance? (class (double-array 0)) value)
      (instance? (class (float-array 0)) value)))

(clojure.core/defn- lane-region
  "Flatten a `{:lane-name values}` context map into the ABI's single lane region.

  The kernel sees one flat region with compile-time offsets; the host supplies
  one named sequence per lane. This is the whole of the impedance mismatch, and
  it costs one copy per call — a real-time host writes the region directly and
  skips this path, passing the region it already owns as `:lanes`."
  [artifact context]
  (let [abi (:abi artifact)
        lanes (:lanes abi)]
    (when (and (some :required? lanes) (nil? (:lanes context)))
      ;; A process with a mandatory (`:lane`) lane has no valid block without
      ;; its region, so say that here rather than letting a backend report an
      ;; unknown buffer. A `:has-lane` lane is optional: an absent region is
      ;; built zeroed, so it reads as "no automation" and the scalar stands.
      (throw (ex-info "this DSP process declares per-frame lanes"
                      {:lanes (mapv :name lanes)})))
    (cond
      (empty? lanes) context
      ;; The region is already built: the host owns it and wrote it itself.
      (real-region? (:lanes context)) context
      :else
      (let [stride (long (:lane-stride abi))
            supplied (:lanes context)
            f64? (= :f64 (get-in abi [:precision]))
            region (if f64?
                     (double-array (* stride (count lanes)))
                     (float-array (* stride (count lanes))))]
        (doseq [{:keys [name offset required?]} lanes]
          ;; The ABI names a lane with a symbol, as it does a control, but accept
          ;; the keyword spelling too: a key that silently missed would publish
          ;; zeros, which is the worst possible failure for automation.
          (let [values (or (get supplied name) (get supplied (keyword name)))]
            (when (and (nil? values) required?)
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

(clojure.core/defn- shared-region
  "Flatten a `{:shared-name value}` context map into the ABI's `:shared` region.

  A shared control is block-rate, so a bare number is broadcast across its
  `:shared-stride` slice and a sequence is written as it comes — which is also
  what a host writing a per-frame *global* curve wants. The region is one for the
  whole pool, not one per voice (M5), so this is the copy that a real-time host
  skips by writing the region directly, passing the region it already owns as
  `:shared`."
  [artifact context]
  (let [abi (:abi artifact)
        shared (:shared abi)]
    (cond
      (empty? shared) context
      ;; The region is already built: the host owns it and wrote it itself.
      (real-region? (:shared context)) context
      :else
      (let [stride (long (:shared-stride abi))
            supplied (:shared context)
            f64? (= :f64 (get-in abi [:precision]))
            region (if f64?
                     (double-array (* stride (count shared)))
                     (float-array (* stride (count shared))))
            write! (if f64?
                     (clojure.core/fn [target at value]
                       (aset ^doubles target (int at) (double value)))
                     (clojure.core/fn [target at value]
                       (aset ^floats target (int at) (float value))))]
        (when (and supplied (not (map? supplied)))
          (throw (ex-info "a DSP shared region must be a map of control to value"
                          {:shared supplied})))
        (doseq [{:keys [name offset required?]} shared]
          ;; A control name is spelled as a symbol in the ABI; accept the keyword
          ;; spelling too, since a key that silently missed would publish zeros.
          (let [value (or (get supplied name) (get supplied (keyword name)))]
            (cond
              (nil? value)
              (when required?
                (throw (ex-info "a DSP shared control has no value for this block"
                                {:shared name :supplied (vec (keys supplied))})))

              (number? value)
              (dotimes [frame stride]
                (write! region (+ offset frame) value))

              :else
              (let [count (min (clojure.core/count value) stride)]
                (dotimes [index count]
                  (write! region (+ offset index) (nth value index)))))))
        (assoc context :shared region)))))

(clojure.core/defn process!
  "Execute a process artifact or instance with its explicit ABI context.

  The context supplies `:output`, `:frames`, `:sample-rate`, and a vector of
  `:controls` in ABI order, plus `:input` when the process declares an input
  port. Interpreter and JVM artifacts and instances expose this checked context
  adapter; generated JS/Wasm modules expose their target-native entry points
  through the same lifecycle vocabulary.

  `:lanes` carries a per-frame automation region (D28); `:shared` carries the
  pool-wide shared controls (M5). Either may be absent when the process declares
  none of that kind, may be a map of control name to values (this adapter then
  flattens it), or may be the region itself, for a host that already owns and
  writes one."
  [holder context]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have a process ABI"
                      {:entry (:entry artifact)})))
    (check-live! holder)
    (if-let [invoke-context (:invoke-context holder)]
      (invoke-context (shared-region artifact (lane-region artifact context)))
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
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw checked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (check-live! holder)
     (jvm/process-checked! holder input output frames sample-rate
                           float-controls int-controls lanes shared))))

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
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw unchecked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (check-live! holder)
     (jvm/process-unchecked! holder input output frames sample-rate
                             float-controls int-controls lanes shared))))

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
