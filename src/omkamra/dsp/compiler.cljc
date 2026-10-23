(ns omkamra.dsp.compiler
  "The portable compilation pipeline (P13c, D77).

  `omkamra.dsp/compile` on the JVM and the browser's own compile entry point are
  the *same* pipeline: resolve the options, check the target can serve them, link
  the definition, lower it to portable IR, prepare that IR for the target, and
  emit the target's artifact. Everything in this namespace is `.cljc` and runs in
  both worlds.

  The one thing that cannot be portable is the JVM backend (`insn`), so a caller
  that has it supplies it as an extra target compiler; `omkamra.dsp` does exactly
  that. Everything else — including the reference interpreter and the JavaScript
  and Wasm emitters — is here, so a browser can compile a descriptor to a kernel
  itself.

  This is the *compiler* half of a build. A program that only runs kernels never
  requires it, and therefore never pulls in `malli` (through the descriptor) or
  `squint` (through the JS emitter) — the rule D77 states as 'the runtime half
  never requires the compiler'."
  (:refer-clojure :exclude [compile])
  (:require [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.targets.capability :as capability]
            [omkamra.dsp.targets.interpreter :as interpreter]
            [omkamra.dsp.targets.js :as js-backend]
            [omkamra.dsp.targets.wasm :as wasm-backend]))

(def wasm-assembler
  "The step that turns a Wasm artifact's WAT into the module a Wasm engine loads.

  A Wasm engine takes bytes and nothing else, so *someone* has to assemble them.
  That assembler lives in the browser's world (WABT, the pinned npm package), not
  in this compiler's, so the artifact names the step instead of carrying it: `:bytes`
  stays nil by design and the consumer that has the named assembler produces the
  module (D73). The version is recorded so an artifact says which assembler it was
  compiled for — keep it in step with `package.json`, which
  `omkamra.dsp.tests.wasm` checks."
  {:generator :browser-wabt
   :version "1.0.39"})

;; ---------------------------------------------------------------------------
;; Artifact construction
;; ---------------------------------------------------------------------------

(defn physical-type-policy
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

(defn artifact
  [target entry ir abi executable options]
  {:target target
   :entry entry
   :ir ir
   :abi abi
   :physical-type (physical-type-policy ir)
   :realtime (ir/validate-runtime-policy! ir)
   :options options
   :invoke executable})

(defn function-abi
  [ir]
  {:kind :function
   :name (:name ir)
   :params (mapv :type (:params ir))
   :return-type (:return-type ir)
   :effects (:effects ir)})

(defn process-abi
  [ir]
  (let [buffers (:buffers ir)
        inputs (filterv #(= :input (:direction %)) buffers)
        outputs (filterv #(= :output (:direction %)) buffers)
        ;; The declared smoothing mode is part of the ABI: it tells a host that
        ;; this control's value is carried across frames by the kernel, so
        ;; setting it is a target, not an instantaneous jump.
        controls (mapv #(select-keys % [:name :type :default :smooth :slew])
                       (:controls ir))
        state (mapv #(select-keys % [:name :type :size :init :constant :special
                                     :special-bit])
                    (:state ir))
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
        ;; Builtin, host-readable slots a voice publishes (D39). `:activity` is
        ;; the finish signal; a pool reads it back and frees a released voice
        ;; when it is <= 0. Specials occupy the low state slots in ascending-bit
        ;; order, so the leading `:special-count` slots are exactly the ones the
        ;; entry declares (never a callee's partition slot).
        special-count (long (or (:special-count ir) 0))
        special-slots (into {}
                            (keep (fn [value]
                                    (when (and (:special value)
                                               (< (long (:index value))
                                                  special-count))
                                      [(:special value)
                                       {:name (:name value)
                                        :type (:type value)
                                        :bit (:special-bit value)
                                        :index (:index value)}])))
                            state-values)
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
     ;; One evaluation per frame (D40): the output has `:count` channels and the
     ;; body addresses them explicitly, so there is no channel loop or binding.
     :channels {:count (:channels ir)
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
     ;; One lane region, one slice per voice: a fused entry renders every voice in
     ;; one call, so each call site reads its own slice and the host writes a
     ;; voice's lanes into the slice that voice is (F4). A plain artifact is one
     ;; voice, with one slice.
     :lane-slots (long (or (:lane-slots ir) 1))
     :lanes (mapv (fn [index lane]
                    (assoc (select-keys lane [:name :type :smooth :slew :required?])
                           :role :lane
                           :index index
                           :buffer :lanes
                           :offset (* index (:frame-capacity ir))))
                  (range)
                  (:lanes ir))
     ;; Host-supplied shared controls. Each is one slice of the flat `:shared`
     ;; region, `:shared-stride` frames apart — the same shape as a lane, but one
     ;; region for the whole pool rather than one per voice, so a broadcast
     ;; parameter is written once instead of once per voice (M5).
     :shared-stride (:frame-capacity ir)
     :shared (mapv (fn [index control]
                     (assoc (select-keys control [:name :type :required?])
                            :role :shared
                            :index index
                            :buffer :shared
                            :offset (* index (:frame-capacity ir))))
                   (range)
                   (:shared ir))
     :state state
     :special-mask (long (or (:special-mask ir) 0))
     :special-slots special-slots
     ;; A callee may publish a special of its own (M6 seam 4). Those are not the
     ;; entry's contract, so they are not in `:special-slots`; they are recorded
     ;; against the call site whose partition owns them, in evaluation order -- so
     ;; a fused host can read one slot's `:activity` and free it. `:index` is the
     ;; logical arena index, which a target resolves to a physical slot.
     :call-partitions (mapv (fn [partition]
                              (select-keys partition [:function-id :base :size
                                                      :specials :special-mask]))
                            (:call-partitions ir))
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

;; ---------------------------------------------------------------------------
;; Target compilers
;; ---------------------------------------------------------------------------

(defn target-instance
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

(defn compile-js-target
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

(defn compile-function-target
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

    :js (compile-js-target :function lowered (function-abi lowered) options)

    (throw (ex-info "unsupported DSP compilation target"
                    {:target target
                     :supported #{:interpreter :jvm :js}}))))

(defn compile-process-target
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
       :binary (assoc wasm-assembler
                      :format :wasm
                      :source :wat
                      :bytes nil)})

    (throw (ex-info "unsupported DSP compilation target"
                    {:target target
                     :supported #{:interpreter :jvm :js :wasm}}))))

;; ---------------------------------------------------------------------------
;; Option resolution
;; ---------------------------------------------------------------------------

(defn check-capabilities!
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

(defn resolve-options
  "The effective options for a definition, and the channel decision they carry.

  Returns `{:target :entry :options :channels :lowering-options}`. `:options` is
  the validated and normalized option map; `:lowering-options` is what module
  lowering reads, with the resolved channel count when there is one and no
  `:channels` key at all when the width is inferred from the body — so each
  definition counts its own (D40, F2/D57)."
  [definition compile-options]
  (let [compile-options (or compile-options {})]
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
    (let [options (descriptor/normalize-options
                   (merge (:options definition) compile-options)
                   (:source definition)
                   [:options])
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
          ;; An explicit `:channels` (compile option, definition option, or
          ;; `:process`) wins; otherwise nil lets the lowerer *count* the width
          ;; from the body's buffer accesses (D40). `normalize-options` always
          ;; injects a default, so the test is on the raw maps, not on `options`.
          channels (cond
                     (or (contains? compile-options :channels)
                         (contains? (:options definition) :channels))
                     (:channels options)
                     (:channels-declared? (:process definition))
                     (get-in definition [:process :channels])
                     :else nil)
          ;; The resolved width is the unit-wide channel policy: an explicit
          ;; top-level width must reach *every* layer, so it is written into the
          ;; options module lowering reads. `options :channels` at this point is
          ;; the `normalize-options` default whenever the width came from the
          ;; `:process` declaration rather than an option, so it is replaced with
          ;; the resolved value; when the width is inferred (`nil` here), the
          ;; option is dropped so each definition counts its own from its body
          ;; instead of being pinned to the default (F2/D57).
          lowering-options (cond-> options
                             (some? channels) (assoc :channels channels)
                             (nil? channels) (dissoc :channels))]
      {:target target
       :entry entry
       :options options
       :channels channels
       :lowering-options lowering-options})))

;; ---------------------------------------------------------------------------
;; Compilation
;; ---------------------------------------------------------------------------

(defn- linked-unit?
  [value]
  (= :compilation-unit (:dsp/kind value)))

(defmulti compile-target
  "Compile a lowered, ABI-described definition for one target.

  Everything before this step is portable; this step is not, so it is the one
  open axis of the pipeline. `:interpreter`, `:js` and `:wasm` share the
  `:default` method, which delegates to the target's own emitter; the JVM backend
  cannot live here — its bytecode generator `insn` is a JVM library — so the JVM
  face registers `:jvm` from the side that has it (D77). A host with another
  backend registers it the same way, which is what makes this a multimethod
  rather than a `case`.

  The method receives `{:keys [entry lowered abi options]}` and returns the
  artifact."
  (fn [target _context] target))

(defmethod compile-target :default
  [target {:keys [entry lowered options]}]
  (case entry
    :function (compile-function-target target lowered options)
    :process (compile-process-target target lowered options)))

(defn compile
  "Compile a DSP descriptor or linked unit for a target.

  The default `:entry` is `:function`, a scalar expression. Use
  `{:entry :process}` to compile the same definition as a block processor.
  Targets are `:interpreter`, `:js`, `:wasm`, and — where the JVM face has
  registered it — `:jvm`; see `compile-target`."
  ([definition] (compile definition {}))
  ([definition compile-options]
   (let [unit (if (linked-unit? definition)
                definition
                (linker/link definition))
         entry-definition (get-in unit [:definitions (:entry unit)])
         {:keys [target entry options channels lowering-options]}
         (resolve-options entry-definition compile-options)]
     (when-not (contains? #{:f32 :f64} (:precision options))
       (throw (ex-info "DSP precision must be :f32 or :f64"
                       {:dsp/error :unsupported-option
                        :option :precision
                        :precision (:precision options)
                        :target target})))
     (check-capabilities! target entry (:precision options) options)
     (let [lowered (case entry
                     :function (ir/lower-unit unit {:options lowering-options})
                     :process (ir/lower-process unit {:channels channels
                                                      :options lowering-options})
                     (throw (ex-info "unsupported DSP compilation entry"
                                     {:entry entry
                                      :supported #{:function :process}})))
           lowered (ir/prepare-for-target target entry lowered)
           abi (if (= :function entry)
                 (function-abi lowered)
                 (process-abi lowered))]
       (compile-target target {:entry entry
                               :lowered lowered
                               :abi abi
                               :options options})))))
