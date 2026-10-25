(ns omkamra.dsp.synth.bundle
  "Compile a synth description into an artifact and its runtime configuration.

  This is the world-independent half of assembling a synth: it compiles the
  description's voice for a target and derives the runtime configuration — the
  control plan, the lanes and the shared parameters — *from the artifact's own
  ABI*, so the two cannot disagree. `:runtime` is pure data, so it can travel; the
  kernel is the world's own and is added beside the bundle, by `omkamra.dsp.hosts.jvm`
  on the JVM and by the browser's assembler from the same bundle.

  Two seams keep it portable. The voice is resolved through `:definitions` —
  `{qualified-id descriptor}`, the same map `dsp/compile` takes — because a world
  without runtime namespace resolution (ClojureScript, T7) cannot look an id up,
  where the JVM falls back to `requiring-resolve`. And compilation goes through
  `omkamra.dsp/compile`, which is each world's own face of the pipeline.

  `:target` defaults to `:jvm`, the world this assembler was written for. It is
  deliberately present: the descriptor's own default is `:interpreter`, so a
  bundle must not fall through to it. A browser caller always passes the target
  it compiles for."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.synth :as synth]))

(def ^:private realtime-requirements
  "What a voice kernel must not do, for a pool to be allowed to run it. The
  metadata is the artifact's own; a kernel that reports anything else is refused
  while the pool is being built, not while audio is running."
  [[:allocations? false] [:locks? false] [:reflection? false] [:io? false]])

(defn- value-of
  "The descriptor a Var or a descriptor value holds."
  [definition]
  (if (var? definition) @definition definition))

(defn- resolve-voice
  "The Var or descriptor a description's `:voice :definition` names.

  A description may carry the definition itself — the JVM's own `dsp/defn` yields
  a Var, a descriptor value stays as it is — or an id to look up. An id is looked
  up in the caller's `:definitions` first, which is the only way a world without
  runtime namespace resolution can name one (ClojureScript, T7); the JVM falls
  back to `requiring-resolve`."
  [definition {:keys [definitions]}]
  (cond
    (or (var? definition) (map? definition))
    definition

    (symbol? definition)
    (or (get definitions definition)
        #?(:clj (requiring-resolve definition))
        (synth/fail "a synth voice definition cannot be resolved"
                    {:definition definition}))

    :else
    (synth/fail "a synth voice :definition must be an id or a descriptor"
                {:definition definition})))

(defn synth-bundle
  "Compile a synth description into an artifact and its runtime configuration.

  Returns `{:description ..., :runtime ..., :artifact ..., :containment ...,
  :realtime ...}` — everything but the kernel. `:runtime` is pure data, so it can
  be sent to another host, and the artifact stays with the compiler. The world
  adds its own kernel beside it: `omkamra.dsp.hosts.jvm/compile-synth` on the JVM, and
  a browser assembler from the same bundle.

  `compile-options` selects the target, precision, and `:containment` policy
  (`:reset` default, or `:sanitize`/`:off`); the entry is always `:process`,
  because a voice is a generator, not a scalar function. `:definitions` reaches
  the voice and its calls, as it does for `dsp/compile`."
  ([description] (synth-bundle description {}))
  ([description compile-options]
   (let [normalized (synth/normalize-description description)
         definition (get-in normalized [:voice :definition])
         resolved (resolve-voice definition compile-options)
         containment (get compile-options :containment :reset)
         ;; Fusion is the one compile option that is *consumed* here rather than
         ;; passed on: it changes what is compiled (D52). The rest travel to
         ;; `dsp/compile`, whose options are closed, which is why a stray key
         ;; fails loudly instead of being ignored.
         fusion (when-let [options (:fusion compile-options)]
                  (let [slots (long (or (:slots options) 0))]
                    (when-not (pos? slots)
                      (synth/fail "a fused synth needs a positive :slots count"
                                  {:fusion options}))
                    (when (seq (dissoc options :slots :guard))
                      (synth/fail "a fused synth takes only :slots and :guard"
                                  {:fusion options}))
                    {:slots slots
                     :guard (not (false? (:guard options)))}))
         base-artifact (dsp/compile resolved
                                    (assoc (merge {:target :jvm}
                                                  (dissoc compile-options
                                                          :containment :fusion))
                                           :entry :process))
         abi (:abi base-artifact)
         ;; The specials a *pool* reads per voice are the voice's, from the base
         ;; artifact: a fused slot publishes the same set (asserted below), so
         ;; the pool's `:activity` position is the same whether it drives a plain
         ;; or a fused kernel.
         voice-specials-mask (long (get-in base-artifact [:abi :special-mask] 0))
         voice-specials-count (long (count (get-in base-artifact [:abi :special-slots])))
         ;; Which controls a fused instance expands per slot: the ones a pool
         ;; writes per *voice*, which are the runtime controls and the per-voice
         ;; parameters. A shared control is written once for the whole pool, so
         ;; hoisting it is already what it means (D50).
         per-slot (when fusion
                    (mapv (comp keyword :control)
                          (filter #(#{:runtime :per-voice} (get-in % [:storage :kind]))
                                  (synth/control-plan normalized (:controls abi)
                                                      (:shared abi)))))
         ;; Per-slot *skipping* (M6b): a slot that has never been gated is not
         ;; evaluated at all, so a fused process with a sparse patch does not pay
         ;; for every slot it was compiled with. `guard` owns a per-call-site
         ;; latch, so a reset clears it; it is applied only to a voice that
         ;; actually declares `gate`, which is the control a pool opens.
         ;; `guard` (M6b) is written for a *module-shaped* voice: it skips a slot by
         ;; reproducing the sample it would have produced. A process-shaped voice
         ;; writes a region instead, so guarding it is a different mechanism; until
         ;; that exists, such a voice is fused without it (an optimisation, not
         ;; correctness).
         guarded? (and fusion (:guard fusion)
                       (not= :void (:return-type (value-of resolved)))
                       (some #(= 'gate (:name %)) (:controls abi)))
         voice (if guarded?
                 (comb/guard (value-of resolved))
                 (value-of resolved))
         artifact (if fusion
                    (dsp/compile (comb/poly voice
                                            {:count (:slots fusion)
                                             :per-slot per-slot})
                                 (assoc (merge {:target :jvm}
                                               (dissoc compile-options
                                                       :containment :fusion))
                                        :entry :process))
                    base-artifact)
         ;; A callee may publish a special (M6 seam 4), so a fused slot carries
         ;; the voice's `:activity`. Every slot must carry the same set the base
         ;; voice does, or the pool's readback would compare the wrong slot; a
         ;; mismatch is a lowering bug, refused here rather than muted later.
         _ (when fusion
             (let [masks (mapv #(long (or (:special-mask %) 0))
                               (:call-partitions (:ir artifact)))]
               (when-not (every? #(= voice-specials-mask %) masks)
                 (synth/fail "the fused slots do not expose the voice's specials"
                             {:voice-mask voice-specials-mask :slot-masks masks}))))
         buffers (set (map :id (:buffers abi)))
         ;; The voice's own width is authoritative (D40); a description may
         ;; declare it, but then it must agree.
         abi-channels (long (get-in abi [:channels :count] 1))]
     (when (and (:channels description)
                (not= (long (:channels description)) abi-channels))
       (synth/fail "the synth description's :channels does not match its voice"
                   {:declared (:channels description) :voice abi-channels}))
     (when (contains? buffers :input)
       (synth/fail "a synth voice must be a generator with no input port"
                   {:definition definition :buffers (:buffers abi)}))
     ;; Every target reports the same real-time policy, so the check is not
     ;; JVM-specific: a voice kernel must not allocate, lock, reflect, or do I/O
     ;; on the audio path, wherever it runs.
     (doseq [[flag expected] realtime-requirements]
       (let [actual (get (:realtime artifact) flag)]
         (when-not (= expected actual)
           (synth/fail "a synth voice kernel is not safe for the audio path"
                       {:definition definition :flag flag
                        :expected expected :actual actual
                        :realtime (:realtime artifact)}))))
     {:description normalized
      :runtime (cond-> (assoc (synth/runtime-config normalized (:controls abi)
                                                    (:shared abi))
                              :channels abi-channels
                              ;; The specials the pool reads are the voice's: a
                              ;; fused slot republishes them from its partition
                              ;; (M6 seam 4), so `:activity` is readable whether
                              ;; the kernel is one voice or S slots.
                              :special-mask voice-specials-mask
                              :special-count voice-specials-count
                      ;; The lane layout travels with the runtime config so a
                      ;; pool can validate a lane's name and scope without the
                      ;; ABI, and so a browser host rebuilt from a served bundle
                      ;; has it too (M4). The shared region travels the same way
                      ;; (M5): it is one region for the whole pool, so it is the
                      ;; pool that fills it.
                              :lanes (mapv (fn [lane]
                                             {:name (name (:name lane))
                                              :offset (long (:offset lane))
                                              :required? (boolean (:required? lane))})
                                           (:lanes abi))
                              :lane-stride (long (or (:lane-stride abi) 0))
                              :shared (mapv (fn [control]
                                              {:name (name (:name control))
                                               :offset (long (:offset control))
                                               :required? (boolean (:required? control))})
                                            (:shared abi))
                              :shared-stride (long (or (:shared-stride abi) 0)))
                 ;; A fused artifact *is* `slots` voices (M6), so the pool's
                 ;; capacity is the slot count rather than the description's: the
                 ;; kernel maps a voice onto a slot by creation order, so a larger
                 ;; pool hands several handles the same slot -- and a host step
                 ;; that writes every voice (the modulation publisher) then
                 ;; clobbers that slot's controls with each handle's in turn.
                 fusion
                 (assoc :capacity (:slots fusion)
                        :fusion {:slots (:slots fusion)
                                 :controls (mapv (comp keyword :name) (:controls abi))
                                 :per-slot per-slot}))
      :artifact artifact
      :containment containment
      :realtime (:realtime artifact)})))
