(ns omkamra.dsp.hosts
  "Compile a synth description into an artifact, a runtime configuration, and the
  host kernel a pool over it runs on (P10).

  This namespace assembles. The kernels live in `omkamra.dsp.hosts.jvm`,
  `omkamra.dsp.hosts.interpreter`, `omkamra.dsp.hosts.js`, and
  `omkamra.dsp.hosts.wasm`, and the plumbing they share — the containment policy
  and the planar blocks a voice is mixed through — in
  `omkamra.dsp.hosts.mixing`.

  `synth-bundle` is the half that travels: it compiles the voice artifact and
  derives the runtime configuration *from that artifact's own ABI*, so the
  control plan's order is the ABI's order by construction rather than by
  agreement. `compile-synth` adds the JVM-side kernel to it; a browser host
  builds its own from the same bundle."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.combinators :as comb]
            [omkamra.dsp.hosts.fusion :as fusion]
            [omkamra.dsp.hosts.interpreter :as interpreter-host]
            [omkamra.dsp.hosts.jvm :as jvm-host]
            [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth :as synth]
            [omkamra.dsp.synth.pool :as pool]))

(def ^:private realtime-requirements
  "What a voice kernel must not do, for a pool to be allowed to run it. The
  metadata is the artifact's own; a kernel that reports anything else is refused
  while the pool is being built, not while audio is running."
  [[:allocations? false] [:locks? false] [:reflection? false] [:io? false]])

(defn- fused-mapping
  "The base -> fused control mapping a fused kernel applies (D50).

  A fused kernel is the one component that holds both interfaces: the base one,
  which the runtime config carries because a bundle crosses to a host that may not
  have the base artifact, and the fused one, which is the artifact's own. The
  conversion itself is portable (`fusion/runtime-mapping`), because the browser
  hosts build the same mapping from the bundle they were handed."
  [artifact fusion]
  (fusion/runtime-mapping (get-in artifact [:abi :controls]) fusion))

(defn host-kernel
  "The JVM-side `Kernel` for a compiled artifact's target.

  Only the JVM and the reference interpreter can host a pool here. A JavaScript
  or Wasm kernel is built by its own host — in the browser, over the artifact
  `synth-bundle` produces (`omkamra.dsp.hosts.js`) — and arrives with it.

  `:containment` selects the non-finite policy: `:reset` (default, per `PLAN.md`)
  silences and resets a voice whose block is non-finite and counts it;
  `:sanitize` contains each sample instead (non-finite -> 0, finite runaway
  clamped, no reset); `:off` skips containment for the plain, faster loop."
  ([artifact] (host-kernel artifact {}))
  ([artifact {:keys [containment fusion] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)]
     (case (:target artifact)
       :jvm (if fusion
              (jvm-host/fused-jvm-kernel artifact (fused-mapping artifact fusion)
                                         containment)
              (jvm-host/jvm-kernel artifact containment))
       :interpreter
       (interpreter-host/interpreter-kernel (:ir artifact) (:abi artifact)
                                            {:containment containment})
       (synth/fail (str "this DSP target has no JVM host kernel; a JavaScript"
                        " or Wasm kernel is built in the browser"
                        " (omkamra.dsp.hosts.js)")
                   {:target (:target artifact)
                    :supported [:jvm :interpreter]})))))

(defn synth-bundle
  "Compile a synth description into an artifact and its runtime configuration.

  Returns `{:description ..., :runtime ..., :artifact ..., :containment ...,
  :realtime ...}` — everything but the kernel. `:runtime` is pure data, so it can
  be sent to another host, and the artifact stays with the compiler. This is what
  a browser host needs: `compile-synth` adds a JVM-side `Kernel` to it, while
  `omkamra.dsp.hosts.js` builds one from the artifact on the other side.

  `compile-options` selects the target, precision, and `:containment` policy
  (`:reset` default, or `:sanitize`/`:off`); the entry is always `:process`,
  because a voice is a generator, not a scalar function."
  ([description] (synth-bundle description {}))
  ([description compile-options]
   (let [normalized (synth/normalize-description description)
         definition (get-in normalized [:voice :definition])
         resolved (requiring-resolve definition)
         _ (when-not resolved
             (synth/fail "a synth voice definition cannot be resolved"
                         {:definition definition}))
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
         guarded? (and fusion (:guard fusion)
                       (some #(= 'gate (:name %)) (:controls abi)))
         voice (if guarded?
                 (comb/guard (deref resolved))
                 (deref resolved))
         artifact (if fusion
                    (dsp/compile (dsp/link (comb/poly voice
                                                      {:count (:slots fusion)
                                                       :per-slot per-slot}))
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
                 fusion
                 (assoc :fusion {:slots (:slots fusion)
                                 :controls (mapv (comp keyword :name) (:controls abi))
                                 :per-slot per-slot}))
      :artifact artifact
      :containment containment
      :realtime (:realtime artifact)})))

(defn compile-synth
  "Compile a synth description into an artifact, a runtime configuration, and the
  JVM-side `Kernel` a pool over that artifact runs on.

  The JavaScript and Wasm targets have no kernel here: their host builds one in
  the browser (`omkamra.dsp.hosts.js`), and `synth-bundle` is what travels to it."
  ([description] (compile-synth description {}))
  ([description compile-options]
   (let [bundle (synth-bundle description compile-options)]
     (assoc bundle :kernel (host-kernel (:artifact bundle)
                                        {:containment (:containment bundle)
                                         :fusion (:fusion (:runtime bundle))})))))

(defn synth-pool
  "Create the live pool for a compiled synth."
  [bundle]
  (pool/voice-pool (:runtime bundle) (:kernel bundle)))
