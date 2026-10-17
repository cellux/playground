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
  ([artifact {:keys [containment] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)]
     (case (:target artifact)
       :jvm (jvm-host/jvm-kernel artifact containment)
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
         artifact (dsp/compile resolved
                               (assoc (merge {:target :jvm}
                                             (dissoc compile-options :containment))
                                      :entry :process))
         abi (:abi artifact)
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
      :runtime (assoc (synth/runtime-config normalized (:controls abi))
                      :channels abi-channels
                      :special-mask (long (get abi :special-mask 0))
                      :special-count (long (count (:special-slots abi))))
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
                                        {:containment (:containment bundle)})))))

(defn synth-pool
  "Create the live pool for a compiled synth."
  [bundle]
  (pool/voice-pool (:runtime bundle) (:kernel bundle)))
