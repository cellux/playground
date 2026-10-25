(ns omkamra.dsp.hosts.jvm
  "Assemble a synth for the JVM world: the `Kernel` a pool over an artifact runs
  on (P10).

  The kernels this world can host live in `omkamra.dsp.kernels.jvm` and
  `omkamra.dsp.kernels.interpreter`, and the plumbing they share — the containment
  policy and the planar blocks a voice is mixed through — in
  `omkamra.dsp.kernels.mixing`. The JavaScript and Wasm kernels belong to the
  browser world, which builds them from the same bundle.

  The bundle is not built here: `omkamra.dsp.synth.bundle` compiles a description
  to an artifact and its runtime configuration, `.cljc`, so either world can do
  it. What this namespace adds is the world's own kernel — `compile-synth` puts
  the two together, and `synth-pool` makes the live pool."
  (:require [omkamra.dsp.fusion :as fusion]
            [omkamra.dsp.kernels.interpreter :as interpreter-kernels]
            [omkamra.dsp.kernels.jvm :as jvm-kernels]
            [omkamra.dsp.kernels.mixing :as mixing]
            [omkamra.dsp.synth :as synth]
            [omkamra.dsp.synth.bundle :as bundle]
            [omkamra.dsp.synth.pool :as pool]))

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
  `synth-bundle` produces (`omkamra.dsp.kernels.js`) — and arrives with it.

  `:containment` selects the non-finite policy: `:reset` (default, per `PLAN.md`)
  silences and resets a voice whose block is non-finite and counts it;
  `:sanitize` contains each sample instead (non-finite -> 0, finite runaway
  clamped, no reset); `:off` skips containment for the plain, faster loop."
  ([artifact] (host-kernel artifact {}))
  ([artifact {:keys [containment fusion] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)]
     (case (:target artifact)
       :jvm (if fusion
              (jvm-kernels/fused-jvm-kernel artifact (fused-mapping artifact fusion)
                                            containment)
              (jvm-kernels/jvm-kernel artifact containment))
       :interpreter
       (interpreter-kernels/interpreter-kernel (:ir artifact) (:abi artifact)
                                               {:containment containment
                                                :mapping (when fusion
                                                           (fused-mapping artifact fusion))})
       (synth/fail (str "this DSP target has no JVM host kernel; a JavaScript"
                        " or Wasm kernel is built in the browser"
                        " (omkamra.dsp.kernels.js)")
                   {:target (:target artifact)
                    :supported [:jvm :interpreter]})))))

(defn bus-kernel
  "The JVM-side bus kernel for a compiled bus artifact's target.

  The post-mix chain is one mono processor compiled like a voice, so the bus
  kernel is chosen by the same target axis `host-kernel` uses. A JavaScript or
  Wasm bus kernel is built by its own host in the browser, from the bus
  artifact's ABI.

  Returns the `{:create-bus, :run-bus!, :reset-bus!, :write-lane!,
  :write-shared!}` map `omkamra.dsp.instrument.program` drives."
  [artifact]
  (case (:target artifact)
    :jvm (jvm-kernels/jvm-bus-kernel artifact)
    :interpreter (interpreter-kernels/interpreter-bus-kernel
                  (:ir artifact) (:abi artifact)
                  (get-in artifact [:physical-type :precision]))
    (synth/fail (str "this DSP target has no JVM-side bus kernel; a JavaScript"
                     " or Wasm bus kernel is built in the browser"
                     " (omkamra.dsp.kernels.js)")
                {:target (:target artifact)
                 :supported [:jvm :interpreter]})))

(defn compile-synth
  "Compile a synth description into an artifact, a runtime configuration, and the
  JVM-side `Kernel` a pool over that artifact runs on.

  The JavaScript and Wasm targets have no kernel here: their host builds one in
  the browser, and `omkamra.dsp.synth.bundle/synth-bundle` is what travels to it."
  ([description] (compile-synth description {}))
  ([description compile-options]
   (let [compiled (bundle/synth-bundle description compile-options)]
     (assoc compiled
            :kernel (host-kernel (:artifact compiled)
                                 {:containment (:containment compiled)
                                  :fusion (:fusion (:runtime compiled))})))))

(defn synth-pool
  "Create the live pool for a compiled synth."
  [bundle]
  (pool/voice-pool (:runtime bundle) (:kernel bundle)))
