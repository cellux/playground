(ns omkamra.dsp.harness.testbed
  "Development endpoints serving generated JavaScript, WAT, and browser adapters."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.bench :as bench]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.conformance :as conformance]
            [omkamra.dsp.harness.fixtures :as fixtures]
            [omkamra.dsp.examples.reference-synth :as reference-synth]
            [omkamra.dsp.hosts :as hosts]
            [omkamra.dsp.hosts.instrument :as instrument-host]
            [squint.compiler :as squint]))

(def one-pole (:definition (catalog/case-by-id :one-pole)))

(def bounded-loop-definition
  {:dsp/kind :function
   :name 'bounded-process-loop
   :params [{:name 'sample :type :float}
            {:name 'limit :type :int}]
   :process {:input :sample
             :controls [{:name :limit :type :int}]}
   :return-type :float
   :body ['(let [index 0
                 total sample]
             (do
               (while (< index limit)
                 (do
                   (set! index (+ index 1))
                   (set! total (+ total 1.0)))
                 {:max-iterations 2})
               total))]})

(defn- request-parameter
  [request parameter]
  (let [decode (fn [s] (java.net.URLDecoder/decode (or s "") "UTF-8"))]
    (some (fn [entry]
            (let [[key value] (str/split entry #"=" 2)]
              (when (= parameter (decode key)) (decode value))))
          (str/split (or (:query-string request) "") #"&"))))

(defn- requested-case
  [request]
  (catalog/case-by-id
   (keyword (or (request-parameter request "case") "one-pole"))))

(defn- requested-precision
  [request]
  (keyword (or (request-parameter request "precision") "f32")))

(defn- requested-wasm-math
  [request]
  (keyword (or (request-parameter request "wasm-math") "polynomial")))

(defn- javascript-response
  [body]
  {:status 200
   :headers {"content-type" "text/javascript; charset=utf-8"
             "cache-control" "no-store"}
   :body body})

(defn- js-entry
  [case]
  (if (catalog/function-case? case) :function :process))

(defn- js-source
  [case precision]
  (let [{:keys [source]} (dsp/compile (conformance/case-definition case)
                                      {:target :js
                                       :entry (js-entry case)
                                       :precision precision})]
    (javascript-response source)))

(defn source
  [request]
  (js-source (requested-case request) (requested-precision request)))

(defn source-f64
  [request]
  (js-source (requested-case request) :f64))

(defn plan
  "Serve the target-neutral conformance plan for a case and precision.

  The plan embeds the compiled portable IR; the browser runs the `.cljc`
  reference interpreter on it to compute expected outputs."
  [request]
  (let [case (requested-case request)
        precision (requested-precision request)
        body (json/generate-string
              (conformance/serialize-plan (conformance/plan case precision)))]
    {:status 200
     :headers {"content-type" "application/json; charset=utf-8"
               "cache-control" "no-store"}
     :body body}))

(defn equivalence
  "Serve the compiler equivalence payload (P13, D79).

  The catalog's definitions and the JVM's own normalization, linked unit and
  portable IR for each, so the page can compile the same definitions with the
  compiler compiled into it and compare. EDN rather than JSON because it is data
  to read back, boxes and all."
  [_request]
  (let [payload (conformance/equivalence-payload)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str payload)}))

(defn source-equivalence
  "Serve the source equivalence payload (P13d).

  DSP source strings and the JVM's own capture of each — descriptor, canonical
  IR and both emissions — so the page can compile the same text with SCI and
  compare. This is the check that pins the interpreted macro's capture to the
  real macro's."
  [_request]
  ;; The reference instrument's own source is *not* served here. Its literals
  ;; include an `:int` (`white-noise`'s seed), and a browser reader has one number
  ;; type, so it cannot tell that `12345` from a `2.0` — which is exactly the
  ;; ambiguity `#dsp/float` exists for (D78). The JVM's own reader keeps the type,
  ;; so the reference instrument's source is checked against SCI on the JVM
  ;; (`omkamra.dsp.tests.reference`), where both readers agree.
  (let [payload (conformance/source-equivalence-payload)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str payload)}))

(defn reference-instrument
  "Serve the reference instrument equivalence payload (P13d).

  The reference synth is the large, real case the catalog's small definitions
  cannot be: one `.cljc` file both worlds capture with their own macros, calling
  into the DSP library, using a `unison` combinator, process- and buffer-shaped,
  with a three-stage post-mix chain. This serves the JVM's own compile of its
  voice and its folded bus — the page compiles the voice from the capture already
  in its bundle and the bus from the definition here, and compares descriptor,
  canonical IR and both artifacts.

  The payload is assembled *here*, where the example is named; `conformance`
  builds it from the entries without knowing whose they are."
  [_request]
  (let [bus (instrument-host/lower-bus reference-synth/reference-instrument)
        payload (conformance/compiled-definition-payload
                 [{:id :voice
                   :definition reference-synth/reference-voice
                   :definition-id 'omkamra.dsp.examples.reference-synth/reference-voice
                   :entry :process
                   :channels 2}
                  {:id :bus
                   :definition bus
                   :entry :process
                   :channels 1
                   :carry-definition? true}])]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str payload)}))

(defn numbers
  "Serve the number-formatting payload (P13b).

  A sweep of floats and the JVM's spelling of each, for the page to spell for
  itself and compare. EDN, and each value travels as its bit pattern: two
  worlds can only name the same double by its bits (D78)."
  [_request]
  (let [payload (conformance/number-corpus-payload)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str payload)}))

(defn- wasm-artifact
  ([case precision] (wasm-artifact case precision :polynomial))
  ([case precision wasm-math]
   (dsp/compile (conformance/case-definition case)
                {:target :wasm
                 :entry :process
                 :precision precision
                 :wasm-math wasm-math})))

(defn wasm-source
  [request]
  (let [{:keys [wat]} (wasm-artifact (requested-case request)
                                     (requested-precision request)
                                     (requested-wasm-math request))]
    {:status 200
     :headers {"content-type" "application/wat; charset=utf-8"
               "cache-control" "no-store"}
     :body wat}))

(defn- wasm-metadata-response
  ([case precision] (wasm-metadata-response case precision :polynomial))
  ([case precision wasm-math]
   (let [{:keys [metadata]} (wasm-artifact case precision wasm-math)]
     {:status 200
      :headers {"content-type" "application/json; charset=utf-8"
                "cache-control" "no-store"}
      :body (json/generate-string metadata)})))

(defn wasm-metadata
  [request]
  (wasm-metadata-response (requested-case request)
                          (requested-precision request)
                          (requested-wasm-math request)))

(defn wasm-source-f64
  [request]
  (let [{:keys [wat]} (wasm-artifact (requested-case request) :f64)]
    {:status 200
     :headers {"content-type" "application/wat; charset=utf-8"
               "cache-control" "no-store"}
     :body wat}))

(defn wasm-metadata-f64
  [request]
  (wasm-metadata-response (requested-case request) :f64))

(defn- bounded-loop-artifact
  [_precision]
  (dsp/compile bounded-loop-definition
               {:target :wasm
                :entry :process
                :precision :f32}))

(defn wasm-bounded-loop-source
  [_request]
  {:status 200
   :headers {"content-type" "application/wat; charset=utf-8"
             "cache-control" "no-store"}
   :body (:wat (bounded-loop-artifact :f32))})

(defn wasm-bounded-loop-metadata
  "The bounded-loop fixture's own memory layout.

  The page drives this module directly, so it needs the region offsets — and taking
  them from here rather than from a constant is the point (P12): the offsets move
  with the declared frame capacity."
  [_request]
  {:status 200
   :headers {"content-type" "application/json; charset=utf-8"
             "cache-control" "no-store"}
   :body (json/generate-string (:metadata (bounded-loop-artifact :f32)))})

(defn squint-core
  [_request]
  (javascript-response (slurp (io/resource "squint/core.js"))))

(defn- worklet-metadata
  [precision]
  (let [artifact (dsp/compile one-pole {:target :js
                                        :entry :process
                                        :precision precision})
        physical-type (:physical-type artifact)
        abi (:abi artifact)]
    {:kernel-url (if (= :f64 precision)
                   "/omkamra/dsp/harness/testbed/source-f64"
                   "/omkamra/dsp/harness/testbed/source")
     ;; The kernel's own shape, from its ABI: the width it writes and whether it
     ;; takes an input at all.
     :channels (long (get-in abi [:channels :count] 1))
     :input-channels (count (:inputs abi))
     :frame-capacity 256
     :element-bytes (:element-bytes physical-type)
     :element-type (name (:element-type physical-type))
     :input-offset 0
     :output-offset 4096
     :channel-stride 4096
     :control-names ["coefficient"]
     :control-defaults [0.5]}))

(defn- control-descriptors
  "AudioWorklet `parameterDescriptors` for an artifact's controls, in ABI order.

  The adapter writes a descriptor's value into the kernel's control array at the
  descriptor's own index, so the order *is* the interface. A fused voice's
  descriptors are therefore its *fused* controls — `frequency_0`, `gate_0`,
  `frequency_1`, ... — which is what lets one node render every slot with
  parameters of its own (F5)."
  [abi]
  (mapv (fn [control]
          {"name" (name (:name control))
           "defaultValue" (double (or (:default control) 0.0))
           "automationRate" "k-rate"})
        (:controls abi)))

(def ^:private javascript-worklet-source
  "src/omkamra/dsp/worklet_adapters/js.cljs")

(def ^:private wasm-worklet-source
  "src/omkamra/dsp/worklet_adapters/wasm.cljs")

(defn- worklet-source
  "An adapter source, with the placeholders filled in.

  The placeholders are how one adapter serves several kernels: the page decides
  the kernel module's URL and the name the processor registers under, and each
  caller states the kernel's parameters here rather than the adapter
  hard-coding them."
  [path processor-name metadata descriptors]
  (cond-> (squint/compile-string (slurp (io/file path)) {:elide-imports true})
    (:kernel-url metadata)
    (str/replace "__OMKAMRA_DSP_KERNEL_URL__" (:kernel-url metadata))

    true
    (str/replace "__OMKAMRA_DSP_PROCESSOR_NAME__" processor-name)

    true
    (str/replace "__OMKAMRA_DSP_PARAMETER_DESCRIPTORS__"
                 (json/generate-string descriptors))))

(defn- squint-worklet-response
  [precision]
  (let [artifact (dsp/compile one-pole {:target :js
                                        :entry :process
                                        :precision precision})
        metadata (worklet-metadata precision)]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          "import { createKernel } from '" (:kernel-url metadata) "';\n"
          (worklet-source javascript-worklet-source "omkamra-dsp-kernel"
                          metadata (control-descriptors (:abi artifact)))))))

(defn squint-worklet
  [_request]
  (squint-worklet-response :f32))

(defn squint-worklet-f64
  [_request]
  (squint-worklet-response :f64))

(defn squint-wasm-worklet
  [_request]
  (let [artifact (dsp/compile one-pole {:target :wasm :entry :process :precision :f32})]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          (worklet-source wasm-worklet-source "omkamra-dsp-wasm-kernel" {}
                          (control-descriptors (:abi artifact)))))))

;; ---------------------------------------------------------------------------
;; The worker-to-worklet event transport (M1)
;; ---------------------------------------------------------------------------

(defn follow-amount-source
  "The `follow-amount` kernel as a standalone JavaScript module."
  [_request]
  (javascript-response
   (:source (dsp/compile fixtures/follow-amount
                         {:target :js :entry :process :precision :f32}))))

(defn- transport-metadata
  []
  (let [abi (:abi (dsp/compile fixtures/follow-amount
                               {:target :js :entry :process :precision :f32}))]
    {:kernel-url "/omkamra/dsp/harness/testbed/follow-amount-source"
     :channels (long (get-in abi [:channels :count] 1))
     :input-channels (count (:inputs abi))
     :control-names [(name (:name (first (:controls abi))))]
     :event-capacity 256}))

(defn transport-worklet
  "The JavaScript adapter serving the `follow-amount` kernel, with an event
  ingress queue, for the worker-to-worklet transport check."
  [_request]
  (let [artifact (dsp/compile fixtures/follow-amount
                              {:target :js :entry :process :precision :f32})
        metadata (transport-metadata)]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          "import { createKernel } from '" (:kernel-url metadata) "';\n"
          (worklet-source javascript-worklet-source "omkamra-dsp-transport"
                          metadata (control-descriptors (:abi artifact)))))))

;; ---------------------------------------------------------------------------
;; The browser pool host (P10d)
;; ---------------------------------------------------------------------------

(defn- requested-target
  [request]
  (keyword (or (request-parameter request "target") "js")))

(defn- browser-synth-bundle
  ([target precision] (browser-synth-bundle target precision fixtures/browser-stereo-description))
  ([target precision description]
   (hosts/synth-bundle description {:target target :precision precision})))

(defn- bundle-response
  "Serialize a synth bundle for the page, as EDN.

  `:runtime` is the sequencing half and is pure data; `:abi` says what the kernel
  looks like; `:ir` travels as EDN text so the page runs the same reference
  interpreter the JVM suite does. That is what makes the browser check a
  differential test rather than a comparison against shipped vectors. A `:wasm`
  bundle also carries the module's WAT and its linear-memory layout."
  [target {:keys [runtime artifact]}]
  {:status 200
   :headers {"content-type" "application/edn; charset=utf-8"
             "cache-control" "no-store"}
   :body (pr-str (cond-> {:runtime runtime
                          :abi (:abi artifact)
                          :ir (pr-str (:ir artifact))
                          :precision (:precision artifact)
                          :target target}
                   (= :wasm target)
                   (assoc :wat (:wat artifact) :memory (:memory artifact))))})

(defn synth-bundle
  "Serve the compiled voice's runtime configuration, ABI, and interpreter IR."
  [request]
  (let [target (requested-target request)]
    (bundle-response target (browser-synth-bundle target (requested-precision request)))))

(defn synth-source
  "Serve the compiled voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request))))))

;; A fused pool: `slots` handles behind one `(poly slots voice)` process (M6c).
;; The page renders this pool and the interpreter pool over the *base* voice side
;; by side -- the base artifact is the oracle, so the check is differential, and
;; the fused artifact is the kernel the browser hosts run.

(defn- requested-slots
  [request]
  (or (some-> (request-parameter request "slots") parse-long) 2))

(defn- browser-fused-pair
  "The shipping fused lowering (D52): the base bundle for the interpreter oracle,
  and a bundle compiled with `:fusion {:slots n}` for the kernel. Both describe
  the same voice, so the pool is base-shaped either way."
  ([target precision slots]
   (browser-fused-pair target precision slots fixtures/fused-browser-description))
  ([target precision slots description]
   (let [description (assoc description :capacity slots :max-block-frames 128)]
     {:base (hosts/synth-bundle description {:target target :precision precision})
      :fused (hosts/synth-bundle description {:target target :precision precision
                                              :fusion {:slots slots}})})))

(defn fused-pool-bundle
  "Serve what the browser fused pool host needs: the base artifact's ABI and IR
  (the interpreter oracle), the fused artifact's ABI, the `:fusion` mapping, and,
  for a Wasm target, the fused module's WAT and memory layout. The JavaScript
  kernel module is served as text by `fused-pool-source`."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        {:keys [base fused]} (browser-fused-pair target precision
                                                 (requested-slots request))
        base-artifact (:artifact base)
        fused-artifact (:artifact fused)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str (cond-> {:runtime (:runtime base)
                            :fusion (:fusion (:runtime fused))
                            :base-abi (:abi base-artifact)
                            :base-ir (pr-str (:ir base-artifact))
                            :abi (:abi fused-artifact)
                            :precision precision
                            :target target}
                     (= :wasm target)
                     (assoc :wat (:wat fused-artifact)
                            :memory (:memory fused-artifact))))}))

(defn- browser-unguarded-fused-pair
  "A fused pair for a voice that must fuse *without* a guard: `:guard false` means
  nothing supplies a per-call-site latch, so a voice with no state of its own has
  slots that are only identities (H4)."
  [target precision slots description]
  (let [description (assoc description :capacity slots :max-block-frames 16)]
    {:base (hosts/synth-bundle description {:target target :precision precision})
     :fused (hosts/synth-bundle description {:target target :precision precision
                                             :fusion {:slots slots :guard false}})}))

(def ^:private stateless-fused-slots 2)

(defn stateless-fused-bundle
  "Serve what the browser fused pool host needs for a *stateless* voice (H4): the
  base artifact's ABI and IR for the interpreter oracle, the fused artifact's ABI
  and `:fusion` mapping, and, for a Wasm target, the module's WAT and layout. Its
  slots own empty call partitions, which is what the Wasm host's range resolution
  has to accept rather than refuse."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        {:keys [base fused]} (browser-unguarded-fused-pair
                              target precision stateless-fused-slots
                              fixtures/stateless-voice-description)
        base-artifact (:artifact base)
        fused-artifact (:artifact fused)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str (cond-> {:runtime (:runtime base)
                            :fusion (:fusion (:runtime fused))
                            :base-abi (:abi base-artifact)
                            :base-ir (pr-str (:ir base-artifact))
                            :abi (:abi fused-artifact)
                            :precision precision
                            :target target}
                     (= :wasm target)
                     (assoc :wat (:wat fused-artifact)
                            :memory (:memory fused-artifact))))}))

(defn stateless-fused-source
  "Serve the stateless fused voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (:fused (browser-unguarded-fused-pair
                                :js (requested-precision request)
                                stateless-fused-slots
                                fixtures/stateless-voice-description))))))

(defn fused-reference-bundle
  "Serve what the browser fused pool host needs for the *reference instrument's
  voice*: stereo and process-shaped, so fusing it exercises a per-slot region, the
  frame, and the stereo sum end to end in the browser (F2c/F2d). The plain bundle
  is the interpreter oracle."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        {:keys [base fused]} (browser-fused-pair target precision
                                                 (requested-slots request)
                                                 reference-synth/description)
        base-artifact (:artifact base)
        fused-artifact (:artifact fused)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str (cond-> {:runtime (:runtime base)
                            :fusion (:fusion (:runtime fused))
                            :base-abi (:abi base-artifact)
                            :base-ir (pr-str (:ir base-artifact))
                            :abi (:abi fused-artifact)
                            ;; The *plain* lowering is the browser-side oracle: the
                            ;; fused kernel must render what the plain kernel
                            ;; renders, and comparing within one target makes the
                            ;; transcendentals cancel where the host interpreter's
                            ;; would only agree to a tolerance (D14).
                            :base-source (:source base-artifact)
                            :precision precision
                            :target target}
                     (= :wasm target)
                     (assoc :wat (:wat fused-artifact)
                            :memory (:memory fused-artifact)
                            :base-wat (:wat base-artifact)
                            :base-memory (:memory base-artifact))))}))

(defn fused-reference-source
  "Serve the fused reference voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (:fused (browser-fused-pair
                                :js (requested-precision request)
                                (requested-slots request)
                                reference-synth/description))))))

(defn fused-pool-source
  "Serve the shipping fused lowering as a JavaScript ES module: the kernel the
  page imports for the fused pool."
  [request]
  (javascript-response
   (:source (:artifact (:fused (browser-fused-pair
                                :js (requested-precision request)
                                (requested-slots request)))))))

(defn fused-lane-bundle
  "Serve the fused-lane pool: the same shape as `fused-pool-bundle`, over a voice
  whose automation exists only as a per-voice lane, so the browser kernels have to
  read each slot's own slice of the one lane region (F4)."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        {:keys [base fused]} (browser-fused-pair target precision
                                                 (requested-slots request)
                                                 fixtures/gated-lane-only-description)
        base-artifact (:artifact base)
        fused-artifact (:artifact fused)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str (cond-> {:runtime (:runtime base)
                            :fusion (:fusion (:runtime fused))
                            :base-abi (:abi base-artifact)
                            :base-ir (pr-str (:ir base-artifact))
                            :abi (:abi fused-artifact)
                            :precision precision
                            :target target}
                     (= :wasm target)
                     (assoc :wat (:wat fused-artifact)
                            :memory (:memory fused-artifact))))}))

(defn fused-lane-source
  "Serve the fused-lane voice as a JavaScript ES module, for the JS kernel."
  [request]
  (javascript-response
   (:source (:artifact (:fused (browser-fused-pair
                                :js (requested-precision request)
                                (requested-slots request)
                                fixtures/gated-lane-only-description))))))

;; ---------------------------------------------------------------------------
;; The post-mix chain's own regions (H2)
;; ---------------------------------------------------------------------------
;;
;; A post-mix stage may declare a lane or a shared control exactly as a voice's
;; may. The chain is one mono processor run once per output channel, so its
;; regions are instrument-level: one lane region and one shared region for the
;; whole instrument, handed to every channel's instance. The fixture voice renders
;; exactly 1.0 while it sounds, so the samples *are* the chain's gain — the page
;; writes the region and reads the curve back frame for frame.

(defn- browser-bus-instrument
  [instrument target precision]
  (instrument-host/compile-instrument instrument {:target target
                                                  :precision precision}))

(defn- bus-instrument-bundle
  [request instrument]
  (let [target (requested-target request)
        precision (requested-precision request)
        program (browser-bus-instrument instrument target precision)
        voice (:artifact (:bundle program))
        bus (:bus program)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str
            (cond-> {:runtime (get-in program [:bundle :runtime])
                     :voice-abi (:abi voice)
                     :voice-ir (pr-str (:ir voice))
                     :bus-abi (:abi bus)
                     :bus-ir (pr-str (:ir bus))
                     :precision precision
                     :target target}
              (= :wasm target)
              (assoc :voice-wat (:wat voice)
                     :voice-memory (:memory voice)
                     :bus-wat (:wat bus)
                     :bus-memory (:memory bus))))}))

(defn- bus-instrument-source
  [request instrument]
  (let [program (browser-bus-instrument instrument :js (requested-precision request))
        voice (:artifact (:bundle program))
        bus (:bus program)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str {:voice (:source voice) :bus (:source bus)})}))

(defn bus-lane-bundle
  "Serve the post-mix-lane fixture: a voice plus a chain stage whose gain is a
  per-frame lane (H2)."
  [request]
  (bus-instrument-bundle request fixtures/bus-lane-instrument))

(defn bus-lane-source
  "Serve the post-mix-lane fixture's JavaScript modules as EDN text: the voice's
  and the bus's."
  [request]
  (bus-instrument-source request fixtures/bus-lane-instrument))

(defn bus-shared-bundle
  "Serve the post-mix-shared fixture: the same chain, its stage reading the bus's
  shared region instead (H2)."
  [request]
  (bus-instrument-bundle request fixtures/bus-shared-instrument))

(defn bus-shared-source
  "Serve the post-mix-shared fixture's JavaScript modules as EDN text."
  [request]
  (bus-instrument-source request fixtures/bus-shared-instrument))

;; ---------------------------------------------------------------------------
;; The fused worklet: one node, `slots` voices (F5)
;; ---------------------------------------------------------------------------
;;
;; A fused voice is one process holding `slots` voices, so a fused worklet node
;; renders all of them in one call. Its interface is the *fused* one — a per-slot
;; control is expanded (`gate_0`, `gate_1`, ...) and the rest are hoisted — and the
;; node's parameters are that interface in ABI order, which is all the adapter
;; needs: it writes each parameter into the kernel's control array at the
;; parameter's own index, so no mapping is involved and the page drives the slots
;; apart directly.

(defn- fused-worklet-artifact
  [target precision slots]
  (:artifact (hosts/synth-bundle (assoc fixtures/fused-browser-description
                                        :capacity slots :max-block-frames 128)
                                 {:target target :precision precision
                                  :fusion {:slots slots}})))

(defn squint-fused-worklet
  "The JavaScript adapter over the *fused* kernel (F5)."
  [request]
  (let [slots (requested-slots request)
        kernel-url (str "/omkamra/dsp/harness/testbed/fused-pool-source"
                        "?slots=" slots "&precision=" (name (requested-precision request)))
        abi (:abi (fused-worklet-artifact :js (requested-precision request) slots))]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          "import { createKernel } from '" kernel-url "';\n"
          (worklet-source javascript-worklet-source "omkamra-dsp-fused-kernel"
                          {:kernel-url kernel-url} (control-descriptors abi))))))

(defn squint-wasm-fused-worklet
  "The Wasm adapter over the *fused* module (F5). The module bytes arrive as
  processor options, so this differs from the plain Wasm arm only in its
  parameters."
  [request]
  (let [slots (requested-slots request)
        abi (:abi (fused-worklet-artifact :wasm (requested-precision request) slots))]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          (worklet-source wasm-worklet-source "omkamra-dsp-wasm-fused-kernel" {}
                          (control-descriptors abi))))))

;; A pool of N independent kernel instances, against one `(poly N voice)`. The
;; same voice either way, so the page can time them against each other and D45
;; gets data instead of reasoning.

(defn- requested-voices
  [request]
  (or (some-> (request-parameter request "voices") parse-long) 8))
(defn- requested-arm
  "`?inline=1` asks for the lowering every target had before D47: every definition
  copied into its call sites. D45's pool-against-fused numbers were taken with it, so
  it is the other arm of a called-against-inlined comparison."
  [request]
  (if (= "1" (request-parameter request "inline")) :inlined :called))

(defn- bench-description
  "The benchmarked voice as a *pool* description of `?voices=` voices."
  [request]
  (assoc fixtures/sine-voice-description :capacity (requested-voices request)))

(defn- bench-fused-artifact
  "The same voice lowered as one `(poly n voice)` process, in the requested arm."
  [request target precision]
  (dsp/compile (bench/unit (requested-arm request)
                           (var-get #'fixtures/sine-voice)
                           (requested-voices request))
               {:target target :entry :process :precision precision}))

(defn bench-bundle
  "Serve the benchmark voice as a synth bundle: the *pool* side."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle target (requested-precision request)
                                           (bench-description request)))))

(defn bench-source
  "Serve the benchmark voice as a JavaScript ES module: the *pool* side."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request)
                                             (bench-description request))))))

(defn fused-bundle
  "Serve `(poly n voice)` as one artifact: the *fused* side."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     {:artifact (bench-fused-artifact request target
                                                      (requested-precision request))})))

(defn fused-source
  "Serve `(poly n voice)` as a JavaScript ES module: the *fused* side."
  [request]
  (javascript-response
   (:source (bench-fused-artifact request :js (requested-precision request)))))

(defn lane-bundle
  "Serve the automation-lane voice's runtime, ABI, interpreter IR, and Wasm
  layout, so the browser pool hosts can be checked on lane delivery (M4b)."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle target (requested-precision request)
                                           fixtures/lane-voice-description))))

(defn lane-source
  "Serve the automation-lane voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request)
                                             fixtures/lane-voice-description)))))

(defn bend-lane-bundle
  "Serve the voice whose lane sits beside a *per-channel* source (`:bend`), so the
  browser pool hosts can be checked on per-channel lane delivery (H3)."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle target (requested-precision request)
                                           fixtures/lane-bend-description))))

(defn bend-lane-source
  "Serve the per-channel-lane voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request)
                                             fixtures/lane-bend-description)))))

(defn voice-lane-bundle
  "Serve the automation-lane voice with a *per-voice* lane, so the browser pool
  hosts can be checked on per-voice delivery (M4c)."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle
                      target (requested-precision request)
                      (assoc-in fixtures/lane-voice-description
                                [:parameters :amount :scope] :per-voice)))))

(defn voice-lane-source
  "Serve the per-voice-lane voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle
                        :js (requested-precision request)
                        (assoc-in fixtures/lane-voice-description
                                  [:parameters :amount :scope] :per-voice))))))

(defn shared-voice-bundle
  "Serve a voice with a *shared* control, so the browser pool hosts can be checked
  on the pool-wide shared region (M5)."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle target (requested-precision request)
                                           fixtures/shared-voice-description))))

(defn shared-voice-source
  "Serve the shared-control voice as a JavaScript ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request)
                                             fixtures/shared-voice-description)))))

(defn- containment-description
  "Select one of the shared containment fixtures (P10c) by `?case=`."
  [request]
  (case (or (request-parameter request "case") "reset")
    "runaway" fixtures/containment-runaway-description
    "loud" fixtures/containment-loud-description
    fixtures/containment-reset-description))

(defn containment-bundle
  "Serve a containment fixture's runtime, ABI, interpreter IR, and Wasm layout.

  `?case=` selects the reset-and-recover voice (`reset`, the default), the
  random runaway (`runaway`), or the finite runaway (`loud`)."
  [request]
  (let [target (requested-target request)]
    (bundle-response target
                     (browser-synth-bundle target (requested-precision request)
                                           (containment-description request)))))

(defn containment-source
  "Serve a containment fixture's JavaScript module as an ES module."
  [request]
  (javascript-response
   (:source (:artifact (browser-synth-bundle :js (requested-precision request)
                                             (containment-description request))))))

;; ---------------------------------------------------------------------------
;; The instrument host (P11c)
;; ---------------------------------------------------------------------------

(defn- browser-instrument-program
  ([target precision] (browser-instrument-program target precision {}))
  ([target precision options]
   ;; The full reference synthesizer, so the browser check is an end-to-end check
   ;; of the whole system: the same voice, modulation matrix, and post-mix chain
   ;; the JVM suite renders.
   (instrument-host/compile-instrument reference-synth/reference-instrument
                                       (merge {:target target :precision precision}
                                              options))))

(def ^:private fused-instrument-slots 2)

(defn- browser-fused-instrument-pair
  "The reference instrument compiled twice at one target: the plain lowering, which
  the page hosts as the same-target oracle, and the fused one, which is the kernel
  under test. Both describe the same instrument, so the same phrase compares."
  [target precision]
  {:base (browser-instrument-program target precision)
   :fused (browser-instrument-program
           target precision
           {:fusion {:slots fused-instrument-slots}})})

(defn instrument-bundle
  "Serve the compiled reference instrument: the voice's runtime configuration,
  ABI, and interpreter IR, the modulation matrix, and the post-mix bus's ABI,
  IR, and control names. A `:wasm` bundle also carries both modules' WAT and
  memory layouts."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        program (browser-instrument-program target precision)
        voice (:artifact (:bundle program))
        bus (:bus program)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str
            (cond-> {:runtime (get-in program [:bundle :runtime])
                     ;; The modulation matrix travels as data; the page compiles
                     ;; it against the runtime plan with the shared
                     ;; `omkamra.dsp.reference` host.
                     :routes (:routes program)
                     :voice-abi (:abi voice)
                     :voice-ir (pr-str (:ir voice))
                     :bus-abi (when bus (:abi bus))
                     :bus-ir (when bus (pr-str (:ir bus)))
                     :bus-control-names (when bus
                                          (mapv :name (get-in bus [:abi :controls])))
                     :precision precision
                     :target target}
              (= :wasm target)
              (assoc :voice-wat (:wat voice)
                     :voice-memory (:memory voice)
                     :bus-wat (when bus (:wat bus))
                     :bus-memory (when bus (:memory bus)))))}))

(defn instrument-source
  "Serve the reference instrument's JavaScript modules as EDN text: the voice's
  and the bus's, so the page can import each."
  [request]
  (let [program (browser-instrument-program :js (requested-precision request))
        voice (:artifact (:bundle program))
        bus (:bus program)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str {:voice (:source voice) :bus (when bus (:source bus))})}))

(defn fused-instrument-bundle
  "Serve what the browser *fused* instrument host needs (F1).

  The fused voice's runtime, ABI, IR, and the `:fusion` mapping are the kernel the
  page threads; the plain lowering of the same voice is served alongside it, so
  the page has a same-target oracle and the transcendentals cancel (D14, as the
  fused reference voice check does). The modulation matrix and the post-mix bus
  travel once: the bus is not fused."
  [request]
  (let [target (requested-target request)
        precision (requested-precision request)
        {:keys [base fused]} (browser-fused-instrument-pair target precision)
        base-voice (:artifact (:bundle base))
        fused-voice (:artifact (:bundle fused))
        bus (:bus fused)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str
            (cond-> {:runtime (get-in fused [:bundle :runtime])
                     :base-runtime (get-in base [:bundle :runtime])
                     :fusion (get-in fused [:bundle :runtime :fusion])
                     ;; The modulation matrix travels as data, as in
                     ;; `instrument-bundle`.
                     :routes (:routes fused)
                     :voice-abi (:abi fused-voice)
                     :voice-ir (pr-str (:ir fused-voice))
                     :base-voice-abi (:abi base-voice)
                     :base-voice-ir (pr-str (:ir base-voice))
                     :base-source (:source base-voice)
                     :bus-abi (when bus (:abi bus))
                     :bus-ir (when bus (pr-str (:ir bus)))
                     :bus-control-names (when bus
                                          (mapv :name (get-in bus [:abi :controls])))
                     :precision precision
                     :target target}
              (= :wasm target)
              (assoc :voice-wat (:wat fused-voice)
                     :voice-memory (:memory fused-voice)
                     :base-voice-wat (:wat base-voice)
                     :base-voice-memory (:memory base-voice)
                     :bus-wat (when bus (:wat bus))
                     :bus-memory (when bus (:memory bus)))))}))

(defn fused-instrument-source
  "Serve the fused instrument's JavaScript modules as EDN text: the fused voice's
  and the bus's. The plain voice module is served in `fused-instrument-bundle`."
  [request]
  (let [program (browser-instrument-program
                 :js (requested-precision request)
                 {:fusion {:slots fused-instrument-slots}})
        voice (:artifact (:bundle program))
        bus (:bus program)]
    {:status 200
     :headers {"content-type" "application/edn; charset=utf-8"
               "cache-control" "no-store"}
     :body (pr-str {:voice (:source voice) :bus (when bus (:source bus))})}))
