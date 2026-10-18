(ns omkamra.dsp.harness.testbed
  "Development endpoints serving generated JavaScript, WAT, and browser adapters."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [omkamra.dsp :as dsp]
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

(defn wasm-bounded-loop-source
  [_request]
  (let [{:keys [wat]} (dsp/compile bounded-loop-definition
                                   {:target :wasm
                                    :entry :process
                                    :precision :f32})]
    {:status 200
     :headers {"content-type" "application/wat; charset=utf-8"
               "cache-control" "no-store"}
     :body wat}))

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

(def parameter-descriptors
  [{"name" "coefficient"
    "defaultValue" 0.5
    "minValue" -10
    "maxValue" 10
    "automationRate" "k-rate"}])

(def ^:private javascript-worklet-source
  "src/omkamra/dsp/worklet_adapters/js.cljs")

(def ^:private wasm-worklet-source
  "src/omkamra/dsp/worklet_adapters/wasm.cljs")

(defn- worklet-source
  "An adapter source, with the placeholders filled in.

  The placeholders are how one adapter serves several kernels: the page decides
  the kernel module's URL and the name the processor registers under, and states
  them here rather than the adapter hard-coding them."
  [path processor-name metadata]
  (cond-> (squint/compile-string (slurp (io/file path)) {:elide-imports true})
    (:kernel-url metadata)
    (str/replace "__OMKAMRA_DSP_KERNEL_URL__" (:kernel-url metadata))

    true
    (str/replace "__OMKAMRA_DSP_PROCESSOR_NAME__" processor-name)

    true
    (str/replace "__OMKAMRA_DSP_PARAMETER_DESCRIPTORS__"
                 (json/generate-string parameter-descriptors))))

(defn- squint-worklet-response
  [precision]
  (let [metadata (worklet-metadata precision)]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
          "import { createKernel } from '" (:kernel-url metadata) "';\n"
          (worklet-source javascript-worklet-source "omkamra-dsp-kernel"
                          metadata)))))

(defn squint-worklet
  [_request]
  (squint-worklet-response :f32))

(defn squint-worklet-f64
  [_request]
  (squint-worklet-response :f64))

(defn squint-wasm-worklet
  [_request]
  (javascript-response
   (str "import * as squint_core from '/omkamra/dsp/harness/testbed/squint-core';\n"
        (worklet-source wasm-worklet-source "omkamra-dsp-wasm-kernel" {}))))

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
  [target precision]
  ;; The full reference synthesizer, so the browser check is an end-to-end check
  ;; of the whole system: the same voice, modulation matrix, and post-mix chain
  ;; the JVM suite renders.
  (instrument-host/compile-instrument reference-synth/reference-instrument
                                      {:target target :precision precision}))

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
