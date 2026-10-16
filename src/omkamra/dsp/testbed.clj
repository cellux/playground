(ns omkamra.dsp.testbed
  "Development endpoints serving generated JavaScript, WAT, and browser adapters."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.acceptance :as acceptance]
            [omkamra.dsp.conformance :as conformance]
            [squint.compiler :as squint]))

(def one-pole (:definition (acceptance/case-by-id :one-pole)))

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
  (some (fn [entry]
          (let [[key value] (str/split entry #"=" 2)]
            (when (= parameter key) value)))
        (str/split (or (:query-string request) "") #"&")))

(defn- requested-case
  [request]
  (acceptance/case-by-id
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
  (if (acceptance/function-case? case) :function :process))

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
        physical-type (:physical-type artifact)]
    {:processor-name "omkamra-dsp-kernel"
     :kernel-url (if (= :f64 precision)
                   "/omkamra/dsp/testbed/source-f64"
                   "/omkamra/dsp/testbed/source")
     :channels 2
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

(defn- worklet-source
  [path metadata]
  (let [processor-name (if (str/ends-with? path "wasm_worklet_adapter.cljs")
                         "omkamra-dsp-wasm-kernel"
                         (:processor-name metadata))]
    (-> (squint/compile-string
         (slurp (io/file path))
         {:elide-imports true})
        (str/replace "__OMKAMRA_DSP_KERNEL_URL__"
                     (:kernel-url metadata))
        (str/replace "__OMKAMRA_DSP_PROCESSOR_NAME__"
                     processor-name)
        (str/replace "__OMKAMRA_DSP_PARAMETER_DESCRIPTORS__"
                     (json/generate-string parameter-descriptors)))))

(defn- squint-worklet-response
  [precision]
  (let [metadata (worklet-metadata precision)]
    (javascript-response
     (str "import * as squint_core from '/omkamra/dsp/testbed/squint-core';\n"
          "import { createKernel } from '" (:kernel-url metadata) "';\n"
          (worklet-source "src/omkamra/dsp/worklet_adapter.cljs" metadata)))))

(defn squint-worklet
  [_request]
  (squint-worklet-response :f32))

(defn squint-worklet-f64
  [_request]
  (squint-worklet-response :f64))

(defn squint-wasm-worklet
  [_request]
  (javascript-response
   (str "import * as squint_core from '/omkamra/dsp/testbed/squint-core';\n"
        (worklet-source "src/omkamra/dsp/wasm_worklet_adapter.cljs"
                        (worklet-metadata :f32)))))
