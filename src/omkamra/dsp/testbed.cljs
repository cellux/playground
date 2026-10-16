(ns omkamra.dsp.testbed
  "Browser conformance testbed.

  Every shared case is run as a differential test against the portable
  reference interpreter, which is compiled into this bundle from
  `omkamra.dsp.interpreter` (`.cljc`).  The JVM serves a target-neutral plan
  (compiled IR, controls, and the invocation sequence); this page runs the
  interpreter on that IR to compute expected outputs, runs the generated
  JavaScript or Wasm kernel, and compares the two.  No expected vectors are
  shipped from the JVM."
  (:require [clojure.string :as str]
            [cljs.reader :as reader]
            [goog.object :as gobj]
            [omkamra.dsp.acceptance :as acceptance]
            [omkamra.dsp.interpreter :as interpreter]
            ["wabt" :default wabt]))

(defonce app-state (atom nil))
(defonce ^:private last-results (atom nil))

(def block-lengths acceptance/block-lengths)

;; ---------------------------------------------------------------------------
;; Small utilities
;; ---------------------------------------------------------------------------

(defn- response-text
  [url]
  (-> (js/fetch url) (.then (fn [response] (.text response)))))

(defn- fetch-json
  [url]
  (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.json response)
                 (js/Promise.reject
                  (js/Error. (str "endpoint returned " (.-status response)))))))
      (.then #(js->clj % :keywordize-keys true))))

(defn- real-array
  [precision values]
  (if (= :f64 precision)
    (js/Float64Array. (clj->js values))
    (js/Float32Array. (clj->js values))))

(defn- require!
  [condition message]
  (when-not condition
    (throw (js/Error. message))))

(defn- matches-value?
  [actual expected]
  (if (and (number? actual) (number? expected))
    (cond
      (and (js/Number.isNaN actual) (js/Number.isNaN expected)) true
      (or (js/Number.isNaN actual) (js/Number.isNaN expected)) false
      (and (zero? actual) (zero? expected))
      (= (js/Object.is actual -0.0) (js/Object.is expected -0.0))
      :else (= actual expected))
    (= actual expected)))

(defn- results-match?
  [actual expected]
  (and (= (count actual) (count expected))
       (every? true?
               (map (fn [a e]
                      (if (and (sequential? e) (sequential? a))
                        (results-match? a e)
                        (matches-value? a e)))
                    actual expected))))

(defn- show-number
  [x]
  (if (and (number? x) (zero? x))
    (if (js/Object.is x -0.0) "-0" "+0")
    (str x)))

(defn- describe-mismatch
  [actual expected]
  (or (first
       (for [i (range (min (count actual) (count expected)))
             :let [a (nth actual i) e (nth expected i)]
             :when (not (results-match? a e))
             j (range (min (count a) (count e)))
             :when (not (matches-value? (nth a j) (nth e j)))]
         (str "block " i " sample " j " actual " (show-number (nth a j))
              " expected " (show-number (nth e j)))))
      (str "block count actual " (count actual) " expected " (count expected))))

(defn- dynamic-import
  [url]
  (js* "import(~{})" url))

;; ---------------------------------------------------------------------------
;; Plan decoding and the browser-side interpreter oracle
;; ---------------------------------------------------------------------------

(defn- decode-number
  [value]
  (cond
    (= value "NaN") js/NaN
    (= value "Inf") js/Infinity
    (= value "-Inf") (- js/Infinity)
    :else (js/parseFloat value)))

(defn- decode-value
  [value]
  (if (string? value) (decode-number value) value))

(defn- decode-values
  [xs]
  (mapv decode-value xs))

(defn- process-case?
  [case]
  (not (acceptance/function-case? case)))

(defn- plan-ir
  [plan]
  (reader/read-string (:ir plan)))

(defn- interpreter-process-outputs
  [ir controls sample-rate invocations]
  (let [state (interpreter/create-state ir)
        precision (:precision ir)
        controls (decode-values controls)
        sample-rate (decode-value sample-rate)]
    (reduce (fn [outputs {:keys [op input]}]
              (case op
                "reset" (do (interpreter/reset-state! ir state) outputs)
                "process" (let [input (decode-values input)
                                frames (count input)
                                in (real-array precision input)
                                out (real-array precision (repeat frames 0.0))]
                            (interpreter/process-context
                             ir {:input in :output out :frames frames
                                 :sample-rate sample-rate
                                 :controls controls :state state})
                            (conj outputs (vec out)))))
            [] invocations)))

(defn- interpreter-function-values
  [ir vectors]
  (mapv (fn [args] (interpreter/invoke ir (decode-values args))) vectors))

;; ---------------------------------------------------------------------------
;; Kernel runners
;; ---------------------------------------------------------------------------

(defn- js-process-runner
  [module precision]
  (let [kernel ((gobj/get module "createKernel"))
        reset (.bind (gobj/get kernel "reset") kernel)
        process (gobj/get kernel "process")]
    {:reset reset
     :process (fn [input controls]
                (let [frames (count input)
                      in (real-array precision input)
                      out (real-array precision (repeat frames 0.0))]
                  (.apply process kernel
                          (to-array (concat [in out frames acceptance/sample-rate]
                                            controls)))
                  (vec out)))}))

(defn- replay-process
  [plan {:keys [reset process]}]
  (let [controls (decode-values (:controls plan))]
    (reset)
    (reduce (fn [outputs {:keys [op input]}]
              (case op
                "reset" (do (reset) outputs)
                "process" (conj outputs (process (decode-values input) controls))))
            [] (:invocations plan))))

;; ---------------------------------------------------------------------------
;; Case runners
;; ---------------------------------------------------------------------------

(defn- acceptance-query
  [c precision]
  (str "?case=" (name (:id c)) "&precision=" (name precision)))

(defn- run-process-case-on-js
  [c precision]
  (let [query (acceptance-query c precision)
        source-url (str "/omkamra/dsp/testbed/source" query)
        plan-url (str "/omkamra/dsp/testbed/plan" query)]
    (-> (js/Promise.all #js [(response-text source-url) (fetch-json plan-url)])
        (.then
         (fn [[source plan]]
           (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
                 url (js/URL.createObjectURL blob)]
             (-> (dynamic-import url)
                 (.then
                  (fn [module]
                    (let [expected (interpreter-process-outputs
                                    (plan-ir plan) (:controls plan)
                                    (:sample-rate plan) (:invocations plan))
                          actual (replay-process plan
                                                 (js-process-runner module precision))]
                      (require! (results-match? actual expected)
                                (str (:id c) " " (name precision)
                                     " JavaScript mismatch: "
                                     (describe-mismatch actual expected)))
                      {:target :js :passed? true})))
                 (.finally #(js/URL.revokeObjectURL url)))))))))

(declare compile-wasm)

(defn- run-process-case-on-wasm
  [c precision]
  (let [query (acceptance-query c precision)
        plan-url (str "/omkamra/dsp/testbed/plan" query)
        wat-url (str "/omkamra/dsp/testbed/wasm-source" query)]
    (-> (js/Promise.all #js [(fetch-json plan-url) (response-text wat-url)])
        (.then
         (fn [[plan wat]]
           (-> (compile-wasm wat)
               (.then
                (fn [bytes]
                  (-> (js/WebAssembly.instantiate bytes)
                      (.then
                       (fn [result]
                         (let [exports (.-exports (.-instance result))
                               memory (gobj/get exports "memory")
                               reset (gobj/get exports "reset")
                               process (gobj/get exports "process")
                               input-offset 0
                               output-offset 4096
                               expected (interpreter-process-outputs
                                         (plan-ir plan) (:controls plan)
                                         (:sample-rate plan) (:invocations plan))
                               controls (decode-values (:controls plan))
                               input-view (fn [frames]
                                            (if (= :f64 precision)
                                              (js/Float64Array. (.-buffer memory) input-offset (max 1 frames))
                                              (js/Float32Array. (.-buffer memory) input-offset (max 1 frames))))
                               output-view (fn [frames]
                                             (if (= :f64 precision)
                                               (js/Float64Array. (.-buffer memory) output-offset (max 1 frames))
                                               (js/Float32Array. (.-buffer memory) output-offset (max 1 frames))))
                               actual (do
                                        (.call reset nil)
                                        (reduce
                                         (fn [outputs {:keys [op input]}]
                                           (case op
                                             "reset" (do (.call reset nil) outputs)
                                             "process"
                                             (let [samples (decode-values input)
                                                   frames (count samples)]
                                               (.set (input-view frames) (real-array precision samples))
                                               (.apply process nil
                                                       (to-array (concat [input-offset output-offset frames
                                                                          acceptance/sample-rate]
                                                                         controls)))
                                               (conj outputs
                                                     (if (zero? frames)
                                                       []
                                                       (vec (output-view frames)))))))
                                         [] (:invocations plan)))]
                           (require! (results-match? actual expected)
                                     (str (:id c) " " (name precision)
                                          " Wasm mismatch: "
                                          (describe-mismatch actual expected)))
                           {:target :wasm :passed? true}))))))))))))

(defn- run-process-case!
  [c precision]
  (-> (js/Promise.all
       #js [(-> (run-process-case-on-js c precision)
                (.catch (fn [error]
                          {:target :js :passed? false
                           :detail (str (:case c) " | " (or (.-message error) (str error))
                                        " | " (or (.-stack error) ""))})))
            (-> (run-process-case-on-wasm c precision)
                (.catch (fn [error]
                          {:target :wasm :passed? false
                           :detail (str (:case c) " | " (or (.-message error) (str error))
                                        " | " (or (.-stack error) ""))})))])
      (.then (fn [[js wasm]]
               {:case (:id c)
                :precision precision
                :javascript (:passed? js)
                :wasm (:passed? wasm)
                :passed? (and (:passed? js) (:passed? wasm))
                :detail (or (:detail js) (:detail wasm))}))))

(defn- run-function-case!
  [c precision]
  (let [query (acceptance-query c precision)
        source-url (str "/omkamra/dsp/testbed/source" query)
        plan-url (str "/omkamra/dsp/testbed/plan" query)]
    (-> (js/Promise.all #js [(response-text source-url) (fetch-json plan-url)])
        (.then
         (fn [[source plan]]
           (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
                 url (js/URL.createObjectURL blob)]
             (-> (dynamic-import url)
                 (.then
                  (fn [module]
                    (let [invoke (gobj/get module "invoke")
                          ir (plan-ir plan)
                          expected (interpreter-function-values ir (:vectors plan))
                          actual (mapv (fn [args] (apply invoke (decode-values args)))
                                       (:vectors plan))]
                      (require! (results-match? actual expected)
                                (str (:id c) " " (name precision)
                                     " function mismatch"))
                      {:target :js :passed? true})))
                 (.finally #(js/URL.revokeObjectURL url)))))))))

;; ---------------------------------------------------------------------------
;; Matrices
;; ---------------------------------------------------------------------------

(defn- run-process-matrix-precision!
  [precision]
  (letfn [(run [remaining results]
            (if-let [c (first remaining)]
              (-> (run-process-case! c precision)
                  (.then #(run (next remaining) (conj results %))))
              (js/Promise.resolve results)))]
    (run (filter process-case? acceptance/cases) [])))

(defn- run-function-matrix-precision!
  [precision]
  (letfn [(run [remaining results]
            (if-let [c (first remaining)]
              (-> (run-function-case! c precision)
                  (.then (fn [result]
                           (run (next remaining)
                                (conj results (assoc result
                                                     :javascript
                                                     (not (false? (:passed? result)))))))))
              (js/Promise.resolve results)))]
    (run (filter #(and (acceptance/function-case? %)
                       (some #{precision} (:precisions %)))
                 acceptance/all-cases)
         [])))

(defn- matrix-result
  [label targets cases]
  {:name label
   :passed? (every? :passed? cases)
   :targets targets
   :detail (str "matched " (count cases) " cases"
                (when-let [failure (first (remove :passed? cases))]
                  (str "; first failure: " (:case failure)
                       " — " (:detail failure))))
   :cases (mapv (fn [entry]
                  (-> entry
                      (select-keys [:case :precision :javascript :wasm :passed?])
                      (update :case str)
                      (update :precision str)))
                cases)})

(defn- run-process-matrix!
  []
  (-> (run-process-matrix-precision! :f32)
      (.then (fn [f32]
               (-> (run-process-matrix-precision! :f64)
                   (.then (fn [f64]
                            [(matrix-result "shared process matrix f32" [:js :wasm] f32)
                             (matrix-result "shared process matrix f64" [:js :wasm] f64)])))))))

(defn- run-function-matrix!
  []
  (-> (run-function-matrix-precision! :f32)
      (.then (fn [f32]
               (-> (run-function-matrix-precision! :f64)
                   (.then (fn [f64]
                            [(matrix-result "shared function matrix f32" [:js] f32)
                             (matrix-result "shared function matrix f64" [:js] f64)])))))))

;; ---------------------------------------------------------------------------
;; DOM
;; ---------------------------------------------------------------------------

(defn- make-element
  [tag]
  (.createElement js/document tag))

(defn- append!
  [parent child]
  (.appendChild parent child)
  child)

(defn- text!
  [element value]
  (set! (.-textContent element) value)
  element)

(defn- style!
  [element property value]
  (gobj/set (.-style element) property value)
  element)

(defn- render-results!
  [summary results-list results]
  (reset! last-results results)
  (let [passed (count (filter :passed? results))
        total (count results)]
    (text! summary (str passed "/" total " checks passed"))
    (set! (.-textContent results-list) "")
    (doseq [{:keys [name passed? detail]} results]
      (let [item (make-element "li")]
        (text! item (str (if passed? "PASS" "FAIL") "  " name " — " detail))
        (style! item "color" (if passed? "#166534" "#b91c1c"))
        (append! results-list item)))))

;; ---------------------------------------------------------------------------
;; Worklet and Wasm trap checks
;; ---------------------------------------------------------------------------

(defn- compile-wasm
  [wat]
  (-> (wabt)
      (.then
       (fn [^js wabt-instance]
         (let [^js module (.parseWat wabt-instance "kernel.wat" wat)]
           (.validate module)
           (let [^js binary (.toBinary module #js {:write_debug_names true})
                 bytes (.-buffer binary)]
             (.destroy module)
             bytes))))))

(defn wat->bytes!
  "Compile WAT to a Wasm ArrayBuffer through the browser WABT runtime."
  [wat]
  (compile-wasm wat))

(defn- worklet-ready
  [node]
  (js/Promise.
   (fn [resolve reject]
     (let [timeout (js/setTimeout
                    #(reject (js/Error. "worklet kernel did not become ready"))
                    5000)]
       (set! (.-onmessage (.-port node))
             (fn [event]
               (let [message (.-data event)]
                 (cond
                   (= "ready" (.-type message))
                   (do (js/clearTimeout timeout) (resolve))
                   (= "error" (.-type message))
                   (do (js/clearTimeout timeout)
                       (reject (js/Error. (.-message message))))))))
       (.postMessage (.-port node) #js {:type "status"})))))

(defn- worklet-reset!
  [node]
  (js/Promise.
   (fn [resolve reject]
     (let [timeout (js/setTimeout
                    #(reject (js/Error. "worklet reset was not acknowledged"))
                    5000)]
       (set! (.-onmessage (.-port node))
             (fn [event]
               (let [message (.-data event)]
                 (when (= "reset" (.-type message))
                   (js/clearTimeout timeout)
                   (resolve)))))
       (.postMessage (.-port node) #js {:type "reset"})))))

(defn- audio-worklet-test
  ([] (audio-worklet-test :f32))
  ([precision]
   (let [context (js/OfflineAudioContext. 2 256 44100)
         f64? (= :f64 precision)
         worklet-url (if f64?
                       "/omkamra/dsp/testbed/squint-worklet-f64"
                       "/omkamra/dsp/testbed/squint-worklet")
         label (if f64?
                 "Squint AudioWorklet f64 adapter"
                 "Squint AudioWorklet adapter")]
     (-> (.addModule (.-audioWorklet context)
                     (str worklet-url "?v=" (js/Date.now)))
         (.then
          (fn [_]
            (let [node (js/AudioWorkletNode.
                        context "omkamra-dsp-kernel"
                        #js {:numberOfInputs 1
                             :numberOfOutputs 1
                             :channelCount 2
                             :channelCountMode "explicit"
                             :outputChannelCount #js [2]
                             :processorOptions #js {:kernelUrl (if f64?
                                                                 "/omkamra/dsp/testbed/source-f64"
                                                                 "/omkamra/dsp/testbed/source")
                                                    :channels 2
                                                    :frameCapacity 256
                                                    :elementBytes (if f64? 8 4)
                                                    :elementType (if f64? "f64" "f32")
                                                    :controlNames #js ["coefficient"]
                                                    :controlDefaults #js [0.5]}
                             :parameterData #js {:coefficient 0.5}})
                  oscillator (.createOscillator context)]
              (.connect node (.-destination context))
              (.connect oscillator node)
              (set! (.-value (.-frequency oscillator)) 440)
              (.start oscillator 0)
              (-> (worklet-ready node)
                  (.then (fn [_] (.startRendering context)))))))
         (.then
          (fn [buffer]
            (let [data (.getChannelData buffer 0)
                  peak (reduce max 0 (map #(js/Math.abs %) (array-seq data)))]
              (require! (> peak 0.0001) "AudioWorklet output was silent")
              (require! (= 2 (.-numberOfChannels buffer))
                        "JavaScript worklet did not preserve configured channels")
              {:name label
               :passed? true
               :detail (str "rendered 256 frames across "
                            (.-numberOfChannels buffer) " channels, peak " peak)})))))))

(defn- wasm-audio-worklet-test
  [bytes metadata]
  (let [context (js/OfflineAudioContext. 2 256 44100)
        memory (:memory metadata)
        physical-type (:physical-type metadata)
        precision (:precision physical-type)
        input-offset (:input-offset memory)
        output-offset (:output-offset memory)
        channels (get-in memory [:regions :output :channels])
        frame-capacity (:frame-capacity memory)
        element-bytes (or (:element-bytes physical-type) (:element-bytes memory))
        element-type (or (:element-type physical-type) (:element-type memory))
        element-type (if (keyword? element-type) (name element-type) element-type)
        channel-stride (:channel-stride memory)
        label (if (= "f64" precision)
                "Wasm AudioWorklet f64 adapter"
                "Wasm AudioWorklet adapter")]
    (-> (.addModule (.-audioWorklet context)
                    (str "/omkamra/dsp/testbed/squint-wasm-worklet?v="
                         (js/Date.now)))
        (.then
         (fn [_]
           (let [node (js/AudioWorkletNode.
                       context "omkamra-dsp-wasm-kernel"
                       #js {:numberOfInputs 1
                            :numberOfOutputs 1
                            :channelCount 2
                            :channelCountMode "explicit"
                            :outputChannelCount #js [2]
                            :processorOptions #js {:wasmBytes bytes
                                                   :inputOffset input-offset
                                                   :outputOffset output-offset
                                                   :channels channels
                                                   :frameCapacity frame-capacity
                                                   :elementBytes element-bytes
                                                   :elementType element-type
                                                   :channelStride channel-stride
                                                   :controlNames #js ["coefficient"]
                                                   :controlDefaults #js [0.5]}
                            :parameterData #js {:coefficient 0.5}})
                 oscillator (.createOscillator context)]
             (.connect node (.-destination context))
             (.connect oscillator node)
             (set! (.-value (.-frequency oscillator)) 440)
             (.start oscillator 0)
             (-> (worklet-ready node)
                 (.then (fn [_] (worklet-reset! node)))
                 (.then (fn [_] (.startRendering context)))))))
        (.then
         (fn [buffer]
           (let [data (.getChannelData buffer 0)
                 peak (reduce max 0 (map #(js/Math.abs %) (array-seq data)))]
             (require! (> peak 0.0001) "Wasm AudioWorklet output was silent")
             (require! (= 2 (.-numberOfChannels buffer))
                       "Wasm worklet did not preserve configured channels")
             {:name label
              :passed? true
              :detail (str "rendered 256 frames across "
                           (.-numberOfChannels buffer) " channels, peak " peak)}))))))

(defn- bounded-loop-wasm-test
  []
  (-> (response-text "/omkamra/dsp/testbed/wasm-bounded-loop-source")
      (.then compile-wasm)
      (.then
       (fn [bytes]
         (-> (js/WebAssembly.instantiate bytes)
             (.then
              (fn [result]
                (let [exports (.-exports (.-instance result))
                      memory (gobj/get exports "memory")
                      init (gobj/get exports "init")
                      reset (gobj/get exports "reset")
                      process (gobj/get exports "process")
                      input (js/Float32Array. (.-buffer memory) 0 1)
                      output (js/Float32Array. (.-buffer memory) 4096 1)]
                  (.call init nil)
                  (.set input #js [1.0])
                  (.call process nil 0 4096 1 44100.0 1)
                  (require! (matches-value? (aget output 0) 2.0)
                            (str "expected bounded-loop output 2, got " (aget output 0)))
                  (.call reset nil)
                  (let [trapped? (try
                                   (.call process nil 0 4096 1 44100.0 3)
                                   false
                                   (catch :default _ true))]
                    (require! trapped? "bounded Wasm loop did not trap on exhaustion")
                    {:name "WAT bounded-loop module"
                     :passed? true
                     :detail "compiled WAT; completed within bound and trapped on exhaustion"})))))))
      (.catch
       (fn [error]
         {:name "WAT bounded-loop module"
          :passed? false
          :detail (or (.-message error) (str error))}))))

;; ---------------------------------------------------------------------------
;; Suite
;; ---------------------------------------------------------------------------

(defn- run-promise-sequence
  [summary results-list previous-results steps]
  (reduce (fn [promise step]
            (.then promise
                   (fn [results]
                     (-> (step)
                         (.then (fn [new-results]
                                  (let [results (into results new-results)]
                                    (render-results! summary results-list results)
                                    results)))))))
          (js/Promise.resolve previous-results)
          steps))

(defn- wasm-worklet-step
  [results]
  (-> (js/fetch "/omkamra/dsp/testbed/wasm-metadata")
      (.then (fn [response] (.json response)))
      (.then (fn [metadata]
               (-> (js/fetch "/omkamra/dsp/testbed/wasm-source")
                   (.then (fn [response] (.text response)))
                   (.then (fn [wat]
                            (-> (compile-wasm wat)
                                (.then (fn [bytes]
                                         (wasm-audio-worklet-test
                                          bytes
                                          (js->clj metadata :keywordize-keys true))))))))))
      (.then (fn [worklet] (conj results worklet)))))

(defn- run-suite!
  [summary results-list]
  (render-results! summary results-list [])
  (-> (js/Promise.resolve [])
      (.then (fn [_] (run-process-matrix!)))
      (.then (fn [process-results]
               (-> (run-function-matrix!)
                   (.then (fn [function-results]
                            (into process-results function-results))))))
      (.then (fn [matrix-results]
               (render-results! summary results-list matrix-results)
               matrix-results))
      (.then (fn [matrix-results]
               (run-promise-sequence
                summary results-list matrix-results
                [(fn [] (-> (bounded-loop-wasm-test) (.then (fn [r] [r]))))
                 (fn [] (-> (audio-worklet-test :f32)
                            (.then (fn [a]
                                     (-> (audio-worklet-test :f64)
                                         (.then (fn [b] [a b])))))))])))
      (.then #(wasm-worklet-step %))
      (.then (fn [results]
               (render-results! summary results-list results)
               results))
      (.catch (fn [error]
                (let [failure {:name "browser suite"
                               :passed? false
                               :detail (or (.-message error) (str error))}
                      results (conj (or @last-results []) failure)]
                  (render-results! summary results-list results)
                  results)))))

(defn run-conformance!
  "Run the complete browser conformance suite and resolve a structured result.

  Safe to call from `cljs-eval` or from a Playwright driver such as
  `omkamra.dsp.browser`.  The result is `{:passed? :passed :total :results}`;
  each entry carries `:name`, `:passed?`, `:detail`, and, for the shared
  matrices, `:targets` and a per-case `:cases` breakdown."
  []
  (reset! last-results nil)
  (let [summary (make-element "strong")
        results-list (make-element "ul")]
    (-> (run-suite! summary results-list)
        (.then
         (fn [results]
           (clj->js
            {:passed? (every? :passed? results)
             :passed (count (filter :passed? results))
             :total (count results)
             :results (mapv #(clj->js (select-keys
                                       % [:name :passed? :detail :targets :cases]))
                            results)}))))))

(defn run-function-case-matrix!
  []
  (-> (run-function-matrix!)
      (.then (fn [results]
               (clj->js {:passed? (every? :passed? results)
                         :results (mapv clj->js results)})))))

(defn init
  []
  (set! (.-innerHTML (.-body js/document)) "")
  (style! (.-body js/document) "fontFamily" "system-ui, sans-serif")
  (style! (.-body js/document) "lineHeight" "1.5")
  (style! (.-body js/document) "margin" "0")
  (let [main (make-element "main")
        heading (text! (make-element "h1") "omkamra.dsp browser testbed")
        intro (text! (make-element "p")
                     "Differential browser testbed: every shared case is compared against the portable reference interpreter compiled into this page, on JavaScript and Wasm.")
        button (text! (make-element "button") "Run checks")
        summary (make-element "strong")
        results-list (make-element "ul")]
    (style! button "padding" "8px 12px")
    (style! button "cursor" "pointer")
    (style! summary "display" "block")
    (style! summary "marginTop" "16px")
    (style! results-list "paddingLeft" "0")
    (style! results-list "listStyle" "none")
    (append! main heading)
    (append! main intro)
    (append! main button)
    (append! main summary)
    (append! main results-list)
    (append! (.-body js/document) main)
    (.addEventListener button "click" #(run-suite! summary results-list))
    (set! (.-omkamraDspTestbed js/window)
          #js {:runConformance run-conformance!
               :runFunctionCaseMatrix run-function-case-matrix!
               :caseIds (clj->js (mapv :id acceptance/all-cases))})
    (reset! app-state {:summary summary :results-list results-list})
    (when-not (str/includes? (or (.-search (.-location js/window)) "") "autorun=0")
      (run-suite! summary results-list))))
