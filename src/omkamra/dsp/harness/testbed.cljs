(ns omkamra.dsp.harness.testbed
  "Browser conformance testbed.

  Every shared case is run as a differential test against the portable
  reference interpreter, which is compiled into this bundle from
  `omkamra.dsp.targets.interpreter` (`.cljc`).  The JVM serves a target-neutral plan
  (compiled IR, controls, and the invocation sequence); this page runs the
  interpreter on that IR to compute expected outputs, runs the generated
  JavaScript or Wasm kernel, and compares the two.  No expected vectors are
  shipped from the JVM."
  (:require [clojure.string :as str]
            [cljs.reader :as reader]
            [goog.object :as gobj]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.event :as event]
            [omkamra.dsp.hosts.interpreter :as interpreter-host]
            [omkamra.dsp.hosts.js :as js-host]
            [omkamra.dsp.hosts.wasm :as wasm-host]
            [omkamra.dsp.targets.interpreter :as interpreter]
            [omkamra.dsp.reference :as reference]
            [omkamra.dsp.schedule :as schedule]
            [omkamra.dsp.synth.pool :as pool]
            ["wabt" :default wabt]))

(defonce app-state (atom nil))
(defonce ^:private last-results (atom nil))

;; Bound around a run to request a non-default Wasm math strategy.
(def ^:dynamic *wasm-math* nil)

(def block-lengths catalog/block-lengths)

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

(defn- within-tolerance?
  [precision actual expected]
  (and (number? actual)
       (number? expected)
       (js/Number.isFinite actual)
       (js/Number.isFinite expected)
       (let [relative (if (= :f64 precision) 1.0e-9 1.0e-6)
             absolute (if (= :f64 precision) 1.0e-12 1.0e-7)]
         (<= (js/Math.abs (- actual expected))
             (+ absolute (* relative (js/Math.abs expected)))))))

(defn- scalar-match?
  "Compare one target value against the interpreter's expected value.

  Exact by default; `:wasm` additionally allows the documented tolerance
  because its transcendental emulations cannot reproduce the host `Math`
  bit-for-bit (DECISIONS.md D14)."
  [target precision actual expected]
  (if (= :wasm target)
    (or (matches-value? actual expected)
        (within-tolerance? precision actual expected))
    (matches-value? actual expected)))

(defn- results-match?
  [target precision actual expected]
  (and (= (count actual) (count expected))
       (every? true?
               (map (fn [a e]
                      (if (and (sequential? e) (sequential? a))
                        (results-match? target precision a e)
                        (scalar-match? target precision a e)))
                    actual expected))))

(defn- show-number
  [x]
  (if (and (number? x) (zero? x))
    (if (js/Object.is x -0.0) "-0" "+0")
    (str x)))

(defn- describe-mismatch
  [target precision actual expected]
  (or (first
       (for [i (range (min (count actual) (count expected)))
             :let [a (nth actual i) e (nth expected i)]
             :when (not (results-match? target precision a e))
             j (range (min (count a) (count e)))
             :when (not (scalar-match? target precision (nth a j) (nth e j)))]
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
  (not (catalog/function-case? case)))

(defn- plan-ir
  [plan]
  (reader/read-string (:ir plan)))

(defn- lane-values-by-name
  "The lane values one invocation supplies, decoded and keyed by lane name.

  The plan travels as JSON, so a name arrives as a string where the source wrote
  a symbol or a keyword; accept any spelling rather than silently reading zeros."
  [precision lanes]
  (when (seq lanes)
    (into {}
          (map (fn [[name values]] [name (real-array precision (decode-values values))]))
          lanes)))

(defn- lane-value-for
  [values name]
  (or (get values (keyword name)) (get values (symbol name)) (get values name)))

(defn- lane-region
  "Build the flat lane region a JavaScript kernel takes.

  Each lane occupies one `:lane-stride`-frame slice at the offset the ABI gives
  it, so the region is the whole of it — the same region a real-time host keeps
  and refills instead of reallocating per block."
  [plan precision lanes]
  (when (seq (:lane-layout plan))
    (let [stride (long (:lane-stride plan))
          layout (:lane-layout plan)
          region (real-array precision (repeat (* stride (count layout)) 0.0))
          values (lane-values-by-name precision lanes)]
      (doseq [{:keys [name offset]} layout
              :let [supplied (lane-value-for values name)]
              :when supplied]
        ;; A lane's values are a JS typed array, which `count` and `nth` do not
        ;; implement; read its length and elements directly.
        (dotimes [i (min (.-length supplied) stride)]
          (aset region (+ (long offset) i) (aget supplied i))))
      region)))

(defn- interpreter-process-outputs
  "Run the portable IR in the browser, which is the oracle every target is
  compared against. It takes the plan because a lane has to be flattened into a
  region the same way the generated-target drivers do it."
  [plan]
  (let [ir (plan-ir plan)
        precision (:precision ir)
        state (interpreter/create-state ir)
        controls (decode-values (:controls plan))
        sample-rate (decode-value (:sample-rate plan))]
    (reduce (fn [outputs {:keys [op input frames lanes]}]
              (case op
                "reset" (do (interpreter/reset-state! ir state) outputs)
                "process" (let [samples (when input (decode-values input))
                                region (lane-region plan precision lanes)
                                out (real-array precision (repeat frames 0.0))]
                            (interpreter/process-context
                             ir (cond-> {:output out :frames frames
                                         :sample-rate sample-rate
                                         :controls controls :state state}
                                  samples (assoc :input (real-array precision samples))
                                  region (assoc :lanes region)))
                            (conj outputs (vec out)))))
            [] (:invocations plan))))

(defn- interpreter-function-values
  [ir vectors]
  (mapv (fn [args] (interpreter/invoke ir (decode-values args))) vectors))

;; ---------------------------------------------------------------------------
;; Kernel runners
;; ---------------------------------------------------------------------------

(defn- js-process-runner
  [module precision plan]
  (let [kernel ((gobj/get module "createKernel"))
        reset (.bind (gobj/get kernel "reset") kernel)
        process (gobj/get kernel "process")]
    {:reset reset
     ;; `samples` is nil for a generator (D23), whose kernel `process` takes no
     ;; input argument; the frame count always comes from the plan.
     :process (fn [samples frames controls lanes]
                (let [out (real-array precision (repeat frames 0.0))
                      ;; Any number of lanes is one flat region, and the kernel
                      ;; takes it as one final argument.
                      region (lane-region plan precision lanes)]
                  (.apply process kernel
                          (to-array (concat (when samples
                                              [(real-array precision samples)])
                                            [out frames catalog/sample-rate]
                                            controls
                                            (when region [region]))))
                  (vec out)))}))

(defn- replay-process
  [plan {:keys [reset process]}]
  (let [controls (decode-values (:controls plan))]
    (reset)
    (reduce (fn [outputs {:keys [op input frames lanes]}]
              (case op
                "reset" (do (reset) outputs)
                "process" (conj outputs (process (when input (decode-values input))
                                                 frames controls lanes))))
            [] (:invocations plan))))

;; ---------------------------------------------------------------------------
;; Case runners
;; ---------------------------------------------------------------------------

(defn- acceptance-query
  [c precision]
  (str "?case=" (name (:id c)) "&precision=" (name precision)
       (when *wasm-math* (str "&wasm-math=" (name *wasm-math*)))))

(defn- run-process-case-on-js
  [c precision]
  (let [query (acceptance-query c precision)
        source-url (str "/omkamra/dsp/harness/testbed/source" query)
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)]
    (-> (js/Promise.all #js [(response-text source-url) (fetch-json plan-url)])
        (.then
         (fn [[source plan]]
           (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
                 url (js/URL.createObjectURL blob)]
             (-> (dynamic-import url)
                 (.then
                  (fn [module]
                    (let [expected (interpreter-process-outputs plan)
                          actual (replay-process plan
                                                 (js-process-runner module precision plan))]
                      (require! (results-match? :js precision actual expected)
                                (str (:id c) " " (name precision)
                                     " JavaScript mismatch: "
                                     (describe-mismatch :js precision actual expected)))
                      {:target :js :passed? true})))
                 (.finally #(js/URL.revokeObjectURL url)))))))))

(declare compile-wasm)

(defn- run-process-case-on-wasm
  [c precision]
  (let [query (acceptance-query c precision)
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)
        wat-url (str "/omkamra/dsp/harness/testbed/wasm-source" query)
        ;; The linear-memory layout is the module's, so read it from the module
        ;; rather than repeating its offsets here.
        metadata-url (str "/omkamra/dsp/harness/testbed/wasm-metadata" query)]
    (-> (js/Promise.all #js [(fetch-json plan-url) (response-text wat-url)
                             (fetch-json metadata-url)])
        (.then
         (fn [[plan wat metadata]]
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
                               layout (:memory metadata)
                               input-offset (:input-offset layout 0)
                               output-offset (:output-offset layout 4096)
                               lanes-offset (:lanes-offset layout 0)
                               width (if (= :f64 precision) 8 4)
                               expected (interpreter-process-outputs plan)
                               controls (decode-values (:controls plan))
                               ;; Lane bases are element offsets, so a byte
                               ;; position scales them by the element width.
                               lane-view (fn [byte-offset frames]
                                           (if (= :f64 precision)
                                             (js/Float64Array. (.-buffer memory) byte-offset (max 1 frames))
                                             (js/Float32Array. (.-buffer memory) byte-offset (max 1 frames))))
                               input-view (partial lane-view input-offset)
                               output-view (partial lane-view output-offset)
                               write-lanes! (fn [lanes frames]
                                              (when (seq (:lane-layout plan))
                                                (let [values (lane-values-by-name precision lanes)]
                                                  (doseq [{:keys [name offset]} (:lane-layout plan)
                                                          :let [supplied (lane-value-for values name)]
                                                          :when supplied]
                                                    (.set (lane-view (+ lanes-offset (* (long offset) width))
                                                                     frames)
                                                          (.subarray supplied 0
                                                                     (min (.-length supplied)
                                                                          frames)))))))
                               actual (do
                                        (.call reset nil)
                                        (reduce
                                         (fn [outputs {:keys [op input frames lanes]}]
                                           (case op
                                             "reset" (do (.call reset nil) outputs)
                                             "process"
                                             (let [samples (when input (decode-values input))]
                                               (when samples
                                                 (.set (input-view frames)
                                                       (real-array precision samples)))
                                               (write-lanes! lanes frames)
                                               (.apply process nil
                                                       (to-array (concat (when samples [input-offset])
                                                                         [output-offset frames
                                                                          catalog/sample-rate]
                                                                         controls
                                                                         (when (seq (:lane-layout plan))
                                                                           [lanes-offset]))))
                                               (conj outputs
                                                     (if (zero? frames)
                                                       []
                                                       (vec (output-view frames)))))))
                                         [] (:invocations plan)))]
                           (require! (results-match? :wasm precision actual expected)
                                     (str (:id c) " " (name precision)
                                          " Wasm mismatch: "
                                          (describe-mismatch :wasm precision actual expected)))
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
        source-url (str "/omkamra/dsp/harness/testbed/source" query)
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)]
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
                      (require! (results-match? :js precision actual expected)
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
    (run (filter process-case? catalog/cases) [])))

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
    (run (filter #(and (catalog/function-case? %)
                       (some #{precision} (:precisions %)))
                 catalog/all-cases)
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
                       "/omkamra/dsp/harness/testbed/squint-worklet-f64"
                       "/omkamra/dsp/harness/testbed/squint-worklet")
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
                             ;; `one-pole` is mono with one input channel:
                             ;; the callback still has two output channels, and
                             ;; the adapter silences the one the kernel does not
                             ;; write (D40 — placing a mono voice is the voice's
                             ;; business, not the adapter's).
                             :processorOptions #js {:kernelUrl (if f64?
                                                                 "/omkamra/dsp/harness/testbed/source-f64"
                                                                 "/omkamra/dsp/harness/testbed/source")
                                                    :inputChannels 1
                                                    :channels 1
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
                    (str "/omkamra/dsp/harness/testbed/squint-wasm-worklet?v="
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
  (-> (response-text "/omkamra/dsp/harness/testbed/wasm-bounded-loop-source")
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

(defn- wasm-table-test
  "Run the f32 sine case on Wasm with the lookup-table math strategy."
  []
  (let [c (catalog/case-by-id :sin)
        name "Wasm sin lookup table f32"]
    (-> (binding [*wasm-math* :table]
          (run-process-case-on-wasm c :f32))
        (.then (fn [_]
                 {:name name
                  :passed? true
                  :detail "table strategy matched the interpreter within tolerance"}))
        (.catch (fn [error]
                  {:name name
                   :passed? false
                   :detail (str (:id c) " | " (or (.-message error) (str error)))})))))

;; ---------------------------------------------------------------------------
;; Suite
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Instance independence (P8)
;; ---------------------------------------------------------------------------

;; A delay line with a delay far longer than one block, because the state has to
;; survive *across* block boundaries for sharing to be detectable at all. A
;; 1-sample delay flushes itself inside a 4-frame block, and a shared instance
;; then looks exactly like two independent ones — which is what the negative
;; control caught.
(def ^:private independence-case :delay-line-wrap)

(def ^:private independence-block 4)

;; Long enough for the impulse to emerge through a 30.25-sample delay (~block 8),
;; so the comparison can never pass vacuously.
(def ^:private independence-rounds 12)

(defn- silence?
  [outputs]
  (every? (fn [block] (every? zero? block)) outputs))

(defn- independence-problems
  "Run the independence protocol over `[alone impulse silent]` instances and
  return the violations it finds.

  The impulse instance runs once on its own and once with the silent instance
  driven between its blocks; the two sequences must agree exactly, and the
  silent instance must stay silent. The impulse must leave observable state, or
  the comparison would pass vacuously."
  [[alone-instance impulse-instance silent-instance]]
  (let [impulse [1.0 0.0 0.0 0.0]
        silence [0.0 0.0 0.0 0.0]
        rounds independence-rounds
        alone (mapv (fn [_] ((:process alone-instance) impulse independence-block))
                    (range rounds))
        ;; Interleaved: impulse instance, then the silent one, each round.
        interleaved (mapv (fn [_] [((:process impulse-instance) impulse independence-block)
                                   ((:process silent-instance) silence independence-block)])
                          (range rounds))
        impulse-out (mapv first interleaved)
        silent-out (mapv second interleaved)]
    (cond-> []
      (not= impulse-out alone)
      (conj (str "the impulse instance's output changed when a second instance "
                 "ran between its blocks"))
      (not (silence? silent-out))
      (conj (str "a silent instance produced output, so it observed another "
                 "instance's state"))
      (silence? alone)
      (conj (str "the impulse left no observable state, so the comparison "
                 "proves nothing")))))

(defn- instance-independence
  "Prove that instances of one compiled kernel do not share state.

  The protocol is run twice. Once over genuinely distinct instances, which must
  find no sharing; and once over a single instance handed to all three roles,
  which *is* sharing and therefore must be caught. A check that cannot fail its
  own negative control cannot detect sharing at all, so a silent control is
  reported as a failure rather than ignored."
  [target create-instances]
  (-> (js/Promise.all
       #js [(-> (create-instances 3)
                (.then (fn [instances] (independence-problems instances))))
            (-> (create-instances 1)
                (.then (fn [[instance]]
                         (independence-problems [instance instance instance]))))])
      (.then
       (fn [reports]
         (let [[problems control] (array-seq reports)
               result (fn [passed? detail]
                        {:name (str "instance independence (" (name target) ")")
                         :passed? passed?
                         :targets [target]
                         :detail detail})]
           (cond
             (empty? control)
             (result false
                     (str "the check failed its own negative control: sharing one "
                          "instance between all three roles went undetected"))

             (seq problems)
             (result false (str/join "; " problems))

             :else
             (result true
                     (str independence-rounds " interleaved rounds at a "
                          "30.25-sample delay: the impulse instance was identical "
                          "alone and alongside a second instance, the silent instance "
                          "stayed all zeros, and one shared instance was correctly "
                          "rejected — " (str/join "; " control) ")"))))))))

(defn- js-process-instance
  "One independent JavaScript kernel from a compiled module."
  [module precision controls]
  (let [kernel ((gobj/get module "createKernel"))
        reset (gobj/get kernel "reset")
        process (gobj/get kernel "process")]
    {:reset (fn [] (.call reset kernel))
     :process (fn [samples frames]
                (let [out (real-array precision (repeat frames 0.0))
                      in (real-array precision samples)]
                  (.apply process kernel
                          (to-array (concat [in out frames catalog/sample-rate]
                                            controls)))
                  (vec out)))
     ;; Context-style entry so `schedule/render-block!` can drive it directly.
     :process! (fn [{:keys [input output frames controls]}]
                 (.apply process kernel
                         (to-array (concat (when input [input])
                                           [output frames catalog/sample-rate]
                                           controls))))}))

(defn- wasm-process-instance
  "One independent Wasm instance, with its own linear memory."
  [^js instance precision controls]
  (let [exports (.-exports instance)
        memory (gobj/get exports "memory")
        reset (gobj/get exports "reset")
        process (gobj/get exports "process")
        input-offset 0
        output-offset 4096
        view (fn [offset frames]
               (if (= :f64 precision)
                 (js/Float64Array. (.-buffer memory) offset (max 1 frames))
                 (js/Float32Array. (.-buffer memory) offset (max 1 frames))))]
    {:reset (fn [] (.call reset nil))
     :process (fn [samples frames]
                (.set (view input-offset frames) (real-array precision samples))
                (.apply process nil
                        (to-array (concat [input-offset output-offset frames
                                           catalog/sample-rate] controls)))
                (if (zero? frames) [] (vec (view output-offset frames))))
     ;; The kernel writes into linear memory, so the rendered frames are copied
     ;; into the caller's array to match the JavaScript driver's shape.
     :process! (fn [{:keys [input output frames controls]}]
                 ;; Only `frames` elements of the caller's buffer are valid: the
                 ;; scratch may be longer than the segment being rendered.
                 (when input
                   (.set (view input-offset frames) (.subarray input 0 frames)))
                 (.apply process nil
                         (to-array (concat (when input [input-offset])
                                           [output-offset frames catalog/sample-rate]
                                           controls)))
                 (.set output (view output-offset frames)))}))

(defn- wasm-table-probe
  "Instantiate a catalog case as an f32 lookup-table kernel.

  Returns its `process`, so a test can drive it with its own inputs rather than
  the catalog's block plan."
  [c]
  (-> (response-text (str "/omkamra/dsp/harness/testbed/wasm-source"
                          (binding [*wasm-math* :table]
                            (acceptance-query c :f32))))
      (.then compile-wasm)
      (.then (fn [bytes] (js/WebAssembly.instantiate bytes)))
      (.then
       (fn [result]
         (let [instance (wasm-process-instance (.-instance result) :f32 [])]
           (:reset instance)
           (:process instance))))))

(defn- wasm-table-cos-accuracy-test
  "Under the lookup-table strategy, cosine must be no worse than sine.

  Cosine was obtained by looking up `sin(x + pi/2)`, which rounds the argument
  *before* the range reduction — an error of order `ulp(x)`. At |x| ~ 1e10 that
  doubled the strategy's error (2.0e-6 against 1.0e-6 for sine at the same
  inputs). Applying the quarter period after reduction, in index space where it
  is exactly `entry-count / 4`, removes the term, so the two errors become the
  same one: the table's, plus the range reduction's.

  So the assertion is relational, `cos <= 1.2 x sin`. That is sharper than an
  absolute bound, whose margin here is a few percent: the term being removed is
  at most ~1/6 of the total error, because the range reduction is ~2*pi times
  larger and dominates. A max over sampled arguments overstates the ratio (the
  errors oscillate), which is why both constants were measured: the shift gives
  1.415 at |x| ~ 1e10 and the fix gives 0.986 to 1.017, so the bound separates
  them with ~18% margin on the passing side and ~18% on the failing side. The
  inputs, the table and the arithmetic are all deterministic, so those numbers
  are reproducible rather than statistical.

  The absolute bound on sine is the calibration that keeps the ratio honest: it
  fails if the table itself is broken, which would otherwise let a ratio of two
  large errors pass.

  Arguments are rounded to f32 before *either* the kernel or the reference sees
  them. Comparing an f32 kernel against a reference computed on the unrounded
  double is a different argument — at |x| ~ 1e10 one f32 step is ~1024, an error
  of order 1."
  []
  (let [name "Wasm lookup table cos accuracy"
        frames 1024
        bounds [[1.0e3 5.0e-7] [1.0e10 1.5e-6]]
        inputs (fn [magnitude]
                 (mapv (fn [index]
                         (js/Math.fround (* magnitude (+ 0.5 (/ index frames)))))
                       (range frames)))
        error (fn [process magnitude reference]
                (let [xs (inputs magnitude)]
                  (apply max
                         (map (fn [a b] (js/Math.abs (- a b)))
                              (process xs frames)
                              (map reference xs)))))]
    (-> (js/Promise.all
         #js [(wasm-table-probe (catalog/case-by-id :cos))
              (wasm-table-probe (catalog/case-by-id :sin))])
        (.then
         (fn [[cos-process sin-process]]
           (let [measured (mapv (fn [[magnitude bound]]
                                  {:magnitude magnitude
                                   :bound bound
                                   :sin (error sin-process magnitude js/Math.sin)
                                   :cos (error cos-process magnitude js/Math.cos)})
                                bounds)
                 violated (filter (fn [{:keys [sin cos bound]}]
                                    (or (> sin bound)
                                        (> cos (+ (* 1.2 sin) 1.0e-9))))
                                  measured)
                 detail (fn [{:keys [magnitude sin cos]}]
                          (str magnitude ": cos/sin " (js/Math.round (* 1000 (/ cos sin))) "e-3"))]
             (require! (empty? violated)
                       (str name ": "
                            (str/join
                             "; "
                             (map (fn [{:keys [magnitude sin cos bound]}]
                                    (str "at |x| ~ " magnitude " sine " sin
                                         " (bound " bound ") cosine " cos
                                         " vs " (* 1.5 sin)))
                                  violated))))
             {:name name
              :passed? true
              :detail (str "error ratios " (str/join ", " (map detail measured)))})))
        (.catch (fn [error]
                  {:name name
                   :passed? false
                   :detail (or (.-message error) (str error))})))))

(defn- js-independence-test
  [precision]
  (let [query (str "?case=" (name independence-case) "&precision=" (name precision))
        source-url (str "/omkamra/dsp/harness/testbed/source" query)
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)]
    (-> (js/Promise.all #js [(response-text source-url) (fetch-json plan-url)])
        (.then
         (fn [[source plan]]
           (let [controls (decode-values (:controls plan))
                 blob (js/Blob. #js [source] #js {:type "text/javascript"})
                 url (js/URL.createObjectURL blob)]
             (-> (dynamic-import url)
                 (.then (fn [module]
                          (instance-independence
                           :js
                           (fn [count]
                             (js/Promise.resolve
                              (mapv (fn [_] (js-process-instance module precision controls))
                                    (range count)))))))
                 (.finally #(js/URL.revokeObjectURL url)))))))))

(defn- wasm-independence-test
  [precision]
  (let [query (str "?case=" (name independence-case) "&precision=" (name precision))
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)
        wat-url (str "/omkamra/dsp/harness/testbed/wasm-source" query)]
    (-> (js/Promise.all #js [(fetch-json plan-url) (response-text wat-url)])
        (.then
         (fn [[plan wat]]
           (let [controls (decode-values (:controls plan))]
             (-> (compile-wasm wat)
                 (.then
                  (fn [bytes]
                    (instance-independence
                     :wasm
                     (fn [count]
                       ;; `mapv`, not `repeat`: each instantiation must be a
                       ;; distinct instance with its own memory.
                       (-> (js/Promise.all
                            (to-array (mapv (fn [_] (js/WebAssembly.instantiate bytes))
                                            (range count))))
                           (.then (fn [results]
                                    (mapv (fn [result]
                                            (wasm-process-instance (.-instance result)
                                                                   precision controls))
                                          (array-seq results))))))))))))))))

;; ---------------------------------------------------------------------------
;; Sample-accurate scheduling (P9)
;; ---------------------------------------------------------------------------

(def ^:private schedule-case :delay-line-wrap)

;; Parameter changes at frames that are deliberately not block boundaries.
(def ^:private schedule-timeline [[17 0.1] [40 0.9] [155 0.6]])

(def ^:private schedule-frames 200)

(defn- schedule-queue
  [control-name timeline]
  (let [queue (event/event-queue (max 4 (count timeline)))]
    (doseq [[frame value] timeline]
      (event/push-event! queue {:frame frame :sequence frame :type :parameter
                                :name control-name :value value} 0))
    queue))

(defn- render-scheduled
  "Render the 200-frame timeline in blocks of `block-size`, splitting each block
  at event boundaries. One instance, one pass: state is not reset between
  blocks, so a fresh instance must be used per pass."
  [instance precision control-name timeline block-size controls]
  (let [total schedule-frames
        input (mapv #(Math/sin (* 0.05 %)) (range total))
        zeros (fn [n] (real-array precision (repeat n 0.0)))
        output (zeros total)
        block-in (zeros block-size)
        block-out (zeros block-size)
        input-scratch (zeros block-size)
        output-scratch (zeros block-size)
        scheduler (schedule/scheduler (schedule-queue control-name timeline))
        process! (:process! instance)]
    (loop [start 0
           controls controls]
      (if (>= start total)
        (vec output)
        (let [length (min block-size (- total start))]
          (dotimes [index length]
            (aset block-in index (nth input (+ start index))))
          (let [result (schedule/render-block!
                        scheduler
                        {:start start
                         :frames length
                         :input block-in
                         :output block-out
                         :input-scratch input-scratch
                         :output-scratch output-scratch
                         :sample-rate catalog/sample-rate
                         :controls controls
                         :control-index {control-name 0}
                         :process! process!})]
            (dotimes [index length]
              (aset output (+ start index) (aget block-out index)))
            (recur (+ start length) (:controls result))))))))

(defn- schedule-verdict
  [target create-instances]
  (-> (create-instances 4)
      (.then
       (fn [[long-block short-block tiny-block quantised-block]]
         (let [control-name (:control-name long-block)
               controls (:controls long-block)
               precision (:precision long-block)
               reference (render-scheduled long-block precision control-name
                                           schedule-timeline schedule-frames controls)
               ;; Three partitionings of the same timeline, each on its own
               ;; instance, must agree exactly...
               short (render-scheduled short-block precision control-name
                                       schedule-timeline 64 controls)
               tiny (render-scheduled tiny-block precision control-name
                                      schedule-timeline 7 controls)
               ;; ...and the control: the same block with the events *quantised*
               ;; to the block boundary, which is what a host that ignores frame
               ;; offsets would produce. If it matched, the comparison would be
               ;; proving nothing about sample accuracy.
               quantised (render-scheduled quantised-block precision control-name
                                           [] schedule-frames [(second (last schedule-timeline))])
               problems (cond-> []
                          (not= reference short)
                          (conj "a 64-frame partitioning differs from a 200-frame one")
                          (not= reference tiny)
                          (conj "a 7-frame partitioning differs from a 200-frame one")
                          (= reference quantised)
                          (conj (str "quantising the events to the block boundary "
                                     "changed nothing, so the check is not sensitive "
                                     "to frame offsets"))
                          (every? zero? reference)
                          (conj "the timeline rendered silence"))]
           {:name (str "sample-accurate scheduling (" (name target) ")")
            :passed? (empty? problems)
            :targets [target]
            :detail (if (empty? problems)
                      (str "3 partitionings of a 200-frame timeline agree exactly, "
                           "and quantising the events to the block boundary changes "
                           "the output")
                      (str/join "; " problems))})))))

(defn- js-invariance-test
  [precision]
  (let [query (str "?case=" (name schedule-case) "&precision=" (name precision))
        source-url (str "/omkamra/dsp/harness/testbed/source" query)
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)]
    (-> (js/Promise.all #js [(response-text source-url) (fetch-json plan-url)])
        (.then
         (fn [[source plan]]
           (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
                 url (js/URL.createObjectURL blob)
                 controls (decode-values (:controls plan))
                 control-name (get-in (plan-ir plan) [:controls 0 :name])]
             ;; The blob URL is deliberately not revoked: the testbed is a
             ;; one-shot page and the source is a few KB.
             (-> (dynamic-import url)
                 (.then
                  (fn [module]
                    (schedule-verdict
                     :js
                     (fn [count]
                       (js/Promise.resolve
                        (mapv (fn [_]
                                (assoc (js-process-instance module precision controls)
                                       :control-name control-name
                                       :controls controls
                                       :precision precision))
                              (range count))))))))))))))

(defn- wasm-invariance-test
  [precision]
  (let [query (str "?case=" (name schedule-case) "&precision=" (name precision))
        plan-url (str "/omkamra/dsp/harness/testbed/plan" query)
        wat-url (str "/omkamra/dsp/harness/testbed/wasm-source" query)]
    (-> (js/Promise.all #js [(fetch-json plan-url) (response-text wat-url)])
        (.then
         (fn [[plan wat]]
           (let [controls (decode-values (:controls plan))
                 control-name (get-in (plan-ir plan) [:controls 0 :name])]
             (-> (compile-wasm wat)
                 (.then
                  (fn [bytes]
                    (schedule-verdict
                     :wasm
                     (fn [count]
                       (-> (js/Promise.all
                            (to-array (mapv (fn [_] (js/WebAssembly.instantiate bytes))
                                            (range count))))
                           (.then (fn [results]
                                    (mapv (fn [result]
                                            (assoc (wasm-process-instance (.-instance result)
                                                                          precision controls)
                                                   :control-name control-name
                                                   :controls controls
                                                   :precision precision))
                                          (array-seq results))))))))))))))))

(defn- schedule-invariance-tests
  []
  (-> (js/Promise.all
       #js [(js-invariance-test :f32)
            (js-invariance-test :f64)
            (wasm-invariance-test :f32)
            (wasm-invariance-test :f64)])
      (.then (fn [results] (vec results)))))

(defn- instance-independence-tests
  []
  (-> (js/Promise.all
       #js [(js-independence-test :f32)
            (js-independence-test :f64)
            (wasm-independence-test :f32)
            (wasm-independence-test :f64)])
      (.then (fn [results] (vec results)))))

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
  "Render through the Wasm AudioWorklet adapter at one precision.

  Both precisions matter, and the f64 one is the interesting case: the adapter
  reads its element type and width from processor options, so *the same adapter
  source* has to work for f64 as well as f32, and only the module behind it
  changes. Checking f32 alone would leave that untested."
  [precision results]
  (let [suffix (if (= :f64 precision) "-f64" "")]
    (-> (js/fetch (str "/omkamra/dsp/harness/testbed/wasm-metadata" suffix))
        (.then (fn [response] (.json response)))
        (.then
         (fn [metadata]
           (-> (js/fetch (str "/omkamra/dsp/harness/testbed/wasm-source" suffix))
               (.then (fn [response] (.text response)))
               (.then
                (fn [wat]
                  (-> (compile-wasm wat)
                      (.then (fn [bytes]
                               (wasm-audio-worklet-test
                                bytes
                                (js->clj metadata :keywordize-keys true))))))))))
        (.then (fn [worklet] (conj results worklet))))))

;; ---------------------------------------------------------------------------
;; The browser pool host (P10d)
;; ---------------------------------------------------------------------------

(defn- fetch-edn
  [url]
  (-> (js/fetch url)
      (.then (clojure.core/fn [response] (.text response)))
      (.then (clojure.core/fn [text] (reader/read-string text)))))

(defn- planar-block
  "A planar `channels` x `frames` block: the shape a pool renders into, and the
  shape an audio callback hands over."
  [channels frames]
  (let [block (js/Array. channels)]
    (dotimes [channel channels]
      (aset block channel (js/Float32Array. frames)))
    block))

(defn- render-pool
  "Render `frames` through `pool`, as a vector of channel sample vectors."
  [pool channels frames]
  (let [block (planar-block channels frames)]
    (pool/render! pool block frames 48000.0)
    (mapv (clojure.core/fn [channel] (vec (array-seq (aget block channel))))
          (range channels))))

(defn- play-note
  "Play one note through `pool`: a held block, then the block after release.

  The voice writes `:activity`, so the release block is also what frees it."
  [pool channels]
  (pool/note-on! pool 60 1.0)
  (let [held (render-pool pool channels 4)]
    (pool/note-off! pool 60)
    (let [released (render-pool pool channels 1)]
      {:held held
       :released released
       :voices (count (pool/voice-snapshot pool))})))

(defn- check-host-pool
  "Compare a pool over a browser kernel against the interpreter pool, over the
  same note and the same block, and report the result as a check.

  The pools differ only in their kernel — the runtime configuration, the events,
  and the block are identical — so any difference is the host's."
  [name runtime abi ir channels kernel]
  (let [actual (play-note (pool/voice-pool runtime kernel) channels)
        expected (play-note (pool/voice-pool runtime
                                             (interpreter-host/interpreter-kernel ir abi))
                            channels)]
    (require! (= actual expected)
              (str name " drifted from the interpreter: "
                   (pr-str actual) " vs " (pr-str expected)))
    (require! (zero? (:voices actual))
              (str name ": a declared :activity should free the released voice"))
    {:name name
     :passed? true
     :detail (str "stereo pool matched the interpreter over "
                  (count (first (:held actual))) " frames across "
                  channels " channels")}))

(defn- javascript-pool-test
  "The JavaScript host: the kernel is handed the callback's own arrays."
  []
  (-> (js/Promise.all
       #js [(fetch-edn "/omkamra/dsp/harness/testbed/synth-bundle")
            (response-text "/omkamra/dsp/harness/testbed/synth-source")])
      (.then
       (clojure.core/fn [[bundle source]]
         (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
               url (js/URL.createObjectURL blob)]
           (-> (dynamic-import url)
               (.then
                (clojure.core/fn [module]
                  (let [abi (:abi bundle)]
                    (check-host-pool
                     "JavaScript pool host"
                     (:runtime bundle) abi (reader/read-string (:ir bundle))
                     (get-in abi [:channels :count])
                     (js-host/js-kernel (gobj/get module "createKernel") abi
                                        {:precision (:precision bundle)})))))
               (.finally (clojure.core/fn [] (js/URL.revokeObjectURL url)))))))))

(defn- wasm-pool-test
  "The Wasm host: the same differential check over a kernel that writes into its
  own linear memory, which is the one way the two browser hosts differ."
  []
  (-> (fetch-edn "/omkamra/dsp/harness/testbed/synth-bundle?target=wasm")
      (.then
       (clojure.core/fn [bundle]
         (-> (compile-wasm (:wat bundle))
             (.then
              (clojure.core/fn [bytes]
                (let [module (js/WebAssembly.Module. bytes)
                      abi (:abi bundle)]
                  (check-host-pool
                   "Wasm pool host"
                   (:runtime bundle) abi (reader/read-string (:ir bundle))
                   (get-in abi [:channels :count])
                   (wasm-host/wasm-kernel module abi (:memory bundle)
                                          {:precision (:precision bundle)}))))))))))

(defn- render-one!
  "Render one frame through `pool` and return the sample."
  [pool]
  (let [output (js/Float32Array. 1)]
    (pool/render! pool output 1 48000.0)
    (aget output 0)))

(defn- check-containment-scenarios
  "Exercise all three containment policies over browser kernels built by
  `make-kernel` (a function of the mode) and `make-loud-kernel` (the
  finite-runaway voice). Throws on the first mismatch."
  [name runtime make-kernel loud-runtime make-loud-kernel]
  (let [reset-pool (pool/voice-pool runtime (make-kernel :reset))]
    (pool/note-on! reset-pool 60 1.0)
    (require! (zero? (render-one! reset-pool))
              (str name ": :reset should silence the non-finite block"))
    (require! (= 1 (:containment (pool/pool-stats reset-pool)))
              (str name ": :reset should count the containment"))
    (require! (zero? (render-one! reset-pool))
              (str name ": :reset should reset the voice, not let it advance"))
    (require! (= 2 (:containment (pool/pool-stats reset-pool)))
              (str name ": :reset should keep counting")))
  (let [sanitize-pool (pool/voice-pool runtime (make-kernel :sanitize))]
    (pool/note-on! sanitize-pool 60 1.0)
    (require! (zero? (render-one! sanitize-pool))
              (str name ": :sanitize should contain the non-finite sample"))
    (require! (zero? (:containment (pool/pool-stats sanitize-pool)))
              (str name ": :sanitize should not count a containment"))
    (require! (= 1.0 (render-one! sanitize-pool))
              (str name ": :sanitize should let the state advance")))
  (let [loud-pool (pool/voice-pool loud-runtime (make-loud-kernel :sanitize))]
    (pool/note-on! loud-pool 60 1.0)
    (require! (= 1.0e6 (render-one! loud-pool))
              (str name ": :sanitize should clamp a finite runaway to the ceiling")))
  (let [off-pool (pool/voice-pool runtime (make-kernel :off))]
    (pool/note-on! off-pool 60 1.0)
    (require! (not (js/Number.isFinite (render-one! off-pool)))
              (str name ": :off should let a non-finite sample through")))
  {:name name
   :passed? true
   :detail ":reset silenced and counted, :sanitize contained and advanced and clamped a finite runaway, :off passed non-finite through"})

(defn- javascript-containment-test
  []
  (-> (js/Promise.all
       #js [(fetch-edn "/omkamra/dsp/harness/testbed/containment-bundle")
            (response-text "/omkamra/dsp/harness/testbed/containment-source")
            (fetch-edn "/omkamra/dsp/harness/testbed/containment-bundle?case=loud")
            (response-text "/omkamra/dsp/harness/testbed/containment-source?case=loud")])
      (.then
       (clojure.core/fn [[bundle source loud-bundle loud-source]]
         (let [voice-blob (js/Blob. #js [source] #js {:type "text/javascript"})
               loud-blob (js/Blob. #js [loud-source] #js {:type "text/javascript"})
               voice-url (js/URL.createObjectURL voice-blob)
               loud-url (js/URL.createObjectURL loud-blob)]
           (-> (js/Promise.all #js [(dynamic-import voice-url)
                                    (dynamic-import loud-url)])
               (.then
                (clojure.core/fn [[module loud-module]]
                  (check-containment-scenarios
                   "JavaScript containment"
                   (:runtime bundle)
                   (clojure.core/fn [mode]
                     (js-host/js-kernel (gobj/get module "createKernel") (:abi bundle)
                                        {:precision (:precision bundle) :containment mode}))
                   (:runtime loud-bundle)
                   (clojure.core/fn [mode]
                     (js-host/js-kernel (gobj/get loud-module "createKernel") (:abi loud-bundle)
                                        {:precision (:precision loud-bundle) :containment mode})))))
               (.finally (clojure.core/fn []
                           (js/URL.revokeObjectURL voice-url)
                           (js/URL.revokeObjectURL loud-url)))))))))

(defn- wasm-containment-test
  []
  (-> (js/Promise.all
       #js [(fetch-edn "/omkamra/dsp/harness/testbed/containment-bundle?target=wasm")
            (fetch-edn "/omkamra/dsp/harness/testbed/containment-bundle?case=loud&target=wasm")])
      (.then
       (clojure.core/fn [[bundle loud-bundle]]
         (-> (js/Promise.all #js [(compile-wasm (:wat bundle))
                                  (compile-wasm (:wat loud-bundle))])
             (.then
              (clojure.core/fn [[voice-bytes loud-bytes]]
                (let [voice-module (js/WebAssembly.Module. voice-bytes)
                      loud-module (js/WebAssembly.Module. loud-bytes)]
                  (check-containment-scenarios
                   "Wasm containment"
                   (:runtime bundle)
                   (clojure.core/fn [mode]
                     (wasm-host/wasm-kernel voice-module (:abi bundle) (:memory bundle)
                                            {:precision (:precision bundle) :containment mode}))
                   (:runtime loud-bundle)
                   (clojure.core/fn [mode]
                     (wasm-host/wasm-kernel loud-module (:abi loud-bundle) (:memory loud-bundle)
                                            {:precision (:precision loud-bundle) :containment mode})))))))))))

;; ---------------------------------------------------------------------------
;; The browser instrument host: a voice pool plus a post-mix bus (P11c)
;; ---------------------------------------------------------------------------

(defn- interpreter-bus-runner
  "A bus runner over the in-page interpreter: one state per channel, run with the
  channel array as both input and output."
  [bus-ir controls]
  {:create-bus
   (clojure.core/fn [] {:state (interpreter/create-state bus-ir)})
   :run-bus!
   (clojure.core/fn [bus input output frames sample-rate _controls]
     (interpreter/process-context
      bus-ir
      {:input input :output output :frames frames
       :sample-rate sample-rate :controls controls :state (:state bus)}))
   :reset-bus!
   (clojure.core/fn [bus] (interpreter/reset-state! bus-ir (:state bus)))})

(defn- channel-concat
  "Concatenate a sequence of per-block channel vectors into one channel vector
  per channel, so renders with different block sizes are comparable."
  [blocks]
  (apply mapv (clojure.core/fn [& channels] (vec (apply concat channels)))
         blocks))

(defn- reference-host
  "Build the reference synthesizer's portable host over a browser or interpreter
  kernel, from the served bundle: the runtime plan and the modulation matrix."
  [bundle kernel]
  (reference/new-host (pool/voice-pool (:runtime bundle) kernel)
                      (:control-plan (:runtime bundle))
                      (:routes bundle)))

(defn- reference-bus-controls
  "The bus control values for the reference post-mix chain, in the bundle's ABI
  order. `delay-time` is small so the feedback tail is reached within the phrase,
  which is what exercises the large-state bus on every backend."
  [bundle]
  (let [settings {"chorus-rate" 0.6 "chorus-depth" 12.0 "chorus-mix" 0.35
                  "drive" 1.5 "delay-time" 8.0 "delay-feedback" 0.45
                  "delay-mix" 0.35}]
    (mapv (clojure.core/fn [control] (get settings (name control) 0.0))
          (:bus-control-names bundle))))

(defn- render-reference-block
  "One block through the reference host: publish the matrix's modulated controls,
  render the pool, run each channel's bus in place, and return the channel
  vectors."
  [host buses channels bus-runner bus-controls frames]
  (let [block (planar-block channels frames)]
    (reference/apply-modulation! host)
    (pool/render! (:pool host) block frames 48000.0)
    (dotimes [channel channels]
      ((:run-bus! bus-runner) (nth buses channel)
                              (aget block channel) (aget block channel)
                              frames 48000.0 bus-controls))
    (mapv (clojure.core/fn [channel] (vec (array-seq (aget block channel))))
          (range channels))))

(defn- play-reference-phrase
  "Run `events` through a fresh reference host over `kernel`, plus the post-mix
  bus, and return the concatenated per-channel samples.

  Events: `[:on note velocity]`, `[:off note]`, `[:controller n value]`,
  `[:pan index value]`, `[:render frames]`."
  [bundle kernel bus-runner bus-controls events]
  (let [host (reference-host bundle kernel)
        channels (long (get-in bundle [:voice-abi :channels :count]))
        buses (mapv (clojure.core/fn [_] ((:create-bus bus-runner)))
                    (range channels))]
    (channel-concat
     (reduce
      (clojure.core/fn [blocks [event & args]]
        (case event
          :on (do (reference/note-on! host (first args) (second args)) blocks)
          :off (do (reference/note-off! host (first args)) blocks)
          :controller (do (reference/set-controller! host (first args) (second args))
                          blocks)
          :pan (do (reference/set-pan! host (first args) (second args)) blocks)
          :render (conj blocks
                        (render-reference-block host buses channels bus-runner
                                                bus-controls (first args)))))
      []
      events))))

(defn- reference-phrase
  "The reference acceptance phrase: two controllers, with every render block the
  same size so a second run can split each block and compare."
  [block repeats]
  (vec (concat [[:on 60 0.8] [:controller 1 0.7] [:controller 74 0.5]]
               (repeat repeats [:render block]))))

(defn- first-mismatch
  "The first [channel index actual expected] where two channel-vector sequences
  differ, or nil when they agree."
  [a b]
  (some (clojure.core/fn [channel]
          (let [ca (nth a channel) cb (nth b channel)]
            (some (clojure.core/fn [index]
                    (when (not= (nth ca index) (nth cb index))
                      [channel index (nth ca index) (nth cb index)]))
                  (range (min (count ca) (count cb))))))
        (range (min (count a) (count b)))))

(defn- all-finite?
  "True when every rendered sample is finite. Containment is off in these checks,
  so a filter that runs away shows up here instead of being silenced by the
  `:reset` policy one whole block at a time."
  [channels]
  (every? (clojure.core/fn [samples]
            (every? js/Number.isFinite samples))
          channels))

(defn- max-abs-diff
  "The largest absolute sample difference between two channel-vector sequences."
  [a b]
  (reduce max 0.0
          (for [channel (range (min (count a) (count b)))
                index (range (min (count (nth a channel)) (count (nth b channel))))]
            (Math/abs (- (double (nth (nth a channel) index))
                         (double (nth (nth b channel) index)))))))

(defn- check-reference-host
  "Render the reference acceptance phrase over a browser kernel, check that it is
  finite and block-size invariant, and — when `tolerance` is non-nil — compare it
  to the in-page interpreter, both with containment off.

  The interpreter differential is exact on JavaScript (both use `Math`). On Wasm
  the transcendentals are approximated, and a long oscillator run through the
  resonant voice drifts, so there is no meaningful sample tolerance; per-operation
  equivalence is covered by the shared process matrix, and this check covers what
  the host adds — that the whole instrument runs, stays finite, and is invariant."
  [name bundle browser-kernel bus-runner tolerance]
  (let [voice-ir (reader/read-string (:voice-ir bundle))
        bus-ir (reader/read-string (:bus-ir bundle))
        bus-controls (reference-bus-controls bundle)
        js-controls (clj->js bus-controls)
        phrase (reference-phrase 64 4)
        split (reference-phrase 32 8)
        actual (play-reference-phrase bundle browser-kernel bus-runner
                                      js-controls phrase)
        split-actual (play-reference-phrase bundle browser-kernel bus-runner
                                            js-controls split)
        expected (when tolerance
                   (play-reference-phrase
                    bundle
                    (interpreter-host/interpreter-kernel
                     voice-ir (:voice-abi bundle) {:containment :off})
                    (interpreter-bus-runner bus-ir bus-controls)
                    bus-controls phrase))]
    (require! (all-finite? actual)
              (str name " rendered a non-finite sample"))
    (when tolerance
      (require! (<= (max-abs-diff actual expected) tolerance)
                (str name " drifted from the interpreter: "
                     (pr-str (first-mismatch actual expected))
                     " (max absolute difference " (max-abs-diff actual expected) ")")))
    (require! (= actual split-actual)
              (str name " changed when its held blocks were split: "
                   (pr-str (first-mismatch actual split-actual))))
    {:name name
     :passed? true
     :detail (str "reference synth (voice + matrix + post-mix bus) ran over "
                  (count (first actual)) " frames across " (count actual)
                  " channels, finite and block-size invariant"
                  (when tolerance
                    (str "; interpreter match (max difference "
                         (max-abs-diff actual expected) ")")))}))

(defn- javascript-instrument-test
  []
  (-> (js/Promise.all
       #js [(fetch-edn "/omkamra/dsp/harness/testbed/instrument-bundle")
            (fetch-edn "/omkamra/dsp/harness/testbed/instrument-source")])
      (.then
       (clojure.core/fn [[bundle sources]]
         (let [voice-blob (js/Blob. #js [(:voice sources)]
                                    #js {:type "text/javascript"})
               bus-blob (js/Blob. #js [(:bus sources)]
                                  #js {:type "text/javascript"})
               voice-url (js/URL.createObjectURL voice-blob)
               bus-url (js/URL.createObjectURL bus-blob)]
           (-> (js/Promise.all #js [(dynamic-import voice-url)
                                    (dynamic-import bus-url)])
               (.then
                (clojure.core/fn [[voice-module bus-module]]
                  (let [voice-kernel
                        (js-host/js-kernel (gobj/get voice-module "createKernel")
                                           (:voice-abi bundle)
                                           {:precision (:precision bundle)
                                            :containment :off})
                        bus-runner
                        (js-host/js-bus-kernel (gobj/get bus-module "createKernel")
                                               (:bus-abi bundle))]
                    (check-reference-host "JavaScript instrument host"
                                          bundle voice-kernel bus-runner 0.0))))
               (.finally (clojure.core/fn []
                           (js/URL.revokeObjectURL voice-url)
                           (js/URL.revokeObjectURL bus-url)))))))))

(defn- wasm-instrument-test
  []
  (-> (fetch-edn "/omkamra/dsp/harness/testbed/instrument-bundle?target=wasm")
      (.then
       (clojure.core/fn [bundle]
         (-> (js/Promise.all #js [(compile-wasm (:voice-wat bundle))
                                  (compile-wasm (:bus-wat bundle))])
             (.then
              (clojure.core/fn [[voice-bytes bus-bytes]]
                (let [voice-module (js/WebAssembly.Module. voice-bytes)
                      bus-module (js/WebAssembly.Module. bus-bytes)
                      voice-kernel
                      (wasm-host/wasm-kernel voice-module (:voice-abi bundle)
                                             (:voice-memory bundle)
                                             {:precision (:precision bundle)
                                              :containment :off})
                      bus-runner
                      (wasm-host/wasm-bus-kernel bus-module (:bus-abi bundle)
                                                 (:bus-memory bundle)
                                                 (:precision bundle))]
                  (check-reference-host "Wasm instrument host"
                                        bundle voice-kernel bus-runner nil)))))))))

(defn- selected?
  "True when check `name` is selected. With no selection (nil or empty),
  everything runs; otherwise a check runs when some selected needle is a
  case-insensitive substring of its name — `\"instrument\"` selects both
  instrument hosts, `\"Wasm pool host\"` selects one."
  [selection name]
  (or (nil? selection)
      (empty? selection)
      (let [lower (str/lower-case (str name))]
        (boolean (some (clojure.core/fn [needle]
                         (str/includes? lower (str/lower-case (str needle))))
                       selection)))))

(defn- run-suite!
  ([summary results-list] (run-suite! summary results-list nil))
  ([summary results-list selection]
   (render-results! summary results-list [])
   (-> (js/Promise.resolve [])
       (.then (fn [_]
                (if (selected? selection "shared process matrix")
                  (run-process-matrix!)
                  (js/Promise.resolve []))))
       (.then (fn [process-results]
                (-> (if (selected? selection "shared function matrix")
                      (run-function-matrix!)
                      (js/Promise.resolve []))
                    (.then (fn [function-results]
                             (into process-results function-results))))))
       (.then (fn [matrix-results]
                (render-results! summary results-list matrix-results)
                matrix-results))
       (.then (fn [matrix-results]
                (run-promise-sequence
                 summary results-list matrix-results
                 (filter
                  identity
                  [(when (selected? selection "JavaScript pool host")
                     (fn [] (-> (javascript-pool-test) (.then (fn [r] [r])))))
                   (when (selected? selection "Wasm pool host")
                     (fn [] (-> (wasm-pool-test) (.then (fn [r] [r])))))
                   (when (selected? selection "JavaScript containment")
                     (fn [] (-> (javascript-containment-test) (.then (fn [r] [r])))))
                   (when (selected? selection "Wasm containment")
                     (fn [] (-> (wasm-containment-test) (.then (fn [r] [r])))))
                   (when (selected? selection "JavaScript instrument host")
                     (fn [] (-> (javascript-instrument-test) (.then (fn [r] [r])))))
                   (when (selected? selection "Wasm instrument host")
                     (fn [] (-> (wasm-instrument-test) (.then (fn [r] [r])))))
                   (when (selected? selection "WAT bounded-loop module")
                     (fn [] (-> (bounded-loop-wasm-test) (.then (fn [r] [r])))))
                   (when (selected? selection "Wasm sin lookup table")
                     (fn [] (-> (wasm-table-test) (.then (fn [r] [r])))))
                   (when (selected? selection "Wasm lookup table cos")
                     (fn [] (-> (wasm-table-cos-accuracy-test) (.then (fn [r] [r])))))
                   (when (selected? selection "instance independence")
                     (fn [] (instance-independence-tests)))
                   (when (selected? selection "sample-accurate scheduling")
                     (fn [] (schedule-invariance-tests)))
                   (when (selected? selection "AudioWorklet")
                     (fn [] (-> (audio-worklet-test :f32)
                                (.then (fn [a]
                                         (-> (audio-worklet-test :f64)
                                             (.then (fn [b] [a b]))))))))]))))
       (.then (fn [results]
                (if (selected? selection "AudioWorklet")
                  (-> (wasm-worklet-step :f32 results)
                      (.then (clojure.core/fn [r] (wasm-worklet-step :f64 r))))
                  (js/Promise.resolve results))))
       (.then (fn [results]
                (let [results (if (empty? (or selection []))
                                results
                                (filterv (clojure.core/fn [result]
                                           (or (not (:passed? result))
                                               (selected? selection (:name result))))
                                         results))]
                  (render-results! summary results-list results)
                  results)))
       (.catch (fn [error]
                 (let [failure {:name "browser suite"
                                :passed? false
                                :detail (or (.-message error) (str error))}
                       results (conj (or @last-results []) failure)]
                   (render-results! summary results-list results)
                   results))))))

(defn run-conformance!
  "Run the browser conformance suite and resolve a structured result.

  With no argument every check runs. An optional `selection`, a collection of
  name substrings (`\"instrument\"`, `\"Wasm pool host\"`), runs and returns
  only the checks whose name matches, so a single check can be iterated on
  without paying for the rest of the suite.

  Safe to call from `cljs-eval` or from a Playwright driver such as
  `omkamra.dsp.harness.browser`.  The result is `{:passed? :passed :total :results}`;
  each entry carries `:name`, `:passed?`, `:detail`, and, for the shared
  matrices, `:targets` and a per-case `:cases` breakdown."
  ([] (run-conformance! nil))
  ([selection]
   (reset! last-results nil)
   (let [selection (when (seq selection) (mapv str (js->clj selection)))
         summary (make-element "strong")
         results-list (make-element "ul")]
     (-> (run-suite! summary results-list selection)
         (.then
          (clojure.core/fn [results]
            (clj->js
             {:passed? (every? :passed? results)
              :passed (count (filter :passed? results))
              :total (count results)
              :results (mapv (clojure.core/fn [result]
                               (clj->js (select-keys
                                         result [:name :passed? :detail :targets :cases])))
                             results)})))))))

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
               :caseIds (clj->js (mapv :id catalog/all-cases))})
    (reset! app-state {:summary summary :results-list results-list})
    (when-not (str/includes? (or (.-search (.-location js/window)) "") "autorun=0")
      (run-suite! summary results-list))))
