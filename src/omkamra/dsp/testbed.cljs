(ns omkamra.dsp.testbed
  (:require [goog.object :as gobj]
            [omkamra.dsp.acceptance :as acceptance]
            ["wabt" :default wabt]))

(defonce app-state (atom nil))

(def block-lengths acceptance/block-lengths)

(defn- fetch-json
  [url]
  (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.json response)
                 (js/Promise.reject
                  (js/Error. (str "metadata endpoint returned "
                                  (.-status response)))))))
      (.then #(js->clj % :keywordize-keys true))))

(defn- float-array
  [values]
  (js/Float32Array. (clj->js values)))

(defn- values
  [array]
  (mapv #(aget array %) (range (.-length array))))

(defn- wasm-array
  [memory buffer offset length]
  (if (= "f64" (:element-type memory))
    (js/Float64Array. buffer offset length)
    (js/Float32Array. buffer offset length)))

(defn- close?
  [actual expected]
  (or (= actual expected)
      (and (js/Number.isNaN actual)
           (js/Number.isNaN expected))
      (< (js/Math.abs (- actual expected)) 0.00001)))

(defn- arrays-close?
  [actual expected]
  (and (= (.-length actual) (count expected))
       (every? true?
               (map close? (values actual) expected))))

(defn- require!
  [condition message]
  (when-not condition
    (throw (js/Error. message))))

(defn- block-input
  [block frames]
  (float-array (acceptance/input-samples :f32 (acceptance/case-by-id :one-pole) block frames)))

(defn- reference-one-pole
  [previous input _coefficient]
  (let [[next output] (acceptance/reference-block
                       :f32 (acceptance/case-by-id :one-pole) previous
                       (values input))]
    [next (float-array output)]))

(defn- reference-one-pole-f64
  [previous input _coefficient]
  (let [[next output] (acceptance/reference-block
                       :f64 (acceptance/case-by-id :one-pole) previous
                       (values input))]
    [next (js/Float64Array. (clj->js output))]))

(defn- gain-process!
  "The block ABI shape that the generated JS kernel must implement."
  [input output frames amount]
  (dotimes [frame frames]
    (aset output frame (float (* (aget input frame) amount))))
  nil)

(defn- one-pole-process!
  "A stateful reference kernel used to exercise block boundaries and reset."
  [state input output frames coefficient]
  (let [previous (atom @state)]
    (dotimes [frame frames]
      (let [next (+ (aget input frame)
                    (* coefficient @previous))]
        (aset output frame (float next))
        (reset! previous next)))
    (reset! state @previous))
  nil)

(defn- test-gain
  []
  (let [input (float-array [1.0 -2.0 0.5])
        output (float-array [99.0 99.0 99.0 99.0])]
    (gain-process! input output 2 2.0)
    (require! (arrays-close? output [2.0 -4.0 99.0 99.0])
              (str "expected [2 -4 99 99], got " (values output)))
    {:detail (str "partial block -> " (values output))}))

(defn- test-one-pole
  []
  (let [state (atom 0.0)
        first-input (float-array [1.0 0.0 0.0])
        second-input (float-array [0.0])
        output (float-array 3)]
    (one-pole-process! state first-input output 3 0.5)
    (require! (arrays-close? output [1.0 0.5 0.25])
              (str "first block mismatch: " (values output)))
    (one-pole-process! state second-input output 1 0.5)
    (require! (close? (aget output 0) 0.125)
              (str "state was not preserved: " (aget output 0)))
    (reset! state 0.0)
    (one-pole-process! state second-input output 1 0.5)
    (require! (close? (aget output 0) 0.0)
              (str "reset did not clear state: " (aget output 0)))
    {:detail "state preserved across blocks and reset correctly"}))

(defn- test-zero-length-block
  []
  (let [input (float-array [])
        output (float-array [7.0])]
    (gain-process! input output 0 2.0)
    (require! (arrays-close? output [7.0])
              (str "zero-length block changed output: " (values output)))
    {:detail "zero-length block left output untouched"}))

(defn- run-test
  [name test-fn]
  (try
    (let [{:keys [detail]} (test-fn)]
      {:name name :passed? true :detail detail})
    (catch :default error
      {:name name
       :passed? false
       :detail (or (.-message error) (str error))})))

(defn- run-suite
  []
  [(run-test "stateless gain block" test-gain)
   (run-test "stateful one-pole block" test-one-pole)
   (run-test "zero-length block" test-zero-length-block)])

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
  (let [passed (count (filter :passed? results))
        total (count results)]
    (text! summary (str passed "/" total " checks passed"))
    (set! (.-textContent results-list) "")
    (doseq [{:keys [name passed? detail]} results]
      (let [item (make-element "li")
            marker (if passed? "PASS" "FAIL")]
        (text! item (str marker "  " name " — " detail))
        (style! item "color" (if passed? "#166534" "#b91c1c"))
        (append! results-list item)))))

(defn- dynamic-import
  [url]
  (js* "import(~{})" url))

(defn- generated-kernel-test
  [source]
  (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
        url (js/URL.createObjectURL blob)]
    (-> (dynamic-import url)
        (.then (fn [module]
                 (let [create-kernel (gobj/get module "createKernel")
                       kernel (create-kernel)
                       process (gobj/get kernel "process")
                       reset (gobj/get kernel "reset")]
                   (.call reset kernel)
                   (loop [block 0
                          previous 0.0]
                     (if (= block (count block-lengths))
                       nil
                       (let [frames (nth block-lengths block)
                             input (block-input block frames)
                             output (float-array (if (zero? frames)
                                                   [77.0]
                                                   (repeat frames 0.0)))
                             [next-previous expected] (reference-one-pole
                                                       previous input 0.5)]
                         (.call process kernel input output frames 44100.0 0.5)
                         (if (zero? frames)
                           (require! (close? (aget output 0) 77.0)
                                     "generated kernel changed zero-length output")
                           (require! (arrays-close? output (values expected))
                                     (str "generated kernel mismatch at block " block
                                          ": " (values output))))
                         (recur (inc block) next-previous))))
                   (.call reset kernel)
                   (let [reset-output (float-array [99.0])]
                     (.call process kernel (float-array [0.0]) reset-output 1 44100.0 0.5)
                     (require! (close? (aget reset-output 0) 0.0)
                               (str "generated kernel reset mismatch: "
                                    (aget reset-output 0))))
                   (doseq [special [js/NaN js/Infinity]]
                     (.call reset kernel)
                     (let [input (float-array [special])
                           output (float-array 1)
                           [_ expected] (reference-one-pole 0.0 input 0.5)]
                       (.call process kernel input output 1 44100.0 0.5)
                       (require! (arrays-close? output (values expected))
                                 (str "generated kernel special-value mismatch: "
                                      (values output)))))
                   {:name "generated JavaScript module"
                    :passed? true
                    :detail "loaded through ES module import; matched shared f32 vectors, reset, NaN, and infinity"})))
        (.finally #(js/URL.revokeObjectURL url)))))

(defn- generated-f64-kernel-test
  [source]
  (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
        url (js/URL.createObjectURL blob)]
    (-> (dynamic-import url)
        (.then
         (fn [module]
           (let [kernel ((gobj/get module "createKernel"))
                 process (gobj/get kernel "process")
                 reset (gobj/get kernel "reset")]
             (.call reset kernel)
             (loop [block 0
                    previous 0.0]
               (if (= block (count block-lengths))
                 (do
                   (.call reset kernel)
                   (let [reset-output (js/Float64Array. #js [99.0])]
                     (.call process kernel (js/Float64Array. #js [0.0])
                            reset-output 1 acceptance/sample-rate (first (:controls (acceptance/case-by-id :one-pole))))
                     (require! (close? (aget reset-output 0) 0.0)
                               "generated f64 kernel reset mismatch"))
                   {:name "generated JavaScript f64 module"
                    :passed? true
                    :detail "loaded through ES module import and matched shared f64 vectors"})
                 (let [frames (nth block-lengths block)
                       samples (acceptance/input-samples
                                :f64 (acceptance/case-by-id :one-pole) block frames)
                       input (js/Float64Array. (clj->js samples))
                       output (if (zero? frames)
                                (js/Float64Array. #js [77.0])
                                (js/Float64Array. frames))
                       [next-previous expected] (acceptance/reference-block
                                                 :f64 (acceptance/case-by-id :one-pole) previous samples)]
                   (.call process kernel input output frames
                          acceptance/sample-rate (first (:controls (acceptance/case-by-id :one-pole))))
                   (if (zero? frames)
                     (require! (close? (aget output 0) 77.0)
                               "generated f64 kernel changed zero-length output")
                     (require! (arrays-close? output expected)
                               (str "generated f64 kernel mismatch at block "
                                    block ": " (values output))))
                   (recur (inc block) next-previous)))))))
        (.finally #(js/URL.revokeObjectURL url)))))

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

(defn- wasm-module-test
  [bytes memory-layout]
  (let [input-offset (:input-offset memory-layout)
        output-offset (:output-offset memory-layout)
        reference-one-pole (if (= "f64" (:element-type memory-layout))
                             reference-one-pole-f64
                             reference-one-pole)]
    (-> (js/WebAssembly.instantiate bytes)
        (.then
         (fn [result]
           (let [exports (.-exports (.-instance result))
                 memory (gobj/get exports "memory")
                 process (gobj/get exports "process")
                 reset (gobj/get exports "reset")
                 memory-buffer (.-buffer memory)]
             (.call reset nil)
             (loop [block 0
                    previous 0.0]
               (if (= block (count block-lengths))
                 nil
                 (let [frames (nth block-lengths block)
                       input (wasm-array memory-layout memory-buffer
                                         input-offset frames)
                       output (wasm-array memory-layout memory-buffer
                                          output-offset (max 1 frames))
                       source (block-input block frames)
                       [next-previous expected] (reference-one-pole
                                                 previous source 0.5)]
                   (.set input source)
                   (when (zero? frames)
                     (aset output 0 77.0))
                   (.call process nil input-offset output-offset frames 44100.0 0.5)
                   (if (zero? frames)
                     (require! (close? (aget output 0) 77.0)
                               "Wasm kernel changed zero-length output")
                     (require! (arrays-close? output (values expected))
                               (str "Wasm kernel mismatch at block " block
                                    ": actual " (values output)
                                    ", expected " (values expected))))
                   (recur (inc block) next-previous))))
             (.call reset nil)
             (let [input (wasm-array memory-layout memory-buffer input-offset 1)
                   output (wasm-array memory-layout memory-buffer output-offset 1)]
               (.set input #js [0.0])
               (.call process nil input-offset output-offset 1 44100.0 0.5)
               (require! (close? (aget output 0) 0.0)
                         (str "Wasm reset mismatch: " (aget output 0))))
             (let [check-special!
                   (fn [special]
                     (.call reset nil)
                     (let [input (wasm-array memory-layout memory-buffer input-offset 1)
                           output (wasm-array memory-layout memory-buffer output-offset 1)]
                       (.set input #js [special])
                       (.call process nil input-offset output-offset 1 44100.0 0.5)
                       (let [[_ expected] (reference-one-pole
                                           0.0
                                           (float-array [special])
                                           0.5)]
                         (require! (arrays-close? output (values expected))
                                   (str "Wasm special-value mismatch: "
                                        (values output))))))]
               (doseq [special [js/NaN js/Infinity]]
                 (check-special! special)))
             {:name "WABT WebAssembly module"
              :passed? true
              :detail "compiled WAT; matched shared f32 vectors, artifact memory metadata, reset, NaN, and infinity"}))))))

(defn- real-array
  [precision samples]
  (if (= :f64 precision)
    (js/Float64Array. (clj->js samples))
    (float-array samples)))

(defn- process-arguments
  [prefix case]
  (to-array (concat prefix [acceptance/sample-rate] (:controls case))))

(defn- generated-acceptance-case-test
  [case precision source]
  (let [blob (js/Blob. #js [source] #js {:type "text/javascript"})
        url (js/URL.createObjectURL blob)]
    (-> (dynamic-import url)
        (.then
         (fn [module]
           (let [kernel ((gobj/get module "createKernel"))
                 process (gobj/get kernel "process")
                 reset (gobj/get kernel "reset")]
             (.call reset kernel)
             (loop [block 0
                    state (acceptance/initial-state case)]
               (when (< block (count acceptance/block-lengths))
                 (let [frames (nth acceptance/block-lengths block)
                       samples (acceptance/input-samples precision case block frames)
                       input (real-array precision samples)
                       output (real-array precision (if (zero? frames)
                                                      [77.0]
                                                      (repeat frames 0.0)))
                       [next-state expected] (acceptance/reference-block
                                              precision case state samples)]
                   (.apply process kernel (process-arguments [input output frames] case))
                   (if (zero? frames)
                     (require! (close? (aget output 0) 77.0)
                               (str (:id case) " changed zero-length output"))
                     (require! (arrays-close? output expected)
                               (str (:id case) " mismatch at block " block)))
                   (recur (inc block) next-state))))
             (.call reset kernel)
             (let [input (real-array precision [0.0])
                   output (real-array precision [99.0])
                   [_ expected] (acceptance/reference-block
                                 precision case (acceptance/initial-state case)
                                 (values input))]
               (.apply process kernel (process-arguments [input output 1] case))
               (require! (arrays-close? output expected)
                         (str (:id case) " reset mismatch")))
             (doseq [special [js/NaN js/Infinity]]
               (.call reset kernel)
               (let [input (real-array precision [special])
                     output (real-array precision [0.0])
                     [_ expected] (acceptance/reference-block
                                   precision case (acceptance/initial-state case)
                                   (values input))]
                 (.apply process kernel (process-arguments [input output 1] case))
                 (require! (arrays-close? output expected)
                           (str (:id case) " special-value mismatch"))))
             {:target :js
              :case (:id case)
              :precision precision
              :passed? true})))
        (.finally #(js/URL.revokeObjectURL url)))))

(defn- wasm-acceptance-case-test
  [case precision bytes memory-layout]
  (let [input-offset (:input-offset memory-layout)
        output-offset (:output-offset memory-layout)]
    (-> (js/WebAssembly.instantiate bytes)
        (.then
         (fn [result]
           (let [exports (.-exports (.-instance result))
                 memory (gobj/get exports "memory")
                 process (gobj/get exports "process")
                 reset (gobj/get exports "reset")
                 memory-buffer (.-buffer memory)]
             (.call reset nil)
             (loop [block 0
                    state (acceptance/initial-state case)]
               (when (< block (count acceptance/block-lengths))
                 (let [frames (nth acceptance/block-lengths block)
                       samples (acceptance/input-samples precision case block frames)
                       input (wasm-array memory-layout memory-buffer input-offset frames)
                       output (wasm-array memory-layout memory-buffer output-offset (max 1 frames))
                       [next-state expected] (acceptance/reference-block
                                              precision case state samples)]
                   (.set input (real-array precision samples))
                   (when (zero? frames) (aset output 0 77.0))
                   (.apply process nil
                           (process-arguments [input-offset output-offset frames] case))
                   (if (zero? frames)
                     (require! (close? (aget output 0) 77.0)
                               (str (:id case) " changed zero-length output"))
                     (require! (arrays-close? output expected)
                               (str (:id case) " mismatch at block " block)))
                   (recur (inc block) next-state))))
             (.call reset nil)
             (let [input (wasm-array memory-layout memory-buffer input-offset 1)
                   output (wasm-array memory-layout memory-buffer output-offset 1)
                   samples [0.0]
                   [_ expected] (acceptance/reference-block
                                 precision case (acceptance/initial-state case) samples)]
               (.set input (real-array precision samples))
               (.apply process nil (process-arguments [input-offset output-offset 1] case))
               (require! (arrays-close? output expected)
                         (str (:id case) " reset mismatch")))
             (doseq [special [js/NaN js/Infinity]]
               (.call reset nil)
               (let [input (wasm-array memory-layout memory-buffer input-offset 1)
                     output (wasm-array memory-layout memory-buffer output-offset 1)
                     samples [special]
                     [_ expected] (acceptance/reference-block
                                   precision case (acceptance/initial-state case) samples)]
                 (.set input (real-array precision samples))
                 (.apply process nil (process-arguments [input-offset output-offset 1] case))
                 (require! (arrays-close? output expected)
                           (str (:id case) " special-value mismatch"))))
             {:target :wasm
              :case (:id case)
              :precision precision
              :passed? true}))))))

(defn- worklet-ready
  [node]
  (js/Promise.
   (fn [resolve reject]
     (let [timeout (js/setTimeout
                    #(reject (js/Error.
                              "worklet kernel did not become ready"))
                    5000)]
       (set! (.-onmessage (.-port node))
             (fn [event]
               (let [message (.-data event)]
                 (cond
                   (= "ready" (.-type message))
                   (do
                     (js/clearTimeout timeout)
                     (resolve))
                   (= "error" (.-type message))
                   (do
                     (js/clearTimeout timeout)
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
                 (cond
                   (= "reset" (.-type message))
                   (do
                     (js/clearTimeout timeout)
                     (resolve))))))
       (.postMessage (.-port node) #js {:type "reset"})))))

(defn- audio-worklet-test
  ([] (audio-worklet-test :f32))
  ([precision]
   (let [context (js/OfflineAudioContext. 2 256 44100)
         f64? (= :f64 precision)
         worklet-url (if f64?
                       "/omkamra/dsp/testbed/squint-worklet-f64"
                       "/omkamra/dsp/testbed/squint-worklet")
         kernel-url (if f64?
                      "/omkamra/dsp/testbed/source-f64"
                      "/omkamra/dsp/testbed/source")
         label (if f64?
                 "Squint AudioWorklet f64 adapter"
                 "Squint AudioWorklet adapter")]
     (-> (.addModule (.-audioWorklet context)
                     (str worklet-url "?v=" (js/Date.now)))
         (.then
          (fn [_]
            (let [node (js/AudioWorkletNode.
                        context
                        "omkamra-dsp-kernel"
                        #js {:numberOfInputs 1
                             :numberOfOutputs 1
                             :channelCount 2
                             :channelCountMode "explicit"
                             :outputChannelCount #js [2]
                             :processorOptions #js {:kernelUrl kernel-url
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
              (require! (> peak 0.0001)
                        "AudioWorklet output was silent")
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
        element-bytes (or (:element-bytes physical-type)
                          (:element-bytes memory))
        element-type (or (:element-type physical-type)
                         (:element-type memory))
        element-type (if (keyword? element-type)
                       (name element-type)
                       element-type)
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
                       context
                       "omkamra-dsp-wasm-kernel"
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
             (require! (> peak 0.0001)
                       "Wasm AudioWorklet output was silent")
             (require! (= 2 (.-numberOfChannels buffer))
                       "Wasm worklet did not preserve configured channels")
             {:name label
              :passed? true
              :detail (str "rendered 256 frames across "
                           (.-numberOfChannels buffer) " channels, peak " peak)}))))))

(defn- run-wasm-f64-test!
  [summary results-list previous-results]
  (-> (js/fetch "/omkamra/dsp/testbed/wasm-source-f64")
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "f64 Wasm endpoint returned "
                                  (.-status response)))))))
      (.then compile-wasm)
      (.then
       (fn [bytes]
         (-> (fetch-json "/omkamra/dsp/testbed/wasm-metadata-f64")
             (.then
              (fn [metadata]
                (-> (wasm-module-test bytes (:memory metadata))
                    (.then
                     (fn [module-result]
                       (let [module-result (assoc module-result
                                                  :name "WABT WebAssembly f64 module")
                             results (conj previous-results module-result)]
                         (render-results! summary results-list results)
                         (-> (wasm-audio-worklet-test bytes metadata)
                             (.then
                              (fn [worklet-result]
                                (let [results (conj results worklet-result)]
                                  (render-results! summary results-list results)
                                  results)))))))))))))
      (.catch
       (fn [error]
         (render-results!
          summary
          results-list
          (conj previous-results
                {:name "WABT WebAssembly f64 module"
                 :passed? false
                 :detail (or (.-message error) (str error))}))))))

(defn- run-wasm-test!
  [summary results-list previous-results]
  (-> (js/fetch "/omkamra/dsp/testbed/wasm-source")
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "Wasm endpoint returned " (.-status response)))))))
      (.then compile-wasm)
      (.then
       (fn [bytes]
         (-> (fetch-json "/omkamra/dsp/testbed/wasm-metadata")
             (.then
              (fn [memory-metadata]
                (-> (wasm-module-test bytes (:memory memory-metadata))
                    (.then
                     (fn [module-result]
                       (let [results (conj previous-results module-result)]
                         (render-results! summary results-list results)
                         (-> (wasm-audio-worklet-test
                              bytes memory-metadata)
                             (.then
                              (fn [worklet-result]
                                (let [results (conj results worklet-result)]
                                  (render-results! summary results-list results)
                                  (run-wasm-f64-test! summary results-list results))))))))))))))
      (.catch (fn [error]
                (render-results!
                 summary
                 results-list
                 (conj previous-results
                       {:name "WABT WebAssembly / AudioWorklet"
                        :passed? false
                        :detail (or (.-message error) (str error))}))))))

(defn run-cross-target!
  "Run generated JavaScript and Wasm kernels against the same browser-side
  reference trace. Public for shadow/cljs-eval and browser automation."
  []
  (let [result-promise
        (-> (js/fetch "/omkamra/dsp/testbed/source")
            (.then (fn [response] (.text response)))
            (.then generated-kernel-test)
            (.then
             (fn [javascript-result]
               (-> (js/fetch "/omkamra/dsp/testbed/wasm-source")
                   (.then (fn [response] (.text response)))
                   (.then compile-wasm)
                   (.then
                    (fn [bytes]
                      (-> (fetch-json "/omkamra/dsp/testbed/wasm-metadata")
                          (.then
                           (fn [metadata]
                             (wasm-module-test bytes (:memory metadata)))))))
                   (.then
                    (fn [wasm-result]
                      {:javascript javascript-result
                       :wasm wasm-result
                       :differential? (and (:passed? javascript-result)
                                           (:passed? wasm-result))}))))))]
    (.then result-promise
           (fn [result]
             (swap! app-state assoc :cross-target result)
             result))))

(defn- response-text
  [url]
  (-> (js/fetch url)
      (.then (fn [response] (.text response)))))

(defn- acceptance-query
  [case precision]
  (str "?case=" (name (:id case)) "&precision=" (name precision)))

(defn- run-acceptance-case!
  [case precision]
  (let [query (acceptance-query case precision)
        source-url (str "/omkamra/dsp/testbed/source" query)
        wasm-url (str "/omkamra/dsp/testbed/wasm-source" query)
        metadata-url (str "/omkamra/dsp/testbed/wasm-metadata" query)]
    (-> (response-text source-url)
        (.then #(generated-acceptance-case-test case precision %))
        (.then
         (fn [javascript]
           (-> (response-text wasm-url)
               (.then compile-wasm)
               (.then
                (fn [bytes]
                  (-> (fetch-json metadata-url)
                      (.then
                       (fn [metadata]
                         (-> (wasm-acceptance-case-test case precision bytes
                                                        (:memory metadata))
                             (.then
                              (fn [wasm]
                                {:case (:id case)
                                 :precision precision
                                 :javascript javascript
                                 :wasm wasm
                                 :passed? (and (:passed? javascript)
                                               (:passed? wasm))}))))))))))))))

(defn- run-shared-target-precision!
  [precision]
  (letfn [(run-cases [remaining results]
            (if-let [case (first remaining)]
              (-> (run-acceptance-case! case precision)
                  (.then #(run-cases (next remaining) (conj results %))))
              (js/Promise.resolve
               {:precision precision
                :cases results
                :passed? (every? :passed? results)})))]
    (run-cases (filter #(some #{precision} (:precisions %)) acceptance/cases) [])))

(defn run-shared-target-vectors!
  "Run every shared acceptance case in the browser for both precisions.

  `omkamra.dsp.acceptance/cases` is also run by the default
  interpreter/JVM test, so adding a case extends every semantic target."
  []
  (-> (run-shared-target-precision! :f32)
      (.then
       (fn [f32]
         (-> (run-shared-target-precision! :f64)
             (.then
              (fn [f64]
                (let [result {:cases (mapv :id acceptance/cases)
                              :f32 f32
                              :f64 f64
                              :passed? (and (:passed? f32) (:passed? f64))}]
                  (swap! app-state assoc :shared-target-vectors result)
                  result))))))))

(defn- shared-target-suite-result
  [result]
  {:name "shared JS/Wasm acceptance matrix"
   :passed? (:passed? result)
   :detail (str "matched " (count acceptance/cases)
                " cases across f32/f64 JavaScript and Wasm")})

(defn- run-shared-target-suite-test!
  [summary results-list previous-results]
  (-> (run-shared-target-vectors!)
      (.then
       (fn [result]
         (let [results (conj previous-results
                             (shared-target-suite-result result))]
           (render-results! summary results-list results)
           results)))
      (.catch
       (fn [error]
         (let [results (conj previous-results
                             {:name "shared JS/Wasm acceptance matrix"
                              :passed? false
                              :detail (or (.-message error) (str error))})]
           (render-results! summary results-list results)
           results)))))

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
                  (require! (close? (aget output 0) 2.0)
                            (str "expected bounded-loop output 2, got "
                                 (aget output 0)))
                  (.call reset nil)
                  (let [trapped? (try
                                   (.call process nil 0 4096 1 44100.0 3)
                                   false
                                   (catch :default _ true))]
                    (require! trapped?
                              "bounded Wasm loop did not trap on exhaustion")
                    {:name "WAT bounded-loop module"
                     :passed? true
                     :detail "compiled WAT; completed within bound and trapped on exhaustion"})))))))
      (.catch
       (fn [error]
         {:name "WAT bounded-loop module"
          :passed? false
          :detail (or (.-message error) (str error))}))))

(defn- run-bounded-loop-wasm-suite-test!
  [summary results-list previous-results]
  (-> (bounded-loop-wasm-test)
      (.then
       (fn [result]
         (let [results (conj previous-results result)]
           (render-results! summary results-list results)
           results)))))

(defn- run-generated-f64-test!
  [summary results-list previous-results]
  (-> (js/fetch "/omkamra/dsp/testbed/source-f64")
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "f64 JavaScript endpoint returned "
                                  (.-status response)))))))
      (.then generated-f64-kernel-test)
      (.then
       (fn [result]
         (let [results (conj previous-results result)]
           (render-results! summary results-list results)
           (-> (audio-worklet-test :f64)
               (.then
                (fn [worklet-result]
                  (let [results (conj results worklet-result)]
                    (render-results! summary results-list results)
                    (run-wasm-test! summary results-list results))))))))))

(defn- run-generated-test!
  [summary results-list previous-results]
  (-> (js/fetch "/omkamra/dsp/testbed/source")
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "kernel endpoint returned " (.-status response)))))))
      (.then generated-kernel-test)
      (.then
       (fn [generated-result]
         (let [results (conj previous-results generated-result)]
           (render-results! summary results-list results)
           (-> (audio-worklet-test)
               (.then (fn [worklet-result]
                        (let [results (conj results worklet-result)]
                          (render-results! summary results-list results)
                          (run-generated-f64-test! summary results-list results))))))))
      (.catch (fn [error]
                (render-results!
                 summary
                 results-list
                 (conj previous-results
                       {:name "generated JavaScript / AudioWorklet"
                        :passed? false
                        :detail (or (.-message error) (str error))}))))))

(defn- run-suite!
  [summary results-list]
  (let [results (run-suite)]
    (render-results! summary results-list results)
    (-> (run-shared-target-suite-test! summary results-list results)
        (.then #(run-bounded-loop-wasm-suite-test! summary results-list %))
        (.then #(run-generated-test! summary results-list %)))))

(defn init
  []
  (set! (.-innerHTML (.-body js/document)) "")
  (style! (.-body js/document) "fontFamily" "system-ui, sans-serif")
  (style! (.-body js/document) "lineHeight" "1.5")
  (style! (.-body js/document) "margin" "0")
  (let [main (make-element "main")
        heading (text! (make-element "h1") "omkamra.dsp browser testbed")
        intro (text! (make-element "p")
                     "A browser-side block ABI testbed. One run covers the shared f32/f64 JavaScript/Wasm acceptance matrix, bounded-loop WAT traps, handwritten CLJS checks, generated JavaScript modules, both Squint AudioWorklet adapters, and WABT-compiled Wasm modules.")
        button (text! (make-element "button") "Run checks")
        summary (make-element "strong")
        results-list (make-element "ul")
        notes (text! (make-element "h2") "What this exercises")
        description (text! (make-element "p")
                           "Float32Array and Float64Array input/output buffers, partial blocks, zero-length blocks, state carried across blocks, reset behavior, shared catalog vectors, bounded-loop exhaustion traps, emitted JavaScript modules, JavaScript and Wasm AudioWorklet block processing, WAT emission, and browser-side Wasm instantiation.")]
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
    (append! main notes)
    (append! main description)
    (append! (.-body js/document) main)
    (.addEventListener button "click" #(run-suite! summary results-list))
    (reset! app-state {:summary summary :results-list results-list})
    (run-suite! summary results-list)))
