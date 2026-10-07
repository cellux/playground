(ns omkamra.dsp.wasm-worklet-adapter
  (:require [squint.defclass :refer [defclass]]))

(defclass DSPWasmProcessor
  (extends AudioWorkletProcessor)
  (constructor [this options]
               (super)
               (set! (.-ready this) false)
               (set! (.-instance this) nil)
               (set! (.-memory-view this) nil)
               (set! (.-memory-buffer this) nil)
               (set! (.-onmessage (.-port this))
                     (fn [event]
                       (let [message (.-data event)]
                         (cond
                           (= "status" (aget message "type"))
                           (if (.-ready this)
                             (.postMessage (.-port this) #js {:type "ready"})
                             (.postMessage (.-port this) #js {:type "pending"}))
                           (= "reset" (aget message "type"))
                           (when (.-ready this)
                             (.call (.-reset-fn this) nil)
                             (.postMessage (.-port this) #js {:type "reset"}))
                           (= "init" (aget message "type"))
                           (when (.-ready this)
                             (.call (.-init-fn this) nil)
                             (.postMessage (.-port this) #js {:type "init"}))))))
               (let [processor-options (.-processorOptions options)
                     wasm-bytes (aget processor-options "wasmBytes")
                     element-bytes (or (aget processor-options "elementBytes") 4)
                     element-type (or (aget processor-options "elementType") "f32")
                     channels (or (aget processor-options "channels") 1)
                     input-offset (or (aget processor-options "inputOffset") 0)
                     output-offset (or (aget processor-options "outputOffset") 0)
                     channel-stride (or (aget processor-options "channelStride") 4096)
                     frame-capacity (or (aget processor-options "frameCapacity") 0)
                     control-names (or (aget processor-options "controlNames")
                                       #js ["coefficient"])
                     control-defaults (or (aget processor-options "controlDefaults")
                                          #js [0.5])
                     aligned? (and (= 0 (mod input-offset element-bytes))
                                   (= 0 (mod output-offset element-bytes))
                                   (= 0 (mod channel-stride element-bytes)))
                     required-bytes (max (+ output-offset
                                            (* (max 0 (dec channels)) channel-stride)
                                            (* frame-capacity element-bytes))
                                         (+ input-offset
                                            (* (max 0 (dec channels)) channel-stride)
                                            (* frame-capacity element-bytes)))]
                 (set! (.-channels this) channels)
                 (set! (.-element-bytes this) element-bytes)
                 (set! (.-element-type this) element-type)
                 (set! (.-input-offset this) input-offset)
                 (set! (.-output-offset this) output-offset)
                 (set! (.-channel-stride this) channel-stride)
                 (set! (.-frame-capacity this) frame-capacity)
                 (set! (.-required-bytes this) required-bytes)
                 (set! (.-valid-config this)
                       (and (pos? channels)
                            (pos? element-bytes)
                            (pos? channel-stride)
                            (pos? frame-capacity)
                            aligned?
                            wasm-bytes))
                 (set! (.-control-names this) control-names)
                 (set! (.-control-defaults this) control-defaults)
                 ;; Reuse this array for the positional Wasm ABI.
                 (set! (.-process-args this)
                       (js/Array. (+ 4 (.-length control-names))))
                 (-> (js/WebAssembly.instantiate wasm-bytes)
                     (.then
                      (fn [result]
                        (let [instance (.-instance result)
                              exports (.-exports instance)
                              memory (aget exports "memory")
                              process-fn (aget exports "process")
                              init-fn (aget exports "init")
                              reset-fn (aget exports "reset")]
                          (when (and memory process-fn init-fn reset-fn
                                     (.-valid-config this)
                                     (>= (.-byteLength (.-buffer memory))
                                         (.-required-bytes this)))
                            (set! (.-instance this) instance)
                            (set! (.-memory this) memory)
                            (set! (.-memory-view this)
                                  (if (= "f64" element-type)
                                    (js/Float64Array. (.-buffer memory))
                                    (js/Float32Array. (.-buffer memory))))
                            (set! (.-memory-buffer this) (.-buffer memory))
                            (set! (.-process-fn this) process-fn)
                            (set! (.-init-fn this) init-fn)
                            (set! (.-reset-fn this) reset-fn)
                            (.call init-fn nil)
                            (set! (.-ready this) true)
                            (.postMessage (.-port this) #js {:type "ready"}))
                          (when-not (and memory process-fn init-fn reset-fn
                                         (.-valid-config this)
                                         (>= (.-byteLength (.-buffer memory))
                                             (.-required-bytes this)))
                            (.postMessage
                             (.-port this)
                             #js {:type "error"
                                  :message "invalid Wasm memory or ABI metadata"})))))
                     (.catch
                      (fn [error]
                        (.postMessage
                         (.-port this)
                         #js {:type "error"
                              :message (.-message error)}))))))
  Object
  (^:static ^:get parameterDescriptors [this]
                                       (js* "__OMKAMRA_DSP_PARAMETER_DESCRIPTORS__"))
  (process [this inputs outputs parameters]
           (let [input-channels (aget inputs 0)
                 output-channels (aget outputs 0)]
             (if-not output-channels
               true
               (let [channels (min (.-channels this)
                                   (.-length output-channels))
                     input-count (when input-channels
                                   (.-length input-channels))
                     output-count (.-length output-channels)
                     frames (if (pos? output-count)
                              (.-length (aget output-channels 0))
                              0)]
                 (if (or (not (.-ready this))
                         (not (.-valid-config this))
                         (not input-channels)
                         (zero? channels)
                         (zero? input-count)
                         (> frames (.-frame-capacity this))
                         (> (max (+ (.-output-offset this)
                                    (* (max 0 (dec (.-channels this)))
                                       (.-channel-stride this))
                                    (* frames (.-element-bytes this)))
                                 (+ (.-input-offset this)
                                    (* (max 0 (dec (.-channels this)))
                                       (.-channel-stride this))
                                    (* frames (.-element-bytes this))))
                            (.-byteLength (.-buffer (.-memory this)))))
                   (do
                     (dotimes [channel output-count]
                       (.fill (aget output-channels channel) 0))
                     true)
                   (let [memory (.-memory this)
                         memory-buffer (.-buffer memory)]
                     (when (not= memory-buffer (.-memory-buffer this))
                       (set! (.-memory-view this)
                             (if (= "f64" (.-element-type this))
                               (js/Float64Array. memory-buffer)
                               (js/Float32Array. memory-buffer)))
                       (set! (.-memory-buffer this) memory-buffer))
                     (let [memory-view (.-memory-view this)
                           process-args (.-process-args this)
                           element-width (.-element-bytes this)
                           input-index (/ (.-input-offset this) element-width)
                           output-index (/ (.-output-offset this) element-width)
                           stride-index (/ (.-channel-stride this) element-width)
                           control-names (.-control-names this)
                           control-defaults (.-control-defaults this)]
                       (dotimes [control (.-length control-names)]
                         (let [name (aget control-names control)
                               parameter (aget parameters name)
                               value (if (and parameter (pos? (.-length parameter)))
                                       (aget parameter 0)
                                       (aget control-defaults control))]
                           (aset process-args (+ 4 control) value)))
                       (dotimes [channel channels]
                         (let [input-channel (min channel (dec input-count))
                               input (aget input-channels input-channel)
                               output (aget output-channels channel)
                               input-base (+ input-index (* channel stride-index))
                               output-base (+ output-index (* channel stride-index))
                               input-offset (+ (.-input-offset this)
                                               (* channel (.-channel-stride this)))
                               output-offset (+ (.-output-offset this)
                                                (* channel (.-channel-stride this)))]
                           (dotimes [frame frames]
                             (aset memory-view (+ input-base frame)
                                   (aget input frame)))
                           (if (= 1 (.-length control-names))
                             (.call (.-process-fn this)
                                    nil input-offset output-offset frames sampleRate
                                    (aget process-args 4))
                             (do
                               (aset process-args 0 input-offset)
                               (aset process-args 1 output-offset)
                               (aset process-args 2 frames)
                               (aset process-args 3 sampleRate)
                               (.apply (.-process-fn this) nil process-args)))
                           (dotimes [frame frames]
                             (aset output frame
                                   (aget memory-view (+ output-base frame))))))
                       (dotimes [channel (max 0 (- output-count channels))]
                         (.fill (aget output-channels (+ channels channel)) 0))
                       true))))))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(registerProcessor "__OMKAMRA_DSP_PROCESSOR_NAME__" DSPWasmProcessor)
