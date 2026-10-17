(ns omkamra.dsp.worklet-adapters.wasm
  (:require [squint.defclass :refer [defclass]]))

;; A Wasm kernel is the one host that does not receive its block: `process` takes
;; the byte offsets of its input and output regions, and writes channel `c` at
;; `offset + c * channel-stride` in its own linear memory. So the adapter calls
;; the kernel **once** for the whole block (D40 — the kernel indexes its own
;; channels), then copies each channel out of memory into the array the callback
;; hands over. That copy is the whole of the boundary here.
;;
;; The kernel's shape comes from processor options: `channels` is the width it
;; writes, and `inputChannels` is 0 for a generator, which is what a voice is
;; (D23), so the process signature is `(output, frames, sampleRate, ...controls)`
;; rather than `(input, output, frames, sampleRate, ...controls)`.
;;
;; Element indices rather than byte offsets index the memory view (the module's
;; ABI is validated to be aligned), and the views are kept, so a block allocates
;; nothing.

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
                     input-channels (or (aget processor-options "inputChannels") 1)
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
                                            (* frame-capacity element-bytes)))
                     control-offset (+ 3 (if (pos? input-channels) 1 0))]
                 (set! (.-channels this) channels)
                 (set! (.-input-channels this) input-channels)
                 (set! (.-element-bytes this) element-bytes)
                 (set! (.-element-type this) element-type)
                 (set! (.-input-offset this) input-offset)
                 (set! (.-output-offset this) output-offset)
                 (set! (.-channel-stride this) channel-stride)
                 (set! (.-frame-capacity this) frame-capacity)
                 (set! (.-required-bytes this) required-bytes)
                 (set! (.-valid-config this)
                       (and (pos? channels)
                            (not (neg? input-channels))
                            (pos? element-bytes)
                            (pos? channel-stride)
                            (pos? frame-capacity)
                            aligned?
                            wasm-bytes))
                 (set! (.-control-names this) control-names)
                 (set! (.-control-defaults this) control-defaults)
                 (set! (.-control-offset this) control-offset)
                 ;; Reuse this array for the positional Wasm ABI.
                 (set! (.-process-args this)
                       (js/Array. (+ control-offset (.-length control-names))))
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
               (let [output-count (.-length output-channels)
                     frames (if (pos? output-count)
                              (.-length (aget output-channels 0))
                              0)
                     input-wanted (.-input-channels this)
                     input-count (if input-channels (.-length input-channels) 0)
                     ;; A kernel writes its own width, and never more channels
                     ;; than the callback has.
                     channels (min (.-channels this) output-count)]
                 (if (or (not (.-ready this))
                         (not (.-valid-config this))
                         (zero? channels)
                         (and (pos? input-wanted) (zero? input-count))
                         (> frames (.-frame-capacity this)))
                   (do
                     (dotimes [channel output-count]
                       (.fill (aget output-channels channel) 0))
                     true)
                   (let [memory (.-memory this)
                         memory-buffer (.-buffer memory)]
                     ;; A growing module replaces its buffer; re-view it when
                     ;; that happens rather than per block.
                     (when (not= memory-buffer (.-memory-buffer this))
                       (set! (.-memory-view this)
                             (if (= "f64" (.-element-type this))
                               (js/Float64Array. memory-buffer)
                               (js/Float32Array. memory-buffer)))
                       (set! (.-memory-buffer this) memory-buffer))
                     (let [view (.-memory-view this)
                           width (.-element-bytes this)
                           input-index (/ (.-input-offset this) width)
                           output-index (/ (.-output-offset this) width)
                           stride-index (/ (.-channel-stride this) width)
                           process-args (.-process-args this)
                           control-names (.-control-names this)
                           control-defaults (.-control-defaults this)
                           control-offset (.-control-offset this)]
                       (dotimes [control (.-length control-names)]
                         (let [name (aget control-names control)
                               parameter (aget parameters name)
                               value (if (and parameter (pos? (.-length parameter)))
                                       (aget parameter 0)
                                       (aget control-defaults control))]
                           (aset process-args (+ control-offset control) value)))
                       ;; Write the input the kernel reads, up-mixing by
                       ;; repeating the last supplied channel when the kernel
                       ;; declares more than the graph connected.
                       (when (pos? input-wanted)
                         (dotimes [channel input-wanted]
                           (let [source (aget input-channels
                                              (min channel (dec input-count)))]
                             (dotimes [frame frames]
                               (aset view (+ input-index (* channel stride-index) frame)
                                     (aget source frame))))))
                       ;; One call for the whole block (D40): the kernel writes
                       ;; every channel itself.
                       (if (pos? input-wanted)
                         (do
                           (aset process-args 0 (.-input-offset this))
                           (aset process-args 1 (.-output-offset this))
                           (aset process-args 2 frames)
                           (aset process-args 3 sampleRate))
                         (do
                           (aset process-args 0 (.-output-offset this))
                           (aset process-args 1 frames)
                           (aset process-args 2 sampleRate)))
                       (.apply (.-process-fn this) nil process-args)
                       ;; Copy each channel the kernel wrote back out, and
                       ;; silence the ones it did not.
                       (dotimes [channel channels]
                         (let [target (aget output-channels channel)
                               base (+ output-index (* channel stride-index))]
                           (dotimes [frame frames]
                             (aset target frame (aget view (+ base frame))))))
                       (dotimes [channel (max 0 (- output-count channels))]
                         (.fill (aget output-channels (+ channels channel)) 0))
                       true))))))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(registerProcessor "__OMKAMRA_DSP_PROCESSOR_NAME__" DSPWasmProcessor)
