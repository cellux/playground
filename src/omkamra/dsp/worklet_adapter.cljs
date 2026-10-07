#_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
(ns omkamra.dsp.worklet-adapter
  (:require [squint.defclass :refer [defclass]]
            ["__OMKAMRA_DSP_KERNEL_URL__" :refer [createKernel]]))

(defclass DSPKernelProcessor
  (extends AudioWorkletProcessor)
  (constructor [this options]
               (super)
               (let [processor-options (.-processorOptions options)
                     control-names (or (aget processor-options "controlNames")
                                       #js ["coefficient"])
                     control-defaults (or (aget processor-options "controlDefaults")
                                          #js [0.5])]
                 (set! (.-channels this)
                       (or (aget processor-options "channels") 1))
                 (set! (.-control-names this) control-names)
                 (set! (.-control-defaults this) control-defaults)
                 ;; Reuse this argument array on every process call. The
                 ;; generated kernel ABI is input, output, frames, sample-rate,
                 ;; followed by positional controls.
                 (set! (.-process-args this)
                       (js/Array. (+ 4 (.-length control-names)))))
               (set! (.-kernel this) (createKernel))
               (set! (.-ready this) true)
               (set! (.-onmessage (.-port this))
                     (fn [event]
                       (when (= "status" (aget (.-data event) "type"))
                         (.postMessage (.-port this) #js {:type "ready"})))))
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
                     input-count (when input-channels (.-length input-channels))
                     output-count (.-length output-channels)
                     frames (if (pos? output-count)
                              (.-length (aget output-channels 0))
                              0)]
                 (if (or (not (.-ready this))
                         (not input-channels)
                         (zero? channels)
                         (zero? input-count))
                   (do
                     (dotimes [channel output-count]
                       (.fill (aget output-channels channel) 0))
                     true)
                   (let [process-args (.-process-args this)
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
                       (let [input-channel (min channel (dec input-count))]
                         (aset process-args 0 (aget input-channels input-channel))
                         (aset process-args 1 (aget output-channels channel))
                         (aset process-args 2 frames)
                         (aset process-args 3 sampleRate)
                         (.apply (.-process (.-kernel this))
                                 (.-kernel this)
                                 process-args)))
                     (dotimes [channel (max 0 (- output-count channels))]
                       (.fill (aget output-channels (+ channels channel)) 0))
                     true)))))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(registerProcessor "__OMKAMRA_DSP_PROCESSOR_NAME__" DSPKernelProcessor)
