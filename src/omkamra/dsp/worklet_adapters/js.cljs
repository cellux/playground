#_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
(ns omkamra.dsp.worklet-adapters.js
  (:require [squint.defclass :refer [defclass]]
            ["__OMKAMRA_DSP_KERNEL_URL__" :refer [createKernel]]))

;; The adapter owns the boundary between an audio callback and a generated
;; kernel, and the two disagree about blocks in exactly one way: a callback hands
;; over an array of channel arrays (`outputs[0]`), while a kernel indexes the
;; channels it was compiled for (`output[1][frame]`, or `output[frame]` for a
;; mono kernel). So the adapter passes the callback's own arrays straight
;; through — one channel array for a mono kernel, all of them otherwise — and
;; the pool's planar block and the callback's block are the same thing.
;;
;; The kernel's shape comes from its artifact ABI, carried in processor options:
;; `channels` is the width it writes, and `inputChannels` is 0 for a generator
;; (which is what a voice is, D23), so the generated argument list is either
;; `(output, frames, sampleRate, ...controls)` or
;; `(input, output, frames, sampleRate, ...controls)`.
;;
;; One call per block, not one per channel: the kernel evaluates a frame once and
;; writes every channel itself (D40). Channels the kernel does not write are
;; silenced, and nothing is up-mixed — placing a mono voice in a stereo field is
;; the voice's own business (`omkamra.dsp.library.effect/pan`).

(defclass DSPKernelProcessor
  (extends AudioWorkletProcessor)
  (constructor [this options]
               (super)
               (let [processor-options (.-processorOptions options)
                     control-names (or (aget processor-options "controlNames")
                                       #js ["coefficient"])
                     control-defaults (or (aget processor-options "controlDefaults")
                                          #js [0.5])
                     input-channels (or (aget processor-options "inputChannels") 1)
                     channels (or (aget processor-options "channels") 1)
                     ;; The generated ABI is (input?), output, frames,
                     ;; sample-rate, then the positional controls.
                     control-offset (+ 3 (if (pos? input-channels) 1 0))]
                 (set! (.-input-channels this) input-channels)
                 (set! (.-channels this) channels)
                 (set! (.-control-offset this) control-offset)
                 (set! (.-control-names this) control-names)
                 (set! (.-control-defaults this) control-defaults)
                 ;; Reuse this argument array on every process call, so a block
                 ;; allocates nothing.
                 (set! (.-process-args this)
                       (js/Array. (+ control-offset (.-length control-names)))))
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
               (let [output-count (.-length output-channels)
                     frames (if (pos? output-count)
                              (.-length (aget output-channels 0))
                              0)
                     input-channels-wanted (.-input-channels this)
                     ;; A kernel writes its own width, and never more channels
                     ;; than the callback has.
                     channels (min (.-channels this) output-count)]
                 (if (or (not (.-ready this))
                         (zero? channels)
                         (and (pos? input-channels-wanted)
                              (not input-channels)))
                   (do
                     (dotimes [channel output-count]
                       (.fill (aget output-channels channel) 0))
                     true)
                   (let [process-args (.-process-args this)
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
                     (let [output (if (= 1 channels)
                                    (aget output-channels 0)
                                    output-channels)]
                       (if (pos? input-channels-wanted)
                         (do
                           (aset process-args 0
                                 (if (= 1 input-channels-wanted)
                                   (aget input-channels 0)
                                   input-channels))
                           (aset process-args 1 output)
                           (aset process-args 2 frames)
                           (aset process-args 3 sampleRate))
                         (do
                           (aset process-args 0 output)
                           (aset process-args 1 frames)
                           (aset process-args 2 sampleRate))))
                     (.apply (.-process (.-kernel this)) (.-kernel this)
                             process-args)
                     (dotimes [channel (max 0 (- output-count channels))]
                       (.fill (aget output-channels (+ channels channel)) 0))
                     true)))))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(registerProcessor "__OMKAMRA_DSP_PROCESSOR_NAME__" DSPKernelProcessor)
