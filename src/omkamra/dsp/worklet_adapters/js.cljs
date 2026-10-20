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
;;
;; **Timestamped events (M1).** When the page connects a `MessagePort` and posts
;; `{frame, name, value}` events, the adapter buffers them in a preallocated
;; typed array — the ingress queue — and, for a mono kernel, renders each block
;; in sub-blocks split at the event frames, applying each control value at its
;; exact frame. With no events the block is rendered in one kernel call exactly
;; as before, so the event path is inert when unused. Stereo event dispatch is
;; left for the planar-worklet slice.

(defn- enqueue-event!
  "Buffer one `{frame, name, value}` event, if it names a known control and the
  preallocated buffer has room."
  [this data]
  (let [count (.-event-count this)
        control (aget (.-control-index this) (.-name data))]
    (when (and (number? control) (< count (.-event-capacity this)))
      (aset (.-event-frames this) count (.-frame data))
      (aset (.-event-controls this) count control)
      (aset (.-event-values this) count (.-value data))
      (set! (.-event-count this) (inc count)))))

(defn handle-event!
  "Handle a message on the connected event port.

  Public because the class body is opaque to clj-kondo; it is called only from
  the processor's constructor."
  [this data]
  (if (= "events-done" (.-type data))
    (do (set! (.-events-ready this) true)
        (.postMessage (.-port this) #js {:type "events-ready"}))
    (enqueue-event! this data)))

(defclass DSPKernelProcessor
  (extends AudioWorkletProcessor)
  (constructor [this options]
               (super)
               ;; A constructor that throws is otherwise invisible: the worklet
               ;; reports a bare `processorerror` and the page only sees a node
               ;; that never answers. Posting it means `worklet-ready` rejects
               ;; with the reason instead of timing out (F5).
               (try
                 ;; A node created without processor options must fall back to the
                 ;; adapter's defaults rather than throw where nothing reports it.
                 (let [processor-options (or (.-processorOptions options) #js {})
                       control-names (or (aget processor-options "controlNames")
                                         #js ["coefficient"])
                       control-defaults (or (aget processor-options "controlDefaults")
                                            #js [0.5])
                       input-channels (or (aget processor-options "inputChannels") 1)
                       channels (or (aget processor-options "channels") 1)
                       event-capacity (or (aget processor-options "eventCapacity") 1024)
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
                         (js/Array. (+ control-offset (.-length control-names))))
                   ;; The event ingress queue: preallocated, so receiving an event
                   ;; never grows it.
                   (set! (.-event-capacity this) event-capacity)
                   (set! (.-event-frames this) (js/Float64Array. event-capacity))
                   (set! (.-event-controls this) (js/Int32Array. event-capacity))
                   (set! (.-event-values this) (js/Float64Array. event-capacity))
                   (set! (.-event-count this) 0)
                   (set! (.-event-pointer this) 0)
                   (set! (.-events-ready this) false)
                   ;; A control an event has set stays set across blocks; AudioParams
                   ;; must not clobber it on the next block's read.
                   (set! (.-event-set this) (js/Int8Array. (.-length control-names)))
                   (set! (.-control-index this)
                         (let [index #js {}]
                           (dotimes [control (.-length control-names)]
                             (aset index (aget control-names control) control))
                           index)))
                 (set! (.-kernel this) (createKernel))
                 (set! (.-ready this) true)
                 (set! (.-onmessage (.-port this))
                       (fn [event]
                         (let [data (.-data event)]
                           (cond
                             (= "status" (.-type data))
                             (.postMessage (.-port this) #js {:type "ready"})

                             ;; The page transfers one end of a MessageChannel here;
                             ;; the worker holds the other.
                             (= "connect" (.-type data))
                             (let [port (aget (.-ports event) 0)]
                               (set! (.-onmessage port)
                                     (fn [message]
                                       (handle-event! this (.-data message))))
                               (set! (.-event-port this) port))

                             :else
                             (handle-event! this data)))))
                 (catch :default e
                   (.postMessage (.-port this)
                                 #js {:type "error"
                                      :message (str "processor options: "
                                                    (.-message e))}))))
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
                     (let [event-set (.-event-set this)]
                       (dotimes [control (.-length control-names)]
                         (when (zero? (aget event-set control))
                           (let [name (aget control-names control)
                                 parameter (aget parameters name)
                                 value (if (and parameter (pos? (.-length parameter)))
                                         (aget parameter 0)
                                         (aget control-defaults control))]
                             (aset process-args (+ control-offset control) value)))))
                     (if (and (= 1 channels)
                              (.-events-ready this)
                              (pos? (.-event-count this)))
                       ;; Split the block at the buffered event frames and apply
                       ;; each control value at its exact frame.
                       (let [base currentFrame
                             out (aget output-channels 0)
                             end (+ base frames)
                             event-frames (.-event-frames this)
                             event-controls (.-event-controls this)
                             event-values (.-event-values this)
                             count (.-event-count this)
                             kernel (.-kernel this)]
                         (loop [frame base
                                pointer (.-event-pointer this)]
                           (if (>= frame end)
                             (do (set! (.-event-pointer this) pointer)
                                 (when (>= pointer count)
                                   (set! (.-event-count this) 0)
                                   (set! (.-event-pointer this) 0)))
                             (let [pointer (loop [p pointer]
                                             (if (and (< p count)
                                                      (<= (aget event-frames p) frame))
                                               (let [control (aget event-controls p)]
                                                 (aset process-args
                                                       (+ control-offset control)
                                                       (aget event-values p))
                                                 (aset (.-event-set this) control 1)
                                                 (recur (inc p)))
                                               p))
                                   next (if (and (< pointer count)
                                                 (< (aget event-frames pointer) end))
                                          (aget event-frames pointer)
                                          end)]
                               (aset process-args 0
                                     (.subarray out (- frame base) (- next base)))
                               (aset process-args 1 (- next frame))
                               (aset process-args 2 sampleRate)
                               (.apply (.-process kernel) kernel process-args)
                               (recur next pointer)))))
                       ;; No events: one kernel call for the whole block.
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
                             (aset process-args 2 sampleRate)))
                         (.apply (.-process (.-kernel this)) (.-kernel this)
                                 process-args)))
                     (dotimes [channel (max 0 (- output-count channels))]
                       (.fill (aget output-channels (+ channels channel)) 0))
                     true)))))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(registerProcessor "__OMKAMRA_DSP_PROCESSOR_NAME__" DSPKernelProcessor)
