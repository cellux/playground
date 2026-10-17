(ns omkamra.dsp.hosts.wasm
  "The Wasm host: a `Kernel` over a Wasm module (P10d).

  A Wasm kernel does not receive the block it renders; it writes into its own
  linear memory. `process` takes the byte offset of the output region and writes
  channel `c` at `offset + c * channel-stride`, so the voice's scratch is a set of
  typed-array **views** over that region, made once per voice. The block a
  callback is handed and the block the kernel wrote are then the same samples, and
  mixing is arithmetic rather than a copy — which is the one place the Wasm host
  differs from `omkamra.dsp.hosts.js`, where the kernel is handed the arrays.

  A Wasm instance owns its memory, so a voice is an instance: `create-voice`
  instantiates the module, which is why the kernel takes a `WebAssembly.Module`
  rather than bytes."
  (:require [goog.object :as gobj]
            [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth.pool :as pool]))

(defn- real-view
  "A typed-array view of a module's memory at `byte-offset`."
  [memory f64? byte-offset length]
  (if f64?
    (js/Float64Array. (.-buffer memory) byte-offset length)
    (js/Float32Array. (.-buffer memory) byte-offset length)))

(defn- output-views
  "The channels a kernel writes, as views over its output region: one array for
  a mono kernel, the array of channel views otherwise — the planar shape the pool
  mixes."
  [memory f64? channels output-offset channel-stride frame-capacity]
  (if (= 1 channels)
    (real-view memory f64? output-offset frame-capacity)
    (let [block (js/Array. channels)]
      (dotimes [channel channels]
        (aset block channel
              (real-view memory f64? (+ output-offset (* channel channel-stride))
                         frame-capacity)))
      block)))

(defn- special-view
  "A view over one declared special's slot in the module's state region.

  The region places element `index` at `offset + index * element-bytes`, at both
  widths, so the ABI's logical index is all a host needs (D39)."
  [memory f64? state-offset element-bytes slot]
  (let [byte-offset (+ state-offset (* (:index slot) element-bytes))]
    (case (:type slot)
      (:int :boolean) (js/Int32Array. (.-buffer memory) byte-offset 1)
      (:float (if f64?
                (js/Float64Array. (.-buffer memory) byte-offset 1)
                (js/Float32Array. (.-buffer memory) byte-offset 1))))))

(defn wasm-kernel
  "A `Kernel` over a Wasm kernel module.

  `module` is a compiled `WebAssembly.Module`, `abi` the artifact's ABI, and
  `memory-layout` its `:memory` region map — the offsets, stride, element width,
  and frame capacity the module was emitted against.

  `containment` selects the non-finite policy, as it does for every kernel."
  ([module abi memory-layout] (wasm-kernel module abi memory-layout {}))
  ([module abi memory-layout {:keys [precision containment]
                              :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)
         f64? (= :f64 precision)
         channels (long (get-in abi [:channels :count] 1))
         control-count (clojure.core/count (:controls abi))
         element-bytes (long (:element-bytes memory-layout))
         frame-capacity (long (:frame-capacity memory-layout))
         output-offset (long (:output-offset memory-layout 0))
         channel-stride (long (:channel-stride memory-layout))
         state-offset (long (get-in memory-layout [:regions :state :offset] 0))
         specials (mixing/special-order (:special-slots abi))
         special-count (clojure.core/count specials)]
     (reify pool/Kernel
       (create-voice [_ _max-block-frames]
         ;; Each voice is its own instance, so each has its own memory, state,
         ;; and output region. The module is fixed and small, so instantiation
         ;; is a construction cost rather than an audio-path one.
         (let [instance (js/WebAssembly.Instance. module)
               exports (.-exports instance)
               memory (gobj/get exports "memory")
               reset (gobj/get exports "reset")]
           {:instance instance
            :memory memory
            :process (gobj/get exports "process")
            ;; The generated ABI is output, frames, sample-rate, then the
            ;; positional controls — a voice is a generator, so there is no
            ;; input offset to pass (D23).
            :arguments (js/Array. (+ 3 control-count))
            :scratch (output-views memory f64? channels output-offset
                                   channel-stride frame-capacity)
            :specials (mapv (clojure.core/fn [slot]
                              (special-view memory f64? state-offset
                                            element-bytes slot))
                            specials)
            :reset! (clojure.core/fn [] (reset))}))
       (destroy-voice! [_ _voice] nil)
       (reset-voice! [_ voice]
         ((:reset! voice)))
       (write-voice-controls! [_ voice values]
         (let [arguments (:arguments voice)]
           (dotimes [position control-count]
             (aset arguments (+ 3 position) (aget values position)))))
       (read-specials! [_ voice target]
         (let [views (:specials voice)]
           (dotimes [position special-count]
             (aset target position (js/Number (aget (nth views position) 0))))))
       (clear-output! [_ output frames]
         (mixing/clear-channels! f64? channels output frames))
       (mix-voice! [_ voice output frames sample-rate]
         (let [arguments (:arguments voice)]
           (aset arguments 0 output-offset)
           (aset arguments 1 frames)
           (aset arguments 2 sample-rate)
           (.apply (:process voice) nil arguments))
         (mixing/mix-block! containment f64? channels voice output frames))))))
