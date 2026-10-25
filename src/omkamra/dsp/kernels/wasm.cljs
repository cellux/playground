(ns omkamra.dsp.kernels.wasm
  "The Wasm target's kernel: a `Kernel` over a Wasm module (P10d).

  A Wasm kernel does not receive the block it renders; it writes into its own
  linear memory. `process` takes the byte offset of the output region and writes
  channel `c` at `offset + c * channel-stride`, so the voice's scratch is a set of
  typed-array **views** over that region, made once per voice. The block a
  callback is handed and the block the kernel wrote are then the same samples, and
  mixing is arithmetic rather than a copy — which is the one place the Wasm host
  differs from `omkamra.dsp.kernels.js`, where the kernel is handed the arrays.

  A Wasm instance owns its memory, so a voice is an instance: `create-voice`
  instantiates the module, which is why the kernel takes a `WebAssembly.Module`
  rather than bytes."
  (:require [goog.object :as gobj]
            [omkamra.dsp.fusion :as fusion]
            [omkamra.dsp.kernels.mixing :as mixing]
            [omkamra.dsp.kernels.regions :as regions]
            [omkamra.dsp.numbers :as numbers]
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

(defn- check-frames!
  "Refuse a block longer than the module's own declared capacity.

  Linear memory is sized from the ABI's `:frame-capacity`, so a longer block would
  write past the output region into whatever follows it: the failure would surface
  as somebody else's samples rather than as an error. The capacity is declared, so
  the boundary can be checked instead of assumed (P12)."
  [frames frame-capacity]
  (when (> (long frames) (long frame-capacity))
    (throw (ex-info (str "a Wasm instance was asked to render more frames than the"
                         " capacity its ABI declares")
                    {:frames (long frames)
                     :frame-capacity (long frame-capacity)}))))

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

  `containment` selects the non-finite policy, as it does for every kernel.

  `:mapping` makes it a *fused* kernel (M6): an `omkamra.dsp.fusion/
  fused-mapping` from the base voice's controls to the fused interface's. `slots`
  handles then share one module instance -- one linear memory, one state region --
  the pool writes each handle's controls through the mapping, and a reset
  restores only that slot's call partition, which `:call-partitions` on the ABI
  records (the contract seam 4 published)."
  ([module abi memory-layout] (wasm-kernel module abi memory-layout {}))
  ([module abi memory-layout {:keys [precision containment mapping]
                              :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)
         f64? (= :f64 precision)
         channels (long (get-in abi [:channels :count] 1))
         control-count (clojure.core/count (:controls abi))
         ;; A fused instance holds `slots` voices, one per call site; a plain
         ;; kernel holds one (M6).
         slots (long (or (:slots mapping) 1))
         _ (when (and mapping
                      (not= slots (clojure.core/count (:call-partitions abi))))
             (throw (ex-info (str "a fused voice's slot count must be the number"
                                  " of call sites the process has, or a slot cannot"
                                  " be reset on its own")
                             {:slots slots
                              :partitions (clojure.core/count (:call-partitions abi))})))
         element-bytes (long (:element-bytes memory-layout))
         frame-capacity (long (:frame-capacity memory-layout))
         output-offset (long (:output-offset memory-layout 0))
         channel-stride (long (:channel-stride memory-layout))
         state-offset (long (get-in memory-layout [:regions :state :offset] 0))
         state-bytes (long (get-in memory-layout [:regions :state :bytes] 0))
         state-layout (:state-layout memory-layout)
         specials (mixing/special-order (:special-slots abi))
         ;; A fused slot publishes the voice's specials from its own call
         ;; partition (M6 seam 4), in slot order, ascending bit; `create-voice`
         ;; turns each into a view into the instance's memory.
         partition-specials (when mapping
                              (mapv (fn [partition]
                                      (vec (sort-by :bit (:specials partition))))
                                    (:call-partitions abi)))
         ;; The byte range of each slot's call partition in the state region
         ;; (M6a, seam 3): a Wasm instance owns its memory, so a per-slot reset
         ;; copies this range back from the instance's initial state, exactly as
         ;; the JVM host does (D51). The layout gives each slot's byte offset,
         ;; because an int slot in an f64 block is padded to the block's
         ;; alignment. A partition is resolved from the layout *segments* that
         ;; overlap it, not from a lookup per arena index: a per-call-site region
         ;; is one entry spanning thousands of indices, and the indices inside it
         ;; have no entry of their own.
         ;; A call site with no state owns an *empty* partition (D70), so a range
         ;; with nothing in it is not an error: it is a slot whose reset does
         ;; nothing, because there is nothing of its own to restore.
         partition-ranges (when mapping
                            (mapv (fn [{:keys [base size]}]
                                    (let [base (long base)
                                          end (+ base (long size))
                                          entries (filter (clojure.core/fn [[index entry]]
                                                            (let [index (long index)
                                                                  span (long (or (:size entry) 1))]
                                                              (and (< index end)
                                                                   (< base (+ index span)))))
                                                          state-layout)]
                                      (if (empty? entries)
                                        {:start 0 :end 0}
                                        {:start (reduce min (map (comp long :offset val) entries))
                                         :end (reduce max
                                                      (map (clojure.core/fn [[_ {:keys [offset size
                                                                                        element-bytes]}]]
                                                             (+ (long offset)
                                                                (* (long (or size 1))
                                                                   (long element-bytes))))
                                                           entries))})))
                                  (:call-partitions abi)))
         ;; The base -> fused control table, one row per slot (M6). A plain kernel
         ;; has one identity row, so the write path is the same loop either way.
         base-count (if mapping (clojure.core/count (:hoisted mapping)) control-count)
         slot-rows (if mapping
                     (mapv (fn [slot]
                             (mapv (fn [position]
                                     (long (fusion/fused-position mapping slot position)))
                                   (range base-count)))
                           (range slots))
                     [(vec (range control-count))])
         ;; A Wasm instance owns its memory, so the lane region is already per
         ;; voice: `mix-voice!` used to copy the pool's shared region into each
         ;; instance, and now the voice's own region *is* that view, written
         ;; directly (M4c).
         lane-stride (long (or (:lane-stride abi) 0))
         lane-layout (into {}
                           (map (clojure.core/fn [lane]
                                  [(clojure.core/name (:name lane))
                                   (long (:offset lane))]))
                           (:lanes abi))
         lane-count (clojure.core/count lane-layout)
         ;; A fused instance renders every slot in one call, so its one region
         ;; holds a slice per slot and the voice's slot says which (F4).
         lane-slots (long (or (:lane-slots abi) 1))
         lanes-offset (long (get-in memory-layout [:regions :lanes :offset] 0))
         ;; The shared region is the *pool's*: one host-side master, copied into
         ;; each instance's own region before it runs, because a Wasm instance
         ;; owns its memory (M5). It has the lane region's shape.
         shared-stride (long (or (:shared-stride abi) 0))
         shared-layout (into {}
                             (map (clojure.core/fn [control]
                                    [(clojure.core/name (:name control))
                                     (long (:offset control))]))
                             (:shared abi))
         shared-count (clojure.core/count shared-layout)
         shared-offset (long (get-in memory-layout [:regions :shared :offset] 0))
         shared-region (when (pos? shared-count)
                         (numbers/real-array f64? (* shared-stride shared-count)))
         lane-slot (when (pos? lane-count) (+ 3 control-count))
         shared-slot (when (pos? shared-count)
                       (+ 3 control-count (if (pos? lane-count) 1 0)))
         ;; Every voice is its own instance, so the key is creation order (M6); a
         ;; fused instance makes `slots` handles share one, and `current` is that
         ;; instance while its handles are being created.
         created (volatile! 0)
         current (volatile! nil)]
     (reify pool/Kernel
       (create-voice [_ _max-block-frames]
         ;; Each voice is its own instance, so each has its own memory, state,
         ;; and output region. The module is fixed and small, so instantiation
         ;; is a construction cost rather than an audio-path one.
         (if (nil? mapping)
           (let [instance (js/WebAssembly.Instance. module)
                 exports (.-exports instance)
                 memory (gobj/get exports "memory")
                 reset (gobj/get exports "reset")
                 arguments (js/Array. (+ 3 control-count
                                         (if (pos? lane-count) 1 0)
                                         (if (pos? shared-count) 1 0)))]
             ;; A Wasm instance starts from zeroed memory, so the module's own
             ;; `reset` seeds the state from the IR's initial values -- the same
             ;; thing the JavaScript host gets from `create`, which writes the
             ;; state array. The pool resets a voice only when it *steals* one,
             ;; so a fresh voice would otherwise render from zero (D59).
             (when reset (reset))
             (when lane-slot
               ;; The generated ABI puts the regions last, by address: the module
               ;; reads its own linear memory, so the pointer is this instance's
               ;; region — which is what makes a Wasm lane per voice here (M4c).
               (aset arguments lane-slot lanes-offset))
             (when shared-slot
               (aset arguments shared-slot shared-offset))
             {:instance instance
              :instance-key (vswap! created inc)
              :memory memory
              :process (gobj/get exports "process")
              ;; A voice is a generator, so there is no input offset to pass
              ;; (D23); `process` takes the output address, frames, sample rate,
              ;; the positional controls, and the lane pointer above.
              :arguments arguments
              :lanes (when (pos? lane-count)
                       (real-view memory f64? lanes-offset
                                  (* lane-stride lane-count lane-slots)))
              :shared (when (pos? shared-count)
                        (real-view memory f64? shared-offset
                                   (* shared-stride shared-count)))
              :scratch (output-views memory f64? channels output-offset
                                     channel-stride frame-capacity)
              :specials (mapv (clojure.core/fn [slot]
                                (special-view memory f64? state-offset
                                              element-bytes slot))
                              specials)
              :reset! (clojure.core/fn [] (reset))})
           (let [slot (long (mod (long @created) slots))]
             (when (zero? slot)
               (let [instance (js/WebAssembly.Instance. module)
                     exports (.-exports instance)
                     ;; The module seeds every state slot from the IR's initial
                     ;; values in its own `reset`, which for a plain kernel runs
                     ;; at note-on. A fused slot instead restores its partition
                     ;; from the snapshot below, so the seed has to run first --
                     ;; otherwise the snapshot is the zeroed memory a Wasm module
                     ;; is instantiated with, and every slot starts from zero.
                     _ (when-let [seed (gobj/get exports "reset")]
                         (seed))
                     memory (gobj/get exports "memory")
                     arguments (js/Array. (+ 3 control-count
                                             (if (pos? lane-count) 1 0)
                                             (if (pos? shared-count) 1 0)))]
                 (when lane-slot (aset arguments lane-slot lanes-offset))
                 (when shared-slot (aset arguments shared-slot shared-offset))
                 (vreset! current
                          {:instance instance
                           :instance-key (inc (quot (long @created) slots))
                           :memory memory
                           :process (gobj/get exports "process")
                           :arguments arguments
                           :lanes (when (pos? lane-count)
                                    (real-view memory f64? lanes-offset
                                               (* lane-stride lane-count lane-slots)))
                           :shared (when (pos? shared-count)
                                     (real-view memory f64? shared-offset
                                                (* shared-stride shared-count)))
                           :scratch (output-views memory f64? channels output-offset
                                                  channel-stride frame-capacity)
                           ;; The state region's initial bytes, taken once, before
                           ;; any block runs: a per-slot reset restores one call
                           ;; partition from here (D51).
                           :initial (.slice (js/Uint8Array. (.-buffer memory)
                                                            state-offset
                                                            state-bytes))})))
             (vswap! created inc)
             (let [instance @current
                   {:keys [start end]} (nth partition-ranges slot)
                   memory (:memory instance)
                   initial (:initial instance)
                   offset (- (long start) state-offset)
                   length (- (long end) (long start))
                   view (js/Uint8Array. (.-buffer memory) (long start) length)]
               {:instance (:instance instance)
                :instance-key (:instance-key instance)
                :slot slot
                :memory memory
                :process (:process instance)
                :arguments (:arguments instance)
                :lanes (:lanes instance)
                :shared (:shared instance)
                :scratch (:scratch instance)
                :specials (mapv (clojure.core/fn [s]
                                  (special-view memory f64? state-offset
                                                element-bytes s))
                                (nth partition-specials slot))
                :reset! (clojure.core/fn []
                          (.set view (.subarray initial offset (+ offset length))))}))))
       (instance-key [_ voice] (:instance-key voice))
       (destroy-voice! [_ _voice] nil)
       (reset-voice! [_ voice]
         ((:reset! voice)))
       (write-voice-controls! [_ voice values]
         (let [row (nth slot-rows (long (or (:slot voice) 0)))
               arguments (:arguments voice)]
           (dotimes [position (clojure.core/count row)]
             (aset arguments (+ 3 (nth row position)) (aget values position)))))
       (write-shared! [_ control values]
         (let [key (if (string? control) control (clojure.core/name control))
               offset (get shared-layout key)]
           (when-not offset
             (throw (ex-info "this DSP voice declares no such shared control"
                             {:control control
                              :controls (vec (sort (keys shared-layout)))})))
           (let [length (min (numbers/value-length values) shared-stride)]
             (dotimes [index length]
               (aset shared-region (+ offset index)
                     (js/Number (numbers/value-at values index)))))))
       (write-voice-lane! [_ voice lane values]
         (let [key (if (string? lane) lane (clojure.core/name lane))
               lane-offset (get lane-layout key)]
           (when-not lane-offset
             (throw (ex-info "this DSP voice declares no such lane"
                             {:lane lane :lanes (vec (sort (keys lane-layout)))})))
           ;; A fused instance holds one slice per slot, so a voice's lane belongs
           ;; to the slice its slot *is* (F4); a plain voice has no slot.
           (let [offset (+ (long lane-offset)
                           (* (long (or (:slot voice) 0)) lane-stride lane-count))
                 length (min (numbers/value-length values) lane-stride)
                 ;; The voice's own region is a view into its linear memory, so
                 ;; this writes the values where the module already reads them.
                 target (:lanes voice)]
             (dotimes [index length]
               (aset target (+ offset index)
                     (js/Number (numbers/value-at values index)))))))
       (read-specials! [_ voice target]
         (let [views (:specials voice)]
           (dotimes [position (clojure.core/count views)]
             (aset target position (js/Number (aget (nth views position) 0))))))
       (clear-output! [_ output frames]
         (mixing/clear-channels! f64? channels output frames))
       (mix-voice! [_ voice output frames sample-rate]
         (check-frames! frames frame-capacity)
         (when (pos? shared-count)
           ;; The module reads its own memory, so hand it this voice's copy of the
           ;; pool's region. Only the frames about to be rendered can be read, so
           ;; only those are copied — one `frames`-long run per slice.
           (let [target (:shared voice)
                 master shared-region
                 frames (js/Math.min frames shared-stride)]
             (dotimes [slice shared-count]
               (let [base (* slice shared-stride)]
                 (dotimes [index frames]
                   (aset target (+ base index) (aget master (+ base index))))))))
         (let [arguments (:arguments voice)]
           (aset arguments 0 output-offset)
           (aset arguments 1 frames)
           (aset arguments 2 sample-rate)
           (.apply (:process voice) nil arguments))
         (mixing/mix-block! containment f64? channels voice output frames))))))

(defn- copy-region!
  "Copy the frames about to be rendered from a host region into an instance's.

  A Wasm instance owns its memory, so the instrument's lane and shared regions
  live outside it: the host writes the master once and this copies the run a
  block can read into the instance's own region, one run per slice — the same
  handoff the pool gives a voice's shared region (M5)."
  [target master stride slices frames]
  (when (and target master (pos? (long slices)))
    (let [frames (js/Math.min frames stride)]
      (dotimes [slice slices]
        (let [base (* slice stride)]
          (dotimes [index frames]
            (aset target (+ base index) (aget master (+ base index)))))))))

(defn wasm-bus-kernel
  "A post-mix bus over a Wasm module.

  A Wasm kernel writes into its own linear memory, so a bus copies the channel
  into the module's input region, calls `process(input-offset, output-offset,
  frames, sample-rate, controls…)`, then copies the output region back. The
  input and output views are made once per instance.

  The chain is instrument-level, so its regions are: one lane region and one
  shared region for the whole instrument, handed to every channel's instance.
  The host writes them with `:write-lane!` / `:write-shared!`, and each call
  copies the frames it can read into the instance's own memory, because a Wasm
  instance owns its memory (M5's handoff, one level up).

  `memory-layout` is the artifact's `:memory` map and `precision` its physical
  precision. Returns the same `{:create-bus, :run-bus!, :reset-bus!,
  :write-lane!, :write-shared!}` map as `omkamra.dsp.kernels.js/js-bus-kernel`."
  [module abi memory-layout precision]
  (let [f64? (= :f64 precision)
        frame-capacity (long (:frame-capacity memory-layout))
        input-offset (long (get-in memory-layout [:regions :input :offset] 0))
        output-offset (long (get-in memory-layout [:regions :output :offset] 0))
        control-count (clojure.core/count (:controls abi))
        lane-stride (long (or (:lane-stride abi) 0))
        lane-layout (regions/region-layout (:lanes abi))
        lane-count (clojure.core/count lane-layout)
        lanes-offset (long (get-in memory-layout [:regions :lanes :offset] 0))
        lane-region (when (pos? lane-count)
                      (numbers/real-array f64? (* lane-stride lane-count)))
        shared-stride (long (or (:shared-stride abi) 0))
        shared-layout (regions/region-layout (:shared abi))
        shared-count (clojure.core/count shared-layout)
        shared-offset (long (get-in memory-layout [:regions :shared :offset] 0))
        shared-region (when (pos? shared-count)
                        (numbers/real-array f64? (* shared-stride shared-count)))
        ;; Where each region sits in the generated argument list: the input, the
        ;; output, frames and the sample rate, then the controls, then the
        ;; regions — which for Wasm are this instance's own region offsets.
        lane-slot (when (pos? lane-count) (+ 4 control-count))
        shared-slot (when (pos? shared-count)
                      (+ 4 control-count (if (pos? lane-count) 1 0)))]
    {:create-bus
     (clojure.core/fn []
       (let [instance (js/WebAssembly.Instance. module)
             exports (.-exports instance)
             memory (gobj/get exports "memory")
             arguments (js/Array. (+ 4 control-count
                                     (if (pos? lane-count) 1 0)
                                     (if (pos? shared-count) 1 0)))]
         (when lane-slot (aset arguments lane-slot lanes-offset))
         (when shared-slot (aset arguments shared-slot shared-offset))
         {:instance instance
          :memory memory
          :process (gobj/get exports "process")
          :reset (gobj/get exports "reset")
          :input-view (real-view memory f64? input-offset frame-capacity)
          :output-view (real-view memory f64? output-offset frame-capacity)
          :lanes (when (pos? lane-count)
                   (real-view memory f64? lanes-offset (* lane-stride lane-count)))
          :shared (when (pos? shared-count)
                    (real-view memory f64? shared-offset (* shared-stride shared-count)))
          :arguments arguments}))
     :run-bus!
     (clojure.core/fn [bus input output frames sample-rate controls]
       (check-frames! frames frame-capacity)
       (copy-region! (:lanes bus) lane-region lane-stride lane-count frames)
       (copy-region! (:shared bus) shared-region shared-stride shared-count frames)
       (let [in-view (:input-view bus)
             out-view (:output-view bus)]
         (dotimes [index frames]
           (aset in-view index (aget input index)))
         (let [arguments (:arguments bus)]
           (aset arguments 0 input-offset)
           (aset arguments 1 output-offset)
           (aset arguments 2 frames)
           (aset arguments 3 sample-rate)
           (dotimes [position control-count]
             (aset arguments (+ 4 position) (aget controls position)))
           (.apply (:process bus) nil arguments))
         (dotimes [index frames]
           (aset output index (aget out-view index)))))
     :reset-bus!
     (clojure.core/fn [bus]
       (when-let [reset (:reset bus)] (.call reset)))
     :write-lane!
     (clojure.core/fn [lane values]
       (when lane-region
         (regions/write-region! lane-region
                                (regions/region-offset lane-layout lane :lane)
                                lane-stride values f64?))
       nil)
     :write-shared!
     (clojure.core/fn [control values]
       (when shared-region
         (regions/write-region! shared-region
                                (regions/region-offset shared-layout control :shared-control)
                                shared-stride values f64?))
       nil)}))
