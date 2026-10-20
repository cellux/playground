(ns omkamra.dsp.hosts.js
  "The JavaScript host: a `Kernel` over a generated JavaScript module (P10d).

  The module exports `createKernel`, which is called once per voice, so each
  voice is an independent kernel object with its own state. A pool voice is a
  generator (D23), so the generated `process` takes the output block and then the
  positional controls; one argument array is built per voice and refilled per
  block, so a render allocates nothing.

  The kernel object also exposes its state arena, which is what makes
  `read-specials!` a single array read rather than a second entry point (D39)."
  (:require [omkamra.dsp.hosts.fusion :as fusion]
            [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth.pool :as pool]))

(defn js-kernel
  "A `Kernel` over a generated JavaScript artifact.

  `create` is the module's exported `createKernel`, and `abi` is that artifact's
  ABI; `:precision` decides the element type of the mixing scratch.

  `containment` selects the non-finite policy, as it does for every kernel.

  `:mapping` makes it a *fused* kernel (M6): an `omkamra.dsp.hosts.fusion/
  fused-mapping` from the base voice's controls to the fused interface's. `slots`
  handles then share one generated object -- one instance, one state arena -- the
  pool writes each handle's controls through the mapping, and a reset restores
  only that slot's call partition, which `:call-partitions` on the ABI records
  (the contract seam 4 published)."
  ([create abi] (js-kernel create abi {}))
  ([create abi {:keys [precision containment mapping]
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
         specials (mixing/special-order (:special-slots abi))
         ;; A fused slot publishes the voice's specials from its own call
         ;; partition (M6 seam 4), in slot order, ascending bit -- the order
         ;; `read-specials!` reports. The arena index in each is what the slot
         ;; reads; a plain voice's are the ABI's own.
         partition-specials (when mapping
                              (mapv (fn [partition]
                                      (vec (sort-by :bit (:specials partition))))
                                    (:call-partitions abi)))
         ;; The arena range of each slot's call partition (M6a, seam 3), from the
         ;; ABI's `:call-partitions`: restoring one slot is copying this range
         ;; back from the instance's initial arena, exactly as the JVM host does
         ;; (D51). A JS arena is one flat region indexed by the state slot, so the
         ;; logical base is the physical one.
         partition-ranges (when mapping
                            (mapv (fn [partition]
                                    {:base (long (:base partition))
                                     :size (long (:size partition))})
                                  (:call-partitions abi)))
         ;; The base -> fused control table, one row per slot: where each control
         ;; the pool writes lands in the fused argument list. A plain kernel has
         ;; one identity row, so the write path is the same loop either way (M6).
         base-count (if mapping (clojure.core/count (:hoisted mapping)) control-count)
         slot-rows (if mapping
                     (mapv (fn [slot]
                             (mapv (fn [position]
                                     (long (fusion/fused-position mapping slot position)))
                                   (range base-count)))
                           (range slots))
                     [(vec (range control-count))])
         ;; The process's lane layout is one flat region, but the *region*
         ;; belongs to the voice (M4c): each voice is allocated its own, and the
         ;; generated process takes it as its last positional argument, so the
         ;; argument array grows by one slot.
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
         ;; The shared region is the *pool's*, not a voice's: one array, passed to
         ;; every voice, so a broadcast parameter is written once (M5). It is the
         ;; lane region's shape, so one addressing serves both.
         shared-stride (long (or (:shared-stride abi) 0))
         shared-layout (into {}
                             (map (clojure.core/fn [control]
                                    [(clojure.core/name (:name control))
                                     (long (:offset control))]))
                             (:shared abi))
         shared-count (clojure.core/count shared-layout)
         shared-region (when (pos? shared-count)
                         (mixing/real-array f64? (* shared-stride shared-count)))
         ;; Where each region sits in the generated argument list: the controls
         ;; first, then the lane region, then the shared region.
         lane-slot (when (pos? lane-count) (+ 3 control-count))
         shared-slot (when (pos? shared-count)
                       (+ 3 control-count (if (pos? lane-count) 1 0)))
         ;; Every voice is its own instance, so the key is creation order (M6); a
         ;; fused instance makes `slots` handles share one, and `current` is that
         ;; instance while its handles are being created.
         created (volatile! 0)
         current (volatile! nil)]
     (reify pool/Kernel
       (create-voice [_ max-block-frames]
         (if (nil? mapping)
           (let [object (create)]
             {:object object
              :instance-key (vswap! created inc)
              ;; The generated ABI is output, frames, sample-rate, then the
              ;; positional controls, then the lane region when the process has
              ;; one; the array is reused on every block.
              :arguments (js/Array. (+ 3 control-count
                                       (if (pos? lane-count) 1 0)
                                       (if (pos? shared-count) 1 0)))
              :lane-region (when (pos? lane-count)
                             (mixing/real-array f64? (* lane-stride lane-count lane-slots)))
              :scratch (mixing/scratch-buffer f64? channels max-block-frames)
              :reset! (clojure.core/fn [] (.call (.-reset object) object))})
           (let [slot (long (mod (long @created) slots))]
             (when (zero? slot)
               (let [object (create)]
                 (vreset! current
                          {:object object
                           :instance-key (inc (quot (long @created) slots))
                           :arguments (js/Array. (+ 3 control-count
                                                    (if (pos? lane-count) 1 0)
                                                    (if (pos? shared-count) 1 0)))
                           :lane-region (when (pos? lane-count)
                                          (mixing/real-array
                                           f64? (* lane-stride lane-count lane-slots)))
                           :scratch (mixing/scratch-buffer f64? channels
                                                           max-block-frames)
                           ;; The arena's initial values, taken once, before any
                           ;; block runs: a per-slot reset restores one call
                           ;; partition from here (D51).
                           :initial (.slice (.-state object))
                           :refs (volatile! slots)})))
             (vswap! created inc)
             (let [instance @current
                   {:keys [base size]} (nth partition-ranges slot)
                   state (.-state (:object instance))
                   initial (:initial instance)]
               {:object (:object instance)
                :instance-key (:instance-key instance)
                :slot slot
                :refs (:refs instance)
                :arguments (:arguments instance)
                :lane-region (:lane-region instance)
                :scratch (:scratch instance)
                :reset! (clojure.core/fn []
                          (dotimes [index size]
                            (aset state (+ base index)
                                  (aget initial (+ base index)))))}))))
       (instance-key [_ voice] (:instance-key voice))
       (destroy-voice! [_ voice]
         (if (nil? mapping)
           (when-let [destroy (.-destroy (:object voice))]
             (.call destroy (:object voice)))
           ;; The process outlives the handle: the pool destroys one voice per
           ;; slot, and there is only one object to release.
           (when (zero? (long (vswap! (:refs voice) dec)))
             (when-let [destroy (.-destroy (:object voice))]
               (.call destroy (:object voice))))))
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
           (let [length (min (mixing/value-length values) shared-stride)]
             (dotimes [index length]
               (aset shared-region (+ offset index)
                     (js/Number (mixing/value-at values index)))))))
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
                 length (min (mixing/value-length values) lane-stride)
                 region (:lane-region voice)]
             (dotimes [index length]
               (aset region (+ offset index)
                     (js/Number (mixing/value-at values index)))))))
       (read-specials! [_ voice target]
         (let [state (.-state (:object voice))
               ;; A fused slot reports its own partition's specials (M6 seam 4); a
               ;; plain voice reports the ABI's.
               declared (if mapping
                          (nth partition-specials (long (:slot voice)))
                          specials)]
           (dotimes [position (clojure.core/count declared)]
             (let [slot (nth declared position)
                   value (aget state (:index slot))]
               (aset target position
                     (if (= :boolean (:type slot))
                       (if value 1.0 0.0)
                       (js/Number value)))))))
       (clear-output! [_ output frames]
         (mixing/clear-channels! f64? channels output frames))
       (mix-voice! [_ voice output frames sample-rate]
         (let [object (:object voice)
               arguments (:arguments voice)]
           (aset arguments 0 (:scratch voice))
           (aset arguments 1 frames)
           (aset arguments 2 sample-rate)
           (when lane-slot
             (aset arguments lane-slot (:lane-region voice)))
           (when shared-slot
             (aset arguments shared-slot shared-region))
           (.apply (.-process object) object arguments))
         (mixing/mix-block! containment f64? channels voice output frames))))))

(defn js-bus-kernel
  "A post-mix bus over a generated JavaScript module.

  A bus is a *processor*, so the generated `process` takes the input array, the
  output array, frames, sample-rate, and then the positional controls. `create`
  is the module's exported `createKernel`; `create-bus` makes one independent
  instance, and the host makes one per output channel.

  Returns `{:create-bus, :run-bus!, :reset-bus!}`. `controls` is a JS array in
  ABI order, reused across blocks."
  [create abi]
  (let [control-count (clojure.core/count (:controls abi))]
    {:create-bus
     (clojure.core/fn []
       ;; A post-mix chain that declares a lane needs the same delivery the
       ;; voices now get. Until it has it, refuse once here rather than on every
       ;; block in the audio path.
       (when (or (seq (:lanes abi)) (seq (:shared abi)))
         (throw (ex-info "this target cannot deliver lanes or a shared region to a bus yet"
                         {:lanes (mapv :name (:lanes abi))
                          :shared (mapv :name (:shared abi))
                          :target :js})))
       (let [object (create)]
         {:object object
          :arguments (js/Array. (+ 4 control-count))}))
     :run-bus!
     (clojure.core/fn [bus input output frames sample-rate controls]
       (let [object (:object bus)
             arguments (:arguments bus)]
         (aset arguments 0 input)
         (aset arguments 1 output)
         (aset arguments 2 frames)
         (aset arguments 3 sample-rate)
         (dotimes [position control-count]
           (aset arguments (+ 4 position) (aget controls position)))
         (.apply (.-process object) object arguments)))
     :reset-bus!
     (clojure.core/fn [bus]
       (let [object (:object bus)
             reset (.-reset object)]
         (when reset (.call reset object))))}))
