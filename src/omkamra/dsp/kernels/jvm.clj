(ns omkamra.dsp.kernels.jvm
  "The JVM target's kernel: a `Kernel` over a compiled `:jvm` artifact (P10).

  This is the real-time path. `create-instance` gives each voice its own state,
  and the raw unchecked call renders it, with the control arrays and the mixing
  scratch preallocated per voice, so a voice render allocates nothing.

  The ABI's layout is resolved into primitive arrays at construction rather than
  read from maps in the audio path: `control-slots` says where each ABI control
  lives in the physical control arrays, and `special-access` where each declared
  special lives in the state arrays (D39)."
  (:require [omkamra.dsp.fusion :as fusion]
            [omkamra.dsp.kernels.mixing :as mixing]
            [omkamra.dsp.instance :as instance]
            [omkamra.dsp.instance.jvm :as instance-jvm]
            [omkamra.dsp.numbers :as numbers]
            [omkamra.dsp.synth.pool :as pool]))

(defn- control-slots
  "Per ABI position, where its value lives in the physical control arrays.

  The ABI's control layout already says this, but reading it in the audio path
  would mean walking a seq of maps once per note-on. Flattening it into two
  primitive arrays at construction is the same treatment the pool gives its
  control plan, for the same reason."
  [layout]
  (let [entries (:entries layout)
        count (clojure.core/count entries)
        float-slot (long-array count)
        int-slot (long-array count)]
    (doseq [entry entries]
      (let [position (long (:state-index entry))]
        (if (= :float (:storage entry))
          (do (aset float-slot position (long (:index entry)))
              (aset int-slot position -1))
          (do (aset float-slot position -1)
              (aset int-slot position (long (:index entry)))))))
    {:float-slot float-slot :int-slot int-slot :count count}))

(defn- special-access
  "Per declared special (ascending bit), where its value lives in the physical
  state arrays.

  Resolving a logical state index to a storage array and an offset happens once,
  at construction — the same treatment `control-slots` gives controls — so
  `read-specials!` reads a primitive and never touches a map on the audio path."
  [state-slots special-slots]
  (let [ordered (sort-by :bit (vals special-slots))
        count (clojure.core/count ordered)
        float-slot (long-array count)
        int-slot (long-array count)]
    (doseq [[position value] (map-indexed vector ordered)]
      (let [{:keys [storage index]} (get state-slots (:index value))]
        (if (= :float storage)
          (do (aset float-slot position (long index))
              (aset int-slot position -1))
          (do (aset float-slot position -1)
              (aset int-slot position (long index))))))
    {:float-slot float-slot :int-slot int-slot :count count}))

(defn- kernel
  "A `Kernel` over a JVM process artifact, plain or fused.

  `mapping` is nil for a plain voice kernel -- one instance per voice -- and a
  `fusion/fused-mapping` for a fused one, whose `slots` handles share an instance
  and one instance key; the mapping is what turns the base control values the pool
  writes into the slice of the fused interface that *that* slot owns, so the pool
  never learns that several of its voices live in one process (M6).

  Everything a fused instance shares, it shares through the handle: the control
  arrays, the scratch buffer, the lane region, and the shared region are one per
  instance, because one process call renders all its slots at once."
  [artifact mapping containment]
  (let [containment (mixing/check-containment containment)
        slots (long (or (:slots mapping) 1))
        _ (when (and mapping (not= slots (count (:partitions artifact))))
            (throw (ex-info (str "a fused voice's slot count must be the number of"
                                 " call sites the process has, or a slot cannot be"
                                 " reset on its own")
                            {:slots slots :partitions (count (:partitions artifact))
                             :mapping mapping})))
        f64? (= :f64 (:precision artifact))
        channels (long (get-in artifact [:abi :channels :count] 1))
        layout (:control-layout artifact)
        float-count (long (:float-count layout))
        int-count (long (:int-count layout))
        {^longs float-slot :float-slot ^longs int-slot :int-slot
         control-count :count}
        (control-slots layout)
        state-slots (into {}
                          (map (clojure.core/fn [entry] [(:state-index entry) entry]))
                          (:entries (:state-layout artifact)))
        {^longs special-float :float-slot
         ^longs special-int :int-slot
         special-count :count}
        (special-access state-slots (:special-slots (:abi artifact)))
        abi (:abi artifact)
        ;; A *fused* slot's specials live in its call partition, not the entry's
        ;; (M6 seam 4): the artifact records each partition's specials, resolved
        ;; here to physical slots, so `read-specials!` reads the voice the pool
        ;; asked about. One entry per slot.
        slot-special-access
        (when mapping
          (mapv (clojure.core/fn [partition]
                  (special-access
                   state-slots
                   (into {}
                         (map (clojure.core/fn [{:keys [special bit index]}]
                                [special {:bit bit :index index}]))
                         (:specials partition))))
                (:partitions artifact)))
        ;; The lane layout is the process's: lane `i` at `i * lane-stride`, both
        ;; compile-time constants (D28). The *region* is the voice's — one per
        ;; voice, allocated with the voice — so a per-voice lane has somewhere of
        ;; its own to live and a shared one is written once per voice by the
        ;; pool (M4c). A fused instance renders every slot in one call, so its one
        ;; region holds a slice per slot and the voice's slot says which (F4).
        lane-stride (long (or (:lane-stride abi) 0))
        lane-layout (into {}
                          (map (clojure.core/fn [lane]
                                 [(clojure.core/name (:name lane))
                                  (long (:offset lane))]))
                          (:lanes abi))
        lane-count (count lane-layout)
        lane-slots (long (or (:lane-slots abi) 1))
        lane-region-size (* lane-stride lane-count lane-slots)
        ;; One shared region per *kernel*, i.e. per pool: a broadcast parameter is
        ;; written once and read by every voice (M5). Its slices are the lanes'
        ;; shape, so one stride and one addressing serve both.
        shared-stride (long (or (:shared-stride abi) 0))
        shared-layout (into {}
                            (map (clojure.core/fn [control]
                                   [(clojure.core/name (:name control))
                                    (long (:offset control))]))
                            (:shared abi))
        shared-count (count shared-layout)
        shared-region (when (pos? shared-count)
                        (numbers/real-array f64? (* shared-stride shared-count)))
        ;; The base -> fused control table, one row per slot: where each control
        ;; the pool writes lands in the fused interface. A plain kernel has one
        ;; identity row, so the write path is the same loop either way (M6).
        slot-rows
        (if mapping
          (let [count (count (:hoisted mapping))]
            (into-array
             (map (clojure.core/fn [slot]
                    (let [row (long-array count)]
                      (dotimes [position count]
                        (aset row position
                              (long (fusion/fused-position mapping slot position))))
                      row))
                  (range slots))))
          (into-array [(long-array (range control-count))]))
        values-count (alength ^longs (aget ^objects slot-rows 0))
        ;; A plain kernel gives every voice its own instance, so the key is the
        ;; order the voices were created in. A fused one makes `slots` handles
        ;; share an instance, so the key is the *instance's*: every handle of a
        ;; group answers the same, which is all the pool's per-block mark needs,
        ;; and `current` is the instance those handles share (M6).
        created (volatile! 0)
        current (volatile! nil)]
    (reify pool/Kernel
      (create-voice [_ max-block-frames]
        (if (nil? mapping)
          (let [handle (instance/create-instance artifact)]
            {:instance handle
             :instance-key (vswap! created inc)
             :float-controls (numbers/real-array f64? float-count)
             :int-controls (int-array (int int-count))
             :lane-region (when (pos? lane-count)
                            (numbers/real-array f64? lane-region-size))
             :scratch (mixing/scratch-buffer f64? channels max-block-frames)
             :reset! (clojure.core/fn [] (instance/reset! handle))})
          (let [slot (long (mod (long @created) slots))]
            (when (zero? slot)
              (let [handle (instance/create-instance artifact)]
                (vreset! current
                         {:instance handle
                          :instance-key (inc (quot (long @created) slots))
                          :float-controls (numbers/real-array f64? float-count)
                          :int-controls (int-array (int int-count))
                          :lane-region (when (pos? lane-count)
                                         (numbers/real-array f64? lane-region-size))
                          :scratch (mixing/scratch-buffer f64? channels max-block-frames)
                          :refs (volatile! slots)})))
            (vswap! created inc)
            (let [held @current]
              {:instance (:instance held)
               :instance-key (:instance-key held)
               :slot slot
               :refs (:refs held)
               :float-controls (:float-controls held)
               :int-controls (:int-controls held)
               :lane-region (:lane-region held)
               :scratch (:scratch held)
               :reset! (:reset! held)}))))
      (instance-key [_ voice] (:instance-key voice))
      (destroy-voice! [_ voice]
        (if (nil? mapping)
          (instance/destroy! (:instance voice))
          ;; The process outlives the handle: the pool destroys one voice per
          ;; slot, and there is only one instance to release.
          (when (zero? (long (vswap! (:refs voice) dec)))
            (instance/destroy! (:instance voice)))))
      (reset-voice! [_ voice]
        (if (nil? mapping)
          (instance/reset! (:instance voice))
          ;; One slot, one partition: a fused instance's slots do not clobber each
          ;; other's state, so stealing or retriggering one is its own business
          ;; (M6a, seam 3).
          ((:reset-partition (:instance voice)) (long (:slot voice)))))
      (write-voice-controls! [_ voice values]
        (let [^longs row (aget ^objects slot-rows (long (or (:slot voice) 0)))
              ^ints ints (:int-controls voice)]
          (if f64?
            (let [^doubles target (:float-controls voice)]
              (dotimes [position values-count]
                (let [physical (aget float-slot (long (aget row position)))]
                  (if (>= physical 0)
                    (aset-double target (int physical)
                                 (aget ^doubles values position))
                    (aset-int ints
                              (int (aget int-slot (long (aget row position))))
                              (long (aget ^doubles values position)))))))
            (let [^floats target (:float-controls voice)]
              (dotimes [position values-count]
                (let [physical (aget float-slot (long (aget row position)))]
                  (if (>= physical 0)
                    (aset-float target (int physical)
                                (float (aget ^doubles values position)))
                    (aset-int ints
                              (int (aget int-slot (long (aget row position))))
                              (long (aget ^doubles values position))))))))))
      (write-voice-lane! [_ voice lane values]
        (let [key (if (string? lane) lane (clojure.core/name lane))
              lane-offset (get lane-layout key)]
          (when-not lane-offset
            (throw (ex-info "this DSP voice declares no such lane"
                            {:lane lane :lanes (vec (sort (keys lane-layout)))})))
          ;; A fused instance holds one slice per slot, so a voice's lane is written
          ;; into the slice its slot *is*; without that, every slot would take
          ;; turns overwriting one automation (F4). A plain voice has no slot, and
          ;; its kernel has one slice.
          (let [offset (+ (long lane-offset)
                          (* (long (or (:slot voice) 0)) lane-stride lane-count))
                length (min (numbers/value-length values) lane-stride)]
            (if f64?
              (let [^doubles target (:lane-region voice)]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (double (numbers/value-at values index)))))
              (let [^floats target (:lane-region voice)]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (float (numbers/value-at values index)))))))))
      (write-shared! [_ control values]
        (let [key (if (string? control) control (clojure.core/name control))
              offset (get shared-layout key)]
          (when-not offset
            (throw (ex-info "this DSP voice declares no such shared control"
                            {:control control
                             :controls (vec (sort (keys shared-layout)))})))
          (let [length (min (numbers/value-length values) shared-stride)]
            (if f64?
              (let [^doubles target shared-region]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (double (numbers/value-at values index)))))
              (let [^floats target shared-region]
                (dotimes [index length]
                  (aset target (int (+ offset index))
                        (float (numbers/value-at values index)))))))))
      (read-specials! [_ voice target]
        (let [{:keys [^longs float-slot ^longs int-slot count]}
              (if mapping
                (nth slot-special-access (long (:slot voice)))
                {:float-slot special-float
                 :int-slot special-int
                 :count special-count})
              state (:state (:instance voice))
              floats (:float state)
              ^ints ints (:int state)]
          (dotimes [position (long count)]
            (let [slot (aget float-slot position)]
              (aset-double target position
                           (if (>= slot 0)
                             (double (if f64?
                                       (aget ^doubles floats (int slot))
                                       (aget ^floats floats (int slot))))
                             (double (aget ints (int (aget int-slot position))))))))))
      (clear-output! [_ output frames]
        (mixing/clear-channels! f64? channels output frames))
      (mix-voice! [_ voice output frames sample-rate]
        (instance-jvm/process-unchecked! (:instance voice) nil (:scratch voice) frames
                                         sample-rate (:float-controls voice)
                                         (:int-controls voice) (:lane-region voice)
                                         shared-region)
        (mixing/mix-block! containment f64? channels voice output frames)))))

(defn jvm-kernel
  "A `Kernel` over a JVM process artifact: one instance per voice.

  The artifact must be a generator: a voice has no input port (D23). Each voice
  gets its own instance, its own physical control arrays, and its own mixing
  scratch of `max-block-frames` samples.

  `containment` selects the non-finite policy: `:off` mixes the block untouched,
  `:sanitize` contains each sample, and `:reset` silences and resets a voice
  whose block is non-finite."
  [artifact containment]
  (kernel artifact nil containment))

(defn fused-jvm-kernel
  "A `Kernel` over a *fused* JVM process artifact: `slots` voices per instance.

  `mapping` is an `omkamra.dsp.fusion/fused-mapping` from the base voice's
  controls to the fused interface's, and it is what lets a pool stay base-shaped
  while the process holds several voices (M6).

  Each slot is reset on its own: the target resolves the state range of every call
  partition (`:partitions` on the artifact, in evaluation order, which for a fused
  process is slot order), so a steal or a retrigger in one slot does not disturb
  its neighbours. A `:shared` control, when there is one, is still one region for
  the whole instance and every slot reads it -- the pool writes it once, which is
  what it means. A fused slot also publishes the voice's builtin specials from its
  call partition (M6 seam 4, D54), so `read-specials!` returns that slot's
  `:activity` and the pool frees a finished voice exactly as it does for a plain
  voice. `poly` carries a voice's control declarations through (D52), so a
  `:shared` control stays shared in the fused interface; expanding one per slot,
  or a lane, is refused with a reason."
  [artifact mapping containment]
  (kernel artifact mapping containment))

(defn jvm-bus-kernel
  "A post-mix bus over a compiled `:jvm` artifact.

  A bus is a *processor*, so the raw JVM call takes the input array, the output
  array, frames, the sample rate, the physical control arrays, and the chain's
  two regions. `control-slots` resolves the ABI's control layout into primitive
  arrays once, at construction, so `:run-bus!` copies the values vector into them
  and never walks a map on the audio path.

  The chain is instrument-level, so its regions are too: one lane region and one
  shared region for the whole instrument, handed to every channel's instance.
  The host writes them with `:write-lane!` / `:write-shared!`, so the instance
  reads the region the host owns — the same arrangement every other bus kernel
  uses.

  Returns the same `{:create-bus, :run-bus!, :reset-bus!, :write-lane!,
  :write-shared!}` map as `omkamra.dsp.kernels.js/js-bus-kernel`."
  [artifact]
  (let [layout (:control-layout artifact)
        {^longs float-slot :float-slot ^longs int-slot :int-slot
         control-count :count}
        (control-slots layout)
        float-count (long (:float-count layout))
        int-count (long (:int-count layout))
        f64? (= :f64 (:precision artifact))
        abi (:abi artifact)
        lane-stride (long (or (:lane-stride abi) 0))
        lane-layout (mixing/region-layout (:lanes abi))
        lane-count (count lane-layout)
        lane-region (when (pos? lane-count)
                      (numbers/real-array f64? (* lane-stride lane-count)))
        shared-stride (long (or (:shared-stride abi) 0))
        shared-layout (mixing/region-layout (:shared abi))
        shared-count (count shared-layout)
        shared-region (when (pos? shared-count)
                        (numbers/real-array f64? (* shared-stride shared-count)))]
    {:create-bus
     (fn []
       {:instance (instance/create-instance artifact)
        :float-controls (numbers/real-array f64? float-count)
        :int-controls (int-array (int int-count))})
     :run-bus!
     (fn [bus input output frames sample-rate controls]
       (let [^doubles values controls]
         (dotimes [position control-count]
           (let [slot (aget float-slot position)]
             (if (>= slot 0)
               (if f64?
                 (aset-double ^doubles (:float-controls bus) (int slot)
                              (aget values position))
                 (aset-float ^floats (:float-controls bus) (int slot)
                             (float (aget values position))))
               (aset-int ^ints (:int-controls bus)
                         (int (aget int-slot position))
                         (long (aget values position)))))))
       (instance-jvm/process-unchecked! (:instance bus) input output frames
                                        (double sample-rate)
                                        (:float-controls bus) (:int-controls bus)
                                        lane-region shared-region))
     :reset-bus!
     (fn [bus] (instance/reset! (:instance bus)))
     :write-lane!
     (fn [lane values]
       (when lane-region
         (mixing/write-region! lane-region
                               (mixing/region-offset lane-layout lane :lane)
                               lane-stride values f64?))
       nil)
     :write-shared!
     (fn [control values]
       (when shared-region
         (mixing/write-region! shared-region
                               (mixing/region-offset shared-layout control
                                                     :shared-control)
                               shared-stride values f64?))
       nil)}))
