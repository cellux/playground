(ns omkamra.dsp.hosts.interpreter
  "The reference host: a `Kernel` over the portable interpreter (P10).

  Every call builds a context map and a control vector, which is exactly why this
  is not a real-time host — but it is the same reference semantics every other
  target is held to, so a pool's mixing and lifecycle are checked against it
  rather than against hand-computed samples. It is `.cljc`, so the JVM suite and
  the browser testbed compare against the very same oracle.

  The IR carries the special *mask* but not the slot table, so the ABI comes
  along for `read-specials!` (D39)."
  (:require [omkamra.dsp.hosts.fusion :as fusion]
            [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth.pool :as pool]
            [omkamra.dsp.targets.interpreter :as interpreter]))

(defn interpreter-kernel
  "A `Kernel` over an interpreter IR and the ABI it was compiled with.

  `containment` selects the non-finite policy, as it does for every kernel.
  `mapping` is nil for a plain kernel — one instance per voice — and a
  `fusion/fused-mapping` for a fused one, whose `slots` handles share an instance
  and one instance key. The mapping turns the base control values the pool writes
  into the slice of the fused interface *that* slot owns, so the interpreter pool
  renders a fused voice too and stays the oracle for it, exactly as the JVM and
  browser kernels do (M6)."
  ([ir abi] (interpreter-kernel ir abi {}))
  ([ir abi {:keys [containment mapping] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)
         f64? (= :f64 (:precision ir))
         channels (long (get-in abi [:channels :count] 1))
         controls (clojure.core/count (:controls ir))
         fused? (some? mapping)
         slots (long (or (:slots mapping) 1))
         partitions (vec (:call-partitions abi))
         _ (when (and fused? (not= slots (clojure.core/count partitions)))
             (throw (ex-info (str "a fused voice's slot count must be the number of"
                                  " call sites the process has, or a slot cannot be"
                                  " reset on its own")
                             {:slots slots
                              :partitions (clojure.core/count partitions)})))
         ;; The base -> fused control table, one row per slot: where each control
         ;; the pool writes lands in the fused interface. A plain kernel has one
         ;; identity row, so the write path is one loop either way (M6).
         base-count (if fused? (clojure.core/count (:hoisted mapping)) controls)
         slot-rows (if fused?
                     (mapv (clojure.core/fn [slot]
                             (mapv (clojure.core/fn [position]
                                     (long (fusion/fused-position mapping slot position)))
                                   (range base-count)))
                           (range slots))
                     [(vec (range controls))])
         ;; A *fused* slot's specials live in its call partition, not the entry's
         ;; (M6 seam 4); `partition-specials` is already in ascending-bit order.
         partition-specials (mapv (clojure.core/fn [partition]
                                    (vec (sort-by :bit (:specials partition))))
                                  partitions)
         specials (mixing/special-order (:special-slots abi))
         special-count (clojure.core/count specials)
         ;; The lane layout is the process's — lane `i` at `i * stride` (D28) —
         ;; but the region is the voice's, so each voice is handed its own, and a
         ;; fused instance renders every slot in one call, so its one region holds
         ;; a slice per slot (F4). The interpreter checks the buffer's primitive
         ;; type against the IR's precision, so the region is allocated at that
         ;; precision too.
         lane-stride (long (or (:lane-stride abi) 0))
         lane-layout (into {}
                           (map (clojure.core/fn [lane]
                                  [(clojure.core/name (:name lane))
                                   (long (:offset lane))]))
                           (:lanes abi))
         lane-count (clojure.core/count lane-layout)
         lane-slots (long (or (:lane-slots abi) 1))
         lane-region-size (* lane-stride lane-count lane-slots)
         ;; One shared region per *kernel*, i.e. per pool: a broadcast parameter
         ;; is written once and read by every voice (M5). Same shape as the lanes,
         ;; so one stride and one addressing serve both.
         shared-stride (long (or (:shared-stride abi) 0))
         shared-layout (into {}
                             (map (clojure.core/fn [control]
                                    [(clojure.core/name (:name control))
                                     (long (:offset control))]))
                             (:shared abi))
         shared-count (clojure.core/count shared-layout)
         shared-region (when (pos? shared-count)
                         (mixing/real-array f64? (* shared-stride shared-count)))
         ;; A plain kernel gives every voice its own instance, so the key is the
         ;; order the voices were created in. A fused one makes `slots` handles
         ;; share an instance, so the key is the *instance's* and `current` is the
         ;; instance those handles share (M6).
         created (volatile! 0)
         current (volatile! nil)]
     (reify pool/Kernel
       (create-voice [_ max-block-frames]
         (if fused?
           (let [slot (long (mod (long @created) slots))]
             (when (zero? slot)
               (vreset! current
                        {:state (interpreter/create-state ir)
                         :instance-key (inc (quot (long @created) slots))
                         :values (mixing/real-array true controls)
                         :lane-region (when (pos? lane-count)
                                        (mixing/real-array f64? lane-region-size))
                         :scratch (mixing/scratch-buffer f64? channels max-block-frames)
                         :refs (volatile! slots)}))
             (vswap! created inc)
             (let [instance @current
                   {:keys [base size]} (nth partitions slot)]
               {:state (:state instance)
                :instance-key (:instance-key instance)
                :slot slot
                :refs (:refs instance)
                :values (:values instance)
                :lane-region (:lane-region instance)
                :scratch (:scratch instance)
                ;; A containment reset silences one voice, which in a fused
                ;; instance is one slot -- so it resets that partition, not the
                ;; whole process (M6a).
                :reset! (clojure.core/fn []
                          (interpreter/reset-partition! ir (:state instance)
                                                        (long base) (long size)))}))
           (let [state (interpreter/create-state ir)]
             {:state state
              :instance-key (vswap! created inc)
              :values (mixing/real-array true controls)
              :lane-region (when (pos? lane-count)
                             (mixing/real-array f64? lane-region-size))
              :scratch (mixing/scratch-buffer f64? channels max-block-frames)
              :reset! (clojure.core/fn [] (interpreter/reset-state! ir state))})))
       (instance-key [_ voice] (:instance-key voice))
       (destroy-voice! [_ voice]
         (if fused?
           ;; The process outlives the handle: the pool destroys one voice per
           ;; slot, and there is only one instance to release.
           (when (zero? (long (vswap! (:refs voice) dec)))
             (interpreter/reset-state! ir (:state voice)))
           (interpreter/reset-state! ir (:state voice))))
       (reset-voice! [_ voice]
         (if fused?
           ;; One slot, one partition: a fused instance's slots do not clobber
           ;; each other's state (M6a, seam 3).
           (let [{:keys [base size]} (nth partitions (long (:slot voice)))]
             (interpreter/reset-partition! ir (:state voice) (long base) (long size)))
           (interpreter/reset-state! ir (:state voice))))
       (write-voice-controls! [_ voice values]
         (let [^doubles target (:values voice)]
           (if fused?
             (let [row (nth slot-rows (long (:slot voice)))]
               (dotimes [position base-count]
                 (aset target (int (long (nth row position)))
                       (double (mixing/value-at values position)))))
             #?(:clj (System/arraycopy ^doubles values 0 target 0 (alength target))
                :cljs (dotimes [position (.-length target)]
                        (aset target position (aget values position)))))))
       (write-voice-lane! [_ voice lane values]
         (let [key (if (string? lane) lane (clojure.core/name lane))
               lane-offset (get lane-layout key)]
           (when-not lane-offset
             (throw (ex-info "this DSP voice declares no such lane"
                             {:lane lane :lanes (vec (sort (keys lane-layout)))})))
           ;; A fused instance holds one slice per slot, so a voice's lane belongs
           ;; to the slice its slot *is* (F4); a plain voice has no slot, and its
           ;; kernel one slice.
           (let [offset (+ (long lane-offset)
                           (* (long (or (:slot voice) 0)) lane-stride lane-count))
                 length (min (mixing/value-length values) lane-stride)]
             #?(:clj (if f64?
                       (let [^doubles target (:lane-region voice)]
                         (dotimes [index length]
                           (aset target (int (+ offset index))
                                 (double (mixing/value-at values index)))))
                       (let [^floats target (:lane-region voice)]
                         (dotimes [index length]
                           (aset target (int (+ offset index))
                                 (float (mixing/value-at values index))))))
                :cljs (let [target (:lane-region voice)]
                        (dotimes [index length]
                          (aset target (+ offset index)
                                (js/Number (mixing/value-at values index)))))))))
       (write-shared! [_ control values]
         (let [key (if (string? control) control (clojure.core/name control))
               offset (get shared-layout key)]
           (when-not offset
             (throw (ex-info "this DSP voice declares no such shared control"
                             {:control control
                              :controls (vec (sort (keys shared-layout)))})))
           (let [length (min (mixing/value-length values) shared-stride)]
             #?(:clj (if f64?
                       (let [^doubles target shared-region]
                         (dotimes [index length]
                           (aset target (int (+ offset index))
                                 (double (mixing/value-at values index)))))
                       (let [^floats target shared-region]
                         (dotimes [index length]
                           (aset target (int (+ offset index))
                                 (float (mixing/value-at values index))))))
                :cljs (let [target shared-region]
                        (dotimes [index length]
                          (aset target (+ offset index)
                                (js/Number (mixing/value-at values index)))))))))
       (read-specials! [_ voice target]
         (if fused?
           (let [state (:state voice)
                 slot-specials (nth partition-specials (long (:slot voice)))]
             (dotimes [position (clojure.core/count slot-specials)]
               (let [value (aget state (int (:index (nth slot-specials position))))]
                 #?(:clj (aset-double ^doubles target position (double value))
                    :cljs (aset target position (js/Number value))))))
           (let [state (:state voice)]
             (dotimes [position special-count]
               (let [value (aget state (int (:index (nth specials position))))]
                 #?(:clj (aset-double ^doubles target position (double value))
                    :cljs (aset target position (js/Number value))))))))
       (clear-output! [_ output frames]
         (mixing/clear-channels! f64? channels output frames))
       (mix-voice! [_ voice output frames sample-rate]
         (interpreter/process-context
          ir (cond-> {:output (:scratch voice)
                      :frames frames
                      :sample-rate sample-rate
                      :controls (vec (:values voice))
                      :state (:state voice)}
               (:lane-region voice) (assoc :lanes (:lane-region voice))
               shared-region (assoc :shared shared-region)))
         (mixing/mix-block! containment f64? channels voice output frames))))))

(defn interpreter-bus-kernel
  "A post-mix bus over the portable interpreter.

  A bus is a *processor*, so this runs one channel's instance over the same
  array as input and output. The reference interpreter is the oracle every real
  host is held to, and the extra allocation — a control vector per call — is
  acceptable here because it is not the audio path.

  The chain is instrument-level, so its regions are too: one lane region and one
  shared region for the whole instrument, handed to every channel's instance as
  the flat arrays the interpreter reads (H2). The host writes them with
  `:write-lane!` / `:write-shared!`.

  `bus-ir` is the compiled interpreter IR, `abi` its ABI, and `precision` its
  physical precision. Returns the same `{:create-bus, :run-bus!, :reset-bus!,
  :write-lane!, :write-shared!}` map as `omkamra.dsp.hosts.js/js-bus-kernel`."
  [bus-ir abi precision]
  (let [f64? (= :f64 precision)
        lane-stride (long (or (:lane-stride abi) 0))
        lane-layout (mixing/region-layout (:lanes abi))
        lane-count (clojure.core/count lane-layout)
        lane-region (when (pos? lane-count)
                      (mixing/real-array f64? (* lane-stride lane-count)))
        shared-stride (long (or (:shared-stride abi) 0))
        shared-layout (mixing/region-layout (:shared abi))
        shared-count (clojure.core/count shared-layout)
        shared-region (when (pos? shared-count)
                        (mixing/real-array f64? (* shared-stride shared-count)))]
    {:create-bus
     (clojure.core/fn [] {:state (interpreter/create-state bus-ir)})
     :run-bus!
     (clojure.core/fn [bus input output frames sample-rate controls]
       (interpreter/process-context
        bus-ir
        (cond-> {:input input :output output :frames frames
                 :sample-rate sample-rate
                 :controls (mixing/values-vector controls)
                 :state (:state bus)}
          lane-region (assoc :lanes lane-region)
          shared-region (assoc :shared shared-region))))
     :reset-bus!
     (clojure.core/fn [bus] (interpreter/reset-state! bus-ir (:state bus)))
     :write-lane!
     (clojure.core/fn [lane values]
       (when lane-region
         (mixing/write-region! lane-region
                               (mixing/region-offset lane-layout lane :lane)
                               lane-stride values f64?))
       nil)
     :write-shared!
     (clojure.core/fn [control values]
       (when shared-region
         (mixing/write-region! shared-region
                               (mixing/region-offset shared-layout control
                                                     :shared-control)
                               shared-stride values f64?))
       nil)}))
