(ns omkamra.dsp.hosts.interpreter
  "The reference host: a `Kernel` over the portable interpreter (P10).

  Every call builds a context map and a control vector, which is exactly why this
  is not a real-time host — but it is the same reference semantics every other
  target is held to, so a pool's mixing and lifecycle are checked against it
  rather than against hand-computed samples. It is `.cljc`, so the JVM suite and
  the browser testbed compare against the very same oracle.

  The IR carries the special *mask* but not the slot table, so the ABI comes
  along for `read-specials!` (D39)."
  (:require [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth.pool :as pool]
            [omkamra.dsp.targets.interpreter :as interpreter]))

(defn interpreter-kernel
  "A `Kernel` over an interpreter IR and the ABI it was compiled with.

  `containment` selects the non-finite policy, as it does for every kernel."
  ([ir abi] (interpreter-kernel ir abi {}))
  ([ir abi {:keys [containment] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)
         f64? (= :f64 (:precision ir))
         channels (long (get-in abi [:channels :count] 1))
         controls (clojure.core/count (:controls ir))
         specials (mixing/special-order (:special-slots abi))
         special-count (clojure.core/count specials)
         ;; The lane layout is the process's — lane `i` at `i * stride` (D28) —
         ;; but the region is the voice's, so each voice is handed its own. The
         ;; interpreter checks the buffer's primitive type against the IR's
         ;; precision, so the region is allocated at that precision too.
         lane-stride (long (or (:lane-stride abi) 0))
         lane-layout (into {}
                           (map (clojure.core/fn [lane]
                                  [(clojure.core/name (:name lane))
                                   (long (:offset lane))]))
                           (:lanes abi))
         lane-count (clojure.core/count lane-layout)
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
         ;; Every voice here is its own instance, so the key is the order they were
         ;; created in — dense, so a host can index an array by it (M6).
         created (volatile! 0)]
     (reify pool/Kernel
       (create-voice [_ max-block-frames]
         (let [state (interpreter/create-state ir)]
           {:state state
            :instance-key (vswap! created inc)
            :values (mixing/real-array true controls)
            :lane-region (when (pos? lane-count)
                           (mixing/real-array f64? (* lane-stride lane-count)))
            :scratch (mixing/scratch-buffer f64? channels max-block-frames)
            :reset! (clojure.core/fn [] (interpreter/reset-state! ir state))}))
       (instance-key [_ voice] (:instance-key voice))
       (destroy-voice! [_ voice]
         (interpreter/reset-state! ir (:state voice)))
       (reset-voice! [_ voice]
         (interpreter/reset-state! ir (:state voice)))
       (write-voice-controls! [_ voice values]
         (let [^doubles target (:values voice)]
           #?(:clj (System/arraycopy ^doubles values 0 target 0 (alength target))
              :cljs (dotimes [position (.-length target)]
                      (aset target position (aget values position))))))
       (write-voice-lane! [_ voice lane values]
         (let [key (if (string? lane) lane (clojure.core/name lane))
               offset (get lane-layout key)]
           (when-not offset
             (throw (ex-info "this DSP voice declares no such lane"
                             {:lane lane :lanes (vec (sort (keys lane-layout)))})))
           (let [length (min (mixing/value-length values) lane-stride)]
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
         (let [state (:state voice)]
           (dotimes [position special-count]
             (let [value (aget state (int (:index (nth specials position))))]
               #?(:clj (aset-double ^doubles target position (double value))
                  :cljs (aset target position (js/Number value)))))))
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
