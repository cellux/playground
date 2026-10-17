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
         special-count (clojure.core/count specials)]
     (reify pool/Kernel
       (create-voice [_ max-block-frames]
         (let [state (interpreter/create-state ir)]
           {:state state
            :values (mixing/real-array true controls)
            :scratch (mixing/scratch-buffer f64? channels max-block-frames)
            :reset! (clojure.core/fn [] (interpreter/reset-state! ir state))}))
       (destroy-voice! [_ voice]
         (interpreter/reset-state! ir (:state voice)))
       (reset-voice! [_ voice]
         (interpreter/reset-state! ir (:state voice)))
       (write-voice-controls! [_ voice values]
         (let [^doubles target (:values voice)]
           #?(:clj (System/arraycopy ^doubles values 0 target 0 (alength target))
              :cljs (dotimes [position (.-length target)]
                      (aset target position (aget values position))))))
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
          ir {:output (:scratch voice)
              :frames frames
              :sample-rate sample-rate
              :controls (vec (:values voice))
              :state (:state voice)})
         (mixing/mix-block! containment f64? channels voice output frames))))))
