(ns omkamra.dsp.hosts.js
  "The JavaScript host: a `Kernel` over a generated JavaScript module (P10d).

  The module exports `createKernel`, which is called once per voice, so each
  voice is an independent kernel object with its own state. A pool voice is a
  generator (D23), so the generated `process` takes the output block and then the
  positional controls; one argument array is built per voice and refilled per
  block, so a render allocates nothing.

  The kernel object also exposes its state arena, which is what makes
  `read-specials!` a single array read rather than a second entry point (D39)."
  (:require [omkamra.dsp.hosts.mixing :as mixing]
            [omkamra.dsp.synth.pool :as pool]))

(defn js-kernel
  "A `Kernel` over a generated JavaScript artifact.

  `create` is the module's exported `createKernel`, and `abi` is that artifact's
  ABI; `:precision` decides the element type of the mixing scratch.

  `containment` selects the non-finite policy, as it does for every kernel."
  ([create abi] (js-kernel create abi {}))
  ([create abi {:keys [precision containment] :or {containment :reset}}]
   (let [containment (mixing/check-containment containment)
         f64? (= :f64 precision)
         channels (long (get-in abi [:channels :count] 1))
         control-count (clojure.core/count (:controls abi))
         specials (mixing/special-order (:special-slots abi))
         special-count (clojure.core/count specials)]
     (reify pool/Kernel
       (create-voice [_ max-block-frames]
         (let [object (create)]
           {:object object
            ;; The generated ABI is output, frames, sample-rate, then the
            ;; positional controls, and the array is reused on every block.
            :arguments (js/Array. (+ 3 control-count))
            :scratch (mixing/scratch-buffer f64? channels max-block-frames)
            :reset! (clojure.core/fn [] (.-reset object))}))
       (destroy-voice! [_ voice]
         (when-let [destroy (.-destroy (:object voice))]
           (.call destroy (:object voice))))
       (reset-voice! [_ voice]
         ((:reset! voice)))
       (write-voice-controls! [_ voice values]
         (let [arguments (:arguments voice)]
           (dotimes [position control-count]
             (aset arguments (+ 3 position) (aget values position)))))
       (read-specials! [_ voice target]
         (let [state (.-state (:object voice))]
           (dotimes [position special-count]
             (let [slot (nth specials position)
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
           (.apply (.-process object) object arguments))
         (mixing/mix-block! containment f64? channels voice output frames))))))
