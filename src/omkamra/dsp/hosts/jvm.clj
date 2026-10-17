(ns omkamra.dsp.hosts.jvm
  "The JVM host: a `Kernel` over a compiled `:jvm` artifact (P10).

  This is the real-time path. `create-instance` gives each voice its own state,
  and the raw unchecked call renders it, with the control arrays and the mixing
  scratch preallocated per voice, so a voice render allocates nothing.

  The ABI's layout is resolved into primitive arrays at construction rather than
  read from maps in the audio path: `control-slots` says where each ABI control
  lives in the physical control arrays, and `special-access` where each declared
  special lives in the state arrays (D39)."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.hosts.mixing :as mixing]
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

(defn jvm-kernel
  "A `Kernel` over a JVM process artifact.

  The artifact must be a generator: a voice has no input port (D23). Each voice
  gets its own instance, its own physical control arrays, and its own mixing
  scratch of `max-block-frames` samples.

  `containment` selects the non-finite policy: `:off` mixes the block untouched,
  `:sanitize` contains each sample, and `:reset` silences and resets a voice
  whose block is non-finite."
  [artifact containment]
  (let [containment (mixing/check-containment containment)
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
        (special-access state-slots (:special-slots (:abi artifact)))]
    (reify pool/Kernel
      (create-voice [_ max-block-frames]
        (let [instance (dsp/create-instance artifact)]
          {:instance instance
           :float-controls (mixing/real-array f64? float-count)
           :int-controls (int-array (int int-count))
           :scratch (mixing/scratch-buffer f64? channels max-block-frames)
           :reset! (clojure.core/fn [] (dsp/reset! instance))}))
      (destroy-voice! [_ voice]
        (dsp/destroy! (:instance voice)))
      (reset-voice! [_ voice]
        (dsp/reset! (:instance voice)))
      (write-voice-controls! [_ voice values]
        (if f64?
          (let [^doubles target (:float-controls voice)
                ^ints ints (:int-controls voice)]
            (dotimes [position control-count]
              (let [slot (aget float-slot position)]
                (if (>= slot 0)
                  (aset-double target (int slot) (aget ^doubles values position))
                  (aset-int ints (int (aget int-slot position))
                            (long (aget ^doubles values position)))))))
          (let [^floats target (:float-controls voice)
                ^ints ints (:int-controls voice)]
            (dotimes [position control-count]
              (let [slot (aget float-slot position)]
                (if (>= slot 0)
                  (aset-float target (int slot)
                              (float (aget ^doubles values position)))
                  (aset-int ints (int (aget int-slot position))
                            (long (aget ^doubles values position)))))))))
      (read-specials! [_ voice target]
        (let [state (:state (:instance voice))
              floats (:float state)
              ^ints ints (:int state)]
          (dotimes [position special-count]
            (let [slot (aget special-float position)]
              (aset-double target position
                           (if (>= slot 0)
                             (double (if f64?
                                       (aget ^doubles floats (int slot))
                                       (aget ^floats floats (int slot))))
                             (double (aget ints (int (aget special-int position))))))))))
      (clear-output! [_ output frames]
        (mixing/clear-channels! f64? channels output frames))
      (mix-voice! [_ voice output frames sample-rate]
        (dsp/process-unchecked! (:instance voice) nil (:scratch voice) frames
                                sample-rate (:float-controls voice)
                                (:int-controls voice) nil)
        (mixing/mix-block! containment f64? channels voice output frames)))))
