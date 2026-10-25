(ns omkamra.dsp.kernels.mixing
  "How a kernel renders a voice and sums it into the output block (P10d).

  Every kernel does the same two things around a voice — render it into its own
  scratch, then sum that into the block under the containment policy — so both are
  written once here. The reader conditionals are the only place the platforms
  differ: the JVM branch keeps the primitive hints that stop the audio path boxing
  a sample, because the JVM's mixing runs at the audio rate.

  The flat regions a pool writes automation *into* are
  `omkamra.dsp.kernels.regions`."
  (:require [omkamra.dsp.numbers :as numbers]
            [omkamra.dsp.synth :as synth]))

(def ^:private ^:const containment-ceiling
  "A voice sample beyond this magnitude is a numeric runaway, not a musical
  signal, so the kernel clamps it here. Non-finite samples become 0. This is a
  safety rail, not a limiter: the ceiling sits far above any single voice's peak
  (D39)."
  1.0e6)

#?(:clj
   (defn contain
     "Contain one voice sample at the kernel seam (D39): non-finite -> 0,
     otherwise clamp to +/- `containment-ceiling`. Applied per voice before its
     samples are summed, so one runaway voice cannot poison the whole block. The
     primitive hints keep the audio path from boxing each sample."
     ^double [^double value]
     (if (Double/isFinite value)
       (cond
         (< value (- containment-ceiling)) (- containment-ceiling)
         (> value containment-ceiling) containment-ceiling
         :else value)
       0.0))
   :cljs
   (defn contain
     "Contain one voice sample at the kernel seam (D39): non-finite -> 0,
     otherwise clamp to +/- `containment-ceiling`."
     [value]
     (if (js/isFinite value)
       (cond
         (< value (- containment-ceiling)) (- containment-ceiling)
         (> value containment-ceiling) containment-ceiling
         :else value)
       0.0)))

(defn planar-block
  "A planar (`channels` x `length`) block of reals: the array of channel arrays
  an audio callback is handed, which is also what a multi-channel kernel indexes."
  [f64? channels length]
  #?(:clj (into-array (if f64? (Class/forName "[D") (Class/forName "[F"))
                      (repeatedly channels
                                  (clojure.core/fn [] (numbers/real-array f64? length))))
     :cljs (let [block (js/Array. channels)]
             (dotimes [channel channels]
               (aset block channel (numbers/real-array f64? length)))
             block)))

(defn scratch-buffer
  "A voice's rendered output: a mono array, or a planar block for stereo (D40)."
  [f64? channels max-block-frames]
  (if (= 1 channels)
    (numbers/real-array f64? max-block-frames)
    (planar-block f64? channels max-block-frames)))

(defn channel-of
  "Channel `channel` of a rendered block: the array itself when mono, else one
  plane of a planar block.

  The `^objects` hint is load-bearing: without it the planar `aget` is
  reflective, which allocates about 24 KB per channel per block on the JVM
  (D26). The mono branch never indexes, so the hint is only reached on a planar
  block, where the buffer really is an object array."
  [channels buffer channel]
  (if (= 1 channels) buffer (aget ^objects buffer (int channel))))

(defn clear-channel!
  ;; `f64?` picks the primitive overload on the JVM; the browser's `fill` is
  ;; element-type blind.
  #_{:clj-kondo/ignore [:unused-binding]}
  [f64? buffer frames]
  #?(:clj (if f64?
            (java.util.Arrays/fill ^doubles buffer 0 (int frames) 0.0)
            (java.util.Arrays/fill ^floats buffer 0 (int frames) (float 0.0)))
     :cljs (.fill buffer 0 0 frames)))

(defn clear-channels!
  "Zero a mono or planar output block."
  [f64? channels output frames]
  (dotimes [channel channels]
    (clear-channel! f64? (channel-of channels output channel) frames)))

#?(:clj
   (defn mix-plain!
     "Add a channel's samples into `target`, untouched (the `:off` policy)."
     [f64? source target frames]
     (if f64?
       (let [^doubles src source
             ^doubles dst target]
         (dotimes [index (int frames)]
           (aset-double dst index (+ (aget dst index) (aget src index)))))
       (let [^floats src source
             ^floats dst target]
         (dotimes [index (int frames)]
           (aset-float dst index
                       (unchecked-float (+ (aget dst index) (aget src index))))))))
   :cljs
   (defn mix-plain!
     "Add a channel's samples into `target`, untouched (the `:off` policy)."
     [_f64? source target frames]
     (dotimes [index frames]
       (aset target index (+ (aget target index) (aget source index))))))

#?(:clj
   (defn mix-contained!
     "Add a channel's samples into `target`, containing each (the `:sanitize`
     policy)."
     [f64? source target frames]
     (if f64?
       (let [^doubles src source
             ^doubles dst target]
         (dotimes [index (int frames)]
           (aset-double dst index
                        (+ (aget dst index) (contain (aget src index))))))
       (let [^floats src source
             ^floats dst target]
         (dotimes [index (int frames)]
           (aset-float dst index
                       (unchecked-float
                        (+ (aget dst index) (contain (aget src index)))))))))
   :cljs
   (defn mix-contained!
     "Add a channel's samples into `target`, containing each (the `:sanitize`
     policy)."
     [_f64? source target frames]
     (dotimes [index frames]
       (aset target index (+ (aget target index) (contain (aget source index)))))))

#?(:clj
   (defn non-finite-channel?
     "True when a channel's rendered block holds any non-finite sample."
     [f64? source frames]
     (let [frames (int frames)]
       (if f64?
         (let [^doubles src source]
           (loop [index 0]
             (if (< index frames)
               (if (Double/isFinite (aget src index))
                 (recur (inc index))
                 true)
               false)))
         (let [^floats src source]
           (loop [index 0]
             (if (< index frames)
               (if (Double/isFinite (double (aget src index)))
                 (recur (inc index))
                 true)
               false))))))
   :cljs
   (defn non-finite-channel?
     "True when a channel's rendered block holds any non-finite sample."
     [_f64? source frames]
     (loop [index 0]
       (if (< index frames)
         (if (js/isFinite (aget source index)) (recur (inc index)) true)
         false))))

(defn mix-block!
  "Sum a voice's rendered scratch into the (mono or planar) `output` under the
  containment policy.

  Returns true only when a containment *event* occurred that the pool should
  count — the `:reset` policy resetting a voice. `:off` does not even scan, so
  it carries no containment cost. With stereo, `:reset` treats the whole voice
  (all channels) as the unit. The voice resets itself, through its own `:reset!`,
  so this stays target-blind."
  [containment f64? channels voice output frames]
  (let [scratch (:scratch voice)]
    (case containment
      :off (do (dotimes [channel channels]
                 (mix-plain! f64? (channel-of channels scratch channel)
                             (channel-of channels output channel) frames))
               false)
      :sanitize (do (dotimes [channel channels]
                      (mix-contained! f64? (channel-of channels scratch channel)
                                      (channel-of channels output channel) frames))
                    false)
      :reset (if (loop [channel 0]
                   (cond
                     (>= channel channels) false
                     (non-finite-channel? f64?
                                          (channel-of channels scratch channel)
                                          frames)
                     true
                     :else (recur (inc channel))))
               (do ((:reset! voice))
                   true)
               (do (dotimes [channel channels]
                     (mix-plain! f64? (channel-of channels scratch channel)
                                 (channel-of channels output channel) frames))
                   false)))))

(def containment-modes
  "The non-finite containment policies a kernel understands (D39): `:off` mixes
  untouched; `:sanitize` contains each voice's samples; `:reset` silences and
  resets a voice whose block is non-finite, so it recovers."
  #{:off :sanitize :reset})

(defn check-containment
  [mode]
  (if (contains? containment-modes mode)
    mode
    (synth/fail "unknown non-finite containment mode"
                {:mode mode :supported (vec containment-modes)})))

(defn special-order
  "A definition's declared specials, in ascending-bit order — the order
  `read-specials!` reports them in (D39)."
  [special-slots]
  (vec (sort-by :bit (vals special-slots))))
