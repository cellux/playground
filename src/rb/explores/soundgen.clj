(ns rb.explores.soundgen
  "Small, deliberately offline sound-synthesis playground.

  A synth is a function from time (seconds) to a sample value.  `render`
  evaluates it into a buffer and `write-wav!` persists that buffer as a mono,
  16-bit PCM WAV file.  Nothing in this namespace opens an audio device."
  (:require [clojure.java.io :as io]
            [omkamra.cgen.core :as cgen])
  (:import (java.io ByteArrayInputStream)
           (java.nio ByteBuffer ByteOrder)
           (javax.sound.sampled AudioFileFormat$Type AudioFormat
                                AudioInputStream AudioSystem Clip DataLine$Info
                                LineListener LineEvent LineEvent$Type)))

;; Keep DSP arithmetic on primitive numeric paths and use unchecked arithmetic.
(set! *unchecked-math* true)

(def ^:const sample-rate 48000)
(def ^:const tau (* 2.0 Math/PI))

(defn frames
  "Return the number of samples needed for `seconds` at `sample-rate`."
  ([seconds]
   (frames seconds sample-rate))
  ([seconds sample-rate]
   (long (Math/round (* (double sample-rate) (double seconds))))))

(defn render
  "Render `synth` for `seconds` into a double array.

  `synth` receives time in seconds and should return a value in roughly the
  range -1..1.  The optional third argument selects the output sample rate.
  The synth comes first so rendered sounds compose naturally with `->`."
  ([synth seconds]
   (render synth seconds sample-rate))
  ([synth seconds sample-rate]
   (let [nframes (frames seconds sample-rate)
         samples (double-array nframes)
         sample-period (/ 1.0 (double sample-rate))]
     (dotimes [i nframes]
       (aset-double samples i (double (synth (* i sample-period)))))
     {:sample-rate (int sample-rate)
      :samples samples})))

(declare play!)

(defn- cgen-input-bytes
  [generator nframes sample-rate arguments]
  (let [{:keys [params return-type]} (cgen/describe generator)
        argument-types (vec (rest params))]
    (when-not (and (= :double (first params))
                   (= :double return-type)
                   (every? #{:double} argument-types))
      (throw (IllegalArgumentException.
              "cgen sound generators must be ^double -> ^double functions")))
    (when-not (= (count arguments) (count argument-types))
      (throw (IllegalArgumentException.
              (str "expected " (count argument-types)
                   " cgen sound-generator arguments"))))
    (let [bytes (byte-array (* 8 nframes (count params)))
          buffer (doto (ByteBuffer/wrap bytes)
                   (.order ByteOrder/LITTLE_ENDIAN))
          arguments (mapv double arguments)]
      (dotimes [i nframes]
        (.putDouble buffer (/ (double i) (double sample-rate)))
        (doseq [argument arguments]
          (.putDouble buffer argument)))
      bytes)))

(defn render-cgen
  "Render a native cgen sample generator into the same sound map as `render`.

  The generator must take time in seconds as its first `^double` parameter,
  return `^double`, and use `^double` for its remaining parameters. `cgen`
  executes the whole binary record stream in one native process invocation."
  ([generator seconds]
   (render-cgen generator seconds sample-rate))
  ([generator seconds sample-rate & arguments]
   (let [nframes (frames seconds sample-rate)
         input (cgen-input-bytes generator nframes sample-rate arguments)
         {:keys [bytes count]} (cgen/invoke-binary generator {:input input})
         _ (when-not (= nframes count)
             (throw (IllegalStateException.
                     "cgen returned an unexpected number of sound samples")))
         buffer (doto (ByteBuffer/wrap ^bytes bytes)
                  (.order ByteOrder/LITTLE_ENDIAN))
         samples (double-array nframes)]
     (.get (.asDoubleBuffer buffer) samples)
     {:sample-rate (int sample-rate)
      :samples samples})))

(defn play-cgen!
  "Render and play a native cgen sample generator through javax.sound.sampled."
  ([generator seconds]
   (play! (render-cgen generator seconds)))
  ([generator seconds sample-rate & arguments]
   (play! (apply render-cgen generator seconds sample-rate arguments))))

(defn sine
  "Create a sine oscillator at `frequency` Hz."
  [frequency]
  (let [frequency (double frequency)]
    (fn [^double t]
      (Math/sin (* tau frequency t)))))

(defn saw
  "Create a bipolar sawtooth oscillator at `frequency` Hz."
  [frequency]
  (let [frequency (double frequency)]
    (fn [^double t]
      (let [phase (- (double (mod (* frequency t) 1.0)) 0.5)]
        (* 2.0 phase)))))

(defn square
  "Create a bipolar square oscillator at `frequency` Hz."
  [frequency]
  (let [frequency (double frequency)]
    (fn [^double t]
      (if (< (mod (* frequency t) 1.0) 0.5)
        1.0
        -1.0))))

(defn noise
  "Create a white-noise source with an optional java.util.Random instance."
  ([]
   (noise (java.util.Random.)))
  ([random]
   (fn [_]
     (- (* 2.0 (.nextDouble ^java.util.Random random)) 1.0))))

(defn gain
  "Scale a signal by `amount`."
  [amount signal]
  (let [amount (double amount)]
    (fn [^double t]
      (* amount (double (signal t))))))

(def ^:private default-kick-options
  {:frequency 150.0
   :end-frequency 45.0
   :pitch-decay 0.045
   :duration 0.5
   :decay 0.22
   :gain 0.9
   :click-gain 0.12
   :click-decay 0.008
   :click-frequency 1800.0})

(defn- normalize-kick-options [options]
  (when-not (map? options)
    (throw (IllegalArgumentException.
            "kick options must be a map")))
  (let [{:keys [frequency end-frequency pitch-decay duration decay gain
                click-gain click-decay click-frequency]
         :as options}
        (merge default-kick-options options)]
    (when-not (and (number? frequency) (pos? frequency)
                   (number? end-frequency) (pos? end-frequency)
                   (number? pitch-decay) (pos? pitch-decay)
                   (number? duration) (pos? duration)
                   (number? decay) (pos? decay)
                   (number? click-decay) (pos? click-decay)
                   (number? gain)
                   (number? click-gain)
                   (number? click-frequency) (pos? click-frequency))
      (throw (IllegalArgumentException.
              "kick frequencies, times, and gains must be valid numbers")))
    (assoc options
           :frequency (double frequency)
           :end-frequency (double end-frequency)
           :pitch-decay (double pitch-decay)
           :duration (double duration)
           :decay (double decay)
           :gain (double gain)
           :click-gain (double click-gain)
           :click-decay (double click-decay)
           :click-frequency (double click-frequency))))

(defn kick
  "Create a synthesized kick from an options map.

  Returns a time -> sample function. The pitch falls exponentially from
  `:frequency` to `:end-frequency`; the body and optional click then decay
  exponentially."
  [options]
  (let [{:keys [frequency end-frequency pitch-decay duration decay gain
                click-gain click-decay click-frequency]}
        (normalize-kick-options options)]
    (fn [^double t]
      (if (or (neg? t) (>= t duration))
        0.0
        (let [phase-cycles (+ (* end-frequency t)
                              (* (- frequency end-frequency)
                                 pitch-decay
                                 (- 1.0 (Math/exp (- (/ t pitch-decay))))))
              body (* gain
                      (Math/exp (- (/ t decay)))
                      (Math/sin (* tau phase-cycles)))
              click (* click-gain
                       (Math/exp (- (/ t click-decay)))
                       (Math/sin (* tau click-frequency t)))]
          (+ body click))))))

(defn- cgen-double-symbol [name]
  (with-meta name {:tag 'double}))

(defn- specialized-kick [options]
  (let [{:keys [frequency end-frequency pitch-decay duration decay gain
                click-gain click-decay click-frequency]
         :as options}
        (normalize-kick-options options)
        t (cgen-double-symbol 't)
        delta-frequency (- frequency end-frequency)
        inverse-pitch-decay (/ 1.0 pitch-decay)
        inverse-decay (/ 1.0 decay)
        inverse-click-decay (/ 1.0 click-decay)
        phase-cycles (if (zero? delta-frequency)
                       `(* ~end-frequency ~t)
                       `(+ (* ~end-frequency ~t)
                           (* ~(* delta-frequency pitch-decay)
                              (- 1.0
                                 (Math/exp (* ~(- inverse-pitch-decay) ~t))))))
        body (when (not (zero? gain))
               `(* ~gain
                   (Math/exp (* ~(- inverse-decay) ~t))
                   (Math/sin (* ~tau ~phase-cycles))))
        click (when (not (zero? click-gain))
                `(* ~click-gain
                    (Math/exp (* ~(- inverse-click-decay) ~t))
                    (Math/sin (* ~(* tau click-frequency) ~t))))
        cgen-body [`(if (or (< ~t 0.0)
                            (>= ~t ~duration))
                      (return 0.0)
                      (let [phase-cycles ~(or phase-cycles 0.0)
                            body ~(or body 0.0)
                            click ~(or click 0.0)]
                        (return (+ body click))))]
        definition (cgen/function-definition (gensym "kick")
                                             [t]
                                             :double
                                             cgen-body)]
    {:definition definition
     :options options}))

(defn render-cgen-kick* [{:keys [definition options]}]
  (let [sample-rate (int (get options :sample-rate sample-rate))
        seconds (double (get options :render-seconds (:duration options)))]
    (when-not (pos? sample-rate)
      (throw (IllegalArgumentException. ":sample-rate must be positive")))
    (when (neg? seconds)
      (throw (IllegalArgumentException. ":render-seconds must not be negative")))
    (render-cgen definition seconds sample-rate)))

(defn render-cgen-kick
  "Create, compile, and render an anonymous specialized native kick.

  Unlike `cgen-kick`, this function accepts a runtime options map. The result
  is a sound map ready for `play!` or `render-wav!`."
  [options]
  (render-cgen-kick* (specialized-kick options)))

(defmacro cgen-kick
  "Compile and render a specialized native kick from a literal options map.

  The generated cgen function is anonymous and is never bound to a Clojure
  Var. Runtime option maps are supported too, but cannot be specialized until
  the call occurs; use `render-cgen-kick` for that case."
  [options]
  (if (map? options)
    (let [specialized (specialized-kick options)]
      `(render-cgen-kick* '~specialized))
    `(render-cgen-kick ~options)))

(defn mix
  "Mix any number of signals by summing their values."
  [& signals]
  (let [signals (vec signals)
        nsignals (count signals)]
    (fn [^double t]
      (loop [i 0
             total 0.0]
        (if (< i nsignals)
          (recur (unchecked-inc i)
                 (+ total (double ((nth signals i) t))))
          total)))))

(defn cents->ratio
  "Convert a pitch offset in cents to a frequency multiplier."
  [cents]
  (Math/pow 2.0 (/ (double cents) 1200.0)))

(defn polyphonic
  "Build a detuned bank of simultaneous voices.

  `synth-factory` is the incoming synth to play.  It receives a frequency in
  Hz and returns a time -> sample function, for example `sine` or `saw`.
  `:voices` controls how many synths are created (64 by default), and
  `:detune-cents` is the maximum pitch offset from the center frequency.
  Voices are spread evenly across that range.

  The voices are averaged to avoid an immediate increase in amplitude.
  `:gain` can be used to compensate for that attenuation.

  Examples:

      (polyphonic sine {:frequency 220 :voices 64 :detune-cents 12})
      (polyphonic saw 220)"
  ([synth-factory frequency-or-options]
   (let [{:keys [frequency voices detune-cents]
          :or {voices 64 detune-cents 0.0}
          :as options}
         (if (map? frequency-or-options)
           frequency-or-options
           {:frequency frequency-or-options})
         output-gain (double (get options :gain 1.0))]
     (when-not (fn? synth-factory)
       (throw (IllegalArgumentException.
               "synth-factory must be a function of frequency")))
     (when-not (and (integer? voices) (pos? voices))
       (throw (IllegalArgumentException.
               ":voices must be a positive integer")))
     (when-not (and (number? frequency) (pos? frequency))
       (throw (IllegalArgumentException.
               ":frequency must be a positive number")))
     (let [frequency (double frequency)
           voices (int voices)
           detune-cents (double detune-cents)
           center (/ (dec voices) 2.0)
           detuned-frequencies
           (map (fn [voice]
                  (let [position (- voice center)
                        normalized-position (if (zero? center)
                                              0.0
                                              (/ position center))]
                    (* frequency
                       (cents->ratio (* detune-cents normalized-position)))))
                (range voices))
           signals (map synth-factory detuned-frequencies)]
       (gain (* output-gain (/ 1.0 voices)) (apply mix signals))))))

(defn trigger-sequence
  "Create a synth which triggers a sequence of overlapping synth instances.

  `:synth` is a frequency -> (time -> sample) factory.  Each trigger uses
  the next multiplier in `:freq-multipliers`, cycling when necessary.  Each
  resulting synth is a `polyphonic` bank with `:voices` detuned copies.

  `:steps` is the number of triggers.  The first trigger starts at time zero;
  successive triggers are separated by `:wait` seconds.  `:randomness` in
  the range 0..1 applies symmetric jitter to each interval, so an interval
  is chosen from `wait * (1 - randomness)` through
  `wait * (1 + randomness)`.  The schedule is generated when this factory is
  called, making one returned synth stable for the whole render."
  [{:keys [synth base-frequency freq-multipliers voices detune-cents steps
           wait randomness seed]
    :or {voices 64
         detune-cents 0.0
         steps 1
         wait 1.0
         randomness 0.0}
    :as options}]
  (let [output-gain (double (get options :gain 1.0))]
    (when-not (fn? synth)
      (throw (IllegalArgumentException.
              ":synth must be a function of frequency")))
    (when-not (and (number? base-frequency) (pos? base-frequency))
      (throw (IllegalArgumentException.
              ":base-frequency must be a positive number")))
    (when-not (and (seq freq-multipliers)
                   (every? #(and (number? %) (pos? %)) freq-multipliers))
      (throw (IllegalArgumentException.
              ":freq-multipliers must be a non-empty sequence of positive numbers")))
    (when-not (and (integer? steps) (pos? steps))
      (throw (IllegalArgumentException.
              ":steps must be a positive integer")))
    (when-not (and (number? wait) (not (neg? wait)))
      (throw (IllegalArgumentException.
              ":wait must be a non-negative number")))
    (when-not (and (number? randomness) (<= 0.0 randomness 1.0))
      (throw (IllegalArgumentException.
              ":randomness must be a number between 0 and 1")))
    (let [base-frequency (double base-frequency)
          freq-multipliers (vec (map double freq-multipliers))
          n-multipliers (count freq-multipliers)
          steps (int steps)
          wait (double wait)
          randomness (double randomness)
          random (if (some? seed)
                   (java.util.Random. (long seed))
                   (java.util.Random.))
          interval (fn []
                     (* wait
                        (+ (- 1.0 randomness)
                           (* 2.0 randomness (.nextDouble random)))))
          events (loop [step 0
                        start 0.0
                        events []]
                   (if (= step steps)
                     events
                     (let [frequency (* base-frequency
                                        (nth freq-multipliers
                                             (mod step n-multipliers)))
                           voice-synth (polyphonic synth
                                                   {:frequency frequency
                                                    :voices voices
                                                    :detune-cents detune-cents})]
                       (recur (inc step)
                              (if (< (inc step) steps)
                                (+ start (interval))
                                start)
                              (conj events {:start start
                                            :synth voice-synth})))))]
      (gain output-gain
            (apply mix
                   (map (fn [{:keys [start synth]}]
                          (fn [^double t]
                            (if (< t start)
                              0.0
                              (synth (- t start)))))
                        events))))))

(defn adsr
  "Create an attack/decay/sustain/release envelope.

  `duration` is the total note duration in seconds.  The release begins
  `:release` seconds before the end of the note."
  [duration {:keys [attack decay sustain release]
             :or {attack 0.01 decay 0.1 sustain 0.7 release 0.2}}]
  (let [duration (double duration)
        attack (max 0.0 (double attack))
        decay (max 0.0 (double decay))
        sustain (double sustain)
        release (max 0.0 (double release))
        decay-end (+ attack decay)
        release-start (max decay-end (- duration release))]
    (fn [t]
      (let [t (double t)]
        (cond
          (<= t 0.0) 0.0
          (< t attack) (/ t (max attack Double/MIN_VALUE))
          (< t decay-end) (+ 1.0 (* (- sustain 1.0)
                                    (/ (- t attack) (max decay Double/MIN_VALUE))))
          (< t release-start) sustain
          (< t duration) (* sustain
                            (- 1.0 (/ (- t release-start)
                                      (max (- duration release-start)
                                           Double/MIN_VALUE))))
          :else 0.0)))))

(defn- pcm-bytes
  [{:keys [^doubles samples] :as sound}]
  (when-not (and (map? sound) (some? samples))
    (throw (IllegalArgumentException.
            "expected a rendered sound; call (render synth duration) first")))
  (let [nframes (alength samples)
        bytes (byte-array (* 2 nframes))
        buffer (doto (ByteBuffer/wrap bytes)
                 (.order ByteOrder/LITTLE_ENDIAN))]
    (dotimes [i nframes]
      (let [sample (max -1.0 (min 1.0 (aget samples i)))
            value (if (neg? sample)
                    (* sample 32768.0)
                    (* sample 32767.0))]
        (.putShort buffer (short (Math/round value)))))
    bytes))

(defn- audio-format
  [sample-rate]
  (AudioFormat. (float sample-rate) 16 1 true false))

(defn render-wav!
  "Write a rendered sound map to `path` as a mono 16-bit little-endian WAV."
  [{:keys [sample-rate] :as sound} path]
  (let [bytes (pcm-bytes sound)
        nframes (quot (alength bytes) 2)
        input (AudioInputStream. (ByteArrayInputStream. bytes)
                                 (audio-format sample-rate)
                                 nframes)
        file (io/file path)]
    (try
      (AudioSystem/write input AudioFileFormat$Type/WAVE file)
      path
      (finally
        (.close input)))))

(defn play!
  "Play a rendered sound through the system's default speakers.

  With one argument, the argument must be a sound map returned by `render`.
  With two arguments, provide the duration and synth directly.  Returns the
  `Clip` immediately; playback happens asynchronously.  The clip closes
  itself when playback finishes."
  ([sound]
   (let [{:keys [sample-rate]} sound
         bytes (pcm-bytes sound)
         format (audio-format sample-rate)
         line-info (DataLine$Info. Clip format)
         clip ^Clip (AudioSystem/getLine line-info)]
     (.open clip format bytes 0 (alength bytes))
     (.addLineListener clip
                       (reify LineListener
                         (update [_ event]
                           (when (= (.getType ^LineEvent event) LineEvent$Type/STOP)
                             (.close clip)))))
     (.start clip)
     clip))
  ([seconds synth]
   (play! (render synth seconds))))

(defn demo
  "A one-second additive tone, useful as a first REPL experiment."
  []
  (let [duration 1.0
        fundamental (sine 220.0)
        harmonic (sine 440.0)
        envelope (adsr duration {:attack 0.03
                                 :decay 0.15
                                 :sustain 0.65
                                 :release 0.2})]
    (render (fn [t]
              (* (envelope t)
                 (+ (* 0.7 (fundamental t))
                    (* 0.25 (harmonic t)))))
            duration)))

(defn render-demo!
  "Render the demo to `path` (default: /tmp/soundgen-demo.wav)."
  ([]
   (render-demo! "/tmp/soundgen-demo.wav"))
  ([path]
   (render-wav! (demo) path)))

(def demo-sounds
  "Zero-argument renderers for quick REPL auditioning."
  {:sine
   (fn []
     (render (gain 0.25 (sine 220.0)) 1.0))

   :saw
   (fn []
     (render (gain 0.15 (saw 110.0)) 1.0))

   :poly
   (fn []
     (-> (polyphonic saw {:frequency 40
                          :voices 64
                          :detune-cents 7})
         (render 32)))

   :trigger-sequence
   (fn []
     (render (trigger-sequence {:synth saw
                                :base-frequency 30.0
                                :freq-multipliers [1.0 2.0 3.0 1.5]
                                :voices 64
                                :detune-cents 10.0
                                :steps 12
                                :wait 1.8
                                :randomness 0.25
                                :seed 7})
             24.0))

   :kick1
   (fn []
     (render (kick {}) 0.5))

   :kick2
   (fn []
     (render (kick {:frequency 170.0
                    :end-frequency 38.0
                    :pitch-decay 0.035
                    :decay 0.3
                    :click-gain 0.2})
             0.6))

   :kick3
   (fn []
     (cgen-kick {:frequency 170.0
                 :end-frequency 38.0
                 :pitch-decay 0.035
                 :duration 0.5
                 :decay 0.3
                 :click-gain 0.2
                 :render-seconds 0.6}))

   :detuned-kicks
   (fn []
     (render (polyphonic #(kick {:frequency %})
                         {:frequency 140.0
                          :voices 8
                          :detune-cents 9.0})
             0.6))})

(defn play-demo-sound!
  [key]
  (let [sound-factory (demo-sounds key)
        sound (sound-factory)]
    (play! sound)))

(comment
  ;; Evaluate this form in the REPL, then open the resulting file in a player:
  ;; (render-demo! "/tmp/soundgen-demo.wav")
  ;;
  ;; Or build a sound directly:
  ;; (def tone (render (gain 0.2 (sine 110)) 2.0))
  ;; (render-wav! tone "/tmp/tone.wav")
  ;;
  ;; Change :kick2 to audition another entry in demo-sounds:
  ;; (play! ((:kick2 demo-sounds)))
  ;;
  ;; A single kick:
  ;; (play! (render (kick {:frequency 160 :end-frequency 42}) 0.5))
  ;;
  ;; Detuned kick voices:
  ;; (def kick-cloud
  ;;   (polyphonic #(kick {:frequency %})
  ;;               {:frequency 140 :voices 8 :detune-cents 9}))
  ;; (play! (render kick-cloud 0.5))
  (play-demo-sound! :sine)
  (play-demo-sound! :saw)
  (play-demo-sound! :poly)
  (play-demo-sound! :trigger-sequence)
  (play-demo-sound! :kick1)
  (play-demo-sound! :kick2)
  (play-demo-sound! :kick3)
  (play-demo-sound! :detuned-kicks))
