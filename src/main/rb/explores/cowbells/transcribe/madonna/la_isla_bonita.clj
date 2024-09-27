(ns rb.explores.cowbells.transcribe.madonna.la-isla-bonita
  (:require
   [omkamra.cowbells :as cowbells]
   [omkamra.sequencer.targets.fluidsynth]
   [clojure.java.io :as jio])
  (:import
   [java.nio ByteOrder ByteBuffer ShortBuffer FloatBuffer DoubleBuffer]
   [javax.sound.sampled AudioSystem AudioFormat AudioFormat$Encoding Clip])
  (:use
   [rb.explores.cowbells.gm]))

(cowbells/defproject la-isla-bonita
  {:target [:fluidsynth "/usr/share/soundfonts/FluidR3_GM.sf2"]
   :bpm 100})

(set! *warn-on-reflection* true)

(defn load-sound
  [source]
  (let [is (jio/input-stream source)
        ais (AudioSystem/getAudioInputStream is)
        format (.getFormat ais)
        frame-size (.getFrameSize format)
        frame-length (.getFrameLength ais)
        byte-length (* frame-size frame-length)
        byte-data (let [byte-data (byte-array byte-length)
                        bytes-read (.read ais byte-data 0 byte-length)]
                    (assert (= bytes-read byte-length))
                    (.close ais)
                    byte-data)
        channels (.getChannels format)
        sample-rate (.getSampleRate format)
        sample-bits (.getSampleSizeInBits format)
        frame-size (.getFrameSize format)
        big-endian? (.isBigEndian format)
        bb (doto (ByteBuffer/wrap byte-data 0 byte-length)
             (.order (if big-endian?
                       ByteOrder/BIG_ENDIAN
                       ByteOrder/LITTLE_ENDIAN)))
        encoding (.getEncoding format)
        sample-buffer (condp = encoding
                        AudioFormat$Encoding/PCM_SIGNED
                        (case sample-bits
                          8 bb
                          16 (.asShortBuffer bb))
                        AudioFormat$Encoding/PCM_FLOAT
                        (case sample-bits
                          32 (.asFloatBuffer bb)
                          64 (.asDoubleBuffer bb))
                        (throw (ex-info "unsupported AudioFormat.Encoding"
                                        {:encoding encoding})))
        get-float-at-offset (condp = encoding
                              AudioFormat$Encoding/PCM_SIGNED
                              (case sample-bits
                                8 (fn [offset]
                                    (float (/ (.get ^ByteBuffer sample-buffer ^int offset) 128.0)))
                                16 (fn [offset]
                                     (float (/ (.get ^ShortBuffer sample-buffer ^int offset) 32768.0))))
                              AudioFormat$Encoding/PCM_FLOAT
                              (case sample-bits
                                32 (fn [offset]
                                     (float (.get ^FloatBuffer sample-buffer ^int offset)))
                                64 (fn [offset]
                                     (float (.get ^DoubleBuffer sample-buffer ^int offset))))
                              (throw (ex-info "unsupported AudioFormat.Encoding"
                                              {:encoding encoding})))
        clip (doto (AudioSystem/getClip)
               (.open format byte-data 0 byte-length))]
    {:clip clip
     :format format
     :channels channels
     :sample-rate sample-rate
     :sample-bits sample-bits
     :frame-size frame-size
     :frame-length frame-length
     :big-endian? big-endian?
     :byte-data byte-data
     :byte-length byte-length
     :sample-buffer sample-buffer
     :get-float-at-offset get-float-at-offset}))

(def sound (load-sound "/home/rb/audio/analyze/Madonna/La_Isla_Bonita.wav"))

(defn get-float-at-offset
  [sound offset]
  (let [{:keys [get-float-at-offset]} sound]
    (get-float-at-offset offset)))

(defn get-float-at-index
  [sound index]
  (let [{:keys [channels get-float-at-offset]} sound]
    (case (int channels)
      1 (get-float-at-offset index)
      2 (let [offset (* index 2)]
          (/ (+ (get-float-at-offset offset)
                (get-float-at-offset (inc offset)))
             2)))))

(defn create-mono-floats-clip
  [sound]
  (let [{:keys [frame-length sample-rate]} sound
        byte-length (* 2 frame-length)
        mono-bytes (let [byte-data (byte-array byte-length)
                         bb (doto (ByteBuffer/wrap byte-data 0 byte-length)
                              (.order (ByteOrder/nativeOrder)))]
                     (dotimes [index frame-length]
                       (.putShort bb (short (* (get-float-at-index sound index) 32767.0))))
                     byte-data)
        encoding AudioFormat$Encoding/PCM_SIGNED
        sample-bits 16
        channels 1
        frame-size 2
        frame-rate sample-rate
        big-endian-system? (.equals (ByteOrder/nativeOrder) ByteOrder/BIG_ENDIAN)
        format (AudioFormat. encoding
                             sample-rate
                             sample-bits
                             channels
                             frame-size
                             frame-rate
                             big-endian-system?)
        clip (doto (AudioSystem/getClip)
               (.open format mono-bytes 0 byte-length))]
    clip))

(comment
  (def mono-clip (create-mono-floats-clip sound))
  (.start mono-clip)
  (.stop mono-clip))

(defn add-slices
  [sound]
  (let [{:keys [frame-length]} sound]
    (loop [index 0
           last-nonzero-amp 0.0
           slices [{:start 0}]]
      (if (= index frame-length)
        (assoc sound :slices slices)
        (let [amp (double (get-float-at-index sound index))]
          (if (and (pos? amp) (neg? last-nonzero-amp))
            (recur (inc index) amp (conj slices {:start index}))
            (recur (inc index) (if (zero? amp) last-nonzero-amp amp) slices)))))))

(comment
  (def sound (add-slices sound))
  (take 10 (:slices sound)))

(defn add-slice-lengths
  [sound]
  (let [{:keys [frame-length]} sound]
    (update sound :slices (fn [slices]
                            (into []
                                  (for [[s1 s2] (partition 2 1 (conj slices {:start frame-length}))]
                                    (assoc s1 :length (- (:start s2) (:start s1)))))))))

(comment
  (def sound (add-slice-lengths sound))
  (take 10 (:slices sound)))

(defn add-slice-peaks
  [sound]
  (update sound :slices
          (fn [slices]
            (mapv (fn [slice]
                    (let [{:keys [start length]} slice
                          end (+ start length)]
                      (assoc slice :peak
                             (reduce max (map #(abs (get-float-at-index sound %))
                                              (range start end))))))
                  slices))))

(comment
  (def sound (add-slice-peaks sound))
  (take 10 (:slices sound)))

(defn merge-successive-slices-with-peak-under-threshold
  [sound threshold]
  (update sound :slices
          (fn [slices]
            (vec
             (mapcat (fn [p]
                       (if (< (:peak (first p)) threshold)
                         [(assoc (first p)
                                 :length (reduce + (map :length p))
                                 :peak (reduce max (map :peak p)))]
                         p))
                     (partition-by #(< (:peak %) threshold) slices))))))

(def sound
    (-> sound
        add-slices
        add-slice-lengths
        add-slice-peaks
        (merge-successive-slices-with-peak-under-threshold 0.1)))

(comment
  (count (:slices sound)))

(defn start-sound
  ([sound start-index end-index loop-count]
   (let [clip (:clip sound)]
     (.setFramePosition clip start-index)
     (if end-index
       (do
         (.setLoopPoints clip start-index (dec end-index))
         (.loop clip (or loop-count Clip/LOOP_CONTINUOUSLY)))
       (.start clip))))
  ([sound start-index end-index]
   (start-sound sound start-index end-index nil))
  ([sound start-index]
   (start-sound sound start-index nil nil))
  ([sound]
   (start-sound sound 0 nil nil)))

(defn start-slice
  ([sound start-index end-index loop-count]
   (let [clip (:clip sound)
         slices (:slices sound)
         start-index (if start-index (get-in slices [start-index :start]) 0)
         end-index (if end-index (get-in slices [end-index :start]) 0)]
     (.setFramePosition clip start-index)
     (if end-index
       (do
         (.setLoopPoints clip start-index (dec end-index))
         (.loop clip (or loop-count Clip/LOOP_CONTINUOUSLY)))
       (.start clip))))
  ([sound start-index end-index]
   (start-slice sound start-index end-index nil))
  ([sound start-index]
   (start-slice sound start-index nil nil))
  ([sound]
   (start-slice sound 0 nil nil)))

(defn stop-sound
  [sound]
  (doto ^Clip (:clip sound)
    (.stop)))

(comment
  (start-sound sound)
  (start-slice sound 1)
  (stop-sound sound))

(defn start-at
  ([n index]
   (let [indices {0 1
                  1 200
                  2 1550
                  3 3000
                  4 4450}]
     (start-slice sound (or index (indices n)))))
  ([n]
   (start-at n nil)))

(comment
  (start-slice sound 3000 4450)
  (start-at 4)
  (stop-sound sound))

(eof)
