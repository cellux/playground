(ns rb.explores.sound.state)

(def ^:dynamic *sr* 48000)
(def ^:dynamic *bpm* 120)

(defn beats->frames
  [beats]
  (let [beats-per-second (/ *bpm* 60)
        seconds-per-beat (/ 1.0 beats-per-second)
        frames-per-beat (* *sr* seconds-per-beat)]
    (* beats frames-per-beat)))

(defn dur->frames
  [x]
  (if (integer? x)
    x
    (beats->frames x)))
