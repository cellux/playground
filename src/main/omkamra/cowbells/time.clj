(ns omkamra.cowbells.time
  (:import [java.util.concurrent TimeUnit]))

(def thread-sleep-precision-ns
  (.toNanos TimeUnit/MILLISECONDS 2))

(defn nanosleep
  [ns]
  (let [end (+ (System/nanoTime) ns)]
    (loop [time-left ns]
      (when (pos? time-left)
        (if (> time-left thread-sleep-precision-ns)
          (Thread/sleep 1)
          (Thread/yield))
        (recur (- end (System/nanoTime)))))))

(def ^:private tpb
  "Number of ticks per beat"
  64)

(defn beats->ms
  [beats bpm]
  (let [beats-per-second (/ bpm 60)
        seconds-per-beat (/ 1.0 beats-per-second)
        ms-per-beat (* 1000 seconds-per-beat)]
    (* beats ms-per-beat)))

(defn beats->ns
  [beats bpm]
  (-> (beats->ms beats bpm)
      (* 1000 1000)))

(defn beats->ticks
  [beats]
  (* beats tpb))

(defn ticks->ms
  [ticks bpm]
  (let [beats-per-tick (/ 1.0 tpb)]
    (beats->ms (* ticks beats-per-tick) bpm)))

(defn ticks->ns
  [ticks bpm]
  (-> (ticks->ms ticks bpm)
      (* 1000 1000)))
