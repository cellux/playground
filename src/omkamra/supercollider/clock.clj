(ns omkamra.supercollider.clock
  "Shared logical clocks and core.async wake scheduling."
  (:require [clojure.core.async :as async])
  (:refer-clojure :exclude [derive])
  (:import (java.time Instant)
           (java.util.concurrent.atomic AtomicLong)))

(def ^:private default-bpm 120.0)

(defn- positive-number!
  [label value]
  (when-not (and (number? value) (pos? value))
    (throw (IllegalArgumentException.
            (str label " must be a positive number: " (pr-str value)))))
  (double value))

(defn- non-negative-number!
  [label value]
  (when-not (and (number? value) (not (neg? value)))
    (throw (IllegalArgumentException.
            (str label " must be non-negative: " (pr-str value)))))
  (double value))

(defn- normalize-tempo-map
  [tempo-map]
  (let [tempo-map (sort-by first tempo-map)]
    (when (or (empty? tempo-map) (not= 0.0 (double (ffirst tempo-map))))
      (throw (IllegalArgumentException.
              "tempo map must contain a tempo at beat 0")))
    (loop [remaining tempo-map
           previous-beat nil]
      (when-let [[beat bpm] (first remaining)]
        (non-negative-number! "tempo beat" beat)
        (when (and previous-beat (<= (double beat) previous-beat))
          (throw (IllegalArgumentException.
                  "tempo-map beats must be strictly increasing")))
        (positive-number! "tempo" bpm)
        (recur (next remaining) (double beat))))
    (vec (map (fn [[beat bpm]] [(double beat) (double bpm)]) tempo-map))))

(defn create
  "Create a shared logical clock.

  Logical time is represented internally as seconds from `:origin`. Beat time
  is mapped through a tempo map, initially using `:bpm` (default 120)."
  ([]
   (create {}))
  ([{:keys [bpm tempo-map]
     :or {bpm default-bpm}}]
   (let [tempo-map (normalize-tempo-map
                    (or tempo-map [[0.0 (positive-number! ":bpm" bpm)]]))]
     {:type :clock
      :origin (Instant/now)
      :monotonic-origin (System/nanoTime)
      :tempo-map (atom tempo-map)
      :wake-requests (async/chan 1024)
      :wake-stop (async/chan)
      :wake-sequence (AtomicLong. 0)
      :scheduler (atom nil)})))

(declare clock?)

(defn derive
  "Create a clock with an independent tempo map and the parent's wall origin.

  Derived clocks stay aligned with their parent in real time while allowing
  players to interpret beats at different tempos."
  ([parent]
   (derive parent {}))
  ([parent options]
   (when-not (clock? parent)
     (throw (IllegalArgumentException. "invalid parent clock")))
   (let [child (create options)]
     (assoc child
            :origin (:origin parent)
            :monotonic-origin (:monotonic-origin parent)))))

(defn clock?
  [value]
  (and (map? value)
       (= :clock (:type value))
       (instance? Instant (:origin value))
       (instance? clojure.lang.IAtom (:tempo-map value))))

(defn now-seconds
  [clock]
  (when-not (clock? clock)
    (throw (IllegalArgumentException. "invalid clock")))
  (/ (- (System/nanoTime) (long (:monotonic-origin clock)))
     1000000000.0))

(defn now-instant
  [clock]
  (.plusNanos ^Instant (:origin clock)
              (long (Math/round (* 1000000000.0 (now-seconds clock))))))

(defn seconds->instant
  [clock seconds]
  (non-negative-number! "logical seconds" seconds)
  (.plusNanos ^Instant (:origin clock)
              (long (Math/round (* 1000000000.0 (double seconds))))))

(defn- tempo-at
  [clock beat]
  (last (take-while #(<= (first %) beat) @(:tempo-map clock))))

(defn beat->seconds
  "Convert a non-negative beat position into logical seconds."
  [clock beat]
  (let [beat (non-negative-number! "beat" beat)
        tempo-segments @(:tempo-map clock)]
    (loop [segments (next tempo-segments)
           previous-beat 0.0
           previous-bpm (second (first tempo-segments))
           seconds 0.0]
      (if-let [[next-beat next-bpm] (first segments)]
        (if (<= beat next-beat)
          (+ seconds (* (- beat previous-beat) (/ 60.0 previous-bpm)))
          (recur (next segments)
                 next-beat
                 next-bpm
                 (+ seconds (* (- next-beat previous-beat)
                               (/ 60.0 previous-bpm)))))
        (+ seconds (* (- beat previous-beat) (/ 60.0 previous-bpm)))))))

(defn seconds->beat
  "Convert logical seconds into a beat position using the tempo map."
  [clock seconds]
  (let [seconds (non-negative-number! "logical seconds" seconds)
        tempo-segments @(:tempo-map clock)]
    (loop [segments (next tempo-segments)
           previous-beat 0.0
           previous-bpm (second (first tempo-segments))
           elapsed 0.0]
      (if-let [[next-beat next-bpm] (first segments)]
        (let [segment-seconds (* (- next-beat previous-beat)
                                 (/ 60.0 previous-bpm))]
          (if (<= seconds (+ elapsed segment-seconds))
            (+ previous-beat (* (- seconds elapsed) (/ previous-bpm 60.0)))
            (recur (next segments)
                   next-beat
                   next-bpm
                   (+ elapsed segment-seconds))))
        (+ previous-beat (* (- seconds elapsed) (/ previous-bpm 60.0)))))))

(defn now-beats
  [clock]
  (seconds->beat clock (now-seconds clock)))

(defn set-tempo!
  "Set the tempo beginning at `beat`, preserving all earlier tempo changes."
  [clock beat bpm]
  (when-not (clock? clock)
    (throw (IllegalArgumentException. "invalid clock")))
  (let [beat (non-negative-number! "tempo beat" beat)
        bpm (positive-number! "tempo" bpm)]
    (swap! (:tempo-map clock)
           (fn [tempo-map]
             (normalize-tempo-map
              (conj (vec (remove #(= beat (first %)) tempo-map))
                    [beat bpm]))))
    clock))

(defn tempo-map
  [clock]
  @(:tempo-map clock))

(defn- wake-seconds
  [clock {:keys [seconds beat]}]
  (if (some? beat)
    (beat->seconds clock beat)
    seconds))

(defn- due-requests
  [clock requests]
  (let [now (now-seconds clock)]
    [(filter #(<= (wake-seconds clock (:target %)) now) requests)
     (remove #(<= (wake-seconds clock (:target %)) now) requests)]))

(defn- scheduler-loop
  [clock]
  (let [requests (:wake-requests clock)
        stop (:wake-stop clock)]
    (loop [pending []]
      (let [pending (sort-by (juxt #(wake-seconds clock (:target %)) :order)
                             pending)
            next-request (first pending)
            delay-ms (when next-request
                       (max 0.0
                            (* 1000.0
                               (- (wake-seconds clock (:target next-request))
                                  (now-seconds clock)))))
            timer (when delay-ms (async/timeout (long (Math/ceil delay-ms))))
            [value port] (async/alts!!
                          (cond-> [requests stop]
                            timer (conj timer)))]
        (cond
          (= port stop)
          (do
            (doseq [{:keys [wake]} pending]
              (async/close! wake))
            (reset! (:scheduler clock) nil))

          (= port requests)
          (recur (conj pending value))

          (= port timer)
          (let [[ready waiting] (due-requests clock pending)]
            (doseq [{:keys [wake target]} ready]
              (async/put! wake {:logical-seconds (wake-seconds clock target)
                                :target target})
              (async/close! wake))
            (recur (vec waiting)))

          :else
          (recur pending))))))

(defn- ensure-scheduler!
  [clock]
  (when (compare-and-set! (:scheduler clock) nil ::starting)
    (let [thread (async/thread (scheduler-loop clock))]
      (reset! (:scheduler clock) thread)))
  clock)

(defn schedule-wake!
  "Return a channel that closes after a logical seconds/beat target.

  The target is a map containing exactly one of `:seconds` or `:beat`."
  [clock target]
  (when-not (and (map? target)
                 (or (contains? target :seconds)
                     (contains? target :beat)))
    (throw (IllegalArgumentException.
            "wake target must contain :seconds or :beat")))
  (when (and (contains? target :seconds) (contains? target :beat))
    (throw (IllegalArgumentException.
            "wake target cannot contain both :seconds and :beat")))
  (when (contains? target :seconds)
    (non-negative-number! "logical seconds" (:seconds target)))
  (when (contains? target :beat)
    (non-negative-number! "beat" (:beat target)))
  (ensure-scheduler! clock)
  (let [wake (async/chan 1)
        request {:target target
                 :wake wake
                 :order (.incrementAndGet ^AtomicLong (:wake-sequence clock))}]
    (if (async/put! (:wake-requests clock) request)
      wake
      (throw (IllegalStateException. "clock scheduler is stopped")))))

(defn stop!
  "Stop the clock's wake scheduler and close pending wake channels."
  [clock]
  (when (clock? clock)
    (async/close! (:wake-stop clock))
    true))
