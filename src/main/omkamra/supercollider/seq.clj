(ns omkamra.supercollider.seq
  "Core.async helpers for concurrent players sharing a performance clock."
  (:require [clojure.core.async :as async]
            [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.performance :as performance]))

(defn- require-player
  [player]
  (when-not (and (map? player)
                 (= :player (:type player))
                 (:session player)
                 (:logical-time player))
    (throw (IllegalArgumentException.
            (str "invalid performance player: " (pr-str player)))))
  player)

(defn- require-duration
  [duration]
  (when-not (and (number? duration) (not (neg? duration)))
    (throw (IllegalArgumentException.
            (str "duration must be a non-negative number: "
                 (pr-str duration)))))
  (double duration))

(defn wait-until
  "Return a channel that delivers when `player` reaches logical time `time`.

  Logical time is measured in seconds from the shared performance clock's
  origin. The player's cursor is moved immediately, before the channel parks,
  so subsequent events are scheduled at the requested time. Use this inside a
  `go` or `go-loop` block with `<!`."
  [player time]
  (require-player player)
  (when-not (and (number? time) (not (neg? time)))
    (throw (IllegalArgumentException.
            (str "logical time must be non-negative: " (pr-str time)))))
  (let [time (double time)
        previous (performance/player-time player)]
    (when (< time previous)
      (throw (IllegalArgumentException.
              (str "logical time cannot move backwards from " previous
                   " to " time))))
    (performance/at! player time)
    (async/go
      (async/<! (clock/schedule-wake! (:clock player)
                                       {:seconds time}))
      nil)))

(defmacro wait
  "Park the current perform/player routine for logical seconds.

  This macro is intended for use inside `perform`, where the body runs inside
  a core.async go block. It makes timed code read synchronously."
  [duration]
  `(async/<! (sleep ~duration)))

(defmacro wait-beats
  "Park the current perform/player routine for logical beats."
  [duration]
  `(async/<! (sleep-beats ~duration)))

(defn sleep
  "Return a channel that delivers after logical seconds.

  With one argument, use the dynamically active player. With two arguments,
  use the supplied player explicitly. This function is non-blocking; use
  `wait` for the synchronous-looking go-block form."
  ([duration]
   (sleep (performance/current-player) duration))
  ([player duration]
   (require-player player)
   (let [duration (require-duration duration)]
     (wait-until player (+ (performance/player-time player) duration)))))

(defn wait-until-beat
  "Return a channel that delivers when `player` reaches logical beat `beat`."
  [player beat]
  (require-player player)
  (when-not (and (number? beat) (not (neg? beat)))
    (throw (IllegalArgumentException.
            (str "beat must be non-negative: " (pr-str beat)))))
  (let [beat (double beat)
        current-beat (clock/seconds->beat
                      (:clock player)
                      (performance/player-time player))]
    (when (< beat current-beat)
      (throw (IllegalArgumentException.
              (str "beat cannot move backwards from " current-beat
                   " to " beat))))
    (performance/at! player
                      (clock/beat->seconds (:clock player) beat))
    (async/go
      (let [wake (async/<! (clock/schedule-wake!
                            (:clock player)
                            {:beat beat}))]
        (performance/at! player (:logical-seconds wake))
        nil))))

(defn sleep-beats
  "Return a channel that delivers after logical beats.

  With one argument, use the dynamically active player. With two arguments,
  use the supplied player explicitly."
  ([duration]
   (sleep-beats (performance/current-player) duration))
  ([player duration]
   (require-player player)
   (let [duration (require-duration duration)
         beat (+ (clock/seconds->beat
                  (:clock player)
                  (performance/player-time player))
                 duration)]
     (wait-until-beat player beat))))

(defn current-beat
  ([]
   (current-beat (performance/current-player)))
  ([player]
   (require-player player)
   (clock/seconds->beat (:clock player)
                        (performance/player-time player))))

(defn set-tempo!
  "Set the shared performance tempo beginning at a beat position."
  [player beat bpm]
  (require-player player)
  (clock/set-tempo! (:clock player) beat bpm)
  player)

(defn synth-function
  "Return a SynthDef instancer bound to `player`."
  [player synthdef-var]
  (performance/synth-function (require-player player) synthdef-var))

(defn fork-player
  "Create an independent player sharing the parent's session and clock."
  ([parent]
   (fork-player parent {}))
  ([parent options]
   (require-player parent)
   (let [player-clock (or (:clock options) (:clock parent))
         options (dissoc options :clock)
         inherited-time (if (contains? options :logical-time)
                          (:logical-time options)
                          (if (identical? player-clock (:clock parent))
                            (performance/player-time parent)
                            (clock/now-seconds player-clock)))]
     (performance/create-player
      (:session parent)
      (assoc options
             :clock player-clock
             :logical-time inherited-time)))))

(defn spawn!
  "Create a new core.async player sharing the parent's clock.

  `body-fn` receives the child player and should return a core.async go channel
  representing the routine. The returned handle contains the player and that
  completion channel. A body may create player-bound SynthDef functions with
  `synth-function`.

  ```clojure
  (spawn! player
    (fn [p]
      (let [bass (synth-function p #'bass)]
        (go-loop []
          (<! (sleep p 1.0))
          (bass {:freq 110})
          (recur)))))
  ```"
  [parent body-fn]
  (when-not (ifn? body-fn)
    (throw (IllegalArgumentException. "player body must be a function")))
  (let [player (fork-player parent)
        done (binding [performance/*player* player]
               (body-fn player))]
    {:type :player-handle
     :player player
     :done done}))

