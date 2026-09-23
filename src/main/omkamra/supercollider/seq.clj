(ns omkamra.supercollider.seq
  "Core.async helpers for concurrent players sharing a performance clock."
  (:require [clojure.core.async :as async]
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
    (let [now (performance/logical-now (:session player))
          delay-ms (max 0.0 (* 1000.0 (- time now)))]
      (async/timeout (long (Math/ceil delay-ms))))))

(defn sleep
  "Return a channel that delivers after `duration` logical seconds.

  This is deliberately non-blocking. A player routine should use:

  ```clojure
  (<! (sleep player 1.0))
  ```

  Other core.async players continue running while this player is parked."
  [player duration]
  (require-player player)
  (let [duration (require-duration duration)]
    (wait-until player (+ (performance/player-time player) duration))))

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
   (performance/create-player
    (:session parent)
    (merge {:logical-time (performance/player-time parent)} options))))

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
        done (body-fn player)]
    {:type :player-handle
     :player player
     :done done}))

