(ns omkamra.supercollider.performance
  "High-level performance helpers and Pattern playback."
  (:require [clojure.core.async :as async]
            [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.pattern :as pattern]
            [omkamra.supercollider.player :as player]
            [omkamra.supercollider.session :as session])
  (:refer-clojure :exclude [run!]))

(def ^:private pattern-event-keys
  #{:instrument :dur :delta :type :rest :stretch})

(defn- pattern-event-value
  [event key]
  (some (fn [candidate]
          (when (contains? event candidate)
            (get event candidate)))
        [key (name key) (symbol (name key))]))

(defn- pattern-event-controls
  [event]
  (reduce (fn [controls key]
            (dissoc controls key (name key) (symbol (name key))))
          event
          pattern-event-keys))

(defn- pattern-duration
  [event]
  (let [duration (or (pattern-event-value event :delta)
                     (pattern-event-value event :dur)
                     1.0)]
    (when-not (and (number? duration) (not (neg? duration)))
      (throw (IllegalArgumentException.
              (str "pattern event duration must be non-negative: "
                   (pr-str duration)))))
    (double duration)))

(defn- pattern-rest?
  [event]
  (or (true? (pattern-event-value event :rest))
      (= :rest (pattern-event-value event :type))
      (= "rest" (pattern-event-value event :type))))

(defn- play-pattern-loop
  [pattern player]
  (async/thread
    (loop [stream (pattern/stream pattern)]
      (if-let [result (pattern/step stream {})]
        (let [event (:value result)
              duration (pattern-duration event)
              current-beat (clock/seconds->beat
                            (:clock player)
                            (player/player-time player))]
          (when-not (pattern-rest? event)
            (let [synthdef-var (pattern-event-value event :instrument)]
              (when-not (var? synthdef-var)
                (throw (IllegalArgumentException.
                        (str "pattern event :instrument must be a SynthDef Var: "
                             (pr-str synthdef-var)))))
              (session/load-synthdefs! (:session player) [synthdef-var])
              (player/instantiate! player synthdef-var
                            (pattern-event-controls event))))
          (let [target-beat (+ current-beat duration)
                target-seconds (clock/beat->seconds (:clock player) target-beat)]
            (player/at! player target-seconds)
            (when-let [wake (async/<!! (clock/schedule-wake!
                                        (:clock player)
                                        {:beat target-beat}))]
              (player/at! player (:logical-seconds wake))
              (recur (:stream result)))))
        :done))))

(defn play-pattern!
  "Play a Pattern, returning a handle with its completion channel.

  The explicit form uses the supplied player. The one-argument form ensures a
  session and creates a player automatically. Pattern events must contain an
  `:instrument` SynthDef Var unless they are rests. `:dur` or `:delta` is
  interpreted as a duration in beats."
  ([pattern]
   (play-pattern! pattern (player/create-player (session/ensure-session!))))
  ([pattern player]
   (when-not (pattern/pattern? pattern)
     (throw (IllegalArgumentException.
             "play-pattern! expects a Pattern")))
   (when-not (and (map? player) (= :player (:type player)))
     (throw (IllegalArgumentException. "play-pattern! expects a player")))
   (let [done (play-pattern-loop pattern player)]
     {:type :pattern-playback
      :pattern pattern
      :player player
      :done done})))

(defmacro perform
  "Ensure scsynth, load named SynthDefs, and run `body` with synth functions.

  ```clojure
  (perform {:synthdefs [bass flute]}
    (bass {:freq 110})
    (flute {:freq 440}))
  ```

  Each symbol in `:synthdefs` must name an unqualified namespace Var created by
  `define-synthdef`. Within `body` it is rebound to a player-bound constructor
  that returns a synth instance. The active player is available through
  `player/current-player` and is dynamically bound while the body runs."
  [options & body]
  (when-not (map? options)
    (throw (IllegalArgumentException. "perform options must be a literal map")))
  (let [synthdefs (:synthdefs options)]
    (when-not (vector? synthdefs)
      (throw (IllegalArgumentException.
              "perform requires a vector at :synthdefs")))
    (when-not (every? #(and (symbol? %) (nil? (namespace %))) synthdefs)
      (throw (IllegalArgumentException.
              "perform :synthdefs must contain unqualified symbols")))
    (when-not (= (count synthdefs) (count (distinct synthdefs)))
      (throw (IllegalArgumentException.
              "perform :synthdefs must not contain duplicates")))
    (let [session-options (dissoc options :synthdefs :target-id :add-action
                                  :logical-time)
          player-options (select-keys options [:target-id :add-action
                                               :logical-time])
          session-symbol (gensym "session")
          player-symbol (gensym "player")]
      `(let [~session-symbol (session/ensure-session! ~session-options)
             ~player-symbol (player/create-player ~session-symbol ~player-options)]
         (session/load-synthdefs! ~session-symbol
                          [~@(map (fn [s] `(var ~s)) synthdefs)])
         (let [~@(mapcat (fn [s]
                            [s `(player/synth-function ~player-symbol (var ~s))])
                          synthdefs)]
           (binding [player/*player* ~player-symbol]
             (async/<!! (async/go
                          ~@body))))))))
