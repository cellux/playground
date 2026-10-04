(ns omkamra.supercollider.player
  "Player records, player-bound synth operations, and logical-time helpers."
  (:require [clojure.core.async :as async]
            [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.session :as session]
            [omkamra.supercollider.scsynth :as scsynth])
  (:refer-clojure :exclude [run!]))

(defrecord Player [type id session clock logical-time target-id add-action])

(def ^:dynamic *player*
  "The player active while a performance routine runs."
  nil)

(defn current-player
  "Return the dynamically bound player for the current routine."
  []
  (or *player*
      (throw (IllegalStateException.
              "no active performance player is dynamically bound"))))

(defn- register-clock!
  [session clock-value]
  (swap! (:clocks session)
         (fn [clocks]
           (if (some #(identical? % clock-value) clocks)
             clocks
             (conj clocks clock-value))))
  clock-value)

(defn create-player
  "Create a Player with an inherited or explicitly supplied clock."
  ([session]
   (create-player session {}))
  ([session {:keys [clock logical-time target-id add-action]
             :or {target-id 1 add-action :head}}]
   (when-not (session/session? session)
     (throw (IllegalArgumentException. "invalid performance session")))
   (let [player-clock (or clock (:default-clock session))
         _ (when-not (clock/clock? player-clock)
             (throw (IllegalArgumentException. "player clock is invalid")))
         logical-time (double (or logical-time
                                  (clock/now-seconds player-clock)))]
     (when (neg? logical-time)
       (throw (IllegalArgumentException.
               ":logical-time must be non-negative")))
     (register-clock! session player-clock)
     (->Player :player
               (swap! (:next-player-id session) inc)
               session
               player-clock
               (atom logical-time)
               target-id
               add-action))))

(declare require-player)

(defn with-player
  "Ensure a session, create a Player, and invoke `f` with it."
  ([f]
   (with-player {} {} f))
  ([session-options player-options f]
   (f (create-player (session/ensure-session! session-options)
                     player-options))))

(defn player-time
  [player]
  (require-player player)
  @(:logical-time player))

(defn at!
  "Set a player's current logical time, in seconds."
  [player logical-time]
  (require-player player)
  (when-not (and (number? logical-time) (not (neg? logical-time)))
    (throw (IllegalArgumentException.
            ":logical-time must be a non-negative number")))
  (reset! (:logical-time player) (double logical-time))
  player)

(defn advance!
  "Advance a player's current logical time by `seconds`."
  [player seconds]
  (require-player player)
  (when-not (and (number? seconds) (not (neg? seconds)))
    (throw (IllegalArgumentException.
            "seconds must be a non-negative number")))
  (swap! (:logical-time player) + (double seconds))
  player)

(defn- require-player
  [player]
  (when-not (and (instance? Player player)
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
        previous (player-time player)]
    (when (< time previous)
      (throw (IllegalArgumentException.
              (str "logical time cannot move backwards from " previous
                   " to " time))))
    (at! player time)
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
   (sleep (current-player) duration))
  ([player duration]
   (require-player player)
   (let [duration (require-duration duration)]
     (wait-until player (+ (player-time player) duration)))))

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
                      (player-time player))]
    (when (< beat current-beat)
      (throw (IllegalArgumentException.
              (str "beat cannot move backwards from " current-beat
                   " to " beat))))
    (at! player
                      (clock/beat->seconds (:clock player) beat))
    (async/go
      (let [wake (async/<! (clock/schedule-wake!
                            (:clock player)
                            {:beat beat}))]
        (at! player (:logical-seconds wake))
        nil))))

(defn sleep-beats
  "Return a channel that delivers after logical beats.

  With one argument, use the dynamically active player. With two arguments,
  use the supplied player explicitly."
  ([duration]
   (sleep-beats (current-player) duration))
  ([player duration]
   (require-player player)
   (let [duration (require-duration duration)
         beat (+ (clock/seconds->beat
                  (:clock player)
                  (player-time player))
                 duration)]
     (wait-until-beat player beat))))

(defn current-beat
  ([]
   (current-beat (current-player)))
  ([player]
   (require-player player)
   (clock/seconds->beat (:clock player)
                        (player-time player))))

(defn set-tempo!
  "Set the shared performance tempo beginning at a beat position."
  [player beat bpm]
  (require-player player)
  (clock/set-tempo! (:clock player) beat bpm)
  player)

(defn- timestamp-for
  [player logical-time]
  (let [clock (:clock player)
        lookahead-ms (:lookahead-ms (:session player))
        lookahead-seconds (/ lookahead-ms 1000.0)
        planned (clock/seconds->instant clock (+ logical-time lookahead-seconds))
        now (clock/now-instant clock)
        minimum (.plusMillis now (long (Math/ceil lookahead-ms)))]
    (if (.isBefore planned minimum) minimum planned)))

(defn synthdef-definition
  "Resolve and validate a SynthDef Var."
  [synthdef-var]
  (when-not (var? synthdef-var)
    (throw (IllegalArgumentException.
            (str "SynthDef reference must be a Var: "
                 (pr-str synthdef-var)))))
  (let [definition @synthdef-var]
    (when-not (and (map? definition) (string? (:name definition)))
      (throw (IllegalArgumentException.
              (str "SynthDef Var does not contain a SynthDef map: "
                   (pr-str synthdef-var)))))
    definition))

(defn- validate-controls!
  ([definition controls]
   (validate-controls! definition controls false))
  ([definition controls require-required?]
   (when-not (map? controls)
     (throw (IllegalArgumentException.
             (str "synth controls must be a map: " (pr-str controls)))))
   (let [known (set (map :name (:params definition)))
         provided (set (map name (keys controls)))
         unknown (seq (remove #(contains? known (name %)) (keys controls)))
         required (set (keep #(when (:required %) (:name %)) (:params definition)))
         missing (seq (remove provided required))]
     (when unknown
       (throw (IllegalArgumentException.
               (str "unknown controls for " (:name definition) ": " unknown))))
     (when (and require-required? missing)
       (throw (IllegalArgumentException.
               (str "missing required controls for " (:name definition) ": " missing))))
     controls)))

(defn instantiate!
  "Schedule a SynthDef Var as a new synth at the player's logical time."
  [player synthdef-var controls]
  (let [{:keys [session target-id add-action]} (require-player player)
        definition (synthdef-definition synthdef-var)
        controls (validate-controls! definition controls true)
        id (swap! (:next-node-id session) inc)
        logical-time (player-time player)
        timestamp (timestamp-for player logical-time)
        control-pairs (mapcat (fn [[control value]] [(name control) value]) controls)
        message (apply scsynth/s-new-message (:name definition) id add-action target-id
                       control-pairs)]
    (scsynth/cmd (:connection session) timestamp message)
    {:type :synth
     :id id
     :name (:name definition)
     :synthdef definition
     :controls controls
     :logical-time logical-time
     :timestamp timestamp
     :player player
     :session session
     :running true}))

(defn set-controls!
  "Schedule control updates for a synth instance at its player's time."
  [instance controls]
  (when-not (and (map? instance) (= :synth (:type instance)))
    (throw (IllegalArgumentException. "invalid synth instance")))
  (let [controls (validate-controls! (:synthdef instance) controls)
        player (:player instance)
        session (:session instance)
        timestamp (timestamp-for player (player-time player))
        message (apply scsynth/n-set-message (:id instance)
                       (mapcat (fn [[control value]] [(name control) value])
                               controls))]
    (scsynth/cmd (:connection session) timestamp message)
    (update instance :controls merge controls)))

(defn set!
  [instance controls]
  (set-controls! instance controls))

(defn run!
  "Schedule a synth run-state change at its player's time."
  [instance running?]
  (when-not (and (map? instance) (= :synth (:type instance)))
    (throw (IllegalArgumentException. "invalid synth instance")))
  (let [player (:player instance)
        session (:session instance)
        timestamp (timestamp-for player (player-time player))
        message (scsynth/n-run-message (:id instance) running?)]
    (scsynth/cmd (:connection session) timestamp message)
    (assoc instance :running (boolean running?))))

(defn free!
  "Schedule a synth node to be freed at its player's time."
  [instance]
  (when-not (and (map? instance) (= :synth (:type instance)))
    (throw (IllegalArgumentException. "invalid synth instance")))
  (let [player (:player instance)
        session (:session instance)
        timestamp (timestamp-for player (player-time player))
        message (scsynth/n-free-message (:id instance))]
    (scsynth/cmd (:connection session) timestamp message)
    (assoc instance :freed true :running false)))

(defn synth-function
  "Return a SynthDef instancer bound to `player`."
  [player synthdef-var]
  (let [player (require-player player)]
    (fn
      ([] (instantiate! player synthdef-var {}))
      ([controls] (instantiate! player synthdef-var controls)))))

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
                            (player-time parent)
                            (clock/now-seconds player-clock)))]
     (create-player
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
        done (binding [*player* player]
               (body-fn player))]
    {:type :player-handle
     :player player
     :done done}))

