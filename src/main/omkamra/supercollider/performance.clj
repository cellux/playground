(ns omkamra.supercollider.performance
  "REPL-oriented performance sessions and player-bound SynthDef functions."
  (:require [clojure.core.async :as async]
            [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.pattern :as pattern]
            [omkamra.supercollider.synth :as synth]
            [omkamra.supercollider.synthdef :as synthdef])
  (:import (java.time Instant))
  (:refer-clojure :exclude [run!]))

(def ^:private default-lookahead-ms 100)
(def ^:private default-startup-delay-ms 500)
(def ^:private default-startup-timeout-ms 5000)

(defonce ^:private ^{:doc "The current scsynth performance session, or nil.\n\nA session owns one process/OSC connection, a shared logical clock, SynthDef\nload cache, and node-ID allocator. It deliberately survives namespace reloads\nso REPL evaluation does not start a new server each time."}
  current-session*
  (atom nil))

(def ^:dynamic *player*
  "The player active while a performance body or player routine runs."
  nil)

(defn current-player
  "Return the dynamically bound player for the current performance routine."
  []
  (or *player*
      (throw (IllegalStateException.
              "no active performance player is dynamically bound"))))

(defn- session?
  [value]
  (and (map? value)
       (:connection value)
       (clock/clock? (:default-clock value))
       (instance? clojure.lang.IAtom (:clocks value))
       (instance? clojure.lang.IAtom (:loaded-synthdefs value))
       (instance? clojure.lang.IAtom (:next-node-id value))))

(defn- register-clock!
  [session clock-value]
  (swap! (:clocks session)
         (fn [clocks]
           (if (some #(identical? % clock-value) clocks)
             clocks
             (conj clocks clock-value))))
  clock-value)

(defn- require-positive-number
  [label value]
  (when-not (and (number? value) (pos? value))
    (throw (IllegalArgumentException.
            (str label " must be a positive number: " (pr-str value))))
  value))

(defn- await-ready!
  [connection timeout-ms]
  (let [result (deref (future
                        (try
                          (synth/status connection)
                          (catch Throwable throwable
                            throwable)))
                      timeout-ms
                      ::timed-out)]
    (cond
      (= ::timed-out result)
      (throw (ex-info "timed out waiting for scsynth"
                      {:timeout-ms timeout-ms}))

      (instance? Throwable result)
      (throw (ex-info "scsynth did not become ready" {} result))

      :else result)))

(defn start!
  "Start scsynth, connect to it, and make a new current performance session.

  Options:
  * `:scsynth` — map accepted by `omkamra.supercollider.synth/start`
  * `:lookahead-ms` — amount by which OSC bundles lead real time (default 100)
  * `:startup-delay-ms` — initial process boot delay (default 500)
  * `:startup-timeout-ms` — readiness timeout after that delay (default 5000)

  Prefer `ensure-session!` in normal code; `start!` rejects replacing a live
  current session accidentally."
  ([]
   (start! {}))
  ([{:keys [scsynth lookahead-ms startup-delay-ms startup-timeout-ms bpm tempo-map]
     :or {scsynth {}
          lookahead-ms default-lookahead-ms
          startup-delay-ms default-startup-delay-ms
          startup-timeout-ms default-startup-timeout-ms
          bpm 120.0}}]
   (when @current-session*
     (throw (IllegalStateException.
             "a current scsynth session already exists; call stop! first")))
   (when-not (map? scsynth)
     (throw (IllegalArgumentException. ":scsynth must be a map")))
   (require-positive-number ":lookahead-ms" lookahead-ms)
   (require-positive-number ":startup-delay-ms" startup-delay-ms)
   (require-positive-number ":startup-timeout-ms" startup-timeout-ms)
   (let [process (synth/start scsynth)
         _ (Thread/sleep (long startup-delay-ms))
         connection (synth/connect process)]
     (try
       (await-ready! connection (long startup-timeout-ms))
       (let [logical-clock (clock/create (cond-> {:bpm bpm}
                                           tempo-map (assoc :tempo-map tempo-map)))
             session {:process process
                      :connection connection
                      :default-clock logical-clock
                      :clocks (atom [logical-clock])
                      :lookahead-ms (double lookahead-ms)
                      :loaded-synthdefs (atom {})
                      :next-node-id (atom 1000)
                      :next-player-id (atom 0)}]
         (reset! current-session* session)
         session)
       (catch Throwable throwable
         (try (synth/close connection) (catch Throwable _))
         (try (synth/stop process) (catch Throwable _))
         (throw throwable))))))

(defn ensure-session!
  "Return the current session, starting scsynth when no session exists."
  ([]
   (ensure-session! {}))
  ([options]
   (locking current-session*
     (or @current-session* (start! options)))))

(defn current-session
  "Return the current performance session, or nil."
  []
  @current-session*)

(defn set-current-session!
  "Install an already-created performance session as the current session.

  This is primarily useful when connecting a performance to externally managed
  scsynth infrastructure."
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException.
            "current scsynth session has invalid shape")))
  (reset! current-session* session))

(defn stop!
  "Close and stop the current scsynth session. Returns true when one existed."
  []
  (locking current-session*
    (when-let [{:keys [connection process clocks]} @current-session*]
      (reset! current-session* nil)
      (try (synth/close connection) (catch Throwable _))
      (doseq [clock-value @clocks]
        (clock/stop! clock-value))
      (when process
        (try (synth/stop process) (catch Throwable _)))
      true)))

(defn logical-now
  "Return the current time in the session's shared logical clock, in seconds."
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (clock/now-seconds (:default-clock session)))

(defn now-beats
  "Return current shared clock position in beats."
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (clock/now-beats (:default-clock session)))

(defn set-tempo!
  "Set the shared clock tempo beginning at `beat`."
  [session beat bpm]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (clock/set-tempo! (:default-clock session) beat bpm)
  session)

(defn tempo-map
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (clock/tempo-map (:default-clock session)))

(defn create-player
  "Create an event producer with an inherited or explicitly supplied clock.

  `:clock` defaults to the session's default clock. `:logical-time` defaults to
  that clock's current time. `:target-id` and `:add-action` configure the
  default `/s_new` placement."
  ([session]
   (create-player session {}))
  ([session {:keys [clock logical-time target-id add-action]
             :or {target-id 1 add-action :head}}]
   (when-not (session? session)
     (throw (IllegalArgumentException. "invalid performance session")))
   (let [player-clock (or clock (:default-clock session))
         _ (when-not (clock/clock? player-clock)
             (throw (IllegalArgumentException. "player clock is invalid")))
         logical-time (double (or logical-time (clock/now-seconds player-clock)))]
     (when (neg? logical-time)
       (throw (IllegalArgumentException.
               ":logical-time must be non-negative")))
     (register-clock! session player-clock)
     {:type :player
      :id (swap! (:next-player-id session) inc)
      :session session
      :clock player-clock
      :logical-time (atom logical-time)
      :target-id target-id
      :add-action add-action})))

(defn player-time
  [player]
  @(:logical-time player))

(defn at!
  "Set a player's current logical time, in seconds."
  [player logical-time]
  (when-not (and (number? logical-time) (not (neg? logical-time)))
    (throw (IllegalArgumentException.
            ":logical-time must be a non-negative number")))
  (reset! (:logical-time player) (double logical-time))
  player)

(defn advance!
  "Advance a player's current logical time by `seconds`."
  [player seconds]
  (when-not (and (number? seconds) (not (neg? seconds)))
    (throw (IllegalArgumentException.
            "seconds must be a non-negative number")))
  (swap! (:logical-time player) + (double seconds))
  player)

(defn- timestamp-for
  [player logical-time]
  (let [clock (:clock player)
        lookahead-ms (:lookahead-ms (:session player))
        lookahead-seconds (/ lookahead-ms 1000.0)
        planned (clock/seconds->instant clock (+ logical-time lookahead-seconds))
        now (clock/now-instant clock)
        minimum (.plusMillis now (long (Math/ceil lookahead-ms)))]
    ;; An already-past logical event must not be sent late. Keep the same lead
    ;; time used for ordinary events so scsynth can execute it jitter-free.
    (if (.isBefore planned minimum) minimum planned)))

(defn- synthdef-from-var
  [synthdef-var]
  (when-not (var? synthdef-var)
    (throw (IllegalArgumentException.
            (str "SynthDef reference must be a Var: " (pr-str synthdef-var)))))
  (let [definition @synthdef-var]
    (when-not (and (map? definition) (string? (:name definition)))
      (throw (IllegalArgumentException.
              (str "SynthDef Var does not contain a SynthDef map: "
                   (pr-str synthdef-var)))))
    definition))

(defn load-synthdefs!
  "Load the SynthDefs held by `synthdef-vars` into `session`.

  Unchanged definitions are skipped. A changed value under the same SynthDef
  name is reloaded, which keeps namespace re-evaluation REPL-friendly."
  [session synthdef-vars]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (when-not (sequential? synthdef-vars)
    (throw (IllegalArgumentException. "SynthDef references must be sequential")))
  (doseq [synthdef-var synthdef-vars]
    (let [definition (synthdef-from-var synthdef-var)
          name (:name definition)]
      (locking (:loaded-synthdefs session)
        (when-not (= definition (get @(:loaded-synthdefs session) name))
          (let [reply (synth/d_recv (:connection session)
                                    (synthdef/serialize definition))]
            (when (:error reply)
              (throw (ex-info "scsynth rejected SynthDef"
                              {:name name :reply reply})))
            (swap! (:loaded-synthdefs session) assoc name definition))))))
  session)

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
  "Schedule a SynthDef Var as a new synth at the player's logical time.

  The returned synth instance contains its allocated node ID, target timestamp,
  controls, player, and session. OSC is always sent in a future-timestamped
  bundle, even for direct `perform` calls."
  [player synthdef-var controls]
  (let [{:keys [session target-id add-action]} player
        definition (synthdef-from-var synthdef-var)
        controls (validate-controls! definition controls true)
        id (swap! (:next-node-id session) inc)
        logical-time (player-time player)
        timestamp (timestamp-for player logical-time)
        control-pairs (mapcat (fn [[control value]] [(name control) value]) controls)
        message (apply synth/s-new-message (:name definition) id add-action target-id
                       control-pairs)]
    (synth/cmd (:connection session) timestamp message)
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
        message (apply synth/n-set-message (:id instance)
                       (mapcat (fn [[control value]] [(name control) value])
                               controls))]
    (synth/cmd (:connection session) timestamp message)
    (update instance :controls merge controls)))

(defn set!
  "Alias for `set-controls!`."
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
        message (synth/n-run-message (:id instance) running?)]
    (synth/cmd (:connection session) timestamp message)
    (assoc instance :running (boolean running?))))

(defn free!
  "Schedule a synth node to be freed at its player's time."
  [instance]
  (when-not (and (map? instance) (= :synth (:type instance)))
    (throw (IllegalArgumentException. "invalid synth instance")))
  (let [player (:player instance)
        session (:session instance)
        timestamp (timestamp-for player (player-time player))
        message (synth/n-free-message (:id instance))]
    (synth/cmd (:connection session) timestamp message)
    (assoc instance :freed true :running false)))

(defn synth-function
  "Return a player-bound function which instantiates `synthdef-var`."
  [player synthdef-var]
  (fn
    ([] (instantiate! player synthdef-var {}))
    ([controls] (instantiate! player synthdef-var controls))))

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
                            (player-time player))]
          (when-not (pattern-rest? event)
            (let [synthdef-var (pattern-event-value event :instrument)]
              (when-not (var? synthdef-var)
                (throw (IllegalArgumentException.
                        (str "pattern event :instrument must be a SynthDef Var: "
                             (pr-str synthdef-var)))))
              (load-synthdefs! (:session player) [synthdef-var])
              (instantiate! player synthdef-var
                            (pattern-event-controls event))))
          (let [target-beat (+ current-beat duration)
                target-seconds (clock/beat->seconds (:clock player) target-beat)]
            (at! player target-seconds)
            (when-let [wake (async/<!! (clock/schedule-wake!
                                        (:clock player)
                                        {:beat target-beat}))]
              (at! player (:logical-seconds wake))
              (recur (:stream result)))))
        :done))))

(defn play-pattern!
  "Play a Pattern, returning a handle with its completion channel.

  The explicit form uses the supplied player. The one-argument form ensures a
  session and creates a player automatically. Pattern events must contain an
  `:instrument` SynthDef Var unless they are rests. `:dur` or `:delta` is
  interpreted as a duration in beats."
  ([pattern]
   (play-pattern! pattern (create-player (ensure-session!))))
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

(defn with-player
  "Ensure a session, create a player, and invoke `f` with that player."
  ([f]
   (with-player {} {} f))
  ([session-options player-options f]
   (f (create-player (ensure-session! session-options) player-options))))

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
  `current-player` and is dynamically bound while the body runs."
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
      `(let [~session-symbol (ensure-session! ~session-options)
             ~player-symbol (create-player ~session-symbol ~player-options)]
         (load-synthdefs! ~session-symbol
                          [~@(map (fn [s] `(var ~s)) synthdefs)])
         (let [~@(mapcat (fn [s]
                            [s `(synth-function ~player-symbol (var ~s))])
                          synthdefs)]
           (binding [*player* ~player-symbol]
             (async/<!! (async/go
                          ~@body))))))))
