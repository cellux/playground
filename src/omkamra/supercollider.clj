(ns omkamra.supercollider
  "Discoverable entrypoint for SuperCollider sessions and performances.

  Start here to start or wrap a session, inspect shared session time, create
  players, define SynthDefs/UGens, or run a performance. Once a player is
  returned, use `omkamra.supercollider.player` for time, scheduling, and synth
  manipulation. This namespace contains only lightweight proxies; behavior
  lives in `session`, `player`, `synthdef`, `ugen`, and `performance`."
  (:require [omkamra.supercollider.performance :as performance]
            [omkamra.supercollider.player :as player]
            [omkamra.supercollider.session :as session]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen :as ugen]))

;; Session lifecycle and inspection ------------------------------------------

(defn session?
  "Return whether `value` is a valid SuperCollider session."
  [value]
  (session/session? value))

(defn start-session!
  "Start or wrap scsynth and make a current session."
  ([]
   (session/start!))
  ([options]
   (session/start! options)))

(defn ensure-session!
  "Return the current session, starting one when necessary."
  ([]
   (session/ensure-session!))
  ([options]
   (session/ensure-session! options)))

(defn current-session
  "Return the current session, or nil."
  []
  (session/current-session))

(defn stop-session!
  "Close the current session and stop its owned scsynth process."
  []
  (session/stop!))

(defn logical-now
  "Return the current logical time for a session in seconds."
  [session-value]
  (session/logical-now session-value))

(defn now-beats
  "Return the current shared clock position for a session in beats."
  [session-value]
  (session/now-beats session-value))

(defn set-tempo!
  "Set a session's shared clock tempo beginning at a beat."
  [session-value beat bpm]
  (session/set-tempo! session-value beat bpm))

(defn tempo-map
  "Return a session's shared clock tempo map."
  [session-value]
  (session/tempo-map session-value))

(defn load-synthdefs!
  "Load SynthDef Vars into a session, skipping unchanged definitions."
  [session-value synthdef-vars]
  (session/load-synthdefs! session-value synthdef-vars))

;; Player lifecycle -----------------------------------------------------------

(defn current-player
  "Return the dynamically active player."
  []
  (player/current-player))

(defn create-player
  "Create a player for a session."
  ([session-value]
   (player/create-player session-value))
  ([session-value options]
   (player/create-player session-value options)))

(defn with-player
  "Ensure a session, create a player, and invoke `f` with it."
  ([f]
   (player/with-player f))
  ([session-options player-options f]
   (player/with-player session-options player-options f)))

(defn fork-player
  "Create a player sharing a parent's session and clock."
  ([parent]
   (player/fork-player parent))
  ([parent options]
   (player/fork-player parent options)))

;; Definition and performance macros -----------------------------------------

(defmacro define-synthdef
  "Define a named SynthDef using lexical control bindings."
  [name args & body]
  `(synthdef/define-synthdef ~name ~args ~@body))

(defmacro define
  "Alias for `define-synthdef`."
  [name args & body]
  `(synthdef/define ~name ~args ~@body))

(defmacro define-ugen
  "Generate a UGen constructor and rate-specific aliases."
  [constructor-name metadata-key]
  `(ugen/define-ugen ~constructor-name ~metadata-key))

(defmacro perform
  "Ensure a session and run a scheduled SuperCollider performance."
  [options & body]
  `(performance/perform ~options ~@body))
