(ns omkamra.supercollider.performance
  "REPL-oriented performance sessions and player-bound SynthDef functions."
  (:require [omkamra.supercollider.synth :as synth]
            [omkamra.supercollider.synthdef :as synthdef])
  (:import (java.time Duration Instant)))

(def ^:private default-lookahead-ms 100)
(def ^:private default-startup-delay-ms 500)
(def ^:private default-startup-timeout-ms 5000)

(defonce ^{:doc "The current scsynth performance session, or nil.\n\nA session owns one process/OSC connection, a shared logical clock, SynthDef\nload cache, and node-ID allocator. It deliberately survives namespace reloads\nso REPL evaluation does not start a new server each time."}
  current-scsynth
  (atom nil))

(defn- session?
  [value]
  (and (map? value)
       (:connection value)
       (instance? Instant (:clock-origin value))
       (instance? clojure.lang.IAtom (:loaded-synthdefs value))
       (instance? clojure.lang.IAtom (:next-node-id value))))

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
  ([{:keys [scsynth lookahead-ms startup-delay-ms startup-timeout-ms]
     :or {scsynth {}
          lookahead-ms default-lookahead-ms
          startup-delay-ms default-startup-delay-ms
          startup-timeout-ms default-startup-timeout-ms}}]
   (when @current-scsynth
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
       (let [session {:process process
                      :connection connection
                      :clock-origin (Instant/now)
                      :lookahead-ms (double lookahead-ms)
                      :loaded-synthdefs (atom {})
                      :next-node-id (atom 1000)
                      :next-player-id (atom 0)}]
         (reset! current-scsynth session)
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
   (locking current-scsynth
     (or @current-scsynth (start! options)))))

(defn current-session
  "Return the current performance session, or nil."
  []
  @current-scsynth)

(defn set-current-scsynth!
  "Install an already-created performance session as the current session.

  This is primarily useful when connecting a performance to externally managed
  scsynth infrastructure."
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException.
            "current scsynth session has invalid shape")))
  (reset! current-scsynth session))

(defn stop!
  "Close and stop the current scsynth session. Returns true when one existed."
  []
  (locking current-scsynth
    (when-let [{:keys [connection process]} @current-scsynth]
      (reset! current-scsynth nil)
      (try (synth/close connection) (catch Throwable _))
      (when process
        (try (synth/stop process) (catch Throwable _)))
      true)))

(defn logical-now
  "Return the current time in the session's shared logical clock, in seconds."
  [session]
  (when-not (session? session)
    (throw (IllegalArgumentException. "invalid performance session")))
  (/ (.toNanos (Duration/between ^Instant (:clock-origin session)
                                  (Instant/now)))
     1000000000.0))

(defn create-player
  "Create an independent event producer sharing `session`'s logical clock.

  `:logical-time` defaults to the clock's current time. `:target-id` and
  `:add-action` configure the default `/s_new` placement."
  ([session]
   (create-player session {}))
  ([session {:keys [logical-time target-id add-action]
             :or {target-id 1 add-action :head}}]
   (when-not (session? session)
     (throw (IllegalArgumentException. "invalid performance session")))
   (let [logical-time (double (or logical-time (logical-now session)))]
     (when (neg? logical-time)
       (throw (IllegalArgumentException.
               ":logical-time must be non-negative")))
     {:type :player
      :id (swap! (:next-player-id session) inc)
      :session session
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
  (let [{:keys [clock-origin lookahead-ms]} (:session player)
        planned (.plusNanos ^Instant clock-origin
                            (long (Math/round
                                   (* (+ logical-time (/ lookahead-ms 1000.0))
                                      1000000000.0))))
        now (Instant/now)
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
  [definition controls]
  (when-not (map? controls)
    (throw (IllegalArgumentException.
            (str "synth controls must be a map: " (pr-str controls)))))
  (let [known (set (map :name (:params definition)))
        unknown (seq (remove #(contains? known (name %)) (keys controls)))]
    (when unknown
      (throw (IllegalArgumentException.
              (str "unknown controls for " (:name definition) ": " unknown))))
    controls))

(defn instantiate!
  "Schedule a SynthDef Var as a new synth at the player's logical time.

  The returned synth instance contains its allocated node ID, target timestamp,
  controls, player, and session. OSC is always sent in a future-timestamped
  bundle, even for direct `perform` calls."
  [player synthdef-var controls]
  (let [{:keys [session target-id add-action]} player
        definition (synthdef-from-var synthdef-var)
        controls (validate-controls! definition controls)
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
     :session session}))

(defn synth-function
  "Return a player-bound function which instantiates `synthdef-var`."
  [player synthdef-var]
  (fn
    ([] (instantiate! player synthdef-var {}))
    ([controls] (instantiate! player synthdef-var controls))))

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
  that returns a synth instance."
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
           ~@body)))))
