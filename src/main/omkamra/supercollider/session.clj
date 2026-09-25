(ns omkamra.supercollider.session
  "scsynth sessions and their shared server resources."
  (:require [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.synth :as synth]
            [omkamra.supercollider.synthdef :as synthdef]))

(def ^:private default-lookahead-ms 100)
(def ^:private default-startup-delay-ms 500)
(def ^:private default-startup-timeout-ms 5000)

(defonce ^:private ^{:doc "The current scsynth performance session, or nil.\n\nA session owns one process/OSC connection, a shared logical clock, SynthDef\nload cache, and node-ID allocator. It deliberately survives namespace reloads\nso REPL evaluation does not start a new server each time."}
  current-session*
  (atom nil))

(defn session?
  [value]
  (and (map? value)
       (:connection value)
       (clock/clock? (:default-clock value))
       (instance? clojure.lang.IAtom (:clocks value))
       (instance? clojure.lang.IAtom (:loaded-synthdefs value))
       (instance? clojure.lang.IAtom (:next-node-id value))
       (instance? clojure.lang.IAtom (:next-player-id value))))

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

(defn- synthdef-definition
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
    (let [definition (synthdef-definition synthdef-var)
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

