(ns omkamra.supercollider.session
  "scsynth sessions and their shared server resources."
  (:require [omkamra.supercollider.clock :as clock]
            [omkamra.supercollider.scsynth :as scsynth]
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
                          (scsynth/status connection)
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

(defn- external-instance?
  [instance]
  (and (map? instance)
       (some? (:pid instance))
       (map? (:params instance))))

(defn start!
  "Start or wrap scsynth, connect to it, and make a current session.

  Options:
  * `:scsynth` — map accepted by `omkamra.supercollider.scsynth/start`;
    this starts a process owned by the session.
  * `:instance` — an existing scsynth process descriptor, as returned by
    `omkamra.supercollider.scsynth/start`; this wraps an external process and
    does not stop it when the session ends.
  * `:lookahead-ms` — amount by which OSC bundles lead real time (default 100)
  * `:startup-delay-ms` — initial process boot delay (default 500)
  * `:startup-timeout-ms` — readiness timeout after that delay (default 5000)

  `:scsynth` and `:instance` are mutually exclusive. Prefer
  `ensure-session!` in normal code; `start!` rejects replacing a live current
  session accidentally."
  ([]
   (start! {}))
  ([{:keys [scsynth instance lookahead-ms startup-delay-ms startup-timeout-ms
            bpm tempo-map]
     :or {lookahead-ms default-lookahead-ms
          startup-delay-ms default-startup-delay-ms
          startup-timeout-ms default-startup-timeout-ms
          bpm 120.0}}]
   (when @current-session*
     (throw (IllegalStateException.
             "a current scsynth session already exists; call stop! first")))
   (when (and scsynth instance)
     (throw (IllegalArgumentException.
             ":scsynth and :instance are mutually exclusive")))
   (when (and scsynth (not (map? scsynth)))
     (throw (IllegalArgumentException. ":scsynth must be a map")))
   (when (and instance (not (external-instance? instance)))
     (throw (IllegalArgumentException.
             ":instance must be an scsynth process descriptor")))
   (require-positive-number ":lookahead-ms" lookahead-ms)
   (require-positive-number ":startup-delay-ms" startup-delay-ms)
   (require-positive-number ":startup-timeout-ms" startup-timeout-ms)
   (let [process-owned? (nil? instance)
         process (or instance (scsynth/start (or scsynth {})))
         _ (when process-owned?
             (Thread/sleep (long startup-delay-ms)))
         connection (scsynth/connect process)]
     (try
       (await-ready! connection (long startup-timeout-ms))
       (let [logical-clock (clock/create (cond-> {:bpm bpm}
                                           tempo-map (assoc :tempo-map tempo-map)))
             session {:process process
                      :process-owned? process-owned?
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
         (try (scsynth/close connection) (catch Throwable _))
         (when process-owned?
           (try (scsynth/stop process) (catch Throwable _)))
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

(defn stop!
  "Close and stop the current scsynth session. Returns true when one existed."
  []
  (locking current-session*
    (when-let [{:keys [connection process process-owned? clocks]} @current-session*]
      (reset! current-session* nil)
      (try (scsynth/close connection) (catch Throwable _))
      (doseq [clock-value @clocks]
        (clock/stop! clock-value))
      (when (and process process-owned?)
        (try (scsynth/stop process) (catch Throwable _)))
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
          (let [reply (scsynth/d_recv (:connection session)
                                      (synthdef/serialize definition))]
            (when (:error reply)
              (throw (ex-info "scsynth rejected SynthDef"
                              {:name name :reply reply})))
            (swap! (:loaded-synthdefs session) assoc name definition))))))
  session)

