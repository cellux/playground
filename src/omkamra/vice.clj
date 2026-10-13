(ns omkamra.vice
  "Public VICE API: process/monitor helpers and the example runner.

  The emulator instance itself is owned by `omkamra.vice.driver`; this
  namespace is the stable public surface over it. `run!` links and injects a
  program, and `omkamra.vice.capture` records it."
  (:refer-clojure :exclude [run!])
  (:require
   [omkamra.vice.assembler :as assembler]
   [omkamra.vice.binary-monitor :as bm]
   [omkamra.vice.capture :as capture]
   [omkamra.vice.driver :as driver]))

(def default-executable driver/default-executable)
(def default-address driver/default-address)
(def default-port driver/default-port)

(defn start
  "Start a VICE emulator asynchronously. See `omkamra.vice.driver/start`."
  ([]
   (driver/start))
  ([params]
   (driver/start params)))

(defn start!
  "Canonical VICE process launcher. See `start`."
  ([] (start! {}))
  ([params] (start (or params {}))))

(defn stop!
  "Close the monitor connection and terminate a VICE process.

  Accepts either a plain instance descriptor or a runtime map returned by
  `run!`. Idempotent: a missing or already-dead process is not an error."
  [runtime-or-instance]
  (driver/stop! runtime-or-instance))

(defn stop
  "Compatibility wrapper around `stop!` for a plain VICE instance descriptor."
  [instance]
  (driver/stop instance))

(defn connect
  "Connect to the binary monitor described by `instance`.

  Connecting to VICE enters the monitor and pauses emulation. Monitor
  requests also leave VICE paused; call
  `omkamra.vice.binary-monitor/resume` (or `exit`) as the final request when
  execution should resume.

  An optional event handler receives unsolicited binary-monitor events; it
  defaults to `prn`."
  ([instance]
   (driver/connect instance))
  ([instance handle-event]
   (driver/connect instance handle-event)))

(defn close
  [conn]
  (driver/close conn))

(defmacro with-conn
  [conn instance & body]
  `(let [~conn (connect ~instance)]
     (try
       ~@body
       (finally
         (bm/close ~conn)))))

;; Runner --------------------------------------------------------------------

(def default-connect-timeout-ms driver/default-connect-timeout-ms)
(def default-connect-retry-ms driver/default-connect-retry-ms)

(defn connect!
  "Connect to `instance`'s binary monitor, retrying until it is ready.

  Throws if the process exits first or the timeout elapses. Options:
  `:connect-timeout-ms`, `:connect-retry-ms`, and `:handle-event`."
  ([instance]
   (driver/connect! instance))
  ([instance options]
   (driver/connect! instance options)))

(defn- run-connected!
  [instance artifact link-options run-options]
  (let [conn (connect! instance run-options)]
    (try
      (merge instance
             {:artifact artifact
              :link-options link-options
              :run-options run-options}
             (driver/inject-and-resume! conn artifact)
             {:connection conn})
      (catch Throwable error
        (try
          (bm/close conn)
          (catch Throwable _ nil))
        (throw error)))))

(defn run!
  "Link (if needed), inject, and start a program on a fresh VICE instance.

  Given an artifact, it runs directly with `options` as run options. Given a
  relocatable object, it links it first using `:link-options` and then runs it
  using `:run-options`. Returns a runtime map extending the instance
  descriptor with the linked artifact, entry point, bank and register
  discovery, and injection metadata.

  VICE serves one binary-monitor client at a time, so the connection is
  released once the program is running; the instance keeps running and its
  descriptor can be handed to `omkamra.vice.capture/start!` as `:instance`.

  A `:capture` map in `options` enables on-demand capture: the linked artifact
  is recorded from its entry point, or from its `:from-pc` when given, and the
  call returns the capture session instead of the runtime. `:full-capture?`
  does not apply to an injected artifact.

  On any startup, injection, or execution-setup failure the monitor connection
  is closed and the process is terminated."
  ([target]
   (run! target {}))
  ([target options]
   (let [object? (assembler/object? target)
         link-options (if object? (:link-options options) {})
         run-options (if object? (:run-options options) options)
         capture-options (:capture options)
         artifact (if object?
                    (assembler/link target link-options)
                    target)]
     (when-not (assembler/artifact? artifact)
       (throw (IllegalArgumentException.
               "run! expects a relocatable object or a linked artifact")))
     (if capture-options
       (capture/start! (assoc capture-options :artifact artifact))
       (let [start-options (if (contains? run-options :port)
                             run-options
                             (assoc run-options :port (driver/free-port)))
             instance (start! (select-keys start-options
                                           [:executable :address :port
                                            :extra-args :options]))]
         (try
           (let [runtime (run-connected! instance artifact
                                         link-options run-options)]
             ;; Release the monitor for the next owner; the program keeps
             ;; running.
             (driver/close (:connection runtime))
             (dissoc runtime :connection))
           (catch Throwable error
             (stop! instance)
             (throw error))))))))
