(ns omkamra.vice
  "Public VICE API: process lifecycle, program startup, monitor connections,
  and capture.

  This is the supported entry point over the VICE implementation:

  * `assemble`/`link` build a program;
  * `start!` launches a fresh instance, optionally autostarting a program;
  * `connect!` opens the instance's exclusive binary-monitor connection
    (close it with `omkamra.vice.binary-monitor/close!`);
  * `with-conn` scopes a connection;
  * `capture!` records execution through `omkamra.vice.capture`;
  * `stop!` stops any capture, closes the connection, and terminates the
    process.

  The analysis pipeline and its projections remain available in their own
  namespaces."
  (:require
   [clojure.java.io :as io]
   [omkamra.vice.assembler :as assembler]
   [omkamra.vice.binary-monitor :as bm]
   [omkamra.vice.capture :as capture]
   [omkamra.vice.driver :as driver]
   [omkamra.vice.prg :as prg]))

(def default-executable driver/default-executable)
(def default-address driver/default-address)
(def default-port driver/default-port)

(def default-connect-timeout-ms driver/default-connect-timeout-ms)
(def default-connect-retry-ms driver/default-connect-retry-ms)

;; Assembly ------------------------------------------------------------------

(defn assemble
  "Assemble quoted source data into a relocatable object.

  See `omkamra.vice.assembler/assemble`."
  ([source]
   (assembler/assemble source))
  ([source options]
   (assembler/assemble source options)))

(defn link
  "Link a relocatable object and its dependencies into a linked artifact.

  See `omkamra.vice.assembler/link`."
  ([object]
   (assembler/link object))
  ([object options]
   (assembler/link object options)))

;; Instance lifecycle --------------------------------------------------------

(defn- start-options
  [options]
  (assoc (select-keys options [:executable :address :extra-args :options])
         :port (or (:port options) (driver/free-port))))

(defn- start-program!
  "Link `target` when it is a relocatable object, autostart it on a fresh
  instance, and return the instance descriptor once the program runs."
  [target options]
  (let [object? (assembler/object? target)
        link-options (if object? (:link-options options) {})
        artifact (if object? (assembler/link target link-options) target)]
    (when-not (assembler/artifact? artifact)
      (throw (IllegalArgumentException.
              "start! :artifact expects a relocatable object or a linked artifact")))
    (let [temp-input (prg/write-temp-artifact-prg! artifact)]
      (try
        (let [instance (driver/start! (start-options options))]
          (try
            (let [conn (driver/connect-until-ready!
                        instance
                        (select-keys options [:connect-timeout-ms :connect-retry-ms]))]
              (try
                (let [start-pc (driver/prepare-program-start! conn temp-input options)
                      checkpoint (atom nil)]
                  ;; `:from-pc` moves the startup stop from the BASIC RUN entry
                  ;; to the requested address; otherwise execution resumes right
                  ;; after the RUN checkpoint.
                  (when (and start-pc (:from-pc options))
                    (driver/await-pc! conn start-pc checkpoint
                                      (or (:program-start-timeout-ms options)
                                          driver/default-program-start-timeout-ms)))
                  (bm/resume! conn))
                (finally
                  (bm/close! conn))))
            (assoc instance
                   :artifact artifact
                   :entry-point (:entry-point artifact)
                   :link-options link-options
                   :from-pc (:from-pc options))
            (catch Throwable error
              (driver/stop! instance)
              (throw error))))
        (finally
          (io/delete-file temp-input true))))))

(defn start!
  "Start a fresh VICE instance.

  Without `:artifact`, launch a blank emulator and return its instance
  descriptor. With `:artifact` (a relocatable object from `assemble`, or a
  linked artifact from `link`), link the object when needed using
  `:link-options`, write a temporary self-relocating PRG, launch a fresh
  instance, autostart the program, and return the descriptor while the program
  runs.

  `:from-pc` selects the execution checkpoint at which startup stops before the
  program is resumed. `:port` (default: an ephemeral port), `:executable`,
  `:address`, `:extra-args`, `:connect-timeout-ms`, `:connect-retry-ms`,
  `:autostart-timeout-ms`, and `:program-start-timeout-ms` are honored.

  On any startup failure the monitor connection is closed, the process is
  terminated, and the temporary PRG is removed."
  ([]
   (start! {}))
  ([options]
   (if-let [target (:artifact options)]
     (start-program! target options)
     (driver/start! (start-options options)))))

(defn connect!
  "Claim `instance`'s binary monitor and return the connection.

  Throws when the instance already has an active capture or another monitor
  connection. Close the connection with
  `omkamra.vice.binary-monitor/close!`, which releases the claim.

  Connecting to VICE enters the monitor and pauses emulation; monitor requests
  also leave VICE paused. Options: `:connect-timeout-ms`, `:connect-retry-ms`,
  and `:handle-event`."
  ([instance]
   (connect! instance {}))
  ([instance options]
   (when-not (driver/claim! instance :connection ::connecting)
     (throw (ex-info
             "The VICE instance already has an active capture or monitor connection"
             {:reason :instance-busy})))
   (try
     (let [holder (atom nil)
           conn (driver/connect-until-ready! instance options)
           conn (assoc conn bm/on-close-key
                       (fn [] (driver/release! instance :connection @holder)))]
       (reset! holder conn)
       (driver/install! instance :connection conn)
       conn)
     (catch Throwable error
       (driver/release! instance :connection ::connecting)
       (throw error)))))

(defmacro with-conn
  "Connect to `instance`, bind the connection to `conn`, and always close it.

  Usage: `(with-conn [conn instance] body...)`."
  [[conn instance] & body]
  `(let [~conn (connect! ~instance)]
     (try
       ~@body
       (finally
         (bm/close! ~conn)))))

(defn stop!
  "Stop an instance: stop any capture, close the monitor connection, and
  terminate the process.

  Idempotent. A capture attached to an external instance is finalized but
  leaves the process running, as documented by `omkamra.vice.capture/capture!`."
  [instance]
  (when instance
    (capture/stop! instance)
    (when-let [conn (driver/owner instance :connection)]
      (bm/close! conn))
    (driver/stop! instance))
  nil)

;; Capture -------------------------------------------------------------------

(defn capture!
  "Capture execution of a VICE instance and return its instance descriptor.

  See `omkamra.vice.capture/capture!` for the options and ownership rules. Use
  the returned descriptor with `omkamra.vice.capture/{status,stop-async!,stop!}`."
  [options]
  (capture/capture! options))
