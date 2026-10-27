(ns omkamra.vice.core
  "Core implementation for the public `omkamra.vice` VICE API.

  The public, discoverable facade lives in `omkamra.vice`; this namespace owns
  the lifecycle, assembly, and capture orchestration delegated by that facade.

  Supported operations include:

  * `assemble`/`link` build a program;
  * `start!` launches a fresh instance, optionally autostarting a program;
  * `run!` starts a program or reloads it into an explicit instance;
  * `reload!` resets and injects a linked artifact into an instance;
  * `connect!` opens the instance's exclusive binary-monitor connection
    (close it with `omkamra.vice.binary-monitor/close!`);
  * `with-conn` scopes a connection;
  * `capture!` records execution through `omkamra.vice.capture`;
  * `stop!` stops any capture, closes the connection, and terminates the
    process.

  The analysis pipeline and its projections remain available in their own
  namespaces."
  (:refer-clojure :exclude [run!])
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
(def default-reset-range driver/default-reset-range)
(def default-reset-timeout-ms driver/default-reset-timeout-ms)
(def default-reset-mode driver/default-reset-mode)

(def ^:private pc-register-id 3)
;; Keep this outside the PRG unpacker's `$0334` scratch area.
(def ^:private default-reload-trampoline-address 0x02f0)
(def ^:private reload-trampoline-length 6)
(def ^:private basic-text-pointer-address 0x002b)
(def ^:private basic-pointer-block-length 8)

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

(declare connect!)

(defn- reload-trampoline-bytes
  [entry return-address]
  ;; JSR entry; JMP return-address. The JSR gives an entry routine ending in
  ;; RTS the same stack contract it has when invoked by BASIC SYS.
  (byte-array
   (map unchecked-byte
        [0x20 (bit-and entry 0xff) (bit-shift-right entry 8)
         0x4c (bit-and return-address 0xff) (bit-shift-right return-address 8)])))

(defn- little-endian-bytes
  [address]
  [(bit-and address 0xff)
   (bit-shift-right address 8)])

(defn- prg-body
  ^bytes [^bytes image]
  (let [length (- (alength image) 2)
        body (byte-array length)]
    (System/arraycopy image 2 body 0 length)
    body))

(defn- prg-stub-address
  [^bytes body]
  (when (< (alength body) 2)
    (throw (ex-info "Generated PRG has no BASIC line link"
                    {:reason :invalid-prg})))
  (+ (bit-and (int (aget body 0)) 0xff)
     (bit-shift-left (bit-and (int (aget body 1)) 0xff) 8)
     2))

(defn- inject-prg!
  "Write a generated PRG and enter it through BASIC RUN.

  The PRG contains the same self-relocating SYS stub used by fresh-instance
  startup, so linked code and assets follow the normal LOAD/RUN layout. Its
  SYS stub is invoked with a JSR trampoline so the entry routine receives the
  same return-stack contract as a BASIC SYS call."
  [conn artifact options reset-range]
  (let [load-address prg/default-load-address
        image (prg/prg-bytes artifact)
        body (prg-body image)
        body-end (+ load-address (alength body))
        stub-address (prg-stub-address body)
        trampoline-address (or (:trampoline-address options)
                               default-reload-trampoline-address)
        return-address (or (:return-address options) (first reset-range))
        trampoline (reload-trampoline-bytes stub-address return-address)
        pointers (byte-array
                  (map unchecked-byte
                       (concat (little-endian-bytes load-address)
                               (mapcat little-endian-bytes
                                       (repeat 3 body-end)))))]
    (when (> body-end 0x10000)
      (throw (ex-info "Generated PRG does not fit in C64 memory"
                      {:reason :prg-too-large
                       :load-address load-address
                       :body-length (alength body)})))
    (bm/mem-set! conn {:start load-address
                       :end (dec body-end)
                       :memspace 0
                       :bank 0
                       :data body})
    ;; TXTTAB, VARTAB, ARYTAB, and STREND. This is the BASIC state normally
    ;; established by LOAD before RUN scans the program at TXTTAB.
    (bm/mem-set! conn {:start basic-text-pointer-address
                       :end (+ basic-text-pointer-address
                               (dec basic-pointer-block-length))
                       :memspace 0
                       :bank 0
                       :data pointers})
    (bm/mem-set! conn {:start trampoline-address
                       :end (+ trampoline-address (dec reload-trampoline-length))
                       :memspace 0
                       :bank 0
                       :data trampoline})
    (bm/registers-set! conn {:memspace 0
                             :register-values [[pc-register-id
                                                trampoline-address]]})))

(defn reload!
  "Reset and run `target` inside an existing VICE instance.

  `target` may be a relocatable assembler object or a linked artifact. The
  instance is reset, waits for `:reset-range` (defaulting to the stock C64
  BASIC READY entry), writes the generated PRG into BASIC memory, and
  invokes its SYS stub with the same call/return contract as BASIC RUN.

  Options:

  * `:link-options` — linker options for a relocatable object;
  * `:reset-range` — inclusive `[start end]` execution range;
  * `:reset-timeout-ms` — timeout while waiting for the reset range;
  * `:reset-mode` — `:cpu` (default) or `:power-cycle`;
  * `:trampoline-address` — scratch address for the monitor RUN trampoline;
  * `:return-address` — address reached if the RUN path returns.

  The returned descriptor is the same instance with its artifact metadata
  updated. A monitor connection or capture already owned by the instance is
  rejected by `connect!`."
  ([instance target]
   (reload! instance target {}))
  ([instance target options]
   (let [link-options (if (assembler/object? target)
                        (:link-options options)
                        {})
         artifact (assembler/link target link-options)]
     (when-not (assembler/artifact? artifact)
       (throw (IllegalArgumentException.
               "reload! target must be a relocatable object or linked artifact")))
     (let [conn (connect! instance)]
       (try
         (let [checkpoint (atom nil)]
           (driver/reset-to-range!
            conn
            (or (:reset-range options) default-reset-range)
            checkpoint
            (or (:reset-timeout-ms options) default-reset-timeout-ms)
            (or (:reset-mode options) default-reset-mode))
           (inject-prg!
            conn
            artifact
            options
            (or (:reset-range options) default-reset-range))
           (bm/resume! conn))
         (finally
           (bm/close! conn))))
     (assoc instance
            :artifact artifact
            :entry-point (:entry-point artifact)
            :link-options link-options
            :from-pc nil))))

(defn run!
  "Run `target`, starting VICE or reusing an explicit instance.

  With `:instance`, this delegates to `reload!`; otherwise it starts a fresh
  instance using the normal `start!` artifact/autostart path. This function
  does not maintain global instance state."
  ([target]
   (run! target {}))
  ([target options]
   (if-let [instance (:instance options)]
     (reload! instance target options)
     (start! (assoc (dissoc options :instance)
                    :artifact target)))))

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
