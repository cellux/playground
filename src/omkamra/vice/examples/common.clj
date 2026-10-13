(ns omkamra.vice.examples.common
  "Shared runner helpers for clean-room C64 examples.

  An example namespace contains assembly objects, asset declarations, optional
  code-generation functions, and a short parameterized `run!` that delegates
  here. These helpers stay thin: `omkamra.vice/run!` already owns process
  startup, linking, injection, and entry-point verification."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice :as vice]))

(defn run!
  "Link (if needed) and run an example program on a fresh VICE instance.

  `target` is a relocatable object or a linked artifact. Options are passed to
  `omkamra.vice/run!`; for a relocatable object the usual `:link-options` and
  `:run-options` apply, and for an artifact the options are run options.

  Returns a runtime map suitable for inspection and cleanup."
  ([target]
   (run! target {}))
  ([target options]
   (vice/run! target options)))

(defn stop!
  "Close the monitor connection and terminate the runtime's VICE process."
  [runtime]
  (vice/stop! runtime))

(defmacro with-run
  "Run `target`, bind the runtime to `runtime`, and always tear it down.

  ```clojure
  (with-run [runtime my-object {:run-options {:connect-timeout-ms 15000}}]
    (Thread/sleep 2000))
  ```"
  [[runtime target options] & body]
  `(let [~runtime (run! ~target ~options)]
     (try
       ~@body
       (finally
         (stop! ~runtime)))))
