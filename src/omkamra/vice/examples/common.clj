(ns omkamra.vice.examples.common
  "Shared runner helpers for clean-room C64 examples.

  An example namespace contains assembly objects, asset declarations, optional
  code-generation functions, and a short parameterized `run!` that delegates
  here. These helpers stay thin: `omkamra.vice/start!` owns process startup,
  linking, autostart, and the `:from-pc` start checkpoint."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice :as vice]))

(defn run!
  "Link (if needed) and run an example program on a fresh VICE instance.

  `target` is a relocatable object or a linked artifact. Options are passed to
  `omkamra.vice/start!`. With an object, `:link-options` applies; every other
  option is a start option. Example runs use the ALSA sound device; any caller
  supplied extra arguments are preserved.

  Returns an instance descriptor suitable for inspection and cleanup."
  ([target]
   (run! target {}))
  ([target options]
   (let [extra-args (or (:extra-args options)
                        (:options options)
                        [])]
     (vice/start! (assoc options
                         :artifact target
                         :extra-args (into (vec extra-args)
                                           ["-sounddev" "alsa"]))))))

(defn stop!
  "Stop the instance's capture, close its connection, and terminate its VICE
  process."
  [instance]
  (vice/stop! instance))

(defmacro with-run
  "Run `target`, bind the instance to `runtime`, and always tear it down.

  ```clojure
  (with-run [runtime my-object {:connect-timeout-ms 15000}]
    (Thread/sleep 2000))
  ```"
  [[runtime target options] & body]
  `(let [~runtime (run! ~target ~options)]
     (try
       ~@body
       (finally
         (stop! ~runtime)))))
