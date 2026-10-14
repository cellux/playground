(ns omkamra.vice.examples.effects.common
  "Shared runner for self-contained C64 effects.

  Effect namespaces own their relocatable assembly objects and delegate their
  `run!` functions here. The general example runner supplies the VICE
  lifecycle and the ALSA sound-device default. Effects themselves do not use
  the SID."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.examples.common :as examples]))

(defn run!
  "Run an effect object on a fresh VICE instance.

  `target` is a relocatable assembler object or linked artifact."
  ([target]
   (examples/run! target))
  ([target options]
   (examples/run! target options)))

(defn stop!
  "Stop an effect instance."
  [instance]
  (examples/stop! instance))

(defmacro with-run
  "Run `target` and always stop the returned VICE instance."
  [[runtime target options] & body]
  `(let [~runtime (run! ~target ~options)]
     (try
       ~@body
       (finally
         (stop! ~runtime)))))
