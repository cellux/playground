(ns omkamra.vice.examples.common
  "Shared runner helpers for clean-room C64 examples.

  Example namespaces own assembly objects and delegate their `run!` functions
  here. The first run starts VICE through `omkamra.vice`; later runs reuse the
  same emulator through the public `omkamra.vice/run!` and `reload!` APIs."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice :as vice]))

(defonce ^:private running-instance (atom nil))

(defn- alive?
  [instance]
  (and instance
       (when-let [process (:process instance)]
         (.isAlive ^Process process))))

(defn- start-options
  [options]
  (let [extra-args (or (:extra-args options)
                       (:options options)
                       [])]
    (assoc options
           :extra-args
           (into (vec extra-args)
                 ["-sounddev" "alsa"]))))

(defn run!
  "Link and run an example.

  The first call starts a VICE instance and autostarts the artifact through a
  temporary PRG, preserving the normal `omkamra.vice` startup behavior. If
  that instance is still alive, later calls reset and reuse it through
  `omkamra.vice/reload!`.

  `:reset-range` and `:reset-timeout-ms` control reusable-instance reset.
  Other options are passed to `omkamra.vice/run!` on the first call."
  ([target]
   (run! target {}))
  ([target options]
   (locking running-instance
     (let [current @running-instance]
       (cond
         (alive? current)
         (try
           (let [instance (vice/run! target (assoc options :instance current))]
             (reset! running-instance instance)
             instance)
           (catch Throwable error
             (reset! running-instance nil)
             (vice/stop! current)
             (throw error)))

         :else
         (do
           (when current
             (vice/stop! current))
           (let [instance (vice/run! target (start-options options))]
             (reset! running-instance instance)
             instance)))))))

(defn stop!
  "Stop an effect instance and clear it from the reusable runner."
  [instance]
  (locking running-instance
    (when (identical? instance @running-instance)
      (reset! running-instance nil))
    (vice/stop! instance)))

(defmacro with-run
  "Run `target`, bind its instance to `runtime`, and always stop it.

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
