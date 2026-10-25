(ns omkamra.dsp.instance.jvm
  "The JVM target's half of the process-instance lifecycle (D89).

  `omkamra.dsp.instance` is target-neutral: it drives whatever `:create-instance`,
  `:invoke`, `:invoke-context`, `:initialize`, `:reset` and `:destroy` an artifact
  carries, and names no target. These two raw ABIs are the exception — they hand a
  JVM artifact's *physical* f32/f64 control arrays to `omkamra.dsp.targets.jvm` —
  so they live here, where the JVM target is named, and the generic namespace
  stays free of one."
  (:require [omkamra.dsp.instance :as instance]
            [omkamra.dsp.targets.jvm :as jvm]))

(defn process-checked!
  "Execute a JVM process artifact or instance through its checked raw ABI.

  Unlike `instance/process!`, controls are supplied in their physical reusable
  arrays: `float-controls` and `int-controls`. It verifies primitive array
  classes, buffer shape/length, control storage, and the holder's own state
  before processing."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw checked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (instance/check-live! holder)
     (jvm/process-checked! holder input output frames sample-rate
                           float-controls int-controls lanes shared))))

(defn process-unchecked!
  "Execute a JVM process artifact or instance through its raw primitive ABI.

  This deliberately performs no validation. Callers must provide buffers and
  physical f32/f64 control arrays matching the artifact ABI. Persistent state
  is owned by the holder: a plain artifact owns one state region, and each
  `instance/create-instance` handle owns its own. Use `process-checked!` at
  setup/development boundaries; retain `:unchecked` and its matching
  `Dsp*Process` interface in a real-time host when direct dispatch is required."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (let [artifact (or (:artifact holder) holder)]
     (when-not (= :process (:entry artifact))
       (throw (ex-info "only process artifacts have a process ABI"
                       {:entry (:entry artifact)})))
     (when-not (= :jvm (:target artifact))
       (throw (ex-info "the raw unchecked process ABI is currently JVM-specific"
                       {:target (:target artifact)})))
     (instance/check-live! holder)
     (jvm/process-unchecked! holder input output frames sample-rate
                             float-controls int-controls lanes shared))))
