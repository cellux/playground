(ns omkamra.vice.analysis
  "Public facade for staged VICE capture analysis.

  Implementation is organized under `omkamra.vice.analysis.*`; callers can use
  this namespace for the stable high-level API."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.analysis.common :as common]
            [omkamra.vice.analysis.runner :as runner]))

(def default-segment-configuration common/default-segment-configuration)
(def normalize-segment-configuration common/normalize-segment-configuration)

(def stages runner/stages)
(def iterate-chunks runner/iterate-chunks)
(def status runner/status)
(def run! runner/run!)
(def run-async! runner/run-async!)
