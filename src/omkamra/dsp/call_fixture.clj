(ns omkamra.dsp.call-fixture
  (:require [omkamra.dsp :as dsp]))

(dsp/defn double-sample
  [sample]
  (* sample 2.0))

(dsp/defn add-one
  [sample]
  (+ sample 1.0))
