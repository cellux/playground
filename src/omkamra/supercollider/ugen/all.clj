(ns omkamra.supercollider.ugen.all
  "All metadata-defined UGen constructors and rate aliases."
  (:require [omkamra.supercollider.ugen :as ugen]))

(defmacro ^:private define-all-ugens
  []
  `(do
     ~@(for [metadata-key (sort-by name (keys ugen/metadata))
             :let [constructor-name (symbol (name metadata-key))]]
         `(ugen/define-ugen ~constructor-name ~metadata-key))))

(define-all-ugens)
