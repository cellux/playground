(ns rb.explores.cowbells
  (:require [omkamra.sequencer :refer [deftarget]])
  (:require [omkamra.cowbells.fluidsynth]))

(deftarget synth
  (omkamra.cowbells.fluidsynth/new
   {:soundfonts {:r3 "/usr/share/soundfonts/FluidR3_GM.sf2"}}))

;; (omkamra.sequencer/stop)
;; (omkamra.sequencer/restart)

(defmacro with-synth
  [& body]
  `[:bind {:target synth}
    ~@body])

(defmacro with-synth-program
  [program & body]
  `(with-synth
     [:program ~program]
     ~@body))

(defmacro with-piano
  [& body]
  `(with-synth-program 0
     ~@body))
