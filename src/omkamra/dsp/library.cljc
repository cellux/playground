(ns omkamra.dsp.library
  "First entries of the portable DSP standard library (P2 deliverable 5).

  This is the seed of the library named by P7: DSP functions written in the
  DSP language itself, composed from builtins rather than host calls.  It is a
  `.cljc` namespace on purpose.  The JVM compiler expands `dsp/defn` today, and
  the same source forms are intended to be read and compiled by the browser
  compiler running in SCI once the front end is portable (P13).  Nothing here
  uses a target-specific operation."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; Hard-limit `sample` to the closed range [`lo`, `hi`].
;;
;; A thin composition of the `min`/`max` builtins.  It exists partly because it
;; is genuinely useful and partly because it is the smallest proof that a
;; separately defined DSP function can be reused by other definitions through
;; the ordinary call mechanism on every target.
(dsp/defn clip
  [sample lo hi]
  (min hi (max lo sample)))

;; Bartlett (triangular) window over the normalised phase `phase`.
;;
;; Rises linearly from 0 at phase 0 to 1 at 0.5 and back to 0 at 1, extending
;; linearly outside [0, 1].  A raised-cosine (Hann) window is deliberately not
;; provided yet: it needs `cos`, which belongs to P3.
(dsp/defn triangular-window
  [phase]
  (- 1.0 (abs (- (* 2.0 phase) 1.0))))
