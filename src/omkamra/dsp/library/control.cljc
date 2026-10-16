(ns omkamra.dsp.library.control
  "Control-rate helpers: smooth a value, or limit how fast it may change.

  Both are stateful, so on the first frame the value starts from zero rather than
  from the target: a compile-time state initialiser cannot depend on a control,
  and per-instance initialisation belongs to the sequencing layer (P8)."
  #?(:clj (:require [omkamra.dsp :as dsp]
                    ;; Resolved by the linker from the quoted DSP bodies, which
                    ;; clj-kondo cannot see as uses.
                    #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
                    [omkamra.dsp.library.envelope :refer [adsr-coefficient]])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

;; One-pole smoothing: approach `target` with the exponential coefficient for
;; `time` seconds. Returns the new value, so smoothings can be chained.
(dsp/defn smoothing
  [sample target time]
  {:state [{:name y
            :init 0.0
            :next (+ y (* (adsr-coefficient time sample-rate) (- target y)))}]
   :process {:controls [{:name target} {:name time}]}}
  (+ y (* (adsr-coefficient time sample-rate) (- target y))))

;; Slew limiting: move toward `target` by at most `rate` units per second, so a
;; step input becomes a ramp of a bounded slope.
(dsp/defn slew-limit
  [sample target rate]
  {:state [{:name y
            :init 0.0
            :next (let [step (* rate (/ 1.0 sample-rate))
                        delta (- target y)]
                    (if (> delta step)
                      (+ y step)
                      (if (< delta (- 0.0 step))
                        (- y step)
                        target)))}]
   :process {:controls [{:name target} {:name rate}]}}
  (let [step (* rate (/ 1.0 sample-rate))
        delta (- target y)]
    (if (> delta step)
      (+ y step)
      (if (< delta (- 0.0 step))
        (- y step)
        target))))
