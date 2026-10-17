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

;; The kernel can carry a control's value across frames itself: a control
;; declared `:smooth` is a one-pole coefficient and one declared `:slew` is a
;; per-frame limit, both applied by the compiled frame loop. These two have the
;; same shape as `smoothing` and `slew-limit` above, but the state lives in the
;; control rather than in the DSP source, so a host gets the behaviour by setting
;; a target and nothing else. Both amounts are **per frame**: a per-block amount
;; would make rendering depend on the host's block size.
(dsp/defn smoothed-gain
  "Gain whose amount is one-pole smoothed by the kernel, one step per frame.

  Setting `amount` is therefore a target, not an instantaneous jump, which is
  what stops a parameter change from clicking."
  [sample amount]
  {:process {:controls [{:name amount :smooth 0.01 :default 0.0}]}}
  (* sample amount))

(dsp/defn slewed-gain
  "Gain whose amount moves by at most a fixed step per frame, the kernel's
  slew limiter, so a jump in `amount` becomes a bounded ramp."
  [sample amount]
  {:process {:controls [{:name amount :slew 0.01 :default 0.0}]}}
  (* sample amount))
