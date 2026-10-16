(ns omkamra.dsp.library.envelope
  "Envelope generators."
  #?(:clj (:require [omkamra.dsp :as dsp])
     :cljs (:require-macros [omkamra.dsp :as dsp])))

(dsp/defn adsr-coefficient
  "One-pole approach factor for an exponential segment of `time` seconds.  The
  lower bound only guards division by zero (or a negative time); a zero time
  then gives an instant segment rather than a stalled one.
"
  [time rate]
  (- 1.0 (exp (- 0.0 (/ 1.0 (* (max time 1.0e-6) rate))))))

(dsp/defn exponential-adsr
  "Exponential ADSR.  The input sample is the gate (`> 0` is on); the state is
  the stage (0 idle, 1 attack, 2 decay, 3 sustain, 4 release) and the current
  level.  Each segment approaches its target by a fixed per-sample factor.
  Stage changes use small thresholds rather than exact equality: an
  exponential approach only reaches its target in the limit.
"
  [gate attack decay sustain release]
  {:state [{:name level
            :init 0.0
            :next (let [a (adsr-coefficient attack sample-rate)
                        d (adsr-coefficient decay sample-rate)
                        r (adsr-coefficient release sample-rate)]
                    (if (= stage 1) (+ level (* a (- 1.0 level)))
                        (if (= stage 2) (+ level (* d (- sustain level)))
                            (if (= stage 3) sustain
                                (if (= stage 4) (* level (- 1.0 r))
                                    0.0)))))}
           {:name stage
            :type :int
            :init 0
            :next (if (> gate 0.0)
                    (if (= stage 4) 1
                        (if (= stage 1) (if (>= level 0.999) 2 1)
                            (if (= stage 2) (if (<= level (+ sustain 0.001)) 3 2)
                                (if (= stage 3) 3 1))))
                    (if (= stage 4) (if (<= level 0.001) 0 4) 4))}]
   :process {:controls [{:name attack} {:name decay}
                        {:name sustain} {:name release}]}}
  level)

(dsp/defn linear-step
  "One sample's worth of a linear segment that covers `time` seconds.
"
  [time]
  (/ 1.0 (* (max time 1.0e-6) sample-rate)))

(dsp/defn ar-envelope
  "Linear attack/release envelope: while the gate is above zero the level rises
  to 1 over `attack` seconds, and while it is not it falls to 0 over `release`.
"
  [gate attack release]
  {:state [{:name level
            :init 0.0
            :next (if (> gate 0.0)
                    (min 1.0 (+ level (linear-step attack)))
                    (max 0.0 (- level (linear-step release))))}]
   :process {:controls [{:name attack} {:name release}]}}
  (if (> gate 0.0)
    (min 1.0 (+ level (linear-step attack)))
    (max 0.0 (- level (linear-step release)))))

(dsp/defn linear-adsr
  "Linear ADSR: the same stage machine as `exponential-adsr`, but the segments
  are straight, so sustain is reached exactly rather than in the limit.
"
  [gate attack decay sustain release]
  {:state [{:name level
            :init 0.0
            :next (if (= stage 1)
                    (min 1.0 (+ level (linear-step attack)))
                    (if (= stage 2)
                      (max sustain (- level (linear-step decay)))
                      (if (= stage 3)
                        sustain
                        (if (= stage 4)
                          (max 0.0 (- level (linear-step release)))
                          0.0))))}
           {:name stage
            :type :int
            :init 0
            :next (if (> gate 0.0)
                    (if (= stage 4)
                      1
                      (if (= stage 1)
                        (if (>= level 1.0) 2 1)
                        (if (= stage 2)
                          (if (<= level sustain) 3 2)
                          (if (= stage 3) 3 1))))
                    (if (= stage 4)
                      (if (<= level 0.0) 0 4)
                      4))}]
   :process {:controls [{:name attack} {:name decay}
                        {:name sustain} {:name release}]}}
  level)
