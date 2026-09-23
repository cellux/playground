(ns omkamra.supercollider.env
  (:refer-clojure :exclude [delay range]))

(def shape-numbers
  {:step 0
   :lin 1
   :linear 1
   :exp 2
   :exponential 2
   :sin 3
   :sine 3
   :wel 4
   :welch 4
   :sqr 6
   :squared 6
   :cub 7
   :cubed 7
   :hold 8})

(defrecord Envelope [levels times curves release-node loop-node offset])

(defn envelope?
  [value]
  (instance? Envelope value))

(defn- numeric-seq
  [label values]
  (let [values (if (sequential? values) values [values])]
    (when-not (seq values)
      (throw (IllegalArgumentException.
              (str label " must not be empty"))))
    (when-not (every? number? values)
      (throw (IllegalArgumentException.
              (str label " must contain only numbers: " (pr-str values)))))
    (vec values)))

(defn- wrap-extend
  [values length]
  (let [values (vec values)]
    (when (and (pos? length) (empty? values))
      (throw (IllegalArgumentException. "cannot extend an empty sequence")))
    (vec (take length (cycle values)))))

(defn- curve-value
  [curve]
  (cond
    (number? curve) curve
    (keyword? curve) (when (contains? shape-numbers curve) curve)
    (symbol? curve) (let [curve (keyword (name curve))]
                      (when (contains? shape-numbers curve) curve))
    (string? curve) (let [curve (keyword curve)]
                      (when (contains? shape-numbers curve) curve))
    :else nil))

(defn- normalize-curve
  [curve]
  (let [curves (if (sequential? curve) (vec curve) [curve])]
    (when-not (every? #(some? (curve-value %)) curves)
      (throw (IllegalArgumentException.
              (str "unknown envelope curve: " (pr-str curve)))))
    (if (= 1 (count curves))
      (first curves)
      curves)))

(defn- normalize-node
  [label value segment-count]
  (when-not (or (nil? value) (and (integer? value)
                                  (<= 0 value segment-count)))
    (throw (IllegalArgumentException.
            (str label " must be nil or an integer in [0, " segment-count "]: "
                 value))))
  value)

(defn Env
  ([levels times]
   (Env levels times :lin nil nil 0.0))
  ([levels times curve]
   (Env levels times curve nil nil 0.0))
  ([levels times curve release-node]
   (Env levels times curve release-node nil 0.0))
  ([levels times curve release-node loop-node]
   (Env levels times curve release-node loop-node 0.0))
  ([levels times curve release-node loop-node offset]
   (let [levels (numeric-seq "envelope levels" levels)
         segment-count (dec (count levels))
         times (wrap-extend (numeric-seq "envelope times" times) segment-count)
         curves (normalize-curve curve)]
     (when (neg? segment-count)
       (throw (IllegalArgumentException. "an envelope needs at least two levels")))
     (->Envelope levels
                 times
                 curves
                 (normalize-node "release-node" release-node segment-count)
                 (normalize-node "loop-node" loop-node segment-count)
                 (if (number? offset) offset 0.0)))))

(defn new-clear
  ([num-segments]
   (new-clear num-segments 1))
  ([num-segments num-channels]
   (when-not (and (integer? num-segments) (pos? num-segments))
     (throw (IllegalArgumentException. "num-segments must be positive")))
   (when-not (= 1 num-channels)
     (throw (UnsupportedOperationException.
             "multi-channel envelopes are not supported yet")))
   (Env (repeat (inc num-segments) 0.0)
        (repeat num-segments 1.0))))

(defn duration
  [envelope]
  (reduce + 0.0 (:times envelope)))

(defn total-duration
  [envelope]
  (let [times (:times envelope)]
    (if (some sequential? times)
      (apply max (map #(reduce + 0.0 %) times))
      (duration envelope))))

(defn release-time
  [envelope]
  (if-let [release-node (:release-node envelope)]
    (reduce + 0.0 (drop release-node (:times envelope)))
    0.0))

(defn sustained?
  [envelope]
  (some? (:release-node envelope)))

(defn- map-levels
  [envelope f]
  (assoc envelope :levels (mapv f (:levels envelope))))

(defn range
  ([envelope]
   (range envelope 0.0 1.0))
  ([envelope lo hi]
   (let [levels (:levels envelope)
         minimum (apply min levels)
         maximum (apply max levels)
         scale (if (= minimum maximum) 0.0 (/ (- hi lo) (- maximum minimum)))]
     (map-levels envelope #(if (= minimum maximum)
                             lo
                             (+ lo (* (- % minimum) scale)))))))

(defn exprange
  ([envelope]
   (exprange envelope 0.01 1.0))
  ([envelope lo hi]
   (let [levels (:levels envelope)
         minimum (apply min levels)
         maximum (apply max levels)
         log-ratio (Math/log (/ hi lo))]
     (map-levels envelope
                 #(if (= minimum maximum)
                    lo
                    (* lo (Math/exp (* (/ (- % minimum) (- maximum minimum))
                                       log-ratio))))))))

(defn curverange
  ([envelope]
   (curverange envelope 0.0 1.0 -4.0))
  ([envelope lo hi curve]
   (let [levels (:levels envelope)
         minimum (apply min levels)
         maximum (apply max levels)
         curve (double curve)
         mapped (fn [value]
                  (let [x (if (= minimum maximum)
                            0.0
                            (/ (- value minimum) (- maximum minimum)))]
                    (if (zero? curve)
                      (+ lo (* x (- hi lo)))
                      (+ lo (* (/ (- 1.0 (Math/exp (* curve x)))
                                  (- 1.0 (Math/exp curve)))
                               (- hi lo))))))]
     (map-levels envelope mapped))))

(defn delay
  [envelope delay-time]
  (when-not (number? delay-time)
    (throw (IllegalArgumentException. "delay must be numeric")))
  (->Envelope (into [(first (:levels envelope))] (:levels envelope))
              (into [delay-time] (:times envelope))
              (if (sequential? (:curves envelope))
                (into [:lin] (:curves envelope))
                (into [:lin] (repeat (count (:times envelope))
                                     (:curves envelope))))
              (some-> (:release-node envelope) inc)
              (some-> (:loop-node envelope) inc)
              (:offset envelope)))

(defn as-array
  [envelope]
  (let [curves (:curves envelope)
        segment-count (count (:times envelope))
        curves (if (sequential? curves)
                 (wrap-extend curves segment-count)
                 (repeat segment-count curves))
        curve-shape (fn [curve]
                      (if (number? curve)
                        [5 curve]
                        [(get shape-numbers (curve-value curve)) 0.0]))]
    (vec (concat [(first (:levels envelope))
                  segment-count
                  (or (:release-node envelope) -99)
                  (or (:loop-node envelope) -99)]
                 (mapcat (fn [level time curve]
                           (let [[shape value] (curve-shape curve)]
                             [level time shape value]))
                         (rest (:levels envelope))
                         (:times envelope)
                         curves)))))

(defn triangle
  ([duration]
   (triangle duration 1.0))
  ([duration level]
   (let [half (* duration 0.5)]
     (Env [0.0 level 0.0] [half half]))))

(defn sine
  ([duration]
   (sine duration 1.0))
  ([duration level]
   (let [half (* duration 0.5)]
     (Env [0.0 level 0.0] [half half] :sine))))

(defn perc
  ([attack-time release-time]
   (perc attack-time release-time 1.0 -4.0))
  ([attack-time release-time level]
   (perc attack-time release-time level -4.0))
  ([attack-time release-time level curve]
   (Env [0.0 level 0.0] [attack-time release-time] curve)))

(defn linen
  ([attack-time sustain-time release-time]
   (linen attack-time sustain-time release-time 1.0 :lin))
  ([attack-time sustain-time release-time level]
   (linen attack-time sustain-time release-time level :lin))
  ([attack-time sustain-time release-time level curve]
   (Env [0.0 level level 0.0]
        [attack-time sustain-time release-time]
        curve)))

(defn xyc
  [points]
  (let [points (mapv (fn [point]
                       (when-not (and (sequential? point) (<= 2 (count point)))
                         (throw (IllegalArgumentException.
                                 (str "XYC point must contain time and level: "
                                      (pr-str point)))))
                       [(nth point 0) (nth point 1) (get point 2 :lin)])
                     points)
        points (sort-by first points)
        offset (ffirst points)
        times (mapv first points)
        levels (mapv second points)
        curves (mapv #(nth % 2) points)]
    (Env levels
         (mapv - (subvec times 1) times)
         (butlast curves)
         nil
         nil
         offset)))

(defn pairs
  ([pairs]
   (xyc pairs))
  ([pairs curve]
   (xyc (map #(conj (vec %) curve) pairs))))

(defn step
  ([levels times]
   (step levels times nil nil 0.0))
  ([levels times release-node loop-node offset]
   (let [levels (numeric-seq "step levels" levels)
         times (numeric-seq "step times" times)]
     (when-not (= (count levels) (count times))
       (throw (IllegalArgumentException.
               "step levels and times must have the same size")))
     (Env (into [(first levels)] levels)
          times
          :step
          release-node
          loop-node
          offset))))

(defn cutoff
  ([release-time]
   (cutoff release-time 1.0 :lin))
  ([release-time level]
   (cutoff release-time level :lin))
  ([release-time level curve]
   (Env [level 0.0] [release-time] curve 0)))

(defn dadsr
  ([delay-time attack-time decay-time sustain-level release-time]
   (dadsr delay-time attack-time decay-time sustain-level release-time
          1.0 -4.0 0.0))
  ([delay-time attack-time decay-time sustain-level release-time
    peak-level curve bias]
   (Env (mapv #(+ % bias)
              [0.0 0.0 peak-level (* peak-level sustain-level) 0.0])
        [delay-time attack-time decay-time release-time]
        curve
        3)))

(defn adsr
  ([attack-time decay-time sustain-level release-time]
   (adsr attack-time decay-time sustain-level release-time 1.0 -4.0 0.0))
  ([attack-time decay-time sustain-level release-time
    peak-level curve bias]
   (Env (mapv #(+ % bias)
              [0.0 peak-level (* peak-level sustain-level) 0.0])
        [attack-time decay-time release-time]
        curve
        2)))

(defn asr
  ([attack-time sustain-level release-time]
   (asr attack-time sustain-level release-time -4.0))
  ([attack-time sustain-level release-time curve]
   (Env [0.0 sustain-level 0.0]
        [attack-time release-time]
        curve
        1)))
