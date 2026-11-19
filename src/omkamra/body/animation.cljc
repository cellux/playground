(ns omkamra.body.animation
  "Poses, clips, blending and deterministic playback.

  A clip is plain data (D2, D15):

      {:omkamra.body.animation/version 1
       :skeleton :omkamra.body/canonical
       :duration 2000          ; milliseconds
       :loop? false
       :easing :smoothstep
       :keys [{:time 0 :pose pose-a}
              {:time 1200 :pose pose-b :easing :ease-in-out}]}

  Sampling never reads a clock. `advance` takes an explicit delta in
  milliseconds and returns the next timeline and pose, so the same clip plays
  identically in a browser render loop, a test, or on the JVM."
  (:require [omkamra.body.math :as m]
            [omkamra.body.pose :as pose]
            [omkamra.body.skeleton :as sk]))

(def schema-version 1)

(defn easing [kind t]
  (let [t (m/clamp t 0.0 1.0)]
    (case kind
      :linear t
      :ease-in (* t t)
      :ease-out (- 1.0 (* (- 1.0 t) (- 1.0 t)))
      :ease-in-out (if (< t 0.5)
                     (* 2.0 t t)
                     (- 1.0 (* 2.0 (- 1.0 t) (- 1.0 t))))
      ;; :smoothstep and any unknown value
      (* t t (- 3.0 (* 2.0 t))))))

(defn blend
  "Blend two poses.

  Joint rotations are blended with quaternion slerp, the root translation
  linearly and the root rotation with slerp. Joints present in only one pose
  are blended against the identity, so a partial pose still blends smoothly."
  [a b t]
  (let [t (m/clamp t 0.0 1.0)
        joint-ids (into (set (keys (:bones a))) (keys (:bones b)))
        bones (into {}
                    (map (fn [joint-id]
                           [joint-id (m/q-slerp (pose/rotation a joint-id)
                                                (pose/rotation b joint-id)
                                                t)]))
                    joint-ids)
        root-a (:root a)
        root-b (:root b)]
    (-> a
        (assoc :bones bones)
        (assoc :root {:translation (m/v-lerp (get root-a :translation m/v-zero)
                                             (get root-b :translation m/v-zero)
                                             t)
                      :rotation (m/q-slerp (get root-a :rotation m/quat-identity)
                                           (get root-b :rotation m/quat-identity)
                                           t)}))))

(defn clip
  "Create a clip.

  Options: `:skeleton`, `:keys` (a collection of `{:time :pose :easing}`),
  `:duration` (defaults to the last key's time), `:loop?`, `:easing` (the
  default easing for segments, `:smoothstep` when absent). Keys are sorted by
  time; two keys at one time are rejected because sampling would be
  ambiguous."
  [{:keys [skeleton keys duration loop? easing]}]
  (let [keys (vec (sort-by :time keys))
        duplicate (some (fn [[a b]] (when (= (:time a) (:time b)) (:time a)))
                        (partition 2 1 keys))]
    (when duplicate
      (throw (ex-info "a clip cannot have two keys at the same time"
                      {:time duplicate})))
    (when (empty? keys)
      (throw (ex-info "a clip needs at least one key" {})))
    (when-not (every? (fn [{:keys [time pose]}]
                        (and (number? time) (pose/pose? pose)))
                      keys)
      (throw (ex-info "every clip key needs a numeric time and a pose" {:keys keys})))
    {:omkamra.body.animation/version schema-version
     :skeleton (or skeleton sk/skeleton-id)
     :keys keys
     :duration (or duration (:time (peek keys)))
     :loop? (boolean loop?)
     :easing (or easing :smoothstep)}))

(defn clip? [value]
  (and (map? value)
       (= schema-version (get value :omkamra.body.animation/version))
       (seq (:keys value))))

(defn keys-of [clip] (:keys clip))

(defn duration [clip] (:duration clip))

(defn loop? [clip] (boolean (:loop? clip)))

(defn to-data
  "Return a serialization-ready copy of `clip`, validating every key's pose."
  [skeleton clip]
  (when-not (clip? clip)
    (throw (ex-info "not a clip"
                    {:expected schema-version
                     :actual (get clip :omkamra.body.animation/version)})))
  (let [key-data (mapv (fn [{:keys [pose] :as key}]
                         (assoc key :pose (pose/to-data skeleton pose)))
                       (:keys clip))]
    (assoc clip :keys key-data :skeleton (:id skeleton))))

(defn from-data
  "Validate and normalize a serialized clip."
  [skeleton data]
  (when-not (and (map? data)
                 (= schema-version (get data :omkamra.body.animation/version)))
    (throw (ex-info "unsupported clip document"
                    {:expected schema-version
                     :actual (get data :omkamra.body.animation/version)})))
  (clip (-> data
            (update :keys (fn [keys]
                            (mapv (fn [key]
                                    (update key :pose #(pose/from-data skeleton %)))
                                  keys)))
            (assoc :skeleton (:id skeleton)))))

(defn- segment
  "The index of the key that starts the segment containing `time`."
  [keys time]
  (let [total (count keys)]
    (loop [index 0]
      (if (or (>= index (dec total))
              (< time (:time (nth keys (inc index)))))
        index
        (recur (inc index))))))

(defn sample
  "Sample the pose of `clip` at `time` milliseconds.

  Times before the first key clamp to the first pose, times after the last key
  clamp to the last pose. Between two keys the local parameter is eased with
  the segment's easing, falling back to the clip's easing."
  [clip time]
  (let [keys (:keys clip)]
    (cond
      (<= time (:time (first keys))) (:pose (first keys))
      (>= time (:time (peek keys))) (:pose (peek keys))
      :else
      (let [index (segment keys time)
            a (nth keys index)
            b (nth keys (inc index))
            span (- (:time b) (:time a))
            local (if (pos? span) (/ (- time (:time a)) span) 0.0)
            kind (or (:easing b) (:easing clip) :smoothstep)]
        (blend (:pose a) (:pose b) (easing kind local))))))

(defn timeline
  "Create a playback timeline.

  Options: `:time` milliseconds, `:playing?` (default true), `:rate`
  (default 1.0), `:loop?` (defaults to the clip's own loop setting)."
  ([clip]
   (timeline clip nil))
  ([clip {:keys [time playing? rate looping?]}]
   {:omkamra.body.animation/timeline-version 1
    :time (double (or time 0.0))
    :playing? (if (nil? playing?) true (boolean playing?))
    :rate (double (or rate 1.0))
    :loop? (if (nil? looping?) (loop? clip) (boolean looping?))}))

(defn seek
  "Move a timeline to `time`, clamped into the clip's range."
  [clip timeline time]
  (assoc timeline :time (double (m/clamp time 0.0 (duration clip)))))

(defn play [timeline]
  (assoc timeline :playing? true))

(defn pause [timeline]
  (assoc timeline :playing? false))

(defn set-rate [timeline rate]
  (assoc timeline :rate (double rate)))

(defn finished?
  "True when a non-looping timeline has reached the end of its clip."
  [clip timeline]
  (and (not (:loop? timeline))
       (>= (:time timeline) (duration clip))))

(defn advance
  "Advance a timeline by `delta` milliseconds.

  Returns `{:timeline timeline' :pose pose :time time' :finished? bool}`. A
  paused timeline does not move; a looping timeline wraps around; a finished
  non-looping timeline stays at its end pose."
  [clip timeline delta]
  (let [duration (duration clip)
        playing? (:playing? timeline)
        rate (:rate timeline)
        time (+ (:time timeline) (* (if playing? 1.0 0.0) rate delta))
        loop? (:loop? timeline)
        time (cond
               (not (pos? duration)) 0.0
               (and loop? (or (> time duration) (neg? time)))
               (let [wrapped (mod time duration)] wrapped)
               :else (m/clamp time 0.0 duration))
        timeline (assoc timeline :time (double time))]
    {:timeline timeline
     :time time
     :pose (sample clip time)
     :finished? (and (not loop?) (>= time duration))}))

(defn transitions
  "Create a clip from a sequence of poses evenly spaced over `duration`."
  ([poses duration]
   (transitions poses duration nil))
  ([poses duration {:keys [loop? easing skeleton]}]
   (let [poses (vec poses)
         step (if (> (count poses) 1)
                (/ duration (dec (count poses)))
                0)]
     (clip {:skeleton skeleton
            :loop? loop?
            :easing easing
            :duration duration
            :keys (map-indexed (fn [index pose]
                                 {:time (* index step) :pose pose})
                               poses)}))))

(defn hold
  "A clip of one pose with an explicit duration."
  [pose duration]
  (clip {:duration duration
         :keys [{:time 0 :pose pose}]}))