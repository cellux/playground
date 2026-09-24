(ns omkamra.supercollider.pattern
  "Pure pattern descriptions and independently advancing pattern streams."
  (:refer-clojure :exclude [next]))

(defprotocol Pattern
  "A reusable description of a stream of values."
  (make-stream [pattern]))

(defprotocol PatternStream
  "One stateful iteration of a Pattern.

  `step` returns `{:value value :stream next-stream}`, or nil when the stream
  has ended. The input event is reserved for event-dependent patterns such as
  Pbind and Pfunc."
  (step [stream event]))

(defn pattern?
  [value]
  (satisfies? Pattern value))

(defn stream?
  [value]
  (satisfies? PatternStream value))

(declare ->ConstantStream ->PseqStream ->PbindStream)

(defrecord ConstantPattern [value]
  Pattern
  (make-stream [pattern]
    (->ConstantStream (:value pattern))))

(defrecord ConstantStream [value]
  PatternStream
  (step [stream _event]
    {:value (:value stream)
     :stream stream}))

(defn constant
  "Create a pattern that yields `value` forever."
  [value]
  (->ConstantPattern value))

(defn- as-pattern
  [value]
  (if (pattern? value)
    value
    (constant value)))

(defn stream
  "Create an independent stream from a Pattern or literal value."
  [pattern]
  (make-stream (as-pattern pattern)))

(defn- valid-repeats!
  [repeats]
  (when-not (or (= :inf repeats)
                (and (integer? repeats) (not (neg? repeats))))
    (throw (IllegalArgumentException.
            (str ":repeats must be a non-negative integer or :inf: "
                 (pr-str repeats)))))
  repeats)

(defn- valid-offset!
  [offset]
  (when-not (integer? offset)
    (throw (IllegalArgumentException.
            (str ":offset must be an integer: " (pr-str offset)))))
  offset)

(defrecord Pseq [items repeats offset]
  Pattern
  (make-stream [pattern]
    (->PseqStream pattern 0 0 nil)))

(defrecord PseqStream [pattern repeat-index position active]
  PatternStream
  (step [stream event]
    (let [items (:items (:pattern stream))
          item-count (count items)
          repeats (:repeats (:pattern stream))
          offset (:offset (:pattern stream))]
      (loop [stream stream]
        (cond
          ;; Finish a nested pattern before moving to the next item.
          (:active stream)
          (if-let [result (omkamra.supercollider.pattern/step
                           (:active stream) event)]
            {:value (:value result)
             :stream (assoc stream :active (:stream result))}
            (recur (-> stream
                       (assoc :active nil)
                       (update :position inc))))

          ;; Start the next repetition, or finish the stream.
          (or (and (integer? repeats) (zero? repeats))
              (>= (:position stream) item-count))
          (if (and (not= :inf repeats)
                   (>= (inc (:repeat-index stream)) repeats))
            nil
            (recur (assoc stream
                          :repeat-index (inc (:repeat-index stream))
                          :position 0)))

          :else
          (let [index (mod (+ offset (:position stream)) item-count)
                item (nth items index)]
            (if (pattern? item)
              (recur (assoc stream
                             :active (omkamra.supercollider.pattern/stream item)))
              {:value item
               :stream (update stream :position inc)})))))))

(defn pseq
  "Yield items in order, repeating the sequence.

  Options:
  * `:repeats` — non-negative integer or `:inf` (default 1)
  * `:offset` — starting index, wrapping around the item list (default 0)

  Items may themselves be Patterns; their streams are embedded before Pseq
  advances to the next item."
  ([items]
   (pseq items {}))
  ([items options]
   (when-not (sequential? items)
     (throw (IllegalArgumentException.
             (str "Pseq items must be sequential: " (pr-str items)))))
   (when-not (seq items)
     (throw (IllegalArgumentException. "Pseq requires at least one item")))
   (when-not (map? options)
     (throw (IllegalArgumentException.
             (str "Pseq options must be a map: " (pr-str options)))))
   (let [{:keys [repeats offset]
          :or {repeats 1 offset 0}} options]
     (valid-repeats! repeats)
     (valid-offset! offset)
     (->Pseq (vec items) repeats offset))))

(defrecord Pbind [pairs]
  Pattern
  (make-stream [pattern]
    (->PbindStream pattern
                   (mapv (fn [[key value]]
                           [key (omkamra.supercollider.pattern/stream value)])
                         (:pairs pattern)))))

(defrecord PbindStream [pattern streams]
  PatternStream
  (step [stream input-event]
    (let [event (or input-event {})]
      (loop [remaining (:streams stream)
             event event
             next-streams []]
        (if-let [[key value-stream] (first remaining)]
          (if-let [result (omkamra.supercollider.pattern/step
                           value-stream event)]
            (recur (clojure.core/next remaining)
                   (assoc event key (:value result))
                   (conj next-streams [key (:stream result)]))
            nil)
          {:value event
           :stream (assoc stream :streams (vec next-streams))})))))

(defn- valid-pbind-pair!
  [pair]
  (when-not (and (vector? pair)
                 (= 2 (count pair)))
    (throw (IllegalArgumentException.
            (str "Pbind entries must be [key value] pairs: "
                 (pr-str pair)))))
  (let [[key _value] pair]
    (when-not (or (keyword? key) (symbol? key) (string? key))
      (throw (IllegalArgumentException.
              (str "Pbind keys must be keywords, symbols, or strings: "
                   (pr-str key))))))
  pair)

(defn pbind
  "Bind independent value patterns into event maps.

  Entries are ordered so later value patterns receive the event containing
  keys produced by earlier entries. Literal values, such as `[:amp 0.5]`, are
  automatically promoted to constant patterns."
  [pairs]
  (when-not (sequential? pairs)
    (throw (IllegalArgumentException.
            (str "Pbind entries must be sequential: " (pr-str pairs)))))
  (->Pbind (mapv valid-pbind-pair! pairs)))
