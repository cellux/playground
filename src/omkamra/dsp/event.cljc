(ns omkamra.dsp.event
  "Canonical timestamped event vocabulary and the SPSC ingress queue (P9).

  The sequencing layer is host-side and portable. It owns the monotonic
  timeline, the preallocated capacity, and the ordering and overflow policies.
  The compiled kernel never learns about events, host objects, or absolute time:
  what crosses into it is a control value applied at a block-relative frame
  offset (see `omkamra.dsp.schedule`).

  Two clocks, deliberately:

  * an event's `:frame` is an absolute, monotonic position on the synth timeline
    and is a 64-bit value, because 32 bits of frames is only about 12 hours at
    48 kHz;
  * an offset handed to a kernel is block-relative and small, so it stays inside
    the language's 32-bit `:int`.

  A `:sequence` orders events sharing a `:frame`, so behaviour does not depend on
  how the host partitioned its blocks.

  **Allocation.** Events are *produced* off the audio thread, where allocating a
  message is fine, and *consumed* by reference, where it is not. The queue's own
  operations — push, drop-oldest, peek, pop, counter increments — allocate
  nothing in either stead. Diagnostics are read separately through
  `queue-stats`, which does allocate and therefore belongs outside the audio
  path.

  **Single producer, single consumer.** The consumer (the audio thread) is the
  only reader of the head and the only writer of the tail; the producer is the
  only writer of the head and the only reader of the tail. On the JVM the
  indices are `AtomicLong`s and the slots an `AtomicReferenceArray`, so a
  published event is visible to the consumer without a lock; in JavaScript the
  runtime is single-threaded and the same code is trivially correct."
  #?(:clj (:import (java.util.concurrent.atomic AtomicLong AtomicReferenceArray))))

;; ---------------------------------------------------------------------------
;; Portable mutable slots
;; ---------------------------------------------------------------------------

#?(:clj (defn- make-index [] (AtomicLong. 0))
   :cljs (defn- make-index [] (atom 0)))

#?(:clj (defn- index-value [index] (.get ^AtomicLong index))
   :cljs (defn- index-value [index] @index))

#?(:clj (defn- index-set! [index value] (.lazySet ^AtomicLong index value))
   :cljs (defn- index-set! [index value] (reset! index value)))

#?(:clj (defn- make-slots [capacity] (AtomicReferenceArray. (int capacity)))
   :cljs (defn- make-slots [capacity] (object-array capacity)))

#?(:clj (defn- slot-value [slots position]
          (.get ^AtomicReferenceArray slots (int position)))
   :cljs (defn- slot-value [slots position] (aget slots position)))

#?(:clj (defn- slot-set! [slots position value]
          (.set ^AtomicReferenceArray slots (int position) value))
   :cljs (defn- slot-set! [slots position value] (aset slots position value)))

;; Fixed counter slots. A plain `case` rather than a vector lookup, because an
;; untyped `.indexOf` on a Var deref is a *reflective* call and a reflective call
;; costs this JVM about 11 KB of allocation — far more than the counter is worth.
(def ^:private stat-accepted 0)
(def ^:private stat-dropped 1)
(def ^:private stat-rejected 2)
(def ^:private stat-dispatched 3)
(def ^:private stat-high-water 4)
(def ^:private stat-count 5)

(defn- make-stats []
  #?(:clj (long-array stat-count)
     :cljs (js/Int32Array. stat-count)))

(defn- stat-bump!
  "Increment one counter and return its new value."
  [^longs stats ^long index]
  (let [next (inc (aget stats index))]
    ;; `(long ...)` is not cosmetic: without it the compiler cannot resolve the
    ;; `long[]` overload of `aset` and falls back to reflection, which costs
    ;; this JVM about 24 KB of allocation per call.
    (aset stats index (long next))
    next))

(defn- note-accepted!
  "Count an acceptance and keep the depth high-water mark up to date."
  [^longs stats]
  (let [accepted (stat-bump! stats stat-accepted)]
    (when (> accepted (aget stats stat-high-water))
      (aset stats stat-high-water (long accepted)))))

;; ---------------------------------------------------------------------------
;; The v1 event vocabulary
;; ---------------------------------------------------------------------------

(defn- unit? [x] (and (number? x) (<= 0.0 (double x) 1.0)))

(defn- signed-unit? [x] (and (number? x) (<= -1.0 (double x) 1.0)))

(defn- channel? [x] (and (integer? x) (<= 0 x 15)))

(defn- note? [x] (and (integer? x) (<= 0 x 127)))

(def ^:private event-payloads
  "The sequencing ABI's v1 event set. Each entry maps a payload key to its
  predicate. Every list is closed: an unknown key is a validation error, so a
  typo cannot silently become a no-op."
  {:note-on {:channel channel? :note note? :velocity unit?}
   :note-off {:channel channel? :note note? :release-velocity unit?}
   :pitch-bend {:channel channel? :value signed-unit?}
   :control-change {:channel channel? :controller #(and (integer? %)
                                                        (<= 0 % 127))
                    :value unit?}
   :parameter {:name keyword? :value number?}
   :pressure {:channel channel? :note note? :value unit?}
   :transport {:tempo #(and (number? %) (pos? (double %))) :beat number?}
   :sample-rate {:value #(and (number? %) (pos? (double %)))}})

(def event-types
  "The v1 event types, as a sorted vector."
  (vec (sort (keys event-payloads))))

(defn- fail
  [message data]
  (throw (ex-info message (assoc data :dsp/error :invalid-event))))

(defn validate-event
  "Validate one event against the v1 vocabulary and return it.

  This is a *producer*-side check: it allocates and throws, so it belongs on the
  control thread before an event is pushed, never in the audio path."
  [event]
  (when-not (map? event)
    (fail "DSP events must be maps" {:event event}))
  (let [frame (:frame event)
        sequence (:sequence event)
        type (:type event)
        payload (event-payloads type)]
    (when-not (and (integer? frame) (not (neg? frame)))
      (fail "DSP event frames must be non-negative absolute integers"
            {:event event :frame frame}))
    (when-not (and (integer? sequence) (not (neg? sequence)))
      (fail "DSP event sequence numbers must be non-negative integers"
            {:event event :sequence sequence}))
    (when-not payload
      (fail "DSP event type is not in the v1 sequencing ABI"
            {:event event :type type :known event-types}))
    (doseq [[key predicate] payload]
      (let [value (get event key)]
        (when-not (predicate value)
          (fail "DSP event payload is missing or out of range"
                {:event event :type type :key key :value value}))))
    (let [unknown (remove #(contains? payload %) (keys (dissoc event :frame :sequence :type)))]
      (when (seq unknown)
        (fail "DSP event payload has keys outside the v1 sequencing ABI"
              {:event event :type type :unknown (vec (sort unknown))})))
    event))

(defn compare-events
  "Order two events by absolute frame, then by ingress sequence number.

  Equal frames are ordered by `:sequence`, which is what makes rendering
  independent of where the host chose to cut its blocks."
  [left right]
  (let [by-frame (compare (:frame left) (:frame right))]
    (if (zero? by-frame)
      (compare (:sequence left) (:sequence right))
      by-frame)))

;; ---------------------------------------------------------------------------
;; The ingress queue
;; ---------------------------------------------------------------------------

(deftype EventQueue [capacity slots head tail stats])

(defn event-queue
  "Create an ingress queue holding at most `capacity` events.

  All storage is allocated here, before audio starts."
  [capacity]
  (when-not (and (integer? capacity) (pos? capacity))
    (fail "DSP event queue capacity must be a positive integer"
          {:capacity capacity}))
  (EventQueue. capacity (make-slots capacity) (make-index) (make-index)
               (make-stats)))

(defn queue-capacity [^EventQueue queue] (.-capacity queue))

(defn queue-count
  "The number of queued events."
  [^EventQueue queue]
  (- (index-value (.-tail queue)) (index-value (.-head queue))))

(defn queue-full? [^EventQueue queue] (= (queue-count queue) (.-capacity queue)))
(defn queue-empty? [^EventQueue queue] (zero? (queue-count queue)))

(defn- slot-of [^EventQueue queue position]
  (slot-value (.-slots queue) (mod position (.-capacity queue))))

(defn- set-slot! [^EventQueue queue position value]
  (slot-set! (.-slots queue) (mod position (.-capacity queue)) value))

(defn peek-event
  "The oldest queued event, or nil."
  [^EventQueue queue]
  (when-not (queue-empty? queue)
    (slot-of queue (index-value (.-head queue)))))

(defn event-at
  "The `offset`th queued event counting from the oldest, or nil."
  [^EventQueue queue offset]
  (let [position (+ (index-value (.-head queue)) offset)]
    (when (< position (index-value (.-tail queue)))
      (slot-of queue position))))

(defn pop-event!
  "Remove and return the oldest event, or nil when the queue is empty.

  The slot is cleared so the consumer does not keep the producer's event alive."
  [^EventQueue queue]
  (when-not (queue-empty? queue)
    (let [head (.-head queue)
          position (index-value head)
          event (slot-of queue position)]
      (set-slot! queue position nil)
      (index-set! head (inc position))
      (stat-bump! (.-stats queue) stat-dispatched)
      event)))

(defn push-event!
  "Enqueue `event`, returning `:accepted`, `:dropped-oldest`, or `:rejected`.

  `now` is the current absolute timeline position. When the queue is full the
  overflow policy is to drop the **oldest future** event — never one that is
  already due or being dispatched — and accept the new one. If nothing is
  droppable the incoming event is rejected instead, so a burst can never make
  the audio thread lose an event it owes the kernel. Either way a fixed counter
  is incremented; nothing is logged, allocated, or thrown."
  [^EventQueue queue event now]
  (let [stats (.-stats queue)]
    (cond
      (not (queue-full? queue))
      (let [tail (.-tail queue)
            position (index-value tail)]
        (set-slot! queue position event)
        (index-set! tail (inc position))
        (note-accepted! stats)
        :accepted)

      :else
      (let [head-index (index-value (.-head queue))
            count (queue-count queue)
            ;; Events are ordered by frame, so the due prefix comes first and
            ;; the oldest droppable event is simply the first future one.
            droppable (loop [offset 0]
                        (if (= offset count)
                          nil
                          (let [event (slot-of queue (+ head-index offset))]
                            (if (> (:frame event) now)
                              offset
                              (recur (inc offset))))))]
        (if (nil? droppable)
          (do (stat-bump! stats stat-rejected)
              :rejected)
          (do
            ;; Shift the tail of the queue down over the evicted event.
            (loop [position (+ head-index droppable)]
              (when (< position (dec (+ head-index count)))
                (set-slot! queue position (slot-of queue (inc position)))
                (recur (inc position))))
            (set-slot! queue (+ head-index (dec count)) event)
            (stat-bump! stats stat-dropped)
            :dropped-oldest))))))

(defn queue-stats
  "A snapshot of the queue's fixed counters.

  Reads the whole counter set at once, so it belongs outside the audio path."
  [^EventQueue queue]
  (let [^longs stats (.-stats queue)]
    {:accepted (aget stats stat-accepted)
     :dropped (aget stats stat-dropped)
     :rejected (aget stats stat-rejected)
     :dispatched (aget stats stat-dispatched)
     :high-water (aget stats stat-high-water)
     :depth (queue-count queue)}))

(defn reset-queue!
  "Empty the queue and zero its counters, without reallocating.

  Counters are zeroed because they describe the current session, not the
  queue's history."
  [^EventQueue queue]
  (let [head (.-head queue)
        tail (.-tail queue)
        head-index (index-value head)
        depth (queue-count queue)]
    (dotimes [offset depth]
      (set-slot! queue (+ head-index offset) nil))
    (index-set! head 0)
    (index-set! tail 0)
    (let [^longs stats (.-stats queue)]
      (dotimes [index stat-count]
        (aset stats index 0)))
    depth))
