(ns omkamra.dsp.schedule
  "Block-relative segmentation of the monotonic timeline (P9).

  The scheduler turns a queue of absolute-frame events into the segments a
  kernel can render: each segment is a block-relative offset, a frame count, and
  the events due at that offset, and an event's `:frame` is dispatched in the
  segment that *starts* at it, so automation is sample-accurate.

  Everything here is preallocated. `next-segment!` returns the scheduler's own
  segment, reused every call, and reads its events out of a preallocated buffer,
  so rendering a block allocates nothing and never throws.

  **Why the kernel does not change.** A segment boundary is exactly a point where
  a control value changes, and the existing process ABI already lets a host set
  controls and render a block. Segmenting a block is therefore repeated
  `process!` calls with the controls updated in between: no backend has to learn
  about events, and rendering stays identical under any partitioning because
  every state update in the language is already per frame.

  The one cost is slicing. A kernel renders into the start of its output buffer,
  so rendering frames `[k, k + n)` of a block means copying that slice in and out
  of a preallocated scratch. That is the honest price of block-relative
  rendering against an ABI with no block offset; a host that grows one would drop
  both copies."
  (:require [omkamra.dsp.event :as event]))

;; ---------------------------------------------------------------------------
;; Preallocated mutable state
;; ---------------------------------------------------------------------------

(defn- make-window []
  #?(:clj (long-array 3)
     :cljs (js/Float64Array. 3)))

(defn- make-counter []
  #?(:clj (long-array 3)
     :cljs (js/Float64Array. 3)))

(deftype Segment [state events])

(defn segment-offset
  "The frame offset of this segment within its block."
  [^Segment segment]
  (aget ^longs (.-state segment) 0))

(defn segment-frames
  "How many frames this segment renders."
  [^Segment segment]
  (aget ^longs (.-state segment) 1))

(defn segment-event-count
  "How many events are due at this segment's offset."
  [^Segment segment]
  (aget ^longs (.-state segment) 2))

(defn segment-event
  "The `index`th due event, in ingress order."
  [^Segment segment index]
  (aget ^objects (.-events segment) index))

(deftype Scheduler [queue segment ^longs window events])

(defn scheduler
  "Create a scheduler consuming `queue`, with preallocated segment storage."
  [queue]
  (let [capacity (event/queue-capacity queue)
        ;; One buffer, shared by the scheduler and its segment: `next-segment!`
        ;; fills it and the segment reads it back.
        events (object-array capacity)]
    (Scheduler. queue
                (Segment. (make-counter) events)
                (make-window)
                events)))

(defn queue
  "The ingress queue this scheduler consumes."
  [^Scheduler scheduler]
  (.-queue scheduler))

(defn begin-block!
  "Open a block of `frames` frames starting at the absolute frame `start`."
  [^Scheduler scheduler start frames]
  (let [^longs window (.-window scheduler)
        start (long start)
        end (+ start (long frames))]
    (aset window 0 start)
    (aset window 1 start)
    (aset window 2 end)
    scheduler))

(defn block-start [^Scheduler scheduler] (aget ^longs (.-window scheduler) 0))
(defn block-end [^Scheduler scheduler] (aget ^longs (.-window scheduler) 2))

(defn block-remaining
  "Frames left to segment in the open block."
  [^Scheduler scheduler]
  (let [^longs window (.-window scheduler)]
    (- (aget window 2) (aget window 1))))

(defn next-segment!
  "The next segment of the open block, or nil when the block is exhausted.

  The returned segment is the scheduler's own, so it is valid until the next
  call. The cursor never advances past a pending event, so a boundary falls on
  **every** event frame inside the block: an event is charged to the segment
  that begins at its own frame, never quantised into a longer one, which is what
  makes the dispatch sample-accurate. Events sharing a frame are charged to the
  same segment in ingress order. An event already due when the block opens (it
  arrived late, after its frame was rendered) is charged to the first segment,
  because it cannot be dispatched retroactively. An event due exactly at the
  block's end is deliberately left for the next block."
  [^Scheduler scheduler]
  (let [^longs window (.-window scheduler)
        block-end (aget window 2)
        cursor (aget window 1)]
    (when (< cursor block-end)
      (let [queue (.-queue scheduler)
            ^objects buffer (.-events scheduler)
            ^Segment segment (.-segment scheduler)
            block-start (aget window 0)
            due (loop [count 0]
                  (let [head (event/peek-event queue)]
                    (if (and head (<= (long (:frame head)) cursor))
                      (do (aset ^objects buffer count (event/pop-event! queue))
                          (recur (inc count)))
                      count)))
            head (event/peek-event queue)
            head-frame (if head (long (:frame head)) nil)
            limit (if (and head-frame (< head-frame block-end)) head-frame block-end)
            frames (- limit cursor)
            ^longs state (.-state segment)]
        ;; The `long` casts keep these on the `long[]` overload of `aset` rather
        ;; than its reflective fallback, which costs about 24 KB per call.
        (aset state 0 (long (- cursor block-start)))
        (aset state 1 (long frames))
        (aset state 2 (long due))
        (aset window 1 (long limit))
        segment))))

;; ---------------------------------------------------------------------------
;; Applying events
;; ---------------------------------------------------------------------------

(defn apply-parameter-events!
  "Fold a segment's `:parameter` events into `controls`.

  `control-index` maps a control name to its position in the block's control
  vector, in ABI order. Only `:parameter` events name a control directly; mapping
  a note-on or a controller change onto a particular instrument's controls is a
  voice-pool concern (P10). Events are applied in ingress order, so the last one
  targeting a control at the same frame wins.

  Returns the updated control vector; building it allocates, which is why a
  real-time host writes into its control arrays directly."
  [segment controls control-index]
  (let [count (segment-event-count segment)]
    (loop [index 0
           controls controls]
      (if (= index count)
        controls
        (let [message (segment-event segment index)
              position (get control-index (:name message))]
          (recur (inc index)
                 (if (and (= :parameter (:type message)) (some? position))
                   (assoc controls position (:value message))
                   controls)))))))

(defn- copy-range!
  "Copy `count` elements of `source` from `source-offset` into `target` at
  `target-offset`. Both offsets matter: a segment is rendered at the start of a
  scratch buffer and copied back to its own place in the block."
  [source source-offset target target-offset count]
  #?(:clj (System/arraycopy source (int source-offset) target (int target-offset)
                            (int count))
     :cljs (dotimes [index count]
             (aset target (+ target-offset index)
                   (aget source (+ source-offset index))))))

(defn render-block!
  "Render one block, splitting it at event boundaries.

  `context` carries `:start` (the block's absolute first frame), `:frames`,
  `:sample-rate`, `:controls`, and `:control-index`, plus `:input` when the
  process has an input port, the preallocated `:input-scratch`/`:output-scratch`
  buffers, each as long as the largest block, and `:process!`, the function that
  renders one segment.

  `:process!` is supplied rather than imported so this namespace stays free of
  any target dependency: the JVM host passes `omkamra.dsp/process!`, and the
  browser passes whatever drives its kernel.

  `:dispatch!`, when supplied, is called with each segment before `:process!`, so
  a host that consumes event types other than `:parameter` — a note pool, say —
  can act at the segment boundary.

  Returns `{:segments n :controls controls}`; the returned controls carry the
  parameter changes into the following block, as a host expects."
  [scheduler context]
  (let [{:keys [input output frames controls control-index dispatch!
                input-scratch output-scratch process!]} context
        controls (or controls [])
        target (or output-scratch output)]
    (begin-block! scheduler (:start context) frames)
    (loop [segments 0
           controls controls]
      (if-let [segment (next-segment! scheduler)]
        (let [offset (long (segment-offset segment))
              segment-frames (long (segment-frames segment))
              _ (when dispatch! (dispatch! segment))
              controls (apply-parameter-events! segment controls control-index)]
          (when (pos? segment-frames)
            (let [;; A segment starting at 0 renders straight into the block's
                  ;; output; a later one renders into scratch and is copied back.
                  direct? (zero? offset)
                  out (if direct? output target)]
              (when input
                (copy-range! input offset input-scratch 0 segment-frames))
              (process! (cond-> {:output out
                                 :frames segment-frames
                                 :sample-rate (:sample-rate context)
                                 :controls controls}
                          input (assoc :input input-scratch)))
              (when-not direct?
                (copy-range! out 0 output offset segment-frames))))
          (recur (inc segments) controls))
        {:segments segments :controls controls}))))
