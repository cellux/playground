(ns omkamra.dsp.sequencer
  "The sequencing layer: one timeline driving one or more voice pools (M1/M2).

  A `Sequencer` runs voice pools against a single monotonic timeline. It owns the
  ingress queue (`omkamra.dsp.event`), the block-relative segmentation
  (`omkamra.dsp.schedule`), and the **dispatcher**: a map from an event's trigger
  (`:type`) to a handler `(fn [definition state event] state')`.

  **One timeline, several pools.** A modular piece has several pools on one
  timeline, routed by channel: the state carries `:pools` and a `:routing` table,
  and every pool is rendered into the sequencer's scratch and summed, because
  `synth.pool/render!` clears the block before mixing. One scheduler cuts the
  block at the union of every event frame, so the pools share one clock.

  **Handlers reduce the state.** The built-in `:note-on`/`:note-off` handlers
  allocate and release voices on the pool an event routes to and set the
  conventional `gate`/`note`/`velocity` parameters; `:parameter` sets a shared
  parameter on every pool that declares it. A host adds its own triggers through
  `:handlers`, and a handler returns a (possibly changed) state — it sees the
  whole sequencer state, so it can touch any pool, parameter, or assignment.
  Handlers run off the audio thread, in ingress order.

  **Rendering is invariant under block partitioning**, as in M1: every state
  update in the pool and the kernel is already per frame.

  **Mono in this slice.** `schedule/render-block!` slices one flat output array,
  so the pools must be mono here; planar (stereo) hosting arrives with the
  browser/lane work.

  **Allocation.** `sequencer` allocates the queue, scheduler, and scratch;
  `render!` allocates nothing; `submit!` is the producer side and may allocate."
  (:require [omkamra.dsp.event :as event]
            [omkamra.dsp.numbers :as numbers]
            [omkamra.dsp.schedule :as schedule]
            [omkamra.dsp.synth.pool :as pool]))

;; ---------------------------------------------------------------------------
;; The trigger handlers
;; ---------------------------------------------------------------------------

(defn- routed-pool
  "The pool an event routes to, or nil when the routing table does not cover its
  channel."
  [state event]
  (let [routing (:routing state)
        channel (long (or (:channel event) 0))
        key (get routing channel (:default routing))]
    (get (:pools state) key)))

(defn note-on-handler
  "The built-in `:note-on` handler: allocate a voice on the routed pool and start
  it. `(fn [definition state event] state')`."
  [_definition state event]
  (when-let [p (routed-pool state event)]
    (pool/note-on! p (long (:note event)) (double (:velocity event))
                   (long (:channel event))))
  state)

(defn note-off-handler
  "The built-in `:note-off` handler: release `note` on the routed pool."
  [_definition state event]
  (when-let [p (routed-pool state event)]
    (pool/note-off! p (long (:note event)) (long (:channel event))))
  state)

(defn parameter-handler
  "The built-in `:parameter` handler: set `:name` to `:value` on every pool that
  declares it as a `:shared` parameter.

  This is the instrument-level default. Until there is a shared store above the
  pools (`DECISIONS.md` D45), a parameter change fans out to each pool's own
  shared cell."
  [_definition state event]
  (let [name (:name event)
        value (:value event)]
    (doseq [p (vals (:pools state))]
      (when (= :shared (:scope (pool/parameter-slot p name)))
        (pool/set-parameter! p name value))))
  state)

(def built-in-handlers
  "The runtime's own event handlers, keyed by an event's `:type`."
  {:note-on note-on-handler
   :note-off note-off-handler
   :parameter parameter-handler})

(defn handle!
  "Apply `event` through `dispatcher`, returning the new state.

  An event whose type has no handler leaves the state unchanged, so a host may
  ignore event types it does not use."
  [dispatcher definition state event]
  (if-let [handler (get dispatcher (:type event))]
    (handler definition state event)
    state))

;; ---------------------------------------------------------------------------
;; The sequencer
;; ---------------------------------------------------------------------------

(defn- normalize-pools
  [pool-or-pools]
  (if (map? pool-or-pools)
    pool-or-pools
    {:main pool-or-pools}))

(defn- normalize-routing
  [routing pools]
  (let [routing (or routing {:default (first (keys pools))})]
    (doseq [[channel key] routing]
      (when-not (= :default channel)
        (when-not (contains? pools key)
          (throw (ex-info "the sequencer's routing table names an unknown pool"
                          {:channel channel :pool key :pools (vec (keys pools))})))))
    (when-not (contains? routing :default)
      (let [missing (remove (set (keys routing)) (range 16))]
        (when (seq missing)
          (throw (ex-info "the sequencer's routing table leaves channels unrouted"
                          {:missing (vec missing)})))))
    routing))

(deftype Sequencer
         [definition state dispatcher queue scheduler frame max-block-frames
          scratch pool-scratch f64?])

(defn sequencer
  "Create a sequencer over one pool, or a map of `name -> pool`.

  `:routing` maps a MIDI channel (0..15) to a pool name; `:default` names the
  pool for every unrouted channel (the first pool when nothing is given).
  `:definition` is the instrument definition handlers may read; `:handlers` adds
  or overrides event handlers; `:capacity` bounds the ingress queue (default
  1024); `:precision` is the pools' physical precision (`:f32` default). Every
  buffer is allocated here."
  ([pool-or-pools] (sequencer pool-or-pools {}))
  ([pool-or-pools {:keys [capacity precision definition routing handlers]
                   :or {capacity 1024 precision :f32}}]
   (let [pools (normalize-pools pool-or-pools)]
     (when (empty? pools)
       (throw (ex-info "a sequencer needs at least one pool" {})))
     (let [max-block-frames (long (apply min (map pool/pool-max-block-frames
                                                  (vals pools))))
           f64? (= :f64 precision)
           queue (event/event-queue capacity)
           state {:pools pools
                  :routing (normalize-routing routing pools)}]
       (Sequencer. definition (atom state)
                   (merge built-in-handlers handlers)
                   queue (schedule/scheduler queue)
                   (atom 0) max-block-frames
                   (numbers/real-array f64? max-block-frames)
                   (numbers/real-array f64? max-block-frames)
                   f64?)))))

(defn sequencer-pools
  "The `name -> pool` map this sequencer drives."
  [^Sequencer sequencer]
  (:pools @(.-state sequencer)))

(defn sequencer-pool
  "The first pool this sequencer drives, for the single-pool case."
  [^Sequencer sequencer]
  (val (first (:pools @(.-state sequencer)))))

(defn sequencer-state
  "The handler state, for a host that inspects or seeds it."
  [^Sequencer sequencer]
  @(.-state sequencer))

(defn sequencer-queue
  "The ingress queue, for a producer that wants to push directly."
  [^Sequencer sequencer]
  (.-queue sequencer))

(defn current-frame
  "The absolute frame the sequencer will render next."
  [^Sequencer sequencer]
  (long @(.-frame sequencer)))

(defn submit!
  "Enqueue `event`, returning `:accepted`, `:dropped-oldest`, or `:rejected`.

  `now` is the sequencer's current frame, so the overflow policy drops the
  oldest *future* event and never one already due."
  [^Sequencer sequencer event]
  (event/push-event! (.-queue sequencer) event (current-frame sequencer)))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defn- add-into!
  "Add `frames` samples of `scratch` into `output`, in place."
  [f64? output scratch frames]
  (let [frames (long frames)]
    (if f64?
      (let [^doubles out output ^doubles src scratch]
        (dotimes [index frames]
          (aset out index (+ (aget out index) (aget src index)))))
      (let [^floats out output ^floats src scratch]
        (dotimes [index frames]
          (aset out index (+ (aget out index) (aget src index))))))))

(defn- render-pools!
  "Render every pool into `output`, summing them.

  `synth.pool/render!` clears its output before mixing, so the first pool renders
  straight into `output` and the rest render into `scratch` and are added."
  [pools scratch f64? output frames sample-rate]
  (let [ordered (vals pools)]
    (pool/render! (first ordered) output frames sample-rate)
    (doseq [p (rest ordered)]
      (pool/render! p scratch frames sample-rate)
      (add-into! f64? output scratch frames))))

(defn render!
  "Render `frames` samples into `output`, starting at the sequencer's frame.

  Every event due inside the block is dispatched, through the dispatcher, at its
  exact frame. `output` is a mono array of the pools' precision. The sequencer's
  frame advances by `frames`, so consecutive calls continue the timeline."
  ([sequencer output frames] (render! sequencer output frames 48000.0))
  ([^Sequencer sequencer output frames sample-rate]
   (let [start (current-frame sequencer)
         definition (.-definition sequencer)
         dispatcher (.-dispatcher sequencer)
         state-cell (.-state sequencer)
         pools (:pools @state-cell)
         f64? (.-f64? sequencer)
         pool-scratch (.-pool-scratch sequencer)
         dispatch! (fn [segment]
                     (dotimes [index (schedule/segment-event-count segment)]
                       (let [event (schedule/segment-event segment index)
                             state @state-cell]
                         (reset! state-cell
                                 (handle! dispatcher definition state event)))))
         process! (fn [{:keys [output frames sample-rate]}]
                    (render-pools! pools pool-scratch f64? output frames sample-rate))]
     (schedule/render-block!
      (.-scheduler sequencer)
      {:start start
       :frames frames
       :output output
       :sample-rate sample-rate
       :output-scratch (.-scratch sequencer)
       :dispatch! dispatch!
       :process! process!})
     (reset! (.-frame sequencer) (+ start (long frames)))
     nil)))
