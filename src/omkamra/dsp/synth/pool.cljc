(ns omkamra.dsp.synth.pool
  "Preallocated voices, deterministic allocation and stealing (P10).

  A pool is N independent kernel voices over one compiled artifact, plus the
  metadata needed to decide which voice plays which note. It owns no DSP code: it
  writes a voice's controls, runs the voice, and mixes the result, through the
  `Kernel` protocol below, so the same pool drives a JVM artifact, the reference
  interpreter, or a browser kernel object.

  **Determinism.** Allocation and stealing are fully determined by the sequence
  of events and the pool's declaration, never by iteration order or by timing:

  * a free voice is the **lowest-index** free voice;
  * when every voice is busy, the pool prefers a **releasing** voice over a
    sounding one — a note the player has already let go is the one expected to
    be lost — and then applies the declared `:stealing` key (oldest note-on,
    lowest note, or highest note) from `omkamra.dsp.synth`;
  * the **lowest voice index** breaks any remaining tie;
  * **stealing resets the voice**, so a stolen voice is indistinguishable from a
    fresh one and the output of a sequence does not depend on when the steal
    happened.

  **Total audio path.** `note-on!`, `note-off!`, `render!`, and `all-notes-off!`
  do not throw, allocate, lock, or log. Everything that can fail — capacities,
  modes, policies, an unmapped control, a block longer than the declared maximum
  — is checked when the pool is built, or is a programming error reported by
  `render!` before it does any work. Note and velocity *ranges* are the event
  vocabulary's producer-side check (see `omkamra.dsp.event`), so the pool trusts
  them rather than re-validating in the audio callback.

  **Voice lifetime.** A voice is free, active, or releasing, and P10a frees a
  voice only through `reset-pool!` or by stealing it. That is not a shortcut: a
  released voice is exactly the voice stealing prefers, so a pool of capacity C
  behaves correctly under any number of notes. *Tail-aware* release — freeing a
  voice when its own envelope has actually finished — needs the kernel to report
  that, which is P10c.

  **Allocation.** Every array is allocated by `voice-pool`, before audio starts.
  The control values of a note-on are resolved into one preallocated double array
  in ABI order, so no control write boxes a number and no plan map is walked in
  the audio path."
  (:require [omkamra.dsp.synth :as synth]))

;; ---------------------------------------------------------------------------
;; The kernel seam
;; ---------------------------------------------------------------------------

(defprotocol Kernel
  "What a pool needs from a target. A `voice` is a target-specific handle, opaque
  here: this is the boundary `PLAN.md` describes as \"target-specific handles
  without leaking them into portable IR\"."
  (create-voice [kernel max-block-frames]
    "Allocate one voice handle, with its control storage and mixing scratch.")
  (destroy-voice! [kernel voice]
    "Release a voice handle.")
  (reset-voice! [kernel voice]
    "Return a voice to its initial state (a fresh envelope, zeroed history).")
  (write-voice-controls! [kernel voice values]
    "Publish `values`, a double array in ABI order, as the voice's controls.")
  (clear-output! [kernel output frames]
    "Zero `frames` samples of the output block.")
  (mix-voice! [kernel voice output frames sample-rate]
    "Run the voice for `frames` samples and add it into the output block.

    Returns true when the kernel contained a non-finite voice (the `:reset`
    policy), so the pool can count it. The `:off` and `:sanitize` policies
    return false.")
  (read-specials! [kernel voice target]
    "Write this voice's builtin special-slot values into `target`, a preallocated
    double array, one entry per declared special in ascending-bit order (D39).
    No-op when the definition declares none. Writing into a preallocated array
    keeps the audio path from allocating."))

;; ---------------------------------------------------------------------------
;; Preallocated mutable state
;; ---------------------------------------------------------------------------

(def ^:private voice-free 0)

(def ^:private activity-bit
  "The fixed bit of the builtin `:activity` special in the ABI special mask
  (D39). Specials occupy the low state slots in ascending-bit order, so this
  bit's readback position is the number of lower set bits."
  0)
(def ^:private voice-active 1)
(def ^:private voice-releasing 2)

;; Voice-mode codes, mirroring `omkamra.dsp.synth/mode-codes`. The pool reads the
;; keyword once, at construction, and the audio path compares numbers.
(def ^:private mode-poly 0)
(def ^:private mode-mono 1)
(def ^:private mode-legato 2)
(def ^:private mode-retrigger 3)

(def ^:private stat-note-ons 0)
(def ^:private stat-note-offs 1)
(def ^:private stat-released 2)
(def ^:private stat-steals 3)
(def ^:private stat-high-water 4)
(def ^:private stat-frames 5)
(def ^:private stat-activations 6)
(def ^:private stat-containment 7)
(def ^:private stat-count 8)

;; The global source bank is indexed by MIDI channel.
(def ^:private channel-count 16)

(defn- make-longs
  [length]
  #?(:clj (long-array length)
     :cljs (js/Float64Array. length)))

(defn- make-doubles
  [length]
  #?(:clj (double-array length)
     :cljs (js/Float64Array. length)))

(defn- make-handles
  [length]
  #?(:clj (object-array length)
     :cljs (object-array length)))

;; A hinted array read and write are direct: with the array's type known and a
;; primitive index, `aget`/`aset` compile to the primitive overload, which is
;; what keeps a counter off the reflective path (see `omkamra.dsp.event`).
(defn- stat-bump!
  [^longs stats position]
  (let [position (long position)
        value (inc (aget stats position))]
    (aset stats position value)
    value))

(defn- stat-add!
  [^longs stats position amount]
  (let [position (long position)
        value (+ (aget stats position) (long amount))]
    (aset stats position value)
    value))

(defn- stat-value
  [^longs stats position]
  (aget stats (long position)))

;; ---------------------------------------------------------------------------
;; The pool
;; ---------------------------------------------------------------------------

(deftype VoicePool
         [config kernel capacity max-block-frames handles states notes channels ages
          velocities frequencies sources constants values stats mode
          held-notes held-channels held-depth
          detunes bends pans bend-range special-mask activity-position special-values live?])

(defn voice-pool
  "Build a pool of `capacity` voices for a runtime configuration and a kernel.

  `config` is `omkamra.dsp.synth/runtime-config` output; `kernel` is a `Kernel`
  implementation, typically from `omkamra.dsp.synth.jvm`. Every voice handle,
  array, and scratch buffer is allocated here, so no later operation allocates."
  [config kernel]
  (when-not (map? config)
    (synth/fail "a synth runtime configuration must be a map" {:value config}))
  (let [capacity (long (:capacity config))
        max-block-frames (long (:max-block-frames config))
        mode (get synth/mode-codes (:mode config))
        _ (when-not mode
            (synth/fail "a synth runtime configuration has no encodable voice mode"
                        {:mode (:mode config)
                         :supported (vec (sort (keys synth/mode-codes)))}))
        held-capacity (long (or (:held-notes config) 1))
        bend-range (double (or (:bend-range config) synth/default-bend-range))
        plan (vec (:control-plan config))
        controls (count plan)
        states (make-longs capacity)
        values (make-doubles controls)
        sources (make-longs controls)
        constants (make-doubles controls)
        handles (make-handles capacity)
        special-mask (long (or (:special-mask config) 0))
        ;; `:activity` is bit 0, the lowest special, so when it is declared it is
        ;; always the first readback slot; higher specials, if any, follow.
        activity-position (long (if (bit-test special-mask activity-bit) 0 -1))
        special-values (make-doubles (max 1 (long (or (:special-count config) 0))))]
    ;; One voice handle per index, created here so that no note-on ever allocates
    ;; a kernel instance, a control array, or a mixing buffer.
    (dotimes [index capacity]
      (aset ^objects handles index (create-voice kernel max-block-frames)))
    (doseq [[index entry] (map-indexed vector plan)]
      (let [code (get synth/plan-source-codes (:source entry))]
        (when-not code
          (synth/fail "a synth control plan entry has no encodable source"
                      {:entry entry :supported (vec (sort (keys synth/plan-source-codes)))}))
        (aset ^longs sources (long index) (long code))
        (aset ^doubles constants (long index) (double (or (:constant entry) 0.0)))))
    (VoicePool. config kernel capacity max-block-frames
                handles
                states (make-longs capacity) (make-longs capacity) (make-longs capacity)
                (make-doubles capacity) (make-doubles capacity)
                sources constants values (make-longs stat-count)
                (long mode)
                (make-longs held-capacity) (make-longs held-capacity) (make-longs 1)
                (make-doubles capacity) (make-doubles channel-count) (make-doubles capacity)
                bend-range
                special-mask activity-position special-values
                #?(:clj (atom true) :cljs (atom true)))))

(defn pool-capacity [^VoicePool pool] (.-capacity pool))
(defn pool-config [^VoicePool pool] (.-config pool))
(defn pool-max-block-frames [^VoicePool pool] (.-max-block-frames pool))

(defn control-count
  "How many controls the pool's plan addresses — the ABI control count.

  A host that computes control values itself (a modulation layer, say) allocates
  one `double` array of this length and reuses it for every voice and block."
  [^VoicePool pool]
  (alength ^doubles (.-values pool)))

(defn- check-live!
  [^VoicePool pool]
  (when-not (deref (.-live? pool))
    (synth/fail "this voice pool has been destroyed" {}))
  pool)

;; ---------------------------------------------------------------------------
;; Allocation
;; ---------------------------------------------------------------------------

(defn- free-index
  "The lowest-index free voice, or -1."
  [^VoicePool pool]
  (let [capacity (.-capacity pool)
        ^longs states (.-states pool)]
    (loop [index 0]
      (cond
        (>= index capacity) -1
        (zero? (aget states index)) index
        :else (recur (inc index))))))

(defn- steal-key
  "The comparison key for the declared policy. Every policy prefers a *smaller*
  key, so `:highest-note` negates its note rather than reversing the comparison.

  The `^long` return hint is load-bearing: without it the loop in `steal-index`
  takes a boxed number for its `best-key`, which the compiler reports as an
  auto-boxed loop argument."
  ^long [^VoicePool pool index]
  (let [^longs notes (.-notes pool)
        ^longs ages (.-ages pool)]
    (case (:stealing (.-config pool))
      :oldest (aget ages index)
      :lowest-note (aget notes index)
      :highest-note (- (aget notes index)))))

(defn- steal-index
  "The voice the declared policy gives up, or -1 for an empty pool.

  Releasing voices are considered before sounding ones — that is the first and
  strongest part of the rule — and a smaller key wins within a class. The
  comparison is strict, so the lowest index breaks a tie."
  [^VoicePool pool]
  (let [capacity (.-capacity pool)
        ^longs states (.-states pool)]
    (loop [index 0 best -1 best-class 2 best-key 0]
      (if (>= index capacity)
        best
        (let [class (if (= voice-releasing (aget states index)) 0 1)
              key (steal-key pool index)]
          (if (or (neg? best)
                  (< class best-class)
                  (and (= class best-class) (< key best-key)))
            (recur (inc index) index class key)
            (recur (inc index) best best-class best-key)))))))

(defn- alloc-index
  "The voice the next note-on takes: a free voice, else the stolen one."
  [^VoicePool pool]
  (let [free (free-index pool)]
    (if (neg? free)
      (steal-index pool)
      free)))

;; ---------------------------------------------------------------------------
;; Writing controls
;; ---------------------------------------------------------------------------

(defn- resolve-controls!
  "Fill `values` with the plan's value for voice `index`, with `gate` as the gate.

  The one place a plan becomes numbers. `write-controls!` publishes the result to
  the kernel; `read-controls!` hands it back to a host."
  [^VoicePool pool index ^double gate ^doubles values]
  (let [^longs sources (.-sources pool)
        ^doubles constants (.-constants pool)
        ^longs notes (.-notes pool)
        ^longs channels (.-channels pool)
        ^doubles velocities (.-velocities pool)
        ^doubles frequencies (.-frequencies pool)
        ^doubles detunes (.-detunes pool)
        ^doubles bends (.-bends pool)
        ^doubles pans (.-pans pool)
        bend-range (.-bend-range pool)
        controls (alength values)]
    (loop [position 0]
      (when (< position controls)
        (let [source (aget sources position)
              ;; The `double` is load-bearing, not decoration: it is what makes
              ;; the compiler infer a primitive here. Without it `aset` is a
              ;; *reflective* `RT.aset` call -- about 24 KB each, once per
              ;; control, per note -- and the compiler says so when reflection
              ;; warnings are on.
              value (double
                     (cond
                       (== source 0) gate
                       (== source 1) (double (aget notes index))
                       (== source 2) (aget frequencies index)
                       (== source 3) (aget velocities index)
                       (== source 4) (aget constants position)
                       (== source 5) (* bend-range (aget bends (aget channels index)))
                       (== source 6) (aget detunes index)
                       :else (aget pans index)))]
          (aset values position value)
          (recur (inc position)))))
    values))

(defn- write-controls!
  "Resolve the plan for voice `index` into the pool's scratch and publish it.

  `gate` is the one control whose value changes over a voice's life: every other
  source is fixed by the note-on, so a note-off rewrites the same values with the
  gate down rather than remembering them."
  [^VoicePool pool index ^double gate]
  (write-voice-controls! (.-kernel pool) (aget ^objects (.-handles pool) index)
                         (resolve-controls! pool index gate (.-values pool))))

;; ---------------------------------------------------------------------------
;; Host control delivery
;; ---------------------------------------------------------------------------

(defn set-voice-controls!
  "Publish `values`, a `double` array in ABI order, as voice `index`'s controls.

  This is the pool's *delivery* channel for a host that computes control values
  itself, such as a modulation layer: it replaces the plan's own value and says
  nothing about how the new one was computed, so a host can compose with the
  plan, override it, or ignore it. It is deliberately the only such affordance,
  and the pool keeps no opinion about routing (see `DECISIONS.md` D34).

  `values` must have `control-count` entries. The pool copies what it needs, so
  the caller allocates once and reuses the array. A bad index is a programming
  error and is reported before any work; nothing else here allocates."
  [^VoicePool pool index values]
  (check-live! pool)
  (let [index (long index)]
    (when-not (and (>= index 0) (< index (.-capacity pool)))
      (synth/fail "a voice pool has no voice at that index"
                  {:index index :capacity (.-capacity pool)}))
    (write-voice-controls! (.-kernel pool)
                           (aget ^objects (.-handles pool) index)
                           values))
  nil)

(defn read-controls!
  "Fill `target` with the control values the plan resolves for voice `index` now.

  The gate is read from the voice's state — open while it is active, closed while
  it is releasing or free — so this is the read half of a read/modify/write:
  `read-controls!`, change what you mean to, `set-voice-controls!`. `target` must
  have `control-count` entries; nothing is allocated here."
  [^VoicePool pool index target]
  (check-live! pool)
  (let [index (long index)]
    (when-not (and (>= index 0) (< index (.-capacity pool)))
      (synth/fail "a voice pool has no voice at that index"
                  {:index index :capacity (.-capacity pool)}))
    (let [^longs states (.-states pool)
          gate (if (= voice-active (aget states index)) 1.0 0.0)]
      (resolve-controls! pool index gate target)))
  nil)

(defn set-pitch-bend!
  "Set channel `channel`'s pitch bend to `value` in -1..1.

  This is the pool's global source bank for bend: a `:bend` control resolves to
  the player's bend scaled by the description's `:bend-range`, so the kernel
  receives cents rather than a normalized value. It applies to every voice on the
  channel the next time that voice's controls are resolved; a bad channel is a
  programming error."
  [^VoicePool pool channel value]
  (check-live! pool)
  (let [channel (long channel)]
    (when-not (and (>= channel 0) (< channel channel-count))
      (synth/fail "a voice pool has no such channel"
                  {:channel channel :channels channel-count}))
    (aset ^doubles (.-bends pool) channel (double value)))
  nil)

(defn set-detune!
  "Set voice `index`'s detune to `value` cents.

  A `:detune` control resolves to this per-voice offset, so it is the per-voice
  fine-tune counterpart of the per-channel `:bend`. A fresh voice starts in tune
  (`sound-voice!`), so a host sets this after `note-on!` returns the index; a
  steal resets it."
  [^VoicePool pool index value]
  (check-live! pool)
  (let [index (long index)]
    (when-not (and (>= index 0) (< index (.-capacity pool)))
      (synth/fail "a voice pool has no voice at that index"
                  {:index index :capacity (.-capacity pool)}))
    (aset ^doubles (.-detunes pool) index (double value)))
  nil)

(defn set-pan!
  "Set voice `index`'s pan to `value` in -1..1 (left..right; 0 is centre).

  A `:pan` control resolves to this per-voice value, so a stereo voice can map it
  to its two channels. A voice that does not bind a `:pan` control is untouched
  and pans inside its own DSP. A fresh voice starts centred (`sound-voice!`), so
  a host sets this after `note-on!` returns the index; a steal resets it.

  Unlike `set-detune!`, this re-resolves the voice's controls when it is
  sounding, so the new pan reaches the kernel on the next render without a
  separate re-write."
  [^VoicePool pool index value]
  (check-live! pool)
  (let [index (long index)]
    (when-not (and (>= index 0) (< index (.-capacity pool)))
      (synth/fail "a voice pool has no voice at that index"
                  {:index index :capacity (.-capacity pool)}))
    (aset ^doubles (.-pans pool) index (double value))
    (let [state (aget ^longs (.-states pool) index)]
      (when-not (zero? state)
        (write-controls! pool index
                         (if (== state voice-active) 1.0 0.0)))))
  nil)

;; ---------------------------------------------------------------------------
;; Starting and stopping one voice
;; ---------------------------------------------------------------------------

(defn- sound-voice!
  "Point voice `index` at `note`/`channel`, set its velocity and `frequency`,
  and open its gate.

  `reset?` restarts the voice's own state, which is what both a steal and a
  retrigger do. Counting it as a *steal* is left to the caller: a monophonic
  retrigger resets the voice without being a steal in the allocation sense. A
  fresh voice is in tune — its detune starts at zero and the host sets it with
  `set-detune!` — and `frequency` is a parameter rather than derived here so the
  legacy `:frequency` source keeps working."
  [^VoicePool pool index note channel velocity frequency reset?]
  (let [^objects handles (.-handles pool)
        ^longs states (.-states pool)
        ^longs notes (.-notes pool)
        ^longs channels (.-channels pool)
        ^longs ages (.-ages pool)
        ^doubles velocities (.-velocities pool)
        ^doubles frequencies (.-frequencies pool)
        ^doubles detunes (.-detunes pool)
        ^doubles pans (.-pans pool)
        ^longs stats (.-stats pool)
        handle (aget handles index)]
    (when reset?
      (reset-voice! (.-kernel pool) handle))
    (aset notes index (long note))
    (aset channels index (long channel))
    (aset velocities index (double velocity))
    (aset frequencies index (double frequency))
    (aset detunes index 0.0)
    (aset pans index 0.0)
    (aset ages index (long (stat-bump! stats stat-activations)))
    (aset states index (long voice-active))
    (write-controls! pool index 1.0)
    index))

(defn- update-high-water!
  "Track how many voices sound at once, for the diagnostics."
  [^VoicePool pool]
  (let [^longs states (.-states pool)
        ^longs stats (.-stats pool)
        capacity (.-capacity pool)]
    (loop [index 0
           active 0]
      (if (>= index capacity)
        (when (> active (stat-value stats stat-high-water))
          (aset stats (long stat-high-water) (long active)))
        (recur (inc index)
               (if (zero? (aget states index)) active (inc active)))))))

;; ---------------------------------------------------------------------------
;; The held-note stack (`:legato` and `:retrigger`)
;; ---------------------------------------------------------------------------
;;
;; A monophonic mode that falls back to the previously held note needs to
;; remember which notes are down, in order. The stack is bounded by the
;; description's `:held-notes` capacity and lives in primitive arrays, so
;; pushing, removing, and reading the top allocate nothing.

(defn- stack-depth
  [^VoicePool pool]
  (aget ^longs (.-held-depth pool) 0))

(defn- stack-find
  "The stack position of `note` on `channel`, or -1."
  [^VoicePool pool note channel]
  (let [^longs notes (.-held-notes pool)
        ^longs channels (.-held-channels pool)
        depth (long (stack-depth pool))]
    (loop [index 0]
      (cond
        (>= index depth) -1
        (and (= (long note) (aget notes index))
             (= (long channel) (aget channels index))) index
        :else (recur (inc index))))))

(defn- stack-shift-down!
  "Move entries `[from, depth)` down one slot, closing the hole at `from`."
  [^longs notes ^longs channels ^long from ^long depth]
  (loop [index from]
    (when (< index (dec depth))
      (aset notes index (aget notes (inc index)))
      (aset channels index (aget channels (inc index)))
      (recur (inc index)))))

(defn- stack-push!
  "Hold `note`/`channel` at the top of the stack.

  A note already held moves to the top, since the newest press is the one that
  sounds. A full stack forgets its oldest held note rather than refusing the new
  one: the work stays bounded, and it matches the ingress queue's drop-oldest
  discipline."
  [^VoicePool pool note channel]
  (let [^longs notes (.-held-notes pool)
        ^longs channels (.-held-channels pool)
        ^longs depth-cell (.-held-depth pool)
        capacity (alength notes)
        depth (long (aget depth-cell 0))
        found (long (stack-find pool note channel))
        depth (if (neg? found)
                depth
                (do (stack-shift-down! notes channels found depth) (dec depth)))
        depth (if (>= depth capacity)
                (do (stack-shift-down! notes channels 0 depth) (dec depth))
                depth)]
    (aset notes depth (long note))
    (aset channels depth (long channel))
    (aset depth-cell 0 (long (inc depth)))))

(defn- stack-remove!
  "Forget `note`/`channel`, returning true when it was the top of the stack (the
  sounding note), so the caller knows whether the pitch must fall back."
  [^VoicePool pool note channel]
  (let [^longs notes (.-held-notes pool)
        ^longs channels (.-held-channels pool)
        ^longs depth-cell (.-held-depth pool)
        depth (long (aget depth-cell 0))
        found (long (stack-find pool note channel))]
    (if (neg? found)
      false
      (do
        (stack-shift-down! notes channels found depth)
        (aset depth-cell 0 (long (dec depth)))
        (= found (dec depth))))))

(defn- stack-top
  "The note at the top of the held stack, or -1."
  [^VoicePool pool]
  (let [depth (long (stack-depth pool))]
    (if (pos? depth)
      (aget ^longs (.-held-notes pool) (dec depth))
      -1)))

;; ---------------------------------------------------------------------------
;; The note lifecycle
;; ---------------------------------------------------------------------------

(defn- poly-note-on!
  "A note-on in `:poly`: the lowest free voice, or the stolen one, reset when
  stolen so the result does not depend on which voice it was."
  [^VoicePool pool note velocity channel]
  (let [index (long (alloc-index pool))]
    (when (neg? index)
      (synth/fail "a voice pool with no voices cannot start a note" {}))
    (let [^longs states (.-states pool)
          ^longs stats (.-stats pool)
          stolen? (not= voice-free (aget states index))]
      (stat-bump! stats stat-note-ons)
      (when stolen?
        (stat-bump! stats stat-steals))
      (sound-voice! pool index note channel velocity
                    (synth/note->frequency note (get (.-config pool) :tuning))
                    stolen?)
      (update-high-water! pool)
      index)))

(defn- mono-note-on!
  "A note-on in `:mono`: one voice, last note wins, every note-on retriggers, and
  there is no fallback — a note-off of anything but the sounding note is ignored."
  [^VoicePool pool note velocity channel]
  (let [^longs states (.-states pool)
        ^longs stats (.-stats pool)
        busy? (not= voice-free (aget states 0))]
    (stat-bump! stats stat-note-ons)
    (when busy? (stat-bump! stats stat-steals))
    (sound-voice! pool 0 note channel velocity
                  (synth/note->frequency note (get (.-config pool) :tuning))
                  busy?)
    (update-high-water! pool)
    0))

(defn- stack-note-on!
  "A note-on in `:legato` or `:retrigger`. Both remember held notes so a release
  falls back to the previous one; they differ only in whether an *overlapping*
  note-on retriggers the voice."
  [^VoicePool pool note velocity channel retrigger?]
  (let [^longs states (.-states pool)
        ^longs stats (.-stats pool)
        sounding? (= voice-active (aget states 0))
        overlap? (and sounding? (pos? (long (stack-depth pool))))
        hold? (and (not retrigger?) overlap?)]
    (stat-bump! stats stat-note-ons)
    (stack-push! pool note channel)
    (if hold?
      ;; Legato: the new note takes over the pitch and the envelope keeps its
      ;; level, so only the note-dependent controls are rewritten.
      (let [frequency (synth/note->frequency note (get (.-config pool) :tuning))]
        (aset ^longs (.-notes pool) 0 (long note))
        (aset ^doubles (.-frequencies pool) 0 frequency)
        (write-controls! pool 0 1.0))
      (let [busy? (not= voice-free (aget states 0))]
        (when busy? (stat-bump! stats stat-steals))
        (sound-voice! pool 0 note channel velocity
                      (synth/note->frequency note (get (.-config pool) :tuning))
                      busy?)))
    (update-high-water! pool)
    0))

(defn- poly-note-off!
  "Release every active voice sounding `note` on `channel` (the `:poly`
  behaviour)."
  [^VoicePool pool note channel]
  (let [^longs states (.-states pool)
        ^longs notes (.-notes pool)
        ^longs channels (.-channels pool)
        ^longs stats (.-stats pool)
        capacity (.-capacity pool)]
    (stat-bump! stats stat-note-offs)
    (loop [index 0
           released 0]
      (if (>= index capacity)
        (do (stat-add! stats stat-released released)
            released)
        (if (and (= voice-active (aget states index))
                 (= (long note) (aget notes index))
                 (= (long channel) (aget channels index)))
          (do (aset states index (long voice-releasing))
              (write-controls! pool index 0.0)
              (recur (inc index) (inc released)))
          (recur (inc index) released))))))

(defn- mono-note-off!
  "Release the single voice when the released note is the sounding one. There is
  no fallback in `:mono`, so a note-off for any other note is ignored."
  [^VoicePool pool note channel]
  (let [^longs states (.-states pool)
        ^longs notes (.-notes pool)
        ^longs channels (.-channels pool)
        ^longs stats (.-stats pool)]
    (stat-bump! stats stat-note-offs)
    (if (and (= voice-active (aget states 0))
             (= (long note) (aget notes 0))
             (= (long channel) (aget channels 0)))
      (do (aset states 0 (long voice-releasing))
          (write-controls! pool 0 0.0)
          (stat-add! stats stat-released 1)
          1)
      0)))

(defn- stack-note-off!
  "Release `note` in `:legato` or `:retrigger`. A release of the sounding note
  falls back to the previously held one, changing pitch without retriggering the
  envelope; releasing the last held note releases the voice."
  [^VoicePool pool note channel]
  (let [^longs states (.-states pool)
        ^longs notes (.-notes pool)
        ^doubles frequencies (.-frequencies pool)
        ^longs stats (.-stats pool)]
    (stat-bump! stats stat-note-offs)
    (if-not (stack-remove! pool note channel)
      0
      (if (zero? (long (stack-depth pool)))
        (do (aset states 0 (long voice-releasing))
            (write-controls! pool 0 0.0)
            (stat-add! stats stat-released 1)
            1)
        (let [note (long (stack-top pool))
              frequency (synth/note->frequency note (get (.-config pool) :tuning))]
          (aset notes 0 note)
          (aset frequencies 0 frequency)
          (write-controls! pool 0 1.0)
          0)))))

(defn note-on!
  "Start `note` on a voice. Returns the voice index.

  `velocity` is the note-on velocity (0..1) and `channel` defaults to 0. What
  the call means depends on the pool's declared mode: `:poly` takes the lowest
  free voice (or the stolen one), and `:mono`/`:legato`/`:retrigger` use the one
  voice and consult the held-note stack. Whichever it is, a voice that was not
  free is reset first, so the result never depends on which voice happened to be
  taken.

  Everything that can fail was checked when the pool was built, so this never
  throws, allocates, locks, or logs."
  ([^VoicePool pool note] (note-on! pool note 1.0 0))
  ([^VoicePool pool note velocity] (note-on! pool note velocity 0))
  ([^VoicePool pool note velocity channel]
   (check-live! pool)
   (let [mode (.-mode pool)]
     (cond
       (== mode mode-poly) (poly-note-on! pool note velocity channel)
       (== mode mode-mono) (mono-note-on! pool note velocity channel)
       (== mode mode-legato) (stack-note-on! pool note velocity channel false)
       (== mode mode-retrigger) (stack-note-on! pool note velocity channel true)
       :else (poly-note-on! pool note velocity channel)))))

(defn note-off!
  "Release `note` on `channel`. Returns how many voices began releasing.

  In `:poly` every voice sounding the note releases, which is what makes a
  repeated note-on followed by one note-off behave as a player expects.
  In `:legato` and `:retrigger` the release may instead fall back to a previously
  held note, and in `:mono` it releases only when the sounding note is the one
  released. A note-off for a note no voice is playing is ignored, not an error:
  it is a legitimate race in a stream of events."
  ([^VoicePool pool note] (note-off! pool note 0))
  ([^VoicePool pool note channel]
   (check-live! pool)
   (let [mode (.-mode pool)]
     (cond
       (== mode mode-mono) (mono-note-off! pool note channel)
       (== mode mode-legato) (stack-note-off! pool note channel)
       (== mode mode-retrigger) (stack-note-off! pool note channel)
       :else (poly-note-off! pool note channel)))))

(defn all-notes-off!
  "Release every sounding voice. Returns how many."
  [^VoicePool pool]
  (check-live! pool)
  (let [^longs states (.-states pool)
        ^longs stats (.-stats pool)
        mode (.-mode pool)
        capacity (.-capacity pool)]
    (if (or (== mode mode-mono) (== mode mode-legato) (== mode mode-retrigger))
      (do (aset ^longs (.-held-depth pool) 0 0)
          (if (= voice-active (aget states 0))
            (do (aset states 0 (long voice-releasing))
                (write-controls! pool 0 0.0)
                (stat-add! stats stat-released 1)
                1)
            0))
      (loop [index 0
             released 0]
        (if (>= index capacity)
          (do (stat-add! stats stat-released released)
              released)
          (if (= voice-active (aget states index))
            (do (aset states index (long voice-releasing))
                (write-controls! pool index 0.0)
                (recur (inc index) (inc released)))
            (recur (inc index) released)))))))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defn render!
  "Render `frames` samples of every sounding voice, mixed, into `output`.

  `output` is a preallocated mono array of at least `frames` samples. A block
  longer than the pool's declared maximum is a programming error and is refused
  before any voice runs, rather than silently overrunning a scratch buffer."
  [^VoicePool pool output frames sample-rate]
  (check-live! pool)
  (let [frames (long frames)]
    (when (neg? frames)
      (synth/fail "a voice pool cannot render a negative number of frames"
                  {:frames frames}))
    (when (> frames (.-max-block-frames pool))
      (synth/fail "a voice pool was asked for a block larger than its declared maximum"
                  {:frames frames :max-block-frames (.-max-block-frames pool)}))
    (when (pos? frames)
      (let [kernel (.-kernel pool)
            capacity (.-capacity pool)
            ^longs states (.-states pool)
            ^objects handles (.-handles pool)
            activity-position (long (.-activity-position pool))
            ^doubles special-values (.-special-values pool)]
        (clear-output! kernel output frames)
        (loop [index 0]
          (when (< index capacity)
            (when-not (zero? (aget states index))
              (when (mix-voice! kernel (aget handles index) output frames sample-rate)
                (stat-add! (.-stats pool) stat-containment 1))
              ;; A released voice that declares its activity is freed as soon as
              ;; the voice says it is finished (D39). No declaration keeps it
              ;; until it is stolen, which is the safe default.
              (when (and (= voice-releasing (aget states index))
                         (>= activity-position 0))
                (read-specials! kernel (aget handles index) special-values)
                (when (<= (aget special-values activity-position) 0.0)
                  (reset-voice! kernel (aget handles index))
                  (aset states index (long voice-free)))))
            (recur (inc index))))
        (stat-add! (.-stats pool) stat-frames frames))))
  nil)

;; ---------------------------------------------------------------------------
;; Lifecycle and diagnostics
;; ---------------------------------------------------------------------------

(defn reset-pool!
  "Return every voice to its initial state and free the whole pool."
  [^VoicePool pool]
  (check-live! pool)
  (let [^longs states (.-states pool)
        capacity (.-capacity pool)]
    (dotimes [index capacity]
      (reset-voice! (.-kernel pool) (aget ^objects (.-handles pool) index))
      (aset states index (long voice-free)))
    (aset ^longs (.-held-depth pool) 0 0))
  nil)

(defn destroy-pool!
  "Release every voice's state. The pool must not be used afterwards."
  [^VoicePool pool]
  (when (deref (.-live? pool))
    (dotimes [index (.-capacity pool)]
      (destroy-voice! (.-kernel pool) (aget ^objects (.-handles pool) index)))
    (reset! (.-live? pool) false))
  nil)

(defn voice-snapshot
  "A diagnostic view of the pool: one map per non-free voice, in index order.

  This allocates and is meant for tests, tools, and the control thread — never
  for the audio path."
  [^VoicePool pool]
  (check-live! pool)
  (let [^longs states (.-states pool)
        ^longs notes (.-notes pool)
        ^longs channels (.-channels pool)
        ^longs ages (.-ages pool)
        ^doubles velocities (.-velocities pool)
        ^doubles frequencies (.-frequencies pool)]
    (into []
          (keep (fn [index]
                  (when-not (zero? (aget states index))
                    {:index index
                     :state (case (long (aget states index))
                              (1) :active
                              (2) :releasing)
                     :note (aget notes index)
                     :channel (aget channels index)
                     :velocity (aget velocities index)
                     :frequency (aget frequencies index)
                     :age (aget ages index)})))
          (range (.-capacity pool)))))

(defn pool-stats
  "A diagnostic view of the pool's counters. This allocates."
  [^VoicePool pool]
  (check-live! pool)
  (let [^longs stats (.-stats pool)]
    {:note-ons (stat-value stats stat-note-ons)
     :note-offs (stat-value stats stat-note-offs)
     :released (stat-value stats stat-released)
     :steals (stat-value stats stat-steals)
     :high-water (stat-value stats stat-high-water)
     :containment (stat-value stats stat-containment)
     :frames (stat-value stats stat-frames)}))
