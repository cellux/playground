(ns omkamra.vice.dedupe
  "The foundational `:dedupe` structural execution image.

  `build!` reads a finalized v2 capture through the shared capture reader and
  produces a globally deduplicated structural image:

  * instruction identities keyed by `[pc opcode]`, each retaining the first
    observed operand template and a per-operand volatility mask;
  * global basic blocks keyed by their ordered instruction-identity sequence;
  * mainline and resolved-interrupt execution contexts with context-qualified
    CFGs and edge counts;
  * dynamic execution frames: every inferred interrupt/BRK entry pushes a frame
    with a stable `:frame-id`, and every entry/return emits a context switch
    carrying that frame id, so repeated invocations of one context stay
    distinguishable;
  * frame leveling: a frame that outlives `:leveling-threshold-events` is
    annotated -- never mutated -- as effective level `:mainline` from its exact
    continuation boundary onward. The context keeps its observed `:irq` kind,
    the occurrence at `:level-start` is forced closed so no occurrence straddles
    the boundary, and the audit annotation is persisted in `index.edn`;
  * chronological block occurrences that keep the fragments of a mainline block
    interrupted by an IRQ as one occurrence, so an interrupt never manufactures
    a mainline CFG edge or splits the block definition;
  * an independent per-occurrence timing channel;
  * a compact CPU-port visibility timeline;
  * logical windows independent of raw capture chunk boundaries.

  The on-disk layout follows the design:

  ```text
  analysis/dedupe/<generation>/
    index.edn
    instructions.bin
    blocks.bin
    blocks.idx
    contexts.bin
    contexts.idx
    windows.idx
    image.bin
  ```

  All multi-byte integers are little-endian. The generation is written fully
  before `analysis/manifest.edn` is atomically rewritten to point at it, so a
  failed build publishes nothing and leaves the previous generation usable.

  The second half of this namespace is the high-level reader
  (`open`, `read-range`, `read-block`, `read-context`, `status`); callers never
  see offsets, windows, or binary layouts."
  (:require [clojure.java.io :as io]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.isa :as isa]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store])
  (:import [java.io BufferedOutputStream DataOutputStream FileOutputStream
            RandomAccessFile]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.file Files]))

(def dedupe-version contract/dedupe-version)
(def default-window-events 250000)
(def default-leveling-threshold-events 50000)

(def kernal-rom-start
  "Start of the C64 KERNAL ROM. A resolved interrupt handler in this range is a
  stock system routine (the default IRQ service routine `$EA31` in particular),
  never a program-installed handler."
  0xE000)

(def ^:private instruction-record-size 8)
(def ^:private block-index-record-size 16)
(def ^:private context-index-record-size 16)
(def ^:private context-header-size 8)
(def ^:private context-edge-size 12)
(def ^:private window-index-record-size 64)
(def ^:private timing-record-size 16)
(def ^:private port-record-size 16)
(def ^:private switch-record-size 24)

(def ^:private terminator-mnemonics
  #{"BRK" "JMP" "JSR" "RTI" "RTS" "KIL"
    "BCC" "BCS" "BEQ" "BMI" "BNE" "BPL" "BVC" "BVS"})

(def ^:private context-kind-codes
  {:main 0 :irq 1 :nmi 2 :brk 3})

(def ^:private context-kinds-by-code
  {0 :main 1 :irq 2 :nmi 3 :brk})

(def ^:private switch-kind-codes
  {:irq 0 :nmi 1 :brk 2 :return 3})

(def ^:private tracked-vector-addresses
  "Vector bytes whose direct stores are tracked so an interrupt's effective
  handler can be resolved. `$0314/$0315` is the KERNAL-dispatched user IRQ
  vector and `$0316/$0317` the BRK vector; `$FFFA-$FFFF` are the physical CPU
  NMI/RESET/IRQ vectors."
  #{0x0314 0x0315 0x0316 0x0317 0x0318 0x0319
    0xFFFA 0xFFFB 0xFFFC 0xFFFD 0xFFFE 0xFFFF})

(def ^:private initial-vector-bytes
  "Stock C64 interrupt vectors, in effect at every supported capture start.

  An IRQ entry at `$FF48` is the KERNAL trampoline, which dispatches through the
  user vector `$0314/$0315`; seeding the power-on values means that resolution
  always succeeds, so `$FF48` itself never becomes a context (it resolves to the
  KERNAL default `$EA31`, which `:segments` ignores as a ROM handler). An
  observed store to a vector byte overrides the seeded value."
  {0x0314 0x31 0x0315 0xEA
   0x0316 0x8B 0x0317 0xE3
   0xFFFA 0x43 0xFFFB 0xFE
   0xFFFC 0xE2 0xFFFD 0xFC
   0xFFFE 0x48 0xFFFF 0xFF})

;; Little-endian primitives --------------------------------------------------

(defn- little-buffer ^ByteBuffer [size]
  (doto (ByteBuffer/allocate size)
    (.order ByteOrder/LITTLE_ENDIAN)))

(defn- write-bytes! [file ^bytes bytes]
  (.mkdirs (.getParentFile (io/file file)))
  (with-open [out (io/output-stream file)]
    (.write out bytes 0 (alength bytes)))
  file)

(defn- byte-array-output
  "Concatenate the byte arrays produced by `f` for every item."
  ^bytes [items f]
  (let [parts (mapv f items)
        total (reduce + 0 (map (fn [^bytes p] (alength p)) parts))
        out (byte-array total)]
    (loop [offset 0 remaining parts]
      (when-let [part (first remaining)]
        (System/arraycopy ^bytes part 0 out (int offset) (alength ^bytes part))
        (recur (+ offset (alength ^bytes part)) (rest remaining))))
    out))

;; Chunk 1: writer state and structural interning ---------------------------

(defn- current-frame [state] (peek (:stack state)))

(defn- set-current-frame [state frame]
  (assoc state :stack (conj (pop (:stack state)) frame)))

(defn- empty-window [start]
  {:start start
   :occurrences (java.util.ArrayList.)
   :timings (java.util.ArrayList.)
   :ports (java.util.ArrayList.)
   :switches (java.util.ArrayList.)})

(defn- initial-state [^DataOutputStream image window-events max-events progress
                      max-window-occurrences leveling-threshold-events]
  {:instructions [] :instruction-ids {}
   :blocks [] :block-ids {}
   :contexts [{:key [:main] :kind :main :handler-pc nil :edges {}}]
   :context-ids {[:main] 0}
   :known-targets (boolean-array 65536)
   :vector-bytes initial-vector-bytes
   :cpu-port 0x37
   :stack [{:frame-id 0 :context-id 0 :open nil :last-block-id nil
            :kind :main :entry-index 0}]
   :frame-counter 1
   :seen-prev? false :prev-sp-after nil
   :occ-counter 0
   :window (empty-window 0)
   :window-events window-events
   :max-window-occurrences (or max-window-occurrences 200000)
   :leveling-threshold-events (or leveling-threshold-events
                                  default-leveling-threshold-events)
   :levelings []
   :max-events max-events
   :windows []
   :image image
   :image-size 0
   :totals (long-array 4)
   :last-index -1
   :progress progress})

(defn- intern-instruction!
  "Return `[state instruction-id]`, updating the template/volatility of an
  existing identity or appending a new one."
  [state ^longs buffer]
  (let [pc (aget buffer reader/ev-pc)
        opcode (aget buffer reader/ev-b0)
        width (aget ^ints isa/width-by-opcode opcode)
        b1 (aget buffer reader/ev-b1)
        b2 (aget buffer reader/ev-b2)
        key (bit-or (bit-shift-left pc 8) opcode)
        id (get (:instruction-ids state) key)]
    (if id
      (let [operand-count (int (dec width))
            instruction (nth (:instructions state) id)
            template (:template instruction)
            old-mask (long (:volatile-mask instruction))
            volatile-mask
            (loop [mask old-mask index 0]
              (if (< index operand-count)
                (let [bit (bit-shift-left 1 index)
                      observed (if (zero? index) b1 b2)]
                  (recur (if (and (zero? (bit-and mask bit))
                                  (not= observed (nth template index)))
                           (bit-or mask bit)
                           mask)
                         (inc index)))
                mask))]
        (if (== volatile-mask old-mask)
          [state id]
          [(assoc-in state [:instructions id :volatile-mask] volatile-mask) id]))
      (let [template [(if (>= width 2) b1 0) (if (>= width 3) b2 0)]
            instruction {:pc pc
                         :opcode opcode
                         :width width
                         :template template
                         :volatile-mask 0}
            id (count (:instructions state))]
        [(-> state
             (update :instructions conj instruction)
             (assoc-in [:instruction-ids key] id))
         id]))))

(defn- intern-block! [state instruction-ids]
  (let [ids (vec instruction-ids)
        id (get (:block-ids state) ids)]
    (if id
      [state id]
      (let [id (count (:blocks state))]
        [(-> state
             (update :blocks conj ids)
             (assoc-in [:block-ids ids] id))
         id]))))

(defn- intern-context! [state key kind handler-pc rom?]
  (let [id (get (:context-ids state) key)]
    (if id
      [state id]
      (let [id (count (:contexts state))]
        [(-> state
             (update :contexts conj {:key key :kind kind
                                     :handler-pc handler-pc :rom? rom?
                                     :edges {}})
             (assoc-in [:context-ids key] id))
         id]))))

;; Occurrence emission and CFG edges -----------------------------------------

(defn- record-edge! [state context-id from to]
  (if (nil? from)
    state
    (update-in state [:contexts context-id :edges]
               (fn [edges]
                 (update (or edges {}) from
                         (fn [row] (update (or row {}) to (fnil inc 0))))))))

(defn- emit-occurrence! [state frame block-id open]
  (let [fragments (:fragments open)
        window (:window state)
        totals ^longs (:totals state)]
    (.add ^java.util.ArrayList (:occurrences window)
          {:occurrence-id (:occurrence-id open)
           :frame-id (:frame-id frame)
           :context-id (:context-id frame)
           :block-id block-id
           :fragments fragments})
    (.add ^java.util.ArrayList (:timings window)
          {:start-cycle (:start-cycle open)
           :end-cycle (:end-cycle open)})
    (aset totals 0 (inc (aget totals 0)))
    (aset totals 1 (+ (aget totals 1) (count fragments)))
    state))

(defn- emit-switch! [state event-index kind-code frame]
  (.add ^java.util.ArrayList (:switches (:window state))
        {:event-index event-index
         :kind kind-code
         :frame-id (:frame-id frame)
         :context-id (:context-id frame)})
  (let [totals ^longs (:totals state)]
    (aset totals 2 (inc (aget totals 2))))
  state)

(defn- emit-port! [state event-index value]
  (.add ^java.util.ArrayList (:ports (:window state))
        {:event-index event-index
         :value (bit-and (int value) 0xff)})
  (let [totals ^longs (:totals state)]
    (aset totals 3 (inc (aget totals 3))))
  state)

;; Block and context lifecycle -----------------------------------------------

(defn- close-fragment [open end]
  (if-let [start (:frag-start open)]
    (-> open
        (update :fragments conj [start end])
        (assoc :frag-start nil))
    open))

(defn- close-block-at!
  "Close the current frame's open block, emitting its occurrence and recording
  the CFG edge from the frame's previous block. `end` is the exclusive event
  index of the occurrence fragment."
  [state end]
  (let [frame (current-frame state)]
    (if-let [open (:open frame)]
      (let [open (close-fragment open end)
            [state block-id] (intern-block! state (:instruction-ids open))
            state (emit-occurrence! state frame block-id open)
            state (record-edge! state (:context-id frame)
                                (:last-block-id frame) block-id)]
        (set-current-frame state (assoc frame :open nil :last-block-id block-id)))
      state)))

(defn- ensure-open!
  "Ensure the current frame has an open block occurrence."
  [state ^long index ^long cycle]
  (let [frame (current-frame state)]
    (if (:open frame)
      state
      (let [open {:occurrence-id (:occ-counter state)
                  :instruction-ids []
                  :fragments []
                  :frag-start index
                  :start-cycle cycle
                  :end-cycle cycle
                  :start-event index}
            frame (assoc frame :open open)]
        (-> state
            (update :occ-counter inc)
            (set-current-frame frame))))))

(defn- append-instruction! [state ^long index ^long cycle instruction-id]
  (let [frame (current-frame state)
        open (-> (:open frame)
                 (update :instruction-ids conj instruction-id)
                 (assoc :end-cycle cycle)
                 (cond-> (nil? (:frag-start (:open frame)))
                   (assoc :frag-start index)))]
    (set-current-frame state (assoc frame :open open))))

(defn- suspend-current! [state index]
  (let [frame (current-frame state)]
    (if (:open frame)
      (set-current-frame state
                         (assoc frame :open (close-fragment (:open frame) index)))
      state)))

(defn- user-vector
  "Resolve a two-byte little-endian vector from the tracked vector bytes, or
  nil when either byte has not been seen."
  [state low-address high-address]
  (let [low (get-in state [:vector-bytes low-address])
        high (get-in state [:vector-bytes high-address])]
    (when (and low high)
      (bit-or (bit-shift-left (int high) 8) (int low)))))

(defn- kernal-mapped?
  "True when the KERNAL ROM is currently visible through `$E000-$FFFF`.

  Only then is `$FF48` the KERNAL IRQ trampoline; with RAM under ROM the CPU
  dispatches through the RAM copy of `$FFFE/$FFFF` and never reaches `$FF48`."
  [state]
  (pos? (bit-and (int (or (:cpu-port state) 0x37)) 0x02)))

(defn- resolved-handler
  "Resolve the effective handler PC for an interrupt entry at `pc`.

  A stock KERNAL IRQ vectors through the physical `$FFFE/$FFFF`, whose target
  is `$FF48`; that trampoline then dispatches through the user `$0314/$0315`
  vector, so the resolved routine is the user vector's value. This only holds
  while the KERNAL ROM is mapped in: with RAM under ROM the CPU uses the RAM
  copy of `$FFFE/$FFFF`, so `$FF48` would be ordinary RAM code and is used
  directly. Any other entry PC is already the physical vector target (a demo
  that repointed `$FFFE/$FFFF`), so it is used directly as well. BRK resolves
  through `$0316/$0317`."
  [state kind pc]
  (case kind
    :irq (if (and (= 0xFF48 pc) (kernal-mapped? state))
           (or (user-vector state 0x0314 0x0315) pc)
           pc)
    :brk (or (user-vector state 0x0316 0x0317) pc)
    pc))

(defn- enter-context! [state ^long index pc kind]
  (let [state (suspend-current! state index)
        handler (resolved-handler state kind pc)
        ;; A handler address in the KERNAL ROM range is stock system code only
        ;; while that ROM is actually mapped in; with RAM under ROM the same
        ;; address holds program code and must not be treated as system.
        rom? (and (>= (long handler) (long kernal-rom-start))
                  (kernal-mapped? state))
        key [kind handler rom?]
        [state context-id] (intern-context! state key kind handler rom?)
        frame {:frame-id (:frame-counter state)
               :context-id context-id :open nil :last-block-id nil
               :kind kind :entry-index index}]
    (-> state
        (update :frame-counter inc)
        (update :stack conj frame)
        (emit-switch! index (get switch-kind-codes kind) frame))))

(defn- frame-leveling
  "Return the durable audit annotation for a leveled frame."
  [frame end threshold-events]
  (when-let [level-start (:level-start frame)]
    {:frame-id (:frame-id frame)
     :context-id (:context-id frame)
     :observed-kind (:kind frame)
     :event-range [(:entry-index frame) end]
     :level-start level-start
     :effective-level :mainline
     :reason :long-lived-frame
     :threshold-events threshold-events}))

(defn- exit-context! [state index]
  (let [frame (current-frame state)]
    (cond-> (-> state
                (emit-switch! index (get switch-kind-codes :return) frame)
                (update :stack pop))
      (:level-start frame) (update :levelings conj
                                   (frame-leveling frame index
                                                   (:leveling-threshold-events state))))))

;; Input inspection ----------------------------------------------------------

(defn- terminator? [^String mnemonic]
  (contains? terminator-mnemonics mnemonic))

(defn- direct-operand-address
  "Resolve a direct (`zp`/`abs`/indexed) operand address without touching
  memory. Indirect modes return nil and are not tracked for port visibility."
  [mode ^long word ^long x ^long y]
  (case mode
    :zp (bit-and word 0xff)
    :zpx (bit-and (+ (bit-and word 0xff) x) 0xff)
    :zpy (bit-and (+ (bit-and word 0xff) y) 0xff)
    :abs word
    :absx (bit-and (+ word x) 0xffff)
    :absy (bit-and (+ word y) 0xffff)
    nil))

(defn- add-targets! [state ^longs buffer]
  (let [opcode (aget buffer reader/ev-b0)
        ^String mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        pc (aget buffer reader/ev-pc)
        b1 (aget buffer reader/ev-b1)
        b2 (aget buffer reader/ev-b2)
        target (cond
                 (contains? isa/branch-mnemonics mnemonic)
                 (isa/relative-target pc b1)
                 (and (contains? #{"JMP" "JSR"} mnemonic)
                      (= mode :abs))
                 (bit-or b1 (bit-shift-left b2 8))
                 :else nil)]
    (if target
      (do (aset ^booleans (:known-targets state) (int target) true) state)
      state)))

(defn- maybe-port! [state ^long index ^longs buffer]
  (let [opcode (aget buffer reader/ev-b0)
        ^String mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        b1 (aget buffer reader/ev-b1)
        b2 (aget buffer reader/ev-b2)
        x (aget buffer reader/ev-x)
        y (aget buffer reader/ev-y)
        a (aget buffer reader/ev-a)
        word (bit-or b1 (bit-shift-left b2 8))]
    (if (and (= 1 (direct-operand-address mode word x y))
             (contains? #{"STA" "STX" "STY"} mnemonic))
      (let [value (bit-and (int (case mnemonic "STA" a "STX" x "STY" y)) 0xff)]
        (-> (emit-port! state index value)
            (assoc :cpu-port value)))
      state)))

(defn- maybe-vector!
  "Track direct stores to the CPU/user interrupt vectors so an entry's effective
  handler can be resolved."
  [state ^longs buffer]
  (let [opcode (aget buffer reader/ev-b0)
        ^String mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        b1 (aget buffer reader/ev-b1)
        b2 (aget buffer reader/ev-b2)
        x (aget buffer reader/ev-x)
        y (aget buffer reader/ev-y)
        a (aget buffer reader/ev-a)
        word (bit-or b1 (bit-shift-left b2 8))
        address (direct-operand-address mode word x y)]
    (if (and address
             (contains? tracked-vector-addresses address)
             (contains? #{"STA" "STX" "STY"} mnemonic))
      (update-in state [:vector-bytes address]
                 (constantly (bit-and (int (case mnemonic
                                             "STX" x
                                             "STY" y
                                             a))
                                      0xff)))
      state)))

(defn- interrupt-entry? [state ^long sp]
  (and (:seen-prev? state)
       (= sp (bit-and (- (long (:prev-sp-after state)) 3) 0xff))))

;; Event processing ----------------------------------------------------------

(defn- process-event [state ^long index ^longs buffer]
  (let [pc (aget buffer reader/ev-pc)
        sp (aget buffer reader/ev-sp)
        cycle (aget buffer reader/ev-cycle)
        opcode (aget buffer reader/ev-b0)
        x (aget buffer reader/ev-x)
        ^String mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        entry? (interrupt-entry? state sp)
        state (cond
                (:pending-brk state)
                (enter-context! (dissoc state :pending-brk) index pc :brk)

                entry?
                (enter-context! state index pc :irq)

                :else state)
        [state instruction-id] (intern-instruction! state buffer)
        state (maybe-port! state index buffer)
        state (maybe-vector! state buffer)
        state (add-targets! state buffer)
        state (ensure-open! state index cycle)
        frame (current-frame state)
        split? (and (seq (:instruction-ids (:open frame)))
                    (aget ^booleans (:known-targets state) (int pc)))
        state (if split?
                (-> state
                    (close-block-at! index)
                    (ensure-open! index cycle))
                state)
        state (append-instruction! state index cycle instruction-id)
        state (if (terminator? mnemonic)
                (let [state (close-block-at! state (inc index))]
                  (cond
                    (= "BRK" mnemonic) (assoc state :pending-brk true)
                    (and (= "RTI" mnemonic)
                         (> (count (:stack state)) 1))
                    (exit-context! state index)
                    :else state))
                state)]
    (assoc state
           :last-index index
           :seen-prev? true
           :prev-sp-after (isa/sp-after mnemonic sp x))))

(declare process-event!)

(defn- process-all! [state reader]
  (let [total (reader/event-count reader)
        report-every 250000]
    (reader/reduce-events
     reader [0 total]
     (fn [state ^longs buffer]
       (let [index (aget buffer reader/ev-index)]
         (when (and (:progress state) (zero? (rem (long index) report-every)))
           (analysis/report! (:progress state)
                             {:processed-events index
                              :total-events total
                              :windows-written (count (:windows state))
                              :blocks (count (:blocks state))
                              :contexts (count (:contexts state))
                              :stack-depth (count (:stack state))
                              :window-occurrences (.size ^java.util.ArrayList (:occurrences (:window state)))}))
         (if (and (:max-events state) (>= index (:max-events state)))
           (reduced state)
           (process-event! state index buffer))))
     state)))

(declare encode-fragment-channel encode-timing-channel
         encode-port-channel encode-switch-channel)

(defn- flush! [state]
  (let [window (:window state)
        ^bytes fragment-bytes (encode-fragment-channel (:occurrences window))
        ^bytes timing-bytes (encode-timing-channel (:timings window))
        ^bytes port-bytes (encode-port-channel (:ports window))
        ^bytes switch-bytes (encode-switch-channel (:switches window))
        ^DataOutputStream image (:image state)
        base (:image-size state)
        timing-offset (+ base (alength fragment-bytes))
        port-offset (+ timing-offset (alength timing-bytes))
        switch-offset (+ port-offset (alength port-bytes))
        _ (.write image fragment-bytes 0 (alength fragment-bytes))
        _ (.write image timing-bytes 0 (alength timing-bytes))
        _ (.write image port-bytes 0 (alength port-bytes))
        _ (.write image switch-bytes 0 (alength switch-bytes))
        record {:start (:start window)
                :end (inc (:last-index state))
                :fragment-offset base
                :timing-offset timing-offset
                :port-offset port-offset
                :switch-offset switch-offset
                :fragment-length (alength fragment-bytes)
                :timing-length (alength timing-bytes)
                :port-length (alength port-bytes)
                :switch-length (alength switch-bytes)}]
    (-> state
        (update :windows conj record)
        (update :image-size + (+ (alength fragment-bytes)
                                 (alength timing-bytes)
                                 (alength port-bytes)
                                 (alength switch-bytes)))
        (assoc :window (empty-window (inc (:last-index state)))))))

(defn- force-close-open!
  "Memory safety valve: close every open occurrence at `end` and drop the
  frame's last-block pointer, so a window with no natural mainline boundary can
  still be flushed. A forced split creates two occurrences of the same block
  and intentionally omits the CFG edge between them."
  [state end]
  (let [[state frames]
        (reduce
         (fn [[state frames] frame]
           (if-let [open (:open frame)]
             (let [open (close-fragment open end)
                   [state block-id] (intern-block! state (:instruction-ids open))
                   state (emit-occurrence! state frame block-id open)]
               [state (conj frames (assoc frame :open nil :last-block-id nil))])
             [state (conj frames frame)]))
         [state []]
         (:stack state))]
    (assoc state :stack frames)))

(defn- maybe-flush! [state]
  (let [window (:window state)
        size (- (inc (:last-index state)) (:start window))
        frame (current-frame state)
        occurrences (.size ^java.util.ArrayList (:occurrences window))]
    (cond
      (and (>= size (:window-events state))
           (= 1 (count (:stack state)))
           (nil? (:open frame)))
      (flush! state)

      (>= occurrences (:max-window-occurrences state))
      (-> state
          (force-close-open! (inc (:last-index state)))
          (flush!))

      :else state)))

(defn- level-overflow!
  "Annotate a long-lived non-main frame at its exact continuation boundary.

  Leveling is an inference over a *frame*, never a mutation of its context. The
  open block closes before the boundary so persisted occurrences cannot straddle
  `:level-start`: the IRQ prefix stays at level `:irq`, while later occurrences
  of this frame have effective level `:mainline`."
  [state index]
  (let [frame (current-frame state)
        threshold (:leveling-threshold-events state)]
    (if (and threshold
             (nil? (:level-start frame))
             (not= :main (:kind frame))
             (:entry-index frame)
             (>= (- (long index) (long (:entry-index frame))) (long threshold)))
      (let [state (close-block-at! state index)]
        (set-current-frame state (assoc (current-frame state) :level-start index)))
      state)))

(defn- process-event! [state ^long index ^longs buffer]
  (-> state
      (level-overflow! index)
      (process-event index buffer)
      (maybe-flush!)))

(defn- close-all! [state]
  (loop [state state]
    (let [frame (current-frame state)
          end (inc (:last-index state))
          state (close-block-at! state end)
          state (cond-> state
                  (:level-start frame)
                  (update :levelings conj
                          (frame-leveling frame end
                                          (:leveling-threshold-events state))))]
      (if (> (count (:stack state)) 1)
        (recur (update state :stack pop))
        state))))

;; Channel encoders ----------------------------------------------------------

(defn- encode-fragment-channel ^bytes [occurrences]
  (byte-array-output
   occurrences
   (fn [{:keys [occurrence-id frame-id context-id block-id fragments]}]
     (let [count (count fragments)
           buffer (little-buffer (+ 20 (* 16 count)))]
       (.putInt buffer (int occurrence-id))
       (.putInt buffer (int frame-id))
       (.putInt buffer (int context-id))
       (.putInt buffer (int block-id))
       (.putInt buffer (int count))
       (doseq [[start end] fragments]
         (.putLong buffer (long start))
         (.putLong buffer (long end)))
       (.array buffer)))))

(defn- encode-timing-channel ^bytes [timings]
  (let [buffer (little-buffer (* timing-record-size (count timings)))]
    (doseq [{:keys [start-cycle end-cycle]} timings]
      (.putLong buffer (long start-cycle))
      (.putLong buffer (long end-cycle)))
    (.array buffer)))

(defn- encode-port-channel ^bytes [ports]
  (let [buffer (little-buffer (* port-record-size (count ports)))]
    (doseq [{:keys [event-index value]} ports]
      (.putLong buffer (long event-index))
      (.put buffer (unchecked-byte value))
      (dotimes [_ 7] (.put buffer (unchecked-byte 0))))
    (.array buffer)))

(defn- encode-switch-channel ^bytes [switches]
  (let [buffer (little-buffer (* switch-record-size (count switches)))]
    (doseq [{:keys [event-index kind frame-id context-id]} switches]
      (.putLong buffer (long event-index))
      (.put buffer (unchecked-byte kind))
      (dotimes [_ 3] (.put buffer (unchecked-byte 0)))
      (.putInt buffer (int frame-id))
      (.putInt buffer (int context-id))
      (.putInt buffer 0))
    (.array buffer)))

;; Dictionary writers --------------------------------------------------------

(defn- write-instructions! [directory instructions]
  (let [buffer (little-buffer (* instruction-record-size (count instructions)))]
    (doseq [{:keys [pc opcode width template volatile-mask]} instructions]
      (.putShort buffer (unchecked-short pc))
      (.put buffer (unchecked-byte opcode))
      (.put buffer (unchecked-byte width))
      (.put buffer (unchecked-byte (nth template 0 0)))
      (.put buffer (unchecked-byte (nth template 1 0)))
      (.put buffer (unchecked-byte volatile-mask))
      (.put buffer (unchecked-byte 0)))
    (write-bytes! (io/file directory "instructions.bin") (.array buffer))))

(defn- write-blocks! [directory blocks]
  (let [bin (io/file directory "blocks.bin")
        index-records (volatile! [])
        offset (volatile! 0)]
    (with-open [out (BufferedOutputStream. (FileOutputStream. bin))]
      (doseq [ids blocks]
        (let [buffer (little-buffer (* 4 (count ids)))]
          (doseq [id ids] (.putInt buffer (int id)))
          (let [bytes (.array buffer)]
            (.write out bytes 0 (alength bytes))
            (vswap! index-records conj {:offset @offset :count (count ids)})
            (vswap! offset + (alength bytes))))))
    (let [buffer (little-buffer (* block-index-record-size (count blocks)))]
      (doseq [{:keys [offset count]} @index-records]
        (.putLong buffer (long offset))
        (.putInt buffer (int count))
        (.putInt buffer 0))
      (write-bytes! (io/file directory "blocks.idx") (.array buffer)))
    bin))

(defn- write-contexts! [directory contexts]
  (let [bin (io/file directory "contexts.bin")
        index-records (volatile! [])
        offset (volatile! 0)]
    (with-open [out (BufferedOutputStream. (FileOutputStream. bin))]
      (doseq [{:keys [kind handler-pc rom? edges]} contexts]
        (let [edge-count (reduce + 0 (map count (vals edges)))
              buffer (little-buffer (+ context-header-size
                                       (* context-edge-size edge-count)))]
          (.put buffer (unchecked-byte (get context-kind-codes kind 0)))
          (.put buffer (unchecked-byte (if rom? 1 0)))
          (.putShort buffer (unchecked-short (or handler-pc 0)))
          (.putInt buffer (int edge-count))
          (doseq [[from row] edges
                  [to count] row]
            (.putInt buffer (int from))
            (.putInt buffer (int to))
            (.putInt buffer (int count)))
          (let [bytes (.array buffer)]
            (.write out bytes 0 (alength bytes))
            (vswap! index-records conj {:offset @offset :length (alength bytes)})
            (vswap! offset + (alength bytes))))))
    (let [buffer (little-buffer (* context-index-record-size (count contexts)))]
      (doseq [{:keys [offset length]} @index-records]
        (.putLong buffer (long offset))
        (.putInt buffer (int length))
        (.putInt buffer 0))
      (write-bytes! (io/file directory "contexts.idx") (.array buffer)))
    bin))

(defn- write-windows-index! [directory windows]
  (let [buffer (little-buffer (* window-index-record-size (count windows)))]
    (doseq [{:keys [start end fragment-offset timing-offset port-offset
                    switch-offset fragment-length timing-length port-length
                    switch-length]} windows]
      (.putLong buffer (long start))
      (.putLong buffer (long end))
      (.putLong buffer (long fragment-offset))
      (.putLong buffer (long timing-offset))
      (.putLong buffer (long port-offset))
      (.putLong buffer (long switch-offset))
      (.putInt buffer (int fragment-length))
      (.putInt buffer (int timing-length))
      (.putInt buffer (int port-length))
      (.putInt buffer (int switch-length)))
    (write-bytes! (io/file directory "windows.idx") (.array buffer))))

(defn- file-spec [file]
  (let [f (io/file file)]
    {:file (.getName f) :size (.length f) :sha256 (store/sha256-file f)}))

(defn- write-index! [directory generation capture-id capture-sha state
                     window-events configuration]
  (let [index {:format contract/dedupe-format
               :version dedupe-version
               :generation generation
               :capture-id capture-id
               :capture-manifest-sha256 capture-sha
               :event-count (inc (:last-index state))
               :window-events window-events
               :configuration configuration
               :levelings (:levelings state)
               :counts {:instructions (count (:instructions state))
                        :blocks (count (:blocks state))
                        :contexts (count (:contexts state))
                        :windows (count (:windows state))
                        :occurrences (aget ^longs (:totals state) 0)
                        :fragments (aget ^longs (:totals state) 1)
                        :context-switches (aget ^longs (:totals state) 2)
                        :port-changes (aget ^longs (:totals state) 3)}
               :files
               {:instructions (assoc (file-spec (io/file directory "instructions.bin"))
                                     :record-size instruction-record-size
                                     :count (count (:instructions state)))
                :blocks (let [spec (file-spec (io/file directory "blocks.bin"))]
                          (assoc spec :index (file-spec (io/file directory "blocks.idx"))
                                 :index-record-size block-index-record-size
                                 :count (count (:blocks state))))
                :contexts (let [spec (file-spec (io/file directory "contexts.bin"))]
                            (assoc spec :index (file-spec (io/file directory "contexts.idx"))
                                   :index-record-size context-index-record-size
                                   :count (count (:contexts state))))
                :windows (assoc (file-spec (io/file directory "windows.idx"))
                                :image (file-spec (io/file directory "image.bin"))
                                :record-size window-index-record-size
                                :count (count (:windows state)))}}]
    (store/atomic-write-edn! (io/file directory "index.edn") index)
    index))

;; Analysis manifest ---------------------------------------------------------

(defn read-manifest
  "Read a dataset's analysis manifest, or nil when none exists."
  [dataset-root]
  (analysis/read-manifest dataset-root))

;; Public build --------------------------------------------------------------

(defn build!
  "Build a `:dedupe` generation for a finalized capture and commit it.

  `reader` is a `omkamra.vice.reader/open` handle. Options: `:window-events`
  (positive integer logical-window target), `:configuration` (recorded in the
  analysis manifest), and `:max-events` (test-only prefix bound). Returns the
  generation directory and its index. On any failure the generation directory
  is removed and nothing is published."
  ([reader] (build! reader {}))
  ([reader {:keys [window-events max-events configuration progress]
            :or {window-events default-window-events}}]
   (when-not (pos-int? window-events)
     (throw (ex-info "Dedupe :window-events must be a positive integer"
                     {:reason :invalid-dedupe-options
                      :window-events window-events})))
   (let [dataset-root (reader/dataset-root reader)
         capture-id (reader/capture-id reader)
         capture-sha (analysis/capture-manifest-sha (reader/directory reader))
         {:keys [generation directory relative]}
         (analysis/new-generation dataset-root :dedupe)]
     (analysis/report! progress {:stage :dedupe
                                 :total-events (reader/event-count reader)
                                 :processed-events 0})
     (try
       (let [image (DataOutputStream.
                    (BufferedOutputStream.
                     (FileOutputStream. (io/file directory "image.bin"))))
             state (-> (initial-state image window-events max-events progress
                                      (:max-window-occurrences configuration)
                                      (:leveling-threshold-events configuration))
                       (process-all! reader)
                       (close-all!))
             state (if (pos? (.size ^java.util.ArrayList (:occurrences (:window state))))
                     (flush! state)
                     state)
             _ (.flush image)
             _ (.close image)
             _ (write-instructions! directory (:instructions state))
             _ (write-blocks! directory (:blocks state))
             _ (write-contexts! directory (:contexts state))
             _ (write-windows-index! directory (:windows state))
             index (write-index! directory generation capture-id capture-sha
                                 state window-events configuration)]
         (analysis/commit-stage!
          dataset-root :dedupe
          {:generation generation
           :directory relative
           :version dedupe-version
           :capture-id capture-id
           :capture-manifest-sha256 capture-sha
           :window-events window-events
           :configuration configuration
           :summary (select-keys index [:counts :event-count])})
         {:dataset-root dataset-root
          :directory (str directory)
          :index index})
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

(defn build-async!
  "Run `build!` on a background future and return a deref-able progress handle.

  Dereference the handle for `:status`, `:processed-events`, `:total-events`,
  and `:windows-written`; on completion it also carries `:result`."
  ([reader] (build-async! reader {}))
  ([reader options]
   (analysis/start-async!
    {:stage :dedupe
     :total-events (reader/event-count reader)
     :processed-events 0}
    (fn [handle]
      (analysis/summarize-result
       (build! reader (assoc options :progress handle)))))))

;; Chunk 2: readers ----------------------------------------------------------

(defn- read-instruction-records [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* instruction-record-size index)]
            {:pc (contract/decode-u16 bytes offset)
             :opcode (contract/unsigned (aget bytes (+ offset 2)))
             :width (contract/unsigned (aget bytes (+ offset 3)))
             :template [(contract/unsigned (aget bytes (+ offset 4)))
                        (contract/unsigned (aget bytes (+ offset 5)))]
             :volatile-mask (contract/unsigned (aget bytes (+ offset 6)))}))
        (range (quot (alength bytes) instruction-record-size))))

(defn- read-block-index [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* block-index-record-size index)]
            {:offset (contract/decode-u64 bytes offset)
             :count (contract/decode-u32 bytes (+ offset 8))}))
        (range (quot (alength bytes) block-index-record-size))))

(defn- read-context-index [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* context-index-record-size index)]
            {:offset (contract/decode-u64 bytes offset)
             :length (contract/decode-u32 bytes (+ offset 8))}))
        (range (quot (alength bytes) context-index-record-size))))

(defn- read-window-index [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* window-index-record-size index)]
            {:start (contract/decode-u64 bytes offset)
             :end (contract/decode-u64 bytes (+ offset 8))
             :fragment-offset (contract/decode-u64 bytes (+ offset 16))
             :timing-offset (contract/decode-u64 bytes (+ offset 24))
             :port-offset (contract/decode-u64 bytes (+ offset 32))
             :switch-offset (contract/decode-u64 bytes (+ offset 40))
             :fragment-length (contract/decode-u32 bytes (+ offset 48))
             :timing-length (contract/decode-u32 bytes (+ offset 52))
             :port-length (contract/decode-u32 bytes (+ offset 56))
             :switch-length (contract/decode-u32 bytes (+ offset 60))}))
        (range (quot (alength bytes) window-index-record-size))))

(defn- file-bytes ^bytes [file]
  (Files/readAllBytes (.toPath (io/file file))))

(defn- verify-artifact!
  "Verify an artifact spec's size and SHA-256, recursing into nested
  `:index`/`:image` specs."
  [directory key spec]
  (let [file (io/file directory (:file spec))]
    (when-not (= (:size spec) (.length file))
      (throw (ex-info "Dedupe artifact size mismatch"
                      {:reason :dedupe-artifact-size-mismatch
                       :artifact key :file (:file spec)})))
    (when-not (= (:sha256 spec) (store/sha256-file file))
      (throw (ex-info "Dedupe artifact checksum mismatch"
                      {:reason :dedupe-artifact-checksum-mismatch
                       :artifact key :file (:file spec)})))
    (doseq [[sub sub-spec] (select-keys spec [:index :image])]
      (verify-artifact! directory (str (name key) "/" (name sub)) sub-spec))))

(defn open
  "Open the committed `:dedupe` generation for a dataset root.

  Validates the analysis manifest, index format/version, and artifact
  checksums, then loads the compact dictionaries and window index. The window
  image payloads stay on disk and are read per range."
  [dataset-root]
  (let [manifest (read-manifest dataset-root)
        stage (get-in manifest [:stages :dedupe])]
    (when-not stage
      (throw (ex-info "Dataset has no committed :dedupe generation"
                      {:reason :missing-dedupe-generation
                       :dataset-root dataset-root})))
    (let [directory (io/file dataset-root (:directory stage))
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= contract/dedupe-format (:format index))
        (throw (ex-info "Not a v2 dedupe image"
                        {:reason :not-a-dedupe-image
                         :format (:format index)})))
      (when-not (= dedupe-version (:version index))
        (throw (ex-info "Unsupported dedupe image version"
                        {:reason :unsupported-dedupe-version
                         :version (:version index)})))
      (doseq [[key spec] (:files index)]
        (verify-artifact! directory key spec))
      (let [context-bytes (file-bytes (io/file directory "contexts.bin"))
            context-index (read-context-index
                           (file-bytes (io/file directory "contexts.idx")))
            context-kinds (mapv (fn [{:keys [offset]}]
                                  (get context-kinds-by-code
                                       (contract/unsigned (aget context-bytes offset))
                                       :main))
                                context-index)
            context-rom? (mapv (fn [{:keys [offset]}]
                                 (pos? (contract/unsigned
                                        (aget context-bytes (inc (long offset))))))
                               context-index)
            system-contexts (into #{}
                                  (keep-indexed
                                   (fn [context-id kind]
                                     (when (and (contains? #{:irq :nmi :brk} kind)
                                                (nth context-rom? context-id))
                                       context-id)))
                                  context-kinds)]
        {:dataset-root dataset-root
         :directory directory
         :index index
         :instructions (read-instruction-records
                        (file-bytes (io/file directory "instructions.bin")))
         :block-bytes (file-bytes (io/file directory "blocks.bin"))
         :block-index (read-block-index
                       (file-bytes (io/file directory "blocks.idx")))
         :context-bytes context-bytes
         :context-index context-index
         :context-kinds context-kinds
         :context-rom? context-rom?
         :system-contexts system-contexts
         :levelings (:levelings index)
         :windows (read-window-index
                   (file-bytes (io/file directory "windows.idx")))
         :image-file (io/file directory "image.bin")}))))

(defn status
  "Return the committed dedupe stage entry for a dataset, or nil."
  [dataset-root]
  (analysis/stage-entry dataset-root :dedupe))

(defn system-contexts
  "Context ids whose handler is a stock system routine in the KERNAL ROM.

  These are execution contexts but never part of a program's behaviour; the
  default IRQ service routine is the usual example. Consumers can skip them."
  [trace]
  (:system-contexts trace))

(defn read-block
  "Return the instruction identities of a global block."
  [trace block-id]
  (let [{:keys [offset count]} (nth (:block-index trace) block-id)
        bytes ^bytes (:block-bytes trace)
        ids (mapv #(contract/decode-u32 bytes (+ offset (* 4 %)))
                  (range count))]
    {:block-id block-id
     :instruction-ids ids
     :instructions (mapv (:instructions trace) ids)}))

(defn read-context
  "Return a context's kind, handler PC, and context-qualified CFG edges."
  [trace context-id]
  (let [{:keys [offset]} (nth (:context-index trace) context-id)
        bytes ^bytes (:context-bytes trace)
        handler-pc (contract/decode-u16 bytes (+ offset 2))
        edge-count (contract/decode-u32 bytes (+ offset 4))
        edges (mapv (fn [index]
                      (let [edge-offset (+ offset context-header-size
                                           (* context-edge-size index))]
                        {:from (contract/decode-u32 bytes edge-offset)
                         :to (contract/decode-u32 bytes (+ edge-offset 4))
                         :count (contract/decode-u32 bytes (+ edge-offset 8))}))
                    (range edge-count))]
    {:context-id context-id
     :kind (get context-kinds-by-code (contract/unsigned (aget bytes offset)) :main)
     :rom? (pos? (contract/unsigned (aget bytes (inc (long offset)))))
     :handler-pc (when (not= :main (get context-kinds-by-code
                                        (contract/unsigned (aget bytes offset)) :main))
                   handler-pc)
     :edge-count edge-count
     :edges edges}))

(defn- read-slice ^bytes [^RandomAccessFile file offset length]
  (let [bytes (byte-array length)]
    (.seek file offset)
    (.readFully file bytes)
    bytes))

(defn- decode-occurrence-channel [^bytes bytes]
  (loop [offset 0 acc []]
    (if (< offset (alength bytes))
      (let [occurrence-id (contract/decode-u32 bytes offset)
            frame-id (contract/decode-u32 bytes (+ offset 4))
            context-id (contract/decode-u32 bytes (+ offset 8))
            block-id (contract/decode-u32 bytes (+ offset 12))
            count (int (contract/decode-u32 bytes (+ offset 16)))
            fragments (mapv (fn [index]
                              (let [fragment-offset (+ offset 20 (* 16 index))]
                                [(contract/decode-u64 bytes fragment-offset)
                                 (contract/decode-u64 bytes (+ fragment-offset 8))]))
                            (range count))]
        (recur (+ offset 20 (* 16 count))
               (conj acc {:occurrence-id occurrence-id
                          :frame-id frame-id
                          :context-id context-id
                          :block-id block-id
                          :fragments fragments})))
      acc)))

(defn- decode-timing-channel [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* timing-record-size index)]
            {:start-cycle (contract/decode-u64 bytes offset)
             :end-cycle (contract/decode-u64 bytes (+ offset 8))}))
        (range (quot (alength bytes) timing-record-size))))

(defn- decode-port-channel [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* port-record-size index)]
            {:event-index (contract/decode-u64 bytes offset)
             :value (contract/unsigned (aget bytes (+ offset 8)))}))
        (range (quot (alength bytes) port-record-size))))

(defn- decode-switch-channel [^bytes bytes]
  (mapv (fn [index]
          (let [offset (* switch-record-size index)
                kind (contract/unsigned (aget bytes (+ offset 8)))]
            {:event-index (contract/decode-u64 bytes offset)
             :kind (case (int kind) 0 :irq 1 :nmi 2 :brk 3 :return)
             :frame-id (contract/decode-u32 bytes (+ offset 12))
             :context-id (contract/decode-u32 bytes (+ offset 16))}))
        (range (quot (alength bytes) switch-record-size))))

(defn- intersects? [start end fragments]
  (boolean (some (fn [[fragment-start fragment-end]]
                   (and (< fragment-start end) (> fragment-end start)))
                 fragments)))

(defn- in-range? [start end index]
  (and (<= start index) (< index end)))

(defn- effective-level
  "Return an occurrence's timeline level without changing its observed context.

  Dedupe forces an occurrence boundary at every `:level-start`, so checking its
  first fragment is sufficient and does not split or duplicate occurrences."
  [context-kinds levelings frame-id context-id start]
  (if (some (fn [{leveling-frame :frame-id :keys [level-start effective-level]
                  [_ level-end] :event-range}]
              (and (= leveling-frame frame-id)
                   (<= (long level-start) (long start))
                   (< (long start) (long level-end))
                   effective-level))
            levelings)
    :mainline
    (if (= :main (get context-kinds context-id :irq)) :mainline :irq)))

(defn- consume-range-window [^RandomAccessFile file window start end acc
                             context-kinds levelings]
  (let [occurrences (decode-occurrence-channel
                     (read-slice file (:fragment-offset window)
                                 (:fragment-length window)))
        timings (decode-timing-channel
                 (read-slice file (:timing-offset window) (:timing-length window)))
        ports (decode-port-channel
               (read-slice file (:port-offset window) (:port-length window)))
        switches (decode-switch-channel
                  (read-slice file (:switch-offset window) (:switch-length window)))]
    (-> acc
        (update :occurrences
                into
                (keep-indexed
                 (fn [index occurrence]
                   (when (intersects? start end (:fragments occurrence))
                     (let [context-kind (get context-kinds (:context-id occurrence) :irq)
                           level (effective-level context-kinds levelings
                                                  (:frame-id occurrence)
                                                  (:context-id occurrence)
                                                  (ffirst (:fragments occurrence)))]
                       (assoc (merge occurrence (nth timings index))
                              :context-kind context-kind
                              :effective-level level))))
                 occurrences))
        (update :port-timeline into
                (filter #(in-range? start end (:event-index %)) ports))
        (update :context-switches into
                (filter #(in-range? start end (:event-index %)) switches)))))

(defn read-range
  "Return the high-level dedupe objects intersecting `[start end)`.

  The result carries `:occurrences` (each with context, frame, fragments, timing,
  and effective level), the CPU-port `:port-timeline`, `:context-switches`, and
  the frame `:levelings` whose continuation begins inside the range. Logical
  ranges cross raw capture chunk boundaries transparently."
  [trace start end]
  (contract/validate-event-range! [start end])
  (let [windows (filter #(and (< (:start %) end) (> (:end %) start))
                        (:windows trace))]
    (with-open [file (RandomAccessFile. (io/file (:image-file trace)) "r")]
      (-> (reduce (fn [acc window]
                    (consume-range-window file window start end acc
                                          (:context-kinds trace)
                                          (:levelings trace)))
                  {:event-range [start end]
                   :occurrences []
                   :port-timeline []
                   :context-switches []}
                  windows)
          (assoc :levelings
                 (filterv #(in-range? start end (:level-start %))
                          (:levelings trace)))))))

(defn port-at
  "Return the CPU-port `$01` value in effect immediately before `event-index`.

  Port changes are rare, so this scans the port channel window by window from
  the latest backwards and stops at the first window holding a change before
  the index. Returns the power-on `$37` when no change precedes the index."
  [trace event-index]
  (let [index (long event-index)]
    (with-open [file (RandomAccessFile. (io/file (:image-file trace)) "r")]
      (or (some (fn [window]
                  (when (and (< (long (:start window)) index)
                             (pos? (long (:port-length window))))
                    (let [ports (decode-port-channel
                                 (read-slice file
                                             (:port-offset window)
                                             (:port-length window)))
                          before (take-while #(< (long (:event-index %)) index) ports)]
                      (when (seq before)
                        (:value (last before))))))
                (reverse (:windows trace)))
          0x37))))

;; Streaming occurrence channel -------------------------------------------------

(def occurrence-field-count
  "Length of the `long-array` used by `scan-occurrences`."
  6)

(def ^:const oc-id 0)
(def ^:const oc-frame 1)
(def ^:const oc-context 2)
(def ^:const oc-block 3)
(def ^:const oc-start 4)
(def ^:const oc-end 5)

(defn- u8
  ^long [^bytes bytes ^long offset]
  (bit-and (aget bytes offset) 0xff))

(defn- u32
  ^long [^bytes bytes ^long offset]
  (bit-or (u8 bytes offset)
          (bit-shift-left (u8 bytes (+ offset 1)) 8)
          (bit-shift-left (u8 bytes (+ offset 2)) 16)
          (bit-shift-left (u8 bytes (+ offset 3)) 24)))

(defn- u64
  ^long [^bytes bytes ^long offset]
  (bit-or (u32 bytes offset)
          (bit-shift-left (u32 bytes (+ offset 4)) 32)))

(defn- decode-occurrences-into!
  [^longs buffer ^bytes bytes rf acc]
  (let [length (alength bytes)]
    (loop [offset (long 0) acc acc]
      (if (< offset length)
        (let [occurrence-id (u32 bytes offset)
              frame-id (u32 bytes (+ offset 4))
              context-id (u32 bytes (+ offset 8))
              block-id (u32 bytes (+ offset 12))
              fragment-count (int (u32 bytes (+ offset 16)))]
          (aset buffer oc-id occurrence-id)
          (aset buffer oc-frame frame-id)
          (aset buffer oc-context context-id)
          (aset buffer oc-block block-id)
          (aset buffer oc-start (u64 bytes (+ offset 20)))
          (aset buffer oc-end (u64 bytes (+ offset 20 (* 16 (dec fragment-count)))))
          (let [acc (rf acc buffer)]
            (if (reduced? acc)
              acc
              (recur (+ offset 20 (* 16 fragment-count)) acc))))
        acc))))

(defn scan-occurrences
  "Reduce `rf` over occurrence records intersecting `event-range`, without
  building per-occurrence maps.

  `rf` is called as `(rf acc buffer)` where `buffer` is a reusable `long-array`
  laid out by the `oc-*` slots: occurrence id, frame id, context id, block id,
  first-fragment start, and last-fragment end. The buffer is valid only for the
  duration of the call. A `reduced` accumulator stops the scan and is
  dereferenced. Windows wholly outside the range are skipped without I/O."
  [trace event-range rf init]
  (let [[start end] (contract/validate-event-range! event-range)
        start (long start)
        end (long end)
        buffer (long-array occurrence-field-count)]
    (with-open [file (RandomAccessFile. (io/file (:image-file trace)) "r")]
      (loop [windows (:windows trace) acc init]
        (if (and (seq windows) (not (reduced? acc)))
          (let [window (first windows)]
            (if (and (< (long (:start window)) end)
                     (> (long (:end window)) start))
              (let [acc (decode-occurrences-into!
                         buffer
                         (read-slice file (:fragment-offset window)
                                     (:fragment-length window))
                         rf acc)]
                (if (reduced? acc)
                  (deref acc)
                  (recur (next windows) acc)))
              (recur (next windows) acc)))
          (if (reduced? acc) (deref acc) acc))))))
