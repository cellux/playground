(ns omkamra.vice.decoder.stream
  "Trace-stream decoding, control-flow structure, and raw chunk projection."
  (:require [omkamra.vice.asm :as asm]
            [omkamra.vice.decoder.write :as write]))

;; Compact trace-record and occurrence layouts shared with the FIFO recorder.
(def trace-pc 0)
(def trace-bytes 1)
(def trace-raster-line 2)
(def trace-cpu-cycle 3)
(def trace-a 4)
(def trace-x 5)
(def trace-y 6)
(def trace-flags 8)
(def occurrence-event 0)
(def occurrence-instruction-id 1)
(def occurrence-instruction 2)
(def occurrence-boundary? 3)
(def occurrence-event-index 4)

(def ^:private basic-block-terminators
  #{"BRK" "JMP" "JSR" "RTI" "RTS" "KIL"
    "BCC" "BCS" "BEQ" "BMI" "BNE" "BPL" "BVC" "BVS"})

(defn basic-block-terminator?
  [instruction]
  (contains? basic-block-terminators (:mnemonic instruction)))

(defn basic-block-builder-xf
  "Group chronological instruction occurrences into dynamic basic blocks.

  A block ends after every control-transfer instruction or explicit boundary.
  Completion flushes a final fall-through block, so a finite FIFO capture never
  loses its tail."
  ([]
   (basic-block-builder-xf (constantly false)))
  ([boundary?]
   (fn [rf]
     (let [block (volatile! nil)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if-let [pending @block]
                         (rf result pending)
                         result)]
            (vreset! block nil)
            (rf result)))
         ([result occurrence]
          (let [event-index (nth occurrence occurrence-event-index)
                instruction-id (nth occurrence occurrence-instruction-id)
                instruction (nth occurrence occurrence-instruction)
                result (if (and @block (boundary? occurrence))
                         (let [result (rf result @block)]
                           (vreset! block nil)
                           result)
                         result)
                pending (or @block
                            {:start-index event-index
                             :instruction-ids []})
                pending (-> pending
                            (update :instruction-ids conj instruction-id)
                            (assoc :end-index (inc event-index)))]
            (if (basic-block-terminator? instruction)
              (do
                (vreset! block nil)
                (rf result pending))
              (do
                (vreset! block pending)
                result)))))))))

(defn deduplicate-basic-blocks-xf
  "Intern basic-block instruction-ID vectors and emit chronological block runs."
  [block-state]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [instruction-ids] :as block}]
       (let [instruction-ids (vec instruction-ids)
             {:keys [block-ids blocks]} @block-state
             block-id (or (get block-ids instruction-ids) (count blocks))]
         (when-not (contains? block-ids instruction-ids)
           (swap! block-state (fn [state]
                                (-> state
                                    (assoc-in [:block-ids instruction-ids] block-id)
                                    (update :blocks conj
                                            {:id block-id
                                             :instruction-ids instruction-ids})))))
         (rf result (-> block
                        (dissoc :instruction-ids)
                        (assoc :block-id block-id
                               :iterations 1))))))))

(declare control-flow-targets)

(defn split-block-at-control-flow-targets-xf
  "Split provisional blocks at backward control-flow targets.

  The ordinary block builder cannot know that a fall-through address is a
  target until it later sees the branch instruction. It retains the complete
  provisional block until its terminator, so this transducer can split that
  block retroactively before it is interned."
  [instruction-by-id initial-memory]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [instruction-ids] :as block}]
       (let [instruction-ids (vec instruction-ids)
             instructions (mapv instruction-by-id instruction-ids)
             targets (->> instructions
                          (mapcat #(or (control-flow-targets
                                        initial-memory %)
                                       #{}))
                          set)
             split-indices (->> instructions
                                (map-indexed vector)
                                (keep (fn [[index instruction]]
                                        (when (and (pos? index)
                                                   (contains?
                                                    targets
                                                    (or (:pc instruction)
                                                        (:address instruction))))
                                          index)))
                                vec)
             boundaries (concat [0] split-indices [(count instruction-ids)])]
         (reduce (fn [result [start end]]
                   (if (< start end)
                     (rf result
                         (assoc block
                                :start-index (+ (:start-index block) start)
                                :end-index (+ (:start-index block) end)
                                :instruction-ids (subvec instruction-ids start end)))
                     result))
                 result
                 (map vector boundaries (rest boundaries))))))))

(def ^:private conditional-branch-mnemonics
  #{"BCC" "BCS" "BEQ" "BMI" "BNE" "BPL" "BVC" "BVS"})

(def ^:private dynamic-transfer-mnemonics
  #{"RTS" "RTI" "KIL"})

(defn instruction-pc
  [instruction]
  (or (:pc instruction) (:address instruction)))

(defn absolute-operand
  [instruction]
  (let [bytes (:bytes instruction)]
    (bit-or (write/u8 (nth bytes 1 0))
            (bit-shift-left (write/u8 (nth bytes 2 0)) 8))))

(defn indirect-jump-target
  [memory instruction]
  (let [pointer (absolute-operand instruction)
        high-address (bit-or (bit-and pointer 0xff00)
                             (bit-and (inc pointer) 0xff))]
    (bit-or (write/memory-byte memory pointer)
            (bit-shift-left (write/memory-byte memory high-address) 8))))

(defn relative-target
  [instruction]
  (let [pc (instruction-pc instruction)
        offset (write/u8 (nth (:bytes instruction) 1 0))]
    (bit-and (+ pc 2 (if (< offset 128) offset (- offset 256))) 0xffff)))

(defn control-flow-successors
  "Return statically knowable PCs that may follow `instruction`.

  A nil result means that the instruction has a dynamic return target. Such
  instructions are block terminators, but their successors cannot by
  themselves distinguish a return from an asynchronous transfer."
  [memory instruction]
  (let [pc (instruction-pc instruction)
        fall-through (bit-and (+ pc (asm/instruction-width instruction)) 0xffff)
        mnemonic (:mnemonic instruction)]
    (cond
      (contains? conditional-branch-mnemonics mnemonic)
      ;; A branch can target its own fall-through address (for example a
      ;; zero-offset branch).  Build this set dynamically so duplicate
      ;; successors are deduplicated instead of throwing.
      (set [fall-through (relative-target instruction)])

      (= "JMP" mnemonic)
      (case (:mode instruction)
        :abs #{(absolute-operand instruction)}
        :ind #{(indirect-jump-target memory instruction)}
        #{})

      (= "JSR" mnemonic)
      #{(absolute-operand instruction)}

      (= "BRK" mnemonic)
      #{(write/memory-word memory 0xfffe)}

      (contains? dynamic-transfer-mnemonics mnemonic)
      nil

      :else
      #{fall-through})))

(defn control-flow-targets
  "Return non-fall-through targets introduced by `instruction`."
  [memory instruction]
  (let [mnemonic (:mnemonic instruction)]
    (cond
      (contains? conditional-branch-mnemonics mnemonic)
      #{(relative-target instruction)}

      (= "JMP" mnemonic)
      (case (:mode instruction)
        :abs #{(absolute-operand instruction)}
        :ind (if memory
               #{(indirect-jump-target memory instruction)}
               #{})
        #{})

      (= "JSR" mnemonic)
      #{(absolute-operand instruction)}

      (= "BRK" mnemonic)
      (if memory #{(write/memory-word memory 0xfffe)} #{})

      :else
      #{})))

(defn unexpected-control-flow?
  [memory previous current]
  (let [successors (control-flow-successors memory previous)]
    (and (seq successors)
         (not (contains? successors (instruction-pc current))))))

(def ^:private stream-events-format
  :omkamra.vice/instruction-block-stream-v4)

(def ^:private stream-sample-keys
  [:raster-line :cpu-cycle :a :x :y :sp :flags :global-cycle])

;; Occurrence layout is declared with the trace layout above because the
;; basic-block transducers consume it before the stream ingester is defined.

(defn ram-code-pc?
  [pc {:keys [ram-start ram-end]
       :or {ram-start 0x0400
            ram-end 0xe000}}]
  (and (integer? pc)
       (<= ram-start pc)
       (< pc ram-end)))

;; Occurrence layout is declared with the trace layout above because the
;; basic-block transducers consume it before the stream ingester is defined.

(defn stream-instruction-ids
  [events]
  (let [blocks (:blocks events)]
    (mapcat (fn [[block-id iterations]]
              (apply concat
                     (repeat iterations
                             (:instruction-ids (nth blocks block-id)))))
            (:block-runs events))))

(defn empty-stream-state
  []
  {:status :starting
   :block-runs []
   :event-count 0
   :error nil})

(defn intern-stream-instructions-xf
  "Intern parsed trace instructions and emit dynamic occurrences.

  The instruction dictionary is side state because it is a static table, while
  the reducer's accumulator remains the chronological capture state."
  [instruction-state]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result event]
       (let [pc (nth event trace-pc)
             bytes (nth event trace-bytes)
             key [pc bytes]
             {:keys [instruction-ids instructions]} @instruction-state
             instruction-id (or (get instruction-ids key) (count instructions))
             instruction (or (nth instructions instruction-id nil)
                             (let [decoded (asm/disassemble-bytes pc bytes)]
                               {:id instruction-id
                                :pc pc
                                :bytes bytes
                                :mnemonic (:mnemonic decoded)
                                :mode (:mode decoded)
                                :operand (:operand decoded)
                                :text (:text decoded)}))]
         (when-not (contains? instruction-ids key)
           (swap! instruction-state
                  (fn [state]
                    (-> state
                        (assoc-in [:instruction-ids key] instruction-id)
                        (update :instructions conj instruction)))))
         (rf result [event instruction-id instruction false nil]))))))

(defn make-analysis-state
  [initial-memory retain-samples?]
  {:memory (byte-array (map unchecked-byte initial-memory))
   :writes (java.util.ArrayList.)
   :boundaries (java.util.ArrayList.)
   :boundary-timings (java.util.HashMap.)
   :pending nil
   :retain-samples? retain-samples?
   :samples (when retain-samples? (java.util.ArrayList.))})

(defn append-analysis-write!
  [analysis pending next-event]
  (let [event (nth pending occurrence-event)
        instruction (nth pending occurrence-instruction)
        memory (:memory @analysis)
        duration-cycles (when (and next-event
                                   (nth event trace-raster-line)
                                   (nth event trace-cpu-cycle)
                                   (nth next-event trace-raster-line)
                                   (nth next-event trace-cpu-cycle))
                          (write/elapsed-cycles event next-event 63 312))
        write (write/inferred-write memory instruction event)]
    (when write
      (let [write-event (merge {:pc (nth event trace-pc)
                                :mnemonic (:mnemonic instruction)
                                :event-index (nth pending occurrence-event-index)}
                               (if (and (nth event trace-raster-line)
                                        (nth event trace-cpu-cycle))
                                 (write/write-timing event duration-cycles 63)
                                 {})
                               (dissoc write :inferred?))]
        (.add ^java.util.ArrayList (:writes @analysis) write-event)
        (aset-byte memory (:address write)
                   (unchecked-byte (:value write)))))))

;; Hardware interrupt vectors -----------------------------------------------

(def kernal-irq-entry
  "CPU address of the KERNAL IRQ/BRK entry visible at `$fffe/$ffff`."
  0xff48)

(def default-cpu-port
  "Assumed CPU port when a chunk carries no memory snapshot."
  0x37)

(defn io-visible?
  "True when the CPU port maps the I/O area at `$d000-$dfff`."
  [port]
  (and (pos? (bit-and (int port) 0x03))
       (pos? (bit-and (int port) 0x04))))

(defn kernal-visible?
  "True when the CPU port maps KERNAL ROM at `$e000-$ffff`."
  [port]
  (pos? (bit-and (int port) 0x02)))

(defn effective-irq-vector*
  "Return the CPU address the 6510 uses for IRQ/BRK."
  [port ram-vector]
  (if (kernal-visible? port)
    kernal-irq-entry
    (bit-and (int ram-vector) 0xffff)))

(defn effective-irq-vector
  "Return the hardware IRQ vector for the current memory image."
  [^bytes memory]
  (effective-irq-vector*
   (bit-and (int (aget memory 1)) 0xff)
   (write/memory-word memory 0xfffe)))

(defn resolve-irq-handler
  "Return the user IRQ handler a span actually executes.

  The KERNAL entry dispatches through `$0314/$0315`, so its address carries no
  per-part information; resolve it to the installed handler. A span that starts
  at a banked-out RAM vector already begins at its handler."
  [entry-pc ram-0314-value]
  (if (= entry-pc kernal-irq-entry)
    (bit-and (int ram-0314-value) 0xffff)
    entry-pc))

(defn cpu-port-values
  "Return the CPU port `$01` value immediately before each local event.

  Returns nil when the chunk carries no memory snapshot, which tells callers
  to fall back to address-only classification."
  [chunk event-count]
  (when-let [initial-memory (get-in chunk [:stages :memory :initial])]
    (let [initial-port (bit-and (int (nth initial-memory 1 0)) 0xff)
          port-writes (->> (get-in chunk [:stages :memory :writes])
                           write/write-records
                           (filter #(= 1 (:address %)))
                           vec)
          ports (int-array (max 0 event-count))]
      (loop [event-index 0
             port initial-port
             write-index 0]
        (when (< event-index event-count)
          (let [[port write-index]
                (loop [port port
                       write-index write-index]
                  (if (and (< write-index (count port-writes))
                           (< (:event-index (nth port-writes write-index))
                              event-index))
                    (recur (bit-and (int (:value (nth port-writes write-index)))
                                    0xff)
                           (inc write-index))
                    [port write-index]))]
            (aset-int ports event-index port)
            (recur (inc event-index) port write-index))))
      ports)))

(defn analyze-stream-occurrence!
  [analysis occurrence]
  (locking analysis
    (let [state @analysis
          pending (:pending state)
          event (nth occurrence occurrence-event)
          instruction (nth occurrence occurrence-instruction)
          event-index (nth occurrence occurrence-event-index)
          memory (:memory state)
          pc (:pc instruction)]
      (when pending
        (append-analysis-write! analysis pending event))
      ;; A boundary is either an unexpected control transfer (the existing
      ;; heuristic) or a landing on the effective `$fffe/$ffff` IRQ vector.
      ;; The vector test is independent of the preceding instruction, so it
      ;; still recognizes an IRQ whose entry follows an RTI. The KERNAL case
      ;; is a constant comparison, avoiding a memory read on every event.
      (when (or (nth occurrence occurrence-boundary?)
                (if (kernal-visible? (bit-and (int (aget ^bytes memory 1)) 0xff))
                  (= pc kernal-irq-entry)
                  (= pc (bit-and (write/memory-word memory 0xfffe) 0xffff))))
        (.add ^java.util.ArrayList (:boundaries state) event-index)
        (.put ^java.util.HashMap (:boundary-timings state)
              event-index
              [(nth event trace-raster-line) (nth event trace-cpu-cycle)]))
      (when (:retain-samples? state)
        (.add ^java.util.ArrayList (:samples state)
              (subvec event trace-raster-line)))
      (swap! analysis assoc :pending occurrence))))

(defn complete-analysis!
  [analysis]
  (locking analysis
    (when-let [pending (:pending @analysis)]
      (append-analysis-write! analysis pending nil)
      (swap! analysis assoc :pending nil))))

(defn assign-event-index-xf
  "Add chronological event indexes without changing reducer state."
  [event-count]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result occurrence]
       (let [indexed (assoc occurrence occurrence-event-index @event-count)]
         (vswap! event-count inc)
         (rf result indexed))))))

(defn collect-stream-analysis-xf
  "Collect compact analysis state without updating a persistent map per event."
  [analysis]
  (fn [rf]
    (fn
      ([] (rf))
      ([result]
       (complete-analysis! analysis)
       (rf result))
      ([result occurrence]
       (analyze-stream-occurrence! analysis occurrence)
       (rf result occurrence)))))

(defn mark-control-flow-boundaries-xf
  [memory]
  (fn [rf]
    (let [previous (volatile! nil)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result occurrence]
         (let [boundary? (and @previous
                              (unexpected-control-flow?
                               memory
                               (nth @previous occurrence-instruction)
                               (nth occurrence occurrence-instruction)))]
           (vreset! previous occurrence)
           (rf result (assoc occurrence occurrence-boundary? boundary?))))))))

(defn append-block-run
  [runs {:keys [block-id iterations]}]
  (if (and (seq runs) (= block-id (first (peek runs))))
    (let [index (dec (count runs))
          [_ prior-iterations] (peek runs)]
      (assoc runs index [block-id (+ prior-iterations iterations)]))
    (conj runs [block-id iterations])))

(defn make-stream-ingester
  "Create one fused transducer pipeline for a FIFO capture.

  Parsed records flow through clearly separated stages: instruction interning,
  event indexing, control-flow boundary marking, compact write/timing
  analysis, basic-block construction, target splitting, and block interning.
  Full per-event register samples are retained only when `:retain-samples?` is
  true."
  ([]
   (make-stream-ingester {}))
  ([{:keys [initial-memory retain-samples?]
     :or {initial-memory (byte-array 65536)
          retain-samples? false}}]
   (let [instruction-state (atom {:instruction-ids {} :instructions []})
         block-state (atom {:block-ids {} :blocks []})
         analysis-state (atom (make-analysis-state initial-memory
                                                   retain-samples?))
         event-count (volatile! 0)
         xf (comp (intern-stream-instructions-xf instruction-state)
                  (assign-event-index-xf event-count)
                  (mark-control-flow-boundaries-xf initial-memory)
                  (collect-stream-analysis-xf analysis-state)
                  (basic-block-builder-xf #(nth % occurrence-boundary?))
                  (split-block-at-control-flow-targets-xf
                   #(nth (:instructions @instruction-state) %)
                   initial-memory)
                  (deduplicate-basic-blocks-xf block-state))
         reducer (xf (fn
                       ([] (empty-stream-state))
                       ([result] result)
                       ([result block-run]
                        (update result :block-runs append-block-run block-run))))]
     {:step (fn [state event] (reducer state event))
      :complete (fn [state] (reducer state))
      :instruction-state instruction-state
      :block-state block-state
      :analysis-state analysis-state
      :event-count event-count
      :retain-samples? retain-samples?})))

(defn analysis-snapshot
  [analysis]
  (let [state @analysis]
    {:writes (vec (.toArray ^java.util.ArrayList (:writes state)))
     :boundaries (vec (.toArray ^java.util.ArrayList (:boundaries state)))
     :boundary-timings (into {} (.entrySet ^java.util.HashMap
                                 (:boundary-timings state)))}))

(defn instruction-block-stream
  [stream-state ingester]
  (let [{:keys [block-runs]} @stream-state
        event-count @(:event-count ingester)
        {:keys [instructions]} @(:instruction-state ingester)
        {:keys [blocks]} @(:block-state ingester)
        analysis @(:analysis-state ingester)]
    (cond-> {:format stream-events-format
             :event-count event-count
             :instructions instructions
             :blocks blocks
             :block-runs block-runs}
      (:retain-samples? ingester)
      (assoc :sample-keys stream-sample-keys
             :samples (vec (.toArray ^java.util.ArrayList (:samples analysis)))))))

(defn decoded-stream-instructions
  [events]
  (mapv (fn [{:keys [id pc bytes]}]
          (let [decoded (asm/disassemble-bytes pc bytes)]
            (assoc decoded :id id :address pc)))
        (:instructions events)))

(defn stream-instruction-id-array
  [events]
  (let [ids (int-array (:event-count events))]
    (loop [event-index 0
           instruction-ids (seq (stream-instruction-ids events))]
      (if-let [instruction-id (first instruction-ids)]
        (do (aset-int ids event-index (int instruction-id))
            (recur (inc event-index) (next instruction-ids)))
        ids))))

(defn sample-map
  [events sample]
  (zipmap (:sample-keys events) sample))

(defn stream-event-at
  [events instructions instruction-ids analysis event-index]
  (let [instruction-id (aget ^ints instruction-ids event-index)
        instruction (nth instructions instruction-id)
        samples (:samples events)
        timing (get (:boundary-timings analysis) event-index)]
    (cond-> (-> (select-keys instruction
                             [:bytes :mnemonic :mode :operand :text])
                (assoc :pc (:address instruction)))
      samples (merge (sample-map events (nth samples event-index)))
      timing (assoc :raster-line (first timing)
                    :cpu-cycle (second timing)))))

(defn stream-node-versions
  [instructions instruction-ids]
  (let [address-state (java.util.HashMap.)
        versions (java.util.ArrayList.)]
    (dotimes [event-index (alength ^ints instruction-ids)]
      (let [instruction-id (aget ^ints instruction-ids event-index)
            address (:address (nth instructions instruction-id))
            prior (.get address-state address)]
        (if (and prior (= instruction-id (aget ^ints prior 0)))
          (aset-int ^ints prior 4 event-index)
          (let [node-id (.size versions)
                version (if prior (inc (aget ^ints prior 2)) 0)
                node (int-array [instruction-id node-id version
                                 event-index event-index address])]
            (.add versions node)
            (.put address-state address node)))))
    (mapv (fn [^ints node]
            {:id (aget node 1)
             :address (aget node 5)
             :version (aget node 2)
             :instruction-id (aget node 0)
             :first-event (aget node 3)
             :last-event (aget node 4)})
          versions)))

(defn occurrence-pcs
  "Expand the run-length encoded event stream into one PC per event."
  [events]
  (let [instructions (:instructions events)
        blocks (into {} (map (juxt :id identity) (:blocks events)))]
    (into []
          (for [[block-id iterations] (:block-runs events)
                _ (range iterations)
                instruction-id (:instruction-ids (get blocks block-id))]
            (:pc (nth instructions instruction-id))))))

(defn memory-word-values
  "Return the 16-bit value at `address` immediately before each local event."
  [initial-memory compact-writes address event-count]
  (let [initial-memory (or initial-memory [])
        writes (->> (write/write-records compact-writes)
                    (filter #(<= address (:address %) (inc address)))
                    vec)
        words (int-array (max 0 event-count))]
    (loop [event-index 0
           write-index 0
           low (write/u8 (nth initial-memory address 0))
           high (write/u8 (nth initial-memory (inc address) 0))]
      (if (< event-index event-count)
        (let [[low high write-index]
              (loop [low low
                     high high
                     write-index write-index]
                (if (and (< write-index (count writes))
                         (< (:event-index (nth writes write-index)) event-index))
                  (let [{write-address :address value :value}
                        (nth writes write-index)]
                    (recur (if (= write-address address) (write/u8 value) low)
                           (if (= write-address (inc address)) (write/u8 value) high)
                           (inc write-index)))
                  [low high write-index]))]
          (aset-int words event-index (bit-or low (bit-shift-left high 8)))
          (recur (inc event-index) write-index low high))
        words))))

(defn resolve-span-handlers
  "Attach the resolved user IRQ handler to every IRQ span of a raw chunk.

  Resolution needs the `$0314/$0315` timeline, which is only available when the
  chunk carries a memory snapshot. Chunks without one are returned unchanged."
  [chunk spans]
  (let [event-count (or (get-in chunk [:events :event-count]) 0)
        ports (cpu-port-values chunk event-count)
        ram-0314 (when ports
                   (memory-word-values
                    (get-in chunk [:stages :memory :initial])
                    (get-in chunk [:stages :memory :writes])
                    0x0314 event-count))]
    (if (nil? ram-0314)
      spans
      (mapv (fn [span]
              (if (= :irq (:kind span))
                (assoc span :handler-pc
                       (resolve-irq-handler
                        (:entry-pc span)
                        (aget ^ints ram-0314 (:start-index span))))
                span))
            spans))))

(defn write-raster-values
  "Return the nearest preceding committed raster line for each local event."
  [compact-writes event-count]
  (let [writes (->> (write/write-records compact-writes)
                    (filter :raster-line)
                    vec)
        rasters (int-array (max 0 event-count))]
    (loop [event-index 0
           write-index 0
           raster 0]
      (if (< event-index event-count)
        (let [[raster write-index]
              (loop [raster raster
                     write-index write-index]
                (if (and (< write-index (count writes))
                         (< (:event-index (nth writes write-index)) event-index))
                  (recur (int (:raster-line (nth writes write-index)))
                         (inc write-index))
                  [raster write-index]))]
          (aset-int rasters event-index raster)
          (recur (inc event-index) write-index raster))
        rasters))))

(defn recover-irq-boundaries
  "Re-derive hardware IRQ entry events for one persisted raw chunk.

  Capture-time boundaries rely on unexpected control flow, which misses an IRQ
  whose entry follows RTI. This walks the persisted event stream again and
  marks every occurrence that lands on the effective `$fffe/$ffff` vector,
  reusing stored timings and write-record raster lines for the new entries."
  [chunk]
  (let [events (:events chunk)
        event-count (or (:event-count events) 0)
        pcs (occurrence-pcs events)
        ports (cpu-port-values chunk event-count)
        vector-words (memory-word-values
                      (get-in chunk [:stages :memory :initial])
                      (get-in chunk [:stages :memory :writes])
                      0xfffe event-count)
        rasters (write-raster-values (get-in chunk [:stages :memory :writes])
                                     event-count)
        stored-timings (get-in chunk [:stages :structure :boundary-timings])
        boundaries
        (into []
              (keep (fn [event-index]
                      (when (= (nth pcs event-index)
                               (effective-irq-vector*
                                (if ports
                                  (aget ports event-index)
                                  default-cpu-port)
                                (aget vector-words event-index)))
                        event-index)))
              (range (min event-count (count pcs))))
        boundary-timings
        (into {}
              (map (fn [event-index]
                     [event-index
                      (or (get stored-timings event-index)
                          [(aget rasters event-index) nil])]))
              boundaries)]
    {:boundaries boundaries
     :boundary-timings boundary-timings}))

(defn stream-irq-data
  [events instructions instruction-ids analysis]
  (let [event-count (:event-count events)
        starts (:boundaries analysis)
        ranges (mapv
                (fn [[start next-start]]
                  (let [limit (or next-start event-count)
                        end (loop [index start]
                              (cond
                                (= index limit) limit
                                (= "RTI" (:mnemonic
                                          (nth instructions
                                               (aget ^ints instruction-ids index))))
                                (inc index)
                                :else (recur (inc index))))]
                    [start end]))
                (map vector starts (concat (rest starts) [nil])))
        span (fn [kind start end]
               (when (< start end)
                 (let [base {:kind kind :start-index start :end-index end}]
                   (if (= kind :irq)
                     (let [[raster-line cpu-cycle]
                           (get (:boundary-timings analysis) start)]
                       (assoc base
                              :trigger {:raster-line raster-line
                                        :cpu-cycle cpu-cycle}
                              :entry-pc (:address
                                         (nth instructions
                                              (aget ^ints instruction-ids start)))
                              :return-pc (:address
                                          (nth instructions
                                               (aget ^ints instruction-ids (dec end))))))
                     base))))
        irq-spans (->> ranges
                       (map (fn [[start end]] (span :irq start end)))
                       (sort-by (juxt (comp :raster-line :trigger)
                                      (comp :cpu-cycle :trigger)
                                      :start-index))
                       vec)
        non-irq-ranges
        (if (empty? ranges)
          [[0 event-count]]
          (->> (concat [[0 (ffirst ranges)]]
                       (map (fn [[[_ end] [next-start _]]]
                              [end next-start])
                            (partition 2 1 ranges))
                       [[(second (last ranges)) event-count]])
               (filter (fn [[start end]] (< start end)))))
        non-irq-spans (mapv (fn [[start end]] (span :non-irq start end))
                            non-irq-ranges)
        spans (->> (concat irq-spans non-irq-spans)
                   (sort-by :start-index)
                   vec)]
    {:spans spans}))

(defn first-stream-code-entry
  [events instructions instruction-ids analysis spans kind]
  (some (fn [{:keys [start-index end-index] :as span}]
          (when (= kind (:kind span))
            (loop [index start-index]
              (when (< index end-index)
                (let [pc (:address
                          (nth instructions
                               (aget ^ints instruction-ids index)))]
                  (if (ram-code-pc? pc {})
                    {:span-kind kind
                     :index index
                     :entry (stream-event-at events instructions
                                             instruction-ids analysis index)}
                    (recur (inc index))))))))
        spans))

(defn minimal-pipeline-artifact
  "Build the capture-time raw projection for one physical chunk.

  Expensive derived views such as node versions, IRQ spans, VIC timelines,
  frame-code detection, and assets belong to post-processing. Keeping this
  projection limited to replayable local dictionaries, compact writes,
  boundary timing, and memory snapshots keeps the FIFO producer independent of
  those analyses."
  [events initial-memory final-memory metadata analysis]
  (let [instructions (decoded-stream-instructions events)
        execution {:format :omkamra.vice/versioned-execution-v5
                   :event-count (:event-count events)
                   :instructions instructions
                   :blocks (:blocks events)
                   :block-runs (:block-runs events)}
        write-data (write/compact-write-data (:writes analysis))]
    {:format :omkamra.vice/chunk-pipeline-v1
     :raw {:metadata metadata}
     :stages {:decoded {:write-count (count (:writes analysis))}
              :memory {:initial initial-memory
                       :final final-memory
                       :writes write-data}
              :structure {:boundaries (:boundaries analysis)
                          :boundary-timings (:boundary-timings analysis)
                          :execution execution}
              :semantics {:status :unclassified}}}))
