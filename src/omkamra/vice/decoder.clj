(ns omkamra.vice.decoder
  "Tools for recording and decoding code executed by VICE's binary monitor."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [omkamra.vice.binary-monitor :as bm]
   [omkamra.vice.asm :as asm]
   [omkamra.vice.trace :as trace])
  (:import [java.io FileReader]
           [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

;; FIFO ingestion keeps trace records as compact vectors.  These ten values are
;; needed while decoding; maps are materialized only in final artifacts.
(def ^:private trace-pc 0)
(def ^:private trace-bytes 1)
(def ^:private trace-raster-line 2)
(def ^:private trace-cpu-cycle 3)
(def ^:private trace-a 4)
(def ^:private trace-x 5)
(def ^:private trace-y 6)
(def ^:private trace-flags 8)

;; [trace-event instruction-id instruction boundary? event-index]
(def ^:private occurrence-event 0)
(def ^:private occurrence-instruction-id 1)
(def ^:private occurrence-instruction 2)
(def ^:private occurrence-boundary? 3)
(def ^:private occurrence-event-index 4)

(defn- u8
  [x]
  (bit-and (int x) 0xff))

;; Pipeline enrichment -------------------------------------------------------
;;
;; The binary monitor does not stream memory-write events. CPU writes are
;; therefore derived from the pre-instruction registers, captured opcode bytes,
;; and a mutable copy of the initial memory snapshot. The raw monitor samples
;; and both memory snapshots remain in the result for replay and inspection.

(defn- memory-word
  [memory address]
  (bit-or (u8 (nth memory (bit-and address 0xffff)))
          (bit-shift-left (u8 (nth memory (bit-and (inc address) 0xffff))) 8)))

(defn- memory-byte
  [memory address]
  (u8 (nth memory (bit-and address 0xffff))))

(defn- carry-set?
  [event]
  (let [flags (nth event trace-flags)]
    (and flags (str/ends-with? flags "C"))))

(defn- effective-address
  "Resolve the effective address of a memory-addressed instruction from its
  pre-instruction register state and the memory image at that instant."
  [memory {:keys [mode bytes]} event]
  (let [operand (u8 (nth bytes 1 0))
        word (bit-or operand (bit-shift-left (u8 (nth bytes 2 0)) 8))
        x (or (nth event trace-x) 0)
        y (or (nth event trace-y) 0)
        zp-word (fn [address]
                  (bit-or (memory-byte memory address)
                          (bit-shift-left (memory-byte memory
                                                       (bit-and (inc address) 0xff))
                                          8)))]
    (case mode
      :zp operand
      :zpx (bit-and (+ operand x) 0xff)
      :zpy (bit-and (+ operand y) 0xff)
      :abs word
      :absx (bit-and (+ word x) 0xffff)
      :absy (bit-and (+ word y) 0xffff)
      :indx (zp-word (bit-and (+ operand x) 0xff))
      :indy (bit-and (+ (zp-word operand) y) 0xffff)
      nil)))

(def ^:private direct-store-mnemonics
  #{"STA" "STX" "STY" "SAX" "AHX" "SHX" "SHY" "TAS"})

(def ^:private read-modify-write-mnemonics
  #{"ASL" "LSR" "ROL" "ROR" "INC" "DEC"
    "SLO" "RLA" "SRE" "RRA" "DCP" "ISC"})

(defn- store-value
  [mnemonic old-value address event]
  (let [a (or (nth event trace-a) 0)
        x (or (nth event trace-x) 0)
        y (or (nth event trace-y) 0)
        carry (if (carry-set? event) 1 0)
        high-byte-plus-one (bit-and (inc (bit-shift-right address 8)) 0xff)]
    (u8
     (case mnemonic
       "STA" a
       "STX" x
       "STY" y
       "SAX" (bit-and a x)
       "AHX" (bit-and a x high-byte-plus-one)
       "SHX" (bit-and x high-byte-plus-one)
       "SHY" (bit-and y high-byte-plus-one)
       "TAS" (bit-and a x high-byte-plus-one)
       "ASL" (bit-shift-left old-value 1)
       "LSR" (bit-shift-right old-value 1)
       "ROL" (bit-or (bit-shift-left old-value 1) carry)
       "ROR" (bit-or (bit-shift-right old-value 1)
                     (bit-shift-left carry 7))
       "INC" (inc old-value)
       "DEC" (dec old-value)
       "SLO" (bit-shift-left old-value 1)
       "RLA" (bit-or (bit-shift-left old-value 1) carry)
       "SRE" (bit-shift-right old-value 1)
       "RRA" (bit-or (bit-shift-right old-value 1)
                     (bit-shift-left carry 7))
       "DCP" (dec old-value)
       "ISC" (inc old-value)))))

(defn- inferred-write
  [memory decoded entry]
  (let [{:keys [mnemonic]} decoded]
    (when (or (contains? direct-store-mnemonics mnemonic)
              (contains? read-modify-write-mnemonics mnemonic))
      (when-let [address (effective-address memory decoded entry)]
        (let [old-value (memory-byte memory address)]
          {:address address
           :value (store-value mnemonic old-value address entry)
           :old-value old-value
           :kind (if (contains? direct-store-mnemonics mnemonic)
                   :store
                   :read-modify-write)
           ;; This flag explicitly distinguishes calculated CPU writes from
           ;; direct remote-monitor memory events (which VICE does not expose
           ;; over the binary protocol).
           :inferred? true})))))

(defn- elapsed-cycles
  [event next-event cycles-per-line raster-lines]
  (when next-event
    (let [line-delta (- (nth next-event trace-raster-line)
                        (nth event trace-raster-line))
          line-delta (if (neg? line-delta) (+ line-delta raster-lines) line-delta)
          elapsed (+ (* line-delta cycles-per-line)
                     (- (nth next-event trace-cpu-cycle)
                        (nth event trace-cpu-cycle)))]
      ;; A sample at exactly the same timing state is the next frame boundary.
      (if (pos? elapsed) elapsed (* cycles-per-line raster-lines)))))

(defn- write-timing
  [event duration-cycles cycles-per-line]
  ;; A 6510 store commits on its final bus cycle.  The sampled timing is the
  ;; beginning of the instruction, so retain both timings in the event.
  (let [offset (max 0 (dec (or duration-cycles 1)))
        instruction-raster-line (nth event trace-raster-line)
        instruction-cpu-cycle (nth event trace-cpu-cycle)
        total (+ instruction-cpu-cycle offset)]
    {:instruction-raster-line instruction-raster-line
     :instruction-cpu-cycle instruction-cpu-cycle
     :raster-line (+ instruction-raster-line (quot total cycles-per-line))
     :cpu-cycle (mod total cycles-per-line)
     :write-cycle-offset offset}))

(def ^:private compact-write-keys
  [:event-index :pc :address :value :old-value
   :raster-line :cpu-cycle
   :instruction-raster-line :instruction-cpu-cycle
   :write-cycle-offset :mnemonic-id :kind-id])

(defn- compact-write-data
  [writes]
  (let [mnemonics (->> writes (map :mnemonic) distinct vec)
        kinds (->> writes (map :kind) distinct vec)
        mnemonic-ids (zipmap mnemonics (range))
        kind-ids (zipmap kinds (range))]
    {:format :omkamra.vice/write-records-v1
     :keys compact-write-keys
     :mnemonics mnemonics
     :kinds kinds
     :writes (mapv (fn [write]
                     [(:event-index write)
                      (:pc write)
                      (:address write)
                      (:value write)
                      (:old-value write)
                      (:raster-line write)
                      (:cpu-cycle write)
                      (:instruction-raster-line write)
                      (:instruction-cpu-cycle write)
                      (:write-cycle-offset write)
                      (get mnemonic-ids (:mnemonic write))
                      (get kind-ids (:kind write))])
                   writes)}))

(defn write-records
  "Return a fresh lazy sequence of expanded maps from compact write records.

  Unlike `expand-writes`, this does not retain a map for every write. Call it
  again for each independent pass over the compact column data."
  [{:keys [mnemonics kinds writes]}]
  (map (fn [[event-index pc address value old-value raster-line cpu-cycle
             instruction-raster-line instruction-cpu-cycle write-cycle-offset
             mnemonic-id kind-id]]
         {:event-index event-index
          :pc pc
          :address address
          :value value
          :old-value old-value
          :raster-line raster-line
          :cpu-cycle cpu-cycle
          :instruction-raster-line instruction-raster-line
          :instruction-cpu-cycle instruction-cpu-cycle
          :write-cycle-offset write-cycle-offset
          :mnemonic (nth mnemonics mnemonic-id)
          :kind (nth kinds kind-id)})
       writes))

(defn expand-writes
  "Eagerly expand compact artifact write records into analysis maps.

  Prefer `write-records` for streaming analysis. This helper remains available
  for callers that explicitly need a retained vector."
  [compact-writes]
  (mapv identity (write-records compact-writes)))

(def ^:private vic-register-addresses
  (conj (set (range 0xd000 0xd02f)) 0xdd00))

(def ^:private layout-register-addresses
  #{0xd011 0xd018 0xdd00})

(defn- vic-register-name
  [address]
  (keyword (format "%04x" address)))

(defn- vic-state
  [memory]
  (into (sorted-map)
        (map (fn [address] [address (memory-byte memory address)]))
        vic-register-addresses))

(defn- vic-configuration
  [state]
  (let [d011 (get state 0xd011 0)
        d016 (get state 0xd016 0)
        d018 (get state 0xd018 0)
        dd00 (get state 0xdd00 0)
        bank-base (* 0x4000 (- 3 (bit-and dd00 0x03)))
        bitmap? (pos? (bit-and d011 0x20))]
    {:d011 d011
     :d016 d016
     :d018 d018
     :dd00 dd00
     :bank-base bank-base
     :screen-base (+ bank-base (* 0x0400 (bit-shift-right d018 4)))
     :charset-base (+ bank-base (* 0x0800 (bit-shift-right (bit-and d018 0x0e) 1)))
     :bitmap? bitmap?
     :bitmap-base (+ bank-base (if (pos? (bit-and d018 0x08)) 0x2000 0))
     :x-scroll (bit-and d016 0x07)
     :y-scroll (bit-and d011 0x07)
     :display-enabled? (pos? (bit-and d011 0x10))
     :multicolor? (pos? (bit-and d016 0x10))}))

(defn- derive-vic
  [initial-memory writes]
  (let [initial-state (vic-state initial-memory)
        {:keys [state events configurations]}
        (reduce
         (fn [{:keys [state events configurations] :as result} write]
           (if (contains? vic-register-addresses (:address write))
             (let [state (assoc state (:address write) (:value write))
                   event (assoc write :register (vic-register-name (:address write)))]
               (assoc result
                      :state state
                      :events (conj events event)
                      :configurations
                      (if (contains? layout-register-addresses (:address write))
                        (conj configurations
                              (merge (select-keys write [:event-index :pc
                                                         :raster-line :cpu-cycle])
                                     (vic-configuration state)))
                        configurations)))
             result))
         {:state initial-state
          :events []
          :configurations [(assoc (vic-configuration initial-state)
                                  :event-index -1)]}
         writes)]
    {:initial-state initial-state
     :final-state state
     :writes events
     :configurations configurations}))

(defn- sprite-pointer-events
  [initial-memory writes]
  (let [initial-state (vic-state initial-memory)]
    (:events
     (reduce
      (fn [{:keys [state events] :as result} write]
        (let [{:keys [screen-base bank-base] :as configuration}
              (vic-configuration state)
              address (:address write)
              offset (- address screen-base)
              event (when (<= 0x3f8 offset 0x3ff)
                      (assoc write
                             :slot (- offset 0x3f8)
                             :pointer (:value write)
                             :sprite-address (+ bank-base (* 64 (:value write)))
                             :configuration configuration))
              state (if (contains? vic-register-addresses address)
                      (assoc state address (:value write))
                      state)]
          (assoc result :state state :events (cond-> events event (conj event)))))
      {:state initial-state :events []}
      writes))))

(defn- memory-after-writes
  [initial-memory writes event-index]
  (let [memory (byte-array (map unchecked-byte initial-memory))]
    (doseq [{:keys [address value]} (take-while #(<= (:event-index %) event-index)
                                                writes)]
      (aset-byte memory address (unchecked-byte value)))
    memory))

(defn- memory-range
  [memory start length]
  (mapv #(memory-byte memory (+ start %)) (range length)))

(defn- asset-sample
  [initial-memory writes-source configuration]
  (let [memory (memory-after-writes initial-memory
                                    (writes-source)
                                    (:event-index configuration))
        {:keys [bank-base screen-base charset-base bitmap? bitmap-base]} configuration
        pointers (memory-range memory (+ screen-base 0x3f8) 8)]
    (merge configuration
           {:screen {:address screen-base :data (memory-range memory screen-base 1024)}
            :charset {:address charset-base :data (memory-range memory charset-base 2048)}
            :color-ram {:address 0xd800 :data (memory-range memory 0xd800 1000)}
            :sprites (mapv (fn [slot pointer]
                             {:slot slot
                              :pointer pointer
                              :address (+ bank-base (* pointer 64))
                              :data (memory-range memory (+ bank-base (* pointer 64)) 64)})
                           (range 8) pointers)}
           (when bitmap?
             {:bitmap {:address bitmap-base :data (memory-range memory bitmap-base 8192)}}))))

(defn- asset-samples
  [initial-memory writes-source configurations]
  ;; Fine-scroll changes in D011/D016 do not alter any asset address.  Only
  ;; snapshot when the address layout changes, while retaining every VIC write
  ;; in :vic/:writes for raster-precise scroll/border analysis.
  (->> configurations
       (partition-by #(select-keys % [:bank-base :screen-base :charset-base
                                      :bitmap? :bitmap-base]))
       (map first)
       (mapv #(asset-sample initial-memory writes-source %))))

(def ^:private basic-block-terminators
  #{"BRK" "JMP" "JSR" "RTI" "RTS" "KIL"
    "BCC" "BCS" "BEQ" "BMI" "BNE" "BPL" "BVC" "BVS"})

(defn- basic-block-terminator?
  [instruction]
  (contains? basic-block-terminators (:mnemonic instruction)))

(defn- basic-block-builder-xf
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

(defn- deduplicate-basic-blocks-xf
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

(defn- split-block-at-control-flow-targets-xf
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

(defn- instruction-pc
  [instruction]
  (or (:pc instruction) (:address instruction)))

(defn- absolute-operand
  [instruction]
  (let [bytes (:bytes instruction)]
    (bit-or (u8 (nth bytes 1 0))
            (bit-shift-left (u8 (nth bytes 2 0)) 8))))

(defn- indirect-jump-target
  [memory instruction]
  (let [pointer (absolute-operand instruction)
        high-address (bit-or (bit-and pointer 0xff00)
                             (bit-and (inc pointer) 0xff))]
    (bit-or (memory-byte memory pointer)
            (bit-shift-left (memory-byte memory high-address) 8))))

(defn- relative-target
  [instruction]
  (let [pc (instruction-pc instruction)
        offset (u8 (nth (:bytes instruction) 1 0))]
    (bit-and (+ pc 2 (if (< offset 128) offset (- offset 256))) 0xffff)))

(defn- control-flow-successors
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
      #{(memory-word memory 0xfffe)}

      (contains? dynamic-transfer-mnemonics mnemonic)
      nil

      :else
      #{fall-through})))

(defn- control-flow-targets
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
      (if memory #{(memory-word memory 0xfffe)} #{})

      :else
      #{})))

(defn- unexpected-control-flow?
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

(defn- ram-code-pc?
  [pc {:keys [ram-start ram-end]
       :or {ram-start 0x0400
            ram-end 0xe000}}]
  (and (integer? pc)
       (<= ram-start pc)
       (< pc ram-end)))

;; Occurrence layout is declared with the trace layout above because the
;; basic-block transducers consume it before the stream ingester is defined.

(defn- stream-instruction-ids
  [events]
  (let [blocks (:blocks events)]
    (mapcat (fn [[block-id iterations]]
              (apply concat
                     (repeat iterations
                             (:instruction-ids (nth blocks block-id)))))
            (:block-runs events))))

(defn- empty-stream-state
  []
  {:status :starting
   :block-runs []
   :event-count 0
   :error nil})

(defn- intern-stream-instructions-xf
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

(defn- make-analysis-state
  [initial-memory retain-samples?]
  {:memory (byte-array (map unchecked-byte initial-memory))
   :writes (java.util.ArrayList.)
   :boundaries (java.util.ArrayList.)
   :boundary-timings (java.util.HashMap.)
   :pending nil
   :retain-samples? retain-samples?
   :samples (when retain-samples? (java.util.ArrayList.))})

(defn- append-analysis-write!
  [analysis pending next-event]
  (let [event (nth pending occurrence-event)
        instruction (nth pending occurrence-instruction)
        memory (:memory @analysis)
        duration-cycles (when (and next-event
                                   (nth event trace-raster-line)
                                   (nth event trace-cpu-cycle)
                                   (nth next-event trace-raster-line)
                                   (nth next-event trace-cpu-cycle))
                          (elapsed-cycles event next-event 63 312))
        write (inferred-write memory instruction event)]
    (when write
      (let [write-event (merge {:pc (nth event trace-pc)
                                :mnemonic (:mnemonic instruction)
                                :event-index (nth pending occurrence-event-index)}
                               (if (and (nth event trace-raster-line)
                                        (nth event trace-cpu-cycle))
                                 (write-timing event duration-cycles 63)
                                 {})
                               (dissoc write :inferred?))]
        (.add ^java.util.ArrayList (:writes @analysis) write-event)
        (aset-byte memory (:address write)
                   (unchecked-byte (:value write)))))))

(defn- analyze-stream-occurrence!
  [analysis occurrence]
  (locking analysis
    (let [state @analysis
          pending (:pending state)
          event (nth occurrence occurrence-event)]
      (when pending
        (append-analysis-write! analysis pending event))
      (when (nth occurrence occurrence-boundary?)
        (.add ^java.util.ArrayList (:boundaries state)
              (nth occurrence occurrence-event-index))
        (.put ^java.util.HashMap (:boundary-timings state)
              (nth occurrence occurrence-event-index)
              [(nth event trace-raster-line) (nth event trace-cpu-cycle)]))
      (when (:retain-samples? state)
        (.add ^java.util.ArrayList (:samples state)
              (subvec event trace-raster-line)))
      (swap! analysis assoc :pending occurrence))))

(defn- complete-analysis!
  [analysis]
  (locking analysis
    (when-let [pending (:pending @analysis)]
      (append-analysis-write! analysis pending nil)
      (swap! analysis assoc :pending nil))))

(defn- assign-event-index-xf
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

(defn- collect-stream-analysis-xf
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

(defn- mark-control-flow-boundaries-xf
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

(defn- append-block-run
  [runs {:keys [block-id iterations]}]
  (if (and (seq runs) (= block-id (first (peek runs))))
    (let [index (dec (count runs))
          [_ prior-iterations] (peek runs)]
      (assoc runs index [block-id (+ prior-iterations iterations)]))
    (conj runs [block-id iterations])))

(defn- make-stream-ingester
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

(defn- analysis-snapshot
  [analysis]
  (let [state @analysis]
    {:writes (vec (.toArray ^java.util.ArrayList (:writes state)))
     :boundaries (vec (.toArray ^java.util.ArrayList (:boundaries state)))
     :boundary-timings (into {} (.entrySet ^java.util.HashMap
                                 (:boundary-timings state)))}))

(defn- instruction-block-stream
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

(defn- create-fifo!
  [^String fifo-path]
  (.mkdirs (.getParentFile (io/file fifo-path)))
  (io/delete-file fifo-path true)
  (let [^java.util.List command ["mkfifo" fifo-path]
        process (.start (ProcessBuilder. command))]
    (when-not (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Timed out creating monitor trace FIFO"
                      {:fifo-path fifo-path})))
    (when-not (zero? (.exitValue process))
      (throw (ex-info "Could not create monitor trace FIFO"
                      {:fifo-path fifo-path
                       :exit-code (.exitValue process)}))))
  fifo-path)

(def ^:private fifo-state-batch-size 512)

(defn- update-stream-state!
  "Apply a stream update while excluding concurrent lifecycle updates.

  The ingestion reducer owns static interning state, so retrying it through an
  atom CAS race could duplicate its side effects. All capture lifecycle writes
  therefore share this lock with the FIFO reader."
  [stream-state f & args]
  (locking stream-state
    (apply swap! stream-state f args)))

(defn- close-fifo-reader!
  [reader-ref reader-thread timeout-ms]
  (.join ^Thread reader-thread (long timeout-ms))
  (when (.isAlive ^Thread reader-thread)
    (when-let [reader @reader-ref]
      (try (.close ^java.io.Closeable reader)
           (catch Throwable _ nil)))
    (.interrupt ^Thread reader-thread)
    (.join ^Thread reader-thread 1000))
  (not (.isAlive ^Thread reader-thread)))

(defn- decoded-stream-instructions
  [events]
  (mapv (fn [{:keys [id pc bytes]}]
          (let [decoded (asm/disassemble-bytes pc bytes)]
            (assoc decoded :id id :address pc)))
        (:instructions events)))

(defn- stream-instruction-id-array
  [events]
  (let [ids (int-array (:event-count events))]
    (loop [event-index 0
           instruction-ids (seq (stream-instruction-ids events))]
      (if-let [instruction-id (first instruction-ids)]
        (do (aset-int ids event-index (int instruction-id))
            (recur (inc event-index) (next instruction-ids)))
        ids))))

(defn- sample-map
  [events sample]
  (zipmap (:sample-keys events) sample))

(defn- stream-event-at
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

(defn- stream-node-versions
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

(defn- stream-irq-data
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

(defn- first-stream-code-entry
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

(defn- minimal-pipeline-artifact
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
        write-data (compact-write-data (:writes analysis))]
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

(def ^:private chunk-format :omkamra.vice/chunk-v1)
(def ^:private capture-format :omkamra.vice/capture-v1)
(def ^:private default-chunk-max-events 50000)
(def ^:private default-chunk-max-bytes 128000000)
(def ^:private default-chunk-queue-capacity 4)
(def ^:private default-writer-backpressure-ms 5000)
(def ^:private estimated-bytes-per-event 128)
(def ^:private writer-stop ::writer-stop)

(defn- file-path
  [directory & parts]
  (.getPath (apply io/file directory parts)))

(defn- write-edn-file!
  [file value]
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  file)

(defn- atomic-write-edn!
  [file value]
  (let [target (.toPath (io/file file))
        partial (.toPath (io/file (str file ".partial")))]
    (write-edn-file! (.toFile partial) value)
    (try
      (Files/move partial target
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (catch AtomicMoveNotSupportedException _
        (Files/move partial target
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING]))))
    file))

(defn read-capture-manifest
  "Read the durable manifest in a chunked capture directory."
  [capture-directory]
  (edn/read-string (slurp (file-path capture-directory "manifest.edn"))))

(defn read-chunk
  "Read one immutable chunk by its number from a capture directory."
  [capture-directory chunk-number]
  (edn/read-string
   (slurp (file-path capture-directory "chunks"
                     (format "chunk-%06d.edn" chunk-number)))))

(defn- validate-chunk-options!
  [{:keys [chunk-max-events chunk-max-bytes chunk-queue-capacity
           writer-backpressure-ms]}]
  (doseq [[key value] [[:chunk-max-events chunk-max-events]
                       [:chunk-max-bytes chunk-max-bytes]
                       [:chunk-queue-capacity chunk-queue-capacity]
                       [:writer-backpressure-ms writer-backpressure-ms]]
          :when (some? value)]
    (when-not (pos-int? value)
      (throw (ex-info (str (name key) " must be a positive integer")
                      {key value})))))

(defn- normalized-chunk-options
  [options]
  (validate-chunk-options! options)
  {:chunk-max-events (or (:chunk-max-events options)
                         default-chunk-max-events)
   :chunk-max-bytes (or (:chunk-max-bytes options)
                        default-chunk-max-bytes)
   :chunk-queue-capacity (or (:chunk-queue-capacity options)
                             default-chunk-queue-capacity)
   :writer-backpressure-ms (or (:writer-backpressure-ms options)
                               default-writer-backpressure-ms)})

(defn- chunk-file-name
  [number]
  (format "chunk-%06d.edn" number))

(defn- manifest-chunk-entry
  [{:keys [chunk-number event-range boundary summary]}]
  {:number chunk-number
   :file (str "chunks/" (chunk-file-name chunk-number))
   :event-range event-range
   :boundary boundary
   :summary summary})

(defn- write-manifest!
  [capture-directory manifest-state]
  (atomic-write-edn! (file-path capture-directory "manifest.edn")
                     @manifest-state))

(defn- make-manifest
  [capture-directory metadata chunk-options]
  {:format capture-format
   :capture-id (:capture-id metadata)
   :capture-directory capture-directory
   :input (:input metadata)
   :status :running
   :capture-mode :continuous
   :trace-transport :fifo
   :options (select-keys chunk-options [:chunk-max-events :chunk-max-bytes
                                        :chunk-queue-capacity])
   :chunks []
   :current-chunk 1
   :event-count 0
   :analysis {:status :not-run
              :requested-stage nil
              :completed-stages []
              :stages {}
              :manifest-file "analysis/manifest.edn"}
   :finalized? false})

(defn- writer-failure
  [writer-state]
  (:error @writer-state))

(defn- update-writer-queue-metrics!
  [writer-state ^ArrayBlockingQueue queue]
  (let [depth (.size queue)]
    (swap! writer-state
           (fn [state]
             (-> state
                 (assoc :queue-depth depth)
                 (update :high-water-mark max depth))))))

(defn- writer-timing!
  [writer-state finalize-ms write-ms byte-count]
  (swap! writer-state
         (fn [state]
           (let [write-ms-total (+ (long (or (:write-ms-total state) 0))
                                   write-ms)
                 bytes-written (+ (long (or (:bytes-written state) 0))
                                  byte-count)]
             (assoc state
                    :last-finalize-ms finalize-ms
                    :last-write-ms write-ms
                    :last-chunk-bytes byte-count
                    :max-finalize-ms (max (long (or (:max-finalize-ms state) 0))
                                          finalize-ms)
                    :max-write-ms (max (long (or (:max-write-ms state) 0))
                                       write-ms)
                    :finalize-ms-total (+ (long (or (:finalize-ms-total state) 0))
                                          finalize-ms)
                    :write-ms-total write-ms-total
                    :bytes-written bytes-written
                    :writer-throughput-bps (if (pos? write-ms-total)
                                             (long (/ (* bytes-written 1000)
                                                      write-ms-total))
                                             0))))))

(declare finalize-chunk)

(defn- start-chunk-writer!
  [capture-directory manifest-state queue writer-state]
  (let [thread
        (Thread.
         (fn []
           (try
             (loop []
               (let [job (.take ^ArrayBlockingQueue queue)]
                 (update-writer-queue-metrics! writer-state queue)
                 (if (= writer-stop job)
                   (swap! writer-state assoc :status :stopped :queue-depth 0)
                   (let [{:keys [chunk metadata boundary]} job
                         finalize-start (System/nanoTime)
                         {:keys [chunk]} (finalize-chunk chunk metadata boundary)
                         finalize-ms (quot (- (System/nanoTime) finalize-start)
                                           1000000)
                         number (:chunk-number chunk)
                         filename (chunk-file-name number)
                         output-file (file-path capture-directory "chunks" filename)
                         write-start (System/nanoTime)]
                     ;; A chunk becomes visible in the manifest only after the
                     ;; final file has been atomically installed.
                     (atomic-write-edn! output-file chunk)
                     (swap! manifest-state
                            (fn [manifest]
                              (-> manifest
                                  (update :chunks conj (manifest-chunk-entry chunk))
                                  (assoc :current-chunk (inc number)
                                         :event-count (second (:event-range chunk))))))
                     (write-manifest! capture-directory manifest-state)
                     (let [write-ms (quot (- (System/nanoTime) write-start)
                                          1000000)
                           byte-count (.length (io/file output-file))]
                       (writer-timing! writer-state finalize-ms write-ms byte-count))
                     (swap! writer-state update :chunks-written (fnil inc 0))
                     (recur)))))
             (catch Throwable error
               (swap! writer-state assoc :status :failed :error error))))
         (str "vice-chunk-writer-" (System/nanoTime)))]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- enqueue-chunk!
  [^ArrayBlockingQueue queue writer-state job writer-backpressure-ms]
  (let [deadline (+ (System/nanoTime) (* 1000000 writer-backpressure-ms))]
    (loop [pressured? false]
      (when-let [error (writer-failure writer-state)]
        (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                        error)))
      (if (.offer queue job 100 TimeUnit/MILLISECONDS)
        (do
          (update-writer-queue-metrics! writer-state queue)
          (when pressured?
            (swap! writer-state update :backpressure-count (fnil inc 0)))
          nil)
        (if (< (System/nanoTime) deadline)
          (recur true)
          (throw (ex-info "Chunk writer queue remained full"
                          {:reason :chunk-writer-backpressure
                           :queue-capacity (.remainingCapacity queue)
                           :timeout-ms writer-backpressure-ms})))))))

(defn- copy-memory
  [memory]
  (mapv u8 memory))

(defn- open-chunk
  [number global-start initial-memory retain-samples?]
  {:number number
   :global-start global-start
   :initial-memory (copy-memory initial-memory)
   :ingester (make-stream-ingester {:initial-memory initial-memory
                                    :retain-samples? retain-samples?})
   :stream-state (atom (empty-stream-state))})

(defn- chunk-event-count
  [chunk]
  @(:event-count (:ingester chunk)))

(defn- chunk-estimated-bytes
  [chunk]
  (* estimated-bytes-per-event (chunk-event-count chunk)))

(defn- chunk-limit-reason
  [chunk {:keys [chunk-max-events chunk-max-bytes]}]
  (cond
    (>= (chunk-event-count chunk) chunk-max-events) :max-events
    (>= (chunk-estimated-bytes chunk) chunk-max-bytes) :max-bytes
    :else nil))

(defn- ingest-chunk-event!
  [chunk event]
  (let [stream-state (:stream-state chunk)
        ingester (:ingester chunk)]
    (locking stream-state
      (let [state ((:step ingester) @stream-state event)]
        (reset! stream-state (assoc state :event-count @(:event-count ingester)))))))

(defn- finalize-chunk
  [chunk metadata boundary]
  (let [{:keys [number global-start initial-memory ingester stream-state]} chunk
        _ (when-not (:sealed? chunk)
            (update-stream-state! stream-state (:complete ingester)))
        events (instruction-block-stream stream-state ingester)
        analysis (analysis-snapshot (:analysis-state ingester))
        final-memory (copy-memory (:memory @(:analysis-state ingester)))
        local-count (:event-count events)
        event-range [global-start (+ global-start local-count)]
        artifact (minimal-pipeline-artifact
                  events initial-memory final-memory
                  (merge metadata {:event-range event-range
                                   :chunk-number number})
                  analysis)
        summary {:event-count local-count
                 :instruction-count (count (:instructions events))
                 :block-count (count (:blocks events))
                 :write-count (count (:writes analysis))}]
    {:chunk {:format chunk-format
             :capture-id (:capture-id metadata)
             :chunk-number number
             :event-range event-range
             :event-index-scope :chunk-local
             :previous-chunk (when (> number 1) (dec number))
             :next-chunk (when-not (= :final (:kind boundary)) (inc number))
             :boundary boundary
             :local-event-count local-count
             :events events
             :stages (:stages artifact)
             :summary summary}
     :final-memory final-memory
     :summary summary}))

(defn- publish-open-chunk!
  [coordinator chunk]
  (swap! coordinator assoc
         :chunk-number (:number chunk)
         :chunk-event-count (chunk-event-count chunk)
         :global-event-count (+ (:global-start chunk) (chunk-event-count chunk))
         :estimated-open-chunk-bytes (chunk-estimated-bytes chunk)))

(defn- seal-chunk!
  "Finish only the mutable transducer work needed before ownership transfer.

  The expensive artifact projection remains on the writer thread. Completing
  here is necessary because the final inferred write changes the memory image
  from which the next chunk starts; it is bounded to one pending event and the
  final basic-block tail."
  [chunk]
  (let [{:keys [ingester stream-state]} chunk]
    (update-stream-state! stream-state (:complete ingester))
    (assoc chunk :sealed? true)))

(defn- close-open-chunk!
  [coordinator metadata chunk-options boundary]
  (let [chunk (:open-chunk @coordinator)]
    (when (pos? (chunk-event-count chunk))
      (let [chunk (seal-chunk! chunk)
            final-memory (copy-memory
                          (:memory @(:analysis-state (:ingester chunk))))
            next-chunk (open-chunk (inc (:number chunk))
                                   (+ (:global-start chunk)
                                      (chunk-event-count chunk))
                                   final-memory
                                   (:retain-samples? @coordinator))]
        ;; Transfer the sealed mutable chunk to the bounded queue. The reader
        ;; never performs artifact projection or EDN serialization.
        (enqueue-chunk! (:writer-queue @coordinator)
                        (:writer-state @coordinator)
                        {:chunk chunk
                         :metadata metadata
                         :boundary boundary}
                        (:writer-backpressure-ms chunk-options))
        (swap! coordinator assoc :open-chunk next-chunk)
        (publish-open-chunk! coordinator next-chunk)
        chunk))))

(defn- ingest-chunked-batch!
  [coordinator metadata chunk-options records]
  (doseq [event records]
    (let [chunk (:open-chunk @coordinator)]
      (ingest-chunk-event! chunk event)
      (publish-open-chunk! coordinator chunk)
      (when-let [reason (chunk-limit-reason chunk chunk-options)]
        (close-open-chunk! coordinator metadata chunk-options
                           {:kind :forced-size :reason reason})))))

(defn- start-chunked-fifo-reader!
  [fifo-path coordinator metadata chunk-options reader-ref on-error!]
  (let [thread
        (Thread.
         (fn []
           (try
             (with-open [reader (FileReader. fifo-path)]
               (reset! reader-ref reader)
               (swap! coordinator assoc :reader-status :streaming)
               (let [batch (volatile! (transient []))
                     flush! (fn []
                              (let [records (persistent! @batch)]
                                (vreset! batch (transient []))
                                (when (seq records)
                                  (ingest-chunked-batch! coordinator metadata
                                                         chunk-options records))))
                     consume! (fn [record]
                                (let [next-batch (conj! @batch record)]
                                  (if (= fifo-state-batch-size (count next-batch))
                                    (do
                                      (vreset! batch (transient []))
                                      (ingest-chunked-batch!
                                       coordinator metadata chunk-options
                                       (persistent! next-batch)))
                                    (vreset! batch next-batch))))]
                 (try
                   (trace/reduce-records! reader consume!
                                          (:retain-samples? @coordinator))
                   (finally (flush!))))
               (swap! coordinator assoc :reader-status :eof))
             (catch java.io.IOException error
               (when-not (#{:stopping :stopped} (:reader-status @coordinator))
                 (swap! coordinator assoc :reader-status :error :reader-error error)
                 (on-error! error)))
             (catch Throwable error
               (swap! coordinator assoc :reader-status :error :reader-error error)
               (on-error! error))))
         (str "vice-chunk-fifo-" (System/nanoTime)))]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- shutdown-chunk-writer!
  [coordinator timeout-ms]
  (let [{:keys [writer-queue writer-thread writer-state]} @coordinator
        deadline (+ (System/nanoTime) (* 1000000 timeout-ms))]
    ;; Preserve FIFO order: the sentinel is accepted only after every chunk
    ;; job. Unlike an unconditional put, this also notices a dead writer.
    (loop []
      (when-let [error (writer-failure writer-state)]
        (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                        error)))
      (cond
        (.offer ^ArrayBlockingQueue writer-queue writer-stop 100 TimeUnit/MILLISECONDS) nil
        (< (System/nanoTime) deadline) (recur)
        :else (throw (ex-info "Chunk writer queue did not drain"
                              {:reason :chunk-writer-timeout
                               :timeout-ms timeout-ms}))))
    (.join ^Thread writer-thread (long timeout-ms))
    (when (.isAlive ^Thread writer-thread)
      (throw (ex-info "Chunk writer did not stop"
                      {:reason :chunk-writer-timeout :timeout-ms timeout-ms})))
    (when-let [error (writer-failure writer-state)]
      (throw (ex-info "Chunk writer failed" {:reason :chunk-writer-error}
                      error)))))

(defn start-capture
  "Start a bounded, FIFO-backed chunked capture while VICE is paused.

  Each closed chunk is persisted by a bounded background writer. Options are
  `:capture-directory`, `:chunk-max-events`, `:chunk-max-bytes`,
  `:chunk-queue-capacity`, `:writer-backpressure-ms`, `:metadata`, and
  `:retain-samples?`. `:capture-directory` must already exist and is never
  replaced; chunks are written below its `chunks/` directory."
  ([conn] (start-capture conn {}))
  ([conn {:keys [fifo-path metadata checkpoint-op retain-samples? capture-directory]
          :or {checkpoint-op 4 retain-samples? false}
          :as options}]
   (when-not capture-directory
     (throw (ex-info "Chunked capture requires :capture-directory" {})))
   (let [chunk-options (normalized-chunk-options options)
         fifo-path (or fifo-path (str "/tmp/omkamra-vice/trace-" (System/nanoTime) ".fifo"))
         capture-directory (.getCanonicalPath (io/file capture-directory))
         chunks-directory (io/file capture-directory "chunks")
         _ (when-not (.isDirectory (io/file capture-directory))
             (throw (ex-info "Capture directory must exist" {:capture-directory capture-directory})))
         _ (when-not (.mkdirs chunks-directory)
             (when-not (.isDirectory chunks-directory)
               (throw (ex-info "Could not create chunks directory"
                               {:capture-directory capture-directory}))))
         manifest-state (atom (make-manifest capture-directory metadata chunk-options))
         queue (ArrayBlockingQueue. (int (:chunk-queue-capacity chunk-options)))
         writer-state (atom {:status :starting :queue-depth 0 :high-water-mark 0
                             :chunks-written 0 :backpressure-count 0
                             :last-finalize-ms 0 :last-write-ms 0
                             :last-chunk-bytes 0 :max-finalize-ms 0
                             :max-write-ms 0 :finalize-ms-total 0
                             :write-ms-total 0 :bytes-written 0
                             :writer-throughput-bps 0})
         writer-thread (start-chunk-writer! capture-directory manifest-state queue writer-state)
         state (atom {:status :starting :instruction-count 0})
         reader-ref (atom nil)
         reader-thread (atom nil)
         checkpoint-number (atom nil)
         prior-ignored-types (bm/ignored-unsolicited-types conn)]
     (try
       (write-manifest! capture-directory manifest-state)
       (swap! writer-state assoc :status :running)
       (bm/drain-events conn)
       (bm/ping conn)
       (bm/drain-events conn)
       (let [initial-memory (mapv u8 (:memory (bm/mem-get conn {:start 0 :end 65535})))
             coordinator (atom {:open-chunk (open-chunk 1 0 initial-memory retain-samples?)
                                :chunk-number 1
                                :chunk-event-count 0
                                :global-event-count 0
                                :estimated-open-chunk-bytes 0
                                :chunks-written 0
                                :reader-status :starting
                                :retain-samples? retain-samples?
                                :writer-queue queue
                                :writer-state writer-state
                                :writer-thread writer-thread})]
         (create-fifo! fifo-path)
         (reset! reader-thread
                 (start-chunked-fifo-reader!
                  fifo-path coordinator metadata chunk-options reader-ref
                  (fn [_]
                    ;; Stop VICE's FIFO writer before the reader closes. This
                    ;; prevents a reader-side failure from becoming SIGPIPE in
                    ;; VICE and preserves the original failure reason.
                    (try
                      (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
                      (catch Throwable _ nil)))))
         (bm/ignore-unsolicited-types! conn (conj prior-ignored-types
                                                  bm/MON_RESPONSE_CHECKPOINT_INFO))
         (let [checkpoint (bm/checkpoint-set conn {:start 0 :end 0xffff
                                                   :stop? false :enabled? true
                                                   :op checkpoint-op :temporary? false})]
           (reset! checkpoint-number (:number checkpoint))
           (bm/resource-set conn {:name "MonitorLogFileName" :value fifo-path})
           (bm/resource-set conn {:name "MonitorLogEnabled" :value 1})
           (reset! state {:status :running :instruction-count 0 :transport :fifo})
           {:kind :omkamra.vice/chunked-capture-v1
            :conn conn :fifo-path fifo-path :checkpoint-number checkpoint-number
            :metadata metadata :capture-directory capture-directory
            :manifest-state manifest-state :state state :coordinator coordinator
            ;; Capture orchestration uses this shared state to notice FIFO
            ;; reader failures and unexpected VICE exits while running.
            :stream-state coordinator
            :chunk-options chunk-options :reader-ref reader-ref
            :reader-thread @reader-thread :prior-ignored-types prior-ignored-types}))
       (catch Throwable error
         (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0}) (catch Throwable _ nil))
         (when-let [number @checkpoint-number]
           (try (bm/checkpoint-delete conn {:number number}) (catch Throwable _ nil)))
         (bm/ignore-unsolicited-types! conn prior-ignored-types)
         (when-let [thread @reader-thread] (close-fifo-reader! reader-ref thread 1000))
         (.offer queue writer-stop)
         (.join writer-thread 1000)
         (io/delete-file fifo-path true)
         (throw error))))))

(defn chunked-capture-status
  "Return bounded progress metadata without loading closed chunk artifacts."
  [capture]
  (let [{:keys [coordinator state]} capture
        {:keys [chunk-number chunk-event-count global-event-count reader-status reader-error
                writer-state]} @coordinator
        writer @writer-state]
    (merge (dissoc @state :artifact)
           {:event-count global-event-count
            :chunk-number chunk-number
            :chunk-event-count chunk-event-count
            :chunk-count (:chunks-written writer)
            :chunks-written (:chunks-written writer)
            :chunks-pending (:queue-depth writer)
            :writer-status (:status writer)
            :writer-queue-depth (:queue-depth writer)
            :writer-high-water-mark (:high-water-mark writer)
            :writer-backpressure-count (:backpressure-count writer)
            :last-finalize-ms (:last-finalize-ms writer)
            :last-write-ms (:last-write-ms writer)
            :last-chunk-bytes (:last-chunk-bytes writer)
            :max-finalize-ms (:max-finalize-ms writer)
            :max-write-ms (:max-write-ms writer)
            :finalize-ms-total (:finalize-ms-total writer)
            :write-ms-total (:write-ms-total writer)
            :bytes-written (:bytes-written writer)
            :writer-throughput-bps (:writer-throughput-bps writer)
            :reader-status reader-status
            :reader-alive? (.isAlive ^Thread (:reader-thread capture))
            :reader-error (some-> reader-error .getMessage)})))

(defn- derived-chunk-source
  [format chunk]
  {:format format
   :capture-id (:capture-id chunk)
   :chunk-number (:chunk-number chunk)
   :event-range (:event-range chunk)
   :source {:format (:format chunk)
            :file (chunk-file-name (:chunk-number chunk))}})

(defn derive-structure-chunk
  "Build the structural view for one immutable raw chunk.

  This stage contains execution-graph and IRQ/control-flow derivation only;
  video timelines and asset materialization are separate analysis stages."
  [chunk]
  (let [events (:events chunk)
        execution (get-in chunk [:stages :structure :execution])
        instructions (:instructions execution)
        instruction-ids (stream-instruction-id-array events)
        analysis {:boundaries (get-in chunk [:stages :structure :boundaries])
                  :boundary-timings
                  (get-in chunk [:stages :structure :boundary-timings])}
        irq-data (stream-irq-data events instructions instruction-ids analysis)
        spans (:spans irq-data)
        execution (assoc execution
                         :node-versions
                         (stream-node-versions instructions instruction-ids))
        frame-code {:first-ram-code
                    (first-stream-code-entry events instructions instruction-ids
                                             analysis spans :non-irq)
                    :first-ram-irq-code
                    (first-stream-code-entry events instructions instruction-ids
                                             analysis spans :irq)}]
    (assoc (derived-chunk-source :omkamra.vice/structure-chunk-v1 chunk)
           :stages {:structure {:spans spans
                                :execution execution
                                :frame-code frame-code}})))

(defn derive-writes-chunk
  "Build a compact reusable-write descriptor for one immutable raw chunk.

  The raw chunk remains the sole persisted owner of compact write records.
  This stage records a versioned reference rather than duplicating millions of
  expanded write maps in EDN. Analysis workers materialize the descriptor once
  per raw chunk only when a dependent stage needs the records."
  [chunk]
  (assoc (derived-chunk-source :omkamra.vice/writes-chunk-v2 chunk)
         :stages {:writes {:source {:stage :memory
                                    :key :writes
                                    :format (get-in chunk
                                                    [:stages :memory :writes
                                                     :format])}}}))

(defn writes-source
  "Return a fresh, non-retaining write-record source for one raw chunk.

  The returned function creates a lazy expanded sequence for each pass. This
  lets video and asset analysis stream the compact records without retaining a
  full expanded vector in the chunk worker heap."
  [raw-chunk writes-chunk]
  (or (::write-source writes-chunk)
      (let [{:keys [stage key]} (get-in writes-chunk [:stages :writes :source])]
        (when-not (= [:memory :writes] [stage key])
          (throw (ex-info "Unsupported writes-stage source"
                          {:source (get-in writes-chunk
                                           [:stages :writes :source])
                           :chunk-number (:chunk-number raw-chunk)})))
        (let [compact-writes (get-in raw-chunk [:stages :memory :writes])]
          (fn [] (write-records compact-writes))))))

(defn derive-video-chunk
  "Build the VIC timeline for one immutable raw chunk.

  The optional `writes` argument accepts already-expanded write records so a
  broadcast analysis pipeline can share them with the dependent asset stage.
  The result is intentionally free of extracted assets; those are produced by
  `derive-assets-chunk` after this stage has persisted its configurations."
  ([chunk]
   (derive-video-chunk chunk nil))
  ([chunk writes]
   (let [memory-stages (get-in chunk [:stages :memory])
         writes-source (if (fn? writes)
                         writes
                         (constantly (or writes
                                         (expand-writes (:writes memory-stages)))))
         vic (derive-vic (:initial memory-stages) (writes-source))]
     (assoc (derived-chunk-source :omkamra.vice/video-chunk-v1 chunk)
            :stages {:video {:vic (assoc (dissoc vic :writes)
                                         :sprite-pointer-writes
                                         (sprite-pointer-events
                                          (:initial memory-stages)
                                          (writes-source)))}}))))

(defn derive-assets-chunk
  "Build replayed display assets using a persisted video-stage chunk.

  The optional `writes` argument accepts the expanded writes retained by an
  in-flight video stage. When omitted, the persisted compact write records are
  expanded here for standalone asset-stage requests."
  ([chunk video-chunk]
   (derive-assets-chunk chunk video-chunk nil))
  ([chunk video-chunk writes]
   (let [memory-stages (get-in chunk [:stages :memory])
         writes-source (if (fn? writes)
                         writes
                         (constantly (or writes
                                         (expand-writes (:writes memory-stages)))))
         configurations (get-in video-chunk [:stages :video :vic :configurations])]
     (assoc (derived-chunk-source :omkamra.vice/assets-chunk-v1 chunk)
            :stages {:assets {:samples (asset-samples (:initial memory-stages)
                                                      writes-source
                                                      configurations)}}))))

(defn- stop-chunked-capture
  [capture]
  (let [{:keys [conn fifo-path checkpoint-number state coordinator metadata
                manifest-state capture-directory reader-ref reader-thread
                prior-ignored-types chunk-options]} capture]
    (if-let [result (:result @state)]
      result
      (try
        (swap! state assoc :status :stopping)
        (swap! coordinator assoc :reader-status :stopping)
        (try
          (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
          (catch Throwable _ nil))
        (when-not (close-fifo-reader! reader-ref reader-thread 5000)
          (throw (ex-info "FIFO trace reader did not stop" {:fifo-path fifo-path})))
        (when-let [error (:reader-error @coordinator)] (throw error))
        (close-open-chunk! coordinator metadata chunk-options {:kind :final :reason :stopped})
        (shutdown-chunk-writer! coordinator 30000)
        ;; Capture finalization owns only raw persistence. Derived output is a
        ;; separate, explicitly requested operation in omkamra.vice.analysis.
        (swap! manifest-state assoc :status :stopped :finalized? true)
        (write-manifest! capture-directory manifest-state)
        (let [result {:status :stopped
                      :capture-directory capture-directory
                      :manifest-path (file-path capture-directory "manifest.edn")
                      :event-count (:event-count @manifest-state)
                      :chunk-count (count (:chunks @manifest-state))}]
          (swap! state assoc :status :stopped :result result)
          result)
        (catch Throwable error
          (swap! state assoc :status :failed :error error)
          (swap! manifest-state assoc :status :failed :finalized? false
                 :failure {:reason (or (:reason (ex-data error)) :capture-error)
                           :message (.getMessage error)})
          (try (write-manifest! capture-directory manifest-state) (catch Throwable _ nil))
          (throw error))
        (finally
          (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0}) (catch Throwable _ nil))
          (when-let [number @checkpoint-number]
            (try (bm/checkpoint-delete conn {:number number}) (catch Throwable _ nil)))
          (bm/ignore-unsolicited-types! conn prior-ignored-types)
          (close-fifo-reader! reader-ref reader-thread 1000)
          (io/delete-file fifo-path true)
          (bm/drain-events conn)
          (reset! reader-ref nil)
          (reset! checkpoint-number nil))))))

(defn capture-status
  "Return lightweight chunked-recorder progress without loading closed chunks."
  [capture]
  (chunked-capture-status capture))

(defn stop-capture
  "Finalize a chunked capture, draining its writer before returning the manifest result."
  [capture]
  (stop-chunked-capture capture))
