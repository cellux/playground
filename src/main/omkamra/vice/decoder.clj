(ns omkamra.vice.decoder
  "Tools for recording and decoding code executed by VICE's binary monitor."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [omkamra.vice.binary-monitor :as bm]
   [omkamra.vice.asm :as asm]))

(defn- parse-monitor-header
  [line]
  (when-let [[_ operation pc raster-line cpu-cycle]
             (re-matches
              #"#\d+ \(Trace\s+(\w+)\s+([0-9A-Fa-f]+)\)\s+(\d+)/\$[0-9A-Fa-f]+,\s+(\d+)/\$[0-9A-Fa-f]+"
              (str/trim line))]
    {:operation (keyword (str/lower-case operation))
     :pc (Integer/parseInt pc 16)
     :raster-line (Integer/parseInt raster-line)
     :cpu-cycle (Integer/parseInt cpu-cycle)}))

(defn- parse-monitor-instruction
  [line header]
  (when-let [[_ pc body] (re-matches #"^\.C:([0-9A-Fa-f]+)\s+(.*)$" line)]
    (let [[instruction-state state] (str/split body #"\s+- A:" 2)
          tokens (str/split (str/trim instruction-state) #"\s+")
          [byte-tokens text-tokens] (split-with #(re-matches #"[0-9A-Fa-f]{2}" %) tokens)
          bytes (mapv #(Integer/parseInt % 16) byte-tokens)
          text (str/join " " text-tokens)
          [_ a x y sp flags global-cycle]
          (when state
            (re-matches
             #"([0-9A-Fa-f]{2}) X:([0-9A-Fa-f]{2}) Y:([0-9A-Fa-f]{2}) SP:([0-9A-Fa-f]{2})\s+(\S+)\s+(\d+)"
             state))]
      (merge header
             {:pc (Integer/parseInt pc 16)
              :bytes bytes
              :vice-text text}
             (when a
               {:a (Integer/parseInt a 16)
                :x (Integer/parseInt x 16)
                :y (Integer/parseInt y 16)
                :sp (Integer/parseInt sp 16)
                :flags flags
                :global-cycle (Long/parseLong global-cycle)})))))

(defn- u8
  [x]
  (bit-and (int x) 0xff))

;; Pipeline enrichment -------------------------------------------------------
;;
;; The binary monitor does not stream memory-write events. CPU writes are
;; therefore derived from the pre-instruction registers, captured opcode bytes,
;; and a mutable copy of the initial memory snapshot. The raw monitor samples
;; and both memory snapshots remain in the result for replay and inspection.

(def ^:private vic-register-addresses
  (conj (set (range 0xd000 0xd02f)) 0xdd00))

(def ^:private layout-register-addresses
  #{0xd011 0xd018 0xdd00})

(defn- memory-word
  [memory address]
  (bit-or (u8 (nth memory (bit-and address 0xffff)))
          (bit-shift-left (u8 (nth memory (bit-and (inc address) 0xffff))) 8)))

(defn- memory-byte
  [memory address]
  (u8 (nth memory (bit-and address 0xffff))))

(defn- carry-set?
  [{:keys [flags]}]
  (and flags (str/ends-with? flags "C")))

(defn- effective-address
  "Resolve the effective address of a memory-addressed instruction from its
  pre-instruction register state and the memory image at that instant."
  [memory {:keys [mode bytes]} {:keys [x y]}]
  (let [operand (u8 (nth bytes 1 0))
        word (bit-or operand (bit-shift-left (u8 (nth bytes 2 0)) 8))
        x (or x 0)
        y (or y 0)
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
  [mnemonic old-value address {:keys [a x y] :as entry}]
  (let [a (or a 0)
        x (or x 0)
        y (or y 0)
        carry (if (carry-set? entry) 1 0)
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
  (let [{:keys [mnemonic mode]} decoded]
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
  [entry next-entry cycles-per-line raster-lines]
  (when next-entry
    (let [line-delta (- (:raster-line next-entry) (:raster-line entry))
          line-delta (if (neg? line-delta) (+ line-delta raster-lines) line-delta)
          elapsed (+ (* line-delta cycles-per-line)
                     (- (:cpu-cycle next-entry) (:cpu-cycle entry)))]
      ;; A sample at exactly the same timing state is the next frame boundary.
      (if (pos? elapsed) elapsed (* cycles-per-line raster-lines)))))

(defn- write-timing
  [entry duration-cycles cycles-per-line]
  ;; A 6510 store commits on its final bus cycle.  The sampled timing is the
  ;; beginning of the instruction, so retain both timings in the event.
  (let [offset (max 0 (dec (or duration-cycles 1)))
        total (+ (:cpu-cycle entry) offset)]
    {:instruction-raster-line (:raster-line entry)
     :instruction-cpu-cycle (:cpu-cycle entry)
     :raster-line (+ (:raster-line entry) (quot total cycles-per-line))
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

(defn expand-writes
  "Expand compact artifact write records into analysis maps.

  Captures persist writes as positional records to avoid repeating map keys
  and mnemonic/kind values millions of times. Use this helper at an analysis
  boundary when map-shaped records are more convenient."
  [{:keys [mnemonics kinds writes]}]
  (mapv (fn [[event-index pc address value old-value raster-line cpu-cycle
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
  [initial-memory writes configuration]
  (let [memory (memory-after-writes initial-memory writes (:event-index configuration))
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
  [initial-memory writes configurations]
  ;; Fine-scroll changes in D011/D016 do not alter any asset address.  Only
  ;; snapshot when the address layout changes, while retaining every VIC write
  ;; in :vic/:writes for raster-precise scroll/border analysis.
  (->> configurations
       (partition-by #(select-keys % [:bank-base :screen-base :charset-base
                                      :bitmap? :bitmap-base]))
       (map first)
       (mapv #(asset-sample initial-memory writes %))))

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
          (let [{:keys [event-index instruction-id instruction]} occurrence
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
      #{fall-through (relative-target instruction)}

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

(defn- ram-code-pc?
  [pc {:keys [ram-start ram-end]
       :or {ram-start 0x0400
            ram-end 0xe000}}]
  (and (integer? pc)
       (<= ram-start pc)
       (< pc ram-end)))

(def ^:private stream-events-format
  :omkamra.vice/instruction-block-stream-v4)

(def ^:private stream-sample-keys
  [:raster-line :cpu-cycle :a :x :y :sp :flags :global-cycle])

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
       (let [pc (:pc event)
             bytes (vec (:bytes event))
             key [pc bytes]
             {:keys [instruction-ids instructions]} @instruction-state
             instruction-id (or (get instruction-ids key) (count instructions))
             instruction (or (nth instructions instruction-id nil)
                             (let [decoded (asm/disassemble-bytes pc bytes)]
                               {:id instruction-id
                                :operation (:operation event)
                                :pc pc
                                :bytes bytes
                                :vice-text (:vice-text event)
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
         (rf result {:event event
                     :instruction-id instruction-id
                     :instruction instruction}))))))

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
  (let [event (:event pending)
        instruction (:instruction pending)
        memory (:memory @analysis)
        duration-cycles (when (and next-event
                                    (:raster-line event)
                                    (:cpu-cycle event)
                                    (:raster-line next-event)
                                    (:cpu-cycle next-event))
                           (elapsed-cycles event next-event 63 312))
        write (inferred-write memory instruction event)]
    (when write
      (let [write-event (merge {:pc (:pc event)
                                :mnemonic (:mnemonic instruction)
                                :event-index (:event-index pending)}
                               (if (and (:raster-line event) (:cpu-cycle event))
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
          event (:event occurrence)]
      (when pending
        (append-analysis-write! analysis pending event))
      (when (:boundary? occurrence)
        (.add ^java.util.ArrayList (:boundaries state)
              (:event-index occurrence))
        (.put ^java.util.HashMap (:boundary-timings state)
              (:event-index occurrence)
              [(:raster-line event) (:cpu-cycle event)]))
      (when (:retain-samples? state)
        (.add ^java.util.ArrayList (:samples state)
              (mapv #(get event %) stream-sample-keys)))
      (swap! analysis assoc :pending occurrence))))

(defn- complete-analysis!
  [analysis]
  (locking analysis
    (when-let [pending (:pending @analysis)]
      (append-analysis-write! analysis pending nil)
      (swap! analysis assoc :pending nil))))

(defn- collect-stream-analysis-xf
  "Collect compact analysis state; full per-event samples are opt-in forensic data."
  [analysis]
  (fn [rf]
    (fn
      ([] (rf))
      ([result]
       (complete-analysis! analysis)
       (rf result))
      ([result occurrence]
       (let [event-index (:event-count result)
             occurrence (assoc occurrence :event-index event-index)]
         (analyze-stream-occurrence! analysis occurrence)
         (rf (-> result
                 (assoc :status :streaming)
                 (update :event-count inc))
             occurrence))))))

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
                               (:instruction @previous)
                               (:instruction occurrence)))]
           (vreset! previous occurrence)
           (rf result (assoc occurrence :boundary? boundary?))))))))

(defn- append-block-run
  [runs {:keys [block-id iterations]}]
  (if (and (seq runs) (= block-id (first (peek runs))))
    (let [index (dec (count runs))
          [_ prior-iterations] (peek runs)]
      (assoc runs index [block-id (+ prior-iterations iterations)]))
    (conj runs [block-id iterations])))

(defn- make-stream-ingester
  "Create one fused transducer pipeline for a FIFO capture.

  Parsed records flow through instruction interning, compact write/timing
  analysis, basic-block construction, and block interning. Full per-event
  register samples are retained only when `:retain-samples?` is true."
  ([]
   (make-stream-ingester {}))
  ([{:keys [initial-memory retain-samples?]
     :or {initial-memory (byte-array 65536)
          retain-samples? false}}]
   (let [instruction-state (atom {:instruction-ids {} :instructions []})
         block-state (atom {:block-ids {} :blocks []})
         analysis-state (atom (make-analysis-state initial-memory
                                                    retain-samples?))
         xf (comp (intern-stream-instructions-xf instruction-state)
                  (mark-control-flow-boundaries-xf initial-memory)
                  (collect-stream-analysis-xf analysis-state)
                  (basic-block-builder-xf :boundary?)
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
  (let [{:keys [block-runs event-count]} @stream-state
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
  [fifo-path]
  (.mkdirs (.getParentFile (io/file fifo-path)))
  (io/delete-file fifo-path true)
  (let [process (.start (ProcessBuilder. ^java.util.List ["mkfifo" fifo-path]))]
    (when-not (.waitFor process 5 java.util.concurrent.TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Timed out creating monitor trace FIFO"
                      {:fifo-path fifo-path})))
    (when-not (zero? (.exitValue process))
      (throw (ex-info "Could not create monitor trace FIFO"
                      {:fifo-path fifo-path
                       :exit-code (.exitValue process)}))))
  fifo-path)

(defn- monitor-trace-records-xf
  "Turn the FIFO's lazy line stream into parsed execution trace records."
  []
  (fn [rf]
    (let [header (volatile! nil)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result line]
         (if-let [next-header (parse-monitor-header line)]
           (do (vreset! header next-header) result)
           (if (and @header (str/starts-with? line ".C:"))
             (let [entry (parse-monitor-instruction line @header)]
               (vreset! header nil)
               (if (= :exec (:operation entry))
                 (rf result entry)
                 result))
             result)))))))

(defn- reduce-fifo-trace-records!
  "Reduce the FIFO-backed lazy line stream through the parser transducer.

  The reader remains owned by the calling `with-open`; reducing it eagerly on
  the reader thread avoids the leaked-resource and arbitrary-thread behaviour
  of exposing a lazy sequence to callers."
  [reader consume!]
  (transduce (monitor-trace-records-xf)
             (fn
               ([] nil)
               ([result] result)
               ([result record]
                (consume! record)
                result))
             nil
             (line-seq reader)))

(defn- update-stream-state!
  "Apply a stream update while excluding concurrent lifecycle updates.

  The ingestion reducer owns static interning state, so retrying it through an
  atom CAS race could duplicate its side effects. All capture lifecycle writes
  therefore share this lock with the FIFO reader."
  [stream-state f & args]
  (locking stream-state
    (apply swap! stream-state f args)))

(defn- start-fifo-reader!
  [fifo-path stream-state ingester reader-ref]
  (let [thread
        (Thread.
         (fn []
           (try
             (with-open [reader (io/reader fifo-path)]
               (reset! reader-ref reader)
               (update-stream-state! stream-state assoc :status :streaming)
               (reduce-fifo-trace-records!
                reader
                #(update-stream-state! stream-state (:step ingester) %))
               (update-stream-state! stream-state assoc :status :eof))
             (catch java.io.IOException error
               ;; Closing the reader is the emergency unblock path during
               ;; cleanup. It is not an error after logging has been disabled.
               (when-not (#{:stopping :stopped} (:status @stream-state))
                 (update-stream-state! stream-state assoc :status :error :error error)))
             (catch Throwable error
               (update-stream-state! stream-state assoc :status :error :error error))))
         (str "vice-trace-fifo-" (System/nanoTime)))]
    (.setDaemon thread true)
    (.start thread)
    thread))

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

(defn start-capture
  "Start a continuous, FIFO-backed execution capture while VICE is paused.

  VICE writes monitor trace text into a Unix FIFO. A dedicated reader reduces
  its lazy line stream through parser, instruction-interning, basic-block, and
  block-interning transducers. Only compact dynamic samples and interned static
  instructions/blocks are retained; no trace log is written to disk.

  The returned recorder is consumed by `capture-status` and `stop-capture`.
  Options are `:fifo-path`, `:metadata`, `:checkpoint-op` (default 4,
  execute), and `:retain-samples?` (default false). The latter preserves the
  full per-instruction register/timing sample stream for forensic analysis;
  normal captures retain only compact write and control-flow timing data."
  ([conn]
   (start-capture conn {}))
  ([conn {:keys [fifo-path metadata checkpoint-op retain-samples?]
          :or {checkpoint-op 4
               retain-samples? false}}]
   (let [fifo-path (or fifo-path
                       (str "/tmp/omkamra-vice/trace-"
                            (System/nanoTime) ".fifo"))
         state (atom {:status :starting :instruction-count 0})
         stream-state (atom (empty-stream-state))
         reader-ref (atom nil)
         reader-thread (atom nil)
         checkpoint-number (atom nil)
         prior-ignored-types (bm/ignored-unsolicited-types conn)]
     (try
       (bm/drain-events conn)
       (bm/ping conn)
       (bm/drain-events conn)
       (let [initial-memory (mapv u8
                                  (:memory (bm/mem-get conn {:start 0
                                                             :end 65535})))
             ingester (make-stream-ingester {:initial-memory initial-memory
                                              :retain-samples? retain-samples?})]
         (create-fifo! fifo-path)
         (reset! reader-thread
                 (start-fifo-reader! fifo-path stream-state ingester reader-ref))
         ;; A tracepoint also emits a binary CHECKPOINT_INFO event for every hit.
         ;; Suppress those unsolicited bodies before decoding/queueing; requested
         ;; checkpoint responses remain available.
         (bm/ignore-unsolicited-types!
          conn (conj prior-ignored-types bm/MON_RESPONSE_CHECKPOINT_INFO))
         (let [checkpoint (bm/checkpoint-set
                           conn {:start 0
                                 :end 0xffff
                                 :stop? false
                                 :enabled? true
                                 :op checkpoint-op
                                 :temporary? false})]
           (reset! checkpoint-number (:number checkpoint))
           (bm/resource-set conn {:name "MonitorLogFileName" :value fifo-path})
           ;; fopen(3) on the writer pairs with the reader thread's blocking open.
           (bm/resource-set conn {:name "MonitorLogEnabled" :value 1})
           (reset! state {:status :running
                          :instruction-count 0
                          :transport :fifo})
           {:kind :omkamra.vice/streaming-capture-v2
            :conn conn
            :fifo-path fifo-path
            :checkpoint-number checkpoint-number
            :initial-memory initial-memory
            :metadata metadata
            :state state
            :stream-state stream-state
            :ingester ingester
            :reader-ref reader-ref
            :reader-thread @reader-thread
            :prior-ignored-types prior-ignored-types}))
       (catch Throwable error
         (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
              (catch Throwable _ nil))
         (when-let [number @checkpoint-number]
           (try (bm/checkpoint-delete conn {:number number})
                (catch Throwable _ nil)))
         (bm/ignore-unsolicited-types! conn prior-ignored-types)
         (when-let [thread @reader-thread]
           (close-fifo-reader! reader-ref thread 1000))
         (io/delete-file fifo-path true)
         (throw error))))))

(defn- release-stream-state!
  "Drop the large mutable ingestion collections after capture finalization.

  The finalized artifact owns the canonical raw stream. The recorder's
  ingestion atom only needs lightweight counters/status afterwards, otherwise
  retaining a stopped recorder would keep a second copy of all samples alive."
  [stream-state ingester]
  (let [instruction-count (count (:instructions @(:instruction-state ingester)))
        block-count (count (:blocks @(:block-state ingester)))]
    (update-stream-state! stream-state
           (fn [stream]
             {:status (:status stream)
              :event-count (:event-count stream)
              :instruction-count (or (:instruction-count stream) instruction-count)
              :block-count (or (:block-count stream) block-count)
              :block-run-count (or (:block-run-count stream)
                                   (count (:block-runs stream)))
              :error (:error stream)}))
    (reset! (:instruction-state ingester) {:instruction-ids {} :instructions []})
    (reset! (:block-state ingester) {:block-ids {} :blocks []})
    (reset! (:analysis-state ingester)
            {:writes (java.util.ArrayList.)
             :boundaries (java.util.ArrayList.)
             :boundary-timings (java.util.HashMap.)
             :pending nil
             :retain-samples? false
             :samples nil
             :memory nil})))

(defn capture-status
  "Return lightweight progress for a streaming recorder without copying events."
  [capture]
  (let [stream @(:stream-state capture)
        ingester (:ingester capture)]
    ;; `:artifact` is retained in the decoder state solely so a repeated
    ;; stop-capture call can return it. Never expose it through a lightweight
    ;; status query (or copy it into higher-level session state).
    (merge (dissoc @(:state capture) :artifact)
           {:event-count (:event-count stream)
            :instruction-count (or (:instruction-count stream)
                                   (count (:instructions @(:instruction-state ingester))))
            :block-count (or (:block-count stream)
                             (count (:blocks @(:block-state ingester))))
            :block-run-count (or (:block-run-count stream)
                                 (count (:block-runs stream)))
            :reader-status (:status stream)
            :reader-alive? (.isAlive ^Thread (:reader-thread capture))
            :reader-error (some-> (:error stream) .getMessage)})))

(defn- stream-instruction-id-array
  [events]
  (let [ids (int-array (:event-count events))]
    (loop [event-index 0
           instruction-ids (seq (stream-instruction-ids events))]
      (if-let [instruction-id (first instruction-ids)]
        (do (aset-int ids event-index (int instruction-id))
            (recur (inc event-index) (next instruction-ids)))
        ids))))

(defn- decoded-stream-instructions
  [events]
  (mapv (fn [{:keys [id pc bytes]}]
          (let [decoded (asm/disassemble-bytes pc bytes)]
            (assoc decoded :id id :address pc)))
        (:instructions events)))

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
                       (map (fn [[[start end] [next-start _]]]
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

(defn- streaming-pipeline-artifact
  [events initial-memory final-memory metadata analysis]
  (let [instructions (decoded-stream-instructions events)
        instruction-ids (stream-instruction-id-array events)
        writes (:writes analysis)
        irq-data (stream-irq-data events instructions instruction-ids analysis)
        spans (:spans irq-data)
        execution {:format :omkamra.vice/versioned-execution-v4
                   :event-count (:event-count events)
                   :instructions instructions
                   :node-versions (stream-node-versions instructions instruction-ids)
                   :blocks (:blocks events)
                   :block-runs (:block-runs events)}
        frame-code {:first-ram-code
                    (first-stream-code-entry events instructions instruction-ids
                                             analysis spans :non-irq)
                    :first-ram-irq-code
                    (first-stream-code-entry events instructions instruction-ids
                                             analysis spans :irq)}
        vic (derive-vic initial-memory writes)
        write-data (compact-write-data writes)
        raw-events (cond-> (select-keys events [:format :event-count])
                     (:samples events)
                     (assoc :sample-keys (:sample-keys events)
                            :samples (:samples events)))]
    {:format :omkamra.vice/pipeline-v2
     :raw {:events raw-events
           :metadata metadata}
     :stages
     {:decoded {:write-count (count writes)}
      :memory {:initial initial-memory :final final-memory :writes write-data}
      :structure {:spans spans
                  :execution execution
                  :frame-code frame-code}
      :video {:vic (assoc (dissoc vic :writes)
                           :sprite-pointer-writes
                           (sprite-pointer-events initial-memory writes))
              :assets (asset-samples initial-memory writes
                                     (:configurations vic))}
      :semantics {:status :unclassified :spans []}}}))

(defn stop-capture
  "Stop a FIFO-backed capture and return a compact canonical artifact.

  Logging is disabled first, closing VICE's FIFO writer. The reader drains to
  EOF, then compact write and structural data are finalized. Full per-event
  register samples are discarded unless explicitly requested. Cleanup always
  removes the checkpoint, restores unsolicited-event handling, closes the
  reader, and deletes the FIFO. VICE remains paused."
  [capture]
  (let [{:keys [conn fifo-path checkpoint-number initial-memory metadata state
                stream-state ingester reader-ref reader-thread prior-ignored-types]}
        capture]
    (if-let [artifact (:artifact @state)]
      artifact
      (try
        (swap! state assoc :status :stopping)
        (update-stream-state! stream-state assoc :status :stopping)
        (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
        (when-not (close-fifo-reader! reader-ref reader-thread 5000)
          (throw (ex-info "FIFO trace reader did not stop"
                          {:fifo-path fifo-path})))
        (when-let [error (:error @stream-state)]
          (throw error))
        (let [final-memory (mapv u8
                                 (:memory (bm/mem-get conn {:start 0
                                                            :end 65535})))
              _ (update-stream-state! stream-state (:complete ingester))
              events (instruction-block-stream stream-state ingester)
              _ (swap! state assoc :status :finalizing
                       :instruction-count (:event-count events))
              artifact (streaming-pipeline-artifact
                        events initial-memory final-memory
                        (merge metadata
                               {:capture-mode :continuous
                                :trace-transport :fifo
                                :first-demo-part (:first-demo-part @state)
                                :cpu-range (:cpu-range @state)})
                        (analysis-snapshot (:analysis-state ingester)))]
          (swap! state assoc :status :stopped :artifact artifact)
          artifact)
        (finally
          (try (bm/resource-set conn {:name "MonitorLogEnabled" :value 0})
               (catch Throwable _ nil))
          (when-let [number @checkpoint-number]
            (try (bm/checkpoint-delete conn {:number number})
                 (catch Throwable _ nil)))
          (bm/ignore-unsolicited-types! conn prior-ignored-types)
          (close-fifo-reader! reader-ref reader-thread 1000)
          (io/delete-file fifo-path true)
          (bm/drain-events conn)
          ;; The artifact now owns the canonical stream. Do not leave the
          ;; ingestion state holding its duplicate samples or static tables.
          (release-stream-state! stream-state ingester)
          (reset! reader-ref nil)
          (reset! checkpoint-number nil))))))

(defn write-artifact!
  "Stream a canonical raw, pipeline, or session artifact as readable EDN."
  [output-file artifact]
  (with-open [writer (io/writer output-file)]
    (binding [*out* writer]
      (pr artifact)
      (newline)))
  output-file)
