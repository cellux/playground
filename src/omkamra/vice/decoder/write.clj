(ns omkamra.vice.decoder.write
  "CPU-write inference and compact write-record encoding."
  (:require [clojure.string :as str]))

;; Positions retained from the compact FIFO trace-record layout.
(def trace-raster-line 2)
(def trace-cpu-cycle 3)
(def trace-a 4)
(def trace-x 5)
(def trace-y 6)
(def trace-flags 8)

(defn u8
  [x]
  (bit-and (int x) 0xff))

;; Pipeline enrichment -------------------------------------------------------
;;
;; The binary monitor does not stream memory-write events. CPU writes are
;; therefore derived from the pre-instruction registers, captured opcode bytes,
;; and a mutable copy of the initial memory snapshot. The raw monitor samples
;; and both memory snapshots remain in the result for replay and inspection.

(defn memory-word
  [memory address]
  (bit-or (u8 (nth memory (bit-and address 0xffff)))
          (bit-shift-left (u8 (nth memory (bit-and (inc address) 0xffff))) 8)))

(defn memory-byte
  [memory address]
  (u8 (nth memory (bit-and address 0xffff))))

(defn carry-set?
  [event]
  (let [flags (nth event trace-flags)]
    (and flags (str/ends-with? flags "C"))))

(defn effective-address
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

(defn store-value
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

(defn inferred-write
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

(defn elapsed-cycles
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

(defn write-timing
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

(defn compact-write-data
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
