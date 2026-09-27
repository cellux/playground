(ns omkamra.vice.decoder
  "Tools for recording and decoding code executed by VICE's binary monitor."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [omkamra.vice.binary-monitor :as bm]))

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

(def ^:private mode-width
  {:imp 1 :acc 1 :imm 2 :zp 2 :zpx 2 :zpy 2 :rel 2
   :abs 3 :absx 3 :absy 3 :ind 3 :indx 2 :indy 2})

(def ^:private opcode-table
  (->> ["BRK/imp ORA/indx KIL/imp SLO/indx NOP/zp ORA/zp ASL/zp SLO/zp PHP/imp ORA/imm ASL/acc ANC/imm NOP/abs ORA/abs ASL/abs SLO/abs"
        "BPL/rel ORA/indy KIL/imp SLO/indy NOP/zpx ORA/zpx ASL/zpx SLO/zpx CLC/imp ORA/absy NOP/imp SLO/absy NOP/absx ORA/absx ASL/absx SLO/absx"
        "JSR/abs AND/indx KIL/imp RLA/indx BIT/zp AND/zp ROL/zp RLA/zp PLP/imp AND/imm ROL/acc ANC/imm BIT/abs AND/abs ROL/abs RLA/abs"
        "BMI/rel AND/indy KIL/imp RLA/indy NOP/zpx AND/zpx ROL/zpx RLA/zpx SEC/imp AND/absy NOP/imp RLA/absy NOP/absx AND/absx ROL/absx RLA/absx"
        "RTI/imp EOR/indx KIL/imp SRE/indx NOP/zp EOR/zp LSR/zp SRE/zp PHA/imp EOR/imm LSR/acc ALR/imm JMP/abs EOR/abs LSR/abs SRE/abs"
        "BVC/rel EOR/indy KIL/imp SRE/indy NOP/zpx EOR/zpx LSR/zpx SRE/zpx CLI/imp EOR/absy NOP/imp SRE/absy NOP/absx EOR/absx LSR/absx SRE/absx"
        "RTS/imp ADC/indx KIL/imp RRA/indx NOP/zp ADC/zp ROR/zp RRA/zp PLA/imp ADC/imm ROR/acc ARR/imm JMP/ind ADC/abs ROR/abs RRA/abs"
        "BVS/rel ADC/indy KIL/imp RRA/indy NOP/zpx ADC/zpx ROR/zpx RRA/zpx SEI/imp ADC/absy NOP/imp RRA/absy NOP/absx ADC/absx ROR/absx RRA/absx"
        "NOP/imm STA/indx NOP/imm SAX/indx STY/zp STA/zp STX/zp SAX/zp DEY/imp NOP/imm TXA/imp XAA/imm STY/abs STA/abs STX/abs SAX/abs"
        "BCC/rel STA/indy KIL/imp AHX/indy STY/zpx STA/zpx STX/zpy SAX/zpy TYA/imp STA/absy TXS/imp TAS/absy SHY/absx STA/absx SHX/absy AHX/absy"
        "LDY/imm LDA/indx LDX/imm LAX/indx LDY/zp LDA/zp LDX/zp LAX/zp TAY/imp LDA/imm TAX/imp LAX/imm LDY/abs LDA/abs LDX/abs LAX/abs"
        "BCS/rel LDA/indy KIL/imp LAX/indy LDY/zpx LDA/zpx LDX/zpy LAX/zpy CLV/imp LDA/absy TSX/imp LAS/absy LDY/absx LDA/absx LDX/absy LAX/absy"
        "CPY/imm CMP/indx NOP/imm DCP/indx CPY/zp CMP/zp DEC/zp DCP/zp INY/imp CMP/imm DEX/imp AXS/imm CPY/abs CMP/abs DEC/abs DCP/abs"
        "BNE/rel CMP/indy KIL/imp DCP/indy NOP/zpx CMP/zpx DEC/zpx DCP/zpx CLD/imp CMP/absy NOP/imp DCP/absy NOP/absx CMP/absx DEC/absx DCP/absx"
        "CPX/imm SBC/indx NOP/imm ISC/indx CPX/zp SBC/zp INC/zp ISC/zp INX/imp SBC/imm NOP/imp SBC/imm CPX/abs SBC/abs INC/abs ISC/abs"
        "BEQ/rel SBC/indy KIL/imp ISC/indy NOP/zpx SBC/zpx INC/zpx ISC/zpx SED/imp SBC/absy NOP/imp ISC/absy NOP/absx SBC/absx INC/absx ISC/absx"]
       (mapcat #(str/split % #" "))
       (mapv #(let [[mnemonic mode] (str/split % #"/")]
                {:mnemonic mnemonic :mode (keyword mode)}))))

(defn- u8
  [x]
  (bit-and (int x) 0xff))

(defn- operand-text
  [mode bytes pc]
  (let [byte (u8 (nth bytes 1 0))
        word (bit-or byte (bit-shift-left (u8 (nth bytes 2 0)) 8))]
    (case mode
      :imp ""
      :acc "A"
      :imm (format "#$%02X" byte)
      :zp (format "$%02X" byte)
      :zpx (format "$%02X,X" byte)
      :zpy (format "$%02X,Y" byte)
      :abs (format "$%04X" word)
      :absx (format "$%04X,X" word)
      :absy (format "$%04X,Y" word)
      :ind (format "($%04X)" word)
      :indx (format "($%02X,X)" byte)
      :indy (format "($%02X),Y" byte)
      :rel (format "$%04X" (bit-and (+ pc 2 (if (< byte 128) byte (- byte 256))) 0xffff)))))

(defn disassemble-bytes
  "Disassemble a MOS 6510 instruction from its captured bytes."
  [pc bytes]
  (let [opcode (u8 (first bytes))
        {:keys [mnemonic mode]} (nth opcode-table opcode)
        width (mode-width mode)
        bytes (vec (take width bytes))
        operand (operand-text mode bytes pc)]
    {:address pc
     :bytes bytes
     :mnemonic mnemonic
     :mode mode
     :operand operand
     :text (str mnemonic (when (seq operand) (str " " operand)))}))

(defn disassemble
  "Disassemble the MOS 6510 instruction at `pc` from a 64 KiB memory image."
  [memory pc]
  (let [bytes (mapv #(u8 (nth memory (bit-and (+ pc %) 0xffff))) (range 4))]
    (disassemble-bytes pc bytes)))

(defn- span-assembly-header
  [{:keys [kind start-index end-index trigger entry-pc return-pc]}]
  (str (format "; span %-7s events %d..%d"
               (name kind) start-index (dec end-index))
       (when trigger (str " trigger=" (pr-str trigger)))
       (when entry-pc (format " entry=$%04X" entry-pc))
       (when return-pc (format " return=$%04X" return-pc))))

(defn- require-execution
  [artifact]
  (or (get-in artifact [:stages :structure :execution])
      (throw (ex-info "Artifact has no structural execution stage"
                      {:format (:format artifact)}))))

(defn- write-static-instruction!
  [writer instruction]
  (let [decoded (if (:text instruction)
                  instruction
                  (disassemble-bytes (:address instruction) (:bytes instruction)))]
    (.write writer
            (format "$%04X  %-8s %s\n"
                    (:address decoded)
                    (str/join " " (map #(format "%02X" %) (:bytes decoded)))
                    (:text decoded)))))

(defn- write-compressed-assembly!
  [writer artifact]
  (let [execution (require-execution artifact)
        instructions (:instructions execution)
        blocks (:blocks execution)
        spans (sort-by :start-index (:spans execution))
        block-runs (vec (:block-runs execution))]
    (.write writer "; interned basic-block dictionary\n")
    (doseq [{:keys [id instruction-ids]} blocks]
      (.write writer
              (format "\n; block %d, %d instructions\n"
                      id (count instruction-ids)))
      (doseq [instruction-id instruction-ids]
        (write-static-instruction! writer (nth instructions instruction-id))))
    (.write writer "\n; chronological span/block timeline\n")
    (loop [remaining-spans (seq spans)
           run-index 0]
      (when-let [span (first remaining-spans)]
        (.write writer "\n")
        (.write writer (span-assembly-header span))
        (.write writer "\n")
        (let [run-index
              (loop [index run-index]
                (if (and (< index (count block-runs))
                         (<= (:end-index (nth block-runs index))
                             (:start-index span)))
                  (recur (inc index))
                  index))
              next-run-index
              (loop [index run-index]
                (if (>= index (count block-runs))
                  index
                  (let [{:keys [start-index end-index block-id iterations]}
                        (nth block-runs index)]
                    (if (>= start-index (:end-index span))
                      index
                      (let [overlap-start (max start-index (:start-index span))
                            overlap-end (min end-index (:end-index span))]
                        (.write writer
                                (format "; block-run events %d..%d block=%d iterations=%d%s\n"
                                        overlap-start
                                        (dec overlap-end)
                                        block-id
                                        iterations
                                        (if (or (not= overlap-start start-index)
                                                (not= overlap-end end-index))
                                          " clipped-to-span"
                                          "")))
                        (if (<= end-index (:end-index span))
                          (recur (inc index))
                          index))))))]
          (recur (next remaining-spans) next-run-index))))))

(defn- render-assembly!
  [writer artifact]
  (write-compressed-assembly! writer artifact))

(defn artifact->assembly
  "Render a pipeline artifact using the canonical assembly renderer.

  Dictionary-coded captures default to a deduplicated report: each interned
  basic block is emitted once and the chronological span/block timeline refers
  to it by block ID. Without `:output-file`, this returns a string; with an
  output file, rendering is streamed and the file is returned. Both paths use
  the same renderer.

  Options:
  * `:output-file` - stream the result to this file instead of returning text."
  ([artifact]
   (artifact->assembly artifact {}))
  ([artifact {:keys [output-file]}]
   (if output-file
     (with-open [writer (io/writer output-file)]
       (render-assembly! writer artifact)
       output-file)
     (let [writer (java.io.StringWriter.)]
       (render-assembly! writer artifact)
       (str writer)))))


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
     :write-cycle-offset offset
     :instruction-cycles duration-cycles}))

(defn- enrich-instructions
  ([raw-instructions initial-memory]
   (enrich-instructions raw-instructions initial-memory {}))
  ([raw-instructions initial-memory {:keys [cycles-per-line raster-lines boundary-entry]
                                    :or {cycles-per-line 63 raster-lines 312}}]
   (let [memory (byte-array (map unchecked-byte initial-memory))
         raw-instructions (vec raw-instructions)]
     (reduce-kv
      (fn [{:keys [instructions writes]} instruction-index entry]
        (let [pc (:pc entry)
              decoded (if (seq (:bytes entry))
                        (disassemble-bytes pc (:bytes entry))
                        (disassemble-bytes pc
                                           (mapv #(memory-byte memory (+ pc %))
                                                 (range 4))))
              write (inferred-write memory decoded entry)
              next-entry (or (nth raw-instructions (inc instruction-index) nil)
                             boundary-entry)
              duration-cycles (elapsed-cycles entry next-entry cycles-per-line raster-lines)
              instruction (merge entry
                                 (select-keys decoded [:bytes :mnemonic :mode :operand :text])
                                 {:instruction-index instruction-index
                                  :instruction-cycles duration-cycles})
              write-event (when write
                            (merge (select-keys entry [:pc])
                                   (write-timing entry duration-cycles cycles-per-line)
                                   write
                                   {:instruction-index instruction-index
                                    :mnemonic (:mnemonic decoded)}))]
          (when write
            (aset-byte memory (:address write) (unchecked-byte (:value write))))
          {:instructions (conj instructions instruction)
           :writes (cond-> writes write-event (conj write-event))}))
      {:instructions [] :writes []}
      raw-instructions))))

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
                              (merge (select-keys write [:instruction-index :pc
                                                         :raster-line :cpu-cycle])
                                     (vic-configuration state)))
                        configurations)))
             result))
         {:state initial-state
          :events []
          :configurations [(assoc (vic-configuration initial-state)
                                  :instruction-index -1)]}
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
  [initial-memory writes instruction-index]
  (let [memory (byte-array (map unchecked-byte initial-memory))]
    (doseq [{:keys [address value]} (take-while #(<= (:instruction-index %) instruction-index)
                                                writes)]
      (aset-byte memory address (unchecked-byte value)))
    memory))

(defn- memory-range
  [memory start length]
  (mapv #(memory-byte memory (+ start %)) (range length)))

(defn- asset-sample
  [initial-memory writes configuration]
  (let [memory (memory-after-writes initial-memory writes (:instruction-index configuration))
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

(defn- static-instruction
  [id instruction]
  {:id id
   :address (:pc instruction)
   :bytes (vec (:bytes instruction))
   :mnemonic (:mnemonic instruction)
   :mode (:mode instruction)
   :operand (:operand instruction)
   :text (:text instruction)})

(defn- instruction-key
  [instruction]
  [(:pc instruction) (vec (:bytes instruction))])

(defn- intern-instructions
  "Intern executed instruction images and record address-version intervals.

  An instruction is the immutable executed image `[PC bytes]`. A node version
  is an address's chronological incarnation: executing changed bytes at a
  previously seen address starts a new version, including when bytes revert to
  an older instruction. `:first-event` and `:last-event` describe observations
  of that image, not an unobservable continuous memory-lifetime interval."
  [executed-instructions]
  (let [state (reduce (fn [{:keys [instruction-ids instructions address-state
                                   node-versions instruction-ids-by-event]
                            :as state}
                           [event-index instruction]]
                        (let [key (instruction-key instruction)
                              instruction-id (or (get instruction-ids key)
                                                 (count instructions))
                              state (if (contains? instruction-ids key)
                                      state
                                      (-> state
                                          (assoc-in [:instruction-ids key] instruction-id)
                                          (update :instructions conj
                                                  (static-instruction instruction-id instruction))))
                              {:keys [node-id version] :as prior}
                              (get-in state [:address-state (:pc instruction)])
                              changed? (not= instruction-id (:instruction-id prior))
                              state (if changed?
                                      (let [next-node-id (count (:node-versions state))
                                            next-version (if prior (inc version) 0)]
                                        (cond-> state
                                          true (update :node-versions conj
                                                       {:id next-node-id
                                                        :address (:pc instruction)
                                                        :version next-version
                                                        :instruction-id instruction-id
                                                        :first-event event-index
                                                        :last-event event-index})
                                          true (assoc-in [:address-state (:pc instruction)]
                                                         {:instruction-id instruction-id
                                                          :node-id next-node-id
                                                          :version next-version})))
                                      (assoc-in state [:node-versions node-id :last-event]
                                                event-index))]
                          (update state :instruction-ids-by-event conj instruction-id)))
                      {:instruction-ids {}
                       :instructions []
                       :address-state {}
                       :node-versions []
                       :instruction-ids-by-event []}
                      (map-indexed vector executed-instructions))]
    (select-keys state [:instructions :node-versions :instruction-ids-by-event])))

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

(defn- basic-block-execution
  ([occurrences]
   (basic-block-execution occurrences (constantly false)))
  ([occurrences boundary?]
   (let [block-state (atom {:block-ids {} :blocks []})
         block-runs (transduce (comp (basic-block-builder-xf boundary?)
                                     (deduplicate-basic-blocks-xf block-state))
                               conj [] occurrences)]
     {:blocks (:blocks @block-state)
      :block-runs block-runs})))

(defn versioned-execution
  "Build the canonical, lossless instruction/block execution representation.

  Static instruction images are interned in `:instructions`. Dynamic basic
  blocks reference them by `:instruction-ids`, and chronological
  `:block-runs` preserve every executed occurrence without retaining expanded
  instruction maps."
  ([instructions spans]
   (versioned-execution instructions spans {}))
  ([executed-instructions spans _options]
   (let [executed-instructions (vec executed-instructions)
         spans (vec (or spans []))
         {:keys [instructions node-versions instruction-ids-by-event]}
         (intern-instructions executed-instructions)
         occurrences (mapv (fn [event-index instruction-id instruction]
                             {:event-index event-index
                              :instruction-id instruction-id
                              :instruction instruction})
                           (range) instruction-ids-by-event executed-instructions)
         boundary-starts (set (map :start-index spans))
         {:keys [blocks block-runs]}
         (basic-block-execution occurrences
                                 #(contains? boundary-starts (:event-index %)))]
     {:format :omkamra.vice/versioned-execution-v3
      :event-count (count executed-instructions)
      :instructions instructions
      :node-versions node-versions
      :blocks blocks
      :block-runs block-runs
      :spans (mapv #(assoc (select-keys % [:kind :start-index :end-index :trigger
                                            :entry-pc :return-pc :capture-index])
                           :instruction-count
                           (- (:end-index %) (:start-index %)))
                   spans)})))

(declare raw-artifact run-pipeline expand-raw-events)

(defn- code-span
  [instructions kind start end]
  (when (< start end)
    (let [trace (subvec (vec instructions) start end)]
      {:kind kind
       :start-index start
       :end-index end
       :trace trace})))

(def ^:private conditional-branch-mnemonics
  #{"BCC" "BCS" "BEQ" "BMI" "BNE" "BPL" "BVC" "BVS"})

(def ^:private dynamic-transfer-mnemonics
  #{"RTS" "RTI" "KIL"})

(defn- instruction-pc
  [instruction]
  (or (:pc instruction) (:address instruction)))

(defn- instruction-width
  [instruction]
  (or (mode-width (:mode instruction))
      (count (:bytes instruction))
      1))

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
        fall-through (bit-and (+ pc (instruction-width instruction)) 0xffff)
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

(defn- unexpected-control-flow?
  [memory previous current]
  (let [successors (control-flow-successors memory previous)]
    (and (seq successors)
         (not (contains? successors (instruction-pc current))))))

(defn- interrupt-entry-indices
  "Find asynchronous-looking entries by comparing observed and legal PCs.

  The monitor trace does not need to expose the IRQ vector read: a hardware
  interrupt appears as a successor that is not explained by the preceding
  instruction's control-flow semantics."
  [instructions initial-memory]
  (keep (fn [[index previous current]]
          (when (unexpected-control-flow? initial-memory previous current)
            index))
        (map vector
             (range 1 (count instructions))
             instructions
             (rest instructions))))

(defn- irq-sections
  [instructions initial-memory]
  (let [instructions (vec instructions)
        starts (vec (interrupt-entry-indices instructions initial-memory))
        ranges (mapv (fn [[start next-start]]
                       (let [end-limit (or next-start (count instructions))
                             rti-index (some (fn [index]
                                               (when (= "RTI"
                                                        (:mnemonic (nth instructions index)))
                                                 index))
                                             (range start end-limit))
                             end (if rti-index (inc rti-index) end-limit)]
                         [start end]))
                     (map vector starts (concat (rest starts) [nil])))
        irq-sections
        (->> ranges
             (map (fn [[start end]]
                    (let [trace (subvec instructions start end)
                          trigger (first trace)]
                      (assoc (code-span instructions :irq start end)
                             :trigger (select-keys trigger [:raster-line :cpu-cycle])
                             :entry-pc (instruction-pc trigger)
                             :return-pc (instruction-pc (last trace))))))
             ;; Present IRQ sections in scanline order while retaining original
             ;; instruction indices for wrap-around and reconstruction.
             (sort-by (juxt (comp :raster-line :trigger)
                            (comp :cpu-cycle :trigger)
                            :start-index))
             vec)
        non-irq-ranges
        (if (empty? ranges)
          [[0 (count instructions)]]
          (let [gaps (concat [[0 (ffirst ranges)]]
                             (map (fn [[[start end] [next-start _next-end]]]
                                      [end next-start])
                                  (partition 2 1 ranges))
                             [[(second (last ranges)) (count instructions)]])]
            (filter (fn [[start end]] (< start end)) gaps)))
        non-irq-sections (mapv #(code-span instructions :non-irq (first %) (second %))
                               non-irq-ranges)
        spans (->> (concat irq-sections non-irq-sections)
                   (sort-by :start-index)
                   vec)]
    {:prelude (or (:trace (first non-irq-sections)) [])
     :sections irq-sections
     :irq-sections irq-sections
     :non-irq-sections non-irq-sections
     :spans spans}))

(defn- ram-code-pc?
  [pc {:keys [ram-start ram-end]
       :or {ram-start 0x0400
            ram-end 0xe000}}]
  (and (integer? pc)
       (<= ram-start pc)
       (< pc ram-end)))

(defn- first-code-entry
  [spans kind options]
  (some (fn [span]
          (when (= kind (:kind span))
            (some (fn [[offset entry]]
                    (when (ram-code-pc? (:pc entry) options)
                      {:span-kind kind
                       :index (+ (:start-index span) offset)
                       :entry entry}))
                  (map-indexed vector (:trace span)))))
        spans))

(defn- frame-code-start
  "Identify the first likely demo-code execution in structural spans.

  `:first-ram-code` is the earliest instruction outside the usual KERNAL ROM
  range. `:first-ram-irq-code` is the stronger raster/frame signal: the first
  IRQ span containing an instruction in RAM. Both are heuristic and retain the
  original trace entry for inspection."
  ([capture]
   (frame-code-start capture {}))
  ([capture options]
   (let [spans (:spans capture)]
     {:first-ram-code (first-code-entry spans :non-irq options)
      :first-ram-irq-code (first-code-entry spans :irq options)})))

(def ^:private stream-events-format
  :omkamra.vice/instruction-block-stream-v3)

(def ^:private stream-sample-keys
  [:raster-line :cpu-cycle :a :x :y :sp :flags :global-cycle])

(defn- instruction-block-stream?
  [events]
  (= stream-events-format (:format events)))

(defn- stream-instruction-ids
  [events]
  (let [blocks (:blocks events)]
    (mapcat (fn [{:keys [block-id iterations]}]
              (apply concat
                     (repeat iterations
                             (:instruction-ids (nth blocks block-id)))))
            (:block-runs events))))

(defn- expand-raw-events
  [events]
  (if (instruction-block-stream? events)
    (let [instructions (:instructions events)]
      (mapv (fn [instruction-id sample]
              (merge (dissoc (nth instructions instruction-id) :id)
                     (zipmap (:sample-keys events) sample)))
            (stream-instruction-ids events)
            (:samples events)))
    (vec events)))

(defn raw-artifact
  "Create the intentionally low-level input to the enrichment pipeline.

  Events may be a direct vector or a lossless dictionary-coded event stream.
  No semantic roles are assigned here. Memory/display snapshots and boundary
  metadata remain available so every enrichment stage can be replayed."
  ([events]
   (raw-artifact events {}))
  ([events {:keys [initial-memory final-memory boundary-entry display palette metadata]}]
   {:format :omkamra.vice/raw-trace-v1
    :raw {:events (if (instruction-block-stream? events) events (vec events))
          :memory {:initial initial-memory :final final-memory}
          :display display
          :palette palette
          :boundary-entry boundary-entry
          :metadata metadata}
    :stages {}}))

(defn normalize-stage
  "Assign the stable chronological event index without semantic analysis."
  [artifact]
  (assoc-in artifact [:stages :normalized-events]
            (mapv (fn [event-index event]
                    (assoc event :event-index event-index))
                  (range)
                  (expand-raw-events (get-in artifact [:raw :events])))))

(defn decode-stage
  "Decode normalized CPU events and infer instruction timing/writes."
  [artifact]
  (let [events (get-in artifact [:stages :normalized-events])
        initial-memory (get-in artifact [:raw :memory :initial])
        boundary-entry (get-in artifact [:raw :boundary-entry])
        decode-options (get-in artifact [:raw :metadata :decode-options])
        {:keys [instructions writes]} (enrich-instructions
                                       events initial-memory
                                       (assoc decode-options
                                              :boundary-entry boundary-entry))]
    (assoc-in artifact [:stages :decoded]
              {:instructions instructions
               :writes (mapv #(assoc % :event-index (:instruction-index %)) writes)})))

(defn memory-stage
  "Publish the memory timeline produced by decoded write effects."
  [artifact]
  (assoc-in artifact [:stages :memory]
            {:initial (get-in artifact [:raw :memory :initial])
             :final (get-in artifact [:raw :memory :final])
             :writes (get-in artifact [:stages :decoded :writes])}))

(defn structure-stage
  "Derive structural IRQ/span/basic-block information, but no semantic roles."
  [artifact]
  (let [instructions (get-in artifact [:stages :decoded :instructions])
        initial-memory (get-in artifact [:raw :memory :initial])
        irq-data (irq-sections instructions initial-memory)
        clean-span (fn [span]
                     (dissoc span :trace :compressed))
        spans (mapv clean-span (:spans irq-data))
        irq-spans (mapv clean-span (:sections irq-data))
        non-irq-spans (mapv clean-span (:non-irq-sections irq-data))
        execution (versioned-execution instructions (:spans irq-data))]
    (assoc-in artifact [:stages :structure]
              {:spans spans
               :irq-sections irq-spans
               :non-irq-sections non-irq-spans
               :execution execution
               :frame-code (frame-code-start {:spans (:spans irq-data)})})))

(defn video-stage
  "Derive VIC configuration, inferred raster writes, and asset samples."
  [artifact]
  (let [initial-memory (get-in artifact [:raw :memory :initial])
        writes (get-in artifact [:stages :decoded :writes])
        vic (derive-vic initial-memory writes)
        assets (asset-samples initial-memory writes (:configurations vic))]
    (assoc-in artifact [:stages :video]
              {:vic (assoc vic :sprite-pointer-writes
                           (sprite-pointer-events initial-memory writes))
               :assets assets})))

(defn semantics-stage
  "Reserved semantic stage.

  It deliberately assigns no roles yet. Later classifiers can add evidence and
  confidence without changing the raw, decoded, or structural layers."
  [artifact]
  (assoc-in artifact [:stages :semantics]
            {:status :unclassified
             :spans []}))

(def ^:private pipeline-stages
  {:normalize normalize-stage
   :decode decode-stage
   :memory memory-stage
   :structure structure-stage
   :video video-stage
   :semantics semantics-stage})

(defn run-pipeline
  "Run enrichment stages over a raw artifact.

  `:stages` defaults to all currently available stages. Stages are pure with
  respect to VICE and can be rerun from the persisted raw artifact."
  ([artifact]
   (run-pipeline artifact {}))
  ([artifact {:keys [stages compact?]
              :or {stages [:normalize :decode :memory :structure :video
                           :semantics]
                   compact? false}}]
   (reduce (fn [artifact stage]
             (if-let [stage-fn (get pipeline-stages stage)]
               (let [artifact (stage-fn artifact)]
                 (if compact?
                   (case stage
                     :decode (update artifact :stages dissoc :normalized-events)
                     :structure (update-in artifact [:stages :decoded]
                                           dissoc :instructions)
                     artifact)
                   artifact))
               (throw (ex-info "Unknown trace enrichment stage"
                               {:stage stage
                                :known-stages (keys pipeline-stages)}))))
           artifact
           stages)))

(defn- empty-stream-state
  []
  {:status :starting
   :block-runs []
   :samples []
   :first-by-pc {}
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
                             (let [decoded (disassemble-bytes pc bytes)]
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

(defn- collect-stream-samples-xf
  "Append compact dynamic samples while passing occurrences to block stages."
  []
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [event] :as occurrence}]
       (let [event-index (:event-count result)
             pc (:pc event)
             result (-> result
                        (assoc :status :streaming)
                        (update :samples conj
                                (mapv #(get event %) stream-sample-keys))
                        (assoc-in [:first-by-pc pc]
                                  (or (get-in result [:first-by-pc pc])
                                      {:event-index event-index :entry event}))
                        (update :event-count inc))]
         (rf result (assoc occurrence :event-index event-index)))))))

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

(defn- make-stream-ingester
  "Create one fused transducer pipeline for a FIFO capture.

  Parsed records flow through instruction interning, sample collection, basic
  block construction, and block interning. `:initial-memory` is used to
  identify unexpected control-flow discontinuities, which are explicit basic
  block boundaries because an IRQ can interrupt a fall-through block without
  executing a terminator. `:complete` must be called exactly once after the
  FIFO reaches EOF so the basic-block transducer flushes its pending
  fall-through block."
  ([]
   (make-stream-ingester {}))
  ([{:keys [initial-memory]}]
   (let [instruction-state (atom {:instruction-ids {} :instructions []})
         block-state (atom {:block-ids {} :blocks []})
         xf (comp (intern-stream-instructions-xf instruction-state)
                  (collect-stream-samples-xf)
                  (mark-control-flow-boundaries-xf initial-memory)
                  (basic-block-builder-xf :boundary?)
                  (deduplicate-basic-blocks-xf block-state))
        reducer (xf (fn
                      ([] (empty-stream-state))
                      ([result] result)
                      ([result block-run]
                       (update result :block-runs conj block-run))))]
    {:step (fn [state event] (reducer state event))
     :complete (fn [state] (reducer state))
     :instruction-state instruction-state
     :block-state block-state})))

(defn- instruction-block-stream
  [stream-state ingester]
  (let [{:keys [block-runs samples event-count]} @stream-state
        {:keys [instructions]} @(:instruction-state ingester)
        {:keys [blocks]} @(:block-state ingester)]
    {:format stream-events-format
     :event-count event-count
     :sample-keys stream-sample-keys
     :instructions instructions
     :blocks blocks
     :block-runs block-runs
     :samples samples}))

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

  The returned recorder is consumed by `await-cpu-range`, `await-demo-part`,
  and `stop-capture`. Options are `:fifo-path`, `:metadata`, and
  `:checkpoint-op` (default 4, execute)."
  ([conn]
   (start-capture conn {}))
  ([conn {:keys [fifo-path metadata checkpoint-op]
          :or {checkpoint-op 4}}]
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
             ingester (make-stream-ingester {:initial-memory initial-memory})]
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
           {:kind :omkamra.vice/streaming-capture-v1
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
    (reset! (:block-state ingester) {:block-ids {} :blocks []})))

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

(defn- first-pc-observation
  [stream-state pred]
  (->> (:first-by-pc @stream-state)
       (keep (fn [[pc observation]]
               (when (pred pc) observation)))
       (sort-by :event-index)
       first))

(defn- await-stream-progress
  [stream-state previous-count timeout-ms]
  (let [deadline (+ (System/nanoTime) (* (long timeout-ms) 1000000))]
    (loop []
      (let [{:keys [event-count error]} @stream-state]
        (when error (throw error))
        (cond
          (> event-count previous-count) event-count
          (< (System/nanoTime) deadline)
          (do (Thread/sleep 5) (recur))
          :else event-count)))))

(defn await-cpu-range
  "Advance a streaming capture until the CPU executes inside `:pc-range`.

  This is intended for synchronization with the KERNAL idle loop before
  AUTOSTART. VICE remains paused at the end of each bounded advance. Options
  are `:pc-range` as `[inclusive-start inclusive-end]`,
  `:chunk-instructions` (default 10000), `:timeout-ms` (default 30000), and
  `:advance-timeout-ms` (default 10000)."
  [capture {:keys [pc-range chunk-instructions timeout-ms advance-timeout-ms]
            :or {chunk-instructions 10000
                 timeout-ms 30000
                 advance-timeout-ms 10000}}]
  (let [[start-pc end-pc] pc-range]
    (when-not (and (integer? start-pc)
                   (integer? end-pc)
                   (<= 0 start-pc end-pc 0xffff))
      (throw (IllegalArgumentException.
              ":pc-range must be [start end] within the 16-bit address space")))
    (let [conn (:conn capture)
          stream-state (:stream-state capture)
          deadline (+ (System/nanoTime) (* (long timeout-ms) 1000000))
          matches? #(<= start-pc % end-pc)]
      (loop [advanced 0]
        (if-let [observation (first-pc-observation stream-state matches?)]
          (let [marker (assoc observation
                              :status :cpu-range
                              :pc-range [start-pc end-pc]
                              :advanced-instructions advanced)]
            (swap! (:state capture) assoc
                   :status :cpu-range
                   :cpu-range marker
                   :instruction-count (:event-count @stream-state))
            marker)
          (if (< (System/nanoTime) deadline)
            (let [before (:event-count @stream-state)]
              (bm/advance-and-wait conn {:count chunk-instructions
                                         :timeout-ms advance-timeout-ms})
              (await-stream-progress stream-state before 1000)
              (recur (+ advanced chunk-instructions)))
            (throw (ex-info "Timed out waiting for CPU PC range"
                            {:pc-range [start-pc end-pc]
                             :timeout-ms timeout-ms
                             :advanced-instructions advanced
                             :event-count (:event-count @stream-state)}))))))))

(defn await-demo-part
  "Wait for one of `:entry-pcs` in a running streaming capture.

  The FIFO reader performs parsing and instruction interning concurrently, so
  this function only polls the in-memory PC index and never rereads trace text.
  Options are `:entry-pcs` (required), `:timeout-ms` (default 30000), and
  `:poll-ms` (default 25)."
  [capture {:keys [entry-pcs timeout-ms poll-ms]
            :or {timeout-ms 30000 poll-ms 25}}]
  (let [entry-pcs (set entry-pcs)
        stream-state (:stream-state capture)]
    (when (empty? entry-pcs)
      (throw (IllegalArgumentException. ":entry-pcs must not be empty")))
    (let [deadline (+ (System/nanoTime) (* (long timeout-ms) 1000000))]
      (loop []
        (let [{:keys [error event-count]} @stream-state]
          (when error (throw error))
          (if-let [observation
                   (first-pc-observation stream-state entry-pcs)]
            (let [marker (assoc observation :entry-pcs entry-pcs)]
              (swap! (:state capture) assoc
                     :status :demo-part
                     :first-demo-part marker
                     :instruction-count event-count)
              marker)
            (if (< (System/nanoTime) deadline)
              (do (Thread/sleep (long poll-ms)) (recur))
              (throw (ex-info "Timed out waiting for first demo part"
                              {:entry-pcs entry-pcs
                               :timeout-ms timeout-ms
                               :event-count event-count})))))))))

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
          (let [decoded (disassemble-bytes pc bytes)]
            (assoc decoded :id id :address pc)))
        (:instructions events)))

(defn- sample-map
  [events sample]
  (zipmap (:sample-keys events) sample))

(defn- stream-event-at
  [events instructions instruction-ids event-index]
  (let [instruction-id (aget ^ints instruction-ids event-index)
        instruction (nth instructions instruction-id)]
    (merge (-> (select-keys instruction
                            [:bytes :mnemonic :mode :operand :text])
               (assoc :pc (:address instruction)))
           (sample-map events (nth (:samples events) event-index)))))

(defn- stream-writes
  [events instructions instruction-ids initial-memory]
  (let [memory (byte-array (map unchecked-byte initial-memory))
        samples (:samples events)
        event-count (:event-count events)]
    (loop [event-index 0
           writes (transient [])]
      (if (= event-index event-count)
        (persistent! writes)
        (let [instruction-id (aget ^ints instruction-ids event-index)
              {:keys [address mnemonic] :as instruction}
              (nth instructions instruction-id)]
          (if (or (contains? direct-store-mnemonics mnemonic)
                  (contains? read-modify-write-mnemonics mnemonic))
            (let [entry (assoc (sample-map events (nth samples event-index))
                               :pc address)
                  next-entry (when (< (inc event-index) event-count)
                               (sample-map events (nth samples (inc event-index))))
                  duration-cycles (elapsed-cycles entry next-entry 63 312)
                  write (inferred-write memory instruction entry)
                  write-event (when write
                                (merge {:pc address
                                        :mnemonic mnemonic
                                        :instruction-index event-index
                                        :event-index event-index}
                                       (write-timing entry duration-cycles 63)
                                       write))]
              (when write
                (aset-byte memory (:address write)
                           (unchecked-byte (:value write))))
              (recur (inc event-index)
                     (if write-event (conj! writes write-event) writes)))
            (recur (inc event-index) writes)))))))

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
  [events instructions instruction-ids initial-memory]
  (let [event-count (:event-count events)
        starts (persistent!
                (loop [index 1 result (transient [])]
                  (if (= index event-count)
                    result
                    (let [previous (nth instructions
                                        (aget ^ints instruction-ids (dec index)))
                          current (nth instructions
                                       (aget ^ints instruction-ids index))]
                      (recur (inc index)
                             (if (unexpected-control-flow?
                                  initial-memory previous current)
                               (conj! result index)
                               result))))))
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
                 (cond-> {:kind kind :start-index start :end-index end}
                   (= kind :irq)
                   (assoc :trigger
                          (select-keys
                           (sample-map events (nth (:samples events) start))
                           [:raster-line :cpu-cycle])
                          :entry-pc (:address
                                     (nth instructions
                                          (aget ^ints instruction-ids start)))
                          :return-pc (:address
                                      (nth instructions
                                           (aget ^ints instruction-ids (dec end))))))))
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
    {:irq-sections irq-spans
     :non-irq-sections non-irq-spans
     :spans spans}))

(defn- first-stream-code-entry
  [events instructions instruction-ids spans kind]
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
                                             instruction-ids index)}
                    (recur (inc index))))))))
        spans))

(defn- streaming-pipeline-artifact
  [events initial-memory final-memory metadata]
  (let [instructions (decoded-stream-instructions events)
        instruction-ids (stream-instruction-id-array events)
        writes (stream-writes events instructions instruction-ids initial-memory)
        irq-data (stream-irq-data events instructions instruction-ids initial-memory)
        spans (:spans irq-data)
        execution {:format :omkamra.vice/versioned-execution-v3
                   :event-count (:event-count events)
                   :instructions instructions
                   :node-versions (stream-node-versions instructions instruction-ids)
                   :blocks (:blocks events)
                   :block-runs (:block-runs events)
                   :spans (mapv (fn [span]
                                  (assoc span
                                         :instruction-count
                                         (- (:end-index span)
                                            (:start-index span))))
                                spans)}
        frame-code {:first-ram-code
                    (first-stream-code-entry events instructions instruction-ids
                                             spans :non-irq)
                    :first-ram-irq-code
                    (first-stream-code-entry events instructions instruction-ids
                                             spans :irq)}
        vic (derive-vic initial-memory writes)]
    {:format :omkamra.vice/pipeline-v1
     :raw {:events events
           :memory {:initial initial-memory :final final-memory}
           :display nil
           :palette nil
           :boundary-entry nil
           :metadata metadata}
     :stages
     {:decoded {:writes writes}
      :memory {:initial initial-memory :final final-memory :writes writes}
      :structure {:spans spans
                  :irq-sections (:irq-sections irq-data)
                  :non-irq-sections (:non-irq-sections irq-data)
                  :execution execution
                  :frame-code frame-code}
      :video {:vic (assoc vic :sprite-pointer-writes
                          (sprite-pointer-events initial-memory writes))
              :assets (asset-samples initial-memory writes
                                     (:configurations vic))}
      :semantics {:status :unclassified :spans []}}}))

(defn stop-capture
  "Stop a FIFO-backed capture and return a compact canonical artifact.

  Logging is disabled first, closing VICE's FIFO writer. The reader drains to
  EOF, then the raw dictionary stream is enriched. Temporary expanded decode
  vectors are discarded after structural execution has been interned. Cleanup
  always removes the checkpoint, restores unsolicited-event handling, closes
  the reader, and deletes the FIFO. VICE remains paused."
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
                                :cpu-range (:cpu-range @state)}))]
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
