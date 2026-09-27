(ns omkamra.vice.asm
  "MOS 6510 disassembly and compact assembly rendering for VICE artifacts."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

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

(defn instruction-width
  "Return the encoded width of a decoded 6510 instruction."
  [instruction]
  (or (mode-width (:mode instruction))
      (count (:bytes instruction))
      1))

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
        width (instruction-width {:mode mode :bytes bytes})
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

(defn- require-execution
  [artifact]
  (or (get-in artifact [:stages :structure :execution])
      (throw (ex-info "Artifact has no structural execution stage"
                      {:format (:format artifact)}))))

(defn- format-assembly-byte
  [byte]
  (if (nil? byte)
    "??"
    (format "%02X" (u8 byte))))

(defn- write-static-instruction!
  [writer instruction]
  (let [decoded (if (:text instruction)
                  instruction
                  (disassemble-bytes (:address instruction) (:bytes instruction)))]
    (.write writer
            (format "$%04X  %-8s %s\n"
                    (:address decoded)
                    (str/join " " (map format-assembly-byte (:bytes decoded)))
                    (:text decoded)))))

(defn- instruction-shape-key
  "Return the assembly-level identity of an instruction.

  The exact execution model keys instructions by all bytes. Assembly output is
  intentionally less specific: the opcode determines the addressing mode and
  instruction width, while operand bytes may be runtime parameters (for
  example, a color value in a raster interrupt)."
  [instruction]
  [(:address instruction) (first (:bytes instruction))])

(defn- assembly-operand-byte
  [instruction position varying-positions]
  (if (contains? varying-positions position)
    "??"
    (format "%02X" (u8 (nth (:bytes instruction) position 0)))))

(defn- assembly-operand-text
  [instruction varying-positions]
  (if (empty? varying-positions)
    (:operand instruction)
    (let [byte-token #(assembly-operand-byte instruction % varying-positions)
          mode (:mode instruction)]
      (case mode
        :imp ""
        :acc "A"
        :imm (str "#$" (byte-token 1))
        :zp (str "$" (byte-token 1))
        :zpx (str "$" (byte-token 1) ",X")
        :zpy (str "$" (byte-token 1) ",Y")
        :abs (str "$" (byte-token 2) (byte-token 1))
        :absx (str "$" (byte-token 2) (byte-token 1) ",X")
        :absy (str "$" (byte-token 2) (byte-token 1) ",Y")
        :ind (str "($" (byte-token 2) (byte-token 1) ")")
        :indx (str "($" (byte-token 1) ",X)")
        :indy (str "($" (byte-token 1) "),Y")
        ;; A relative operand is displayed as a target address when stable.
        ;; Once it varies, displaying the raw displacement would be
        ;; misleading because the target is the semantically relevant value.
        :rel "$????"
        (:operand instruction)))))

(defn- assembly-instruction-template
  [instructions instruction-ids]
  (let [variants (->> instruction-ids distinct (mapv #(nth instructions %)))
        representative (let [instruction (first variants)]
                         (if (:text instruction)
                           instruction
                           (merge instruction
                                  (disassemble-bytes (:address instruction)
                                                     (:bytes instruction)))))
        byte-count (count (:bytes representative))
        varying-positions
        (->> (range 1 byte-count)
             (filter (fn [position]
                       (> (count (distinct (map #(nth (:bytes %) position 0)
                                                 variants)))
                          1)))
             set)
        operand (assembly-operand-text representative varying-positions)]
    (assoc representative
           :bytes (mapv (fn [position]
                          (when-not (contains? varying-positions position)
                            (nth (:bytes representative) position 0)))
                        (range byte-count))
           :operand operand
           :text (str (:mnemonic representative)
                      (when (seq operand) (str " " operand))))))

(defn- assembly-dictionary
  "Project exact execution blocks onto compact structural block templates.

  The persisted execution model remains lossless. This projection merges
  blocks whose instructions have the same addresses and opcodes, retaining
  operand variation only in the rendered instruction templates and variant
  counts."
  [execution]
  (let [instructions (:instructions execution)
        exact-blocks (:blocks execution)
        {:keys [shape-blocks shape-key-to-id]}
        (reduce
         (fn [{:keys [shape-blocks shape-key-to-id] :as state}
              block]
           (let [shape-key (mapv #(instruction-shape-key (nth instructions %))
                                 (:instruction-ids block))
                 existing-shape-id (get shape-key-to-id shape-key)
                 shape-id (if (nil? existing-shape-id)
                            (count shape-blocks)
                            existing-shape-id)]
             (if (nil? existing-shape-id)
               (-> state
                   (assoc-in [:shape-key-to-id shape-key] shape-id)
                   (update :shape-blocks conj
                           {:id shape-id
                            :shape-key shape-key
                            :exact-block-ids [(:id block)]}))
               (update-in state [:shape-blocks shape-id :exact-block-ids]
                          conj (:id block)))))
         {:shape-blocks [] :shape-key-to-id {}}
         exact-blocks)
        exact-block-by-id (into {} (map (juxt :id identity) exact-blocks))
        shape-blocks
        (mapv (fn [{:keys [id shape-key exact-block-ids]}]
                (let [exact-blocks (mapv exact-block-by-id exact-block-ids)
                      instruction-id-groups
                      (mapv (fn [position]
                              (mapv #(nth (:instruction-ids %) position)
                                    exact-blocks))
                            (range (count shape-key)))]
                  {:id id
                   :instruction-id-groups instruction-id-groups
                   :instructions
                   (mapv #(assembly-instruction-template instructions %)
                         instruction-id-groups)
                   :variant-count (count exact-block-ids)}))
              shape-blocks)]
    {:blocks shape-blocks}))

(defn- write-compressed-assembly!
  [writer artifact]
  (let [execution (require-execution artifact)
        {:keys [blocks]} (assembly-dictionary execution)]
    (.write writer "; structural basic-block template dictionary\n")
    (doseq [{:keys [id instructions variant-count]} blocks]
      (.write writer
              (format "\n; block %d, %d instructions%s\n"
                      id
                      (count instructions)
                      (if (> variant-count 1)
                        (format ", %d code-image variants" variant-count)
                        "")))
      (doseq [instruction instructions]
        (write-static-instruction! writer instruction)))))

(defn- render-assembly!
  [writer artifact]
  (write-compressed-assembly! writer artifact))

(defn artifact->assembly
  "Render a pipeline artifact using the canonical assembly renderer.

  Dictionary-coded captures default to a deduplicated report: each
  structural basic-block template is emitted once. Operand bytes that vary
  across exact code images are rendered as wildcards; the exact variants and
  chronological execution data remain in the artifact. Without `:output-file`,
  this returns a string; with an output file, rendering is streamed and the
  file is returned. Both paths use the same renderer.

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
