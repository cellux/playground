(ns omkamra.vice.isa
  "Self-contained MOS 6510 opcode vocabulary and instruction-class semantics
  used by the v2 shared capture reader for logical write inference.

  This namespace deliberately does not depend on the v1 `omkamra.vice.asm`
  disassembler: v2 owns its instruction vocabulary so that retiring v1 does not
  change which writes are inferred from a capture. It provides the 256-entry
  opcode table, addressing-mode widths and operand resolution, the instruction
  effect classification, the final-value rules for stores and read-modify-write
  instructions, and the branch/stack helpers the reader needs to compute a
  deterministic successor PC."
  (:require [clojure.string :as str]))

(def mode-width
  {:imp 1 :acc 1 :imm 2 :zp 2 :zpx 2 :zpy 2 :rel 2
   :abs 3 :absx 3 :absy 3 :ind 3 :indx 2 :indy 2})

(def mode-operand-width
  "Number of operand bytes encoded after the opcode for each mode."
  {:imp 0 :acc 0 :imm 1 :zp 1 :zpx 1 :zpy 1 :rel 1
   :abs 2 :absx 2 :absy 2 :ind 2 :indx 1 :indy 1})

(def mode-operand-kind
  "Syntactic shape of each addressing mode, shared by assembly and
  disassembly."
  {:imp :implied :acc :accumulator :imm :immediate
   :zp :zero-page :zpx :zero-page-x :zpy :zero-page-y
   :rel :relative :abs :absolute :absx :absolute-x :absy :absolute-y
   :ind :indirect :indx :indexed-indirect :indy :indirect-indexed})

(def mode-index-register
  "Index register a mode implicitly uses, or nil for unindexed modes."
  {:zpx :x :absx :x :indx :x
   :zpy :y :absy :y :indy :y})

(def memory-modes
  "Modes whose operand names a memory location."
  #{:zp :zpx :zpy :abs :absx :absy :ind :indx :indy})

(def zero-page-modes
  "Memory modes that encode a one-byte address."
  #{:zp :zpx :zpy :indx :indy})

(def relative-modes #{:rel})

(def ^:private opcode-rows
  ["BRK/imp ORA/indx KIL/imp SLO/indx NOP/zp ORA/zp ASL/zp SLO/zp PHP/imp ORA/imm ASL/acc ANC/imm NOP/abs ORA/abs ASL/abs SLO/abs"
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
   "BEQ/rel SBC/indy KIL/imp ISC/indy NOP/zpx SBC/zpx INC/zpx ISC/zpx SED/imp SBC/absy NOP/imp ISC/absy NOP/absx SBC/absx INC/absx ISC/absx"])

(def opcode-table
  "The 256 MOS 6510 opcodes in numeric order as `{:mnemonic ... :mode ...}`."
  (->> opcode-rows
       (mapcat #(str/split % #" "))
       (mapv (fn [token]
               (let [[mnemonic mode] (str/split token #"/")]
                 {:mnemonic mnemonic :mode (keyword mode)})))))

(def documented-opcodes
  "The legal NMOS 6502 opcode bytes. Everything else in `opcode-table` is an
  undocumented variant."
  #{0x00 0x01 0x05 0x06 0x08 0x09 0x0a 0x0d 0x0e
    0x10 0x11 0x15 0x16 0x18 0x19 0x1d 0x1e
    0x20 0x21 0x24 0x25 0x26 0x28 0x29 0x2a 0x2c 0x2d 0x2e
    0x30 0x31 0x35 0x36 0x38 0x39 0x3d 0x3e
    0x40 0x41 0x45 0x46 0x48 0x49 0x4a 0x4c 0x4d 0x4e
    0x50 0x51 0x55 0x56 0x58 0x59 0x5d 0x5e
    0x60 0x61 0x65 0x66 0x68 0x69 0x6a 0x6c 0x6d 0x6e
    0x70 0x71 0x75 0x76 0x78 0x79 0x7d 0x7e
    0x81 0x84 0x85 0x86 0x88 0x8a 0x8c 0x8d 0x8e
    0x90 0x91 0x94 0x95 0x96 0x98 0x99 0x9a 0x9d
    0xa0 0xa1 0xa2 0xa4 0xa5 0xa6 0xa8 0xa9 0xaa 0xac 0xad 0xae
    0xb0 0xb1 0xb4 0xb5 0xb6 0xb8 0xb9 0xba 0xbc 0xbd 0xbe
    0xc0 0xc1 0xc4 0xc5 0xc6 0xc8 0xc9 0xca 0xcc 0xcd 0xce
    0xd0 0xd1 0xd5 0xd6 0xd8 0xd9 0xdd 0xde
    0xe0 0xe1 0xe4 0xe5 0xe6 0xe8 0xe9 0xea 0xec 0xed 0xee
    0xf0 0xf1 0xf5 0xf6 0xf8 0xf9 0xfd 0xfe})

(def mnemonic-mode-opcodes
  "Ascending opcode bytes for every `[mnemonic mode]` pair, mnemonic as a
  string and mode as a keyword."
  (reduce (fn [acc [opcode entry]]
            (update acc [(:mnemonic entry) (:mode entry)]
                    (fnil conj []) opcode))
          {}
          (map-indexed vector opcode-table)))

(def documented-mnemonics
  "Mnemonics that have at least one documented opcode. Some of these (for
  example NOP) also have undocumented variants."
  (into #{}
        (keep (fn [[opcode entry]]
                (when (contains? documented-opcodes opcode)
                  (:mnemonic entry))))
        (map-indexed vector opcode-table)))

(defn documented-opcode?
  "True when `opcode` is a legal, documented NMOS 6502 opcode."
  [opcode]
  (contains? documented-opcodes (bit-and (int opcode) 0xff)))

(defn documented-mnemonic?
  "True when `mnemonic` has at least one documented opcode."
  [mnemonic]
  (contains? documented-mnemonics mnemonic))

(defn opcodes-for
  "Ascending opcode bytes for a mnemonic and addressing mode, or `[]`."
  [mnemonic mode]
  (get mnemonic-mode-opcodes [mnemonic mode] []))

(defn modes-for
  "Set of addressing modes a mnemonic supports."
  [mnemonic]
  (into #{}
        (keep (fn [[[m mode] _]] (when (= m mnemonic) mode)))
        mnemonic-mode-opcodes))

(defn preferred-opcode
  "Opcode for a mnemonic and mode, preferring the documented opcode and
  otherwise the lowest opcode."
  [mnemonic mode]
  (let [opcodes (opcodes-for mnemonic mode)
        documented (first (filter documented-opcode? opcodes))]
    (or documented (first opcodes))))

(defn valid-opcode?
  "True when `opcode` is a valid encoding of `mnemonic`."
  [mnemonic opcode]
  (let [opcode (bit-and (int opcode) 0xff)]
    (= mnemonic (:mnemonic (nth opcode-table opcode)))))

(def mnemonic-by-opcode
  "Per-opcode mnemonic, indexed by the raw opcode byte."
  (object-array (mapv :mnemonic opcode-table)))

(def mode-by-opcode
  "Per-opcode addressing mode, indexed by the raw opcode byte."
  (object-array (mapv :mode opcode-table)))

(def width-by-opcode
  "Per-opcode instruction width in bytes, indexed by the raw opcode byte."
  (int-array (mapv #(get mode-width (:mode %)) opcode-table)))

(defn decode
  "Decode the opcode of `bytes` at `pc` without needing the full instruction."
  [pc bytes]
  (let [opcode (bit-and (int (nth bytes 0 0)) 0xff)
        {:keys [mnemonic mode]} (nth opcode-table opcode)]
    {:pc pc
     :opcode opcode
     :bytes (vec bytes)
     :mnemonic mnemonic
     :mode mode
     :width (mode-width mode)}))

;; Instruction classes -------------------------------------------------------

(def store-mnemonics
  #{"STA" "STX" "STY" "SAX" "AHX" "SHX" "SHY" "TAS"})

(def rmw-mnemonics
  #{"ASL" "LSR" "ROL" "ROR" "INC" "DEC"
    "SLO" "RLA" "SRE" "RRA" "DCP" "ISC"})

(def branch-mnemonics
  #{"BPL" "BMI" "BVC" "BVS" "BCC" "BCS" "BNE" "BEQ"})

(defn branch?
  "True when `mnemonic` is a relative branch."
  [mnemonic]
  (contains? branch-mnemonics mnemonic))

(def pull-mnemonics #{"PLA" "PLP"})

(def read-mnemonics
  "Mnemonics whose effective address is read before any write."
  (into #{"LDA" "LDX" "LDY" "LAX" "LAS" "ORA" "AND" "EOR" "ADC" "SBC"
          "CMP" "CPX" "CPY" "BIT"}
        rmw-mnemonics))

(defn category
  "Classify an instruction by its effect on memory and control flow."
  [mnemonic]
  (cond
    (store-mnemonics mnemonic) :store
    (rmw-mnemonics mnemonic) :rmw
    (pull-mnemonics mnemonic) :pull
    (= mnemonic "PHA") :push
    (= mnemonic "PHP") :push
    (= mnemonic "JSR") :jsr
    (= mnemonic "BRK") :brk
    (= mnemonic "RTS") :rts
    (= mnemonic "RTI") :rti
    (= mnemonic "JMP") :jmp
    (branch-mnemonics mnemonic) :branch
    :else :other))

(defn reads-memory?
  [mnemonic]
  (contains? read-mnemonics mnemonic))

;; Flag and value rules ------------------------------------------------------

(defn- u8
  "Coerce an optional byte-ish value to an unsigned 0..255 integer."
  [x]
  (bit-and (int (or x 0)) 0xff))

(defn carry?
  "Carry flag from a native 6502 status byte."
  [event]
  (pos? (bit-and (u8 (:flags event)) 0x01)))

(defn sign?
  [event]
  (pos? (bit-and (u8 (:flags event)) 0x80)))

(defn overflow?
  [event]
  (pos? (bit-and (u8 (:flags event)) 0x40)))

(defn zero-flag?
  [event]
  (pos? (bit-and (u8 (:flags event)) 0x02)))

(defn store-value
  "The byte a store instruction writes to `address`.

  The unstable undocumented stores mask their value with the high byte of the
  effective address plus one, which is the value the reference 6510 places on
  the bus."
  [mnemonic event address]
  (let [a (u8 (:a event))
        x (u8 (:x event))
        y (u8 (:y event))
        high-plus-one (bit-and (inc (bit-shift-right (int address) 8)) 0xff)]
    (bit-and
     (case mnemonic
       "STA" a
       "STX" x
       "STY" y
       "SAX" (bit-and a x)
       "AHX" (bit-and a x high-plus-one)
       "SHX" (bit-and x high-plus-one)
       "SHY" (bit-and y high-plus-one)
       "TAS" (bit-and a x high-plus-one))
     0xff)))

(defn rmw-value
  "The final byte a read-modify-write instruction leaves at its destination."
  [mnemonic old-value carry]
  (let [old (bit-and (int old-value) 0xff)
        carry-bit (if carry 1 0)]
    (bit-and
     (case mnemonic
       "ASL" (bit-shift-left old 1)
       "LSR" (bit-shift-right old 1)
       "ROL" (bit-or (bit-shift-left old 1) carry-bit)
       "ROR" (bit-or (bit-shift-right old 1) (bit-shift-left carry-bit 7))
       "INC" (inc old)
       "DEC" (dec old)
       "SLO" (bit-shift-left old 1)
       "RLA" (bit-or (bit-shift-left old 1) carry-bit)
       "SRE" (bit-shift-right old 1)
       "RRA" (bit-or (bit-shift-right old 1) (bit-shift-left carry-bit 7))
       "DCP" (dec old)
       "ISC" (inc old))
     0xff)))

(defn branch-taken?
  [mnemonic event]
  (case mnemonic
    "BPL" (not (sign? event))
    "BMI" (sign? event)
    "BVC" (not (overflow? event))
    "BVS" (overflow? event)
    "BCC" (not (carry? event))
    "BCS" (carry? event)
    "BNE" (not (zero-flag? event))
    "BEQ" (zero-flag? event)))

(defn relative-target
  [pc byte]
  (let [offset (if (< byte 128) byte (- byte 256))]
    (bit-and (+ pc 2 offset) 0xffff)))

;; Operand resolution --------------------------------------------------------

(defn absolute-operand
  [bytes]
  (bit-or (bit-and (int (nth bytes 1 0)) 0xff)
          (bit-shift-left (bit-and (int (nth bytes 2 0)) 0xff) 8)))

(defn relative-operand
  [bytes]
  (let [byte (bit-and (int (nth bytes 1 0)) 0xff)]
    (if (< byte 128) byte (- byte 256))))

(defn- zero-page-word
  [read address]
  (bit-or (read address)
          (bit-shift-left (read (bit-and (inc address) 0xff)) 8)))

(defn resolve-address
  "Resolve the effective address of a memory-operand instruction.

  `read` is a CPU-visible byte reader `(fn [address] u8)` reflecting the state
  immediately before the instruction, which is required for zero-page indirect
  modes. Returns nil for modes which do not address memory."
  [read {:keys [mode bytes]} event]
  (let [operand (bit-and (int (nth bytes 1 0)) 0xff)
        word (absolute-operand bytes)
        x (u8 (:x event))
        y (u8 (:y event))]
    (case mode
      :zp operand
      :zpx (bit-and (+ operand x) 0xff)
      :zpy (bit-and (+ operand y) 0xff)
      :abs word
      :absx (bit-and (+ word x) 0xffff)
      :absy (bit-and (+ word y) 0xffff)
      :indx (zero-page-word read (bit-and (+ operand x) 0xff))
      :indy (bit-and (+ (zero-page-word read operand) y) 0xffff)
      nil)))

(defn jmp-indirect-target
  "Resolve a `JMP (addr)` target, including the NMOS page-wrap behaviour when
  the pointer's low byte is `$FF`."
  [read bytes]
  (let [word (absolute-operand bytes)
        low (read word)
        high (read (bit-or (bit-and word 0xff00)
                           (bit-and (inc word) 0xff)))]
    (bit-or low (bit-shift-left high 8))))

;; Stack and control-flow bookkeeping ---------------------------------------

(defn sp-after
  "The stack pointer after an instruction, excluding an unexpected hardware
  interrupt. `TXS` transfers X, so it needs the pre-instruction X value."
  [mnemonic sp x]
  (cond
    (= mnemonic "TXS") (u8 x)
    (contains? #{"PHA" "PHP"} mnemonic) (bit-and (dec sp) 0xff)
    (= mnemonic "JSR") (bit-and (- sp 2) 0xff)
    (= mnemonic "BRK") (bit-and (- sp 3) 0xff)
    (contains? pull-mnemonics mnemonic) (bit-and (inc sp) 0xff)
    (= mnemonic "RTS") (bit-and (+ sp 2) 0xff)
    (= mnemonic "RTI") (bit-and (+ sp 3) 0xff)
    :else sp))
