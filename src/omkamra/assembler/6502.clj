(ns omkamra.assembler.6502
  "MOS 6502/6510 assembler and disassembler target.

  The NMOS 6502 and its 6510 variant share one instruction set; the 6510 adds
  an on-chip I/O port but encodes instructions identically. This namespace
  therefore describes the whole family: all 256 opcode bytes, including the
  undocumented variants.

  Everything is driven by data: `opcode-table` is the single source of truth
  for mnemonic, addressing mode, instruction width and instruction-class
  semantics. `asm` and `define-asm` build deferred descriptors; the generic
  `omkamra.assembler` multimethods do the actual work."

  (:require
   [clojure.string :as str]
   [omkamra.assembler :as assembler]
   [omkamra.assembler.core :as core]))

;; Opcode vocabulary ---------------------------------------------------------

(declare target-descriptor)

(def mode-width
  {:imp 1 :acc 1 :imm 2 :zp 2 :zpx 2 :zpy 2 :rel 2
   :abs 3 :absx 3 :absy 3 :ind 3 :indx 2 :indy 2})

(def mode-operand-width
  "Number of operand bytes encoded after the opcode for each mode."
  {:imp 0 :acc 0 :imm 1 :zp 1 :zpx 1 :zpy 1 :rel 1
   :abs 2 :absx 2 :absy 2 :ind 2 :indx 1 :indy 1})

(def mode-operand-kind
  "Syntactic shape of each addressing mode, shared by assembly and disassembly."
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

;; Mnemonic parsing ----------------------------------------------------------

(def ^:private known-mnemonics
  (into #{} (map (comp first key)) mnemonic-mode-opcodes))

(defn parse-mnemonic
  "Parse a mnemonic symbol/string into `{:mnemonic \"LDA\" :forced-opcode nil}`.

  A dotted mnemonic such as `nop.3a` forces the exact opcode `$3A`."
  [x]
  (let [raw (if (symbol? x) (name x) (str x))
        dot (str/index-of raw ".")
        [base hex] (if dot
                     [(subs raw 0 dot) (subs raw (inc dot))]
                     [raw nil])]
    {:mnemonic (str/upper-case base)
     :dotted? (boolean dot)
     :forced-opcode (when hex
                      (try
                        (Integer/parseInt hex 16)
                        (catch NumberFormatException _ nil)))}))

(defn- valid-mnemonic? [{:keys [mnemonic dotted? forced-opcode]}]
  (and (contains? known-mnemonics mnemonic)
       (if dotted?
         (and forced-opcode (valid-opcode? mnemonic forced-opcode))
         true)))

(defn mnemonic-symbol?
  "True when `x` is a symbol naming a 6502 mnemonic."
  [x]
  (and (symbol? x) (valid-mnemonic? (parse-mnemonic x))))

;; Mode inference ------------------------------------------------------------

(defn- supported!
  [modes mode form]
  (when-not (contains? modes mode)
    (core/fail! :unsupported-addressing-mode
                (str "Addressing mode " (name mode) " is not supported here")
                {:mode mode :form form} form -1))
  mode)

(defn- infer-memory-mode
  [mnemonic modes contents form]
  (let [{:keys [force-zp? nested index value]}
        (core/memory-shape {:register-names #{"x" "y"}} contents)]
    (cond
      nested
      (let [{inner-index :index} (core/memory-shape {:register-names #{"x" "y"}}
                                                    nested)
            mode (cond
                   (and (= :x inner-index) (nil? index)) :indx
                   (and (nil? inner-index) (= :y index)) :indy)]
        (supported! modes (or mode :indx) form))

      index
      (let [zero-page? (or force-zp? (core/operand-small-address? value))
            mode (case [index zero-page?]
                   [:x true] :zpx
                   [:x false] :absx
                   [:y true] :zpy
                   [:y false] :absy)]
        (supported! modes mode form))

      :else
      (cond
        force-zp? (supported! modes :zp form)
        (and (core/operand-small-address? value) (contains? modes :zp))
        (supported! modes :zp form)
        (contains? modes :ind) (supported! modes :ind form)
        (contains? modes :abs) (supported! modes :abs form)
        (contains? modes :zp) (supported! modes :zp form)
        :else (core/fail! :unsupported-addressing-mode
                          (str mnemonic " does not support a memory operand")
                          {:mnemonic mnemonic :form form} form -1)))))

(defn strip-annotations
  "Remove transparent `%meta`/`%volatile` wrappers from an operand."
  [operand]
  (cond
    (and (seq? operand) (= '%volatile (first operand))) (recur (second operand))
    (and (seq? operand) (= '%meta (first operand))) (recur (nth operand 2 nil))
    :else operand))

(defn infer-mode
  "Infer the addressing mode for a mnemonic and normalized operand.

  `mnemonic` is a symbol or string, possibly dotted (`nop.3a`). Throws when
  the mnemonic does not support the shape of `operand`."
  [mnemonic operand]
  (let [operand (strip-annotations operand)
        {:keys [mnemonic forced-opcode]} (parse-mnemonic mnemonic)
        modes (modes-for mnemonic)]
    (cond
      forced-opcode (aget ^objects mode-by-opcode forced-opcode)

      (nil? operand)
      ;; An accumulator opcode also accepts the conventional omitted-A form;
      ;; the ISA table's :acc entry is the registration for that shorthand.
      (if (contains? modes :acc)
        (supported! modes :acc mnemonic)
        (supported! modes :imp mnemonic))

      (= operand 'a) (supported! modes :acc mnemonic)

      (vector? operand) (infer-memory-mode mnemonic modes operand mnemonic)

      :else
      (cond
        (contains? modes :imm) :imm
        (contains? modes :rel) :rel
        (contains? modes :abs) :abs
        :else (core/fail! :unsupported-addressing-mode
                          (str mnemonic " does not support an immediate operand")
                          {:mnemonic mnemonic} mnemonic -1)))))

;; Instruction items ---------------------------------------------------------

(defn instruction-item
  "Build the code-generation item for one normalized 6502 instruction."
  [target form index meta]
  (let [[head & operands] form
        operand (first operands)
        {:keys [mnemonic forced-opcode]} (parse-mnemonic head)
        mode (infer-mode head operand)
        opcode (or forced-opcode (preferred-opcode mnemonic mode))]
    (when (nil? opcode)
      (core/fail! :unsupported-instruction
                  (str "No opcode for " mnemonic " " (name mode))
                  {:mnemonic mnemonic :mode mode} form index))
    (let [encoded (core/encode-value target mode operand form index)
          [_ raw] (core/unwrap-volatile operand)
          operand-value (when (contains? memory-modes mode)
                          (if (vector? raw)
                            (core/operand-value target mode raw)
                            raw))
          operand-address (when (integer? operand-value)
                            (long operand-value))]
      {:type :instruction
       :mnemonic mnemonic
       :mode mode
       :opcode opcode
       :operand operand
       :operand-address operand-address
       :volatile? (:volatile? encoded)
       :bytes (into [opcode] (:bytes encoded))
       :size (inc (count (:bytes encoded)))
       :meta meta
       :relocations (when-let [relocation (:relocation encoded)]
                      [(assoc relocation :in-item 1)])})))

;; Disassembly ---------------------------------------------------------------

(defn- operand-of
  "The resolved operand of a decoded instruction."
  [mode pc bytes]
  (case mode
    (:imp) nil
    (:acc) 'a
    (:imm) (bit-and (int (nth bytes 1 0)) 0xff)
    (:zp :zpx :zpy :indx :indy) (bit-and (int (nth bytes 1 0)) 0xff)
    (:abs :absx :absy :ind) (absolute-operand bytes)
    (:rel) (relative-target pc (bit-and (int (nth bytes 1 0)) 0xff))))

(defn decode-instructions
  "Linear-sweep decode of `request`'s bytes.

  Returns one structured instruction per decoded opcode, in address order.
  A truncated instruction raises a structured error naming its address."
  [_target {:keys [address bytes]}]
  (let [address (long (or address 0))
        bytes (vec bytes)
        total (count bytes)]
    (loop [offset 0
           out []]
      (if (>= offset total)
        out
        (let [opcode (bit-and (int (nth bytes offset)) 0xff)
              width (long (aget ^ints width-by-opcode opcode))
              pc (bit-and (+ address offset) 0xffff)]
          (when (> (+ offset width) total)
            (throw (ex-info (str "Truncated instruction at " (format "%04X" pc))
                            {:reason :truncated-instruction
                             :address pc
                             :opcode opcode
                             :available (- total offset)
                             :required width})))
          (let [instruction-bytes (subvec bytes offset (+ offset width))
                {:keys [mnemonic mode]} (nth opcode-table opcode)]
            (recur (+ offset width)
                   (conj out {:pc pc
                              :opcode opcode
                              :bytes instruction-bytes
                              :mnemonic mnemonic
                              :mode mode
                              :operand-kind (mode-operand-kind mode)
                              :operand (operand-of mode pc instruction-bytes)
                              :width width}))))))))

;; Human-oriented rendering --------------------------------------------------

(def mode-operand-text-width
  {:imm 1 :zp 1 :zpx 1 :zpy 1 :indx 1 :indy 1
   :abs 2 :absx 2 :absy 2 :ind 2 :rel 2})

(defn- asm-operand
  [{:keys [mode operand volatile?]} {:keys [symbols]}]
  (let [width (case mode
                (:imm :zp :zpx :zpy :indx :indy) 1
                2)
        [_ raw] (core/unwrap-volatile operand)
        value (if (and (contains? memory-modes mode) (vector? raw))
                (core/operand-value target-descriptor mode raw)
                raw)
        text (if volatile?
               "??"
               (core/value-text value width (if (= mode :imm) {} symbols)))]
    (case mode
      :imp ""
      :acc "A"
      :imm (str "#" text)
      :rel text
      :zp text
      :zpx (str text ",X")
      :zpy (str text ",Y")
      :abs text
      :absx (str text ",X")
      :absy (str text ",Y")
      :ind (str "(" text ")")
      :indx (str "(" text ",X)")
      :indy (str "(" text "),Y"))))

(defn- asm-mnemonic
  [{:keys [mnemonic mode opcode]}]
  (if (= opcode (preferred-opcode mnemonic mode))
    mnemonic
    (str mnemonic "." (format "%02x" (bit-and (long opcode) 0xff)))))

(defn- format-meta
  "Render an item's metadata as a comment, preferring human fields."
  [meta]
  (let [remark (:remark meta)
        note (:note meta)]
    (cond
      (and remark note) (str remark "  " note)
      remark remark
      note note
      :else (pr-str meta))))

(defn- item-label
  "The label operand of an instruction, if it has one."
  [{:keys [mode operand]}]
  (let [[_ raw] (core/unwrap-volatile operand)]
    (cond
      (and (contains? memory-modes mode) (vector? raw))
      (some (fn [element] (when (keyword? element) element)) raw)

      (keyword? raw) raw
      :else nil)))

(defn- item-body
  [item options]
  (case (:type item)
    :instruction
    (let [operand (asm-operand item options)
          mnemonic (asm-mnemonic item)]
      (if (str/blank? operand) mnemonic (str mnemonic " " operand)))

    :padding
    (str ";; align " (:align item) " (" (:size item) " bytes of padding)")

    :data
    (let [directive (case (long (:width item 1))
                      1 ".byte"
                      2 ".word"
                      4 ".dword"
                      8 ".qword"
                      (str ".bytes" (long (:width item 1))))]
      (str directive " "
           (str/join ", "
                     (map (fn [b] (format "$%02X" (bit-and (long b) 0xff)))
                          (:bytes item)))))

    (pr-str (:type item))))

(defn- render-asm-item
  [address item {:keys [comments label-addresses] :as options}]
  (let [prefix (format "%04X  " (bit-and (long address) 0xffff))
        meta-comment (when (seq (:meta item)) (format-meta (:meta item)))
        operand-address (or (:operand-address item)
                            (when-let [label (item-label item)]
                              (get label-addresses label)))
        operand-comment (when operand-address
                          (get comments (long operand-address)))
        comment (cond
                  (and meta-comment operand-comment)
                  (str meta-comment "  " operand-comment)

                  meta-comment meta-comment
                  operand-comment operand-comment)]
    (str prefix (item-body item options) (when comment (str "  ; " comment)))))

(defn- render-section
  [section options]
  (let [base (:base section)
        labels (reduce-kv (fn [m label offset]
                            (update m offset (fnil conj []) (name label)))
                          {}
                          (:labels section))
        emit-labels (fn [offset]
                      (mapv (fn [label] (str label ":")) (get labels offset)))]
    (loop [items (:items section)
           lines []]
      (if-let [item (first items)]
        (let [offset (:offset item)
              address (if (integer? base) (+ (long base) (long offset)) offset)]
          (recur (rest items)
                 (into lines
                       (concat (emit-labels offset)
                               [(render-asm-item address item options)]))))
        (into lines (emit-labels (:size section)))))))

(defn serialize-asm
  "Render an object, graph, or descriptor as conventional 6502 assembly.

  There is no round-trip guarantee. Volatile operands render as `??` and
  metadata renders as comments.

  Options:

  - `:symbols` — a map from an absolute address to a name (for example the
    KERNAL jump table); memory operands render the name;
  - `:comments` — a map from an absolute address to a trailing comment."
  ([x]
   (serialize-asm x {}))
  ([x options]
   (let [object (cond
                  (core/object? x) x
                  (core/object-graph? x) (assembler/object-graph->object x)
                  :else (assembler/object-graph->object (assembler/assemble x)))
         label-addresses (into {}
                               (for [section (:sections object)
                                     :when (integer? (:base section))
                                     [label offset] (:labels section)]
                                 [label (+ (long (:base section)) (long offset))]))
         options {:symbols (or (:symbols options) (:symbols object) {})
                  :comments (or (:comments options) (:comments object) {})
                  :label-addresses label-addresses}
         section-lines (fn [section]
                         (let [base (:base section)]
                           (concat
                            (when (integer? base)
                              [(format "        .org $%04X"
                                       (bit-and (long base) 0xffff))])
                            (when-not (integer? base)
                              ["; unplaced object section"])
                            (render-section section options))))
         blocks (mapv section-lines (:sections object))
         lines (mapcat identity (interpose [""] blocks))]
     (str (str/join "\n"
                    (concat [(str "; " (or (:comment object)
                                           (:object-id object)
                                           "anonymous object"))]
                            lines))
          "\n"))))

;; Target descriptor ---------------------------------------------------------

(def target-descriptor
  "The 6502/6510 target descriptor."
  {:isa :6502
   :address-max 0xffff
   :byte-order :little
   :register-names #{"x" "y"}
   :memory-modes memory-modes
   :mode-operand-width mode-operand-width
   :mnemonic-symbol? mnemonic-symbol?
   :infer-mode infer-mode
   :instruction-item instruction-item
   :decode-instructions decode-instructions
   :serialize-asm serialize-asm})

(defmethod assembler/target :6502
  [_]
  target-descriptor)

;; Definition API ------------------------------------------------------------

(defn asm*
  "Build a deferred 6502 assembly descriptor at run time.

  Options: `:namespace` (the namespace used to resolve symbols and evaluate
  expressions), `:object-id` (a qualified identity), `:entry` (an entry label
  overriding `%entry`), and `:objects`/`:assets` for explicit injection."
  ([source]
   (asm* source {}))
  ([source options]
   (assembler/asm-descriptor :6502 source options)))

(defmacro asm
  "Build a deferred 6502 assembly descriptor from `body`.

  Nothing is assembled when the macro is evaluated: the descriptor records the
  source and the calling namespace, and the generic `assembler/assemble` turns
  it into a relocatable object graph later."
  [& body]
  `(asm* '~body {:namespace '~(ns-name *ns*)}))

(defmacro define-asm
  "Bind a deferred 6502 assembly descriptor to `name`.

  A leading string in `body` becomes the Var's docstring. The descriptor's
  `:object-id` is the Var's fully qualified name, which is also what other
  objects use to reference its entry point."
  [name & body]
  (let [ns-name (str *ns*)
        object-id (keyword ns-name (str name))
        docstring (when (string? (first body)) (first body))
        body (if docstring (next body) body)
        name (if docstring
               (with-meta name (assoc (meta name) :doc docstring))
               name)]
    `(def ~name
       (asm* '~body {:namespace '~(symbol ns-name)
                     :object-id ~object-id}))))

(defn disassemble
  "Decode `{:address n :bytes [...]}` into structured 6502 instructions."
  ([request]
   (assembler/disassemble (assoc request :isa :6502)))
  ([address bytes]
   (disassemble {:address address :bytes bytes})))
