(ns omkamra.vice.disassembly
  "Structural disassembly of a `:dedupe` image along `:segments` boundaries.

  This is the last stage of the v2 pipeline. It consumes exactly two
  committed images — the structural execution image (`:dedupe`) and the
  semantic segmentation (`:segments`) — and never touches the raw capture.

  For each semantic segment it selects the globally deduplicated basic blocks
  whose occurrences *begin* inside the segment (so a block whose execution
  straddles a boundary belongs to the segment it started in, and neighbouring
  segments never share a block in the same output), and renders each selected
  block exactly once per file.

  Each segment becomes one anonymous normalized assembly object. Every
  discovered block variant is emitted as a fixed-address `%org` fragment,
  including variants that overlap at the same address; labels are synthesized
  for block starts and direct control-flow targets; volatile operands are
  wrapped in `(%volatile ...)`; block/segment/role annotation is attached with
  `(%meta ...)`. The shared `assembler/serialize-clj` and
  `assembler/serialize-asm` functions render the object.

  Files:

  ```text
  analysis/disassembly/<generation>/
    index.edn
    segments/segment-000.clj   ; canonical normalized object
    segments/segment-000.asm   ; human-readable listing
    roles/<role>.asm           ; derived analysis view, one per role
  ```

  No file contains a basic block twice. A disassembly object that contains
  overlapping fragments remains valid for analysis and serialization but the
  linker rejects it as a runnable artifact until the overlap is resolved."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.isa :as isa]
            [omkamra.vice.parallel :as parallel]
            [omkamra.vice.store :as store]))

(def disassembly-version 1)
(def disassembly-format (contract/format-keyword :disassembly disassembly-version))

(def default-configuration
  {:roles [:raster :music :loader :decruncher]})

;; Address-space visibility --------------------------------------------------

(defn- io-visible? [port]
  (and (pos? (bit-and (long port) 0x03)) (pos? (bit-and (long port) 0x04))))

(defn- basic-visible? [port]
  (pos? (bit-and (long port) 0x01)))

(defn- kernal-visible? [port]
  (pos? (bit-and (long port) 0x02)))

(defn- mapping
  "Address space a `pc` maps to for a CPU-port `$01` value."
  [pc port]
  (cond
    (and (<= 0xd000 (long pc) 0xdfff) (io-visible? port)) :io
    (and (<= 0xa000 (long pc) 0xbfff) (basic-visible? port)) :basic-rom
    (and (<= 0xe000 (long pc) 0xffff) (kernal-visible? port)) :kernal-rom
    :else :ram))

(def ^:private mapped-rom? #{:basic-rom :kernal-rom})

;; Segment selection and statistics ------------------------------------------

(defn- pack-key
  "Pack `[context-id block-id]` into one non-negative long key.

  Block ids and context ids are small (thousands and tens), so 20 and 12 bits
  suffice, avoiding a vector allocation per occurrence."
  ^long [^long context-id ^long block-id]
  (bit-or (bit-shift-left context-id 20) (bit-and block-id 0xfffff)))

(defn- unpack-key [^long key]
  [(bit-shift-right key 20) (bit-and key 0xfffff)])

(defn- context-label
  [trace context-id]
  (let [kind (get (:context-kinds trace) context-id :irq)
        handler (when (not= :main kind)
                  (long (or (:handler-pc (dedupe/read-context trace context-id)) 0)))]
    (case kind
      :main "m"
      :irq (str "i" (format "%x" handler))
      :nmi (str "n" (format "%x" handler))
      :brk (str "b" (format "%x" handler)))))

(defn- segment-stats
  "Segment- and context-scoped statistics, in one streaming pass.

  Returns `:executions`, `:first-index`, `:predecessors`, `:successors`, and
  `:backedges`, keyed by `pack-key`. An occurrence belongs to the segment that
  contains its first fragment, and an edge is recorded only between two
  occurrences assigned to the same segment and frame, so neighbouring segments
  never share a block or an edge. `last-block` is per frame within this segment,
  matching the dedupe edge semantics (consecutive closed occurrences in a
  frame)."
  [trace segment]
  (let [[start end] (:event-range segment)
        start (long start)
        end (long end)
        executions (volatile! (transient {}))
        first-index (volatile! (transient {}))
        predecessors (volatile! (transient {}))
        successors (volatile! (transient {}))
        backedges (volatile! (transient {}))
        last-block (volatile! (transient {}))]
    (dedupe/scan-occurrences
     trace [start end]
     (fn [acc ^longs buffer]
       (let [index (aget buffer dedupe/oc-start)]
         (when (and (>= index start) (< index end))
           (let [frame (aget buffer dedupe/oc-frame)
                 context (aget buffer dedupe/oc-context)
                 block (aget buffer dedupe/oc-block)
                 key (pack-key context block)
                 from (get @last-block frame)]
             (vswap! executions assoc! key (inc (long (get @executions key 0))))
             (let [previous (get @first-index key)]
               (when (or (nil? previous) (< index (long previous)))
                 (vswap! first-index assoc! key index)))
             (when (some? from)
               (let [from (long from)
                     from-key (pack-key context from)]
                 (vswap! predecessors assoc! key (conj (get @predecessors key #{}) from))
                 (vswap! successors assoc! from-key (conj (get @successors from-key #{}) block))
                 (when (<= (long (get @first-index key Long/MAX_VALUE))
                           (long (get @first-index from-key Long/MAX_VALUE)))
                   (vswap! backedges assoc! key (inc (long (get @backedges key 0)))))))
             (vswap! last-block assoc! frame block))))
       acc)
     nil)
    {:executions (persistent! @executions)
     :first-index (persistent! @first-index)
     :predecessors (persistent! @predecessors)
     :successors (persistent! @successors)
     :backedges (persistent! @backedges)}))

(defn- primary-context
  "Context a block renders under: the one with the most occurrences, ties by
  the smallest context id, so a shared block still renders once per file."
  [stats block-id]
  (->> (:executions stats)
       (keep (fn [[key count]]
               (let [[context-id occurrence-block-id] (unpack-key (long key))]
                 (when (= block-id occurrence-block-id) [context-id (long count)]))))
       (sort-by (fn [[context-id count]] [(- count) (long context-id)]))
       ffirst))

(defn- saturate [value] (min 99 (long value)))

(defn- block-record
  [trace stats block-id]
  (let [context-id (primary-context stats block-id)
        key (pack-key context-id block-id)
        block (dedupe/read-block trace block-id)
        port (dedupe/port-at trace (long (get (:first-index stats) key 0)))
        pc (long (:pc (first (:instructions block))))
        annotation {:block block-id
                    :context (context-label trace context-id)
                    :predecessors (saturate (count (get (:predecessors stats) key #{})))
                    :successors (saturate (count (get (:successors stats) key #{})))
                    :executions (get (:executions stats) key 0)
                    :repeats (get (:backedges stats) key 0)}]
    {:block-id block-id
     :context-id context-id
     :pc pc
     :mapping (mapping pc port)
     :mapped-rom? (mapped-rom? (mapping pc port))
     :annotation annotation
     :true-predecessors (count (get (:predecessors stats) key #{}))
     :true-successors (count (get (:successors stats) key #{}))
     :instructions (:instructions block)}))

;; Well-known addresses -------------------------------------------------------

(def ^:private c64-comments
  "Hardware and KERNAL remarks, keyed by the accessed address."
  {0x0001 "CPU port: 6510 memory mapping"
   0x0314 "IRQ vector (lo)"
   0x0315 "IRQ vector (hi)"
   0x028d "shift-key flags"
   0x028e "shift-key flags mirror"
   0x00c5 "keyboard buffer length"
   0x00c6 "keys waiting in the KERNAL buffer"
   0x00cb "KERNAL screen-editor pointer"
   0xd000 "VIC sprite 0 X"
   0xd001 "VIC sprite 0 Y"
   0xd002 "VIC sprite 1 X"
   0xd003 "VIC sprite 1 Y"
   0xd004 "VIC sprite 2 X"
   0xd005 "VIC sprite 2 Y"
   0xd006 "VIC sprite 3 X"
   0xd007 "VIC sprite 3 Y"
   0xd008 "VIC sprite 4 X"
   0xd009 "VIC sprite 4 Y"
   0xd00a "VIC sprite 5 X"
   0xd00b "VIC sprite 5 Y"
   0xd00c "VIC sprite 6 X"
   0xd00d "VIC sprite 6 Y"
   0xd00e "VIC sprite 7 X"
   0xd00f "VIC sprite 7 Y"
   0xd010 "VIC sprite X MSB"
   0xd011 "VIC control 1 (raster/MSB/rows/scroll)"
   0xd012 "VIC raster line"
   0xd013 "VIC light pen X"
   0xd014 "VIC light pen Y"
   0xd015 "VIC sprite enable"
   0xd016 "VIC control 2 (reset/scroll/columns)"
   0xd017 "VIC sprite Y expand"
   0xd018 "VIC memory pointers (screen/charset)"
   0xd019 "VIC IRQ flag (write to acknowledge)"
   0xd01a "VIC IRQ mask"
   0xd01b "VIC sprite/background priority"
   0xd01c "VIC sprite multicolour enable"
   0xd01d "VIC sprite X expand"
   0xd01e "VIC sprite-sprite collision"
   0xd01f "VIC sprite-background collision"
   0xd020 "VIC border colour"
   0xd021 "VIC background colour 0"
   0xd022 "VIC background colour 1"
   0xd023 "VIC background colour 2"
   0xd024 "VIC background colour 3"
   0xd025 "VIC sprite multicolour 0"
   0xd026 "VIC sprite multicolour 1"
   0xd027 "VIC sprite 0 colour"
   0xd028 "VIC sprite 1 colour"
   0xd029 "VIC sprite 2 colour"
   0xd02a "VIC sprite 3 colour"
   0xd02b "VIC sprite 4 colour"
   0xd02c "VIC sprite 5 colour"
   0xd02d "VIC sprite 6 colour"
   0xd02e "VIC sprite 7 colour"
   0xd400 "SID voice 1 frequency lo"
   0xd401 "SID voice 1 frequency hi"
   0xd402 "SID voice 1 pulse width lo"
   0xd403 "SID voice 1 pulse width hi"
   0xd404 "SID voice 1 control"
   0xd405 "SID voice 1 attack/decay"
   0xd406 "SID voice 1 sustain/release"
   0xd407 "SID voice 2 frequency lo"
   0xd408 "SID voice 2 frequency hi"
   0xd409 "SID voice 2 pulse width lo"
   0xd40a "SID voice 2 pulse width hi"
   0xd40b "SID voice 2 control"
   0xd40c "SID voice 2 attack/decay"
   0xd40d "SID voice 2 sustain/release"
   0xd40e "SID voice 3 frequency lo"
   0xd40f "SID voice 3 frequency hi"
   0xd410 "SID voice 3 pulse width lo"
   0xd411 "SID voice 3 pulse width hi"
   0xd412 "SID voice 3 control"
   0xd413 "SID voice 3 attack/decay"
   0xd414 "SID voice 3 sustain/release"
   0xd415 "SID filter cutoff lo"
   0xd416 "SID filter cutoff hi"
   0xd417 "SID filter control"
   0xd418 "SID volume / filter mode"
   0xdc00 "CIA1 port A (keyboard columns)"
   0xdc01 "CIA1 port B (keyboard rows/joystick)"
   0xdc0d "CIA1 interrupt control"
   0xdc0e "CIA1 control A"
   0xdc0f "CIA1 control B"
   0xdd00 "CIA2 port A (VIC bank/serial)"
   0xdd01 "CIA2 port B"
   0xdd0d "CIA2 interrupt control"
   0xff48 "KERNAL hardware IRQ entry"
   0xff81 "KERNAL CINT"
   0xff84 "KERNAL IOINIT"
   0xff87 "KERNAL RAMTAS"
   0xff8a "KERNAL RESTOR"
   0xff8d "KERNAL VECTOR"
   0xff90 "KERNAL SETMSG"
   0xff93 "KERNAL SECOND"
   0xff96 "KERNAL TKSA"
   0xff99 "KERNAL MEMTOP"
   0xff9c "KERNAL MEMBOT"
   0xff9f "KERNAL SCNKEY"
   0xffa2 "KERNAL SETTMO"
   0xffa5 "KERNAL ACPTR"
   0xffa8 "KERNAL CIOUT"
   0xffab "KERNAL UNTLK"
   0xffae "KERNAL UNLSN"
   0xffb1 "KERNAL LISTEN"
   0xffb4 "KERNAL TALK"
   0xffb7 "KERNAL READST"
   0xffba "KERNAL SETLFS"
   0xffbd "KERNAL SETNAM"
   0xffc0 "KERNAL OPEN"
   0xffc3 "KERNAL CLOSE"
   0xffc6 "KERNAL CHKIN"
   0xffc9 "KERNAL CHKOUT"
   0xffcc "KERNAL CLRCHN"
   0xffcf "KERNAL CHRIN"
   0xffd2 "KERNAL CHROUT"
   0xffd5 "KERNAL LOAD"
   0xffd8 "KERNAL SAVE"
   0xffdb "KERNAL SETTIM"
   0xffde "KERNAL RDTIM"
   0xffe1 "KERNAL STOP"
   0xffe4 "KERNAL GETIN"
   0xffe7 "KERNAL CLALL"
   0xffea "KERNAL UDTIM"
   0xffed "KERNAL SCREEN"
   0xfff0 "KERNAL PLOT"
   0xfff3 "KERNAL IOBASE"
   0xea31 "KERNAL IRQ service routine"
   0xea81 "KERNAL IRQ exit"})

;; Normalized form generation ------------------------------------------------

(defn- synthesize-label
  [address]
  (keyword (format "L%04X" (bit-and (long address) 0xffff))))

(defn- relative-displacement
  "The signed displacement encoded in a relative instruction's template.

  `:template` holds the operand bytes only (no opcode), and for a relative
  branch the displacement is `template[0]`; `isa/relative-operand` expects the
  displacement at index 1, so it is padded here."
  [template]
  (isa/relative-operand [0 (nth template 0 0)]))

(defn- instruction-target
  "The direct control-flow target of an instruction, or nil."
  [{:keys [pc opcode template]}]
  (let [mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        lo (bit-and (long (nth template 0 0)) 0xff)
        hi (bit-and (long (nth template 1 0)) 0xff)
        word (bit-or lo (bit-shift-left hi 8))]
    (case mode
      :rel (bit-and (+ (long pc) 2 (relative-displacement template)) 0xffff)
      :abs (when (contains? #{"JMP" "JSR"} mnemonic) word)
      nil)))

(defn- instruction-access-address
  "The address an instruction reads/writes, for hardware remarks (nil for
  immediates and implied/accumulator forms)."
  [{:keys [pc opcode template]}]
  (let [mode (aget ^objects isa/mode-by-opcode opcode)
        lo (bit-and (long (nth template 0 0)) 0xff)
        hi (bit-and (long (nth template 1 0)) 0xff)
        word (bit-or lo (bit-shift-left hi 8))]
    (case mode
      (:zp :zpx :zpy :indx :indy) lo
      (:abs :absx :absy :ind) word
      :rel (bit-and (+ (long pc) 2 (relative-displacement template)) 0xffff)
      nil)))

(defn- label-map-for
  "Map every address that needs a synthesized label to its keyword.

  Only addresses that are actually reached by an instruction are labelled, so
  a computed target in the middle of an encoding cannot produce an unresolved
  reference."
  [blocks]
  (let [instruction-pcs (into #{}
                              (comp (mapcat :instructions)
                                    (map (comp long :pc)))
                              blocks)
        starts (into #{} (map (comp long :pc)) blocks)
        targets (into #{}
                      (comp (mapcat (fn [block]
                                      (keep instruction-target (:instructions block))))
                            (map long))
                      blocks)
        addresses (into starts (filter instruction-pcs) targets)]
    (into {}
          (map (fn [address] [address (synthesize-label address)]))
          (sort addresses))))

(defn- memory-form
  [mode value]
  (case mode
    :zp ['%zp value]
    :zpx ['%zp value 'x]
    :zpy ['%zp value 'y]
    :abs [value]
    :absx [value 'x]
    :absy [value 'y]
    :ind [value]
    :indx [[value 'x]]
    :indy [[value] 'y]))

(defn- instruction-form
  [instruction label-map]
  (let [{:keys [pc opcode template volatile-mask]} instruction
        mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        preferred (isa/preferred-opcode mnemonic mode)
        base-sym (symbol (str/lower-case mnemonic))
        lo (bit-and (long (nth template 0 0)) 0xff)
        hi (bit-and (long (nth template 1 0)) 0xff)
        word (bit-or lo (bit-shift-left hi 8))
        volatile? (pos? (bit-and (long volatile-mask) 0x03))
        value-form (or (get label-map word) word)
        wrap (fn [operand] (if volatile? (list '%volatile operand) operand))
        operand-form
        (case mode
          :imp nil
          :acc 'a
          :imm (wrap lo)
          :rel (let [target (bit-and (+ (long pc) 2 (relative-displacement template))
                                     0xffff)]
                 (wrap (or (get label-map target) target)))
          :abs (if (= "JMP" mnemonic)
                 ;; A bracketed operand means indirect for JMP, so a direct
                 ;; jump must use a bare value.
                 (wrap value-form)
                 (wrap (memory-form mode value-form)))
          (:zp :zpx :zpy :absx :absy :ind :indx :indy)
          (wrap (memory-form mode value-form))
          nil)
        ;; The operand's syntactic shape must infer back to the same mode when
        ;; the form is re-assembled (a small absolute address would otherwise
        ;; become zero page); a dotted mnemonic pins the exact opcode and mode.
        inferred (try
                   (assembler/infer-mode base-sym operand-form)
                   (catch Throwable _ mode))
        exact? (and (= opcode preferred) (= inferred mode))
        mnemonic-sym (if exact?
                       base-sym
                       (symbol (str (str/lower-case mnemonic) "."
                                    (format "%02x" (bit-and (long opcode) 0xff)))))]
    (if (nil? operand-form)
      (list mnemonic-sym)
      (list mnemonic-sym operand-form))))

(defn- instruction-size
  [instruction]
  (long (:width instruction)))

(defn- block-end
  [block]
  (+ (long (:pc block))
     (reduce + 0 (map instruction-size (:instructions block)))))

(defn- merge-runs
  "Group blocks into address-ordered runs, one `%org` fragment each.

  Blocks are sorted by address. A block joins the current run when it starts
  exactly where the run ends and shares its context, so fall-through code
  reads linearly. Overlapping or gapped blocks start a new run, keeping every
  discovered variant as its own fixed-address fragment."
  [blocks]
  (reduce (fn [runs block]
            (let [run (peek runs)]
              (if (and run
                       (= (long (:pc block)) (long (:end run)))
                       (= (:context-id block) (:context-id run)))
                (conj (pop runs)
                      (-> run
                          (update :blocks conj block)
                          (assoc :end (block-end block))))
                (conj runs {:pc (long (:pc block))
                            :end (block-end block)
                            :context-id (:context-id block)
                            :blocks [block]}))))
          []
          (sort-by (juxt :pc :block-id) blocks)))

(defn- annotation-note
  "A compact, human-readable form of a block's control-flow annotation."
  [annotation]
  (str "blk " (:block annotation)
       " ctx " (:context annotation)
       " pred " (:predecessors annotation)
       " succ " (:successors annotation)
       " exec " (:executions annotation)
       " rep " (:repeats annotation)))

(defn- run-entries
  "The instructions of a run, each carrying the annotation to emit before it."
  [run extra-meta]
  (mapcat (fn [block]
            (map-indexed (fn [index instruction]
                           {:instruction instruction
                            :annotation (when (zero? index)
                                          (let [annotation (merge (:annotation block)
                                                                  extra-meta)]
                                            (assoc annotation
                                                   :note (annotation-note annotation))))})
                         (:instructions block)))
          (:blocks run)))

(defn- run-forms
  "Forms for one merged run as a `%org` fragment, declaring new labels.

  Returns `[forms form-addresses declared]`; `form-addresses` is parallel to
  `forms` and carries the instruction address of each form (nil for labels
  and `%org` directives)."
  [run extra-meta label-map declared]
  (loop [entries (run-entries run extra-meta)
         declared declared
         forms [(list '%org (:pc run))]
         addresses [nil]
         operands [nil]]
    (if-let [{:keys [instruction annotation]} (first entries)]
      (let [address (long (:pc instruction))
            label (get label-map address)
            declare? (and label (not (contains? declared label)))
            form (instruction-form instruction label-map)
            form (if annotation (list '%meta annotation form) form)]
        (recur (rest entries)
               (if declare? (conj declared label) declared)
               (cond-> forms
                 declare? (conj label)
                 true (conj form))
               (cond-> addresses
                 declare? (conj nil)
                 true (conj address))
               (cond-> operands
                 declare? (conj nil)
                 true (conj (instruction-access-address instruction)))))
      [forms addresses operands declared])))

(defn- blocks->forms
  [blocks extra-meta]
  (let [label-map (label-map-for blocks)]
    (loop [runs (merge-runs blocks)
           declared #{}
           forms []
           addresses []
           operands []]
      (if-let [run (first runs)]
        (let [[run-forms run-addresses run-operands declared]
              (run-forms run extra-meta label-map declared)]
          (recur (rest runs)
                 declared
                 (into forms run-forms)
                 (into addresses run-addresses)
                 (into operands run-operands)))
        {:forms forms
         :form-addresses addresses
         :form-operands operands
         :labels (vec (vals label-map))}))))

;; Enrichment ----------------------------------------------------------------

(defn- referenced-addresses
  [object]
  (into (into #{} (keep identity (:form-operands object)))
        (comp (mapcat :items)
              (keep :operand-address)
              (map long))
        (:sections object)))

(defn enrich
  "Return `object` enriched with C64 symbol names and hardware remarks.

  Scans every instruction operand and attaches `:symbols` (KERNAL jump-table
  names), `:comments` (VIC/SID/CIA/KERNAL remarks keyed by address), and
  `:form-comments` (one remark per canonical source form). Serialization is
  then a pure projection of the object."
  [object]
  (let [addresses (referenced-addresses object)
        comments (select-keys c64-comments addresses)
        form-comments (mapv (fn [address]
                              (when address (get comments (long address))))
                            (:form-operands object))]
    (assoc object
           :comments comments
           :form-comments form-comments)))

(defn- build-object
  [blocks extra-meta header]
  (let [{:keys [forms form-addresses form-operands]} (blocks->forms blocks extra-meta)]
    (-> (assembler/assemble forms {})
        (assoc :form-addresses form-addresses
               :form-operands form-operands
               :comment header)
        (enrich))))

;; Rendering -----------------------------------------------------------------

(defn- render-instructions
  [blocks extra-meta header]
  (let [object (build-object blocks extra-meta header)]
    {:object object
     :clj-text (assembler/serialize-clj object)
     :asm-text (assembler/serialize-asm object)}))

(defn- render-segment
  [trace segment]
  (let [stats (segment-stats trace segment)
        block-ids (->> (:executions stats) keys
                       (map (fn [key] (second (unpack-key (long key)))))
                       distinct sort vec)
        blocks (mapv #(block-record trace stats %) block-ids)
        [start end] (:event-range segment)
        header (str "segment " (:segment-id segment)
                    "  kind=" (name (:kind segment))
                    (when (seq (:roles segment))
                      (str "  roles="
                           (str/join "," (sort (map name (:roles segment))))))
                    (when (seq (:handler-contexts segment))
                      (str "  handlers="
                           (str/join "," (sort (:handler-contexts segment))))))
        rendered (render-instructions blocks nil header)]
    {:segment-id (:segment-id segment)
     :kind (:kind segment)
     :roles (vec (sort (:roles segment)))
     :handler-contexts (vec (sort (:handler-contexts segment)))
     :event-range [start end]
     :blocks blocks
     :block-ids block-ids
     :object (:object rendered)
     :clj-text (:clj-text rendered)
     :asm-text (:asm-text rendered)}))

(defn- role-blocks
  "Blocks contributing to `role`, deduplicated by block id across all segments
  carrying that role (a block is never rendered twice in one file)."
  [rendered role]
  (->> rendered
       (filter #(contains? (set (:roles %)) role))
       (mapcat :blocks)
       (reduce (fn [acc block]
                 (update acc (:block-id block)
                         (fn [existing]
                           (if (and existing
                                    (>= (long (:executions (:annotation existing)))
                                        (long (:executions (:annotation block)))))
                             existing
                             block))))
               {})
       vals
       (sort-by :block-id)
       vec))

(defn- render-role
  [rendered role]
  (let [blocks (role-blocks rendered role)
        rendered (render-instructions blocks nil (str "role " (name role)))]
    {:role role
     :count (count blocks)
     :asm-text (:asm-text rendered)}))

;; Publication ---------------------------------------------------------------

(defn- write-text!
  [file text]
  (store/write-artifact!
   file
   (fn [^java.io.OutputStream out]
     (.write out (.getBytes ^String text "UTF-8")))))

(defn- segment-file-specs
  [directory rendered]
  (mapcat
   (fn [entry]
     (let [id (:segment-id entry)
           stem (format "segments/segment-%03d" id)
           clj-file (str stem ".clj")
           asm-file (str stem ".asm")
           clj-spec (write-text! (io/file directory clj-file) (:clj-text entry))
           asm-spec (write-text! (io/file directory asm-file) (:asm-text entry))]
       [(assoc clj-spec
               :kind :segment :file clj-file :segment-id id :format :clj)
        (assoc asm-spec
               :kind :segment :file asm-file :segment-id id :format :asm)]))
   rendered))

(defn- role-file-specs
  [directory role-renders]
  (map (fn [{:keys [role asm-text]}]
         (let [relative-file (str "roles/" (name role) ".asm")
               spec (write-text! (io/file directory relative-file) asm-text)]
           (assoc spec :kind :role :file relative-file :role role)))
       role-renders))

(defn build!
  "Build and commit a `:disassembly` generation from `:dedupe` and `:segments`.

  `dedupe-image` and `segments-image` are opened images. Options:
  `:configuration` (recorded only)."
  ([dedupe-image segments-image]
   (build! dedupe-image segments-image {}))
  ([dedupe-image segments-image {:keys [configuration] :or {configuration {}}}]
   (let [configuration (merge default-configuration configuration)
         dataset-root (:dataset-root dedupe-image)
         segments (:segments segments-image)
         total-events (get-in segments-image [:index :event-count])
         {:keys [generation directory relative]}
         (analysis/new-generation! dataset-root :disassembly)]
     (try
       (let [rendered (parallel/pmap-ordered #(render-segment dedupe-image %) segments)
             roles (filterv (set (:roles configuration))
                            (distinct (mapcat :roles rendered)))
             role-renders (mapv #(render-role rendered %) roles)
             segment-specs (segment-file-specs directory rendered)
             clj-files (->> segment-specs
                            (filter #(= :clj (:format %)))
                            (mapv #(select-keys % [:file :size :sha256])))
             asm-files (->> segment-specs
                            (filter #(= :asm (:format %)))
                            (mapv #(select-keys % [:file :size :sha256])))
             role-files (mapv (fn [{:keys [file size sha256]}]
                                {:file file :size size :sha256 sha256})
                              (role-file-specs directory role-renders))
             index {:format disassembly-format
                    :version disassembly-version
                    :capture-id (get-in segments-image [:index :capture-id])
                    :capture-manifest-sha256
                    (get-in segments-image [:index :capture-manifest-sha256])
                    :configuration configuration
                    :event-count total-events
                    :annotation-vocabulary contract/annotation-keys
                    :summary {:segment-count (count rendered)
                              :role-file-count (count role-renders)
                              :block-count (count (distinct (mapcat :block-ids rendered)))
                              :file-count (+ (count clj-files)
                                             (count asm-files)
                                             (count role-files))}
                    :files (vec (concat clj-files asm-files role-files))
                    :segments (mapv (fn [entry]
                                      (let [stem (format "segments/segment-%03d"
                                                         (:segment-id entry))]
                                        {:segment-id (:segment-id entry)
                                         :kind (:kind entry)
                                         :roles (:roles entry)
                                         :handler-contexts (:handler-contexts entry)
                                         :event-range (:event-range entry)
                                         :block-count (count (:block-ids entry))
                                         :block-ids (:block-ids entry)
                                         :mappings (frequencies (map :mapping (:blocks entry)))
                                         :rom-blocks (count (filter :mapped-rom? (:blocks entry)))
                                         :file (str stem ".asm")
                                         :asm-file (str stem ".asm")
                                         :clj-file (str stem ".clj")}))
                                    rendered)
                    :roles (mapv (fn [{:keys [role count]}]
                                   {:role role
                                    :block-count count
                                    :file (str "roles/" (name role) ".asm")})
                                 role-renders)}]
         (store/atomic-write-edn! (io/file directory "index.edn") index)
         (analysis/commit-stage!
          dataset-root :disassembly
          {:generation generation
           :directory relative
           :version disassembly-version
           :capture-id (:capture-id index)
           :capture-manifest-sha256 (:capture-manifest-sha256 index)
           :configuration configuration
           :summary (:summary index)})
         {:dataset-root dataset-root
          :directory (str directory)
          :index index})
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open!
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :disassembly)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :disassembly generation"
                      {:reason :missing-disassembly-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= disassembly-format (:format index))
        (throw (ex-info "Not a v2 disassembly image"
                        {:reason :not-a-disassembly-image :format (:format index)})))
      (doseq [{:keys [file size sha256]} (:files index)]
        (let [artifact (io/file directory file)]
          (when-not (= (long size) (.length artifact))
            (throw (ex-info "Disassembly artifact size mismatch"
                            {:reason :disassembly-artifact-size-mismatch :file file})))
          (when-not (= sha256 (store/sha256-file artifact))
            (throw (ex-info "Disassembly artifact checksum mismatch"
                            {:reason :disassembly-artifact-checksum-mismatch
                             :file file})))))
      {:dataset-root dataset-root
       :directory directory
       :index index
       :segments (:segments index)
       :files (:files index)
       :roles (:roles index)})))

(defn status [dataset-root]
  (analysis/stage-entry dataset-root :disassembly))

(defn segments [disassembly-image]
  (:segments disassembly-image))

(defn role-files [disassembly-image]
  (:roles disassembly-image))

(defn read-file
  "Return the text of a committed disassembly artifact by its relative path."
  [disassembly-image file]
  (slurp (io/file (:directory disassembly-image) file)))
