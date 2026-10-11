(ns omkamra.vice.disassembly
  "Structural disassembly of a `:dedupe` image along `:segments` boundaries.

  This is the last stage of the v2 pipeline. It consumes exactly two
  committed images — the structural execution image (`:dedupe`) and the
  semantic segmentation (`:segments`) — and never touches the raw capture.

  For each semantic segment it selects the globally deduplicated basic blocks
  whose occurrences *begin* inside the segment (so a block whose execution
  straddles a boundary belongs to the segment it started in, and neighbouring
  segments never share a block in the same output), and renders each selected
  block exactly once per file. Volatile operand bytes render as `??`; operand
  and instruction bytes come from the instruction dictionary, so no replay or
  raw memory access is needed.

  Every rendered block carries one fixed annotation line
  (`contract/format-annotation`): `; B:.. C:.. P:.. S:.. E:.. R:..`, the global
  block id, the context, the distinct predecessor/successor counts within the
  selected segment and context, the occurrence count in the segment, and the
  observed back-edge repeats.

  Files:

  ```text
  analysis/disassembly/<generation>/
    index.edn
    segments/segment-000.asm   ; one file per semantic segment
    roles/<role>.asm           ; raster/music/loader/decruncher routines
  ```

  No `.asm` file contains a basic block twice."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.isa :as isa]
            [omkamra.vice.parallel :as parallel]
            [omkamra.vice.store :as store]))

(def disassembly-version 1)
(def disassembly-format (contract/format-keyword :disassembly disassembly-version))

(def default-configuration
  {:roles [:raster :music :loader :decruncher]})

;; Instruction rendering -----------------------------------------------------

(defn- operand-text
  [pc mode template volatile-mask]
  (let [lo (long (nth template 0 0))
        hi (long (nth template 1 0))
        vol0? (pos? (bit-and (long volatile-mask) 1))
        vol1? (pos? (bit-and (long volatile-mask) 2))
        byte (if vol0? "??" (format "$%02X" (bit-and lo 0xff)))
        word (if (or vol0? vol1?)
               "??"
               (format "$%04X" (bit-and (bit-or lo (bit-shift-left hi 8)) 0xffff)))]
    (case mode
      :imp ""
      :acc "A"
      :imm (str "#" byte)
      :zp byte
      :zpx (str byte ",X")
      :zpy (str byte ",Y")
      :abs word
      :absx (str word ",X")
      :absy (str word ",Y")
      :ind (str "(" word ")")
      :indx (str "(" byte ",X)")
      :indy (str "(" byte "),Y")
      :rel (if vol0?
             "??"
             (format "$%04X" (bit-and (long (isa/relative-target pc lo)) 0xffff)))
      nil)))

(defn- render-instruction
  [{:keys [pc opcode width template volatile-mask]}]
  (let [mnemonic (aget ^objects isa/mnemonic-by-opcode opcode)
        mode (aget ^objects isa/mode-by-opcode opcode)
        operand (operand-text pc mode template volatile-mask)
        operand-count (dec (long width))
        byte-text (str/join
                   " "
                   (cons (format "%02X" (bit-and (long opcode) 0xff))
                         (map (fn [index]
                                (if (pos? (bit-and (long volatile-mask)
                                                   (bit-shift-left 1 index)))
                                  "??"
                                  (format "%02X"
                                          (bit-and (long (nth template index 0)) 0xff))))
                              (range operand-count))))
        text (if (str/blank? operand) mnemonic (str mnemonic " " operand))]
    (format "%04X  %-11s %s" (bit-and (long pc) 0xffff) byte-text text)))

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
     :mapping (mapping pc port)
     :mapped-rom? (mapped-rom? (mapping pc port))
     :annotation annotation
     :true-predecessors (count (get (:predecessors stats) key #{}))
     :true-successors (count (get (:successors stats) key #{}))
     :instructions (:instructions block)}))

(defn- block-text
  [{:keys [annotation instructions]}]
  (str/join "\n"
            (cons (contract/format-annotation annotation)
                  (map render-instruction instructions))))

(defn- blocks-text [block-records]
  (str/join "\n" (map block-text block-records)))

(defn- render-segment
  [trace segment]
  (let [stats (segment-stats trace segment)
        block-ids (->> (:executions stats) keys
                       (map (fn [key] (second (unpack-key (long key)))))
                       distinct sort vec)
        blocks (mapv #(block-record trace stats %) block-ids)
        [start end] (:event-range segment)
        header (str ";; segment " (:segment-id segment)
                    "  kind=" (name (:kind segment))
                    "  events=[" start "," end ")"
                    "  roles=" (if (seq (:roles segment))
                                 (str/join "," (sort (map name (:roles segment))))
                                 "-")
                    "  handlers=" (if (seq (:handler-contexts segment))
                                    (str/join "," (sort (:handler-contexts segment)))
                                    "-"))]
    {:segment-id (:segment-id segment)
     :kind (:kind segment)
     :roles (vec (sort (:roles segment)))
     :handler-contexts (vec (sort (:handler-contexts segment)))
     :event-range [start end]
     :blocks blocks
     :block-ids block-ids
     :text (str header "\n" (blocks-text blocks) "\n")}))

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
        header (str ";; role " (name role)
                    "  segments=" (count (filter #(contains? (set (:roles %)) role) rendered))
                    "  blocks=" (count blocks))]
    {:role role
     :count (count blocks)
     :text (str header "\n" (blocks-text blocks) "\n")}))

;; Publication ---------------------------------------------------------------

(defn- write-text!
  [file text]
  (store/write-artifact!
   file
   (fn [^java.io.OutputStream out]
     (.write out (.getBytes ^String text "UTF-8")))))

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
         (analysis/new-generation dataset-root :disassembly)]
     (try
       (let [rendered (parallel/pmap-ordered #(render-segment dedupe-image %) segments)
             roles (filterv (set (:roles configuration))
                            (distinct (mapcat :roles rendered)))
             role-renders (mapv #(render-role rendered %) roles)
             files (into []
                         (concat
                          (map (fn [entry]
                                 (let [relative-file (str "segments/"
                                                          (format "segment-%03d.asm"
                                                                  (:segment-id entry)))
                                       spec (write-text! (io/file directory relative-file)
                                                         (:text entry))]
                                   (assoc spec
                                          :kind :segment
                                          :file relative-file
                                          :segment-id (:segment-id entry))))
                               rendered)
                          (map (fn [{:keys [role text]}]
                                 (let [relative-file (str "roles/" (name role) ".asm")
                                       spec (write-text! (io/file directory relative-file)
                                                         text)]
                                   (assoc spec
                                          :kind :role
                                          :file relative-file
                                          :role role)))
                               role-renders)))
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
                              :file-count (count files)}
                    :files (mapv (fn [{:keys [file size sha256]}]
                                   {:file file :size size :sha256 sha256})
                                 files)
                    :segments (mapv (fn [entry]
                                      {:segment-id (:segment-id entry)
                                       :kind (:kind entry)
                                       :roles (:roles entry)
                                       :handler-contexts (:handler-contexts entry)
                                       :event-range (:event-range entry)
                                       :block-count (count (:block-ids entry))
                                       :block-ids (:block-ids entry)
                                       :mappings (frequencies (map :mapping (:blocks entry)))
                                       :rom-blocks (count (filter :mapped-rom? (:blocks entry)))
                                       :file (str "segments/"
                                                  (format "segment-%03d.asm"
                                                          (:segment-id entry)))})
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

(defn open
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
