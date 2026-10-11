(ns omkamra.vice.features
  "Generic behavioral feature evidence.

  `:features` is an exact consumer of raw writes and of the `:dedupe`
  structural image. It partitions execution into two kinds of neutral logical
  unit:

  * mainline **fixed windows**, which count only mainline-context events so
    interrupt time neither consumes nor dilutes a window;
  * **IRQ invocations**, one per interrupt entry through its `RTI`.

  Each unit carries hardware write-domain counts and densities, an ordinary-RAM
  destination footprint, code-overwrite and hard write-to-execute handoff
  records, serial/IEC activity, VIC/SID writing PCs, and self-loop/repeat
  evidence. IRQ invocations additionally keep per-occurrence evidence so a
  mixed handler can be split at block-occurrence granularity later.

  `:features` emits evidence, not roles or spans; classification decides roles.

  Units are streamed to `units.edn` one unit per line as soon as they close, so
  the stage never retains the whole capture's evidence in memory. Unit-level
  evidence is small EDN; the heavy per-occurrence evidence of an IRQ invocation
  is written to a binary `occurrences.bin` channel (delta + evidence-dictionary
  encoded) and referenced from the unit line by offset and length. Handoffs are
  streamed to `handoffs.edn` the same way, and `index.edn` keeps only the small
  summary/configuration.

  Generation layout:

  ```text
  analysis/features/<generation>/
    index.edn
    units.edn
    occurrences.bin
    handoffs.edn
  ```"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.dedupe :as dedupe]
            [omkamra.vice.parallel :as parallel]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store])
  (:import [java.io BufferedOutputStream ByteArrayOutputStream FileOutputStream
            OutputStream RandomAccessFile]
           [java.util BitSet]))

(def features-version 7)
(def features-format (contract/format-keyword :features features-version))

(def default-configuration
  {:window-events 100000
   :destination-min-address 512
   :kernal-iec-ranges [[0xed00 0xeeff]]
   :iec-register-ranges [[0xdd00 0xddff]]
   :max-invocation-occurrences 50000
   :max-invocation-events 500000})

(defn- address-in-ranges?
  [address ranges]
  (boolean (some (fn [[start end]] (<= start address end)) ranges)))

(defn- serial-write?
  "A CIA2 port-A write which drives at least one IEC output bit.

  Bits 3-5 are ATN, clock, and data output. Bits 0-1 select the VIC bank and
  must not turn ordinary display-bank changes into loader evidence."
  [write]
  (and (= :cia2 (:domain write))
       (= 0xdd00 (:address write))
       (pos? (bit-and (int (:value write)) 0x38))))

;; Evidence accumulator ------------------------------------------------------

(defn- new-evidence
  [configuration]
  {:event-count 0
   :write-total 0
   :domain-counts {}
   :vic-write-count 0
   :sid-write-count 0
   :ram-write-count 0
   :cia1-write-count 0
   :cia2-write-count 0
   :color-ram-write-count 0
   :serial-write-count 0
   :iec-write-count 0
   :serial-read-count 0
   :iec-read-count 0
   :code-overwrite-count 0
   :kernal-iec-execution-count 0
   :kernal-iec-transfer-count 0
   :vic-write-pcs {}
   :sid-write-pcs {}
   :destination-min-address (:destination-min-address configuration)})

(defn- add-footprint
  [evidence address]
  (if-let [^BitSet footprint (:footprint evidence)]
    (do (.set footprint (int address)) evidence)
    (let [footprint (BitSet. 65536)]
      (.set footprint (int address))
      (assoc evidence :footprint footprint))))

(defn- add-write
  [evidence write code-addresses configuration]
  (let [address (:address write)
        domain (:domain write)
        pc (:pc write)
        local? (and (= domain :ram)
                    (>= address (:destination-min-address evidence)))]
    (-> evidence
        (update :write-total inc)
        (update-in [:domain-counts domain] (fnil inc 0))
        (cond->
         (= domain :vic) (update :vic-write-count inc)
         (= domain :sid) (update :sid-write-count inc)
         (= domain :ram) (update :ram-write-count inc)
         (= domain :cia1) (update :cia1-write-count inc)
         (= domain :cia2) (update :cia2-write-count inc)
         (= domain :color-ram) (update :color-ram-write-count inc)

         (serial-write? write) (update :serial-write-count inc)
         (address-in-ranges? address (:iec-register-ranges configuration))
         (update :iec-write-count inc)
         (and (= domain :ram) (contains? code-addresses address))
         (update :code-overwrite-count inc)
         (= domain :vic) (update-in [:vic-write-pcs pc] (fnil inc 0))
         (= domain :sid) (update-in [:sid-write-pcs pc] (fnil inc 0))
         local? (add-footprint address)))))

(defn- add-occurrence
  [evidence occurrence block-pcs configuration]
  (let [event-count (reduce + 0 (map (fn [[start end]] (- end start))
                                     (:fragments occurrence)))
        pcs (get block-pcs (:block-id occurrence) [])
        kernal (count (filter #(address-in-ranges?
                                % (:kernal-iec-ranges configuration))
                              pcs))]
    (-> evidence
        (update :event-count + event-count)
        (update :kernal-iec-execution-count + kernal))))

(defn- page-words
  [^BitSet footprint]
  (let [words (long-array 4)]
    (loop [address (.nextSetBit footprint 0)]
      (when (>= address 0)
        (let [page (bit-shift-right address 8)
              word (quot page 64)
              bit (mod page 64)]
          (aset-long words word
                     (bit-or (aget words word) (bit-shift-left 1 bit))))
        (recur (.nextSetBit footprint (inc address)))))
    (vec words)))

(defn- page-stats
  [words]
  (let [pages (for [word (range 4)
                    bit (range 64)
                    :when (pos? (bit-and (long (nth words word))
                                         (bit-shift-left 1 bit)))]
                (+ (* word 64) bit))]
    {:count (count pages) :min (first pages) :max (last pages)}))

(defn- finalize
  [evidence]
  (let [^BitSet footprint (:footprint evidence)
        base (dissoc evidence :footprint :destination-min-address)]
    (if footprint
      (let [unique (.cardinality footprint)
            minimum (.nextSetBit footprint 0)
            words (page-words footprint)
            stats (page-stats words)]
        (assoc base
               :destination-unique-address-count unique
               :destination-address-range [minimum (dec (.length footprint))]
               :destination-page-words words
               :destination-page-count (:count stats)
               :destination-page-min (:min stats)
               :destination-page-max (:max stats)))
      (assoc base
             :destination-unique-address-count 0
             :destination-address-range nil
             :destination-page-words [0 0 0 0]
             :destination-page-count 0
             :destination-page-min nil
             :destination-page-max nil))))

(def ^:private occurrence-evidence-keys
  "Role-relevant evidence persisted for an IRQ block occurrence.

  Full generic evidence is retained at unit level. Repeating its footprint,
  domain, and per-PC maps for every occurrence made the acceptance unit stream
  several gigabytes even though classification never reads those fields."
  [:event-count :write-total :vic-write-count :sid-write-count
   :ram-write-count :serial-write-count :serial-read-count :iec-write-count
   :kernal-iec-execution-count :destination-unique-address-count
   :destination-page-count])

(defn- role-evidence
  [evidence]
  (into {}
        (keep (fn [key]
                (let [value (get evidence key)]
                  (when (and (some? value)
                             (not (and (number? value) (zero? value))))
                    [key value]))))
        occurrence-evidence-keys))

(defn- close-unit
  [unit]
  (-> unit
      (update :evidence finalize)
      (update :occurrences
              (fn [occurrences]
                (mapv #(update % :evidence (comp role-evidence finalize))
                      occurrences)))))

;; Binary occurrence channel -------------------------------------------------

(defn- write-uvarint!
  [^OutputStream out ^long value]
  (loop [value value]
    (if (zero? (bit-and value (bit-not 0x7f)))
      (.write out (int value))
      (do (.write out (int (bit-or (bit-and value 0x7f) 0x80)))
          (recur (bit-shift-right value 7))))))

(defn- write-svarint!
  [^OutputStream out ^long value]
  (write-uvarint! out (bit-xor (bit-shift-left value 1)
                               (bit-shift-right value 63))))

(defn- evidence-vector
  [occurrence]
  (mapv (fn [key] (long (or (get-in occurrence [:evidence key]) 0)))
        occurrence-evidence-keys))

(defn- dictionary-evidence
  "Reconstruct one dictionary entry's sparse evidence map."
  [values]
  (into {}
        (keep-indexed (fn [index value]
                        (when-not (zero? (long value))
                          [(nth occurrence-evidence-keys index) value])))
        values))

(defn- encode-occurrences
  "Encode one invocation's occurrences as a self-contained block.

  Layout: occurrence count; an evidence dictionary (the distinct role-evidence
  tuples, in `occurrence-evidence-keys` order) so a repeated occurrence costs
  one id; then per occurrence a varint occurrence-id delta, a zig-zag block-id
  delta, a zig-zag event-start delta, the event length, and the evidence id."
  ^bytes [occurrences]
  (let [out (ByteArrayOutputStream.)
        dictionary (java.util.LinkedHashMap.)]
    (doseq [occurrence occurrences]
      (let [tuple (evidence-vector occurrence)]
        (when-not (.containsKey dictionary tuple)
          (.put dictionary tuple (count dictionary)))))
    (write-uvarint! out (count occurrences))
    (write-uvarint! out (count dictionary))
    (doseq [tuple (keys dictionary)]
      (doseq [value tuple] (write-uvarint! out value)))
    (loop [remaining (seq occurrences)
           previous-id 0 previous-block 0 previous-start 0]
      (when-let [occurrence (first remaining)]
        (let [id (long (:occurrence-id occurrence))
              block (long (:block-id occurrence))
              [start end] (:event-range occurrence)
              start (long start)
              end (long end)]
          (write-uvarint! out (- id previous-id))
          (write-svarint! out (- block previous-block))
          (write-svarint! out (- start previous-start))
          (write-uvarint! out (- end start))
          (write-uvarint! out (long (.get dictionary (evidence-vector occurrence))))
          (recur (next remaining) id block start))))
    (.toByteArray out)))

(defn- read-uvarint!
  ^long [^bytes bytes ^longs position]
  (loop [shift 0 result 0]
    (let [byte-value (bit-and (aget bytes (aget position 0)) 0xff)]
      (aset position 0 (inc (aget position 0)))
      (let [result (bit-or result
                           (bit-shift-left (long (bit-and byte-value 0x7f)) shift))]
        (if (zero? (bit-and byte-value 0x80))
          result
          (recur (+ shift 7) result))))))

(defn- read-svarint!
  ^long [^bytes bytes ^longs position]
  (let [value (read-uvarint! bytes position)]
    (bit-xor (bit-shift-right value 1) (- (bit-and value 1)))))

(defn- decode-occurrences
  "Decode the occurrence block that starts at `offset` in `bytes`."
  [^bytes bytes ^long offset]
  (let [position (long-array [offset])
        count (read-uvarint! bytes position)
        dictionary-size (read-uvarint! bytes position)
        dictionary (mapv (fn [_]
                           (dictionary-evidence
                            (mapv (fn [_] (read-uvarint! bytes position))
                                  occurrence-evidence-keys)))
                         (range dictionary-size))]
    (loop [index 0
           previous-id 0 previous-block 0 previous-start 0
           occurrences (transient [])]
      (if (< index count)
        (let [id (+ previous-id (read-uvarint! bytes position))
              block (+ previous-block (read-svarint! bytes position))
              start (+ previous-start (read-svarint! bytes position))
              end (+ start (read-uvarint! bytes position))
              evidence (nth dictionary (read-uvarint! bytes position))]
          (recur (inc index) id block start
                 (conj! occurrences {:occurrence-id id
                                     :block-id block
                                     :event-range [start end]
                                     :evidence evidence})))
        (persistent! occurrences)))))

;; Streaming unit output -----------------------------------------------------

(defn- write-line!
  [^java.io.Writer writer value]
  (.write writer (pr-str value))
  (.write writer "\n"))

(defn- emit-unit!
  "Finalize, stream, and count a closed unit; the unit is not retained.

  An IRQ invocation's occurrences are appended to the binary occurrence
  channel and referenced from the unit line by offset and length."
  [state unit]
  (let [occurrences (:occurrences unit)
        [unit state]
        (if (seq occurrences)
          (let [bytes (encode-occurrences occurrences)
                ^OutputStream out (:occurrence-writer state)
                offset (long (:occurrence-offset state))]
            (.write out bytes 0 (alength bytes))
            [(assoc unit :occurrences {:occurrence-offset offset
                                       :occurrence-length (alength bytes)})
             (update state :occurrence-offset + (alength bytes))])
          [(dissoc unit :occurrences) state])]
    (write-line! (:unit-writer state) unit)
    (-> state
        (update :unit-count inc)
        (update (if (= :mainline-window (:kind unit))
                  :mainline-unit-count
                  :irq-unit-count)
                inc))))

(defn- emit-handoff!
  [state handoff]
  (write-line! (:handoff-writer state) handoff)
  (update state :handoff-count inc))

;; Write attribution ---------------------------------------------------------

(defn- occurrence-envelope
  [occurrence]
  (let [fragments (:fragments occurrence)]
    [(first (first fragments)) (second (last fragments))]))

(defn- attribute-access!
  "Assign one chronological access to the fragment that covers its event."
  [frags cursor state access]
  (let [event (:event-index access)]
    (loop []
      (let [index @cursor]
        (cond
          (>= index (count frags)) state
          (< event (:start (nth frags index))) state
          (< event (:end (nth frags index)))
          (let [occurrence-id (:occurrence-id (nth frags index))]
            (vreset! cursor index)
            (if (= :read (:kind access))
              (update-in state [occurrence-id :read-count] (fnil inc 0))
              (update-in state [occurrence-id :writes] (fnil conj []) access)))
          :else (do (vreset! cursor (inc index)) (recur)))))))

(defn- scan-window-accesses
  "Attribute a window's writes and role-relevant reads to occurrences.

  The shared reader exposes every data read, but features only consumes reads
  in the configured IEC ranges. Dropping all other reads before attribution
  keeps a parallel extraction batch bounded without weakening its evidence.
  Accesses are chronological, so one monotonic fragment cursor suffices."
  [reader start end fragments configuration]
  (let [frags (vec (sort-by :start fragments))
        cursor (volatile! 0)]
    (reader/scan-filtered-accesses
     reader
     [start end]
     #(address-in-ranges? % (:iec-register-ranges configuration))
     (fn [state access]
       (if (and (= :read (:kind access))
                (not= :cia2 (:domain access)))
         state
         (attribute-access! frags cursor state access)))
     {})))

;; Unit construction ---------------------------------------------------------

(defn- open-window
  [configuration frame-id context-id context-kind envelope]
  {:kind :mainline-window
   :frame-id frame-id
   :context-id context-id
   :context-kind context-kind
   :effective-level :mainline
   :event-range envelope
   :evidence (new-evidence configuration)})

(defn- open-invocation
  [configuration frame-id context-id context-kind event-index]
  {:kind :irq-invocation
   :frame-id frame-id
   :context-id context-id
   :context-kind context-kind
   :effective-level :irq
   :event-range [event-index event-index]
   :evidence (new-evidence configuration)
   :occurrences []})

(defn- add-closed-write-evidence
  [evidence writes code-addresses configuration]
  (reduce (fn [evidence write]
            (add-write evidence write code-addresses configuration))
          evidence
          writes))

(defn- add-closed-read-evidence
  [evidence read-count]
  (-> evidence
      (update :serial-read-count + read-count)
      (update :iec-read-count + read-count)))

(defn- process-mainline
  [state occurrence writes reads configuration block-pcs code-addresses]
  (let [envelope (occurrence-envelope occurrence)
        frame-id (:frame-id occurrence)
        context-id (:context-id occurrence)
        state (if (and (:mainline state)
                       (not= frame-id (get-in state [:mainline :frame-id])))
                (-> state
                    (emit-unit! (close-unit (:mainline state)))
                    (assoc :mainline nil))
                state)
        first-pc (first (get block-pcs (:block-id occurrence) []))
        handoff? (contains? (:written-code state) first-pc)
        state (if handoff?
                (-> state
                    (emit-handoff! {:event-index (first envelope)
                                    :pc first-pc
                                    :block-id (:block-id occurrence)})
                    (update :written-code disj first-pc))
                state)
        window (or (:mainline state)
                   (open-window configuration frame-id context-id
                                (:context-kind occurrence) envelope))
        window (-> window
                   (assoc :event-range [(first (:event-range window))
                                        (second envelope)])
                   (update :evidence
                           (fn [evidence]
                             (-> evidence
                                 (add-occurrence occurrence block-pcs configuration)
                                 (add-closed-write-evidence writes code-addresses
                                                            configuration)
                                 (add-closed-read-evidence reads)))))
        written (keep #(when (and (= :ram (:domain %))
                                  (contains? code-addresses (:address %)))
                         (:address %))
                      writes)
        state (-> state
                  (assoc :mainline window)
                  (update :written-code into written))]
    (if (>= (get-in window [:evidence :event-count]) (:window-events configuration))
      (-> state
          (emit-unit! (close-unit window))
          (assoc :mainline nil))
      state)))

(defn- invocation-overflow?
  "True when an open IRQ invocation has grown past its memory valve.

  A handler which never returns within the capture (or which dedupe never closes
  with an RTI) would otherwise accumulate every subsequent occurrence in one
  unit. Force-closing at a cap bounds memory; the continuation keeps the same
  context id and is emitted when the handler finally returns."
  [invocation configuration]
  (or (>= (count (:occurrences invocation))
          (:max-invocation-occurrences configuration))
      (>= (get-in invocation [:evidence :event-count])
          (:max-invocation-events configuration))))

(defn- process-irq
  [state occurrence writes reads configuration block-pcs]
  (let [frame-id (:frame-id occurrence)]
    (if-let [index (first (keep-indexed
                           (fn [index invocation]
                             (when (= (:frame-id invocation) frame-id) index))
                           (:irq-stack state)))]
      (let [invocation (nth (:irq-stack state) index)
            envelope (occurrence-envelope occurrence)
            occurrence-evidence (-> (new-evidence configuration)
                                    (add-occurrence occurrence block-pcs configuration)
                                    (add-closed-write-evidence writes #{} configuration)
                                    (add-closed-read-evidence reads))
            invocation (-> invocation
                           (assoc :event-range [(first (:event-range invocation))
                                                (second envelope)])
                           (update :evidence
                                   (fn [evidence]
                                     (-> evidence
                                         (add-occurrence occurrence block-pcs configuration)
                                         (add-closed-write-evidence writes #{} configuration)
                                         (add-closed-read-evidence reads))))
                           (update :occurrences conj
                                   {:occurrence-id (:occurrence-id occurrence)
                                    :block-id (:block-id occurrence)
                                    :event-range envelope
                                    :evidence occurrence-evidence}))
            state (assoc-in state [:irq-stack index] invocation)]
        (if (invocation-overflow? invocation configuration)
          (-> state
              (emit-unit! (close-unit invocation))
              (assoc-in [:irq-stack index]
                        (open-invocation configuration frame-id
                                         (:context-id occurrence)
                                         (:context-kind occurrence)
                                         (second (:event-range invocation)))))
          state))
      state)))

(defn- process-switch
  [state switch configuration]
  (if (= :return (:kind switch))
    (if (seq (:irq-stack state))
      (-> state
          (emit-unit! (close-unit (peek (:irq-stack state))))
          (update :irq-stack pop))
      state)
    (let [invocation (open-invocation configuration (:frame-id switch)
                                      (:context-id switch) (:kind switch)
                                      (:event-index switch))]
      (update state :irq-stack conj invocation))))

(defn- mainline-occurrence?
  "True when `:dedupe` assigned this occurrence effective level `:mainline`.

  A leveled frame's continuation carries observed context kind `:irq` but is
  placed on the mainline timeline; a `:dedupe` occurrence boundary at the
  leveling point keeps the classification exact."
  [occurrence]
  (= :mainline (:effective-level occurrence)))

(defn- process-item
  [state item configuration block-pcs code-addresses]
  (case (:type item)
    :switch (process-switch state (:switch item) configuration)
    :occurrence (let [occurrence (:occurrence item)]
                  (if (mainline-occurrence? occurrence)
                    (process-mainline state occurrence (:writes item)
                                      (:read-count item)
                                      configuration block-pcs code-addresses)
                    (process-irq state occurrence (:writes item)
                                 (:read-count item)
                                 configuration block-pcs)))))

(defn- extract-window
  "Scan one dedupe window's accesses and attribute them to occurrences.

  This is the expensive, side-effect-free part of a window and is safe to run
  in parallel across windows."
  [reader trace window configuration]
  (let [start (:start window)
        end (:end window)
        {:keys [occurrences context-switches]} (dedupe/read-range trace start end)
        ;; A system context (a stock KERNAL routine such as the default IRQ
        ;; service routine) is execution, not program behaviour: drop its
        ;; enter/return switches so it never opens an invocation unit. Its
        ;; occurrences are then ignored because no invocation is open.
        system-contexts (:system-contexts trace)
        switches (remove #(contains? system-contexts (:context-id %))
                         context-switches)
        relevant (filter #(<= start (ffirst (:fragments %))) occurrences)
        fragments (mapcat (fn [occurrence]
                            (map (fn [[fragment-start fragment-end]]
                                   {:start fragment-start :end fragment-end
                                    :occurrence-id (:occurrence-id occurrence)})
                                 (:fragments occurrence)))
                          relevant)
        by-occurrence (scan-window-accesses reader start end fragments
                                            configuration)]
    {:items (sort-by (juxt :event-index #(if (= :switch (:type %)) 0 1))
                     (concat
                      (map (fn [occurrence]
                             {:type :occurrence
                              :event-index (ffirst (:fragments occurrence))
                              :occurrence occurrence
                              :writes (get-in by-occurrence
                                              [(:occurrence-id occurrence) :writes]
                                              [])
                              :read-count (get-in by-occurrence
                                                  [(:occurrence-id occurrence)
                                                   :read-count]
                                                  0)})
                           relevant)
                      (map (fn [switch]
                             {:type :switch :event-index (:event-index switch)
                              :switch switch})
                           switches)))}))

(defn- assemble-window
  [state window-evidence configuration code-addresses block-pcs]
  (reduce (fn [state item]
            (process-item state item configuration block-pcs code-addresses))
          state
          (:items window-evidence)))

(defn- build-units
  [reader trace configuration code-addresses block-pcs progress
   unit-writer occurrence-writer handoff-writer]
  (let [parallelism (or (:parallelism configuration) parallel/default-parallelism)
        batch-size (or (:extract-batch-windows configuration)
                       (max 1 parallelism))
        max-events (:max-events configuration)
        windows (cond->> (:windows trace)
                  max-events (filter #(< (:start %) max-events)))
        total (count windows)
        processed (volatile! 0)
        initial {:mainline nil :irq-stack []
                 :written-code #{}
                 :unit-writer unit-writer
                 :occurrence-writer occurrence-writer
                 :occurrence-offset 0
                 :handoff-writer handoff-writer
                 :unit-count 0 :mainline-unit-count 0 :irq-unit-count 0
                 :handoff-count 0}
        state (reduce
               (fn [state batch]
                 (let [extracted (parallel/pmap-ordered
                                  #(extract-window reader trace % configuration)
                                  batch parallelism)]
                   (vswap! processed + (count batch))
                   (analysis/report! progress {:processed-windows @processed
                                               :windows-total total})
                   (reduce (fn [state window-evidence]
                             (assemble-window state window-evidence configuration
                                              code-addresses block-pcs))
                           state extracted)))
               initial
               (partition-all batch-size windows))
        state (if (:mainline state)
                (emit-unit! state (close-unit (:mainline state)))
                state)
        state (reduce (fn [state invocation]
                        (emit-unit! state (close-unit invocation)))
                      state
                      (:irq-stack state))]
    state))

;; Build ---------------------------------------------------------------------

(defn build!
  "Build and commit a `:features` generation.

  `reader` is a capture reader and `trace` is an opened `:dedupe` image.
  Options: `:configuration`. Returns the generation directory and index.

  Units and handoffs are streamed to disk as they close; the build never
  retains them, so peak memory is bounded by the current dedupe window and
  extraction batch. `:configuration :max-events` (test-only) bounds the dedupe
  windows considered."
  ([reader trace] (build! reader trace {}))
  ([reader trace {:keys [configuration progress]
                  :or {configuration default-configuration}}]
   (let [configuration (merge default-configuration configuration)
         dataset-root (reader/dataset-root reader)
         capture-id (reader/capture-id reader)
         capture-sha (analysis/capture-manifest-sha (reader/directory reader))
         code-addresses (set (map :pc (:instructions trace)))
         block-pcs (into {}
                         (map (fn [block-id]
                                [block-id (mapv :pc
                                                (:instructions
                                                 (dedupe/read-block trace block-id)))]))
                         (range (count (:block-index trace))))
         {:keys [generation directory relative]}
         (analysis/new-generation dataset-root :features)]
     (try
       (let [index (with-open [unit-writer (io/writer (io/file directory "units.edn"))
                               occurrence-writer (BufferedOutputStream.
                                                  (FileOutputStream.
                                                   (io/file directory "occurrences.bin")))
                               handoff-writer (io/writer (io/file directory "handoffs.edn"))]
                     (let [state (build-units reader trace configuration
                                              code-addresses block-pcs progress
                                              unit-writer occurrence-writer
                                              handoff-writer)]
                       {:format features-format
                        :version features-version
                        :capture-id capture-id
                        :capture-manifest-sha256 capture-sha
                        :event-count (:event-count (:index trace))
                        :configuration configuration
                        :unit-file "units.edn"
                        :occurrence-file "occurrences.bin"
                        :handoff-file "handoffs.edn"
                        :summary {:unit-count (:unit-count state)
                                  :mainline-window-count (:mainline-unit-count state)
                                  :irq-invocation-count (:irq-unit-count state)
                                  :handoff-count (:handoff-count state)}}))]
         (store/atomic-write-edn! (io/file directory "index.edn") index)
         (analysis/commit-stage!
          dataset-root :features
          {:generation generation
           :directory relative
           :version features-version
           :capture-id capture-id
           :capture-manifest-sha256 capture-sha
           :configuration configuration
           :summary (:summary index)})
         {:dataset-root dataset-root
          :directory (str directory)
          :index index})
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

(defn build-async!
  "Run `build!` on a background future and return a deref-able progress handle."
  ([reader trace] (build-async! reader trace {}))
  ([reader trace options]
   (analysis/start-async!
    {:stage :features
     :windows-total (count (:windows trace))
     :processed-windows 0}
    (fn [handle]
      (analysis/summarize-result
       (build! reader trace (assoc options :progress handle)))))))

;; Readers -------------------------------------------------------------------

(defn- image-file ^java.io.File [features name]
  (io/file (:directory features) (get-in features [:index name])))

(defn- reduce-lines
  "Reduce `rf` over the newline-delimited EDN records in `file`."
  [file rf init]
  (let [file (io/file file)]
    (if (.isFile file)
      (with-open [reader (io/reader file)]
        (reduce (fn [acc line]
                  (if (or (nil? line) (empty? line))
                    acc
                    (rf acc (edn/read-string line))))
                init
                (line-seq reader)))
      init)))

(defn open
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :features)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :features generation"
                      {:reason :missing-features-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= features-format (:format index))
        (throw (ex-info "Not a v2 features image"
                        {:reason :not-a-features-image :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index})))

(defn status [dataset-root]
  (analysis/stage-entry dataset-root :features))

(defn- decode-unit
  "Replace a unit's occurrence reference with its decoded occurrences."
  [^RandomAccessFile occurrence-file unit]
  (let [reference (:occurrences unit)]
    (if (map? reference)
      (let [length (int (:occurrence-length reference))
            bytes (byte-array length)]
        (.seek occurrence-file (long (:occurrence-offset reference)))
        (.readFully occurrence-file bytes)
        (assoc unit :occurrences (decode-occurrences bytes 0)))
      unit)))

(defn reduce-units
  "Reduce `rf` over the streamed feature units in order, without retaining them.

  Each unit is returned with its IRQ occurrences decoded from the binary
  occurrence channel, so a consumer sees the same shape regardless of how the
  occurrences were encoded."
  [features rf init]
  (let [occurrence-file (image-file features :occurrence-file)]
    (if (.isFile occurrence-file)
      (with-open [reader (RandomAccessFile. occurrence-file "r")]
        (reduce-lines (image-file features :unit-file)
                      (fn [acc unit] (rf acc (decode-unit reader unit)))
                      init))
      (reduce-lines (image-file features :unit-file) rf init))))

(defn units
  "Eagerly materialize the feature units (small / test convenience)."
  [features]
  (reduce-units features conj []))

(defn handoffs
  "Eagerly materialize the streamed write-to-execute handoffs."
  [features]
  (reduce-lines (image-file features :handoff-file) conj []))
