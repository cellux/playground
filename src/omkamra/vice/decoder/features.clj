(ns omkamra.vice.decoder.features
  "Bounded behavioral feature extraction from raw VICE chunks."
  (:require [omkamra.vice.asm :as asm]
            [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.stream :as stream]
            [omkamra.vice.decoder.video :as video]))

;; Offline feature extraction ------------------------------------------------
;;
;; Features deliberately remain chunk-local.  Their frame records retain enough
;; boundary context for a later classifier to join neighbouring chunks, while
;; the capture and analysis workers retain only one raw chunk at a time.

(def default-feature-configuration
  "Default configuration for the bounded offline `:features` analysis stage.

  Callers may override these values through `analysis/run!` under
  `{:config {:features ...}}`; the effective configuration is retained in the
  analysis manifest.  The KERNAL range is intentionally configurable because
  custom fastloaders do not necessarily use KERNAL IEC entry points."
  {:raster-lines 312
   :fingerprint-window-frames 8
   :max-vic-writes-per-frame 64
   ;; No-IRQ stretches use these bounded units for write/decrunch evidence.
   ;; They are deliberately smaller than a normal physical chunk, so a long
   ;; interrupt-disabled decruncher does not become one opaque observation.
   :write-window-events 10000
   ;; Persist only a bounded tail/head handoff summary. Ordered classification
   ;; joins it across physical chunks without retaining complete event data.
   :cross-chunk-handoff-events 20000
   ;; Decrunchers routinely copy their own code and scratch state into zero
   ;; page and the stack page, so those writes would swamp the genuine
   ;; memory-to-memory destination region. Destination evidence therefore
   ;; starts at this address; keep it configurable because numbered demos can
   ;; place part data anywhere in RAM.
   :destination-min-address 512
   :iec-register-ranges [[0xdd00 0xddff]]
   :kernal-iec-ranges [[0xed00 0xeeff]]})

(defn valid-address-ranges?
  [ranges]
  (and (vector? ranges)
       (every? (fn [[start end]]
                 (and (integer? start)
                      (integer? end)
                      (<= 0 start end 0xffff)))
               ranges)))

(defn normalize-feature-configuration
  "Merge and validate feature-stage configuration.

  Address ranges are inclusive `[start end]` 16-bit CPU address pairs.  The
  returned value is suitable for persistence in an analysis-stage manifest."
  [configuration]
  (when-not (map? configuration)
    (throw (ex-info "Feature configuration must be a map"
                    {:configuration configuration})))
  (let [configuration (merge default-feature-configuration configuration)]
    (doseq [key [:raster-lines :fingerprint-window-frames
                 :max-vic-writes-per-frame :write-window-events
                 :cross-chunk-handoff-events]]
      (when-not (pos-int? (get configuration key))
        (throw (ex-info "Feature configuration value must be a positive integer"
                        {:key key :value (get configuration key)}))))
    (when-not (and (integer? (:destination-min-address configuration))
                   (<= 0 (:destination-min-address configuration) 0xffff))
      (throw (ex-info "Destination minimum address must be a 16-bit integer"
                      {:destination-min-address
                       (:destination-min-address configuration)})))
    (doseq [key [:iec-register-ranges :kernal-iec-ranges]]
      (when-not (valid-address-ranges? (get configuration key))
        (throw (ex-info "Feature address ranges must be inclusive 16-bit pairs"
                        {:key key :value (get configuration key)}))))
    configuration))

(defn address-in-ranges?
  [address ranges]
  (boolean (some (fn [[start end]] (<= start address end)) ranges)))

(def serial-bus-mask
  "CIA2 `$dd00` bits 2-7 carry the serial bus (ATN/CLOCK/DATA). Bits 0-1 only
  select the VIC bank, so a write that leaves bits 2-7 untouched is not serial
  traffic even though it targets the same register."
  0xfc)

(defn write-domain
  [address]
  (cond
    (video/vic-register-address address) :vic
    (video/sid-register-address? address) :sid
    (video/cia1-register-address? address) :cia1
    (video/cia2-register-address? address) :cia2
    (<= 0xd800 address 0xdbff) :color-ram
    :else :memory))

(defn write-domain-at
  "Classify a write address using the CPU port value at that instant.

  With the I/O area banked out, `$d000-$dfff` is ordinary RAM, so those writes
  are neither peripheral traffic nor memory-mapped registers."
  [address port]
  (if (and port
           (<= 0xd000 address 0xdfff)
           (not (stream/io-visible? port)))
    :memory
    (write-domain address)))

(defn executed-code-addresses
  [events]
  (let [addresses (boolean-array 65536)]
    (doseq [{:keys [pc bytes mode]} (:instructions events)]
      (dotimes [offset (asm/instruction-width {:bytes bytes :mode mode})]
        (aset-boolean addresses (bit-and (+ pc offset) 0xffff) true)))
    addresses))

(defn kernal-iec-instruction-counts
  [events kernal-iec-ranges]
  (let [instructions (:instructions events)]
    (reduce
     (fn [counts instruction-id]
       (let [{:keys [pc mnemonic mode] :as instruction}
             (nth instructions instruction-id)
             transfer? (and (contains? #{"JSR" "JMP"} mnemonic)
                            (= :abs mode)
                            (address-in-ranges? (stream/absolute-operand instruction)
                                                kernal-iec-ranges))]
         (cond-> counts
           (address-in-ranges? pc kernal-iec-ranges)
           (update :kernal-iec-execution-count inc)

           transfer?
           (update :kernal-iec-transfer-count inc))))
     {:kernal-iec-execution-count 0
      :kernal-iec-transfer-count 0}
     (stream/stream-instruction-ids events))))

(defn irq-frames
  [structure event-count global-start raster-lines]
  (let [irq-spans (->> (get-in structure [:stages :structure :spans])
                       (filter #(= :irq (:kind %)))
                       (sort-by :start-index))
        grouped
        (reduce
         (fn [frames {:keys [start-index entry-pc handler-pc trigger]}]
           (let [handler (or handler-pc entry-pc)
                 raster-line (some-> (:raster-line trigger)
                                     (mod raster-lines))
                 previous-raster (get-in frames [(dec (count frames))
                                                 :last-raster-line])
                 new-frame? (and previous-raster raster-line
                                 (< raster-line previous-raster))
                 frame (fn [] {:start-index start-index
                               :irq-entry-pcs []
                               :irq-handler-pcs []
                               :irq-triggers []
                               :last-raster-line raster-line})]
             (if (or (empty? frames) new-frame?)
               (conj frames (-> (frame)
                                (update :irq-entry-pcs conj entry-pc)
                                (update :irq-handler-pcs conj handler)
                                (update :irq-triggers conj
                                        [(:raster-line trigger)
                                         (:cpu-cycle trigger)])))
               (let [index (dec (count frames))]
                 (-> frames
                     (update-in [index :irq-entry-pcs] conj entry-pc)
                     (update-in [index :irq-handler-pcs] conj handler)
                     (update-in [index :irq-triggers] conj
                                [(:raster-line trigger) (:cpu-cycle trigger)])
                     (assoc-in [index :last-raster-line] raster-line))))))
         []
         irq-spans)]
    (mapv (fn [index {:keys [start-index] :as frame}]
            (let [end-index (or (:start-index (nth grouped (inc index) nil))
                                event-count)]
              (-> frame
                  (dissoc :last-raster-line)
                  (assoc :unit-kind :irq-frame
                         :end-index end-index
                         :event-range [(+ global-start start-index)
                                       (+ global-start end-index)]))))
          (range (count grouped))
          grouped)))

(defn write-windows
  "Partition an interrupt-free chunk into bounded classifier observations.

  IRQ frames remain the preferred semantic units when present.  A chunk with
  no extracted IRQ frame otherwise used to collapse into one feature summary,
  losing destination-footprint progression and leaving an interrupt-disabled
  decruncher effectively invisible to the classifier."
  [start-index end-index global-start window-events]
  (mapv (fn [start-index]
          (let [end-index (min end-index (+ start-index window-events))]
            {:unit-kind :write-window
             :start-index start-index
             :end-index end-index
             :event-range [(+ global-start start-index)
                           (+ global-start end-index)]}))
        (range start-index end-index window-events)))

(defn feature-fingerprint-id
  [signature]
  ;; The complete signature is retained alongside this compact, stable display
  ;; ID, so a hash collision cannot make two frame patterns indistinguishable.
  (format "fp-%08x" (bit-and (hash signature) 0xffffffff)))

(defn frame-index-for-event
  [frames event-index initial-index]
  (loop [index initial-index]
    (if (and (< index (count frames))
             (>= event-index (:end-index (nth frames index))))
      (recur (inc index))
      index)))

(defn page-words
  "Pack a 256-bit page bitmap into four longs for compact EDN persistence."
  [^java.util.BitSet pages]
  (let [words (long-array 4)]
    (dotimes [page 256]
      (when (.get pages page)
        (let [word (quot page 64)
              bit (mod page 64)]
          (aset-long words word
                     (bit-or (aget words word) (bit-shift-left 1 bit))))))
    (vec words)))

(defn destination-profile
  "Summarize destination-eligible writes from one unit's RAM footprint.

  Zero page and the stack are excluded by construction: a decruncher copies
  its own code and pointers there, and those writes otherwise dominate the
  minimum address and hide the real memory-to-memory destination. The page
  bitmap makes broad, contiguous destination coverage cheap to accumulate
  across units during ordered classification."
  [^java.util.BitSet footprint maximum destination-floor]
  (when (and footprint (>= maximum destination-floor))
    (let [minimum (.nextSetBit footprint destination-floor)
          pages (java.util.BitSet. 256)
          unique-count (volatile! 0)]
      (loop [address (.nextSetBit footprint destination-floor)]
        (when (>= address 0)
          (vswap! unique-count inc)
          (.set pages (bit-shift-right address 8))
          (recur (.nextSetBit footprint (inc address)))))
      {:destination-unique-address-count @unique-count
       :destination-address-range [minimum maximum]
       :destination-address-span (inc (- maximum minimum))
       :destination-page-count (.cardinality pages)
       :destination-page-min (.nextSetBit pages 0)
       :destination-page-max (let [last (.length pages)]
                               (when (pos? last) (dec last)))
       :destination-page-words (page-words pages)})))

(defn finalize-frame-footprints
  "Attach compact, ordinary-RAM destination-footprint evidence to frames.

  A `BitSet` exists only while this one chunk is analysed. The persisted
  fingerprint combines its content hash with exact count/range fields, while
  `:ram-footprint-repeat?` uses `BitSet.equals` and is therefore not based on
  that hash alone. This bounds retained footprint state to at most one 64 KiB
  address bitmap per frame in the open analysis chunk. Destination evidence is
  derived from the same bitmap but restricted to addresses at or above
  `destination-floor`, so decrunch scratch does not masquerade as the
  destination region."
  [frames footprints minimums maximums destination-floor]
  (loop [index 0
         prior nil
         result []]
    (if (= index (count frames))
      result
      (let [footprint (aget ^objects footprints index)
            minimum (aget ^ints minimums index)
            maximum (aget ^ints maximums index)
            present? (some? footprint)
            unique-count (if present? (.cardinality ^java.util.BitSet footprint) 0)
            address-range (when present? [minimum maximum])
            prior-range (:address-range prior)
            movement (when (and address-range prior-range)
                       (- minimum (first prior-range)))
            repeat? (and present?
                         (:bitset prior)
                         (.equals ^java.util.BitSet footprint
                                  ^java.util.BitSet (:bitset prior)))
            frame (cond-> (assoc (nth frames index)
                                 :ram-unique-address-count unique-count
                                 :ram-address-range address-range
                                 :ram-address-span (if present?
                                                     (inc (- maximum minimum))
                                                     0)
                                 :ram-footprint-id
                                 (when present?
                                   (format "ram-%08x-%d-%04x-%04x"
                                           (bit-and (.hashCode ^java.util.BitSet footprint)
                                                    0xffffffff)
                                           unique-count minimum maximum))
                                 :ram-footprint-repeat? repeat?)
                    movement (assoc :ram-frontier-movement movement
                                    :ram-frontier-direction
                                    (cond
                                      (pos? movement) :forward
                                      (neg? movement) :backward
                                      :else :stationary))
                    true (merge (destination-profile footprint maximum
                                                     destination-floor)))]
        (recur (inc index)
               {:bitset footprint :address-range address-range}
               (conj result frame))))))

(defn- bump-pc-count!
  [^objects counts index pc]
  (let [counts-map (or (aget counts index)
                       (let [counts-map (java.util.HashMap.)]
                         (aset counts index counts-map)
                         counts-map))]
    (.put counts-map pc (inc (long (.getOrDefault counts-map pc 0))))))

(defn feature-write-summary
  ([writes-source frames code-addresses configuration global-start]
   (feature-write-summary writes-source frames code-addresses configuration
                          global-start nil))
  ([writes-source frames code-addresses configuration global-start ports]
   (let [frame-count (count frames)
         write-counts (int-array frame-count)
         ram-write-counts (int-array frame-count)
         changed-write-counts (int-array frame-count)
         overwritten-code-write-counts (int-array frame-count)
         iec-register-write-counts (int-array frame-count)
         serial-write-counts (int-array frame-count)
         cia1-write-counts (int-array frame-count)
         sid-write-counts (int-array frame-count)
         vic-write-counts (int-array frame-count)
         d012-write-counts (int-array frame-count)
         d012-samples (object-array frame-count)
         vic-samples (object-array frame-count)
         ;; Per-domain writing PCs let later stages identify the raster and
         ;; music routines by the hardware they poke, symmetrically.
         vic-pc-counts (object-array frame-count)
         sid-pc-counts (object-array frame-count)
        ;; Footprints contain ordinary RAM writes only, so peripheral writes
        ;; cannot make a raster routine resemble a decrunch destination.
         footprints (object-array frame-count)
         minimums (int-array frame-count)
         maximums (int-array frame-count)
         _ (dotimes [index frame-count]
             (aset-int minimums index 0x10000)
             (aset-int maximums index -1))
         frame-index (volatile! 0)
         max-vic-writes (:max-vic-writes-per-frame configuration)
         unique-addresses (java.util.HashSet.)
         counters (volatile! {:write-count 0
                              :ram-write-count 0
                              :changed-write-count 0
                              :overwritten-code-write-count 0
                              :iec-register-write-count 0
                              :domains {}})]
     (doseq [{:keys [event-index pc address value old-value raster-line cpu-cycle]}
             (writes-source)]
       (.add unique-addresses address)
       (let [port (when (and ports event-index)
                    (aget ^ints ports event-index))
             domain (write-domain-at address port)
             changed? (not= value old-value)
             code-write? (aget ^booleans code-addresses address)
             iec-write? (and (or (nil? port) (stream/io-visible? port))
                             (address-in-ranges? address
                                                 (:iec-register-ranges configuration)))]
         (vswap! counters
                 (fn [summary]
                   (cond-> (-> summary
                               (update :write-count inc)
                               (update-in [:domains domain] (fnil inc 0)))
                     (= domain :memory) (update :ram-write-count inc)
                     changed? (update :changed-write-count inc)
                     code-write? (update :overwritten-code-write-count inc)
                     iec-write? (update :iec-register-write-count inc))))
         (let [index (frame-index-for-event frames event-index @frame-index)]
           (vreset! frame-index index)
           (when (and (< index frame-count)
                      (>= event-index (:start-index (nth frames index))))
             (aset-int write-counts index
                       (inc (aget write-counts index)))
             (when (= domain :memory)
               (aset-int ram-write-counts index
                         (inc (aget ram-write-counts index)))
               (let [footprint (or (aget footprints index)
                                   (let [footprint (java.util.BitSet. 65536)]
                                     (aset footprints index footprint)
                                     footprint))]
                 (.set ^java.util.BitSet footprint address)
                 (aset-int minimums index (min address (aget minimums index)))
                 (aset-int maximums index (max address (aget maximums index)))))
             (when changed?
               (aset-int changed-write-counts index
                         (inc (aget changed-write-counts index))))
             (when code-write?
               (aset-int overwritten-code-write-counts index
                         (inc (aget overwritten-code-write-counts index))))
             (when iec-write?
               (aset-int iec-register-write-counts index
                         (inc (aget iec-register-write-counts index))))
             (when (and iec-write?
                        (not= (bit-and (int value) serial-bus-mask)
                              (bit-and (int old-value) serial-bus-mask)))
               (aset-int serial-write-counts index
                         (inc (aget serial-write-counts index))))
             (case domain
               :vic (bump-pc-count! vic-pc-counts index pc)
               :sid (bump-pc-count! sid-pc-counts index pc)
               nil)
             (when (= domain :cia1)
               (aset-int cia1-write-counts index
                         (inc (aget cia1-write-counts index))))
             (when (= domain :sid)
               (aset-int sid-write-counts index
                         (inc (aget sid-write-counts index))))
             (when (= domain :vic)
               (aset-int vic-write-counts index
                         (inc (aget vic-write-counts index)))
               (let [sample [address value raster-line cpu-cycle]
                     samples (or (aget vic-samples index) [])]
                 (when (< (count samples) max-vic-writes)
                   (aset vic-samples index (conj samples sample)))))
             (when (and (= domain :vic)
                        (= (video/vic-register-address address) 0xd012))
               (aset-int d012-write-counts index
                         (inc (aget d012-write-counts index)))
               (let [sample {:event-index (+ global-start event-index)
                             :value value
                             :raster-line raster-line
                             :cpu-cycle cpu-cycle}
                     samples (or (aget d012-samples index) [])]
                 (when (< (count samples) max-vic-writes)
                   (aset d012-samples index (conj samples sample)))))))))
     {:summary (assoc @counters :unique-address-count (.size unique-addresses))
      :frames
      (finalize-frame-footprints
       (mapv (fn [index frame]
               (assoc frame
                      :write-count (aget write-counts index)
                      :ram-write-count (aget ram-write-counts index)
                      :changed-write-count (aget changed-write-counts index)
                      :overwritten-code-write-count
                      (aget overwritten-code-write-counts index)
                      :iec-register-write-count
                      (aget iec-register-write-counts index)
                      :serial-write-count (aget serial-write-counts index)
                      :cia1-write-count (aget cia1-write-counts index)
                      :sid-write-count (aget sid-write-counts index)
                      :vic-write-count (aget vic-write-counts index)
                      :d012-write-count (aget d012-write-counts index)
                      :d012-writes (or (aget d012-samples index) [])
                      :vic-write-sample (or (aget vic-samples index) [])
                      :vic-write-pcs (into {} (aget vic-pc-counts index))
                      :sid-write-pcs (into {} (aget sid-pc-counts index))))
             (range frame-count)
             frames)
       footprints minimums maximums
       (:destination-min-address configuration))})))

(defn execution-after-write-summary
  "Attach per-unit evidence that ordinary RAM written by the CPU was executed
  later in the same raw chunk.

  The last-write table is deliberately bounded to the C64 address space. A
  write at event N becomes visible only to later events, matching the CPU's
  instruction timing. The source unit receives the count, so a decruncher can
  be recognized when it hands off to its freshly produced code without
  retaining per-event data after this feature pass."
  ([events writes-source frames]
   (execution-after-write-summary events writes-source frames nil))
  ([events writes-source frames ports]
   (let [frame-count (count frames)
         execution-counts (int-array frame-count)
         executed-addresses (object-array frame-count)
         last-write-frame (int-array 65536)
         _ (java.util.Arrays/fill last-write-frame -1)
         instructions (:instructions events)
         frame-index* (volatile! 0)]
     (loop [event-index 0
            instruction-ids (seq (stream/stream-instruction-ids events))
            writes (seq (writes-source))]
       (when-let [instruction-id (first instruction-ids)]
         (let [[writes frame-index]
               (loop [writes writes
                      frame-index @frame-index*]
                 (if (and writes (< (:event-index (first writes)) event-index))
                   (let [write (first writes)
                         frame-index (frame-index-for-event frames
                                                            (:event-index write)
                                                            frame-index)]
                     (when (and (< frame-index frame-count)
                                (= :memory
                                   (write-domain-at
                                    (:address write)
                                    (when (and ports (:event-index write))
                                      (aget ^ints ports (:event-index write))))))
                       (aset-int last-write-frame (:address write) frame-index))
                     (recur (next writes) frame-index))
                   [writes frame-index]))
               _ (vreset! frame-index* frame-index)
               pc (:pc (nth instructions instruction-id))
               source-frame (aget last-write-frame pc)]
           (when (not= -1 source-frame)
             (aset-int execution-counts source-frame
                       (inc (aget execution-counts source-frame)))
             (let [addresses (or (aget executed-addresses source-frame)
                                 (let [addresses (java.util.BitSet. 65536)]
                                   (aset executed-addresses source-frame addresses)
                                   addresses))]
               (.set ^java.util.BitSet addresses pc)))
           (recur (inc event-index) (next instruction-ids) writes))))
     (mapv (fn [index frame]
             (assoc frame
                    :written-code-execution-count (aget execution-counts index)
                    :written-code-address-count
                    (if-let [addresses (aget executed-addresses index)]
                      (.cardinality ^java.util.BitSet addresses)
                      0)))
           (range frame-count)
           frames))))

(defn frame-kernal-counters
  [events frames kernal-iec-ranges]
  (let [execution-counts (int-array (count frames))
        transfer-counts (int-array (count frames))
        frame-index (volatile! 0)
        instructions (:instructions events)]
    (doseq [[event-index instruction-id]
            (map-indexed vector (stream/stream-instruction-ids events))]
      (let [index (frame-index-for-event frames event-index @frame-index)]
        (vreset! frame-index index)
        (when (< index (count frames))
          (let [{:keys [pc mnemonic mode] :as instruction}
                (nth instructions instruction-id)
                transfer? (and (contains? #{"JSR" "JMP"} mnemonic)
                               (= :abs mode)
                               (address-in-ranges?
                                (stream/absolute-operand instruction)
                                kernal-iec-ranges))]
            (when (address-in-ranges? pc kernal-iec-ranges)
              (aset-int execution-counts index
                        (inc (aget execution-counts index))))
            (when transfer?
              (aset-int transfer-counts index
                        (inc (aget transfer-counts index))))))))
    (mapv (fn [index frame]
            (assoc frame
                   :kernal-iec-execution-count
                   (aget execution-counts index)
                   :kernal-iec-transfer-count
                   (aget transfer-counts index)))
          (range (count frames))
          frames)))

(def ^:private serial-read-mnemonics
  #{"LDA" "LDX" "LDY" "CMP" "CPX" "CPY" "BIT"
    "AND" "ORA" "EOR" "ADC" "SBC"})

(defn frame-serial-access-counters
  "Count per-unit *reads* of the serial/IEC register range.

  A C64 load samples the bus by reading `$dd00`, so reads are the transfer
  signal. Serial writes are counted separately from inferred writes and are
  filtered to changes in the serial bits, because raster routines also store to
  `$dd00` for VIC bank switching."
  [events frames serial-ranges]
  (let [frame-count (count frames)
        read-counts (int-array frame-count)
        frame-index (volatile! 0)
        instructions (:instructions events)]
    (doseq [[event-index instruction-id]
            (map-indexed vector (stream/stream-instruction-ids events))]
      (let [index (frame-index-for-event frames event-index @frame-index)]
        (vreset! frame-index index)
        (when (< index frame-count)
          (let [{:keys [mnemonic mode] :as instruction}
                (nth instructions instruction-id)]
            (when (and (= :abs mode)
                       (contains? serial-read-mnemonics mnemonic)
                       (address-in-ranges?
                        (stream/absolute-operand instruction) serial-ranges))
              (aset-int read-counts index (inc (aget read-counts index))))))))
    (mapv (fn [index frame]
            (let [reads (aget read-counts index)
                  writes (long (:serial-write-count frame 0))]
              (assoc frame
                     :serial-read-count reads
                     :serial-access-count (+ reads writes))))
          (range frame-count)
          frames)))

(defn finalize-feature-frames
  [frames]
  (mapv (fn [frame]
          ;; Write windows carry behavioral evidence but no IRQ topology. They
          ;; must never manufacture a stable IRQ fingerprint or demopart.
          (if (= :irq-frame (:unit-kind frame))
            (let [signature {:irq-entry-pcs (:irq-entry-pcs frame)
                             :irq-triggers (:irq-triggers frame)
                             :d012-writes (mapv #(select-keys % [:value :raster-line
                                                                 :cpu-cycle])
                                                (:d012-writes frame))
                             :vic-write-count (:vic-write-count frame)}]
              (assoc frame
                     :fingerprint signature
                     :fingerprint-id (feature-fingerprint-id signature)))
            (assoc frame :fingerprint nil :fingerprint-id nil)))
        frames))

(defn rolling-fingerprints
  [frames window-size]
  (mapv (fn [window]
          (let [fingerprint-ids (mapv :fingerprint-id window)
                signature {:frame-fingerprints fingerprint-ids}]
            {:event-range [(first (:event-range (first window)))
                           (second (:event-range (last window)))]
             :frame-count (count window)
             :fingerprint-ids fingerprint-ids
             :fingerprint-id (feature-fingerprint-id signature)}))
        (partition window-size 1 frames)))

(defn boundary-handoff-evidence
  "Persist compact evidence for a write-to-execution handoff at a chunk edge.

  Only the last and first bounded event windows are retained. The ordered
  classifier intersects them across adjacent chunks, so a physical boundary
  cannot terminate a semantic decrunch span."
  ([events writes-source window-events]
   (boundary-handoff-evidence events writes-source window-events nil))
  ([events writes-source window-events ports]
   (let [event-count (:event-count events)
         tail-start (max 0 (- event-count window-events))
         tail-written-addresses
         (->> (writes-source)
              (filter #(and (>= (:event-index %) tail-start)
                            (= :memory
                               (write-domain-at
                                (:address %)
                                (when (and ports (:event-index %))
                                  (aget ^ints ports (:event-index %)))))))
              (map :address)
              distinct
              vec)
         head-executed-addresses
         (loop [event-index 0
                instruction-ids (seq (stream/stream-instruction-ids events))
                addresses #{}]
           (if (or (= event-index window-events)
                   (nil? instruction-ids))
             (vec (sort addresses))
             (recur (inc event-index)
                    (next instruction-ids)
                    (conj addresses
                          (:pc (nth (:instructions events)
                                    (first instruction-ids)))))))]
     {:window-events window-events
      :tail-event-range [tail-start event-count]
      :tail-written-addresses tail-written-addresses
      :head-event-range [0 (min event-count window-events)]
      :head-executed-addresses head-executed-addresses})))

(defn derive-features-chunk
  "Build bounded behavioral features for one raw chunk.

  The result contains exact write/loader counters plus frame-local IRQ/VIC
  fingerprints and rolling windows.  It intentionally makes no semantic
  labels: Phase 4 consumes this evidence with cross-chunk hysteresis."
  ([chunk structure-chunk video-chunk]
   (derive-features-chunk chunk structure-chunk video-chunk {}))
  ([chunk structure-chunk video-chunk configuration]
   (derive-features-chunk chunk structure-chunk video-chunk configuration nil))
  ([chunk structure-chunk video-chunk configuration writes]
   (let [configuration (normalize-feature-configuration configuration)
         events (:events chunk)
         event-count (:event-count events)
         global-start (first (:event-range chunk))
         irq-frames (irq-frames structure-chunk event-count global-start
                                (:raster-lines configuration))
         first-irq-start (some-> irq-frames first :start-index)
         ;; Preserve IRQ frames for segment assembly. A substantial prefix
         ;; before the first IRQ frame—and every no-IRQ chunk—uses bounded
         ;; write windows. A short prefix is a physical-chunk artifact, so
         ;; fold it into the first IRQ unit rather than inserting a tiny
         ;; negative-evidence unit that would break hysteresis across chunks.
         write-windows (if (or (nil? first-irq-start)
                               (>= first-irq-start
                                   (:write-window-events configuration)))
                         (write-windows 0 (or first-irq-start event-count)
                                        global-start
                                        (:write-window-events configuration))
                         [])
         irq-frames (if (and first-irq-start (empty? write-windows))
                      (assoc-in irq-frames [0 :start-index] 0)
                      irq-frames)
         irq-frames (if (and first-irq-start (empty? write-windows))
                      (assoc-in irq-frames [0 :event-range 0] global-start)
                      irq-frames)
         ;; Classifier units are chronological and non-overlapping, avoiding
         ;; double-counting writes in both a frame and a window over it.
         classifier-units (into write-windows irq-frames)
         writes-source (or writes (video/writes-source chunk (video/derive-writes-chunk chunk)))
         ports (stream/cpu-port-values chunk event-count)
         {:keys [summary frames]}
         (feature-write-summary writes-source classifier-units
                                (executed-code-addresses events)
                                configuration global-start ports)
         classifier-units (execution-after-write-summary events writes-source frames
                                                         ports)
         classifier-units (frame-kernal-counters events classifier-units
                                                 (:kernal-iec-ranges configuration))
         classifier-units (frame-serial-access-counters
                           events classifier-units
                           (:iec-register-ranges configuration))
         classifier-units (finalize-feature-frames classifier-units)
         irq-frames (filterv #(= :irq-frame (:unit-kind %)) classifier-units)
         write-windows (filterv #(= :write-window (:unit-kind %)) classifier-units)
         rolling (rolling-fingerprints irq-frames
                                       (:fingerprint-window-frames configuration))
         instruction-counts (kernal-iec-instruction-counts
                             events (:kernal-iec-ranges configuration))
         configurations (get-in video-chunk [:stages :video :vic :configurations])
         handoff (boundary-handoff-evidence
                  events writes-source (:cross-chunk-handoff-events configuration)
                  ports)
         summary (merge summary instruction-counts
                        {:event-count event-count
                         :frame-count (count irq-frames)
                         :frame-fingerprint-count (count irq-frames)
                         :write-window-count (count write-windows)
                         :classifier-unit-count (count classifier-units)
                         :rolling-fingerprint-count (count rolling)
                         :vic-configuration-count (count configurations)
                         :write-density (if (pos? event-count)
                                          (/ (double (:write-count summary))
                                             event-count)
                                          0.0)})]
     (assoc (artifact/derived-chunk-source :omkamra.vice/features-chunk-v1 chunk)
            :stages {:features {:configuration configuration
                                :summary summary
                                :frames irq-frames
                                :write-windows write-windows
                                :classifier-units classifier-units
                                :rolling-fingerprints rolling
                                :handoff handoff}}))))
