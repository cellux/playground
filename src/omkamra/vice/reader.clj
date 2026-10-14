(ns omkamra.vice.reader
  "Shared, thread-safe reader for finalized v2 raw captures.

  All higher analysis layers obtain raw capture data through this namespace;
  they never construct chunk paths or decode `.bin` files themselves. The
  reader owns manifest validation, chunk lookup by global event index, bounded
  LRU caching of decoded chunks and snapshots, nearest-checkpoint replay, and
  logical CPU-write inference.

  Logical write inference reconstructs the underlying RAM, color RAM, CPU-port,
  and device write-shadow state from the nearest committed boundary snapshot and
  replays every traced instruction forward. It emits one mutation per affected
  destination address, including explicit stores, final read-modify-write
  results, `PHA`/`PHP`/`JSR`/`BRK` stack pushes, and hardware-interrupt stack
  pushes detected by the three-byte stack-pointer discontinuity. Writes are
  emitted even when the resulting value equals the prior value, and old values
  are retained only internally for replay.

  The CPU-visible image is assembled from the replayed underlying RAM, the
  immutable ROM bytes of the boundary `.mem` image, device write shadows, and
  the live CPU-port mapping, so effective addresses, indirect targets, and IRQ
  vectors resolve correctly after `$01` changes."
  (:require [clojure.core.cache :as cache]
            [clojure.java.io :as io]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.isa :as isa]
            [omkamra.vice.store :as store])
  (:import [java.nio.file Files]))

(def default-cache-size
  "Default number of recently used chunks whose payloads and snapshots are
  retained. The cache only affects I/O and decoding cost, never results."
  4)

;; Opening and validation ----------------------------------------------------

(defn open!
  "Validate a finalized capture under `dataset-root` and return a reader.

  Options: `:cache-size` (positive integer). Validation hashes every artifact
  once; after that the reader trusts the manifest and verifies sizes lazily."
  ([dataset-root]
   (open! dataset-root {}))
  ([dataset-root {:keys [cache-size] :or {cache-size default-cache-size}}]
   (when-not (pos-int? cache-size)
     (throw (ex-info "Reader :cache-size must be a positive integer"
                     {:reason :invalid-reader-options :cache-size cache-size})))
   (let [capture-directory (io/file dataset-root "capture")
         {:keys [directory manifest]} (store/validate-capture! capture-directory)
         chunks (vec (:chunks manifest))]
     {:dataset-root (.getCanonicalPath (io/file dataset-root))
      :directory directory
      :manifest manifest
      :chunks chunks
      :chunk-starts (mapv (comp first :event-range) chunks)
      :event-count (:event-count manifest)
      :timing-model (:timing-model manifest)
      :cache (atom (cache/lru-cache-factory {} :threshold cache-size))})))

(defn dataset-root [reader] (:dataset-root reader))
(defn capture-id [reader] (:capture-id (:manifest reader)))
(defn manifest [reader] (:manifest reader))
(defn directory [reader] (:directory reader))
(defn event-count [reader] (:event-count reader))
(defn chunk-count [reader] (count (:chunks reader)))
(defn chunk-starts [reader] (:chunk-starts reader))
(defn timing-model [reader] (:timing-model reader))

;; Chunk lookup and caching --------------------------------------------------

(defn- resolve-chunk
  [reader index]
  (let [starts (:chunk-starts reader)
        last-index (dec (count starts))]
    (loop [low 0 high last-index best nil]
      (if (> low high)
        (or best
            (throw (ex-info "Event index is outside the capture"
                            {:reason :event-index-out-of-range
                             :event-index index
                             :event-count (:event-count reader)})))
        (let [mid (quot (+ low high) 2)
              start (nth starts mid)]
          (if (<= start index)
            (recur (inc mid) high (nth (:chunks reader) mid))
            (recur low (dec mid) best)))))))

(defn- cache-load
  [reader key load-fn]
  (let [current @(:cache reader)]
    (if-let [value (cache/lookup current key)]
      (do (swap! (:cache reader) cache/hit key)
          value)
      (let [value (load-fn)]
        (swap! (:cache reader) cache/miss key value)
        value))))

(defn- artifact-file
  ^java.io.File [reader entry kind]
  (io/file (:directory reader) (get-in entry [:artifacts kind :file])))

(defn- chunk-metadata
  [reader entry]
  (cache-load reader [:metadata (:number entry)]
              #(store/read-edn (io/file (:directory reader) (:file entry)))))

(defn- chunk-bytes
  [reader entry]
  (cache-load reader [:events (:number entry)]
              #(Files/readAllBytes (.toPath (artifact-file reader entry :events)))))

(defn- chunk-images
  [reader entry]
  (cache-load reader [:images (:number entry)]
              (fn []
                {:ram (store/read-image (artifact-file reader entry :ram)
                                        store/ram-format-descriptor)
                 :mem (store/read-image (artifact-file reader entry :mem)
                                        store/mem-format-descriptor)})))

(defn- decode-chunk-event
  [entry bytes index]
  (let [start (first (:event-range entry))
        local (- (long index) (long start))]
    (event/decode-event bytes (* local contract/event-record-size))))

(defn- event-at-index
  [reader entry index]
  (decode-chunk-event entry (chunk-bytes reader entry) index))

(defn- global-event-pairs
  "Lazy `[index event]` pairs for `from <= index < to`."
  [reader from to]
  (when (< from to)
    (mapcat
     (fn [entry]
       (let [[start end] (:event-range entry)
             low (max start from)
             high (min end to)]
         (when (< low high)
           (let [bytes (chunk-bytes reader entry)]
             (map (fn [index]
                    [index (decode-chunk-event entry bytes index)])
                  (range low high))))))
     (:chunks reader))))

(defn- boundary-index-for
  [reader index]
  (first (:event-range (resolve-chunk reader index))))

;; Public event access -------------------------------------------------------

(defn boundary-snapshot
  "Return the exact canonical snapshot committed at a chunk boundary.

  `event-index` must be the first event index of a committed chunk."
  [reader event-index]
  (let [entry (resolve-chunk reader event-index)]
    (when-not (= event-index (first (:event-range entry)))
      (throw (ex-info "Not a committed chunk boundary"
                      {:reason :not-a-chunk-boundary
                       :event-index event-index
                       :chunk-start (first (:event-range entry))})))
    (:snapshot (chunk-metadata reader entry))))

(defn chunk-screenshot
  "Return the committed VIC screenshot description of a chunk, or nil.

  The description carries `:file`, the frame geometry (`:width`, `:height`),
  the visible inner rectangle (`:x-offset`, `:y-offset`, `:inner-width`,
  `:inner-height`), and `:bpp`. The PNG itself is the full debug frame,
  border included."
  [reader entry]
  (:screenshot (chunk-metadata reader entry)))

(defn chunk-screenshots
  "Return the committed VIC screenshot descriptions in chunk order.

  Chunks without a screenshot yield nil."
  [reader]
  (mapv #(chunk-screenshot reader %) (:chunks reader)))

(defn screenshot-file
  "Return the `File` of a chunk's screenshot PNG, or nil."
  ^java.io.File [reader entry]
  (when-let [description (chunk-screenshot reader entry)]
    (io/file (:directory reader) (:file description))))

(defn screenshot-bytes
  "Return the bytes of a chunk's screenshot PNG."
  [reader entry]
  (when-let [^java.io.File file (screenshot-file reader entry)]
    (Files/readAllBytes (.toPath file))))

(defn boundary-snapshots
  "Return the committed boundary snapshots in chunk order."
  [reader]
  (mapv #(:snapshot (chunk-metadata reader %)) (:chunks reader)))

(defn chunk-boundaries
  "Return the global event index at the start of every committed chunk."
  [reader]
  (:chunk-starts reader))

(defn scan-events
  "Reduce `rf` over every decoded event whose global index is in `event-range`."
  [reader event-range rf init]
  (let [[start end] (contract/validate-event-range! event-range)]
    (reduce (fn [acc [_ event]] (rf acc event)) init
            (global-event-pairs reader start end))))

(defn read-events
  "Eagerly materialize the events in `event-range`."
  [reader event-range]
  (scan-events reader event-range (fn [acc event] (conj acc event)) []))

(defn event-pairs
  "Lazy `[index event]` pairs for the half-open `event-range`.

  Unlike `read-events`, this decodes each event on demand, so a consumer can
  stream a large range with memory bounded by the reader's chunk cache."
  [reader event-range]
  (let [[start end] (contract/validate-event-range! event-range)]
    (global-event-pairs reader start end)))

;; Primitive event streaming --------------------------------------------------

(def event-field-count
  "Length of the `long-array` used by `reduce-events`."
  12)

(def ^:const ev-index 0)
(def ^:const ev-pc 1)
(def ^:const ev-length 2)
(def ^:const ev-b0 3)
(def ^:const ev-b1 4)
(def ^:const ev-b2 5)
(def ^:const ev-a 6)
(def ^:const ev-x 7)
(def ^:const ev-y 8)
(def ^:const ev-sp 9)
(def ^:const ev-flags 10)
(def ^:const ev-cycle 11)

(defn- byte-at
  ^long [^bytes bytes ^long offset]
  (bit-and (aget bytes offset) 0xff))

(defn- decode-event-into!
  "Decode the event record at `offset` into the primitive `buffer`."
  ^longs [^longs buffer ^bytes bytes ^long offset ^long index]
  (aset buffer ev-index index)
  (aset buffer ev-pc (bit-or (byte-at bytes offset)
                             (bit-shift-left (byte-at bytes (+ offset 1)) 8)))
  (aset buffer ev-length (byte-at bytes (+ offset 2)))
  (aset buffer ev-b0 (byte-at bytes (+ offset 3)))
  (aset buffer ev-b1 (byte-at bytes (+ offset 4)))
  (aset buffer ev-b2 (byte-at bytes (+ offset 5)))
  (aset buffer ev-a (byte-at bytes (+ offset 6)))
  (aset buffer ev-x (byte-at bytes (+ offset 7)))
  (aset buffer ev-y (byte-at bytes (+ offset 8)))
  (aset buffer ev-sp (byte-at bytes (+ offset 9)))
  (aset buffer ev-flags (byte-at bytes (+ offset 10)))
  (aset buffer ev-cycle
        (bit-or (byte-at bytes (+ offset 11))
                (bit-shift-left (byte-at bytes (+ offset 12)) 8)
                (bit-shift-left (byte-at bytes (+ offset 13)) 16)
                (bit-shift-left (byte-at bytes (+ offset 14)) 24)
                (bit-shift-left (byte-at bytes (+ offset 15)) 32)
                (bit-shift-left (byte-at bytes (+ offset 16)) 40)
                (bit-shift-left (byte-at bytes (+ offset 17)) 48)
                (bit-shift-left (byte-at bytes (+ offset 18)) 56)))
  buffer)

(defn reduce-events
  "Reduce `rf` over every event in `event-range`, without per-event allocation.

  `rf` is called as `(rf acc buffer)` where `buffer` is a reusable `long-array`
  whose layout is the `ev-*` slot constants. The buffer is valid only for the
  duration of the call; copy any value that must be retained. Ordering is
  global and crosses raw chunk boundaries. A `reduced` accumulator stops the
  reduction and is dereferenced, mirroring `clojure.core/reduce`."
  [reader event-range rf init]
  (let [[start end] (contract/validate-event-range! event-range)
        buffer (long-array event-field-count)]
    (loop [chunks (seq (:chunks reader)) acc init]
      (if (and (seq chunks) (not (reduced? acc)))
        (let [entry (first chunks)
              [cs ce] (:event-range entry)
              low (max (long cs) (long start))
              high (min (long ce) (long end))
              acc
              (if (< low high)
                (let [^bytes bytes (chunk-bytes reader entry)
                      record-size (long contract/event-record-size)
                      offset (* (- low (long cs)) record-size)]
                  (loop [index (long low) offset offset acc acc]
                    (if (< index high)
                      (let [acc (rf acc (decode-event-into! buffer bytes offset index))]
                        (if (reduced? acc)
                          acc
                          (recur (inc index)
                                 (+ offset record-size)
                                 acc)))
                      acc)))
                acc)]
          (if (reduced? acc) (deref acc) (recur (next chunks) acc)))
        (if (reduced? acc) (deref acc) acc)))))

;; Replay state --------------------------------------------------------------

(defn- io-visible?
  [port]
  (and (pos? (bit-and (int port) 0x03))
       (pos? (bit-and (int port) 0x04))))

(defn- kernal-visible?
  [port]
  (pos? (bit-and (int port) 0x02)))

(defn- basic-visible?
  [port]
  (pos? (bit-and (int port) 0x01)))

(defn- read-device-byte
  [state address]
  (let [device (:device state)]
    (cond
      (<= 0xd000 address 0xd3ff)
      (contract/unsigned (aget ^bytes (:vic device)
                               (mod (- address 0xd000) 47)))
      (<= 0xd400 address 0xd7ff)
      (contract/unsigned (aget ^bytes (:sid device)
                               (mod (- address 0xd400) 32)))
      (<= 0xd800 address 0xdbff)
      (bit-and 0x0f (contract/unsigned
                     (aget ^bytes (:color-ram state) (- address 0xd800))))
      (<= 0xdc00 address 0xdcff)
      (contract/unsigned (aget ^bytes (:cia1 device) (- address 0xdc00)))
      (<= 0xdd00 address 0xddff)
      (contract/unsigned (aget ^bytes (:cia2 device) (- address 0xdd00)))
      :else 0xff)))

(defn- read-cpu-byte
  "A side-effect-free CPU-visible read from the replayed state."
  [state address]
  (let [address (bit-and (int address) 0xffff)
        data (contract/unsigned (aget ^bytes (:ports state) 1))]
    (cond
      (<= address 1)
      (contract/unsigned (aget ^bytes (:ports state) address))
      (and (<= 0xd000 address 0xdfff) (io-visible? data))
      (read-device-byte state address)
      (and (<= 0xa000 address 0xbfff) (basic-visible? data))
      (contract/unsigned (aget ^bytes (:mem state) address))
      (and (<= 0xe000 address 0xffff) (kernal-visible? data))
      (contract/unsigned (aget ^bytes (:mem state) address))
      :else
      (contract/unsigned (aget ^bytes (:ram state) address)))))

(defn- cpu-vector
  [state address]
  (bit-or (read-cpu-byte state address)
          (bit-shift-left (read-cpu-byte state (bit-and (inc address) 0xffff)) 8)))

(defn- make-state
  [reader entry]
  (let [metadata (chunk-metadata reader entry)
        snapshot (:snapshot metadata)
        images (chunk-images reader entry)
        registers (:registers snapshot)
        cpu-port (:cpu-port snapshot)]
    {:ram (aclone ^bytes (:ram images))
     :mem ^bytes (:mem images)
     :color-ram (byte-array (map unchecked-byte (:color-ram snapshot)))
     :ports (byte-array [(unchecked-byte (nth cpu-port 0))
                         (unchecked-byte (nth cpu-port 1))])
     :device {:vic (byte-array (map unchecked-byte (:vic registers)))
              :sid (byte-array (map unchecked-byte (:sid registers)))
              :cia1 (byte-array (map unchecked-byte (:cia1 registers)))
              :cia2 (byte-array (map unchecked-byte (:cia2 registers)))}}))

(defn- write-domain
  [state address]
  (let [data (contract/unsigned (aget ^bytes (:ports state) 1))]
    (cond
      (<= address 1) :cpu-port
      (and (<= 0xd000 address 0xdfff) (io-visible? data))
      (cond
        (<= 0xd800 address 0xdbff) :color-ram
        (<= 0xd000 address 0xd3ff) :vic
        (<= 0xd400 address 0xd7ff) :sid
        (<= 0xdc00 address 0xdcff) :cia1
        (<= 0xdd00 address 0xddff) :cia2
        :else :io)
      :else :ram)))

(defn- read-domain
  [state address]
  (cond
    (<= address 1) :cpu-port
    (and (<= 0xa000 address 0xbfff) (basic-visible? (contract/unsigned (aget ^bytes (:ports state) 1)))) :rom
    (and (<= 0xe000 address 0xffff) (kernal-visible? (contract/unsigned (aget ^bytes (:ports state) 1)))) :rom
    :else (write-domain state address)))

(defn- canonical-address
  [domain address]
  (case domain
    :vic (+ 0xd000 (mod (- address 0xd000) 47))
    :sid (+ 0xd400 (mod (- address 0xd400) 32))
    :cia1 (+ 0xdc00 (mod (- address 0xdc00) 16))
    :cia2 (+ 0xdd00 (mod (- address 0xdd00) 16))
    address))

(defn- register-offset
  [domain address]
  (case domain
    :vic (mod (- address 0xd000) 47)
    :sid (mod (- address 0xd400) 32)
    :cia1 (mod (- address 0xdc00) 16)
    :cia2 (mod (- address 0xdd00) 16)
    :color-ram (- address 0xd800)
    nil))

(defn- make-mutation
  [state address value kind event-index event]
  (let [address (bit-and (int address) 0xffff)
        domain (write-domain state address)]
    {:event-index event-index
     :pc (:pc event)
     :address address
     :value (bit-and (int value) 0xff)
     :kind kind
     :domain domain
     :canonical-address (canonical-address domain address)
     :register (register-offset domain address)
     :global-cycle (:global-cycle event)
     :raster-line (:raster-line event)
     :cpu-cycle (:cpu-cycle event)}))

(defn- make-read
  [state address event]
  (let [address (bit-and (int address) 0xffff)]
    {:event-index (:event-index event)
     :pc (:pc event)
     :address address
     :value (read-cpu-byte state address)
     :kind :read
     :domain (read-domain state address)
     :global-cycle (:global-cycle event)
     :raster-line (:raster-line event)
     :cpu-cycle (:cpu-cycle event)}))

(defn- apply-mutation!
  [state mutation]
  (case (:domain mutation)
    :cpu-port
    (aset-byte ^bytes (:ports state) (int (:address mutation))
               (unchecked-byte (:value mutation)))
    :ram
    (aset-byte ^bytes (:ram state) (int (:address mutation))
               (unchecked-byte (:value mutation)))
    :color-ram
    (aset-byte ^bytes (:color-ram state) (int (:register mutation))
               (unchecked-byte (bit-and (:value mutation) 0x0f)))
    :vic
    (aset-byte ^bytes (:vic (:device state)) (int (:register mutation))
               (unchecked-byte (:value mutation)))
    :sid
    (aset-byte ^bytes (:sid (:device state)) (int (:register mutation))
               (unchecked-byte (:value mutation)))
    :cia1
    (aset-byte ^bytes (:cia1 (:device state)) (int (:register mutation))
               (unchecked-byte (:value mutation)))
    :cia2
    (aset-byte ^bytes (:cia2 (:device state)) (int (:register mutation))
               (unchecked-byte (:value mutation)))
    nil)
  state)

;; Instruction-level inference ----------------------------------------------

(defn- instruction-step
  "Compute one instruction's mutations, selected reads, stack pointer, and PC.

  `read-predicate` is nil when the caller does not consume reads, or an address
  predicate used to avoid allocating records for irrelevant reads."
  [state event event-index next-event read-predicate]
  (let [{:keys [pc instruction-bytes a x sp flags]} event
        ins (isa/decode pc instruction-bytes)
        mnemonic (:mnemonic ins)
        reads (volatile! [])
        mk-read (fn [address]
                  (make-read state address (assoc event :event-index event-index)))
        read (fn [address]
               (let [value (read-cpu-byte state address)]
                 (when (and read-predicate (read-predicate address))
                   (vswap! reads conj (mk-read address)))
                 value))
        sp-after (isa/sp-after mnemonic sp x)
        next-pc (bit-and (+ pc (:width ins)) 0xffff)
        base {:sp-after sp-after :mutations []}
        mk (fn [address value kind]
             (make-mutation state address value kind event-index event))
        stack (fn [delta]
                (bit-and (+ 0x100 sp delta) 0xffff))
        result
        (case (isa/category mnemonic)
          :store
          (let [address (isa/resolve-address read ins event)]
            (assoc base
                   :mutations [(mk address (isa/store-value mnemonic event address)
                                   :store)]
                   :successor next-pc))

          :rmw
          (if (= (:mode ins) :acc)
            (assoc base :successor next-pc)
            (let [address (isa/resolve-address read ins event)
                  old (read address)]
              (assoc base
                     :mutations [(mk address
                                     (isa/rmw-value mnemonic old (isa/carry? event))
                                     :read-modify-write)]
                     :successor next-pc)))

          :push
          (let [value (if (= mnemonic "PHP")
                        (bit-or (bit-and flags 0xff) 0x30)
                        (bit-and a 0xff))]
            (assoc base
                   :mutations [(mk (stack 0) value :stack-push)]
                   :successor next-pc))

          :jsr
          (let [return (+ pc 2)
                target (isa/absolute-operand instruction-bytes)]
            (assoc base
                   :mutations [(mk (stack 0) (bit-shift-right return 8) :stack-push)
                               (mk (stack -1) (bit-and return 0xff) :stack-push)]
                   :successor target))

          :brk
          (let [return (+ pc 2)
                status (if next-event
                         (bit-or (bit-and (:flags next-event) (bit-not 0x14)) 0x30)
                         (bit-or (bit-and flags 0xff) 0x30))]
            (assoc base
                   :mutations [(mk (stack 0) (bit-shift-right return 8) :stack-push)
                               (mk (stack -1) (bit-and return 0xff) :stack-push)
                               (mk (stack -2) status :stack-push)]
                   :successor (cpu-vector state 0xfffe)))

          :pull
          (do
            (read (stack 1))
            (assoc base :successor next-pc))

          :rts
          (let [lo (read (stack 1))
                hi (read (stack 2))]
            (assoc base
                   :successor (bit-and (inc (bit-or (bit-shift-left hi 8) lo))
                                       0xffff)))

          :rti
          (let [_status (read (stack 1))
                lo (read (stack 2))
                hi (read (stack 3))]
            (assoc base
                   :successor (bit-or (bit-shift-left hi 8) lo)))

          :jmp
          (assoc base
                 :successor (if (= (:mode ins) :ind)
                              (isa/jmp-indirect-target read instruction-bytes)
                              (isa/absolute-operand instruction-bytes)))

          :branch
          (assoc base
                 :successor (if (isa/branch-taken? mnemonic event)
                              (isa/relative-target
                               pc (bit-and (int (nth instruction-bytes 1 0)) 0xff))
                              next-pc))

          (do
            (when (isa/reads-memory? mnemonic)
              (when-let [address (isa/resolve-address read ins event)]
                (read address)))
            (assoc base :successor next-pc)))]
    (assoc result :reads @reads)))

(defn- interrupt-status-byte
  [state event]
  (let [flags (bit-and (int (:flags event)) 0xff)
        base (bit-or (bit-and flags (bit-not 0x14)) 0x20)]
    (if (= (:pc event) (cpu-vector state 0xfffe))
      (bit-and base (bit-not 0x04))
      base)))

(defn- interrupt-pushes
  "The three stack pushes of a hardware interrupt whose first handler
  instruction is `event` at `event-index` and whose interrupted successor is
  `successor`."
  [state event-index event successor]
  (let [sp (:sp event)
        status (interrupt-status-byte state event)
        high (bit-and (bit-shift-right successor 8) 0xff)
        low (bit-and successor 0xff)
        mk (fn [address value]
             (make-mutation state address value :interrupt-entry
                            event-index event))]
    [(mk (bit-and (+ 0x100 sp 3) 0xffff) high)
     (mk (bit-and (+ 0x100 sp 2) 0xffff) low)
     (mk (bit-and (+ 0x100 sp 1) 0xffff) status)]))

(defn- interrupt-at?
  "True when the event at `index` is a hardware-interrupt entry, detected as an
  unexplained three-byte stack-pointer drop relative to the previous event."
  [reader index]
  (and (pos? index)
       (< index (:event-count reader))
       (let [previous (event-at-index reader (resolve-chunk reader (dec index))
                                      (dec index))
             current (event-at-index reader (resolve-chunk reader index) index)
             ins (isa/decode (:pc previous) (:instruction-bytes previous))]
         (= (:sp current)
            (bit-and (- (isa/sp-after (:mnemonic ins)
                                      (:sp previous)
                                      (:x previous))
                        3)
                     0xff)))))

(defn- straddling-pushes
  "Reconstruct the interrupt pushes already baked into a chunk boundary
  snapshot by reading them from that snapshot's underlying RAM."
  [reader state event-index]
  (let [entry (resolve-chunk reader event-index)
        event (event-at-index reader entry event-index)
        sp (:sp event)
        ram ^bytes (:ram state)
        read (fn [delta]
               (contract/unsigned
                (aget ram (bit-and (+ 0x100 sp delta) 0xffff))))
        mk (fn [address value]
             (make-mutation state address value :interrupt-entry
                            event-index event))]
    [(mk (bit-and (+ 0x100 sp 3) 0xffff) (read 3))
     (mk (bit-and (+ 0x100 sp 2) 0xffff) (read 2))
     (mk (bit-and (+ 0x100 sp 1) 0xffff) (read 1))]))

(defn- emit-records
  [emit-fn acc emit-range records]
  (if emit-range
    (let [[start end] emit-range]
      (reduce (fn [acc record]
                (let [index (long (:event-index record))]
                  (if (and (<= (long start) index) (< index (long end)))
                    (emit-fn acc record)
                    acc)))
              acc records))
    acc))

(defn- run-replay
  "Replay events from a chunk boundary, emitting records in `emit-range`.

  `emit-mode` is `:writes` (default) or `:accesses`. Returns
  `{:state state :result accumulator}`."
  [reader {:keys [from stop emit-mode read-predicate]
           :or {emit-mode :writes}}
   emit-range emit-fn init]
  (let [read-predicate (when (= :accesses emit-mode)
                         (or read-predicate (constantly true)))
        entry0 (resolve-chunk reader from)
        state0 (make-state reader entry0)
        straddling? (and (pos? from) (interrupt-at? reader from))
        [state0 acc0]
        (if straddling?
          (let [pushes (straddling-pushes reader state0 from)]
            [state0 (emit-records emit-fn init emit-range pushes)])
          [state0 init])
        bytes0 (chunk-bytes reader entry0)
        event0 (when (< from stop)
                 (decode-chunk-event entry0 bytes0 from))]
    (loop [state state0
           acc acc0
           index from
           entry entry0
           bytes bytes0
           event event0]
      (if (>= index stop)
        {:state state :result acc}
        (let [next-index (inc index)
              [next-entry next-bytes]
              (if (and (< next-index (:event-count reader))
                       (< next-index (second (:event-range entry))))
                [entry bytes]
                (when (< next-index (:event-count reader))
                  (let [next-entry (resolve-chunk reader next-index)]
                    [next-entry (chunk-bytes reader next-entry)])))
              next-event (when next-entry
                           (decode-chunk-event next-entry next-bytes next-index))
              step (instruction-step state event index next-event read-predicate)
              records (case emit-mode
                        :writes (:mutations step)
                        :accesses (concat (:reads step) (:mutations step)))
              acc (emit-records emit-fn acc emit-range records)
              state (reduce apply-mutation! state (:mutations step))
              [state acc]
              (if (and next-event
                       (= (:sp next-event)
                          (bit-and (- (:sp-after step) 3) 0xff)))
                (let [pushes (interrupt-pushes state next-index next-event
                                               (:successor step))]
                  [(reduce apply-mutation! state pushes)
                   (emit-records emit-fn acc emit-range pushes)])
                [state acc])]
          (recur state acc next-index next-entry next-bytes next-event))))))

(defn- run-range
  ([reader event-range emit-mode rf init]
   (run-range reader event-range emit-mode nil rf init))
  ([reader event-range emit-mode read-predicate rf init]
   (let [[start end] (contract/validate-event-range! event-range)]
     (if (>= start end)
       init
       (:result (run-replay reader
                            {:from (boundary-index-for reader start)
                             :stop end
                             :emit-mode emit-mode
                             :read-predicate read-predicate}
                            [start end] rf init))))))

(defn scan-writes
  "Reduce `rf` over every inferred logical write in `event-range`.

  Emitted writes are chronological across raw-chunk boundaries; a write is
  never lost or duplicated by a boundary. Each write carries `:event-index`,
  `:pc`, `:address`, `:value`, `:kind`, `:domain`, `:canonical-address`, and
  (for device registers) `:register`."
  [reader event-range rf init]
  (run-range reader event-range :writes rf init))

(defn read-writes
  "Eagerly materialize the inferred logical writes in `event-range`."
  [reader event-range]
  (scan-writes reader event-range (fn [acc write] (conj acc write)) []))

(defn scan-accesses
  "Reduce `rf` over every resolved CPU read and write access in `event-range`.

  Read accesses precede the write they feed where an instruction performs both.
  This is the API feature analysis uses for serial-bus and device reads."
  [reader event-range rf init]
  (run-range reader event-range :accesses rf init))

(defn scan-filtered-accesses
  "Reduce over all writes and only reads whose address satisfies `read-address?`.

  Filtering happens before read-record allocation. This is intended for exact
  consumers interested in a small address domain, while `scan-accesses` remains
  the complete logical data-access API."
  [reader event-range read-address? rf init]
  (run-range reader event-range :accesses read-address? rf init))

(defn read-accesses
  [reader event-range]
  (scan-accesses reader event-range (fn [acc access] (conj acc access)) []))

(defn scan-vic-writes
  [reader event-range rf init]
  (scan-writes reader event-range
               (fn [acc write]
                 (if (= :vic (:domain write)) (rf acc write) acc))
               init))

(defn scan-sid-writes
  [reader event-range rf init]
  (scan-writes reader event-range
               (fn [acc write]
                 (if (= :sid (:domain write)) (rf acc write) acc))
               init))

(defn- summarize-state
  [state event-index]
  {:event-index event-index
   :ram (aclone ^bytes (:ram state))
   :color-ram (aclone ^bytes (:color-ram state))
   :cpu-port [(contract/unsigned (aget ^bytes (:ports state) 0))
              (contract/unsigned (aget ^bytes (:ports state) 1))]
   :device-shadows
   {:vic (mapv contract/unsigned (:vic (:device state)))
    :sid (mapv contract/unsigned (:sid (:device state)))
    :cia1 (mapv contract/unsigned (:cia1 (:device state)))
    :cia2 (mapv contract/unsigned (:cia2 (:device state)))}
   :capabilities
   {:ram :exact
    :color-ram :exact
    :cpu-port :exact
    :vic :write-shadow
    :sid :write-shadow
    :cia1 :write-shadow
    :cia2 :write-shadow
    :rom :boundary-image
    :char-rom :unmodeled
    :opaque-device-state :unmodeled}})

(defn- replay-next-state
  [reader state index]
  (let [event (event-at-index reader (resolve-chunk reader index) index)
        next-index (inc index)
        next-event (when (< next-index (:event-count reader))
                     (event-at-index reader (resolve-chunk reader next-index)
                                     next-index))
        step (instruction-step state event index next-event nil)
        state (reduce apply-mutation! state (:mutations step))]
    (if (and next-event
             (= (:sp next-event) (bit-and (- (:sp-after step) 3) 0xff)))
      (reduce apply-mutation! state
              (interrupt-pushes state next-index next-event (:successor step)))
      state)))

(defn scan-replay-states
  "Reduce over reconstructed states immediately before ordered event indexes.

  Requested indexes must be nondecreasing. Targets in the same raw chunk share
  one replay from that chunk's exact boundary snapshot, avoiding the quadratic
  repeated replay incurred by calling `replay-state` for every target. The
  reducer receives `[acc state]`; each state owns cloned RAM/color arrays."
  [reader event-indexes rf init]
  (let [targets (vec event-indexes)]
    (when-not (apply <= (cons 0 targets))
      (throw (ex-info "Replay-state indexes must be nondecreasing"
                      {:reason :unordered-replay-indexes :indexes targets})))
    (when-let [invalid (first (remove #(<= 0 % (:event-count reader)) targets))]
      (throw (ex-info "Replay-state index is outside the capture"
                      {:reason :invalid-replay-index :event-index invalid
                       :event-count (:event-count reader)})))
    (reduce
     (fn [acc group]
       (let [checkpoint (boundary-index-for reader (first group))
             entry (resolve-chunk reader checkpoint)]
         (loop [state (make-state reader entry)
                index checkpoint
                remaining (seq group)
                acc acc]
           (if-let [target (first remaining)]
             (if (= index target)
               (recur state index (next remaining)
                      (rf acc (summarize-state state target)))
               (recur (replay-next-state reader state index)
                      (inc index) remaining acc))
             acc))))
     init
     (partition-by #(boundary-index-for reader %) targets))))

(defn replay-states
  "Return reconstructed states for ordered event indexes using shared replay."
  [reader event-indexes]
  (scan-replay-states reader event-indexes conj []))

(defn replay-state
  "Reconstruct the exact mutable CPU state immediately before `event-index`.

  Replay starts from the nearest committed boundary snapshot at or before the
  request and applies inferred writes forward. The optional `checkpoint-index`
  selects an explicit earlier committed boundary, which the cross-chunk
  continuity test uses to replay a specific chunk. Returns underlying RAM,
  color RAM, CPU-port bytes, device write shadows, and explicit capability
  metadata."
  ([reader event-index]
   (replay-state reader event-index nil))
  ([reader event-index checkpoint-index]
   (let [from (or checkpoint-index (boundary-index-for reader event-index))]
     (when-not (some #(= from (first (:event-range %))) (:chunks reader))
       (throw (ex-info "Replay checkpoint must be a committed chunk boundary"
                       {:reason :invalid-replay-checkpoint :checkpoint from})))
     (when (< event-index from)
       (throw (ex-info "Replay target precedes its checkpoint"
                       {:reason :invalid-replay-range
                        :event-index event-index :checkpoint from})))
     (let [{:keys [state]}
           (run-replay reader {:from from :stop event-index}
                       nil (fn [acc _] acc) nil)]
       (summarize-state state event-index)))))
