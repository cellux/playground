(ns omkamra.vice.contract
  "Frozen formats, descriptors, and validation helpers for pipeline v2.

  This namespace is the single source of truth for every cross-stage agreement
  that must exist before capture or analysis code is written:

  * format keywords and the little-endian binary convention;
  * the packed 23-byte execution-event record descriptor and its field offsets;
  * unambiguous conversion between VICE's textual trace flags and the native
    6502 status byte;
  * canonical CPU/VIC/SID/CIA register windows, color RAM, and the CPU port;
  * the capture-level VIC value-blacklist policy and its protected registers;
  * the half-open global event-range convention;
  * the fixed, one-line disassembly annotation vocabulary;
  * descriptor and snapshot validators that run without VICE.

  Everything here is deliberately independent of `omkamra.vice.decoder` (v1).
  A malformed descriptor or snapshot throws an `ex-info` with a `:reason`; it
  never silently degrades."
  (:require [clojure.string :as str]))

;; Versions and formats ------------------------------------------------------

(def supported-vice-version "3.9")

(defn format-keyword
  "Derive a stage's own format keyword from its schema version, so the format
  and the version recorded in the generation index cannot drift apart."
  [stage version]
  (keyword "omkamra.vice" (str (name stage) "-v" version)))

(def capture-format :omkamra.vice/capture-v2)
(def event-stream-format :omkamra.vice/exec-events-v2)
(def ram-image-format :omkamra.vice/ram-image-v2)
(def mem-image-format :omkamra.vice/mem-image-v2)
(def snapshot-format :omkamra.vice/snapshot-v2)
(def chunk-format :omkamra.vice/chunk-v2)
(def dedupe-version 4)
(def dedupe-format (format-keyword :dedupe dedupe-version))
(def analysis-format :omkamra.vice/analysis-v2)
(def stage-format :omkamra.vice/analysis-stage-v2)
(def segment-format :omkamra.vice/semantic-segment-v2)

(def memory-image-size 65536)
(def color-ram-size 1024)

(def byte-order :little-endian)

;; Little-endian primitives --------------------------------------------------

(defn unsigned
  "Coerce a signed JVM byte (or any integer) to an unsigned 0..255 value."
  [x]
  (bit-and (int x) 0xff))

(defn u8?
  [x]
  (and (integer? x) (<= 0 x 0xff)))

(defn u16?
  [x]
  (and (integer? x) (<= 0 x 0xffff)))

(defn u64?
  [x]
  (and (integer? x) (<= 0 x 0xffffffffffffffff)))

(defn decode-u16
  "Decode a little-endian unsigned 16-bit integer from `bytes` at `offset`."
  [^bytes bytes ^long offset]
  (bit-or (unsigned (aget bytes offset))
          (bit-shift-left (unsigned (aget bytes (inc offset))) 8)))

(defn decode-u32
  "Decode a little-endian unsigned 32-bit integer from `bytes` at `offset`."
  [^bytes bytes ^long offset]
  (bit-or (long (decode-u16 bytes offset))
          (bit-shift-left (long (decode-u16 bytes (+ offset 2))) 16)))

(defn decode-u64
  "Decode a little-endian unsigned 64-bit integer from `bytes` at `offset`."
  [^bytes bytes ^long offset]
  (bit-or (decode-u32 bytes offset)
          (bit-shift-left (long (decode-u32 bytes (+ offset 4))) 32)))

(defn encode-u16
  "Little-endian 2-byte encoding of `value`."
  [^long value]
  (byte-array [(unchecked-byte (bit-and value 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 8) 0xff))]))

(defn encode-u32
  "Little-endian 4-byte encoding of `value`."
  [^long value]
  (byte-array [(unchecked-byte (bit-and value 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 8) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 16) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 24) 0xff))]))

(defn encode-u64
  "Little-endian 8-byte encoding of `value`."
  [^long value]
  (byte-array [(unchecked-byte (bit-and value 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 8) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 16) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 24) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 32) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 40) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 48) 0xff))
               (unchecked-byte (bit-and (bit-shift-right value 56) 0xff))]))

;; 6502 status flags ---------------------------------------------------------

(def status-flag-bits
  "Native 6502 status-bit positions.

  Bit 5 is the documented unused bit. VICE's monitor does not force it and the
  textual trace renders it as `-`, so v2 stores it exactly as observed: a
  captured status byte compares equal to VICE's `FL` register without masking."
  {:n 0x80 :v 0x40 :unused 0x20 :b 0x10 :d 0x08 :i 0x04 :z 0x02 :c 0x01})

(def trace-flag-template
  "The eight-character template VICE uses for trace flags (mon_register6502.c)."
  "NV-BDIZC")

(def ^:private trace-flag-letters
  [\N \V nil \B \D \I \Z \C])

(def ^:private trace-flag-set-bit
  [0x80 0x40 nil 0x10 0x08 0x04 0x02 0x01])

(defn trace-flags->status-byte
  "Convert an eight-character VICE trace flag string to the native status byte.

  Position 2 is the unused bit and is always rendered as `-`. Every other
  position is either its canonical flag letter (case-insensitive, bit set) or
  `.` (bit clear). Any other character is malformed and throws."
  [flags]
  (when-not (and (string? flags) (= 8 (count flags)))
    (throw (ex-info "Trace flags must be an eight-character string"
                    {:reason :invalid-trace-flags :flags flags})))
  (reduce
   (fn [status [index character]]
     (let [letter (nth trace-flag-letters index)
           bit (nth trace-flag-set-bit index)]
       (cond
         (nil? letter)
         (if (= \- character)
           status
           (throw (ex-info "Malformed unused trace flag position"
                           {:reason :invalid-trace-flags
                            :flags flags :position index})))

         (or (= character letter)
             (= (Character/toUpperCase ^Character character)
                (Character/toUpperCase ^Character letter)))
         (bit-or status bit)

         (= \. character)
         status

         :else
         (throw (ex-info "Unknown trace flag character"
                         {:reason :invalid-trace-flags
                          :flags flags :position index :character character})))))
   0
   (map-indexed vector flags)))

(defn status-byte->trace-flags
  "Render a native status byte as VICE's eight-character trace flag string."
  [status]
  (when-not (u8? status)
    (throw (ex-info "Status byte must be an unsigned 8-bit value"
                    {:reason :invalid-status-byte :status status})))
  (apply str
         (map-indexed
          (fn [index letter]
            (let [bit (nth trace-flag-set-bit index)]
              (cond
                (nil? letter) \-
                (not (zero? (bit-and status bit))) letter
                :else \.)))
          trace-flag-letters)))

;; Packed execution-event record --------------------------------------------

(def event-record-fields
  "Field layout of the packed v2 execution-event record.

  Offsets are byte offsets from the start of the record. Instruction bytes are
  stored in a fixed three-byte field; positions at or beyond
  `:instruction-length` are zero-filled and ignored."
  [{:key :pc :type :u16 :offset 0 :width 2}
   {:key :instruction-length :type :u8 :offset 2 :width 1}
   {:key :instruction-bytes :type :u8-vector :offset 3 :width 3 :length 3}
   {:key :a :type :u8 :offset 6 :width 1}
   {:key :x :type :u8 :offset 7 :width 1}
   {:key :y :type :u8 :offset 8 :width 1}
   {:key :sp :type :u8 :offset 9 :width 1}
   {:key :flags :type :u8 :offset 10 :width 1}
   {:key :global-cycle :type :u64 :offset 11 :width 8}
   {:key :raster-line :type :u16 :offset 19 :width 2}
   {:key :cpu-cycle :type :u16 :offset 21 :width 2}])

(def event-record-size 23)
(def max-instruction-bytes 3)

(def event-record-descriptor
  {:format event-stream-format
   :record-size event-record-size
   :byte-order byte-order
   :padding :none
   :max-instruction-bytes max-instruction-bytes
   :status-flags status-flag-bits
   :fields event-record-fields})

(defn- field-end
  [{:keys [offset width]}]
  (+ offset width))

(defn validate-event-record-descriptor!
  "Validate a packed event-record descriptor, including every field offset.

  Rejects overlapping fields, gaps, a wrong total size, and a wrong byte order
  so that schema arithmetic cannot silently drift."
  [descriptor]
  (when-not (map? descriptor)
    (throw (ex-info "Event record descriptor must be a map"
                    {:reason :invalid-event-descriptor :descriptor descriptor})))
  (when-not (= byte-order (:byte-order descriptor))
    (throw (ex-info "Event record descriptor must be little-endian"
                    {:reason :invalid-event-descriptor
                     :byte-order (:byte-order descriptor)})))
  (when-not (= event-record-size (:record-size descriptor))
    (throw (ex-info "Event record descriptor has the wrong record size"
                    {:reason :invalid-event-descriptor
                     :record-size (:record-size descriptor)
                     :expected event-record-size})))
  (let [fields (:fields descriptor)
        sorted (sort-by :offset fields)]
    (when-not (= (count fields) (count sorted))
      (throw (ex-info "Event record descriptor has duplicate fields"
                      {:reason :invalid-event-descriptor})))
    (reduce
     (fn [cursor field]
       (when-not (= cursor (:offset field))
         (throw (ex-info "Event record field offset is not contiguous"
                         {:reason :invalid-event-descriptor
                          :field (:key field)
                          :offset (:offset field)
                          :expected cursor})))
       (field-end field))
     0
     sorted)
    (let [total (field-end (last sorted))]
      (when-not (= total event-record-size)
        (throw (ex-info "Event record fields do not cover the record size"
                        {:reason :invalid-event-descriptor
                         :covered total
                         :expected event-record-size}))))
    (let [instruction (first (filter #(= :instruction-bytes (:key %)) fields))]
      (when-not (and instruction
                     (= max-instruction-bytes (:length instruction))
                     (= max-instruction-bytes (:width instruction)))
        (throw (ex-info "Instruction-byte field must be three bytes wide"
                        {:reason :invalid-event-descriptor
                         :field instruction}))))
    descriptor))

(defn event-record-descriptor-valid?
  [descriptor]
  (try
    (validate-event-record-descriptor! descriptor)
    true
    (catch clojure.lang.ExceptionInfo _ false)))

;; Canonical register windows ------------------------------------------------

(def register-windows
  "Canonical, reserved-position-free register windows per device.

  `:base` is the C64 address of offset 0, `:size` is the fixed vector length,
  and `:reserved` lists offsets that no register occupies but which stay in
  the vector so later offsets never move."
  {:vic {:base 0xd000 :size 47 :reserved #{}}
   :sid {:base 0xd400 :size 32 :reserved #{29 30 31}}
   :cia1 {:base 0xdc00 :size 16 :reserved #{}}
   :cia2 {:base 0xdd00 :size 16 :reserved #{}}})

(def color-ram-window
  {:base 0xd800 :size color-ram-size :address-space :cpu})

(def cpu-port-window
  {:base 0x0000 :size 2 :address-space :main})

(defn validate-register-windows!
  "Validate canonical register windows: window sizing, reserved offsets, and
  that reserved offsets fall inside their window."
  [windows]
  (when-not (map? windows)
    (throw (ex-info "Register windows must be a map"
                    {:reason :invalid-register-windows :windows windows})))
  (doseq [[domain {:keys [base size reserved]}] windows]
    (when-not (and (u16? base) (pos-int? size))
      (throw (ex-info "Register window needs an address base and size"
                      {:reason :invalid-register-windows
                       :domain domain :base base :size size})))
    (when-not (= size (count (distinct (range base (+ base size)))))
      (throw (ex-info "Register window extends past the address space"
                      {:reason :invalid-register-windows
                       :domain domain :base base :size size})))
    (doseq [offset reserved]
      (when-not (<= 0 offset (dec size))
        (throw (ex-info "Reserved register offset lies outside its window"
                        {:reason :invalid-register-windows
                         :domain domain :offset offset :size size})))))
  windows)

;; VIC value-blacklist policy ------------------------------------------------

(def required-registers
  "Registers whose values are required to reconstruct VIC asset layout.

  A capture-level blacklist may never contain any of these, because omitting
  them would make screen, charset, bitmap, or bank geometry unknowable."
  #{0xd011 0xd016 0xd018 0xdd00})

(def default-vic-value-blacklist
  "Default VIC registers whose values are omitted from the video value stream.

  Sprite coordinates and enables, border/background, and sprite colours are
  display state: they are needed neither for asset-layout reconstruction nor
  for the structural write shape, so their values are omitted. The writes
  themselves remain present with order, timing, and global event index."
  (into (set (range 0xd000 0xd011))
        (concat [0xd015 0xd017]
                (range 0xd020 0xd02f))))

(defn validate-vic-value-blacklist!
  "Reject a blacklist which omits a register required for VIC reconstruction."
  [blacklist]
  (when-not (set? blacklist)
    (throw (ex-info "VIC value blacklist must be a set of addresses"
                    {:reason :invalid-vic-blacklist :blacklist blacklist})))
  (doseq [address blacklist]
    (when-not (u16? address)
      (throw (ex-info "VIC blacklist address must be an unsigned 16-bit value"
                      {:reason :invalid-vic-blacklist :address address})))
    (when (contains? required-registers address)
      (throw (ex-info "VIC blacklist omits a register required for reconstruction"
                      {:reason :protected-register-blacklisted
                       :address address}))))
  blacklist)

;; Snapshot schema -----------------------------------------------------------

(def register-semantics
  "Observed acquisition semantics per snapshot domain.

  `:hardware-read` means the value is what the device returns on a
  side-effect-free read, `:vice-latch` means VICE returns the last value
  written to a write-only register, and `:capture-shadow` means the capture
  must maintain the value from traced writes. The feasibility probe pins the
  concrete classification for the supported VICE version."
  {:underlying-ram :hardware-read
   :cpu-visible :hardware-read
   :color-ram :hardware-read
   :cpu-port :hardware-read
   :vic :hardware-read
   :sid :vice-latch
   :cia1 :hardware-read
   :cia2 :hardware-read})

(defn- fail-snapshot!
  [reason data]
  (throw (ex-info "Invalid v2 snapshot" (assoc data :reason reason))))

(defn- valid-register-vector?
  [expected-length values]
  (and (vector? values)
       (= expected-length (count values))
       (every? u8? values)))

(defn validate-snapshot!
  "Validate a canonical v2 snapshot.

  Checks the required CPU, timing, register, CPU-port, color-RAM, and semantics
  sections against the canonical windows. Returns the snapshot or throws."
  [snapshot]
  (when-not (map? snapshot)
    (throw (ex-info "Snapshot must be a map"
                    {:reason :invalid-snapshot :snapshot snapshot})))
  (let [cpu (:cpu snapshot)
        timing (:timing snapshot)
        registers (:registers snapshot)
        cpu-port (:cpu-port snapshot)
        color-ram (:color-ram snapshot)]
    (when-not (map? cpu)
      (fail-snapshot! :missing-cpu {}))
    (doseq [[key predicate] [[:pc u16?] [:a u8?] [:x u8?] [:y u8?]
                             [:sp u8?] [:flags u8?]]]
      (when-not (predicate (get cpu key))
        (fail-snapshot! :invalid-cpu-register
                        {:register key :value (get cpu key)})))
    (when-not (map? timing)
      (fail-snapshot! :missing-timing {}))
    (doseq [[key predicate] [[:global-cycle u64?]
                             [:raster-line u16?]
                             [:cpu-cycle u16?]]]
      (when-not (predicate (get timing key))
        (fail-snapshot! :invalid-timing
                        {:field key :value (get timing key)})))
    (when-not (map? registers)
      (fail-snapshot! :missing-registers {}))
    (doseq [[domain {:keys [size]}] register-windows]
      (when-not (valid-register-vector? size (get registers domain))
        (fail-snapshot! :invalid-register-window
                        {:domain domain
                         :expected-size size
                         :value (get registers domain)})))
    (when-not (valid-register-vector? (:size cpu-port-window) cpu-port)
      (fail-snapshot! :invalid-cpu-port
                      {:expected-size (:size cpu-port-window)
                       :value cpu-port}))
    (when-not (valid-register-vector? color-ram-size color-ram)
      (fail-snapshot! :invalid-color-ram
                      {:expected-size color-ram-size
                       :value color-ram}))
    (let [semantics (:semantics snapshot)]
      (when-not (map? semantics)
        (fail-snapshot! :missing-semantics {}))
      (doseq [domain (keys register-semantics)]
        (let [value (get semantics domain)]
          (when-not (contains? #{:hardware-read :vice-latch :capture-shadow} value)
            (fail-snapshot! :invalid-semantics
                            {:domain domain :value value}))))))
  snapshot)

;; Half-open event ranges ----------------------------------------------------

(defn event-range?
  "True when `value` is a two-element half-open `[start end)` event range."
  [value]
  (and (vector? value)
       (= 2 (count value))
       (every? (fn [x] (and (integer? x) (<= 0 x))) value)
       (<= (first value) (second value))))

(defn validate-event-range!
  [value]
  (when-not (event-range? value)
    (throw (ex-info "Event range must be a half-open [start end) vector"
                    {:reason :invalid-event-range :event-range value})))
  value)

(defn range-length
  [event-range]
  (validate-event-range! event-range)
  (- (second event-range) (first event-range)))

(defn range-contains?
  [event-range index]
  (validate-event-range! event-range)
  (and (<= (first event-range) index) (< index (second event-range))))

(defn ranges-overlap?
  [a b]
  (validate-event-range! a)
  (validate-event-range! b)
  (and (< (first a) (second b)) (< (first b) (second a))))

(defn ranges-adjacent?
  [a b]
  (validate-event-range! a)
  (validate-event-range! b)
  (= (second a) (first b)))

;; Disassembly annotation vocabulary -----------------------------------------

(def annotation-keys
  "Fixed annotation identifiers. Every annotation is one comment line.
  `:width` is the minimum zero-padded width; `:variable` means natural width."
  [{:id "B" :key :block :description "global block id" :width :variable}
   {:id "C" :key :context :description "context: m, or i<hex> for IRQ handler"
    :width :variable}
   {:id "P" :key :predecessors :description "selected-context predecessors"
    :width 2}
   {:id "S" :key :successors :description "selected-context successors"
    :width 2}
   {:id "E" :key :executions :description "occurrences in the selected range"
    :width :variable}
   {:id "R" :key :repeats :description "observed back-edge repeats"
    :width :variable}])

(def annotation-max-width 79)

(defn- annotation-value
  [{:keys [id key width]} annotation]
  (let [value (get annotation key)]
    (when (nil? value)
      (throw (ex-info "Annotation is missing a required field"
                      {:reason :invalid-annotation :id id :key key})))
    (if (= :variable width)
      (str value)
      (let [text (str value)]
        (when (> (count text) width)
          (throw (ex-info "Annotation value exceeds its fixed width"
                          {:reason :invalid-annotation
                           :id id :value value :width width})))
        (str (apply str (repeat (- width (count text)) \0)) text)))))

(defn format-annotation
  "Render one fixed, terse disassembly annotation comment line.

  The result always starts with `; ` and always fits on one comment line."
  [annotation]
  (let [parts (map #(str (:id %) ":" (annotation-value % annotation))
                   annotation-keys)
        line (str "; " (str/join " " parts))]
    (when (> (count line) annotation-max-width)
      (throw (ex-info "Annotation does not fit on one comment line"
                      {:reason :invalid-annotation
                       :line line
                       :max-width annotation-max-width})))
    line))

;; Aggregate validation ------------------------------------------------------

(defn validate-contracts!
  "Validate every frozen v2 contract that can be checked without VICE."
  []
  (validate-event-record-descriptor! event-record-descriptor)
  (validate-register-windows! register-windows)
  (validate-vic-value-blacklist! default-vic-value-blacklist)
  true)
