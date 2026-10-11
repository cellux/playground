(ns omkamra.vice.event
  "Packed v2 execution-event codec and trace-record adaptation.

  Events are encoded as the fixed little-endian 23-byte record described by
  `omkamra.vice.contract/event-record-descriptor`. Encoding validates every
  field and zero-fills instruction-byte positions beyond the instruction
  length; decoding restores only the declared instruction bytes. The codec is
  allocation-light on the write path: `encode-event-into!` writes into a
  caller-owned scratch buffer and `write-events!` reuses one record buffer."
  (:require [clojure.java.io :as io]
            [omkamra.vice.contract :as contract])
  (:import [java.io OutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.file Files]))

(def record-size contract/event-record-size)

(def ^:private trace-record-fields 10)

(defn- unsigned-short
  [value]
  (bit-and (int value) 0xffff))

(defn- fail
  [reason data]
  (throw (ex-info "Unsupported v2 execution event" (assoc data :reason reason))))

(defn- require-field!
  [event key predicate]
  (let [value (get event key)]
    (when-not (predicate value)
      (fail :invalid-event-field {:field key :value value}))
    value))

(defn encode-event-into!
  "Encode `event` into `buffer` at `offset` and return the offset after it.

  `buffer` must have room for one 23-byte record at `offset`. Instruction-byte
  positions beyond the instruction length are zero-filled."
  [^bytes buffer ^long offset event]
  (when (< (alength buffer) (+ offset record-size))
    (fail :short-event-buffer {:length (alength buffer) :offset offset}))
  (let [length (require-field!
                event :instruction-length
                #(and (integer? %) (<= 1 % contract/max-instruction-bytes)))
        bytes (:instruction-bytes event)
        _ (when-not (and (vector? bytes) (= length (count bytes)))
            (fail :instruction-length-mismatch
                  {:instruction-length length :instruction-bytes bytes}))
        pc (require-field! event :pc contract/u16?)
        a (require-field! event :a contract/u8?)
        x (require-field! event :x contract/u8?)
        y (require-field! event :y contract/u8?)
        sp (require-field! event :sp contract/u8?)
        flags (require-field! event :flags contract/u8?)
        global-cycle (require-field! event :global-cycle contract/u64?)
        raster-line (require-field! event :raster-line contract/u16?)
        cpu-cycle (require-field! event :cpu-cycle contract/u16?)
        bb (doto (ByteBuffer/wrap buffer (int offset) record-size)
             (.order ByteOrder/LITTLE_ENDIAN))]
    (.putShort bb (unchecked-short pc))
    (.put bb (unchecked-byte length))
    (dotimes [index contract/max-instruction-bytes]
      (.put bb (unchecked-byte (nth bytes index 0))))
    (.put bb (unchecked-byte a))
    (.put bb (unchecked-byte x))
    (.put bb (unchecked-byte y))
    (.put bb (unchecked-byte sp))
    (.put bb (unchecked-byte flags))
    (.putLong bb (long global-cycle))
    (.putShort bb (unchecked-short raster-line))
    (.putShort bb (unchecked-short cpu-cycle))
    (+ offset record-size)))

(defn encode-event
  "Return a fresh 23-byte encoding of `event`."
  [event]
  (let [buffer (byte-array record-size)]
    (encode-event-into! buffer 0 event)
    buffer))

(defn decode-event
  "Decode the event record at `offset` in `buffer`."
  [^bytes buffer ^long offset]
  (when (< (alength buffer) (+ offset record-size))
    (fail :short-event-record {:length (alength buffer) :offset offset}))
  (let [bb (doto (ByteBuffer/wrap buffer (int offset) record-size)
             (.order ByteOrder/LITTLE_ENDIAN))
        pc (unsigned-short (.getShort bb))
        length (contract/unsigned (.get bb))
        byte-buffer (byte-array contract/max-instruction-bytes)
        _ (.get bb byte-buffer)
        a (contract/unsigned (.get bb))
        x (contract/unsigned (.get bb))
        y (contract/unsigned (.get bb))
        sp (contract/unsigned (.get bb))
        flags (contract/unsigned (.get bb))
        global-cycle (.getLong bb)
        raster-line (unsigned-short (.getShort bb))
        cpu-cycle (unsigned-short (.getShort bb))]
    (when-not (<= 1 length contract/max-instruction-bytes)
      (fail :invalid-instruction-length
            {:pc pc :instruction-length length}))
    {:pc pc
     :instruction-length length
     :instruction-bytes (mapv contract/unsigned
                              (take length (seq byte-buffer)))
     :a a
     :x x
     :y y
     :sp sp
     :flags flags
     :global-cycle global-cycle
     :raster-line raster-line
     :cpu-cycle cpu-cycle}))

(defn write-events!
  "Encode every event in `events` into `out`, reusing one record buffer.

  Returns the number of records written."
  [^OutputStream out events]
  (let [buffer (byte-array record-size)]
    (reduce (fn [count event]
              (encode-event-into! buffer 0 event)
              (.write out buffer 0 record-size)
              (inc count))
            0
            events)))

(defn decode-events
  "Decode every fixed-width record in a complete event byte array."
  [^bytes bytes]
  (when-not (zero? (mod (alength bytes) record-size))
    (fail :misaligned-event-bytes
          {:length (alength bytes) :record-size record-size}))
  (mapv #(decode-event bytes (* (long %) record-size))
        (range (quot (alength bytes) record-size))))

(defn read-events-file
  "Decode every event from a packed event file."
  [file]
  (decode-events (Files/readAllBytes (.toPath (io/file file)))))

(defn trace-record->event
  "Convert one forensic `omkamra.vice.trace` record into a v2 event map.

  The source record is `[pc bytes raster-line cpu-cycle a x y sp flags
  global-cycle]` with `retain-samples?` enabled. Records without the forensic
  SP/global-cycle fields, or with more than three instruction bytes, are
  unsupported and fail closed."
  [record]
  (when-not (= trace-record-fields (count record))
    (fail :unsupported-trace-record {:record record}))
  (let [[pc bytes raster-line cpu-cycle a x y sp flags global-cycle] record]
    (when (nil? global-cycle)
      (fail :trace-record-without-global-cycle {:pc pc}))
    (when (nil? sp)
      (fail :trace-record-without-stack-pointer {:pc pc}))
    (when-not (and (vector? bytes)
                   (<= 1 (count bytes) contract/max-instruction-bytes))
      (fail :unsupported-instruction-length {:pc pc :bytes bytes}))
    {:pc pc
     :instruction-length (count bytes)
     :instruction-bytes (vec bytes)
     :a (or a 0)
     :x (or x 0)
     :y (or y 0)
     :sp sp
     :flags (contract/trace-flags->status-byte flags)
     :global-cycle global-cycle
     :raster-line raster-line
     :cpu-cycle cpu-cycle}))
