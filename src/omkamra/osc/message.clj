(ns omkamra.osc.message
  (:require [clojure.string :as str])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util Date)
           (java.awt Color)
           (javax.sound.midi ShortMessage)))

(defn round-up
  "If `n` is not divisible by `alignment`, round it up to the next
  multiple of `alignment`."
  [n alignment]
  (let [rem (mod n alignment)]
    (if (zero? rem)
      n
      (+ n (- alignment rem)))))

(defn align
  "Write zero bytes into buf until its position is divisible by
  `alignment`."
  [buf alignment]
  (let [rem (mod (.position buf) alignment)]
    (when (pos? rem)
      (dotimes [_ (- alignment rem)]
        (.put buf (byte 0))))))

(defn instant->ntp-time
  [^Instant instant]
  (let [t (.getEpochSecond instant)
        msb0-base-time 2085978496
        msb1-base-time -2208988800
        use-base-1 (< t msb0-base-time)
        seconds (if use-base-1
                  (- t msb1-base-time)
                  (- t msb0-base-time))
        nanos (.getNano instant)
        fraction (quot (* nanos 0x100000000) 1000000000)]
    (bit-or (bit-shift-left seconds 32) fraction)))

(defn ntp-time->instant
  [^long ntp-time]
  (let [seconds (unsigned-bit-shift-right ntp-time 32)
        fraction (bit-and ntp-time 0xFFFFFFFF)
        nanos (quot (* fraction 1000000000) 0x100000000)
        msb0-base-time 2085978496
        msb1-base-time -2208988800
        use-base-0 (zero? (bit-and seconds 0x80000000))
        seconds (+ seconds
                   (if use-base-0
                     msb0-base-time
                     msb1-base-time))]
    (Instant/ofEpochSecond seconds nanos)))

(defn color->i32
  [^Color color]
  (bit-or (bit-shift-left (.getRed color) 24)
          (bit-shift-left (.getGreen color) 16)
          (bit-shift-left (.getBlue color) 8)
          (.getAlpha color)))

(defn i32->color
  [color]
  (Color. (bit-shift-right color 24)
          (bit-and (bit-shift-right color 16) 0xff)
          (bit-and (bit-shift-right color 8) 0xff)
          (bit-and color 0xff)))

(defn midi->i32
  [^ShortMessage message]
  (bit-or (bit-shift-left (.getChannel message) 24)
          (bit-shift-left (.getCommand message) 16)
          (bit-shift-left (.getData1 message) 8)
          (.getData2 message)))

(defn i32->midi
  [message]
  (let [channel (bit-shift-right message 24)
        command (bit-and (bit-shift-right message 16) 0xff)
        data1 (bit-and (bit-shift-right message 8) 0xff)
        data2 (bit-and message 0xff)]
    (ShortMessage. command channel data1 data2)))

(defn inf?
  [x]
  (= x ##Inf))

(defprotocol OSCValue
  (sizeof [_])
  (typetag [_])
  (bufput [_ buf]))

(extend-protocol OSCValue
  Integer
  (sizeof [_] 4)
  (typetag [_] \i)
  (bufput [n buf] (.putInt buf n))

  Float
  (sizeof [_] 4)
  (typetag [_] \f)
  (bufput [x buf] (.putFloat buf x))

  String
  (sizeof [s] (round-up (inc (.length s)) 4))
  (typetag [_] \s)
  (bufput [s buf]
    (doseq [c s]
      (.put buf (byte c)))
    (.put buf (byte 0))
    (align buf 4))

  ByteBuffer
  (sizeof [b] (+ 4 (round-up (.limit b) 4)))
  (typetag [_] \b)
  (bufput [b buf]
    (.putInt buf (.limit b))
    (.put buf b)
    (align buf 4))

  Long
  (sizeof [_] 8)
  (typetag [_] \h)
  (bufput [n buf] (.putLong buf n))

  Instant
  (sizeof [_] 8)
  (typetag [_] \t)
  (bufput [t buf] (.putLong buf (instant->ntp-time t)))

  Date
  (sizeof [_] 8)
  (typetag [_] \t)
  (bufput [t buf] (.putLong buf (instant->ntp-time (.toInstant t))))

  Double
  (sizeof [x] (if (inf? x) 0 8))
  (typetag [x] (if (inf? x) \I \d))
  (bufput [x buf] (when-not (inf? x) (.putDouble buf x)))

  clojure.lang.Symbol
  (sizeof [s] (sizeof (name s)))
  (typetag [_] \S)
  (bufput [s buf] (bufput (name s) buf))

  Character
  (sizeof [_] 4)
  (typetag [_] \c)
  (bufput [c buf] (.putInt buf (byte c)))

  java.awt.Color
  (sizeof [_] 4)
  (typetag [_] \r)
  (bufput [color buf] (.putInt buf (color->i32 color)))

  javax.sound.midi.ShortMessage
  (sizeof [_] 4)
  (typetag [_] \m)
  (bufput [message buf] (.putInt buf (midi->i32 message)))

  Boolean
  (sizeof [_] 0)
  (typetag [b] (if b \T \F))
  (bufput [_ buf])

  nil
  (sizeof [_] 0)
  (typetag [_] \N)
  (bufput [_ buf])

  clojure.lang.IPersistentCollection
  (sizeof [coll] (reduce + 0 (map sizeof coll)))
  (typetag [coll] (str \[ (str/join (map typetag coll)) \]))
  (bufput [coll buf] (doseq [item coll] (bufput item buf))))

(extend (class (byte-array 0))
  OSCValue
  {:sizeof (fn [ba] (alength ba))
   :typetag (fn [_] \b)
   :bufput (fn [ba buf] (.put buf ba))})

(defn encode
  [[address-pattern & args]]
  (let [sig (apply str \, (map typetag args))
        bufsize (+ (sizeof address-pattern)
                   (sizeof sig)
                   (reduce + 0 (map sizeof args)))
        buf (ByteBuffer/allocate bufsize)]
    (bufput address-pattern buf)
    (bufput sig buf)
    (doseq [arg args]
      (bufput arg buf))
    (.rewind buf)))

(defn bufget-asciiz-string
  [buf]
  (let [^int saved-pos (.position buf)
        ^int limit (.limit buf)
        zero-index (loop [pos saved-pos]
                     (if (and (< pos limit)
                              (not (zero? (.get buf pos))))
                       (recur (inc pos))
                       (if (zero? (.get buf pos))
                         pos
                         (throw (IllegalStateException.
                                 "OSC string without terminating zero")))))
        string-length (- zero-index saved-pos)]
    (.position buf (round-up (inc zero-index) 4))
    (String. (.array buf)
             saved-pos
             string-length
             (StandardCharsets/US_ASCII))))

(defn bufget-bytes
  [buf]
  (let [pos (.position buf)
        length (.getInt buf)
        ba (byte-array length)
        next-pos (round-up (+ pos 4 length) 4)]
    (.get buf ba 0 length)
    (.position buf next-pos)
    ba))

(defn bufget-instant
  [buf]
  (ntp-time->instant (.getLong buf)))

(defn bufget
  [sig buf]
  (loop [sig sig
         results []]
    (if sig
      (case (first sig)
        \i (recur (next sig) (conj results (.getInt buf)))
        \f (recur (next sig) (conj results (.getFloat buf)))
        \s (recur (next sig) (conj results (bufget-asciiz-string buf)))
        \b (recur (next sig) (conj results (bufget-bytes buf)))
        \h (recur (next sig) (conj results (.getLong buf)))
        \t (recur (next sig) (conj results (bufget-instant buf)))
        \d (recur (next sig) (conj results (.getDouble buf)))
        \S (recur (next sig) (conj results (symbol (bufget-asciiz-string buf))))
        \c (recur (next sig) (conj results (char (.getInt buf))))
        \r (recur (next sig) (conj results (i32->color (.getInt buf))))
        \m (recur (next sig) (conj results (i32->midi (.getInt buf))))
        \T (recur (next sig) (conj results true))
        \F (recur (next sig) (conj results false))
        \N (recur (next sig) (conj results nil))
        \I (recur (next sig) (conj results ##Inf))
        \[ (let [[asig rest-sig] (split-with #(not= \] %) (next sig))]
             (recur (next rest-sig) (conj results (bufget asig buf)))))
      results)))

(defn decode
  [buf]
  (let [address-pattern (bufget-asciiz-string buf)
        sig (bufget-asciiz-string buf)]
    (when-not (str/starts-with? sig ",")
      (throw (IllegalStateException.
              (str "invalid OSC type tag string: " sig))))
    (apply vector address-pattern (bufget (next sig) buf))))
