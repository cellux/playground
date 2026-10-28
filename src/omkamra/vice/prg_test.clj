(ns omkamra.vice.prg-test
  "Guards the self-relocating PRG builder.

  The unpacker's code and its copy table share one scratch image: the code is
  followed by the table. The code must therefore be measured with the same
  addressing form the table address will really use. Measuring it with a zero
  table address assembles the six indexed table reads as zero-page (2 bytes),
  undercounting the code and burying the table inside the code."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.prg :as prg]
            [omkamra.vice.zx0 :as zx0]))

(defn- u8 [x] (bit-and (long x) 0xff))

(defn- hi [x] (u8 (bit-shift-right (long x) 8)))

(defn- one-segment-artifact
  "A one-section object pinned at `$2000` so the payload layout is known."
  []
  (assembler/link
   (assembler/assemble
    '((%entry :start)
      (%org 0x2000)
      :start
      (lda 0x00)
      (rts))
    {:ns 'omkamra.vice.prg-test})))

(def ^:private table-entry-length 6)
(def ^:private table-terminator-length 6)

(defn- compressible-artifact
  []
  (assembler/link
   (assembler/assemble
    (list '(%entry :start)
          '(%org 0xc000)
          :start
          '(jmp :start)
          (list '%db (vec (take 1000 (cycle [0x11 0x22 0x33 0x44])))))
    {:ns 'omkamra.vice.prg-test})))

(deftest basic-sys-line-is-plainly-formatted
  (let [prg (prg/prg-bytes (one-segment-artifact))
        byte-at (fn [offset]
                  (bit-and (int (aget ^bytes prg (+ 2 offset))) 0xff))
        line (mapv byte-at (range 13))]
    (is (= [0x0c 0x08                 ; link to the next line
            0x0a 0x00                 ; line number 10
            0x9e 0x20                 ; SYS token, space
            0x32 0x30 0x36 0x32       ; "2062", no zero padding
            0x00                      ; line terminator
            0x00 0x00]                ; end-of-program marker
           line)
        "LIST shows `10 SYS 2062`, not `10 SYS02062`")
    (is (= 2062
           (Integer/parseInt (apply str (map char (subvec line 6 10)))))
        "the operand is the stub address")))

(deftest mover-measures-its-code-with-an-absolute-table-address
  (let [artifact (one-segment-artifact)
        segment (first (:segments artifact))
        dst (:start-address segment)
        payload-length (count (:bytes segment))
        prg (prg/prg-bytes artifact)
        image-length (- (alength prg) 2)
        payload-address prg/default-payload-address
        mover-address (+ payload-address payload-length)
        mover-offset (- mover-address prg/default-load-address)
        mover-length (- image-length mover-offset)
        code-length (- mover-length table-entry-length
                       table-terminator-length)
        table-address (+ prg/default-scratch-address code-length)
        byte-at (fn [offset]
                  (bit-and (int (aget ^bytes prg (+ 2 offset))) 0xff))
        table (mapv #(byte-at (+ mover-offset code-length %))
                    (range table-entry-length))]
    (testing "the code indexes its table with an absolute operand"
      (is (= 0xbd (byte-at (+ mover-offset 5)))
          "the first table read is LDA table,X")
      (is (= (u8 table-address) (byte-at (+ mover-offset 6))))
      (is (= (hi table-address) (byte-at (+ mover-offset 7)))))
    (testing "the embedded table starts exactly where the code reads it"
      (is (= table
             [(u8 dst) (hi dst)
              (u8 payload-length) (hi payload-length)
              (u8 payload-address) (hi payload-address)])))))

(defn- find-subsequence
  [bytes pattern]
  (some (fn [index]
          (when (every? (fn [[offset expected]]
                          (= expected (nth bytes (+ index offset) nil)))
                        (map-indexed vector pattern))
            index))
        (range (count bytes))))

(defn- assembled-mover
  "A mover assembled for `direction`, as it would be emitted."
  [direction]
  (let [scratch prg/default-scratch-address
        assemble @#'prg/assemble-bytes
        forms @#'prg/mover-forms
        sized (count (assemble (forms scratch 0x0e00 direction) scratch))]
    (assemble (forms (+ scratch sized) 0x0e00 direction) scratch)))

(deftest lzss-compressor-round-trips-and-compresses-repetition
  (let [compress @#'prg/lzss-compress
        decompress @#'prg/lzss-decompress
        bytes (vec (take 4096 (cycle [0x11 0x22 0x33 0x44])))
        packed (compress bytes)]
    (is (= bytes (decompress packed (count bytes))))
    (is (< (count packed) 500)
        "long repeated data is represented mostly by two-byte matches")))

(deftest compressed-prg-is-smaller-and-keeps-the-unpacker-in-scratch
  (let [artifact (compressible-artifact)
        raw (prg/prg-bytes artifact)
        compressed (prg/prg-bytes artifact {:compression :lzss})
        prepared (@#'prg/prepare-compressed-layout
                  (:segments artifact) prg/default-payload-address :lzss)
        layout (:layout prepared)
        payload-length (reduce + (map :stored-len layout))
        mover-length (- (alength ^bytes compressed) 2
                        (- prg/default-payload-address prg/default-load-address)
                        payload-length)]
    (is (some #(= :lzss (:mode %)) layout))
    (is (< (alength ^bytes compressed) (alength ^bytes raw)))
    (is (<= (+ prg/default-scratch-address mover-length)
            prg/default-scratch-end))))

(deftest zx0-compressor-round-trips-and-builds-a-prg
  (let [artifact (compressible-artifact)
        raw (prg/prg-bytes artifact)
        compressed (prg/prg-bytes artifact {:compression :zx0})
        packed (zx0/compress
                (vec (take 4096 (cycle [0x11 0x22 0x33 0x44]))))]
    (is (= (vec (take 4096 (cycle [0x11 0x22 0x33 0x44])))
           (zx0/decompress packed 4096)))
    (is (< (alength ^bytes compressed) (alength ^bytes raw)))
    (is (pos? (alength ^bytes compressed)))))

(deftest prg-rejects-unknown-compression
  (is (= :unknown-compression
         (try
           (prg/prg-bytes (one-segment-artifact) {:compression :unknown})
           nil
           (catch clojure.lang.ExceptionInfo error
             (:reason (ex-data error)))))))

(deftest copy-direction-picks-the-smallest-correct-loop
  (let [choose @#'prg/copy-direction]
    (is (= :forward (choose [{:dst 0x2000 :len 3 :src 0x0900}]))
        "a destination above the payload with no overlap needs no backward pass")
    (is (= :backward (choose [{:dst 0x0905 :len 0x100 :src 0x0900}]))
        "a destination inside its source from above forces backward")
    (is (= :forward (choose [{:dst 0x0800 :len 0x200 :src 0x0900}]))
        "a destination inside its source from below still copies forward")
    (is (= :both (choose [{:dst 0x0905 :len 0x100 :src 0x0900}
                          {:dst 0x0800 :len 0x200 :src 0x0900}]))
        "disagreeing segments keep both loops")))

(deftest mover-omits-loops-its-direction-does-not-need
  (let [inc-src [0xe6 0xfb]                       ; INC $FB, forward only
        dec-src [0xc6 0xfb]                       ; DEC $FB, backward only
        compare [0xc5 0xfc]]                      ; CMP $FC, direction test
    (testing ":forward emits only the forward loop"
      (let [code (assembled-mover :forward)]
        (is (some? (find-subsequence code inc-src)))
        (is (nil? (find-subsequence code dec-src)))
        (is (nil? (find-subsequence code compare)))))
    (testing ":backward emits only the backward loop"
      (let [code (assembled-mover :backward)]
        (is (some? (find-subsequence code dec-src)))
        (is (nil? (find-subsequence code inc-src)))
        (is (nil? (find-subsequence code compare)))))
    (testing ":both keeps the runtime direction test and both loops"
      (let [code (assembled-mover :both)]
        (is (some? (find-subsequence code inc-src)))
        (is (some? (find-subsequence code dec-src)))
        (is (some? (find-subsequence code compare)))))))
