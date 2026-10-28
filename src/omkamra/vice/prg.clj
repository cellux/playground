(ns omkamra.vice.prg
  "Self-relocating PRG for a linked artifact.

  The PRG loads at `$0801` as a BASIC `SYS` program. The SYS routine copies a
  small unpacker — its copy table baked in — to scratch RAM below `$0400`,
  jumps there, and the unpacker moves the artifact's segments from the packed
  payload to their linked addresses before jumping to the entry point.

  Only the BASIC hand-off (RUN) is left to VICE's autostart, so a capture that
  gates at the entry sees the same long run-up as an `:input` capture and the
  entry stop latches.

  Layout, from the load address:

      $0801  BASIC line `10 SYS <stub>` + end marker
      $080E  stub            disables IRQs, copies the unpacker to scratch, JMPs
      $0900  payload         the segments in increasing destination order
      ...    unpacker        the mover routine with its copy table baked in
      $0334  scratch         where the stub copies the unpacker

  Because the segments are packed in increasing destination order, the copy
  dependency graph (segment `i` must copy after `j` when `dst_i` overlaps
  `src_j`) is acyclic; the generator bakes a topological copy order into the
  table. From the same layout it emits only the copy direction the segments
  need — forward when no destination overlaps its own source from above,
  backward when none does from below, and both (with a runtime test) only when
  they disagree — so destinations may sit anywhere in RAM, including inside the
  loaded image, without corrupting an unread source."
  (:require [clojure.java.io :as io]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.zx0 :as zx0]))

(def default-load-address 0x0801)
(def default-payload-address 0x0900)
(def default-scratch-address 0x0334)
(def default-scratch-end 0x07e8)

(def ^:private basic-token-sys 0x9e)
(def ^:private basic-line-number 10)
(def ^:private basic-space 0x20)
(def ^:private basic-end-marker-length 2)
;; link(2) number(2) token(1), then a space, the operand digits, and the line
;; terminator
(def ^:private basic-line-fixed-length 7)
(def ^:private table-entry-length 6) ; dst(2) len(2) src(2)
(def ^:private compressed-table-entry-length 7) ; dst(2) len(2) src(2) mode(1)
(def ^:private compression-window 2048)
(def ^:private compression-max-match 18)
(def ^:private compression-max-candidates 64)

(defn- u8 ^long [x] (bit-and (long x) 0xff))

(defn- hi ^long [x] (u8 (bit-shift-right (long x) 8)))

(defn- basic-sys-line
  "The tokenized BASIC `10 SYS <stub>` line, and the stub address.

  The stub sits after the line and the end-of-program marker, so the line's
  length depends on how many digits that address has; iterate to the fixed
  point. The operand is a space and plain decimal digits, no zero padding, so
  `LIST` shows `10 SYS 2062` rather than `10 SYS02062`."
  [load-address]
  (loop [length (+ basic-line-fixed-length 5)]
    (let [stub (+ (long load-address) (long length) basic-end-marker-length)
          operand (mapv #(u8 (int %)) (str (long stub)))
          actual (+ basic-line-fixed-length (count operand))]
      (if (= actual length)
        (let [link (+ (long load-address) (long length))]
          {:stub stub
           :line (into [(u8 link) (hi link) basic-line-number 0x00
                        basic-token-sys basic-space]
                       (conj operand 0x00))})
        (recur actual)))))

;; Compression ---------------------------------------------------------------

(defn- matching-length
  "Length of the match at `position` against `candidate`, capped at 18 bytes."
  [bytes position candidate]
  (let [limit (min compression-max-match (- (count bytes) position))]
    (loop [length 0]
      (if (and (< length limit)
               (= (nth bytes (+ candidate length))
                  (nth bytes (+ position length))))
        (recur (inc length))
        length))))

(defn- three-byte-match?
  [bytes position candidate]
  (and (<= (+ position 3) (count bytes))
       (= (nth bytes candidate) (nth bytes position))
       (= (nth bytes (inc candidate)) (nth bytes (inc position)))
       (= (nth bytes (+ candidate 2)) (nth bytes (+ position 2)))))

(defn- matches-at
  "Useful `[length distance]` matches ending before `position`.

  Candidates are examined nearest-first inside the 2 KiB window. At most 64
  three-byte-prefix matches are expanded; this keeps highly repetitive assets
  fast while retaining several alternatives for the optimal parser. Since all
  matches have the same encoded size, only the nearest distance for each
  available length is needed."
  [bytes position]
  (let [first-candidate (max 0 (- position compression-window))]
    (loop [candidate (dec position)
           examined 0
           matches {}]
      (if (or (< candidate first-candidate)
              (= examined compression-max-candidates))
        (mapv (fn [[length distance]] [length distance]) matches)
        (if (three-byte-match? bytes position candidate)
          (let [length (matching-length bytes position candidate)
                distance (- position candidate)]
            (recur (dec candidate)
                   (inc examined)
                   (reduce (fn [matches match-length]
                             (if (contains? matches match-length)
                               matches
                               (assoc matches match-length distance)))
                           matches
                           (range 3 (inc length)))))
          (recur (dec candidate) examined matches))))))

(defn- lzss-compress
  "Compress bytes with a tiny-decoder LZ format.

  Literal token: `0LLLLLLL`, followed by `L+1` literal bytes.
  Match token: `1DDDLLLL BYTE`, where length is `LLLL+3` and distance is
  `DDD:BYTE+1` (1..2048). The parser uses dynamic programming to find a
  minimum-size token stream rather than making greedy match choices."
  [input]
  (let [bytes (mapv u8 input)
        n (count bytes)
        costs (long-array (inc n))
        choices (object-array n)]
    (aset-long costs n 0)
    (doseq [position (range (dec n) -1 -1)]
      (let [max-literal (min 128 (- n position))
            literal-options
            (for [length (range 1 (inc max-literal))]
              {:cost (+ 1 length (aget costs (+ position length)))
               :kind :literal
               :length length})
            match-options
            (for [[length distance] (matches-at bytes position)]
              {:cost (+ 2 (aget costs (+ position length)))
               :kind :match
               :length length
               :distance distance})
            choice (first (sort-by (juxt :cost
                                         #(if (= :match (:kind %)) 0 1)
                                         (comp - :length))
                                   (concat literal-options match-options)))]
        (aset-long costs position (:cost choice))
        (aset choices position choice)))
    (loop [position 0
           output (transient [])]
      (if (= position n)
        (persistent! output)
        (let [{:keys [kind length distance]} (aget choices position)]
          (if (= kind :literal)
            (recur (+ position length)
                   (reduce conj! (conj! output (dec length))
                           (subvec bytes position (+ position length))))
            (let [encoded-distance (dec distance)
                  control (bit-or 0x80
                                  (bit-shift-left
                                   (bit-and (bit-shift-right encoded-distance 8)
                                            0x07)
                                   4)
                                  (- length 3))]
              (recur (+ position length)
                     (-> output
                         (conj! control)
                         (conj! (u8 encoded-distance)))))))))))

(defn- lzss-decompress
  "Reference decoder used by tests and generator self-checks."
  [packed output-length]
  (let [packed (mapv u8 packed)]
    (loop [input-position 0
           output (transient [])]
      (let [output-count (count output)]
        (if (= output-count output-length)
          (persistent! output)
          (let [control (nth packed input-position)]
            (if (zero? (bit-and control 0x80))
              (let [length (inc control)
                    start (inc input-position)
                    end (+ start length)]
                (recur end (reduce conj! output (subvec packed start end))))
              (let [length (+ 3 (bit-and control 0x0f))
                    encoded-distance
                    (+ (bit-shift-left (bit-and control 0x70) 4)
                       (nth packed (inc input-position)))
                    distance (inc encoded-distance)]
                (when (> distance output-count)
                  (throw (ex-info "Invalid compressed match distance"
                                  {:reason :invalid-compressed-data
                                   :distance distance
                                   :output-count output-count})))
                (recur (+ input-position 2)
                       (loop [remaining length
                              output output]
                         (if (zero? remaining)
                           output
                           (let [value (nth output (- (count output) distance))]
                             (recur (dec remaining) (conj! output value))))))))))))))

;; Copy order ----------------------------------------------------------------

(defn- overlaps?
  [a la b lb]
  (and (< (long a) (+ (long b) (long lb)))
       (< (long b) (+ (long a) (long la)))))

(defn- copy-order
  "Topological copy order for `layout` (segments in increasing-dst order).

  `i` depends on `j` — must copy after it — when `dst_i` overlaps `src_j`;
  copying `i` first would overwrite `j`'s unread source. The increasing-dst
  layout makes the graph acyclic; a cycle would be a generator bug, so it is
  reported loudly."
  [layout]
  (let [n (count layout)
        deps (vec (for [i (range n)]
                    (vec (for [j (range n)
                               :when (and (not= i j)
                                          (overlaps? (:dst (layout i)) (:len (layout i))
                                                     (:src (layout j))
                                                     (or (:stored-len (layout j))
                                                         (:len (layout j)))))]
                           j))))
        state (boolean-array n)
        on-stack (boolean-array n)
        order (java.util.ArrayList.)]
    (letfn [(visit [i]
              (when (aget on-stack i)
                (throw (ex-info "Segment relocation is cyclic"
                                {:reason :cyclic-relocation :segment i})))
              (when-not (aget state i)
                (aset on-stack i true)
                (doseq [j (deps i)] (visit j))
                (aset on-stack i false)
                (aset state i true)
                (.add order i)))]
      (doseq [i (range n)] (visit i)))
    (vec order)))

;; Unpacker ------------------------------------------------------------------

(defn- copy-direction
  "The minimal copy direction that is correct for `layout`.

  Forward copy is unsafe only for a segment whose destination lies inside its
  own source range from above (`src < dst < src+len`); backward copy is unsafe
  only from below (`dst < src < dst+len`). A layout with no self-overlap makes
  both safe, and forward (the smaller loop) wins. `:both` is the general
  memmove, needed only when the segments disagree."
  [layout]
  (let [forward-unsafe?
        (some (fn [{:keys [dst len src]}]
                (and (< (long src) (long dst))
                     (< (long dst) (+ (long src) (long len)))))
              layout)
        backward-unsafe?
        (some (fn [{:keys [dst len src]}]
                (and (< (long dst) (long src))
                     (< (long src) (+ (long dst) (long len)))))
              layout)]
    (cond
      (not forward-unsafe?) :forward
      (not backward-unsafe?) :backward
      :else :both)))

(defn- mover-forms
  "The unpacker, running from `scratch`.

  Entry layout in `table` is `dst-lo dst-hi len-lo len-hi src-lo src-hi`; a zero
  length terminates. Zero-page pointers are `$f9/$fa` length, `$fb/$fc` source,
  `$fd/$fe` destination. `$fb-$fe` is FREKZP and `$f9/$fa` is ROBUF (RS-232
  only), so clobbering them leaves BASIC's interpreter workspace alone.

  `direction` (`:forward`, `:backward`, or `:both`) selects the emitted copy
  loops; `copy-direction` derives it from the layout, so each PRG carries only
  the paths its segments need. The baked table order handles inter-segment
  overlap."
  [table entry direction]
  (let [prologue
        `[(sei)
          (ldx 0x00)
          (ldy 0x00)
          :next
          (lda [~table x]) (sta [0xfd])
          (lda [~(+ table 1) x]) (sta [0xfe])
          (lda [~(+ table 2) x]) (sta [0xf9])
          (lda [~(+ table 3) x]) (sta [0xfa])
          (ora [0xf9])
          (beq :done)
          (lda [~(+ table 4) x]) (sta [0xfb])
          (lda [~(+ table 5) x]) (sta [0xfc])]
        select
        `[(lda [0xfe]) (cmp [0xfc]) (bcc :forward) (bne :backward)
          (lda [0xfd]) (cmp [0xfb]) (bcc :forward)]
        backward
        `[:backward
          (lda [0xfb]) (clc) (adc [0xf9]) (sta [0xfb])
          (lda [0xfc]) (adc [0xfa]) (sta [0xfc])
          (lda [0xfd]) (clc) (adc [0xf9]) (sta [0xfd])
          (lda [0xfe]) (adc [0xfa]) (sta [0xfe])
          :copy-back
          (lda [0xfb]) (bne :src-down) (dec [0xfc])
          :src-down
          (dec [0xfb])
          (lda [0xfd]) (bne :dst-down) (dec [0xfe])
          :dst-down
          (dec [0xfd])
          (lda [[0xfb] y]) (sta [[0xfd] y])
          (lda [0xf9]) (bne :back-count) (dec [0xfa])
          :back-count
          (dec [0xf9])
          (lda [0xf9]) (ora [0xfa])
          (bne :copy-back)
          (jmp :advance)]
        forward
        `[:forward
          :copy-fwd
          (lda [[0xfb] y]) (sta [[0xfd] y])
          (inc [0xfb]) (bne :src-up) (inc [0xfc])
          :src-up
          (inc [0xfd]) (bne :dst-up) (inc [0xfe])
          :dst-up
          (lda [0xf9]) (bne :fwd-count) (dec [0xfa])
          :fwd-count
          (dec [0xf9])
          (lda [0xf9]) (ora [0xfa])
          (bne :copy-fwd)]
        tail
        `[:advance
          (inx) (inx) (inx) (inx) (inx) (inx)
          (jmp :next)
          :done
          (jmp ~entry)]]
    (vec (concat prologue
                 (when (= direction :both) select)
                 (when (not= direction :forward) backward)
                 (when (not= direction :backward) forward)
                 tail))))

(defn- compressed-mover-forms
  "Mixed raw-copy/LZSS unpacker used by `:compression :lzss` PRGs.

  Table entries add a mode byte to the ordinary mover table: zero copies raw
  bytes and one expands the tiny LZSS stream. The generated routine omits raw
  copy directions and the decompressor when the payload does not need them."
  [table entry {:keys [compressed? raw-direction]}]
  (let [prologue
        `[(sei)
          (ldx 0x00)
          (ldy 0x00)
          :next
          (lda [~table x]) (sta [0xfd])
          (lda [~(+ table 1) x]) (sta [0xfe])
          (lda [~(+ table 2) x]) (sta [0xf9])
          (lda [~(+ table 3) x]) (sta [0xfa])
          (ora [0xf9])
          (bne :nonempty)
          (jmp :done)
          :nonempty
          (lda [~(+ table 4) x]) (sta [0xfb])
          (lda [~(+ table 5) x]) (sta [0xfc])]
        dispatch
        (cond
          (and compressed? raw-direction)
          `[(lda [~(+ table 6) x])
            (beq :raw)
            (jmp :compressed)]

          compressed?
          `[(jmp :compressed)]

          :else
          `[(jmp :raw)])
        raw-select
        (when (= raw-direction :both)
          `[:raw
            (lda [0xfe]) (cmp [0xfc]) (bcc :forward) (bne :backward)
            (lda [0xfd]) (cmp [0xfb]) (bcc :forward)
            (jmp :backward)])
        raw-label
        (when (and raw-direction (not= raw-direction :both))
          `[:raw])
        backward
        (when (and raw-direction (not= raw-direction :forward))
          `[:backward
            (lda [0xfb]) (clc) (adc [0xf9]) (sta [0xfb])
            (lda [0xfc]) (adc [0xfa]) (sta [0xfc])
            (lda [0xfd]) (clc) (adc [0xf9]) (sta [0xfd])
            (lda [0xfe]) (adc [0xfa]) (sta [0xfe])
            :copy-back
            (lda [0xfb]) (bne :src-down) (dec [0xfc])
            :src-down
            (dec [0xfb])
            (lda [0xfd]) (bne :dst-down) (dec [0xfe])
            :dst-down
            (dec [0xfd])
            (lda [[0xfb] y]) (sta [[0xfd] y])
            (lda [0xf9]) (bne :back-count) (dec [0xfa])
            :back-count
            (dec [0xf9])
            (lda [0xf9]) (ora [0xfa])
            (bne :copy-back)
            (jmp :advance)])
        forward
        (when (and raw-direction (not= raw-direction :backward))
          `[:forward
            :copy-fwd
            (lda [[0xfb] y]) (sta [[0xfd] y])
            (inc [0xfb]) (bne :src-up) (inc [0xfc])
            :src-up
            (inc [0xfd]) (bne :dst-up) (inc [0xfe])
            :dst-up
            (lda [0xf9]) (bne :fwd-count) (dec [0xfa])
            :fwd-count
            (dec [0xf9])
            (lda [0xf9]) (ora [0xfa])
            (bne :copy-fwd)
            (jmp :advance)])
        decompressor
        (when compressed?
          `[:compressed
            :token
            (lda [0xf9]) (ora [0xfa]) (bne :token-data)
            (jmp :advance)
            :token-data
            ;; Read a control byte and advance the packed source pointer.
            (lda [[0xfb] y]) (sta [0xf5])
            (inc [0xfb]) (bne :control-read) (inc [0xfc])
            :control-read
            (lda [0xf5]) (bmi :match)
            ;; Literal: control+1 bytes follow directly in the stream.
            (clc) (adc 0x01) (sta [0xf6])
            :literal-byte
            (lda [[0xfb] y]) (sta [[0xfd] y])
            (inc [0xfb]) (bne :literal-src) (inc [0xfc])
            :literal-src
            (inc [0xfd]) (bne :literal-dst) (inc [0xfe])
            :literal-dst
            (lda [0xf9]) (bne :literal-len) (dec [0xfa])
            :literal-len
            (dec [0xf9])
            (dec [0xf6]) (bne :literal-byte)
            (jmp :token)
            ;; Match: low nibble is length-3; bits 4..6 and the next byte are
            ;; distance-1. Copying forward supports overlapping LZ matches.
            :match
            (and 0x0f) (clc) (adc 0x03) (sta [0xf6])
            (lda [0xf5]) (and 0x70)
            (lsr ~'a) (lsr ~'a) (lsr ~'a) (lsr ~'a) (sta [0xf8])
            (lda [[0xfb] y]) (sta [0xf7])
            (inc [0xfb]) (bne :distance-read) (inc [0xfc])
            :distance-read
            (lda [0xfd]) (sec) (sbc [0xf7]) (sta [0xf7])
            (lda [0xfe]) (sbc [0xf8]) (sta [0xf8])
            (lda [0xf7]) (bne :distance-low) (dec [0xf8])
            :distance-low
            (dec [0xf7])
            :match-byte
            (lda [[0xf7] y]) (sta [[0xfd] y])
            (inc [0xf7]) (bne :match-src) (inc [0xf8])
            :match-src
            (inc [0xfd]) (bne :match-dst) (inc [0xfe])
            :match-dst
            (lda [0xf9]) (bne :match-len) (dec [0xfa])
            :match-len
            (dec [0xf9])
            (dec [0xf6]) (bne :match-byte)
            (jmp :token)])
        tail
        `[:advance
          (inx) (inx) (inx) (inx) (inx) (inx) (inx)
          (jmp :next)
          :done
          (jmp ~entry)]]
    (vec (concat prologue dispatch raw-select raw-label backward forward
                 decompressor tail))))

(defn- zx0-mover-forms
  "Mixed raw-copy/ZX0 unpacker used by `:compression :zx0` PRGs.

  The ZX0 decoder is a translation of DMSC's 148-byte MIT-licensed mode-2
  decoder. Zero-page `$f5-$fe` holds offset, source, destination, bit reserve,
  Elias high byte, and match pointer."
  [table entry {:keys [compressed? raw-direction]}]
  (let [prologue
        `[(sei)
          (ldx 0x00)
          (ldy 0x00)
          :next
          (lda [~(+ table 2) x]) (sta [0xf5])
          (lda [~(+ table 3) x]) (sta [0xf6])
          (ora [0xf5])
          (bne :nonempty)
          (jmp :done)
          :nonempty
          (lda [~table x]) (sta [0xf9])
          (lda [~(+ table 1) x]) (sta [0xfa])
          (lda [~(+ table 4) x]) (sta [0xf7])
          (lda [~(+ table 5) x]) (sta [0xf8])]
        dispatch
        (cond
          (and compressed? raw-direction)
          `[(lda [~(+ table 6) x])
            (beq :raw)
            (txa) (pha)
            (lda 0x00) (sta [0xf5]) (sta [0xf6])
            (lda 0x80) (sta [0xfb])
            (jsr :zx0-decompress)
            (pla) (tax)
            (jmp :advance)]

          compressed?
          `[(txa) (pha)
            (lda 0x00) (sta [0xf5]) (sta [0xf6])
            (lda 0x80) (sta [0xfb])
            (jsr :zx0-decompress)
            (pla) (tax)
            (jmp :advance)]

          :else
          `[(jmp :raw)])
        raw-select
        (when (= raw-direction :both)
          `[:raw
            (lda [0xfa]) (cmp [0xf8]) (bcc :raw-forward) (bne :raw-backward)
            (lda [0xf9]) (cmp [0xf7]) (bcc :raw-forward)
            (jmp :raw-backward)])
        raw-label
        (when (and raw-direction (not= raw-direction :both))
          `[:raw])
        backward
        (when (and raw-direction (not= raw-direction :forward))
          `[:raw-backward
            (lda [0xf7]) (clc) (adc [0xf5]) (sta [0xf7])
            (lda [0xf8]) (adc [0xf6]) (sta [0xf8])
            (lda [0xf9]) (clc) (adc [0xf5]) (sta [0xf9])
            (lda [0xfa]) (adc [0xf6]) (sta [0xfa])
            :raw-copy-back
            (lda [0xf7]) (bne :raw-src-down) (dec [0xf8])
            :raw-src-down
            (dec [0xf7])
            (lda [0xf9]) (bne :raw-dst-down) (dec [0xfa])
            :raw-dst-down
            (dec [0xf9])
            (lda [[0xf7] y]) (sta [[0xf9] y])
            (lda [0xf5]) (bne :raw-back-count) (dec [0xf6])
            :raw-back-count
            (dec [0xf5])
            (lda [0xf5]) (ora [0xf6])
            (bne :raw-copy-back)
            (jmp :advance)])
        forward
        (when (and raw-direction (not= raw-direction :backward))
          `[:raw-forward
            :raw-copy-forward
            (lda [[0xf7] y]) (sta [[0xf9] y])
            (inc [0xf7]) (bne :raw-src-up) (inc [0xf8])
            :raw-src-up
            (inc [0xf9]) (bne :raw-dst-up) (inc [0xfa])
            :raw-dst-up
            (lda [0xf5]) (bne :raw-forward-count) (dec [0xf6])
            :raw-forward-count
            (dec [0xf5])
            (lda [0xf5]) (ora [0xf6])
            (bne :raw-copy-forward)
            (jmp :advance)])
        decoder
        (when compressed?
          `[:zx0-decompress
            :zx0-literal
            (jsr :zx0-get-elias)
            :zx0-copy-literal
            (jsr :zx0-get-byte)
            (jsr :zx0-put-byte)
            (bne :zx0-copy-literal)
            (dec [0xfc])
            (bpl :zx0-copy-literal)
            (asl [0xfb])
            (bcs :zx0-new-offset)
            (jsr :zx0-get-elias-carry)
            :zx0-copy
            (lda [0xf9]) (sbc [0xf5]) (sta [0xfd])
            (lda [0xfa]) (sbc [0xf6]) (sta [0xfe])
            :zx0-copy-match
            (lda [[0xfd] y])
            (inc [0xfd]) (bne :zx0-match-src) (inc [0xfe])
            :zx0-match-src
            (jsr :zx0-put-byte)
            (bne :zx0-copy-match)
            (dec [0xfc])
            (bpl :zx0-copy-match)
            (asl [0xfb])
            (bcc :zx0-literal)
            :zx0-new-offset
            (jsr :zx0-get-elias)
            (beq :zx0-exit)
            (dex) (stx [0xf6])
            (jsr :zx0-get-byte)
            (lsr [0xf6])
            (ror ~'a)
            (sta [0xf5])
            (ror [0xfb])
            (jsr :zx0-get-elias-carry)
            (inx)
            (bne :zx0-length-ready)
            (inc [0xfc])
            :zx0-length-ready
            (bne :zx0-copy)
            :zx0-get-elias
            (clc)
            :zx0-get-elias-carry
            (lda 0x01)
            (sty [0xfc])
            :zx0-elias-loop
            (rol [0xfb])
            (bne :zx0-have-bit)
            (tax)
            (jsr :zx0-get-byte)
            (rol ~'a)
            (sta [0xfb])
            (txa)
            :zx0-have-bit
            (bcs :zx0-elias-data)
            (tax)
            :zx0-exit
            (rts)
            :zx0-elias-data
            (asl [0xfb])
            (rol ~'a)
            (rol [0xfc])
            (bcc :zx0-elias-loop)
            :zx0-get-byte
            (lda [[0xf7] y])
            (inc [0xf7]) (bne :zx0-byte-ready) (inc [0xf8])
            :zx0-byte-ready
            (rts)
            :zx0-put-byte
            (sta [[0xf9] y])
            (inc [0xf9]) (bne :zx0-output-ready) (inc [0xfa])
            :zx0-output-ready
            (dex)
            (rts)])
        tail
        `[:advance
          (inx) (inx) (inx) (inx) (inx) (inx) (inx)
          (jmp :next)
          :done
          (jmp ~entry)]]
    (vec (concat prologue dispatch raw-select raw-label backward forward
                 decoder tail))))

(defn- stub-forms
  "The stub: copy `mover-length` bytes from `mover-image` to `scratch`, JMP there.
  A 16-bit count, because the unpacker and its table can exceed one page."
  [mover-image mover-length scratch]
  `[(sei)
    (lda ~(u8 mover-image)) (sta [0xfb])
    (lda ~(hi mover-image)) (sta [0xfc])
    (lda ~(u8 scratch)) (sta [0xfd])
    (lda ~(hi scratch)) (sta [0xfe])
    (lda ~(u8 mover-length)) (sta [0xf9])
    (lda ~(hi mover-length)) (sta [0xfa])
    (ldy 0x00)
    :copy
    (lda [[0xfb] y]) (sta [[0xfd] y])
    (inc [0xfb]) (bne :src-ok) (inc [0xfc])
    :src-ok
    (inc [0xfd]) (bne :dst-ok) (inc [0xfe])
    :dst-ok
    (lda [0xf9]) (bne :len-ok) (dec [0xfa])
    :len-ok
    (dec [0xf9])
    (lda [0xf9]) (ora [0xfa])
    (bne :copy)
    (jmp ~scratch)])

(defn- assemble-bytes
  "Assemble `forms` as a `%org address` fragment and return its bytes."
  [forms address]
  (let [object (assembler/assemble (into [(list '%org (long address))] forms)
                                   {:ns (the-ns 'omkamra.vice.prg)})
        ;; The stub has to live in the harness page the default reserve keeps
        ;; clear, so `prg`'s own fragments are linked without that reserve.
        artifact (assembler/link object {:reserved []})
        segment (first (filter #(= (long address) (long (:start-address %)))
                               (:segments artifact)))]
    (when-not segment
      (throw (ex-info "PRG fragment did not link at its origin"
                      {:address address})))
    (mapv u8 (:bytes segment))))

(defn- place!
  [^bytes image base address data]
  (let [^bytes bytes (byte-array (map u8 data))]
    (System/arraycopy bytes 0 image
                      (int (- (long address) (long base)))
                      (alength bytes))))

(defn- check-scratch-size!
  [scratch-address mover]
  (let [mover-length (count mover)
        end (+ scratch-address mover-length)]
    (when (> end default-scratch-end)
      (throw (ex-info "PRG unpacker does not fit in $0334-$07e7"
                      {:reason :unpacker-too-large
                       :scratch-address scratch-address
                       :scratch-end default-scratch-end
                       :unpacker-length mover-length
                       :overflow (- end default-scratch-end)})))))

(defn- build-prg-image
  [load-address payload-address scratch-address payload mover]
  (let [{stub :stub line :line} (basic-sys-line load-address)
        mover-address (+ payload-address (count payload))
        end (+ mover-address (count mover))
        stub-bytes (assemble-bytes (stub-forms mover-address (count mover)
                                               scratch-address)
                                   stub)]
    (when (> end 0x10000)
      (throw (ex-info "Generated PRG does not fit in C64 memory"
                      {:reason :prg-too-large
                       :end end})))
    (let [image (byte-array (- end load-address))]
      (place! image load-address load-address
              (byte-array (concat line (repeat basic-end-marker-length 0x00))))
      (place! image load-address stub stub-bytes)
      (place! image load-address payload-address payload)
      (place! image load-address mover-address mover)
      (byte-array (concat [(u8 load-address) (hi load-address)] image)))))

(defn- raw-layout
  [segments payload-address]
  (loop [segments (sort-by :start-address segments)
         offset 0
         layout []]
    (if-let [segment (first segments)]
      (let [bytes (mapv u8 (:bytes segment))
            length (count bytes)]
        (recur (rest segments)
               (+ offset length)
               (conj layout {:dst (long (:start-address segment))
                             :len length
                             :stored-len length
                             :src (+ payload-address offset)
                             :bytes bytes
                             :mode :raw})))
      layout)))

(defn- legacy-prg-bytes
  [artifact load-address payload-address scratch-address]
  (let [entry (long (:entry-point artifact))
        layout (raw-layout (:segments artifact) payload-address)
        order (copy-order layout)
        direction (copy-direction layout)
        payload (vec (mapcat :bytes layout))
        table (vec (mapcat (fn [i]
                             (let [{:keys [dst len src]} (layout i)]
                               [(u8 dst) (hi dst)
                                (u8 len) (hi len)
                                (u8 src) (hi src)]))
                           order))
        table (into table (repeat table-entry-length 0x00))
        ;; Measure with an absolute table operand, not a zero-page placeholder.
        code-length (count (assemble-bytes (mover-forms scratch-address entry
                                                        direction)
                                           scratch-address))
        table-address (+ scratch-address code-length)
        code (assemble-bytes (mover-forms table-address entry direction)
                             scratch-address)
        mover (into code table)]
    (check-scratch-size! scratch-address mover)
    (build-prg-image load-address payload-address scratch-address payload mover)))

(defn- compressed-candidates
  [segments compression]
  (let [[compress decompress]
        (case compression
          :lzss [lzss-compress lzss-decompress]
          :zx0 [zx0/compress zx0/decompress])]
    (mapv (fn [segment]
            (let [bytes (mapv u8 (:bytes segment))
                  packed (if (seq bytes) (compress bytes) bytes)
                  compressed? (< (count packed) (count bytes))]
              (when (and compressed?
                         (not= bytes (decompress packed (count bytes))))
                (throw (ex-info "Internal PRG compression verification failed"
                                {:reason :compression-verification-failed
                                 :compression compression
                                 :start-address (:start-address segment)})))
              {:dst (long (:start-address segment))
               :len (count bytes)
               :raw-bytes bytes
               :packed-bytes packed
               :mode (if compressed? compression :raw)}))
          (sort-by :start-address segments))))

(defn- assign-compressed-sources
  [candidates payload-address]
  (loop [candidates candidates
         offset 0
         layout []]
    (if-let [candidate (first candidates)]
      (let [bytes (if (= :raw (:mode candidate))
                    (:raw-bytes candidate)
                    (:packed-bytes candidate))
            stored-length (count bytes)]
        (recur (rest candidates)
               (+ offset stored-length)
               (conj layout (assoc candidate
                                   :src (+ payload-address offset)
                                   :stored-len stored-length
                                   :bytes bytes))))
      layout)))

(defn- prepare-compressed-layout
  "Choose compressed segments that can expand directly into their destination.

  A compressed stream may not overlap its own output because expansion is
  forward-only. Cross-segment source overlap is handled by `copy-order`. If
  compaction creates a dependency cycle, the generator conservatively falls
  back to the proven raw mover."
  [segments payload-address compression]
  (loop [candidates (compressed-candidates segments compression)]
    (let [layout (assign-compressed-sources candidates payload-address)
          unsafe (into #{}
                       (keep-indexed
                        (fn [index {:keys [dst len src stored-len mode]}]
                          (when (and (not= mode :raw)
                                     (overlaps? dst len src stored-len))
                            index)))
                       layout)]
      (if (seq unsafe)
        (recur (mapv (fn [index candidate]
                       (if (contains? unsafe index)
                         (assoc candidate :mode :raw)
                         candidate))
                     (range)
                     candidates))
        (when (some #(not= :raw (:mode %)) layout)
          (try
            {:layout layout
             :order (copy-order layout)}
            (catch clojure.lang.ExceptionInfo error
              (when-not (= :cyclic-relocation (:reason (ex-data error)))
                (throw error)))))))))

(defn- compressed-prg-bytes
  [artifact load-address payload-address scratch-address compression]
  (if-let [{:keys [layout order]}
           (prepare-compressed-layout (:segments artifact) payload-address
                                      compression)]
    (let [entry (long (:entry-point artifact))
          raw-layout (filterv #(= :raw (:mode %)) layout)
          raw-direction (when (seq raw-layout) (copy-direction raw-layout))
          compressed? (some #(not= :raw (:mode %)) layout)
          payload (vec (mapcat :bytes layout))
          table (vec (mapcat (fn [i]
                               (let [{:keys [dst len src mode]} (layout i)]
                                 [(u8 dst) (hi dst)
                                  (u8 len) (hi len)
                                  (u8 src) (hi src)
                                  (if (= mode :raw) 0 1)]))
                             order))
          table (into table (repeat compressed-table-entry-length 0x00))
          table-offset (* compressed-table-entry-length (count layout))]
      (when (> table-offset 0xff)
        (throw (ex-info "Compressed PRG has too many segments for its unpacker"
                        {:reason :too-many-segments
                         :segments (count layout)
                         :maximum (quot 0xff compressed-table-entry-length)})))
      (let [mover-options {:compressed? (boolean compressed?)
                           :raw-direction raw-direction}
            mover-forms (case compression
                          :lzss compressed-mover-forms
                          :zx0 zx0-mover-forms)
            code-length
            (count (assemble-bytes
                    (mover-forms scratch-address entry mover-options)
                    scratch-address))
            table-address (+ scratch-address code-length)
            code (assemble-bytes
                  (mover-forms table-address entry mover-options)
                  scratch-address)
            mover (into code table)]
        (check-scratch-size! scratch-address mover)
        (build-prg-image load-address payload-address scratch-address payload mover)))
    (legacy-prg-bytes artifact load-address payload-address scratch-address)))

(defn prg-bytes
  "Bytes of a PRG (including its two-byte load address) that installs `artifact`.

  Options:
  - `:load-address` — BASIC load address, default `$0801`;
  - `:payload-address` — packed segment payload, default `$0900`;
  - `:scratch-address` — unpacker destination, default `$0334`;
  - `:compression` — `:none` (default), `:lzss`, or `:zx0`.

  Compression is performed independently per segment. Segments that do not
  become smaller, cannot safely expand in place, or produce a cyclic copy
  dependency remain raw. The generated unpacker must fit in `$0334-$07e7`."
  ([artifact] (prg-bytes artifact {}))
  ([artifact {:keys [load-address payload-address scratch-address compression]
              :or {load-address default-load-address
                   payload-address default-payload-address
                   scratch-address default-scratch-address
                   compression :none}}]
   (let [load-address (long load-address)
         payload-address (long payload-address)
         scratch-address (long scratch-address)]
     (case compression
       :none (legacy-prg-bytes artifact load-address payload-address scratch-address)
       :lzss (compressed-prg-bytes artifact load-address payload-address
                                   scratch-address :lzss)
       :zx0 (compressed-prg-bytes artifact load-address payload-address
                                  scratch-address :zx0)
       (throw (ex-info "Unknown PRG compression mode"
                       {:reason :unknown-compression
                        :compression compression}))))))

(defn write!
  "Write `artifact`'s PRG to `file`, using optional PRG generation options."
  ([artifact file]
   (write! artifact file {}))
  ([artifact file options]
   (with-open [out (io/output-stream file)]
     (.write out ^bytes (prg-bytes artifact options)))
   (.getPath (io/file file))))

(defn write-temp-artifact-prg!
  "Write `artifact`'s self-relocating PRG to a fresh temp file and return its
  path. The caller owns the file and is responsible for deleting it."
  ([artifact]
   (write-temp-artifact-prg! artifact {}))
  ([artifact options]
   (let [directory (io/file "/tmp/omkamra-vice-v2")]
     (.mkdirs directory)
     (let [file (io/file directory (str "artifact-" (System/nanoTime) ".prg"))]
       (write! artifact file options)
       (.getPath file)))))
