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
      $080D  stub            disables IRQs, copies the unpacker to scratch, JMPs
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
            [omkamra.vice.assembler :as assembler]))

(def default-load-address 0x0801)
(def default-payload-address 0x0900)
(def default-scratch-address 0x0334)

(def ^:private basic-token-sys 0x9e)
(def ^:private basic-line-number 10)
(def ^:private basic-digits 5) ; zero-padded decimal operand
(def ^:private basic-line-length 11) ; link(2) number(2) token(1) digits(5) 00(1)
(def ^:private basic-end-marker-length 2)
(def ^:private table-entry-length 6) ; dst(2) len(2) src(2)

(defn- u8 ^long [x] (bit-and (long x) 0xff))

(defn- hi ^long [x] (u8 (bit-shift-right (long x) 8)))

(defn- stub-address
  [load-address]
  (+ (long load-address) basic-line-length basic-end-marker-length))

(defn- basic-sys-line
  "Tokenized BASIC `10 SYS <stub>` line followed by the end-of-program marker."
  [load-address stub]
  (let [digits (mapv #(u8 (int %)) (format (str "%0" basic-digits "d")
                                           (long stub)))
        link (+ (long load-address) basic-line-length)]
    (into [(u8 link) (hi link) basic-line-number 0x00 basic-token-sys]
          (conj digits 0x00))))

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
                                                     (:src (layout j)) (:len (layout j))))]
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

(defn prg-bytes
  "Bytes of a PRG (with its two-byte load address) that installs `artifact`.

  Options: `:load-address` (default `$0801`), `:payload-address` (default
  `$0900`), `:scratch-address` (default `$0334`)."
  ([artifact] (prg-bytes artifact {}))
  ([artifact {:keys [load-address payload-address scratch-address]
              :or {load-address default-load-address
                   payload-address default-payload-address
                   scratch-address default-scratch-address}}]
   (let [load-address (long load-address)
         payload-address (long payload-address)
         scratch-address (long scratch-address)
         entry (long (:entry-point artifact))
         segments (:segments artifact)
         ;; payload in increasing destination order
         layout (loop [ss (sort-by :start-address segments)
                       off 0
                       acc []]
                  (if-let [segment (first ss)]
                    (let [len (count (:bytes segment))]
                      (recur (rest ss)
                             (+ off len)
                             (conj acc {:dst (long (:start-address segment))
                                        :len len
                                        :src (+ payload-address off)
                                        :bytes (mapv u8 (:bytes segment))})))
                    acc))
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
         ;; the unpacker and its table live in scratch, so the table survives.
         ;; Size the code with an absolute table address: a zero table address
         ;; assembles the six table reads as zero-page indexed (2 bytes),
         ;; undercounting the code and placing the table six bytes too early.
         code-length (count (assemble-bytes (mover-forms scratch-address entry
                                                         direction)
                                            scratch-address))
         table-address (+ scratch-address code-length)
         code (assemble-bytes (mover-forms table-address entry direction)
                              scratch-address)
         mover (into code table)
         mover-length (count mover)
         stub (stub-address load-address)
         line (basic-sys-line load-address stub)
         mover-address (+ payload-address (count payload))
         end (+ mover-address mover-length)
         stub-bytes (assemble-bytes (stub-forms mover-address mover-length
                                                scratch-address)
                                    stub)
         image (byte-array (- end load-address))]
     (place! image load-address load-address
             (byte-array (concat line (repeat basic-end-marker-length 0x00))))
     (place! image load-address stub stub-bytes)
     (place! image load-address payload-address payload)
     (place! image load-address mover-address mover)
     (byte-array (concat [(u8 load-address) (hi load-address)] image)))))

(defn write!
  "Write `artifact`'s PRG to `file`."
  [artifact file]
  (with-open [out (io/output-stream file)]
    (.write out ^bytes (prg-bytes artifact)))
  (.getPath (io/file file)))

(defn write-temp-artifact-prg!
  "Write `artifact`'s self-relocating PRG to a fresh temp file and return its
  path. The caller owns the file and is responsible for deleting it."
  [artifact]
  (let [directory (io/file "/tmp/omkamra-vice-v2")]
    (.mkdirs directory)
    (let [file (io/file directory (str "artifact-" (System/nanoTime) ".prg"))]
      (write! artifact file)
      (.getPath file))))
