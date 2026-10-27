(ns omkamra.vice.examples.fairlight.legendary-intro.core-test
  "Guards the example's assembly and linking without starting VICE."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.vice.assembler :as assembler]
            [omkamra.vice.examples.fairlight.legendary-intro.core :as intro]
            [omkamra.vice.examples.fairlight.legendary-intro.music :as music]))

(def ^:private intro-ns
  "omkamra.vice.examples.fairlight.legendary-intro.core")

(def ^:private music-ns
  "omkamra.vice.examples.fairlight.legendary-intro.music")

(defn- linked []
  (assembler/link intro/intro))

(defn- bytes-at
  "The linked bytes starting at `address`."
  [artifact address length]
  (let [segment (first (filter (fn [{:keys [start-address bytes]}]
                                 (<= (long start-address)
                                     (long address)
                                     (+ (long start-address) (count bytes))))
                               (:segments artifact)))]
    (when segment
      (mapv #(bit-and (int %) 0xff)
            (take length (drop (- (long address) (long (:start-address segment)))
                               (:bytes segment)))))))

(defn- asset-address [artifact name]
  (:start-address (first (filter #(= name (:name %)) (:assets artifact)))))

(defn- object-section
  "The first placement section of the linked object with `object-id`."
  [artifact object-id]
  (let [object (first (filter #(= object-id (:object-id %)) (:objects artifact)))]
    (first (get-in object [:placement :sections]))))

(defn- object-bytes
  "The linked bytes of the object with `object-id`."
  [artifact object-id]
  (let [{:keys [base size]} (object-section artifact object-id)]
    (bytes-at artifact base size)))

(defn- code-bytes [artifact] (object-bytes artifact (keyword intro-ns "intro")))

(defn- matches-at?
  "True when `pattern` occurs in `bytes` at `index`; `nil` entries match any
  byte."
  [bytes pattern index]
  (every? (fn [[offset expected]]
            (or (nil? expected) (= expected (nth bytes (+ index offset) nil))))
          (map-indexed vector pattern)))

(defn- find-subsequence
  "Index of the first occurrence of `pattern` in `bytes`, or nil."
  [bytes pattern]
  (first (filter #(matches-at? bytes pattern %) (range (count bytes)))))

(defn- subsequence-indexes [bytes pattern]
  (filterv #(matches-at? bytes pattern %) (range (count bytes))))

(defn- handler-bytes [artifact] (object-bytes artifact (keyword intro-ns "irq")))

(defn- object-base
  "The linked base address of the object named `object` — its entry label."
  [artifact object]
  (:base (object-section artifact (keyword intro-ns (name object)))))

(defn- effect-bytes
  "The linked bytes of the per-frame effect object `object`."
  [artifact object]
  (object-bytes artifact (keyword intro-ns (name object))))

(defn- music-labels
  "The music object's labels, relative to its own start."
  []
  (:labels (first (:sections music/music))))

(defn- music-address
  "The linked address of the label `label` inside the music object."
  [artifact label]
  (+ (long (:base (object-section artifact (keyword music-ns "music"))))
     (long (get (music-labels) label))))

(deftest the-intro-links-with-the-vic-assets-in-place
  ;; Only the hardware-facing assets are pinned; the code and the CPU-only data
  ;; are placed by the linker, page-aligned where it matters.
  (let [artifact (linked)]
    (is (= (:base (object-section artifact (keyword intro-ns "intro")))
           (:entry-point artifact))
        "the entry point is the intro object's placement")
    (testing "pinned assets stay at their VIC address"
      (doseq [[asset address] {:charset 0xC800 :screen 0xC400 :color-ram 0xD800}]
        (is (= address (asset-address artifact (symbol intro-ns (name asset))))
            (str asset " stays at its VIC address"))))
    (testing "everything else is the linker's to place"
      (doseq [asset [:band-table :sprite-curve-bytes :sprite-bar-shades
                     :scroller-colors :scroller-text]]
        (is (< (asset-address artifact (symbol intro-ns (name asset))) 0xC000)
            (str asset " is placed by the linker, in low RAM"))))
    (testing "assets"
      (is (= #{:charset :screen :color-ram :band-table :sprite-curve-bytes
               :sprite-bar-shades :scroller-colors :scroller-text}
             (set (map (comp keyword name :name) (:assets artifact))))))))

(deftest the-sprite-curve-is-generated
  (is (= 256 (count intro/sprite-curve-bytes)))
  (is (= 50 (apply min intro/sprite-curve-bytes)))
  (is (= 123 (apply max intro/sprite-curve-bytes)))
  (is (= [87 89 92 95 97 100] (subvec intro/sprite-curve-bytes 0 6))))

(deftest the-sprite-bar-gradient-is-generated
  (is (= 21 (count intro/sprite-bar-shades)))
  (is (= [0x55 0x55 0xAA 0x55 0xAA 0xAA 0xAA 0xFF 0xAA 0xFF 0xFF 0xFF
          0xAA 0xFF 0xAA 0xAA 0xAA 0x55 0xAA 0x55 0x55]
         intro/sprite-bar-shades))
  (testing "expanded to the capture's three bytes per line"
    (is (= [0x55 0x55 0x55 0x55 0x55 0x55 0xAA 0xAA 0xAA 0x55 0x55 0x55
            0xAA 0xAA 0xAA 0xAA 0xAA 0xAA 0xAA 0xAA 0xAA 0xFF 0xFF 0xFF
            0xAA 0xAA 0xAA 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF
            0xAA 0xAA 0xAA 0xFF 0xFF 0xFF 0xAA 0xAA 0xAA 0xAA 0xAA 0xAA
            0xAA 0xAA 0xAA 0x55 0x55 0x55 0xAA 0xAA 0xAA 0x55 0x55 0x55
            0x55 0x55 0x55]
           (vec (mapcat #(repeat 3 %) intro/sprite-bar-shades))))))

(deftest the-handler-draws-the-sprite-bar
  (let [artifact (linked)
        bytes (effect-bytes artifact :sprite-bar)
        curve (asset-address artifact (symbol intro-ns "sprite-curve-bytes"))]
    (testing "one Y written to all eight sprite Y registers"
      (is (some? (find-subsequence bytes [0xBD (bit-and curve 0xff) (bit-shift-right curve 8)]))
          "LDA curve,X")
      (is (some? (find-subsequence bytes [0x99 0x01 0xD0]))))  ; STA $D001,Y
    (testing "the priority flip at the curve extremes"
      (is (some? (find-subsequence bytes [0xC9 0x32])))       ; CMP #$32
      (is (some? (find-subsequence bytes [0xC9 0x7B])))       ; CMP #$7B
      (is (some? (find-subsequence bytes [0x8D 0x1B 0xD0])))))) ; STA $D01B

(deftest the-generated-bitmap-region-is-reserved
  (is (= [[0xC3C0 0xC400]] (:reserved (:constraints intro/intro)))
      "the linker keeps out of the region `intro` fills at runtime"))

(deftest the-init-configures-the-sprites
  (let [bytes (code-bytes (linked))]
    (is (some? (find-subsequence bytes [0xA2 0x14 0xA0 0x3C]))
        "the bitmap expansion: LDX #20, LDY #60")
    (is (some? (find-subsequence bytes [0x99 0xC0 0xC3])) "STA $C3C0,Y")
    (is (some? (find-subsequence bytes [0x99 0xC1 0xC3])) "STA $C3C1,Y")
    (is (some? (find-subsequence bytes [0x99 0xC2 0xC3])) "STA $C3C2,Y")
    (is (some? (find-subsequence bytes [0xA9 0xFF 0x8D 0x15 0xD0])))
    (is (some? (find-subsequence bytes [0x8D 0x1C 0xD0])))
    (is (some? (find-subsequence bytes [0x8D 0x1D 0xD0])))
    (is (some? (find-subsequence bytes [0xA9 0xC0 0x8D 0x10 0xD0])))
    (is (some? (find-subsequence bytes [0xA9 0x0D 0xA2 0x07 0x9D 0x27 0xD0])))
    (is (some? (find-subsequence bytes [0xA9 0x05 0x8D 0x25 0xD0])))
    (is (some? (find-subsequence bytes [0xA9 0x01 0x8D 0x26 0xD0])))))

(deftest the-handler-is-a-separate-object
  ;; `intro` names the handler through its Var, which resolves to the handler
  ;; object's default entry label; the linker discovers and places it.
  (is (= (sort [:omkamra.vice.examples.fairlight.legendary-intro.core/irq
                :omkamra.vice.examples.fairlight.legendary-intro.music/init])
         (sort (:objects (:dependencies intro/intro)))))
  (is (nil? (:entry intro/irq)))
  (is (contains? (:labels intro/irq) :irq))
  (let [artifact (linked)
        handler (first (filter #(= (keyword intro-ns "irq") (:object-id %))
                               (:objects artifact)))]
    (is (= (get-in handler [:placement :sections 0 :base])
           (get (:labels handler) :irq))
        "the default entry label is at the handler's first instruction")))

(deftest the-irq-is-installed-on-raster-line-1
  (let [bytes (code-bytes (linked))
        ;; LDA #$01 / STA $D012 / STA $D01A: raster compare line 1, IRQ on.
        enable [0xa9 0x01 0x8d 0x12 0xd0 0x8d 0x1a 0xd0]
        index (find-subsequence bytes enable)]
    (is (some? index) "raster IRQ enable sequence is emitted")
    (testing "vector written after enabling, before CLI"
      (is (some? (find-subsequence (subvec bytes index)
                                   [0xa9 nil 0x8d 0x14 0x03
                                    0xa9 nil 0x8d 0x15 0x03 0x58]))))))

(deftest the-bands-are-positioned-on-the-credit-rows
  (testing "band write lines come from the capture"
    (is (= [170 171 173 174 175 176 177 178 179] intro/red-band-lines))
    (is (= [186 187 189 190 191 192 193 194 195] intro/blue-band-lines)))
  (testing "the bad line (≡ 4 mod 8) is the one skipped"
    (is (not-any? #(= 4 (mod % 8)) intro/red-band-lines))
    (is (not-any? #(= 4 (mod % 8)) intro/blue-band-lines))))

(deftest the-band-table-carries-every-line-of-the-bar
  ;; The loop writes one entry per raster line, so the table has an entry for
  ;; every line in the bar's range; pads position the two bands.
  (is (= 42 intro/band-length))
  (is (= [170 171 173 174 175 176 177 178 179] intro/red-band-lines))
  (is (= [186 187 189 190 191 192 193 194 195] intro/blue-band-lines))
  (testing "the bands sit on the rows they back"
    (is (= intro/red-band (subvec intro/band-table 15 24)))
    (is (= intro/blue-band (subvec intro/band-table 29 38))))
  (testing "everything between and after them is black"
    (is (every? zero? (concat (subvec intro/band-table 0 15)
                              (subvec intro/band-table 24 29)
                              (subvec intro/band-table 38 42))))))

(defn- vector-writes
  "The `[lo hi]` immediates of every `LDA #imm / STA $0314 / LDA #imm / STA $0315`
  group in `bytes`."
  [bytes]
  (mapv (fn [index] [(nth bytes (inc index)) (nth bytes (+ index 6))])
        (subsequence-indexes bytes [0xa9 nil 0x8d 0x14 0x03 0xa9 nil 0x8d 0x15 0x03])))

(deftest the-raster-handler-runs-in-one-interrupt
  ;; Pinning each band write to its raster line makes the interrupt-entry
  ;; cycle irrelevant, so a single handler does the whole frame and leaves
  ;; through the KERNAL tail (jiffy clock, keyboard, RTI).
  (let [intro-bytes (code-bytes (linked))
        irq-bytes (handler-bytes (linked))]
    (is (= 1 (count (vector-writes intro-bytes)))
        "the init installs the one handler")
    (is (some? (find-subsequence irq-bytes [0x4c 0x31 0xea]))
        "the handler leaves through $EA31")))

(deftest the-bands-loop-waits-for-the-line-then-for-each-change
  ;; The opening wait is exact; every following entry is written when `$D012`
  ;; changes. Reading the register twice per iteration, four cycles apart,
  ;; catches the change within four cycles — early enough to land in the new
  ;; line's first few, which is what the write has to do.
  (let [artifact (linked)
        bytes (effect-bytes artifact :bands)
        table (asset-address artifact (symbol intro-ns "band-table"))]
    (is (some? (find-subsequence bytes [0xa9 0x98 0xa2 0x00 0xcd 0x12 0xd0 0xd0 0xfb]))
        "LDA #$98 / LDX #$00 / CMP $D012 / BNE: wait for line 152")
    (is (some? (find-subsequence bytes [0xbd (bit-and table 0xff)
                                        (bit-shift-right table 8)]))
        "LDA band-table,X")
    (is (some? (find-subsequence bytes [0xad 0x12 0xd0 0xcd 0x12 0xd0 0xf0 0xfb]))
        "LDA $D012 / CMP $D012 / BEQ: the two-sample change wait")
    (is (some? (find-subsequence bytes [0x8c 0x21 0xd0 0xe8 0xe0 0x2a 0xd0]))
        "STY $D021 / INX / CPX #42 / BNE")))

(deftest the-handler-is-the-frames-running-order
  ;; The handler only acknowledges the interrupt and calls the frame's effects;
  ;; each effect is its own object, so the linker places them independently.
  (is (= (sort [:omkamra.vice.examples.fairlight.legendary-intro.core/bands
                :omkamra.vice.examples.fairlight.legendary-intro.core/scroller
                :omkamra.vice.examples.fairlight.legendary-intro.core/sprite-bar
                :omkamra.vice.examples.fairlight.legendary-intro.core/volume
                :omkamra.vice.examples.fairlight.legendary-intro.music/play])
         (sort (:objects (:dependencies intro/irq)))))
  (let [artifact (linked)
        bytes (handler-bytes artifact)
        jsr (fn [address]
              [0x20 (bit-and (long address) 0xff) (bit-shift-right (long address) 8)])
        call (fn [object] (jsr (object-base artifact object)))]
    (is (= [0xa9 0x01 0x8d 0x19 0xd0] (subvec bytes 0 5))
        "the interrupt is acknowledged first")
    (testing "one JSR per effect, in the order the frame draws them"
      ;; The SID player goes first, as in the original — it only advances its
      ;; voices every tenth frame. `volume` follows `sprite-bar`: it reads the Y
      ;; that the bar published this frame from `$D001`.
      (is (= [(jsr (music-address artifact :play))
              (call :sprite-bar) (call :volume) (call :bands) (call :scroller)]
             (mapv #(subvec bytes % (+ % 3)) [5 8 11 14 17]))))
    (is (= [0x4c 0x31 0xea] (subvec bytes 20 23))
        "it leaves through the KERNAL tail")))

(deftest the-scroller-text-is-a-string-in-screen-codes
  ;; The text is a plain Var holding a string; the assembler translates it with
  ;; the Var's `:encoding` metadata and pins it with `:at`, because the pointer
  ;; `$39/$3A` walks it a page at a time and wraps from $CBFF back to $CA00.
  (is (= 512 (count intro/scroller-text)) "two pages")
  (is (= "CRACKED ON THE 21ST OF NOVEMBER 1987...   "
         (subs intro/scroller-text 0 42))
      "the stream starts with the capture's text")
  (is (= "L8R" (subs intro/scroller-text 498 501))
      "and ends where the pointer wraps")
  (let [artifact (linked)
        address (asset-address artifact (symbol intro-ns "scroller-text"))
        encoded (bytes-at artifact address 512)]
    (is (zero? (mod address 512))
        "a page pair: the pointer walks it two pages at a time")
    (is (= [3 18 1 3 11 5 4] (subvec encoded 0 7)) "CRACKED, as screen codes")
    (is (= [0x20 0x20 0x20] (subvec encoded 39 42))
        "the runs of spaces are part of the stream")
    (is (every? #(<= 0x01 % 0x3A) encoded)
        "nothing is above the 64 characters the charset holds")))

(deftest the-scroller-colours-are-the-capture-ramp
  (is (= 40 (count intro/scroller-colors)) "one video-matrix row")
  (is (= (vec (mapcat #(repeat 5 %) [0x01 0x0F 0x0C 0x0B]))
         (subvec intro/scroller-colors 0 20)))
  (is (= (subvec intro/scroller-colors 0 20)
         (subvec intro/scroller-colors 20 40))
      "the four-run ramp repeats"))

(deftest the-init-hands-the-scroller-its-state
  (let [bytes (code-bytes (linked))]
    (is (some? (find-subsequence bytes [0xa9 0x00 0x85 0x09]))
        "LDA #$00 / STA $09: the scroll value starts at 0, so frame 1 wraps")
    (is (some? (find-subsequence bytes [0x85 0x39])) "STA $39")
    (is (some? (find-subsequence bytes [0x85 0x3a 0x85 0x3c 0x18 0x69 0x02 0x85 0x3b]))
        "the start page and, two pages on, the page to wrap at")
    (is (some? (find-subsequence bytes [0x9d 0x20 0xdb]))
        "STA $DB20,X: the colour ramp copy")
    (is (some? (find-subsequence bytes [0xe0 0x28])) "CPX #40: the whole row")))

(deftest the-scroller-splits-row-20-and-shifts-both-rows
  (let [bytes (effect-bytes (linked) :scroller)]
    (testing "the $D016 split at line 210, restored around line 218"
      (is (some? (find-subsequence bytes [0xa9 0xd2 0xcd 0x12 0xd0 0xd0 0xfb]))
          "LDA #$D2 / CMP $D012 / BNE: wait for line 210")
      (is (some? (find-subsequence bytes [0xa5 0x09 0x8d 0x16 0xd0]))
          "LDA $09 / STA $D016: the fine scroll, multicolour off")
      (is (some? (find-subsequence bytes [0xa2 0x64 0xca 0xd0 0xfd]))
          "LDX #100 / DEX / BNE: ~500 cycles")
      (is (some? (find-subsequence bytes [0xa9 0xd8 0x8d 0x16 0xd0]))
          "LDA #$D8 / STA $D016: back to multicolour"))
    (testing "the video matrix shifts on the wrap frame"
      (is (some? (find-subsequence bytes [0xbd 0x21 0xc7 0x9d 0x20 0xc7]))
          "LDA $C721,X / STA $C720,X")
      (is (some? (find-subsequence bytes [0xa1 0x39 0x8d 0x47 0xc7]))
          "LDA ($39,X) / STA $C747: the new character")
      (is (some? (find-subsequence bytes [0xe6 0x39])) "INC $39")
      (is (some? (find-subsequence bytes [0xe6 0x3a])) "INC $3A at the page boundary")
      (is (some? (find-subsequence bytes [0xa5 0x3a 0xc5 0x3b]))
          "LDA $3A / CMP $3B: the page the stream wraps at")
      (is (some? (find-subsequence bytes [0xa5 0x3c 0x85 0x3a]))
          "LDA $3C / STA $3A: back to the page it starts on"))
    (testing "the colour row shifts every frame"
      (is (some? (find-subsequence bytes [0xad 0x20 0xdb 0x48]))
          "LDA $DB20 / PHA: save the byte leaving column 0")
      (is (some? (find-subsequence bytes [0xbd 0x21 0xdb 0x9d 0x20 0xdb]))
          "LDA $DB21,X / STA $DB20,X")
      (is (some? (find-subsequence bytes [0x68 0x8d 0x47 0xdb]))
          "PLA / STA $DB47: re-insert it at column 39"))))

(deftest the-scroller-row-is-derived-from-the-pinned-assets
  ;; The handler does not write `$C720`/`$DB20`: it computes the row from the
  ;; screen and colour-RAM assets, and the linker resolves it.
  (is (= 20 intro/scroller-row))
  (is (= 40 intro/screen-columns))
  (is (= 800 intro/scroller-row-offset))
  (is (= 39 intro/scroller-shift-columns))
  (is (= 0xC720 (+ 0xC400 intro/scroller-row-offset))
      "row 20 of the video matrix")
  (is (= 0xDB20 (+ 0xD800 intro/scroller-row-offset))
      "row 20 of colour RAM")
  (let [bytes (effect-bytes (linked) :scroller)]
    (is (some? (find-subsequence bytes [0xbd 0x21 0xc7 0x9d 0x20 0xc7]))
        "which still links to LDA $C721,X / STA $C720,X")
    (is (some? (find-subsequence bytes [0xbd 0x21 0xdb 0x9d 0x20 0xdb]))
        "and to LDA $DB21,X / STA $DB20,X")))

(deftest the-zero-page-variables-are-named
  ;; The handler and init name their zero-page cells instead of repeating the
  ;; addresses; the assembler folds the names away, so the byte-level tests
  ;; below still see `$02`, `$09`, `$39`, `$3A`, `$3B` and `$3C`.
  (is (= 0x02 intro/frame-counter))
  (is (= 0x09 intro/scroll-value))
  (is (= 0x39 intro/text-pointer))
  (is (= 0x3A intro/text-pointer-high) "the high byte follows the low")
  (is (= 0x3B intro/text-wrap-page))
  (is (= 0x3C intro/text-start-page)))

(deftest the-volume-fades-in-once-per-bar-pass
  ;; One byte is the SID volume, the fade counter and the operand of the `LDA`
  ;; that reads it; the routine patches that operand in place, as the original
  ;; does. Every branch lands on the `LDA`, so the published value is always the
  ;; byte — never the Y the routine happened to read.
  (let [artifact (linked)
        bytes (effect-bytes artifact :volume)
        base (object-base artifact :volume)
        patch (find-subsequence bytes [0xa9 0x10 0x8d 0x18 0xd4 0x60])
        operand-address (+ base patch 1)
        inc-at (find-subsequence bytes [0xee])
        branch-target (fn [at]
                        (let [d (nth bytes (inc at))]
                          (+ at 2 (if (>= d 128) (- d 256) d))))]
    (is (some? (find-subsequence bytes [0xad 0x01 0xd0 0xc9 0x33]))
        "LDA $D001 / CMP #$33: sprite 0's Y, at the bump point")
    (is (some? (find-subsequence bytes [0xad nil nil 0xc9 0x1f 0xf0 0x03 0xee]))
        "LDA volume-byte / CMP #$1F / BEQ / INC volume-byte: stop at full volume")
    (is (= operand-address
           (+ (nth bytes (inc inc-at)) (* 256 (nth bytes (+ inc-at 2)))))
        "the INC patches the immediate's own operand byte")
    (is (some? patch) "the initial value is in the image: $10, volume 0")
    (is (= [patch patch]
           [(branch-target (+ 2 (find-subsequence bytes [0xc9 0x33])))
            (branch-target (+ 2 (find-subsequence bytes [0xc9 0x1f])))])
        "both branches land on the LDA, so it publishes the byte")))

(deftest the-sprite-bar-publishes-its-y-before-the-volume-reads-it
  ;; `volume` reads `$D001`, so `sprite-bar` must write the Y registers high
  ;; register first, ending on sprite 0's — otherwise the read is a frame stale.
  (let [bytes (effect-bytes (linked) :sprite-bar)]
    (is (some? (find-subsequence bytes [0xa0 0x0e 0x99 0x01 0xd0 0x88 0x88 0x10 0xf9]))
        "LDY #$0E / STA $D001,Y / DEY / DEY / BPL: $D001 is written last")))

(deftest the-tune-image-points-at-its-streams
  ;; The tune is a relocatable object: its three voice pointers are words
  ;; referring to the streams' own labels, so nothing here knows about $CE00.
  (let [artifact (linked)
        table (music-address artifact :tune-pointers)
        addresses (mapv (fn [[lo hi]] (+ lo (* 256 hi)))
                        (partition 2 (bytes-at artifact table 6)))]
    (is (= [(music-address artifact :tune-lead)
            (music-address artifact :tune-bass)
            (music-address artifact :tune-lead)]
           addresses)
        "one pointer per voice, in voice order: lead, bass, lead")
    (testing "each pointer addresses the compiled stream, closed by zeroes"
      (doseq [[stream address] [[:bass (music-address artifact :tune-bass)]
                                [:lead (music-address artifact :tune-lead)]]]
        (let [expected (music/stream-bytes (music/streams stream))]
          (is (= expected (bytes-at artifact address (count expected))))
          (is (= [0 0 0] (subvec expected (- (count expected) 3)))
              "the stream ends with the player's restart marker")
          (is (= [0 0 0]
                 (bytes-at artifact (+ address (- (count expected) 3)) 3))))))
    (testing "the streams are back to back, as in the original"
      (is (= (music-address artifact :tune-lead)
             (+ (music-address artifact :tune-bass)
                (count (music/stream-bytes (music/streams :bass)))))))))

(deftest the-player-keeps-its-state-in-one-block
  ;; Every access is `lda [:row x]` with X the voice index, which works only if
  ;; the rows are three bytes — one per voice — apart, in this order, wherever
  ;; the linker puts the block.
  (let [labels (music-labels)
        rows [:counter :freq-lo :freq-hi :gate :control :ptr-lo :ptr-hi]
        addresses (mapv #(long (get labels %)) rows)]
    (is (= (mapv #(+ % 3) (butlast addresses)) (vec (rest addresses)))
        "each row is three bytes after the last"))
  (let [artifact (linked)]
    (is (= [(nth music/sid-init 4) (nth music/sid-init 11) (nth music/sid-init 18)]
           (bytes-at artifact (music-address artifact :control) 3))
        "the control row starts as the voices' gate-off bytes")
    (is (= [0 0 0] (bytes-at artifact (music-address artifact :gate) 3))
        "the gate flags start off")
    (is (= [0 0 0] (bytes-at artifact (music-address artifact :ptr-lo) 3))
        "and so do the stream pointers, which `init` fills from the tune")))

(deftest the-player-advances-its-voices-every-tenth-frame
  ;; `play` decrements one divider byte and returns; only when it reaches zero
  ;; does it reload it with ten and walk the streams. That is the whole reason
  ;; the volume fade can be published in between, every frame.
  (let [artifact (linked)
        bytes (bytes-at artifact (music-address artifact :play) 12)
        tick (music-address artifact :tick)
        low (bit-and tick 0xff)
        high (bit-shift-right tick 8)]
    (is (= [0xce low high 0xf0] (subvec bytes 0 4))
        "DEC tick / BEQ: nine calls in ten fall through")
    (is (= [0x60 0xa9 music/tick-frames 0x8d low high] (subvec bytes 5 11))
        "RTS / LDA #10 / STA tick: the tenth advances them")))

(deftest the-player-starts-the-sid-from-its-own-copy
  ;; `init` writes $D400-$D418 out of the image's own table, so the register
  ;; values are ordinary data here rather than a literal in the code.
  (let [artifact (linked)
        sid-init (music-address artifact :sid-init)
        bytes (bytes-at artifact (music-address artifact :init) 8)]
    (is (= [0xa2 0x18 0xbd (bit-and sid-init 0xff) (bit-shift-right sid-init 8) 0x9d 0x00 0xd4]
           bytes)
        "LDX #24 / LDA sid-init,X / STA $D400,X")
    (is (= (vec music/sid-init) (bytes-at artifact sid-init 25))
        "the table is the SID's opening register values")))
