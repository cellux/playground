(ns omkamra.vice.examples.fairlight.legendary-intro.core
  "Clean-room reimplementation of the Fairlight “legendary” intro.

  Step 1 — the static screen. The intro runs in VIC bank 3 ($C000) with a
  custom multicolour charset at $C800, a screen matrix at $C400 and colour RAM
  at $D800. The assets are placed directly at their VIC addresses by the
  self-relocating PRG: screen and charset through RAM, colour RAM through the
  CPU-visible bank (the RAM bank cannot reach it).

  Step 2 — the raster IRQ and the rasterbars. One raster interrupt per frame,
  on line 1; the handler acknowledges it and paints the two colour bands, one
  `$D021` write per raster line, positioned on the credit rows they back. It
  leaves through the KERNAL IRQ tail at $EA31. The mainline is just an idle
  loop: everything happens in the handler.

  Later steps slot into the same handler: the music (step 6) first, the sprite
  bar (step 3) before the bands, the text scroller (step 4) after them. The handler
  uses KERNAL *entry points* ($EA31), never KERNAL *vectors*, because the
  runner autostarts into a freshly booted machine whose vectors are only just
  being set up.

  Step 3 — the sprite bar. Eight X-expanded multicolour sprites share one
  21-line gradient bitmap (`$C3C0`) and one Y per frame, from a generated sine
  curve (`$C2C0`) indexed by the frame counter; `$D01B` flips only at the
  curve's extremes, so the bar passes in front of the logo on the way up and
  behind it on the way down.

  Step 4 — the text scroller. Row 20 scrolls left: the video matrix by one
  column every eighth frame, the colour RAM by one column *every* frame, so a
  light/dark grey ramp sweeps across the text eight times faster than the
  characters move. The split is at line 210 (`$D016` = the fine-scroll value
  `$09`, multicolour off) and restored around line 218. The text stream is a
  plain Var holding a string, which the assembler both translates to screen
  codes and places; the pointer `$39/$3A` walks it a byte at a time and the
  handler remembers the pages it spans.

  Step 6 — the SID player, in `omkamra.vice.examples.fairlight.legendary-intro.music`.
  The handler calls it first, before the sprite bar, and the initialisation
  before anything touches the VIC, both as in the original. It advances its
  voices only every tenth frame; the volume fade runs in between, every frame."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler]
            [omkamra.vice.examples.common :as common]
            ;; Referenced only through its labels — `::music/init`, `::music/play`
            ;; — inside bodies the assembler reads as data, which is invisible to
            ;; the linter.
            ^{:clj-kondo/ignore [:unused-namespace]}
            [omkamra.vice.examples.fairlight.legendary-intro.music :as music]))

(assembler/define-asset charset
  {:resource "./charset.bin" :at 0xC800})

(assembler/define-asset screen
  {:resource "./screen.bin" :at 0xC400})

(assembler/define-asset color-ram
  {:resource "./color.bin" :at 0xD800})

(defn- row-line
  "First raster line of character row `row` on screen: 25 rows with YSCROLL 3,
  i.e. row 0 covers lines 48-55."
  [row]
  (+ 48 (* 8 row)))

;; The rasterbars.
;;
;; A `$D021` store takes effect immediately, but the VIC latches the register
;; early in the line — around cycle 9 — so a store later than that shows up a
;; line late. `draw-bands` waits for one exact line and then writes one entry
;; per *following* raster line, waiting each time for `$D012` to change; that
;; catches the new line within four cycles and so lands in the first few cycles
;; of it. The table therefore carries an entry for every line of the bar's
;; range, with pads positioning the two bands — the original's `$C280` table.
;;
;; Bad lines are where it shows: on every eighth line (`$D012 & 7` = YSCROLL +
;; 1 = 4) the VIC steals ~40 cycles mid-line to fetch the video matrix and
;; colour RAM, so the store is held past the visible pixels and the line keeps
;; the previous colour. That doubling is exactly what the capture shows, and it
;; falls out of the mechanism rather than being arranged.

(def band-start-line
  "The line the band loop's opening wait lands on; the first entry is written
  on the next line."
  152)

(def band-end-line
  "The last line the bar paints. The original's table runs on past the bands to
  here writing black, and those entries are part of its write pattern."
  200)

(defn bad-line?
  "True on the lines where the VIC steals cycles for the video matrix.

  The loop consumes one entry per line that is *not* one of these — the stall
  makes it skip them — so the table is indexed by the painted, non-bad lines."
  [line]
  (= 4 (mod line 8)))

(defn painted-lines
  "The `n` non-bad lines a band of `n` entries occupies, starting at `start`."
  [start n]
  (->> (iterate inc start)
       (remove bad-line?)
       (take n)
       vec))

(def red-band
  "`$D021` entries written behind `COMBAT SCHOOL`: the capture's eight colours,
  then black to restore the background."
  [0x02 0x0A 0x01 0x01 0x01 0x0A 0x0A 0x02 0x00])

(def blue-band
  "`$D021` entries written behind `CRACKED 21.11.87 BY STRIDER`."
  [0x06 0x0E 0x0E 0x01 0x01 0x0E 0x0E 0x06 0x00])

(def red-band-lines
  "Raster lines the red band paints: 170-178 with the bad line 172 skipped,
  then black on 179."
  (painted-lines (+ 2 (row-line 15)) (count red-band)))

(def blue-band-lines
  "Raster lines the blue band paints: 186-194 with the bad line 188 skipped,
  then black on 195."
  (painted-lines (+ 2 (row-line 17)) (count blue-band)))

(defn black-run
  "The black entries that carry the bar from `from` to `to`: one per painted
  line in `[from to)`."
  [from to]
  (repeat (count (remove bad-line? (range from to))) 0x00))

;; The table is read with `LDA table,X` over its whole length, so `^{:align
;; true}` — the next power of two ≥ the byte count — keeps the access inside
;; one page.

(def ^{:align true} band-table
  "Black up to the red band, the band, black up to the blue band, the band, and
  the trailing black the original writes too: one entry per painted line, and
  it comes out equal to the capture's `$C280` byte for byte."
  (vec (concat (black-run (inc band-start-line) (first red-band-lines))
               red-band
               (black-run (inc (last red-band-lines)) (first blue-band-lines))
               blue-band
               (black-run (inc (last blue-band-lines)) (inc band-end-line)))))

(def band-length
  "Entries in `band-table`, and so the loop's bound."
  (count band-table))

;; The sprite bar's Y curve and bitmap. The curve is generated from a formula
;; that reproduces the capture's 256 bytes exactly. The bitmap's 21 line shades
;; are transcribed (there is no formula for the dither), and `intro` expands
;; them into the three bytes per line the multicolour sprite reads. Both are
;; plain vectors referenced in operand position, so the linker places them.

(def ^{:align true} sprite-curve-bytes
  "Bar Y per frame-counter value: three sine cycles, clipped to 50-123."
  (mapv (fn [k]
          (let [y (+ 86.5 (* 37.0 (Math/sin (/ (* 6.0 Math/PI k) 256))))]
            (min 123 (max 50 (Math/round y)))))
        (range 256)))

(def ^{:align true} sprite-bar-shades
  "Shade of each of the bar's 21 lines: `$55` green (`$D025`), `$AA` light
  green (`$D027`), `$FF` white (`$D026`). A multicolour sprite reads three
  bytes per line, so `intro` expands each shade into three bytes at `$C3C0`."
  [0x55 0x55 0xAA 0x55 0xAA 0xAA 0xAA 0xFF 0xAA 0xFF 0xFF 0xFF
   0xAA 0xFF 0xAA 0xAA 0xAA 0x55 0xAA 0x55 0x55])

;; The scroller runs on one character row of the video matrix. With the screen
;; pinned at `$C400` that row is `$C720`, and `$DB20` in colour RAM; the code
;; addresses it from the assets rather than repeating those numbers, so the
;; geometry (the row, the width, where the shifted column goes) has one home.

(def scroller-row
  "The character row the scroller runs on: row 20."
  20)

(def screen-columns
  "The VIC's video matrix is 40 columns wide, so one row is 40 bytes."
  40)

(def scroller-row-offset
  "Byte offset of the scroller row within the video matrix and colour RAM."
  (* screen-columns scroller-row))

(def scroller-shift-columns
  "How many columns each shift moves: all but the last, which is where the
  next character enters from the text stream."
  (dec screen-columns))

;; The scroller's two tables. `scroller-text` is a plain Var holding a string:
;; the assembler turns it into bytes with the `:charset` metadata (screen codes
;; — the VIC reads the video matrix) and places it like any other data. It is
;; transcribed from the capture (the runs of spaces are part of the stream, not
;; formatting) and, because the pointer walks it a page at a time, it is page
;; pair aligned: `intro` records where it starts and where it wraps.

(def ^{:align true} scroller-colors
  "The row-20 colour ramp `intro` copies to `$DB20`: white, light grey, grey
  and dark grey, five columns each, twice."
  (vec (mapcat #(repeat 5 %) [0x01 0x0F 0x0C 0x0B 0x01 0x0F 0x0C 0x0B])))

(def ^{:charset :screen-codes :align true} scroller-text
  "The scroller's text stream — exactly two pages, so the pointer's page walk
  covers it and the handler knows where to wrap."
  (str "CRACKED ON THE 21ST OF NOVEMBER 1987...   "
       "NOW YOU CAN TRAIN YOURSELF TO KILL COMMUNISTS AND IRANIANS...    "
       "LATEST TOP PIRATES : BEASTIE BOYS  IKARI  ACE  HOTLINE  DANISH GOLD"
       "  NEW WIZAX  TPI  TLC  ANTITRAX  C64CG  TRIAD  1001 CREW"
       "  YETI  TRITON T  FCS  SCA    "
       "OVERSEAS : EAGLESOFT  FBR  SOL  NEPA  ABYSS  XPB  TS  TIH          "
       "PRAY THAT YOU WILL GET AN INVITATION TO OUR GREAT COPY PARTY IN"
       " STOCKHOLM IN DECEMBER...        "
       "FUCKINGS TO WATCHER OF THE SILENTS. YOU'LL NOT DESTROY THIS PARTY...       "
       "L8R           "))

(defn draw-bands
  "Assembly macro: paint `band-table` onto `$D021`, one entry per raster line,
  from the line the opening wait lands on.

  The opening wait is exact. Each following entry is written when `$D012`
  changes: reading it twice per iteration, four cycles apart, catches the
  change within four cycles, so the store lands in the new line's first few —
  early enough for that line's own colour, and late enough on a bad line that
  the VIC keeps the previous one, which is the capture's doubled line.

  `%labels` keeps the loop's labels unique."
  []
  `(%labels
    [:loop :wait :changed]
    (lda band-start-line)
    (ldx 0x00)
    :wait
    (cmp [0xD012])
    (bne :wait)
    :loop
    (lda [band-table x])
    (tay)
    (lda [0xD012])
    :changed
    (cmp [0xD012])
    (beq :changed)
    (sty [0xD021])
    (inx)
    (cpx band-length)
    (bne :loop)))

(defn shift-row-left
  "Assembly macro: shift the `scroller-shift-columns` bytes of the scroller row
  at `base` one column left — column 1 into column 0. The video matrix and the
  colour row shift the same way; only what enters the last column differs, so
  that is left to the caller. `%labels` keeps the loop label unique per
  expansion, so both rows can use it."
  [base]
  `(%labels
    [:shift]
    (ldx 0x00)
    :shift
    (lda [(+ ~base scroller-row-offset 1) x])
    (sta [(+ ~base scroller-row-offset) x])
    (inx)
    (cpx scroller-shift-columns)
    (bne :shift)))

;; Zero-page variables.
;;
;; These are the cells the original uses for the same values, and they are
;; free: the only other code we run is the KERNAL IRQ tail at `$EA31`, which
;; uses `$A0-$A2` (jiffy clock), `$C5/$C6` (keyboard buffer) and `$CB/$CC`
;; (cursor), not these. Naming them keeps the addresses in one place; the
;; assembler folds the names away, so the code bytes are unchanged.

(def frame-counter 0x02)                ; the sprite curve index, per frame
(def scroll-value 0x09)                 ; `$D016` fine scroll, and wrap counter

;; The text pointer is the one place zero page is not a choice: `LDA (zp,X)`
;; reaches nothing else. `text-pointer` is its low byte, the high byte follows
;; it; the last two remember where the stream starts and where it wraps,
;; because where the linker puts the stream is the linker's business.
(def text-pointer 0x39)
(def text-pointer-high (inc text-pointer))
(def text-wrap-page 0x3B)
(def text-start-page 0x3C)

;; Each per-frame effect is its own object, entered from `irq` with `JSR`: the
;; linker places them independently, each keeps its own local labels, and the
;; handler reads as the frame's running order. The 12 cycles a call costs are
;; harmless — the bands and the scroller wait for their exact raster lines.

(assembler/define-asm sprite-bar
  ;; One Y for all eight sprites, from the curve, indexed by the frame counter.
  (inc [frame-counter])
  (ldx [frame-counter])
  (lda [sprite-curve-bytes x])
  (ldy 0x0E)
  :sprite-y
  (sta [0xD001 y])
  (dey)
  (dey)
  (bpl :sprite-y)
  ;; `$D01B` changes only at the curve's extremes: in front of the logo at the
  ;; top (Y=$32), behind it at the bottom (Y=$7B).
  (cmp 0x32)
  (bne :sprite-not-top)
  (lda 0x00)
  :sprite-priority
  (sta [0xD01B])
  (jmp :sprite-done)
  :sprite-not-top
  (cmp 0x7B)
  (bne :sprite-done)
  (lda 0xFF)
  (jmp :sprite-priority)
  :sprite-done
  (rts))

(assembler/define-asm volume
  ;; The SID volume fade. One byte does three jobs, as in the original: it is
  ;; the value written to `$D418`, the fade counter, and the immediate operand
  ;; of the `LDA` below, patched in place. It starts at `$10` — volume 0 with
  ;; voice 3 off — and steps up each time the bar passes Y = `$33`, so the tune
  ;; fades in over fifteen passes. It stops at `$1F`, which is full volume and
  ;; the point where the original arms its quit: past it, bit 5 would switch
  ;; the filter on.
  ;;
  ;; Every branch lands on the `LDA`, so the published value always comes from
  ;; the byte and never from the Y this routine happened to read.
  (lda [0xD001])                      ; the bar's Y, as sprite 0 holds it
  (cmp 0x33)
  (bne :volume-value)                 ; not at the bump point: just publish
  (lda [(+ :volume-value 1)])         ; already at full volume?
  (cmp 0x1F)
  (beq :volume-value)
  (inc [(+ :volume-value 1)])         ; patch the operand in place
  :volume-value
  (lda 0x10)                          ; volume 0, voice 3 off, to start
  (sta [0xD418])
  (rts))

(assembler/define-asm bands
  ;; The two rasterbar bands, on the credit rows they back: one table and one
  ;; loop, exactly as the original has it.
  (draw-bands)
  (rts))

(assembler/define-asm scroller
  ;; Row 20 (`$C720`, colour `$DB20`). The row is split in at line 210: `$09`
  ;; is the fine scroll, 0-7, and doubles as the wrap counter. XSCROLL takes
  ;; effect at once, so the write has to land on an exact line; 210 and the
  ;; ~line-218 restore are both clear of the bad lines.
  (lda 0xD2)
  :scroll-split
  (cmp [0xD012])
  (bne :scroll-split)
  (lda [scroll-value])
  (sta [0xD016])
  (ldx 0x64)                          ; ~500-cycle delay: back to normal mid-row
  :scroll-delay
  (dex)
  (bne :scroll-delay)
  (lda 0xD8)
  (sta [0xD016])
  (dec [scroll-value])
  (lda [scroll-value])
  (cmp 0xFF)
  (bne :shift-colors)
  ;; The scroll wrapped: shift the video-matrix row left and pull the next
  ;; screen code in at column 39 through the text pointer.
  (lda 0x07)
  (sta [scroll-value])
  (shift-row-left screen)
  (ldx 0x00)                          ; the shift left X at 39
  (lda [[text-pointer x]])            ; the byte the text pointer addresses
  (sta [(+ screen scroller-row-offset scroller-shift-columns)])
  (inc [text-pointer])
  (bne :shift-colors)
  (inc [text-pointer-high])
  (lda [text-pointer-high])
  (cmp [text-wrap-page])              ; `intro` put the wrap page there
  (bne :shift-colors)
  (lda [text-start-page])
  (sta [text-pointer-high])
  ;; The colour row shifts every frame, so the ramp sweeps the text eight
  ;; times faster than the characters move; the byte leaving column 0
  ;; re-enters at column 39.
  :shift-colors
  (lda [(+ color-ram scroller-row-offset)])
  (pha)
  (shift-row-left color-ram)
  (pla)
  (sta [(+ color-ram scroller-row-offset scroller-shift-columns)])
  (rts))

(assembler/define-asm irq
  ;; The per-frame handler, in its own object so `intro` can name it. It has no
  ;; `%entry`: `define-asm` gives it the default entry label `:irq` at its first
  ;; instruction, so `(lda (< irq))` resolves here. All it does is acknowledge
  ;; the interrupt and run the frame's effects in order.
  ;; One interrupt per frame, on line 1.
  (lda 0x01) (sta [0xD019])          ; acknowledge
  (jsr ::music/play)                  ; first, as in the original: it advances
  (jsr sprite-bar)                    ; every tenth frame, the fade every one
  (jsr volume)                        ; after the bar: it reads this frame's Y
  (jsr bands)
  (jsr scroller)
  (jmp 0xEA31))                       ; KERNAL IRQ tail: UDTIM, keyboard, RTI

(assembler/define-asm intro
  (%entry :start)
  (%asset charset)
  (%asset screen)
  (%asset color-ram)
  :start
  (sei)                              ; no interrupts while setting up
  ;; The SID player's own initialisation comes first, as in the original: it
  ;; writes the SID's opening registers and points each voice at its stream.
  (jsr ::music/init)
  ;; VIC bank 3 ($C000); screen matrix $C400, charset $C800.
  (lda 0x94) (sta [0xDD00])
  (lda 0x12) (sta [0xD018])
  ;; Display on, 24 rows, YSCROLL 3; multicolour text mode, 40 columns.
  (lda 0x1B) (sta [0xD011])
  (lda 0xD8) (sta [0xD016])
  ;; Border and background black, the two multicolour backgrounds.
  (lda 0x00)
  (sta [0xD020])
  (sta [0xD021])
  (lda 0x0A) (sta [0xD023])
  (lda 0x02) (sta [0xD022])
  ;; Generate the sprite bitmap into bank-3 RAM at $C3C0 (the sprite data
  ;; pointer `$0F` = `$C000 + $0F*64` points there). `%reserve` keeps the
  ;; linker out of the range while emitting nothing: the loop below is the only
  ;; source of its content.
  (%reserve 0xC3C0 64)
  ;; Expand the bar's 21 line shades into the three bytes per line a
  ;; multicolour sprite reads.
  (ldx 0x14)                          ; line 20
  (ldy 0x3C)                          ; its first byte, 3*20
  :sprite-bitmap
  (lda [sprite-bar-shades x])
  (sta [0xC3C0 y])
  (sta [0xC3C1 y])
  (sta [0xC3C2 y])
  (dey)
  (dey)
  (dey)
  (dex)
  (bpl :sprite-bitmap)
  ;; Eight sprites, all on, X-expanded and multicolour; light-green bodies with
  ;; green/white multicolour. Sprite 3 keeps X = 0, overlapping sprite 0 (as in
  ;; the capture: the bitmap varies only vertically).
  (lda 0xFF)
  (sta [0xD015])
  (sta [0xD01C])
  (sta [0xD01D])
  (lda 0xC0) (sta [0xD010])
  (lda 0x18) (sta [0xD000])
  (lda 0x48) (sta [0xD002])
  (lda 0x78) (sta [0xD004])
  (lda 0xA8) (sta [0xD008])
  (lda 0xD8) (sta [0xD00A])
  (lda 0x08) (sta [0xD00C])
  (lda 0x38) (sta [0xD00E])
  (lda 0x0D)
  (ldx 0x07)
  :sprite-colors
  (sta [0xD027 x])
  (dex)
  (bpl :sprite-colors)
  (lda 0x05) (sta [0xD025])
  (lda 0x01) (sta [0xD026])
  (lda 0x00) (sta [frame-counter])
  ;; Scroller state: the fine scroll written to `$D016` for the scroller row,
  ;; which starts at 0 so the first frame is already a wrap frame.
  (lda 0x00)
  (sta [scroll-value])
  ;; The text stream: `$39/$3A` walks it, `text-start-page` is the page it
  ;; starts on and `text-wrap-page` the page it wraps at, two pages on — so
  ;; the handler wraps at whatever address the linker chose.
  (lda (< scroller-text))
  (sta [text-pointer])
  (lda (> scroller-text))
  (sta [text-pointer-high])
  (sta [text-start-page])
  (clc)
  (adc 0x02)
  (sta [text-wrap-page])
  ;; The colour row starts as the ramp; the handler rotates it from there.
  (ldx 0x00)
  :copy-colors
  (lda [scroller-colors x])
  (sta [(+ color-ram scroller-row-offset) x])
  (inx)
  (cpx screen-columns)
  (bne :copy-colors)
  ;; One raster interrupt per frame, on line 1, with no CIA1 timer interrupt.
  ;; Clear any VIC interrupt flag the machine is already holding first: enabling
  ;; the raster IRQ with a stale flag set runs the handler at whatever line the
  ;; init happened to finish on, and the frame's line waits then slip a frame
  ;; and lock there — which drifts the band writes' cycles with them.
  (lda 0x0F) (sta [0xD019])
  (lda 0x01)
  (sta [0xD012])
  (sta [0xD01A])
  (lda 0x7F) (sta [0xDC0D])
  ;; The handler object, resolved through its Var to its default entry label.
  (lda (< irq)) (sta [0x0314])
  (lda (> irq)) (sta [0x0315])
  (cli)
  :idle
  (jmp :idle))

(defn run!
  "Run the intro on a fresh VICE instance.

  Arguments are passed straight to `omkamra.vice/start!`; for this relocatable
  object that means an optional map with `:link-options` and start options such
  as `:from-pc` and `:connect-timeout-ms`."
  [& args]
  (apply common/run! intro args))
