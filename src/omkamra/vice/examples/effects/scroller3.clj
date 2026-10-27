(ns omkamra.vice.examples.effects.scroller3
  "A four-pixel C64 text scroller drawn with a 4x4 charset reduced by
  deduplication.

  Every on-screen character is a 32x32 cell built from sixteen ordinary 8x8
  characters. A naive 4x4 charset would hold 64 scaled characters, sixteen
  tiles each -- 8 KB, four times the 2 KB the VIC can address. The scale is
  rigid, though: an 8x8 tile at tile position (row, column) covers a 2x2 block
  of source pixels, each pixel becoming a 4x4 quadrant, so a tile is one of
  only 2^4 = 16 patterns -- four quadrants, each all on or all off. Across the
  first 64 ROM characters just 15 of those patterns occur, so the whole scaled
  charset collapses to a 16-tile pool of 128 bytes.

  That pool is the charset the VIC reads: screen codes 0..15 name its tiles.
  The startup routine reads the ROM charset and, for every character and every
  tile position, records which pattern (hence which pool tile) the scaled
  pixels select, into a per-character table of 16 codes. That table is what
  makes the deduplication usable: it turns `character k at tile position
  (row, column)' into a tile code.

  A 4-wide block cannot hide in the right border the way the 2x2 scroller's
  block does -- columns 36..39 would show its left half before it slid in --
  so the scroller moves *tile columns*. One coarse shift enters a single
  4-tall tile column at column 39, which the 38-column window keeps in the
  border, and a big character spans four consecutive shifts.

  The last four character rows carry the scroller. The text moves two pixels
  per frame, so the fine-scroll register steps by two (0, 6, 4, 2) and a
  coarse shift happens every fourth frame, entering one more tile column.

  The CIA-1 timer IRQ is masked, so the VIC raster interrupt is the only IRQ
  source. The handler buys the cycles needed to reach the right border, writes
  `$D016` and `$D018` there so the narrow columns and the pool charset only
  materialize on the next raster line, keeps both for the four scroller rows,
  restores the full width and the ROM charset once they are past,
  acknowledges the raster IRQ and jumps to the KERNAL IRQ tail at `$EA31`, so
  the KERNAL keyboard and cursor service keeps running."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; The text lives in the source as a string: the assembler encodes it to
;; screen-code bytes (the VIC's video-matrix byte) through the `:encoding`
;; metadata and places it like any other data. Only screen codes 0-63 name a
;; big character, which is exactly the range `:screen-codes` gives to letters,
;; digits and `$20-$3f`.
(def ^{:encoding :screen-codes :terminator 0xff} scrolltext
  (str "THIS SCROLLER USES A 4X4 CHARSET. EVERY ON-SCREEN CHARACTER IS SIXTEEN "
       "ORDINARY CHARACTERS, SO A NAIVE CHARSET WOULD NEED EIGHT KILOBYTES. "
       "BUT SCALING EACH PIXEL TO A FOUR BY FOUR BLOCK LEAVES ONLY SIXTEEN "
       "DISTINCT 8X8 TILES, SO THE WHOLE CHARSET FITS IN A SIXTEEN TILE POOL. "
       "THE STARTUP ROUTINE READS THE ROM CHARSET AND MAPS EVERY CHARACTER TO "
       "ITS TILES. THE LAST FOUR ROWS MOVE TWO PIXELS PER FRAME, SO ONE TILE "
       "COLUMN ENTERS EVERY FOURTH FRAME.          "))

(def screen 0x0400)
(def screen-columns 40)
;; VIC bank 0 (the reset default) is $0000-$3fff, so a 128-byte pool there is
;; all the generated charset needs; the screen stays at $0400.
(def charset-base 0x2000)

;; $D018 values: screen matrix $0400 for both, with the charset base switched
;; per raster line — the KERNAL's ROM charset ($1000) for the whole screen and
;; the tile pool ($2000) for the four scroller rows only.
(def d018-rom 0x14)     ; $10 = screen $0400, $04 = ROM charset $1000
(def d018-pool 0x18)    ; $10 = screen $0400, $08 = tile pool $2000

;; PAL: the display window starts on raster line 51 (0-based), so text row `n`
;; begins on line `51 + 8n`. The interrupt fires one line before the first
;; scroller row, and the full width is restored once the last one is past.
(def scroller-top-row 21)
(def scroller-height 4)
(def scroller-top-line (+ 51 (* 8 scroller-top-row)))
(def scroller-restore-line (+ scroller-top-line (* 8 scroller-height)))
(def scroller-irq-line (dec scroller-top-line))

;; helper function for calculating the memory address of scroller row y
(defn row [y]
  (+ screen (* screen-columns (+ scroller-top-row y))))

;; value written to $D016 (0-7) for smooth scroll of the scroller rows
(def scroll-value [0])
;; which of the current character's four tile columns enters next
(def tile-col [0])

;; The charset routine's scratch bytes. The pointers live in zero page because
;; they are dereferenced with `(zp),Y`; the routine runs before the scroller
;; does, so the scroller's column pointer reuses the first of them.
(def charset-src 0xFD)
(def charset-src-lo 0xFD)
(def charset-src-hi 0xFE)
(def index-dst 0xFB)
(def index-dst-lo 0xFB)
(def index-dst-hi 0xFC)
(def col-ptr 0xFB)
(def col-ptr-lo 0xFB)
(def col-ptr-hi 0xFC)
(def glyph-count [0])
(def tile-row [0])
(def row-offset [0])
(def row-a [0])
(def row-b [0])
(def scratch [0])

(def quadrant
  "One source pixel at 4x4 is a run of four on or off pixels; two side by side
  fill a byte. These are the four values a 2-pixel pair can produce."
  [0x00 0x0F 0xF0 0xFF])

;; Every 8x8 tile a 4x4 scale can produce, indexed by its 2x2 source pattern:
;; the top half (rows 0..3) comes from the upper of the two source rows, the
;; bottom half from the lower one. Sixteen patterns, so the scaled charset that
;; would otherwise need 8 KB is this 128-byte pool.
(def ^{:at charset-base} tile-pool
  (vec (mapcat (fn [pattern]
                 (let [top (nth quadrant (bit-shift-right pattern 2))
                       bottom (nth quadrant (bit-and pattern 0x03))]
                   [top top top top bottom bottom bottom bottom]))
               (range 16))))

(def number-of-glyphs 64)

(def ^{:size (* number-of-glyphs 16)} glyph-table
  "Per-character tile codes: character `k` at tile position (row, column) is
  the byte at `k*16 + column*4 + row`. Filled in from the ROM at startup."
  [])

(define-asm make-charset
  (%asset tile-pool)
  ;; Scale the ROM charset 4x4 and deduplicate it. A tile's content is fixed
  ;; by its 2x2 source pattern, so the charset needs only the 16-pattern pool;
  ;; what the ROM decides is *which* pattern every character selects at every
  ;; tile position, and that is the table this routine fills. No 8 KB of scaled
  ;; bitmaps is ever materialized.
  ;;
  ;; The character ROM is at $D000 only while I/O is banked out. The interrupt
  ;; is masked for the whole routine, so nothing needs the I/O registers while
  ;; they are gone; the memory map is put back before returning.

  (lda [0x01])
  (pha)                                 ; save original value to stack

  (lda 0x33)
  (sta [0x01])

  ;; source pointer: the first 64 ROM characters
  (lda (< 0xD000))
  (sta [charset-src-lo])
  (lda (> 0xD000))
  (sta [charset-src-hi])

  ;; destination pointer: the glyph table
  (lda (< glyph-table))
  (sta [index-dst-lo])
  (lda (> glyph-table))
  (sta [index-dst-hi])

  (lda 64)
  (sta [glyph-count])

  :glyph
  (lda 0)
  (sta [tile-row])
  (sta [row-offset])

  :tile-row
  ;; the two source rows this tile row covers
  (ldy [row-offset])
  (lda [[charset-src] y])
  (sta [row-a])
  (iny)
  (lda [[charset-src] y])
  (sta [row-b])

  ;; Column major: the leftmost pair of source pixels is tile column 0. Walk
  ;; the pairs from the least significant end, so each step shifts the rows
  ;; right by one pair; pattern = (upper pair << 2) | lower pair.
  (lda [tile-row])
  (clc)
  (adc 12)                              ; destination offset of tile column 3
  (tay)
  (ldx 3)
  :tile-column
  (lda [row-a])
  (and 0x03)
  (asl a)
  (asl a)
  (sta [scratch])
  (lda [row-b])
  (and 0x03)
  (ora [scratch])
  (sta [[index-dst] y])
  (lda [row-a])
  (lsr a)
  (lsr a)
  (sta [row-a])
  (lda [row-b])
  (lsr a)
  (lsr a)
  (sta [row-b])
  (tya)
  (sec)
  (sbc 0x04)                            ; previous tile column
  (tay)
  (dex)
  (bpl :tile-column)

  ;; next tile row: the next pair of source rows, two bytes further in
  (inc [tile-row])
  (lda [row-offset])
  (clc)
  (adc 0x02)
  (sta [row-offset])
  (lda [tile-row])
  (cmp 0x04)
  (bne :tile-row)

  ;; next character: eight source bytes, sixteen table bytes
  (lda [charset-src-lo])
  (clc)
  (adc 0x08)
  (sta [charset-src-lo])
  (bcc :0f)
  (inc [charset-src-hi])
  :0
  (lda [index-dst-lo])
  (clc)
  (adc 0x10)
  (sta [index-dst-lo])
  (bcc :0f)
  (inc [index-dst-hi])
  :0
  (dec [glyph-count])
  (bne :glyph)

  (pla)                                 ; pop original $01 value from stack
  (sta [0x01])
  (rts))

(define-asm scroller3
  (%entry :start)
  ;; Keep the whole effect in RAM above BASIC ($C000-$CFFF), so BASIC programs
  ;; can still be written while it runs. The pool and the glyph table are the
  ;; object's own assets and travel with it.
  (%region [0xC000 0xD000])

  :start
  (sei)

  ;; clear any pending interrupts
  (lda 0xff)
  (sta [0xD019])

  ;; disable CIA-1 timer IRQs so that they don't clash with the VIC
  ;; raster interrupt
  (lda 0x7f)
  (sta [0xDC0D])

  ;; Build the pool charset's index table from the ROM.
  (jsr make-charset)

  ;; VIC bank 0: screen matrix $0400, the ROM charset for the whole screen;
  ;; the raster handler swaps in the tile pool for the last four rows.
  (lda d018-rom)
  (sta [0xD018])

  ;; $D011 bit 7 is the ninth bit of the raster compare. Set it rather
  ;; than rely on the default: if it happens to be set, compare line X
  ;; becomes X + 256 and the raster IRQ never fires.
  (lda 0x1B)
  (sta [0xD011])

  ;; Blank the scroller rows with the pool's blank tile (screen code 0); the
  ;; rest of the screen keeps its normal charset content.
  (ldx 0)
  (lda 0)
  :clear-scroller
  (sta [(row 0) x])
  (sta [(row 1) x])
  (sta [(row 2) x])
  (sta [(row 3) x])
  (inx)
  (cpx screen-columns)
  (bne :clear-scroller)

  ;; scroller state: the first frame writes the top fine-scroll value and,
  ;; being the first shift, enters the first tile column
  (lda 0x00)
  (sta [scroll-value])
  (jsr :load-glyph)

  ;; set raster line where we want to get our IRQ handler invoked
  ;; one before the first rasterline of the first scroller row
  (lda scroller-irq-line)
  (sta [0xD012])

  ;;  enable VIC interrupt when raster line reaches value in $D012
  (lda 0x01)
  (sta [0xD01A])

  ;; set up IRQ vector
  (lda (< :irq))
  (sta [0x0314])
  (lda (> :irq))
  (sta [0x0315])

  ;; wait until just before the irq line to minimize screen artifacts
  (ldx (- scroller-irq-line 1))
  :0 (cpx [0xD012])
  (bne :0b)

  ;; enable interrupts
  (cli)

  ;; Return from the SYS call so BASIC's screen editor can scan the keyboard;
  ;; the raster IRQ remains installed and keeps the scroller alive.
  (rts)

  ;; Point `col-ptr` at the next character's first tile column. The text
  ;; pointer is the operand of the `lda` below, which we patch; the text is a
  ;; stream of screen codes ending in $ff, and wraps at the end.
  :load-glyph
  (lda [scrolltext])
  (cmp 0xff)
  (bne :non-sentinel)
  (lda (< scrolltext))
  (sta [(+ :load-glyph 1)])
  (lda (> scrolltext))
  (sta [(+ :load-glyph 2)])
  (jmp :load-glyph)

  :non-sentinel
  ;; character -> glyph table entry, sixteen bytes per character: the index is
  ;; a sixteen-bit `character * 16`, so the high byte is `character >> 4` and
  ;; the low byte is `character << 4` (a bare shift cannot carry it)
  (tax)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (tay)
  (txa)
  (asl a)
  (asl a)
  (asl a)
  (asl a)
  (clc)
  (adc (< glyph-table))
  (sta [col-ptr-lo])
  (tya)
  (adc (> glyph-table))
  (sta [col-ptr-hi])
  (lda 0)
  (sta [tile-col])
  ;; advance the text pointer, wrapping into the high byte
  (inc [(+ :load-glyph 1)])
  (bne :0f)
  (inc [(+ :load-glyph 2)])
  :0
  (rts)

  :irq
  ;; the IRQ handler starts at approx. char 27 in the row (with a bit
  ;; of jitter due to the volatile IRQ context)
  ;;
  ;; we wait 5*4 = 20 cycles to reach the right border area
  (bit [0xD021])                        ; change to INC to see
  (bit [0xD021])                        ; change to DEC to see
  (bit [0xD021])
  (bit [0xD021])
  (bit [0xD021])

  ;; we set $D016 and $D018 in the right border to ensure the 38->40 and
  ;; ROM->pool changes only materialize on the next rasterline; both stay
  ;; in effect across the four scroller rows
  (lda [scroll-value])            ; fine scroll, CSEL = 0 (38 columns)
  (sta [0xD016])
  (lda d018-pool)                ; the tile pool for the scroller rows
  (sta [0xD018])

  ;; wait until the last scroller row is past
  (lda scroller-restore-line)
  :wait-for-restore-line
  (cmp [0xd012])
  (bne :wait-for-restore-line)
  (lda 0x08)                            ; back to 40 columns
  (sta [0xD016])
  (lda d018-rom)                        ; back to the ROM charset
  (sta [0xD018])

  ;; acknowledge VIC interrupt
  (lda 0x01)
  (sta [0xD019])

  ;; Two pixels per frame: subtract two from the fine scroll and, when it
  ;; underflows, shift the four rows one column and reload the top value.
  (lda [scroll-value])
  (sec)
  (sbc 0x02)
  (sta [scroll-value])
  (bpl :irq-exit)                       ; branch on result plus (>= 0)

  ;; reset
  (and 0x07)
  (sta [scroll-value])

  ;; shift all four scroller rows left one character
  (ldx 0)
  :shift
  (lda [(inc (row 0)) x])
  (sta [(row 0) x])
  (lda [(inc (row 1)) x])
  (sta [(row 1) x])
  (lda [(inc (row 2)) x])
  (sta [(row 2) x])
  (lda [(inc (row 3)) x])
  (sta [(row 3) x])
  (inx)
  (cpx (dec screen-columns))
  (bne :shift)

  ;; enter one 4-tall tile column at the last column, in the border
  (ldy 0)
  (lda [[col-ptr] y])
  (sta [(+ (row 0) (dec screen-columns))])
  (iny)
  (lda [[col-ptr] y])
  (sta [(+ (row 1) (dec screen-columns))])
  (iny)
  (lda [[col-ptr] y])
  (sta [(+ (row 2) (dec screen-columns))])
  (iny)
  (lda [[col-ptr] y])
  (sta [(+ (row 3) (dec screen-columns))])

  ;; advance to the next tile column of this character
  (lda [col-ptr-lo])
  (clc)
  (adc 0x04)
  (sta [col-ptr-lo])
  (bcc :0f)
  (inc [col-ptr-hi])
  :0
  (inc [tile-col])
  (lda [tile-col])
  (cmp 0x04)
  (bne :irq-exit)

  ;; four columns done: fetch the next character
  (jsr :load-glyph)

  :irq-exit
  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(defn run!
  "Run the 4x4 scroller on a fresh VICE instance."
  [& args]
  (apply common/run! scroller3 args))
