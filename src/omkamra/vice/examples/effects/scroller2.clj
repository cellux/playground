(ns omkamra.vice.examples.effects.scroller2
  "A two-pixel C64 text scroller drawn with a 2x2 charset.

  Every on-screen character is a 16x16 cell built from four ordinary 8x8
  characters, so the 2 KB charset holds only 64 big characters: big character
  `k` owns screen codes `4k..4k+3`, laid out column-major as top-left,
  bottom-left, top-right, bottom-right. The charset is generated once at
  startup by doubling the first 64 characters of the ROM charset, one pixel
  becoming a 2x2 block.

  The scroller runs on the last two character rows and moves two pixels per
  frame, so the fine-scroll register steps by two (0, 6, 4, 2) and a coarse
  shift happens every fourth frame. The two rows shift one column left each
  time; every second shift also pulls the next big character in, writing its
  four codes into the last two columns, which hide in the border while the rows
  are narrowed to 38 columns.

  An increase in the value of $D016 pushes the row right, so counting
  it down 6 -> 4 -> 2 -> 0 is what slides the row left, followed by
  reset to 6 and a coarse shift which moves the video matrix itself
  one column left. The 38-column window ends with column 38, so column
  38 is the rightmost column that can ever show and column 39 stays in
  the border at every value of $D016. That is why the fine scroll can
  never bring a character into view on its own: even at its smallest
  value (0) column 39 still starts past the right edge of the window,
  so it takes a coarse shift to carry a character out of column
  39. And even then it does not appear at once: the shift resets the
  register to its largest value (6), which parks the content that has
  just moved into column 38 at the very edge, and it slides the rest
  of the way in as the register counts down over the following frames.

  The CIA-1 timer IRQ is masked, so the VIC raster interrupt is the only IRQ
  source. The handler first buys the cycles needed to reach the right border,
  writes `$D016` and `$D018` there so the narrow columns and the doubled
  charset only materialize on the next raster line, keeps both for the two
  scroller rows, restores the full width and the ROM charset once they are
  past, acknowledges the raster IRQ and jumps to the KERNAL IRQ tail at
  `$EA31`, so the KERNAL keyboard and cursor service keeps running. The rest of
  the screen is left untouched, on the ROM charset."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; The text lives in the source as a string: the assembler encodes it to
;; screen-code bytes (the VIC's video-matrix byte) through the `:charset`
;; metadata and places it like any other data. Only screen codes 0-63 name a
;; big character, which is exactly the range `:screen-codes` gives to letters,
;; digits and `$20-$3f`.
(def ^{:charset :screen-codes :terminator 0xff} scrolltext
  (str "THIS SCROLLER USES A 2X2 CHARSET. EVERY ON-SCREEN CHARACTER IS FOUR "
       "ORDINARY CHARACTERS, SO THE 2K CHARSET HOLDS ONLY 64 BIG CHARACTERS. "
       "THE INIT ROUTINE DOUBLES THE ROM CHARSET, TURNING EACH PIXEL INTO A "
       "2X2 BLOCK. THE LAST TWO ROWS MOVE TWO PIXELS PER FRAME, SO THE FINE "
       "SCROLL STEPS BY TWO AND A COARSE SHIFT HAPPENS EVERY FOURTH FRAME. "
       "A NEW BIG CHARACTER IS PULLED IN EVERY EIGHTH FRAME.          "))

(def screen 0x0400)
(def screen-columns 40)
;; VIC bank 0 (the reset default) is $0000-$3fff, so a 2 KB-aligned slot there
;; is all the generated charset needs; the screen stays at $0400.
(def charset-base 0x2000)

;; $D018 values: screen matrix $0400 for both, with the charset base switched
;; per raster line — the KERNAL's ROM charset ($1000) for the whole screen and
;; the generated one ($2000) for the two scroller rows only.
(def d018-rom 0x14)     ; $10 = screen $0400, $04 = ROM charset $1000
(def d018-doubled 0x18) ; $10 = screen $0400, $08 = doubled charset $2000

;; A big character is a 2x2 block of cells: top-left/top-right on row 23,
;; bottom-left/bottom-right on row 24.
(def scroller-top-row 23)
(def scroller-bottom-row 24)
(def scroller-top-offset (* screen-columns scroller-top-row))
(def scroller-bottom-offset (* screen-columns scroller-bottom-row))
(def scroller-top-address (+ screen scroller-top-offset))
(def scroller-bottom-address (+ screen scroller-bottom-offset))

;; PAL: the display window starts on raster line 51 (0-based), so text row `n`
;; begins on line `51 + 8n`. The interrupt fires one line before the first
;; scroller row, and the full width is restored once the last one is past.
(def scroller-top-line (+ 51 (* 8 scroller-top-row)))
(def scroller-restore-line (+ 51 (* 8 (inc scroller-bottom-row))))
(def scroller-irq-line (dec scroller-top-line))

;; value written to $D016 (0-7) for smooth scroll of the scroller rows
(def scroll-value [0])
;; toggles between pulling in a new big character and only shifting
(def place-flag [0])

;; The charset generator's scratch bytes. The row within a character is kept
;; in X, not in memory.
(def charset-count [0])

(defn- nibble->bitmap
  "Spread the four bits of `nibble`, high bit first, into four 2-bit runs."
  [nibble]
  (reduce (fn [acc bit-index]
            ;; shift in a 2-bit run: 11 for a set bit, 00 for a clear one
            (bit-or (bit-shift-left acc 2)
                    (if (bit-test nibble bit-index) 2r11 2r00)))
          0
          [3 2 1 0]))

(def ^{:align 16} nibble-bitmaps
  "Doubled nibble: each input bit becomes a 2-bit run."
  (mapv nibble->bitmap (range 16)))

(define-asm irq
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
  ;; ROM->doubled changes only materialize on the next rasterline; both stay
  ;; in effect across the two scroller rows
  (lda [scroll-value])            ; fine scroll, CSEL = 0 (38 columns)
  (sta [0xD016])
  (lda d018-doubled)           ; doubled charset for the scroller rows
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
  ;; underflows, shift the two rows one column and reload the top value.
  (lda [scroll-value])
  (sec)
  (sbc 0x02)
  (sta [scroll-value])
  (bpl :irq-exit)                       ; branch on result plus (>= 0)

  ;; reset
  (lda 0x06)
  (sta [scroll-value])

  ;; shift both scroller rows left one character
  (ldx 0)
  :shift
  (lda [(+ scroller-top-address 1) x])
  (sta [scroller-top-address x])
  (lda [(+ scroller-bottom-address 1) x])
  (sta [scroller-bottom-address x])
  (inx)
  (cpx (dec screen-columns))
  (bne :shift)

  ;; Every second shift a new big character enters; on the others the two rows
  ;; have only moved on and the last columns are left alone.
  (lda [place-flag])
  (eor 0x01)
  (sta [place-flag])
  (beq :irq-exit)

  ;; place the next scrolltext character to the last two columns, its codes in
  ;; the charset's column-major order: TL, BL, TR, BR
  :scrolltext-ptr
  (lda [scrolltext])
  (cmp 0xff)
  (bne :non-sentinel)
  (lda (< scrolltext))
  (sta [(+ :scrolltext-ptr 1)])
  (lda (> scrolltext))
  (sta [(+ :scrolltext-ptr 2)])
  (jmp :scrolltext-ptr)

  :non-sentinel
  (asl a)                         ; screen code -> first of four codes
  (asl a)
  (tax)
  (stx [(+ scroller-top-address (- screen-columns 2))])
  (inx)
  (stx [(+ scroller-bottom-address (- screen-columns 2))])
  (inx)
  (stx [(+ scroller-top-address (dec screen-columns))])
  (inx)
  (stx [(+ scroller-bottom-address (dec screen-columns))])

  ;; advance scrolltext pointer lo/hi
  (inc [(+ :scrolltext-ptr 1)])
  (bne :irq-exit)
  (inc [(+ :scrolltext-ptr 2)])

  :irq-exit
  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

;; Zero-page pointers used only while the charset is built; the interrupt is
;; masked for the whole of that routine.
(def charset-src 0xFB)
(def charset-src-lo 0xFB)
(def charset-src-hi 0xFC)
(def charset-dst 0xFD)
(def charset-dst-lo 0xFD)
(def charset-dst-hi 0xFE)

(define-asm make-charset
  ;; Doubling the ROM charset. Big character `k` owns codes 4k..4k+3,
  ;; i.e. 32 bytes in column-major order. Each source row is expanded
  ;; horizontally through `nibble-bitmaps`, a nibble at a time, and the
  ;; result is written twice so it grows vertically too.

  (lda [0x01])
  (pha)                                 ; save original value to stack

  ;; The character ROM is at $D000 only while I/O is banked out. The interrupt
  ;; is masked for the whole routine, so nothing needs the I/O registers while
  ;; they are gone; the memory map is put back before returning.
  (lda 0x33)
  (sta [0x01])

  (lda (< 0xD000))
  (sta [charset-src-lo])
  (lda (> 0xD000))
  (sta [charset-src-hi])
  (lda (< charset-base))
  (sta [charset-dst-lo])
  (lda (> charset-base))
  (sta [charset-dst-hi])
  (lda 64)
  (sta [charset-count])
  :charset-outer
  (ldx 0)                        ; X = source row within the character
  :charset-inner
  (txa)
  (tay)
  (lda [[charset-src] y])
  (pha)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (tay)
  (lda [nibble-bitmaps y])
  (sta [(inc :charset-hi)])             ; patch LDA operand
  (pla)
  (and 0x0F)
  (tay)
  (lda [nibble-bitmaps y])
  (sta [(inc :charset-lo)])             ; patch LDA operand
  (txa)
  (asl a)                               ; left offset = 2 * source row
  (tay)
  :charset-hi
  (lda 0x00)
  (sta [[charset-dst] y])
  (iny)
  (sta [[charset-dst] y])
  (tya)
  (clc)
  (adc 0x0f)                         ; right offset = left offset + 16
  (tay)
  :charset-lo
  (lda 0x00)
  (sta [[charset-dst] y])
  (iny)
  (sta [[charset-dst] y])
  (inx)
  (cpx 0x08)
  (bne :charset-inner)
  (lda [charset-src-lo])
  (clc)
  (adc 0x08)
  (sta [charset-src-lo])
  (bcc :0f)
  (inc [charset-src-hi])
  :0
  (lda [charset-dst-lo])
  (clc)
  (adc 0x20)
  (sta [charset-dst-lo])
  (bcc :0f)
  (inc [charset-dst-hi])
  :0
  (dec [charset-count])
  (bne :charset-outer)

  (pla)                            ; pop original $01 value from stack
  (sta [0x01])
  (rts))

(define-asm scroller2
  (%entry :start)
  ;; Keep the whole effect in RAM above BASIC ($C000-$CFFF), so BASIC programs
  ;; can still be written while it runs. The generated 2 KB charset lives
  ;; separately at $2000; `%reserve` keeps the linker out of it.
  (%region [0xC000 0xD000])
  (%reserve [0x2000 0x2800])

  :start
  (sei)

  ;; clear any pending interrupts
  (lda 0xff)
  (sta [0xD019])

  ;; disable CIA-1 timer IRQs so that they don't clash with the VIC
  ;; raster interrupt
  (lda 0x7f)
  (sta [0xDC0D])

  ;; Build the doubled charset into RAM from the character ROM.
  (jsr make-charset)

  ;; VIC bank 0: screen matrix $0400, the ROM charset for the whole screen;
  ;; the raster handler swaps in the generated charset for the last two rows.
  (lda d018-rom)
  (sta [0xD018])

  ;; Blank the two scroller rows, with the top-left code of the
  ;; doubled space character; the rest of the screen keeps its normal
  ;; charset content.
  (ldx 0)
  (lda 0x80)
  :clear-scroller
  (sta [scroller-top-address x])
  (inx)
  (cpx (* screen-columns 2))
  (bne :clear-scroller)

  ;; $D011 bit 7 is the ninth bit of the raster compare. Set it rather
  ;; than rely on the default: if it happens to be set, compare line X
  ;; becomes X + 256 and the raster IRQ never fires.
  (lda 0x1B)
  (sta [0xD011])

  ;; scroller state: the first frame already writes the top fine-scroll value
  ;; and, being the first shift, pulls in a character
  (lda 0x00)
  (sta [scroll-value])
  (sta [place-flag])

  ;; set raster line where we want to get our IRQ handler invoked
  ;; one before the first rasterline of the first scroller row
  (lda scroller-irq-line)
  (sta [0xD012])

  ;;  enable VIC interrupt when raster line reaches value in $D012
  (lda 0x01)
  (sta [0xD01A])

  ;; set up IRQ vector
  (lda (< irq))
  (sta [0x0314])
  (lda (> irq))
  (sta [0x0315])

  ;; wait until just before the irq line to minimize screen artifacts
  (ldx (- scroller-irq-line 1))
  :0 (cpx [0xD012])
  (bne :0b)

  ;; enable interrupts
  (cli)

  ;; Return from the SYS call so BASIC's screen editor can scan the keyboard;
  ;; the raster IRQ remains installed and keeps the scroller alive.
  (rts))

(defn run!
  "Run the 2x2 scroller on a fresh VICE instance."
  [& args]
  (apply common/run! scroller2 args))
