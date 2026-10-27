(ns omkamra.vice.examples.effects.scroller4
  "An eight-pixel C64 text scroller drawn with an 8x8 charset.

  Each source character becomes a 64x64 character made from eight rows and
  eight columns of ordinary screen cells. There are only two tiles in the
  scaled charset: an all-off tile and an all-on tile. The raster interrupt
  shifts the eight scroller rows one character column per frame, so the
  effect advances by eight pixels without using the fine-scroll bits of
  `$D016`. It uses the stock character set's blank space and reverse blank
  space as its two tiles.

  The first 64 character bitmaps are copied from the character ROM at `$D000`
  into `$CE00-$CFFF` during startup. The interrupt can then read the glyph
  bytes directly from RAM while the normal KERNAL interrupt tail continues to
  run each frame."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; The text lives in the source as a string: the assembler encodes it to
;; screen-code bytes (the VIC's video-matrix byte) through the `:encoding`
;; metadata and places it like any other data. The first 64 screen codes are
;; exactly the characters whose bitmaps are copied from the ROM.
(def ^{:encoding :screen-codes :terminator 0xff} scrolltext
  (str "THIS SCROLLER USES AN 8X8 CHARSET. EACH SOURCE CHARACTER BECOMES A "
       "SIXTY FOUR BY SIXTY FOUR PIXEL CHARACTER MADE FROM EIGHT ROWS AND "
       "EIGHT COLUMNS OF CELLS. THE INTERRUPT SHIFTS ONE CELL PER FRAME, "
       "WHICH IS EIGHT PIXELS WITHOUT FINE SCROLLING. THE FIRST 64 ROM "
       "CHARACTERS ARE COPIED TO RAM SO THE RASTER ROUTINE CAN READ THEM. "
       "          "))

(def screen 0x0400)
(def screen-columns 40)
(def scroller-top-row 17)
(def scroller-height 8)
(def scroller-top-line (+ 51 (* 8 scroller-top-row)))
(def scroller-restore-line (+ scroller-top-line (* 8 scroller-height)))
(def scroller-irq-line (dec scroller-top-line))

(def d018-rom 0x14)                 ; screen $0400, stock charset $1000

;; This is a run-time destination, not initialized data. Its fixed address is
;; deliberately in $CE00-$CFFF, immediately below the I/O area.
(def ^{:at 0xCE00 :size 512} char-rom-copy [])

(defn row
  "The screen-matrix address of scroller row `y`."
  [y]
  (+ screen (* screen-columns (+ scroller-top-row y))))

;; Zero-page scratch. The copy routine and the raster routine do not overlap in
;; time, so the copy pointers can be reused for the current glyph pointer.
(def charset-src 0xFB)
(def charset-src-lo 0xFB)
(def charset-src-hi 0xFC)
(def charset-dst 0xFD)
(def charset-dst-lo 0xFD)
(def charset-dst-hi 0xFE)
(def glyph-ptr 0xFB)
(def glyph-ptr-lo 0xFB)
(def glyph-ptr-hi 0xFC)
(def tile-mask 0xFD)

(define-asm scroller4
  (%entry :start)
  ;; Keep the code, text and state in RAM above BASIC.
  ;; The copied character ROM lives separately at $CE00.
  (%region [0xC000 0xCE00])

  :start
  (sei)

  ;; clear any pending interrupts
  (lda 0xff)
  (sta [0xD019])

  ;; disable CIA-1 timer IRQs so that they don't clash with the VIC raster
  ;; interrupt
  (lda 0x7f)
  (sta [0xDC0D])

  ;; Copy the first 64 character bitmaps while the character ROM is visible.
  ;; The copy leaves the I/O mapping exactly as it found it.
  (jsr :copy-char-rom)

  ;; VIC bank 0: screen matrix $0400 and the stock ROM charset at $1000.
  (lda d018-rom)
  (sta [0xD018])

  ;; The first frame enters the first tile column of the first character.
  (jsr :load-glyph)

  ;; $D011 bit 7 is the ninth bit of the raster compare. Set it rather
  ;; than rely on the default: if it happens to be set, compare line X
  ;; becomes X + 256 and the raster IRQ never fires.
  (lda 0x1B)
  (sta [0xD011])

  ;; Set the raster line where the handler is invoked, one line before the
  ;; first rasterline of the first scroller row.
  (lda scroller-irq-line)
  (sta [0xD012])

  ;; Enable VIC raster interrupts.
  (lda 0x01)
  (sta [0xD01A])

  ;; Set up the KERNAL IRQ vector.
  (lda (< :irq))
  (sta [0x0314])
  (lda (> :irq))
  (sta [0x0315])

  ;; Wait until just before the IRQ line to minimize screen artifacts.
  (ldx (- scroller-irq-line 1))
  :wait-for-irq
  (cpx [0xD012])
  (bne :wait-for-irq)

  ;; Enable interrupts. The raster IRQ remains installed after BASIC's SYS
  ;; call returns, so the KERNAL screen editor remains usable.
  (cli)
  (rts)

  ;; Copy $D000-$D1FF to $CE00-$CFFF. I/O is banked out only for this
  ;; routine, while interrupts are masked by the caller.
  :copy-char-rom
  (lda [0x01])
  (pha)
  (lda 0x33)
  (sta [0x01])

  (lda (< 0xD000))
  (sta [charset-src-lo])
  (lda (> 0xD000))
  (sta [charset-src-hi])
  (lda (< char-rom-copy))
  (sta [charset-dst-lo])
  (lda (> char-rom-copy))
  (sta [charset-dst-hi])

  (ldx 2)
  :copy-page
  (ldy 0)
  :copy-byte
  (lda [[charset-src] y])
  (sta [[charset-dst] y])
  (iny)
  (bne :copy-byte)
  (inc [charset-src-hi])
  (inc [charset-dst-hi])
  (dex)
  (bne :copy-page)

  (pla)
  (sta [0x01])
  (rts)

  ;; Load the next screen code and point at its eight copied ROM bitmap rows.
  ;; The text operand is patched in place so the pointer can cross pages.
  :load-glyph
  (lda [scrolltext])
  (cmp 0xff)
  (bne :got-screen-code)
  (lda (< scrolltext))
  (sta [(+ :load-glyph 1)])
  (lda (> scrolltext))
  (sta [(+ :load-glyph 2)])
  (jmp :load-glyph)

  :got-screen-code
  ;; The copied glyph starts at CE00 + screen-code * 8.
  (tax)
  (txa)
  (asl a)
  (asl a)
  (asl a)
  (clc)
  (adc (< char-rom-copy))
  (sta [glyph-ptr-lo])
  (txa)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (clc)
  (adc (> char-rom-copy))
  (sta [glyph-ptr-hi])

  ;; Bitmap bit 7 is the leftmost source pixel.
  (lda 0x80)
  (sta [tile-mask])

  ;; Advance the text pointer, wrapping into the high byte.
  (inc [(+ :load-glyph 1)])
  (bne :load-glyph-done)
  (inc [(+ :load-glyph 2)])
  :load-glyph-done
  (rts)

  :irq
  ;; The IRQ handler starts around character 27 in the row. Buy the cycles
  ;; needed to reach the right border before changing the raster registers.
  (bit [0xD021])
  (bit [0xD021])
  (bit [0xD021])
  (bit [0xD021])
  (bit [0xD021])

  ;; Narrow the display. D016 is used only for the 38-column border window:
  ;; no fine-scroll value changes and no charset switch are needed.
  (lda 0x00)
  (sta [0xD016])

  ;; Keep the narrow window active for all eight scroller rows.
  (lda scroller-restore-line)
  :wait-for-restore-line
  (cmp [0xD012])
  (bne :wait-for-restore-line)
  (lda 0x08)
  (sta [0xD016])

  ;; Acknowledge the VIC raster interrupt.
  (lda 0x01)
  (sta [0xD019])

  ;; Shift the eight scroller rows one screen cell left.
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
  (lda [(inc (row 4)) x])
  (sta [(row 4) x])
  (lda [(inc (row 5)) x])
  (sta [(row 5) x])
  (lda [(inc (row 6)) x])
  (sta [(row 6) x])
  (lda [(inc (row 7)) x])
  (sta [(row 7) x])
  (inx)
  (cpx (dec screen-columns))
  (bne :shift)

  ;; Enter one vertical tile column at the right edge. The copied bitmap row
  ;; supplies one source pixel; blank space is $20 and reverse blank is $A0.
  (ldy 0)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 0) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 1) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 2) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 3) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 4) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 5) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 6) (dec screen-columns))])

  (iny)
  (lda [[glyph-ptr] y])
  (and [tile-mask])
  (beq :0f)
  (lda 0x80)
  :0
  (ora 0x20)
  (sta [(+ (row 7) (dec screen-columns))])

  ;; Move to the next source pixel column. After eight columns, load the next
  ;; source character and restart at its leftmost bitmap bit.
  (lda [tile-mask])
  (lsr a)
  (sta [tile-mask])
  (bne :irq-exit)
  (jsr :load-glyph)

  :irq-exit
  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(defn run!
  "Run the 8x8 scroller on a fresh VICE instance."
  [& args]
  (apply common/run! scroller4 args))
