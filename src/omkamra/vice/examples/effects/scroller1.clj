(ns omkamra.vice.examples.effects.scroller1
  "A plain one-pixel C64 text scroller.

  One standard character row moves one pixel per frame and a new
  character enters every eight frames. The scroller is on the last
  row, drawn 38 columns wide so the rightmost column (where new codes
  enter) hides in the border until it has scrolled into view. The text
  is a stream of screen codes that explains how the effect works.

  The CIA-1 timer IRQ is masked, so the VIC raster interrupt is the only IRQ
  source and the handler always performs the row split. It first buys the
  cycles needed to reach the right border, writes `$D016` there so the
  narrow-column change only materializes on the next raster line, restores the
  full width once the row is past, acknowledges the raster IRQ and jumps to the
  KERNAL IRQ tail at `$EA31`, so the KERNAL keyboard and cursor service keeps
  running."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler]
            [omkamra.vice.examples.effects.common :as common]))

;; The text lives in the source as a string: the assembler encodes it to
;; screen-code bytes (the VIC's video-matrix byte) through the `:encoding`
;; metadata and places it like any other data.
(def ^{:encoding :screen-codes :terminator 0xff} scrolltext
  (str "THIS SCROLLER EXPLAINS ITSELF. A RASTER INTERRUPT WAITS EACH FRAME "
       "FOR THE BEAM TO REACH THE SCROLL ROW. IT SETS THE FINE SCROLL "
       "REGISTER FOR A FEW HUNDRED CYCLES, NUDGING THE DISPLAY ONE PIXEL, SO "
       "ONLY THAT BAND MOVES. AFTER EIGHT FRAMES IT WRAPS: THE ROW SHIFTS "
       "LEFT AND THE NEXT SCREEN CODE IS DRAWN IN THE LAST COLUMN. THE TEXT "
       "IS SCREEN CODES, NOT PETSCII, READ THROUGH A POINTER THAT WRAPS AT "
       "THE END, SO IT LOOPS. THE HANDLER ENDS IN THE KERNAL INTERRUPT TAIL, "
       "KEEPING CLOCK AND KEYBOARD ALIVE.                                  "))

(def screen 0x0400)
(def screen-columns 40)
(def scroller-row 24)
;; PAL: the display window starts on raster line 51 (0-based), so text
;; row `n` begins on line `51 + 8n`.
(def scroller-raster-line (+ 51 (* 8 scroller-row)))
(def scroller-row-offset (* screen-columns scroller-row))
(def scroller-row-address (+ screen scroller-row-offset))

;; value written to $D016 (0-7) for smooth scroll of last row
(def scroll-value [0])

(assembler/define-asm irq
  ;; the IRQ handler starts at approx. char 27 in the row (with a bit
  ;; of jitter due to the volatile IRQ context)
  ;;
  ;; we wait 5*4 = 20 cycles to reach the right border area
  (bit [0xD021])                        ; change to INC to see
  (bit [0xD021])                        ; change to DEC to see
  (bit [0xD021])
  (bit [0xD021])
  (bit [0xD021])

  ;; we set $D016 in the right border to ensure the 38->40 change only
  ;; materializes on the next rasterline
  (lda [scroll-value])            ; fine scroll, CSEL = 0 (38 columns)
  (sta [0xD016])

  ;; wait 8 rasterlines
  (lda (+ scroller-raster-line 8))
  :wait-for-end-of-screen
  (cmp [0xd012])
  (bne :wait-for-end-of-screen)
  (lda 0x08)                            ; back to 40 columns
  (sta [0xD016])

  ;; acknowledge VIC interrupt
  (lda 0x01)
  (sta [0xD019])

  ;; Eight frames move one character column. At the wrap, take the next screen
  ;; code from the read pointer and store it in the last column.
  (dec [scroll-value])
  (bpl :irq-exit)                       ; branch on result plus (>= 0)
  (lda 0x07)
  (sta [scroll-value])

  ;; shift the last row left one character
  (ldx 0)
  :shift
  (lda [(+ screen scroller-row-offset 1) x])
  (sta [(+ screen scroller-row-offset) x])
  (inx)
  (cpx (dec screen-columns))
  (bne :shift)

  ;; place the next scrolltext character to the last column
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
  (sta [(+ scroller-row-address (dec screen-columns))])

  ;; advance scrolltext pointer lo/hi
  (inc [(+ :scrolltext-ptr 1)])
  (bne :irq-exit)
  (inc [(+ :scrolltext-ptr 2)])

  :irq-exit
  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(assembler/define-asm scroller1
  (%entry :start)
  ;; Keep the whole effect in RAM above BASIC ($C000-$CFFF), so BASIC programs
  ;; can still be written while it runs. The `irq` object, the text asset and
  ;; the scroll counter all inherit this region. Neither asset needs `%asset`:
  ;; naming one in an operand (`(< scrolltext)`, `[scroll-value]`) already
  ;; registers it.
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

  ;; $D011 bit 7 is the ninth bit of the raster compare. Set it rather
  ;; than rely on the default: if it happens to be set, compare line X
  ;; becomes X + 256 and the raster IRQ never fires.
  (lda 0x1B)
  (sta [0xD011])

  ;; set raster line where we want to get our IRQ handler invoked
  ;; one before the first rasterline of the last character row
  (lda (dec scroller-raster-line))
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
  (ldx (- scroller-raster-line 2))
  :0 (cpx [0xD012])
  (bne :0b)

  ;; enable interrupts
  (cli)

  ;; Return from the SYS call so BASIC's screen editor can scan the keyboard;
  ;; the raster IRQ remains installed and keeps the scroller alive.
  (rts))

(defn run!
  "Run the one-pixel scroller on a fresh VICE instance."
  [& args]
  (apply common/run! scroller1 args))
