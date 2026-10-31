(ns omkamra.vice.examples.effects.scroller5
  "A full-width 2x2 sprite scroller in the opened bottom border.

  Eight expanded sprites form one 384 pixel wide strip, and the 2x2
  scaled text lives in their bitmaps: each sprite pixel corresponds to
  2x2 screen pixels. One frame shifts the whole strip one sprite
  bit (two screen pixels) to the left with a chain of ROL instructions
  and shifts one pixel column of the next character in at the right
  edge. Every eight frames the next character is copied from the ROM
  charset into the hidden input block; the first byte in each 3-byte
  row supplies eight source columns.

  The strip runs in the bottom border. The raster is stabilised at the
  bottom of the screen with the double-IRQ method. The bottom border
  is opened with the RSEL trick, `$D021` is switched to the border
  color so that the screen shows up as if no border was open. The side
  borders are opened for the sixteen raster lines the sprites occupy,
  so the strip reaches into both side borders by the same amount.

  The CIA-1 timer IRQ is masked, so the VIC raster interrupt is the only IRQ
  source. The handler ends in the KERNAL IRQ tail at `$EA31`, keeping the
  clock and keyboard alive."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; The text lives in the source as a string and the assembler encodes
;; it to screen-code bytes. Screen codes 0-63 identify one of the
;; first 64 ROM characters, whose bitmaps the scroller scales 2x2.
(def ^{:encoding :screen-codes :terminator 0xff} scrolltext
  (str "THIS FULL WIDTH SCROLLER RUNS IN THE OPENED BOTTOM BORDER. EIGHT "
       "EXPANDED SPRITES FORM ONE 384 PIXEL WIDE STRIP. EVERY FRAME A CHAIN "
       "OF ROL INSTRUCTIONS SHIFTS THE WHOLE STRIP ONE SPRITE PIXEL TO THE "
       "LEFT AND ONE COLUMN OF THE NEXT BIG CHARACTER ENTERS AT THE RIGHT. "
       "EVERY EIGHT FRAMES THE NEXT CHARACTER IS LOADED AND ITS COLUMN TABLE "
       "IS REBUILT FROM THE ROM CHARSET. THE SIDE BORDERS ARE OPENED FOR THE "
       "SIXTEEN PIXEL TALL SPRITE AREA.          "))

(def screen 0x0400)

;; Zero-page pointers. The character-ROM copy and the character loader
;; share the source and destination pointers.

(def src 0xfb)
(def src-lo 0xfb)
(def src-hi 0xfc)

(def dst 0xfd)
(def dst-lo 0xfd)
(def dst-hi 0xfe)

(def col-idx [8])              ; 0-7, then load the next character
(def bg-orig [0])              ; original value of $D021

;; displayed sprite data
(def ^{:size (* 8 64) :at 0x2000} sprite-blocks []) ; displayed sprite data

;; first 64 ROM characters, 8 * 64 = 512 bytes
(def ^{:size (* 8 64) :at 0x2200} char-bitmaps [])

;; extra sprite block for the next character
;;
;; first byte of each row is the input for the right-most ROL
(def ^{:size 64 :at 0x2400} block-8 [])

;; rasterline where we want first IRQ to trigger
(def irq-line 0xf7)

;; Y position of the eight scroller sprites
(def sprite-y 0x04)

;; sprite X coordinates
;;
;; each sprite takes up 48 (0x30) pixels
;;
;; sprite #0 is at 0x1f0 because that's how we can make the first
;; sprite "touch" the left side of the visible area. contrary to what
;; one would expect, 0x1f0 is 8 pixels left of 0x00
(def sprite-x [0x1f0 0x28 0x58 0x88 0xb8 0xe8 0x118 0x148])

;; The ROL chain for one row: from sprite 7's rightmost byte to sprite 0's
;; leftmost byte, so the carry walks the whole 192 bit row to the left. The
;; caller first ROLs the one-byte input column in `block-8`; its carry then
;; enters sprite 7.
(defn rol-chain
  []
  (into []
        (mapcat (fn [n]
                  (let [base ^{:clj-kondo/ignore [:type-mismatch]}
                        (+ sprite-blocks (* n 64))]
                    `[(rol [(+ ~base 2) x])
                      (rol [(+ ~base 1) x])
                      (rol [(+ ~base 0) x])])))
        (range 7 -1 -1)))

(defn waste-cycles
  [n]
  (loop [n n
         w []]
    (cond (zero? n) w
          (>= n 8) (recur (- n 6) (conj w '(cmp [[0x00 x]])))
          (= n 7) (recur (- n 4) (conj w '(cmp [0x00 x])))
          (= n 6) (recur (- n 4) (conj w '(cmp [0x00 x])))
          (= n 5) (recur (- n 3) (conj w '(cmp [0x00])))
          (= n 4) (recur (- n 4) (conj w '(cmp [0x00 x])))
          (= n 3) (recur (- n 3) (conj w '(cmp [0x00])))
          (= n 2) (recur (- n 2) (conj w '(cmp 0x00)))
          :else (throw (ex-info "Cannot waste a single cycle" {})))))

(defn stabilize-raster
  [sp-offset]
  `[;; First stage: arm a second IRQ two lines later, save the stack
    ;; pointer so the real handler can discard this frame, enable
    ;; interrupts and wait with NOPs so the CPU is executing a NOP
    ;; when the second IRQ hits.

    (ldx [0xD012])
    (inx)
    (inx)
    (stx [0xD012])
    (lda (< :1f))
    (sta [0x0314])
    (lda (> :1f))
    (sta [0x0315])
    (tsx)
    (stx [~sp-offset])
    (cli)
    (%splice (repeat 64 '(nop)))
    ;; We should never get here, but if we do, make it visible.
    :0
    (inc [0x07e7])
    (jmp :0b)

    ;; Second stage: reduce the IRQ delay to zero or one cycle, then
    ;; converge on a fixed cycle of the next raster line.

    :1
    (ldy [0xD012])
    (waste-cycles 17)
    (cpy [0xD012])
    (beq :2f)
    :2])

(defn setup-raster-irq
  [irq-line irq-handler]
  `[;; disable interrupts
    (sei)

    ;; clear any pending interrupts
    (lda 0xff)
    (ora [0xDC0D])
    (ora [0xDD0D])
    (sta [0xD019])

    ;; disable CIA-1 timer IRQs so that they don't clash with the VIC
    ;; raster interrupt
    (lda 0x7f)
    (sta [0xDC0D])

    ;; DEN on, RSEL=1, YSCROLL=3, RST8=0.
    (lda 0x1b)
    (sta [0xD011])

    (lda ~irq-line)
    (sta [0xD012])

    ;; Enable VIC raster interrupts.
    (lda 0x01)
    (sta [0xD01A])

    ;; Set up the KERNAL IRQ vector.
    (lda (< ~irq-handler))
    (sta [0x0314])
    (lda (> ~irq-handler))
    (sta [0x0315])

    ;; Wait until just before the first IRQ line to minimize screen
    ;; artifacts.
    (ldx (- ~irq-line 1))
    :0
    (cpx [0xD012])
    (bne :0b)

    ;; Enable interrupts.
    (cli)])

(define-asm load-char
  "Load the next screen-code character into the first byte of each row
  of the hidden input block (block-8). The text is a stream of screen
  codes ending in $ff, and wraps at the end. The eight source bits in
  each byte are shifted out by the extra ROL before the visible
  eight-sprite chain."
  :load-char
  (lda [scrolltext])
  (cmp 0xff)
  (bne :0f)
  (lda (< scrolltext))
  (sta [(+ :load-char 1)])
  (lda (> scrolltext))
  (sta [(+ :load-char 2)])
  (jmp :load-char)

  :0
  (tax)
  (asl a)
  (asl a)
  (asl a)
  (clc)
  (adc (< char-bitmaps))
  (sta [src-lo])
  (txa)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (lsr a)
  (clc)
  (adc (> char-bitmaps))
  (sta [src-hi])

  (ldx 0x00)
  (ldy 0x00)
  :load-row
  (lda [[src] y])
  (sta [block-8 x])
  (inx)
  (inx)
  (inx)
  (iny)
  (cpy 0x08)
  (bne :load-row)

  (inc [(+ :load-char 1)])
  (bne :0f)
  (inc [(+ :load-char 2)])
  :0
  (rts))

(define-asm shift
  "Shift the whole sprite strip left by one bit. The carry produced by
  the one-byte ROL in block-8 is the next character column entering
  sprite 7."
  (lda [col-idx])
  (cmp 0x08)
  (bne :0f)
  (jsr load-char)
  (lda 0x00)
  (sta [col-idx])

  :0
  (ldx 0x00)
  :shift-row
  (clc)
  (rol [block-8 x])
  (rol-chain)
  (inx)
  (inx)
  (inx)
  (cpx 0x18)
  (bne :shift-row)

  ;; Advance to the next column. At eight, the next frame loads a new
  ;; character into block-8 before shifting its first column.
  (inc [col-idx])
  (rts))

(define-asm irq
  ;; avoid timing issues due to page crossing
  (%align 0x0100)

  :start
  (lda 0x01)
  (sta [0xD019])

  ;; stabilize raster
  ;;
  ;; save SP of first IRQ handler for restore at end
  (stabilize-raster (inc :restore-sp))

  ;; switch to 24 rows mode to prevent opening of the border on
  ;; rasterline $FB
  (lda [0xD011])
  (and 0xf7)
  (sta [0xD011])

  ;; use border color for the background
  (lda [0xD020])
  (waste-cycles 37)
  (sta [0xD021])

  ;; enable sprites
  (lda 0xff)
  (sta [0xD015])

  ;; set CSEL=1 (40 column mode)
  (lda 0x08)
  (sta [0xD016])

  ;; wait until we get right above the sprites
  ;;
  ;; apparently sprite position Y corresponds to rasterline Y-1
  (lda sprite-y)
  :0
  (cmp [0xD012])
  (bne :0b)

  ;; wait until we get close to the right border
  (waste-cycles 42)

  ;; To open the sideborder, we have to change the CSEL field (bit 3)
  ;; in $D016 from 1 to 0 in cycle 56.
  ;;
  ;; With sprite #0 enabled, the VIC starts to fetch its data at cycle
  ;; 58 and sets a flag (pulls BA low) at cycle 55 which forces the
  ;; CPU to halt at the next read (but not write) operation. This
  ;; means if we do any read operation in cycles 55-57, the CPU is
  ;; halted immediately. As a consequence, we cannot use STA and
  ;; friends to write the desired value into $D016 because write
  ;; instructions read the value in cycle N then write it to the
  ;; effective address in cycle N+1.
  ;;
  ;; Read-modify-instructions (ASL, LSR, ROL, ROR, INC, DEC) on the
  ;; other hand work like this:
  ;;
  ;; 1. Cycle N: Read old value
  ;; 2. Cycle N+1: Write old value, do the operation on it
  ;; 3. Cycle N+2: Write new value
  ;;
  ;; This means if we use DEC, we can time it such that the read
  ;; happens on cycle 54, the write of the old value at cycle 55, and
  ;; the write of the new value at cycle 56.
  (%splice (mapcat (fn [_]
                     `[(dec [0xD016])   ; => 0x07 (CSEL=0)
                       (inc [0xD016])   ; => 0x08 (CSEL=1)
                       (waste-cycles 34)])
                   (range 16)))

  ;; Back to 25 rows; the vertical flip-flop was never set, so the
  ;; bottom border stays open for the rest of the frame.
  (lda [0xD011])
  (and 0x7f)
  (ora 0x08)
  (sta [0xD011])

  ;; disable sprites
  ;;
  ;; without this, they would show up at the top of the screen again
  (lda 0x00)
  (sta [0xD015])

  ;; wait until the rasterbeam jumps back to the top of the screen
  :0
  (bit [0xD011])
  (bmi :0b)                             ; branch if minus (bit 7 set)

  ;;wait until we reach the last line of the top border
  (lda 0x32)
  :0
  (cmp [0xD012])
  (bne :0b)

  ;; wait until we get to the right border area
  (waste-cycles 43)

  ;; restore original background color
  (lda [bg-orig])
  (sta [0xD021])

  ;; ROL scroller sprites left by one pixel
  ;;
  ;; we also visualize the consumed rastertime by changing the border
  ;; color while the routine is running
  (inc [0xd020])
  (jsr shift)
  (dec [0xd020])

  ;; re-arm the stable raster for the next frame.
  (lda irq-line)
  (sta [0xD012])
  (lda (< :start))
  (sta [0x0314])
  (lda (> :start))
  (sta [0x0315])
  (lda 0x01)
  (sta [0xD019])
  :restore-sp
  (ldx 0x00)
  (txs)
  (jmp 0xEA31))

(define-asm copy-char-rom
  "Copy $D000-$D1FF to the reserved RAM area. I/O is banked out only
  for this routine, while interrupts are masked by the caller."
  (lda [0x01])
  (pha)
  (lda 0x33)
  (sta [0x01])

  (lda (< 0xD000))
  (sta [src-lo])
  (lda (> 0xD000))
  (sta [src-hi])
  (lda (< char-bitmaps))
  (sta [dst-lo])
  (lda (> char-bitmaps))
  (sta [dst-hi])

  (ldx 0x02)
  :copy-page
  (ldy 0x00)
  :copy-byte
  (lda [[src] y])
  (sta [[dst] y])
  (iny)
  (bne :copy-byte)
  (inc [src-hi])
  (inc [dst-hi])
  (dex)
  (bne :copy-page)

  (pla)
  (sta [0x01])
  (rts))

(define-asm scroller5
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  ;; Copy the first 64 character bitmaps to RAM
  (sei)
  (jsr copy-char-rom)
  (cli)

  ;; Remember the original background color
  (lda [0xD021])
  (sta [bg-orig])

  ;; Clear the sprite data so the strip starts empty.
  (ldx 0x00)
  (lda 0x00)
  :clear-sprites
  (sta [(+ sprite-blocks 0x0000) x])
  (sta [(+ sprite-blocks 0x0100) x])
  (inx)
  (bne :clear-sprites)

  ;; The 64 byte blocks start at `sprite-blocks`; point the eight sprite
  ;; pointers at consecutive blocks.
  (ldx 0x00)
  (lda (/ (mod sprite-blocks 0x4000) 64))
  (clc)
  :set-sprite-pointers
  (sta [(+ screen 0x3f8) x])
  (adc 0x01)
  (inx)
  (cpx 0x08)
  (bne :set-sprite-pointers)

  ;; Sprite X positions and their ninth bits.
  (%splice (mapcat (fn [i]
                     `((lda (bit-and (nth sprite-x ~i) 0xff))
                       (sta [(+ 0xD000 (* ~i 2))])))
                   (range (count sprite-x))))
  (lda (reduce (fn [mask i]
                 (let [x (nth sprite-x i)]
                   (if (>= x 0x100)
                     (bit-or mask (bit-shift-left 1 i))
                     mask)))
               0
               (range (count sprite-x))))
  (sta [0xD010])

  ;; Sprite Y positions.
  (lda sprite-y)
  (%splice (map (fn [i] `(sta (+ 0xD001 (* ~i 2)))) (range (count sprite-x))))

  ;; disable multicolor mode for all sprites
  (lda 0x00)
  (sta [0xD01C])

  ;; set color of all sprites to white
  (lda 0x01)
  (ldx 0x07)
  :set-color
  (sta [0xD027 x])
  (dex)
  (bpl :set-color)

  ;; expand sprites in both X and Y direction
  (lda 0xff)
  (sta [0xD017])
  (sta [0xD01D])

  (setup-raster-irq irq-line irq)
  (rts))

(defn run!
  "Run the full-width 2x2 sprite scroller on a fresh VICE instance."
  [& args]
  (apply common/run! scroller5 args))
