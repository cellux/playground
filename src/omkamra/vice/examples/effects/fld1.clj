(ns omkamra.vice.examples.effects.fld1
  "FLD = Flexible Line Distance.

  The 40x25 screen consists of 25*8 = 200 rasterlines.

  Normally, the first rasterline of each character row is a `badline`:
  on this line the VIC steals 40 cycles from the CPU in order to fetch
  the next row of screen codes from screen RAM and displays the bitmap
  patterns corresponding to these screen codes on rasterlines 0-7 of
  the row.

  The VIC knows that it is on a badline when the following conditions
  are all satisfied:

  1. ($D012 >= 0x30) AND ($D012 <= 0xF7) - we are in visible area
  2. ($D012 & 0x07) == ($D011 & 0x07)
  3. $D011 bit 4 - the DEN or Display ENable bit - is set

  By manipulating $D011 we can change when a badline occurs. This is
  how the screen is scrolled up or down by 3 or 4 pixels: by changing
  $D011 from the default 0x1b to 0x18 for example, the VIC triggers a
  badline 3 rasterlines above the usual position and thus the first
  character row starts displaying from rasterline 48 instead of 51.

  The heart of an FLD is this condition:

      ($D012 & 0x07) == ($D011 & 0x07)

  We wait until the VIC finishes the display of the current screen
  row, and at the end of the last rasterline of it (#7 of 0..7) we
  change $D011 such that the badline condition is NOT satisfied on the
  next rasterline. When the VIC finishes the display of a screen row,
  it goes into a special `idle` mode. While in this mode, it displays
  the value of memory address $3FFF (last byte of the current VIC
  bank) on the screen instead of the character rows. Under normal
  circumstances, the next rasterline after line #7 is a badline, and
  display of the next screen row begins. But in our case, the badline
  condition does not happen, so the VIC remains in idle mode. If we
  keep changing $D011 such that the badline condition is never
  satisfied, we can delay the display of the next and all consecutive
  character rows by N rasterlines.

  In this FLD example we slide 12 rows in the middle of the screen
  down or up depending on the cursor Y position: if the cursor is
  below the middle point of these rows, we slide the area down,
  otherwise upwards.

  The original idea behind this was that the middle area acts as a
  boat, the cursor is a passenger, and when the passenger gets into
  the boat, the boat moves to the other shore.

  To make the example a bit more realistic, we write wave-like bit
  patterns into $3FFF during the FLD - this is supposed to give a hint
  of water swirling on the two sides of the boat."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

(def top-rows 4)
(def middle-rows 17)
(def bottom-rows 4)

(def moving-rows 12)
(def fld-rows (- middle-rows moving-rows))
(def fld-lines (* 8 fld-rows))

(def ^{:align true} wave-patterns
  (let [patterns [(bit-xor (bit-shift-left 0x11 0) 0xff)
                  (bit-xor (bit-shift-left 0x11 1) 0xff)
                  (bit-xor (bit-shift-left 0x11 2) 0xff)
                  (bit-xor (bit-shift-left 0x11 3) 0xff)]]
    (mapv (fn [i] (let [phase (* 2.0 Math/PI (/ i fld-lines))
                        y (Math/sin (* phase 2.0))
                        x (+ (* 3 y) 4)]
                    (nth patterns (mod (int x) (count patterns)))))
          (range fld-lines))))

(def fld-distance-target [0])
(def fld-distance-current [0])

(defn- make-distance-table
  "Generate actual FLD distances for linear positions from shore to
  shore."
  [distance-fn]
  (mapv (fn [position]
          (-> (distance-fn position)
              (max 0)
              (min fld-lines)))
        (range (inc fld-lines))))

;; The internal position always advances linearly. These tables only
;; map that position to the actual FLD distance, so changing curves
;; cannot change the speed at which the boat travels between the
;; shores.
(def fld-ease-linear
  (make-distance-table identity))

(def fld-ease-in
  (make-distance-table #(quot (* % %) fld-lines)))

(def fld-ease-out
  (make-distance-table
   #(let [remaining (- fld-lines %)]
      (- fld-lines
         (quot (* remaining remaining) fld-lines)))))

(def fld-ease-in-out
  (make-distance-table
   (fn [position]
     (quot (* position position
              (- (* 3 fld-lines) (* 2 position)))
           (* fld-lines fld-lines)))))

(def fld-ease-sine
  (make-distance-table
   (fn [position]
     (let [phase (* 0.5 Math/PI
                    (/ (double position) fld-lines))
           value (Math/sin phase)]
       (int (Math/round (* fld-lines value value)))))))

;; Change only this line to try another curve.
;;
;; Available: fld-ease-linear, fld-ease-in, fld-ease-out,
;; fld-ease-in-out, and fld-ease-sine.
(def ^{:align true} fld-ease-distances fld-ease-sine)

;; rasterline where we want first IRQ to trigger
(def irq-line (- (dec (+ 51 (* top-rows 8))) 3))

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

  ;; we are at the last rasterline covered by top-rows
  ;;
  ;; with $D011=0x1b the next rasterline would be a badline by default

  (waste-cycles 37)
  (ldy 0)
  :fld-distance-top
  (ldx 0)                               ; N
  (bne :0f)
  (jmp :exit-top)
  :0
  ;; change $D011 to ensure the next rasterline is NOT a badline
  ;;
  ;; by repeating this pattern we push the badline down N times
  (lda [0xD012])
  (clc)
  (adc 2)
  (and 0x07)
  (ora 0x18)
  (sta [0xD011])
  (lda [wave-patterns y])
  (sta [0x3FFF])
  (iny)
  (waste-cycles 32)
  (dex)
  (bne :0b)
  (waste-cycles 3)
  :exit-top

  ;; here we could just wait
  ;;
  ;; but we DEC/INC $D021 on every line instead
  ;;
  ;; the background change cannot be seen as it happens under the
  ;; right border area - replace $D021 with $D020 if you want to see
  ;; the vertical bars
  (ldy moving-rows)
  :0
  (dec [0xD021])                        ; 0
  (inc [0xD021])
  (waste-cycles 8)
  (inc [0xD021])                        ; 1
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 2
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 3
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 4
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 5
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 6
  (dec [0xD021])
  (waste-cycles 51)
  (inc [0xD021])                        ; 7
  (dec [0xD021])
  (dey)
  (beq :1f)
  (waste-cycles 44)
  (jmp :0b)

  :1
  (waste-cycles 35)
  (ldy 0)
  :fld-distance-bottom
  (ldx 0)
  (bne :0f)
  (jmp :exit-bottom)
  :0
  (lda [0xD012])
  (clc)
  (adc 2)
  (and 0x07)
  (ora 0x18)
  (sta [0xD011])
  (lda [wave-patterns y])
  (sta [0x3FFF])
  (iny)
  (waste-cycles 32)
  (dex)
  (bne :0b)
  (waste-cycles 3)
  :exit-bottom

  ;; reset YSCROLL
  (lda 0x1b)
  (sta [0xD011])

  ;; Select the shore from the cursor's position relative to the
  ;; middle of the four moving rows. The target changes immediately,
  ;; but the current distance below eases toward it, so crossing the
  ;; middle row does not teleport the moving rows to the other side.
  (lda fld-lines)
  (ldx [0xD6])                          ; cursor Y position
  (cpx (+ top-rows (/ moving-rows 2)))
  (bpl :0f)
  (lda 0)
  :0
  (sta [fld-distance-target])

  ;; Move the internal position linearly by one step toward its
  ;; target. The selected table is applied only after this state has
  ;; moved, so all curves use the same travel speed between the two
  ;; shores.
  (lda [fld-distance-target])
  (cmp [fld-distance-current])
  (beq :distance-map)
  (bcc :move-up)
  (inc [fld-distance-current])
  (jmp :distance-map)
  :move-up
  (dec [fld-distance-current])

  :distance-map
  ;; Map the linear position to the actual FLD distance.
  (ldx [fld-distance-current])
  (lda [fld-ease-distances x])
  (sta [(inc :fld-distance-top)])
  (lda fld-lines)
  (sec)
  (sbc [(inc :fld-distance-top)])
  (sta [(inc :fld-distance-bottom)])

  ;; move the waves every second frame
  :move-flag
  (lda 0)
  (eor 1)
  (sta [(inc :move-flag)])
  (beq :skip-move)
  (ldy [wave-patterns])
  (ldx 0)
  :0
  (lda [(inc wave-patterns) x])
  (sta [wave-patterns x])
  (inx)
  (cpx fld-lines)
  (bne :0b)
  (sty [(+ wave-patterns fld-lines -1)])
  :skip-move

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

(define-asm fld1
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  (setup-raster-irq irq-line irq)
  (rts))

(defn run!
  [& args]
  (apply common/run! fld1 args))
