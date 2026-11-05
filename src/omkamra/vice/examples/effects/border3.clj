(ns omkamra.vice.examples.effects.border3
  "Turn off all borders.

  The vertical blank interval starts on rasterline 300 ($12C) and ends
  at the top of the screen on line 15 ($0F). The active video range
  thus covers rasterlines 16-299 ($10-$12B).

  We trigger our IRQ on line 12 and get a stable raster on line 15. We
  then open the right border and keep opening it on all subsequent
  lines, up until line 289.

  The reason we stop at line 289 is userspace: if we keep going until
  line 299, there is so little time left for the KERNAL loop that even
  moving the cursor breaks up the timing.

  On line 250 ($FA) we set RSEL=0, then back to RSEL=1 on line
  252 ($FC). This opens the top and bottom borders as well."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; rasterline where we want first IRQ to trigger
(def irq-line 12)

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

  ;; at the left edge of rasterline 15 (last invisible vblank line)
  ;;
  ;; the first badline is on rasterline 51 ($33)
  ;;
  ;; we open the right border on lines 15-50

  (ldy (dec (- 51 15)))                 ; the last line is handled
                                        ; separately due to the tight
                                        ; timing requirements on
                                        ; rasterline 51

  (ldx 0x00)                            ; CSEL=0 (38 columns mode)
  (lda 0x08)                            ; CSEL=1 (40 columns mode)

  :0
  (waste-cycles 43)
  (stx [0xD016])
  (sta [0xD016])
  (waste-cycles 7)
  (dey)
  (bne :0b)

  ;; rasterline 50

  (waste-cycles 44)
  (stx [0xD016])
  (sta [0xD016])

  ;; rasterline 51 = first line of 40x25 area

  (ldy 24)                              ; handle last row separately
  (waste-cycles 10)

  :loop
  (stx [0xD016])                        ; line #0 (badline)
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #1
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #2
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #3
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #4
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #5
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #6
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #7
  (sta [0xD016])
  ;; here we wait much less to compensate for the
  ;; cycles lost due to the upcoming badline
  (waste-cycles 5)
  (dey)
  (beq :exit)
  (jmp :loop)
  :exit

  ;; rasterline 243 ($F3)

  (nop)
  (stx [0xD016])                        ; line #0 (badline)
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #1
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #2
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #3
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #4
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #5
  (sta [0xD016])
  (waste-cycles 55)
  (stx [0xD016])                        ; line #6
  (sta [0xD016])
  (ldy 0x13)                            ; RSEL=0
  (sty [0xD011])
  (waste-cycles 49)
  (stx [0xD016])                        ; line #7
  (sta [0xD016])

  (waste-cycles 55)
  (stx [0xD016])                        ; rasterline 251 ($FB)
  (sta [0xD016])

  (ldy 0x1b)                            ; RSEL=1
  (sty [0xD011])

  (waste-cycles 49)
  (stx [0xD016])                        ; rasterline 252 ($FC)
  (sta [0xD016])

  (ldy (- 290 253))
  (waste-cycles 3)

  :0
  (waste-cycles 50)
  (stx [0xD016])
  (sta [0xD016])
  (dey)
  (bne :0b)

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

(define-asm border3
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  (setup-raster-irq irq-line irq)
  (rts))

(defn run!
  [& args]
  (apply common/run! border3 args))
