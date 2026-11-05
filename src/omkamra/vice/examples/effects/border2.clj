(ns omkamra.vice.examples.effects.border2
  "Turn off the left and right borders.

  When the screen is enabled ($D011 DEN bit is enabled), the VIC chip
  turns off the border at cycle 17/18 and turns it back on at cycle
  56/57 on every rasterline of the 40x25 screen.

  The actual off/on cycles depend on the value of CSEL (bit 3) in
  $D016:

  CSEL=0 (38 columns mode): cycle 18 off, cycle 56 on
  CSEL=1 (40 columns mode): cycle 17 off, cycle 57 on

  To open the sideborders:

  1. Start with CSEL=1 (40 columns mode)
  2. Set CSEL=0 in cycle 56
  3. Set CSEL=1 in a later cycle

  When the VIC arrives to cycle 56, CSEL is still 1 as the CPU sets it
  to 0 only in the second phase of the clock cycle (the first phase is
  used by the VIC). As the VIC sees CSEL=1, it does not turn on the
  border in cycle 56.

  By the time the VIC gets to cycle 57, CSEL is already 0. As the VIC
  sees CSEL=0, it does not turn on the border in cycle 57 either.

  Thus the sideborder disappears."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

;; rasterline where we want first IRQ to trigger
(def irq-line 0x30)

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

  ;; at the left edge of rasterline $33 (first visible line)
  ;;
  ;; this is a badline so we don't need to spend too many cycles

  (ldy 25)                              ; 25 screen rows
  (ldx 0x00)                            ; CSEL=0 (38 columns mode)
  (lda 0x08)                            ; CSEL=1 (40 columns mode)

  ;; just before the right sideborder on rasterline $33

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

(define-asm border2
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  (setup-raster-irq irq-line irq)
  (rts))

(defn run!
  [& args]
  (apply common/run! border2 args))
