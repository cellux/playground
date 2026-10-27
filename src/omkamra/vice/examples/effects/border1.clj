(ns omkamra.vice.examples.effects.border1
  "Turn off the top and bottom borders.

  When the screen is enabled ($D011 DEN bit is enabled), the VIC chip
  turns off the border at cycle 17/18 and turns it back on at cycle
  56/57 on every rasterline if an internal flag called the `V-flag`
  has the value zero.

  The V-flag is set to zero on rasterline $33/$37 and set back to 1 on
  rasterline $FB/$F7. The actual rasterline depends on the value of
  the RSEL bit in $D011: $33/$FB if RSEL=1 (25 rows) and $37/$F7 if
  RSEL=0 (24 rows).

  The trick works as follows:

  1. We start with RSEL=1
  2. On rasterline $F7, the VIC does not set the V-flag to 1 as RSEL=1
  3. On rasterline $F8, we set RSEL=0
  4. On rasterline $FB, the VIC does not set the V-flag to 1 as RSEL=0
  5. On rasterline $FC, we set RSEL=1

  As the V-Flag is never set to 1, the border keeps turning off at
  cycle 17/18 and turned back on at cycle 56/57 of every rasterline,
  including those which ordinarily lie in the top and bottom borders."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

(define-asm border1
  (%entry :start)
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  :start
  (sei)

  ;; clear any pending interrupts
  (lda 0xff)
  (sta [0xD019])

  ;; disable CIA-1 timer IRQs so that they don't clash with the VIC raster
  ;; interrupt
  (lda 0x7f)
  (sta [0xDC0D])

  (lda 0x1B)                            ; DEN=1 RSEL=1 YSCROLL=3
  (sta [0xD011])

  (lda 0xf8)
  (sta [0xD012])

  ;; Enable VIC raster interrupts.
  (lda 0x01)
  (sta [0xD01A])

  ;; Set up the KERNAL IRQ vector.
  (lda (< :irq))
  (sta [0x0314])
  (lda (> :irq))
  (sta [0x0315])

  ;; Enable interrupts. The raster IRQ remains installed after BASIC's SYS
  ;; call returns, so the KERNAL screen editor remains usable.
  (cli)
  (rts)

  :irq
  (lda [0xD011])
  (and 0xf7)                            ; RSEL=0 -> 24 rows
  (sta [0xD011])

  (ldx 0xfc)
  :wait-for-restore-line
  (cpx [0xD012])
  (bne :wait-for-restore-line)

  (ora 0x08)
  (sta [0xD011])                        ; RSEL=1 -> 25 rows

  ;; Acknowledge the VIC raster interrupt.
  (lda 0x01)
  (sta [0xD019])

  :irq-exit
  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(defn run!
  [& args]
  (apply common/run! border1 args))
