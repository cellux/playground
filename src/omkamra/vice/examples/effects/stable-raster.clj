(ns omkamra.vice.examples.effects.stable-raster
  "Get rid of random jitter caused by uncontrolled IRQ timing."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

(define-asm stable-raster
  (%entry :start)
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  :start
  (sei)

  ;; clear any pending VIC interrupts
  (lda 0xff)
  (ora [0xDC0D])
  (sta [0xD019])

  ;; disable CIA-1 timer IRQs so that they don't clash with the VIC raster
  ;; interrupt
  (lda 0x7f)
  (sta [0xDC0D])

  (lda 0x1B)
  (sta [0xD011])

  (lda 0x7c)
  (sta [0xD012])

  ;; Enable VIC raster interrupts.
  (lda 0x01)
  (sta [0xD01A])

  ;; Set up the KERNAL IRQ vector.
  (lda (< :irq1))
  (sta [0x0314])
  (lda (> :irq1))
  (sta [0x0315])

  ;; Enable interrupts. The raster IRQ remains installed after BASIC's SYS
  ;; call returns, so the KERNAL screen editor remains usable.
  (cli)
  (rts)

  :irq1
  (lda 0x01)
  (sta [0xD019])
  (lda 0x7e)
  (sta [0xD012])
  (lda (< :irq2))
  (sta [0x314])
  (lda (> :irq2))
  (sta [0x315])
  (tsx)
  (stx [(inc :load-sp)])
  (cli)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (brk)                                ; we never get here due to irq2

  :irq2
  (ldy [0xD012])
  (ldx 0x00)
  (ror [0xD3FF])
  (ror [0xD3FF])
  (eor [0xff])
  (cpy [0xD012])
  (bne :0f)
  (inx)
  :0
  (ldy 0x06)
  (ror [0xD3FF])
  (eor [0xff])
  (stx [0xD021])
  (sty [0xD021])
  (stx [0xD021])
  (sty [0xD021])
  (stx [0xD021])
  (sty [0xD021])
  (stx [0xD021])
  (sty [0xD021])
  (stx [0xD021])
  (sty [0xD021])
  (lda 0x7c)
  (sta [0xD012])
  (lda (< :irq1))
  (sta [0x314])
  (lda (> :irq1))
  (sta [0x315])
  (lda 0x01)
  (sta [0xD019])
  :load-sp
  (ldx 0x00)
  (txs)

  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(defn run!
  [& args]
  (apply common/run! stable-raster args))
