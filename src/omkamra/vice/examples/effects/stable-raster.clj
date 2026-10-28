(ns omkamra.vice.examples.effects.stable-raster
  "Get rid of random jitter caused by uncontrolled IRQ timing.

  When an IRQ is triggered, the CPU finishes execution of the current
  instruction before pushing PCH/PCL/P and jumping to the IRQ routine
  through the ($FFFE) vector. Depending on the number of cycles
  remaining from the instruction the CPU is executing when the IRQ
  hits, the IRQ routine may be delayed by 0-8 cycles (maybe even more
  - my understanding of the confounding factors is hazy).

  If our goal is to make exactly timed changes to various VIC
  registers, such jitter is clearly unacceptable.

  In this example we demonstrate one possible solution to this
  problem: the so-called `double-irq` method:

  1. We set up an IRQ on rasterline $7D

  2. In the IRQ handler, we set up another IRQ handler with $D012 set
     to two rasterlines below ($7F). We save the stack pointer, enable
     interrupts, then start executing a series of NOP instructions, each
     taking 2 CPU cycles. Note that this IRQ handler never returns.

  3. When the next IRQ handler is triggered on rasterline $7F, the CPU
     is executing a NOP instruction. As a NOP takes 2 cycles, we can be
     sure that the timing delay is either zero or one cycle.

  4. We store the current value of $D012 ($7F) into Y, wait until we
     reach the end of the rasterline and execute CPY $D012 such that the
     read of $D012 either happens on cycle 63 (delay=0) or cycle 1 of the
     next rasterline (delay=1).

  5. If the $D012 read happened on cycle 63 of rasterline $7F, we
     **waste a single cycle** so that the two execution paths
     end up at the same position on line $80:

     $7F/63 case: BEQ (3) + NOP (2) + EOR (3) = 8 cyles, X=0
     $80/01 case: BEQ (2) + INX (2) + BNE (3) = 7 cyles, X=1

  6. We check what happened by executing INC $07E6,X - this increases
     one of the bytes corresponding to the last two characters in screen
     memory, depending on the path taken

  7. We create a raster split by writing the value of X (0=black,
     1=white) followed by $06 (background blue) five times into $D021 -
     if we took the $7F/63 path, the lines are black, otherwise white

  8. At the end of the IRQ handler we restore SP so that the IRQ
     handler at $EA31 pops the right values from the stack.

  The IRQ routine is aligned to $0100 so that extra cycles due to
  page-boundary crossing do not cause unwanted surprises in the
  timing."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

(def irq-line 0x7d)

(define-asm irq
  (%align 0x0100)

  :start
  (lsr [0xD019])
  (lda (+ irq-line 2))
  (sta [0xD012])
  (lda (< :irq2))
  (sta [0x314])
  (tsx)
  (stx [(inc :restore-sp)])
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
  ;; we should never get here due to :irq2
  :0
  (inc [0x07e5])                        ; but if we do, we'll see it
  (jmp :0b)

  :irq2
  (ldy [0xD012])
  (ldx 0x00)
  (ror [0xD3FF])
  (ror [0xD3FF])
  (eor [0xff])
  (cpy [0xD012])
  (beq :0f)
  (inx)
  (bne :1f)
  :0
  (nop)
  (eor [0xff])
  :1
  (inc [0x07e6 x])
  (ldy 0x06)
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
  (lda irq-line)
  (sta [0xD012])
  (lda (< :start))
  (sta [0x314])
  (lda 0x01)
  (sta [0xD019])
  :restore-sp
  (ldx 0x00)
  (txs)

  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

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

  (lda irq-line)
  (sta [0xD012])

  ;; Enable VIC raster interrupts.
  (lda 0x01)
  (sta [0xD01A])

  ;; Set up the KERNAL IRQ vector.
  (lda (< irq))
  (sta [0x0314])
  (lda (> irq))
  (sta [0x0315])

  ;; Enable interrupts. The raster IRQ remains installed after BASIC's SYS
  ;; call returns, so the KERNAL screen editor remains usable.
  (cli)
  (rts))

(defn run!
  [& args]
  (apply common/run! stable-raster args))
