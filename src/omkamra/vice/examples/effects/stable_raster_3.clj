(ns omkamra.vice.examples.effects.stable-raster-3
  "Neutralize jitter by compensating 4/2/1 cycles on successive rasterlines.

  In stable-raster-2 we compensated only one cycle per rasterline, so
  we had to spend 8 rasterlines to neutralize all possible jitter
  values (1-8).

  In this example we `pull back` the CMP $D012 instructions on the
  timeline such that the probability of the two branches becomes
  50%/50%. If the read cycle of the CMP happens on the current
  rasterline then we waste 4, 2 or 1 cycles, halving the jitter at
  each step. This method lets us stabilize the raster on 3 consecutive
  rasterlines.

  To find the right timing, I pushed the CMP $D012 instructions
  forward until I reached the first point where the read cycle always
  executed on the next rasterline, then I pulled it back by 4/2/1
  cycles."
  (:refer-clojure :exclude [run!])
  (:require [omkamra.vice.assembler :as assembler
             :refer [define-asm]]
            [omkamra.vice.examples.effects.common :as common]))

(def irq-line 0x20)

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

(define-asm irq
  (%align 0x0100)

  (ldx 0xe6)

  (lda [0xD012])
  (waste-cycles (- 17 4))
  (cmp [0xD012])
  (bne :1f)
  (waste-cycles 5)
  (inc [0x0700 x])
  (jmp :2f)
  :1
  (inc [0x0701 x])
  (jmp :2f)
  :2
  (dex)
  (dex)

  (lda [0xD012])
  (waste-cycles (- 38 2))
  (cmp [0xD012])
  (bne :1f)
  (waste-cycles 3)
  (inc [0x0700 x])
  (jmp :2f)
  :1
  (inc [0x0701 x])
  (jmp :2f)
  :2
  (dex)
  (dex)

  (lda [0xD012])
  (waste-cycles (- 38 1))
  (cmp [0xD012])
  (beq :1f)
  (inc [0x0701 x])
  (jmp :2f)
  :1
  (inc [0x0700 x])
  (jmp :2f)
  :2
  (dex)
  (dex)

  (waste-cycles 30)
  ;; prove that the raster has been stabilized
  (inc [0xD020])
  (dec [0xD020])

  (lda irq-line)
  (sta [0xD012])
  (lda (< ::irq))
  (sta [0x314])
  (lda 0x01)
  (sta [0xD019])

  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(define-asm stable-raster-3
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

  (lda 0x1b)
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
  (apply common/run! stable-raster-3 args))
