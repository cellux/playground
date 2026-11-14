(ns omkamra.vice.examples.effects.stable-raster-2
  "Neutralize jitter by compensating one cycle per rasterline."
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
  (ldy 8)                               ; max amount of expected jitter

  (lda [0xD012])
  (waste-cycles 9)
  :loop
  ;; the following CMP is pushed to the end of the rasterline such
  ;; that its read cycle (cycle #4 of 1..4) happens either on cycle 1
  ;; of the next rasterline or during one of the last cycles of the
  ;; current rasterline, depending on the amount of jitter
  (cmp [0xD012])
  ;; the next BEQ is taken - and spends an extra CPU cycle - if the
  ;; CMP read happened on the current rasterline
  (beq :1f)
  ;; we increase one of two adjacent bytes at the bottom right region
  ;; of the screen to indicate whether BEQ was taken
  ;;
  ;; I played with the timing until I found the configuration where
  ;; the BEQ is taken in almost 100% of the cases (but not always)
  (inc [0x0700 x])
  (jmp :2f)
  :1
  (inc [0x0701 x])
  (jmp :2f)
  :2
  (dex)
  (dex)
  (lda [0xD012])
  (waste-cycles 34)
  (dey)
  (bne :loop)

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

(define-asm stable-raster-2
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
  (apply common/run! stable-raster-2 args))
