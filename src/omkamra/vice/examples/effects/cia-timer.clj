(ns omkamra.vice.examples.effects.cia-timer
  "Stabilize the raster, then set up CIA timer A to count the number of
  cycles within a rasterline."
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

(defmacro stabilize-raster
  []
  '((lda [0xD012])
    (waste-cycles (- 19 4))
    (cmp [0xD012])
    (bne :1f)
    (waste-cycles 5)
    :1

    (lda [0xD012])
    (waste-cycles (- 52 2))
    (cmp [0xD012])
    (bne :1f)
    (waste-cycles 3)
    :1

    (lda [0xD012])
    (waste-cycles (- 52 1))
    (cmp [0xD012])
    (beq :1f)
    :1
    ))

(def hex-digits [0x30 0x31 0x32 0x33
                 0x34 0x35 0x36 0x37
                 0x38 0x39 0x01 0x02
                 0x03 0x04 0x05 0x06])

(defmacro display-byte
  [x-pos y-pos]
  `((tay)
    (lsr)
    (lsr)
    (lsr)
    (lsr)
    (tax)
    (lda [hex-digits x])
    (sta [(+ 0x0400 (* 40 ~y-pos) ~x-pos)])
    (tya)
    (and 0x0f)
    (tax)
    (lda [hex-digits x])
    (sta [(+ 0x0401 (* 40 ~y-pos) ~x-pos)])))

(define-asm irq
  (%align 0x0100)

  (stabilize-raster)

  (waste-cycles 40)

  (lda 62)                     ; 62,61,60,...,03,02,01,62,62,61,60,...
  (sta [0xDC04])
  (lda 0)
  (sta [0xDC05])
  (lda 0x11)
  (sta [0xDC0E])

  (lda 0x30)
  :0
  (cmp [0xD012])
  (bne :0b)
  (waste-cycles 53)
  (cmp [0xD012])        ; the read cycle of this CMP falls on cycle 63
  (beq :1f)             ; 1..3
  (sta [0x07e7])
  :1
  ;; $DC04 = 62,61,60,...,03,02,01,62,62,61,60,...
  (lda 63)            ; 4..5
  (sec)               ; 6..7
  (sbc [0xDC04])      ; 8..11, with the read on cycle 11
  ;;     A = 01,02,03,...,60,61,62,01,01,02,03,...
  (pha)                            ; 12..14
  (lda 63)                         ; 15..16
  (sec)                            ; 17..18
  (sbc [0xDC04])                   ; 19..22, with the read on cycle 22
  (display-byte 38 0) ; if we are in sync, this should display $16 (cycle 22)
  (pla)
  (display-byte 36 0) ; if we are in sync, this should display $0B (cycle 11)

  ;; verify that one loop of 0xDC04 takes exactly 63 cycles

  (lda (+ 0x33 (* 8 0)))
  :0
  (cmp [0xD012])
  (bne :0b)
  (lda [0xDC04])
  (display-byte 0 0)
  (dec [0xD021])
  (inc [0xD021])
  (waste-cycles 4)

  (lda (+ 0x33 (* 8 1)))
  :0
  (cmp [0xD012])
  (bne :0b)
  (lda [0xDC04])
  (display-byte 0 1)
  (dec [0xD021])
  (inc [0xD021])
  (waste-cycles 4)

  (lda (+ 0x33 (* 8 2)))
  :0
  (cmp [0xD012])
  (bne :0b)
  (lda [0xDC04])
  (display-byte 0 2)
  (dec [0xD021])
  (inc [0xD021])
  (waste-cycles 4)

  (lda (+ 0x33 (* 8 3)))
  :0
  (cmp [0xD012])
  (bne :0b)
  (lda [0xDC04])
  (display-byte 0 3)
  (dec [0xD021])
  (inc [0xD021])
  (waste-cycles 4)

  (lda irq-line)
  (sta [0xD012])
  (lda (< ::irq))
  (sta [0x314])
  (lda 0x01)
  (sta [0xD019])

  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(define-asm cia-timer
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
  (apply common/run! cia-timer args))
