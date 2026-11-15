(ns omkamra.vice.examples.effects.stable-raster-4
  "Neutralize jitter by synchronizing the CPU with CIA timer A."
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

(def ^{:align true} hex-digits
  [0x30 0x31 0x32 0x33
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

  ;; these extra cycles ensure that 62-$DC04 falls between 0x30-0x37
  (waste-cycles 3)

  ;; this code does not handle the $DC04 = 62 case (but that's fine)

  (lda 62)                              ; 62 - [62..1] -> [0..61]
  (sec)
  (sbc [0xDC04])                        ; A = cycle number (0..61)
  (tax)

  ;; the only varying bits in A should be bits 0-2, otherwise this won't work

  ;; compensate jitter
  (and 0x07)
  (sta [(inc :0f)])
  :0
  (bcs :1f)
  :1
  (cmp 0xc9)
  (cmp 0xc9)
  (cmp 0xc9)
  (bit [0xea24])

  ;; branch offset = 0
  ;; -----------------
  ;; (cmp 0xc9)      2
  ;; (cmp 0xc9)      2
  ;; (cmp 0xc9)      2
  ;; (bit [0xea24])  4 -> 10 cycles

  ;; branch offset = 1
  ;; -----------------
  ;; (cmp 0xc9)      2
  ;; (cmp 0xc9)      2
  ;; (cmp 0x2c)      2
  ;; (bit [0xea])    3 -> 9 cycles

  ;; branch offset = 2
  ;; -----------------
  ;; (cmp 0xc9)      2
  ;; (cmp 0xc9)      2
  ;; (bit [0xea24])  4 -> 8 cycles

  ;; branch offset = 3
  ;; -----------------
  ;; (cmp 0xc9)      2
  ;; (cmp 0x2c)      2
  ;; (bit [0xea])    3 -> 7 cycles

  ;; branch offset = 4
  ;; -----------------
  ;; (cmp 0xc9)      2
  ;; (bit [0xea24])  4 -> 6 cycles

  ;; branch offset = 5
  ;; -----------------
  ;; (cmp 0x2c)      2
  ;; (bit [0xea])    3 -> 5 cycles

  ;; branch offset = 6
  ;; -----------------
  ;; (bit [0xea24])  4 -> 4 cycles

  ;; branch offset = 7
  ;; -----------------
  ;; (bit [0xea])    3 -> 3 cycles

  ;; prove that the raster has been stabilized
  (waste-cycles 10)
  (inc [0xD020])
  (dec [0xD020])

  (txa)
  (display-byte 30 24)                  ; cycle number (0..61)

  (lda irq-line)
  (sta [0xD012])
  (lda (< ::irq))
  (sta [0x314])
  (lda 0x01)
  (sta [0xD019])

  ;; The KERNAL updates the jiffy clock, scans the keyboard and returns with
  ;; RTI. In particular, the cursor remains movable while the effect runs.
  (jmp 0xEA31))

(define-asm stable-raster-4
  (%align 0x100)
  (%entry :start)
  ;; Keep the code, text and state in RAM above BASIC.
  (%region [0xC000 0xD000])

  :start
  (sei)

  ;; stabilize the raster

  (lda 0x20)
  :0
  (cmp [0xD012])  ; 1 (only the last read cycle is on the target line)
  (bne :0b)       ; 2 (when we are on the target line)
  (waste-cycles (- 63 1 2 6 5 6 3 4))   ; 36
  (inc [(inc :attempts-lo)])            ; 6
  (bne :1f)
  (waste-cycles 3)                      ; 2 (BNE not taken) + 3
  (inc [(inc :attempts-hi)])            ; 6
  (jmp :2f)                             ; 3
  :1
  (waste-cycles 2)                      ; 3 (BNE taken) + 2
  (waste-cycles 6)                      ; 6
  (waste-cycles 3)                      ; 3
  :2
  (cmp [0xD012])                        ; 4
  (bne :0b)                             ; 3

  ;; show how many rounds were needed to stabilize

  :attempts-hi
  (lda 0)
  (display-byte 0 0)
  :attempts-lo
  (lda 0)
  (display-byte 2 0)

  ;; set up CIA timer

  (waste-cycles 32)

  (lda 62)                     ; 62,61,60,...,03,02,01,62,62,61,60,...
  (sta [0xDC04])
  (lda 0)
  (sta [0xDC05])
  (lda 0x11)
  (sta [0xDC0E])

  (waste-cycles 8)

  ;; verify that timer values 62..01 correspond to rasterline cycles 1..62

  (lda 0x30)
  :0
  (cmp [0xD012])
  (bne :0b)
  (waste-cycles 52)
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
  (apply common/run! stable-raster-4 args))
