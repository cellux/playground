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

  ;; Experiments have shown that with this IRQ routine the value of
  ;; 62-$DC04 falls into the 0x2d..0x34 range. By subtracting $DC04
  ;; from (62 - 0x2d), we get the amount of jitter between 0-7.

  (lda (- 62 0x2d))
  (sec)
  (sbc [0xDC04])                        ; A = jitter amount (0..7)
  (tax)

  ;; compensate jitter

  ;; (and 0x07) ; this was only necessary until the value 0x2D was found
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
  (display-byte 30 24)                  ; jitter amount

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
  ;; we are looking for a configuration where the last read cycle of
  ;; the following CMP instruction falls on cycle 1 of rasterline 0x20
  (cmp [0xD012])                        ; 1
  (bne :0b)                             ; 2
  ;; push the read cycle of the second CMP to cycle 63 of rasterline
  ;; 0x20 - if that read returns 0x20, we are stable
  (waste-cycles (- 63 1 2 6 5 6 3 4))
  ;; count the number of rounds we needed to find the right config
  (inc [(inc :rounds-lo)])              ; 6
  (bne :1f)
  (waste-cycles 3)                      ; 2 (BNE not taken) + 3 = 5
  (inc [(inc :rounds-hi)])              ; 6
  (jmp :2f)                             ; 3
  :1
  (waste-cycles 2)                      ; 3 (BNE taken) + 2 = 5
  (waste-cycles 6)                      ; 6
  (waste-cycles 3)                      ; 3
  :2
  (cmp [0xD012])                        ; 4
  (bne :0b)                             ; 3

  ;; show how many rounds were needed to stabilize

  :rounds-hi
  (lda 0)
  (display-byte 0 0)
  :rounds-lo
  (lda 0)
  (display-byte 2 0)

  ;; set up CIA timer A such that the timer loops exactly once per
  ;; rasterline:
  ;;
  ;; Cycle #         $DC04 (CIA timer A LO)
  ;;       1         62
  ;;       2         61
  ;;       3         60
  ;;       4         59
  ;;       5         58
  ;;     ...        ...
  ;;      61         02
  ;;      62         01
  ;;      63         62         ; this should be 00 but the timer wraps around

  (seq
   (let [delay 32]
     ;; fit the timer setup into exactly one rasterline
     `((waste-cycles ~delay)

       (lda 62)                         ; 2
       (sta [0xDC04])                   ; 4
       (lda 0)                          ; 2
       (sta [0xDC05])                   ; 4
       (lda 0x11)                       ; 2
       (sta [0xDC0E])                   ; 4

       (waste-cycles ~(- 63 6 6 6 delay)))))

  ;; verify that timer values 62..01 correspond to rasterline cycles 1..62

  (lda 0x30)
  :0
  (cmp [0xD012])
  (bne :0b)
  (waste-cycles 54)
  (cmp [0xD012])        ; the read cycle of this CMP falls on cycle 63
  (beq :1f)             ; 1..3
  (sta [0x07e7])        ; indicates if the read happened on the next line
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
