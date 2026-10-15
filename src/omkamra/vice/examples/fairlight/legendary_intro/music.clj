(ns omkamra.vice.examples.fairlight.legendary-intro.music
  "Music for the Fairlight “legendary” intro.

  The intro has one SID player, called once per frame from the raster IRQ.
  It runs three voices from three-byte events — `[ticks freq-lo freq-hi]` —
  walking one pointer per voice; a zero duration byte restarts that voice
  from the address in the pointer table, which is how the tune loops.

  This namespace owns the music: the tune as readable data (`streams`,
  `voices`), the compiler that turns it into the image the player walks
  (`tune-data`), and the player itself, with its state in one reserved block.

  Tune image, as the player expects it (`$CE00` in the original, but placed by
  the linker here):

      +0   3 x little-endian voice pointer, in voice order
      +6   the streams, back to back, each closed by three zero bytes

  The closing zeroes are not padding. The player reads an event backwards —
  frequency hi, then lo, then ticks — so a zero ticks byte sends the voice back
  to its pointer, and the zeroes before it leave the gate off and the frequency
  unchanged while that happens.

  A note is a keyword (`:G2`, `:D#4`, …); `note->frequency` converts it to
  a SID oscillator value using the PAL clock and equal temperament. The
  original's own values are within 0.8 cents of that, i.e. the same notes.
  `ticks` is the player's unit: one tick is 10 frames (the player only
  advances a voice every tenth call)."
  (:require [omkamra.vice.assembler :as assembler]))

(def clock-hz
  "PAL C64 clock, for note name -> SID oscillator value."
  985248)

(def sid-init
  "Values the player writes to `$D400-$D418` before it starts."
  [0x00 0x00 0x00 0x08 0x10 0x2E 0x4E 0x00 0x00 0x00 0x08 0x40
   0x8A 0x0A 0x00 0x00 0x00 0x07 0x40 0x0A 0x0A 0x00 0x7D 0xD6 0x1F])

(def sid-registers
  "How many SID registers `init` writes: `$D400` through `$D418`."
  (count sid-init))

(def note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn note->midi
  "MIDI note number of a note keyword such as `:G2` or `:D#4`."
  ^long [note]
  (let [s (name note)
        octave (Long/parseLong (subs s (dec (count s))))
        step (subs s 0 (dec (count s)))]
    (+ (* 12 (inc octave)) (.indexOf ^java.util.List note-names step))))

(defn note->frequency
  "The 16-bit SID oscillator value for `note`."
  ^long [note]
  (let [hz (* 440.0 (Math/pow 2.0 (/ (- (note->midi note) 69.0) 12.0)))]
    (Math/round (* hz (/ 16777216.0 clock-hz)))))

(def streams
  "The tune. Each stream is a sequence of `[ticks note]` events; the final
  `[:loop]` is the zero duration byte the player loops on."
  {:bass
   [[3 :G2] [3 :D2] [6 :D#2] [3 :F2]
    [3 :F2] [6 :G2] [3 :G2] [3 :D2]
    [6 :D#2] [3 :F2] [3 :F2] [6 :G2]
    [3 :G2] [2 :G2] [1 :G3] [6 :D#2]
    [3 :F2] [2 :D2] [1 :F2] [6 :G2]
    [3 :G2] [3 :D2] [6 :D#2] [3 :F2]
    [3 :D2] [6 :G1] [:loop]]
   :lead
   [[96 :rest] [3 :G5] [1 :A5] [1 :A#5]
    [1 :G5] [4 :C6] [2 :A#5] [1 :A5]
    [1 :A#5] [4 :A5] [6 :G5] [3 :G5]
    [1 :A5] [1 :A#5] [1 :G5] [4 :C6]
    [2 :A#5] [1 :A5] [1 :A#5] [4 :C6]
    [6 :D6] [3 :D6] [1 :D#6] [1 :D6]
    [1 :D#6] [4 :D6] [2 :C6] [1 :A#5]
    [1 :C6] [4 :A5] [6 :G5] [3 :G5]
    [1 :A5] [1 :A#5] [1 :G5] [4 :C6]
    [2 :A#5] [1 :A5] [1 :A#5] [2 :A5]
    [2 :F5] [6 :G5] [2 :G3] [2 :G3]
    [1 :A3] [1 :A#3] [4 :D#4] [2 :D4]
    [2 :C4] [2 :A#3] [2 :A3] [3 :A#3]
    [1 :C4] [2 :A3] [2 :G3] [2 :G3]
    [1 :A3] [1 :A#3] [2 :D#4] [2 :D4]
    [2 :C4] [2 :A#3] [2 :A3] [2 :A#3]
    [6 :D3] [2 :G3] [2 :A3] [2 :A#3]
    [2 :D#4] [2 :D4] [2 :C4] [2 :A#3]
    [2 :A3] [2 :A#3] [6 :A#3] [2 :G3]
    [2 :G3] [1 :A3] [1 :A#3] [2 :D#4]
    [2 :D#4] [2 :D4] [2 :C4] [2 :A#3]
    [1 :A3] [1 :A#3] [6 :G3] [6 :G3]
    [9 :A#3] [3 :A3] [6 :G3] [6 :D5]
    [9 :C5] [3 :A#4] [6 :G4] [3 :G4]
    [2 :G4] [1 :D5] [6 :D#5] [3 :C5]
    [2 :A#4] [1 :A4] [6 :G4] [3 :G4]
    [3 :D4] [6 :D#4] [4 :A#4] [5 :C5]
    [3 :G4] [:loop]]})

(def voices
  "Voice order. Voices 1 and 3 share the lead stream, as in the original."
  [:lead :bass :lead])

(defn- u8 ^long [x] (bit-and (long x) 0xff))

(def stream-order
  "The order the streams are laid out in, after the pointer table."
  [:bass :lead])

(defn stream-label
  "The tune image's label for `stream`."
  [stream]
  (keyword (str "tune-" (name stream))))

(defn- event-bytes
  "One event as the player reads it (three bytes), or the stream's closing
  zeroes.

  `:rest` is an event whose frequency high byte is zero: the player reads the
  duration, keeps the voice gated off, and moves on."
  [event]
  (if (= [:loop] event)
    [0 0 0]
    (let [[ticks note] event
          frequency (if (= :rest note) 0 (note->frequency note))]
      [(u8 ticks) (u8 frequency) (u8 (bit-shift-right frequency 8))])))

(defn stream-bytes [events] (vec (mapcat event-bytes events)))

(defn tune-data
  "Assembly macro: the tune image — the three voice pointers, then the streams
  back to back — placed wherever the linker puts it.

  The pointers are words referring to the streams' own labels, so the whole
  image relocates; `:tune-pointers` names the table the player reads on a
  restart, and the player initialises each voice's own pointer from it."
  []
  (vec (concat [:tune-pointers
                (list '%dw (mapv stream-label voices))]
               (mapcat (fn [stream]
                         [(stream-label stream)
                          (list '%db (stream-bytes (streams stream)))])
                       stream-order))))

;; The player -----------------------------------------------------------------
;;
;; Two entries: `init`, called once from the intro's initialisation, and
;; `play`, called from the raster handler every frame. `play` walks the streams
;; only every tenth call, so one tune `tick` is ten frames — which is also why
;; the intro's volume fade is published in between, every frame.
;;
;; The state is one reserved block: a column per voice, a row per variable, so
;; X is always the voice index and every access is `lda [:row x]`. The rows are
;; three bytes apart whatever they hold, which is what makes that index work.
;; The block is part of the image, so the parts that start out zeroed or
;; constant — the frequency shadows, the gate flags, the gate-off control
;; bytes — need no code at all.

(def voice-count (count voices))
(def last-voice (dec voice-count))

(def tick-frames
  "Frames between two voice advances."
  10)

(def jiffy-low
  "KERNAL jiffy clock low byte, read for voice 2's pulse-width jitter."
  0xA0)

(def stream-pointer
  "Zero page cell the player reads tune events through (`$9E`/`$9F`)."
  0x9E)

(defn sid-init-data
  "Assembly macro: `:sid-init`, the register values `init` writes to
  `$D400-$D418`."
  []
  [:sid-init (list '%db (vec sid-init))])

(defn control-data
  "Assembly macro: the voices' gate-off control bytes — the `$D404`, `$D40B`
  and `$D412` values out of `sid-init` — which the player ORs the gate bit
  into when a voice sounds."
  []
  (list '%db (mapv #(nth sid-init %) [4 11 18])))

(assembler/define-asm music
  (%entry :init)

  ;; State. X indexes the voices, so these rows are what `lda [:row x]`
  ;; reaches; voice 1 is column 0.
  :tick    (%db [0])                    ; frames until the next voice advance
  :counter (%db [0 0 0])                ; frames until each voice's next event
  :freq-lo (%db [0 0 0])
  :freq-hi (%db [0 0 0])
  :gate    (%db [0 0 0])
  :control (control-data)
  :ptr-lo  (%db [0 0 0])                ; where each voice walks its stream
  :ptr-hi  (%db [0 0 0])

  ;; `$D400`, `$D407`, `$D40E`: seven registers per voice, too far apart to
  ;; add up in the loop, so they are a table.
  :voice-offsets (%db [0 7 14])

  (sid-init-data)

  :init
  ;; The SID's opening registers.
  (ldx (dec sid-registers))
  :init-sid
  (lda [:sid-init x])
  (sta [0xD400 x])
  (dex)
  (bpl :init-sid)
  ;; A first counter of 1 for every voice, then voice 1 one tick later, as in
  ;; the original: voice 3 starts the lead ten frames before voice 1 does.
  (lda 0x01)
  (ldx last-voice)
  :init-counters
  (sta [:counter x])
  (dex)
  (bpl :init-counters)
  (inc [:counter])
  ;; The divider starts at 1, so the very first call already advances them.
  (sta [:tick])
  ;; Each voice's stream pointer, out of the tune's own table — two bytes per
  ;; voice, so Y (not X) is the index here.
  (ldx last-voice)
  :init-pointers
  (txa)
  (asl a)
  (tay)
  (lda [:tune-pointers y])
  (sta [:ptr-lo x])
  (lda [(+ :tune-pointers 1) y])
  (sta [:ptr-hi x])
  (dex)
  (bpl :init-pointers)
  (rts)

  ::play
  ;; Nine calls in ten stop here: only the tick itself advances the voices.
  (dec [:tick])
  (beq :advance)
  (rts)
  :advance
  (lda tick-frames)
  (sta [:tick])
  (ldx last-voice)
  :voice
  (dec [:counter x])
  (bne :next-voice)
  ;; This voice's event is due. Gate it off first, so that the gate bit the
  ;; event sets below retriggers the envelope instead of leaving it running.
  (lda [:voice-offsets x])
  (tay)
  (lda [:control x])
  (sta [0xD404 y])
  ;; Read the event through the voice's own pointer, advancing it by three.
  ;; The three bytes are a duration, a frequency low byte and a frequency high
  ;; byte, and the player reads them backwards, high byte first.
  :read-event
  (lda [:ptr-lo x])
  (sta [stream-pointer])
  (clc)
  (adc 0x03)
  (sta [:ptr-lo x])
  (lda [:ptr-hi x])
  (sta [(inc stream-pointer)])
  (adc 0x00)                            ; the carry is still the low byte's
  (sta [:ptr-hi x])
  (ldy 0x02)
  (lda [[stream-pointer] y])
  (beq :hold)                           ; frequency high byte 0: a rest
  (sta [:freq-hi x])
  (dey)
  (lda [[stream-pointer] y])
  (sta [:freq-lo x])
  (lda 0x01)
  (sta [:gate x])
  :ticks
  (dey)
  (lda [[stream-pointer] y])
  (sta [:counter x])
  (bne :next-voice)
  ;; A zero duration ends the stream: point the voice back at its first event
  ;; and read that instead.
  (txa)
  (asl a)
  (tay)
  (lda [:tune-pointers y])
  (sta [:ptr-lo x])
  (lda [(+ :tune-pointers 1) y])
  (sta [:ptr-hi x])
  (jmp :read-event)
  :hold
  (dey)
  (lda 0x00)
  (sta [:gate x])
  (jmp :ticks)
  :next-voice
  (dex)
  (bpl :voice)
  ;; Publish: voice 2's pulse width, jittered from the jiffy clock, then every
  ;; voice's frequency, then its control byte with the gate bit — frequencies
  ;; all in place before any gate opens.
  (lda [jiffy-low])
  (and 0x07)
  (clc)
  (adc 0x04)
  (sta [0xD40A])
  (ldx last-voice)
  :publish-frequencies
  (lda [:voice-offsets x])
  (tay)
  (lda [:freq-lo x])
  (sta [0xD400 y])
  (lda [:freq-hi x])
  (sta [0xD401 y])
  (dex)
  (bpl :publish-frequencies)
  (ldx last-voice)
  :publish-controls
  (lda [:voice-offsets x])
  (tay)
  (lda [:control x])
  (ora [:gate x])
  (sta [0xD404 y])
  (dex)
  (bpl :publish-controls)
  (rts)

  (tune-data))
