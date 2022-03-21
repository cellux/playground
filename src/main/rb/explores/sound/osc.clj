(ns rb.explores.sound.osc
  (:require
   [rb.explores.sound.buffer :as buffer]
   [rb.explores.sound.table :as table]
   [rb.explores.sound.state :refer [*sr*]]
   [rb.explores.sound.protocols.FrameBuffer :refer [->samples nframes nchannels nsamples]]
   [rb.explores.sound.protocols.PhaseIndexed :refer [phase->value]]))

(defn phasor
  [dur freq phase]
  (let [fb (buffer/create dur)
        buf (->samples fb)
        nframes (nframes fb)
        frames-per-period (/ *sr* freq)
        period 1.0
        phase-increment-per-frame (/ period frames-per-period)]
    (loop [phase phase
           idx 0]
      (when (< idx nframes)
        (assert (< phase 1.0))
        (aset-double buf idx phase)
        (assert (< (aget buf idx) 1.0))
        (recur (mod (+ phase phase-increment-per-frame) period)
               (inc idx))))
    fb))

(defn phase-indexed-osc
  [phase-indexed dur freq phase]
  (let [fb (phasor dur freq phase)
        buf (->samples fb)]
    (dotimes [idx (nframes fb)]
      (let [phase (aget buf idx)]
        (assert (< phase 1.0) (str "idx = " idx " phase = " phase))
        (aset-double buf idx (phase->value phase-indexed phase))))
    fb))

(defmacro gen-table-osc
  [name table]
  `(defn ~name
     ([~'dur ~'freq ~'phase]
      (phase-indexed-osc ~table ~'dur ~'freq ~'phase))
     ([~'dur ~'freq]
      (~name ~'dur ~'freq 0))))

(gen-table-osc sin table/sin)
(gen-table-osc saw table/saw)
(gen-table-osc tri table/tri)
(gen-table-osc sqr table/sqr)
