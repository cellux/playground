(ns rb.explores.sound.table
  (:require
   [rb.explores.sound.protocols.FrameBuffer :as FrameBuffer :refer [->samples nframes]]
   [rb.explores.sound.protocols.PhaseIndexed :as PhaseIndexed]))

(defrecord Table [^doubles buf]
  FrameBuffer/protocol
  (nframes [_] (alength buf))
  (nchannels [_] 1)
  (nsamples [_] (alength buf))
  (->samples [_] buf)

  PhaseIndexed/protocol
  (phase->value [_ phase]
    (let [size (alength buf)
          phase-increment (/ 1.0 size)
          scaled-phase (* size phase)
          lo-idx (Math/floor scaled-phase)
          _ (assert (< lo-idx size) (str "phase = " phase))
          lo-phase (* lo-idx phase-increment)
          hi-phase (+ lo-phase phase-increment)
          mul (/ (- phase lo-phase)
                 (- hi-phase lo-phase))
          lo-val (aget buf lo-idx)
          hi-idx (mod (inc lo-idx) size)
          hi-val (aget buf hi-idx)]
      (+ lo-val (* (- hi-val lo-val) mul)))))

(defn create
  ([size]
   (->Table (double-array size)))
  ([]
   (create 4096)))

(defn gen-table
  [period f]
  (let [t (create)
        buf (->samples t)
        size (nframes t)
        phase-increment-per-frame (/ period size)]
    (loop [phase 0
           idx 0]
      (when (< idx size)
        (aset-double buf idx (f phase))
        (recur (+ phase phase-increment-per-frame)
               (inc idx))))
    t))

(def sin (gen-table (* Math/PI 2) #(Math/sin %)))

(def tri (gen-table 1.0 #(if (< % 0.5)
                           (- (* % 4) 1.0)
                           (- 1.0 (* (- % 0.5) 4)))))

(def saw (gen-table 1.0 #(- (* % 2) 1.0)))

(def sqr (gen-table 1.0 #(if (< % 0.5) -1.0 1.0)))
