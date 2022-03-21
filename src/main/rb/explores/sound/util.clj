(ns rb.explores.sound.util
  (:import
   (java.nio ByteBuffer)
   (javax.sound.sampled AudioSystem AudioFormat
                        LineListener LineEvent LineEvent$Type))
  (:require
   [rb.explores.sound.protocols.FrameBuffer :refer [->samples nframes nchannels nsamples]]
   [rb.explores.sound.state :refer [*sr*]]))

(defn play
  [fb]
  (let [buf (->samples fb)
        nchannels (nchannels fb)
        nframes (nframes fb)
        nsamples (nsamples fb)]
    (let [byte-buf (byte-array (* Short/BYTES nsamples))
          short-buf (.asShortBuffer (ByteBuffer/wrap byte-buf))]
      (dotimes [i nsamples]
        (.put short-buf (short (* 32767 (aget ^doubles buf i)))))
      (let [clip (AudioSystem/getClip)
            sample-rate *sr*
            sample-bits 16
            signed true
            big-endian? true
            af (AudioFormat. sample-rate sample-bits nchannels signed big-endian?)]
        (.open clip af byte-buf 0 (count byte-buf))
        (.addLineListener clip (reify
                                 LineListener
                                 (^void update [this ^LineEvent event]
                                  (when (= (.type event) LineEvent$Type/STOP)
                                    (.close clip)))))
        (.start clip)
        fb))))
