(ns rb.explores.sound.buffer
  (:require
   [rb.explores.sound.protocols.FrameBuffer :as FrameBuffer]
   [rb.explores.sound.state :refer [dur->frames]]))

(defrecord Buffer [^doubles buf
                   ^int nchannels]
  FrameBuffer/protocol
  (nframes [_] (/ (alength buf) nchannels))
  (nchannels [_] nchannels)
  (nsamples [_] (alength buf))
  (->samples [_] buf))

(defn create
  ([dur nchannels]
   (let [nframes (dur->frames dur)
         nsamples (* nframes nchannels)]
     (->Buffer (double-array nsamples)
               (int nchannels))))
  ([dur]
   (create dur 1)))
