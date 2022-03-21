(ns rb.explores.sound.protocols.FrameBuffer)

(defprotocol protocol
  (nframes [_])
  (nchannels [_])
  (nsamples [_])
  (->samples [_]))
