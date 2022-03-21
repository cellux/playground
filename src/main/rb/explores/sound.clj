(ns rb.explores.sound
  (:import (java.util Arrays))
  (:import (java.nio ByteBuffer))
  (:import (javax.sound.sampled AudioSystem AudioFormat
                                LineListener LineEvent LineEvent$Type))
  (:require
   [rb.explores.sound.osc :as osc]
   [rb.explores.sound.util :as util]))

(def one-beat-of-sound
  (-> (osc/sin 1.0 220)
      (util/play)))
