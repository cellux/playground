(ns omkamra.osc.transport
  (:refer-clojure :exclude [send]))

(defprotocol Transport
  (send [_ osc-packet])
  (recv [_])
  (add-recv-callback [_ callback])
  (close [_]))
