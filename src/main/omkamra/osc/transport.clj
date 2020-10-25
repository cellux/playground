(ns omkamra.osc.transport)

(defprotocol Transport
  (send [_ osc-packet])
  (recv [_])
  (add-recv-callback [_ callback])
  (close [_]))
