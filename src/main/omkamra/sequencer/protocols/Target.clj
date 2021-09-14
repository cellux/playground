(ns omkamra.sequencer.protocols.Target)

(defprotocol protocol
  (start [this])
  (stop [this])
  (restart [this])
  (get-default-bindings [this])
  (compile-form [this form])
  (compile-pattern [this pattern])
  (compile-bind-form [this expr])
  (resolve-binding [this k v]))
