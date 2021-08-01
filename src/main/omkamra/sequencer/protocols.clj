(ns omkamra.sequencer.protocols)

(defprotocol Sequencer
  (clear! [this])
  (play [this pf] [this pf bindings])
  (bpm! [this new-bpm])
  (status [this]))

(defprotocol Target
  (start [this])
  (stop [this])
  (restart [this])
  (get-default-bindings [this])
  (compile-form [this form])
  (compile-pattern [this pattern])
  (resolve-binding [this k v]))
