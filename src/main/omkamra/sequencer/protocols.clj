(ns omkamra.sequencer.protocols)

(defprotocol Sequencer
  (clear! [this])
  (play [this pf] [this pf bindings])
  (bpm! [this new-bpm])
  (status [this]))

(defprotocol Transport
  (start [this])
  (stop [this])
  (restart [this]))

(defprotocol Plugin
  (get-default-bindings [this]))
