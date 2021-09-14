(ns omkamra.sequencer.protocols.Sequencer)

(defprotocol protocol
  (clear! [this])
  (play [this pf] [this pf bindings])
  (bpm! [this new-bpm])
  (status [this]))
