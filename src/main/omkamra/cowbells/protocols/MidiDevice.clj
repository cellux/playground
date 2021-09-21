(ns omkamra.cowbells.protocols.MidiDevice)

(defprotocol protocol
  (note-on [this channel key vel])
  (note-off [this channel key])
  (cc [this channel ctrl value])
  (pitch-bend [this channel value])
  (program-change [this channel program])
  (bank-select [this channel bank])
  (all-notes-off [this channel])
  (all-sounds-off [this channel]))
