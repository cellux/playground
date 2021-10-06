(ns oben.core.protocols.Target)

(defprotocol protocol
  (attrs [this])
  (compile-function [this fnode])
  (invoke-function [this fnode args])
  (dispose [this]))
