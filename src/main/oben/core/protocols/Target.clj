(ns oben.core.protocols.Target)

(defprotocol protocol
  (compile-function [this fnode])
  (invoke-function [this fnode args])
  (dispose [this]))
