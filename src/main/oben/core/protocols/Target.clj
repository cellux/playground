(ns oben.core.protocols.Target)

(defprotocol protocol
  (platform [this])
  (compile-function [this fnode])
  (invoke-function [this fnode args])
  (dispose [this]))
