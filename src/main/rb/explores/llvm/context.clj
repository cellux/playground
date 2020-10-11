(ns rb.explores.llvm.context
  (:require [rb.explores.llvm.api :refer [$llvm ok?]]))

(defn create
  []
  (.LLVMContextCreate $llvm))

(defn dispose
  [context]
  (.LLVMContextDispose $llvm context))
