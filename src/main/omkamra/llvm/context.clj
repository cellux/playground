(ns omkamra.llvm.context
  (:require [omkamra.llvm.api :refer [$llvm ok?]]))

(defn create
  []
  (.LLVMContextCreate $llvm))

(defn dispose
  [context]
  (.LLVMContextDispose $llvm context))
