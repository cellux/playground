(ns omkamra.llvm.module
  (:require [omkamra.llvm.api :refer [$llvm ok? check]]
            [omkamra.llvm.buffer :as buffer]))

(defn create
  [name context]
  (.LLVMModuleCreateWithNameInContext $llvm name context))

(defn dispose
  [module]
  (.LLVMDisposeModule $llvm module))

(defn from-buffer
  [buffer context]
  (let [module-ptr (jnr.ffi.byref.PointerByReference.)
        message-ptr (jnr.ffi.byref.PointerByReference.)
        status (.LLVMParseIRInContext $llvm context buffer module-ptr message-ptr)]
    (check status message-ptr)
    (.getValue module-ptr)))

(defn from-file
  [path context]
  (let [buffer (buffer/from-file path)
        module (from-buffer buffer context)]
    module))

(defn dump
  [module]
  (.LLVMDumpModule $llvm module))

(defn print-to-file
  [module path]
  (let [message-ptr (jnr.ffi.byref.PointerByReference.)
        status (.LLVMPrintModuleToFile $llvm module path message-ptr)]
    (check status message-ptr)))

(defn print-to-string
  [module]
  (.LLVMPrintModuleToString $llvm module))
