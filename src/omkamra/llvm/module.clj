(ns omkamra.llvm.module
  (:require [omkamra.llvm.api :refer [$llvm ok? check]]
            [omkamra.llvm.buffer :as buffer])
  (:import [omkamra.llvm.api LLVMVerifierFailureAction]))

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

(defn verify
  "Verifies an LLVM module and throws ExceptionInfo with LLVM's diagnostic
   when verification fails. Returns true when the module is valid."
  [module]
  (let [message-ptr (jnr.ffi.byref.PointerByReference.)
        status (.LLVMVerifyModule $llvm module
                                  LLVMVerifierFailureAction/LLVMReturnStatusAction
                                  message-ptr)]
    (if (zero? status)
      true
      (let [p (.getValue message-ptr)
            message (when p (.getString p 0))]
        (when p
          (.LLVMDisposeMessage $llvm p))
        (throw (ex-info (or message "LLVM module verification failed")
                        {:module module}))))))

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
