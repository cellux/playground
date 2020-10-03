(ns rb.explores.llvm.api
  (:import (jnr.ffi LibraryLoader)
           (jnr.ffi.types size_t))
  (:require [clojure.string :as str]
            [omkamra.jnr.util :refer [defenum]]))

(defenum LLVMVerifierFailureAction
  LLVMAbortProcessAction
  LLVMPrintMessageAction
  LLVMReturnStatusAction)

(definterface LLVM
  ;; Core.h
  (^jnr.ffi.Pointer LLVMContextCreate)
  (^void LLVMContextDispose [^jnr.ffi.Pointer ctx])

  (^jnr.ffi.Pointer LLVMModuleCreateWithNameInContext [^jnr.ffi.Pointer module-id
                                                       ^jnr.ffi.Pointer ctx])
  (^void LLVMDisposeModule)
  (^void LLVMDumpModule [^jnr.ffi.Pointer module])
  (^int LLVMPrintModuleToFile [^jnr.ffi.Pointer module
                               ^jnr.ffi.Pointer filename
                               ^jnr.ffi.Pointer error-message])
  (^jnr.ffi.Pointer LLVMPrintModuleToString [^jnr.ffi.Pointer module])
  (^jnr.ffi.Pointer LLVMCreateModuleProviderForExistingModule [^jnr.ffi.Pointer module])
  (^void LLVMDisposeModuleProvider [^jnr.ffi.Pointer module-provider])
  (^int LLVMCreateMemoryBufferWithContentsOfFile [^jnr.ffi.Pointer path
                                                  ^jnr.ffi.Pointer out-mem-buf
                                                  ^jnr.ffi.Pointer out-message])
  (^jnr.ffi.Pointer LLVMCreateMemoryBufferWithMemoryRange [^jnr.ffi.Pointer input-data
                                                           ^{:tag long size_t true} input-data-length
                                                           ^jnr.ffi.Pointer buffer-name
                                                           ^int requires-null-terminator])
  (^jnr.ffi.Pointer LLVMCreateMemoryBufferWithMemoryRangeCopy [^jnr.ffi.Pointer input-data
                                                               ^{:tag long size_t true} input-data-length
                                                               ^jnr.ffi.Pointer buffer-name])
  (^{:tag long size_t true} LLVMGetBufferSize [^jnr.ffi.Pointer mem-buf])
  (^void LLVMDisposeMemoryBuffer [^jnr.ffi.Pointer mem-buf])
  (^jnr.ffi.Pointer LLVMCreatePassManager [])
  (^jnr.ffi.Pointer LLVMCreateFunctionPassManagerForModule [^jnr.ffi.Pointer module])
  (^int LLVMRunPassManager [^jnr.ffi.Pointer pass-manager
                            ^jnr.ffi.Pointer module])
  (^int LLVMInitializeFunctionPassManager [^jnr.ffi.Pointer function-pass-manager])
  (^int LLVMRunFunctionPassManager [^jnr.ffi.Pointer function-pass-manager
                                    ^jnr.ffi.Pointer f])
  (^int LLVMFinalizeFunctionPassManager [^jnr.ffi.Pointer function-pass-manager])
  (^void LLVMDisposePassManager [^jnr.ffi.Pointer pass-manager])

  ;; Analysis.h
  (^int LLVMVerifyModule [^jnr.ffi.Pointer module
                          ^rb.explores.llvm.api.LLVMVerifierFailureAction action])

  ;; BitReader.h
  (^int LLVMParseBitcodeInContext2 [^jnr.ffi.Pointer ctx
                                    ^jnr.ffi.Pointer mem-buf
                                    ^jnr.ffi.Pointer out-module])

  ;; BitWriter.h
  (^int LLVMWriteBitcodeToFile [^jnr.ffi.Pointer module
                                String path])
  (^jnr.ffi.Pointer LLVMWriteBitcodeToMemoryBuffer [^jnr.ffi.Pointer module])

  ;; ExecutionEngine.h
  (^void LLVMLinkInMCJIT [])
  (^void LLVMLinkInInterpreter [])

  )

(def ^:private $llvm
  (.load (LibraryLoader/create LLVM) "LLVM"))
