(ns omkamra.llvm.engine
  (:require [omkamra.llvm.api :as api :refer [$llvm ok? check]])
  (:import (jnr.ffi.byref PointerByReference)
           (omkamra.llvm.api LLVMMCJITCompilerOptions
                             LLVMCodeGenOptLevel
                             LLVMCodeModel)))

(defn create
  [module]
  (let [ee-ptr (PointerByReference.)
        message-ptr (PointerByReference.)
        status (.LLVMCreateExecutionEngineForModule
                $llvm ee-ptr module message-ptr)]
    (check status message-ptr)
    (.getValue ee-ptr)))

(defn create-interpreter
  [module]
  (let [ee-ptr (PointerByReference.)
        message-ptr (PointerByReference.)
        status (.LLVMCreateInterpreterForModule
                $llvm ee-ptr module message-ptr)]
    (check status message-ptr)

    (.getValue ee-ptr)))

(defn create-jit-compiler
  [module opt-level]
  (let [ee-ptr (PointerByReference.)
        message-ptr (PointerByReference.)
        status (.LLVMCreateJITCompilerForModule
                $llvm ee-ptr module opt-level message-ptr)]
    (check status message-ptr)
    (.getValue ee-ptr)))

(defn build-mcjit-compiler-options
  [opts]
  (let [options (api/make-struct LLVMMCJITCompilerOptions)
        size (jnr.ffi.Struct/size options)]
    (.LLVMInitializeMCJITCompilerOptions
     $llvm (jnr.ffi.Struct/getMemory options) size)
    (doseq [[k v] opts]
      (case k
        :opt-level (.set (.OptLevel options)
                         (if (instance? LLVMCodeGenOptLevel v)
                           (.intValue v)
                           (int v)))
        :code-model (.set (.CodeModel options) v)
        :no-frame-pointer-elim (.set (.NoFramePointerElim options) (if v 1 0))
        :enable-fast-isel (.set (.EnableFastISel options) (if v 1 0))
        :mcjmm (.set (.MCJMM options) v)
        (throw (ex-info "invalid mcjit compiler option"
                        {:option k :value v}))))
    options))

(defn create-mcjit-compiler
  ([module opts]
   (let [ee-ptr (PointerByReference.)
         message-ptr (PointerByReference.)
         options (build-mcjit-compiler-options opts)
         size (jnr.ffi.Struct/size options)
         status (.LLVMCreateMCJITCompilerForModule
                 $llvm ee-ptr module
                 (jnr.ffi.Struct/getMemory options) size
                 message-ptr)]
     (check status message-ptr)
     (.getValue ee-ptr)))
  ([module]
   (create-mcjit-compiler
    module
    {:opt-level LLVMCodeGenOptLevel/LLVMCodeGenLevelDefault
     :code-model LLVMCodeModel/LLVMCodeModelJITDefault
     :no-frame-pointer-elim false
     :enable-fast-isel false
     :mcjmm (jnr.ffi.Address/valueOf 0)})))

(defn get-global-value-address
  [engine name]
  (.LLVMGetGlobalValueAddress $llvm engine name))

(defn get-function-address
  [engine name]
  (.LLVMGetFunctionAddress $llvm engine name))

(defn dispose
  [engine]
  (.LLVMDisposeExecutionEngine $llvm engine))
