(ns rb.explores.llvm.api
  (:import (jnr.ffi LibraryLoader Pointer Struct)
           (jnr.ffi.types size_t)
           (jnr.ffi.annotations In Out LongLong)
           (jnr.ffi.byref PointerByReference))
  (:require [clojure.string :as str]
            [omkamra.jnr :as jnr]))

(jnr/define-enum LLVMVerifierFailureAction
  LLVMAbortProcessAction,
  LLVMPrintMessageAction,
  LLVMReturnStatusAction)

(jnr/define-enum LLVMCodeGenOptLevel
  LLVMCodeGenLevelNone,
  LLVMCodeGenLevelLess,
  LLVMCodeGenLevelDefault,
  LLVMCodeGenLevelAggressive)

(jnr/define-enum LLVMCodeModel
  LLVMCodeModelDefault,
  LLVMCodeModelJITDefault,
  LLVMCodeModelTiny,
  LLVMCodeModelSmall,
  LLVMCodeModelKernel,
  LLVMCodeModelMedium,
  LLVMCodeModelLarge)

(jnr/define-struct LLVMMCJITCompilerOptions
  [^uint OptLevel]
  [^LLVMCodeModel CodeModel]
  [^int NoFramePointerElim]
  [^int EnableFastISel]
  [^Pointer MCJMM])

(jnr/define-library $llvm
  "LLVM"
  ;; Core.h
  (^void
   LLVMDisposeMessage [^{:tag String In true} Message])

  (^Pointer
   LLVMContextCreate)
  (^void
   LLVMContextDispose [^{:tag Pointer In true} C])

  (^Pointer
   LLVMModuleCreateWithNameInContext [^{:tag String In true} ModuleID
                                      ^{:tag Pointer In true} C])
  (^void
   LLVMDisposeModule)
  (^void
   LLVMDumpModule [^{:tag Pointer In true} M])
  (^int
   LLVMPrintModuleToFile [^{:tag Pointer In true} M
                          ^{:tag String In true} Filename
                          ^{:tag PointerByReference Out true} ErrorMessage])
  (^String
   LLVMPrintModuleToString [^{:tag Pointer In true} M])
  (^Pointer
   LLVMCreateModuleProviderForExistingModule [^{:tag Pointer In true} M])
  (^void
   LLVMDisposeModuleProvider [^{:tag Pointer In true} M])
  (^int
   LLVMCreateMemoryBufferWithContentsOfFile [^{:tag String In true} Path
                                             ^{:tag PointerByReference Out true} OutMemBuf
                                             ^{:tag PointerByReference Out true} OutMessage])
  (^Pointer
   LLVMCreateMemoryBufferWithMemoryRange [^{:tag String In true} InputData
                                          ^{:tag long size_t true In true} InputDataLength
                                          ^{:tag String In true} BufferName
                                          ^{:tag int In true} RequiresNullTerminator])
  (^Pointer
   LLVMCreateMemoryBufferWithMemoryRangeCopy [^{:tag String In true} InputData
                                              ^{:tag long size_t true In true} InputDataLength
                                              ^{:tag String In true} BufferName])
  (^Pointer
   LLVMGetBufferStart [^{:tag Pointer In true} MemBuf])
  (^{:tag long size_t true}
   LLVMGetBufferSize [^{:tag Pointer In true} MemBuf])
  (^void
   LLVMDisposeMemoryBuffer [^{:tag Pointer In true} MemBuf])

  (^Pointer
   LLVMCreatePassManager [])
  (^Pointer
   LLVMCreateFunctionPassManagerForModule [^{:tag Pointer In true} M])
  (^int
   LLVMRunPassManager [^{:tag Pointer In true} PM
                       ^{:tag Pointer In true} M])
  (^int
   LLVMInitializeFunctionPassManager [^{:tag Pointer In true} FPM])
  (^int
   LLVMRunFunctionPassManager [^{:tag Pointer In true} FPM
                               ^{:tag Pointer In true} F])
  (^int
   LLVMFinalizeFunctionPassManager [^{:tag Pointer In true} FPM])
  (^void
   LLVMDisposePassManager [^{:tag Pointer In true} PM])

  ;; Analysis.h
  (^int
   LLVMVerifyModule [^{:tag Pointer In true} M
                     ^{:tag LLVMVerifierFailureAction In true} Action
                     ^{:tag PointerByReference Out true} OutMessage])

  ;; BitReader.h
  (^int
   LLVMParseBitcodeInContext2 [^{:tag Pointer In true} ContextRef
                               ^{:tag Pointer In true} MemBuf
                               ^{:tag PointerByReference Out true} OutModule])

  ;; BitWriter.h
  (^int
   LLVMWriteBitcodeToFile [^{:tag Pointer In true} M
                           ^{:tag String In true} Path])
  (^Pointer
   LLVMWriteBitcodeToMemoryBuffer [^{:tag Pointer In true} M])

  ;; ExecutionEngine.h
  (^void
   LLVMLinkInMCJIT [])
  (^void
   LLVMLinkInInterpreter [])
  (^int
   LLVMCreateExecutionEngineForModule [^{:tag PointerByReference Out true} OutEE
                                       ^{:tag Pointer In true} M
                                       ^{:tag PointerByReference Out true} OutError])
  (^int
   LLVMCreateInterpreterForModule [^{:tag PointerByReference Out true} OutInterp
                                   ^{:tag Pointer In true} M
                                   ^{:tag PointerByReference Out true} OutError])
  (^int
   LLVMCreateJITCompilerForModule [^{:tag PointerByReference Out true} OutJIT
                                   ^{:tag Pointer In true} M
                                   ^{:tag int In true} OptLevel
                                   ^{:tag PointerByReference Out true} OutError])
  (^void
   LLVMInitializeMCJITCompilerOptions [^{:tag Pointer In true} Options
                                       ^{:tag long size_t true In true} SizeOfOptions])
  (^int
   LLVMCreateMCJITCompilerForModule [^{:tag PointerByReference Out true} OutJIT
                                     ^{:tag Pointer In true} M
                                     ^{:tag Pointer In true} Options
                                     ^{:tag long size_t true In true} SizeOfOptions
                                     ^{:tag PointerByReference Out true} OutError])
  (^void
   LLVMDisposeExecutionEngine [^{:tag Pointer In true} EE])

  (^void
   LLVMAddModule [^{:tag Pointer In true} EE
                  ^{:tag Pointer In true} M])
  (^int
   LLVMRemoveModule [^{:tag Pointer In true} EE
                     ^{:tag Pointer In true} M
                     ^{:tag PointerByReference Out true} OutMod
                     ^{:tag PointerByReference Out true} OutError])
  (^int
   LLVMFindFunction [^{:tag Pointer In true} EE
                     ^{:tag String In true} Name
                     ^{:tag PointerByReference Out true} OutFn])
  (^{:tag long LongLong true}
   LLVMGetGlobalValueAddress [^{:tag Pointer In true} EE
                              ^{:tag String In true} Name])
  (^{:tag long LongLong true}
   LLVMGetFunctionAddress [^{:tag Pointer In true} EE
                           ^{:tag String In true} Name])

  ;; IRReader.h
  (^int
   LLVMParseIRInContext [^{:tag Pointer In true} ContextRef
                         ^{:tag Pointer In true} MemBuf
                         ^{:tag PointerByReference Out true} OutM
                         ^{:tag PointerByReference Out true} OutMessage])

  ;; Linker.h
  (^int
   LLVMLinkModules2 [^{:tag Pointer In true} Dest
                     ^{:tag Pointer In true} Src]))

(def all-targets
  [:AArch64 :AMDGPU :ARM
   :BPF :Hexagon :Lanai
   :Mips :MSP430 :NVPTX
   :PowerPC :RISCV :Sparc
   :SystemZ :WebAssembly
   :X86 :XCore :AVR])

(def native-target
  (let [arch (System/getProperty "os.arch")]
    (case arch
      "i386" :X86
      "amd64" :X86
      "arm" :ARM
      "aarch64" :AArch64
      (throw (ex-info "unsupported architecture" {:arch arch})))))

(def all-subsystems
  ["TargetInfo"
   "Target"
   "TargetMC"
   "AsmPrinter"
   "AsmParser"
   "Disassembler"])

(defmacro define-target-initializers
  [library-name]
  `(do
     (define-library ~library-name
       "LLVM"
       ~@(for [target all-targets
               subsystem all-subsystems]
           (list (with-meta
                   (symbol (str "LLVMInitialize"
                                (name target)
                                subsystem))
                   {:tag 'void})
                 [])))
     ~@(for [subsystem all-subsystems]
         `(defn ~(symbol (str "LLVMInitializeAll" subsystem "s"))
            []
            ~@(for [target all-targets]
                (list (symbol (str ".LLVMInitialize"
                                   (name target)
                                   subsystem))
                      library-name))))
     (defn ~'LLVMInitializeNativeTarget
       []
       ~@(for [subsystem all-subsystems]
           (list (symbol (str ".LLVMInitialize"
                              (name native-target)
                              subsystem))
                 library-name)))))

(define-target-initializers $llvm-init)

(defmacro create-struct
  [struct-name]
  `(new ~struct-name (.getRuntime $llvm)))

(LLVMInitializeNativeTarget)

(defn ok?
  [return-value]
  (zero? return-value))

(defn check
  [status message-ptr]
  (if (ok? status)
    status
    (let [p (.getValue message-ptr)
          msg (.getString p 0)]
      (.LLVMDisposeMessage $llvm p)
      (throw (ex-info msg {})))))
