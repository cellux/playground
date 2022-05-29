(ns omkamra.llvm.api
  (:require [clojure.string :as str]
            [omkamra.jnr.library :as library]
            [omkamra.jnr.enum :as enum]
            [omkamra.jnr.struct :as struct]))

(enum/define LLVMVerifierFailureAction
  LLVMAbortProcessAction,
  LLVMPrintMessageAction,
  LLVMReturnStatusAction)

(enum/define LLVMCodeGenOptLevel
  LLVMCodeGenLevelNone,
  LLVMCodeGenLevelLess,
  LLVMCodeGenLevelDefault,
  LLVMCodeGenLevelAggressive)

(enum/define LLVMCodeModel
  LLVMCodeModelDefault,
  LLVMCodeModelJITDefault,
  LLVMCodeModelTiny,
  LLVMCodeModelSmall,
  LLVMCodeModelKernel,
  LLVMCodeModelMedium,
  LLVMCodeModelLarge)

(struct/define LLVMMCJITCompilerOptions
  ^int OptLevel
  ^LLVMCodeModel CodeModel
  ^int NoFramePointerElim
  ^int EnableFastISel
  ^Pointer MCJMM)

(library/define $llvm
  "LLVM"
  ;; Core.h
  (^void
   LLVMDisposeMessage [^Pointer Message])

  (^Pointer
   LLVMContextCreate)
  (^void
   LLVMContextDispose [^Pointer C])

  (^Pointer
   LLVMModuleCreateWithNameInContext [^String ModuleID
                                      ^Pointer C])
  (^void
   LLVMDisposeModule)
  (^void
   LLVMDumpModule [^Pointer M])
  (^int
   LLVMPrintModuleToFile [^Pointer M
                          ^String Filename
                          ^Pointer* ^:out ErrorMessage])
  (^String
   LLVMPrintModuleToString [^Pointer M])
  (^Pointer
   LLVMCreateModuleProviderForExistingModule [^Pointer M])
  (^void
   LLVMDisposeModuleProvider [^Pointer M])
  (^int
   LLVMCreateMemoryBufferWithContentsOfFile [^String Path
                                             ^Pointer* ^:out OutMemBuf
                                             ^Pointer* ^:out OutMessage])
  (^Pointer
   LLVMCreateMemoryBufferWithMemoryRange [^String InputData
                                          ^size_t InputDataLength
                                          ^String BufferName
                                          ^int RequiresNullTerminator])
  (^Pointer
   LLVMCreateMemoryBufferWithMemoryRangeCopy [^String InputData
                                              ^size_t InputDataLength
                                              ^String BufferName])
  (^Pointer
   LLVMGetBufferStart [^Pointer MemBuf])
  (^size_t
   LLVMGetBufferSize [^Pointer MemBuf])
  (^void
   LLVMDisposeMemoryBuffer [^Pointer MemBuf])

  (^Pointer
   LLVMCreatePassManager [])
  (^Pointer
   LLVMCreateFunctionPassManagerForModule [^Pointer M])
  (^int
   LLVMRunPassManager [^Pointer PM
                       ^Pointer M])
  (^int
   LLVMInitializeFunctionPassManager [^Pointer FPM])
  (^int
   LLVMRunFunctionPassManager [^Pointer FPM
                               ^Pointer F])
  (^int
   LLVMFinalizeFunctionPassManager [^Pointer FPM])
  (^void
   LLVMDisposePassManager [^Pointer PM])

  ;; Analysis.h
  (^int
   LLVMVerifyModule [^Pointer M
                     ^LLVMVerifierFailureAction ^:in Action
                     ^Pointer* ^:out OutMessage])

  ;; BitReader.h
  (^int
   LLVMParseBitcodeInContext2 [^Pointer ContextRef
                               ^Pointer MemBuf
                               ^Pointer* ^:out OutModule])

  ;; BitWriter.h
  (^int
   LLVMWriteBitcodeToFile [^Pointer M
                           ^String Path])
  (^Pointer
   LLVMWriteBitcodeToMemoryBuffer [^Pointer M])

  ;; ExecutionEngine.h
  (^void
   LLVMLinkInMCJIT [])
  (^void
   LLVMLinkInInterpreter [])
  (^int
   LLVMCreateExecutionEngineForModule [^Pointer* ^:out OutEE
                                       ^Pointer M
                                       ^Pointer* ^:out OutError])
  (^int
   LLVMCreateInterpreterForModule [^Pointer* ^:out OutInterp
                                   ^Pointer M
                                   ^Pointer* ^:out OutError])
  (^int
   LLVMCreateJITCompilerForModule [^Pointer* ^:out OutJIT
                                   ^Pointer M
                                   ^int OptLevel
                                   ^Pointer* ^:out OutError])
  (^void
   LLVMInitializeMCJITCompilerOptions [^LLVMMCJITCompilerOptions Options
                                       ^size_t SizeOfOptions])
  (^int
   LLVMCreateMCJITCompilerForModule [^Pointer* ^:out OutJIT
                                     ^Pointer M
                                     ^LLVMMCJITCompilerOptions Options
                                     ^size_t SizeOfOptions
                                     ^Pointer* ^:out OutError])
  (^void
   LLVMDisposeExecutionEngine [^Pointer EE])

  (^void
   LLVMAddModule [^Pointer EE
                  ^Pointer M])
  (^int
   LLVMRemoveModule [^Pointer EE
                     ^Pointer M
                     ^Pointer* ^:out OutMod
                     ^Pointer* ^:out OutError])
  (^int
   LLVMFindFunction [^Pointer EE
                     ^String Name
                     ^Pointer* ^:out OutFn])
  (^uint64_t
   LLVMGetGlobalValueAddress [^Pointer EE
                              ^String Name])
  (^uint64_t
   LLVMGetFunctionAddress [^Pointer EE
                           ^String Name])

  ;; IRReader.h
  (^int
   LLVMParseIRInContext [^Pointer ContextRef
                         ^Pointer MemBuf
                         ^Pointer* ^:out OutM
                         ^Pointer* ^:out OutMessage])

  ;; Linker.h
  (^int
   LLVMLinkModules2 [^Pointer Dest
                     ^Pointer Src])

  ;; Support.h
  (^int
   LLVMLoadLibraryPermanently [^String Filename]))

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
     (library/define ~library-name
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
