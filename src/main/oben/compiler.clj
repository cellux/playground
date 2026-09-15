(ns oben.compiler
  "The explicit Oben compilation boundary.

   This namespace owns the target-independent part of compiling an Oben
   function into a rendered, verified LLVM module. Targets decide what to do
   with the resulting module and function IR."
  (:require [oben.core.context :as ctx]
            [omkamra.llvm.buffer :as llvm-buffer]
            [omkamra.llvm.context :as llvm-context]
            [omkamra.llvm.ir :as ir]
            [omkamra.llvm.module :as llvm-module]
            [omkamra.llvm.platform :as platform]))

(defn module-data
  [compiler-ctx]
  (assoc (:m compiler-ctx)
         :data-layout platform/data-layout
         :target-triple platform/target-triple))

(defn render-module
  [compiler-ctx]
  (ir/render-module (module-data compiler-ctx)))

(defn verify-module-source!
  "Parses and verifies textual LLVM IR, then releases all temporary LLVM
   resources. Returns the original source when it is valid."
  [source]
  (let [llvm-ctx (llvm-context/create)
        buffer (llvm-buffer/from-string source)]
    (try
      ;; LLVMParseIRInContext consumes the memory buffer. Do not dispose it
      ;; here; doing so after parsing double-frees native memory.
      (let [module (llvm-module/from-buffer buffer llvm-ctx)]
        (try
          (llvm-module/verify module)
          source
          (finally
            (llvm-module/dispose module))))
      (finally
        (llvm-context/dispose llvm-ctx)))))

(defn compile-function
  "Compiles `fnode` into a target-independent result map.

   The returned `:ctx` retains Oben's compilation cache, `:module` contains the
   rendered-module data, `:source` is verified textual LLVM IR, and `:function`
   is the compiled IR function corresponding to `fnode`."
  [compiler-ctx fnode]
  (let [compiler-ctx (-> compiler-ctx
                         ctx/next-epoch
                         (ctx/compile-node fnode))
        source (render-module compiler-ctx)]
    (verify-module-source! source)
    {:ctx compiler-ctx
     :fnode fnode
     :function (ctx/compiled-node compiler-ctx fnode)
     :module (module-data compiler-ctx)
     :source source}))
