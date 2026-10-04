(ns oben.core.compiler
  "The explicit Oben compilation boundary.

   This namespace orchestrates target-aware compilation of an Oben function
   into a rendered, verified LLVM module. Targets decide the target-specific
   context, lowering, and what to do with the resulting module and function IR."
  (:require [oben.core.context :as ctx]
            [oben.core.target :as target-api]
            [omkamra.llvm.buffer :as llvm-buffer]
            [omkamra.llvm.context :as llvm-context]
            [omkamra.llvm.ir :as ir]
            [omkamra.llvm.module :as llvm-module]))

(defn module-data
  [compiler-ctx]
  (merge (:m compiler-ctx)
         (:target-layout compiler-ctx)))

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

(defn- target-cell
  [target]
  (if (instance? clojure.lang.IDeref target)
    target
    (atom target)))

(defn compile-function
  "Compiles `fnode` using the explicitly supplied target.

   The target is allowed to provide target-specific parsing/lowering behavior;
   it is dynamically bound only while the compilation is running so existing
   Oben nodes can access target attributes. The target attributes and layout
   are retained in the returned compilation context. The returned `:ctx`
   retains Oben's compilation cache, `:module` contains rendered-module data,
   `:source` is verified textual LLVM IR, and `:function` is the compiled IR
   function corresponding to `fnode`."
  [target compiler-ctx fnode]
   (let [target (target-cell target)
         compiler-ctx (assoc compiler-ctx
                             :target-attrs (target-api/attrs* target))]
     (binding [target-api/*current-target* target]
       (let [compiler-ctx (-> compiler-ctx
                              ctx/next-epoch
                              (ctx/compile-node fnode))
             source (render-module compiler-ctx)]
         (verify-module-source! source)
         {:ctx compiler-ctx
          :target-attrs (:target-attrs compiler-ctx)
          :fnode fnode
          :function (ctx/compiled-node compiler-ctx fnode)
          :module (module-data compiler-ctx)
          :source source}))))
