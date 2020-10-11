(ns rb.explores.llvm.test
  (:require
   [rb.explores.llvm.api :as api]
   [rb.explores.llvm.context :as context]
   [rb.explores.llvm.buffer :as buffer]
   [rb.explores.llvm.module :as module]
   [rb.explores.llvm.engine :as engine])
  (:import (com.kenai.jffi Type CallContext CallingConvention Invoker)))

(defn hello-test
  []
  (let [ctx (context/create)
        mod (module/from-file "/home/rb/projects/llvm-server/hello.ll" ctx)
        ee (engine/create-mcjit-compiler mod)
        hello1 (engine/get-function-address ee "hello1")
        cc (CallContext/getCallContext Type/VOID (into-array Type [])
                                       CallingConvention/DEFAULT
                                       false)
        invoker (Invoker/getInstance)]
    (.invokeI0 invoker cc hello1)
    (engine/dispose ee)
    ;; do not dispose the module as it was owned by ee.
    (context/dispose ctx)))
