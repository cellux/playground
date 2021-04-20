(ns oben.lang.context
  (:require [clojure.core :as clj])
  (:require [oben.lang.types :as t])
  (:require [omkamra.llvm.ir :as ir])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.buffer :as llvm-buffer])
  (:require [omkamra.llvm.module :as llvm-module])
  (:require [omkamra.llvm.engine :as llvm-engine])
  (:import (java.nio ByteBuffer ByteOrder))
  (:import (com.kenai.jffi Type CallContext CallingConvention
                           Invoker HeapInvocationBuffer))
  (:require [midje.sweet :as m]))

(def default-ftab-size 65536)

(defn allocate-ftab
  [size]
  (.asLongBuffer
   (doto (ByteBuffer/allocateDirect size)
     (.order (ByteOrder/nativeOrder)))))

(defn new
  ([ftabsize]
   {:llvm {:context nil
           :ee nil}
    :epoch 0
    :node-id 0
    :m (ir/module)
    :f nil
    :bb nil
    :ftab (allocate-ftab ftabsize)
    :fid 0
    :mode :dev
    :types #{}
    :compiled {}
    :ir nil})
  ([]
   (oben.lang.context/new default-ftab-size)))

(defn next-epoch
  [ctx]
  (-> ctx
      (assoc :m (ir/module)
             :f nil
             :bb nil)
      (update :epoch inc)))

(defn get-llvm-context
  [ctx]
  (get-in ctx [:llvm :context]))

(defn set-llvm-context
  [ctx llvm-context]
  (assoc-in ctx [:llvm :context] llvm-context))

(defn get-llvm-execution-engine
  [ctx]
  (get-in ctx [:llvm :ee]))

(defn set-llvm-execution-engine
  [ctx ee]
  (assoc-in ctx [:llvm :ee] ee))

(defn dispose
  [ctx]
  (when-let [ee (get-llvm-execution-engine ctx)]
    (llvm-engine/dispose ee))
  (when-let [llvm-context (get-llvm-context ctx)]
    (llvm-context/dispose llvm-context))
  (dissoc ctx :llvm))

(defn flush-bb
  [ctx]
  (assert (:f ctx) "no compiled function")
  (if (:bb ctx)
    (-> ctx
        (update :f ir/add-bb (:bb ctx))
        (dissoc :bb))
    ctx))

(defn ensure-bb
  [ctx]
  (update ctx :bb #(or % (ir/basic-block))))

(defn store-ir
  [ctx ir]
  (assoc ctx :ir ir))

(defn compile-instruction
  [ctx instruction]
  (assert (:f ctx) "no compiled function")
  (letfn [(add-instruction [ctx]
            (update ctx :bb ir/add-i instruction))
          (flush-if-terminator [ctx]
            (if (ir/terminator? instruction)
              (flush-bb ctx)
              ctx))]
    (-> ctx 
        ensure-bb
        add-instruction
        flush-if-terminator
        (store-ir instruction))))

(defn compiled
  [ctx node]
  (get (:compiled ctx) node))

(defn node-name-prefix
  [node]
  (case (t/typeclass-of node)
    ::t/Fn "fn"
    ::t/Int "i"
    ::t/FP "f"
    "node"))

(defn store-node-name
  [ctx node]
  (let [name (symbol (str (or (:name (meta node))
                              (node-name-prefix node))
                          "."
                          (:epoch ctx)
                          "."
                          (:node-id ctx)))]
    (-> ctx
        (update :node-id inc)
        (assoc :node-name name))))

(def node-name :node-name)

(defn compile-node
  [ctx node]
  (if-let [ir (get (:compiled ctx) node)]
    (if (:global? (meta node))
      (case (t/typeclass-of node)
        ::t/Fn (if (contains? (:functions (:m ctx)) (:name ir))
                 ctx
                 (update ctx :m ir/add-function (dissoc ir :blocks)))
        (if (contains? (:globals (:m ctx)) (:name ir))
          ctx
          (update ctx :m ir/add-global (dissoc ir :initializer))))
      ctx)
    (letfn [(store-compiled [ctx]
              (update ctx :compiled assoc node (:ir ctx)))]
      (-> ctx
          (store-node-name node)
          node
          store-compiled))))

(defn compile-nodes
  [ctx nodes]
  (reduce compile-node ctx nodes))

(defn forget-node
  [ctx node]
  (dissoc ctx :compiled node))

(defn assemble-module
  [ctx]
  (if (seq (:m ctx))
    (let [module-src (ir/render-module (:m ctx))
          buf (llvm-buffer/from-string module-src)
          llvm-context (or (get-llvm-context ctx)
                           (llvm-context/create))
          mod (llvm-module/from-buffer buf llvm-context)
          existing-ee (get-llvm-execution-engine ctx)
          ee (or existing-ee
                 (llvm-engine/create-mcjit-compiler mod))]
      (when existing-ee
        (llvm-engine/add-module existing-ee mod))
      (-> ctx
          (set-llvm-context llvm-context)
          (set-llvm-execution-engine ee)
          (dissoc :f :bb)
          (assoc :m (ir/module))))
    ctx))

(defn get-function-address
  [ctx f]
  (let [ee (get-in ctx [:llvm :ee])]
    (llvm-engine/get-function-address ee (str (:name f)))))

(defn jnr-type-of
  [ir-type]
  (case (ir/extract-type-tag ir-type)
    :void Type/VOID
    :float Type/FLOAT
    :double Type/DOUBLE
    :integer (let [[_ size] ir-type]
               (case size
                 8 Type/UINT8
                 16 Type/UINT16
                 32 Type/UINT32
                 64 Type/UINT64))
    :ptr Type/POINTER))

(defn invoker
  [ctx f]
  (let [address (get-function-address ctx f)
        result-type (:result-type f)
        param-types (map :type (:params f))
        cc (CallContext/getCallContext
            (jnr-type-of result-type)
            (into-array Type (map jnr-type-of param-types))
            CallingConvention/DEFAULT
            false)
        invoker (Invoker/getInstance)]
    (fn [& args]
      (let [hib (HeapInvocationBuffer. cc)]
        (doseq [[arg type] (map vector args param-types)]
          (case (ir/extract-type-tag type)
            :integer (let [[_ size] type]
                       (case size
                         8 (.putByte hib (int arg))
                         16 (.putShort hib (int arg))
                         32 (.putInt hib (int arg))
                         64 (.putLong hib (long arg))))
            :float (.putFloat hib (float arg))
            :double (.putDouble hib (double arg))
            :ptr (.putAddress hib (long arg))))
        (case (ir/extract-type-tag result-type)
          :ptr (.invokeAddress invoker cc address hib)
          :integer (let [[_ size] result-type]
                     (case size
                       32 (.invokeInt invoker cc address hib)
                       64 (.invokeLong invoker cc address hib)))
          :float (.invokeFloat invoker cc address hib)
          :double (.invokeDouble invoker cc address hib))))))
