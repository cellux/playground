(ns oben.core.context
  (:require [clojure.core :as clj])
  (:require [oben.core.types :as t])
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

(def blockbin-ids [:entry :init :main :exit])

(defn new-blockbins
  []
  (reduce #(assoc %1 %2 []) {} blockbin-ids))

(defn new
  ([ftabsize]
   {:llvm {:context nil
           :ee nil}
    :epoch 0
    :node-id 0
    :m (ir/module)
    :f nil
    :blockbins (new-blockbins)
    :blockbin-id :main
    :label-blocks {}
    :ftab (allocate-ftab ftabsize)
    :fid 0
    :mode :dev
    :types #{}
    :compiled {}
    :compiled-node nil
    :ir nil
    :node-blocks {}
    :node-block nil})
  ([]
   (oben.core.context/new default-ftab-size)))

(defn next-epoch
  [ctx]
  (-> ctx
      (assoc :m (ir/module)
             :f nil
             :blockbins (new-blockbins)
             :blockbin-id :main
             :node-id 0
             :compiled-node nil
             :label-blocks {}
             :compiled-node nil
             :ir nil
             :node-blocks {}
             :node-block nil)
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

(defn node-name-prefix
  [node]
  (or (some-> (meta node)
              :name
              name)
      (some-> (meta node)
              :class
              name)
      "node"))

(defn assign-node-name
  [ctx node]
  (let [node-name (symbol (str (node-name-prefix node)
                               "."
                               (:epoch ctx)
                               "."
                               (:node-id ctx)))]
    (-> ctx
        (update :node-id inc)
        (assoc :node-name node-name))))

(defn get-assigned-name
  [ctx]
  (:node-name ctx))

(defn add-label-block
  [ctx label-node]
  (letfn [(create-label-block
            [ctx label-node]
            (let [block-name (keyword (get-assigned-name ctx))]
              (update ctx :label-blocks
                      assoc label-node
                      (ir/basic-block block-name))))]
    (-> ctx
        (assign-node-name label-node)
        (create-label-block label-node))))

(defn get-label-block
  [ctx label-node]
  (get (:label-blocks ctx) label-node))

(defn save-ir
  [ctx ir]
  (assoc ctx :ir ir))

(defn save-node-block
  [ctx bb]
  (assoc ctx :node-block bb))

(defn ensure-bb
  ([ctx blockbin-id]
   (let [blockbin (get (:blockbins ctx) blockbin-id)]
     (if (seq blockbin)
       ctx
       (update-in ctx [:blockbins blockbin-id]
                  conj (ir/basic-block)))))
  ([ctx]
   (ensure-bb ctx (:blockbin-id ctx))))

(defn flush-bb
  ([ctx blockbin-id next-block]
   (let [blockbin (get (:blockbins ctx) blockbin-id)
         last-block (last blockbin)]
     (if (or (seq (:instructions last-block)) next-block)
       (letfn [(flush-last-block [ctx]
                 (update-in
                  ctx [:blockbins blockbin-id]
                  conj (or next-block (ir/basic-block))))]
         (-> ctx flush-last-block))
       ctx)))
  ([ctx blockbin-id]
   (flush-bb ctx blockbin-id nil))
  ([ctx]
   (flush-bb ctx (:blockbin-id ctx))))

(defn append-bb
  ([ctx bb blockbin-id]
   (-> ctx
       (ensure-bb blockbin-id)
       (flush-bb blockbin-id bb)))
  ([ctx bb]
   (append-bb ctx bb (:blockbin-id ctx))))

(defn current-bb
  ([ctx blockbin-id]
   (let [blockbin (get (:blockbins ctx) blockbin-id)]
     (last blockbin)))
  ([ctx]
   (current-bb ctx (:blockbin-id ctx))))

(defn compile-instruction
  ([ctx ins blockbin-id]
   (letfn [(add-instruction [ctx]
             (let [blockbin (get (:blockbins ctx) blockbin-id)
                   last-index (dec (count blockbin))]
               (update-in ctx [:blockbins blockbin-id last-index]
                          ir/add-i ins)))
           (flush-if-terminator [ctx]
             (if (ir/terminator? ins)
               (flush-bb ctx blockbin-id)
               ctx))]
     (-> (ensure-bb ctx blockbin-id)
         add-instruction
         flush-if-terminator
         (save-ir ins))))
  ([ctx ins]
   (compile-instruction ctx ins (:blockbin-id ctx))))

(defn compiled
  [ctx node]
  (get (:compiled ctx) node))

(defn get-node-block
  [ctx node]
  (get (:node-blocks ctx) node))

(defn with-blockbin
  [ctx blockbin-id f]
  (let [saved ctx
        ctx (assoc ctx :blockbin-id blockbin-id)]
    (merge (f ctx) (select-keys saved [:blockbin-id]))))

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
    (letfn [(save-node-ir [ctx]
              (update ctx :compiled
                      assoc node (:ir ctx)))
            (save-node-block [ctx]
              (update ctx :node-blocks
                      assoc node (or (:node-block ctx)
                                     (current-bb ctx))))]
      (let [saved ctx]
        (-> ctx
            (assign-node-name node)
            (assoc :compiled-node node)
            (dissoc :ir :node-block)
            node
            save-node-ir
            save-node-block
            (merge (select-keys saved [:compiled-node])))))))

(defn collect-blocks
  [ctx]
  (letfn [(collect? [block]
            (or (:name block)
                (seq (:instructions block))))]
    (reduce (fn [ctx bb]
              (update ctx :f ir/add-bb bb))
            ctx
            (->> (mapcat (:blockbins ctx) blockbin-ids)
                 (filter collect?)))))

(defn forget-node
  [ctx node]
  (-> ctx
      (update :compiled dissoc node)
      (update :node-blocks dissoc node)))

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
          (set-llvm-execution-engine ee)))
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
                 1 Type/UINT8
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
                         1 (.putByte hib (int arg))
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
                       1 (unchecked-byte (.invokeInt invoker cc address hib))
                       8 (unchecked-byte (.invokeInt invoker cc address hib))
                       16 (unchecked-short (.invokeInt invoker cc address hib))
                       32 (unchecked-int (.invokeInt invoker cc address hib))
                       64 (unchecked-long (.invokeLong invoker cc address hib))))
          :float (.invokeFloat invoker cc address hib)
          :double (.invokeDouble invoker cc address hib))))))
