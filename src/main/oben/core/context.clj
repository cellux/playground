(ns oben.core.context
  (:require [clojure.core :as clj])
  (:require [oben.core.types :as t])
  (:require [oben.core.types.Fn :as Fn])
  (:require [omkamra.llvm.ir :as ir])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.buffer :as llvm-buffer])
  (:require [omkamra.llvm.module :as llvm-module])
  (:require [omkamra.llvm.engine :as llvm-engine])
  (:import (java.nio ByteBuffer ByteOrder))
  (:import (com.kenai.jffi Type CallContext CallingConvention
                           Invoker HeapInvocationBuffer ArrayFlags))
  (:require [midje.sweet :as m]))

(def default-ftab-size 65536)

(defn allocate-ftab
  [size]
  (.asLongBuffer
   (doto (ByteBuffer/allocateDirect size)
     (.order (ByteOrder/nativeOrder)))))

(def blockbin-ids [:entry :init :main :exit])

(def init-fdata
  {:blockbins (reduce #(assoc %1 %2 (list)) {} blockbin-ids)
   :blockbin-id :main
   :label-blocks {}
   :return-values {}})

(defn new
  ([ftabsize]
   {:llvm {:context nil
           :ee nil}
    :m (ir/module)
    :epoch 0
    :node-id 0
    :node-name nil
    :f nil
    :fdata nil
    :ftab (allocate-ftab ftabsize)
    :fid 0
    :ir nil
    :compiled-node nil
    :compiled {}
    :mode :dev})
  ([]
   (oben.core.context/new default-ftab-size)))

(defn next-epoch
  [ctx]
  (-> ctx
      (assoc :m (ir/module)
             :node-id 0
             :node-name nil
             :f nil
             :fdata nil
             :ir nil
             :compiled-node nil)
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
  (or (some-> (meta node) :name name)
      (some-> (meta node) :class name)
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
              (update-in ctx [:fdata :label-blocks]
                         assoc label-node
                         (ir/basic-block block-name))))]
    (-> ctx
        (assign-node-name label-node)
        (create-label-block label-node))))

(defn get-label-block
  [ctx label-node]
  (get-in ctx [:fdata :label-blocks label-node]))

(defn save-ir
  [ctx ir]
  (assoc ctx :ir ir))

(defn current-bb
  ([ctx blockbin-id]
   (first (get-in ctx [:fdata :blockbins blockbin-id])))
  ([ctx]
   (current-bb ctx (get-in ctx [:fdata :blockbin-id]))))

(defn ensure-bb
  ([ctx blockbin-id]
   (let [blockbin (get-in ctx [:fdata :blockbins blockbin-id])]
     (if (seq blockbin)
       ctx
       (update-in ctx [:fdata :blockbins blockbin-id]
                  conj (ir/basic-block)))))
  ([ctx]
   (ensure-bb ctx (get-in ctx [:fdata :blockbin-id]))))

(defn flush-bb
  ([ctx blockbin-id next-block]
   (let [current-block (current-bb ctx blockbin-id)]
     (if (or (seq (:instructions current-block)) next-block)
       (update-in ctx [:fdata :blockbins blockbin-id]
                  conj (or next-block (ir/basic-block)))
       ctx)))
  ([ctx blockbin-id]
   (flush-bb ctx blockbin-id nil))
  ([ctx]
   (flush-bb ctx (get-in ctx [:fdata :blockbin-id]))))

(defn append-bb
  ([ctx bb blockbin-id]
   (-> ctx
       (ensure-bb blockbin-id)
       (flush-bb blockbin-id bb)))
  ([ctx bb]
   (append-bb ctx bb (get-in ctx [:fdata :blockbin-id]))))

(defn update-current-bb
  [ctx blockbin-id f & args]
  (update-in ctx [:fdata :blockbins blockbin-id]
             (fn [blockbin]
               (cons (apply f (first blockbin) args)
                     (next blockbin)))))

(defn compile-instruction
  ([ctx ins blockbin-id]
   (letfn [(flush-if-terminator [ctx]
             (if (ir/terminator? ins)
               (flush-bb ctx blockbin-id)
               ctx))]
     (-> ctx
         (ensure-bb blockbin-id)
         (update-current-bb blockbin-id ir/add-i ins)
         flush-if-terminator
         (save-ir ins))))
  ([ctx ins]
   (compile-instruction ctx ins (get-in ctx [:fdata :blockbin-id]))))

(defn register-return-value
  [ctx block-id return-block return-value]
  (assoc-in
   ctx
   [:fdata :return-values block-id return-block]
   return-value))

(defn get-return-values
  [ctx block-id]
  (get-in ctx [:fdata :return-values block-id]))

(defn compile-node
  [ctx node]
  (if-let [ir (get (:compiled ctx) node)]
    (letfn [(declare-previously-compiled-globals [ctx]
              (if (and (:global? (meta node)) (:name ir))
                (if (isa? (t/tid-of node) ::Fn/Fn)
                  (if (contains? (:functions (:m ctx)) (:name ir))
                    ctx
                    (update ctx :m ir/add-function (dissoc ir :blocks)))
                  (if (contains? (:globals (:m ctx)) (:name ir))
                    ctx
                    (update ctx :m ir/add-global (dissoc ir :initializer))))
                ctx))
            (save-ir [ctx]
              (assoc ctx :ir ir))]
      (-> ctx
          declare-previously-compiled-globals
          save-ir))
    (letfn [(save-node-ir [ctx]
              (update ctx :compiled
                      assoc node (:ir ctx)))]
      (let [saved ctx
            compile-fn node]
        (-> ctx
            (assign-node-name node)
            (assoc :compiled-node node)
            (dissoc :ir)
            compile-fn
            save-node-ir
            (merge (select-keys saved [:compiled-node])))))))

(defn compiled
  [ctx node]
  (get (:compiled ctx) node))

(defn with-blockbin
  [ctx blockbin-id f]
  (let [saved (get-in ctx [:fdata :blockbin-id])
        ctx (assoc-in ctx [:fdata :blockbin-id] blockbin-id)]
    (assoc-in (f ctx) [:fdata :blockbin-id] saved)))

(defn collect-blocks
  [ctx]
  (let [referenced-block? (set (concat (vals (get-in ctx [:fdata :label-blocks]))
                                       (mapcat keys (vals (get-in ctx [:fdata :return-values])))))]
    (letfn [(collect? [bb]
              (or (:name bb)
                  (seq (:instructions bb))
                  (referenced-block? bb)))]
      (reduce (fn [ctx bb]
                (update ctx :f ir/add-bb bb))
              ctx
              (->> (mapcat (get-in ctx [:fdata :blockbins])
                           (reverse blockbin-ids))
                   (filter collect?)
                   reverse)))))

(defn forget-node
  [ctx node]
  (-> ctx
      (update :compiled dissoc node)))

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
    :ptr Type/POINTER
    :array Type/POINTER))

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
            :ptr (.putAddress hib (long arg))
            :array (let [[_ elt size] type
                         flags ArrayFlags/PINNED]
                     (case (ir/extract-type-tag elt)
                       :integer (let [[_ size] type]
                                  (case size
                                    1 (.putArray hib (bytes arg) 0 (count arg) flags)
                                    8 (.putArray hib (bytes arg) 0 (count arg) flags)
                                    16 (.putArray hib (shorts arg) 0 (count arg) flags)
                                    32 (.putArray hib (ints arg) 0 (count arg) flags)
                                    64 (.putArray hib (longs arg) 0 (count arg) flags)))
                       :float (.putArray hib (floats arg) 0 (count arg) flags)
                       :double (.putArray hib (doubles arg) 0 (count arg) flags)))))
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
