(ns oben.core.context
  (:require [clojure.core :as clj])
  (:require [oben.core.api :as o])
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
   (doto (ByteBuffer/allocateDirect (* size Long/BYTES))
     (.order (ByteOrder/nativeOrder)))))

(def blockbin-ids [:entry :init :main :exit])

(def init-fdata
  {:blockbins (reduce #(assoc %1 %2 (list)) {} blockbin-ids)
   :blockbin-id :main
   :label-blocks {}
   :return-values {}})

(defn create
  ([ftabsize]
   {:llvm {:context nil
           :ee nil}
    :m (ir/module)
    :epoch 0
    :next-id 0
    :next-name nil
    :f nil
    :fdata nil
    :ftab (allocate-ftab ftabsize)
    :fid 0
    :ir nil
    :compiling-node nil
    :compiled-nodes {}
    :compiling-type nil
    :compiled-types {}
    :mode :dev})
  ([]
   (create default-ftab-size)))

(defn next-epoch
  [ctx]
  (-> ctx
      (assoc :m (ir/module)
             :next-id 0
             :next-name nil
             :f nil
             :fdata nil
             :ir nil
             :compiling-node nil
             :compiling-type nil)
      (update :epoch inc)))

(defn get-llvm-context
  [ctx]
  (get-in ctx [:llvm :context]))

(defn set-llvm-context
  [ctx llvm-context]
  (assoc-in ctx [:llvm :context] llvm-context))

(defn dispose-llvm-context
  [ctx]
  (when-let [llvm-context (get-llvm-context ctx)]
    (llvm-context/dispose llvm-context))
  (update ctx :llvm dissoc :context))

(defn get-llvm-execution-engine
  [ctx]
  (get-in ctx [:llvm :ee]))

(defn set-llvm-execution-engine
  [ctx ee]
  (assoc-in ctx [:llvm :ee] ee))

(defn dispose-llvm-execution-engine
  [ctx]
  (when-let [llvm-engine (get-llvm-execution-engine ctx)]
    (llvm-engine/dispose llvm-engine))
  (update ctx :llvm dissoc :ee))

(defn assign-next-name
  [ctx prefix]
  (let [next-name (symbol (str prefix
                               "."
                               (:epoch ctx)
                               "."
                               (:next-id ctx)))]
    (-> ctx
        (update :next-id inc)
        (assoc :next-name next-name))))

(defn clear-next-name
  [ctx]
  (dissoc ctx :next-name))

(defn get-assigned-name
  [ctx]
  (:next-name ctx))

(defn node-name-prefix
  [node default-prefix]
   (or (some-> (meta node) :name name)
       (some-> (meta node) :class name)
       default-prefix))

(defn add-label-block
  [ctx label-node]
  (letfn [(create-label-block
            [ctx label-node]
            (let [block-name (keyword (get-assigned-name ctx))]
              (update-in ctx [:fdata :label-blocks]
                         assoc label-node
                         (ir/basic-block block-name))))]
    (-> ctx
        (assign-next-name (node-name-prefix label-node "label"))
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
  (if-let [ir (get (:compiled-nodes ctx) node)]
    (letfn [(declare-previously-compiled-globals [ctx]
              (if (and (:global? (meta node)) (:name ir))
                (if (isa? (o/tid-of-node node) :oben.core.types.Fn/Fn)
                  (if (contains? (:functions (:m ctx)) (:name ir))
                    ctx
                    (update ctx :m ir/add-function (dissoc ir :blocks)))
                  (if (contains? (:globals (:m ctx)) (:name ir))
                    ctx
                    (update ctx :m ir/add-global (dissoc ir :initializer))))
                ctx))]
      (-> ctx
          declare-previously-compiled-globals
          (save-ir ir)))
    (letfn [(save-node-ir [ctx]
              (update ctx :compiled-nodes
                      assoc node (:ir ctx)))]
      (let [saved ctx
            compile-fn node]
        (-> ctx
            (assoc :compiling-node node)
            (assign-next-name (node-name-prefix node "node"))
            (dissoc :ir)
            compile-fn
            save-node-ir
            (merge (select-keys saved [:compiling-node])))))))

(defn compiled-node
  [ctx node]
  (get (:compiled-nodes ctx) node))

(defn add-type-to-module-if-named
  ([ctx ir]
   (if-let [name (and (ir/struct-type? ir)
                      (ir/struct-name ir))]
     (if (contains? (:types (:m ctx)) name)
       ctx
       (update ctx :m ir/add-type ir))
     ctx))
  ([ctx]
   (add-type-to-module-if-named ctx (:ir ctx))))

(defn compile-type
  [ctx type]
  (if-let [ir (get (:compiled-types ctx) type)]
    (-> ctx
        (add-type-to-module-if-named ir)
        (save-ir ir))
    (letfn [(assign-name-if-named-type [ctx type]
              (if-let [name (:name (meta type))]
                (assign-next-name ctx name)
                (clear-next-name ctx)))
            (save-type-ir [ctx]
              (update ctx :compiled-types
                      assoc type (:ir ctx)))]
      (let [saved ctx
            compile-fn type]
        (-> ctx
            (assoc :compiling-type type)
            (assign-name-if-named-type type)
            (dissoc :ir)
            compile-fn
            save-type-ir
            add-type-to-module-if-named
            (merge (select-keys saved [:compiling-type])))))))

(defn compiled-type
  [ctx type]
  (get (:compiled-types ctx) type))

(defn with-blockbin
  [ctx blockbin-id f]
  (let [saved (get-in ctx [:fdata :blockbin-id])
        ctx (assoc-in ctx [:fdata :blockbin-id] blockbin-id)]
    (assoc-in (f ctx) [:fdata :blockbin-id] saved)))

(defn collect-blocks
  [ctx]
  (let [label-blocks (vals (get-in ctx [:fdata :label-blocks]))
        return-blocks (mapcat keys (vals (get-in ctx [:fdata :return-values])))
        referenced-block? (set (concat label-blocks return-blocks))]
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
      (update :compiled-nodes dissoc node)))

(defn forget-type
  [ctx type]
  (-> ctx
      (update :compiled-types dissoc type)))

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
  (let [ee (get-llvm-execution-engine ctx)]
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
        _ (when (zero? address)
            (throw (ex-info "cannot get function address" {:function f})))
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
            :ptr (letfn [(put-arg-as-array
                           [arg-elt]
                           (let [[_ object-type] type]
                             (case (ir/extract-type-tag object-type)
                               :array (let [[_ elt size] object-type]
                                        (when (not= elt arg-elt)
                                          (throw (ex-info "array element type mismatch"
                                                          {:actual arg-elt :desired elt})))
                                        (when (not= size (count arg))
                                          (throw (ex-info "array size mismatch"
                                                          {:actual (count arg) :desired size}))))
                               (when (not= arg-elt object-type)
                                 (throw (ex-info "array element type mismatch"
                                                 {:actual arg-elt :desired object-type}))))
                             (.putArray hib arg 0 (count arg) ArrayFlags/PINNED)))]
                   (cond (integer? arg)
                         (.putAddress hib (long arg))
                         (instance? (Class/forName "[B") arg)
                         (put-arg-as-array [:integer 8])
                         (instance? (Class/forName "[S") arg)
                         (put-arg-as-array [:integer 16])
                         (instance? (Class/forName "[I") arg)
                         (put-arg-as-array [:integer 32])
                         (instance? (Class/forName "[J") arg)
                         (put-arg-as-array [:integer 64])
                         (instance? (Class/forName "[F") arg)
                         (put-arg-as-array [:float 32])
                         (instance? (Class/forName "[D") arg)
                         (put-arg-as-array [:float 64])
                         :else (throw (ex-info "cannot convert vector arg into array pointer" {:vector arg :type type}))))))
        (case (ir/extract-type-tag result-type)
          :void (do (.invokeInt invoker cc address hib) nil)
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
