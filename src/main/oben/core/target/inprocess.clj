(ns oben.core.target.inprocess
  (:require [oben.core.target :as target])
  (:require [oben.core.protocols.Target :as Target])
  (:require [oben.core.context :as ctx])
  (:require [omkamra.llvm.ir :as ir])
  (:require [omkamra.llvm.platform :as platform])
  (:require [omkamra.llvm.context :as llvm-context])
  (:require [omkamra.llvm.buffer :as llvm-buffer])
  (:require [omkamra.llvm.module :as llvm-module])
  (:require [omkamra.llvm.engine :as llvm-engine])
  (:import (com.kenai.jffi Type CallContext CallingConvention
                           Invoker HeapInvocationBuffer ArrayFlags)))

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

(defn assemble-module
  [ctx]
  (let [m (assoc (:m ctx)
                 :data-layout platform/data-layout
                 :target-triple platform/target-triple)
        module-src (ir/render-module m)
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
        (set-llvm-execution-engine ee))))

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

(defmulti make-function-invoker
  (fn [invoke-strategy ctx f] invoke-strategy))

(defmethod make-function-invoker :jnr
  [_ ctx f]
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

(defrecord InProcessTarget [ctx attrs invoke-strategy]
  Target/protocol

  (compile-function [this fnode]
    (let [ctx (ctx/next-epoch ctx)
          ctx (ctx/compile-node ctx fnode)
          ctx (assemble-module ctx)]
      (assoc this :ctx ctx)))

  (invoke-function [this fnode args]
    (let [f (ctx/compiled-node ctx fnode)
          invoker (make-function-invoker invoke-strategy ctx f)]
      (apply invoker args)))

  (dispose [this]
    (let [ctx (dispose-llvm-execution-engine ctx)
          ctx (dispose-llvm-context ctx)]
      (assoc this :ctx ctx))))

(def default-attrs
  {:address-size platform/address-size
   :align-min 1})

(defn create
  [{:keys [attrs invoke-strategy] :as opts}]
  (map->InProcessTarget
   {:ctx (ctx/create)
    :attrs (merge default-attrs attrs)
    :invoke-strategy (or invoke-strategy :jnr)}))
