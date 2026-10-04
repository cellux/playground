(ns oben.core.abi
  "The shared byte-buffer ABI used by the out-of-process and in-process
   adapters. Values are encoded using the target's LLVM memory layout. Pointer
   values are intentionally excluded: an address is meaningful only inside the
   process that owns it."
  (:require [clojure.string :as str]
            [oben.core.api :as o]
            [oben.core.context :as ctx]
            [oben.core.types.Struct :as Struct]
            [omkamra.llvm.engine :as llvm-engine]
            [omkamra.llvm.ir :as ir])
  (:import [java.nio ByteBuffer ByteOrder]
           [com.kenai.jffi Type CallContext CallingConvention Invoker
            HeapInvocationBuffer ArrayFlags]))

(defn- integer-ir?
  [ir-type]
  (and (vector? ir-type) (= :integer (first ir-type))))

(defn- pointer-type?
  [t]
  (isa? (o/tid-of-type t) :oben.core.types.Ptr/Ptr))

(defn- type-layout
  [compiler-ctx t]
  (let [ir-type (ctx/compiled-type compiler-ctx t)]
    (cond
      (= ir-type :void)
      {:kind :void :type t :ir-type ir-type :wire-size 0}

      (pointer-type? t)
      (throw (ex-info "byte-buffer ABI does not support pointer values"
                      {:type t}))

      (integer-ir? ir-type)
      {:kind :scalar :type t :ir-type ir-type
       :wire-size (max 1 (quot (second ir-type) 8))}

      (= ir-type :float)
      {:kind :scalar :type t :ir-type ir-type :wire-size 4}

      (= ir-type :double)
      {:kind :scalar :type t :ir-type ir-type :wire-size 8}

      (isa? (o/tid-of-type t) :oben.core.types.Array/Array)
      (let [{:keys [element-type size]} (meta t)
            element-layout (type-layout compiler-ctx element-type)]
        {:kind :array :type t :ir-type ir-type :wire-size (* size (:wire-size element-layout))
         :size size :element-type element-type :element-layout element-layout})

      (isa? (o/tid-of-type t) :oben.core.types.Struct/Struct)
      (let [{:keys [field-types field-names packed?]} (meta t)
            field-layouts (mapv #(type-layout compiler-ctx %) field-types)
            offsets (vec (Struct/field-types->offsets compiler-ctx field-types packed?))]
        {:kind :struct :type t :ir-type ir-type
         :wire-size (o/sizeof compiler-ctx t)
         :field-names field-names :field-types field-types
         :field-layouts field-layouts :offsets offsets :packed? packed?})

      :else
      (throw (ex-info "unsupported value in byte-buffer ABI"
                      {:type t :ir-type ir-type})))))

(defn supported-function?
  "Returns true when the function has a definition and contains no pointer
   values at the host boundary. External declarations remain on the native
   target path because they have no adapter body to invoke."
  [fnode function]
  (let [ftype (:object-type (meta (o/type-of fnode)))]
    (and (seq (:basic-blocks function))
         ;; The byte-buffer adapter has a fixed host-side parameter layout and
         ;; therefore cannot represent an arbitrary number of varargs.  Such
         ;; functions use the native invocation path instead.
         (not (:variadic? (meta ftype)))
         (not (some pointer-type? (:param-types (meta ftype))))
         (not (pointer-type? (:return-type (meta ftype)))))))

(defn function-abi
  [compiler-ctx fnode function adapter-name]
  (let [ftype (:object-type (meta (o/type-of fnode)))
        param-types (:param-types (meta ftype))
        return-type (:return-type (meta ftype))
        param-layouts (mapv #(type-layout compiler-ctx %) param-types)
        return-layout (type-layout compiler-ctx return-type)]
    {:adapter adapter-name
     :function function
     :param-types param-types
     :param-layouts param-layouts
     :return-type return-type
     :return-layout return-layout
     :input-size (reduce + (map :wire-size param-layouts))
     :output-size (:wire-size return-layout)}))

(defn- gep
  [name base offset]
  (str "  %" name " = getelementptr i8, ptr %" base ", i64 " offset))

(defn adapter-source
  [{:keys [adapter function param-layouts return-layout input-size output-size]}]
  (let [offsets (butlast (reductions + 0 (map :wire-size param-layouts)))
        decoded (mapv (fn [index layout offset]
                        [(gep (str "arg" index "-ptr") "input" offset)
                         (str "  %arg" index " = load "
                              (ir/render-type (:ir-type layout))
                              ", ptr %arg" index "-ptr, align 1")])
                      (range) param-layouts offsets)
        call-args (str/join ", "
                            (map-indexed
                             (fn [index layout]
                               (str (ir/render-type (:ir-type layout)) " %arg" index))
                             param-layouts))
        return-ir (ir/render-type (:ir-type return-layout))
        call (str "  " (when-not (= :void (:kind return-layout)) "%result = ")
                  "call " return-ir " @" (:name function) "(" call-args ")")
        encode (when-not (= :void (:kind return-layout))
                 [(gep "result-ptr" "output" 0)
                  (str "  store " return-ir " %result, ptr %result-ptr, align 1")])]
    (str/join "\n"
              (concat
               [(str "define i64 @" adapter
                     "(ptr %input, i64 %input_length, ptr %output, i64 %output_capacity) {")
                "entry:"
                (str "  %input-ok = icmp eq i64 %input_length, " input-size)
                "  br i1 %input-ok, label %decode, label %invalid-input"
                "invalid-input:"
                "  ret i64 -1"
                "decode:"]
               (mapcat identity decoded)
               [call
                (str "  %output-ok = icmp uge i64 %output_capacity, " output-size)
                "  br i1 %output-ok, label %encode, label %output-too-small"
                "output-too-small:"
                "  ret i64 -2"
                "encode:"]
               encode
               [(str "  ret i64 " output-size)
                "}"]))))

(defn- little-endian-buffer
  [^bytes bytes]
  (doto (ByteBuffer/wrap bytes)
    (.order (ByteOrder/LITTLE_ENDIAN))))

(defn- zeroes!
  [^ByteBuffer buffer n]
  (dotimes [_ n]
    (.put buffer (byte 0))))

(defn- put-value!
  [^ByteBuffer buffer layout value]
  (case (:kind layout)
    :void nil
    :scalar (let [ir-type (:ir-type layout)]
              (cond
                (and (vector? ir-type) (= :integer (first ir-type)))
                (case (second ir-type)
                  (1 8) (.put buffer (unchecked-byte value))
                  16 (.putShort buffer (unchecked-short value))
                  32 (.putInt buffer (unchecked-int value))
                  64 (.putLong buffer (unchecked-long value)))
                (= :float ir-type) (.putFloat buffer (float value))
                (= :double ir-type) (.putDouble buffer (double value))))
    :array (do
             (when-not (= (:size layout) (count value))
               (throw (ex-info "array value has incorrect length"
                               {:expected (:size layout) :actual (count value)})))
             (doseq [element value]
               (put-value! buffer (:element-layout layout) element)))
    :struct (let [start (.position buffer)
                   fields (if (map? value)
                            (mapv value (:field-names layout))
                            (vec value))]
               (when-not (= (count fields) (count (:field-layouts layout)))
                 (throw (ex-info "struct value has incorrect field count"
                                 {:expected (count (:field-layouts layout))
                                  :actual (count fields)})))
               (doseq [[offset field-layout field]
                       (map vector (:offsets layout) (:field-layouts layout) fields)]
                 (zeroes! buffer (- (+ start offset) (.position buffer)))
                 (put-value! buffer field-layout field))
               (zeroes! buffer (- (:wire-size layout) (- (.position buffer) start))))))

(defn encode-args
  [{:keys [param-layouts input-size]} args]
  (when-not (= (count param-layouts) (count args))
    (throw (ex-info "invalid number of arguments"
                    {:expected (count param-layouts) :actual (count args)})))
  (let [bytes (byte-array input-size)
        buffer (little-endian-buffer bytes)]
    (doseq [[layout arg] (map vector param-layouts args)]
      (put-value! buffer layout arg))
    bytes))

(defn- get-value!
  [^ByteBuffer buffer layout]
  (case (:kind layout)
    :void nil
    :scalar (let [ir-type (:ir-type layout)]
              (cond
                (and (vector? ir-type) (= :integer (first ir-type)))
                (case (second ir-type)
                  (1 8) (.get buffer)
                  16 (.getShort buffer)
                  32 (.getInt buffer)
                  64 (.getLong buffer))
                (= :float ir-type) (.getFloat buffer)
                (= :double ir-type) (.getDouble buffer)))
    :array (mapv (fn [_]
                   (get-value! buffer (:element-layout layout)))
                 (range (:size layout)))
    :struct (let [start (.position buffer)
                  values (mapv (fn [offset field-layout]
                                 (.position buffer (+ start offset))
                                 (get-value! buffer field-layout))
                               (:offsets layout) (:field-layouts layout))]
              (.position buffer (+ start (:wire-size layout)))
              (if (every? keyword? (:field-names layout))
                (zipmap (:field-names layout) values)
                values))))

(defn decode-result
  [{:keys [return-layout output-size]} ^bytes bytes]
  (when-not (= output-size (alength bytes))
    (throw (ex-info "unexpected output length"
                    {:expected output-size :actual (alength bytes)})))
  (get-value! (little-endian-buffer bytes) return-layout))

(defn- jnr-function-address
  [ctx adapter]
  (let [ee (get-in ctx [:llvm :ee])
        address (llvm-engine/get-function-address ee adapter)]
    (when (zero? address)
      (throw (ex-info "cannot get function address" {:function adapter})))
    address))

(defn invoke-inprocess
  [ctx abi args]
  (let [input (encode-args abi args)
        output (byte-array (:output-size abi))
        address (jnr-function-address ctx (:adapter abi))
        cc (CallContext/getCallContext
            Type/UINT64
            (into-array Type [Type/POINTER Type/UINT64 Type/POINTER Type/UINT64])
            CallingConvention/DEFAULT
            false)
        hib (HeapInvocationBuffer. cc)
        invoker (Invoker/getInstance)]
    (.putArray hib input 0 (alength input) ArrayFlags/PINNED)
    (.putLong hib (long (alength input)))
    (.putArray hib output 0 (alength output) ArrayFlags/PINNED)
    (.putLong hib (long (alength output)))
    (let [return-code (.invokeLong invoker cc address hib)]
      (when-not (= return-code (long (:output-size abi)))
        (throw (ex-info "inprocess ABI adapter invocation failed"
                        {:return-code return-code
                         :expected (:output-size abi)})))
      (decode-result abi output))))
