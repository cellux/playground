(ns rb.explores.jffi
  (:import (com.kenai.jffi Foreign Library Type
                           CallContext CallingConvention
                           Invoker HeapInvocationBuffer)))

(defn getpid-v0
  []
  (let [cc (CallContext/getCallContext
            Type/SINT
            (into-array Type [])
            CallingConvention/DEFAULT
            false)
        default-library (Library/getDefault)
        getpid-address (.getSymbolAddress default-library "getpid")
        invoker (Invoker/getInstance)]
    (.invokeI0 invoker cc getpid-address)))

(defn resolve-type-sym
  [t]
  (case (keyword t)
    :void 'com.kenai.jffi.Type/VOID
    :float 'com.kenai.jffi.Type/FLOAT
    :double 'com.kenai.jffi.Type/DOUBLE
    (:longdouble :ldouble) 'com.kenai.jffi.Type/LONGDOUBLE
    (:uint8 :uchar) 'com.kenai.jffi.Type/UINT8
    (:sint8 :int8 :char) 'com.kenai.jffi.Type/SINT8
    (:uint16 :ushort) 'com.kenai.jffi.Type/UINT16
    (:sint16 :int16 :sshort :short) 'com.kenai.jffi.Type/SINT16
    (:uint32 :uint) 'com.kenai.jffi.Type/UINT32
    (:sint32 :int32 :sint :int) 'com.kenai.jffi.Type/SINT32
    (:uint64 :ulonglong :ullong) 'com.kenai.jffi.Type/UINT64
    (:sint64 :int64 :slonglong :longlong :sllong :llong) 'com.kenai.jffi.Type/SINT64
    (:pointer :ptr) 'com.kenai.jffi.Type/POINTER))

(defn type->invoke-sym
  [t]
  (case t
    com.kenai.jffi.Type/FLOAT '.invokeFloat
    com.kenai.jffi.Type/DOUBLE '.invokeDouble
    (com.kenai.jffi.Type/UINT8 com.kenai.jffi.Type/SINT8) '.invokeInt
    (com.kenai.jffi.Type/UINT16 com.kenai.jffi.Type/SINT16) '.invokeInt
    (com.kenai.jffi.Type/UINT32 com.kenai.jffi.Type/SINT32) '.invokeInt
    (com.kenai.jffi.Type/UINT64 com.kenai.jffi.Type/SINT64) '.invokeLong
    com.kenai.jffi.Type/POINTER '.invokeAddress))

(defn type->put-sym
  [t]
  (case t
    com.kenai.jffi.Type/FLOAT '.putFloat
    com.kenai.jffi.Type/DOUBLE '.putDouble
    com.kenai.jffi.Type/LONGDOUBLE '.putLongDouble
    (com.kenai.jffi.Type/UINT8 com.kenai.jffi.Type/SINT8) '.putByte
    (com.kenai.jffi.Type/UINT16 com.kenai.jffi.Type/SINT16) '.putShort
    (com.kenai.jffi.Type/UINT32 com.kenai.jffi.Type/SINT32) '.putInt
    (com.kenai.jffi.Type/UINT64 com.kenai.jffi.Type/SINT64) '.putLong
    com.kenai.jffi.Type/POINTER '.putAddress))

(defmacro define-foreign
  ([func-name args save-errno]
   (let [return-type (resolve-type-sym (:tag (meta args)))
         arg-types (map (comp resolve-type-sym :tag meta) args)]
     `(let [arg-types# (into-array Type ~(vec arg-types))
            cc# (CallContext/getCallContext ~return-type arg-types#
                                            CallingConvention/DEFAULT
                                            ~save-errno)
            lib# (Library/getDefault)
            func-address# (.getSymbolAddress lib# ~(str func-name))
            invoker# (Invoker/getInstance)]
        (defn ~func-name
          ~(vec args)
          (let [buffer# (HeapInvocationBuffer. cc#)]
            ~@(for [i (range (count arg-types))]
                (let [type (aget arg-types i)]
                  (list ~(type->put-sym type) 'buffer# (nth args i))))
            (~(type->invoke-sym return-type) invoker# cc# func-address# buffer#))))))
  ([func-name args]
   `(define-foreign ~func-name ~args false)))

(define-foreign getpid ^int [])
