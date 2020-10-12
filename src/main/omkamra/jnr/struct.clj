(ns omkamra.jnr.struct
  (:require
   [clojure.string :as str]
   [insn.core :as insn]
   [insn.util :refer [type-desc]]
   [omkamra.jnr.util :refer [qualified-class-name]]))

(defn build-struct-field
  [{:keys [name type] :as spec}]
  {:name (clojure.core/name name)
   :type type
   :flags #{:public :final}})

(defn build-struct
  [cls field-specs]
  {:name cls
   :flags #{:public :super}
   :super jnr.ffi.Struct
   :fields
   (mapv build-struct-field field-specs)
   :methods
   [{:name :init
     :flags #{:public}
     :desc [jnr.ffi.Runtime :void]
     :emit
     (->> [[[:aload 0]
            [:aload 1]
            [:invokespecial jnr.ffi.Struct :init [jnr.ffi.Runtime :void]]]
           (mapcat (fn [{:keys [name type ctor-args] :as spec}]
                     (->> [[[:aload 0]
                            [:new type]
                            [:dup]
                            [:aload 0]]
                           (for [arg ctor-args]
                             [:ldc arg])
                           [[:invokespecial type :init
                             (vec (concat
                                   [jnr.ffi.Struct]
                                   (mapv class ctor-args)
                                   [:void]))]
                            [:putfield cls (clojure.core/name name) type]]]
                          (apply concat)
                          (vec)))
                   field-specs)
           [[:return]]]
          (apply concat)
          (vec))}]})

(defn resolve-struct-field-tag
  [tag]
  (case tag
    int8_t jnr.ffi.Struct$int8_t
    char jnr.ffi.Struct$Signed8
    uint8_t jnr.ffi.Struct$u_int8_t
    byte jnr.ffi.Struct$u_int8_t
    int16_t jnr.ffi.Struct$int16_t
    short jnr.ffi.Struct$Signed16
    uint16_t jnr.ffi.Struct$u_int16_t
    ushort jnr.ffi.Struct$Unsigned16
    int32_t jnr.ffi.Struct$int32_t
    int jnr.ffi.Struct$Signed32
    uint32_t jnr.ffi.Struct$u_int32_t
    uint jnr.ffi.Struct$Unsigned32
    long jnr.ffi.Struct$SignedLong
    ulong jnr.ffi.Struct$UnsignedLong
    int64_t jnr.ffi.Struct$int64_t
    uint64_t jnr.ffi.Struct$u_int64_t
    float jnr.ffi.Struct$Float
    double jnr.ffi.Struct$Double
    Pointer jnr.ffi.Struct$Pointer
    tag))

(defn resolve-struct-field-spec
  [[name]]
  (let [tag (resolve-struct-field-tag (:tag (meta name)))
        invalid #(throw (ex-info "invalid struct field type"
                                 {:name name :tag tag}))]
    (if (class? tag)
      {:name name
       :type tag
       :ctor-args []}
      (let [cls (resolve tag)]
        (when-not (class? cls)
          (invalid))
        (cond
          (.isEnum cls)
          {:name name
           :type jnr.ffi.Struct$Enum
           :ctor-args [cls]}
          :else (invalid))))))

(defmacro define
  {:style/indent 1}
  [struct-name & field-specs]
  (let [cls (qualified-class-name struct-name)
        cls-parts (str/split cls #"\.")
        ns-sym (symbol (str/join "." (butlast cls-parts)))
        name-sym (symbol (last cls-parts))
        field-specs (map resolve-struct-field-spec field-specs)
        t (build-struct cls field-specs)]
    `(do
       (insn/define ~t)
       (import '(~ns-sym ~name-sym)))))
