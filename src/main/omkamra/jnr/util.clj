(ns omkamra.jnr.util
  (:require
   [clojure.string :as str]
   [insn.core :as insn]
   [insn.util :refer [type-desc]]))

(defn qualified-name?
  [s]
  (<= 0 (.indexOf s (int \.))))

(defn qualified-class-name
  [class-name]
  (let [s (name class-name)]
    (if (qualified-name? s)
      s
      (str (munge (ns-name *ns*)) "." s))))

(defn build-enum-field
  [name type]
  {:name name
   :type type
   :flags #{:public :static :final :enum}})

(defn build-enum
  [cls field-names]
  {:name cls
   :flags #{:public :final :super :enum}
   :super Enum
   :fields
   (-> (mapv #(build-enum-field % cls) field-names)
       (conj {:name "value"
              :type :int
              :flags #{:private :final}})
       (conj {:name "$VALUES"
              :type [cls]
              :flags #{:private :static :final :synthetic}}))
   :methods
   [{:name :init
     :flags #{:private}
     :desc [String :int :int :void]
     :emit [[:aload 0]
            [:aload 1]
            [:iload 2]
            [:invokespecial Enum :init [String :int :void]]
            [:aload 0]
            [:iload 3]
            [:putfield cls "value" :int]
            [:return]]}
    {:name "values"
     :flags #{:public :static}
     :desc [[cls]]
     :emit [[:getstatic cls "$VALUES" [cls]]
            [:invokevirtual (type-desc [cls]) "clone" [Object]]
            [:checkcast [cls]]
            [:areturn]]}
    {:name "valueOf"
     :flags #{:public :static}
     :desc [String cls]
     :emit [[:ldc :this]
            [:aload 0]
            [:invokestatic Enum "valueOf"]
            [:checkcast cls]
            [:areturn]]}
    {:name "intValue"
     :flags #{:public}
     :desc [:int]
     :emit [[:aload 0]
            [:getfield cls "value" :int]
            [:ireturn]]}
    {:name :clinit
     :emit
     (->> [(mapcat (fn [name ordinal value]
                     (vector
                      [:new cls]
                      [:dup]
                      [:ldc name]
                      [:ldc ordinal]
                      [:ldc value]
                      [:invokespecial cls :init [String :int :int :void]]
                      [:putstatic cls name cls]))
                   field-names (range) (range))
           [[:ldc (count field-names)]
            [:anewarray cls]]
           (mapcat (fn [name ordinal]
                     (vector
                      [:dup]
                      [:ldc ordinal]
                      [:getstatic cls name cls]
                      [:aastore]))
                   field-names (range))
           [[:putstatic cls "$VALUES" [cls]]
            [:return]]]
          (apply concat)
          (vec))}]})

(defmacro define-enum
  {:style/indent 1}
  [enum-name & field-specs]
  (let [cls (qualified-class-name enum-name)
        cls-parts (str/split cls #"\.")
        ns-sym (symbol (str/join "." (butlast cls-parts)))
        name-sym (symbol (last cls-parts))
        t (build-enum cls (map name field-specs))]
    `(do
       (insn/define ~t)
       (import '(~ns-sym ~name-sym)))))

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

(defmacro define-struct
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

(defn resolve-class-tag
  [tag]
  (let [cls (resolve tag)]
    (if (class? cls)
      (symbol (.getName cls))
      tag)))

(defn resolve-tag
  ([x resolve]
   (if-let [m (meta x)]
     (if-let [tag (:tag m)]
      (vary-meta x update :tag resolve)
      x)
    x))
  ([x]
   (resolve-tag x resolve-class-tag)))

(defmacro define-library
  {:style/indent 1}
  [name libname & sigs]
  (let [iface-name (symbol (str "omkamra_jnr_interface_" name))]
    `(do
       (clojure.core/definterface ~iface-name
         ~@(map (fn [[method-name method-args]]
                  (list (resolve-tag method-name)
                        (mapv resolve-tag method-args)))
             sigs))
       (def ~name
         (-> (jnr.ffi.LibraryLoader/create ~iface-name)
             (.load ~libname))))))
