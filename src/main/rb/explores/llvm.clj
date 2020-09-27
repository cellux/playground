(ns rb.explores.llvm
  (:require [clojure.string :as str]
            [clojure.test :refer [is]])
  (:import [java.nio ByteOrder]))

(def i1 [:integer 1])
(def i8 [:integer 8])
(def i16 [:integer 16])
(def i32 [:integer 32])
(def i64 [:integer 64])

(defn complex-type?
  ([t]
   (and (vector? t) (keyword? (first t))))
  ([t tag]
   (and (complex-type? t) (= tag (first t)))))

(defn integer-type?
  [t]
  (complex-type? t :integer))

(defn pointer-type?
  [t]
  (complex-type? t :ptr))

(defn i8-type?
  [t]
  (and (integer-type? t)
       (= 8 (second t))))

(def float-types
  #{:half :bfloat :float :double :x86_fp80 :fp128 :pp-cfp128})

(defn float-type?
  [t]
  (contains? float-types t))

(defn platform-byte-order
  []
  (condp = (ByteOrder/nativeOrder)
    ByteOrder/BIG_ENDIAN :big-endian
    ByteOrder/LITTLE_ENDIAN :little-endian))

(defn platform-mangling-mode
  []
  :elf)

(defn platform-pointer-size
  []
  {:size 32 :abi 32})

(defn platform-float-sizes
  []
  [{:size 64 :abi 32 :pref 64}
   {:size 80 :abi 32}])

(defn platform-legal-int-widths
  []
  [8 16 32])

(defn platform-natural-stack-alignment
  []
  128)

(defn platform-data-layout
  []
  {:byte-order (platform-byte-order)
   :mangling-mode (platform-mangling-mode)
   :pointer-size (platform-pointer-size)
   :float-sizes (platform-float-sizes)
   :legal-int-widths (platform-legal-int-widths)
   :natural-stack-alignment (platform-natural-stack-alignment)})

(defn platform-target-triple
  []
  {:arch :i686
   :vendor :pc
   :os :linux
   :env :gnu})

(defmulti render-data-layout-item first)

(defmethod render-data-layout-item :byte-order
  [[_ byte-order]]
  (case byte-order
    :little-endian "e"
    :big-endian "E"))

(defmethod render-data-layout-item :mangling-mode
  [[_ mode]]
  (str "m:" (case mode
              :elf "e"
              :mips "m"
              :mach-o "o"
              :windows-x86-coff "x"
              :windows-coff "w"
              :xcoff "a")))

(defmethod render-data-layout-item :pointer-size
  [[_ {:keys [address-space size abi pref idx]}]]
  (format "p%d:%d:%d:%d:%d"
          (or address-space 0)
          size
          abi
          (or pref abi)
          (or idx size)))

(defmethod render-data-layout-item :float-sizes
  [[_ size-descriptors]]
  (->> (for [{:keys [size abi pref]} size-descriptors]
         (format "f%d:%d:%d" size abi (or pref abi)))
       (str/join "-")))

(defmethod render-data-layout-item :legal-int-widths
  [[_ widths]]
  (->> widths
       (str/join ":")
       (str "n")))

(defmethod render-data-layout-item :natural-stack-alignment
  [[_ alignment]]
  (format "S%d" alignment))

(defn render-data-layout
  [layout]
  (->> layout
       (map render-data-layout-item)
       (str/join "-")))

(defn render-target-triple
  [triple]
  (->> (if (contains? triple :env)
         [:arch :vendor :os :env]
         [:arch :vendor :os])
       (map triple)
       (map name)
       (str/join "-")))

(defn escape-byte-string
  [byte-string]
  (with-out-str
    (doseq [b byte-string]
      (cond
        (= b (byte \\))
        (print "\\\\")
        (and (<= 0x20 b 0x7e) (not= b (byte \")))
        (print (char b))
        :else
        (printf "\\%02X" b)))))

(defn identifier-part?
  [ch]
  (let [b (int ch)]
    (or (<= 0x61 b 0x7a)
        (<= 0x41 b 0x5a)
        (<= 0x30 b 0x39)
        (== 0x2d b)
        (== 0x2e b)
        (== 0x5f b))))

(defn digit?
  [ch]
  (<= 0x30 (int ch) 0x39))

(defn needs-quoting?
  [name]
  (or (digit? (first name))
      (not (every? identifier-part? name))))

(defn render-name-string-without-prefix
  [name-string]
  (if (needs-quoting? name-string)
    (format "\"%s\""
            (-> (.getBytes name-string "utf-8")
                escape-byte-string))
    name-string))

(defn render-name
  [x]
  (cond
    (integer? x) (str \% x)
    (double? x) (str \@ (int x))
    (keyword? x) (str \% (render-name-string-without-prefix (name x)))
    (symbol? x) (str \@ (render-name-string-without-prefix (name x)))))

(declare render-type)

(defmulti render-complex-type first)

(defmethod render-complex-type :integer
  [[_ size]]
  (format "i%d" size))

(defmethod render-complex-type :array
  [[_ elt size]]
  (format "[%d x %s]" size (render-type elt)))

(defmethod render-complex-type :vector
  [[_ elt size]]
  (format "<%d x %s>" size (render-type elt)))

(defmethod render-complex-type :ptr
  [[_ elt]]
  (format "%s*" (render-type elt)))

(defmethod render-complex-type :fn
  [[_ return-type param-types]]
  (format "%s (%s)"
          (render-type return-type)
          (->> param-types
               (map render-type)
               (str/join ", "))))

(defn render-struct-type
  [format-string [_ element-types]]
  (->> element-types
       (map render-type)
       (str/join ", ")
       (format format-string)))

(defmethod render-complex-type :struct
  [t]
  (render-struct-type "{ %s }" t))

(defmethod render-complex-type :packed-struct
  [t]
  (render-struct-type "<{ %s }>" t))

(defmethod render-complex-type :opaque-struct
  [t]
  "opaque")

(defmethod render-complex-type :named-struct
  [[_ name]]
  (render-name name))

(defn render-simple-type
  [t]
  (case t
    :void "void"
    :half "half"
    :bfloat "bfloat"
    :float "float"
    :double "double"
    :x86_fp80 "x86_fp80"
    :fp128 "fp128"
    :ppc-fp128 "ppc_fp128"
    :label "label"
    :md "metadata"
    :x86-mmx "x86_mmx"
    :token "token"
    :& "..."
    (throw (ex-info "invalid type" {:type t}))))

(defn render-type
  [t]
  (cond
    (vector? t) (render-complex-type t)
    (keyword? t) (render-simple-type t)
    :else (throw (ex-info "invalid type" {:type t}))))

(defn render-type-definition
  [id type]
  (str (render-name id) " = type " (render-type type)))

(defn render-comdat
  [comdat]
  (case (:selection-kind comdat)
    :any "any"
    :exact-match "exactmatch"
    :largest "largest"
    :no-duplicates "noduplicates"
    :same-size "samesize"))

(defn render-comdat-definition
  [id comdat]
  (str (render-name id) " = comdat " (render-comdat comdat)))

(defn render-string-byte
  [b]
  (if (<= 0x20 b 0x7e)
    (char b)
    (format "\\%02X" b)))

(defn render-string-constant
  [value]
  (->> value
       (map byte)
       (map render-string-byte)
       (apply str)
       (format "c\"%s\"")))

(defn render-address-space
  [address-space]
  (format "addrspace(%d)" address-space))

(defn render-linkage
  [linkage]
  (case linkage
    :private "private"
    :internal "internal"
    :available-externally "available_externally"
    :linkonce "linkonce"
    :weak "weak"
    :common "common"
    :appending "appending"
    :extern-weak "extern_weak"
    :linkonce-odr "linkonce_odr"
    :weak-odr "weak_odr"
    :external "external"))

(defn render-preemption-specifier
  [specifier]
  (case specifier
    :dso-preemptable "dso_preemptable"
    :dso-local "dso_local"))

(defn render-visibility-style
  [style]
  (case style
    :default "default"
    :hidden "hidden"
    :protected "protected"))

(defn render-dll-storage-class
  [class]
  (case class
    :dllimport "dllimport"
    :dllexport "dllexport"))

(defn render-tls-storage-model
  [model]
  (if (keyword? model)
    (format "thread_local(%s)"
            (case model
              :local-dynamic "localdynamic"
              :initial-exec "initialexec"
              :local-exec "localexec"))
    "thread_local"))

(defn render-unnamed-addr
  [value]
  (case value
    :global "unnamed_addr"
    :local "local_unnamed_addr"))

(defn render-externally-initialized
  [_]
  "externally_initialized")

(defn render-variable-qualifier
  [name value]
  (case name
    :address-space (render-address-space value)
    :linkage (render-linkage value)
    :preempt (render-preemption-specifier value)
    :visibility (render-visibility-style value)
    :dll-storage-class (render-dll-storage-class value)
    :thread-local (render-tls-storage-model value)
    :unnamed-addr (render-unnamed-addr value)
    :externally-initialized (render-externally-initialized value)))

(defn render-variable-qualifiers
  [v]
  (let [qualifiers (for [q [:address-space
                            :linkage
                            :preempt
                            :visibility
                            :dll-storage-class
                            :thread-local
                            :unnamed-addr
                            :externally-initialized]
                         :when (contains? v q)]
                     (render-variable-qualifier q (v q)))]
    (when (seq qualifiers)
      (str/join " " qualifiers))))

(defn render-variable
  [v constant?]
  (format
   "@%s = %s"
   (:name v)
   (->>
    [(render-variable-qualifiers v)
     (if constant? "constant" "global")
     (render-type (:type v))
     (when (contains? v :initializer)
       (render-constant (:initializer v)))
     (when (contains? v :section)
       (format ", section \"%s\"" (:section v)))
     (when (contains? v :comdat)
       (if (true? (:comdat v))
         ", comdat"
         (format ", comdat ($%s)" (:comdat v))))
     (when (contains? v :align)
       (format ", align %d" (:align v)))]
    (filter some?)
    (flatten)
    (str/join " "))))

(defn render-function
  [f]
  "")

(defn named-value?
  [value]
  (contains? :name value))

(defn constant-value?
  [value]
  (= (:kind value) :constant))

(defn global-value?
  [value]
  (:global value))

(defn integer-value?
  [value]
  (integer-type? (:type value)))

(defn float-value?
  [value]
  (float-type? (:type value)))

(defn null-value?
  [value]
  (and (pointer-type? (:type value))
       (nil? (:value value))))

(defn zero-initializer?
  [value]
  (= :zero-initializer (:type value)))

(defn block-address?
  [value]
  (= :block-address (:type value)))

(declare render-operand)

(defn render-struct-value
  [format-string [_ element-types] elements]
  (->> (map vector element-types elements)
       (map (fn [[elt value]]
              (format "%s %s"
                      (render-type elt)
                      (render-operand value))))
       (str/join ", ")
       (format format-string)))

(defn render-constant-value
  [c]
  (cond
    (integer-value? c)
    (let [[_ size] (:type c)]
      (if (= 1 size)
        (if (zero? (:value c)) "false" "true")
        (str (:value c))))

    (float-value? c)
    (str (:value c))

    (zero-initializer? c)
    "zeroinitializer"

    (null-value? c)
    "null"

    ;; TODO
    ;; constant token none
    ;; undef value

    (block-address? c)
    (let [[_ function basic-block] (:value c)]
      (format "blockaddress(%s, %s)"
              (render-operand function)
              (render-operand basic-block)))

    (vector? (:type c))
    (let [t (:type c)]
      (case (first t)
        :array (let [[_ elt size] t
                     elt-str (render-type elt)]
                 (if (i8-type? elt)
                   (render-string-constant (:value c))
                   (->> (:value c)
                        (map #(format "%s %s"
                                      elt-str
                                      (render-operand %)))
                        (str/join ", ")
                        (format "[%s]"))))
        :struct (render-struct-value "{ %s }" t (:value c))
        :packed-struct (render-struct-value "<{ %s }>" t (:value c))
        :vector (let [[_ elt size] t
                      elt-str (render-type elt)]
                  (->> (:value c)
                       (map #(format "%s %s"
                                     elt-str
                                     (render-operand %)))
                       (str/join ", ")
                       (format "<%s>")))))))

(defn render-operand
  [value]
  (cond
    (named-value? value)
    (render-name (:name value))

    (and (constant-value? value)
         (not (global-value? value)))
    (render-constant-value value)))

(defn render-global
  [g]
  (case (:kind g)
    :constant (render-variable g true)
    :variable (render-variable g false)
    :function (render-function g)))

(defn render-attribute
  [id attr]
  "")

(defn render-metadata
  [id md]
  "")

(defn render-module
  [{:keys [data-layout
           target-triple
           types
           comdats
           globals
           attributes
           metadata]}]
  (map #(str % "\n")
       [(render-data-layout (or data-layout
                                (platform-data-layout)))
        (render-target-triple (or target-triple
                                  (platform-target-triple)))
        (str/join "\n" (for [[id type] types]
                         (render-type-definition id type)))
        (str/join "\n" (for [[id comdat] comdats]
                         (render-comdat-definition id comdat)))
        (str/join "\n" (for [[name value] globals]
                         (render-global value)))
        (str/join "\n" (for [[id value] attributes]
                         (render-attribute id value)))
        (str/join "\n" (for [[id value] metadata]
                         (render-metadata id value)))]))

(defn str->byte-array
  [s]
  (into-array Byte/TYPE s))

(defn hello-module
  []
  {:data-layout
   {:byte-order :little-endian
    :mangling-mode :elf
    :pointer-size {:size 32 :abi 32}
    :float-sizes [{:size 64 :abi 32 :pref 64}
                  {:size 80 :abi 32}]
    :legal-int-widths [8 16 32]
    :natural-stack-alignment 128}

   :target-triple
   {:arch :i686
    :vendor :pc
    :os :linux
    :env :gnu}

   :globals
   {'.str
    {:name "@.str"
     :kind :constant
     :linkage :private
     :unnamed-addr :global
     :type [:array i8 15]
     :initializer {:type [:array i8 15]
                   :value (str->byte-array "Hello, world!\n\0")}
     :align 1}}

   :functions
   {'printf
    {:name "@printf"
     :kind :function
     :type [:fn i32 [[:ptr i8] :&]]
     :attrs 1}
    'main
    {:name "@main"
     :kind :function
     :preempt :dso-local
     :type [:fn i32 [i32 [:ptr [:ptr i8]]]]
     :params [:argc :argv]
     :attrs 0
     :body
     [{:name :entry
       :instructions
       [{:op :alloca
         :type i32
         :result :argc.addr
         :align 4}
        {:op :alloca
         :type [:ptr [:ptr i8]]
         :result :argv.addr
         :align 4}
        {:op :store
         :type i32
         :src :argc
         :dst :argc.addr
         :align 4}
        {:op :store
         :type [:ptr [:ptr i8]]
         :src :argv
         :dst :argv.addr
         :align 4}
        {:op :call
         :result :call
         :function {:name 'printf :type [:fn i32 [[:ptr i8] :&]]}
         :args [{:op :getelementptr
                 :inbounds true
                 :type [:array i8 15]
                 :base '.str
                 :index [[i32 0] [i32 0]]
                 }]
         }
        {:op :ret
         :type i32
         :value {:type i32 :value 0}}]}]}}

   :attributes
   {0 {:noinline true
       :nounwind true
       :optnone true
       :sspstrong true
       :correctly-rounded-divide-sqrt-fp-math false
       :disable-tail-calls false
       :frame-pointer :all
       :less-precise-fpmad false
       :min-legal-vector-width 0
       :no-infs-fp-math false
       :no-jump-tables false
       :no-nans-fp-math false
       :no-signed-zeros-fp-math false
       :no-trapping-math false
       :stack-protector-buffer-size 8
       :target-cpu :pentium4
       :target-features #{:cx8 :fxsr :mmx :sse :sse2 :x87}
       :unsafe-fp-math false
       :use-soft-float false}
    1 {:correctly-rounded-divide-sqrt-fp-math false
       :disable-tail-calls false
       :frame-pointer :all
       :less-precise-fpmad false
       :no-infs-fp-math false
       :no-nans-fp-math false
       :no-signed-zeros-fp-math false
       :no-trapping-math false
       :stack-protector-buffer-size 8
       :target-cpu :pentium4
       :target-features #{:cx8 :fxsr :mmx :sse :sse2 :x87}
       :unsafe-fp-math false
       :use-soft-float false}}

   :metadata
   {:llvm.module.flags [0 1 2 3]
    :llvm.ident [4]
    0 [[i32 1] "NumRegisterParameters" [i32 0]]
    1 [[i32 1] "wchar_size" [i32 4]]
    2 [[i32 7] "PIC Level" [i32 2]]
    3 [[i32 7] "PIE Level" [i32 2]]
    4 ["clang version 10.0.1 "]}})
