(ns omkamra.llvm.ir
  (:refer-clojure :exclude [and or load])
  (:require [clojure.core :as clj])
  (:require [midje.sweet :as m])
  (:require [clojure.string :as str])
  (:import (java.nio ByteOrder))
  (:import (java.util IdentityHashMap))
  (:import (com.kenai.jffi Platform Platform$OS Platform$CPU)))

(def i1 [:integer 1])
(def i8 [:integer 8])
(def i16 [:integer 16])
(def i32 [:integer 32])
(def i64 [:integer 64])

(def platform-byte-order
  (condp = (ByteOrder/nativeOrder)
    ByteOrder/BIG_ENDIAN :big-endian
    ByteOrder/LITTLE_ENDIAN :little-endian))

(def known-operating-systems
  {Platform$OS/DARWIN :darwin
   Platform$OS/FREEBSD :freebsd
   Platform$OS/NETBSD :netbsd
   Platform$OS/OPENBSD :openbsd
   Platform$OS/DRAGONFLY :dragonfly
   Platform$OS/LINUX :linux
   Platform$OS/SOLARIS :solaris
   Platform$OS/WINDOWS :windows
   Platform$OS/AIX :aix
   Platform$OS/ZLINUX :zos})

(def platform (Platform/getPlatform))

(def platform-os
  (let [os (.getOS platform)]
    (clj/or (known-operating-systems os)
            (throw (ex-info "unknown operating system" {:os os})))))

(def platform-object-format
  (case platform-os
    (:linux :solaris :freebsd :netbsd :openbsd :dragonfly) :elf
    :darwin :mach-o
    :windows :coff))

(def known-cpu-architectures
  {Platform$CPU/I386 :i386
   Platform$CPU/X86_64 :x86_64
   Platform$CPU/PPC :powerpc
   Platform$CPU/PPC64 :powerpc64
   Platform$CPU/PPC64LE :powerpc64le
   Platform$CPU/SPARC :sparc
   Platform$CPU/SPARCV9 :sparcv9
   Platform$CPU/S390X :s390x
   Platform$CPU/ARM :arm
   Platform$CPU/AARCH64 :aarch64})

(def platform-arch
  (let [cpu (.getCPU platform)]
    (clj/or (known-cpu-architectures cpu)
            (throw (ex-info "unknown CPU architecture" {:cpu cpu})))))

(def platform-mangling-mode
  (case platform-object-format
    :elf :elf
    :mach-o :mach-o
    :coff (case platform-arch
            :i386 :windows-x86-coff
            :x86_64 :windows-coff)))

(def platform-address-size
  (.addressSize platform))

(def platform-pointer-layout
  {:size platform-address-size :abi platform-address-size})

(def platform-integer-layout
  [{:size 64
    :abi (if (clj/or (= platform-address-size 64)
                     (= platform-os :windows))
           64 32)}])

(def platform-float-layout
  (-> []
      (conj {:size 64 :abi (if (clj/or (= platform-address-size 64)
                                       (= platform-os :windows))
                             64 32)})
      (conj {:size 80 :abi (if (clj/or (= platform-address-size 64)
                                       (= platform-os :darwin))
                             128 32)})))

(def platform-legal-int-widths
  (case platform-address-size
    32 [8 16 32]
    64 [8 16 32 64]))

(def platform-natural-stack-alignment
  (if (clj/and (= platform-address-size 32)
               (= platform-os :windows))
    32 128))

(def platform-data-layout
  {:byte-order platform-byte-order
   :mangling-mode platform-mangling-mode
   :pointer-layout platform-pointer-layout
   :integer-layout platform-integer-layout
   :float-layout platform-float-layout
   :legal-int-widths platform-legal-int-widths
   :natural-stack-alignment platform-natural-stack-alignment})

(def platform-target-triple
  {:arch platform-arch
   :vendor :unknown
   :os platform-os
   :env :unknown})

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

(defmethod render-data-layout-item :pointer-layout
  [[_ {:keys [address-space size abi pref idx]}]]
  (format "p%d:%d:%d:%d:%d"
          (clj/or address-space 0)
          size
          abi
          (clj/or pref abi)
          (clj/or idx size)))

(defmethod render-data-layout-item :integer-layout
  [[_ size-descriptors]]
  (->> (for [{:keys [size abi pref]} size-descriptors]
         (format "i%d:%d:%d" size abi (clj/or pref abi)))
       (str/join "-")))

(defmethod render-data-layout-item :float-layout
  [[_ size-descriptors]]
  (->> (for [{:keys [size abi pref]} size-descriptors]
         (format "f%d:%d:%d" size abi (clj/or pref abi)))
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
        (clj/and (<= 0x20 b 0x7e) (not= b (byte \")))
        (print (char b))
        :else
        (printf "\\%02X" b)))))

(defn digit?
  [ch]
  (<= 0x30 (int ch) 0x39))

(defn identifier-part?
  [ch]
  (let [b (int ch)]
    (clj/or (<= 0x61 b 0x7a)
            (<= 0x41 b 0x5a)
            (<= 0x30 b 0x39)
            (== 0x2d b)
            (== 0x2e b)
            (== 0x5f b))))

(defn needs-quoting?
  [name]
  (clj/or (digit? (first name))
          (not (every? identifier-part? name))))

(defn render-quoted-string
  [s]
  (str \"
       (-> (.getBytes s "utf-8")
           escape-byte-string)
       \"))

(defn render-name-string
  [name-string]
  (if (needs-quoting? name-string)
    (render-quoted-string name-string)
    name-string))

(defn render-name
  [x]
  (cond
    (integer? x) (str \% x)
    (keyword? x) (str \% (render-name-string (name x)))
    (symbol? x) (str \@ (render-name-string (name x)))))

(defn named-type
  [name type]
  {:kind :type
   :name name
   :type type})

(defn named-type?
  [t]
  (clj/and (map? t) (= :type (:kind t))))

(declare render-type)

(def simple-type? keyword?)

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
    :ppc_fp128 "ppc_fp128"
    :label "label"
    :md "metadata"
    :x86_mmx "x86_mmx"
    :token "token"
    :& "..."
    (throw (ex-info "invalid type" {:type t}))))

(def complex-type? vector?)

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

(defn- format-struct-type
  [format-string [_ element-types]]
  (->> element-types
       (map render-type)
       (str/join ", ")
       (format format-string)))

(defmethod render-complex-type :struct
  [t]
  (format-struct-type "{ %s }" t))

(defmethod render-complex-type :packed-struct
  [t]
  (format-struct-type "<{ %s }>" t))

(defmethod render-complex-type :opaque-struct
  [t]
  "opaque")

(defn render-type
  [t]
  (cond
    (simple-type? t) (render-simple-type t)
    (complex-type? t) (render-complex-type t)
    (named-type? t) (render-name (:name t))
    :else (throw (ex-info "invalid type" {:type t}))))

(defn extract-type-tag
  [t]
  (cond
    (simple-type? t) t
    (complex-type? t) (first t)
    (named-type? t) (recur (:type t))
    :else (throw (ex-info "invalid type" {:type t}))))

(m/facts
 (m/fact (extract-type-tag :float) => :float)
 (m/fact (extract-type-tag i8) => :integer)
 (m/fact (extract-type-tag [:packed-struct :t]) => :packed-struct)
 (m/fact (extract-type-tag :void) => :void)
 (m/fact (extract-type-tag (named-type 'T [:struct [i32 [:ptr i8]]]))
         => :struct))

(defmulti render-literal (fn [type value] (extract-type-tag type)))

(defmethod render-literal :void
  [_ value]
  "void")

(defmethod render-literal :integer
  [[_ size] value]
  (if (clj/and (= size 1) (boolean? value))
    (str value)
    (format "%d" value)))

(defmethod render-literal :ptr
  [_ value]
  (if (nil? value) "null"
      (throw (ex-info "invalid pointer literal" {:value value}))))

(declare render-typed-value)

(defmethod render-literal :array
  [[_ elt size] value]
  (cond
    (clj/and (= elt i8) (string? value))
    (str \c (render-quoted-string value))

    (vector? value)
    (format "[ %s ]" (str/join ", " (map render-typed-value value)))))

(m/facts
 (m/fact (render-literal :void :void) => "void")
 (m/fact (render-literal i32 1234) => "1234")
 (m/fact (render-literal i1 true) => "true")
 (m/fact (render-literal i1 false) => "false")
 (m/fact (render-literal i1 0) => "0")
 (m/fact (render-literal i1 1) => "1")
 (m/fact (render-literal [:ptr i8] nil) => "null")
 (m/fact (render-literal [:array i8] "Hello, world\n\0") => "c\"Hello, world\\0A\\00\""))

(def
  ^{:private true
    :dynamic true
    :doc "Integer names for unnamed values like function parameters,
     basic blocks or instructions."}
  *names-of-the-unnamed* {})

(defn find-name-of
  [x]
  (clj/or (:name x) (get *names-of-the-unnamed* x)))

(defn render-typed-value
  [{:keys [type value] :as obj}]
  (let [name (find-name-of obj)]
    (cond
      name
      (format "%s %s" (render-type type) (render-name name))

      (= :void type)
      "void"

      :else
      (format "%s %s" (render-type type) (render-literal type value)))))

(defn render-value
  [{:keys [type value] :as obj}]
  (let [name (find-name-of obj)]
    (if name
      (render-name name)
      (render-literal type value))))

(defn const
  [type value]
  {:kind :const
   :type type
   :value value})

(m/facts
 (m/fact
  (render-typed-value (const i64 1234)) => "i64 1234")
 (m/fact
  (render-value (const i64 1234)) => "1234"))

(def void {:type :void :value :void})

(defmulti render-instruction :op)

(defn ret
  ([value]
   {:kind :instruction
    :op :ret
    :value value})
  ([]
   {:kind :instruction
    :op :ret}))

(defmethod render-instruction :ret
  [{:keys [value]}]
  (if value
    (format "ret %s" (render-typed-value value))
    "ret void"))

(m/facts
 (m/fact
  (render-instruction
   (ret (const i32 0))) => "ret i32 0")
 (m/fact
  (render-instruction
   (ret void)) => "ret void")
 (m/fact
  (render-instruction
   (ret)) => "ret void"))

(defn br
  ([dest]
   {:kind :instruction
    :op :br
    :dest dest})
  ([cond dest else]
   {:kind :instruction
    :op :br
    :cond cond
    :dest dest
    :else else}))

(defmethod render-instruction :br
  [{:keys [cond dest else]}]
  (if cond
    (format "br %s, %s, %s"
            (render-typed-value cond)
            (render-typed-value dest)
            (render-typed-value else))
    (format "br %s" (render-typed-value dest))))

(m/facts
 (m/fact
  (render-instruction
   (br {:type :label :name :for.cond}))
  => "br label %for.cond")
 (m/fact
  (render-instruction
   (br {:type i1 :name :cmp}
       {:type :label :name :for.body}
       {:type :label :name :for.end8}))
  => "br i1 %cmp, label %for.body, label %for.end8"))

(defn switch
  [value dest cases]
  {:kind :instruction
   :op :switch
   :value value
   :dest dest
   :cases cases})

(defmethod render-instruction :switch
  [{:keys [value dest cases]}]
  (format "switch %s, %s [ %s ]"
          (render-typed-value value)
          (render-typed-value dest)
          (->> cases
               (map (fn [[value dest]]
                      (format "%s, %s"
                              (render-typed-value value)
                              (render-typed-value dest))))
               (str/join " "))))

(m/facts
 (m/fact
  (render-instruction
   (switch {:type i32 :name 2}
           {:type :label :name :sw.default}
           [[(const i32 1)
             {:type :label :name :sw.bb}]
            [(const i32 2)
             {:type :label :name :sw.bb1}]
            [(const i32 3)
             {:type :label :name :sw.bb3}]]))
  => "switch i32 %2, label %sw.default [ i32 1, label %sw.bb i32 2, label %sw.bb1 i32 3, label %sw.bb3 ]"))

(defmacro define-unary-op
  [op opts]
  `(do
     (defn ~op
       [~'value ~'opts]
       (assoc ~'opts
              :kind :instruction
              :op ~(keyword op)
              :value ~'value
              :type (:type ~'value)))
     (defmethod render-instruction ~(keyword op)
       [{:keys [~'value ~@opts] :as ~'i}]
       (let [~'name (find-name-of ~'i)]
         (with-out-str
           (print (render-name ~'name))
           (print (str " = " ~(name op)))
           ~@(for [opt opts] `(when ~opt
                                (print ~(str " " (name opt)))))
           (print (str " " (render-typed-value ~'value))))))))

(define-unary-op fneg [])

(defmacro define-binary-op
  [op opts]
  `(do
     (defn ~op
       [~'lhs ~'rhs ~'opts]
       (assoc ~'opts
              :kind :instruction
              :op ~(keyword op)
              :lhs ~'lhs
              :rhs ~'rhs
              :type (:type ~'lhs)))
     (defmethod render-instruction ~(keyword op)
       [{:keys [~'lhs ~'rhs ~@opts] :as ~'i}]
       (let [~'name (find-name-of ~'i)]
         (with-out-str
           (print (render-name ~'name))
           (print (str " = " ~(name op)))
           ~@(for [opt opts] `(when ~opt
                                (print ~(str " " (name opt)))))
           (print (str " " (render-typed-value ~'lhs)))
           (print (str ", " (render-value ~'rhs))))))))

(define-binary-op add [nsw nuw])

(m/facts
 (m/fact
  (render-instruction
   (add {:type i32 :name 6}
        (const i32 1)
        {:nsw true
         :name :inc}))
  => "%inc = add nsw i32 %6, 1")
 (m/fact
  (render-instruction
   (add {:type i32 :name 1}
        {:type i32 :name 0}
        {:nsw true
         :name :add}))
  => "%add = add nsw i32 %1, %0"))

(define-binary-op fadd [])

(define-binary-op sub [nsw nuw])

(m/facts
 (m/fact
  (render-instruction
   (sub {:type i32 :name 0}
        (const i32 1)
        {:nsw true
         :name :sub}))
  => "%sub = sub nsw i32 %0, 1"))

(define-binary-op fsub [])

(define-binary-op mul [nsw nuw])

(m/facts
 (render-instruction
  (mul {:type i32 :name 2}
       {:type i32 :name 3}
       {:nsw true
        :name :mul}))
 => "%mul = mul nsw i32 %2, %3")

(define-binary-op fmul [])

(define-binary-op udiv [exact])

(define-binary-op sdiv [exact])

(m/facts
 (m/fact
  (render-instruction
   (sdiv {:type i32 :name 6}
         {:type i32 :name 7}
         {:name :div}))
  => "%div = sdiv i32 %6, %7"))

(define-binary-op fdiv [])

(define-binary-op urem [])

(define-binary-op srem [])

(m/facts
 (m/fact
  (render-instruction
   (srem {:type i32 :name 8}
         {:type i32 :name 9}
         {:name :rem}))
  => "%rem = srem i32 %8, %9"))

(define-binary-op frem [])

(define-binary-op shl [nsw nuw])

(m/facts
 (m/fact
  (render-instruction
   (shl {:type i32 :name 21}
        (const i32 3)
        {:name :shl}))
  => "%shl = shl i32 %21, 3"))

(define-binary-op lshr [exact])

(define-binary-op ashr [exact])

(m/facts
 (m/fact
  (render-instruction
   (ashr {:type i32 :name 22}
         (const i32 3)
         {:name :shr}))
  => "%shr = ashr i32 %22, 3"))

(define-binary-op and [])

(m/facts
 (m/fact
  (render-instruction
   (and {:type i32 :name 14}
        {:type i32 :name 15}
        {:name :and}))
  => "%and = and i32 %14, %15"))

(define-binary-op or [])

(m/facts
 (m/fact
  (render-instruction
   (or {:type i32 :name 16}
       {:type i32 :name 17}
       {:name :or}))
  => "%or = or i32 %16, %17"))

(define-binary-op xor [])

(m/facts
 (m/fact
  (render-instruction
   (xor {:type i32 :name 18}
        {:type i32 :name 19}
        {:name :xor}))
  => "%xor = xor i32 %18, %19")
 (m/fact
  (render-instruction
   (xor {:type i32 :name 20}
        (const i32 -1)
        {:name :neg}))
  => "%neg = xor i32 %20, -1")
 (m/fact
  (render-instruction
   (xor {:type i1 :name :tobool6}
        (const i1 true)
        {:name :lnot}))
  => "%lnot = xor i1 %tobool6, true"))

(defn alloca
  [object-type opts]
  (assoc opts
         :kind :instruction
         :op :alloca
         :object-type object-type
         :type [:ptr object-type]))

(defmethod render-instruction :alloca
  [{:keys [object-type address-space array-size align] :as i}]
  (let [name (find-name-of i)]
    (format "%s = alloca %s, align %d"
            (render-name name)
            (render-type object-type)
            align)))

(m/facts
 (m/fact
  (render-instruction
   (alloca i32 {:align 4 :name :retval}))
  => "%retval = alloca i32, align 4")
 (m/fact
  (render-instruction
   (alloca [:ptr [:ptr i8]] {:align 8 :name :argv.addr}))
  => "%argv.addr = alloca i8**, align 8")
 (m/fact
  (render-instruction
   (alloca [:array [:array i32 3] 4] {:align 16 :name :matrix}))
  => "%matrix = alloca [4 x [3 x i32]], align 16")
 (m/fact
  (render-instruction
   (alloca :float {:align 4 :name :f}))
  => "%f = alloca float, align 4")
 (m/fact
  (render-instruction
   (alloca :double {:align 8 :name :d}))
  => "%d = alloca double, align 8")
 (m/fact
  (render-instruction
   (alloca :x86_fp80 {:align 16 :name :ld}))
  => "%ld = alloca x86_fp80, align 16")
 (m/fact
  (render-instruction
   (let [ftype [:fn i32 [i32]]]
     (alloca [:ptr ftype] {:align 8 :name :ifn1})))
  => "%ifn1 = alloca i32 (i32)*, align 8"))

(defn load
  [target opts]
  (assoc opts
         :kind :instruction
         :op :load
         :object-type (:object-type target)
         :ptr target
         :type (:type target)))

(defmethod render-instruction :load
  [{:keys [object-type ptr align] :as i}]
  (let [name (find-name-of i)]
    (format "%s = load %s, %s, align %d"
            (render-name name)
            (render-type object-type)
            (render-typed-value ptr)
            align)))

(m/facts
 (m/fact
  (render-instruction
   (load {:object-type i32
          :type [:ptr i32]
          :name :i}
         {:align 4 :name 0}))
  => "%0 = load i32, i32* %i, align 4")
 (m/fact
  (render-instruction
   (load {:object-type i32
          :type [:ptr i32]
          :name 'sum}
         {:align 4 :name 1}))
  => "%1 = load i32, i32* @sum, align 4"))

(defn store
  [value target opts]
  (assoc opts
         :kind :instruction
         :op :store
         :value value
         :ptr target))

(defmethod render-instruction :store
  [{:keys [value ptr align]}]
  (format "store %s, %s, align %d"
          (render-typed-value value)
          (render-typed-value ptr)
          align))

(m/facts
 (m/fact
  (render-instruction
   (store (const i32 0)
          {:type [:ptr i32]
           :object-type i32
           :name :retval}
          {:align 4}))
  => "store i32 0, i32* %retval, align 4")
 (m/fact
  (render-instruction
   (store (const i8 -56)
          {:type [:ptr i8]
           :object-type :i8
           :name :c3}
          {:align 1}))
  => "store i8 -56, i8* %c3, align 1")
 (m/fact
  (render-instruction
   (store {:type i32 :name :argc}
          {:type [:ptr i32]
           :object-type i32
           :name :argc.addr}
          {:align 4}))
  => "store i32 %argc, i32* %argc.addr, align 4")
 (m/fact
  (render-instruction
   (store {:type [:ptr [:ptr i8]] :name :argv}
          {:type [:ptr [:ptr [:ptr i8]]]
           :object-type [:ptr [:ptr i8]]
           :name :argv.addr}
          {:align 8}))
  => "store i8** %argv, i8*** %argv.addr, align 8")
 (m/fact
  (render-instruction
   (store (const [:ptr i8] nil)
          {:type [:ptr [:ptr i8]]
           :object-type [:ptr i8]
           :name :ptr}
          {:align 8}))
  => "store i8* null, i8** %ptr, align 8"))

(defmacro define-conversion-op
  [op]
  `(do
     (defn ~op
       [~'value ~'dest-type ~'opts]
       (assoc ~'opts
              :kind :instruction
              :op ~(keyword op)
              :value ~'value
              :dest-type ~'dest-type
              :type ~'dest-type))
     (defmethod render-instruction ~(keyword op)
       [{:keys [~'value ~'dest-type] :as ~'i}]
       (let [~'name (find-name-of ~'i)]
         (format ~(str "%s = " op " %s to %s")
                 (render-name ~'name)
                 (render-typed-value ~'value)
                 (render-type ~'dest-type))))))

(define-conversion-op trunc)

(define-conversion-op zext)

(m/facts
 (m/fact
  (render-instruction
   (zext {:type i8 :name 2} i32 {:name :conv2}))
  => "%conv2 = zext i8 %2 to i32"))

(define-conversion-op sext)

(m/facts
 (m/fact
  (render-instruction
   (sext {:type i32 :name 4} i64 {:name :idxprom}))
  => "%idxprom = sext i32 %4 to i64"))

(define-conversion-op fptrunc)

(define-conversion-op fpext)

(m/facts
 (m/fact
  (render-instruction
   (fpext {:type :float :name 16} :double {:name :conv11}))
  => "%conv11 = fpext float %16 to double"))

(define-conversion-op fptoui)

(define-conversion-op fptosi)

(m/facts
 (m/fact
  (render-instruction
   (fptosi {:type :float :name 3} i32 {:name :conv}))
  => "%conv = fptosi float %3 to i32"))

(define-conversion-op uitofp)
(define-conversion-op sitofp)

(define-conversion-op ptrtoint)
(define-conversion-op inttoptr)

(define-conversion-op bitcast)

(m/facts
 (m/fact
  (render-instruction
   (let [union-type (named-type :union.u [:struct [[:ptr i8]]])]
     (bitcast {:type [:ptr union-type] :name :tmp}
              [:ptr i8]
              {:name :c})))
  => "%c = bitcast %union.u* %tmp to i8*"))

(define-conversion-op addrspacecast)

(defn render-predicate
  [pred]
  (name pred))

(defn icmp
  [pred lhs rhs opts]
  (assoc opts
         :kind :instruction
         :op :icmp
         :pred pred
         :lhs lhs
         :rhs rhs
         :type i1))

(defmethod render-instruction :icmp
  [{:keys [pred lhs rhs] :as i}]
  (let [name (find-name-of i)]
    (format "%s = icmp %s %s, %s"
            (render-name name)
            (render-predicate pred)
            (render-typed-value lhs)
            (render-value rhs))))

(m/facts
 (m/fact
  (render-instruction
   (icmp :slt {:type i32 :name 0} (const i32 4) {:name :cmp}))
  => "%cmp = icmp slt i32 %0, 4")
 (m/fact
  (render-instruction
   (icmp :eq {:type i32 :name 1} (const i32 5) {:name :cmp1}))
  => "%cmp1 = icmp eq i32 %1, 5")
 (m/fact
  (render-instruction
   (icmp :ne {:type i32 :name 23} (const i32 0) {:name :tobool}))
  => "%tobool = icmp ne i32 %23, 0"))

(defn fcmp
  [pred lhs rhs opts]
  (assoc opts
         :kind :instruction
         :op :fcmp
         :pred pred
         :lhs lhs
         :rhs rhs
         :type i1))

(defmethod render-instruction :fcmp
  [{:keys [pred lhs rhs] :as i}]
  (let [name (find-name-of i)]
    (format "%s = fcmp %s %s, %s"
            (render-name name)
            (render-predicate pred)
            (render-typed-value lhs)
            (render-value rhs))))

(defn phi
  [values opts]
  (assoc opts
         :kind :instruction
         :op :phi
         :type (let [[label value] (first values)]
                 (:type value))
         :values values))

(defmethod render-instruction :phi
  [{:keys [type values] :as i}]
  (let [name (find-name-of i)]
    (format "%s = phi %s %s"
            (render-name name)
            (render-type type)
            (->> values
                 (map #(format "[ %s, %s ]"
                               (render-value (second %))
                               (render-value (first %))))
                 (str/join ", ")))))

(m/facts
 (m/fact
  (render-instruction
   (phi {{:type :label :name :entry}
         (const i1 false)
         {:type :label :name :land.rhs}
         {:type i1 :name :tobool3}}
        {:name 25}))
  => "%25 = phi i1 [ false, %entry ], [ %tobool3, %land.rhs ]")
 (m/fact
  (render-instruction
   (phi {{:type :label :name :land.end}
         {:type i1 :value true}
         {:type :label :name :lor.rhs}
         {:type i1 :name :tobool5}}
        {:name 28}))
  => "%28 = phi i1 [ true, %land.end ], [ %tobool5, %lor.rhs ]"))

(defn select
  [cond then else opts]
  (assoc opts
         :kind :instruction
         :op :select
         :cond cond
         :then then
         :else else
         :type (:type then)))

(defmethod render-instruction :select
  [{:keys [cond then else] :as i}]
  (let [name (find-name-of i)]
    (format "%s = select %s, %s, %s"
            (render-name name)
            (render-typed-value cond)
            (render-typed-value then)
            (render-typed-value else))))

(m/facts
 (m/fact
  (render-instruction
   (select {:type i1 :name :cmp17}
           (const i32 1)
           (const i32 0)
           {:name :cond}))
  => "%cond = select i1 %cmp17, i32 1, i32 0"))

(defn result-type
  [f]
  (let [[_ result-type param-types] (:type f)]
    result-type))

(defn vararg?
  [f]
  (let [[_ result-type param-types] (:type f)]
    (= :& (last param-types))))

(defn call
  ([callee args opts]
   (assoc opts
          :kind :instruction
          :op :call
          :callee callee
          :args args
          :type (result-type callee)))
  ([callee args]
   (call callee args nil)))

(defmethod render-instruction :call
  [{:keys [callee args] :as i}]
  (let [name (find-name-of i)
        type-str (render-type (if (vararg? callee)
                                (:type callee)
                                (result-type callee)))
        callee-name (render-name (find-name-of callee))]
    (if name
      (format "%s = call %s %s(%s)"
              (render-name name)
              type-str
              callee-name
              (str/join ", " (map render-typed-value args)))
      (format "call %s %s(%s)"
              type-str
              callee-name
              (str/join ", " (map render-typed-value args))))))

(m/facts
 (m/fact
  (render-instruction
   (call {:type [:fn :void [i32]] :name 'add}
         [{:type i32 :name 0}]))
  => "call void @add(i32 %0)")
 (m/fact
  (render-instruction
   (call {:type [:fn i32 [i32]] :name 1}
         [(const i32 0)]
         {:name :call}))
  => "%call = call i32 %1(i32 0)"))

(defn sanitize-gep-index
  [index]
  (cond (integer? index) (const i32 index)
        (map? index) index
        :else (throw (ex-info "invalid gep index" {:index index}))))

(defn infer-element-type
  [type indices]
  (if (seq indices)
    (case (extract-type-tag type)
      :ptr
      (let [[_ elt] type]
        (recur elt (rest indices)))
      
      (:array :vector)
      (let [[_ elt size] type]
        (recur elt (rest indices)))

      (:struct :packed-struct)
      (let [[_ element-types] type]
        (recur (nth element-types (:value (first indices)))
               (rest indices))))
    type))

(m/facts
 (m/fact
  (infer-element-type [:ptr [:array [:struct [i32 i8 i16]] 10]]
                      [{:type i32 :value 0}
                       {:type i32 :name :index}
                       {:type i32 :value 1}])
  => i8))

(defn getelementptr
  [target indices opts]
  (let [target-type (:type target)]
    (assert (vector? target-type))
    (assert (= :ptr (first target-type)))
    (let [[_ base-type] target-type
          indices (map sanitize-gep-index indices)]
      (assoc opts
             :kind :instruction
             :op :getelementptr
             :base-type base-type
             :ptr target
             :indices indices
             :type [:ptr (infer-element-type (:type target) indices)]))))

(defmethod render-instruction :getelementptr
  [{:keys [base-type ptr indices inbounds] :as i}]
  (let [name (find-name-of i)]
    (format "%s = getelementptr%s %s, %s, %s"
            (render-name name)
            (if inbounds " inbounds" "")
            (render-type base-type)
            (render-typed-value ptr)
            (str/join ", " (map render-typed-value indices)))))

(m/facts
 (m/fact
  (render-instruction
   (let [matrix (alloca [:array [:array i32 3] 4]
                        {:align 8 :name :matrix})]
     (getelementptr matrix
                    [{:type i64 :value 0}
                     {:type i64 :name :idxprom}]
                    {:inbounds true
                     :name :arrayidx})))
  => "%arrayidx = getelementptr inbounds [4 x [3 x i32]], [4 x [3 x i32]]* %matrix, i64 0, i64 %idxprom"))

(defn basic-block
  ([name]
   {:kind :basic-block
    :name name
    :type :label
    :instructions []})
  ([]
   (basic-block nil)))

(defn add-i
  [block instruction]
  (update block :instructions conj instruction))

(defn render-basic-block
  [{:keys [instructions] :as block}]
  (let [name (find-name-of block)]
    (with-out-str
      (if (integer? name)
        (printf "%d:\n" name)
        (printf "%s:\n" (clj/name name)))
      (doseq [i instructions]
        (printf "  %s\n" (render-instruction i))))))

(m/facts
 (m/fact
  (render-basic-block
   (-> (basic-block :entry)
       (add-i (alloca i32 {:align 4 :name :retval}))
       (add-i (ret (const i32 0)))))
  => "entry:
  %retval = alloca i32, align 4
  ret i32 0
"))

(def known-linkages
  {:external "external"
   :private "private"
   :internal "internal"
   :linkonce "linkonce"
   :linkonce-odr "linkonce_odr"
   :weak "weak"
   :weak-odr "weak_odr"
   :common "common"
   :appending "appending"
   :extern-weak "extern_weak"
   :available-externally "available_externally"})

(defn render-linkage
  [linkage]
  (clj/or (known-linkages linkage)
          (throw (ex-info "invalid linkage" {:linkage linkage}))))

(defn render-visibility
  [visibility]
  (case visibility
    :hidden "hidden"
    :protected "protected"))

(defn render-dll-storage-class
  [dll-storage-class]
  (case dll-storage-class
    :dllimport "dllimport"
    :dllexport "dllexport"))

(defn render-calling-convention
  [cconv]
  (throw (UnsupportedOperationException.)))

(defn render-attribute
  [[key value]]
  (cond
    (keyword? key) (name key)
    (string? key) (format "\"%s\"=\"%s\"" key value)
    :else (throw (ex-info "cannot render attribute" {:key key :value value}))))

(defn attribute-group
  [attrs]
  attrs)

(defn render-attributes
  [attrs]
  (if (integer? attrs)
    (format "#%d" attrs)
    (str/join " " (map render-attribute attrs))))

(defn param
  ([name type attrs]
   {:kind :function-parameter
    :name name
    :type type
    :attrs attrs})
  ([name type]
   (param name type nil)))

(defn render-function-parameter
  [{:keys [type attrs] :as param}]
  (let [name (find-name-of param)]
    (with-out-str
      (printf "%s" (render-type type))
      (when attrs
        (printf " %s" (render-attributes attrs)))
      (when name
        (printf " %s" (render-name name))))))

(m/facts
 (m/fact
  (render-function-parameter
   (param :argc i32)) => "i32 %argc")
 (m/fact
  (render-function-parameter
   (param 5 i32)) => "i32 %5")
 (m/fact
  (render-function-parameter
   (param :argv [:ptr [:ptr i8]])) => "i8** %argv")
 (m/fact
  (render-function-parameter
   (param nil [:ptr [:ptr i8]])) => "i8**"))

(defn render-unnamed-addr
  [unnamed-addr]
  (case unnamed-addr
    :local "local_unnamed_addr"
    :global "unnamed_addr"))

(defn render-address-space
  [address-space]
  (format "addrspace(%d)" address-space))

(defn render-section
  [section]
  (throw (UnsupportedOperationException.)))

(defn render-partition
  [partition]
  (throw (UnsupportedOperationException.)))

(defn render-comdat
  [comdat]
  (throw (UnsupportedOperationException.)))

(defn global
  [name type opts]
  (assoc opts
         :kind :global
         :name name
         :object-type type
         :type [:ptr type]))

(defn render-global
  [{:keys [name object-type linkage
           dso-local visibility dll-storage-class
           thread-local-model unnamed-addr address-space
           externally-initialized constant initializer
           section partition comdat align metadata attrs]}]
  (with-out-str
    (printf "%s = " (render-name name))
    (when linkage
      (printf "%s " (render-linkage linkage)))
    (when dso-local
      (printf "dso_local "))
    (when visibility
      (printf "%s " (render-visibility visibility)))
    (when dll-storage-class
      (printf "%s " (render-dll-storage-class dll-storage-class)))
    (when thread-local-model
      (print (case thread-local-model
               :general-dynamic "thread_local "
               :local-dynamic "thread_local(localdynamic) "
               :initial-exec "thread_local(initialexec) "
               :local-exec "thread_local(localexec) ")))
    (when unnamed-addr
      (printf "%s " (render-unnamed-addr unnamed-addr)))
    (when address-space
      (printf "%s " (render-address-space address-space)))
    (when externally-initialized
      (print "externally_initialized "))
    (print (if constant "constant " "global "))
    (printf "%s" (render-type object-type))
    (when initializer
      (printf " %s" (render-value initializer)))
    (when section
      (printf ", %s" (render-section section)))
    (when partition
      (printf ", %s" (render-partition partition)))
    (when comdat
      (printf " %s" (render-comdat comdat)))
    (when align
      (printf ", align %d" align))
    (when attrs
      (printf "%s " (render-attributes attrs)))))

(m/facts
 (m/fact
  (render-global
   (global 'add.z i32
           {:initializer (const i32 8)
            :linkage :internal
            :align 4}))
  => "@add.z = internal global i32 8, align 4")
 (m/fact
  (render-global
   (global '.str [:array i8 15]
           {:linkage :private
            :unnamed-addr :global
            :constant true
            :initializer (const [:array i8] "Hello, world!\n\0")
            :align 1}))
  => "@.str = private unnamed_addr constant [15 x i8] c\"Hello, world!\\0A\\00\", align 1"))

(defn sanitize-param
  [param]
  (cond
    (map? param) param
    (clj/or (simple-type? param)
            (complex-type? param)) {:type param}
    :else (throw (ex-info "invalid function parameter" {:param param}))))

(defn function
  ([name result-type params opts]
   (let [params (map sanitize-param params)]
     (assoc opts
            :kind :function
            :name name
            :result-type result-type
            :type [:fn result-type (map :type params)]
            :params params
            :blocks [])))
  ([name result-type params]
   (function name result-type params nil)))

(defn add-bb
  [f bb]
  (update f :blocks conj bb))

(def
  ^{:private true}
  ops-with-no-name
  #{:ret :br :switch :indirectbr :resume
    :catchret :cleanupret :unreachable
    :store :fence})

(defn needs-generated-name?
  [obj]
  (case (:kind obj)
    :function-parameter (not (:name obj))
    :basic-block (not (:name obj))
    :instruction (not (clj/or (:name obj)
                              (ops-with-no-name (:op obj))
                              (clj/and (= (:op obj) :call)
                                       (not= (:type obj) :void))))))

(defn render-function
  [{:keys [name linkage dso-local visibility dll-storage-class
           cconv result-attrs result-type name params
           unnamed-addr address-space function-attrs
           section comdat align gc prefix prologue personality
           metadata blocks] :as f}]
  (let [definition? (if (seq blocks) true false)
        next-name (let [counter (atom 0)]
                    (fn []
                      (let [name @counter]
                        (swap! counter inc)
                        name)))]
    (binding [*names-of-the-unnamed*
              (if definition?
                (reduce
                 (fn [names obj]
                   (when (needs-generated-name? obj)
                     (.put names obj (next-name)))
                   names)
                 (IdentityHashMap.)
                 (concat
                  params
                  (mapcat #(cons % (:instructions %)) blocks)))
                {})]
      (with-out-str
        (print (if definition? "define " "declare "))
        (when linkage
          (printf "%s " (render-linkage linkage)))
        (when dso-local
          (printf "dso_local "))
        (when visibility
          (printf "%s " (render-visibility visibility)))
        (when dll-storage-class
          (printf "%s " (render-dll-storage-class dll-storage-class)))
        (when cconv
          (printf "%s " (render-calling-convention cconv)))
        (when result-attrs
          (printf "%s " (render-attributes result-attrs)))
        (printf "%s " (render-type result-type))
        (printf "%s(" (render-name name))
        (print
         (->> params
              (map render-function-parameter)
              (str/join ", ")))
        (print ")")
        (when unnamed-addr
          (printf " %s" (render-unnamed-addr unnamed-addr)))
        (when address-space
          (printf " %s" (render-address-space address-space)))
        (when function-attrs
          (printf " %s" (render-attributes function-attrs)))
        (when section
          (printf " %s" (render-section section)))
        (when comdat
          (printf " %s" (render-comdat comdat)))
        (when align
          (printf " align %d" align))
        (when definition?
          (print " {\n")
          (doseq [block blocks]
            (print (render-basic-block block)))
          (print "}\n"))))))

(m/facts
 (m/fact
  (render-function
   (let [bb (-> (basic-block :entry)
                (add-i (alloca i32 {:align 4 :name :retval}))
                (add-i (ret (const i32 0))))
         f (function 'main
                     i32
                     [(param :argc i32)
                      (param :argv [:ptr [:ptr i8]])]
                     {:dso-local true
                      :function-attrs 0})]
     (add-bb f bb)))
  => "define dso_local i32 @main(i32 %argc, i8** %argv) #0 {
entry:
  %retval = alloca i32, align 4
  ret i32 0
}
")
 (m/fact
  (render-function
   (function 'printf i32 [[:ptr i8] :&] {:function-attrs 1}))
  => "declare i32 @printf(i8*, ...) #1"))

(defn module
  []
  {:data-layout platform-data-layout
   :target-triple platform-target-triple
   :types {}
   :globals {}
   :functions {}
   :attribute-groups {}})

(defn add-fn
  [m f]
  (update m :functions assoc (:name f) f))

(defn render-module
  [{:keys [data-layout
           target-triple
           types
           globals
           functions
           attribute-groups]}]
  (with-out-str
    (printf "target datalayout = %s\n" (render-data-layout data-layout))
    (printf "target triple = %s\n" (render-target-triple target-triple))
    (doseq [[k v] types]
      (printf "%s = type %s\n"
              (render-name k)
              (render-type v)))
    (doseq [[k v] globals]
      (println (render-global v)))
    (doseq [[k v] functions]
      (println (render-function v)))
    (doseq [[k v] attribute-groups]
      (printf "attributes #%d = { %s }\n"
              k
              (str/join " " (map render-attribute v))))))

(m/facts
 (m/fact
  (let [str (global '.str [:array i8 15]
                    {:initializer "Hello, world\n\0"
                     :align 1})
        argc (param :argc i32)
        argv (param :argv [:ptr [:ptr i8]])
        retval (alloca i32 {:align 4})
        argc-addr (alloca i32 {:align 4})
        argv-addr (alloca [:ptr [:ptr i8]] {:align 4})
        printf (function 'printf i32 [[:ptr i8] :&])
        pstr (getelementptr str [0 0] {:inbounds true})
        call (call printf [pstr])
        entry (-> (basic-block :entry)
                  (add-i retval)
                  (add-i argc-addr)
                  (add-i argv-addr)
                  (add-i (store (const i32 0) retval {:align 4}))
                  (add-i (store argc argc-addr {:align 4}))
                  (add-i (store argv argv-addr {:align 8}))
                  (add-i pstr)
                  (add-i call)
                  (add-i (ret (const i32 0))))
        attrs (attribute-group
               {:noinline true
                :nounwind true
                :sspstrong true
                :uwtable true
                "correctly-rounded-divide-sqrt-fp-math" "false"})
        main (-> (function 'main i32
                           [(param :argc i32)
                            (param :argv [:ptr [:ptr i8]])]
                           {:dso-local true
                            :function-attrs attrs})
                 (add-bb entry))]
    (-> (module)
        (assoc :data-layout
               {:byte-order :little-endian,
                :mangling-mode :elf,
                :pointer-layout {:size 32, :abi 32},
                :integer-layout [{:size 64, :abi 32}],
                :float-layout [{:size 64, :abi 32} {:size 80, :abi 32}],
                :legal-int-widths [8 16 32],
                :natural-stack-alignment 128}
               :target-triple
               {:arch :i386,
                :vendor :unknown,
                :os :linux,
                :env :unknown})
        (add-fn main)
        (render-module)))
  => "target datalayout = e-m:e-p0:32:32:32:32-i64:32:32-f64:32:32-f80:32:32-n8:16:32-S128
target triple = i386-unknown-linux-unknown
define dso_local i32 @main(i32 %argc, i8** %argv) noinline nounwind sspstrong uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" {
entry:
  %0 = alloca i32, align 4
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 4
  store i32 0, i32* %0, align 4
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  %3 = getelementptr inbounds [15 x i8], [15 x i8]* @.str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %3)
  ret i32 0
}

"))
