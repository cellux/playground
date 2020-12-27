(ns omkamra.llvm.ir
  (:require [midje.sweet :as m])
  (:require [clojure.string :as str])
  (:import (java.nio ByteOrder))
  (:import (com.kenai.jffi Platform Platform$OS Platform$CPU)))

(defn platform-byte-order
  []
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

(defn platform-os
  []
  (let [platform (Platform/getPlatform)
        os (.getOS platform)]
    (or (known-operating-systems os)
        (throw (ex-info "unknown operating system" {:os os})))))

(defn platform-object-format
  []
  (case (platform-os)
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

(defn platform-arch
  []
  (let [platform (Platform/getPlatform)
        cpu (.getCPU platform)]
    (or (known-cpu-architectures cpu)
        (throw (ex-info "unknown CPU architecture" {:cpu cpu})))))

(defn platform-mangling-mode
  []
  (case (platform-object-format)
    :elf :elf
    :mach-o :mach-o
    :coff (case (platform-arch)
            :i386 :windows-x86-coff
            :x86_64 :windows-coff)))

(defn platform-pointer-layout
  []
  (let [size (.addressSize (Platform/getPlatform))]
    {:size size :abi size}))

(defn platform-integer-layout
  []
  (let [os (platform-os)
        pointer-size (:size (platform-pointer-layout))]
    [{:size 64 :abi (if (or (= pointer-size 64) (= os :windows)) 64 32)}]))

(defn platform-float-layout
  []
  (let [os (platform-os)
        pointer-size (:size (platform-pointer-layout))]
    (-> []
        (conj {:size 64 :abi (if (or (= pointer-size 64) (= os :windows)) 64 32)})
        (conj {:size 80 :abi (if (or (= pointer-size 64) (= os :darwin)) 128 32)}))))

(defn platform-legal-int-widths
  []
  (case (:size (platform-pointer-layout))
    32 [8 16 32]
    64 [8 16 32 64]))

(defn platform-natural-stack-alignment
  []
  (let [os (platform-os)
        pointer-size (:size (platform-pointer-layout))]
    (if (and (= 32 pointer-size) (= os :windows))
      32
      128)))

(defn platform-data-layout
  []
  {:byte-order (platform-byte-order)
   :mangling-mode (platform-mangling-mode)
   :pointer-layout (platform-pointer-layout)
   :float-layout (platform-float-layout)
   :legal-int-widths (platform-legal-int-widths)
   :natural-stack-alignment (platform-natural-stack-alignment)})

(defn platform-target-triple
  []
  {:arch (platform-arch)
   :vendor :unknown
   :os (platform-os)
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
          (or address-space 0)
          size
          abi
          (or pref abi)
          (or idx size)))

(defmethod render-data-layout-item :float-layout
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

(defn digit?
  [ch]
  (<= 0x30 (int ch) 0x39))

(defn identifier-part?
  [ch]
  (let [b (int ch)]
    (or (<= 0x61 b 0x7a)
        (<= 0x41 b 0x5a)
        (<= 0x30 b 0x39)
        (== 0x2d b)
        (== 0x2e b)
        (== 0x5f b))))

(defn needs-quoting?
  [name]
  (or (digit? (first name))
      (not (every? identifier-part? name))))

(defn render-quoted-string
  [s]
  (str \"
       (-> (.getBytes s "utf-8")
           escape-byte-string)
       \"))

(defn render-name-string-without-prefix
  [name-string]
  (if (needs-quoting? name-string)
    (render-quoted-string name-string)
    name-string))

(defn render-name
  [x]
  (cond
    (integer? x) (str \% x)
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
    (keyword? t) (render-simple-type t)
    (vector? t) (render-complex-type t)
    :else (throw (ex-info "invalid type" {:type t}))))

(defn extract-type-tag
  [t]
  (cond
    (keyword? t) t
    (vector? t) (first t)
    :else (throw (ex-info "invalid type" {:type t}))))

(m/facts
 (extract-type-tag [:integer 8]) => :integer
 (extract-type-tag [:named-struct :t]) => :named-struct
 (extract-type-tag :void) => :void)

(defmulti render-literal (fn [type value] (extract-type-tag type)))

(defmethod render-literal :void
  [_ value]
  "void")

(defmethod render-literal :integer
  [[_ size] value]
  (if (and (= size 1) (boolean? value))
    (str value)
    (format "%d" value)))

(defmethod render-literal :ptr
  [_ value]
  (if (nil? value) "null"
      (throw (ex-info "invalid pointer literal" {:value value}))))

(defmethod render-literal :array
  [[_ elt size] value]
  (cond
    (and (= elt [:integer 8]) (string? value))
    (str \c (render-quoted-string value))
    (vector? value)
    (format "[ %s ]" (str/join ", " (map render-typed-value value)))))

(m/facts
 (render-literal :void :void) => "void"
 (render-literal [:integer 32] 1234) => "1234"
 (render-literal [:integer 1] true) => "true"
 (render-literal [:integer 1] false) => "false"
 (render-literal [:integer 1] 0) => "0"
 (render-literal [:integer 1] 1) => "1"
 (render-literal [:ptr [:integer 8]] nil) => "null"
 (render-literal [:array [:integer 8]] "Hello, world\n\0") => "c\"Hello, world\\0A\\00\"")

(declare render-expr)

(defn render-typed-value
  [{:keys [type name value]}]
  (if name
    (format "%s %s" (render-type type) (render-name name))
    (if (map? value)
      (format "%s %s" (render-type type) (render-expr value))
      (if (= :void type)
        "void"
        (format "%s %s" (render-type type) (render-literal type value))))))

(defn render-value
  [{:keys [type name value]}]
  (if name
    (render-name name)
    (render-literal type value)))

(defmulti render-expr :op)

(defmethod render-expr :getelementptr
  [{:keys [type ptr indices inbounds]}]
  (format "getelementptr%s (%s, %s, %s)"
          (if inbounds " inbounds" "")
          (render-type type)
          (render-typed-value ptr)
          (str/join ", " (map render-typed-value indices))))

(m/facts
 (render-expr {:op :getelementptr
               :inbounds true
               :type [:array [:array [:integer 32] 3] 4]
               :ptr {:type [:ptr [:array [:array [:integer 32] 3] 4]] :name :matrix}
               :indices [{:type [:integer 64] :value 0}
                         {:type [:integer 64] :name :idxprom}]})
 => "getelementptr inbounds ([4 x [3 x i32]], [4 x [3 x i32]]* %matrix, i64 0, i64 %idxprom)")

(defmulti render-instruction :op)

(defmethod render-instruction :ret
  [{:keys [value]}]
  (format "ret %s" (render-typed-value value)))

(m/facts
 (render-instruction {:op :ret
                      :value {:type [:integer 32] :value 0}})
 => "ret i32 0"

 (render-instruction {:op :ret
                      :value {:type :void :value :void}})
 => "ret void")

(defmethod render-instruction :br
  [{:keys [cond dest else]}]
  (if cond
    (format "br %s, %s, %s"
            (render-typed-value cond)
            (render-typed-value dest)
            (render-typed-value else))
    (format "br %s" (render-typed-value dest))))

(m/facts
 (render-instruction {:op :br
                      :dest {:type :label :name :for.cond}})
 => "br label %for.cond"

 (render-instruction {:op :br
                      :cond {:type [:integer 1] :name :cmp}
                      :dest {:type :label :name :for.body}
                      :else {:type :label :name :for.end8}})
 => "br i1 %cmp, label %for.body, label %for.end8")

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
 (render-instruction {:op :switch
                      :value {:type [:integer 32] :name 2}
                      :dest {:type :label :name :sw.default}
                      :cases [[{:type [:integer 32] :value 1} {:type :label :name :sw.bb}]
                              [{:type [:integer 32] :value 2} {:type :label :name :sw.bb1}]
                              [{:type [:integer 32] :value 3} {:type :label :name :sw.bb3}]]})
 => "switch i32 %2, label %sw.default [ i32 1, label %sw.bb i32 2, label %sw.bb1 i32 3, label %sw.bb3 ]")

(defmacro define-unary-op
  [op opts]
  `(defmethod render-instruction ~(keyword op)
     [{:keys [~'name ~'value ~@opts]}]
     (with-out-str
       (print (render-name ~'name))
       (print (str " = " ~(name op)))
       ~@(for [opt opts] `(when ~opt
                            (print ~(str " " (name opt)))))
       (print (str " " (render-typed-value ~'value))))))

(define-unary-op fneg [])

(defmacro define-binary-op
  [op opts]
  `(defmethod render-instruction ~(keyword op)
     [{:keys [~'name ~'lhs ~'rhs ~@opts]}]
     (with-out-str
       (print (render-name ~'name))
       (print (str " = " ~(name op)))
       ~@(for [opt opts] `(when ~opt
                            (print ~(str " " (name opt)))))
       (print (str " " (render-typed-value ~'lhs)))
       (print (str ", " (render-value ~'rhs))))))

(define-binary-op add [nsw nuw])

(m/facts
 (render-instruction {:name :inc
                      :op :add
                      :nsw true
                      :lhs {:type [:integer 32] :name 6}
                      :rhs {:type [:integer 32] :value 1}})
 => "%inc = add nsw i32 %6, 1"

 (render-instruction {:name :add
                      :op :add
                      :nsw true
                      :lhs {:type [:integer 32] :name 1}
                      :rhs {:type [:integer 32] :name 0}})
 => "%add = add nsw i32 %1, %0")

(define-binary-op fadd [])

(define-binary-op sub [nsw nuw])

(m/facts
 (render-instruction {:name :sub
                      :op :sub
                      :nsw true
                      :lhs {:type [:integer 32] :name 0}
                      :rhs {:type [:integer 32] :value 1}})
 => "%sub = sub nsw i32 %0, 1")

(define-binary-op fsub [])

(define-binary-op mul [nsw nuw])

(m/facts
 (render-instruction {:name :mul
                      :op :mul
                      :nsw true
                      :lhs {:type [:integer 32] :name 2}
                      :rhs {:type [:integer 32] :name 3}})
 => "%mul = mul nsw i32 %2, %3")

(define-binary-op fmul [])

(define-binary-op udiv [exact])

(define-binary-op sdiv [exact])

(m/facts
 (render-instruction {:name :div
                      :op :sdiv
                      :lhs {:type [:integer 32] :name 6}
                      :rhs {:type [:integer 32] :name 7}})
 => "%div = sdiv i32 %6, %7")

(define-binary-op fdiv [])

(define-binary-op urem [])

(define-binary-op srem [])

(m/facts
 (render-instruction {:name :rem
                      :op :srem
                      :lhs {:type [:integer 32] :name 8}
                      :rhs {:type [:integer 32] :name 9}})
 => "%rem = srem i32 %8, %9")

(define-binary-op frem [])

(define-binary-op shl [nsw nuw])

(m/facts
 (render-instruction {:name :shl
                      :op :shl
                      :lhs {:type [:integer 32] :name 21}
                      :rhs {:type [:integer 32] :value 3}})
 => "%shl = shl i32 %21, 3")

(define-binary-op lshr [exact])

(define-binary-op ashr [exact])

(m/facts
 (render-instruction {:name :shr
                      :op :ashr
                      :lhs {:type [:integer 32] :name 22}
                      :rhs {:type [:integer 32] :value 3}})
 => "%shr = ashr i32 %22, 3")

(define-binary-op and [])

(m/facts
 (render-instruction {:name :and
                      :op :and
                      :lhs {:type [:integer 32] :name 14}
                      :rhs {:type [:integer 32] :name 15}})
 => "%and = and i32 %14, %15")

(define-binary-op or [])

(m/facts
 (render-instruction {:name :or
                      :op :or
                      :lhs {:type [:integer 32] :name 16}
                      :rhs {:type [:integer 32] :name 17}})
 => "%or = or i32 %16, %17")

(define-binary-op xor [])

(m/facts
 (render-instruction {:name :xor
                      :op :xor
                      :lhs {:type [:integer 32] :name 18}
                      :rhs {:type [:integer 32] :name 19}})
 => "%xor = xor i32 %18, %19"

 (render-instruction {:name :neg
                      :op :xor
                      :lhs {:type [:integer 32] :name 20}
                      :rhs {:type [:integer 32] :value -1}})
 => "%neg = xor i32 %20, -1"

 (render-instruction {:name :lnot
                      :op :xor
                      :lhs {:type [:integer 1] :name :tobool6}
                      :rhs {:type [:integer 1] :value true}})
 => "%lnot = xor i1 %tobool6, true")

(defmethod render-instruction :alloca
  [{:keys [name type address-space array-size align]}]
  (format "%s = alloca %s, align %d"
          (render-name name)
          (render-type type)
          align))

(m/facts
 (render-instruction {:name :retval
                      :op :alloca
                      :type [:integer 32]
                      :align 4})
 => "%retval = alloca i32, align 4"
 
 (render-instruction {:name :argv.addr
                      :op :alloca
                      :type [:ptr [:ptr [:integer 8]]]
                      :align 8})
 => "%argv.addr = alloca i8**, align 8"

 (render-instruction {:name :matrix
                      :op :alloca
                      :type [:array [:array [:integer 32] 3] 4]
                      :align 16})
 => "%matrix = alloca [4 x [3 x i32]], align 16"

 (render-instruction {:name :f
                      :op :alloca
                      :type :float
                      :align 4})
 => "%f = alloca float, align 4"

 (render-instruction {:name :d
                      :op :alloca
                      :type :double
                      :align 8})
 => "%d = alloca double, align 8"

 (render-instruction {:name :ld
                      :op :alloca
                      :type :x86_fp80
                      :align 16})
 => "%ld = alloca x86_fp80, align 16"

 (render-instruction {:name :ifn1
                      :op :alloca
                      :type [:ptr [:fn [:integer 32] [[:integer 32]]]]
                      :align 8})
 => "%ifn1 = alloca i32 (i32)*, align 8")

(defmethod render-instruction :load
  [{:keys [name type ptr align]}]
  (format "%s = load %s, %s, align %d"
          (render-name name)
          (render-type type)
          (render-typed-value ptr)
          align))

(m/facts
 (render-instruction {:name 0
                      :op :load
                      :type [:integer 32]
                      :ptr {:type [:ptr [:integer 32]] :name :i}
                      :align 4})
 => "%0 = load i32, i32* %i, align 4"

 (render-instruction {:name 1
                      :op :load
                      :type [:integer 32]
                      :ptr {:type [:ptr [:integer 32]] :name 'sum}
                      :align 4})
 => "%1 = load i32, i32* @sum, align 4")

(defmethod render-instruction :store
  [{:keys [name val ptr align]}]
  (format "store %s, %s, align %d"
          (render-typed-value val)
          (render-typed-value ptr)
          align))

(m/facts
 (render-instruction {:op :store
                      :val {:type [:integer 32] :value 0}
                      :ptr {:type [:ptr [:integer 32]] :name :retval}
                      :align 4})
 => "store i32 0, i32* %retval, align 4"

 (render-instruction {:op :store
                      :val {:type [:integer 8] :value -56}
                      :ptr {:type [:ptr [:integer 8]] :name :c3}
                      :align 1})
 => "store i8 -56, i8* %c3, align 1"

 (render-instruction {:op :store
                      :val {:type [:integer 32] :name :argc}
                      :ptr {:type [:ptr [:integer 32]] :name :argc.addr}
                      :align 4})
 => "store i32 %argc, i32* %argc.addr, align 4"
 
 (render-instruction {:op :store
                      :val {:type [:ptr [:ptr [:integer 8]]] :name :argv}
                      :ptr {:type [:ptr [:ptr [:ptr [:integer 8]]]] :name :argv.addr}
                      :align 8})
 => "store i8** %argv, i8*** %argv.addr, align 8"

 (render-instruction {:op :store
                      :val {:type [:ptr [:integer 8]] :value nil}
                      :ptr {:type [:ptr [:ptr [:integer 8]]] :name :ptr}
                      :align 8})
 => "store i8* null, i8** %ptr, align 8")

(defmacro define-conversion-op
  [op]
  `(defmethod render-instruction ~(keyword op)
     [{:keys [~'name ~'value ~'dest-type]}]
     (format ~(str "%s = " op " %s to %s")
             (render-name ~'name)
             (render-typed-value ~'value)
             (render-type ~'dest-type))))

(define-conversion-op trunc)

(define-conversion-op zext)

(m/facts
 (render-instruction {:name :conv2
                      :op :zext
                      :value {:type [:integer 8] :name 2}
                      :dest-type [:integer 32]})
 => "%conv2 = zext i8 %2 to i32")

(define-conversion-op sext)

(m/facts
 (render-instruction {:name :idxprom
                      :op :sext
                      :value {:type [:integer 32] :name 4}
                      :dest-type [:integer 64]})
 => "%idxprom = sext i32 %4 to i64")

(define-conversion-op fptrunc)

(define-conversion-op fpext)

(m/facts
 (render-instruction {:name :conv11
                      :op :fpext
                      :value {:type :float :name 16}
                      :dest-type :double})
 => "%conv11 = fpext float %16 to double")

(define-conversion-op fptoui)

(define-conversion-op fptosi)

(m/facts
 (render-instruction {:name :conv
                      :op :fptosi
                      :value {:type :float :name 3}
                      :dest-type [:integer 32]})
 => "%conv = fptosi float %3 to i32")

(define-conversion-op uitofp)
(define-conversion-op sitofp)

(define-conversion-op ptrtoint)
(define-conversion-op inttoptr)

(define-conversion-op bitcast)

(m/facts
 (render-instruction {:name :c
                      :op :bitcast
                      :value {:type [:ptr [:named-struct :union.u]] :name :tmp}
                      :dest-type [:ptr [:integer 8]]})
 => "%c = bitcast %union.u* %tmp to i8*")

(define-conversion-op addrspacecast)

(defn render-predicate
  [pred]
  (name pred))

(defmethod render-instruction :icmp
  [{:keys [name pred lhs rhs]}]
  (format "%s = icmp %s %s, %s"
          (render-name name)
          (render-predicate pred)
          (render-typed-value lhs)
          (render-value rhs)))

(m/facts
 (render-instruction {:name :cmp
                      :op :icmp
                      :pred :slt
                      :lhs {:type [:integer 32] :name 0}
                      :rhs {:type [:integer 32] :value 4}})
 => "%cmp = icmp slt i32 %0, 4"

 (render-instruction {:name :cmp1
                      :op :icmp
                      :pred :eq
                      :lhs {:type [:integer 32] :name 1}
                      :rhs {:type [:integer 32] :value 5}})
 => "%cmp1 = icmp eq i32 %1, 5"

 (render-instruction {:name :tobool
                      :op :icmp
                      :pred :ne
                      :lhs {:type [:integer 32] :name 23}
                      :rhs {:type [:integer 32] :value 0}})
 => "%tobool = icmp ne i32 %23, 0")

(defmethod render-instruction :fcmp
  [{:keys [name pred lhs rhs]}]
  (format "%s = fcmp %s %s, %s"
          (render-name name)
          (render-predicate pred)
          (render-typed-value lhs)
          (render-value rhs)))

(defmethod render-instruction :phi
  [{:keys [name type values]}]
  (format "%s = phi %s %s"
          (render-name name)
          (render-type type)
          (->> values
               (map #(format "[ %s, %s ]"
                             (render-value (second %))
                             (render-value (first %))))
               (str/join ", "))))

(m/facts
 (render-instruction {:name 25
                      :op :phi
                      :type [:integer 1]
                      :values {{:type :label :name :entry} {:type [:integer 1] :value false}
                               {:type :label :name :land.rhs} {:type [:integer 1] :name :tobool3}}})
 => "%25 = phi i1 [ false, %entry ], [ %tobool3, %land.rhs ]"

 (render-instruction {:name 28
                      :op :phi
                      :type [:integer 1]
                      :values {{:type :label :name :land.end} {:type [:integer 1] :value true}
                               {:type :label :name :lor.rhs} {:type [:integer 1] :name :tobool5}}})
 => "%28 = phi i1 [ true, %land.end ], [ %tobool5, %lor.rhs ]")

(defmethod render-instruction :select
  [{:keys [name cond then else]}]
  (format "%s = select %s, %s, %s"
          (render-name name)
          (render-typed-value cond)
          (render-typed-value then)
          (render-typed-value else)))

(m/facts
 (render-instruction {:name :cond
                      :op :select
                      :cond {:type [:integer 1] :name :cmp17}
                      :then {:type [:integer 32] :value 1}
                      :else {:type [:integer 32] :value 0}})
 => "%cond = select i1 %cmp17, i32 1, i32 0")

(defn render-callee
  [callee]
  (render-name (:name callee)))

(defmethod render-instruction :call
  [{:keys [name type callee args]}]
  (if name
    (format "%s = call %s %s(%s)"
            (render-name name)
            (render-type type)
            (render-callee callee)
            (str/join ", " (map render-typed-value args)))
    (format "call %s %s(%s)"
            (render-type type)
            (render-callee callee)
            (str/join ", " (map render-typed-value args)))))

(m/facts
 (render-instruction {:op :call
                      :type :void
                      :callee {:type :void :name 'add}
                      :args [{:type [:integer 32] :name 0}]})
 => "call void @add(i32 %0)"

 (render-instruction {:name :call
                      :op :call
                      :type [:integer 32]
                      :callee {:type [:integer 32] :name 1}
                      :args [{:type [:integer 32] :value 0}]})
 => "%call = call i32 %1(i32 0)"

 (render-instruction {:name :call
                      :op :call
                      :type [:fn [:integer 32] [[:ptr [:integer 8]] :&]]
                      :callee {:type [:fn [:integer 32] [[:ptr [:integer 8]] :&]] :name 'printf}
                      :args [{:type [:ptr [:integer 8]]
                              :value {:op :getelementptr
                                      :inbounds true
                                      :type [:array [:integer 8] 21]
                                      :ptr {:type [:ptr [:array [:integer 8] 21]] :name '.str}
                                      :indices [{:type [:integer 64] :value 0}
                                                {:type [:integer 64] :value 0}]}}
                             {:type [:integer 32] :name :conv}
                             {:type [:integer 32] :name :conv1}
                             {:type [:integer 32] :name :conv2}]})
 => "%call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str, i64 0, i64 0), i32 %conv, i32 %conv1, i32 %conv2)")

(defmethod render-instruction :getelementptr
  [{:keys [name type ptr indices inbounds]}]
  (format "%s = getelementptr%s %s, %s, %s"
          (render-name name)
          (if inbounds " inbounds" "")
          (render-type type)
          (render-typed-value ptr)
          (str/join ", " (map render-typed-value indices))))

(m/facts
 (render-instruction {:name :arrayidx
                      :op :getelementptr
                      :inbounds true
                      :type [:array [:array [:integer 32] 3] 4]
                      :ptr {:type [:ptr [:array [:array [:integer 32] 3] 4]] :name :matrix}
                      :indices [{:type [:integer 64] :value 0}
                                {:type [:integer 64] :name :idxprom}]})
 => "%arrayidx = getelementptr inbounds [4 x [3 x i32]], [4 x [3 x i32]]* %matrix, i64 0, i64 %idxprom")

(defn render-basic-block
  [block]
  (with-out-str
    (printf "%s:\n" (name (:name block)))
    (doseq [i (:instructions block)]
      (printf "  %s\n" (render-instruction i)))))

(m/facts
 (render-basic-block {:name :entry
                      :instructions
                      [{:name :retval
                        :op :alloca
                        :type [:integer 32]
                        :align 4}
                       {:op :ret
                        :value {:type [:integer 32] :value 0}}]})
 => "entry:
  %retval = alloca i32, align 4
  ret i32 0
")

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
  (or (known-linkages linkage)
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

(defn render-attributes
  [attrs]
  (if (integer? attrs)
    (format "#%d" attrs)
    (str/join " " (map render-attribute attrs))))

(defn render-function-parameter
  [{:keys [name type attrs]}]
  (with-out-str
    (printf "%s" (render-type type))
    (when attrs
      (printf " %s" (render-attributes attrs)))
    (when name
      (printf " %s" (render-name name)))))

(m/facts
 (render-function-parameter {:type [:integer 32] :name :argc})
 => "i32 %argc"
 (render-function-parameter {:type [:ptr [:ptr [:integer 8]]] :name :argv})
 => "i8** %argv")

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

(defn render-global
  [{:keys [name type linkage dso-local visibility dll-storage-class
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
    (printf "%s" (render-type type))
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
 (render-global {:name 'add.z
                 :linkage :internal
                 :type [:integer 32]
                 :initializer {:type [:integer 32] :value 8}
                 :align 4})
 => "@add.z = internal global i32 8, align 4"

 (render-global {:name '.str
                 :linkage :private
                 :unnamed-addr :global
                 :constant true
                 :type [:array [:integer 8] 15]
                 :initializer {:type [:array [:integer 8]] :value "Hello, world!\n\0"}
                 :align 1})
 => "@.str = private unnamed_addr constant [15 x i8] c\"Hello, world!\\0A\\00\", align 1")

(defn render-function
  [{:keys [name linkage dso-local visibility dll-storage-class
           cconv result-attrs result-type name params
           unnamed-addr address-space function-attrs
           section comdat align gc prefix prologue personality
           metadata blocks] :as f}]
  (with-out-str
    (print (if blocks "define " "declare "))
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
    (print (str/join ", " (map render-function-parameter (or params []))))
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
    (when blocks
      (print " {\n")
      (doseq [block blocks]
        (print (render-basic-block block)))
      (print "}\n"))))

(m/facts
 (render-function {:name 'main
                   :dso-local true
                   :result-type [:integer 32]
                   :params [{:type [:integer 32] :name :argc}
                            {:type [:ptr [:ptr [:integer 8]]] :name :argv}]
                   :function-attrs 0
                   :blocks
                   [{:name :entry
                     :instructions
                     [{:name :retval
                       :op :alloca
                       :type [:integer 32]
                       :align 4}
                      {:op :ret
                       :value {:type [:integer 32] :value 0}}]}]})
 => "define dso_local i32 @main(i32 %argc, i8** %argv) #0 {
entry:
  %retval = alloca i32, align 4
  ret i32 0
}
"
 
 (render-function {:name 'printf
                   :result-type [:integer 32]
                   :params [{:type [:ptr [:integer 8]]}
                            {:type :&}]
                   :function-attrs 1})
 => "declare i32 @printf(i8*, ...) #1")

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
