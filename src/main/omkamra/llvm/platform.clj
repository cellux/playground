(ns omkamra.llvm.platform
  (:import (java.nio ByteOrder))
  (:import (com.kenai.jffi Platform Platform$OS Platform$CPU)))

(def platform (Platform/getPlatform))

(def byte-order
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

(def os
  (let [os (.getOS platform)]
    (or (known-operating-systems os)
        (throw (ex-info "unknown operating system" {:os os})))))

(def object-format
  (case os
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

(def arch
  (let [cpu (.getCPU platform)]
    (or (known-cpu-architectures cpu)
        (throw (ex-info "unknown CPU architecture" {:cpu cpu})))))

(def mangling-mode
  (case object-format
    :elf :elf
    :mach-o :mach-o
    :coff (case arch
            :i386 :windows-x86-coff
            :x86_64 :windows-coff)))

(def address-size
  (.addressSize platform))

(def pointer-layout
  {:size address-size :abi address-size})

(def integer-layout
  [{:size 64
    :abi (if (or (= address-size 64)
                 (= os :windows))
           64 32)}])

(def float-layout
  (-> []
      (conj {:size 64 :abi (if (or (= address-size 64)
                                   (= os :windows))
                             64 32)})
      (conj {:size 80 :abi (if (or (= address-size 64)
                                   (= os :darwin))
                             128 32)})))

(def legal-int-widths
  (case address-size
    32 [8 16 32]
    64 [8 16 32 64]))

(def natural-stack-alignment
  (if (and (= address-size 32)
           (= os :windows))
    32 128))

(def data-layout
  {:byte-order byte-order
   :mangling-mode mangling-mode
   :pointer-layout pointer-layout
   :integer-layout integer-layout
   :float-layout float-layout
   :legal-int-widths legal-int-widths
   :natural-stack-alignment natural-stack-alignment})

(def target-triple
  {:arch arch
   :vendor :unknown
   :os os
   :env :unknown})
