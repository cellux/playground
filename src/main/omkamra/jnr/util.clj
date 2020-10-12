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

(defn alloc-temp-int
  [lib]
  (let [runtime (jnr.ffi.Runtime/getRuntime lib)
        int-type jnr.ffi.NativeType/SINT]
    (jnr.ffi.Memory/allocateTemporary runtime int-type)))

(defn alloc-temp-double
  [lib]
  (let [runtime (jnr.ffi.Runtime/getRuntime lib)
        double-type jnr.ffi.NativeType/DOUBLE]
    (jnr.ffi.Memory/allocateTemporary runtime double-type)))

(defn alloc-temp-buffer
  [lib size]
  (let [runtime (jnr.ffi.Runtime/getRuntime lib)
        mm (.getMemoryManager runtime)]
    (.allocateTemporary mm size false)))
