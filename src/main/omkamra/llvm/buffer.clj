(ns omkamra.llvm.buffer
  (:require [omkamra.llvm.api :refer [$llvm ok? check]])
  (:import (jnr.ffi.byref PointerByReference)
           (java.nio.charset Charset)))

(defn from-file
  [path]
  (let [membuf-ptr (PointerByReference.)
        message-ptr (PointerByReference.)
        status (.LLVMCreateMemoryBufferWithContentsOfFile $llvm path membuf-ptr message-ptr)]
    (check status message-ptr)
    (.getValue membuf-ptr)))

(defn from-string
  [s]
  (.LLVMCreateMemoryBufferWithMemoryRangeCopy $llvm s (count s) nil))

(defn size
  [buffer]
  (.LLVMGetBufferSize $llvm buffer))

(defn contents
  [buffer]
  (let [start (.LLVMGetBufferStart $llvm buffer)]
    (.getString start 0 (size buffer) (Charset/defaultCharset))))

(defn dispose
  [buffer]
  (.LLVMDisposeMemoryBuffer $llvm buffer))
