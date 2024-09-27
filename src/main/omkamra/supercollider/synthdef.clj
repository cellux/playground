(ns omkamra.supercollider.synthdef
  (:import (java.nio ByteBuffer))
  (:import (java.nio.charset Charset)))

(defn create
  []
  {:name nil
   :constants []
   :param-values []
   :params []
   :ugens []
   :variants []})

(defn put-i32
  [buf n]
  (.putInt buf (unchecked-int n)))

(defn put-i16
  [buf n]
  (.putShort buf (unchecked-short n)))

(defn put-i8
  [buf n]
  (.put buf (unchecked-byte n)))

(defn put-f32
  [buf x]
  (.putFloat buf (unchecked-float x)))

(defn put-pstr
  [buf s]
  (let [charset (Charset/forName "UTF-8")
        bytes (.getBytes s charset)
        length (.length bytes)]
    (when (> length 255)
      (throw (IllegalArgumentException. (str "string too long: " s))))
    (.put buf (unchecked-byte length))
    (.put buf (.getBytes s (Charset/forName "UTF-8")))))

(defn sizeof-pstr
  [s]
  (let [charset (Charset/forName "UTF-8")
        bytes (.getBytes s charset)
        length (.length bytes)]
    (when (> length 255)
      (throw (IllegalArgumentException. (str "string too long: " s))))
    (inc (count bytes))))

(defn sizeof-param
  [p]
  (+ (sizeof-pstr (:name p))
     ;; index in parameter array
     Integer/BYTES))

(defn sizeof-ugen
  [u]
  (+ (sizeof-pstr (:name u))
     ;; calculation rate
     Byte/BYTES
     ;; number of inputs (I)
     Integer/BYTES
     ;; number of outputs (O)
     Integer/BYTES
     ;; special index
     Short/BYTES
     ;; input specs
     (* (+ Integer/BYTES Integer/BYTES) (count (:inputs u)))
     ;; output specs
     (* Byte/BYTES (count (:outputs u)))))

(defn sizeof-variant [v]
  (+ (sizeof-pstr (:name v))
     (* Float/BYTES (count (:param-values v)))))

(defn sizeof
  [sdef]
  (+ (sizeof-pstr (:name sdef))
     ;; constants (K)
     Integer/BYTES
     (* Float/BYTES (count (:constants sdef)))
     ;; initial parameter values (P)
     Integer/BYTES
     (* Float/BYTES (count (:param-values sdef)))
     ;; parameter names (N)
     Integer/BYTES
     (reduce + (map sizeof-param (:params sdef)))
     ;; unit generators (U)
     Integer/BYTES
     (reduce + (map sizeof-ugen (:ugens sdef)))
     ;; variants (V)
     Short/BYTES
     (reduce + (map sizeof-variant (:variants sdef)))))

(defn put-param
  [buf p]
  (put-pstr buf (:name p))
  (put-i32 buf (:index p)))

(defn put-input-spec
  [buf i]
  (put-i32 buf (i 0))
  (put-i32 buf (i 1)))

(defn put-output-spec
  [buf o]
  (put-i8 buf o))

(defn put-ugen
  [buf u]
  (put-pstr buf (:name u))
  (put-i8 buf (:rate u))
  (put-i32 buf (count (:inputs u)))
  (put-i32 buf (count (:outputs u)))
  (put-i16 buf (:special-index u))
  (doseq [i (:inputs u)]
    (put-input-spec buf i))
  (doseq [o (:outputs u)]
    (put-output-spec buf o)))

(defn put-variant
  [buf v]
  (put-pstr buf (:name v))
  (doseq [p (:param-values v)]
    (put-f32 buf p)))

(defn put
  [buf sdef]
  (put-pstr buf (:name sdef))
  (put-i32 buf (count (:constants sdef)))
  (doseq [k (:constants sdef)]
    (put-f32 buf k))
  (put-i32 buf (count (:param-values sdef)))
  (doseq [p (:param-values sdef)]
    (put-f32 buf p))
  (put-i32 buf (count (:params sdef)))
  (doseq [p (:params sdef)]
    (put-pstr buf (:name p))
    (put-i32 buf (:index p)))
  (put-i32 buf (count (:ugens sdef)))
  (doseq [u (:ugens sdef)]
    (put-ugen buf u))
  (put-i16 buf (count (:variants sdef)))
  (doseq [v (:variants sdef)]
    (put-variant buf v)))

(def sizeof-file-header
  (+
   ;; file type id "SCgf"
   Integer/BYTES
   ;; file version
   Integer/BYTES
   ;; number of synthdefs in this file
   Short/BYTES))

(defn serialize
  [sdefs]
  (if (sequential? sdefs)
    (let [size (reduce + sizeof-file-header (map sizeof sdefs))
          buf (ByteBuffer/allocate size)
          file-format-marker 0x53436766
          file-version 2]
      (put-i32 buf file-format-marker)
      (put-i32 buf file-version)
      (put-i32 buf (count sdefs))
      (doseq [sdef sdefs]
        (put buf sdef))
      (.rewind buf))
    (recur (vector sdefs))))
