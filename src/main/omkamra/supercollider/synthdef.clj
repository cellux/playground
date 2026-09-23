(ns omkamra.supercollider.synthdef
  (:require [omkamra.supercollider.ugen :as ugen])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets)))

(def ^:private file-format-marker 0x53436766)
(def ^:private file-version 2)
(def ^:private max-pstring-length 255)

(defn- synthdef-name
  [name]
  (cond
    (string? name) name
    (or (keyword? name) (symbol? name)) (clojure.core/name name)
    :else (throw (IllegalArgumentException.
                  (str "synthdef name must be a string, keyword, or symbol: "
                       (pr-str name))))))

(defn- argument-spec
  [[name default :as spec]]
  (when-not (and (sequential? spec) (= 2 (count spec)))
    (throw (IllegalArgumentException.
            (str "synthdef argument must be [name default]: "
                 (pr-str spec)))))
  [(synthdef-name name) default])

(defn create
  "Create a SynthDef map from a name, argument specifications, and body fn.

  The body function receives one control node for each `[name default]`
  argument specification and must return the root UGen graph node."
  ([]
   {:name nil
    :constants []
    :param-values []
    :params []
    :ugens []
    :variants []})
  ([name args body]
   (when-not (ifn? body)
     (throw (IllegalArgumentException.
             "synthdef body must be a function")))
   (let [args (mapv argument-spec args)
         controls (mapv (fn [[arg-name default] index]
                         (ugen/control arg-name default index))
                       args
                       (range))
         root (apply body controls)]
     (assoc (ugen/compile root controls)
            :name (synthdef-name name)))))

(defmacro define-synthdef
  "Define a named SynthDef var using lexical control bindings."
  [name args & body]
  (when-not (symbol? name)
    (throw (IllegalArgumentException.
            "define-synthdef name must be a symbol")))
  (when-not (vector? args)
    (throw (IllegalArgumentException.
            "define-synthdef arguments must be a vector")))
  (let [arg-names (mapv first args)
        arg-specs (mapv (fn [[arg-name default :as spec]]
                          (when-not (and (symbol? arg-name)
                                         (= 2 (count spec)))
                            (throw (IllegalArgumentException.
                                    (str "invalid synthdef argument: "
                                         (pr-str spec)))))
                          [(clojure.core/name arg-name) default])
                        args)]
    `(def ~name
       (create ~(clojure.core/name name)
               ~arg-specs
               (fn [~@arg-names]
                 ~@body)))))

(defmacro define
  "Alias for `define-synthdef`."
  [name args & body]
  `(define-synthdef ~name ~args ~@body))

(defn- require-integer
  [label value]
  (when-not (integer? value)
    (throw (IllegalArgumentException.
            (str label " must be an integer: " (pr-str value)))))
  value)

(defn- require-range
  [label value minimum maximum]
  (require-integer label value)
  (when-not (<= minimum value maximum)
    (throw (IllegalArgumentException.
            (str label " out of range [" minimum ", " maximum "]: " value))))
  value)

(defn- require-count
  [label values maximum]
  (let [n (count values)]
    (require-range label n 0 maximum)
    n))

(defn- require-string
  [label value]
  (when-not (string? value)
    (throw (IllegalArgumentException.
            (str label " must be a string: " (pr-str value)))))
  value)

(defn- pstring-bytes
  [s]
  (let [s (require-string "pstring" s)
        bytes (.getBytes ^String s StandardCharsets/UTF_8)
        length (alength bytes)]
    (when (> length max-pstring-length)
      (throw (IllegalArgumentException.
              (str "pstring is too long (maximum is " max-pstring-length
                   " UTF-8 bytes): " (pr-str s)))))
    bytes))

(defn put-i32
  [buf n]
  (.putInt buf
           (int (require-range "int32" n Integer/MIN_VALUE Integer/MAX_VALUE))))

(defn put-i16
  [buf n]
  (.putShort buf
             (short (require-range "int16" n Short/MIN_VALUE Short/MAX_VALUE))))

(defn put-i8
  [buf n]
  (.put buf
        (byte (require-range "int8" n Byte/MIN_VALUE Byte/MAX_VALUE))))

(defn put-f32
  [buf x]
  (when-not (number? x)
    (throw (IllegalArgumentException.
            (str "float32 must be numeric: " (pr-str x)))))
  (.putFloat buf (float x)))

(defn put-pstr
  [buf s]
  (let [bytes (pstring-bytes s)]
    (.put buf (byte (alength bytes)))
    (.put buf bytes)))

(defn sizeof-pstr
  [s]
  (inc (alength (pstring-bytes s))))

(defn- values-at
  [m key]
  (let [values (or (get m key) [])]
    (when-not (sequential? values)
      (throw (IllegalArgumentException.
              (str key " must be sequential: " (pr-str values)))))
    values))

(defn- validate-synthdef
  [sdef]
  (when-not (map? sdef)
    (throw (IllegalArgumentException.
            (str "synth definition must be a map: " (pr-str sdef)))))
  (let [param-values (values-at sdef :param-values)
        params (values-at sdef :params)
        variants (values-at sdef :variants)
        parameter-count (count param-values)]
    (doseq [param params]
      (require-string "parameter name" (:name param))
      (require-range "parameter index" (:index param)
                     0 (dec parameter-count)))
    (doseq [variant variants]
      (require-string "variant name" (:name variant))
      (let [variant-values (values-at variant :param-values)]
        (when-not (= parameter-count (count variant-values))
          (throw (IllegalArgumentException.
                  (str "variant parameter value count must be "
                       parameter-count ": " (pr-str variant))))))))
  sdef)

(defn sizeof-param
  [p]
  (+ (sizeof-pstr (:name p))
     ;; index in parameter array
     Integer/BYTES))

(defn sizeof-ugen
  [u]
  (let [inputs (values-at u :inputs)
        outputs (values-at u :outputs)]
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
       (* (+ Integer/BYTES Integer/BYTES) (count inputs))
       ;; output specs
       (* Byte/BYTES (count outputs)))))

(defn sizeof-variant
  [v]
  (+ (sizeof-pstr (:name v))
     (* Float/BYTES (count (values-at v :param-values)))))

(defn sizeof
  [sdef]
  (let [constants (values-at sdef :constants)
        param-values (values-at sdef :param-values)
        params (values-at sdef :params)
        ugens (values-at sdef :ugens)
        variants (values-at sdef :variants)]
    (+ (sizeof-pstr (:name sdef))
       ;; constants (K)
       Integer/BYTES
       (* Float/BYTES (count constants))
       ;; initial parameter values (P)
       Integer/BYTES
       (* Float/BYTES (count param-values))
       ;; parameter names (N)
       Integer/BYTES
       (reduce + 0 (map sizeof-param params))
       ;; unit generators (U)
       Integer/BYTES
       (reduce + 0 (map sizeof-ugen ugens))
       ;; variants (V)
       Short/BYTES
       (reduce + 0 (map sizeof-variant variants)))))

(defn put-param
  [buf p]
  (put-pstr buf (:name p))
  (put-i32 buf (:index p)))

(defn put-input-spec
  [buf input]
  (when-not (and (sequential? input) (= 2 (count input)))
    (throw (IllegalArgumentException.
            (str "input spec must be [ugen-index output-index]: "
                 (pr-str input)))))
  (put-i32 buf (nth input 0))
  (put-i32 buf (nth input 1)))

(defn put-output-spec
  [buf output-rate]
  (put-i8 buf output-rate))

(defn put-ugen
  [buf u]
  (let [inputs (values-at u :inputs)
        outputs (values-at u :outputs)]
    (put-pstr buf (:name u))
    (put-i8 buf (:rate u))
    (put-i32 buf (require-count "ugen input count" inputs Integer/MAX_VALUE))
    (put-i32 buf (require-count "ugen output count" outputs Integer/MAX_VALUE))
    (put-i16 buf (or (:special-index u) 0))
    (doseq [input inputs]
      (put-input-spec buf input))
    (doseq [output-rate outputs]
      (put-output-spec buf output-rate))))

(defn put-variant
  [buf v]
  (let [param-values (values-at v :param-values)]
    (put-pstr buf (:name v))
    (doseq [value param-values]
      (put-f32 buf value))))

(defn put
  [buf sdef]
  (validate-synthdef sdef)
  (let [constants (values-at sdef :constants)
        param-values (values-at sdef :param-values)
        params (values-at sdef :params)
        ugens (values-at sdef :ugens)
        variants (values-at sdef :variants)]
    (put-pstr buf (:name sdef))
    (put-i32 buf (require-count "constant count" constants Integer/MAX_VALUE))
    (doseq [constant constants]
      (put-f32 buf constant))
    (put-i32 buf (require-count "parameter value count" param-values Integer/MAX_VALUE))
    (doseq [value param-values]
      (put-f32 buf value))
    (put-i32 buf (require-count "parameter name count" params Integer/MAX_VALUE))
    (doseq [param params]
      (put-param buf param))
    (put-i32 buf (require-count "ugen count" ugens Integer/MAX_VALUE))
    (doseq [ugen ugens]
      (put-ugen buf ugen))
    (put-i16 buf (require-count "variant count" variants Short/MAX_VALUE))
    (doseq [variant variants]
      (put-variant buf variant))))

(def sizeof-file-header
  (+
   ;; file type id "SCgf"
   Integer/BYTES
   ;; file version
   Integer/BYTES
   ;; number of synthdefs in this file
   Short/BYTES))

(defn serialize
  "Serialize one SynthDef map or a sequential collection of SynthDef maps.

  SynthDef maps use the following wire-oriented representation:

  * `:constants` and `:param-values` are sequences of numbers.
  * `:params` contains `{:name string :index int}` maps.
  * `:ugens` contains maps with `:name`, `:rate`, `:inputs`, `:outputs`,
    and optional `:special-index` keys. An input is `[ugen-index output-index]`;
    `[-1 constant-index]` refers to the constants table.
  * `:variants` contains `{:name string :param-values values}` maps.

  The returned, rewound ByteBuffer contains a SynthDef2 file in big-endian
  order, ready to use as an OSC blob."
  [sdefs]
  (let [sdefs (cond
                (map? sdefs) [sdefs]
                (sequential? sdefs) (vec sdefs)
                :else (throw (IllegalArgumentException.
                              "synthdefs must be a map or sequential collection")))
        definition-count (require-count "synth definition count"
                                        sdefs Short/MAX_VALUE)]
    (doseq [sdef sdefs]
      (validate-synthdef sdef))
    (let [size (+ sizeof-file-header (reduce + 0 (map sizeof sdefs)))]
      (when (> size Integer/MAX_VALUE)
        (throw (IllegalArgumentException.
                (str "serialized synthdefs exceed the maximum buffer size: " size))))
      (let [buf (doto (ByteBuffer/allocate (int size))
                  (.order ByteOrder/BIG_ENDIAN))]
        (put-i32 buf file-format-marker)
        (put-i32 buf file-version)
        (put-i16 buf definition-count)
        (doseq [sdef sdefs]
          (put buf sdef))
        (.rewind buf)))))
