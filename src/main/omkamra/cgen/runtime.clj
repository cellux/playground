(ns omkamra.cgen.runtime
  "Compile and execute cgen translation units with the host C compiler."
  (:require [clojure.string :as str]
            [omkamra.cgen.emit :as emit]
            [omkamra.cgen.linker :as linker]
            [omkamra.cgen.parser :as parser])
  (:import [java.io ByteArrayOutputStream InputStream]
           [java.nio.file Files Path]))

(def ^:dynamic *compiler* "cc")

(defn- process! [command]
  (let [process (.start (doto (ProcessBuilder. ^java.util.List (vec command))
                          (.redirectErrorStream true)))
        output (slurp (.getInputStream process))
        exit (.waitFor process)]
    {:command command :exit exit :output output}))

(defn- read-bytes [^InputStream input]
  (with-open [input input]
    (let [output (ByteArrayOutputStream.)
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read input buffer)]
          (when (pos? n)
            (.write output buffer 0 n)
            (recur))))
      (.toByteArray output))))

(defn- write-bytes [output bytes]
  (with-open [output output]
    (when (seq bytes)
      (.write output ^bytes bytes))))

(defn- process-binary!
  ([command]
   (process-binary! command (byte-array 0)))
  ([command input]
   (let [process (.start (doto (ProcessBuilder. ^java.util.List (vec command))
                           (.redirectErrorStream false)))
         stdin (future (write-bytes (.getOutputStream process) input))
         stdout (future (read-bytes (.getInputStream process)))
         stderr (future (String. ^bytes (read-bytes (.getErrorStream process))
                                 java.nio.charset.StandardCharsets/UTF_8))
         exit (.waitFor process)]
     @stdin
     {:command command
      :exit exit
      :stdout @stdout
      :stderr @stderr})))

(defn- delete-tree! [^Path path]
  (when (Files/exists path (make-array java.nio.file.LinkOption 0))
    (with-open [paths (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path child (reverse (sort-by str (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists child)))))

(defn- type-double? [type]
  (= :double type))

(defn- parse-argument-line [idx name type argv-offset]
  (let [argv-index (+ argv-offset idx)]
    (if (type-double? type)
      (str "  char *end" idx ";\n"
           "  errno = 0;\n"
           "  double " name " = strtod(argv[" argv-index "], &end" idx ");\n"
           "  if (errno || *end" idx " != '\\0') return 64;")
      (str "  char *end" idx ";\n"
           "  errno = 0;\n"
           "  long long raw" idx " = strtoll(argv[" argv-index "], &end" idx ", 10);\n"
           "  if (errno || *end" idx " != '\\0') return 64;\n"
           "  int64_t " name " = (int64_t)raw" idx ";"))))

(defn- scalar-launcher [entry params return-type]
  (let [names (map #(str "arg" %) (range (count params)))
        parse-lines (map-indexed (fn [idx [name type]]
                                   (parse-argument-line idx name type 1))
                                 (map vector names (map emit/param-type params)))
        call (str (name entry) "(" (str/join ", " names) ")")
        output (if (type-double? return-type)
                 "  printf(\"%.17g\\n\", result);\n"
                 "  printf(\"%\" PRId64 \"\\n\", result);\n")]
    (str "#include <errno.h>\n"
         "#include <inttypes.h>\n"
         "#include <stdint.h>\n"
         "#include <stdio.h>\n"
         "#include <stdlib.h>\n\n"
         "int main(int argc, char **argv) {\n"
         "  if (argc != " (inc (count params)) ") return 64;\n"
         (str/join "\n" parse-lines) "\n"
         "  " (if (type-double? return-type) "double" "int64_t")
         " result = " call ";\n"
         output
         "  return 0;\n}\n")))

(defn- binary-read-line [name type]
  (str "  " (if (type-double? type) "double " "int64_t ") name ";\n"
       "  if (fread(&" name ", sizeof " name ", 1, stdin) != 1) return 65;"))

(defn- binary-launcher [entry params return-type]
  (let [names (map #(str "arg" %) (range (count params)))
        types (map emit/param-type params)
        reads (map binary-read-line names types)
        call (str (name entry) "(" (str/join ", " names) ")")]
    (str "#include <stdint.h>\n"
         "#include <stdio.h>\n"
         (when (empty? params) "#include <stdlib.h>\n")
         "\n"
         "int main(int argc, char **argv) {\n"
         (if (empty? params)
           (str "  if (argc != 2) return 64;\n"
                "  char *end;\n"
                "  long long calls = strtoll(argv[1], &end, 10);\n"
                "  if (*end != '\\0' || calls < 0) return 64;\n"
                "  for (long long i = 0; i < calls; ++i) {\n"
                "    " (if (type-double? return-type) "double" "int64_t")
                " result = " call ";\n"
                "    if (fwrite(&result, sizeof result, 1, stdout) != 1) return 74;\n"
                "  }\n")
           (str "  if (argc != 1) return 64;\n"
                "  for (;;) {\n"
                "    int first = fgetc(stdin);\n"
                "    if (first == EOF) break;\n"
                "    ungetc(first, stdin);\n"
                (str/join "\n" (map #(str "    " %) reads)) "\n"
                "    " (if (type-double? return-type) "double" "int64_t")
                " result = " call ";\n"
                "    if (fwrite(&result, sizeof result, 1, stdout) != 1) return 74;\n"
                "  }\n"))
         "  return 0;\n}\n")))

(defn- as-entry-var [entry]
  (cond
    (var? entry) entry
    (:cgen/var (meta entry)) (:cgen/var (meta entry))
    :else (throw (ex-info "cgen entry must be a cgen function or Var"
                          {:entry entry}))))

(defn- entry-unit [entry]
  (let [{:keys [forms entry]}
        (if (map? entry)
          (linker/link-anonymous entry)
          (linker/link-entry (as-entry-var entry)))
        unit (parser/parse forms)
        entry-function (first (filter #(= entry (:name %)) (:functions unit)))]
    {:unit unit
     :entry entry
     :entry-function entry-function}))

(defn source-for-entry
  "Lowers a cgen function Var and every reachable dependency to C17 source.

  `:mode` is `:scalar` (the default) or `:binary`. Binary mode reads a stream
  of fixed-width function arguments from stdin and writes fixed-width return
  values to stdout. The wire representation is the host C representation of
  `int64_t` and `double`; callers should use the same host ABI."
  ([entry-var]
   (source-for-entry entry-var {:mode :scalar}))
  ([entry-var {:keys [mode] :or {mode :scalar}}]
   (let [{:keys [unit entry entry-function]} (entry-unit entry-var)
         params (:params entry-function)
         return-type (:return-type entry-function)
         launcher (case mode
                    :scalar (scalar-launcher entry params return-type)
                    :binary (binary-launcher entry params return-type)
                    (throw (ex-info "unsupported cgen launcher mode" {:mode mode})))]
     {:source (str (emit/emit-translation-unit unit) "\n" launcher)
      :entry entry
      :params params
      :return-type return-type
      :arity (count params)})))

(defn describe
  "Returns the native signature of a cgen function or Var."
  [entry-var]
  (let [{:keys [params return-type]} (source-for-entry entry-var)]
    {:params (mapv emit/param-type params)
     :return-type return-type}))

(defn- checked-int64 [argument]
  (when-not (integer? argument)
    (throw (ex-info "cgen expected integer arguments" {:argument argument})))
  (when (or (< argument Long/MIN_VALUE) (> argument Long/MAX_VALUE))
    (throw (ex-info "cgen argument is outside the signed 64-bit range"
                    {:argument argument})))
  (long argument))

(defn- checked-double [argument]
  (when-not (number? argument)
    (throw (ex-info "cgen expected numeric arguments" {:argument argument})))
  (let [value (double argument)]
    (when-not (Double/isFinite value)
      (throw (ex-info "cgen arguments must be finite" {:argument argument})))
    value))

(defn- checked-arguments [params arguments]
  (when-not (= (count params) (count arguments))
    (throw (ex-info "wrong number of cgen arguments"
                    {:expected (count params) :actual (count arguments)})))
  (mapv (fn [param argument]
          (if (= :double (emit/param-type param))
            (checked-double argument)
            (checked-int64 argument)))
        params arguments))

(defn- bytes-value [value]
  (cond
    (instance? (class (byte-array 0)) value) value
    (sequential? value) (byte-array value)
    :else (throw (ex-info "cgen binary input must be a byte array or sequence"
                          {:input value}))))

(defn- compile-source! [source]
  (let [dir (Files/createTempDirectory "cgen-"
                                       (make-array java.nio.file.attribute.FileAttribute 0))
        source-path (.resolve dir "program.c")
        executable-path (.resolve dir "program")]
    (spit (.toFile source-path) source)
    (let [{:keys [exit output command]} (process! [*compiler* "-std=c17" "-O2"
                                                   (str source-path) "-lm"
                                                   "-o" (str executable-path)])]
      (if (zero? exit)
        {:dir dir :executable executable-path}
        (do
          (delete-tree! dir)
          (throw (ex-info "cgen host C compilation failed"
                          {:command command :exit exit :output output :source source})))))))

(defn invoke
  "Compiles and executes a scalar cgen function."
  [entry-var arguments]
  (let [{:keys [source params return-type]} (source-for-entry entry-var)
        arguments (checked-arguments params arguments)
        {:keys [dir executable]} (compile-source! source)]
    (try
      (let [{:keys [exit stdout stderr command]}
            (process-binary! (into [(str executable)] (map str arguments)))]
        (when-not (zero? exit)
          (throw (ex-info "cgen executable failed"
                          {:command command :exit exit :stderr stderr :source source})))
        (let [output (String. ^bytes stdout java.nio.charset.StandardCharsets/UTF_8)]
          (try
            (if (= :double return-type)
              (Double/parseDouble (str/trim output))
              (Long/parseLong (str/trim output)))
            (catch NumberFormatException e
              (throw (ex-info "cgen executable returned an invalid scalar"
                              {:output output :source source}
                              e))))))
      (finally
        (delete-tree! dir)))))

(defn- wire-size [_type]
  8)

(defn invoke-binary
  "Executes a cgen function over a binary stream.

  For a function with parameters, stdin contains zero or more consecutive
  records, each containing one native 8-byte value per parameter. For a
  parameterless function, pass `:calls` in the options map and stdin may be
  empty. stdout contains one native 8-byte return value per call. The result
  contains the raw `:bytes`, invocation `:count`, and `:return-type`."
  [entry-var {:keys [input calls]}]
  (let [{:keys [source params return-type]} (source-for-entry entry-var {:mode :binary})
        input (bytes-value (or input (byte-array 0)))
        record-size (reduce + (map wire-size (map emit/param-type params)))
        input-size (alength ^bytes input)
        count (if (empty? params)
                (do
                  (when-not (and (integer? calls) (<= 0 calls))
                    (throw (ex-info "parameterless cgen invoke-binary requires non-negative :calls"
                                    {:calls calls})))
                  (long calls))
                (do
                  (when-not (zero? (mod input-size record-size))
                    (throw (ex-info "binary input is not an integral number of argument records"
                                    {:input-size input-size :record-size record-size})))
                  (quot input-size record-size)))
        {:keys [dir executable]} (compile-source! source)
        command (cond-> [(str executable)]
                  (empty? params) (conj (str count)))]
    (try
      (let [{:keys [exit stdout stderr]}
            (process-binary! command input)
            expected-size (* count (wire-size return-type))
            actual-size (alength ^bytes stdout)]
        (when-not (zero? exit)
          (throw (ex-info "cgen binary invocation failed"
                          {:command command :exit exit :stderr stderr :source source})))
        (when-not (= expected-size actual-size)
          (throw (ex-info "cgen binary invocation returned an incomplete result"
                          {:expected-size expected-size
                           :actual-size actual-size
                           :stderr stderr
                           :source source})))
        {:bytes stdout
         :count count
         :return-type return-type})
      (finally
        (delete-tree! dir)))))