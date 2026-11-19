(ns omkamra.dev.cached-refs
  "Development-only lazy persistent cache references.

  A cached reference is a read-only, dereferenceable value backed by an
  expensive producer and by a directory below `.cache/refs` in the process
  working directory. The first dereference loads a valid persisted entry or,
  when no valid entry exists, invokes the producer, persists its result and
  returns it. The value is memoized for the lifetime of the reference.

  Usage:

    (require '[omkamra.dev.cached-refs :as cached-refs])

    (def data
      (cached-refs/cached-ref
        :my.project/bigfile
        #(download \"https://foo.bar/bigfile.zip\")))

    @data
    (cached-refs/refresh! data)

  This namespace is meant for local development only. It assumes one Clojure
  process, a working directory at the repository root, and no coordination of
  producers across processes. See `cached-ref`, `refresh!`, `store` and `load`
  for the public contracts."
  (:refer-clojure :exclude [load])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file CopyOption Files StandardCopyOption)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util UUID)))

(def ^:dynamic *cache-root*
  "Root directory of the cache, relative to the process working directory.

  Defaults to `.cache/refs`. Rebinding this Var is intended for tests; a
  reference resolves its entry directory when it is constructed."
  (io/file ".cache" "refs"))

(def ^:private uninitialized
  "Sentinel distinguishing an uninitialized reference from a memoized nil."
  (Object.))

(def ^:private not-found
  "Sentinel returned by cache lookup when no valid entry is present."
  (Object.))

(defmulti store
  "Write `value` to `data-path` and return metadata describing its
  representation.

  The returned metadata must be a map containing at least a `:type`
  discriminator. The core adds a `:checksum` field covering the exact bytes
  written to `data-path`, so a method must not rely on its own checksum.

  Add a method for a new representation together with a matching `load`
  method; the core reference logic does not change. The default method accepts
  standard EDN values and rejects anything the default `clojure.edn` reader
  cannot restore."
  (fn [value _data-path]
    (class value)))

(defmulti load
  "Read `data-path` according to `metadata` and return the stored value.

  Dispatches on `(:type metadata)`. Throw for unsupported or malformed
  metadata; `cached-ref` treats a throwing load as an invalid cache entry and
  rebuilds it."
  (fn [metadata _data-path]
    (:type metadata)))

(defn- safe-component?
  "True when `component` is a safe single path component."
  [component]
  (and (string? component)
       (not (str/blank? component))
       (not (re-find #"[/\\]" component))
       (not (contains? #{"." ".."} component))
       (not (str/starts-with? component "/"))
       (not (str/starts-with? component "\\"))
       (not (str/includes? component "\u0000"))))

(defn- validate-key!
  "Throw unless `key` is a qualified keyword with safe path components."
  [key]
  (when-not (qualified-keyword? key)
    (throw (ex-info "Cache key must be a qualified keyword" {:key key})))
  (doseq [segment (str/split (namespace key) #"\." -1)]
    (when-not (safe-component? segment)
      (throw (ex-info "Cache key namespace segment is not a safe path component"
                      {:key key :component segment}))))
  (when-not (safe-component? (name key))
    (throw (ex-info "Cache key name is not a safe path component"
                    {:key key :component (name key)})))
  key)

(defn- entry-dir
  "Resolve the canonical entry directory for `key` below `*cache-root*`."
  [key]
  (validate-key! key)
  (let [root (.getCanonicalFile ^File *cache-root*)
        segments (conj (vec (str/split (namespace key) #"\." -1)) (name key))
        dir (.getCanonicalFile ^File (apply io/file *cache-root* segments))]
    (when-not (str/starts-with? (.getPath dir)
                                (str (.getPath root) File/separator))
      (throw (ex-info "Cache key escapes the cache root" {:key key})))
    dir))

(defn- ensure-dir!
  "Create `dir` and its parents when missing."
  [^File dir]
  (when-not (.isDirectory dir)
    (Files/createDirectories (.toPath dir) (make-array FileAttribute 0)))
  dir)

(defn- sha256-hex
  "Return the SHA-256 checksum of the exact bytes in `file` as `sha256:<hex>`."
  [^File file]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 65536)]
    (with-open [in (io/input-stream file)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (str "sha256:"
         (apply str (map #(format "%02x" (bit-and (int %) 0xff))
                         (.digest digest))))))

(defn- delete-quietly!
  "Best-effort delete of `file`."
  [^File file]
  (try
    (Files/deleteIfExists (.toPath file))
    (catch Exception _ nil)))

(defn- move-file!
  "Move `source` over `target`, replacing an existing target file."
  [^File source ^File target]
  (Files/move (.toPath source) (.toPath target)
              (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))

(defn- move-if-exists!
  "Move `source` to `target` when `source` is a file. Return true when moved."
  [^File source ^File target]
  (if (.isFile source)
    (do (move-file! source target) true)
    false))

(defn- replace-files!
  "Replace the final `data` and `metadata.edn` files with complete temporaries.

  Existing files are moved aside first so an ordinary failure can restore
  them. The pair of moves is not atomic; a crash between them leaves a mixed
  entry that the checksum rejects on the next load."
  [^File data-tmp ^File meta-tmp ^File data-file ^File meta-file]
  (let [suffix (str (UUID/randomUUID))
        parent (.getParentFile data-file)
        data-bak (io/file parent (str "data.bak-" suffix))
        meta-bak (io/file parent (str "metadata.edn.bak-" suffix))
        backed-up (volatile! #{})]
    (try
      (when (move-if-exists! data-file data-bak)
        (vswap! backed-up conj :data))
      (when (move-if-exists! meta-file meta-bak)
        (vswap! backed-up conj :meta))
      (move-file! data-tmp data-file)
      (move-file! meta-tmp meta-file)
      (delete-quietly! data-bak)
      (delete-quietly! meta-bak)
      (catch Throwable t
        (when (contains? @backed-up :data)
          (try (move-file! data-bak data-file)
               (catch Throwable _ nil)))
        (when (contains? @backed-up :meta)
          (try (move-file! meta-bak meta-file)
               (catch Throwable _ nil)))
        (throw t)))))

(defn- persist!
  "Persist `value` as a complete entry in `dir` and return its metadata."
  [^File dir value]
  (ensure-dir! dir)
  (let [suffix (str (UUID/randomUUID))
        data-tmp (io/file dir (str "data.tmp-" suffix))
        meta-tmp (io/file dir (str "metadata.edn.tmp-" suffix))
        data-file (io/file dir "data")
        meta-file (io/file dir "metadata.edn")]
    (try
      (let [metadata (store value data-tmp)]
        (when-not (map? metadata)
          (throw (ex-info "store must return a metadata map"
                          {:value-type (class value) :metadata metadata})))
        (when-not (:type metadata)
          (throw (ex-info "store metadata must contain a :type"
                          {:metadata metadata})))
        (let [metadata (assoc metadata :checksum (sha256-hex data-tmp))]
          (spit meta-tmp (pr-str metadata) :encoding "UTF-8")
          (replace-files! data-tmp meta-tmp data-file meta-file)
          metadata))
      (finally
        (delete-quietly! data-tmp)
        (delete-quietly! meta-tmp)))))

(defn- try-load
  "Return the value of a valid entry in `dir`, or `not-found`."
  [^File dir]
  (try
    (let [meta-file (io/file dir "metadata.edn")
          data-file (io/file dir "data")]
      (if (and (.isFile meta-file) (.isFile data-file))
        (let [metadata (edn/read-string (slurp meta-file :encoding "UTF-8"))]
          (if (and (map? metadata)
                   (:type metadata)
                   (= (:checksum metadata) (sha256-hex data-file)))
            (load metadata data-file)
            not-found))
        not-found))
    (catch Exception _
      not-found)))

(defn- produce-and-persist!
  "Invoke `producer`, persist its result in `dir` and return the value."
  [producer ^File dir]
  (let [value (producer)]
    (persist! dir value)
    value))

(defn- load-or-produce!
  "Return a valid cached value from `dir`, or produce and persist a new one."
  [producer ^File dir]
  (let [value (try-load dir)]
    (if (identical? value not-found)
      (produce-and-persist! producer dir)
      value)))

(deftype CachedRef [key producer ^File entry-dir ^clojure.lang.Volatile state]
  clojure.lang.IDeref
  (deref [this]
    (locking this
      (let [^clojure.lang.Volatile state (.-state this)
            cached @state]
        (if (identical? cached uninitialized)
          (let [value (load-or-produce! producer entry-dir)]
            (vreset! state value)
            value)
          cached))))
  Object
  (toString [_]
    (str "#cached-ref " (pr-str key))))

(defn cached-ref
  "Return a lazy, persistent, memoized reference for `key`.

  `key` must be a qualified keyword whose dot-separated namespace segments and
  name are safe single path components. The entry lives below `.cache/refs/`,
  with each namespace segment as a directory and the name as the final path
  component, relative to the process working directory.

  The first dereference loads a valid persisted entry or, when no valid entry
  exists, invokes `producer`, persists its result and returns it. The value is
  memoized for the lifetime of the reference; repeated dereferences do not
  touch the filesystem. Use `refresh!` to force recomputation.

  This library is intended for local development only. It assumes a single
  Clojure process and does not coordinate producers across processes."
  [key producer]
  (validate-key! key)
  (when-not (ifn? producer)
    (throw (ex-info "Producer must be callable" {:key key :producer producer})))
  (->CachedRef key producer (entry-dir key) (volatile! uninitialized)))

(defn refresh!
  "Recompute the value of `ref` and return the newly persisted value.

  Always invokes the producer, even when the reference is already memoized or
  a valid persistent entry exists. The new value is persisted before the
  reference's memoized value is replaced. If production or persistence fails,
  the previous memoized value is preserved and the error propagates."
  [^CachedRef ref]
  (locking ref
    (let [value (produce-and-persist! (.-producer ref) (.-entry-dir ref))]
      (vreset! (.-state ref) value)
      value)))

(defmethod store (Class/forName "[B")
  [value ^File data-path]
  (with-open [out (io/output-stream data-path)]
    (.write out ^bytes value))
  {:type :bytes})

(defmethod load :bytes
  [metadata ^File data-path]
  (when-not (map? metadata)
    (throw (ex-info "Malformed metadata for :bytes entry" {:metadata metadata})))
  (Files/readAllBytes (.toPath data-path)))

(defmethod store :default
  [value ^File data-path]
  (let [text (pr-str value)]
    (try
      (edn/read-string text)
      (catch Exception e
        (throw (ex-info "Value cannot be represented as EDN"
                        {:value-type (class value)}
                        e))))
    (spit data-path text :encoding "UTF-8")
    {:type :edn}))

(defmethod load :edn
  [metadata ^File data-path]
  (when-not (map? metadata)
    (throw (ex-info "Malformed metadata for :edn entry" {:metadata metadata})))
  (edn/read-string (slurp data-path :encoding "UTF-8")))

(defmethod load :default
  [metadata _data-path]
  (throw (ex-info "Unsupported cache representation" {:metadata metadata})))
