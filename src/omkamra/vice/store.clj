(ns omkamra.vice.store
  "Raw-capture directory, manifest, chunk metadata, and artifact storage.

  This namespace owns the on-disk v2 capture contract: atomic artifact
  publication, SHA-256 checksums, the top-level manifest commit log, per-chunk
  metadata, memory-image files, and complete-capture validation. It never
  touches VICE and never writes analysis output.

  Directory layout under a dataset root:

  ```text
  <dataset-root>/capture/manifest.edn
  <dataset-root>/capture/chunks/chunk-000001.edn
  <dataset-root>/capture/chunks/chunk-000001.bin
  <dataset-root>/capture/chunks/chunk-000001.ram
  <dataset-root>/capture/chunks/chunk-000001.mem
  <dataset-root>/capture/chunks/chunk-000001.png
  ```"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [omkamra.vice.contract :as contract])
  (:import [java.io BufferedOutputStream OutputStream]
           [java.nio.file AtomicMoveNotSupportedException Files LinkOption
            StandardCopyOption]
           [java.security DigestOutputStream MessageDigest]
           [java.util Arrays]))

(def default-timing-model
  "Fallback PAL timing model recorded in the manifest so raster and cycle
  values can be interpreted without re-deriving the machine geometry."
  {:cycles-per-line 63
   :raster-lines 312
   :clock-hz 985248})

(def ram-format-descriptor
  {:format contract/ram-image-format
   :size contract/memory-image-size
   :byte-order contract/byte-order
   :address-order :ascending})

(def mem-format-descriptor
  {:format contract/mem-image-format
   :size contract/memory-image-size
   :byte-order contract/byte-order
   :address-order :ascending})

(def snapshot-format-descriptor
  {:format contract/snapshot-format})

(def screen-format-descriptor
  {:format contract/screen-png-format
   :media-type "image/png"
   :pixel-format :rgb24
   :frame :vic-debug-frame})

(defn- fail
  [reason data]
  (throw (ex-info "Invalid v2 capture artifact" (assoc data :reason reason))))

(def ^:private png-signature
  (byte-array [(unchecked-byte 0x89) 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a]))

(def ^:private png-signature-length (alength ^bytes png-signature))

;; Checksums and atomic publication -----------------------------------------

(defn hex
  [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes)))

(defn sha256-bytes
  [^bytes bytes]
  (hex (.digest (MessageDigest/getInstance "SHA-256") bytes)))

(defn sha256-file
  [file]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream (io/file file))]
      (let [buffer (byte-array 65536)]
        (loop []
          (let [read (.read in buffer)]
            (when (pos? read)
              (.update digest buffer 0 read)
              (recur))))))
    (hex (.digest digest))))

(defn atomic-move!
  [source target]
  (let [src (.toPath (io/file source))
        dst (.toPath (io/file target))]
    (try
      (Files/move src dst
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (catch AtomicMoveNotSupportedException _
        (Files/move src dst
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING]))))))

(defn write-edn-file!
  [file value]
  (.mkdirs (.getParentFile (io/file file)))
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  file)

(defn atomic-write-edn!
  [file value]
  (let [target (io/file file)
        partial (io/file (str (.getPath target) ".partial"))]
    (write-edn-file! partial value)
    (atomic-move! partial target)
    target))

(defn atomic-write-bytes!
  [file ^bytes bytes]
  (let [target (io/file file)
        partial (io/file (str (.getPath target) ".partial"))]
    (.mkdirs (.getParentFile target))
    (with-open [out (io/output-stream partial)]
      (.write out bytes 0 (alength bytes)))
    (atomic-move! partial target)
    target))

(defn read-edn
  [file]
  (edn/read-string (slurp (io/file file))))

(defn write-artifact!
  "Write `writer-fn`'s output to `target` atomically.

  The payload goes to a sibling `.partial` file first, so a partially written
  artifact is never visible. Returns `{:size n :sha256 hex}`."
  [target writer-fn]
  (let [target (io/file target)
        partial (io/file (str (.getPath target) ".partial"))
        digest (MessageDigest/getInstance "SHA-256")]
    (.mkdirs (.getParentFile target))
    (with-open [out (DigestOutputStream.
                     (BufferedOutputStream. (io/output-stream partial))
                     digest)]
      (writer-fn out))
    (let [size (.length partial)
          hash (hex (.digest digest))]
      (atomic-move! partial target)
      {:size size :sha256 hash})))

(def ^:private no-follow-links (make-array LinkOption 0))

(defn delete-tree!
  "Recursively delete `file` without following symbolic links.

  A link is removed as a link; its target is never traversed or deleted. The
  earlier `File`-based walk followed links through `File.isDirectory`, so
  deleting a directory that contained a symlink to a capture silently deleted
  the capture's contents."
  [file]
  (let [^java.nio.file.Path path (.toPath (io/file file))]
    (cond
      (Files/isSymbolicLink path) (Files/deleteIfExists path)
      (not (Files/exists path no-follow-links)) nil
      (Files/isDirectory path no-follow-links)
      (do (with-open [stream (Files/newDirectoryStream path)]
            (doseq [^java.nio.file.Path child stream]
              (delete-tree! (.toFile child))))
          (Files/deleteIfExists path))
      :else (Files/deleteIfExists path))))

;; Memory images -------------------------------------------------------------

(defn write-image!
  "Write a 64 KiB memory image as one of the canonical image files."
  [target ^bytes image descriptor]
  (let [expected (:size descriptor)]
    (when-not (and (bytes? image) (= expected (alength image)))
      (fail :invalid-memory-image
            {:expected expected
             :actual (when (bytes? image) (alength image))}))
    (write-artifact! target
                     (fn [^OutputStream out]
                       (.write out image 0 (alength image))))))

(defn read-image
  [file descriptor]
  (let [bytes (Files/readAllBytes (.toPath (io/file file)))
        expected (:size descriptor)]
    (when-not (= expected (alength bytes))
      (fail :invalid-memory-image
            {:file (str file) :expected expected :actual (alength bytes)}))
    bytes))

;; Paths --------------------------------------------------------------------

(defn manifest-file
  ^java.io.File [directory]
  (io/file directory "manifest.edn"))

(defn chunks-directory
  ^java.io.File [directory]
  (io/file directory "chunks"))

(defn chunk-file-name
  [number]
  (format "chunk-%06d.edn" number))

(def ^:private artifact-extensions
  {:events "bin" :ram "ram" :mem "mem" :screenshot "png"})

(defn chunk-artifact-name
  [number kind]
  (format "chunk-%06d.%s" number (get artifact-extensions kind)))

(defn chunk-metadata-path
  [directory number]
  (io/file (chunks-directory directory) (chunk-file-name number)))

;; Manifest -----------------------------------------------------------------

(defn new-manifest
  [{:keys [capture-id directory timing-model options]}]
  (when-not (and (string? capture-id) (seq capture-id))
    (fail :missing-capture-id {:capture-id capture-id}))
  (let [blacklist (vec (sort (or (:vic-value-blacklist options)
                                 contract/default-vic-value-blacklist)))]
    (contract/validate-vic-value-blacklist! (set blacklist))
    {:format contract/capture-format
     :capture-id capture-id
     :directory directory
     :status :capturing
     :timing-model (merge default-timing-model timing-model)
     :options (merge {:event-target 50000}
                     options
                     {:vic-value-blacklist blacklist})
     :formats {:events contract/event-record-descriptor
               :ram ram-format-descriptor
               :mem mem-format-descriptor
               :snapshot snapshot-format-descriptor
               :screenshot screen-format-descriptor}
     :chunk-count 0
     :event-count 0
     :chunks []}))

(defn write-manifest!
  [directory manifest]
  (atomic-write-edn! (manifest-file directory) manifest))

(defn read-manifest
  [directory]
  (let [file (manifest-file directory)]
    (when-not (.isFile (io/file file))
      (fail :missing-manifest {:file (str file)}))
    (read-edn file)))

;; Validation ---------------------------------------------------------------

(defn- validate-artifact!
  [directory kind spec]
  (let [file (io/file directory (:file spec))]
    (when-not (.isFile file)
      (fail :missing-artifact {:kind kind :file (str file)}))
    (when-not (= (:size spec) (.length file))
      (fail :artifact-size-mismatch
            {:kind kind :file (str file)
             :expected (:size spec) :actual (.length file)}))
    (when-not (= (:sha256 spec) (sha256-file file))
      (fail :artifact-checksum-mismatch {:kind kind :file (str file)}))
    (case kind
      :events
      (let [records (:records spec)]
        (when-not (and (int? records)
                       (= (* contract/event-record-size records)
                          (.length file)))
          (fail :event-artifact-size-mismatch
                {:file (str file) :records records :size (.length file)})))
      :ram
      (when-not (= contract/memory-image-size (.length file))
        (fail :ram-image-size-mismatch {:file (str file)}))
      :mem
      (when-not (= contract/memory-image-size (.length file))
        (fail :mem-image-size-mismatch {:file (str file)}))
      :screenshot
      (let [magic (byte-array png-signature-length)]
        (with-open [in (io/input-stream file)]
          (when-not (= png-signature-length (.read in magic))
            (fail :screenshot-truncated {:file (str file)})))
        (when-not (Arrays/equals ^bytes magic ^bytes png-signature)
          (fail :screenshot-not-a-png {:file (str file)})))
      nil)))

(defn- validate-chunk-entry!
  [directory expected-number entry]
  (when-not (= expected-number (:number entry))
    (fail :chunk-number-mismatch
          {:expected expected-number :actual (:number entry)}))
  (let [metadata-file (io/file directory (:file entry))]
    (when-not (.isFile metadata-file)
      (fail :missing-chunk-metadata {:file (str metadata-file)}))
    (when-not (= (:size entry) (.length metadata-file))
      (fail :chunk-metadata-size-mismatch {:file (str metadata-file)}))
    (when-not (= (:sha256 entry) (sha256-file metadata-file))
      (fail :chunk-metadata-checksum-mismatch {:file (str metadata-file)}))
    (let [metadata (read-edn metadata-file)]
      (when-not (= contract/chunk-format (:format metadata))
        (fail :not-a-v2-chunk {:format (:format metadata)}))
      (when-not (= (:number entry) (:chunk-number metadata))
        (fail :chunk-number-metadata-mismatch {:number (:number entry)}))
      (when-not (= (:event-range entry) (:event-range metadata))
        (fail :chunk-event-range-mismatch {:number (:number entry)}))
      (when-not (= (:event-count entry) (:event-count metadata))
        (fail :chunk-event-count-mismatch {:number (:number entry)}))
      (contract/validate-snapshot! (:snapshot metadata))
      (doseq [[kind spec] (:artifacts entry)]
        (validate-artifact! directory kind spec)))))

(defn validate-capture!
  "Validate a finalized capture and every committed artifact.

  Checks the manifest identity and status, chunk numbering and half-open
  event-range contiguity, per-chunk metadata sizes and checksums, artifact
  sizes and checksums, and the fixed memory-image and event-record sizes.
  Returns `{:directory ... :manifest ...}` or throws."
  [directory]
  (let [directory (.getCanonicalPath (io/file directory))
        manifest (read-manifest directory)]
    (when-not (= contract/capture-format (:format manifest))
      (fail :not-a-v2-capture {:format (:format manifest)}))
    (when-not (= :finalized (:status manifest))
      (fail :capture-not-finalized {:status (:status manifest)}))
    (let [chunks (:chunks manifest)]
      (when-not (= (:chunk-count manifest) (count chunks))
        (fail :chunk-count-mismatch
              {:declared (:chunk-count manifest) :actual (count chunks)}))
      (loop [remaining chunks
             expected-number 1
             cursor 0]
        (if-let [entry (first remaining)]
          (do
            (validate-chunk-entry! directory expected-number entry)
            (let [[start end] (:event-range entry)]
              (when-not (= start cursor)
                (fail :non-contiguous-event-range
                      {:number (:number entry)
                       :expected-start cursor :actual-start start}))
              (when-not (= end (+ start (:event-count entry)))
                (fail :chunk-event-range-length-mismatch
                      {:number (:number entry)
                       :event-range (:event-range entry)
                       :event-count (:event-count entry)}))
              (recur (rest remaining) (inc expected-number) (long end))))
          (when-not (= cursor (:event-count manifest))
            (fail :total-event-count-mismatch
                  {:expected (:event-count manifest) :actual cursor})))))
    {:directory directory :manifest manifest}))

(defn open-capture
  "Validate and open a finalized capture directory."
  [directory]
  (validate-capture! directory))
