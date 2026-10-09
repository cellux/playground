(ns omkamra.vice.analysis.common
  "Shared persistence, stage-path, and analysis configuration helpers."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [omkamra.vice.decoder.classification :as classification]
            [omkamra.vice.decoder.features :as features])
  (:import [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]
           [java.time Instant]))

(def analysis-format :omkamra.vice/analysis-v1)
(def stage-format :omkamra.vice/analysis-stage-v1)
(def active-runs (atom #{}))
;; Expanded write analysis can transiently require substantial heap for one
;; large raw chunk. One worker is safe by default; callers with sufficient heap
;; can opt into bounded overlap explicitly.
(def default-chunk-parallelism 1)

(declare normalize-segment-configuration)

(defn file-path
  [directory & parts]
  (.getPath (apply io/file directory parts)))

(defn now
  []
  (str (Instant/now)))

(defn write-edn-file!
  [file value]
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (pr value)
      (newline)))
  file)

(defn atomic-write-edn!
  [file value]
  (let [target (.toPath (io/file file))
        partial (.toPath (io/file (str file ".partial")))]
    (.mkdirs (.getParentFile (.toFile target)))
    (write-edn-file! (.toFile partial) value)
    (try
      (Files/move partial target
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (catch AtomicMoveNotSupportedException _
        (Files/move partial target
                    (into-array StandardCopyOption
                                [StandardCopyOption/REPLACE_EXISTING]))))
    file))

(defn delete-tree!
  [file]
  (when (.isDirectory ^java.io.File file)
    (doseq [child (.listFiles ^java.io.File file)]
      (delete-tree! child)))
  (io/delete-file file true))

(defn move!
  [source target]
  (try
    (Files/move (.toPath (io/file source))
                (.toPath (io/file target))
                (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (catch AtomicMoveNotSupportedException _
      (Files/move (.toPath (io/file source))
                  (.toPath (io/file target))
                  (make-array StandardCopyOption 0)))))

(defn replace-directory!
  "Install a fully written stage directory while retaining the old output until
  the replacement is ready. A failed install restores the previous directory."
  [staging target]
  (let [target-file (io/file target)
        backup (str target ".previous")]
    (delete-tree! (io/file backup))
    (if-not (.exists target-file)
      (move! staging target)
      (do
        (move! target backup)
        (try
          (move! staging target)
          (delete-tree! (io/file backup))
          (catch Throwable error
            (when-not (.exists target-file)
              (move! backup target))
            (throw error)))))))

(defn stage-directory
  [capture-directory id]
  (file-path capture-directory "analysis" "stages" (name id)))

(defn stage-output-file
  [id]
  (str "analysis/stages/" (name id) "/index.edn"))

(defn stage-configuration
  [options stage]
  (let [configuration (or (get-in options [:config (:id stage)]) {})]
    (cond
      (= :features (:id stage))
      (features/normalize-feature-configuration configuration)

      (= :classification (:id stage))
      (classification/normalize-classifier-configuration configuration)

      (= :segments (:id stage))
      (normalize-segment-configuration configuration)

      :else configuration)))

(defn stage-chunk-file
  [stage-directory chunk-number]
  (file-path stage-directory "chunks"
             (format "chunk-%06d.edn" chunk-number)))

(defn read-stage-chunk
  [capture-directory completed-stages stage-id chunk-number]
  (let [stage-directory (get-in completed-stages
                                [stage-id :output-directory])]
    (when-not stage-directory
      (throw (ex-info "Required analysis stage output is unavailable"
                      {:stage stage-id
                       :chunk-number chunk-number})))
    (edn/read-string
     (slurp (stage-chunk-file (file-path capture-directory stage-directory)
                              chunk-number)))))

;; Semantic segments ---------------------------------------------------------

(def default-segment-configuration
  {:frame-gap-events 250000
   :min-demopart-events 200000
   :transition-gap-events 250000
   :activity-confidence 0.35
   ;; The exact frame fingerprint includes raster timing and is intentionally
   ;; sensitive to normal effect animation. Segment splitting instead uses a
   ;; coarser IRQ/VIC signature which must persist for this many frames.
   :signature-stability-frames 4
   :signature-vic-write-bucket 64
   ;; A coarse signature is an epoch boundary, not automatically a new part.
   ;; These distances distinguish a semantic effect change from normal raster
   ;; animation while retaining every epoch as a reviewable candidate.
   :signature-vic-write-distance 4
   :signature-d012-write-distance 2
   :corroboration-gap-events 250000
   ;; Review-only diagnostics for ranges not covered by a semantic descriptor.
   :gap-short-events 500000
   :gap-write-density-threshold 0.05
   :gap-peripheral-ratio-threshold 0.35
   :gap-transfer-unit-ratio-threshold 0.1
   :gap-transfer-unit-accesses 8
   :gap-decode-ratio-threshold 0.5
   :gap-iec-write-threshold 128
   :gap-transition-execution-threshold 10000
   :gap-transition-frontier-threshold 256
   ;; Per-demopart routine detection. A raster or music routine is the compact
   ;; cluster of PCs that writes VIC or SID respectively with a per-frame
   ;; cadence. Anchors must write at least this often and at least this
   ;; fraction of the most frequent writing PC, so incidental one-off register
   ;; pokes do not drag unrelated code into the routine listing.
   :routine-min-pc-count 8
   :routine-pc-relative-threshold 0.1
   :expected-parts-per-file [3 4]})

(defn normalize-segment-configuration
  [configuration]
  (when-not (map? configuration)
    (throw (ex-info "Segment configuration must be a map"
                    {:configuration configuration})))
  (let [configuration (merge default-segment-configuration configuration)]
    (doseq [key [:frame-gap-events :min-demopart-events
                 :transition-gap-events :signature-stability-frames
                 :signature-vic-write-bucket :signature-vic-write-distance
                 :signature-d012-write-distance :corroboration-gap-events
                 :gap-short-events :gap-iec-write-threshold
                 :gap-transition-execution-threshold
                 :gap-transition-frontier-threshold
                 :gap-transfer-unit-accesses
                 :routine-min-pc-count]]
      (when-not (pos-int? (get configuration key))
        (throw (ex-info "Segment configuration value must be a positive integer"
                        {:key key :value (get configuration key)}))))
    (when-not (and (number? (:activity-confidence configuration))
                   (<= 0 (:activity-confidence configuration) 1))
      (throw (ex-info "Segment activity confidence must be between zero and one"
                      {:activity-confidence
                       (:activity-confidence configuration)})))
    (doseq [key [:gap-write-density-threshold
                 :gap-peripheral-ratio-threshold
                 :gap-transfer-unit-ratio-threshold
                 :gap-decode-ratio-threshold
                 :routine-pc-relative-threshold]]
      (when-not (and (number? (get configuration key))
                     (<= 0 (get configuration key) 1))
        (throw (ex-info "Gap diagnostic ratio must be between zero and one"
                        {:key key :value (get configuration key)}))))
    (when-not (and (vector? (:expected-parts-per-file configuration))
                   (= 2 (count (:expected-parts-per-file configuration)))
                   (every? pos-int? (:expected-parts-per-file configuration))
                   (<= (first (:expected-parts-per-file configuration))
                       (second (:expected-parts-per-file configuration))))
      (throw (ex-info "Expected parts per file must be an increasing two-item vector"
                      {:expected-parts-per-file
                       (:expected-parts-per-file configuration)})))
    configuration))

