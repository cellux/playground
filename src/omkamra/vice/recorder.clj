(ns omkamra.vice.recorder
  "Raw v2 capture recording: directory reservation, transactional chunk
  commitment, and trace-record ingestion.

  This is the raw-capture storage recorder. It does not own the VICE lifecycle
  or snapshot acquisition; callers supply the boundary snapshot and memory
  images. Every chunk commit is transactional: a
  failure destroys the entire capture directory rather than exposing a partial
  forensic capture."
  (:require [clojure.java.io :as io]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.event :as event]
            [omkamra.vice.screen :as screen]
            [omkamra.vice.store :as store])
  (:import [java.io OutputStream]))

(defn- fail
  [reason data]
  (throw (ex-info "Invalid v2 capture request" (assoc data :reason reason))))

(def ^:private default-boundary {:kind :forced-size :reason :max-events})

(defn- capture-directory
  ^java.io.File [dataset-root]
  (io/file dataset-root "capture"))

(defn start-capture!
  "Reserve a fresh capture directory under `dataset-root` and write a
  `:capturing` manifest.

  Options: `:dataset-root`, `:capture-id`, `:timing-model`, and `:options`.
  An existing finalized capture is never overwritten. A leftover unfinalized
  capture (for example after a host crash) is removed before starting from
  scratch. Returns a capture handle for `commit-chunk!` and
  `finalize-capture!`."
  [{:keys [dataset-root capture-id timing-model options]}]
  (when-not dataset-root
    (fail :missing-dataset-root {}))
  (let [root (io/file dataset-root)
        directory (capture-directory root)]
    (.mkdirs root)
    (when (.exists directory)
      (let [existing (try (store/read-manifest directory)
                          (catch Throwable _ nil))]
        (if (= :finalized (:status existing))
          (fail :capture-already-finalized {:directory (str directory)})
          (store/delete-tree! directory))))
    (let [directory (.getCanonicalPath directory)]
      (.mkdirs (store/chunks-directory directory))
      (let [manifest (store/new-manifest {:capture-id capture-id
                                          :directory directory
                                          :timing-model timing-model
                                          :options options})]
        (store/write-manifest! directory manifest)
        {:capture-id capture-id
         :directory directory
         :manifest-path (.getPath (store/manifest-file directory))
         :state (atom manifest)}))))

(defn abort-capture!
  "Destroy an entire in-progress capture directory."
  [capture]
  (store/delete-tree! (:directory capture))
  (reset! (:state capture) nil)
  nil)

(defn- chunk-artifact-spec
  [number kind artifact]
  (assoc artifact
         :file (str "chunks/" (store/chunk-artifact-name number kind))))

(defn- chunk-artifact-reference
  [number kind]
  {:file (str "chunks/" (store/chunk-artifact-name number kind))})

(defn- screenshot-description
  "Describe the VIC screenshot artifact of a chunk.

  The geometry belongs to the metadata rather than the manifest, so a consumer
  can crop the border without guessing. The frame is the full debug frame."
  [number screenshot]
  (let [{:keys [debug-width debug-height x-offset y-offset inner-width
                inner-height bpp]}
        (:frame screenshot)]
    (when-not (and (pos-int? debug-width) (pos-int? debug-height))
      (fail :invalid-screenshot-frame
            {:debug-width debug-width :debug-height debug-height}))
    {:file (str "chunks/" (store/chunk-artifact-name number :screenshot))
     :width debug-width
     :height debug-height
     :inner-width inner-width
     :inner-height inner-height
     :x-offset x-offset
     :y-offset y-offset
     :bpp bpp}))

(defn- start-screenshot-artifact!
  "Start writing a chunk's PNG artifact on its own thread.

  Encoding and storing a 384x272 frame costs tens of milliseconds. Running it
  on the controller thread would stall trace consumption, so it overlaps the
  event, ram, and mem writes; the result is awaited before the manifest is
  published, keeping the commit transactional."
  [chunks-directory number screenshot]
  (when screenshot
    (let [target (io/file chunks-directory
                          (store/chunk-artifact-name number :screenshot))
          writer (screen/png-writer (:frame screenshot) (:palette screenshot))]
      (future (store/write-artifact! target writer)))))

(defn- do-commit-chunk!
  [capture event-count event-writer ram mem snapshot snapshot-timing boundary
   screenshot]
  (when-not (pos-int? event-count)
    (fail :invalid-event-count {:event-count event-count}))
  (when-not (fn? event-writer)
    (fail :missing-event-writer {}))
  (contract/validate-snapshot! snapshot)
  (when-not (map? snapshot-timing)
    (fail :missing-snapshot-timing {}))
  (let [manifest @(:state capture)
        directory (:directory capture)
        number (inc (count (:chunks manifest)))
        start (:event-count manifest)
        end (+ start event-count)
        chunks-directory (store/chunks-directory directory)
        screenshot-task (start-screenshot-artifact! chunks-directory number
                                                    screenshot)]
    (try
      (let [events-artifact (store/write-artifact!
                             (io/file chunks-directory
                                      (store/chunk-artifact-name number :events))
                             event-writer)
            _ (when-not (= (* contract/event-record-size event-count)
                           (:size events-artifact))
                (fail :event-writer-size-mismatch
                      {:event-count event-count
                       :expected (* contract/event-record-size event-count)
                       :actual (:size events-artifact)}))
            ram-artifact (store/write-image!
                          (io/file chunks-directory
                                   (store/chunk-artifact-name number :ram))
                          ram
                          store/ram-format-descriptor)
            mem-artifact (store/write-image!
                          (io/file chunks-directory
                                   (store/chunk-artifact-name number :mem))
                          mem
                          store/mem-format-descriptor)
            screenshot-artifact (some-> screenshot-task deref)
            metadata {:format contract/chunk-format
                      :capture-id (:capture-id manifest)
                      :chunk-number number
                      :event-range [start end]
                      :event-count event-count
                      :target (get-in manifest [:options :events-per-chunk])
                      :boundary boundary
                      :snapshot snapshot
                      :snapshot-timing snapshot-timing
                      :screenshot (when screenshot
                                    (screenshot-description number screenshot))
                      :schemas (cond-> {:events contract/event-stream-format
                                        :ram contract/ram-image-format
                                        :mem contract/mem-image-format
                                        :snapshot contract/snapshot-format}
                                 screenshot
                                 (assoc :screenshot contract/screen-png-format))
                      :artifacts (cond-> {:events (chunk-artifact-reference number :events)
                                          :ram (chunk-artifact-reference number :ram)
                                          :mem (chunk-artifact-reference number :mem)}
                                   screenshot
                                   (assoc :screenshot
                                          (chunk-artifact-reference number :screenshot)))}
            metadata-path (store/chunk-metadata-path directory number)
            _ (store/atomic-write-edn! metadata-path metadata)
            entry {:number number
                   :file (str "chunks/" (store/chunk-file-name number))
                   :size (.length (io/file metadata-path))
                   :sha256 (store/sha256-file metadata-path)
                   :event-range [start end]
                   :event-count event-count
                   :boundary boundary
                   :artifacts (cond-> {:events (assoc (chunk-artifact-spec number :events
                                                                           events-artifact)
                                                      :records event-count)
                                       :ram (chunk-artifact-spec number :ram ram-artifact)
                                       :mem (chunk-artifact-spec number :mem mem-artifact)}
                                screenshot
                                (assoc :screenshot
                                       (chunk-artifact-spec number :screenshot
                                                            screenshot-artifact)))}
            manifest' (-> manifest
                          (update :chunks conj entry)
                          (assoc :chunk-count number :event-count end))]
        (store/write-manifest! directory manifest')
        (reset! (:state capture) manifest')
        entry)
      (catch Throwable error
        ;; Join a failed or in-flight screenshot write before the capture is
        ;; aborted, so its directory is not recreated while being removed.
        (some-> screenshot-task (deref 5000 nil))
        (throw error)))))

(defn commit-chunk!
  "Commit one non-empty raw chunk transactionally.

  Request keys: `:event-count` (positive), `:event-writer` (a function of an
  `OutputStream` writing exactly `event-count` packed records), `:ram` and
  `:mem` (64 KiB `byte-array`s), `:snapshot` (a canonical snapshot map),
  `:snapshot-timing`, optional `:screenshot` (`{:frame ... :palette ...}` as
  returned by the binary monitor) and optional `:boundary`."
  [capture {:keys [event-count event-writer ram mem snapshot snapshot-timing
                   boundary screenshot]
            :or {boundary default-boundary}}]
  (try
    (do-commit-chunk! capture event-count event-writer ram mem snapshot
                      snapshot-timing boundary screenshot)
    (catch Throwable error
      (abort-capture! capture)
      (throw error))))

(defn finalize-capture!
  "Mark a capture finalized. Only a finalized capture is analyzable."
  [capture]
  (let [manifest' (assoc @(:state capture) :status :finalized)]
    (store/write-manifest! (:directory capture) manifest')
    (reset! (:state capture) manifest')
    {:capture-id (:capture-id capture)
     :directory (:directory capture)
     :manifest-path (:manifest-path capture)
     :chunk-count (:chunk-count manifest')
     :event-count (:event-count manifest')}))

;; Trace ingestion -----------------------------------------------------------

(defn- commit-batch!
  [capture events snapshot-provider boundary]
  (let [chunk-number (inc (count (:chunks @(:state capture))))
        provided (snapshot-provider {:chunk-number chunk-number
                                     :event-count (count events)
                                     :first-event (first events)
                                     :last-event (last events)})]
    (commit-chunk! capture
                   {:event-count (count events)
                    :event-writer (fn [^OutputStream out]
                                    (event/write-events! out events))
                    :ram (:ram provided)
                    :mem (:mem provided)
                    :snapshot (:snapshot provided)
                    :snapshot-timing (:snapshot-timing provided)
                    :screenshot (:screenshot provided)
                    :boundary (or (:boundary provided) boundary)})))

(defn ingest-trace-records!
  "Convert and commit trace records from `read-source!`.

  `read-source!` receives a consume callback and is expected to drive
  `omkamra.vice.trace/reduce-records!` with forensic samples enabled. Events
  are batched to `:events-per-chunk` before each commit. `:snapshot-provider` is
  called for each batch with `{:chunk-number :event-count :first-event
  :last-event}` and returns `{:ram :mem :snapshot :snapshot-timing}`. Any
  failure destroys the entire capture directory and rethrows."
  [capture read-source!
   {:keys [events-per-chunk snapshot-provider boundary]
    :or {events-per-chunk 50000 boundary default-boundary}}]
  (when-not (fn? read-source!)
    (fail :missing-record-source {}))
  (when-not (fn? snapshot-provider)
    (fail :missing-snapshot-provider {}))
  (let [batch (volatile! (transient []))]
    (try
      (read-source!
       (fn [record]
         (let [next-batch (conj! @batch (event/trace-record->event record))]
           (if (>= (count next-batch) events-per-chunk)
             (do
               (commit-batch! capture (persistent! next-batch)
                              snapshot-provider boundary)
               (vreset! batch (transient [])))
             (vreset! batch next-batch)))))
      (let [remaining (persistent! @batch)]
        (when (seq remaining)
          (commit-batch! capture remaining snapshot-provider boundary)))
      capture
      (catch Throwable error
        (abort-capture! capture)
        (throw error)))))
