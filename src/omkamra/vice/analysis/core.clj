(ns omkamra.vice.analysis.core
  "Shared analysis-dataset manifest and stage-generation helpers.

  Every v2 analysis stage publishes an immutable generation below
  `<dataset-root>/analysis/<stage>/<generation>/` and then atomically updates
  the single `analysis/manifest.edn` commit log. A stage entry records the
  source capture identity/checksum, the stage version and configuration, the
  generation directory, and a summary. A failed build removes its generation
  and never touches the manifest.

  Dependency-closure execution and reuse invalidation live in
  `omkamra.vice.pipeline`, which builds on the commit primitive here."
  (:require [clojure.java.io :as io]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.store :as store])
  (:import [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]
           [java.time Instant]
           [java.util UUID]))

(defn manifest-path
  ^java.io.File [dataset-root]
  (io/file dataset-root "analysis" "manifest.edn"))

(def ^:dynamic *transaction*
  "When bound to an atom holding a `{:stages {stage entry}}` map, stage
  commits are staged there instead of being written to `analysis/manifest.edn`,
  and `read-manifest`/`stage-entry` see the staged view. `pipeline/build!` binds
  this for one invocation, so every newly completed pointer is published in a
  single atomic rewrite and a failed invocation publishes nothing."
  nil)

(defn- file-manifest
  "Read the on-disk analysis manifest, ignoring any transaction."
  [dataset-root]
  (let [file (manifest-path dataset-root)]
    (when (.isFile file) (store/read-edn file))))

(defn read-manifest
  "Read a dataset's analysis manifest, or nil when none exists.

  Within a transaction the staged stage pointers are merged over the file
  manifest, so a caller observes the invocation's would-be committed state.
  The file manifest is read exactly once per call, so a reader pins the
  generation set it resolved against a single snapshot."
  [dataset-root]
  (let [base (file-manifest dataset-root)]
    (if (nil? *transaction*)
      base
      (let [staged (:stages @*transaction*)]
        (if (seq staged)
          (update (or base {}) :stages merge staged)
          base)))))

(defn stage-entry
  "Return the committed entry for `stage-id`, or nil."
  [dataset-root stage-id]
  (get-in (read-manifest dataset-root) [:stages stage-id]))

(defn stage-directory
  "Resolve a committed stage entry to its absolute generation directory."
  [dataset-root entry]
  (io/file dataset-root (:directory entry)))

(defn capture-manifest-sha
  "SHA-256 of a capture's authoritative `manifest.edn`."
  [capture-directory]
  (store/sha256-file (store/manifest-file capture-directory)))

(defn new-generation!
  "Create a fresh generation directory for `stage-id`.

  Returns `{:generation name :directory file :relative path}`. The directory is
  private until `commit-stage!` publishes it."
  [dataset-root stage-id]
  (let [generation (str (System/currentTimeMillis) "-"
                        (subs (str (UUID/randomUUID)) 0 8))
        relative (str "analysis/" (name stage-id) "/" generation)
        directory (io/file dataset-root relative)]
    (.mkdirs directory)
    {:generation generation
     :directory directory
     :relative relative}))

;; JVM-wide lock serialising the analysis-manifest read-modify-write.
;; Stages run concurrently under parallel pipeline scheduling, and a stage's
;; own `build!` may commit at the same time the pipeline adds its signature, so
;; every writer must be serialised or an update can be lost.
(defonce ^:private manifest-lock (Object.))

(defn with-manifest-lock
  "Run `f` while holding the dataset's analysis-manifest write lock.

  Writes are serialised within this JVM by a private monitor and across JVMs by
  an OS-visible `FileLock` on `analysis/.lock`, so two JVMs cannot interleave a
  manifest read-modify-write (creating the lock file also creates the
  `analysis/` directory). Not reentrant: do not call `commit-stage!` or
  `publish!` from inside `f`."
  [dataset-root f]
  (locking manifest-lock
    (let [directory (io/file dataset-root "analysis")
          _ (.mkdirs directory)
          lock-file (io/file directory ".lock")
          options (into-array OpenOption [StandardOpenOption/CREATE
                                          StandardOpenOption/WRITE])]
      (with-open [channel (FileChannel/open (.toPath lock-file) options)]
        (let [lock (.lock channel)]
          (try (f) (finally (.release lock))))))))

(defn commit-stage!
  "Add or replace `stage-id`'s pointer in the analysis manifest.

  Outside a transaction the manifest is rewritten atomically while holding the
  dataset write lock (`with-manifest-lock`). Within a transaction
  (`*transaction*`) the entry is staged and nothing is written until `publish!`,
  which is how the pipeline makes a whole invocation all-or-nothing. The
  generation named by `entry` must already be fully written."
  [dataset-root stage-id entry]
  (if (nil? *transaction*)
    (with-manifest-lock
      dataset-root
      (fn []
        (let [existing (or (file-manifest dataset-root) {})
              manifest (-> existing
                           (assoc :format contract/analysis-format
                                  :capture-id (or (:capture-id existing)
                                                  (:capture-id entry))
                                  :capture-manifest-sha256
                                  (or (:capture-manifest-sha256 existing)
                                      (:capture-manifest-sha256 entry))
                                  :updated-at (str (Instant/now)))
                           (update :stages (fnil assoc {}) stage-id entry))]
          (store/atomic-write-edn! (manifest-path dataset-root) manifest)
          manifest)))
    (do (swap! *transaction* update-in [:stages stage-id] (fn [_] entry))
        (read-manifest dataset-root))))

(defn publish!
  "Atomically merge a transaction's staged stage pointers into the manifest.

  Called once per successful invocation, after every staged generation is
  complete, so all newly completed pointers become visible together. Hold the
  dataset write lock across the rewrite. Returns the new manifest, or the
  effective manifest when nothing was staged."
  [dataset-root transaction]
  (let [staged (:stages @transaction)]
    (if (empty? staged)
      (read-manifest dataset-root)
      (with-manifest-lock
        dataset-root
        (fn []
          (let [existing (or (file-manifest dataset-root) {})
                sample (val (first staged))
                manifest (-> existing
                             (assoc :format contract/analysis-format
                                    :capture-id (or (:capture-id existing)
                                                    (:capture-id sample))
                                    :capture-manifest-sha256
                                    (or (:capture-manifest-sha256 existing)
                                        (:capture-manifest-sha256 sample))
                                    :updated-at (str (Instant/now)))
                             (update :stages (fnil merge {}) staged))]
            (store/atomic-write-edn! (manifest-path dataset-root) manifest)
            manifest))))))

(defn delete-generation!
  "Delete an unpublished generation directory."
  [directory]
  (store/delete-tree! (io/file directory)))

;; Progress handles ----------------------------------------------------------

(defn report!
  "Merge `data` into a stage progress handle atom, if one was supplied."
  [handle data]
  (when handle (swap! handle merge data)))

(defn start-async!
  "Run `build-fn` on a background future and return a deref-able handle atom.

  `build-fn` receives the handle and should call `report!` as it progresses.
  Dereferencing the returned atom yields the current progress map; on
  completion it gains `:status :complete` and `:result`, and on failure
  `:status :failed` and `:error`. While running it carries `:future` so a
  caller can cancel the run; the future is dropped on completion so a retained
  handle does not pin the build callable or its captured state.
  `build-fn` should return a lightweight result (see `summarize-result`)."
  [init build-fn]
  (let [handle (atom (assoc init :status :running
                            :started-at (System/currentTimeMillis)))
        worker (future
                 (try
                   (let [result (build-fn handle)]
                     (swap! handle assoc
                            :status :complete
                            :result result
                            :finished-at (System/currentTimeMillis)
                            :future nil))
                   (catch Throwable error
                     (swap! handle assoc
                            :status :failed
                            :error (.getMessage error)
                            :finished-at (System/currentTimeMillis)
                            :future nil))))]
    (swap! handle assoc :future worker)
    handle))

(defn summarize-result
  "Return a lightweight build result for a progress handle.

  Progress handles may be kept by callers for a long time, so they must not
  retain the heavy per-stage payload (full unit lists, span lists, or window
  indexes). Keep only the generation location and the compact summary/counts."
  [result]
  (let [index (:index result)]
    (cond-> {:dataset-root (:dataset-root result)
             :directory (:directory result)}
      (:summary index) (assoc :summary (:summary index))
      (:counts index) (assoc :counts (:counts index))
      (:event-count index) (assoc :event-count (:event-count index))
      (:generation index) (assoc :generation (:generation index)))))

(defn cancel-async!
  "Request cancellation of a `start-async!` run. Returns true when the
  underlying future accepted the request."
  [handle]
  (if-let [worker (:future @handle)]
    (future-cancel worker)
    false))
