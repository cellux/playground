(ns omkamra.vice.assets
  "The exact `:assets` display-asset projection.

  `:assets` consumes the incremental VIC layout configuration points published
  by `:video` and, for each one, replays exact underlying RAM and color RAM
  through the shared capture reader. It snapshots the initial configuration and
  every effective asset-layout change (bank, screen base, charset base, bitmap
  mode/base); ordinary animation does not create a snapshot.

  Every snapshot is a fixed binary payload of 65536 bytes underlying RAM
  followed by 1024 bytes color RAM, appended to `assets.bin`. The EDN index
  records the payload offset/length, the VIC configuration, and the screen,
  charset, color-RAM, sprite-pointer, sprite-data, and bitmap memory ranges.
  Large byte arrays are never embedded in EDN.

  Generation layout:

  ```text
  analysis/assets/<generation>/
    index.edn
    assets.bin
  ```"
  (:require [clojure.java.io :as io]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store]
            [omkamra.vice.video :as video])
  (:import [java.io BufferedOutputStream FileOutputStream RandomAccessFile]))

(def assets-version 1)
(def assets-format :omkamra.vice/assets-v2)
(def payload-size (+ contract/memory-image-size contract/color-ram-size))

(def default-configuration {})

(defn- payload-bytes
  ^bytes [state]
  (let [ram ^bytes (:ram state)
        color ^bytes (:color-ram state)
        out (byte-array payload-size)]
    (System/arraycopy ram 0 out 0 contract/memory-image-size)
    (System/arraycopy color 0 out contract/memory-image-size
                      contract/color-ram-size)
    out))

(defn- snapshot-entry
  [snapshot-id point payload offset]
  (let [config (:config point)
        {:keys [bank-base screen-base charset-base bitmap? bitmap-base]} config
        pointers (mapv #(contract/unsigned
                         (aget ^bytes payload (+ screen-base 0x3f8 %)))
                       (range 8))]
    {:snapshot-id snapshot-id
     :event-index (:event-index point)
     :base-cycle (:base-cycle point)
     :timing (select-keys point [:base-cycle :raster-line :cpu-cycle])
     :payload {:offset offset :length payload-size}
     :config config
     :ranges {:screen {:address screen-base :length 1024}
              :charset {:address charset-base :length 2048}
              :color-ram {:address 0xd800 :length contract/color-ram-size}
              :sprite-pointers {:address (+ screen-base 0x3f8) :length 8}
              :bitmap (when bitmap? {:address bitmap-base :length 8192})}
     :sprites (mapv (fn [slot pointer]
                      {:slot slot
                       :pointer pointer
                       :address (+ bank-base (* pointer 64))
                       :length 64})
                    (range 8)
                    pointers)}))

(defn build!
  "Build and commit an `:assets` generation from a `:video` image.

  `reader` is a `omkamra.vice.reader/open` handle and `video` is an opened
  `omkamra.vice.video` image. Options: `:configuration`."
  ([reader video] (build! reader video {}))
  ([reader video {:keys [configuration] :or {configuration default-configuration}}]
   (let [dataset-root (reader/dataset-root reader)
         capture-id (reader/capture-id reader)
         capture-sha (analysis/capture-manifest-sha (reader/directory reader))
         configs (vec (video/configurations video))
         replay-indexes (mapv (fn [point]
                                (let [event-index (:event-index point)]
                                  (if (neg? (long event-index))
                                    0
                                    (inc event-index))))
                              configs)
         {:keys [generation directory relative]}
         (analysis/new-generation dataset-root :assets)]
     (try
       (with-open [out (BufferedOutputStream.
                        (FileOutputStream. (io/file directory "assets.bin")))]
         (let [[snapshots offset]
               (reader/scan-replay-states
                reader replay-indexes
                (fn [[snapshots offset] state]
                  (let [point (nth configs (count snapshots))
                        payload (payload-bytes state)
                        snapshot (snapshot-entry (count snapshots) point
                                                 payload offset)]
                    (.write out payload 0 (alength payload))
                    [(conj snapshots snapshot) (+ offset (alength payload))]))
                [[] 0])
               index {:format assets-format
                      :version assets-version
                      :generation generation
                      :capture-id capture-id
                      :capture-manifest-sha256 capture-sha
                      :configuration configuration
                      :payload-file "assets.bin"
                      :payload-size payload-size
                      :snapshot-count (count snapshots)
                      :snapshots snapshots}]
           (store/atomic-write-edn! (io/file directory "index.edn") index)
           (analysis/commit-stage!
            dataset-root :assets
            {:generation generation
             :directory relative
             :version assets-version
             :capture-id capture-id
             :capture-manifest-sha256 capture-sha
             :configuration configuration
             :summary {:snapshot-count (count snapshots)
                       :payload-bytes offset}})
           {:dataset-root dataset-root
            :directory (str directory)
            :index index}))
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open
  "Open the committed `:assets` generation for a dataset root."
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :assets)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :assets generation"
                      {:reason :missing-assets-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= assets-format (:format index))
        (throw (ex-info "Not a v2 assets image"
                        {:reason :not-an-assets-image :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index
       :snapshots (:snapshots index)})))

(defn status
  "Return the committed assets stage entry for a dataset, or nil."
  [dataset-root]
  (analysis/stage-entry dataset-root :assets))

(defn snapshots
  "Return the asset snapshot index entries."
  [assets]
  (:snapshots assets))

(defn read-payload
  "Read the fixed RAM+color-RAM payload of one snapshot."
  ^bytes [assets snapshot-id]
  (let [snapshot (nth (:snapshots assets) snapshot-id)
        {:keys [offset length]} (:payload snapshot)
        file (io/file (:directory assets) (get-in assets [:index :payload-file]))
        bytes (byte-array length)]
    (with-open [raf (RandomAccessFile. file "r")]
      (.seek raf offset)
      (.readFully raf bytes))
    bytes))

(defn snapshot-ram
  "Return the 64 KiB underlying-RAM part of a snapshot payload."
  [assets snapshot-id]
  (let [payload (read-payload assets snapshot-id)]
    (java.util.Arrays/copyOfRange payload 0 (int contract/memory-image-size))))

(defn snapshot-color-ram
  "Return the 1 KiB color-RAM part of a snapshot payload."
  [assets snapshot-id]
  (let [payload (read-payload assets snapshot-id)]
    (java.util.Arrays/copyOfRange payload (int contract/memory-image-size)
                                  (int payload-size))))
