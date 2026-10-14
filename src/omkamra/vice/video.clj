(ns omkamra.vice.video
  "The exact `:video` VIC projection.

  `:video` is an exact raw-capture consumer. `build!` scans every canonical VIC
  state write through the shared capture reader, derives frame boundaries from
  raster-line wraps, applies the capture-level VIC value blacklist, and
  deduplicates repeated ordered write shapes across frames.

  A *pattern* is an ordered vector of `[canonical-register event-offset
  cycle-offset]` triples relative to the frame base. It excludes absolute
  global cycles, values, and dedupe ids. A *frame* stores its base global
  event/cycle, the pattern id, and the non-blacklisted value vector, so a
  blacklisted register expands as explicitly unknown rather than as zero or an
  omitted write.

  The stage also publishes an incremental layout configuration point whenever
  the VIC memory bank, screen base, charset base, bitmap mode, or bitmap base
  changes. Those points feed `:assets`.

  Generation layout:

  ```text
  analysis/video/<generation>/
    index.edn    ; patterns, configurations, blacklist, timing model, counts
    frames.edn   ; chronological frame records
  ```"
  (:require [clojure.java.io :as io]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store]))

(def video-version 1)
(def video-format :omkamra.vice/video-v2)

(def default-configuration {})

;; VIC-II register decoding (exact mirror semantics) -------------------------

(defn vic-register-address
  "Return the canonical VIC-II register address decoded by `address`, or nil.

  VIC-II registers `$d000-$d02e` repeat every `$40` bytes through
  `$d000-$d3ff`."
  [address]
  (when (<= 0xd000 address 0xd3ff)
    (let [offset (bit-and (int address) 0x3f)]
      (when (<= offset 0x2e)
        (+ 0xd000 offset)))))

(defn vic-state-register-address
  "Return the canonical state address relevant to VIC reconstruction.

  This includes VIC mirrors and CIA2 port A (`$dd00`), whose low two bits
  select the active VIC bank."
  [address]
  (or (vic-register-address address)
      (when (and (<= 0xdd00 address 0xddff)
                 (zero? (bit-and (int address) 0x0f)))
        0xdd00)))

(defn vic-configuration
  "Reconstruct the VIC layout/scroll configuration from a register state map."
  [state]
  (let [d011 (get state 0xd011 0)
        d016 (get state 0xd016 0)
        d018 (get state 0xd018 0)
        dd00 (get state 0xdd00 0)
        bank-base (* 0x4000 (- 3 (bit-and dd00 0x03)))
        bitmap? (pos? (bit-and d011 0x20))]
    {:d011 d011
     :d016 d016
     :d018 d018
     :dd00 dd00
     :bank-base bank-base
     :screen-base (+ bank-base (* 0x0400 (bit-shift-right d018 4)))
     :charset-base (+ bank-base (* 0x0800 (bit-shift-right (bit-and d018 0x0e) 1)))
     :bitmap? bitmap?
     :bitmap-base (+ bank-base (if (pos? (bit-and d018 0x08)) 0x2000 0))
     :x-scroll (bit-and d016 0x07)
     :y-scroll (bit-and d011 0x07)
     :display-enabled? (pos? (bit-and d011 0x10))
     :multicolor? (pos? (bit-and d016 0x10))}))

(defn- layout-keys
  [configuration]
  (select-keys configuration
               [:bank-base :screen-base :charset-base :bitmap? :bitmap-base]))

(defn- initial-state
  "Seed the VIC register state from the capture's initial snapshot."
  [snapshot]
  (let [vic (:vic (:registers snapshot))
        cia2 (:cia2 (:registers snapshot))]
    (into {}
          (concat (map-indexed (fn [offset value] [(+ 0xd000 offset) value]) vic)
                  {0xdd00 (first cia2)}))))

;; Build ---------------------------------------------------------------------

(defn- close-frame!
  [frame pattern-ids patterns frames counter]
  (when-let [{:keys [base-event base-cycle elements values]} @frame]
    (let [id (or (get @pattern-ids elements)
                 (let [id (count @patterns)]
                   (vswap! patterns conj elements)
                   (vswap! pattern-ids assoc elements id)
                   id))]
      (vswap! frames conj {:frame-id @counter
                           :base-event base-event
                           :base-cycle base-cycle
                           :pattern id
                           :values values})
      (vswap! counter inc)
      (vreset! frame nil))))

(defn- scan-vic-writes
  [reader blacklist initial scan-end]
  (let [state (volatile! initial)
        pattern-ids (volatile! {})
        patterns (volatile! [])
        frames (volatile! [])
        frame (volatile! nil)
        counter (volatile! 0)
        last-raster (volatile! nil)
        initial-configuration (vic-configuration initial)
        configurations (volatile! [{:event-index -1
                                    :base-cycle nil
                                    :config initial-configuration}])
        previous-layout (volatile! (layout-keys initial-configuration))]
    (reader/scan-writes
     reader [0 scan-end]
     (fn [_ write]
       (when-let [register (vic-state-register-address (:address write))]
         (let [raster (:raster-line write)
               current @frame]
           (when (and current
                      (some? @last-raster)
                      (some? raster)
                      (< raster @last-raster))
             (close-frame! frame pattern-ids patterns frames counter))
           (when (nil? @frame)
             (vreset! frame {:base-event (:event-index write)
                             :base-cycle (:global-cycle write)
                             :elements []
                             :values []}))
           (let [{:keys [base-event base-cycle]} @frame]
             (vswap! frame update :elements conj
                     [register
                      (- (:event-index write) base-event)
                      (- (:global-cycle write) base-cycle)])
             (vswap! frame update :values conj
                     (if (contains? blacklist register) nil (:value write))))
           (vreset! last-raster raster)
           (vswap! state assoc register (:value write))
           (let [configuration (vic-configuration @state)
                 layout (layout-keys configuration)]
             (when (not= layout @previous-layout)
               (vswap! configurations conj
                       {:event-index (:event-index write)
                        :base-cycle (:global-cycle write)
                        :raster-line raster
                        :cpu-cycle (:cpu-cycle write)
                        :pc (:pc write)
                        :config configuration})
               (vreset! previous-layout layout)))))
       nil)
     nil)
    (close-frame! frame pattern-ids patterns frames counter)
    {:patterns @patterns
     :frames @frames
     :configurations @configurations}))

(defn- vic-write-count [patterns frames]
  (reduce + 0 (map #(count (nth patterns (:pattern %))) frames)))

(defn- write-generation!
  [directory generation capture-id capture-sha blacklist timing-model result
   configuration]
  (let [{:keys [patterns frames configurations]} result
        index {:format video-format
               :version video-version
               :generation generation
               :capture-id capture-id
               :capture-manifest-sha256 capture-sha
               :blacklist (vec (sort blacklist))
               :timing-model timing-model
               :configuration configuration
               :patterns patterns
               :configurations configurations
               :summary {:frame-count (count frames)
                         :pattern-count (count patterns)
                         :configuration-count (count configurations)
                         :vic-write-count (vic-write-count patterns frames)}}]
    (with-open [writer (io/writer (io/file directory "frames.edn"))]
      (binding [*out* writer]
        (pr frames)
        (newline)))
    (store/atomic-write-edn! (io/file directory "index.edn") index)
    index))

(defn build!
  "Build and commit a `:video` generation for a finalized capture.

  `reader` is a `omkamra.vice.reader/open!` handle. Options: `:configuration`
  (recorded in the manifest). Returns the generation directory and index."
  ([reader] (build! reader {}))
  ([reader {:keys [configuration max-events]
            :or {configuration default-configuration}}]
   (let [dataset-root (reader/dataset-root reader)
         capture-id (reader/capture-id reader)
         capture-sha (analysis/capture-manifest-sha (reader/directory reader))
         blacklist (set (get-in (reader/manifest reader)
                                [:options :vic-value-blacklist]))
         timing-model (reader/timing-model reader)
         initial (initial-state (reader/boundary-snapshot reader 0))
         scan-end (or max-events (reader/event-count reader))
         {:keys [generation directory relative]}
         (analysis/new-generation! dataset-root :video)]
     (try
       (let [result (scan-vic-writes reader blacklist initial scan-end)
             index (write-generation! directory generation capture-id
                                      capture-sha blacklist timing-model result
                                      configuration)]
         (analysis/commit-stage!
          dataset-root :video
          {:generation generation
           :directory relative
           :version video-version
           :capture-id capture-id
           :capture-manifest-sha256 capture-sha
           :configuration configuration
           :summary (:summary index)})
         {:dataset-root dataset-root
          :directory (str directory)
          :index index})
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open!
  "Open the committed `:video` generation for a dataset root."
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :video)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :video generation"
                      {:reason :missing-video-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= video-format (:format index))
        (throw (ex-info "Not a v2 video image"
                        {:reason :not-a-video-image :format (:format index)})))
      (let [frames (store/read-edn (io/file directory "frames.edn"))]
        {:dataset-root dataset-root
         :directory directory
         :index index
         :patterns (:patterns index)
         :configurations (:configurations index)
         :blacklist (set (:blacklist index))
         :frames frames}))))

(defn status
  "Return the committed video stage entry for a dataset, or nil."
  [dataset-root]
  (analysis/stage-entry dataset-root :video))

(defn frames
  "Return the chronological frame records, optionally restricted to frames
  whose base event index is in the half-open `event-range`."
  ([video] (:frames video))
  ([video [start end]]
   (filterv #(and (<= start (:base-event %)) (< (:base-event %) end))
            (:frames video))))

(defn configurations
  "Return the incremental VIC layout configuration points."
  [video]
  (:configurations video))

(defn expand-frame
  "Expand one frame record into ordered VIC write maps.

  Blacklisted values expand with `:value nil` and `:value-known? false`."
  [video frame]
  (let [pattern (nth (:patterns video) (:pattern frame))]
    (mapv (fn [[register event-offset cycle-offset] value]
            {:frame-id (:frame-id frame)
             :register register
             :event-index (+ (:base-event frame) event-offset)
             :global-cycle (+ (:base-cycle frame) cycle-offset)
             :value value
             :value-known? (some? value)})
          pattern
          (:values frame))))

(defn vic-writes
  "Return every VIC write as an expanded ordered sequence."
  [video]
  (mapcat #(expand-frame video %) (:frames video)))
