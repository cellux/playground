(ns omkamra.vice.decoder.video
  "VIC state reconstruction and display-asset projections."
  (:require [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.write :as write]))

(defn vic-register-address
  "Return the canonical VIC-II register address decoded by `address`, or nil.

  VIC-II registers at `$d000-$d02e` repeat every `$40` bytes throughout
  `$d000-$d3ff`; demos sometimes deliberately use those mirrors."
  [address]
  (when (<= 0xd000 address 0xd3ff)
    (let [offset (bit-and address 0x3f)]
      (when (<= offset 0x2e)
        (+ 0xd000 offset)))))

(def ^:private vic-register-addresses
  ;; `$dd00` is CIA2, but it selects the VIC memory bank and therefore belongs
  ;; to the initial state used for VIC configuration reconstruction.
  (conj (set (range 0xd000 0xd02f)) 0xdd00))

(defn sid-register-address?
  [address]
  ;; The SID register select lines repeat every $20 bytes in its `$d400-$d7ff`
  ;; I/O decode range. `$d41d-$d41f` are unused on a 6581/8580, but remain
  ;; SID-decoded addresses and are retained as SID-domain evidence.
  (<= 0xd400 address 0xd7ff))

(defn cia1-register-address?
  [address]
  ;; Both CIAs decode only four low address lines, so their sixteen registers
  ;; repeat through their respective 256-byte I/O pages.
  (<= 0xdc00 address 0xdcff))

(defn cia2-register-address?
  [address]
  (<= 0xdd00 address 0xddff))

(defn vic-state-register-address
  "Return the canonical state address relevant to VIC reconstruction.

  This includes VIC mirrors and CIA2 port A (`$dd00`), whose mirrors select
  the active VIC bank. Other CIA2 registers are peripheral evidence but do not
  alter the VIC configuration state."
  [address]
  (or (vic-register-address address)
      (when (and (cia2-register-address? address)
                 (zero? (bit-and address 0x0f)))
        0xdd00)))

(def ^:private layout-register-addresses
  #{0xd011 0xd018 0xdd00})

(defn vic-register-name
  [address]
  (keyword (format "%04x" address)))

(defn vic-state
  [memory]
  (into (sorted-map)
        (map (fn [address] [address (write/memory-byte memory address)]))
        vic-register-addresses))

(defn vic-configuration
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

(defn derive-vic
  [initial-memory writes]
  (let [initial-state (vic-state initial-memory)
        {:keys [state events configurations]}
        (reduce
         (fn [{:keys [state events configurations] :as result} write]
           (if-let [register-address (vic-state-register-address (:address write))]
             (let [state (assoc state register-address (:value write))
                   event (assoc write
                                :register (vic-register-name register-address)
                                :register-address register-address)]
               (assoc result
                      :state state
                      :events (conj events event)
                      :configurations
                      (if (contains? layout-register-addresses register-address)
                        (conj configurations
                              (merge (select-keys write [:event-index :pc
                                                         :raster-line :cpu-cycle])
                                     (vic-configuration state)))
                        configurations)))
             result))
         {:state initial-state
          :events []
          :configurations [(assoc (vic-configuration initial-state)
                                  :event-index -1)]}
         writes)]
    {:initial-state initial-state
     :final-state state
     :writes events
     :configurations configurations}))

(defn sprite-pointer-events
  [initial-memory writes]
  (let [initial-state (vic-state initial-memory)]
    (:events
     (reduce
      (fn [{:keys [state events] :as result} write]
        (let [{:keys [screen-base bank-base] :as configuration}
              (vic-configuration state)
              address (:address write)
              offset (- address screen-base)
              event (when (<= 0x3f8 offset 0x3ff)
                      (assoc write
                             :slot (- offset 0x3f8)
                             :pointer (:value write)
                             :sprite-address (+ bank-base (* 64 (:value write)))
                             :configuration configuration))
              register-address (vic-state-register-address address)
              state (if register-address
                      (assoc state register-address (:value write))
                      state)]
          (assoc result :state state :events (cond-> events event (conj event)))))
      {:state initial-state :events []}
      writes))))

(defn memory-after-writes
  [initial-memory writes event-index]
  (let [memory (byte-array (map unchecked-byte initial-memory))]
    (doseq [{:keys [address value]} (take-while #(<= (:event-index %) event-index)
                                                writes)]
      (aset-byte memory address (unchecked-byte value)))
    memory))

(defn memory-range
  [memory start length]
  (mapv #(write/memory-byte memory (+ start %)) (range length)))

(defn asset-sample
  [initial-memory writes-source configuration]
  (let [memory (memory-after-writes initial-memory
                                    (writes-source)
                                    (:event-index configuration))
        {:keys [bank-base screen-base charset-base bitmap? bitmap-base]} configuration
        pointers (memory-range memory (+ screen-base 0x3f8) 8)]
    (merge configuration
           {:screen {:address screen-base :data (memory-range memory screen-base 1024)}
            :charset {:address charset-base :data (memory-range memory charset-base 2048)}
            :color-ram {:address 0xd800 :data (memory-range memory 0xd800 1000)}
            :sprites (mapv (fn [slot pointer]
                             {:slot slot
                              :pointer pointer
                              :address (+ bank-base (* pointer 64))
                              :data (memory-range memory (+ bank-base (* pointer 64)) 64)})
                           (range 8) pointers)}
           (when bitmap?
             {:bitmap {:address bitmap-base :data (memory-range memory bitmap-base 8192)}}))))

(defn asset-samples
  [initial-memory writes-source configurations]
  ;; Fine-scroll changes in D011/D016 do not alter any asset address.  Only
  ;; snapshot when the address layout changes, while retaining every VIC write
  ;; in :vic/:writes for raster-precise scroll/border analysis.
  (->> configurations
       (partition-by #(select-keys % [:bank-base :screen-base :charset-base
                                      :bitmap? :bitmap-base]))
       (map first)
       (mapv #(asset-sample initial-memory writes-source %))))

(defn derive-writes-chunk
  "Build a compact reusable-write descriptor for one immutable raw chunk.

  The raw chunk remains the sole persisted owner of compact write records.
  This stage records a versioned reference rather than duplicating millions of
  expanded write maps in EDN. Analysis workers materialize the descriptor once
  per raw chunk only when a dependent stage needs the records."
  [chunk]
  (assoc (artifact/derived-chunk-source :omkamra.vice/writes-chunk-v2 chunk)
         :stages {:writes {:source {:stage :memory
                                    :key :writes
                                    :format (get-in chunk
                                                    [:stages :memory :writes
                                                     :format])}}}))

(defn writes-source
  "Return a fresh, non-retaining write-record source for one raw chunk.

  The returned function creates a lazy expanded sequence for each pass. This
  lets video and asset analysis stream the compact records without retaining a
  full expanded vector in the chunk worker heap."
  [raw-chunk writes-chunk]
  (or (::write-source writes-chunk)
      (let [{:keys [stage key]} (get-in writes-chunk [:stages :writes :source])]
        (when-not (= [:memory :writes] [stage key])
          (throw (ex-info "Unsupported writes-stage source"
                          {:source (get-in writes-chunk
                                           [:stages :writes :source])
                           :chunk-number (:chunk-number raw-chunk)})))
        (let [compact-writes (get-in raw-chunk [:stages :memory :writes])]
          (fn [] (write/write-records compact-writes))))))

(defn derive-video-chunk
  "Build the VIC timeline for one immutable raw chunk.

  The optional `writes` argument accepts already-expanded write records so a
  broadcast analysis pipeline can share them with the dependent asset stage.
  The result is intentionally free of extracted assets; those are produced by
  `derive-assets-chunk` after this stage has persisted its configurations."
  ([chunk]
   (derive-video-chunk chunk nil))
  ([chunk writes]
   (let [memory-stages (get-in chunk [:stages :memory])
         writes-source (if (fn? writes)
                         writes
                         (constantly (or writes
                                         (write/expand-writes (:writes memory-stages)))))
         vic (derive-vic (:initial memory-stages) (writes-source))]
     (assoc (artifact/derived-chunk-source :omkamra.vice/video-chunk-v1 chunk)
            :stages {:video {:vic (assoc (dissoc vic :writes)
                                         :sprite-pointer-writes
                                         (sprite-pointer-events
                                          (:initial memory-stages)
                                          (writes-source)))}}))))

(defn derive-assets-chunk
  "Build replayed display assets using a persisted video-stage chunk.

  The optional `writes` argument accepts the expanded writes retained by an
  in-flight video stage. When omitted, the persisted compact write records are
  expanded here for standalone asset-stage requests."
  ([chunk video-chunk]
   (derive-assets-chunk chunk video-chunk nil))
  ([chunk video-chunk writes]
   (let [memory-stages (get-in chunk [:stages :memory])
         writes-source (if (fn? writes)
                         writes
                         (constantly (or writes
                                         (write/expand-writes (:writes memory-stages)))))
         configurations (get-in video-chunk [:stages :video :vic :configurations])]
     (assoc (artifact/derived-chunk-source :omkamra.vice/assets-chunk-v1 chunk)
            :stages {:assets {:samples (asset-samples (:initial memory-stages)
                                                      writes-source
                                                      configurations)}}))))

