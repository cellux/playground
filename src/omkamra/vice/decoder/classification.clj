(ns omkamra.vice.decoder.classification
  "Cross-chunk semantic classification from bounded feature evidence."
  (:require [clojure.set :as set]
            [omkamra.vice.decoder.artifact :as artifact]))

(def default-classifier-configuration
  "Default conservative offline classifier settings.

  Hysteresis is measured in evidence units, normally IRQ frames. Chunks that
  contain no IRQ frame evidence contribute one synthetic unit, allowing loader
  and decrunch activity to cross boot/loader chunk boundaries."
  {:enter-confidence 0.6
   :exit-confidence 0.35
   :hysteresis-units 2
   :lookback-units 8
   :demopart-stability-units 4
   :evidence-window-units 8
   :loader-iec-write-threshold 1
   ;; IEC register writes alone are common in raster/peripheral loops. Treat
   ;; them as custom-loader evidence only when they coincide with a bounded
   ;; transfer-like RAM destination, otherwise require KERNAL IEC execution or
   ;; a KERNAL transfer call.
   :loader-iec-only-write-threshold 16
   :loader-iec-only-destination-threshold 128
   :loader-iec-only-ram-write-threshold 256
   :loader-kernal-transfer-threshold 1
   :loader-kernal-execution-threshold 1
   ;; Require two independent strong signals before a decruncher candidate
   ;; can reach the default enter threshold. Ordinary copy loops often write
   ;; many bytes into previously executed areas without being decrunchers.
   :decrunch-write-threshold 10000
   :decrunch-overwrite-threshold 2000
   :decrunch-density-threshold 0.35
   :decrunch-unique-address-threshold 2048
   :decrunch-frontier-threshold 256
   :decrunch-expansion-threshold 256
   :decrunch-written-code-execution-threshold 1
   ;; A repeatedly rewritten screen/bitmap footprint is evidence against a
   ;; decruncher even when its raw write rate is high.
   :decrunch-repeated-footprint-penalty 0.6
   :self-modifying-raster-stability-units 2
   :self-modifying-raster-range-overlap 0.85
   :self-modifying-raster-execution-threshold 1
   ;; General hardware-traffic mode. A unit is classified by where its writes
   ;; go (VIC / ordinary RAM / CIA IEC / SID) rather than by code patterns, so
   ;; transfers, decrunching, raster effects, and music separate uniformly.
   ;; A unit that performs this many serial-register accesses is driving the
   ;; bus, whereas an occasional `$dd00` VIC-bank store is not. Accesses rather
   ;; than writes, because a load samples the bus by *reading* `$dd00`.
   :traffic-mode-min-writes 32
   :traffic-mode-share-threshold 0.5
   :traffic-transfer-unit-accesses 8
   ;; Destination-phase detection. A decruncher is a *phase* of consecutive
   ;; RAM-dominant, serial-free units whose combined destination footprint is
   ;; broad and contiguous, not a property of any single 10k-event window.
   ;; These thresholds are deliberately about the accumulated region.
   :decruncher-phase-ram-write-floor 512
   :decruncher-phase-min-units 2
   :decruncher-phase-gap-units 4
   :decruncher-destination-pages 16
   :decruncher-destination-contiguous-pages 8
   :decruncher-destination-unique-addresses 2048
   ;; A transfer phase is a run of units that drive the serial bus. It is
   ;; promoted to a loader transition only after enough bus activity, so the
   ;; occasional `$dd00` VIC-bank store or KERNAL tick is not a load.
   :loader-phase-serial-threshold 4096
   :loader-phase-gap-units 4})

(defn normalize-classifier-configuration
  "Merge and validate classifier configuration for persisted analysis."
  [configuration]
  (when-not (map? configuration)
    (throw (ex-info "Classifier configuration must be a map"
                    {:configuration configuration})))
  (let [configuration (merge default-classifier-configuration configuration)]
    (doseq [key [:hysteresis-units :lookback-units
                 :demopart-stability-units :evidence-window-units
                 :loader-iec-write-threshold
                 :loader-iec-only-write-threshold
                 :loader-iec-only-destination-threshold
                 :loader-iec-only-ram-write-threshold
                 :loader-kernal-transfer-threshold
                 :loader-kernal-execution-threshold
                 :decrunch-write-threshold
                 :decrunch-overwrite-threshold
                 :decrunch-unique-address-threshold
                 :decrunch-frontier-threshold
                 :decrunch-expansion-threshold
                 :decrunch-written-code-execution-threshold
                 :self-modifying-raster-stability-units
                 :self-modifying-raster-execution-threshold
                 :traffic-mode-min-writes
                 :traffic-transfer-unit-accesses
                 :decruncher-phase-ram-write-floor
                 :decruncher-phase-min-units
                 :decruncher-phase-gap-units
                 :decruncher-destination-pages
                 :decruncher-destination-contiguous-pages
                 :decruncher-destination-unique-addresses
                 :loader-phase-serial-threshold
                 :loader-phase-gap-units]]
      (when-not (pos-int? (get configuration key))
        (throw (ex-info "Classifier configuration value must be positive"
                        {:key key :value (get configuration key)}))))
    (doseq [key [:enter-confidence :exit-confidence :decrunch-density-threshold
                 :decrunch-repeated-footprint-penalty
                 :traffic-mode-share-threshold
                 :self-modifying-raster-range-overlap]]
      (when-not (and (number? (get configuration key))
                     (<= 0 (get configuration key) 1))
        (throw (ex-info "Classifier confidence value must be between zero and one"
                        {:key key :value (get configuration key)}))))
    (when-not (<= (:exit-confidence configuration)
                  (:enter-confidence configuration))
      (throw (ex-info "Classifier exit confidence cannot exceed enter confidence"
                      {:exit-confidence (:exit-confidence configuration)
                       :enter-confidence (:enter-confidence configuration)})))
    configuration))

(defn evidence-confidence
  [threshold value weight]
  (if (>= value threshold) weight 0.0))

(defn traffic-metrics
  "Summarize where a unit sends its writes.

  Phase kinds are distinguished by the *mix* of write domains rather than by
  absolute counts, so the same evidence works for an IRQ frame and for an
  interrupt-free write window."
  [unit]
  (let [events (max 1 (- (second (:event-range unit))
                         (first (:event-range unit))))
        vic (:vic-write-count unit 0)
        ram (:ram-write-count unit 0)
        iec (:iec-register-write-count unit 0)
        sid (:sid-write-count unit 0)
        cia1 (:cia1-write-count unit 0)
        serial-read (:serial-read-count unit 0)
        serial-write (:serial-write-count unit 0)
        serial-access (or (:serial-access-count unit)
                          (+ serial-read serial-write))
        total (+ vic ram iec sid cia1)
        share (fn [n] (if (pos? total) (/ (double n) total) 0.0))]
    {:event-count events
     :write-total total
     :vic-write-count vic
     :ram-write-count ram
     :iec-write-count iec
     :sid-write-count sid
     :serial-read-count serial-read
     :serial-write-count serial-write
     :serial-access-count serial-access
     :vic-share (share vic)
     :ram-share (share ram)
     :iec-share (share iec)
     :sid-share (share sid)
     :vic-write-density (/ (double vic) events)
     :ram-write-density (/ (double ram) events)
     :iec-write-density (/ (double iec) events)
     :sid-write-density (/ (double sid) events)}))

(defn traffic-mode
  "Classify a unit's dominant activity from its hardware write mix.

  `:transfer` drives the serial/IEC interface, `:decode` writes ordinary RAM,
  `:raster` programs the VIC, and `:audio` writes the SID. This is deliberately
  address-class based, so it generalizes across demos and does not depend on
  any particular routine's code shape. Serial access is tested first and by
  volume, because a transfer interleaves bus access with buffering stores and
  would otherwise look RAM-dominant."
  ([unit] (traffic-mode unit default-classifier-configuration))
  ([unit {:keys [traffic-mode-min-writes traffic-mode-share-threshold
                 traffic-transfer-unit-accesses]}]
   (let [{:keys [write-total serial-access-count
                 vic-share ram-share sid-share]}
         (traffic-metrics unit)]
     (cond
       (>= serial-access-count traffic-transfer-unit-accesses) :transfer
       (< write-total traffic-mode-min-writes) :idle
       (>= sid-share traffic-mode-share-threshold) :audio
       (>= vic-share traffic-mode-share-threshold) :raster
       (>= ram-share traffic-mode-share-threshold) :decode
       :else :mixed))))

(defn behavior-flags
  "Return the unit's behavioural flag set from its hardware write mix.

  A unit is not forced into one exclusive label: a raster handler that also
  drives the serial bus reports both `:raster` and `:loader`, and a part that
  runs a music player reports `:raster` and `:music` together. Raster and music
  are identified symmetrically by *what hardware the code pokes* rather than by
  a dominant-share test, because a per-frame music player writes only a small
  fraction of the VIC-heavy frame and would otherwise never be visible."
  ([unit] (behavior-flags unit default-classifier-configuration))
  ([unit {:keys [traffic-mode-min-writes traffic-mode-share-threshold
                 traffic-transfer-unit-accesses]}]
   (let [{:keys [write-total serial-access-count
                 vic-write-count sid-write-count]
          :as metrics} (traffic-metrics unit)
         active? (>= write-total traffic-mode-min-writes)
         ram-share (:ram-share metrics)]
     (cond-> #{}
       (>= serial-access-count traffic-transfer-unit-accesses) (conj :loader)
       (>= vic-write-count traffic-mode-min-writes) (conj :raster)
       (>= sid-write-count traffic-mode-min-writes) (conj :music)
       (and active? (>= (or ram-share 0.0) traffic-mode-share-threshold))
       (conj :calculation)))))

(defn unit-domain-write-pcs
  "Return the VIC and SID writing PCs recorded for a classifier unit."
  [unit]
  (select-keys unit [:vic-write-pcs :sid-write-pcs]))

(defn page-words->pages
  "Decode the compact `:destination-page-words` page bitmap into page numbers."
  [words]
  (if (seq words)
    (into #{}
          (for [word-index (range 4)
                bit (range 64)
                :let [word (long (nth words word-index 0))]
                :when (pos? (bit-and word (bit-shift-left 1 bit)))]
            (+ (* word-index 64) bit)))
    #{}))

(defn contiguous-page-run
  "Length of the longest run of consecutive covered destination pages."
  [pages]
  (loop [previous nil
         run 0
         best 0
         pages (sort pages)]
    (if (empty? pages)
      (max best run)
      (let [page (first pages)
            run (if (and previous (= page (inc previous))) (inc run) 1)]
        (recur page run (max best run) (rest pages))))))

(defn serial-access-count
  [unit]
  (long (or (:serial-access-count unit)
            (+ (:serial-read-count unit 0)
               (:serial-write-count unit 0)))))

(def default-kernal-irq-handler
  "The KERNAL's installed-IRQ default (`$0314/$0315` = `$EA31`)."
  0xea31)

(defn default-irq-handler-only?
  [unit]
  (let [handlers (or (:irq-handler-pcs unit) (:irq-entry-pcs unit))]
    (and (seq handlers)
         (every? #(= default-kernal-irq-handler %) handlers))))

(defn custom-irq-frame?
  "An IRQ frame that runs a program-installed handler, i.e. a demo part."
  [unit]
  (and (= :irq-frame (:unit-kind unit))
       (seq (or (:irq-handler-pcs unit) (:irq-entry-pcs unit)))
       (not (default-irq-handler-only? unit))))

(defn loader-phase-unit?
  [unit configuration]
  (>= (serial-access-count unit)
      (:traffic-transfer-unit-accesses configuration)))

(defn decruncher-phase-unit?
  "True when a unit predominantly writes ordinary RAM without driving IEC.

  A custom-handler IRQ frame is a running demo part, not part of a main-loop
  decrunch; this is what separates a real memory-to-memory decoder from a
  calculation/raster routine that merely writes a broad RAM region."
  [unit {:keys [decruncher-phase-ram-write-floor]}]
  (let [ram (long (:ram-write-count unit 0))
        peripheral (+ (long (:vic-write-count unit 0))
                      (long (:sid-write-count unit 0))
                      (long (:iec-register-write-count unit 0)))]
    (and (not (custom-irq-frame? unit))
         (>= ram decruncher-phase-ram-write-floor)
         (>= ram peripheral)
         (zero? (serial-access-count unit)))))

(defn decrunch-phase-confidence
  [{:keys [page-count contiguous-run unique-addresses]} configuration]
  (let [page-ratio (min 1.0 (/ (double page-count)
                               (max 1 (:decruncher-destination-pages configuration))))
        contiguous-ratio (min 1.0 (/ (double contiguous-run)
                                     (max 1 (:decruncher-destination-contiguous-pages
                                             configuration))))
        address-ratio (min 1.0 (/ (double unique-addresses)
                                  (max 1 (:decruncher-destination-unique-addresses
                                          configuration))))]
    (min 1.0 (+ (* 0.4 page-ratio) (* 0.3 contiguous-ratio)
                (* 0.3 address-ratio)))))

(defn close-decrunch-phase
  [state configuration]
  (let [{:keys [start end page-count contiguous-run unique-addresses
                unit-count written-code-execution-count
                followed-by-custom-part?]
         :as phase} (:decrunch-phase state)]
    (if (and phase
             (>= unit-count (:decruncher-phase-min-units configuration))
             (>= page-count (:decruncher-destination-pages configuration))
             (>= contiguous-run
                 (:decruncher-destination-contiguous-pages configuration))
             (>= unique-addresses
                 (:decruncher-destination-unique-addresses configuration))
             ;; A real decruncher hands execution to the code it produced.
             ;; That may be observed inside the phase, or as the part that
             ;; starts when the phase closes. A pure calculation routine is
             ;; kept out because its units are custom-handler part frames.
             (or (pos? (long written-code-execution-count))
                 followed-by-custom-part?))
      (update state :activities conj
              {:kind :decruncher
               :event-range [start end]
               :confidence (decrunch-phase-confidence
                            {:page-count page-count
                             :contiguous-run contiguous-run
                             :unique-addresses unique-addresses}
                            configuration)
               :signals (cond-> [:broad-ram-destination-footprint
                                 :contiguous-ram-destination
                                 :destination-phase-growth
                                 :no-serial-access]
                          (pos? (long written-code-execution-count))
                          (conj :transferred-to-produced-code)
                          followed-by-custom-part?
                          (conj :followed-by-demo-part))
               :phase true
               :destination-page-count page-count
               :destination-contiguous-pages contiguous-run
               :destination-unique-addresses unique-addresses
               :phase-unit-count unit-count})
      state)))

(defn accumulate-decrunch-phase
  [state unit configuration]
  (let [phase? (decruncher-phase-unit? unit configuration)]
    (if phase?
      (let [{:keys [pages unique-addresses
                    unit-count written-code-execution-count ram-write-count]}
            (:decrunch-phase state)
            unit-pages (page-words->pages (:destination-page-words unit))
            pages (into (or pages #{}) unit-pages)
            page-count (count pages)
            phase {:start (or (:start (:decrunch-phase state))
                              (first (:event-range unit)))
                   :end (second (:event-range unit))
                   :pages pages
                   :page-count page-count
                   :contiguous-run (contiguous-page-run pages)
                   :unique-addresses (max (or unique-addresses 0)
                                          (long (:destination-unique-address-count
                                                 unit 0)))
                   :unit-count (inc (or unit-count 0))
                   :written-code-execution-count
                   (+ (long (or written-code-execution-count 0))
                      (long (:written-code-execution-count unit 0)))
                   :ram-write-count (+ (long (or ram-write-count 0))
                                       (long (:ram-write-count unit 0)))
                   :idle-units 0}]
        (assoc state :decrunch-phase phase))
      (let [{:keys [idle-units] :as phase} (:decrunch-phase state)
            phase (if (and phase (custom-irq-frame? unit))
                    (assoc phase :followed-by-custom-part? true)
                    phase)]
        (if (and phase (< (long (or idle-units 0))
                          (:decruncher-phase-gap-units configuration)))
          (assoc state :decrunch-phase (assoc phase :idle-units (inc (long (or idle-units 0)))))
          (-> (close-decrunch-phase state configuration)
              (assoc :decrunch-phase nil)))))))

(defn close-loader-phase
  [state configuration]
  (let [{:keys [start end serial-access-count unit-count]
         :as phase} (:loader-phase state)]
    (if (and phase
             (>= unit-count 2)
             (>= (long serial-access-count)
                 (:loader-phase-serial-threshold configuration)))
      (update state :activities conj
              {:kind :loader
               :event-range [start end]
               :confidence 1.0
               :signals [:sustained-serial-access :transfer-phase]
               :phase true
               :serial-access-count serial-access-count
               :phase-unit-count unit-count})
      state)))

(defn accumulate-loader-phase
  [state unit configuration]
  (if (loader-phase-unit? unit configuration)
    (let [phase (:loader-phase state)
          serial (serial-access-count unit)]
      (assoc state :loader-phase
             (if phase
               (-> phase
                   (assoc :end (second (:event-range unit)))
                   (update :serial-access-count + serial)
                   (update :unit-count inc)
                   (assoc :idle-units 0))
               {:start (first (:event-range unit))
                :end (second (:event-range unit))
                :serial-access-count serial
                :unit-count 1
                :idle-units 0})))
    (let [{:keys [idle-units] :as phase} (:loader-phase state)]
      (if (and phase (< (long (or idle-units 0))
                        (:loader-phase-gap-units configuration)))
        (assoc state :loader-phase (assoc phase :idle-units (inc (long (or idle-units 0)))))
        (-> (close-loader-phase state configuration)
            (assoc :loader-phase nil))))))

(defn frame-loader-evidence
  [frame configuration]
  (let [iec-writes (:iec-register-write-count frame 0)
        kernal-execution (:kernal-iec-execution-count frame 0)
        kernal-transfer (:kernal-iec-transfer-count frame 0)
        kernal? (or (>= kernal-execution
                        (:loader-kernal-execution-threshold configuration))
                    (>= kernal-transfer
                        (:loader-kernal-transfer-threshold configuration)))
        custom-transfer?
        (and (not kernal?)
             (>= iec-writes (:loader-iec-only-write-threshold configuration))
             (or (>= (:ram-unique-address-count frame 0)
                     (:loader-iec-only-destination-threshold configuration))
                 (>= (:ram-write-count frame 0)
                     (:loader-iec-only-ram-write-threshold configuration))))
        ;; IEC-only evidence remains a weak candidate. A fast-loader may be
        ;; uploading or driving a custom 1541 routine, so absence of a CPU RAM
        ;; destination must not discard it. Promotion still requires the
        ;; normal enter-confidence/hysteresis evidence.
        iec? (>= iec-writes (:loader-iec-write-threshold configuration))
        signals (cond-> []
                  iec? (conj :iec-register-activity)
                  (>= kernal-execution
                      (:loader-kernal-execution-threshold configuration))
                  (conj :kernal-iec-execution)
                  (>= kernal-transfer
                      (:loader-kernal-transfer-threshold configuration))
                  (conj :kernal-iec-call)
                  (and iec? (not kernal?))
                  (conj :possible-custom-drive-transport)
                  custom-transfer? (conj :custom-transfer-footprint))
        confidence (+ (if iec?
                        (evidence-confidence
                         (:loader-iec-write-threshold configuration)
                         iec-writes 0.35)
                        0.0)
                      (evidence-confidence
                       (:loader-kernal-execution-threshold configuration)
                       kernal-execution 0.2)
                      (evidence-confidence
                       (:loader-kernal-transfer-threshold configuration)
                       kernal-transfer 0.45)
                      (if custom-transfer? 0.2 0.0))]
    ;; IEC-only evidence is intentionally retained as weak evidence. It may be
    ;; a custom-drive upload, while a long raster loop will normally remain
    ;; below the default enter threshold and be visible through its
    ;; hysteresis/lead-out metadata rather than being asserted as a loader.
    (when (seq signals)
      {:confidence (min 1.0 confidence)
       :signals signals})))

(defn destination-relation
  [frame prior-footprint configuration]
  (let [[start end :as address-range] (:ram-address-range frame)
        [prior-start prior-end :as prior-range] (:ram-address-range prior-footprint)
        span (:ram-address-span frame 0)
        prior-span (:ram-address-span prior-footprint 0)
        overlap (when (and address-range prior-range)
                  (let [overlap (max 0 (inc (- (min end prior-end)
                                               (max start prior-start))))]
                    (/ (double overlap) (max 1 (min span prior-span)))))
        expanded? (and address-range prior-range
                       (or (>= (- span prior-span)
                               (:decrunch-expansion-threshold configuration))
                           (>= (- prior-start start)
                               (:decrunch-expansion-threshold configuration))
                           (>= (- end prior-end)
                               (:decrunch-expansion-threshold configuration))))
        advancing? (and (:ram-frontier-movement frame)
                        (>= (Math/abs (long (:ram-frontier-movement frame)))
                            (:decrunch-frontier-threshold configuration))
                        (not (:ram-footprint-repeat? frame)))]
    {:range-overlap overlap
     :expanded? expanded?
     :advancing? advancing?
     :progress? (or expanded? advancing?)}))

(defn frame-decrunch-evidence
  ([frame configuration]
   (frame-decrunch-evidence frame configuration nil))
  ([frame configuration prior-footprint]
   (let [event-count (max 1 (- (second (:event-range frame))
                               (first (:event-range frame))))
         ;; Only ordinary RAM writes are decrunch output candidates. Raster,
         ;; SID, CIA, color-RAM, and other mapped-register writes are not.
         memory-write-count (long (:ram-write-count frame 0))
         write-density (/ (double memory-write-count) event-count)
         unique-address-count (:ram-unique-address-count frame 0)
         footprint-id (:ram-footprint-id frame)
         repeated? (or (:ram-footprint-repeat? frame)
                       (and footprint-id
                            (= footprint-id (:ram-footprint-id prior-footprint))))
         frontier-movement (:ram-frontier-movement frame)
         relation (destination-relation frame prior-footprint configuration)
         broad-footprint? (>= unique-address-count
                              (:decrunch-unique-address-threshold configuration))
         advancing-frontier? (and broad-footprint? (:advancing? relation))
         expanded-destination? (and broad-footprint? (:expanded? relation))
         progressed? (and broad-footprint? (:progress? relation))
         written-code-execution-count (:written-code-execution-count frame 0)
         executed-written-code?
         (>= written-code-execution-count
             (:decrunch-written-code-execution-threshold configuration))
         signals (cond-> []
                   (>= memory-write-count
                       (:decrunch-write-threshold configuration))
                   (conj :high-write-rate)

                   (>= (:overwritten-code-write-count frame 0)
                       (:decrunch-overwrite-threshold configuration))
                   (conj :overwritten-executed-code)

                   (>= write-density (:decrunch-density-threshold configuration))
                   (conj :write-density)

                   broad-footprint? (conj :broad-ram-destination-footprint)
                   advancing-frontier? (conj :advancing-ram-destination-frontier)
                   expanded-destination? (conj :expanded-ram-destination)
                   executed-written-code? (conj :executed-newly-written-code)
                   repeated? (conj :repeated-ram-destination-footprint))
         confidence (- (+ (evidence-confidence
                           (:decrunch-write-threshold configuration)
                           memory-write-count 0.35)
                          (evidence-confidence
                           (:decrunch-overwrite-threshold configuration)
                           (:overwritten-code-write-count frame 0) 0.4)
                          (if (>= write-density (:decrunch-density-threshold configuration))
                            0.25
                            0.0)
                          (if broad-footprint? 0.15 0.0)
                          (if advancing-frontier? 0.2 0.0)
                          (if expanded-destination? 0.2 0.0)
                          (if executed-written-code? 0.2 0.0))
                       (if repeated?
                         (:decrunch-repeated-footprint-penalty configuration)
                         0.0))]
     ;; A real decruncher must advance or expand its ordinary-RAM destination
     ;; and eventually transfer execution into the produced code. High write
     ;; density alone is deliberately not enough.
     (when (and progressed? executed-written-code?)
       {:confidence (max 0.0 (min 1.0 confidence))
        :signals signals
        :memory-write-count memory-write-count
        :ram-unique-address-count unique-address-count
        :ram-frontier-movement frontier-movement
        :written-code-execution-count written-code-execution-count
        :destination-relation relation}))))

(defn classify-frame-evidence
  [frame configuration]
  (assoc (select-keys frame [:unit-kind :event-range :fingerprint-id :fingerprint
                             :write-count :ram-write-count
                             :changed-write-count
                             :overwritten-code-write-count
                             :iec-register-write-count
                             :kernal-iec-execution-count
                             :kernal-iec-transfer-count
                             :vic-write-count :d012-write-count
                             :sid-write-count :cia1-write-count
                             :irq-entry-pcs :irq-handler-pcs
                             :serial-read-count :serial-write-count
                             :serial-access-count
                             :ram-unique-address-count :ram-address-range
                             :ram-address-span :ram-footprint-id
                             :ram-footprint-repeat? :ram-frontier-movement
                             :ram-frontier-direction
                             :destination-unique-address-count
                             :destination-address-range
                             :destination-address-span
                             :destination-page-count
                             :destination-page-min
                             :destination-page-max
                             :destination-page-words
                             :written-code-execution-count
                             :written-code-address-count])
         :behavior-flags (behavior-flags frame configuration)
         :loader (frame-loader-evidence frame configuration)
         :decruncher (frame-decrunch-evidence frame configuration)))

(defn derive-classification-chunk
  "Extract per-unit classifier evidence from one feature-stage chunk.

  This stage does not assert semantic labels.  It writes bounded candidates;
  `consume-classification-chunk` joins them in manifest order and applies
  cross-chunk lookback and hysteresis."
  [raw-chunk features configuration]
  (let [configuration (normalize-classifier-configuration configuration)
        feature-stage (get-in features [:stages :features])
        units (:classifier-units feature-stage)
        evidence (mapv #(classify-frame-evidence % configuration) units)
        handoff (get-in features [:stages :features :handoff])
        candidate-counts (into {}
                               (for [kind [:loader :decruncher]]
                                 [kind (count (filter #(get % kind) evidence))]))]
    (assoc (artifact/derived-chunk-source :omkamra.vice/classification-chunk-v1
                                          raw-chunk)
           :boundary (:boundary raw-chunk)
           :stages {:classification {:configuration configuration
                                     :frame-evidence evidence
                                     :handoff handoff
                                     :summary {:unit-count (count evidence)
                                               :unit-kind-counts
                                               (frequencies (map :unit-kind evidence))
                                               :candidate-counts candidate-counts}}})))

(defn observation
  [unit candidate]
  {:start (first (or (:event-range candidate) (:event-range unit)))
   :end (second (or (:event-range candidate) (:event-range unit)))
   :confidence (:confidence candidate)
   :signals (set (:signals candidate))
   :fingerprint-id (:fingerprint-id unit)
   :source-event-range (:source-event-range candidate)})

(defn append-observation
  [current unit candidate max-evidence strong?]
  (let [item (observation unit candidate)
        evidence (conj (or (:evidence current) []) item)
        evidence (if (> (count evidence) max-evidence)
                   (vec (take-last max-evidence evidence))
                   evidence)]
    (cond-> (-> current
                (assoc :start (or (:start current) (:start item))
                       :end (:end item)
                       :evidence evidence)
                (update :confidence-total (fnil + 0) (:confidence item))
                (update :observation-count (fnil inc 0))
                (update :signals (fnil into #{}) (:signals item))
                (update-in [:fingerprints (:fingerprint-id item)] (fnil inc 0)))
      strong? (assoc :strong-start (or (:strong-start current) (:start item))
                     :strong-end (:end item)))))

(defn activity-result
  [kind activity end-override]
  (let [observed-end (or end-override (:end activity))
        semantic-start (or (:strong-start activity) (:start activity))
        semantic-end (min observed-end (or (:strong-end activity) observed-end))
        fingerprint (some->> (:fingerprints activity)
                             (sort-by (juxt (comp - val) key))
                             ffirst)]
    (cond-> {:kind kind
             ;; Hysteresis confirms a candidate but no longer expands the
             ;; public semantic range beyond its strong-evidence onset/exit.
             :event-range [semantic-start semantic-end]
             :hysteresis-event-range [(:start activity) observed-end]
             :confidence (/ (:confidence-total activity)
                            (max 1 (:observation-count activity)))
             :signals (vec (sort (:signals activity)))
             :evidence (:evidence activity)
             :fingerprint-id (when (= kind :demopart) fingerprint)}
      (< (:start activity) semantic-start)
      (assoc :transition-lead-in-event-range [(:start activity) semantic-start])

      (< semantic-end observed-end)
      (assoc :transition-trail-out-event-range [semantic-end observed-end]))))

(defn consume-kind
  [state kind unit candidate configuration]
  (let [active (get-in state [:active kind])
        pending (get-in state [:pending kind])
        enter-confidence (:enter-confidence configuration)
        exit-confidence (:exit-confidence configuration)
        hysteresis (:hysteresis-units configuration)
        max-evidence (:evidence-window-units configuration)
        strong? (and candidate (>= (:confidence candidate) enter-confidence))
        continue? (and candidate (>= (:confidence candidate) exit-confidence))]
    (cond
      active
      (if continue?
        (assoc-in state [:active kind]
                  (assoc (append-observation active unit candidate max-evidence strong?)
                         :negative-count 0
                         :negative-start nil))
        (let [negative-count (inc (or (:negative-count active) 0))
              negative-start (or (:negative-start active)
                                 (first (:event-range unit)))]
          (if (>= negative-count hysteresis)
            (-> state
                (update :activities conj
                        (activity-result kind active negative-start))
                (update :active dissoc kind)
                (update :pending dissoc kind))
            (assoc-in state [:active kind]
                      (assoc active
                             :negative-count negative-count
                             :negative-start negative-start)))))

      strong?
      (let [pending (if pending
                      (-> pending
                          (update :count inc)
                          (update :activity
                                  append-observation unit candidate max-evidence true))
                      {:count 1
                       :activity (append-observation {} unit candidate
                                                     max-evidence true)})]
        (if (>= (:count pending) hysteresis)
          (-> state
              (assoc-in [:active kind]
                        (assoc (:activity pending)
                               :negative-count 0
                               :negative-start nil))
              (update :pending dissoc kind))
          (assoc-in state [:pending kind] pending)))

      :else
      (update state :pending dissoc kind))))

(defn self-modifying-raster-evidence
  "Identify a stable IRQ routine that repeatedly rewrites bounded RAM it runs.

  This is intentionally evaluated across classifier units because physical
  chunks often contain only one displayed frame. It is negative evidence for
  decrunching, but persists as an activity for later semantic review."
  [unit prior-footprint stable-raster-unit-count configuration]
  (let [relation (destination-relation unit prior-footprint configuration)
        stable-topology? (and (= :irq-frame (:unit-kind unit))
                              (= :irq-frame (:unit-kind prior-footprint))
                              (= (:irq-entry-pcs unit)
                                 (:irq-entry-pcs prior-footprint)))
        self-modifying? (and stable-topology?
                             (>= stable-raster-unit-count
                                 (:self-modifying-raster-stability-units
                                  configuration))
                             (>= (or (:range-overlap relation) 0.0)
                                 (:self-modifying-raster-range-overlap
                                  configuration))
                             (not (:progress? relation))
                             (>= (:written-code-execution-count unit 0)
                                 (:self-modifying-raster-execution-threshold
                                  configuration)))]
    (when self-modifying?
      {:confidence 1.0
       :signals [:stable-irq-topology :repeated-bounded-ram-destination
                 :write-then-immediate-execute]
       :destination-relation relation
       :written-code-execution-count (:written-code-execution-count unit 0)})))

(defn classifier-state
  "Create a pure accumulator for cross-chunk classifier evidence."
  [configuration]
  {:configuration (normalize-classifier-configuration configuration)
   :unit-count 0
   :recent []
   ;; One compact prior-frame footprint is enough to recognize a repeated
   ;; destination across raw chunk boundaries.
   :prior-footprint nil
   :before-last-footprint nil
   :last-unit nil
   :units-in-current-chunk 0
   :boundary-tail-written-addresses []
   :stable-raster-unit-count 0
   :fingerprint-counts {}
   :active {}
   :pending {}
   :loader-phase nil
   :decrunch-phase nil
   :activities []
   :candidate-counts {:loader 0 :decruncher 0 :demopart 0
                      :self-modifying-raster 0}
   :physical-boundaries []})

(defn consume-classification-chunk
  "Consume one persisted classification chunk in chronological order."
  [state chunk]
  (let [configuration (:configuration state)
        evidence (get-in chunk [:stages :classification :frame-evidence])
        handoff (get-in chunk [:stages :classification :handoff])
        prior-tail (set (:boundary-tail-written-addresses state))
        current-head (set (:head-executed-addresses handoff))
        handoff-addresses (vec (sort (set/intersection prior-tail current-head)))
        lookback (:lookback-units configuration)
        state (assoc state :units-in-current-chunk 0)]
    (reduce
     (fn [state unit]
       (let [recent (conj (:recent state) unit)
             recent (if (> (count recent) lookback)
                      (vec (take-last lookback recent))
                      recent)
             fingerprint-counts (frequencies (keep :fingerprint-id recent))
             fingerprint-id (:fingerprint-id unit)
             stable? (and fingerprint-id
                          (>= (get fingerprint-counts fingerprint-id 0)
                              (:demopart-stability-units configuration)))
             demopart (when stable?
                        {:confidence (min 1.0
                                          (/ (double
                                              (get fingerprint-counts fingerprint-id))
                                             (:demopart-stability-units configuration)))
                         :signals [:stable-irq-fingerprint]})
             cross-chunk-handoff?
             (and (zero? (:units-in-current-chunk state))
                  (seq handoff-addresses)
                  (:last-unit state))
             cross-chunk-decruncher
             (when cross-chunk-handoff?
               (let [source-unit
                     (assoc (:last-unit state)
                            :written-code-execution-count
                            (count handoff-addresses))
                     candidate (frame-decrunch-evidence
                                source-unit configuration
                                (:before-last-footprint state))]
                 (when candidate
                   (assoc candidate
                          :signals (conj (set (:signals candidate))
                                         :cross-chunk-write-to-execute)
                          :event-range
                          [(first (:event-range source-unit))
                           (second (:event-range unit))]
                          :source-event-range (:event-range source-unit)
                          :cross-chunk-handoff-addresses handoff-addresses))))
             stable-topology? (and (= :irq-frame (:unit-kind unit))
                                   (= :irq-frame
                                      (:unit-kind (:prior-footprint state)))
                                   (= (:irq-entry-pcs unit)
                                      (:irq-entry-pcs (:prior-footprint state))))
             stable-raster-unit-count (if stable-topology?
                                        (inc (:stable-raster-unit-count state))
                                        1)
             self-modifying-raster
             (self-modifying-raster-evidence
              unit (:prior-footprint state) stable-raster-unit-count configuration)
             ;; Re-evaluate decrunch evidence here rather than trusting the
             ;; chunk-local candidate. The retained prior footprint lets the
             ;; classifier recognize a repeated destination across chunks.
             ;; A stable self-modifying raster is explicit negative evidence:
             ;; it is retained as its own activity and cannot also be called a
             ;; decruncher merely because it writes heavily.
             decruncher (when-not self-modifying-raster
                          (or cross-chunk-decruncher
                              (frame-decrunch-evidence unit configuration
                                                       (:prior-footprint state))))
             candidates {:loader (:loader unit)
                         :decruncher decruncher
                         :self-modifying-raster self-modifying-raster
                         ;; Include nil candidates as well: otherwise a
                         ;; demopart activity never receives negative evidence
                         ;; and can coalesce across arbitrarily large gaps.
                         :demopart demopart}
             state (reduce (fn [state kind]
                             (update state :candidate-counts
                                     update kind (fnil inc 0)))
                           (assoc state
                                  :recent recent
                                  :before-last-footprint (:prior-footprint state)
                                  :prior-footprint
                                  (select-keys unit [:unit-kind :irq-entry-pcs
                                                     :ram-footprint-id
                                                     :ram-address-range
                                                     :ram-address-span
                                                     :ram-frontier-movement])
                                  :last-unit unit
                                  :units-in-current-chunk
                                  (inc (:units-in-current-chunk state))
                                  :boundary-tail-written-addresses
                                  (vec (:tail-written-addresses handoff))
                                  :stable-raster-unit-count
                                  stable-raster-unit-count
                                  :fingerprint-counts fingerprint-counts
                                  :unit-count (inc (:unit-count state)))
                           (keep (fn [[kind candidate]]
                                   (when candidate kind)) candidates))]
         ;; Destination and serial-bus phases are accumulated here because a
         ;; decruncher/loader is a run of consecutive units, not a property of
         ;; any single bounded window.
         (reduce (fn [state [kind candidate]]
                   (consume-kind state kind unit candidate configuration))
                 (-> state
                     (accumulate-loader-phase unit configuration)
                     (accumulate-decrunch-phase unit configuration))
                 candidates)))
     (update state :physical-boundaries conj
             {:chunk-number (:chunk-number chunk)
              :event-range (:event-range chunk)
              :boundary (:boundary chunk)})
     evidence)))

(defn finish-classification
  "Close open activities and return the durable classifier index."
  [state]
  (let [configuration (:configuration state)
        state (-> state
                  (close-loader-phase configuration)
                  (assoc :loader-phase nil)
                  (close-decrunch-phase configuration)
                  (assoc :decrunch-phase nil))
        activities (reduce-kv
                    (fn [activities kind activity]
                      (conj activities (activity-result kind activity nil)))
                    (:activities state)
                    (:active state))
        activities (vec (sort-by (juxt (comp first :event-range) :kind)
                                 activities))
        parts (->> activities
                   (filter #(= :demopart (:kind %)))
                   (map-indexed (fn [index activity]
                                  (assoc activity
                                         :part-id (inc index)
                                         :fingerprint-id
                                         (:fingerprint-id activity))))
                   vec)
        summary {:unit-count (:unit-count state)
                 :activity-count (count activities)
                 :activity-counts (frequencies (map :kind activities))
                 :candidate-counts (:candidate-counts state)
                 :physical-boundary-count
                 (count (:physical-boundaries state))}]
    {:format :omkamra.vice/classification-v1
     :configuration (:configuration state)
     :activities activities
     :parts parts
     :physical-boundaries (:physical-boundaries state)
     :summary summary}))
