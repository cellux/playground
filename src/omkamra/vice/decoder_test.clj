(ns omkamra.vice.decoder-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.decoder :as decoder]
            [omkamra.vice.decoder.capture]
            [omkamra.vice.decoder.classification]
            [omkamra.vice.decoder.stream]
            [omkamra.vice.decoder.write]))

(defn- trace-event
  [{:keys [pc bytes raster-line cpu-cycle a x y sp flags global-cycle]}]
  [pc bytes raster-line cpu-cycle a x y sp flags global-cycle])

(deftest streaming-transducers-intern-instructions-and-basic-blocks
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'instruction-block-stream))
        ingester (make-ingester)
        state (atom
               (reduce (:step ingester) (empty-state)
                       (for [index (range 12)]
                         (trace-event
                          {:pc (+ 0x1000 (mod index 2))
                           :bytes [(if (even? index) 0xea 0x60)]
                           :raster-line 1 :cpu-cycle index
                           :a 0 :x 0 :y 0 :sp 0xff
                           :flags "........" :global-cycle index}))))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= 12 (:event-count stream)))
    (is (= 2 (count (:instructions stream))))
    (is (= [{:id 0 :instruction-ids [0 1]}] (:blocks stream)))
    ;; Consecutive occurrences of the same block are run-length encoded.
    (is (= [[0 6]] (:block-runs stream)))
    ;; Full register samples are opt-in rather than retained by default.
    (is (nil? (:samples stream)))
    (is (nil? (:occurrences stream)))))

(deftest irq-vector-follows-cpu-port-banking
  (let [effective-irq-vector* (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                                   'effective-irq-vector*))]
    ;; KERNAL mapped in: the ROM vector at $fffe/$ffff reads as $ff48.
    (is (= 0xff48 (effective-irq-vector* 0x37 0xc931)))
    ;; KERNAL banked out: the RAM word underneath is authoritative.
    (is (= 0xc931 (effective-irq-vector* 0x34 0xc931)))))

(deftest banked-out-io-writes-count-as-ordinary-ram
  (let [write-domain-at (var-get (ns-resolve 'omkamra.vice.decoder.features
                                             'write-domain-at))]
    (is (= :cia2 (write-domain-at 0xdd00 0x37)))
    (is (= :vic (write-domain-at 0xd012 0x37)))
    ;; With I/O banked out `$d000-$dfff` is plain RAM, not a register.
    (is (= :memory (write-domain-at 0xdd00 0x34)))
    (is (= :memory (write-domain-at 0xd012 0x34)))))

(deftest traffic-mode-classifies-hardware-write-mix
  (let [traffic-mode (var-get (ns-resolve 'omkamra.vice.decoder.classification
                                          'traffic-mode))
        unit (fn [counts] (merge {:event-range [0 1000]} counts))]
    ;; A load samples the serial bus by reading $dd00.
    (is (= :transfer (traffic-mode (unit {:serial-read-count 200
                                          :serial-access-count 200
                                          :ram-write-count 50}))))
    ;; Handshake writes alone do not make a transfer; the buffering stores
    ;; make it a decode phase instead.
    (is (= :decode (traffic-mode (unit {:iec-register-write-count 20
                                        :ram-write-count 300}))))
    (is (= :decode (traffic-mode (unit {:ram-write-count 300
                                        :vic-write-count 20}))))
    (is (= :raster (traffic-mode (unit {:vic-write-count 400
                                        :ram-write-count 20}))))
    (is (= :audio (traffic-mode (unit {:sid-write-count 100
                                       :ram-write-count 20}))))
    ;; Too little activity to name a phase.
    (is (= :idle (traffic-mode (unit {:ram-write-count 4}))))))

(defn- destination-page-words
  [pages]
  (let [words (long-array 4)]
    (doseq [page pages]
      (let [word (quot page 64)
            bit (mod page 64)]
        (aset-long words word
                   (bit-or (aget words word) (bit-shift-left 1 bit)))))
    (vec words)))

(deftest behavior-flags-do-not-force-a-unit-into-one-role
  (let [behavior-flags (var-get (ns-resolve 'omkamra.vice.decoder.classification
                                            'behavior-flags))
        unit (fn [counts] (merge {:event-range [0 1000]} counts))]
    ;; A raster handler that also drives the serial bus keeps both flags.
    (is (= #{:loader :raster}
           (behavior-flags (unit {:serial-access-count 400
                                  :vic-write-count 400
                                  :ram-write-count 20}))))
    (is (= #{:music}
           (behavior-flags (unit {:sid-write-count 400
                                  :ram-write-count 20}))))
    (is (= #{:calculation}
           (behavior-flags (unit {:ram-write-count 400
                                  :vic-write-count 20}))))
    ;; A part frame runs a raster routine and a per-frame music player; both
    ;; are reported even though SID is a tiny fraction of the VIC-heavy frame.
    (is (= #{:raster :music}
           (behavior-flags (unit {:vic-write-count 5000
                                  :sid-write-count 40
                                  :ram-write-count 100}))))
    (is (= #{} (behavior-flags (unit {:ram-write-count 4}))))))

(deftest destination-profile-excludes-zero-page-and-stack
  (let [destination-profile (var-get (ns-resolve 'omkamra.vice.decoder.features
                                                 'destination-profile))
        footprint (java.util.BitSet. 65536)]
    ;; Zero page and stack are decruncher scratch, not destination data.
    (doseq [address [0x0020 0x0100]]
      (.set footprint address))
    (doseq [address [0x0400 0x0401 0x0500 0x0900]]
      (.set footprint address))
    (let [profile (destination-profile footprint 0x0900 0x0200)]
      (is (= 4 (:destination-unique-address-count profile)))
      (is (= [0x0400 0x0900] (:destination-address-range profile)))
      (is (= #{4 5 9}
             (omkamra.vice.decoder.classification/page-words->pages
              (:destination-page-words profile))))
      (is (= 2 (omkamra.vice.decoder.classification/contiguous-page-run
                #{4 5 9}))))
    ;; A unit with no destination-eligible writes has no profile.
    (is (nil? (destination-profile (doto (java.util.BitSet. 65536)
                                     (.set 0x0020))
                                   0x0020 0x0200)))))

(deftest decrunch-phase-accumulates-a-broad-contiguous-destination
  (let [unit (fn [start pages execution]
               {:unit-kind :write-window
                :event-range [start (+ start 100)]
                :ram-write-count 3000
                :destination-unique-address-count 3000
                :destination-page-words (destination-page-words pages)
                :written-code-execution-count execution
                :serial-access-count 0
                :vic-write-count 0 :sid-write-count 0
                :iec-register-write-count 0})
        chunk {:chunk-number 1
               :event-range [0 500]
               :boundary {:kind :final}
               :stages {:classification
                        {:handoff {}
                         :frame-evidence [(unit 0 (range 9 30) 100)
                                          (unit 100 (range 20 45) 100)
                                          ;; A VIC frame closes the RAM phase.
                                          (assoc (unit 200 [50] 0)
                                                 :vic-write-count 5000)]}}}
        result (-> (omkamra.vice.decoder.classification/classifier-state {})
                   (omkamra.vice.decoder.classification/consume-classification-chunk chunk)
                   omkamra.vice.decoder.classification/finish-classification)
        decruncher (some #(when (= :decruncher (:kind %)) %)
                         (:activities result))]
    (is (some? decruncher))
    ;; The phase spans the whole destination sweep, not one 10k-event window.
    (is (= [0 200] (:event-range decruncher)))
    (is (:phase decruncher))
    (is (some #{:broad-ram-destination-footprint} (:signals decruncher)))
    (is (some #{:transferred-to-produced-code} (:signals decruncher)))))

(deftest loader-transfer-phase-spans-sustained-serial-access
  (let [unit (fn [start serial]
               {:unit-kind :write-window
                :event-range [start (+ start 100)]
                :serial-access-count serial
                :ram-write-count 3000
                :destination-page-words (destination-page-words [40 41])
                :written-code-execution-count 0})
        chunk {:chunk-number 1
               :event-range [0 500]
               :boundary {:kind :final}
               :stages {:classification
                        {:handoff {}
                         :frame-evidence [(unit 0 1000)
                                          (unit 100 1000)
                                          (unit 200 1000)
                                          (unit 300 1000)
                                          (unit 400 1000)]}}}
        result (-> (omkamra.vice.decoder.classification/classifier-state {})
                   (omkamra.vice.decoder.classification/consume-classification-chunk chunk)
                   omkamra.vice.decoder.classification/finish-classification)
        loader (some #(when (= :loader (:kind %)) %)
                     (:activities result))]
    (is (some? loader))
    (is (= [0 500] (:event-range loader)))
    (is (:phase loader))
    (is (some #{:sustained-serial-access} (:signals loader)))
    ;; A brief bus hiccup is not a load.
    (is (empty? (filter #(= :loader (:kind %))
                        (:activities
                         (-> (omkamra.vice.decoder.classification/classifier-state {})
                             (omkamra.vice.decoder.classification/consume-classification-chunk
                              (assoc-in chunk [:stages :classification :frame-evidence]
                                        [(unit 0 8) (unit 100 0) (unit 200 0)]))
                             omkamra.vice.decoder.classification/finish-classification)))))))

(deftest recover-irq-boundaries-detects-vector-entry-after-rti
  (let [recover-irq-boundaries (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                                    'recover-irq-boundaries))
        chunk {:event-count 4
               :events {:event-count 4
                        :instructions [{:id 0 :pc 0xc000 :bytes [0x4c 0x00 0xc0]
                                        :mnemonic "JMP" :mode :abs}
                                       {:id 1 :pc 0xff48 :bytes [0x48]
                                        :mnemonic "PHA" :mode :imp}]
                        :blocks [{:id 0 :instruction-ids [0]}
                                 {:id 1 :instruction-ids [1]}]
                        :block-runs [[0 1] [1 1] [0 1] [1 1]]}
               :stages {:memory {:initial [0 0x37] :writes nil}
                        :structure {:boundaries [] :boundary-timings {}}}}
        recovered (recover-irq-boundaries chunk)]
    (is (= [1 3] (:boundaries recovered)))
    (is (= #{1 3} (set (keys (:boundary-timings recovered)))))))

(deftest irq-handler-resolves-through-kernal-vector
  (let [resolve-irq-handler (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                                 'resolve-irq-handler))]
    ;; The KERNAL entry dispatches through $0314/$0315.
    (is (= 0x2071 (resolve-irq-handler 0xff48 0x2071)))
    ;; A span that begins at a banked-out RAM vector is already the handler.
    (is (= 0x6039 (resolve-irq-handler 0x6039 0xea31)))))

(deftest recover-irq-boundaries-honours-ram-vector-updates
  (let [recover (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                     'recover-irq-boundaries))
        ;; port $34 banks KERNAL out, so the $fffe/$ffff RAM word is the
        ;; hardware IRQ vector. Two runtime stores install $1400 there, and the
        ;; stream then vectors to it.
        chunk {:event-count 3
               :events {:event-count 3
                        :instructions [{:id 0 :pc 0xc000 :bytes [0x4c 0x00 0xc0]
                                        :mnemonic "JMP" :mode :abs}
                                       {:id 1 :pc 0x1400 :bytes [0x48]
                                        :mnemonic "PHA" :mode :imp}]
                        :blocks [{:id 0 :instruction-ids [0]}
                                 {:id 1 :instruction-ids [1]}]
                        :block-runs [[0 2] [1 1]]}
               :stages {:memory
                        {:initial (assoc (vec (repeat 2 0)) 1 0x34)
                         :writes {:format :omkamra.vice/write-records-v1
                                  :mnemonics ["STA"] :kinds [:store]
                                  :keys [:event-index :pc :address :value :old-value
                                         :raster-line :cpu-cycle
                                         :instruction-raster-line :instruction-cpu-cycle
                                         :write-cycle-offset :mnemonic-id :kind-id]
                                  :writes [[0 0xc000 0xfffe 0x00 0 0 0 0 0 0 0 0]
                                           [1 0xc000 0xffff 0x14 0 0 0 0 0 0 0 0]]}}
                        :structure {:boundaries [] :boundary-timings {}}}}
        ram-vector (assoc-in chunk [:stages :memory :initial 1] 0x37)]
    ;; Banked out: the runtime-installed RAM vector $1400 is authoritative.
    (is (= [2] (:boundaries (recover chunk))))
    ;; KERNAL visible: $fffe/$ffff reads as ROM $ff48, so the RAM word is
    ;; correctly ignored and no spurious boundary appears at $1400.
    (is (= [] (:boundaries (recover ram-vector))))))

(deftest structure-union-recovers-irq-span-after-rti
  (let [events {:event-count 3
                :instructions [{:id 0 :pc 0xc000 :bytes [0x4c 0x00 0xc0]
                                :mnemonic "JMP" :mode :abs :operand "$C000"}
                               {:id 1 :pc 0xff48 :bytes [0x48]
                                :mnemonic "PHA" :mode :imp :operand nil}
                               {:id 2 :pc 0xea31 :bytes [0x40]
                                :mnemonic "RTI" :mode :imp :operand nil}]
                :blocks [{:id 0 :instruction-ids [0]}
                         {:id 1 :instruction-ids [1]}
                         {:id 2 :instruction-ids [2]}]
                :block-runs [[0 1] [1 1] [2 1]]}
        execution {:instructions (mapv #(-> %
                                            (dissoc :pc)
                                            (assoc :address (:pc %)))
                                       (:instructions events))
                   :blocks (:blocks events)
                   :block-runs (:block-runs events)}
        chunk {:format :omkamra.vice/chunk-v1
               :capture-id "irq-vector-test"
               :chunk-number 1
               :event-range [0 3]
               :events events
               :stages {:memory {:initial [0 0x37] :writes nil}
                        :structure {:execution execution
                                    :boundaries []
                                    :boundary-timings {}}}}
        derived (decoder/derive-structure-chunk chunk)
        irq-spans (filterv #(= :irq (:kind %))
                           (get-in derived [:stages :structure :spans]))]
    (is (= 1 (count irq-spans)))
    (is (= {:start-index 1 :end-index 3 :entry-pc 0xff48}
           (select-keys (first irq-spans) [:start-index :end-index :entry-pc])))))

(deftest conditional-branch-to-fall-through-is-deduplicated
  (let [control-flow-successors (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                                     'control-flow-successors))
        branch {:pc 0xc000
                :bytes [0xd0 0x00]
                :mnemonic "BNE"}]
    (is (= #{0xc002}
           (control-flow-successors nil branch)))))

(deftest streaming-splits-at-backward-control-flow-target
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'instruction-block-stream))
        ingester (make-ingester {:initial-memory (byte-array 65536)})
        events [[0xc000 [0xa2 0x00] nil nil nil nil nil nil nil nil]
                [0xc002 [0xea] nil nil nil nil nil nil nil nil]
                [0xc003 [0xe8] nil nil nil nil nil nil nil nil]
                [0xc004 [0xd0 0xfc] nil nil nil nil nil nil nil nil]]
        state (atom (reduce (:step ingester) (empty-state) events))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= [[0] [1 2 3]]
           (mapv :instruction-ids (:blocks stream))))))

(deftest streaming-ingestion-splits-at-control-flow-discontinuity
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'instruction-block-stream))
        ingester (make-ingester {:initial-memory (byte-array 65536)})
        events [[0xe5cd [0xa5 0xc6] 1 0 nil nil nil nil nil nil]
                [0xff48 [0x48] 1 2 nil nil nil nil nil nil]
                [0xff49 [0x8a] 1 4 nil nil nil nil nil nil]]
        state (atom (reduce (:step ingester) (empty-state) events))
        _ (swap! state (:complete ingester))
        stream (finish-stream state ingester)]
    (is (= [[0] [1 2]]
           (mapv :instruction-ids (:blocks stream))))))

(deftest retains-full-samples-only-when-requested
  (let [empty-state (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                         'empty-stream-state))
        make-ingester (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'make-stream-ingester))
        finish-stream (var-get (ns-resolve 'omkamra.vice.decoder.stream
                                           'instruction-block-stream))
        ingester (make-ingester {:retain-samples? true})
        events [[0x1000 [0xea] 1 0 1 2 3 4 "........" 5]]
        state (atom (reduce (:step ingester) (empty-state) events))]
    (swap! state (:complete ingester))
    (is (= [[1 0 1 2 3 4 "........" 5]]
           (:samples (finish-stream state ingester))))))

(deftest compact-write-records-round-trip
  (let [compact-write (var-get (ns-resolve 'omkamra.vice.decoder.write
                                           'compact-write-data))
        writes [{:event-index 10 :pc 0x1000 :address 0xd018
                 :value 3 :old-value 0 :raster-line 2 :cpu-cycle 5
                 :instruction-raster-line 2 :instruction-cpu-cycle 2
                 :write-cycle-offset 2 :mnemonic "STA" :kind :store}]
        compact (compact-write writes)]
    (is (= :omkamra.vice/write-records-v1 (:format compact)))
    (is (= [[10 0x1000 0xd018 3 0 2 5 2 2 2 0 0]]
           (:writes compact)))
    (is (= writes (decoder/expand-writes compact)))))

(deftest feature-extraction-keeps-bounded-loader-write-and-frame-evidence
  (let [chunk {:format :omkamra.vice/chunk-v1
               :capture-id "features-test"
               :chunk-number 1
               :event-range [100 108]
               :events {:event-count 8
                        :instructions [{:id 0
                                        :pc 0x1000
                                        :bytes [0x8d 0x12 0xd0]
                                        :mnemonic "STA"
                                        :mode :abs}]
                        :blocks [{:id 0 :instruction-ids [0]}]
                        :block-runs [[0 8]]}
               :stages {:memory
                        {:writes {:format :omkamra.vice/write-records-v1
                                  :mnemonics ["STA"]
                                  :kinds [:store]
                                  ;; `$d052` mirrors VIC `$d012`; `$dd10`
                                  ;; mirrors CIA2 `$dd00`.
                                  :writes [[1 0x1000 0xd052 42 0 300 5 300 1 4 0 0]
                                           [3 0x1000 0xdd10 1 0 50 4 50 0 4 0 0]
                                           [4 0x1000 0x1000 255 0 100 3 100 0 4 0 0]]}}}}
        structure {:stages
                   {:structure
                    {:spans [{:kind :irq
                              :start-index 0
                              :end-index 2
                              :entry-pc 0x1000
                              :trigger {:raster-line 300 :cpu-cycle 2}}
                             {:kind :irq
                              :start-index 2
                              :end-index 4
                              :entry-pc 0x1000
                              :trigger {:raster-line 20 :cpu-cycle 2}}
                             {:kind :irq
                              :start-index 4
                              :end-index 8
                              :entry-pc 0x1000
                              :trigger {:raster-line 100 :cpu-cycle 2}}]}}}
        video {:stages {:video {:vic {:configurations [{} {}]}}}}
        mirrored-video (decoder/derive-video-chunk
                        (assoc-in chunk [:stages :memory :initial]
                                  (byte-array 65536)))
        features (decoder/derive-features-chunk
                  chunk structure video {:fingerprint-window-frames 1})
        {:keys [summary frames rolling-fingerprints]}
        (get-in features [:stages :features])]
    (is (= :omkamra.vice/features-chunk-v1 (:format features)))
    (is (= 42 (get-in mirrored-video [:stages :video :vic :final-state 0xd012])))
    (is (= 1 (get-in mirrored-video [:stages :video :vic :final-state 0xdd00])))
    (is (= 8 (:event-count summary)))
    (is (= 2 (:frame-count summary)))
    (is (= 3 (:write-count summary)))
    (is (= 1 (:iec-register-write-count summary)))
    (is (= 1 (:overwritten-code-write-count summary)))
    (is (= [[100 102] [102 108]] (mapv :event-range frames)))
    (is (= [42] (mapv :value (:d012-writes (first frames)))))
    ;; Only the ordinary RAM write contributes to a destination footprint.
    (is (= [0 1] (mapv :ram-unique-address-count frames)))
    (is (= [nil [0x1000 0x1000]] (mapv :ram-address-range frames)))
    (is (= 2 (count rolling-fingerprints)))
    (is (every? :fingerprint-id frames))))

(deftest interrupt-free-write-windows-preserve-decrunch-evidence
  (let [writes (mapv (fn [event-index]
                       [event-index 0x1000 (+ 0x2000 event-index) 1 0
                        0 event-index 0 event-index 0 0 0])
                     (range 12))
        chunk {:format :omkamra.vice/chunk-v1
               :capture-id "no-irq-window-test"
               :chunk-number 1
               :event-range [0 12]
               :events {:event-count 12
                        :instructions [{:id 0 :pc 0x1000 :bytes [0xea]
                                        :mnemonic "NOP" :mode :imp}]
                        :blocks [{:id 0 :instruction-ids [0]}]
                        :block-runs [[0 12]]}
               :stages {:memory
                        {:writes {:format :omkamra.vice/write-records-v1
                                  :mnemonics ["STA"]
                                  :kinds [:store]
                                  :writes writes}}}}
        structure {:stages {:structure {:spans []}}}
        video {:stages {:video {:vic {:configurations []}}}}
        features (decoder/derive-features-chunk
                  chunk structure video {:write-window-events 4})
        feature-stage (get-in features [:stages :features])
        classification (decoder/derive-classification-chunk
                        chunk features
                        {:hysteresis-units 1
                         :decrunch-write-threshold 4
                         :decrunch-overwrite-threshold 1
                         :decrunch-density-threshold 0.5
                         :decrunch-unique-address-threshold 4
                         :decrunch-frontier-threshold 1})
        result (-> (decoder/classifier-state
                    {:hysteresis-units 1
                     :decrunch-write-threshold 4
                     :decrunch-overwrite-threshold 1
                     :decrunch-density-threshold 0.5
                     :decrunch-unique-address-threshold 4
                     :decrunch-frontier-threshold 1})
                   (decoder/consume-classification-chunk classification)
                   decoder/finish-classification)]
    ;; The feature stage does not invent IRQ fingerprints or demoparts for
    ;; interrupt-disabled code, but preserves bounded, non-overlapping RAM
    ;; destination observations for classification.
    (is (= [] (:frames feature-stage)))
    (is (= [[0 4] [4 8] [8 12]]
           (mapv :event-range (:write-windows feature-stage))))
    (is (= [:write-window :write-window :write-window]
           (mapv :unit-kind (:classifier-units feature-stage))))
    (is (every? #(nil? (:fingerprint-id %))
                (:classifier-units feature-stage)))
    (is (= {:write-window 3}
           (get-in classification [:stages :classification :summary
                                   :unit-kind-counts])))
    ;; Write density without a transfer into the produced code remains only
    ;; evidence, not a decrunch classification.
    (is (empty? (filter #(= :decruncher (:kind %)) (:activities result))))
    (is (empty? (:parts result)))))

(deftest short-no-irq-prefix-is-folded-into-the-following-irq-unit
  (let [chunk {:format :omkamra.vice/chunk-v1
               :capture-id "short-prefix-test"
               :chunk-number 1
               :event-range [100 110]
               :events {:event-count 10
                        :instructions [{:id 0 :pc 0x1000 :bytes [0xea]
                                        :mnemonic "NOP" :mode :imp}]
                        :blocks [{:id 0 :instruction-ids [0]}]
                        :block-runs [[0 10]]}
               :stages {:memory {:writes {:mnemonics [] :kinds [] :writes []}}}}
        structure {:stages {:structure
                            {:spans [{:kind :irq :start-index 2 :end-index 10
                                      :entry-pc 0x1000
                                      :trigger {:raster-line 10 :cpu-cycle 0}}]}}}
        video {:stages {:video {:vic {:configurations []}}}}
        feature-stage (get-in (decoder/derive-features-chunk
                               chunk structure video {:write-window-events 4})
                              [:stages :features])]
    ;; A tiny prefix at a physical boundary must not become an extra negative
    ;; classifier observation between otherwise continuous IRQ frames.
    (is (= [] (:write-windows feature-stage)))
    (is (= [[100 110]] (mapv :event-range (:frames feature-stage))))
    (is (= [:irq-frame] (mapv :unit-kind (:classifier-units feature-stage))))))

(deftest loader-evidence-does-not-promote-iec-only-raster-loops
  (let [loader-evidence (var-get (ns-resolve 'omkamra.vice.decoder.classification
                                             'frame-loader-evidence))
        configuration (merge (decoder/normalize-classifier-configuration {})
                             {:loader-iec-only-write-threshold 16
                              :loader-iec-only-destination-threshold 128
                              :loader-iec-only-ram-write-threshold 256})
        raster-loop {:iec-register-write-count 900
                     :kernal-iec-execution-count 0
                     :kernal-iec-transfer-count 0
                     :ram-write-count 1400
                     :ram-unique-address-count 3}
        custom-loader (assoc raster-loop
                             :ram-write-count 600
                             :ram-unique-address-count 900)
        kernal-loader (assoc raster-loop
                             :kernal-iec-execution-count 4
                             :kernal-iec-transfer-count 1)]
    (is (some #{:iec-register-activity}
              (:signals (loader-evidence raster-loop configuration))))
    (is (some #{:custom-transfer-footprint}
              (:signals (loader-evidence custom-loader configuration))))
    (is (some #{:kernal-iec-call}
              (:signals (loader-evidence kernal-loader configuration))))))

(deftest decrunch-evidence-ignores-vic-raster-writes
  (let [evidence (var-get (ns-resolve 'omkamra.vice.decoder.classification
                                      'frame-decrunch-evidence))
        configuration (decoder/normalize-classifier-configuration {})
        raster-frame {:event-range [0 100000]
                      :write-count 40000
                      :ram-write-count 5000
                      :vic-write-count 35000
                      :overwritten-code-write-count 0}
        decrunch-frame {:event-range [0 100000]
                        :write-count 40000
                        :ram-write-count 40000
                        :vic-write-count 0
                        :ram-unique-address-count 3000
                        :ram-frontier-movement 1024
                        :written-code-execution-count 1
                        :overwritten-code-write-count 3000}]
    (is (nil? (evidence raster-frame configuration)))
    (is (= 1.0 (:confidence (evidence decrunch-frame configuration))))
    (is (= 40000 (:memory-write-count
                  (evidence decrunch-frame configuration))))))

(deftest footprint-evidence-favors-an-advancing-destination-over-a-repeat
  (let [evidence (var-get (ns-resolve 'omkamra.vice.decoder.classification
                                      'frame-decrunch-evidence))
        configuration (decoder/normalize-classifier-configuration {})
        common {:event-range [0 100000]
                :ram-write-count 40000
                :overwritten-code-write-count 3000
                :written-code-execution-count 1
                :ram-unique-address-count 3000}
        repeated (evidence (assoc common
                                  :ram-footprint-id "ram-a"
                                  :ram-footprint-repeat? true)
                           configuration)
        advancing (evidence (assoc common
                                   :ram-footprint-id "ram-b"
                                   :ram-frontier-movement 1024)
                            configuration
                            {:ram-footprint-id "ram-a"})]
    (is (nil? repeated))
    (is (= 1.0 (:confidence advancing)))
    (is (some #{:advancing-ram-destination-frontier} (:signals advancing)))))

(deftest cross-chunk-write-to-execute-confirms-decrunch-span
  (let [unit (fn [start range frontier]
               {:unit-kind :write-window
                :event-range [start (+ start 10)]
                :ram-write-count 3000
                :ram-unique-address-count 3000
                :ram-address-range range
                :ram-address-span (inc (- (second range) (first range)))
                :ram-frontier-movement frontier
                :overwritten-code-write-count 3000
                :written-code-execution-count 0
                :fingerprint-id nil
                :loader nil
                :decruncher nil})
        first-chunk {:chunk-number 1
                     :event-range [0 20]
                     :boundary {:kind :forced-size}
                     :stages {:classification
                              {:handoff {:tail-written-addresses [0x3000]
                                         :head-executed-addresses []}
                               :frame-evidence [(unit 0 [0x1000 0x1fff] 0)
                                                (unit 10 [0x2000 0x3000] 1024)]}}}
        second-unit (assoc (unit 20 [0x3000 0x3000] 0)
                           :event-range [20 30])
        second-chunk {:chunk-number 2
                      :event-range [20 30]
                      :boundary {:kind :final}
                      :stages {:classification
                               {:handoff {:tail-written-addresses []
                                          :head-executed-addresses [0x3000]}
                                :frame-evidence [second-unit]}}}
        configuration {:hysteresis-units 1
                       :decrunch-write-threshold 100
                       :decrunch-overwrite-threshold 10
                       :decrunch-unique-address-threshold 100
                       :decrunch-expansion-threshold 100
                       :decrunch-frontier-threshold 100
                       :decrunch-written-code-execution-threshold 1}
        result (-> (decoder/classifier-state configuration)
                   (decoder/consume-classification-chunk first-chunk)
                   (decoder/consume-classification-chunk second-chunk)
                   decoder/finish-classification)
        decruncher (some #(when (= :decruncher (:kind %)) %) (:activities result))]
    (is (= [10 30] (:event-range decruncher)))
    (is (some #{:cross-chunk-write-to-execute} (:signals decruncher)))
    (is (= [10 20] (:source-event-range
                    (first (:evidence decruncher)))))))

(deftest stable-self-modifying-raster-is-not-a-decruncher
  (let [unit (fn [start]
               {:unit-kind :irq-frame
                :event-range [start (+ start 100)]
                :irq-entry-pcs [0x1000 0x1100]
                :ram-address-range [0x2000 0x23ff]
                :ram-address-span 1024
                :ram-unique-address-count 800
                :ram-write-count 20000
                :overwritten-code-write-count 4000
                :written-code-execution-count 20})
        chunk {:chunk-number 1
               :event-range [0 300]
               :boundary {:kind :final}
               :stages {:classification
                        {:frame-evidence [(unit 0) (unit 100) (unit 200)]}}}
        result (-> (decoder/classifier-state {:hysteresis-units 1
                                              :decrunch-write-threshold 100
                                              :decrunch-overwrite-threshold 10
                                              :decrunch-unique-address-threshold 100
                                              :decrunch-written-code-execution-threshold 1})
                   (decoder/consume-classification-chunk chunk)
                   decoder/finish-classification)]
    (is (empty? (filter #(= :decruncher (:kind %)) (:activities result))))
    (is (= [[100 300]]
           (mapv :event-range
                 (filter #(= :self-modifying-raster (:kind %))
                         (:activities result)))))))

(deftest classifier-closes-demoparts-at-fingerprint-gaps
  (let [unit (fn [start fingerprint]
               {:event-range [start (+ start 10)]
                :fingerprint-id fingerprint
                :loader nil
                :decruncher nil})
        chunk {:chunk-number 1
               :event-range [0 50]
               :boundary {:kind :final}
               :stages {:classification
                        {:frame-evidence [(unit 0 "fp-a")
                                          (unit 10 "fp-a")
                                          (unit 20 nil)
                                          (unit 30 "fp-b")
                                          (unit 40 "fp-b")]}}}
        result (-> (decoder/classifier-state {:hysteresis-units 1
                                              :demopart-stability-units 2
                                              :lookback-units 2})
                   (decoder/consume-classification-chunk chunk)
                   decoder/finish-classification)]
    (is (= [[10 20] [40 50]]
           (mapv :event-range (:parts result))))))

(deftest classifier-applies-cross-chunk-hysteresis-and-overlapping-evidence
  (let [candidate {:confidence 0.9 :signals [:test-signal]}
        unit (fn [start fingerprint loader?]
               {:event-range [start (+ start 10)]
                :fingerprint-id fingerprint
                :loader (when loader? candidate)
                :decruncher nil})
        chunk (fn [number start units]
                {:chunk-number number
                 :event-range [start (+ start (* 10 (count units)))]
                 :boundary {:kind (if (= number 2) :final :forced-size)}
                 :stages {:classification {:frame-evidence units}}})
        configuration {:hysteresis-units 2
                       :demopart-stability-units 2
                       :lookback-units 4}
        state (-> (decoder/classifier-state configuration)
                  (decoder/consume-classification-chunk
                   (chunk 1 0 [(unit 0 "fp-a" true)
                               (unit 10 "fp-a" true)]))
                  (decoder/consume-classification-chunk
                   (chunk 2 20 [(unit 20 "fp-a" false)
                                (unit 30 "fp-a" false)])))
        result (decoder/finish-classification state)
        loader (some #(when (= :loader (:kind %)) %) (:activities result))
        part (some #(when (= :demopart (:kind %)) %) (:activities result))]
    (is (= 4 (get-in result [:summary :unit-count])))
    (is (= 2 (get-in result [:summary :physical-boundary-count])))
    (is (= [0 20] (:event-range loader)))
    (is (= [:test-signal] (:signals loader)))
    (is (= [10 40] (:event-range part)))
    (is (= "fp-a" (:fingerprint-id part)))
    (is (= 1 (count (:parts result))))
    (is (= 3 (get-in result [:summary :candidate-counts :demopart])))))

(defn- delete-tree!
  [file]
  (when (.isDirectory file)
    (doseq [child (.listFiles file)]
      (delete-tree! child)))
  (.delete file))

(deftest chunked-recorder-persists-local-artifacts-and-global-ranges
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder.capture %))
        directory (doto (java.io.File/createTempFile "omkamra-chunks-" "")
                    (.delete)
                    (.mkdirs))
        _ (.mkdirs (java.io.File. directory "chunks"))
        options ((private 'normalized-chunk-options)
                 {:chunk-max-events 2 :chunk-queue-capacity 2})
        metadata {:capture-id "chunk-test"
                  :input "/tmp/demo.prg"
                  :full-capture? true}
        manifest (atom ((private 'make-manifest) (.getPath directory)
                                                 metadata options))
        queue (java.util.concurrent.ArrayBlockingQueue. 2)
        writer-state (atom {:status :running :queue-depth 0 :high-water-mark 0
                            :chunks-written 0 :backpressure-count 0})
        writer ((private 'start-chunk-writer!) (.getPath directory) manifest
                                               queue writer-state)
        coordinator (atom {:open-chunk ((private 'open-chunk)
                                        1 0 (byte-array 65536) false)
                           :retain-samples? false
                           :writer-queue queue
                           :writer-state writer-state
                           :writer-thread writer})
        records [[0x1000 [0xea] 1 1 0 0 0 nil "........" nil]
                 [0x1001 [0x60] 1 2 0 0 0 nil "........" nil]
                 [0x1002 [0xea] 1 3 0 0 0 nil "........" nil]]]
    (try
      ((private 'write-manifest!) (.getPath directory) manifest)
      ((private 'ingest-chunked-batch!) coordinator metadata options records)
      ((private 'close-open-chunk!) coordinator metadata options
                                    {:kind :final :reason :stopped})
      ((private 'shutdown-chunk-writer!) coordinator 5000)
      (swap! manifest assoc :status :stopped :finalized? true)
      ((private 'write-manifest!) (.getPath directory) manifest)
      (is (not (.exists (io/file directory "analysis"))))
      (let [analysis (analysis/run! (.getPath directory) {:stage :all})
            structure-index (read-string
                             (slurp (io/file directory "analysis" "stages"
                                             "structure" "index.edn")))
            writes (read-string
                    (slurp (io/file directory "analysis" "stages"
                                    "writes" "chunks"
                                    "chunk-000001.edn")))
            structure (read-string
                       (slurp (io/file directory "analysis" "stages"
                                       "structure" "chunks"
                                       "chunk-000001.edn")))
            video (read-string
                   (slurp (io/file directory "analysis" "stages"
                                   "video" "chunks"
                                   "chunk-000001.edn")))
            assets (read-string
                    (slurp (io/file directory "analysis" "stages"
                                    "assets" "chunks"
                                    "chunk-000001.edn")))
            stored-manifest (decoder/read-capture-manifest (.getPath directory))
            first-chunk (decoder/read-chunk (.getPath directory) 1)
            second-chunk (decoder/read-chunk (.getPath directory) 2)]
        (is (= :omkamra.vice/capture-v1 (:format stored-manifest)))
        (is (= true (get-in stored-manifest [:options :full-capture?])))
        (is (= [[0 2] [2 3]] (mapv :event-range (:chunks stored-manifest))))
        (is (= {:kind :forced-size :reason :max-events}
               (:boundary first-chunk)))
        (is (= 2 (:local-event-count first-chunk)))
        (is (= #{:decoded :memory :structure :semantics}
               (set (keys (:stages first-chunk)))))
        (is (nil? (get-in first-chunk [:stages :video])))
        (is (nil? (get-in first-chunk
                          [:stages :structure :execution :node-versions])))
        (is (= 2 (:next-chunk first-chunk)))
        (is (= 1 (:previous-chunk second-chunk)))
        (is (= :final (get-in second-chunk [:boundary :kind])))
        (is (= 2 (:chunks-written @writer-state)))
        (is (= :complete (:status analysis)))
        (is (= [:writes :structure :video :assets :features :classification
                :segments :disassembly]
               (:executed-stages analysis)))
        (is (= 2 (:chunk-count structure-index)))
        (is (= :omkamra.vice/writes-chunk-v2 (:format writes)))
        (is (= {:stage :memory
                :key :writes
                :format :omkamra.vice/write-records-v1}
               (get-in writes [:stages :writes :source])))
        (is (= :omkamra.vice/structure-chunk-v1 (:format structure)))
        (is (map? (get-in structure [:stages :structure :execution])))
        (is (= :omkamra.vice/video-chunk-v1 (:format video)))
        (is (map? (get-in video [:stages :video :vic])))
        (is (= :omkamra.vice/assets-chunk-v1 (:format assets)))
        (is (vector? (get-in assets [:stages :assets :samples])))
        (is (= 2 (count (:chunks stored-manifest)))))
      (finally
        (delete-tree! directory)))))

(deftest chunk-writer-backpressure-is-explicit
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder.capture %))
        queue (java.util.concurrent.ArrayBlockingQueue. 1)
        writer-state (atom {:status :running})]
    (.put queue :already-full)
    (try
      (is (= :chunk-writer-backpressure
             (:reason (ex-data
                       (try
                         ((private 'enqueue-chunk!) queue writer-state
                                                    {:chunk-number 1} 1)
                         (catch clojure.lang.ExceptionInfo error error))))))
      (finally
        (.clear queue)))))

(deftest writer-failure-is-retained-for-capture-shutdown
  (let [private #(var-get (ns-resolve 'omkamra.vice.decoder.capture %))
        directory (doto (java.io.File/createTempFile "omkamra-writer-error-" "")
                    (.delete)
                    (.mkdirs))
        ;; A file at `chunks` makes the writer's chunk output path invalid.
        chunks-file (io/file directory "chunks")
        _ (spit chunks-file "not a directory")
        manifest (atom ((private 'make-manifest) (.getPath directory)
                                                 {:capture-id "writer-error" :input "x"}
                                                 ((private 'normalized-chunk-options) {})))
        queue (java.util.concurrent.ArrayBlockingQueue. 1)
        writer-state (atom {:status :running :queue-depth 0 :high-water-mark 0
                            :chunks-written 0 :backpressure-count 0})
        writer ((private 'start-chunk-writer!) (.getPath directory) manifest
                                               queue writer-state)]
    (try
      (.put queue {:chunk {:chunk-number 1
                           :event-range [0 1]
                           :boundary {:kind :final}
                           :summary {}}})
      (.join writer 5000)
      (is (= :failed (:status @writer-state)))
      (is (instance? Throwable (:error @writer-state)))
      (finally
        (delete-tree! directory)))))
