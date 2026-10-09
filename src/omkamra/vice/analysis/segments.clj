(ns omkamra.vice.analysis.segments
  "Semantic segmentation, boundary audits, and segment assembly selection."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [omkamra.vice.asm :as asm]
            [omkamra.vice.analysis.common :as common]
            [omkamra.vice.decoder.artifact :as artifact]
            [omkamra.vice.decoder.classification :as classification]
            [omkamra.vice.decoder.write :as write])
  (:import [java.nio.file AtomicMoveNotSupportedException Files StandardCopyOption]))

(defn interval-overlap?
  [[left-start left-end] [right-start right-end]]
  (and (< left-start right-end) (< right-start left-end)))

(def default-kernal-irq-handler
  "The KERNAL's installed-IRQ default (`$0314/$0315` = `$EA31`).

  An IRQ epoch that only ever resolves to this handler is the scheduler
  ticking while the main loop loads or decrunches, not a demo part; this demo
  installs a custom handler for every real part."
  0xea31)

(defn default-irq-handler-only?
  [frame]
  (let [handlers (or (:irq-handler-pcs frame) (:irq-entry-pcs frame))]
    (and (seq handlers)
         (every? #(= default-kernal-irq-handler %) handlers))))

(defn epoch-default-irq-handler?
  "True only when every IRQ frame in the epoch uses the KERNAL default handler.

  The epoch's stored `:signature` can be stale because the stability window
  rejects a later custom handler whose VIC write volume keeps changing.
  Inspecting the frames prevents a short default-handler lead-in from
  demoting a part that then installs its own raster handler."
  [frames]
  (and (seq frames)
       (every? default-irq-handler-only? frames)))

(defn frame-signature
  "Return the stable part identity: the set of installed IRQ handlers.

  A demopart is identified by the code that runs, not by how much VIC traffic
  a particular frame happens to emit. VIC write volume animates within a single
  part, so including it in the signature used to split epochs into fragments
  and to prevent a valid part from ever stabilising. The KERNAL IRQ entry is
  resolved to the installed handler upstream, so parts that share `$ff48` but
  install different handlers remain distinguishable, and handler-subset
  fluctuation is still screened by `topology-changed?`."
  [frame _configuration]
  (let [handlers (or (:irq-handler-pcs frame) (:irq-entry-pcs frame))]
    (when (seq handlers)
      {:irq-handler-pcs (->> handlers distinct sort vec)})))

(defn epoch-range
  [frames]
  [(first (:event-range (first frames)))
   (second (:event-range (last frames)))])

(declare meaningful-signature-change?)

(defn split-frame-epoch-at-signature-changes
  "Split a gap-contiguous frame epoch only after a new meaningful signature is stable.

  Small changes in VIC write volume are common animation noise. They remain in
  the current reviewable epoch until the configured signature-distance rules
  say that the change is meaningful.

  Candidate frames are held outside the prior epoch until confirmed. A short
  startup signature is absorbed into the first sustained signature rather than
  yielding a misleading tiny demopart."
  [{:keys [frames]}
   {:keys [min-demopart-events signature-stability-frames] :as configuration}]
  (let [frames (vec frames)]
    (if (empty? frames)
      []
      (loop [remaining (rest frames)
             current {:frames [(first frames)]
                      :signature (frame-signature (first frames) configuration)}
             candidate nil
             completed []]
        (if-let [frame (first remaining)]
          (let [signature (frame-signature frame configuration)
                current-signature (:signature current)]
            (cond
              ;; Missing IRQ evidence and a return to the existing signature
              ;; cannot establish an effect change. Preserve a pending
              ;; candidate as ordinary current activity in both cases.
              (or (nil? signature) (= signature current-signature))
              (recur (rest remaining)
                     (update current :frames into (concat (:frames candidate)
                                                          [frame]))
                     nil
                     completed)

              (= signature (:signature candidate))
              (let [candidate (update candidate :frames conj frame)]
                (if (>= (count (:frames candidate)) signature-stability-frames)
                  (let [current-range (epoch-range (:frames current))
                        signature-change
                        {:kind :stable-frame-signature-change
                         :from current-signature
                         :to signature
                         :confirmation-frame-count
                         signature-stability-frames}]
                    (cond
                      ;; The initial setup was too short to be a part. Keep it
                      ;; with the new stable effect rather than yielding a
                      ;; misleading tiny demopart.
                      (< (- (second current-range) (first current-range))
                         min-demopart-events)
                      (recur (rest remaining)
                             {:frames (into (:frames current)
                                            (:frames candidate))
                              :signature signature}
                             nil
                             completed)

                      ;; Long raster routines can change VIC write volume by a
                      ;; bucket or two while retaining the same IRQ topology.
                      ;; Keep that as evidence, but do not manufacture a new
                      ;; semantic part candidate from it.
                      (not (meaningful-signature-change?
                            signature-change configuration))
                      (recur (rest remaining)
                             (update current :frames into (:frames candidate))
                             nil
                             completed)

                      :else
                      (recur (rest remaining)
                             {:frames (:frames candidate)
                              :signature signature
                              :signature-change signature-change}
                             nil
                             (conj completed
                                   (assoc current :event-range current-range)))))
                  (recur (rest remaining) current candidate completed)))

              :else
              ;; A different second candidate invalidates the first one; the
              ;; first was transient, so fold it into the current epoch.
              (recur (rest remaining)
                     (update current :frames into (:frames candidate))
                     {:signature signature :frames [frame]}
                     completed)))
          (let [current (update current :frames into (:frames candidate))]
            (conj completed (assoc current :event-range
                                   (epoch-range (:frames current))))))))))

(defn frame-epochs
  [frames {:keys [frame-gap-events min-demopart-events] :as configuration}]
  (let [gap-epochs
        (reduce (fn [epochs frame]
                  (let [event-range (:event-range frame)
                        previous (peek epochs)]
                    (if (and previous
                             (<= (- (first event-range)
                                    (second (:event-range previous)))
                                 frame-gap-events))
                      (conj (pop epochs)
                            (-> previous
                                (assoc-in [:event-range 1]
                                          (second event-range))
                                (update :frames conj frame)))
                      (conj epochs {:event-range event-range
                                    :frames [frame]}))))
                []
                (sort-by (comp first :event-range) frames))]
    (->> gap-epochs
         (mapcat #(split-frame-epoch-at-signature-changes % configuration))
         (filter #(>= (- (second (:event-range %))
                         (first (:event-range %)))
                      min-demopart-events))
         vec)))

(defn activity-clusters
  [activities {:keys [activity-confidence transition-gap-events]}]
  (let [activities (->> activities
                        (filter #(contains? #{:loader :decruncher} (:kind %)))
                        (filter #(>= (:confidence %) activity-confidence))
                        (sort-by (comp first :event-range)))]
    (reduce (fn [clusters activity]
              (let [previous (peek clusters)
                    [start end] (:event-range activity)]
                (if (and previous
                         (<= (- start (second (:event-range previous)))
                             transition-gap-events))
                  (conj (pop clusters)
                        (-> previous
                            (assoc-in [:event-range 1]
                                      (max end (second (:event-range previous))))
                            (update :activities conj activity)))
                  (conj clusters {:event-range [start end]
                                  :activities [activity]}))))
            []
            activities)))

(defn source-slices
  [chunks segment-range]
  (->> chunks
       (keep (fn [{:keys [number] chunk-range :event-range}]
               (when (interval-overlap? chunk-range segment-range)
                 (let [[chunk-start chunk-end] chunk-range
                       [segment-start segment-end] segment-range]
                   {:chunk-number number
                    :event-range [(max chunk-start segment-start)
                                  (min chunk-end segment-end)]}))))
       vec))

(defn clipped-activity
  "Project an overlapping classifier activity into one segment's exact range.

  The classifier index remains the authoritative full activity view. Segment
  descriptors deliberately carry only the relevant evidence, preventing a
  long loader/decrunch activity from implying support everywhere it overlaps a
  demopart."
  [activity [segment-start segment-end :as segment-range]]
  (when (interval-overlap? (:event-range activity) segment-range)
    (let [[activity-start activity-end] (:event-range activity)
          activity-range [(max segment-start activity-start)
                          (min segment-end activity-end)]
          evidence (->> (:evidence activity)
                        (keep (fn [{:keys [start end] :as observation}]
                                (when (interval-overlap? [start end]
                                                         segment-range)
                                  (assoc observation
                                         :start (max start segment-start)
                                         :end (min end segment-end)))))
                        vec)]
      (cond-> (assoc activity
                     :source-event-range (:event-range activity)
                     :event-range activity-range
                     :evidence evidence)
        (not= activity-range (:event-range activity))
        (assoc :clipped? true)))))

(defn activity-role
  [activities]
  (let [roles (->> activities (map :kind) distinct sort vec)]
    {:role (case (count roles)
             0 :unclassified
             1 (first roles)
             :compound)
     :roles roles}))

(defn topology-changed?
  "Return true when two IRQ-handler sets represent a genuine part change.

  A raster part does not necessarily run every installed handler in every
  frame, so one frame's handler set is often a subset of another's. Treat a
  subset fluctuation as sampling noise rather than a boundary; require each set
  to contain a handler the other lacks."
  [from-handlers to-handlers]
  (let [from (set from-handlers)
        to (set to-handlers)]
    (and (not= from to)
         (not (or (set/subset? from to)
                  (set/subset? to from))))))

(defn signature-change-distance
  [{:keys [from to]}]
  (when (and from to)
    {:irq-topology-changed?
     (topology-changed? (:irq-handler-pcs from) (:irq-handler-pcs to))
     :d012-write-distance
     (Math/abs (long (- (:d012-write-count to 0)
                        (:d012-write-count from 0))))
     :vic-write-band-distance
     (Math/abs (long (- (:vic-write-band to 0)
                        (:vic-write-band from 0))))}))

(defn meaningful-signature-change?
  [signature-change configuration]
  (let [{:keys [irq-topology-changed? d012-write-distance
                vic-write-band-distance]}
        (signature-change-distance signature-change)]
    (or irq-topology-changed?
        (>= (or d012-write-distance 0)
            (:signature-d012-write-distance configuration))
        (>= (or vic-write-band-distance 0)
            (:signature-vic-write-distance configuration)))))

(defn epoch-confirmation
  "Return explicit reasons to promote a sustained IRQ epoch to a demopart.

  IRQ continuity alone is deliberately insufficient: an effect candidate stays
  materialized and reviewable until stable execution, a meaningful topology/
  VIC change, or a completed transition activity corroborates it."
  [{:keys [event-range signature-change]} activities configuration]
  (let [[start _] event-range
        classifier-demopart?
        (some #(and (= :demopart (:kind %))
                    (interval-overlap? (:event-range %) event-range))
              activities)
        meaningful-change? (meaningful-signature-change?
                            signature-change configuration)
        completed-transition?
        (some #(and (contains? #{:loader :decruncher} (:kind %))
                    (<= (second (:event-range %)) start)
                    (<= (- start (second (:event-range %)))
                        (:corroboration-gap-events configuration)))
              activities)]
    (cond-> []
      classifier-demopart? (conj :stable-execution)
      meaningful-change? (conj :meaningful-signature-change)
      completed-transition? (conj :completed-transition))))

(defn epoch-role
  "Make transition overlap explicit without presenting an unconfirmed epoch as
  a semantic demopart."
  [kind activities]
  (let [activity-roles (->> activities (map :kind) distinct sort vec)
        transition? (some #{:loader :decruncher} activity-roles)]
    {:role (cond
             transition? :compound
             (= kind :demopart) :demopart
             :else :effect-candidate)
     :roles (->> (conj activity-roles kind) distinct sort vec)}))

(defn boundary-confidence
  [confidence signals]
  {:start {:confidence confidence :signals signals}
   :end {:confidence confidence :signals signals}})

(defn subtract-intervals
  "Return the sub-ranges of `[start end]` not covered by `ranges`."
  [[start end] ranges]
  (loop [cursor start
         ranges (sort-by first ranges)
         result []]
    (if-let [[range-start range-end] (first ranges)]
      (if (<= range-end cursor)
        (recur cursor (rest ranges) result)
        (let [result (cond-> result
                       (< cursor range-start)
                       (conj [cursor (min end range-start)]))
              cursor (max cursor (min end range-end))]
          (if (< cursor end)
            (recur cursor (rest ranges) result)
            result)))
      (cond-> result (< cursor end) (conj [cursor end])))))

(defn custom-handler-frame?
  [frame]
  (let [handlers (or (:irq-handler-pcs frame) (:irq-entry-pcs frame))]
    (and (seq handlers)
         (not (every? #(= default-kernal-irq-handler %) handlers)))))

(defn transition-concurrent-with-epoch?
  "True when a transition range overlaps a custom-handler frame of an epoch.

  A trackmo loader can run inside a raster part's IRQ handler, so that
  transition genuinely overlaps the part. A main-loop decruncher or loader
  never runs a custom-handler frame, so its transition is exclusive."
  [transition epoch]
  (some #(and (custom-handler-frame? %)
              (interval-overlap? (:event-range transition) (:event-range %)))
        (:frames epoch)))

(defn epoch-exclusive-ranges
  "Clip a frame epoch to the ranges not owned by non-concurrent transitions."
  [epoch transitions]
  (subtract-intervals
   (:event-range epoch)
   (->> transitions
        (remove #(transition-concurrent-with-epoch? % epoch))
        (map :event-range))))

(defn epoch-descriptor
  "Build one part descriptor for one exclusive sub-range of a frame epoch."
  [classification epoch event-range configuration]
  (let [frames (filterv #(interval-overlap? (:event-range %) event-range)
                        (:frames epoch))
        activities (->> (:activities classification)
                        (keep #(clipped-activity % event-range))
                        vec)
        confirmation (epoch-confirmation (assoc epoch :event-range event-range)
                                         (:activities classification)
                                         configuration)
        default-handler? (epoch-default-irq-handler? frames)
        kind (cond
               default-handler? :effect-candidate
               (seq confirmation) :demopart
               :else :effect-candidate)
        signals (cond-> [:sustained-irq-frame-epoch]
                  (:signature-change epoch)
                  (conj :stable-frame-signature-change)
                  default-handler?
                  (conj :default-kernal-irq-handler)
                  (and (seq confirmation) (not default-handler?))
                  (into confirmation))
        confidence (if (= kind :demopart) 1.0 0.5)]
    (merge {:kind kind
            :event-range event-range
            :confidence confidence
            :signals signals
            :confirmation-reasons confirmation
            :default-irq-handler? default-handler?
            :custom-irq-handler? (not default-handler?)
            :boundary-confidence (boundary-confidence confidence signals)
            :frame-count (count frames)
            :frame-signature (:signature epoch)
            :signature-change (:signature-change epoch)
            :fingerprint-ids (->> frames (map :fingerprint-id) distinct vec)
            :activities activities}
           (epoch-role kind activities))))

(defn aggregate-write-pcs
  "Sum per-PC write counts for one hardware domain across an event range."
  [units event-range domain-key]
  (reduce (fn [counts unit]
            (if (interval-overlap? (:event-range unit) event-range)
              (reduce-kv (fn [counts pc n]
                           (update counts pc (fnil + 0) (long n)))
                         counts
                         (get unit domain-key))
              counts))
          {}
          units))

(defn routine-descriptor
  "Describe the compact code cluster that writes one hardware domain.

  A raster routine is identified by the VIC registers it pokes and a music
  routine by the SID registers, symmetrically. The returned anchors are the
  write PCs, which the disassembly renderer uses to select exactly the basic
  blocks that belong to the routine."
  [write-pcs kind {:keys [routine-min-pc-count routine-pc-relative-threshold]}]
  (when (seq write-pcs)
    (let [max-count (long (apply max (vals write-pcs)))
          minimum (max (long routine-min-pc-count)
                       (long (Math/ceil (* (double routine-pc-relative-threshold)
                                           max-count))))
          anchors (->> write-pcs
                       (keep (fn [[pc n]] (when (>= (long n) minimum) pc)))
                       set)]
      (when (seq anchors)
        {:kind kind
         :anchor-pcs anchors
         :pc-range [(apply min anchors) (apply max anchors)]
         :pc-count (count anchors)
         :write-count (reduce + 0 (map #(long (get write-pcs % 0)) anchors))
         :max-pc-count max-count}))))

(defn attach-routines
  "Attach detected raster and music routines to each demopart descriptor."
  [descriptors units configuration]
  (let [configuration (common/normalize-segment-configuration configuration)]
    (mapv (fn [descriptor]
            (if (contains? #{:demopart :effect-candidate} (:kind descriptor))
              (let [event-range (:event-range descriptor)
                    music (routine-descriptor
                           (aggregate-write-pcs units event-range :sid-write-pcs)
                           :music configuration)
                    raster (routine-descriptor
                            (aggregate-write-pcs units event-range :vic-write-pcs)
                            :raster configuration)]
                (cond-> descriptor
                  music (assoc :music-routine music)
                  raster (assoc :raster-routine raster)))
              descriptor))
          descriptors)))

(defn segment-descriptors
  "Combine classifier activities and sustained feature-frame epochs.

  Activities remain capable of overlapping in the classifier index. A segment
  has one explicit role (or a `:compound` role), and receives only clipped
  activity evidence for its own event range; source slices remain exact,
  non-overlapping materialization inputs. Part epochs are clipped against
  non-concurrent loader/decruncher transitions so each materialization contains
  code with one responsibility; a concurrently running trackmo loader is kept
  and marked `:compound` instead.
  "
  [capture-manifest classification frames configuration]
  (let [configuration (common/normalize-segment-configuration configuration)
        chunks (:chunks capture-manifest)
        ;; Cluster each behaviour independently so a fast-loader and the
        ;; decrunchers on either side of it become separate segments instead of
        ;; one compound file-swap transition.
        transitions (concat
                     (activity-clusters
                      (filter #(= :loader (:kind %)) (:activities classification))
                      configuration)
                     (activity-clusters
                      (filter #(= :decruncher (:kind %)) (:activities classification))
                      configuration))
        demoparts (frame-epochs frames configuration)
        descriptors
        (concat
         (map (fn [{:keys [event-range activities]}]
                (let [confidence (apply max 0.0 (map :confidence activities))
                      signals (->> activities (mapcat :signals) distinct sort vec)
                      activities (mapv #(clipped-activity % event-range) activities)]
                  (merge {:kind :transition
                          :event-range event-range
                          :confidence confidence
                          :activities activities
                          :signals signals
                          :boundary-confidence
                          (boundary-confidence confidence signals)}
                         (activity-role activities))))
              transitions)
         (mapcat (fn [epoch]
                   (for [event-range (epoch-exclusive-ranges epoch transitions)
                         :when (>= (- (second event-range) (first event-range))
                                   (:min-demopart-events configuration))]
                     (epoch-descriptor classification epoch event-range
                                       configuration)))
                 demoparts))]
    (->> descriptors
         (sort-by (juxt (comp first :event-range) :kind))
         (map-indexed (fn [index descriptor]
                        (assoc descriptor
                               :format :omkamra.vice/semantic-segment-v1
                               :segment-id (inc index)
                               :source-chunks
                               (source-slices chunks (:event-range descriptor)))))
         vec)))

(defn segment-assembly-file
  [segment-id]
  (str "analysis/stages/disassembly/assemblies/"
       (format "segment-%06d.asm" segment-id)))

(defn merged-ranges
  [ranges]
  (reduce (fn [merged [start end]]
            (let [[previous-start previous-end] (peek merged)]
              (if (and previous-start (<= start previous-end))
                (conj (pop merged) [previous-start (max previous-end end)])
                (conj merged [start end]))))
          []
          (sort-by first ranges)))

(defn uncovered-ranges
  [capture-range covered]
  (let [[capture-start capture-end] capture-range]
    (loop [cursor capture-start
           [[start end] & remaining] covered
           gaps []]
      (if start
        (recur (max cursor end)
               remaining
               (cond-> gaps
                 (< cursor start) (conj [cursor start])))
        (cond-> gaps
          (< cursor capture-end) (conj [cursor capture-end]))))))

(defn segment-boundary-audit
  "Produce a compact, repeatable manual-review report for semantic boundaries."
  [capture-manifest descriptors]
  (let [capture-range [(first (:event-range (first (:chunks capture-manifest))))
                       (second (:event-range (last (:chunks capture-manifest))))]
        descriptors (vec (sort-by (comp first :event-range) descriptors))
        audit-segments
        (mapv (fn [{:keys [segment-id file-id kind role roles event-range confidence
                           boundary-confidence signals source-chunks activities
                           frame-signature signature-change confirmation-reasons]}]
                {:segment-id segment-id
                 :file-id file-id
                 :kind kind
                 :role role
                 :roles roles
                 :event-range event-range
                 :confidence confidence
                 :boundary-confidence boundary-confidence
                 :signals signals
                 :frame-signature frame-signature
                 :signature-change signature-change
                 :source-chunks source-chunks
                 :assembly-file (segment-assembly-file segment-id)
                 :confirmation-reasons confirmation-reasons
                 :activities
                 (mapv #(select-keys % [:kind :event-range :source-event-range
                                        :clipped? :confidence :signals])
                       activities)})
              descriptors)
        adjacencies
        (mapv (fn [left right]
                (let [distance (- (first (:event-range right))
                                  (second (:event-range left)))]
                  {:left-segment-id (:segment-id left)
                   :right-segment-id (:segment-id right)
                   :left-event-range (:event-range left)
                   :right-event-range (:event-range right)
                   :event-distance distance
                   :relationship (cond
                                   (neg? distance) :overlap
                                   (pos? distance) :gap
                                   :else :touching)
                   :shared-activity-kinds
                   (->> (concat (:activities left) (:activities right))
                        (map :kind) frequencies
                        (keep (fn [[kind count]]
                                (when (> count 1) kind)))
                        sort vec)}))
              descriptors (rest descriptors))
        covered (merged-ranges (map :event-range descriptors))
        gaps (uncovered-ranges capture-range covered)]
    {:format :omkamra.vice/segment-boundary-audit-v1
     :capture-id (:capture-id capture-manifest)
     :capture-event-range capture-range
     :summary {:segment-count (count audit-segments)
               :adjacency-count (count adjacencies)
               :overlap-count (count (filter #(= :overlap (:relationship %))
                                             adjacencies))
               :gap-count (count gaps)
               :unclassified-event-count
               (reduce + 0 (map #(- (second %) (first %)) gaps))}
     :segments audit-segments
     :adjacencies adjacencies
     :unclassified-gaps gaps}))

(defn feature-units-for-range
  [units event-range]
  (->> units
       (filter #(interval-overlap? (:event-range %) event-range))
       vec))

(defn gap-evidence-summary
  "Summarize persisted classifier units inside one uncovered event range.

  This is deliberately derived from the actual feature trace rather than from
  segment labels, so a gap can explain why the segmenter did not promote it."
  [units event-range]
  (let [units (feature-units-for-range units event-range)
        event-count (reduce + 0 (map (fn [[start end]]
                                       (- end start))
                                     (map :event-range units)))
        write-count (reduce + 0 (map #(long (:write-count % 0)) units))
        ram-write-count (reduce + 0 (map #(long (:ram-write-count % 0)) units))
        iec-write-count (reduce + 0 (map #(long (:iec-register-write-count % 0)) units))
        kernal-execution-count
        (reduce + 0 (map #(long (:kernal-iec-execution-count % 0)) units))
        kernal-transfer-count
        (reduce + 0 (map #(long (:kernal-iec-transfer-count % 0)) units))
        irq-count (count (filter #(= :irq-frame (:unit-kind %)) units))
        peripheral-write-count (- write-count ram-write-count)
        written-code-execution-count
        (reduce + 0 (map #(long (:written-code-execution-count % 0)) units))
        max-written-code-execution
        (apply max 0 (map #(long (:written-code-execution-count % 0)) units))
        frontier (apply max 0 (map #(Math/abs
                                     (long (:ram-frontier-movement % 0))) units))
        unit-count (count units)
        mode-counts (frequencies (map classification/traffic-mode units))
        mode-ratio (fn [mode]
                     (if (pos? unit-count)
                       (/ (double (get mode-counts mode 0)) unit-count)
                       0.0))
        serial-of (fn [unit]
                    (long (or (:serial-access-count unit)
                              (+ (:serial-read-count unit 0)
                                 (:serial-write-count unit 0)))))
        serial-access-count (reduce + 0 (map serial-of units))
        serial-active-units (count (filter #(pos? (serial-of %)) units))
        max-serial-access-count (apply max 0 (map serial-of units))]
    {:event-range event-range
     :unit-count (count units)
     :unit-kind-counts (frequencies (map :unit-kind units))
     :traffic-mode-counts mode-counts
     :transfer-unit-ratio (mode-ratio :transfer)
     :decode-unit-ratio (mode-ratio :decode)
     :raster-unit-ratio (mode-ratio :raster)
     :serial-access-count serial-access-count
     :serial-active-unit-ratio (if (pos? unit-count)
                                 (/ (double serial-active-units) unit-count)
                                 0.0)
     :max-serial-access-count max-serial-access-count
     :event-count event-count
     :write-count write-count
     :ram-write-count ram-write-count
     :peripheral-write-count peripheral-write-count
     :peripheral-write-ratio (if (pos? write-count)
                               (/ (double peripheral-write-count) write-count)
                               0.0)
     :write-density (if (pos? event-count)
                      (/ (double write-count) event-count)
                      0.0)
     :irq-frame-count irq-count
     :iec-register-write-count iec-write-count
     :kernal-iec-execution-count kernal-execution-count
     :kernal-iec-transfer-count kernal-transfer-count
     :iec-only-transport? (and (pos? iec-write-count)
                               (zero? kernal-execution-count)
                               (zero? kernal-transfer-count))
     :written-code-execution-count written-code-execution-count
     :max-written-code-execution max-written-code-execution
     :max-frontier-movement frontier
     :ram-footprint-count
     (count (distinct (keep :ram-footprint-id units)))}))

(defn gap-cause
  [summary {:keys [gap-write-density-threshold
                   gap-peripheral-ratio-threshold gap-iec-write-threshold
                   gap-transfer-unit-ratio-threshold gap-transfer-unit-accesses
                   gap-decode-ratio-threshold
                   gap-transition-execution-threshold
                   gap-transition-frontier-threshold]}]
  (cond
    (zero? (:unit-count summary))
    :low-evidence

    (and (zero? (:irq-frame-count summary))
         (zero? (:write-count summary)))
    :low-evidence

    ;; Serial-register access sustained across the phase names a transfer.
    ;; A load samples the bus by reading `$dd00`, so this is access based, and
    ;; it outranks the peripheral alias heuristic that would say raster-heavy.
    (and (>= (or (:serial-active-unit-ratio summary) 0.0)
             gap-transfer-unit-ratio-threshold)
         (>= (or (:max-serial-access-count summary) 0)
             gap-transfer-unit-accesses))
    :loader-transfer

    (or (>= (or (:peripheral-write-ratio summary) 0.0)
            gap-peripheral-ratio-threshold)
        (>= (or (:iec-register-write-count summary) 0)
            gap-iec-write-threshold))
    :raster-heavy

    ;; RAM-dominant phases that never touch the serial bus decode data that is
    ;; already in memory, which is exactly a memory-to-memory decruncher.
    (and (>= (or (:decode-unit-ratio summary) 0.0) gap-decode-ratio-threshold)
         (zero? (or (:serial-access-count summary) 0)))
    :decruncher-stretch

    (and (or (>= (:max-written-code-execution summary)
                 gap-transition-execution-threshold)
             (and (>= (:max-frontier-movement summary)
                      gap-transition-frontier-threshold)
                  (>= (:max-written-code-execution summary)
                      (quot gap-transition-execution-threshold 4))))
         (pos? (:ram-write-count summary)))
    :candidate-transition

    (>= (:write-density summary) gap-write-density-threshold)
    :write-heavy

    (zero? (:irq-frame-count summary))
    :no-irq

    :else
    :low-evidence))

(defn uncovered-range-diagnostics
  "Classify every uncovered range using feature evidence from the raw trace."
  [audit units configuration]
  (let [configuration (common/normalize-segment-configuration configuration)]
    (mapv (fn [event-range]
            (let [evidence (gap-evidence-summary units event-range)]
              (assoc evidence :cause (gap-cause evidence configuration))))
          (:unclassified-gaps audit))))

(defn short-gap-diagnostics
  "Audit short gaps without silently merging them.

  A short gap is a candidate continuation only when both sides retain the
  same coarse frame signature. Otherwise it is a transition requiring review;
  the feature summary records whether the missing interval is write/raster
  heavy or merely absent from the classifier's units."
  [audit descriptors units configuration]
  (let [configuration (common/normalize-segment-configuration configuration)
        by-id (into {} (map (juxt :segment-id identity) descriptors))]
    (->> (:adjacencies audit)
         (filter #(and (= :gap (:relationship %))
                       (<= (:event-distance %)
                           (:gap-short-events configuration))))
         (mapv (fn [{:keys [left-segment-id right-segment-id]
                     :as adjacency}]
                 (let [left (get by-id left-segment-id)
                       right (get by-id right-segment-id)
                       evidence (gap-evidence-summary
                                 units
                                 [(second (:left-event-range adjacency))
                                  (first (:right-event-range adjacency))])
                       same-signature? (and (:frame-signature left)
                                            (= (:frame-signature left)
                                               (:frame-signature right)))]
                   (assoc adjacency
                          :kind :short-gap
                          :cause (gap-cause evidence configuration)
                          :evidence evidence
                          :recommendation
                          (if same-signature?
                            :candidate-continuation
                            :candidate-transition))))))))

(defn provisional-file-boundaries
  "Use clustered loader-transfer phases as provisional boundaries between files.

  A single fast-loader can emit several bounded serial phases as the bus
  momentarily goes idle, so the file boundary is the start of the *clustered*
  transfer, not of every phase. Otherwise one file load fragments the capture
  into many spurious files."
  [capture-manifest classification configuration]
  (let [configuration (common/normalize-segment-configuration configuration)
        capture-start (first (:event-range (first (:chunks capture-manifest))))
        capture-end (second (:event-range (last (:chunks capture-manifest))))
        loader-clusters (activity-clusters
                         (filter #(= :loader (:kind %)) (:activities classification))
                         configuration)
        boundaries (mapv (fn [cluster]
                           {:event-index (first (:event-range cluster))
                            :activity (select-keys cluster
                                                   [:event-range :confidence
                                                    :signals])})
                         loader-clusters)
        points (vec (concat [capture-start]
                            (map :event-index boundaries)
                            [capture-end]))]
    {:boundaries boundaries
     :ranges (mapv (fn [index]
                     {:file-id (inc index)
                      :event-range [(nth points index) (nth points (inc index))]
                      :boundary-before (when (pos? index)
                                         (nth boundaries (dec index)))
                      :boundary-after (nth boundaries index nil)})
                   (range (dec (count points))))}))

(defn attach-file-groups
  "Attach provisional file IDs and summarize candidate-part counts."
  [capture-manifest classification descriptors configuration]
  (let [configuration (common/normalize-segment-configuration configuration)
        {:keys [ranges boundaries]}
        (provisional-file-boundaries capture-manifest classification configuration)
        file-id-for (fn [event-index]
                      (or (some (fn [{:keys [file-id event-range]}]
                                  (when (and (<= (first event-range) event-index)
                                             (< event-index (second event-range)))
                                    file-id))
                                ranges)
                          (:file-id (last ranges))))
        descriptors (mapv #(assoc % :file-id
                                  (file-id-for (first (:event-range %))))
                          descriptors)
        expected (or (:expected-parts-per-file configuration) [3 4])
        groups (mapv (fn [{:keys [file-id] :as range}]
                       (let [members (filter #(= file-id (:file-id %)) descriptors)
                             candidates (filter #(and (contains? #{:demopart
                                                                   :effect-candidate}
                                                                 (:kind %))
                                                      (not (:default-irq-handler? %)))
                                                members)
                             confirmed (filter #(= :demopart (:kind %)) candidates)
                             count (count candidates)]
                         (assoc range
                                :segment-ids (mapv :segment-id members)
                                :transition-segment-ids
                                (mapv :segment-id (filter #(= :transition (:kind %))
                                                          members))
                                :part-candidate-segment-ids
                                (mapv :segment-id candidates)
                                :confirmed-demopart-segment-ids
                                (mapv :segment-id confirmed)
                                :part-candidate-count count
                                :expected-part-count expected
                                :part-count-status
                                (cond
                                  (< count (first expected)) :underfull
                                  (> count (second expected)) :overfull
                                  :else :within-range))))
                     ranges)]
    {:boundaries boundaries
     :ranges groups
     :segments descriptors}))

(defn boundary-audit-text
  [{:keys [capture-id capture-event-range summary segments adjacencies
           unclassified-gaps short-gaps file-grouping]}]
  (str "; semantic segment boundary audit\n"
       "; capture " capture-id ", event range " capture-event-range "\n"
       "; segments " (:segment-count summary)
       ", overlaps " (:overlap-count summary)
       ", unclassified gaps " (:gap-count summary) "\n\n"
       (apply str
              (map (fn [{:keys [segment-id kind role event-range confidence
                                signals assembly-file activities confirmation-reasons]}]
                     (str "segment " segment-id " " (name kind) "/" (name role)
                          " " event-range " confidence " confidence "\n"
                          "  signals " signals "\n"
                          "  confirmation " confirmation-reasons "\n"
                          "  assembly " assembly-file "\n"
                          (apply str
                                 (map #(str "  activity " (:kind %) " "
                                            (:event-range %) ""
                                            (when (:clipped? %) " (clipped)")
                                            " " (:signals %) "\n")
                                      activities))))
                   segments))
       "\nadjacent segment relationships\n"
       (apply str
              (map #(str "  " (:left-segment-id %) " -> " (:right-segment-id %)
                         " " (name (:relationship %)) " "
                         (:event-distance %) " events\n")
                   adjacencies))
       "\nunclassified ranges\n"
       (apply str
              (map #(str "  " (:event-range %) " cause " (:cause %)
                         " density " (:write-density %)
                         " irq-frames " (:irq-frame-count %) "\n")
                   unclassified-gaps))
       "\nshort gaps\n"
       (apply str
              (map #(str "  " (:left-segment-id %) " -> "
                         (:right-segment-id %) " " (:event-distance %)
                         " cause " (:cause %)
                         " recommendation " (:recommendation %) "\n")
                   short-gaps))
       "\nprovisional file groups\n"
       (apply str
              (map #(str "  file " (:file-id %) " " (:event-range %)
                         " candidates " (:part-candidate-count %)
                         " status " (:part-count-status %) "\n")
                   (:ranges file-grouping)))))

(defn atomic-write-text!
  [file text]
  (let [target (.toPath (io/file file))
        partial (.toPath (io/file (str file ".partial")))]
    (.mkdirs (.getParentFile (.toFile target)))
    (spit (.toFile partial) text)
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

(defn stage-output-directory
  [completed-stages active-stage-directories stage-id]
  (or (get active-stage-directories stage-id)
      (get-in completed-stages [stage-id :output-directory])
      (throw (ex-info "Required completed analysis stage is unavailable"
                      {:stage stage-id}))))

(defn stage-root
  [capture-directory output-directory]
  (let [directory (io/file output-directory)]
    (if (.isAbsolute directory)
      directory
      (io/file capture-directory output-directory))))

(defn read-completed-stage-index
  [capture-directory completed-stages active-stage-directories stage-id]
  (let [output-directory (stage-output-directory completed-stages
                                                 active-stage-directories
                                                 stage-id)]
    (edn/read-string (slurp (io/file (stage-root capture-directory
                                                 output-directory)
                                     "index.edn")))))

(defn completed-feature-evidence
  [capture-directory completed-stages active-stage-directories]
  (let [feature-index (read-completed-stage-index capture-directory
                                                  completed-stages
                                                  active-stage-directories
                                                  :features)
        feature-directory (stage-output-directory completed-stages
                                                  active-stage-directories
                                                  :features)]
    {:frames (mapcat (fn [{:keys [number]}]
                       (let [chunk (edn/read-string
                                    (slurp (common/stage-chunk-file
                                            (stage-root capture-directory
                                                        feature-directory)
                                            number)))]
                         (get-in chunk [:stages :features :frames])))
                     (:chunks feature-index))
     :units (mapcat (fn [{:keys [number]}]
                      (let [chunk (edn/read-string
                                   (slurp (common/stage-chunk-file
                                           (stage-root capture-directory
                                                       feature-directory)
                                           number)))]
                        (get-in chunk [:stages :features :classifier-units])))
                    (:chunks feature-index))}))

(defn completed-feature-frames
  [capture-directory completed-stages active-stage-directories]
  (:frames (completed-feature-evidence capture-directory completed-stages
                                       active-stage-directories)))

(defn completed-feature-units
  [capture-directory completed-stages active-stage-directories]
  (:units (completed-feature-evidence capture-directory completed-stages
                                      active-stage-directories)))

(defn u8-value
  [value]
  (bit-and (int value) 0xff))

(defn rom-visible?
  [address cpu-port]
  (let [address (int address)
        cpu-port (u8-value cpu-port)
        loram? (pos? (bit-and cpu-port 0x01))
        hiram? (pos? (bit-and cpu-port 0x02))]
    (cond
      ;; HIRAM maps the KERNAL ROM into $e000-$ffff. When it is clear,
      ;; those addresses are writable RAM and must remain disassembled.
      (<= 0xe000 address 0xffff) hiram?
      ;; BASIC ROM is visible in $a000-$bfff only when both LORAM and HIRAM
      ;; are set. Otherwise this range is writable RAM or unmapped.
      (<= 0xa000 address 0xbfff) (and loram? hiram?)
      :else false)))

(defn port-info
  "Extract the compact CPU-port `$01` timeline needed for ROM filtering.

  A raw chunk is dominated by its 64 KiB memory snapshot and inferred writes.
  Persisting only the initial `$01` byte and the writes to `$01` lets the
  disassembly stage reconstruct `cpu-port-values` without re-parsing the raw
  chunk, which is an order of magnitude larger than its structural view."
  [raw-chunk]
  (let [initial-memory (get-in raw-chunk [:stages :memory :initial])]
    {:initial-port (when initial-memory
                     (u8-value (nth initial-memory 1 0)))
     :port-writes (when initial-memory
                    (->> (get-in raw-chunk [:stages :memory :writes])
                         write/write-records
                         (filter #(= 1 (:address %)))
                         (mapv (fn [write]
                                 [(:event-index write)
                                  (u8-value (:value write))]))))}))

(defn cpu-port-values*
  "Rebuild the `$01` value immediately before each local event.

  A write recorded at event N takes effect after instruction N, hence the
  strict event-index comparison while advancing the write cursor."
  [initial-port port-writes event-count]
  (let [ports (int-array (max 0 event-count))]
    (loop [event-index 0
           port (u8-value (or initial-port 0))
           write-index 0]
      (when (< event-index event-count)
        (let [[port write-index]
              (loop [port port
                     write-index write-index]
                (if (and (< write-index (count port-writes))
                         (< (first (nth port-writes write-index)) event-index))
                  (recur (u8-value (second (nth port-writes write-index)))
                         (inc write-index))
                  [port write-index]))]
          (aset-int ports event-index port)
          (recur (inc event-index) port write-index))))
    ports))

(defn cpu-port-values
  "Reconstruct the CPU-port timeline directly from a raw chunk."
  [raw-chunk event-count]
  (let [{:keys [initial-port port-writes]} (port-info raw-chunk)]
    (cpu-port-values* initial-port port-writes event-count)))

(defn raw-chunk->render-chunk
  "Project a raw chunk onto the inputs the assembly renderer needs.

  The structural execution (including its `:block-runs`) is already persisted
  by the structure stage, so callers that avoid re-reading the raw chunk can
  supply the same keys from the compact structure chunk plus `port-info`."
  [raw-chunk]
  (let [{:keys [initial-port port-writes]} (port-info raw-chunk)
        execution (get-in raw-chunk [:stages :structure :execution])]
    {:chunk-number (:chunk-number raw-chunk)
     :event-range (:event-range raw-chunk)
     :event-count (:event-count execution)
     :execution execution
     :block-runs (:block-runs execution)
     :initial-port initial-port
     :port-writes port-writes}))

(defn visible-instruction-ids
  [execution ports block-runs local-start local-end]
  (let [block-by-id (into {} (map (juxt :id identity) (:blocks execution)))]
    (loop [position 0
           visible #{}
           [[block-id iterations] & runs] block-runs]
      (if (nil? block-id)
        visible
        (let [instruction-ids (:instruction-ids (get block-by-id block-id))
              width (count instruction-ids)
              end (+ position (* width iterations))
              visible
              (reduce
               (fn [visible local-position]
                 (let [instruction-id
                       (nth instruction-ids (mod (- local-position position) width))
                       instruction (nth (:instructions execution) instruction-id)]
                   (if (and (<= local-start local-position)
                            (< local-position local-end)
                            (not (rom-visible?
                                  (:address instruction)
                                  (aget ^ints ports local-position))))
                     (conj visible instruction-id)
                     visible)))
               visible
               (range (max position local-start)
                      (min end local-end)))]
          (recur end visible runs))))))

(defn block-anchor?
  "True when a block contains one of the routine's writing PCs."
  [execution block anchor-pcs]
  (some #(contains? anchor-pcs (:address (nth (:instructions execution) %)))
        (:instruction-ids block)))

(defn selected-execution
  "Select templates whose run-length encoded occurrences overlap a segment.

  When `omit-rom?` is true, instruction IDs are retained only when an
  occurrence in the selected range executed with BASIC and KERNAL ROM
  unmapped. This uses the live CPU port rather than treating address ranges
  alone as ROM; `$a000-$bfff` and `$e000-$ffff` can contain RAM.

  When `anchor-pcs` is supplied, only blocks that contain one of those writing
  PCs are retained. This carves a single routine (raster or music) out of a
  demopart whose frames run several routines at once."
  ([render-chunk event-range omit-rom?]
   (selected-execution render-chunk event-range omit-rom? nil))
  ([render-chunk [segment-start segment-end] omit-rom? anchor-pcs]
   (let [chunk-start (first (:event-range render-chunk))
         local-start (max 0 (- segment-start chunk-start))
         local-end (min (:event-count render-chunk)
                        (- segment-end chunk-start))
         execution (:execution render-chunk)
         block-runs (:block-runs render-chunk)
         blocks (:blocks execution)
         block-by-id (into {} (map (juxt :id identity) blocks))
         selected-ids
         (loop [position 0
                selected #{}
                [[block-id iterations] & runs] block-runs]
           (if (nil? block-id)
             selected
             (let [width (count (:instruction-ids (get block-by-id block-id)))
                   end (+ position (* width iterations))]
               (recur end
                      (if (interval-overlap? [position end]
                                             [local-start local-end])
                        (conj selected block-id)
                        selected)
                      runs))))
         routine-ids (if anchor-pcs
                       (set (filter #(and (contains? selected-ids (:id %))
                                          (block-anchor? execution % anchor-pcs))
                                    blocks))
                       nil)
         selected-ids (if routine-ids
                        (set (map :id routine-ids))
                        selected-ids)
         visible-ids (if omit-rom?
                       (visible-instruction-ids
                        execution
                        (cpu-port-values* (:initial-port render-chunk)
                                          (:port-writes render-chunk)
                                          (:event-count render-chunk))
                        block-runs local-start local-end)
                       (set (mapcat :instruction-ids
                                    (filter #(contains? selected-ids (:id %))
                                            blocks))))]
     (assoc execution
            :blocks
            (->> blocks
                 (filter #(contains? selected-ids (:id %)))
                 (map (fn [block]
                        (update block :instruction-ids
                                #(vec (filter visible-ids %)))))
                 (filter #(seq (:instruction-ids %)))
                 vec)))))

(defn write-routine-assembly!
  "Materialize only the basic blocks that belong to one raster/music routine."
  [output-file {:keys [segment-id event-range source-chunks]}
   {:keys [kind anchor-pcs pc-range pc-count write-count]} omit-rom?
   render-chunk-reader]
  (with-open [writer (io/writer output-file)]
    (.write writer (format "; %s routine for segment %d, event range %s\n"
                           (name kind) segment-id event-range))
    (when (and pc-count write-count pc-range)
      (.write writer (format "; %d anchor PCs, %d writes, code range %s\n"
                             pc-count write-count pc-range)))
    (asm/write-executions-assembly!
     writer
     (keep (fn [{:keys [chunk-number event-range]}]
             (let [execution (selected-execution
                              (render-chunk-reader chunk-number)
                              event-range
                              omit-rom?
                              anchor-pcs)]
               (when (seq (:blocks execution)) execution)))
           source-chunks)))
  output-file)

(defn write-segment-assembly!
  ([capture-directory output-file segment omit-rom?]
   (write-segment-assembly!
    capture-directory output-file segment omit-rom?
    (fn [chunk-number]
      (raw-chunk->render-chunk
       (artifact/read-chunk capture-directory chunk-number)))))
  ([_capture-directory output-file
    {:keys [segment-id kind role event-range source-chunks activities]} omit-rom?
    render-chunk-reader]
   (with-open [writer (io/writer output-file)]
     (.write writer (format "; semantic segment %d, %s/%s, event range %s\n"
                            segment-id (name kind) (name role) event-range))
     (when (= :compound role)
       (.write writer
               (str "; this materialization includes overlapping transition"
                    " activity; see the segment boundary audit\n"))
       (doseq [{:keys [kind event-range signals]} activities
               :when (contains? #{:loader :decruncher} kind)]
         (.write writer (format "; overlapping %s %s %s\n"
                                (name kind) event-range signals))))
     ;; `keep` supplies each selected execution only when the global renderer
     ;; consumes it. The renderer retains one structural template dictionary,
     ;; rather than re-emitting the same block for every source chunk.
     (asm/write-executions-assembly!
      writer
      (keep (fn [{:keys [chunk-number event-range]}]
              (let [execution (selected-execution
                               (render-chunk-reader chunk-number)
                               event-range
                               omit-rom?)]
                (when (seq (:blocks execution)) execution)))
            source-chunks)))
   output-file))

