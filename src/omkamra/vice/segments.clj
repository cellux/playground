(ns omkamra.vice.segments
  "Execution-span assembly into semantic segments and file groups.

  `:segments` consumes `:classification` spans, `:features` handoffs, and the
  `:dedupe` context table. It never rediscovers loader, decruncher, raster, or
  demopart boundaries from raw structure.

  A demo's *parts* are identified structurally, not by activity gaps: a part is
  a stable set of display IRQ handlers driving the frame, and a boundary is
  where that handler set changes. This is robust for both multi-part demos
  (whose next file loads or decrunches while the current part plays) and
  trackmos (whose loading and decrunching run inside IRQ handlers alongside the
  effects).

  Concretely:

  * display handlers are the demo's own interrupt handlers, i.e. `:irq`/`:nmi`
    contexts whose resolved handler lives outside the KERNAL ROM; the default
    KERNAL IRQ routine is ignored so it cannot define a part;
  * the timeline is bucketed, each bucket's active handler set is collapsed
    into runs, and a short run whose handler set nests inside a neighbour's is
    treated as a *transition bridge* and merged (e.g. `{7} {7 8} {8}`);
  * each remaining run becomes a segment: a run with handlers or visible
    raster effects is a `:demopart`; a run with neither is the `:transition`
    region between parts;
  * `:raster`, `:music`, `:loader`, and `:decruncher` are all evidence *roles*.
    Loader and decruncher do not define boundaries; each segment carries
    `:phases`, the merged role intervals overlapping it, so a decruncher that
    runs under a live handler set (or straddles a part boundary) is still
    visible rather than being absorbed into a part or splitting one;
  * file groups are split after a `:transition` segment whose roles include
    `:loader`;
  * a boundary audit covers gaps, overlaps, hard write-to-execute handoffs, and
    role evidence.

  Every descriptor uses global logical event coordinates; no descriptor refers
  to a raw capture chunk.

  Generation layout:

  ```text
  analysis/segments/<generation>/
    index.edn
    boundary-audit.edn
  ```"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.features :as features]
            [omkamra.vice.store :as store]))

(def segments-version 7)
(def segments-format (contract/format-keyword :segments segments-version))

(def default-configuration
  "Tuning for the demopart partition.

  * `:display-bucket-events` — timeline bucket used to derive the active
    display-handler set;
  * `:bridge-max-events` — a run shorter than this whose handler set nests
    inside a neighbour's is a transition bridge, not a part of its own;
  * `:handoff-snap-tolerance` — how far before a bucket-derived boundary a
    write-to-execute handoff may be for the boundary to snap onto it."
  {:display-bucket-events 250000
   :bridge-max-events 3000000
   :handoff-snap-tolerance 2000000})

(defn- display-contexts
  "The demo's display-handler context ids, taken from its IRQ spans.

  `:features` already drops system contexts (stock KERNAL routines) before they
  become units, so every IRQ span belongs to a program-installed handler."
  [spans]
  (into #{}
        (comp (filter #(= :irq (:effective-level %))) (keep :context-id))
        spans))

(defn- active-handler-runs
  "Bucket the timeline and collapse it into runs of a stable handler set."
  [irq-spans display-contexts total-events bucket]
  (let [bucket (long bucket)
        total-events (long total-events)
        bucket-count (max 1 (long (quot (+ total-events (dec bucket)) bucket)))
        active (reduce (fn [active {:keys [context-id event-range]}]
                         (if (contains? display-contexts context-id)
                           (let [[start end] event-range
                                 first-bucket (quot (long start) bucket)
                                 last-bucket (quot (long (max start (dec (long end))))
                                                   bucket)]
                             (reduce (fn [active index]
                                       (update active index (fnil conj #{}) context-id))
                                     active
                                     (range first-bucket (inc last-bucket))))
                           active))
                       {}
                       irq-spans)]
    (reduce (fn [runs [index ctxs]]
              (let [end (min total-events (* (inc (long index)) bucket))]
                (if (and (seq runs) (= (:ctxs (peek runs)) ctxs))
                  (conj (pop runs) (assoc (peek runs) :end end))
                  (conj runs {:start (* (long index) bucket)
                              :end end
                              :ctxs ctxs}))))
            []
            (map-indexed (fn [index _] [index (get active index #{})])
                         (range bucket-count)))))

(defn- nested-handler-sets?
  "True when two non-empty handler sets contain one another."
  [a b]
  (and (seq a) (seq b)
       (or (set/subset? a b) (set/subset? b a))))

(defn- merge-bridge-runs
  "Fold transition-bridge runs into an adjacent related run until stable.

  A handler set that momentarily gains or loses one handler while a part
  continues (`{7} {7 8} {8}`) is one part with a transition, not three."
  [runs max-events]
  (loop [runs (vec runs)]
    (let [index (first (keep-indexed
                        (fn [index run]
                          (let [length (- (long (:end run)) (long (:start run)))
                                left (nth runs (dec index) nil)
                                right (nth runs (inc index) nil)]
                            (when (and (< length (long max-events))
                                       (or (nested-handler-sets? (:ctxs run) (:ctxs left))
                                           (nested-handler-sets? (:ctxs run) (:ctxs right))))
                              index)))
                        runs))]
      (if (nil? index)
        runs
        (let [run (nth runs index)
              left (nth runs (dec index) nil)
              merge-left? (nested-handler-sets? (:ctxs run) (:ctxs left))
              other (if merge-left? left (nth runs (inc index)))
              merged {:start (long (if merge-left? (:start left) (:start run)))
                      :end (long (if merge-left? (:end run) (:end other)))
                      :ctxs (set/union (:ctxs run) (:ctxs other))}
              from (if merge-left? (dec index) index)
              rebuilt (vec (concat (subvec runs 0 from)
                                   [merged]
                                   (subvec runs (+ from 2))))]
          (recur rebuilt))))))

(defn- span-overlaps-run?
  [span run]
  (let [[start end] (:event-range span)]
    (and (< (long start) (long (:end run)))
         (< (long (:start run)) (long end)))))

(def phase-roles
  "Roles represented as merged intervals within a part, so loader and
  decruncher activity is locatable even when it runs under a live handler set."
  [:raster :music :loader :decruncher])

(def phase-merge-gap
  "Adjacent same-role intervals closer than this are merged into one phase."
  200000)

(defn- merge-intervals
  [ranges gap]
  (reduce (fn [merged [start end]]
            (if (and (seq merged)
                     (<= (- (long start) (long (second (peek merged)))) (long gap)))
              (conj (pop merged) [(first (peek merged))
                                  (max (long (second (peek merged))) (long end))])
              (conj merged [start end])))
          []
          ranges))

(defn- handoffs-in-range
  [handoffs [start end]]
  (filterv #(and (<= (long start) (long (:event-index %)))
                 (< (long (:event-index %)) (long end)))
           handoffs))

(defn role-phases
  "Global merged intervals per phase role, from the classified spans.

  Each interval records the first and last write-to-execute handoff inside it.
  For `:loader` and `:decruncher` the interval is trimmed to end at that last
  handoff: the code the phase wrote has then started running, so the phase is
  over. This is what keeps a decruncher from bleeding into the effect loop that
  follows it when the effect runs entirely in IRQ handlers."
  [spans handoffs]
  (into (sorted-map)
        (for [role phase-roles]
          (let [intervals (merge-intervals (->> spans
                                                (filter #(contains? (:roles %) role))
                                                (map :event-range)
                                                (sort-by first))
                                           phase-merge-gap)]
            [role (mapv (fn [[start end]]
                          (let [hs (handoffs-in-range handoffs [start end])
                                entry (:event-index (first hs))
                                exit (:event-index (last hs))
                                ;; The handoff event itself is the loader/
                                ;; decruncher's final instruction, so the phase
                                ;; includes it and ends just after.
                                end (if (and exit
                                             (contains? #{:loader :decruncher} role)
                                             (< (long exit) (long end)))
                                      (inc (long exit))
                                      (long end))]
                            {:event-range [(long start) end]
                             :entry-event entry
                             :exit-event exit}))
                        intervals)]))))

(defn- run-kind
  "A run is a demopart when it has display handlers or visible raster effects;
  otherwise it is the idle/loading transition between parts."
  [handlers roles]
  (if (or (seq handlers) (contains? roles :raster))
    :demopart
    :transition))

(defn- run-phases
  "Phases overlapping a run, plus the immediately preceding phase.

  A loader/decruncher ends exactly where the next part begins, so that phase
  touches the run boundary without overlapping it; it is still the part's
  preceding transition and is kept."
  [run phases]
  (->> phases
       (mapcat (fn [[role intervals]]
                 (for [{:keys [event-range] :as phase} intervals
                       :let [[start end] event-range]
                       :when (and (< (long start) (long (:end run)))
                                  (<= (long (:start run)) (long end)))]
                   (assoc phase :role role))))
       (sort-by (comp first :event-range))
       vec))

(defn- precise-run-start
  "The earliest invocation of a run's handler set inside the run's buckets.

  Bucket edges are coarse; the actual part begins when the new handler set
  first fires, which is also where the handoff preceding it lands."
  [run irq-spans]
  (if (seq (:ctxs run))
    (let [candidates (keep (fn [{:keys [context-id event-range]}]
                             (when (and (contains? (:ctxs run) context-id)
                                        (<= (long (:start run)) (long (first event-range)))
                                        (< (long (first event-range)) (long (:end run))))
                               (long (first event-range))))
                           irq-spans)]
      (if (seq candidates)
        (reduce min candidates)
        (long (:start run))))
    (long (:start run))))

(defn- assign-starts
  [runs starts total-events]
  (let [count (count runs)]
    (mapv (fn [index run]
            (assoc run
                   :start (long (nth starts index))
                   :end (if (< (inc index) count)
                          (long (nth starts (inc index)))
                          (long total-events))))
          (range) runs)))

(defn- with-precise-starts
  [runs irq-spans total-events]
  (assign-starts runs
                 (vec (reductions max (map #(precise-run-start % irq-spans) runs)))
                 total-events))

(defn- snapped-starts
  "Move each run boundary onto just after the last write-to-execute handoff
  before it.

  The decruncher/loader jumps into the code it wrote just before the new part's
  handlers start firing; the handoff event is that final instruction, so the
  part begins on the next event."
  [runs handoffs total-events tolerance]
  (let [indices (vec (sort (map :event-index handoffs)))]
    (loop [index 1
           starts [(long (:start (first runs)))]]
      (if (< index (count runs))
        (let [start (long (:start (nth runs index)))
              previous (peek starts)
              handoff (last (filter #(and (>= (long %) previous)
                                          (< (long %) start)
                                          (<= (- start (long %)) (long tolerance)))
                                    indices))]
          (recur (inc index) (conj starts (long (or (some-> handoff inc) start)))))
        (assign-starts runs starts total-events)))))

(defn- run->segment
  [segment-id run spans phases]
  (let [overlapping (filter #(span-overlaps-run? % run) spans)
        roles (into #{} (mapcat :roles) overlapping)
        irq-roles (into #{} (comp (filter #(= :irq (:effective-level %)))
                                  (mapcat :roles))
                        overlapping)
        context-ids (into #{} (keep :context-id) overlapping)
        signals (into [] (comp (mapcat :signals) (distinct)) overlapping)
        confidences (keep :confidence overlapping)
        handlers (:ctxs run)]
    {:segment-id segment-id
     :kind (run-kind handlers roles)
     :roles roles
     :phases (run-phases run phases)
     :context-ids context-ids
     :handler-contexts handlers
     :irq-roles irq-roles
     :signals signals
     :confidence (if (seq confidences)
                   (/ (reduce + confidences) (count confidences))
                   0.0)
     :event-range [(:start run) (:end run)]
     :event-count (- (long (:end run)) (long (:start run)))}))

(defn- with-leading-run
  "Cover the timeline before the first handler run.

  A handler can first fire inside the first bucket, which moves that run's
  precise start past 0. The region before it is still real — a program's init,
  or an effect already running — and must be represented."
  [runs]
  (if (and (seq runs) (pos? (long (:start (first runs)))))
    (into [{:start 0 :end (long (:start (first runs))) :ctxs #{}}] runs)
    runs))

(defn assemble
  "Assemble classification spans into semantic segments.

  `total-events` bounds the timeline. Demoparts are runs of a stable
  display-handler set, taken from the IRQ spans; run boundaries are refined to
  the first invocation of the new handler set and snapped to the
  write-to-execute handoff that precedes it (the loader/decruncher jumping into
  the code it wrote). Each segment also carries `:phases`, the merged
  raster/music/loader/decruncher intervals overlapping it, each with
  `:entry-event`/`:exit-event` handoffs. `configuration` overrides
  `default-configuration`."
  ([spans handoffs total-events]
   (assemble spans handoffs total-events {}))
  ([spans handoffs total-events configuration]
   (let [configuration (merge default-configuration configuration)
         irq-spans (filter #(= :irq (:effective-level %)) spans)
         phases (role-phases spans handoffs)
         runs (-> (active-handler-runs irq-spans (display-contexts spans)
                                       total-events
                                       (:display-bucket-events configuration))
                  (with-precise-starts irq-spans total-events)
                  (merge-bridge-runs (:bridge-max-events configuration))
                  (with-leading-run)
                  (snapped-starts handoffs total-events
                                  (:handoff-snap-tolerance configuration)))]
     (mapv (fn [index run] (run->segment index run spans phases))
           (range)
           runs))))

(defn- merge-range
  [[start end]]
  [start end])

(defn- covered-ranges
  [segments]
  (->> segments (map :event-range) (sort-by first) vec))

(defn- uncovered-ranges
  [covered total]
  (loop [ranges (sort-by first covered)
         cursor 0
         gaps []]
    (if-let [[start end] (first ranges)]
      (let [gaps (cond-> gaps (> start cursor) (conj [cursor start]))
            cursor (long (max cursor end))]
        (recur (rest ranges) cursor gaps))
      (cond-> gaps (< cursor total) (conj [cursor total])))))

(defn- overlapping-pairs
  [ranges]
  (->> ranges
       (sort-by first)
       (partition 2 1)
       (keep (fn [[[start-a end-a] [start-b end-b]]]
               (when (< start-b end-a) [[start-a end-a] [start-b end-b]])))
       vec))

(defn boundary-audit
  "Build the boundary audit for assembled segments.

  `total-events` bounds the coverage check. The audit reports uncovered gaps,
  overlapping segment ranges, and per-role event evidence. Handoff alignment is
  deliberately not reported: demoparts come from the display-handler set, so a
  write-to-execute handoff sitting inside a part is expected."
  [segments file-groups total-events]
  (let [covered (covered-ranges segments)
        role-evidence
        (reduce (fn [acc segment]
                  (reduce (fn [acc role]
                            (-> acc
                                (update-in [role :event-count] (fnil + 0)
                                           (:event-count segment))
                                (update-in [role :segment-count] (fnil inc 0))))
                          acc
                          (:roles segment)))
                {}
                segments)]
    {:segment-count (count segments)
     :file-group-count (count file-groups)
     :event-count total-events
     :covered (mapv merge-range covered)
     :uncovered (uncovered-ranges covered total-events)
     :overlaps (overlapping-pairs covered)
     :role-evidence role-evidence
     :role-counts (reduce-kv (fn [acc role {:keys [segment-count]}]
                               (assoc acc role segment-count))
                             {}
                             role-evidence)}))

(defn group-files
  "Group ordered segments into files, ending a file after a loader transition.

  Loader and decruncher are evidence roles rather than partition kinds, so a
  file ends after a `:transition` segment whose roles include `:loader` (the
  region between parts where the next file is read)."
  [segments]
  (loop [remaining segments
         current nil
         loader-run? false
         groups []]
    (if-let [segment (first remaining)]
      (let [loader? (and (= :transition (:kind segment))
                         (contains? (:roles segment) :loader))
            [current groups]
            (if (and loader-run? (not loader?))
              [nil (conj groups current)]
              [current groups])
            current (or current
                        {:file-id (count groups)
                         :event-range (:event-range segment)
                         :segments []
                         :roles #{}})
            current (-> current
                        (update :event-range
                                (fn [[start _]] [start (second (:event-range segment))]))
                        (update :segments conj (:segment-id segment))
                        (update :roles set/union (:roles segment)))]
        (recur (rest remaining) current loader? groups))
      (cond-> groups current (conj current)))))

(defn build!
  "Build and commit a `:segments` generation from classification and features.

  `classification-image` and `features-image` are opened images. Options:
  `:configuration` (recorded only)."
  ([classification-image features-image]
   (build! classification-image features-image {}))
  ([classification-image features-image {:keys [configuration]
                                         :or {configuration {}}}]
   (let [configuration (merge default-configuration configuration)
         dataset-root (:dataset-root classification-image)
         spans (:spans classification-image)
         handoffs (features/handoffs features-image)
         total-events (get-in features-image [:index :event-count])
         segments (assemble spans handoffs total-events configuration)
         file-groups (group-files segments)
         audit (boundary-audit segments file-groups total-events)
         index {:format segments-format
                :version segments-version
                :capture-id (get-in classification-image [:index :capture-id])
                :capture-manifest-sha256
                (get-in classification-image
                        [:index :capture-manifest-sha256])
                :configuration configuration
                :event-count total-events
                :summary {:segment-count (count segments)
                          :segment-counts (frequencies (map :kind segments))
                          :file-group-count (count file-groups)
                          :gap-count (count (:uncovered audit))
                          :overlap-count (count (:overlaps audit))}
                :file-groups file-groups
                :segments segments}
         {:keys [generation directory relative]}
         (analysis/new-generation! dataset-root :segments)]
     (try
       (store/atomic-write-edn! (io/file directory "index.edn") index)
       (store/atomic-write-edn! (io/file directory "boundary-audit.edn") audit)
       (analysis/commit-stage!
        dataset-root :segments
        {:generation generation
         :directory relative
         :version segments-version
         :capture-id (:capture-id index)
         :capture-manifest-sha256 (:capture-manifest-sha256 index)
         :configuration configuration
         :summary (:summary index)})
       {:dataset-root dataset-root
        :directory (str directory)
        :index index
        :audit audit}
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open!
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :segments)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :segments generation"
                      {:reason :missing-segments-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))
          audit (store/read-edn (io/file directory "boundary-audit.edn"))]
      (when-not (= segments-format (:format index))
        (throw (ex-info "Not a v2 segments image"
                        {:reason :not-a-segments-image :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index
       :segments (:segments index)
       :file-groups (:file-groups index)
       :audit audit})))

(defn status [dataset-root]
  (analysis/stage-entry dataset-root :segments))

(defn segments [segments-image]
  (:segments segments-image))

(defn file-groups [segments-image]
  (:file-groups segments-image))

(defn audit [segments-image]
  (:audit segments-image))
