(ns omkamra.vice.classification
  "Role-tagged execution spans.

  `:classification` is a pure ordered consumer of `:features` evidence (and,
  where available, `:audio` music evidence). It assigns composable roles to
  neutral logical units and assembles them into spans:

  * `:mainline-window` units are stabilized with hysteresis and merged into
    non-overlapping mainline spans; a hard write-to-execute handoff always
    starts a new span;
  * `:irq-invocation` units are split at block-occurrence boundaries wherever
    the role set changes, so one handler can contribute raster, music, loader,
    and decruncher sub-spans.

  A span is `{:context-id :frame-id :context-kind :effective-level
  :event-range :roles :confidence :signals}`. `:context-kind` is the observed
  execution context kind and `:effective-level` is the timeline level (a
  leveled frame's continuation is `:mainline` despite an `:irq` context).
  Spans are non-overlapping within one context; spans from different contexts
  may overlap envelopes.

  Generation layout:

  ```text
  analysis/classification/<generation>/
    index.edn
  ```"
  (:require [clojure.java.io :as io]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.audio :as audio]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.features :as features]
            [omkamra.vice.store :as store]))

(def classification-version 7)
(def classification-format
  (contract/format-keyword :classification classification-version))

(def default-configuration
  {:raster-write-threshold 32
   :music-write-threshold 32
   :loader-serial-threshold 32
   :loader-iec-threshold 16
   :loader-kernal-threshold 64
   :decruncher-min-ram-writes 512
   :decruncher-min-unique-addresses 2048
   :decruncher-min-page-count 16
   :idle-max-writes 8
   :enter-units 2
   :exit-units 2})

(defn unit-roles
  "Return the role set implied by a unit's evidence, or nil for a neutral unit.

  `audio-blocks` and `audio-contexts` are the sets identified by the `:audio`
  stage; they contribute `:music` even when the unit's SID write count is below
  the generic threshold."
  ([unit configuration] (unit-roles unit configuration #{} #{}))
  ([unit configuration audio-blocks audio-contexts]
   (let [evidence (:evidence unit)
         vic (:vic-write-count evidence 0)
         sid (:sid-write-count evidence 0)
         ram (:ram-write-count evidence 0)
         serial (+ (:serial-read-count evidence 0)
                   (:serial-write-count evidence 0))
         iec (:iec-write-count evidence 0)
         kernal (:kernal-iec-execution-count evidence 0)
         total (:write-total evidence 0)
         unique (:destination-unique-address-count evidence 0)
         pages (:destination-page-count evidence 0)
         roles
         (cond-> #{}
           (>= vic (:raster-write-threshold configuration)) (conj :raster)
           (or (>= sid (:music-write-threshold configuration))
               ;; A block candidate has no `:context` and vice versa, so require
               ;; the unit to actually carry the key: a missing `:block-id`
               ;; must not match a candidate whose block is nil.
               (and (some? (:block-id unit))
                    (contains? audio-blocks (:block-id unit)))
               (and (some? (:context-id unit))
                    (contains? audio-contexts (:context-id unit)))) (conj :music)
           (or (>= serial (:loader-serial-threshold configuration))
               (>= iec (:loader-iec-threshold configuration))
               (>= kernal (:loader-kernal-threshold configuration))) (conj :loader)
           (and (>= ram (:decruncher-min-ram-writes configuration))
                (>= unique (:decruncher-min-unique-addresses configuration))
                (>= pages (:decruncher-min-page-count configuration)))
           (conj :decruncher)
           (<= total (:idle-max-writes configuration)) (conj :idle))]
     (when (seq roles) roles))))

(defn- advance-role-state
  "Advance the per-role hysteresis state with one unit's role set.

  Returns `[state stable-roles]`. Roles are tracked from the first time they
  appear, which is equivalent to the batch form because an unseen role is
  always off."
  [role-state roles enter exit]
  (let [all (into (set (keys role-state)) roles)
        state (reduce
               (fn [state role]
                 (let [{:keys [on? present absent]}
                       (get state role {:on? false :present 0 :absent 0})
                       present? (contains? roles role)
                       present (if present? (inc (or present 0)) 0)
                       absent (if present? 0 (inc (or absent 0)))
                       on? (cond
                             (and (not on?) (>= present enter)) true
                             (and on? (>= absent exit)) false
                             :else on?)]
                   (assoc state role {:on? on? :present present :absent absent})))
               role-state
               all)]
    [state (into #{} (keep (fn [[role {:keys [on?]}]] (when on? role)) state))]))

(defn- confidence [unit roles]
  (let [evidence (:evidence unit)
        total (max 1 (:write-total evidence 1))]
    (min 1.0
         (if (empty? roles)
           0.0
           (max 0.25
                (double (/ (apply max
                                  (map #(case %
                                          :raster (:vic-write-count evidence 0)
                                          :music (:sid-write-count evidence 0)
                                          :loader (max (+ (:serial-read-count evidence 0)
                                                          (:serial-write-count evidence 0))
                                                       (:iec-write-count evidence 0)
                                                       (:kernal-iec-execution-count evidence 0))
                                          :decruncher (:ram-write-count evidence 0)
                                          :idle (- total (:write-total evidence 0))
                                          0)
                                       roles))
                           total)))))))

(defn- signals [unit roles]
  (let [evidence (:evidence unit)]
    (vec (keep (fn [role]
                 (case role
                   :raster (when (pos? (:vic-write-count evidence 0)) [:vic-writes])
                   :music (when (pos? (:sid-write-count evidence 0)) [:sid-writes])
                   :loader (when (pos? (+ (:serial-read-count evidence 0)
                                          (:serial-write-count evidence 0)
                                          (:iec-write-count evidence 0)))
                             [:serial])
                   :decruncher (when (pos? (:ram-write-count evidence 0)) [:ram-footprint])
                   :idle [[:low-traffic]]
                   nil))
               roles))))

(defn- unit-span [unit roles effective-level event-range]
  {:context-id (:context-id unit)
   :frame-id (:frame-id unit)
   :context-kind (:context-kind unit :irq)
   :effective-level effective-level
   :event-range event-range
   :roles (or roles #{})
   :confidence (confidence unit roles)
   :signals (signals unit roles)})

(defn- emit-span
  [spans unit roles effective-level event-range]
  (if (seq roles)
    (conj spans (unit-span unit roles effective-level event-range))
    spans))

(defn- mainline-step
  "Fold one mainline unit into the streaming accumulator.

  A span continues while the stable role set, context and frame stay the same.
  Handoffs are deliberately *not* span boundaries: part boundaries come from the
  display-handler set in `:segments`, and handoff evidence reaches it as phase
  `:entry-event`/`:exit-event` metadata."
  [acc unit roles]
  (let [current (:current acc)]
    (if (and current
             (= (:roles current) roles)
             (= (:context-id (:unit current)) (:context-id unit))
             (= (:frame-id (:unit current)) (:frame-id unit)))
      (update-in acc [:current :event-range]
                 (fn [[start _]] [start (second (:event-range unit))]))
      (let [acc (cond-> acc
                  current (-> (update :spans emit-span (:unit current)
                                      (:roles current) :mainline
                                      (:event-range current))
                              (assoc :current nil)
                              (update :stable-role-seen?
                                      #(or % (boolean (seq (:roles current)))))))]
        (assoc acc :current {:unit unit :roles roles
                             :event-range (:event-range unit)})))))

(defn- flush-mainline
  "Flush the open mainline span.

  Hysteresis needs `:enter-units` consecutive units before a role switches on,
  so a mainline timeline that never stabilized a role (e.g. a short intro whose
  only mainline unit is its init window) would otherwise emit nothing and leave
  an uncovered gap. In that case the pending unit's own evidence is used. Once
  any role has stabilized, the trailing unit is left to hysteresis as before."
  [acc configuration audio-blocks audio-contexts]
  (if-let [current (:current acc)]
    (let [roles (if (or (seq (:roles current)) (:stable-role-seen? acc))
                  (:roles current)
                  (or (unit-roles (:unit current) configuration
                                  audio-blocks audio-contexts)
                      #{}))]
      (-> acc
          (update :spans emit-span (:unit current) roles
                  :mainline (:event-range current))
          (assoc :current nil)))
    acc))

(defn- irq-spans
  "Split one IRQ invocation into role-tagged spans.

  The invocation is the unit of interrupt execution, so its aggregated evidence
  decides the base roles (`:raster`/`:music`/`:loader`/`:decruncher`). A single
  block occurrence rarely reaches a role threshold on its own — a raster handler
  writes a couple of VIC registers per occurrence but fifty per invocation — so
  an occurrence with no role of its own is attributed the invocation's base
  roles, while an occurrence that does carry a role keeps its own (which is what
  splits a handler that, say, does music then raster into sub-spans)."
  [invocation configuration audio-blocks audio-contexts]
  (let [occurrences (:occurrences invocation)
        base-roles (or (unit-roles invocation configuration
                                   audio-blocks audio-contexts)
                       #{})
        contextualize (fn [occurrence]
                        (assoc occurrence
                               :context-id (:context-id invocation)
                               :frame-id (:frame-id invocation)
                               :context-kind (:context-kind invocation)
                               :effective-level :irq))
        roles (mapv (fn [occurrence]
                      (let [own (or (unit-roles occurrence configuration
                                                audio-blocks audio-contexts)
                                    #{})
                            ;; A block-level audio candidate gives an occurrence
                            ;; `:music` even when its own evidence is quiet; that
                            ;; must not mask the invocation's base roles, so the
                            ;; split decision uses evidence-only roles.
                            strong (disj (or (unit-roles occurrence configuration) #{})
                                         :idle)]
                        (if (seq strong) (disj own :idle) base-roles)))
                    occurrences)
        segments (loop [remaining (map vector occurrences roles)
                        current nil
                        out []]
                   (if-let [[occurrence role-set] (first remaining)]
                     (if (and current (= (:roles current) role-set))
                       (recur (rest remaining)
                              (update current :event-range
                                      (fn [[start _]] [start (second (:event-range occurrence))]))
                              out)
                       (recur (rest remaining)
                              {:roles role-set :event-range (:event-range occurrence)
                               :unit (contextualize occurrence)}
                              (cond-> out
                                current (conj current))))
                     (cond-> out current (conj current))))]
    (reduce (fn [spans segment]
              (emit-span spans (:unit segment) (:roles segment) :irq
                         (:event-range segment)))
            []
            segments)))

(defn- classify-step
  [configuration audio-blocks audio-contexts]
  (fn [acc unit]
    (if (= :mainline-window (:kind unit))
      (let [[role-state roles]
            (advance-role-state (:role-state acc)
                                (or (unit-roles unit configuration
                                                audio-blocks audio-contexts)
                                    #{})
                                (:enter-units configuration)
                                (:exit-units configuration))]
        (mainline-step (assoc acc :role-state role-state) unit roles))
      (update acc :spans into
              (irq-spans unit configuration audio-blocks audio-contexts)))))

(defn- finalize-spans
  [acc configuration audio-blocks audio-contexts]
  (vec (sort-by (juxt :context-id (comp first :event-range))
                (:spans (flush-mainline acc configuration audio-blocks
                                        audio-contexts)))))

(defn classify
  "Classify feature units into role-tagged spans.

  `audio-blocks`/`audio-contexts` are optional sets of blocks/contexts the
  `:audio` stage flagged as music candidates. This is a single pass and works
  on a lazy unit sequence."
  ([units configuration]
   (classify units configuration #{} #{}))
  ([units configuration audio-blocks audio-contexts]
   (let [configuration (merge default-configuration configuration)]
     (finalize-spans
      (reduce (classify-step configuration audio-blocks audio-contexts)
              {:role-state {} :current nil :spans []}
              units)
      configuration audio-blocks audio-contexts))))

;; Build ---------------------------------------------------------------------

(defn build!
  "Build and commit a `:classification` generation.

  `features-image` is an opened `:features` image; `audio-image` is an opened
  `:audio` image or nil. Options: `:configuration`."
  ([features-image] (build! features-image nil {}))
  ([features-image audio-image] (build! features-image audio-image {}))
  ([features-image audio-image {:keys [configuration]
                                :or {configuration default-configuration}}]
   (let [configuration (merge default-configuration configuration)
         dataset-root (:dataset-root features-image)
         audio-blocks (audio/music-blocks audio-image)
         audio-contexts (audio/music-contexts audio-image)
         spans (finalize-spans
                (features/reduce-units
                 features-image
                 (classify-step configuration audio-blocks audio-contexts)
                 {:role-state {} :current nil :spans []})
                configuration audio-blocks audio-contexts)
         index {:format classification-format
                :version classification-version
                :capture-id (get-in features-image [:index :capture-id])
                :capture-manifest-sha256
                (get-in features-image [:index :capture-manifest-sha256])
                :configuration configuration
                :summary {:span-count (count spans)
                          :role-counts (frequencies (mapcat :roles spans))
                          :mainline-span-count (count (filter #(= :mainline (:effective-level %)) spans))
                          :irq-span-count (count (filter #(= :irq (:effective-level %)) spans))}
                :spans spans}
         {:keys [generation directory relative]}
         (analysis/new-generation! dataset-root :classification)]
     (try
       (store/atomic-write-edn! (io/file directory "index.edn") index)
       (analysis/commit-stage!
        dataset-root :classification
        {:generation generation
         :directory relative
         :version classification-version
         :capture-id (:capture-id index)
         :capture-manifest-sha256 (:capture-manifest-sha256 index)
         :configuration configuration
         :summary (:summary index)})
       {:dataset-root dataset-root
        :directory (str directory)
        :index index}
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open!
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :classification)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :classification generation"
                      {:reason :missing-classification-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= classification-format (:format index))
        (throw (ex-info "Not a v2 classification image"
                        {:reason :not-a-classification-image
                         :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index
       :spans (:spans index)})))

(defn status [dataset-root]
  (analysis/stage-entry dataset-root :classification))

(defn spans [classification]
  (:spans classification))
