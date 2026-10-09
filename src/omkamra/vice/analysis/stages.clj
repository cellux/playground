(ns omkamra.vice.analysis.stages
  "Reducer implementations for the registered offline analysis stages."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [omkamra.vice.analysis.common :as common]
            [omkamra.vice.analysis.segments :as segments]
            [omkamra.vice.decoder.classification :as classification]
            [omkamra.vice.decoder.features :as features]
            [omkamra.vice.decoder.structure :as structure]
            [omkamra.vice.decoder.video :as video]))

(defn init-chunk-stage!
  "Create the mutable state for one staged, chunk-at-a-time projection.

  Stage state is private to one analysis request.  The broadcast coordinator
  owns raw-chunk reads; stages own only their current derived chunk and their
  staged output index."
  [{:keys [output-directory capture-manifest] :as context}]
  (.mkdirs (io/file output-directory "chunks"))
  {:context context
   :chunk-count (count (:chunks capture-manifest))
   ;; Several chunk workers may persist independent files concurrently. The
   ;; counters/index are the only shared stage state and are updated after a
   ;; chunk file has been atomically installed.
   :completed (atom 0)
   :entries (atom (sorted-map))})

(defn persist-stage-chunk!
  ([state raw-chunk derived]
   (persist-stage-chunk! state raw-chunk derived derived))
  ([state raw-chunk derived output]
   (let [{:keys [output-directory stage progress!]} (:context state)
         chunk-number (:chunk-number raw-chunk)
         output-file (common/stage-chunk-file output-directory chunk-number)]
     (common/atomic-write-edn! output-file derived)
     (let [completed
           (locking state
             (swap! (:entries state)
                    assoc chunk-number
                    {:number chunk-number
                     :file (str "chunks/"
                                (format "chunk-%06d.edn" chunk-number))
                     :event-range (:event-range raw-chunk)})
             (swap! (:completed state) inc))]
       (progress! {:stage (:id stage)
                   :chunks-completed completed
                   :chunk-count (:chunk-count state)
                   :current-chunk chunk-number})
       {:state state
        :output output}))))

(defn writes-stage-step
  [state raw-chunk _dependencies]
  (let [writes-chunk (video/derive-writes-chunk raw-chunk)]
    (persist-stage-chunk! state raw-chunk writes-chunk
                          (assoc writes-chunk
                                 ::video/write-source
                                 (video/writes-source raw-chunk writes-chunk)))))

(defn close-chunk-stage!
  [{:keys [context chunk-count entries]}]
  (let [{:keys [output-directory capture-manifest stage]} context
        {:keys [number id version]} stage
        index {:format common/stage-format
               :stage number
               :stage-id id
               :version version
               :capture-id (:capture-id capture-manifest)
               :source-format (:format capture-manifest)
               :chunk-count chunk-count
               :chunks (vec (vals @entries))}]
    (common/atomic-write-edn! (common/file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count}))

(defn structure-stage-step
  [state raw-chunk _dependencies]
  (persist-stage-chunk! state raw-chunk
                        (structure/derive-structure-chunk raw-chunk)))

(defn video-stage-step
  [state raw-chunk dependencies]
  (let [writes-chunk (or (:writes dependencies)
                         (throw (ex-info "Video analysis requires writes output"
                                         {:chunk-number (:chunk-number raw-chunk)})))
        writes (video/writes-source raw-chunk writes-chunk)]
    (persist-stage-chunk! state raw-chunk
                          (video/derive-video-chunk raw-chunk writes))))

(defn assets-stage-step
  [state raw-chunk dependencies]
  (let [writes-chunk (or (:writes dependencies)
                         (throw (ex-info "Assets analysis requires writes output"
                                         {:chunk-number (:chunk-number raw-chunk)})))
        video-chunk (or (:video dependencies)
                        (throw (ex-info "Assets analysis requires video output"
                                        {:chunk-number (:chunk-number raw-chunk)})))
        writes (video/writes-source raw-chunk writes-chunk)]
    (persist-stage-chunk! state raw-chunk
                          (video/derive-assets-chunk raw-chunk video-chunk writes))))

(defn merge-counter-maps
  [left right]
  (merge-with + (or left {}) (or right {})))

(defn add-feature-summary
  [total summary]
  (-> total
      (update :event-count + (:event-count summary))
      (update :frame-count + (:frame-count summary))
      (update :frame-fingerprint-count + (:frame-fingerprint-count summary))
      (update :write-window-count + (or (:write-window-count summary) 0))
      (update :classifier-unit-count + (or (:classifier-unit-count summary) 0))
      (update :rolling-fingerprint-count + (:rolling-fingerprint-count summary))
      (update :vic-configuration-count + (:vic-configuration-count summary))
      (update :write-count + (:write-count summary))
      (update :ram-write-count + (:ram-write-count summary))
      (update :changed-write-count + (:changed-write-count summary))
      (update :overwritten-code-write-count +
              (:overwritten-code-write-count summary))
      (update :iec-register-write-count + (:iec-register-write-count summary))
      (update :kernal-iec-execution-count +
              (:kernal-iec-execution-count summary))
      (update :kernal-iec-transfer-count +
              (:kernal-iec-transfer-count summary))
      (update :domains merge-counter-maps (:domains summary))))

(defn init-features-stage!
  [context]
  (assoc (init-chunk-stage! context)
         :summary (atom {:event-count 0
                         :ram-write-count 0
                         :frame-count 0
                         :frame-fingerprint-count 0
                         :write-window-count 0
                         :classifier-unit-count 0
                         :rolling-fingerprint-count 0
                         :vic-configuration-count 0
                         :write-count 0
                         :changed-write-count 0
                         :overwritten-code-write-count 0
                         :iec-register-write-count 0
                         :kernal-iec-execution-count 0
                         :kernal-iec-transfer-count 0
                         :domains {}})))

(defn features-stage-step
  [state raw-chunk dependencies]
  (let [writes-chunk (or (:writes dependencies)
                         (throw (ex-info "Feature analysis requires writes output"
                                         {:chunk-number (:chunk-number raw-chunk)})))
        structure-chunk (or (:structure dependencies)
                            (throw (ex-info "Feature analysis requires structure output"
                                            {:chunk-number (:chunk-number raw-chunk)})))
        video-chunk (or (:video dependencies)
                        (throw (ex-info "Feature analysis requires video output"
                                        {:chunk-number (:chunk-number raw-chunk)})))
        features (features/derive-features-chunk
                  raw-chunk structure-chunk video-chunk
                  (get-in state [:context :configuration])
                  (video/writes-source raw-chunk writes-chunk))
        result (persist-stage-chunk! state raw-chunk features)]
    (swap! (:summary state) add-feature-summary
           (get-in features [:stages :features :summary]))
    result))

(defn close-features-stage!
  [{:keys [context chunk-count entries summary]}]
  (let [{:keys [output-directory capture-manifest stage]} context
        {:keys [number id version]} stage
        summary (let [summary @summary]
                  (assoc summary
                         :write-density
                         (if (pos? (:event-count summary))
                           (/ (double (:write-count summary))
                              (:event-count summary))
                           0.0)))
        index {:format common/stage-format
               :stage number
               :stage-id id
               :version version
               :capture-id (:capture-id capture-manifest)
               :source-format (:format capture-manifest)
               :chunk-count chunk-count
               :summary summary
               :chunks (vec (vals @entries))}]
    (common/atomic-write-edn! (common/file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count
     :summary summary}))

(defn stage-chunk-output
  [stage-state chunk-number]
  (edn/read-string
   (slurp (common/stage-chunk-file (:output-directory (:context stage-state))
                                   chunk-number))))

(defn classification-stage-step
  [state raw-chunk dependencies]
  (let [features (or (:features dependencies)
                     (throw (ex-info "Classification requires feature output"
                                     {:chunk-number (:chunk-number raw-chunk)})))
        classification (classification/derive-classification-chunk
                        raw-chunk features
                        (get-in state [:context :configuration]))]
    (persist-stage-chunk! state raw-chunk classification)))

(defn close-classification-stage!
  [{:keys [context chunk-count entries]}]
  (let [{:keys [output-directory capture-manifest stage]} context
        {:keys [number id version]} stage
        configuration (get-in context [:configuration])
        classifier (reduce
                    (fn [classifier {:keys [number]}]
                      (classification/consume-classification-chunk
                       classifier
                       (stage-chunk-output
                        {:context {:output-directory output-directory}}
                        number)))
                    (classification/classifier-state configuration)
                    (vals @entries))
        result (classification/finish-classification classifier)
        index (assoc result
                     :stage number
                     :stage-id id
                     :version version
                     :capture-id (:capture-id capture-manifest)
                     :source-format (:format capture-manifest)
                     :chunk-count chunk-count
                     :chunks (vec (vals @entries)))]
    (common/atomic-write-edn! (common/file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count
     :summary (:summary result)
     :activity-count (count (:activities result))
     :part-count (count (:parts result))}))

(defn segments-stage-step
  [state raw-chunk dependencies]
  (let [classification (or (:classification dependencies)
                           (throw (ex-info "Segment assembly requires classification"
                                           {:chunk-number (:chunk-number raw-chunk)})))]
    (persist-stage-chunk!
     state raw-chunk
     {:format :omkamra.vice/segments-input-chunk-v1
      :capture-id (:capture-id raw-chunk)
      :chunk-number (:chunk-number raw-chunk)
      :event-range (:event-range raw-chunk)
      :stages {:segments
               {:classification-summary
                (get-in classification [:stages :classification :summary])}}})))

(defn close-segments-stage!
  [{:keys [context chunk-count entries]}]
  (let [{:keys [capture-directory output-directory capture-manifest
                completed-stages active-stage-directories configuration stage]} context
        classification (segments/read-completed-stage-index capture-directory
                                                            completed-stages
                                                            active-stage-directories
                                                            :classification)
        feature-evidence (segments/completed-feature-evidence
                          capture-directory completed-stages
                          active-stage-directories)
        descriptors (segments/segment-descriptors capture-manifest classification
                                                  (:frames feature-evidence)
                                                  configuration)
        descriptors (segments/attach-routines descriptors (:units feature-evidence)
                                              configuration)
        file-grouping (segments/attach-file-groups capture-manifest
                                                   classification
                                                   descriptors
                                                   configuration)
        descriptors (:segments file-grouping)
        descriptors
        (mapv (fn [descriptor]
                (let [file (format "segments/segment-%06d.edn"
                                   (:segment-id descriptor))]
                  (.mkdirs (io/file output-directory "segments"))
                  (common/atomic-write-edn! (common/file-path output-directory file) descriptor)
                  (assoc descriptor :file file)))
              descriptors)
        summary {:segment-count (count descriptors)
                 :segment-counts (frequencies (map :kind descriptors))
                 :role-counts (frequencies (map :role descriptors))
                 :demopart-count (count (filter #(= :demopart (:kind %))
                                                descriptors))
                 :effect-candidate-count
                 (count (filter #(= :effect-candidate (:kind %)) descriptors))
                 :transition-count (count (filter #(= :transition (:kind %))
                                                  descriptors))}
        audit (segments/segment-boundary-audit capture-manifest descriptors)
        gap-diagnostics (segments/uncovered-range-diagnostics
                         audit (:units feature-evidence) configuration)
        short-gaps (segments/short-gap-diagnostics
                    audit descriptors (:units feature-evidence) configuration)
        audit (assoc audit
                     :unclassified-gaps gap-diagnostics
                     :short-gaps short-gaps
                     :file-grouping file-grouping)
        audit-files {:edn-file "boundary-audit.edn"
                     :text-file "boundary-audit.txt"}
        _ (common/atomic-write-edn! (common/file-path output-directory (:edn-file audit-files))
                                    audit)
        _ (segments/atomic-write-text! (common/file-path output-directory (:text-file audit-files))
                                       (segments/boundary-audit-text audit))
        {:keys [number id version]} stage
        index {:format common/stage-format
               :stage number
               :stage-id id
               :version version
               :capture-id (:capture-id capture-manifest)
               :source-format (:format capture-manifest)
               :chunk-count chunk-count
               :configuration configuration
               :summary summary
               :audit (assoc audit-files
                             :summary (:summary audit)
                             :gap-cause-counts (frequencies
                                                (map :cause gap-diagnostics))
                             :short-gap-count (count short-gaps))
               :file-grouping (select-keys file-grouping [:boundaries :ranges])
               :segments descriptors
               :chunks (vec (vals @entries))}]
    (common/atomic-write-edn! (common/file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count
     :summary summary
     :file-grouping (select-keys file-grouping [:boundaries :ranges])
     :audit (assoc audit-files
                   :summary (:summary audit)
                   :gap-cause-counts (frequencies
                                      (map :cause gap-diagnostics))
                   :short-gap-count (count short-gaps))}))

(defn disassembly-stage-step
  [state raw-chunk dependencies]
  (when-not (:segments dependencies)
    (throw (ex-info "Disassembly requires segment output"
                    {:chunk-number (:chunk-number raw-chunk)})))
  ;; Persist only the compact CPU-port timeline. The structural execution
  ;; (including block runs) is already owned by the structure stage, so the
  ;; close pass never needs to re-parse the much larger raw chunk.
  (persist-stage-chunk!
   state raw-chunk
   {:format :omkamra.vice/disassembly-input-chunk-v1
    :capture-id (:capture-id raw-chunk)
    :chunk-number (:chunk-number raw-chunk)
    :event-range (:event-range raw-chunk)
    :stages {:disassembly (segments/port-info raw-chunk)}}))

(defn close-disassembly-stage!
  [{:keys [context chunk-count entries]}]
  (let [{:keys [capture-directory output-directory capture-manifest
                completed-stages active-stage-directories stage]} context
        segment-index (segments/read-completed-stage-index capture-directory
                                                           completed-stages
                                                           active-stage-directories
                                                           :segments)
        segments (:segments segment-index)
        omit-rom? (not (true? (get-in capture-manifest
                                      [:options :full-capture?])))
        structure-directory (segments/stage-output-directory
                             completed-stages active-stage-directories
                             :structure)
        structure-root (segments/stage-root capture-directory
                                            structure-directory)
        render-chunk-reader
        (fn [chunk-number]
          (let [structure (edn/read-string
                           (slurp (common/stage-chunk-file structure-root
                                                           chunk-number)))
                input (edn/read-string
                       (slurp (common/stage-chunk-file output-directory
                                                       chunk-number)))
                execution (get-in structure [:stages :structure :execution])
                port (get-in input [:stages :disassembly])]
            {:chunk-number chunk-number
             :event-range (:event-range input)
             :event-count (:event-count execution)
             :execution execution
             :block-runs (:block-runs execution)
             :initial-port (:initial-port port)
             :port-writes (:port-writes port)}))
        _ ((:progress! context)
           {:stage (:id stage)
            :chunks-completed chunk-count
            :chunk-count chunk-count
            :current-chunk chunk-count
            :phase :materializing-assemblies
            :segments-completed 0
            :segment-count (count segments)})
        assemblies
        (mapv (fn [index {:keys [segment-id kind role event-range
                                 music-routine raster-routine] :as segment}]
                (let [file (format "assemblies/segment-%06d.asm" segment-id)
                      routine-file (fn [routine-kind]
                                     (format "assemblies/segment-%06d.%s.asm"
                                             segment-id (name routine-kind)))
                      phase-kind (case role
                                   :loader :loader
                                   :decruncher :decrunch
                                   nil)
                      phase-file (when phase-kind (routine-file phase-kind))
                      music-file (when music-routine (routine-file :music))
                      raster-file (when raster-routine (routine-file :raster))]
                  (.mkdirs (io/file output-directory "assemblies"))
                  (segments/write-segment-assembly! capture-directory
                                                    (common/file-path output-directory file)
                                                    segment
                                                    omit-rom?
                                                    render-chunk-reader)
                  (when phase-file
                    (segments/write-routine-assembly!
                     (common/file-path output-directory phase-file)
                     segment {:kind phase-kind} omit-rom? render-chunk-reader))
                  (when music-file
                    (segments/write-routine-assembly!
                     (common/file-path output-directory music-file)
                     segment music-routine omit-rom? render-chunk-reader))
                  (when raster-file
                    (segments/write-routine-assembly!
                     (common/file-path output-directory raster-file)
                     segment raster-routine omit-rom? render-chunk-reader))
                  ((:progress! context)
                   {:stage (:id stage)
                    :chunks-completed chunk-count
                    :chunk-count chunk-count
                    :current-chunk chunk-count
                    :phase :materializing-assemblies
                    :segments-completed (inc index)
                    :segment-count (count segments)})
                  (cond-> {:segment-id segment-id
                           :kind kind
                           :role (:role segment)
                           :event-range event-range
                           :file file}
                    phase-file (assoc :phase-file phase-file)
                    music-file (assoc :music-file music-file)
                    raster-file (assoc :raster-file raster-file))))
              (range)
              segments)
        summary {:assembly-count (count assemblies)
                 :assembly-counts (frequencies (map :kind assemblies))
                 :music-assembly-count (count (filter :music-file assemblies))
                 :raster-assembly-count (count (filter :raster-file assemblies))
                 :loader-assembly-count
                 (count (filter #(= :loader (:role %)) assemblies))
                 :decruncher-assembly-count
                 (count (filter #(= :decruncher (:role %)) assemblies))}
        {:keys [number id version]} stage
        index {:format common/stage-format
               :stage number
               :stage-id id
               :version version
               :capture-id (:capture-id capture-manifest)
               :source-format (:format capture-manifest)
               :chunk-count chunk-count
               :summary summary
               :assemblies assemblies
               :chunks (vec (vals @entries))}]
    (common/atomic-write-edn! (common/file-path output-directory "index.edn") index)
    {:output-file "index.edn"
     :chunk-count chunk-count
     :summary summary}))
