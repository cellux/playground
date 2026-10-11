(ns omkamra.vice.report
  "The human-readable `:report` HTML projection.

  `:report` is a presentation stage: it reads a committed `:segments` index, a
  committed `:disassembly` index, and the raw capture's per-chunk screenshots,
  then writes one self-contained HTML page plus a machine-readable EDN index.
  It copies nothing. Screenshots and `.asm` files are referenced with relative
  links, so a report stays small and always shows the artifacts it claims to;
  the index records every link, so a consumer never has to guess a path.

  Screenshots come from committed chunk boundaries, which are exact rendered
  frames at known event indices. `:screenshots-per-segment` (default 3)
  boundaries are chosen nearest to 25%, 50%, 75% … of a segment's event range
  (its duration), so the frames sample the interior of the segment rather than
  its edges. A boundary outside the segment range is kept and marked as such,
  which is what happens for segments shorter than the chunk granularity. A
  capture without screenshots still produces a report, with the screenshot
  count recorded as zero.

  Generation layout:

  ```text
  analysis/report/<generation>/
    index.edn
    report.html
  ```"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [omkamra.vice.analysis :as analysis]
            [omkamra.vice.contract :as contract]
            [omkamra.vice.disassembly :as disassembly]
            [omkamra.vice.reader :as reader]
            [omkamra.vice.store :as store]))

(def report-version 3)
(def report-format (contract/format-keyword :report report-version))

(def default-configuration
  {:screenshots-per-segment 3})

(def ^:private html-file-name "report.html")

(defn- fail
  [reason data]
  (throw (ex-info "Cannot build the report" (assoc data :reason reason))))

;; Relative links ------------------------------------------------------------
;;
;; A report generation lives at `<dataset-root>/analysis/report/<generation>/`,
;; so the dataset root is three levels up and sibling analysis stages two.

(defn screenshot-link
  "Relative link from a report page to a chunk's screenshot PNG."
  [chunk-number]
  (format "../../../capture/chunks/chunk-%06d.png" (long chunk-number)))

(defn assembly-link
  "Relative link from a report page to a file in a `:disassembly` generation."
  [generation file]
  (str "../../disassembly/" generation "/" file))

;; Screenshot selection ------------------------------------------------------

(defn target-indices
  "Event indices to sample inside `event-range`: 25%, 50%, 75% … of its span.

  Percentages are of the segment's event range, which tracks its duration
  closely enough for choosing frames."
  [event-range wanted]
  (let [[start end] event-range
        start (long start)
        length (- (long end) start)
        wanted (long wanted)]
    (mapv (fn [position]
            (+ start (quot (* length (long position)) (inc wanted))))
          (range 1 (inc wanted)))))

(defn- nearest-boundary
  "The boundary closest to `target`, preferring the earlier one on a tie."
  [boundaries target]
  (let [target (long target)
        distance (fn [boundary]
                   (Math/abs (- target (long (:event-index boundary)))))]
    (reduce (fn [best candidate]
              (if (< (long (distance candidate)) (long (distance best)))
                candidate
                best))
            boundaries)))

(defn choose-boundaries
  "Choose up to `wanted` chunk boundaries to show for one segment.

  `boundaries` is an ascending vector of `{:chunk-number n :event-index n}`
  entries. Each target (25%, 50%, 75% … of the segment's event range) is
  matched with the nearest boundary, keeping the order of the targets and
  dropping duplicates. A chosen boundary outside the segment range is kept and
  marked `:fallback? true`, so the page can say the frame sits outside the
  segment."
  [boundaries event-range wanted]
  (let [[start end] event-range
        wanted (long wanted)]
    (if (or (empty? boundaries) (<= wanted 0))
      []
      (let [length (double (max 1 (- (long end) (long start))))]
        (->> (target-indices event-range wanted)
             (reduce (fn [chosen target]
                       (let [boundary (nearest-boundary boundaries target)
                             chunk-number (:chunk-number boundary)]
                         (if (some #(= chunk-number (:chunk-number %)) chosen)
                           chosen
                           (conj chosen
                                 (assoc boundary
                                        :target (/ (double (- (long target)
                                                              (long start)))
                                                   length)
                                        :fallback?
                                        (not (and (<= (long start)
                                                      (long (:event-index boundary)))
                                                  (< (long (:event-index boundary))
                                                     (long end)))))))))
                     [])
             vec)))))

;; HTML rendering ------------------------------------------------------------

(def ^:private style
  (str "body{font:14px/1.5 system-ui,sans-serif;margin:2rem auto;max-width:64rem;"
       "color:#1b1b1b;background:#fdfdfd}"
       "h1{font-size:1.6rem;margin-bottom:.2rem}"
       "h2{font-size:1.15rem;margin-top:2rem;border-bottom:1px solid #ddd;"
       "padding-bottom:.2rem}"
       "h3{font-size:1rem;margin:1.2rem 0 .4rem}"
       "table{border-collapse:collapse;margin:.6rem 0;width:100%}"
       "th,td{text-align:left;padding:.25rem .5rem;border-bottom:1px solid #eee;"
       "vertical-align:top}"
       "th{color:#555;font-weight:600;white-space:nowrap}"
       "td.num{text-align:right;font-variant-numeric:tabular-nums}"
       "code,.mono{font-family:ui-monospace,monospace}"
       ".muted{color:#777}"
       ".shots{display:flex;flex-wrap:wrap;gap:1rem;margin:.6rem 0}"
       "figure{margin:0;max-width:22rem}"
       "img{width:100%;image-rendering:pixelated;border:1px solid #ccc}"
       "figcaption{font-size:.8rem;color:#666;margin-top:.2rem}"
       ".kind{font-weight:400;color:#666;font-size:.9rem}"
       "a{color:#0645ad}"))

(defn- escape
  [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn- item-text
  [value]
  (if (instance? clojure.lang.Named value)
    (name value)
    (str value)))

(defn- item-list
  [values]
  (if (seq values)
    (str/join ", " (sort (map item-text values)))
    "—"))

(defn- table
  [headings rows]
  (str "<table><thead><tr>"
       (apply str (map #(format "<th>%s</th>" (escape %)) headings))
       "</tr></thead><tbody>"
       (apply str rows)
       "</tbody></table>"))

(defn- row
  [label value]
  (format "<tr><th>%s</th><td>%s</td></tr>" (escape label) value))

(defn- seconds
  [cycles clock-hz]
  (if (and (number? cycles) (pos? (long (or clock-hz 0))))
    (format "%.2f s" (/ (double cycles) (double clock-hz)))
    "—"))

(defn- frames
  [cycles timing-model]
  (let [line-cycles (* (long (or (:cycles-per-line timing-model) 0))
                       (long (or (:raster-lines timing-model) 0)))]
    (if (and (number? cycles) (pos? line-cycles))
      (format "%.0f frames" (/ (double cycles) (double line-cycles)))
      "—")))

(defn- summary-rows
  [segments timing-model]
  (mapv (fn [{:keys [segment-id kind roles event-count cycles confidence]}]
          (format (str "<tr><td><a href=\"#segment-%03d\">segment-%03d</a></td>"
                       "<td>%s</td><td>%s</td><td class=\"num\">%d</td>"
                       "<td class=\"num\">%s</td><td class=\"num\">%s</td>"
                       "<td class=\"num\">%.2f</td></tr>")
                  (long segment-id) (long segment-id)
                  (escape (item-text kind))
                  (escape (item-list roles))
                  (long event-count)
                  (escape (seconds (:duration cycles) (:clock-hz timing-model)))
                  (escape (frames (:duration cycles) timing-model))
                  (double (or confidence 0.0))))
        segments))

(defn- screenshot-figure
  [screenshot]
  (let [{:keys [chunk-number event-index timing fallback? target]} screenshot
        caption (format "chunk %d · event %d · cycle %d · raster %d · ~%d%% of segment%s"
                        (long chunk-number)
                        (long event-index)
                        (long (or (:global-cycle timing) 0))
                        (long (or (:raster-line timing) 0))
                        (long (Math/round (* 100.0 (double (or target 0.0)))))
                        (if fallback?
                          " · boundary outside the segment"
                          ""))
        link (screenshot-link chunk-number)]
    (str "<figure><a href=\"" (escape link) "\">"
         "<img src=\"" (escape link) "\" alt=\"" (escape caption) "\"></a>"
         "<figcaption>" (escape caption) "</figcaption></figure>")))

(defn- segment-section
  [{:keys [segment-id kind roles irq-roles signals confidence event-range
           event-count cycles phases handler-contexts context-ids asm-file
           disassembly-generation screenshots]} timing-model]
  (let [[start end] event-range
        asm (when asm-file
              (format "<a href=\"%s\">%s</a>"
                      (escape (assembly-link disassembly-generation asm-file))
                      (escape asm-file)))]
    (str "<section id=\"segment-" (format "%03d" (long segment-id)) "\">"
         "<h2>segment-" (format "%03d" (long segment-id))
         " <span class=\"kind\">" (escape (item-text kind))
         (when (seq roles) (str " · " (escape (item-list roles))))
         "</span></h2>"
         (table ["Property" "Value"]
                (str (row "event range" (format "%d – %d" (long start) (long end)))
                     (row "events" (format "%d" (long event-count)))
                     (row "cycle span" (format "%d – %d"
                                               (long (or (:start cycles) 0))
                                               (long (or (:end cycles) 0))))
                     (row "duration" (escape (str (seconds (:duration cycles)
                                                           (:clock-hz timing-model))
                                                  " ("
                                                  (frames (:duration cycles) timing-model)
                                                  ")")))
                     (row "roles" (escape (item-list roles)))
                     (row "IRQ roles" (escape (item-list irq-roles)))
                     (row "signals" (escape (item-list signals)))
                     (row "confidence" (format "%.2f" (double (or confidence 0.0))))
                     (row "handler contexts" (escape (item-list handler-contexts)))
                     (row "context ids" (escape (item-list context-ids)))
                     (row "phases" (format "%d" (count phases)))
                     (row "disassembly" (or asm "—"))))
         (when (seq phases)
           (str "<h3>Phases</h3>"
                (table ["Role" "Event range" "Events"]
                       (apply str
                              (map (fn [{:keys [role event-range]}]
                                     (format (str "<tr><td>%s</td>"
                                                  "<td class=\"mono\">%d – %d</td>"
                                                  "<td class=\"num\">%d</td></tr>")
                                             (escape (item-text role))
                                             (long (first event-range))
                                             (long (second event-range))
                                             (- (long (second event-range))
                                                (long (first event-range)))))
                                   phases)))))
         "<h3>Screenshots</h3>"
         (if (seq screenshots)
           (str "<div class=\"shots\">"
                (apply str (map screenshot-figure screenshots))
                "</div>")
           "<p class=\"muted\">None: this capture has no committed chunk screenshot for this segment.</p>")
         "</section>")))

(defn- role-file-rows
  [generation role-files]
  (mapv (fn [{:keys [role block-count file]}]
          (format "<tr><td>%s</td><td class=\"num\">%d</td><td><a href=\"%s\">%s</a></td></tr>"
                  (escape (item-text role))
                  (long (or block-count 0))
                  (escape (assembly-link generation file))
                  (escape file)))
        role-files))

(defn render-html
  "Render the whole report page from a report index map.

  Pure: everything the page shows comes from the index, including every link,
  so a rendered page can be checked without touching the dataset."
  [{:keys [capture-id generation created-at event-count upstream summary
           segments timing-model capture-manifest-sha256 audit role-files
           disassembly-generation]
    :or {segments [] role-files []}}]
  (let [clock-hz (:clock-hz timing-model)
        screenshot-count (reduce + 0 (map (comp count :screenshots) segments))
        kinds (frequencies (map :kind segments))]
    (str "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
         "<title>" (escape (str capture-id " — VICE capture report")) "</title>\n"
         "<style>" style "</style>\n</head>\n<body>\n"
         "<h1>VICE capture report</h1>\n"
         "<p class=\"muted\">" (escape (str capture-id))
         " · report generation " (escape (str generation))
         (when created-at (str " · " (escape (str created-at))))
         "</p>\n"
         "<section id=\"overview\"><h2>Overview</h2>"
         (table ["Property" "Value"]
                (str (row "capture id" (escape (str capture-id)))
                     (row "capture manifest"
                          (format "<span class=\"mono\" title=\"%s\">%s…</span>"
                                  (escape (str capture-manifest-sha256))
                                  (escape (subs (str capture-manifest-sha256) 0 16))))
                     (row "events" (format "%d" (long (or event-count 0))))
                     (row "segments" (format "%d (%s)"
                                             (long (count segments))
                                             (escape (if (seq kinds)
                                                       (str/join ", "
                                                                 (map (fn [[kind n]]
                                                                        (str (item-text kind) " " n))
                                                                      (sort-by (comp item-text key) kinds)))
                                                       "none"))))
                     (row "screenshots" (format "%d" (long screenshot-count)))
                     (when summary
                       (row "screenshot boundaries"
                            (format "%d of %d chunks"
                                    (long (or (:screenshot-chunk-count summary) 0))
                                    (long (or (:capture-chunk-count summary) 0)))))
                     (row "timing model" (format "%.0f Hz · %d lines · %s cycles/line"
                                                 (double (or clock-hz 0))
                                                 (long (or (:raster-lines timing-model) 0))
                                                 (escape (str (:cycles-per-line timing-model)))))
                     (row "segments generation"
                          (format "<span class=\"mono\">%s</span>"
                                  (escape (str (:segments upstream)))))
                     (row "disassembly generation"
                          (format "<span class=\"mono\">%s</span>"
                                  (escape (str (:disassembly upstream)))))
                     (when audit
                       (row "boundary audit"
                            (escape (str "gaps " (count (:uncovered audit))
                                         ", overlaps " (count (:overlaps audit))))))))
         "</section>\n"
         "<section id=\"segments\"><h2>Segments</h2>"
         (table ["Segment" "Kind" "Roles" "Events" "Duration" "Frames" "Confidence"]
                (apply str (summary-rows segments timing-model)))
         "</section>\n"
         (when (seq role-files)
           (str "<section id=\"roles\"><h2>Role files</h2>"
                (table ["Role" "Blocks" "File"]
                       (apply str (role-file-rows disassembly-generation role-files)))
                "</section>\n"))
         (apply str (map #(segment-section % timing-model) segments))
         "</body>\n</html>\n")))

;; Build ---------------------------------------------------------------------

(defn- screenshots-per-segment
  [configuration]
  (let [value (:screenshots-per-segment configuration)]
    (when-not (and (integer? value) (not (neg? (long value))))
      (fail :invalid-screenshots-per-segment
            {:screenshots-per-segment value}))
    (long value)))

(defn- chunk-boundaries
  "Committed chunk boundaries that carry a screenshot, in ascending order."
  [capture]
  (filterv :screenshot
           (mapv (fn [chunk start]
                   {:chunk-number (:number chunk)
                    :event-index (long start)
                    :screenshot (reader/chunk-screenshot capture chunk)})
                 (:chunks capture)
                 (reader/chunk-starts capture))))

(defn- event-timing
  [capture event-index]
  (let [event-count (long (reader/event-count capture))
        index (min (max 0 (long event-index)) (dec event-count))
        [_ event] (first (reader/event-pairs capture [index (inc index)]))]
    (select-keys event [:global-cycle :raster-line :cpu-cycle])))

(defn- segment-entry
  [capture boundaries generation asm-by-id configuration segment]
  (let [segment-id (long (:segment-id segment))
        [start end] (:event-range segment)
        cycles {:start (long (or (:global-cycle (event-timing capture start)) 0))
                :end (long (or (:global-cycle (event-timing capture (dec (long end))))
                               0))}
        chosen (choose-boundaries boundaries (:event-range segment)
                                  (screenshots-per-segment configuration))]
    (assoc segment
           :cycles (assoc cycles :duration (- (:end cycles) (:start cycles)))
           :asm-file (:file (get asm-by-id segment-id))
           :disassembly-generation generation
           :screenshots (mapv (fn [boundary]
                                {:chunk-number (:chunk-number boundary)
                                 :event-index (:event-index boundary)
                                 :timing (event-timing capture (:event-index boundary))
                                 :target (:target boundary)
                                 :fallback? (boolean (:fallback? boundary))
                                 :file (screenshot-link (:chunk-number boundary))})
                              chosen))))

(defn build!
  "Build and commit a `:report` generation from `:segments` and `:disassembly`.

  `segments-image` and `disassembly-image` are opened images. Options:
  `:configuration` (`:screenshots-per-segment`), recorded in the index."
  ([segments-image disassembly-image]
   (build! segments-image disassembly-image {}))
  ([segments-image disassembly-image
    {:keys [configuration] :or {configuration {}}}]
   (let [configuration (merge default-configuration configuration)
         _ (screenshots-per-segment configuration)
         dataset-root (:dataset-root segments-image)
         capture (reader/open dataset-root)
         disassembly-stage (analysis/stage-entry dataset-root :disassembly)
         segments-stage (analysis/stage-entry dataset-root :segments)
         generation (or (:generation disassembly-stage)
                        (fail :missing-disassembly-generation
                              {:dataset-root dataset-root}))
         asm-by-id (into {} (map (juxt :segment-id identity))
                         (disassembly/segments disassembly-image))
         boundaries (chunk-boundaries capture)
         built (mapv #(segment-entry capture boundaries generation asm-by-id
                                     configuration %)
                     (:segments segments-image))
         index {:format report-format
                :version report-version
                :capture-id (get-in segments-image [:index :capture-id])
                :capture-manifest-sha256
                (get-in segments-image [:index :capture-manifest-sha256])
                :configuration configuration
                :event-count (get-in segments-image [:index :event-count])
                :timing-model (:timing-model capture)
                :upstream {:segments (:directory segments-stage)
                           :disassembly (:directory disassembly-stage)}
                :disassembly-generation generation
                :role-files (disassembly/role-files disassembly-image)
                :audit (:audit segments-image)
                :summary {:segment-count (count built)
                          :screenshot-count (reduce + 0 (map (comp count :screenshots)
                                                             built))
                          :screenshot-chunk-count (count boundaries)
                          :capture-chunk-count (count (:chunks capture))
                          :file-count 1}
                :segments built}
         {:keys [generation directory relative]}
         (analysis/new-generation dataset-root :report)
         html (render-html (assoc index
                                  :generation generation
                                  :created-at (str (java.time.Instant/now))))]
     (try
       (let [spec (store/write-artifact!
                   (io/file directory html-file-name)
                   (fn [out]
                     (let [bytes (.getBytes ^String html "UTF-8")]
                       (.write out bytes 0 (alength bytes)))))
             index (assoc index
                          :files [{:file html-file-name
                                   :size (:size spec)
                                   :sha256 (:sha256 spec)}])]
         (store/atomic-write-edn! (io/file directory "index.edn") index)
         (analysis/commit-stage!
          dataset-root :report
          {:generation generation
           :directory relative
           :version report-version
           :capture-id (:capture-id index)
           :capture-manifest-sha256 (:capture-manifest-sha256 index)
           :configuration configuration
           :summary (:summary index)})
         {:dataset-root dataset-root
          :directory (str directory)
          :index index})
       (catch Throwable error
         (store/delete-tree! directory)
         (throw error))))))

;; Readers -------------------------------------------------------------------

(defn open
  [dataset-root]
  (let [stage (analysis/stage-entry dataset-root :report)]
    (when-not stage
      (throw (ex-info "Dataset has no committed :report generation"
                      {:reason :missing-report-generation
                       :dataset-root dataset-root})))
    (let [directory (analysis/stage-directory dataset-root stage)
          index (store/read-edn (io/file directory "index.edn"))]
      (when-not (= report-format (:format index))
        (throw (ex-info "Not a v2 report image"
                        {:reason :not-a-report-image :format (:format index)})))
      {:dataset-root dataset-root
       :directory directory
       :index index
       :segments (:segments index)})))

(defn status [dataset-root]
  (analysis/stage-entry dataset-root :report))

(defn segments [report-image]
  (:segments report-image))

(defn screenshots
  "Screenshots chosen for one segment entry of a report image."
  [segment]
  (:screenshots segment))

(defn html-file
  "The `File` of a report image's HTML page."
  [report-image]
  (io/file (:directory report-image) html-file-name))

(defn html
  "Read a report image's HTML page."
  [report-image]
  (slurp (html-file report-image)))
