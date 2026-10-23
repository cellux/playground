(ns omkamra.dsp.harness.equivalence
  "The compiler equivalence check, run in the page (P13, D79).

  The JVM serves the catalog's definitions and its own results for them; this
  compiles each definition with *the compiler compiled into this page* —
  `descriptor/normalize`, `linker/link`, `ir/lower-*`, both emitters and
  `squint.compiler` as real ClojureScript, not as Clojure on a server — and
  compares with `omkamra.dsp.harness.diff`. It compiles through
  `omkamra.dsp.compiler`, the same pipeline `dsp/compile` uses, so the artifact a
  page makes for itself is what is compared and not an emitter half (P13c).

  That is the phase's claim turned into a test: the browser and the JVM are one
  implementation in two worlds, and the check is what says so, per definition,
  every time the suite runs.

  Two things are deliberately not failures here. A definition whose unit reaches
  for a library definition cannot be linked in the page, because the page has not
  been given those definitions — that is P13d's browser definition registry, and
  the count is reported as `:skipped` so the gap stays visible instead of being a
  green nothing. Everything else that goes wrong is a failure, including an
  exception: a compiler that throws is exactly what this check exists to catch."
  (:require [cljs.reader :as reader]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.compiler :as compiler]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.harness.call-fixture]
            [omkamra.dsp.harness.diff :as diff]
            [omkamra.dsp.harness.emission :as emission]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.library-registry :as library]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.registry :as registry]
            [omkamra.dsp.source :as source]))

(def ^:private payload-url "/omkamra/dsp/harness/testbed/equivalence")

(defonce definitions
  ;; The library the page compiled, plus the call fixtures the catalog's
  ;; call/partition cases reach for. This is the registry P13d adds: the page can
  ;; now link a definition whose unit reaches beyond its own data.
  (registry/seed (merge library/library-definitions
                        (registry/definitions-of
                          (ns-publics 'omkamra.dsp.harness.call-fixture)))))

(defn- link-here
  "Link a definition in the page, resolving library and fixture calls through
  the registry rather than a Var lookup."
  [definition]
  (linker/link definition {:resolve (registry/resolver definitions)}))

(defn- fetch-text
  [url]
  (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "equivalence endpoint returned " (.-status response)))))))))

(defn- compile-here
  "Compile one payload entry with this build's compiler."
  [{:keys [definition entry channels]}]
  (let [unit (link-here definition)
        ir (if (= entry :function)
             (ir/lower-unit unit)
             (ir/lower-process unit {:channels channels}))]
    {:normalized (descriptor/normalize definition)
     :unit unit
     :ir ir
     ;; The canonical serialization is compared as text (D77): it is the one
     ;; spelling both worlds can produce, and a cache key later rests on it.
     :canonical (ir/canonical-text ir)}))

(defn- artifact-differences
  "What the browser's own compile entry point makes of a definition, compared
  with the JVM's module (P13c).

  `compile-here` above exercises the compiler's *parts*; this exercises the
  entry point a page actually uses — `omkamra.dsp.compiler/compile` — from the
  raw definition and options to the JavaScript artifact, Squint included. A
  difference here is one the artifact would have, not only one an emitter half
  would."
  [entry expected unit]
  (vec
   (mapcat
    (fn [precision]
      (let [compile (fn [target options]
                      (compiler/compile unit
                                        (merge {:target target
                                                :entry (:entry entry)
                                                :precision precision
                                                :channels (:channels entry)}
                                               options)))
            compare-to (fn [target label artifact-text expected-text]
                         (when-let [difference (diff/text-difference artifact-text
                                                                     expected-text)]
                           (assoc difference
                                  :target target
                                  :precision precision
                                  :detail (str label " at " (name precision)
                                               ": " (pr-str (dissoc difference :target
                                                                    :precision))))))]
        (try
          (keep identity
                [(compare-to :js "compiled JS module"
                             (:source (compile :js {}))
                             (get-in expected [precision :js-module]))
                 ;; A Wasm artifact exists only for a process entry; a function
                 ;; entry is refused by the capability check (D29), so it is not
                 ;; asked for here.
                 (when (= :process (:entry entry))
                   (compare-to :wasm "compiled WAT"
                               (:wat (compile :wasm {}))
                               (get-in expected [precision :wat])))])
          (catch :default error
            [{:target :js
              :precision precision
              :detail (str "compiling to an artifact threw: " error)}]))))
    emission/precisions)))

(defn- compare-case
  [[id entry]]
  (try
    (let [{:keys [normalized unit ir canonical]} (compile-here entry)
          compiled (remove nil?
                           [(diff/first-difference normalized (:normalized entry))
                            (diff/first-difference unit (:unit entry))
                            (diff/first-difference ir (:ir entry))])
          ;; The canonical serialization, compared as text: it is the one
          ;; spelling both worlds can produce, and a cache key rests on it.
          serialized (when-let [difference (diff/text-difference
                                            canonical (:canonical entry))]
                       (assoc difference
                              :detail (str "canonical IR differs at "
                                           (pr-str (:path difference)) ": "
                                           (pr-str difference))))
          ;; The emitters, from an IR lowered *here*, at each width. Nothing is
          ;; taken from the payload but the definition itself and how to compile
          ;; it, so a difference is a difference the emitter made.
          emitted (emission/differences unit entry (:emissions entry))
          ;; And the artifact the page compiles for itself, through the compile
          ;; entry point rather than the emitter's half (P13c).
          artifacts (artifact-differences entry (:emissions entry) unit)
          differences (concat compiled (when serialized [serialized])
                              emitted artifacts)]
      {:case id
       :passed? (empty? differences)
       :failures (mapv #(assoc % :detail (pr-str %)) differences)})
    (catch :default error
      {:case id
       :passed? false
       :failures [{:detail (str "compiling threw: " error)}]})))

(defn check!
  "Fetch the payload, compile it here, and return one check in the suite's shape."
  []
  (-> (fetch-text payload-url)
      (.then
       (fn [text]
         (let [{:keys [cases uncompilable]} (reader/read-string text)
               rows (mapv compare-case cases)
               failed (filterv (fn [row] (and (not (::skipped row))
                                              (not (:passed? row))))
                               rows)
               not-compiled (filterv ::skipped rows)
               uncompilable (or uncompilable [])]
           {:name "compiler equivalence"
            :passed? (and (empty? failed) (empty? uncompilable))
            :case-count (count rows)
            :case-failures (count failed)
            :skipped (mapv ::skipped not-compiled)
            :detail (str "compiled " (count rows) " catalog definitions in the page"
                         " and compared each with the JVM's own normalization,"
                         " linked unit, IR, canonical IR text, emitted JS forms and"
                         " module, and Wasm module"
                         (when (seq not-compiled)
                           (str "; skipped " (count not-compiled)
                                " that call library definitions the page has not"
                                " been given (P13d's registry)"))
                         (when (seq uncompilable)
                           (str "; " (count uncompilable) " the JVM could not compile"))
                         (when-let [row (first failed)]
                           (str "; first failure: " (:case row) " — "
                                (:detail (first (:failures row))))))
            :cases (into [] (comp (remove ::skipped)) rows)})))))

;; ---------------------------------------------------------------------------
;; Source equivalence (P13d)

(def ^:private source-payload-url
  "/omkamra/dsp/harness/testbed/source-equivalence")

(defn- source-artifact-differences
  "What the page's own `dsp/compile` makes of a source case, compared with the
  JVM's module and Wasm text."
  [entry]
  (vec
   (mapcat
    (fn [precision]
      (let [compile (fn [target]
                      (dsp/compile (:source entry)
                                   {:definition (:definition entry)
                                    :entry (:entry entry)
                                    :precision precision
                                    :channels (:channels entry)
                                    :target target}))
            compare-to (fn [target label actual expected]
                         (when-let [difference (diff/text-difference actual expected)]
                           (assoc difference
                                  :target target
                                  :precision precision
                                  :detail (str label " at " (name precision)
                                               ": " (pr-str difference)))))]
        (try
          (keep identity
                [(compare-to :js "compiled JS module"
                             (:source (compile :js))
                             (get-in entry [:emissions precision :js-module]))
                 (when (= :process (:entry entry))
                   (compare-to :wasm "compiled WAT"
                               (:wat (compile :wasm))
                               (get-in entry [:emissions precision :wat])))])
          (catch :default error
            [{:target :js
              :precision precision
              :detail (str "compiling source to an artifact threw: " error)}]))))
    (:precisions entry))))

(defn- compare-source-case
  [[id entry]]
  (try
    (let [{captured :definition captured-definitions :definitions}
          (source/evaluate (:source entry)
                           {:definition (:definition entry)
                            :evaluator :sci
                            :library library/library-definitions})
          unit (source/link-definition captured
                                       {:definitions captured-definitions
                                        :resolve (registry/resolver definitions)})
          ir-value (if (= :function (:entry entry))
                     (ir/lower-unit unit)
                     (ir/lower-process unit {:channels (:channels entry)}))
          differences (concat
                       (when-let [d (diff/first-difference (descriptor/normalize captured)
                                                           (:normalized entry))]
                         [(assoc d :detail (str "descriptor differs: " (pr-str d)))])
                       (when-let [d (diff/text-difference (ir/canonical-text ir-value)
                                                          (:canonical entry))]
                         [(assoc d :detail (str "canonical IR differs: " (pr-str d)))])
                       (emission/differences unit {:entry (:entry entry)
                                                   :channels (:channels entry)}
                                             (:emissions entry))
                       (source-artifact-differences entry))]
      {:case id
       :passed? (empty? differences)
       :failures (mapv #(assoc % :detail (pr-str %)) differences)})
    (catch :default error
      {:case id
       :passed? false
       :failures [{:detail (str "compiling source threw: " error)}]})))

;; ---------------------------------------------------------------------------
;; Compiling given definitions here (P13d)

(defn- compare-entry
  "Compile one payload entry here and compare it with the JVM's.

  `definition` is what to compile: this build's own capture when the entry names
  a definition the build already has, the served data otherwise."
  [id entry definition resolve]
  (try
    ;; `source/link-definition` rather than `linker/link`: a derived definition
    ;; reaches the definitions it was built from through `:dsp/dependencies`,
    ;; which only the source layer carries into the link.
    (let [unit (source/link-definition definition {:resolve resolve})
          ir-value (if (= :function (:entry entry))
                     (ir/lower-unit unit)
                     (ir/lower-process unit {:channels (:channels entry)}))
          differences (concat
                       (when-let [d (diff/first-difference (descriptor/normalize definition)
                                                           (:normalized entry))]
                         [(assoc d :detail (str "descriptor differs: " (pr-str d)))])
                       (when-let [d (diff/text-difference (ir/canonical-text ir-value)
                                                          (:canonical entry))]
                         [(assoc d :detail (str "canonical IR differs: " (pr-str d)))])
                       (emission/differences unit entry (:emissions entry))
                       (artifact-differences entry (:emissions entry) unit))]
      {:case id
       :passed? (empty? differences)
       :failures (mapv #(assoc % :detail (pr-str %)) differences)})
    (catch :default error
      {:case id
       :passed? false
       :failures [{:detail (str "compiling " id " threw: " error)}]})))

(defn check-definitions!
  "Fetch a payload of compiled definitions from `url` and compile each here.

  `local` is a `{qualified-id descriptor}` map of definitions this build already
  has, because its own macros captured them. An entry that names one is compiled
  from *this* world's capture rather than from the served data, which is what
  shows the two macro captures agree; every other entry is compiled from the
  definition the payload carries. Either way the descriptor, the canonical IR and
  both artifacts are compared, so one generic path serves both a catalog of small
  definitions and one large instrument."
  [name url local]
  (let [resolve (registry/resolver
                 (registry/seed (merge library/library-definitions local)))]
    (-> (fetch-text url)
        (.then
         (fn [text]
           (let [cases (reader/read-string text)
                 rows (mapv (fn [[id entry]]
                              (compare-entry id entry
                                             (or (get local (:definition-id entry))
                                                 (:definition entry))
                                             resolve))
                            cases)
                 failed (filterv (complement :passed?) rows)]
             {:name name
              :passed? (empty? failed)
              :case-count (count rows)
              :case-failures (count failed)
              :detail (str "compiled " (count rows)
                           " definitions here and compared each with the JVM's"
                           " descriptor, canonical IR and both artifacts"
                           (when-let [row (first failed)]
                             (str "; first failure: " (:case row) " — "
                                  (:detail (first (:failures row))))))
              :cases rows}))))))

(defn source-check!
  "Fetch the source payload, compile each source string here with SCI, and
  compare the descriptors, IR and artifacts with the JVM's own reader and macros
  (P13d)."
  []
  (-> (fetch-text source-payload-url)
      (.then
       (fn [text]
         (let [{:keys [cases]} (reader/read-string text)
               rows (mapv compare-source-case cases)
               failed (filterv (complement :passed?) rows)]
           {:name "source equivalence"
            :passed? (empty? failed)
            :case-count (count rows)
            :case-failures (count failed)
            :detail (str "compiled " (count rows)
                         " source strings with SCI in the page and compared each"
                         " with the JVM's own reader and macros"
                         (when-let [row (first failed)]
                           (str "; first failure: " (:case row) " — "
                                (:detail (first (:failures row))))))
            :cases rows})))))
