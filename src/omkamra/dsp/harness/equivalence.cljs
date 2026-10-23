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
            [omkamra.dsp.compiler :as compiler]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.harness.diff :as diff]
            [omkamra.dsp.harness.emission :as emission]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.linker :as linker]))

(def ^:private payload-url "/omkamra/dsp/harness/testbed/equivalence")

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
  (let [unit (linker/link definition)
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
  [entry expected]
  (vec
   (mapcat
    (fn [precision]
      (let [compile (fn [target options]
                      (compiler/compile (:definition entry)
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
  (if (:library? entry)
    {::skipped id}
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
            artifacts (artifact-differences entry (:emissions entry))
            differences (concat compiled (when serialized [serialized])
                                emitted artifacts)]
        {:case id
         :passed? (empty? differences)
         :failures (mapv #(assoc % :detail (pr-str %)) differences)})
      (catch :default error
        {:case id
         :passed? false
         :failures [{:detail (str "compiling threw: " error)}]}))))

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
