(ns omkamra.dsp.harness.equivalence
  "The compiler equivalence check, run in the page (P13, D79).

  The JVM serves the catalog's definitions and its own results for them; this
  compiles each definition with *the compiler compiled into this page* —
  `descriptor/normalize`, `linker/link` and `ir/lower-*` as real ClojureScript,
  not as Clojure on a server — and compares with `omkamra.dsp.harness.diff`.

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
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.harness.diff :as diff]
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
  (let [unit (linker/link definition)]
    {:normalized (descriptor/normalize definition)
     :unit unit
     :ir (if (= entry :function)
           (ir/lower-unit unit)
           (ir/lower-process unit {:channels channels}))}))

(defn- compare-case
  [[id entry]]
  (if (:library? entry)
    {::skipped id}
    (try
      (let [{:keys [normalized unit ir]} (compile-here entry)
            differences (remove nil?
                                [(diff/first-difference normalized (:normalized entry))
                                 (diff/first-difference unit (:unit entry))
                                 (diff/first-difference ir (:ir entry))])]
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
                         " linked unit and IR"
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
