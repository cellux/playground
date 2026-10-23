(ns omkamra.dsp.harness.formatting
  "The number-formatting check, run in the page (P13b).

  A kernel's constants become text, and the two worlds make that text from
  different primitives: Clojure has a float type and ClojureScript has one number
  type made of doubles. `omkamra.dsp.numbers` is the one place that decides, and
  this is what says the decision came out the same on both sides.

  It is deliberately not a comparison of the catalog. The catalog's constants are
  small round numbers, and every divergence the sweep behind this check found is
  outside them: the shortest spelling of the smallest subnormals, an exact decimal
  tie at f32 width, an f32 that rounds to the largest one, and an integer printed
  beyond 2^53. Those are the values a formatter is actually wrong about, so they
  are the values it is asked about."
  (:require [cljs.reader :as reader]
            [omkamra.dsp.numbers :as numbers]))

(def ^:private payload-url "/omkamra/dsp/harness/testbed/numbers")

(def ^:private reported-mismatches
  "How many differing values a failure names. The first few are the diagnosis;
  the count is the measure."
  4)

(defn- fetch-text
  [url]
  (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (js/Promise.reject
                  (js/Error. (str "numbers endpoint returned " (.-status response)))))))))

(defn- mismatches
  "Every value in `corpus` whose spelling this world does not produce."
  [width of-bits format corpus]
  (vec (keep (fn [[bits expected]]
               (let [value (of-bits bits)
                     actual (format value)]
                 (when (not= expected actual)
                   {:width width :bits bits :expected expected :actual actual})))
             corpus)))

(defn- case-result
  [width failures count]
  {:case (keyword width)
   :passed? (empty? failures)
   :failures (mapv (fn [failure]
                     ;; No target: this is not a backend's difference, it is
                     ;; the one formatter both backends ask (D75's rule that a
                     ;; failure without a target reports no target).
                     {:detail (str "bits " (:bits failure)
                                   ": expected " (:expected failure)
                                   ", got " (:actual failure))})
                   (take reported-mismatches failures))
   :count count})

(defn check!
  "Fetch the corpus, spell every value here, and return one check in the suite's
  shape."
  []
  (-> (fetch-text payload-url)
      (.then
       (fn [text]
         (let [{:keys [f64 f32]} (reader/read-string text)
               f64-failures (mismatches "f64" (fn [bits] (numbers/f64-of-bits (js/BigInt bits)))
                                        numbers/f64->text f64)
               f32-failures (mismatches "f32" (fn [bits] (numbers/f32-of-bits (js/parseInt bits 16)))
                                        numbers/f32->text f32)
               rows [(case-result "f64" f64-failures (count f64))
                     (case-result "f32" f32-failures (count f32))]
               failed (filterv (fn [row] (not (:passed? row))) rows)
               total (+ (count f64) (count f32))]
           {:name "number formatting"
            :passed? (empty? failed)
            :case-count (count rows)
            :case-failures (count failed)
            :detail (str "spelled " (count f64) " doubles and " (count f32)
                         " floats in the page and compared each with the JVM's"
                         " own spelling"
                         (when (seq failed)
                           (str "; " (count f64-failures) " doubles and "
                                (count f32-failures) " floats differ, of " total))
                         (when-let [row (first failed)]
                           (str "; first: " row)))
            :cases rows})))))
