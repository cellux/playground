(ns omkamra.dsp.conformance
  "Interpreter-oracle conformance driving (DECISIONS.md D2/D7/D13).

  A conformance *plan* is target-neutral data built from an
  `omkamra.dsp.acceptance` case: the compiled portable IR, the process controls
  or function vectors, and the invocation sequence (including reset and special
  values).  There are no hand-written expected values: the reference
  interpreter computes them, on the JVM here and in the browser via the
  `.cljc` interpreter.

  The JVM suite compares the interpreter with the JVM backend; the browser
  serves the serialized plan and compares the JS/Wasm kernel with the
  interpreter it runs locally."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.acceptance :as acceptance]))

(def process-blocks
  "Block lengths exercised per process case, in order."
  acceptance/block-lengths)

(defn function-plan?
  [plan]
  (= :function (:entry plan)))

(defn target-artifact
  [case precision target]
  (dsp/compile (:definition case)
               (cond-> {:target target :precision precision}
                 (not (acceptance/function-case? case))
                 (assoc :entry :process))))

(defn- special-value
  [precision kind]
  (if (= :f64 precision)
    (case kind
      :nan Double/NaN
      :infinity Double/POSITIVE_INFINITY
      :-infinity Double/NEGATIVE_INFINITY)
    (case kind
      :nan Float/NaN
      :infinity Float/POSITIVE_INFINITY
      :-infinity Float/NEGATIVE_INFINITY)))

(defn- process-invocations
  [case precision blocks]
  (let [block-invocations
        (mapv (fn [block]
                (let [frames (nth blocks block)]
                  {:op :process
                   :frames frames
                   :input (acceptance/input-samples precision case block frames)}))
              (range (count blocks)))
        special-invocations
        (mapv (fn [kind]
                {:op :process :frames 1 :input [(special-value precision kind)]})
              [:nan :infinity :-infinity])]
    (into [{:op :reset}]
          (concat block-invocations
                  [{:op :reset} {:op :process :frames 1 :input [0.0]}]
                  (mapcat (fn [invocation] [{:op :reset} invocation])
                          special-invocations)))))

(defn plan
  "Build the target-neutral conformance plan for a case and precision.

  The plan embeds the interpreter's portable IR; expected outputs are computed
  by running that IR, not stored in the plan."
  [case precision]
  (let [artifact (target-artifact case precision :interpreter)
        ir (:ir artifact)]
    (if (acceptance/function-case? case)
      {:case (:id case)
       :entry :function
       :precision precision
       :return-type (get-in artifact [:abi :return-type])
       :ir ir
       :vectors (:vectors case)}
      {:case (:id case)
       :entry :process
       :precision precision
       :ir ir
       :controls (:controls case)
       :sample-rate acceptance/sample-rate
       :invocations (process-invocations case precision
                                         (acceptance/case-blocks case))})))

(defn- real-array
  [precision values]
  (if (= :f64 precision)
    (double-array values)
    (float-array values)))

(defn- run-process-once
  [artifact input controls sample-rate]
  (let [precision (:precision (:ir artifact))
        out (real-array precision (repeat (count input) 0.0))]
    (dsp/process! artifact {:input (real-array precision input)
                            :output out
                            :frames (count input)
                            :sample-rate sample-rate
                            :controls controls})
    (vec out)))

(defn run-function-plan
  [artifact plan]
  (mapv (fn [args] (apply dsp/invoke artifact args)) (:vectors plan)))

(defn run-process-plan
  [artifact plan]
  (dsp/reset! artifact)
  (reduce (fn [outputs {:keys [op input]}]
            (case op
              :reset (do (dsp/reset! artifact) outputs)
              :process (conj outputs
                             (run-process-once artifact input
                                               (:controls plan)
                                               (:sample-rate plan)))))
          []
          (:invocations plan)))

(defn run-plan
  "Run a plan on a target artifact, returning outputs (process) or values."
  [artifact plan]
  (if (function-plan? plan)
    (run-function-plan artifact plan)
    (run-process-plan artifact plan)))

;; ---------------------------------------------------------------------------
;; Comparison: exact for finite values, signed-zero aware, NaN-equal-as-NaN
;; ---------------------------------------------------------------------------

(defn matches?
  [actual expected]
  (if (and (number? actual) (number? expected))
    (let [a (double actual)
          e (double expected)]
      (cond
        (and (Double/isNaN a) (Double/isNaN e)) true
        (or (Double/isNaN a) (Double/isNaN e)) false
        (and (zero? a) (zero? e))
        (= (Double/doubleToRawLongBits a) (Double/doubleToRawLongBits e))
        :else (= a e)))
    (= actual expected)))

(defn results-match?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true?
               (map (fn [e a]
                      (cond
                        (and (sequential? e) (sequential? a)) (results-match? e a)
                        :else (matches? a e)))
                    expected actual))))

;; ---------------------------------------------------------------------------
;; Serialization for the browser endpoint
;; ---------------------------------------------------------------------------

(defn- encode-number
  [value]
  (let [x (double value)]
    (cond
      (Double/isNaN x) "NaN"
      (Double/isInfinite x) (if (pos? x) "Inf" "-Inf")
      :else (str x))))

(defn- encode-value
  [value]
  (cond
    (boolean? value) value
    (number? value) (encode-number value)
    :else (throw (ex-info "cannot encode conformance value"
                          {:value value}))))

(defn serialize-plan
  "Return a JSON-ready representation of a plan (IR as EDN text, numbers encoded
  as strings so NaN and signed zero survive JSON)."
  [plan]
  (let [plan (update plan :ir pr-str)]
    (if (function-plan? plan)
      (update plan :vectors
              (fn [vectors]
                (mapv (fn [args] (mapv encode-value args)) vectors)))
      (-> plan
          (update :controls #(mapv encode-value %))
          (update :invocations
                  (fn [invocations]
                    (mapv (fn [invocation]
                            (cond-> invocation
                              (:input invocation)
                              (update :input #(mapv encode-value %))))
                          invocations)))))))
