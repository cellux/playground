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

(defn case-definition
  "Resolve a case's DSP definition: an inline descriptor map, or a library Var
  named by `:definition-var` (so the catalog can test user-facing library code
  without inlining it)."
  [case]
  (if-let [var-symbol (:definition-var case)]
    (or (requiring-resolve var-symbol)
        (throw (ex-info "conformance case refers to an unknown DSP library Var"
                        {:case (:id case) :definition-var var-symbol})))
    (:definition case)))

(defn target-artifact
  [case precision target]
  (dsp/compile (case-definition case)
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
  [case precision blocks input?]
  (let [block-invocations
        (mapv (fn [block]
                (let [frames (nth blocks block)]
                  (cond-> {:op :process :frames frames}
                    input?
                    (assoc :input (acceptance/input-samples precision case block
                                                            frames)))))
              (range (count blocks)))
        ;; NaN/infinity propagation is injected through the input sample, so it
        ;; is meaningless for a generator, which has no input port.
        special-invocations
        (if input?
          (mapv (fn [kind]
                  {:op :process :frames 1 :input [(special-value precision kind)]})
                [:nan :infinity :-infinity])
          [])]
    (into [{:op :reset}]
          (concat block-invocations
                  (when input?
                    [{:op :reset} {:op :process :frames 1 :input [0.0]}])
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
                                         (acceptance/case-blocks case)
                                         (boolean (seq (get-in artifact [:abi :inputs]))))})))

(defn- real-array
  [precision values]
  (if (= :f64 precision)
    (double-array values)
    (float-array values)))

(defn- run-process-once
  [artifact input controls sample-rate frames]
  (let [precision (:precision (:ir artifact))
        out (real-array precision (repeat frames 0.0))
        ;; Whether an input is passed is a property of the process, not of the
        ;; block: a 0-frame invocation still has an (empty) input buffer, while a
        ;; generator has none at any length.
        input? (boolean (seq (get-in artifact [:abi :inputs])))]
    (dsp/process! artifact (cond-> {:output out
                                    :frames frames
                                    :sample-rate sample-rate
                                    :controls controls}
                             input? (assoc :input (real-array precision (or input [])))))
    (vec out)))

(defn run-function-plan
  [artifact plan]
  (mapv (fn [args] (apply dsp/invoke artifact args)) (:vectors plan)))

(defn run-process-plan
  [artifact plan]
  (dsp/reset! artifact)
  (reduce (fn [outputs {:keys [op input frames]}]
            (case op
              :reset (do (dsp/reset! artifact) outputs)
              :process (conj outputs
                             (run-process-once artifact input
                                               (:controls plan)
                                               (:sample-rate plan)
                                               frames))))
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
  "Exact scalar comparison: NaN-equal-as-NaN and signed-zero aware (D9)."
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

(defn within-tolerance?
  "Documented transcendental tolerance for a target whose op is an emulation."
  [precision actual expected]
  (let [a (double actual)
        e (double expected)
        relative (if (= :f64 precision) 1.0e-9 1.0e-6)
        absolute (if (= :f64 precision) 1.0e-12 1.0e-7)]
    (and (Double/isFinite a)
         (Double/isFinite e)
         (<= (Math/abs (- a e)) (+ absolute (* relative (Math/abs e)))))))

(defmulti scalar-match?
  "Compare one target value against the interpreter's expected value.

  The default is exact; `:wasm` additionally allows the documented tolerance
  because its transcendental emulations cannot reproduce the host `Math`
  bit-for-bit (`DECISIONS.md` D14). The interpreter stays the single oracle."
  (fn [target _precision _actual _expected] target))

(defmethod scalar-match? :default
  [_target _precision actual expected]
  (matches? actual expected))

(defmethod scalar-match? :wasm
  [_target precision actual expected]
  (or (matches? actual expected)
      (and (number? actual)
           (number? expected)
           (within-tolerance? precision actual expected))))

(defn results-match?
  "Recursively compare a target's outputs against the interpreter's expected."
  [target precision expected actual]
  (and (= (count expected) (count actual))
       (every? true?
               (map (fn [e a]
                      (cond
                        (and (sequential? e) (sequential? a))
                        (results-match? target precision e a)
                        :else (scalar-match? target precision a e)))
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
