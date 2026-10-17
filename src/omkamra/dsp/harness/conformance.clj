(ns omkamra.dsp.harness.conformance
  "Interpreter-oracle conformance driving (DECISIONS.md D2/D7/D13).

  A conformance *plan* is target-neutral data built from an
  `omkamra.dsp.harness.catalog` case: the compiled portable IR, the process controls
  or function vectors, and the invocation sequence (including reset and special
  values).  There are no hand-written expected values: the reference
  interpreter computes them, on the JVM here and in the browser via the
  `.cljc` interpreter.

  The JVM suite compares the interpreter with the JVM backend; the browser
  serves the serialized plan and compares the JS/Wasm kernel with the
  interpreter it runs locally."
  (:require [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.catalog :as catalog]))

(def process-blocks
  "Block lengths exercised per process case, in order."
  catalog/block-lengths)

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
                 (not (catalog/function-case? case))
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
  (let [;; A lane carries one float per frame, so its values are block-indexed
        ;; like the input trace. Every process invocation needs them, including
        ;; the one-frame probes below: a lane process has no valid block without
        ;; its region.
        lane-item (fn [block frames]
                    (when (:lane-samples case)
                      {:lanes (catalog/lane-samples precision case block frames)}))
        block-invocations
        (mapv (fn [block]
                (let [frames (nth blocks block)]
                  (cond-> (merge {:op :process :frames frames} (lane-item block frames))
                    input?
                    (assoc :input (catalog/input-samples precision case block
                                                         frames)))))
              (range (count blocks)))
        ;; NaN/infinity propagation is injected through the input sample, so it
        ;; is meaningless for a generator, which has no input port.
        special-invocations
        (if input?
          (mapv (fn [kind]
                  (merge {:op :process :frames 1 :input [(special-value precision kind)]}
                         (lane-item 0 1)))
                [:nan :infinity :-infinity])
          [])]
    (into [{:op :reset}]
          (concat block-invocations
                  (when input?
                    [{:op :reset}
                     (merge {:op :process :frames 1 :input [0.0]} (lane-item 0 1))])
                  (mapcat (fn [invocation] [{:op :reset} invocation])
                          special-invocations)))))

(defn plan
  "Build the target-neutral conformance plan for a case and precision.

  The plan embeds the interpreter's portable IR; expected outputs are computed
  by running that IR, not stored in the plan."
  [case precision]
  (let [artifact (target-artifact case precision :interpreter)
        ir (:ir artifact)]
    (if (catalog/function-case? case)
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
       ;; The lane region's shape is the ABI's, shared by every target: each
       ;; lane is one `:lane-stride`-frame slice, named individually. Targets
       ;; that address linear memory need the offsets; targets that take a host
       ;; array need only the layout's order.
       :lane-stride (get-in artifact [:abi :lane-stride])
       ;; `name`, not `str`: the plan travels as JSON, where a keyword's `str`
       ;; would carry its leading colon and would never match a lane lookup.
       :lane-layout (mapv (fn [lane] {:name (name (:name lane)) :offset (:offset lane)})
                          (get-in artifact [:abi :lanes]))
       :sample-rate catalog/sample-rate
       :invocations (process-invocations case precision
                                         (catalog/case-blocks case)
                                         (boolean (seq (get-in artifact [:abi :inputs]))))})))

(defn- real-array
  [precision values]
  (if (= :f64 precision)
    (double-array values)
    (float-array values)))

(defn- run-process-once
  [artifact input controls sample-rate frames lanes]
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
                             input? (assoc :input (real-array precision (or input [])))
                             ;; A lane is supplied by name; the artifact flattens
                             ;; the names into its own region.
                             (seq lanes)
                             (assoc :lanes (into {}
                                                 (map (fn [[name values]]
                                                        [name (real-array precision values)]))
                                                 lanes))))
    (vec out)))

(defn run-function-plan
  [artifact plan]
  (mapv (fn [args] (apply dsp/invoke artifact args)) (:vectors plan)))

(defn run-process-plan
  [artifact plan]
  (dsp/reset! artifact)
  (reduce (fn [outputs {:keys [op input frames lanes]}]
            (case op
              :reset (do (dsp/reset! artifact) outputs)
              :process (conj outputs
                             (run-process-once artifact input
                                               (:controls plan)
                                               (:sample-rate plan)
                                               frames
                                               lanes))))
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
                              (update :input #(mapv encode-value %))
                              (:lanes invocation)
                              (update :lanes
                                      (fn [lanes]
                                        (into {} (map (fn [[lane values]]
                                                        [(name lane)
                                                         (mapv encode-value values)]))
                                              lanes)))))
                          invocations)))))))
