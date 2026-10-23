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
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.emission :as emission]
            [omkamra.dsp.numbers :as numbers]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.source-cases :as source-cases]
            [omkamra.dsp.ir :as ir]
            [omkamra.dsp.library-registry :as library]
            [omkamra.dsp.linker :as linker]
            [omkamra.dsp.source :as source]))

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
       ;; The process's channel count: the shape of both audio ports, and the one
       ;; thing a runner needs to allocate them. It is a property of the
       ;; definition, so it belongs in the target-neutral plan.
       :channels (long (or (get-in artifact [:abi :channels :count]) 1))
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

(defn- per-channel
  "One value per channel: a flat buffer when the process is mono, one buffer per
  channel when it is not — which is the shape both audio ports have, and every
  adapter checks it as a primitive array of primitive arrays."
  [precision channels values]
  (if (= 1 (long channels))
    (real-array precision values)
    (into-array (map (fn [_] (real-array precision values)) (range channels)))))

(defn- run-process-once
  [artifact input controls sample-rate frames lanes]
  (let [precision (:precision (:ir artifact))
        channels (long (or (get-in artifact [:abi :channels :count]) 1))
        out (per-channel precision channels (repeat frames 0.0))
        ;; Whether an input is passed is a property of the process, not of the
        ;; block: a 0-frame invocation still has an (empty) input buffer, while a
        ;; generator has none at any length.
        input? (boolean (seq (get-in artifact [:abi :inputs])))]
    (dsp/process! artifact (cond-> {:output out
                                    :frames frames
                                    :sample-rate sample-rate
                                    :controls controls}
                             input? (assoc :input (per-channel precision channels (or input [])))
                             ;; A lane is supplied by name; the artifact flattens
                             ;; the names into its own region.
                             (seq lanes)
                             (assoc :lanes (into {}
                                                 (map (fn [[name values]]
                                                        [name (real-array precision values)]))
                                                 lanes))))
    (if (= 1 channels) (vec out) (mapv vec out))))

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
  ;; No caller on this side passes `:wasm`: the JVM-side runner compares the
  ;; interpreter with the JVM, and the browser half compares the Wasm *values* in
  ;; `omkamra.dsp.harness.testbed`, with its own copy of this rule. The method stays
  ;; because `results-match?` is target-generic, but the live copy is the page's —
  ;; so a change to what the Wasm tolerance forgives belongs there, and changing
  ;; only this one would change nothing at all.
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
;; Case rows: what each target did with each case, as data
;; ---------------------------------------------------------------------------

(def target-row-keys
  "The row key each target's verdict is reported under.

  One vocabulary for every half: the browser reports `:javascript` and `:wasm`,
  this namespace reports `:interpreter` and `:jvm`, and both are read here."
  {:interpreter :interpreter
   :jvm :jvm
   :js :javascript
   :wasm :wasm})

(defn target-ran?
  "True when a row says `target` ran the case and matched the oracle."
  [row target]
  (true? (get row (get target-row-keys target))))

(defn coverage
  "What a set of case rows proves, per case and precision:
  `{case-id {precision #{targets}}}`.

  The set holds the targets that ran the case *and* matched the oracle, so a
  target that mismatched is absent from it. Comparing two of these — before and
  after a change, or one half of the suite against the other — then shows exactly
  what moved."
  [rows]
  (reduce (fn [coverage row]
            (let [targets (into #{}
                                (keep #(when (target-ran? row %) %))
                                (keys target-row-keys))]
              (update-in coverage [(:case row) (:precision row)] (fnil into #{}) targets)))
          {}
          rows))

(defn parity
  "The declared targets a case row does not reach: `{:cases n :gaps [...]}`.

  Each row declares the targets it means to cover in `:targets`, and `parity`
  reports every declared target it cannot see in that row's verdicts, carrying the
  row's `:check` when it has one. A `:passed?` cannot answer this: a case that
  never ran and a case that passed are equally silent, so parity is only ever
  proved by reading which targets a case actually reached."
  [rows]
  (let [gaps (into []
                   (for [row rows
                         target (:targets row)
                         :when (not (target-ran? row target))]
                     (assoc (select-keys row [:check :case :precision]) :target target)))]
    {:cases (count (distinct (map (juxt :case :precision) rows)))
     :gaps gaps}))

(defn case-rows
  "Every catalog case's verdict on the interpreter and the JVM, as data.

  The interpreter is the oracle and the JVM backend is compared against it — what
  `omkamra.dsp.tests.conformance` asserts one case at a time. Building the same
  rows as data is what lets a parity check read the two JVM-side targets in the
  shape the browser reports `:js` and `:wasm` in, so one table can cover all four."
  ([] (case-rows catalog/all-cases))
  ([cases]
   (into []
         (for [case cases
               precision (:precisions case)
               :let [plan (plan case precision)
                     expected (run-plan (target-artifact case precision :interpreter) plan)
                     actual (run-plan (target-artifact case precision :jvm) plan)
                     matched (results-match? :jvm precision expected actual)]]
           {:case (:id case)
            :precision precision
            :targets [:interpreter :jvm]
            ;; The interpreter is the oracle, so it cannot disagree with itself;
            ;; it is reported because it *ran*, which is the question parity asks.
            :interpreter true
            :jvm matched
            :passed? matched}))))

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

;; ---------------------------------------------------------------------------
;; Compiler equivalence (P13, D79)

(defn- equivalence-entry
  "How a case's entry is lowered: a function case lowers a whole unit, a process
  case lowers a process with its channel count."
  [case definition]
  (if (catalog/function-case? case)
    {:entry :function :channels 1}
    {:entry :process
     :channels (long (or (get-in definition [:process :channels])
                         (:channels case) 1))}))

;; ---------------------------------------------------------------------------
;; Number formatting (P13b)

(def ^:private catalog-constants
  "Every float constant the catalog emits, at both widths."
  (for [case catalog/cases
        :when (and (nil? (:definition-var case)) (:definition case))
        precision [:f32 :f64]
        :let [artifact (dsp/compile (:definition case)
                                    (cond-> {:target :js :precision precision}
                                      (not (catalog/function-case? case))
                                      (assoc :entry :process)))]
        expression (tree-seq coll? seq (:ir artifact))
        :when (and (map? expression) (= :const (:op expression))
                   (= :float (:type expression))
                   (number? (:value expression)))]
    (double (:value expression))))

(defn- number-corpus
  "The doubles and floats the formatter is checked on, the same ones every run so
  that a difference is a difference in the code and not in the sample.

  Deliberately not only the catalog's constants: those are small round numbers,
  and every divergence the sweep found is outside them — the shortest spelling of
  the smallest subnormals, an exact decimal tie, an f32 that rounds to the largest
  one, a number printed beyond 2^53."
  []
  (let [random (java.util.Random. 20251022)]
    {:f64 (vec (distinct (concat catalog-constants
                                 ;; the smallest subnormals, one by one
                                 (map (fn [bits] (Double/longBitsToDouble bits))
                                      (range 0 4097))
                                 (map (fn [n] (Math/pow 10.0 n)) (range -320 309))
                                 [0.0 -0.0 1.0 -1.0 (+ 0.1 0.2)
                                  Double/MIN_VALUE Double/MAX_VALUE
                                  Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY
                                  Double/NaN]
                                 (repeatedly 6000 (fn [] (Double/longBitsToDouble
                                                          (.nextLong random)))))))
     :f32 (vec (distinct (concat [(float 0.1) (float 0.5) (float -0.16666667)
                                  (float 0.0) (float -0.0)
                                  Float/MAX_VALUE Float/MIN_VALUE Float/MIN_NORMAL
                                  Float/POSITIVE_INFINITY Float/NEGATIVE_INFINITY
                                  Float/NaN]
                                 (repeatedly 8000 (fn [] (Float/intBitsToFloat
                                                          (.nextInt random)))))))}))

(defn number-corpus-payload
  "The JVM's spelling of a wide sample of floats, for the browser to match.

  The emitters turn constants into text, and the two worlds do that from
  different primitives: Clojure has a float type and ClojureScript has one number
  type made of doubles, and neither JDK method nor JavaScript conversion is the
  other. A catalog comparison would not see that — its constants are small round
  numbers — so the sample is swept and the value travels as its bit pattern,
  which is the only way two worlds can name the same double (D78).

  Each entry is `[bits spelling]`, and the bits are hexadecimal text: 64 of them
  do not fit a JavaScript number, so an integer would arrive rounded and the
  check would be checking a different double."
  []
  (let [{:keys [f64 f32]} (number-corpus)]
    {:f64 (mapv (fn [value] [(format "0x%016x" (Double/doubleToRawLongBits (double value)))
                             (numbers/f64->text value)])
                f64)
     :f32 (mapv (fn [value] [(format "0x%08x" (bit-and 0xffffffff
                                                       (long (Float/floatToRawIntBits value))))
                             (numbers/f32->text value)])
                f32)}))

(defn equivalence-payload
  "The JVM's half of the compiler equivalence check, as data.

  One entry per catalog definition the browser can compile on its own: the raw
  definition, how to lower it, and the JVM's own normalization, linked unit and
  portable IR for it. The browser compiles the same definition with the compiler
  compiled into the page and compares (`omkamra.dsp.harness.diff`), which is what
  makes the phase's equivalence claim a test rather than a claim.

  A case whose unit pulls in a definition from outside the case's own data is
  marked `:library?`. The JVM links it — it resolves a Var through `find-ns` —
  and the browser cannot, because it has not been given those definitions: they
  are what P13d's browser definition registry will supply. The expected values
  are shipped for those cases too, so that closing the gap needs no change here;
  the page decides whether it *can* compile them, and says which it skipped
  rather than failing on something it was never told how to do.

  A definition the JVM itself cannot link is reported separately as
  `:uncompilable`, which is a JVM-side surprise and not the browser's business.

  EDN, not JSON: this is data to read back, boxes and all (D78), and the
  comparison is of values rather than of JSON-encoded strings."
  []
  (let [cases (filter #(and (nil? (:definition-var %)) (:definition %))
                      catalog/cases)
        compiled
        (mapv (fn [case]
                (let [definition (:definition case)
                      {:keys [entry channels]} (equivalence-entry case definition)]
                  (try
                    (let [unit (linker/link definition)
                          ir-value (if (= entry :function)
                                     (ir/lower-unit unit)
                                     (ir/lower-process unit {:channels channels}))]
                      [(name (:id case))
                       {:definition definition
                        :entry entry
                        :channels channels
                        ;; More than the entry means the linker reached a
                        ;; definition the case's own data does not carry.
                        :library? (> (count (:definitions unit)) 1)
                        :normalized (descriptor/normalize definition)
                        :unit unit
                        :ir ir-value
                        ;; The one serialization both worlds can compare (D77):
                        ;; order-stable and explicit about numbers. Shipping it
                        ;; alongside the IR makes the page prove it produces the
                        ;; same text, which is what a cache key rests on.
                        :canonical (ir/canonical-text ir-value)
                        ;; What the two emitters produce for this definition, at
                        ;; each width. The page lowers its own IR from the unit
                        ;; above and emits from that, so what is compared is the
                        ;; emitter in both worlds rather than a stored result.
                        :emissions (emission/emissions unit {:entry entry :channels channels})}])
                    (catch clojure.lang.ExceptionInfo error
                      {::skipped (name (:id case))
                       ::message (.getMessage error)}))))
              cases)]
    {:cases (into {} (remove ::skipped compiled))
     :uncompilable (mapv (fn [entry] [(::skipped entry) (::message entry)])
                         (filter ::skipped compiled))}))

;; ---------------------------------------------------------------------------
;; Reference instrument equivalence (P13d)

(defn- compiled-description
  "One definition compiled here: its descriptor, canonical IR and both emitters'
  products, at every width a page will compile it for."
  [definition {:keys [entry channels]}]
  ;; `source/link-definition` rather than `linker/link`: a derived definition
  ;; (a folded post-mix chain) reaches the definitions it was built from through
  ;; `:dsp/dependencies`, which only the source layer carries into the link.
  (let [unit (source/link-definition definition {})
        ir-value (if (= entry :function)
                   (ir/lower-unit unit)
                   (ir/lower-process unit {:channels channels}))]
    {:entry entry
     :channels channels
     :normalized (descriptor/normalize definition)
     :canonical (ir/canonical-text ir-value)
     :emissions (emission/emissions unit {:entry entry :channels channels})}))

(defn compiled-definition-payload
  "The JVM's own compile of each entry, as data for a page to compare against.

  `entries` is a sequence of `{:id ..., :definition ..., :entry ..., :channels
  ..., :definition-id ..., :carry-definition? ...}` and the result is
  `{id description}`. `:definition-id` names the definition by its fully
  qualified id, so a page that has its *own* capture of it can compile that
  instead of the served data — which is how two macro captures are shown to
  agree. When `:carry-definition?` is true the definition itself travels too, for
  a case the page does not have — a folded post-mix chain is a JVM compile step,
  but the folded definition is plain data, so the page can compile the very
  definition this compiled from.

  It names no example: assembling an example's payload is the caller's business,
  so the compiler and its general harness carry no knowledge of any one of them."
  [entries]
  (into {}
        (map (fn [{:keys [id definition entry channels definition-id
                          carry-definition?]}]
               [id (cond-> (compiled-description definition {:entry entry
                                                             :channels channels})
                     definition-id (assoc :definition-id definition-id)
                     carry-definition? (assoc :definition definition))]))
        entries))

;; ---------------------------------------------------------------------------
;; The source a definition was written as (P13d)

(def ^:private definition-heads
  "The forms that declare a DSP definition, so a file's own source can be handed
  to a front end without its prose, its description or its host."
  '#{dsp/defn dsp/def dsp/defstruct})

(defn- read-portable-forms
  "Every top-level form of a `.cljc` resource, read for *no* world.

  Reader conditionals are resolved for the empty feature set, so a form that
  exists only under `:clj` or `:cljs` — an `ns` clause, a JVM-only constructor —
  is left out rather than half-read. What remains is the portable half."
  [resource]
  (with-open [reader (io/reader (io/resource resource))]
    (let [pushback (java.io.PushbackReader. reader)]
      (loop [forms []]
        (let [form (read {:read-cond :allow :features #{} :eof ::eof} pushback)]
          (if (= ::eof form)
            forms
            (recur (conj forms form))))))))

(defn portable-definition-source
  "The `dsp/defn`/`dsp/def` forms of a `.cljc` resource, as a source string.

  `resource` is a classpath path, `namespace` the namespace the forms are read
  into, and `requires` the `:require` clauses naming what the definitions call.
  This is how a front end is handed the source a definition was *written as* — the
  same text the static macros captured — rather than a copy that could drift; a
  check that compiles it both ways is then a check on the source, not on a
  transcription."
  [resource namespace requires]
  (let [forms (filter (fn [form]
                        (and (seq? form) (contains? definition-heads (first form))))
                      (read-portable-forms resource))]
    (str "(ns " namespace
         "\n  (:require " (str/join " " (map pr-str requires)) "))\n\n"
         (str/join "\n\n" (map pr-str forms))
         "\n")))

;; ---------------------------------------------------------------------------
;; Source equivalence (P13d)

(defn source-equivalence-payload
  "The JVM's half of the source equivalence check (P13d).

  Each case is DSP source text. This evaluates it with the JVM's own reader and
  macros — `:evaluator :native` — links it, and records the descriptor, the
  canonical IR and what both emitters make of it. The page evaluates the same
  text with SCI and compares, which is what pins the SCI macro's capture to the
  real macro's: identical descriptors, identical IR, identical artifacts.

  The source is evaluated exactly once here, at the entry precision; the
  emissions are per precision because a width is part of what an emitter reads.

  The two evaluators must agree *here*, where both have a reader that keeps a
  literal's type. A browser's does not (D78), so the reference check that needs
  it runs on this side; see `portable-definition-source`."
  []
  (let [cases source-cases/cases]
    {:cases
     (into {}
           (map (fn [{:keys [id source definition entry channels precisions]}]
                  (let [{evaluated :definition definitions :definitions}
                        (source/evaluate source {:definition definition
                                                 :evaluator :native
                                                 :library library/library-definitions})
                        unit (source/link-definition evaluated {:definitions definitions})
                        ir-value (if (= entry :function)
                                   (ir/lower-unit unit)
                                   (ir/lower-process unit {:channels channels}))]
                    [id
                     {:source source
                      :definition definition
                      :entry entry
                      :channels channels
                      :precisions precisions
                      :normalized (descriptor/normalize evaluated)
                      :canonical (ir/canonical-text ir-value)
                      :emissions (emission/emissions unit {:entry entry
                                                           :channels channels})}])))
           cases)}))
