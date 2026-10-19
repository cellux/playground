(ns omkamra.dsp.modulation
  "The modulation matrix as data, and the scheduling pass both generators share
  (M3).

  A matrix is a sequence of routes; a route says a target is its base plus a
  depth-scaled source:

      {:from :velocity :to :cutoff :depth 1800.0 :delay 0}

  `:from` and `:to` name parameters (or whatever names the generator validates
  against); `:depth` is a number or another name (second-order modulation);
  `:delay` is the number of evaluation steps the source is read from earlier
  (`0`, the default, is instantaneous).

  `matrix-plan` resolves the one thing that is not per-step: the **scheduling**.
  A route with `:delay 0` reads its source's value for the current step, so when
  the source is itself a target the two form an edge; the plan topologically
  orders the `:delay 0` subgraph once, at generation time, and refuses a cycle
  that has no delayed edge — the only way a cycle is well defined. A `:delay n>0`
  route is not an edge, because its source comes from `n` steps ago.

  The plan is generator-independent: the instrument reducer walks it over host
  arrays, the kernel reducer emits it as straight-line DSP. Both share the one
  route schema and the one ordering."
  (:require [omkamra.dsp.graph :as graph]
            [omkamra.dsp.synth.pool :as pool]))

(defn fail
  [message data]
  (throw (ex-info message (assoc data :dsp/error :invalid-matrix))))

(defn name*
  "A route name's canonical form: a keyword, namespaced if the spelling was."
  [x]
  (cond
    (keyword? x) x
    (symbol? x) (if-let [ns (namespace x)]
                  (keyword ns (clojure.core/name x))
                  (keyword (clojure.core/name x)))
    (string? x) (keyword x)
    :else x))

(defn normalize-matrix
  "Expand `matrix` into a flat vector of canonical routes.

  The grouped form `{:source S :routes [{:target T :depth D :delay N} …]}` — what
  the reference instrument uses — becomes one route per inner entry; a flat entry
  is taken as itself. `:from`/`:source` and `:to`/`:target` are aliases, and
  `:delay` defaults to 0. Names are canonicalised to keywords."
  [matrix]
  (when-not (sequential? matrix)
    (fail "a modulation matrix must be a sequence of routes" {:matrix matrix}))
  (into []
        (mapcat
         (fn [entry]
           (when-not (map? entry)
             (fail "a modulation matrix entry must be a map" {:entry entry}))
           (let [source (or (:source entry) (:from entry))
                 routes (or (:routes entry) [entry])]
             (mapv (fn [route]
                     (when-not (map? route)
                       (fail "a modulation route must be a map" {:route route}))
                     (cond-> {:from (name* (or (:from route) source))
                              :to (name* (or (:to route) (:target route)))
                              :depth (:depth route)
                              :delay (or (:delay route) 0)}
                       (contains? route :offset) (assoc :offset (:offset route))))
                   routes)))
         matrix)))

(defn validate-matrix
  "Validate `routes` (already flat and canonical) against `known`, the set of
  names a route may reference. Returns the routes.

  An unknown source or target, a depth that is neither a number nor a known name,
  or a delay that is not a non-negative integer is a loud error."
  [routes known]
  (let [known (set (map name* known))]
    (doseq [route routes]
      (let [from (name* (:from route))
            to (name* (:to route))]
        (when-not (contains? known from)
          (fail "a modulation route names a source that is not declared"
                {:route route :source from :known (vec (sort known))}))
        (when-not (contains? known to)
          (fail "a modulation route names a target that is not declared"
                {:route route :target to :known (vec (sort known))}))
        (let [depth (:depth route)]
          (when-not (or (number? depth) (contains? known (name* depth)))
            (fail "a modulation route's depth must be a number or a declared name"
                  {:route route :depth depth})))
        (when-not (or (nil? (:offset route)) (number? (:offset route)))
          (fail "a modulation route's offset must be a number"
                {:route route :offset (:offset route)}))
        (let [delay (:delay route)]
          (when-not (and (integer? delay) (not (neg? delay)))
            (fail "a modulation route's delay must be a non-negative integer"
                  {:route route :delay delay})))))
    routes))

(defn matrix-plan
  "Schedule `routes` once: the incoming routes per target, the topological order
  of the `:delay 0` subgraph, and the set of sources.

  The order is resolved at generation time and is deterministic (targets are
  ordered by name), so a generator never sorts per step. A cycle with no delayed
  edge is refused, naming the targets it spans."
  [routes]
  (let [routes (mapv (fn [route]
                       (assoc route
                              :from (name* (:from route))
                              :to (name* (:to route))
                              :delay (long (:delay route))))
                     routes)
        targets (vec (distinct (map :to routes)))
        target-set (set targets)
        incoming (group-by :to routes)
        dependencies (reduce (fn [acc route]
                               (if (and (zero? (long (:delay route)))
                                        (contains? target-set (:from route)))
                                 (update acc (:to route) (fnil conj #{}) (:from route))
                                 acc))
                             {}
                             routes)]
    (loop [order []
           done #{}
           remaining (set targets)]
      (cond
        (empty? remaining)
        {:targets (into {} (map (fn [target] [target (get incoming target)]) targets))
         :order order
         :sources (into #{} (map :from) routes)}

        :else
        (let [ready (sort (filter (fn [target]
                                    (every? done (get dependencies target #{})))
                                  remaining))]
          (if (empty? ready)
            (fail "the modulation matrix has a cycle with no delayed edge"
                  {:cycle (vec (sort remaining))})
            (recur (into order ready)
                   (into done ready)
                   (reduce disj remaining ready))))))))

(defn- parameter-scopes
  "Normalise a parameter declaration map to `name -> :per-voice | :shared`.
  A name the map does not mention is `:per-voice` (the runtime's own sources)."
  [parameters]
  (into {}
        (map (fn [[name declaration]]
               [(name* name)
                (cond
                  (keyword? declaration) declaration
                  (map? declaration) (get declaration :scope :per-voice)
                  :else :per-voice)]))
        parameters))

(defn- validate-routes
  "Validate `matrix` against `parameters` and schedule it — the one pass both
  generators share. Returns the scopes, the flat routes, and the plan.

  A `:shared` target may not be modulated by a `:per-voice` source: the shared
  value is one cell, and a per-voice source has no single value to give it."
  [matrix parameters]
  (let [scopes (parameter-scopes parameters)
        routes (normalize-matrix matrix)]
    (validate-matrix routes (keys scopes))
    (doseq [route routes]
      (when (and (= :shared (get scopes (:to route)))
                 (= :per-voice (get scopes (name* (:from route)))))
        (fail "a shared target cannot be modulated by a per-voice source"
              {:route route})))
    {:scopes scopes :routes routes :plan (matrix-plan routes)}))

;; ---------------------------------------------------------------------------
;; The instrument reducer generator (M3b)
;; ---------------------------------------------------------------------------
;;
;; The matrix is a generator input; the generator returns a *reducer* that closes
;; over it. The instrument state is the instrument's parameter values, as data:
;;
;;     {:shared {name value, ...}          ; the :shared parameters
;;      :voices [{name value, ...}, ...]}  ; the :per-voice parameters, one map
;;                                         ; per voice
;;
;; The reducer reads that state and returns a state whose targets are the base
;; plus their depth-scaled sources, computed in the plan's topological order. It
;; is immutable data in and out, so the host keeps the base and calls the reducer
;; each control step without the modulated values compounding; a separate publish
;; step maps the effective values to the pool's controls.

(defn- read-value
  [state scope name slot]
  (if (= :shared scope)
    (get-in state [:shared name])
    (get-in state [:voices (long slot) name])))

(defn- write-value
  [state scope name slot value]
  (if (= :shared scope)
    (assoc-in state [:shared name] value)
    (assoc-in state [:voices (long slot) name] value)))

(defn- route-delta
  "The depth-scaled source of one route, read from the in-progress `effective`
  state or, for a `:delay`, from `previous` (the earlier effective states, newest
  first)."
  [route scopes effective previous slot]
  (let [source (name* (:from route))
        source-scope (get scopes source)
        source-slot (when (= :per-voice source-scope) slot)
        source-value
        (if (zero? (long (:delay route)))
          (read-value effective source-scope source source-slot)
          (read-value (nth previous (dec (long (:delay route))))
                      source-scope source source-slot))
        ;; An offset biases the *source* before the depth scales it, which is how
        ;; a route expresses "octaves from middle C" or any other zero point.
        source-value (+ (double source-value) (double (or (:offset route) 0.0)))
        depth (:depth route)]
    (if (number? depth)
      (* (double depth) (double source-value))
      (let [depth-name (name* depth)
            depth-scope (get scopes depth-name)]
        (* (double (read-value effective depth-scope depth-name
                               (when (= :per-voice depth-scope) slot)))
           (double source-value))))))

(defn instrument-reducer
  "Return a reducer `(fn [state] state')` that modulates an instrument's
  parameter values according to `matrix`.

  `parameters` maps every name the matrix may reference to its declaration or its
  scope (`:per-voice`, the default, or `:shared`). The matrix is validated and
  scheduled once, here; the returned reducer closes over the plan and never sees
  the matrix.

  `state` is `{:shared {name value} :voices [{name value} ...]}` and the result
  has the same shape, each target in the plan's order set to
  `base + Σ depth × source`. Sources are read from the in-progress result, so an
  instantaneous chain is complete; a `:delay n` route reads its source from `n`
  steps earlier (the reducer keeps the earlier results). A `:shared` target may
  not be modulated by a `:per-voice` source."
  [matrix parameters]
  (let [{:keys [scopes routes plan]} (validate-routes matrix parameters)
        order (:order plan)
        max-delay (long (reduce max 0 (map (comp long :delay) routes)))
        history (atom nil)]
    (fn [state]
      (let [previous (or @history
                         (if (pos? max-delay) (vec (repeat max-delay state)) []))
            voice-count (count (:voices state))
            effective
            (reduce
             (fn [effective target]
               (let [target-scope (get scopes target)
                     slots (if (= :shared target-scope) [nil] (range voice-count))
                     target-routes (get-in plan [:targets target])]
                 (reduce
                  (fn [effective slot]
                    (let [base (double (read-value effective target-scope target slot))
                          delta (reduce (fn [sum route]
                                          (+ sum (route-delta route scopes effective
                                                              previous slot)))
                                        0.0
                                        target-routes)]
                      (write-value effective target-scope target slot (+ base delta))))
                  effective
                  slots)))
             state
             order)]
        (when (pos? max-delay)
          (reset! history (vec (take max-delay (cons effective previous)))))
        effective))))

;; ---------------------------------------------------------------------------
;; The publish step (M3b')
;; ---------------------------------------------------------------------------

(defn- control-positions
  "A map from a control's canonical name to its ABI position in `pool`."
  [pool]
  (let [plan (vec (get (pool/pool-config pool) :control-plan))]
    (into {}
          (map-indexed (fn [position entry] [(name* (:control entry)) (long position)]))
          plan)))

(defn- matrix-names
  "Every name the routes reference: sources, targets, and symbolic depths."
  [routes]
  (into #{}
        (concat (map :from routes)
                (map :to routes)
                (keep (fn [route] (when-not (number? (:depth route))
                                    (name* (:depth route))))
                      routes))))

(defn- position! [positions name]
  (or (get positions name)
      (fail "a modulation name is not a control of the voice"
            {:name name :controls (vec (sort (keys positions)))})))

(defn- read-at
  "Read `index` from a real-valued array. The JVM hint keeps `aget` primitive:
  an unhinted `aget` compiles to a reflective `RT.aget`, about 24 KB a call."
  [values index]
  #?(:clj (aget ^doubles values (int index))
     :cljs (aget values index)))

(defn- write-at
  "Write `value` at `index` in a real-valued array. Hinted like `read-at`."
  [values index value]
  #?(:clj (aset ^doubles values (int index) (double value))
     :cljs (aset values index value)))

(defn- fetch
  "The cached `build` result for `pool`, computed once."
  [cache pool build]
  (or (get @cache pool)
      (let [computed (build pool)]
        (swap! cache assoc pool computed)
        computed)))

(defn instrument-state
  "Return `(fn [pool] state)` reading the pool's current values for every name
  `matrix` references, producing the state `instrument-reducer` consumes:
  `{:shared {name value} :voices [{name value} ...]}`.

  A `:shared` name is read from the pool's shared parameter store; a `:per-voice`
  name from the voice's resolved controls. The matrix, the names, and each pool's
  control positions are resolved once, here, not per control step."
  [matrix parameters]
  (let [scopes (parameter-scopes parameters)
        names (matrix-names (normalize-matrix matrix))
        per-voice (vec (filter #(= :per-voice (get scopes %)) names))
        shared (vec (filter #(= :shared (get scopes %)) names))
        cache (atom {})]
    (fn [pool]
      (let [{:keys [positions values]}
            (fetch cache pool
                   (fn [pool]
                     {:positions (control-positions pool)
                      :values (pool/make-doubles (pool/control-count pool))}))]
        {:shared (into {} (for [name shared]
                            [name (pool/parameter-value pool name)]))
         :voices (mapv (fn [voice]
                         (pool/read-controls! pool voice values)
                         (into {} (for [name per-voice]
                                    [name (read-at values (position! positions name))])))
                       (range (pool/pool-capacity pool)))}))))

(defn publisher
  "Return `(fn [pool state] ...)` that publishes `state`'s effective values for
  the matrix's targets back to `pool`'s voices.

  Each voice's base controls are read first, so a control that is not a target
  keeps its own value; the target's control is replaced by the reduced value."
  [matrix parameters]
  (let [scopes (parameter-scopes parameters)
        routes (normalize-matrix matrix)
        plan (matrix-plan routes)
        targets (into {} (for [name (:order plan)] [name (get scopes name)]))
        cache (atom {})]
    (fn [pool state]
      (let [{:keys [positions values]}
            (fetch cache pool
                   (fn [pool]
                     {:positions (control-positions pool)
                      :values (pool/make-doubles (pool/control-count pool))}))]
        (dotimes [voice (pool/pool-capacity pool)]
          (pool/read-controls! pool voice values)
          (doseq [[name scope] targets]
            (write-at values (position! positions name)
                      (if (= :shared scope)
                        (get-in state [:shared name])
                        (get-in state [:voices voice name]))))
          (pool/set-voice-controls! pool voice values))
        nil))))

;; ---------------------------------------------------------------------------
;; The kernel reducer generator (M3c)
;; ---------------------------------------------------------------------------
;;
;; The same plan, emitted as DSP instead of walked over host arrays. The
;; generator returns `(fn [definition] derived-definition)`: the derived
;; definition exposes the voice's controls plus one for every source the voice
;; lacks, computes each target into a fresh `-mod` local in the plan's order,
;; and calls the voice with the target arguments replaced — the language
;; forbids a local shadowing a control, so the substitution happens at the
;; call, not in the body. A `:delay n` route becomes a chain of scalar state
;; slots shifted once per frame (D22), so the derived kernel carries the
;; modulation with no host step at all.

(defn- control-symbol
  "A matrix name as the symbol a DSP definition spells it with."
  [x]
  (graph/normalize-control-name x))

(defn- control-key
  "A matrix name's canonical keyword, for comparing names across spellings."
  [x]
  (keyword (clojure.core/name (graph/normalize-control-name x))))

(defn- definition-parameters
  "A definition's parameter names, as symbols."
  [definition]
  (mapv graph/normalize-control-name (graph/parameter-symbols definition)))

(defn- control-defaults
  "A definition's control defaults, keyed by canonical name. Used to seed a
  delay slot, so the first frames read the source's own value."
  [definition]
  (into {}
        (keep (fn [entry]
                (let [entry (if (map? entry) entry {:name entry})]
                  (when (contains? entry :default)
                    [(control-key (:name entry)) (:default entry)]))))
        (get-in definition [:process :controls])))

(defn- add-controls
  "Append a control for each of `names` to a definition, in its parameters and
  its process controls, materializing the control list when the definition used
  parameter shorthand."
  [definition names]
  (if (empty? names)
    definition
    (let [process (:process definition)
          input? (not (false? (:input process)))
          controls (or (:controls process)
                       (mapv control-symbol
                             (if input? (rest (:params definition))
                                 (:params definition))))
          extra (mapv (fn [name] {:name name}) names)]
      (-> definition
          (update :params into extra)
          (assoc-in [:process :controls] (into (vec controls) extra))))))

(defn- delayed-sources
  "The greatest delay each source is read at, over the routes that read late."
  [routes]
  (reduce (fn [acc route]
            (let [delay (long (:delay route))]
              (if (pos? delay)
                (update acc (:from route) (fnil max 0) delay)
                acc)))
          {}
          routes))

(defn- delay-state-name
  "The state slot holding `source` `level` frames ago."
  [source level]
  (symbol (str (clojure.core/name source) "-z" level)))

(defn- kernel-source-form
  "The form a route reads as its source: a target's `-mod` local, a delay slot,
  or the control itself."
  [route locals]
  (if (pos? (long (:delay route)))
    (delay-state-name (:from route) (long (:delay route)))
    (get locals (:from route) (control-symbol (:from route)))))

(defn- kernel-route-form
  "One route's contribution: `depth x (source + offset)`."
  [route locals]
  (let [source (kernel-source-form route locals)
        source (if (contains? route :offset)
                 (list '+ source (double (:offset route)))
                 source)
        depth (:depth route)]
    (if (number? depth)
      (list '* (double depth) source)
      (list '* (control-symbol depth) source))))

(defn- kernel-target-form
  "A target's modulated value: its base control plus its routes' contributions.

  Folded pairwise, because the language's `+` is binary, not variadic."
  [target routes locals]
  (reduce (fn [form route]
            (list '+ form (kernel-route-form route locals)))
          (control-symbol target)
          routes))

(defn- delay-declarations
  "One scalar state slot per delay level, seeded with the source's default."
  [delays defaults]
  (vec (for [[source max-delay] (sort-by (comp str key) delays)
             level (range 1 (inc (long max-delay)))]
         {:name (delay-state-name source level)
          :type :float
          :init (double (get defaults (control-key source) 0.0))})))

(defn- delay-shifts
  "The per-frame shift register writes: the highest slot takes its predecessor
  and the first takes the source's effective value, so slot `n` holds the value
  `n` frames ago. Descending, so no write clobbers a value still to be read."
  [delays locals]
  (vec (mapcat
        (fn [[source max-delay]]
          (concat
           (for [level (range (long max-delay) 1 -1)]
             (list 'set! (delay-state-name source level)
                   (delay-state-name source (dec level))))
           [(list 'set! (delay-state-name source 1)
                  (get locals source (control-symbol source)))]))
        (sort-by (comp str key) delays))))

(defn kernel-reducer
  "Return a generator `(fn [definition] derived-definition)` that modulates a
  voice's controls according to `matrix`.

  `parameters` names every control the matrix may reference, exactly as
  `instrument-reducer` takes it, so the two generators share one name universe
  and one validation pass.

  The derived definition exposes the voice's controls plus one for every
  *source* it does not already take; computes each target into a fresh `-mod`
  local in the plan's order, reading an earlier target's local so a chain is
  complete; and calls the voice with the target arguments replaced by those
  locals. A *target* must already be a control of the voice — a name the voice
  never reads cannot be modulated, and a typo must not silently add a dead
  control (the instrument generator requires the same).

  A `:delay n` route reads its source from a chain of scalar state slots
  shifted once per frame (D22), which is what makes a feedback loop well
  defined in straight-line DSP."
  [matrix parameters]
  (let [{:keys [routes plan]} (validate-routes matrix parameters)
        order (:order plan)
        targets (set order)
        reads (matrix-names routes)
        delays (delayed-sources routes)]
    (fn [definition]
      (when-not (and (map? definition) (= :function (:dsp/kind definition)))
        (fail "the kernel reducer needs a DSP definition value"
              {:definition definition}))
      (let [params (definition-parameters definition)
            present (set (map control-key params))
            missing (vec (remove present order))
            _ (when (seq missing)
                (fail "a modulation target must be a control of the voice"
                      {:targets (mapv control-key missing)
                       :controls (mapv control-key params)}))
            added (vec (sort-by str
                                (map control-symbol
                                     (remove present (remove targets reads)))))
            locals (into {}
                         (map (fn [target]
                                [target (symbol (str (clojure.core/name target)
                                                     "-mod"))]))
                         order)
            pairs (loop [remaining order
                         seen {}
                         result []]
                    (if-let [target (first remaining)]
                      (recur (rest remaining)
                             (assoc seen target (get locals target))
                             (conj result
                                   (get locals target)
                                   (kernel-target-form
                                    target (get-in plan [:targets target]) seen)))
                      result))
            call (graph/definition-call
                   definition
                   (mapv (fn [param] (get locals (control-key param) param))
                         params))
            shifts (delay-shifts delays locals)
            body (let [call (if (seq shifts)
                              (apply list 'do (concat shifts [call]))
                              call)]
                   (if (seq pairs)
                     (list 'let pairs call)
                     call))
            ;; Derive from the *original* value, not the extended one: the
            ;; derived value records its callee in `:dsp/dependencies`, and the
            ;; call passes the voice's own parameters, so the callee the linker
            ;; resolves must be the original interface, not the wider one.
            extended (add-controls definition added)
            derived (graph/derived-definition
                     definition
                     body
                     {:params (:params extended)
                      :controls (get-in extended [:process :controls])})]
        (-> derived
            (assoc :state (delay-declarations delays (control-defaults definition)))
            (assoc :doc (str "modulation of " (graph/definition-id definition))))))))
