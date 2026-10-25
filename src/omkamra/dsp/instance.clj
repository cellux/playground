(ns omkamra.dsp.instance
  "The target-neutral lifecycle of a compiled process artifact (P3, P13d, D89).

  A compiled artifact is an immutable program shared by every caller; an
  *instance* is a mutable handle that owns its own persistent state and scratch.
  This namespace is that vocabulary — `process-instance?`, `create-instance`,
  `destroy!`, `invoke`, `process!`, `initialize!`, `reset!` — and it names no
  target: it drives the `:create-instance`/`:invoke`/`:invoke-context`/
  `:initialize`/`:reset`/`:destroy` the artifact itself carries, so the
  interpreter and the JVM run through it while a browser's own kernel does the
  same job its own way.

  It is a namespace of its own rather than part of the `omkamra.dsp` facade for
  one reason: the JVM host drives an instance in its own body, and it should not
  have to load the macro namespace — the definition forms, the SCI front end, the
  library registry — to do so. `omkamra.dsp` re-exports that vocabulary, so a
  caller that reaches the lifecycle through the facade still names one thing;
  `check-live!` is the shared holder check, public only so the JVM half can use
  it.

  The JVM target's two raw ABIs (`process-checked!`, `process-unchecked!`) are
  deliberately **not** here: they name a target, so they live in
  `omkamra.dsp.instance.jvm`, and this namespace stays free of one."
  (:refer-clojure :exclude [reset!]))

(defn check-live!
  "Throw when a destroyed instance is used; otherwise return `holder`.

  A plain artifact has no lifecycle to check, so it passes through. This is
  public for the JVM half of the lifecycle, which validates a holder before the
  raw ABI."
  [holder]
  (when (and (:live? holder) (not (deref (:live? holder))))
    (throw (ex-info "this DSP process instance has been destroyed"
                    {:target (:target holder)})))
  holder)

(defn process-instance?
  "True when `value` is an independent instance from `create-instance`."
  [value]
  (boolean (:dsp/instance value)))

(defn create-instance
  "Create an independent process instance from a compiled process artifact.

  The artifact is an immutable program shared by every instance; an instance
  owns its persistent state and its scratch arena. `:interpreter` and `:jvm`
  artifacts reuse one compiled code object and allocate a fresh state region
  per instance, so instantiating never recompiles. Call `destroy!` when the
  instance is no longer needed; the artifact remains usable either way."
  [artifact]
  (when-not (= :process (:entry artifact))
    (throw (ex-info "only process artifacts can be instantiated"
                    {:entry (:entry artifact)})))
  (if-let [factory (:create-instance artifact)]
    (assoc (factory)
           :dsp/instance true
           :artifact artifact
           :target (:target artifact)
           :live? (atom true))
    (throw (ex-info "this DSP target does not support explicit process instances"
                    {:target (:target artifact)}))))

(defn destroy!
  "Release the persistent state owned by a process instance.

  The instance is unusable afterwards and further calls through it throw. A
  plain artifact owns its state for the artifact's lifetime and cannot be
  destroyed."
  [holder]
  (when-not (process-instance? holder)
    (throw (ex-info "destroy! expects an instance created by create-instance"
                    {:target (:target holder)})))
  (when (deref (:live? holder))
    (when-let [destroy (:destroy holder)]
      (destroy))
    (clojure.core/reset! (:live? holder) false))
  nil)

(defn invoke
  "Invoke a compiled artifact or instance through its target-specific adapter."
  [holder & args]
  (check-live! holder)
  (apply (:invoke holder) args))

(defn- real-region?
  "True for a primitive float/double array: a region the host built itself.

  A real-time host writes its regions in place, so the context may carry the
  region *itself* rather than the named values this adapter would flatten into
  one. Both are accepted; the difference is who owns the copy."
  [value]
  (or (instance? (class (double-array 0)) value)
      (instance? (class (float-array 0)) value)))

(defn- lane-region
  "Flatten a `{:lane-name values}` context map into the ABI's single lane region.

  The kernel sees one flat region with compile-time offsets; the host supplies
  one named sequence per lane. This is the whole of the impedance mismatch, and
  it costs one copy per call — a real-time host writes the region directly and
  skips this path, passing the region it already owns as `:lanes`."
  [artifact context]
  (let [abi (:abi artifact)
        lanes (:lanes abi)]
    (when (and (some :required? lanes) (nil? (:lanes context)))
      ;; A process with a mandatory (`:lane`) lane has no valid block without
      ;; its region, so say that here rather than letting a backend report an
      ;; unknown buffer. A `:has-lane` lane is optional: an absent region is
      ;; built zeroed, so it reads as "no automation" and the scalar stands.
      (throw (ex-info "this DSP process declares per-frame lanes"
                      {:lanes (mapv :name lanes)})))
    (cond
      (empty? lanes) context
      ;; The region is already built: the host owns it and wrote it itself.
      (real-region? (:lanes context)) context
      :else
      (let [stride (long (:lane-stride abi))
            supplied (:lanes context)
            f64? (= :f64 (get-in abi [:precision]))
            region (if f64?
                     (double-array (* stride (count lanes)))
                     (float-array (* stride (count lanes))))]
        (doseq [{:keys [name offset required?]} lanes]
          ;; The ABI names a lane with a symbol, as it does a control, but accept
          ;; the keyword spelling too: a key that silently missed would publish
          ;; zeros, which is the worst possible failure for automation.
          (let [values (or (get supplied name) (get supplied (keyword name)))]
            (when (and (nil? values) required?)
              (throw (ex-info "a DSP lane has no values for this block"
                              {:lane name :supplied (vec (keys supplied))})))
            (let [count (min (count values) stride)]
              (if f64?
                (let [^doubles target region]
                  (dotimes [index count]
                    (aset target (int (+ offset index)) (double (nth values index)))))
                (let [^floats target region]
                  (dotimes [index count]
                    (aset target (int (+ offset index)) (float (nth values index)))))))))
        (assoc context :lanes region)))))

(defn- shared-region
  "Flatten a `{:shared-name value}` context map into the ABI's `:shared` region.

  A shared control is block-rate, so a bare number is broadcast across its
  `:shared-stride` slice and a sequence is written as it comes — which is also
  what a host writing a per-frame *global* curve wants. The region is one for the
  whole pool, not one per voice (M5), so this is the copy that a real-time host
  skips by writing the region directly, passing the region it already owns as
  `:shared`."
  [artifact context]
  (let [abi (:abi artifact)
        shared (:shared abi)]
    (cond
      (empty? shared) context
      ;; The region is already built: the host owns it and wrote it itself.
      (real-region? (:shared context)) context
      :else
      (let [stride (long (:shared-stride abi))
            supplied (:shared context)
            f64? (= :f64 (get-in abi [:precision]))
            region (if f64?
                     (double-array (* stride (count shared)))
                     (float-array (* stride (count shared))))
            write! (if f64?
                     (fn [target at value]
                       (aset ^doubles target (int at) (double value)))
                     (fn [target at value]
                       (aset ^floats target (int at) (float value))))]
        (when (and supplied (not (map? supplied)))
          (throw (ex-info "a DSP shared region must be a map of control to value"
                          {:shared supplied})))
        (doseq [{:keys [name offset required?]} shared]
          ;; A control name is spelled as a symbol in the ABI; accept the keyword
          ;; spelling too, since a key that silently missed would publish zeros.
          (let [value (or (get supplied name) (get supplied (keyword name)))]
            (cond
              (nil? value)
              (when required?
                (throw (ex-info "a DSP shared control has no value for this block"
                                {:shared name :supplied (vec (keys supplied))})))

              (number? value)
              (dotimes [frame stride]
                (write! region (+ offset frame) value))

              :else
              (let [count (min (count value) stride)]
                (dotimes [index count]
                  (write! region (+ offset index) (nth value index)))))))
        (assoc context :shared region)))))

(defn process!
  "Execute a process artifact or instance with its explicit ABI context.

  The context supplies `:output`, `:frames`, `:sample-rate`, and a vector of
  `:controls` in ABI order, plus `:input` when the process declares an input
  port. Interpreter and JVM artifacts and instances expose this checked context
  adapter; generated JS/Wasm modules expose their target-native entry points
  through the same lifecycle vocabulary.

  `:lanes` carries a per-frame automation region (D28); `:shared` carries the
  pool-wide shared controls (M5). Either may be absent when the process declares
  none of that kind, may be a map of control name to values (this adapter then
  flattens it), or may be the region itself, for a host that already owns and
  writes one."
  [holder context]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have a process ABI"
                      {:entry (:entry artifact)})))
    (check-live! holder)
    (if-let [invoke-context (:invoke-context holder)]
      (invoke-context (shared-region artifact (lane-region artifact context)))
      (throw (ex-info "this DSP target does not expose an explicit process context adapter"
                      {:target (:target artifact)})))))

(defn initialize!
  "Initialize a process artifact or instance according to its lifecycle ABI."
  [holder]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have a process lifecycle"
                      {:entry (:entry artifact)})))
    (when-not (get-in artifact [:abi :lifecycle :init])
      (throw (ex-info "this DSP instance does not support initialization"
                      {:target (:target artifact)})))
    (check-live! holder)
    (if-let [initialize (:initialize holder)]
      (initialize)
      (throw (ex-info "this DSP target does not expose initialization"
                      {:target (:target artifact)})))))

(defn reset!
  "Reset the persistent state of a process artifact or instance.

  Each instance resets only its own state; resetting one instance never
  disturbs another."
  [holder]
  (let [artifact (or (:artifact holder) holder)]
    (when-not (= :process (:entry artifact))
      (throw (ex-info "only process artifacts have persistent state"
                      {:entry (:entry artifact)})))
    (when-not (get-in artifact [:abi :lifecycle :reset])
      (throw (ex-info "this DSP instance does not support reset"
                      {:target (:target artifact)})))
    (check-live! holder)
    (if-let [reset (:reset holder)]
      (reset)
      (throw (ex-info "this DSP target does not expose reset"
                      {:target (:target artifact)})))))
