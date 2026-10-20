(ns omkamra.dsp.targets.jvm
  "Lower explicit typed f32-audio DSP IR to JVM bytecode using insn."
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [insn.core :as insn]))

(definterface DspKernel
  (^void process [^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats float-state ^ints int-state
                  ^floats lanes ^floats shared]))

(definterface DspMultiKernel
  (^void process [^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats float-state ^ints int-state
                  ^floats lanes ^floats shared]))

(definterface DspKernelD64
  (^void process [^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles float-state ^ints int-state
                  ^doubles lanes ^doubles shared]))

(definterface DspMultiKernelD64
  (^void process [^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles float-state ^ints int-state
                  ^doubles lanes ^doubles shared]))

;; The final `lanes` argument is the block's flat per-frame automation region,
;; or nil when the process declares no lane. All of a process's lanes share it,
;; one `:frame-capacity`-frame slice each, so any number of lanes costs one
;; argument rather than one argument per lane (D28). The `shared` argument after
;; it is the same shape, but one region for the whole pool rather than one per
;; voice, so a broadcast parameter is written once (M5) — nil when the process
;; declares no shared control.
(definterface DspProcess
  (^void process [^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats lanes ^floats shared]))

(definterface DspMultiProcess
  (^void process [^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats lanes ^floats shared]))

(definterface DspProcessD64
  (^void process [^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles lanes ^doubles shared]))

(definterface DspMultiProcessD64
  (^void process [^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles lanes ^doubles shared]))

(deftype ^:private F32ProcessAdapter [^DspKernel kernel
                                      ^floats float-state
                                      ^ints int-state]
  DspProcess
  (^void process [_ ^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls ^floats lanes
                  ^floats shared]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state lanes shared)))

(deftype ^:private F32MultiProcessAdapter [^DspMultiKernel kernel
                                           ^floats float-state
                                           ^ints int-state]
  DspMultiProcess
  (^void process [_ ^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls ^floats lanes
                  ^floats shared]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state lanes shared)))

(deftype ^:private F64ProcessAdapter [^DspKernelD64 kernel
                                      ^doubles float-state
                                      ^ints int-state]
  DspProcessD64
  (^void process [_ ^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls ^doubles lanes
                  ^doubles shared]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state lanes shared)))

(deftype ^:private F64MultiProcessAdapter [^DspMultiKernelD64 kernel
                                           ^doubles float-state
                                           ^ints int-state]
  DspMultiProcessD64
  (^void process [_ ^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls ^doubles lanes
                  ^doubles shared]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state lanes shared)))

(def ^:private float-array-class (Class/forName "[F"))
(def ^:private int-array-class (Class/forName "[I"))
(def ^:private float-2d-array-class (Class/forName "[[F"))

(def ^:dynamic *precision* nil)

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "JVM lowering requires explicit DSP precision"
                    {:precision *precision*}))))

(defn- real-type [] (if (f64?) :double :float))
(defn- real-array-class [] (if (f64?) (Class/forName "[D") float-array-class))
(defn- real-2d-array-class [] (if (f64?) (Class/forName "[[D") float-2d-array-class))
(defn- real-load-op [] (if (f64?) :daload :faload))
(defn- real-store-op [] (if (f64?) :dastore :fastore))
(defn- real-local-load-op [] (if (f64?) :dload :fload))
(defn- real-local-store-op [] (if (f64?) :dstore :fstore))
(defn- real-return-op [] (if (f64?) :dreturn :freturn))
(defn- real-const-zero-op [] (if (f64?) :dconst-0 :fconst-0))
(defn- real-const-instructions
  [value]
  (if (f64?)
    [[:ldc2 (double value)]]
    [[:ldc (float value)]]))
(defn- real-param-width [] (if (f64?) 2 1))

(def ^:private class-counter (atom 0))
(defn- class-name
  [name]
  (let [fragment (-> (clojure.core/name name)
                     (str/replace #"[^A-Za-z0-9_$]" "_")
                     (str/replace #"^[0-9]" "_$0"))]
    (symbol (str "omkamra.dsp.generated." fragment "__"
                 (swap! class-counter inc)))))

(defn- next-label
  [labels prefix]
  (keyword (str prefix "-" (swap! labels inc))))

(defn- next-temp-slot
  [slots]
  (let [slot @(:next-temp slots)]
    (swap! (:next-temp slots) inc)
    slot))

(declare expression-instructions statement-instructions condition-instructions
         builtin-instructions state-reference-instructions)

(defn- channel-instructions
  [channel slots labels]
  (if (= :channel (:op channel))
    [[:iload (:channel slots)]]
    (if (map? channel)
      (expression-instructions channel slots labels)
      [[:ldc channel]])))

(defn- index-instructions
  [index slots labels]
  (case (:op index)
    :frame [[:iload (:frame slots)]]
    :frame-offset [[:iload (:frame slots)] [:ldc (:offset index)] [:iadd]]
    ;; A lane read in a fused artifact lands in the slice its call site was
    ;; handed, so the slice base the callee was given comes first (F4).
    :lane-frame-offset (vec (concat [[:iload (:lane-base slots)] [:iload (:frame slots)]
                                     [:ldc (:offset index)]]
                                    [[:iadd] [:iadd]]))
    :frame-plus (vec (concat [[:iload (:frame slots)]]
                             (expression-instructions (:offset index) slots labels)
                             [[:iadd]]))
    (throw (ex-info "unknown JVM buffer index" {:index index}))))

(defn- buffer-reference-instructions
  [operation slots labels]
  (let [buffer (get slots (:buffer-id operation))]
    (when (nil? buffer)
      (throw (ex-info "JVM buffer access has no slot"
                      {:buffer-id (:buffer-id operation) :slots (vec (keys slots))})))
    ;; A broadcast buffer — a one-channel buffer in a multi-channel process, and
    ;; in particular a lane region — is read identically at every channel, so it
    ;; is never subscripted.
    (if (:per-channel? operation)
      (concat [[:aload buffer]]
              (channel-instructions (:channel operation) slots labels)
              [[:aaload]])
      [[:aload buffer]])))

(defn- comparison-branch-instructions
  [comparison slots labels true-label false-label]
  ;; Only `:float` uses floating-point comparison; `:int` and `:boolean` are
  ;; both represented as JVM ints.
  (if (not= :float (get-in comparison [:left :type]))
    (let [branch (case (:operator comparison)
                   :< :if-icmplt :<= :if-icmple :> :if-icmpgt
                   :>= :if-icmpge := :if-icmpeq :not= :if-icmpne)]
      (concat (expression-instructions (:left comparison) slots labels)
              (expression-instructions (:right comparison) slots labels)
              [[branch true-label] [:goto false-label]]))
    (let [[compare branch] (case (:operator comparison)
                             :< [(if (f64?) :dcmpg :fcmpg) :iflt]
                             :<= [(if (f64?) :dcmpg :fcmpg) :ifle]
                             :> [(if (f64?) :dcmpl :fcmpl) :ifgt]
                             :>= [(if (f64?) :dcmpl :fcmpl) :ifge]
                             := [(if (f64?) :dcmpl :fcmpl) :ifeq]
                             :not= [(if (f64?) :dcmpl :fcmpl) :ifne])]
      (concat (expression-instructions (:left comparison) slots labels)
              (expression-instructions (:right comparison) slots labels)
              [[compare] [branch true-label] [:goto false-label]]))))

(defn- condition-instructions
  [condition slots labels true-label false-label]
  (case (:op condition)
    :compare (comparison-branch-instructions condition slots labels
                                             true-label false-label)
    :logical (case (:operator condition)
               :not (condition-instructions (first (:args condition)) slots labels
                                            false-label true-label)
               :and (let [right-label (next-label labels "and-right")]
                      (concat
                       (condition-instructions (first (:args condition)) slots labels
                                               right-label false-label)
                       [[:mark right-label]]
                       (condition-instructions (second (:args condition)) slots labels
                                               true-label false-label)))
               :or (let [right-label (next-label labels "or-right")]
                     (concat
                      (condition-instructions (first (:args condition)) slots labels
                                              true-label right-label)
                      [[:mark right-label]]
                      (condition-instructions (second (:args condition)) slots labels
                                              true-label false-label))))
    :const (if (:value condition) [[:goto true-label]] [[:goto false-label]])
    (:local :state-load) (concat (expression-instructions condition slots labels)
                                 [[:ifne true-label]
                                  [:goto false-label]])
    (throw (ex-info "JVM condition has an unsupported expression"
                    {:condition condition}))))

(defn- boolean-value-instructions
  [expression slots labels]
  (let [true-label (next-label labels "bool-true")
        false-label (next-label labels "bool-false")
        done-label (next-label labels "bool-done")]
    (vec (concat
          (condition-instructions expression slots labels true-label false-label)
          [[:mark true-label] [:iconst-1] [:goto done-label]
           [:mark false-label] [:iconst-0] [:mark done-label]]))))

(defn- int-division-instructions
  "Lower integer `/` or `rem` with the D12 zero-divisor guard.

  Both operands are evaluated left-to-right into temporary slots so the guard
  does not evaluate an effectful operand twice.  The JVM `idiv`/`irem` wrap
  `INT_MIN / -1` to `INT_MIN` natively, so only the zero case needs handling."
  [operator left right slots labels]
  (let [left-slot (next-temp-slot slots)
        right-slot (next-temp-slot slots)
        nonzero-label (next-label labels "int-div-nonzero")
        done-label (next-label labels "int-div-done")]
    (vec (concat
          (expression-instructions left slots labels)
          [[:istore left-slot]]
          (expression-instructions right slots labels)
          [[:istore right-slot]
           [:iload right-slot] [:ifne nonzero-label]
           [:iconst-0] [:goto done-label]
           [:mark nonzero-label]
           [:iload left-slot] [:iload right-slot]
           [(if (= operator :div) :idiv :irem)]
           [:mark done-label]]))))

(defn- real-jvm-type []
  (if (f64?) Double/TYPE Float/TYPE))

(defn- double-transcendental-instructions
  "Emit `Math/<method>(double)` for a f32 or f64 unary operation."
  [method expression slots labels]
  (let [x (first (:args expression))]
    (concat (expression-instructions x slots labels)
            (when-not (f64?) [[:f2d]])
            [[:invokestatic java.lang.Math method [Double/TYPE Double/TYPE]]]
            (when-not (f64?) [[:d2f]]))))

(defn- double-binary-transcendental-instructions
  "Emit `Math/<method>(double, double)` for a f32 or f64 binary operation."
  [method expression slots labels]
  (let [[x y] (:args expression)]
    (concat (expression-instructions x slots labels)
            (when-not (f64?) [[:f2d]])
            (expression-instructions y slots labels)
            (when-not (f64?) [[:f2d]])
            [[:invokestatic java.lang.Math method [Double/TYPE Double/TYPE Double/TYPE]]]
            (when-not (f64?) [[:d2f]]))))

(defmulti builtin-instructions
  "JVM lowering for a primitive builtin, dispatched on `[operator type]`."
  (fn [expression _slots _labels] [(:builtin expression) (:type expression)]))

(defmethod builtin-instructions :default
  [expression _slots _labels]
  (throw (ex-info "JVM backend does not implement this DSP builtin"
                  {:builtin (:builtin expression) :type (:type expression)})))

(defmethod builtin-instructions [:abs :float]
  [expression slots labels]
  (let [t (real-jvm-type)]
    (concat (expression-instructions (first (:args expression)) slots labels)
            [[:invokestatic java.lang.Math "abs" [t t]]])))

(defmethod builtin-instructions [:abs :int]
  [expression slots labels]
  (concat (expression-instructions (first (:args expression)) slots labels)
          [[:invokestatic java.lang.Math "abs" [Integer/TYPE Integer/TYPE]]]))

(defn- min-max-instructions
  [method expression slots labels]
  (let [[x y] (:args expression)]
    (concat (expression-instructions x slots labels)
            (expression-instructions y slots labels)
            (if (= :int (:type expression))
              [[:invokestatic java.lang.Math method
                [Integer/TYPE Integer/TYPE Integer/TYPE]]]
              (let [t (real-jvm-type)]
                [[:invokestatic java.lang.Math method [t t t]]])))))

(defmethod builtin-instructions [:min :float] [e slots labels] (min-max-instructions "min" e slots labels))
(defmethod builtin-instructions [:max :float] [e slots labels] (min-max-instructions "max" e slots labels))
(defmethod builtin-instructions [:min :int] [e slots labels] (min-max-instructions "min" e slots labels))
(defmethod builtin-instructions [:max :int] [e slots labels] (min-max-instructions "max" e slots labels))

(defmethod builtin-instructions [:floor :float] [e slots labels]
  (double-transcendental-instructions "floor" e slots labels))
(defmethod builtin-instructions [:ceil :float] [e slots labels]
  (double-transcendental-instructions "ceil" e slots labels))
(defmethod builtin-instructions [:sqrt :float] [e slots labels]
  (double-transcendental-instructions "sqrt" e slots labels))
(defmethod builtin-instructions [:exp :float] [e slots labels]
  (double-transcendental-instructions "exp" e slots labels))
(defmethod builtin-instructions [:sin :float] [e slots labels]
  (double-transcendental-instructions "sin" e slots labels))
(defmethod builtin-instructions [:cos :float] [e slots labels]
  (double-transcendental-instructions "cos" e slots labels))
(defmethod builtin-instructions [:tan :float] [e slots labels]
  (double-transcendental-instructions "tan" e slots labels))
(defmethod builtin-instructions [:asin :float] [e slots labels]
  (double-transcendental-instructions "asin" e slots labels))
(defmethod builtin-instructions [:acos :float] [e slots labels]
  (double-transcendental-instructions "acos" e slots labels))
(defmethod builtin-instructions [:atan :float] [e slots labels]
  (double-transcendental-instructions "atan" e slots labels))
(defmethod builtin-instructions [:atan2 :float] [e slots labels]
  (double-binary-transcendental-instructions "atan2" e slots labels))
(defmethod builtin-instructions [:log :float] [e slots labels]
  (double-transcendental-instructions "log" e slots labels))
(defmethod builtin-instructions [:pow :float] [e slots labels]
  (double-binary-transcendental-instructions "pow" e slots labels))
(defmethod builtin-instructions [:sinh :float] [e slots labels]
  (double-transcendental-instructions "sinh" e slots labels))
(defmethod builtin-instructions [:cosh :float] [e slots labels]
  (double-transcendental-instructions "cosh" e slots labels))
(defmethod builtin-instructions [:tanh :float] [e slots labels]
  (double-transcendental-instructions "tanh" e slots labels))

(defmethod builtin-instructions [:copysign :float]
  [expression slots labels]
  (let [[x y] (:args expression)
        t (real-jvm-type)]
    (concat (expression-instructions x slots labels)
            (expression-instructions y slots labels)
            [[:invokestatic java.lang.Math "copySign" [t t t]]])))

(defmethod builtin-instructions [:trunc :float]
  [expression slots labels]
  (let [x (first (:args expression))]
    (concat (expression-instructions x slots labels)
            (when-not (f64?) [[:f2d]])
            [[:invokestatic java.lang.Math "abs" [Double/TYPE Double/TYPE]]
             [:invokestatic java.lang.Math "floor" [Double/TYPE Double/TYPE]]]
            (expression-instructions x slots labels)
            (when-not (f64?) [[:f2d]])
            [[:invokestatic java.lang.Math "copySign" [Double/TYPE Double/TYPE Double/TYPE]]]
            (when-not (f64?) [[:d2f]]))))

(defmethod builtin-instructions [:rem :int]
  [expression slots labels]
  (int-division-instructions :rem (first (:args expression))
                             (second (:args expression)) slots labels))

(defn- state-reference-instructions
  "The array reference a state or scratch access reads or writes through.

  A callee's arena starts at its own runtime base register (D47), so its layout
  entries carry a `:base-slot` and the constant index becomes `base + index`. The
  process entry's own base is zero, so its accesses stay `array[index]` and its hot
  loop does not pay for any of this."
  [entry element-index slots labels]
  (let [{:keys [slot index base-slot]} entry]
    (concat (if base-slot
              [[:aload slot] [:iload base-slot] [:ldc index] [:iadd]]
              [[:aload slot] [:ldc index]])
            (when element-index
              (concat (expression-instructions element-index slots labels)
                      [[:iadd]])))))

(defn- partition-ranges
  "Where each storage of a call site's state partition lives in the arena (D19).

  A call site's `:state-base` is a *logical* index into the enclosing definition's
  state, and the arena is two physical arrays, so the partition is resolved per
  storage: the physical index of its first float (resp. int) slot and how many it
  holds. Relative physical positions are stable under rebasing, so a callee's own
  zero-based slots add to that base directly -- which is also why each storage's
  slots are one contiguous run, and why a host can restore exactly one call site's
  state without touching its neighbours (M6a). A storage the partition does not use
  is empty, with a zero base the callee never reads."
  [state-entries base size]
  (let [storage (fn [entry] (if (= :float (:storage entry)) :float :int))
        in-range? (fn [entry]
                    (let [logical (long (:state-index entry))]
                      (and base
                           (<= (long base) logical)
                           (< logical (+ (long base) (long size))))))
        ;; An entry may own several slots -- a delay line's 64-sample buffer is one
        ;; entry, not 64 -- so a range is the span of every entry in it, not a set
        ;; of first indices.
        slots (fn [entry]
                (let [index (long (:index entry))]
                  (range index (+ index (long (or (:size entry) 1))))))
        indices (reduce (fn [indices entry]
                          (if (in-range? entry)
                            (update indices (storage entry) into (slots entry))
                            indices))
                        {:float [] :int []}
                        state-entries)]
    (reduce-kv
     (fn [resolved storage index]
       (let [index (sort index)
             base (long (or (first index) 0))
             contiguous? (= index (vec (range base (+ base (count index)))))]
         (when-not contiguous?
           (throw (ex-info "DSP call partition state is not contiguous"
                           {:storage storage :indices index})))
         (assoc resolved storage {:base base :count (count index)})))
     {}
     indices)))

(defn- partition-bases
  "The physical arena base of each storage in a call site's state partition."
  [state-layout base size]
  (let [ranges (partition-ranges (vals state-layout) base size)]
    {:float (long (:base (:float ranges)))
     :int (long (:base (:int ranges)))}))

(defn- partition-slots
  "The physical arena range of every call site that owns a state partition.

  `ir`'s `:call-partitions` is already in evaluation order, so entry `k` here is
  the `k`-th call site -- for a `(poly S voice)` process, slot `k`. This is what
  lets one instance hold several slots and still reset one of them (M6a)."
  [ir state-layout]
  (mapv (fn [{:keys [base size specials special-mask]}]
          (let [ranges (partition-ranges (:entries state-layout) base size)]
            (cond-> {:float-base (long (:base (:float ranges)))
                     :float-count (long (:count (:float ranges)))
                     :int-base (long (:base (:int ranges)))
                     :int-count (long (:count (:int ranges)))}
              ;; A call site's specials (a fused slot's `:activity`, M6 seam 4),
              ;; as logical indices the instance resolves to physical slots. The
              ;; entry's own specials are the ABI's; these are each partition's.
              (seq specials) (assoc :specials specials
                                    :special-mask (long special-mask)))))
        (:call-partitions ir)))

(defn- base-instructions
  "Push the arena base a call passes for one storage.

  The constant is the partition's physical offset. A callee method adds it to its
  own base register; the entry, whose base is zero, just loads it."
  [storage slots constant]
  (let [base-slot (get (:base-slots slots) storage)]
    (if base-slot
      [[:iload base-slot] [:ldc (long constant)] [:iadd]]
      [[:ldc (long constant)]])))

(defn- call-instructions
  "Emit a call to a definition that was lowered to a method on this class (D47).

  The ABI's leading parameters come first, in order — the two state arrays, the two
  bases, then the context values every definition may reference — and then the
  arguments, which are evaluated left to right."
  [expression slots labels]
  (let [{:keys [function-id args state-base]} expression
        definition (get (:definitions slots) function-id)
        signature (get (:calls slots) function-id)]
    (when-not signature
      (throw (ex-info "JVM call site refers to a definition that was not emitted"
                      {:function-id function-id
                       :emitted (vec (keys (:calls slots)))})))
    (let [bases (partition-bases (:state-layout slots) state-base
                                 (:state-size definition))]
      (vec (concat
            [[:aload (:float-state slots)] [:aload (:int-state slots)]]
            (base-instructions :float slots (:float bases))
            (base-instructions :int slots (:int bases))
            [[:iload (:frames slots)]]
            [[(real-local-load-op) (:sample-rate slots)]]
            ;; A callee that addresses anything by frame is handed the current
            ;; frame (F2, F3) before its own arguments, matching
            ;; `callee-descriptor`.
            (when (or (seq (:region-slots definition))
                      (:lane-region? definition))
              [[:iload (:frame slots)]])
            ;; And a lane-reading one the lane region (F3): the caller's own, which
            ;; is the one region every definition in the artifact shares — and, in
            ;; a fused artifact, the base of the slice this call site reads, since
            ;; that region holds one slice per voice (F4).
            (when (:lane-region? definition) [[:aload (:lanes slots)]])
            (when (:lane-base? definition)
              (expression-instructions (:lane-base expression) slots labels))
            (mapcat #(expression-instructions % slots labels) args)
            [[:invokestatic (:class slots) (:method signature) (:desc signature)]])))))

(defn- expression-instructions
  [expression slots labels]
  (case (:op expression)
    :const (case (:type expression)
             :boolean [(if (:value expression) [:iconst-1] [:iconst-0])]
             :int [[:ldc (int (:value expression))]]
             (real-const-instructions (:value expression)))
    :local (let [slot (get slots (:name expression))]
             (when (nil? slot)
               (throw (ex-info "JVM local has no slot"
                               {:name (:name expression) :slots (vec (keys slots))})))
             (if (= :float (:type expression))
               [[(real-local-load-op) slot]]
               [[:iload slot]]))
    (:state-load :array-load) (let [entry (get (:state-layout slots) (:index expression))]
                                (when-not entry
                                  (throw (ex-info "JVM state access has no layout entry"
                                                  {:index (:index expression)
                                                   :state-id (:state-id expression)
                                                   :layout-keys (vec (sort (keys (:state-layout slots))))})))
                                (vec (concat (state-reference-instructions
                                              entry (:element-index expression) slots labels)
                                             [[(if (= :float (:type entry))
                                                 (real-load-op) :iaload)]])))
    :frame [[:iload (:frame slots)]]
    :lane-base [[:iload (:lane-base slots)]]
    :frames [[:iload (:frames slots)]]
    :sample-rate [[(real-local-load-op) (:sample-rate slots)]]
    :channel [[:iload (:channel slots)]]
    :frame-plus (vec (concat [[:iload (:frame slots)]]
                             (expression-instructions (:offset expression) slots labels)
                             [[:iadd]]))
    :buffer-load (vec (concat (buffer-reference-instructions expression slots labels)
                              (index-instructions (:index expression) slots labels)
                              [[(real-load-op)]]))
    :binary (if (and (= :int (:type expression)) (= :/ (:operator expression)))
              (int-division-instructions :div (:left expression)
                                         (:right expression) slots labels)
              (vec
               (concat
                (expression-instructions (:left expression) slots labels)
                (expression-instructions (:right expression) slots labels)
                [(if (= :int (:type expression))
                   (case (:operator expression)
                     :+ [:iadd] :- [:isub] :* [:imul] :/ [:idiv])
                   (case (:operator expression)
                     :+ [(if (f64?) :dadd :fadd)]
                     :- [(if (f64?) :dsub :fsub)]
                     :* [(if (f64?) :dmul :fmul)]
                     :/ [(if (f64?) :ddiv :fdiv)]))])))
    :builtin (vec (builtin-instructions expression slots labels))
    :call (call-instructions expression slots labels)
    :compare (boolean-value-instructions expression slots labels)
    :logical (boolean-value-instructions expression slots labels)
    :bitwise (vec
              (case (:operator expression)
                :bit-not (concat (expression-instructions (first (:args expression))
                                                          slots labels)
                                 [[:iconst-m1] [:ixor]])
                (concat (expression-instructions (first (:args expression))
                                                 slots labels)
                        (expression-instructions (second (:args expression))
                                                 slots labels)
                        [(case (:operator expression)
                           :bit-and [:iand]
                           :bit-or [:ior]
                           :bit-xor [:ixor]
                           :bit-shift-left [:ishl]
                           :bit-shift-right [:ishr]
                           :unsigned-bit-shift-right [:iushr])])))
    :convert (case (:operator expression)
               :int->float (conj (vec (expression-instructions (:value expression)
                                                               slots labels))
                                 [(if (f64?) :i2d :i2f)])
               :float->int (conj (vec (expression-instructions (:value expression)
                                                               slots labels))
                                 [(if (f64?) :d2i :f2i)]))
    (throw (ex-info "unsupported JVM expression" {:expression expression}))))

(defn- statement-instructions
  [statement slots labels loop-stack]
  (case (:op statement)
    :block (vec (mapcat #(statement-instructions % slots labels loop-stack)
                        (:statements statement)))
    :declare (if-let [init (:init statement)]
               (vec (concat (expression-instructions init slots labels)
                            [[(if (= :float (:type statement))
                                (real-local-store-op) :istore)
                              (get slots (:name statement))]]))
               (if (= :float (:type statement))
                 [[(real-const-zero-op)] [(real-local-store-op) (get slots (:name statement))]]
                 [[:iconst-0] [:istore (get slots (:name statement))]]))
    :assign (vec (concat (expression-instructions (:value statement) slots labels)
                         [[(if (= :float (:type statement))
                             (real-local-store-op) :istore)
                           (get slots (:name statement))]]))
    :expression (let [instructions (vec (expression-instructions (:value statement) slots labels))]
                  ;; A void call leaves nothing to discard (F2).
                  (if (= :void (:type (:value statement)))
                    instructions
                    (conj instructions [:pop])))
    :return (if (nil? (:value statement))
              [[:return]]
              (vec (concat (expression-instructions (:value statement) slots labels)
                           [(if (= :float (:type (:value statement)))
                              [(real-return-op)]
                              [:ireturn])])))
    :buffer-store (vec (concat
                        (buffer-reference-instructions statement slots labels)
                        (index-instructions (:index statement) slots labels)
                        (expression-instructions (:value statement) slots labels)
                        [[(real-store-op)]]))
    (:state-store :array-store) (let [entry (get (:state-layout slots) (:index statement))]
                                  (vec (concat (state-reference-instructions
                                                entry (:element-index statement) slots labels)
                                               (expression-instructions (:value statement) slots labels)
                                               [[(if (= :float (:type entry))
                                                   (real-store-op) :iastore)]])))
    :if (let [then-label (next-label labels "if-then")
              else-label (next-label labels "if-else")
              done-label (next-label labels "if-done")]
          (vec (concat
                (condition-instructions (:condition statement) slots labels
                                        then-label else-label)
                [[:mark then-label]]
                (statement-instructions (:then statement) slots labels loop-stack)
                [[:goto done-label] [:mark else-label]]
                (statement-instructions (:else statement) slots labels loop-stack)
                [[:mark done-label]])))
    :while (let [condition-label (next-label labels "while-condition")
                 body-label (next-label labels "while-body")
                 done-label (next-label labels "while-done")
                 bound (:bound statement)
                 counter-slot (when bound (next-temp-slot slots))
                 within-bound-label (when bound (next-label labels "while-within-bound"))
                 loop-stack (conj loop-stack {:continue condition-label
                                              :break done-label})]
             (vec (concat
                   (when bound [[:iconst-0] [:istore counter-slot]])
                   [[:mark condition-label]]
                   (condition-instructions (:condition statement) slots labels
                                           body-label done-label)
                   [[:mark body-label]]
                   (when bound
                     [[:iload counter-slot] [:ldc bound]
                      [:if-icmplt within-bound-label]
                      [:new IllegalStateException] [:dup]
                      [:ldc "DSP loop exceeded its iteration bound"]
                      [:invokespecial IllegalStateException "<init>" [String Void/TYPE]]
                      [:athrow]
                      [:mark within-bound-label]
                      [:iinc counter-slot 1]])
                   (statement-instructions (:body statement) slots labels loop-stack)
                   [[:goto condition-label] [:mark done-label]])))
    :break [[:goto (:break (peek loop-stack))]]
    :continue [[:goto (:continue (peek loop-stack))]]
    (throw (ex-info "unsupported JVM statement" {:statement statement}))))

(defn- invoke-method
  [class ir]
  (.getMethod class "invoke"
              (into-array Class
                          (map #(if (= :float (:type %))
                                  (if (f64?) Double/TYPE Float/TYPE)
                                  Integer/TYPE)
                               (:params ir)))))

(defn- jvm-type
  [type]
  (case type
    :float (real-type)
    ;; A process-shaped module writes its own region and returns nothing (F2).
    :void :void
    :int))

(defn- state-layout
  [state]
  (reduce (fn [{:keys [entries float-count int-count] :as layout}
               [position {:keys [index type size]}]]
            (let [storage (if (= :float type) :float :int)
                  size (or size 1)
                  logical-index (or index position)
                  physical-index (if (= storage :float) float-count int-count)
                  entry {:state-index logical-index
                         :type type
                         :size size
                         :storage storage
                         :index physical-index}]
              (cond-> (assoc layout :entries (conj entries entry))
                (= storage :float) (update :float-count + size)
                (= storage :int) (update :int-count + size))))
          {:entries [] :float-count 0 :int-count 0}
          (map-indexed vector state)))

(defn- control-value
  [type value]
  (case type
    :float (if (f64?) (double value) (float value))
    :boolean (if value (int 1) (int 0))
    :int (int value)))

(defn- write-controls!
  [layout controls values]
  (doseq [{:keys [state-index storage index type]} (:entries layout)]
    (let [value (control-value type (nth controls state-index))]
      (if (= storage :float)
        (if (f64?)
          (aset-double ^doubles (:float values) index value)
          (aset-float ^floats (:float values) index value))
        (aset-int ^ints (:int values) index value))))
  values)

(defn- callable
  [precision method ir]
  (fn [& args]
    (binding [*precision* precision]
      (when-not (= (count (:params ir)) (count args))
        (throw (ex-info "wrong number of arguments for DSP function"
                        {:expected (count (:params ir)) :actual (count args)
                         :name (:name ir)})))
      (let [arguments (map (fn [{:keys [type]} value]
                             (if (= :float type)
                               (if (f64?) (double value) (float value))
                               (if (= :boolean type)
                                 (if value (int 1) (int 0))
                                 (int value))))
                           (:params ir) args)
            result (.invoke method nil (object-array arguments))]
        (case (:return-type ir)
          :float (if (f64?) (double result) (float result))
          :int (int result)
          :boolean (not (zero? (int result))))))))

(defn- check-float-array!
  [value name]
  (when-not (instance? (real-array-class) value)
    (throw (ex-info (if (f64?) "DSP process expects primitive double arrays"
                        "DSP process expects primitive float arrays")
                    {:argument name :value value}))))

(defn- real-array-length
  "Length of a primitive real array, typed by the current precision.

  Hinted on purpose: an unhinted `alength` compiles to a reflective call at
  about 24 KB per invocation (D26), and the checked adapter is called by hosts
  that are not necessarily on the audio path but should not pay that anyway."
  [value]
  (if (f64?) (alength ^doubles value) (alength ^floats value)))

(defn- check-buffer!
  [ir value name frames]
  (let [required-length (+ frames (get-in ir [:memory name :max-frame-offset] 0))]
    (if (= 1 (:channels ir))
      (do
        (check-float-array! value name)
        (let [length (real-array-length value)]
          (when (< length required-length)
            (throw (ex-info "DSP buffer is shorter than the frame-access requirement"
                            {:argument name :frames frames :required-length required-length
                             :length length})))))
      (do
        (when-not (instance? (real-2d-array-class) value)
          (throw (ex-info (if (f64?) "DSP multi-channel process expects double[][] buffers"
                              "DSP multi-channel process expects float[][] buffers")
                          {:argument name :value value})))
        (when-not (= (:channels ir) (alength ^objects value))
          (throw (ex-info "DSP buffer has the wrong channel count"
                          {:argument name :expected (:channels ir)
                           :actual (alength ^objects value)})))
        (dotimes [channel (:channels ir)]
          (let [buffer (aget ^objects value channel)]
            (check-float-array! buffer [name channel])
            (let [length (real-array-length buffer)]
              (when (< length required-length)
                (throw (ex-info "DSP buffer channel is shorter than the frame-access requirement"
                                {:argument [name channel] :frames frames
                                 :required-length required-length
                                 :length length}))))))))))

(defn- check-state!
  [layout state]
  (when-not (map? state)
    (throw (ex-info "DSP process state has the wrong representation"
                    {:expected :typed-state-arrays :actual state})))
  (let [float-state (:float state)
        int-state (:int state)]
    (check-float-array! float-state :float-state)
    (when-not (instance? int-array-class int-state)
      (throw (ex-info "DSP process integer state must be an int array"
                      {:value int-state})))
    (when-not (= (:float-count layout) (real-array-length float-state))
      (throw (ex-info "DSP process float state has the wrong size"
                      {:expected (:float-count layout)
                       :actual (real-array-length float-state)})))
    (when-not (= (:int-count layout) (alength ^ints int-state))
      (throw (ex-info "DSP process integer state has the wrong size"
                      {:expected (:int-count layout) :actual (alength ^ints int-state)})))
    state))

(defn- reset-state!
  [ir layout state]
  (check-state! layout state)
  (doseq [{:keys [index state-index storage type]} (:entries layout)
          :let [state-description (some #(when (= (:index %) state-index) %) (:state ir))]
          :when state-description
          [element-index value] (map-indexed vector
                                             (if (= 1 (or (:size state-description) 1))
                                               [(:init state-description)]
                                               (:init state-description)))]
    (let [index (+ index element-index)]
      (if (= storage :float)
        (if (f64?)
          (aset-double ^doubles (:float state) index (double value))
          (aset-float ^floats (:float state) index (float value)))
        (aset-int ^ints (:int state) index (control-value type value)))))
  nil)

(defn- check-controls!
  [layout float-controls int-controls]
  (check-float-array! float-controls :float-controls)
  (when-not (instance? int-array-class int-controls)
    (throw (ex-info "DSP process integer controls must be an int array"
                    {:value int-controls})))
  (when-not (= (:float-count layout) (real-array-length float-controls))
    (throw (ex-info "DSP process float controls have the wrong size"
                    {:expected (:float-count layout)
                     :actual (real-array-length float-controls)})))
  (when-not (= (:int-count layout) (alength ^ints int-controls))
    (throw (ex-info "DSP process integer controls have the wrong size"
                    {:expected (:int-count layout)
                     :actual (alength ^ints int-controls)}))))

(defn- unchecked-adapter
  [precision kernel multi-channel? state]
  (let [float-state (:float state)
        int-state (:int state)]
    (case precision
      :f32 (if multi-channel?
             (F32MultiProcessAdapter. ^DspMultiKernel kernel float-state int-state)
             (F32ProcessAdapter. ^DspKernel kernel float-state int-state))
      :f64 (if multi-channel?
             (F64MultiProcessAdapter. ^DspMultiKernelD64 kernel float-state int-state)
             (F64ProcessAdapter. ^DspKernelD64 kernel float-state int-state)))))

(defn- check-input!
  "Validate the input buffer when the process declares one.

  A generator (`:input false`, D23) has no input buffer, so its input argument
  is nil and there is nothing to check; the kernel never loads it."
  [ir input frames]
  (when (some #(= :input (:direction %)) (:buffers ir))
    (check-buffer! ir input :input frames)))

(defn- check-lanes!
  "Validate the flat lane region against the artifact's lane layout.

  Lane bases are compile-time constants (`i * frame-capacity`), so a host must
  allocate the whole region even when it renders a short block. Validating that
  here keeps a short region from becoming an out-of-bounds read inside the audio
  callback."
  [artifact lanes]
  (let [ir (:process-ir artifact)
        declared (count (:lanes ir))]
    (when (pos? declared)
      (when (nil? lanes)
        (throw (ex-info "this DSP process declares per-frame lanes"
                        {:lanes (mapv :name (:lanes ir))})))
      (when-not (instance? (real-array-class) lanes)
        (throw (ex-info "DSP lane region has the wrong primitive type"
                        {:precision (:precision ir)
                         :actual (class lanes)})))
      (let [required (* declared (long (:frame-capacity ir))
                        (long (or (:lane-slots ir) 1)))]
        (when (< (alength lanes) required)
          (throw (ex-info "DSP lane region is shorter than the lane layout"
                          {:expected required
                           :actual (alength lanes)
                           :lane-count declared
                           :lane-stride (:frame-capacity ir)}))))))
  lanes)

(defn- check-shared!
  "Validate the flat shared region against the artifact's shared layout.

  Shared bases are compile-time constants (`i * frame-capacity`), exactly like a
  lane's, so the region is one `frame-capacity`-frame slice per shared control —
  allocated once for the whole pool, not once per voice (M5)."
  [artifact shared]
  (let [ir (:process-ir artifact)
        declared (count (:shared ir))]
    (when (pos? declared)
      (when (nil? shared)
        (throw (ex-info "this DSP process declares shared controls"
                        {:shared (mapv :name (:shared ir))})))
      (when-not (instance? (real-array-class) shared)
        (throw (ex-info "DSP shared region has the wrong primitive type"
                        {:precision (:precision ir)
                         :actual (class shared)})))
      (let [required (* declared (long (:frame-capacity ir)))]
        (when (< (alength shared) required)
          (throw (ex-info "DSP shared region is shorter than the shared layout"
                          {:expected required
                           :actual (alength shared)
                           :shared-count declared
                           :shared-stride (:frame-capacity ir)}))))))
  shared)

(defn process-unchecked!
  "Run a JVM process artifact through its primitive, state-owning ABI.

  This performs no representation, shape, length, or control-layout checks.
  `input` and `output` must be the artifact's f32/f64 mono or multichannel
  primitive arrays. `float-controls` and `int-controls` must be the reusable
  physical control arrays described by `:control-layout`; state remains owned
  by the artifact. Real-time hosts should retain and call `:unchecked`
  directly through its matching `Dsp*Process` interface when possible.

  The optional final arguments are the flat per-frame lane region (D28) and the
  pool-wide shared region (M5)."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls nil nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (process-unchecked! holder input output frames sample-rate float-controls
                       int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (let [artifact (or (:artifact holder) holder)
         adapter (:unchecked holder)
         multi-channel? (:multi-channel? holder)]
     ;; One field read, so the real-time path stays allocation-free.
     (when (and (:has-lanes? artifact) (nil? lanes))
       (throw (ex-info "this DSP process declares per-frame lanes"
                       {:lanes (mapv :name (:lanes (:process-ir artifact)))})))
     (when (and (:has-shared? artifact) (nil? shared))
       (throw (ex-info "this DSP process declares shared controls"
                       {:shared (mapv :name (:shared (:process-ir artifact)))})))
     (case (:precision holder)
       :f32 (if multi-channel?
              (.process ^DspMultiProcess adapter input output (int frames)
                        (float sample-rate) float-controls int-controls lanes shared)
              (.process ^DspProcess adapter input output (int frames)
                        (float sample-rate) float-controls int-controls lanes shared))
       :f64 (if multi-channel?
              (.process ^DspMultiProcessD64 adapter input output (int frames)
                        (double sample-rate) float-controls int-controls lanes shared)
              (.process ^DspProcessD64 adapter input output (int frames)
                        (double sample-rate) float-controls int-controls lanes shared))))))

(defn process-checked!
  "Validate and run the raw primitive JVM process ABI.

  This has the same arguments as `process-unchecked!`, but verifies buffer
  shapes and lengths, lane region storage, control storage, and
  artifact-owned state before it delegates to the unchecked adapter."
  ([holder input output frames sample-rate float-controls int-controls]
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls nil nil))
  ([holder input output frames sample-rate float-controls int-controls lanes]
   (process-checked! holder input output frames sample-rate float-controls
                     int-controls lanes nil))
  ([holder input output frames sample-rate float-controls int-controls lanes
    shared]
   (binding [*precision* (:precision holder)]
     (let [artifact (or (:artifact holder) holder)
           frames (int frames)
           ir (:process-ir artifact)]
       (when (neg? frames)
         (throw (ex-info "DSP process frame count must not be negative"
                         {:frames frames})))
       (check-input! ir input frames)
       (check-buffer! ir output :output frames)
       (check-controls! (:control-layout holder) float-controls int-controls)
       (check-lanes! artifact lanes)
       (check-shared! artifact shared)
       (check-state! (:state-layout holder) (:state holder))
       (process-unchecked! holder input output frames sample-rate
                           float-controls int-controls lanes shared)))))

(defn- kernel-process!
  [kernel multi-channel? input output frames sample-rate controls state lanes
   shared]
  (if (f64?)
    (if multi-channel?
      (.process ^DspMultiKernelD64 kernel input output frames sample-rate
                ^doubles (:float controls) ^ints (:int controls)
                ^doubles (:float state) ^ints (:int state)
                ^doubles lanes ^doubles shared)
      (.process ^DspKernelD64 kernel ^doubles input ^doubles output frames sample-rate
                ^doubles (:float controls) ^ints (:int controls)
                ^doubles (:float state) ^ints (:int state)
                ^doubles lanes ^doubles shared))
    (if multi-channel?
      (.process ^DspMultiKernel kernel input output frames sample-rate
                ^floats (:float controls) ^ints (:int controls)
                ^floats (:float state) ^ints (:int state)
                ^floats lanes ^floats shared)
      (.process ^DspKernel kernel ^floats input ^floats output frames sample-rate
                ^floats (:float controls) ^ints (:int controls)
                ^floats (:float state) ^ints (:int state)
                ^floats lanes ^floats shared))))

(defn- process-callable
  [precision kernel ir layout state control-layout controls-state]
  (fn [input output frames & controls]
    (binding [*precision* precision]
      (when-not (= (count (:controls ir)) (count controls))
        (throw (ex-info "wrong number of arguments for DSP process controls"
                        {:expected (count (:controls ir)) :actual (count controls)})))
      (let [frames (int frames)]
        (when (neg? frames)
          (throw (ex-info "DSP process frame count must not be negative"
                          {:frames frames})))
        (check-input! ir input frames)
        (check-buffer! ir output :output frames)
        (check-state! layout state)
        (write-controls! control-layout controls controls-state)
        (kernel-process! kernel (> (:channels ir) 1) input output frames
                         (if (f64?) 0.0 (float 0.0))
                         controls-state state nil nil)
        nil))))

(defn- process-context-callable
  [precision kernel ir layout state control-layout controls-state]
  (fn [{:keys [input output frames sample-rate controls lanes shared]
        :or {sample-rate 0.0 controls []}}]
    (binding [*precision* precision]
      (when-not (= (count (:controls ir)) (count controls))
        (throw (ex-info "wrong number of arguments for DSP process controls"
                        {:expected (count (:controls ir)) :actual (count controls)})))
      (let [frames (int frames)]
        (when (neg? frames)
          (throw (ex-info "DSP process frame count must not be negative"
                          {:frames frames})))
        (check-input! ir input frames)
        (check-buffer! ir output :output frames)
        (check-state! layout state)
        (write-controls! control-layout controls controls-state)
        (kernel-process! kernel (> (:channels ir) 1) input output frames
                         (if (f64?) (double sample-rate) (float sample-rate))
                         controls-state state lanes shared)
        nil))))

(defn- slot-layout
  [entries start]
  (loop [entries entries
         slot start
         result {}]
    (if-let [entry (first entries)]
      (recur (next entries)
             (+ slot (if (and (= :float (:type entry)) (f64?)) 2 1))
             (assoc result (:name entry) slot))
      [result slot])))

(defn- mangle-id
  "A JVM-legal fragment for a definition ID, for a method name."
  [id]
  (-> (str id)
      (str/replace #"[^A-Za-z0-9_$]" "_")
      (str/replace #"^[0-9]" "_$0")))

(defn- callee-descriptor
  "The JVM descriptor of a callee method: the ABI, then the definition's params.

  A callee that addresses anything by frame is handed the current frame: a
  region-writing callee's buffers are frame-indexed (F2) and a lane read is
  per-frame (F3), and a per-sample callee has no frame local of its own. A
  lane-reading callee is also handed the lane region — one array for the whole
  artifact, since a shared lane is laid out once by the entry — and, when the
  artifact is fused, the base of the slice its call site reads, because that one
  region holds a slice per voice (F4). A callee that needs neither gets neither
  parameter."
  [definition]
  (let [frame? (boolean (or (seq (:region-slots definition))
                            (:lane-region? definition)))
        lane? (boolean (:lane-region? definition))
        lane-base? (boolean (:lane-base? definition))]
    (conj (into [(real-array-class) int-array-class :int :int :int (real-type)]
                (concat (when frame? [:int])
                        (when lane? [(real-array-class)])
                        (when lane-base? [:int])
                        (map (comp jvm-type :type) (:params definition))))
          (jvm-type (:return-type definition)))))

(defn- callee-signatures
  "The method name and descriptor each callable definition is emitted under (D47).

  Every call site reads this one map, so a definition's name does not depend on
  which site calls it, and a nested call inside a callee needs no ordering."
  [ir]
  (into (array-map)
        (map (fn [[id definition]]
               [id {:method (str "dsp$call$" (mangle-id id))
                    :desc (callee-descriptor definition)}]))
        (:definitions ir)))

(defn- callee-slots
  "Local slots for one callee method (D47).

  Slots 0..5 are the ABI: two state arrays, two arena bases, `frames`,
  `sample-rate`. The process method names the same values through its own slots, so
  a call site reads one map either way. The state layout is the definition's own,
  addressed relative to the base registers it was handed. A callee that addresses
  anything by frame takes the current frame after the ABI (F2, F3), and a
  lane-reading one the lane region after that, in the order `callee-descriptor`
  declares them — and, when the artifact is fused, the base of the lane slice it
  reads after that (F4)."
  [definition layout]
  (let [region? (boolean (seq (:region-slots definition)))
        lane? (boolean (:lane-region? definition))
        lane-base? (boolean (:lane-base? definition))
        frame? (or region? lane?)
        frame-slot (+ 5 (real-param-width))
        lane-slot (when lane? (if frame? (inc frame-slot) frame-slot))
        ;; Only a lane reader is handed a slice base, and a lane base implies the
        ;; region it slices, so the base always follows it.
        lane-base-slot (when lane-base? (inc lane-slot))
        first-param (if lane-base?
                      (inc lane-base-slot)
                      (if lane? (inc lane-slot) (if frame? (inc frame-slot) frame-slot)))
        [param-slots next-slot] (slot-layout (:params definition) first-param)
        [local-slots next-slot] (slot-layout (:locals definition) next-slot)]
    (merge {:float-state 0
            :int-state 1
            :base-slots {:float 2 :int 3}
            :frames 4
            :sample-rate 5
            :state-layout (into {}
                                (map (fn [entry]
                                       [(:state-index entry)
                                        (assoc entry
                                               :slot (if (= :float (:storage entry)) 0 1)
                                               :base-slot (if (= :float (:storage entry)) 2 3))])
                                     (:entries layout)))}
           param-slots
           local-slots
           (when frame? {:frame frame-slot})
           (when lane? {:lanes lane-slot})
           (when lane-base? {:lane-base lane-base-slot})
           {:next-temp (atom next-slot)})))

(defn- callee-methods
  "One static method per definition the target was asked to call (D47).

  A method's body is the definition's own IR, emitted with its state addressed
  relative to the two base registers it was handed — which is why one method serves
  every call site even though D19 gives each site its own partition."
  [ir class signatures]
  (mapv (fn [[id {:keys [method desc]}]]
          (let [definition (get (:definitions ir) id)
                slots (assoc (callee-slots definition
                                           (state-layout (:state definition)))
                             :definitions (:definitions ir)
                             :calls signatures
                             :class class)]
            {:flags #{:public :static}
             :name (keyword method)
             :desc desc
             :emit (let [emitted (statement-instructions (:body definition) slots (atom 0) [])]
                     ;; A void module's body ends without a return; the JVM needs one.
                     (if (= :void (:return-type definition))
                       (conj emitted [:return])
                       emitted))}))
        signatures))

(defn compile
  [ir]
  (when (seq (:definitions ir))
    (throw (ex-info "JVM function compilation expects flat IR (D47)"
                    {:definitions (vec (keys (:definitions ir)))
                     :note "a function entry has no arena; lower it flat"})))
  (binding [*precision* (:precision ir)]
    (let [[param-slots next-slot] (slot-layout (:params ir) 0)
          [local-slots next-slot] (slot-layout (:locals ir) next-slot)
          slots (assoc (merge param-slots local-slots)
                       :next-temp (atom next-slot))
          labels (atom 0)
          instructions (statement-instructions (:body ir) slots labels [])
          descriptor (conj (vec (map (comp jvm-type :type) (:params ir)))
                           (jvm-type (:return-type ir)))
          type {:name (class-name (:name ir))
                :flags #{:public :final}
                :methods [{:flags #{:public :static}
                           :name :invoke
                           :desc descriptor
                           :emit instructions}]}
          class (insn/define type)
          method (invoke-method class ir)]
      {:class class :class-name (.getName class) :method method
       :bytes (insn/get-bytes type)
       :invoke (callable (:precision ir) method ir)})))

(defn compile-process
  [ir]
  (let [precision (:precision ir)]
    (binding [*precision* precision]
      (let [channels (:channels ir)
            multi-channel? (> channels 1)
            control-layout (state-layout (:controls ir))
            ;; State and local-array scratch share one arena, so scratch slots
            ;; are additional entries in the same typed arrays. The state-only
            ;; layout is kept as well: it says how much of each physical array a
            ;; reset covers.
            state-only-layout (state-layout (:state ir))
            state-layout (state-layout (concat (:state ir) (:scratch ir)))
            sample-rate-slot 4
            float-control-slot (+ sample-rate-slot (real-param-width))
            int-control-slot (inc float-control-slot)
            float-state-slot (inc int-control-slot)
            int-state-slot (inc float-state-slot)
            ;; The lane and shared regions are the last array parameters, so
            ;; adding them shifts no existing slot number.
            lanes-slot (inc int-state-slot)
            shared-slot (inc lanes-slot)
            first-local-slot (inc shared-slot)
            [control-slots next-slot] (slot-layout (:controls ir) first-local-slot)
            [local-slots next-slot] (slot-layout (:locals ir) next-slot)
            channel-slot (when multi-channel? next-slot)
            frame-slot (if multi-channel? (inc channel-slot) next-slot)
            class-symbol (class-name (symbol (str (name (:name ir)) "_process")))
            ;; A call site and the method it calls read one map (D47).
            signatures (callee-signatures ir)
            ;; The physical state range of every call site that owns a partition,
            ;; in evaluation order -- slot order for a fused process (M6a).
            partitions (partition-slots ir state-layout)
            state-slots (into {}
                              (map (fn [entry]
                                     [(:state-index entry)
                                      (assoc entry :slot (if (= :float (:storage entry))
                                                           float-state-slot int-state-slot))])
                                   (:entries state-layout)))
            slots (merge {:input 1 :output 2 :frames 3 :sample-rate sample-rate-slot
                          :frame frame-slot :multi-channel? multi-channel?
                          :state-layout state-slots
                          :float-state float-state-slot
                          :int-state int-state-slot
                          :class class-symbol
                          :definitions (:definitions ir)
                          :calls signatures}
                         {:lanes lanes-slot :shared shared-slot}
                         (when multi-channel? {:channel channel-slot})
                         control-slots local-slots
                         {:next-temp (atom (inc frame-slot))})
            labels (atom 0)
            frame-body (statement-instructions (:frame-body ir) slots labels [])
            frame-loop (concat [[:iload frame-slot] [:iload 3] [:if-icmpge :frame-done]]
                               frame-body
                               [[:iinc frame-slot 1] [:goto :frame-loop]
                                [:mark :frame-done]])
            ;; One evaluation per frame (D40): no channel loop. A multi-channel
            ;; process writes its output channels explicitly.
            loops (concat [[:iconst-0] [:istore frame-slot] [:mark :frame-loop]]
                          frame-loop)
            control-initializers
            (mapcat (fn [[control {:keys [storage index]}]]
                      (let [array-slot (if (= :float storage)
                                         float-control-slot int-control-slot)
                            load-op (if (= :float storage) (real-load-op) :iaload)
                            store-op (if (= :float storage)
                                       (real-local-store-op) :istore)]
                        [[:aload array-slot] [:ldc index] [load-op]
                         [store-op (get control-slots (:name control))]]))
                    (map vector (:controls ir) (:entries control-layout)))
            input-class (if multi-channel? (real-2d-array-class) (real-array-class))
            interface (if (f64?)
                        (if multi-channel? DspMultiKernelD64 DspKernelD64)
                        (if multi-channel? DspMultiKernel DspKernel))
            type {:name class-symbol
                  :flags #{:public :final}
                  :interfaces [interface]
                  :methods (conj (callee-methods ir class-symbol signatures)
                                 {:flags #{:public}
                                  :name :process
                                  :desc [input-class input-class :int (real-type)
                                         (real-array-class) int-array-class
                                         (real-array-class) int-array-class
                                         (real-array-class)
                                         (real-array-class) :void]
                                  :emit (vec (concat control-initializers loops [[:return]]))})}
            class (insn/define type)
            kernel (.newInstance ^Class class)
            real-array (if (f64?) double-array float-array)
            controls-state {:float (real-array (:float-count control-layout))
                            :int (int-array (:int-count control-layout))}
            ;; One compiled kernel class serves every instance: DSP state lives in
            ;; the arrays handed to `process`, never in the kernel object, so a
            ;; fresh instance is a fresh state region and nothing is recompiled.
            create-instance
            (fn []
              (let [state {:float (real-array (:float-count state-layout))
                           :int (int-array (:int-count state-layout))}
                    ;; The factory runs long after `compile-process` returns, so
                    ;; the precision the layout helpers read is rebound here.
                    f64-precision (binding [*precision* precision]
                                    (reset-state! ir state-layout state)
                                    (boolean (f64?)))
                    ;; The initial state, captured once and copied back by
                    ;; `reset`. Re-deriving it from the IR costs this JVM about
                    ;; 50 KB a call -- it is a `doseq` over lazy seqs -- which no
                    ;; lifecycle operation in the audio path can afford, and a
                    ;; voice steal is a reset. State slots come first in the
                    ;; arena, so the prefix of each physical array is exactly
                    ;; what `reset-state!` writes; scratch is left alone.
                    state-floats (int (:float-count state-only-layout))
                    state-ints (int (:int-count state-only-layout))
                    initial-float (if f64-precision
                                    (java.util.Arrays/copyOfRange
                                     ^doubles (:float state) 0 state-floats)
                                    (java.util.Arrays/copyOfRange
                                     ^floats (:float state) 0 state-floats))
                    initial-int (java.util.Arrays/copyOfRange
                                 ^ints (:int state) 0 state-ints)
                    ;; One slot's initial state, per call partition: an instance
                    ;; that holds several slots restores exactly one this way,
                    ;; which is what keeps a reset from disturbing its neighbours.
                    partition-resets
                    (mapv (fn [partition]
                            (let [float-base (long (:float-base partition))
                                  float-count (long (:float-count partition))
                                  int-base (long (:int-base partition))
                                  int-count (long (:int-count partition))]
                              {:float-base float-base
                               :int-base int-base
                               :float (if f64-precision
                                        (java.util.Arrays/copyOfRange
                                         ^doubles initial-float
                                         (int float-base) (int (+ float-base float-count)))
                                        (java.util.Arrays/copyOfRange
                                         ^floats initial-float
                                         (int float-base) (int (+ float-base float-count))))
                               :int (java.util.Arrays/copyOfRange
                                     ^ints initial-int
                                     (int int-base) (int (+ int-base int-count)))}))
                          partitions)]
                {:state state
                 :state-layout state-layout
                 :partitions partitions
                 :control-layout control-layout
                 :precision precision
                 :multi-channel? multi-channel?
                 :reset-partition
                 (fn [slot]
                   (let [partition (nth partition-resets (int slot))
                         float-slice (:float partition)
                         int-slice (:int partition)]
                     (if f64-precision
                       (System/arraycopy ^doubles float-slice 0 ^doubles (:float state)
                                         (int (:float-base partition))
                                         (int (alength ^doubles float-slice)))
                       (System/arraycopy ^floats float-slice 0 ^floats (:float state)
                                         (int (:float-base partition))
                                         (int (alength ^floats float-slice))))
                     (System/arraycopy ^ints int-slice 0 ^ints (:int state)
                                       (int (:int-base partition))
                                       (int (alength ^ints int-slice)))))
                 :reset (fn []
                          (if f64-precision
                            (System/arraycopy ^doubles initial-float 0
                                              ^doubles (:float state) 0 state-floats)
                            (System/arraycopy ^floats initial-float 0
                                              ^floats (:float state) 0 state-floats))
                          (System/arraycopy ^ints initial-int 0
                                            ^ints (:int state) 0 state-ints))
                 :destroy (fn []
                            (binding [*precision* precision]
                              (if (f64?)
                                (do (java.util.Arrays/fill ^doubles (:float state) 0.0)
                                    (java.util.Arrays/fill ^ints (:int state) 0))
                                (do (java.util.Arrays/fill ^floats (:float state) (float 0.0))
                                    (java.util.Arrays/fill ^ints (:int state) 0)))))
                 :has-lanes? (boolean (seq (:lanes ir)))
                 :has-shared? (boolean (seq (:shared ir)))
                 :unchecked (unchecked-adapter precision kernel multi-channel? state)
                 :invoke (process-callable precision kernel ir state-layout state
                                           control-layout controls-state)
                 :invoke-context (process-context-callable precision kernel ir state-layout state
                                                           control-layout controls-state)}))
            instance (create-instance)]
        (merge instance
               {:class class :class-name (.getName class) :kernel kernel
                :bytes (insn/get-bytes type)
                :controls controls-state
                :process-ir ir
                :create-instance create-instance})))))
