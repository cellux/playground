(ns omkamra.dsp.interpreter
  "Portable reference interpreter for portable DSP IR.

  This is the executable semantic specification for the DSP language.  It is
  a `.cljc` namespace with a small platform abstraction so the same code runs on
  the JVM (over primitive `float`/`double` arrays) and in ClojureScript (over
  `Float32Array`/`Float64Array`), which lets the browser compute expected
  outputs from compiled IR instead of trusting vectors shipped from the JVM.")

(def ^:dynamic *precision* nil)

(defn- precision!
  [ir]
  (case (:precision ir)
    :f32 :f32
    :f64 :f64
    (throw (ex-info "interpreter requires explicit DSP precision"
                    {:precision (:precision ir)}))))

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "interpreter precision is not bound"
                    {:precision *precision*}))))

;; ---------------------------------------------------------------------------
;; Platform abstraction: buffers and numeric narrowing
;; ---------------------------------------------------------------------------

#?(:clj (def ^:private float-array-class (Class/forName "[F")))
#?(:clj (def ^:private double-array-class (Class/forName "[D")))
#?(:clj (def ^:private float-2d-array-class (Class/forName "[[F")))
#?(:clj (def ^:private double-2d-array-class (Class/forName "[[D")))
#?(:clj (def ^:private object-array-class (Class/forName "[Ljava.lang.Object;")))

(defn- real-buffer?
  [value]
  #?(:clj (or (instance? float-array-class value)
              (instance? double-array-class value))
     :cljs (or (instance? js/Float32Array value)
               (instance? js/Float64Array value))))

(defn- buffer-array?
  [value]
  #?(:clj (or (instance? float-2d-array-class value)
              (instance? double-2d-array-class value))
     :cljs (and (js/Array.isArray value)
                (every? real-buffer? (array-seq value)))))

(defn- object-buffer?
  [value]
  #?(:clj (instance? object-array-class value)
     :cljs (js/Array.isArray value)))

(defn- make-real-array
  [values]
  #?(:clj (if (f64?) (double-array values) (float-array values))
     :cljs (if (f64?)
             (js/Float64Array. (to-array values))
             (js/Float32Array. (to-array values)))))

(defn- make-object-array
  [values]
  #?(:clj (object-array values)
     :cljs (to-array values)))

(defn- set-real!
  [buffer index value]
  #?(:clj (if (instance? double-array-class buffer)
            (aset-double ^doubles buffer index (double value))
            (aset-float ^floats buffer index (float value)))
     :cljs (aset buffer index value)))

(defn- set-object!
  [buffer index value]
  #?(:clj (aset ^objects buffer index value)
     :cljs (aset buffer index value)))

;; `(float x)` range-checks and throws on infinity, while the f32 contract is
;; IEEE-754, so narrowing uses `unchecked-float` on the JVM.
(defn- logical-real
  [value]
  #?(:clj (if (f64?) (double value) (unchecked-float value))
     :cljs (if (f64?) value (js/Math.fround value))))

(defn logical-float
  [value]
  #?(:clj (unchecked-float value)
     :cljs (js/Math.fround value)))

(defn logical-int
  [value]
  #?(:clj (int value)
     :cljs (bit-or value 0)))

(defn- int32
  "Narrow to a signed two's-complement 32-bit integer."
  [value]
  #?(:clj (unchecked-int value)
     :cljs (bit-or value 0)))

(defn logical-boolean
  [value]
  (if (boolean? value)
    value
    (throw (ex-info "DSP boolean value must be true or false" {:value value}))))

(defn- coerce-value
  [type value]
  (case type
    :float (logical-real value)
    :int (logical-int value)
    :boolean (logical-boolean value)
    (throw (ex-info "unknown DSP logical type" {:type type :value value}))))

;; ---------------------------------------------------------------------------
;; Builtin reference semantics
;; ---------------------------------------------------------------------------

(defn- math-abs [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- math-abs-int [x] #?(:clj (Math/abs (int x)) :cljs (js/Math.abs x)))
(defn- math-min [a b] #?(:clj (Math/min (double a) (double b)) :cljs (js/Math.min a b)))
(defn- math-max [a b] #?(:clj (Math/max (double a) (double b)) :cljs (js/Math.max a b)))
(defn- math-min-int [a b] #?(:clj (Math/min (int a) (int b)) :cljs (js/Math.min a b)))
(defn- math-max-int [a b] #?(:clj (Math/max (int a) (int b)) :cljs (js/Math.max a b)))
(defn- math-floor [x] #?(:clj (Math/floor (double x)) :cljs (js/Math.floor x)))
(defn- math-ceil [x] #?(:clj (Math/ceil (double x)) :cljs (js/Math.ceil x)))
(defn- math-sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))

(defn- math-copy-sign
  [x y]
  #?(:clj (Math/copySign (double x) (double y))
     :cljs (if (or (neg? y) (and (zero? y) (neg? (/ 1 y))))
             (- (js/Math.abs x))
             (js/Math.abs x))))

(defn- truncate
  [x]
  (if (neg? x) (math-ceil x) (math-floor x)))

(defmulti ^:private evaluate-builtin
  "Reference semantics for primitive builtins, dispatched on [operator type]."
  (fn [operator type _args] [operator type]))

(defmethod evaluate-builtin :default
  [operator type _args]
  (throw (ex-info "DSP builtin has no interpreter implementation"
                  {:operator operator :type type})))

(defmethod evaluate-builtin [:abs :float] [_ _ [x]] (math-abs x))
(defmethod evaluate-builtin [:abs :int] [_ _ [x]] (math-abs-int x))
(defmethod evaluate-builtin [:min :float] [_ _ [x y]] (math-min x y))
(defmethod evaluate-builtin [:min :int] [_ _ [x y]] (math-min-int x y))
(defmethod evaluate-builtin [:max :float] [_ _ [x y]] (math-max x y))
(defmethod evaluate-builtin [:max :int] [_ _ [x y]] (math-max-int x y))
(defmethod evaluate-builtin [:floor :float] [_ _ [x]] (math-floor x))
(defmethod evaluate-builtin [:ceil :float] [_ _ [x]] (math-ceil x))
(defmethod evaluate-builtin [:trunc :float] [_ _ [x]] (truncate x))
(defmethod evaluate-builtin [:sqrt :float] [_ _ [x]] (math-sqrt x))
(defmethod evaluate-builtin [:copysign :float] [_ _ [x y]] (math-copy-sign x y))
;; `rem` is the truncated remainder on every target.  ClojureScript's
;; `unchecked-remainder-int` is floored (it uses `mod`), so the reference uses
;; `rem` and narrows explicitly.
(defmethod evaluate-builtin [:rem :int]
  [_ _ [x y]]
  (if (zero? (logical-int y))
    0
    (int32 (rem (logical-int x) (logical-int y)))))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn- check-arity!
  [ir args]
  (when-not (= (count (:params ir)) (count args))
    (throw (ex-info "wrong number of arguments for DSP function"
                    {:expected (count (:params ir))
                     :actual (count args)
                     :name (:name ir)}))))

(defn- check-control-arity!
  [ir controls]
  (when-not (= (count (:controls ir)) (count controls))
    (throw (ex-info "wrong number of arguments for DSP process controls"
                    {:expected (count (:controls ir))
                     :actual (count controls)
                     :name (:name ir)}))))

(defn- check-real-buffer!
  [value name]
  (when-not (real-buffer? value)
    (throw (ex-info (if (f64?) "DSP process expects primitive double arrays"
                        "DSP process expects primitive float arrays")
                    {:argument name :value value}))))

(defn- check-buffer!
  [ir value name frames]
  (let [required-length (+ frames (get-in ir [:memory name :max-frame-offset] 0))]
    (if (= 1 (:channels ir))
      (do
        (check-real-buffer! value name)
        (when (< (alength value) required-length)
          (throw (ex-info "DSP buffer is shorter than the frame-access requirement"
                          {:argument name :frames frames :required-length required-length
                           :length (alength value)}))))
      (do
        (when-not (buffer-array? value)
          (throw (ex-info (if (f64?) "DSP multi-channel process expects double[][] buffers"
                              "DSP multi-channel process expects float[][] buffers")
                          {:argument name :value value})))
        (when-not (= (:channels ir) (alength value))
          (throw (ex-info "DSP buffer has the wrong channel count"
                          {:argument name :expected (:channels ir)
                           :actual (alength value)})))
        (dotimes [channel (:channels ir)]
          (let [buffer (aget value channel)]
            (check-real-buffer! buffer [name channel])
            (when (< (alength buffer) required-length)
              (throw (ex-info "DSP buffer channel is shorter than the frame-access requirement"
                              {:argument [name channel]
                               :frames frames :required-length required-length
                               :length (alength buffer)})))))))))

(defn- state-size
  [state]
  (or (:size state) 1))

(defn- state-elements
  [states]
  (mapcat (fn [{:keys [init] :as state}]
            (if (= 1 (state-size state))
              [init]
              init))
          states))

(defn- float-state?
  [ir]
  (every? #(= :float (:type %)) (:state ir)))

(defn- state-length
  [ir]
  (reduce + 0 (map state-size (:state ir))))

(defn- check-state!
  [ir state]
  (if (float-state? ir)
    (do
      (when-not (real-buffer? state)
        (throw (ex-info "DSP process state has the wrong representation"
                        {:expected :float-array :value state :name (:name ir)})))
      (when-not (= (state-length ir) (alength state))
        (throw (ex-info "DSP process state has the wrong size"
                        {:expected (state-length ir)
                         :actual (alength state)
                         :name (:name ir)}))))
    (do
      (when-not (object-buffer? state)
        (throw (ex-info "DSP process state has the wrong representation"
                        {:expected :object-array :value state :name (:name ir)})))
      (when-not (= (state-length ir) (alength state))
        (throw (ex-info "DSP process state has the wrong size"
                        {:expected (state-length ir)
                         :actual (alength state)
                         :name (:name ir)})))))
  state)

(defn- state-value
  [state index]
  (aget state index))

(defn- set-state-value!
  [state index type value]
  (let [value (coerce-value type value)]
    (if (real-buffer? state)
      (set-real! state index value)
      (set-object! state index value))
    nil))

(defn- check-block!
  [ir input output frames state]
  (let [frames (logical-int frames)]
    (when (neg? frames)
      (throw (ex-info "DSP process frame count must not be negative"
                      {:frames frames :name (:name ir)})))
    (check-buffer! ir input :input frames)
    (check-buffer! ir output :output frames)
    (check-state! ir state)
    frames))

;; ---------------------------------------------------------------------------
;; State lifecycle
;; ---------------------------------------------------------------------------

(defn create-state
  [ir]
  (binding [*precision* (precision! ir)]
    (if (float-state? ir)
      (make-real-array (state-elements (:state ir)))
      (make-object-array
       (mapcat (fn [{:keys [type] :as state}]
                 (map #(coerce-value type %)
                      (if (= 1 (state-size state))
                        [(:init state)]
                        (:init state))))
               (:state ir))))))

(defn reset-state!
  [ir state]
  (binding [*precision* (precision! ir)]
    (check-state! ir state)
    (doseq [{:keys [index type init] :as state-description} (:state ir)
            [element-index value] (map-indexed vector
                                               (if (= 1 (state-size state-description))
                                                 [init]
                                                 init))]
      (set-state-value! state (+ index element-index) type value))
    nil))

;; ---------------------------------------------------------------------------
;; Evaluation
;; ---------------------------------------------------------------------------

(defn- evaluate-binary
  [type operator left right]
  (case type
    :float
    (logical-real
     (case operator
       :+ (+ (double left) (double right))
       :- (- (double left) (double right))
       :* (* (double left) (double right))
       :/ (/ (double left) (double right))))
    :int
    (case operator
      :+ (unchecked-add-int left right)
      :- (unchecked-subtract-int left right)
      :* (unchecked-multiply-int left right)
      ;; Total and wrapping (D12): division by zero yields zero, and
      ;; INT_MIN / -1 wraps to INT_MIN rather than overflowing.
      :/ (if (zero? right)
           0
           ;; Wrapped explicitly: ClojureScript's `unchecked-divide-int` does
           ;; not narrow `INT_MIN / -1` to 32 bits.
           (int32 (unchecked-divide-int left right))))))

(defn- evaluate-comparison
  [operator left right]
  (case operator
    := (= left right)
    :not= (not= left right)
    :< (< left right)
    :<= (<= left right)
    :> (> left right)
    :>= (>= left right)))

(declare evaluate-expression execute-statement)

;; Bitwise operations use signed two's-complement 32-bit semantics on every
;; target.  Clojure's `bit-shift-*` operate on 64-bit boxed longs and do not
;; mask the shift distance the way JVM/JS/Wasm i32 shifts do, so the reference
;; implementation masks the distance to five bits and narrows results with
;; `int32`.  Logical right shift first reinterprets the operand as an unsigned
;; 32-bit value to avoid 64-bit sign extension.
(defn- shift-distance
  [value]
  (bit-and (logical-int value) 0x1f))

(defn- evaluate-bitwise
  [expression locals context]
  (let [args (mapv #(logical-int (evaluate-expression % locals context))
                   (:args expression))]
    (int32
     (case (:operator expression)
       :bit-and (bit-and (nth args 0) (nth args 1))
       :bit-or (bit-or (nth args 0) (nth args 1))
       :bit-xor (bit-xor (nth args 0) (nth args 1))
       :bit-not (bit-not (nth args 0))
       :bit-shift-left (bit-shift-left (nth args 0) (shift-distance (nth args 1)))
       :bit-shift-right (bit-shift-right (nth args 0) (shift-distance (nth args 1)))
       :unsigned-bit-shift-right (unsigned-bit-shift-right
                                  (bit-and (nth args 0) 0xffffffff)
                                  (shift-distance (nth args 1)))))))

(defn- state-element-index
  [expression locals context]
  (let [element-index (if-let [index (:element-index expression)]
                        (logical-int (evaluate-expression index locals context))
                        0)
        size (or (:size expression) 1)]
    (when (or (neg? element-index) (>= element-index size))
      (throw (ex-info "DSP aggregate state index is out of bounds"
                      {:state-id (:state-id expression)
                       :index element-index :size size})))
    (+ (:index expression) element-index)))

(defn- buffer-index
  [expr locals context]
  (case (:op expr)
    :frame (:frame context)
    :frame-offset (+ (:frame context) (:offset expr))
    :frame-plus (+ (:frame context)
                   (logical-int (evaluate-expression (:offset expr) locals context)))
    (logical-int (evaluate-expression expr locals context))))

(defn- buffer-channel
  [channel locals context]
  (if (= :channel (:op channel))
    (:channel context)
    (if (map? channel)
      (logical-int (evaluate-expression channel locals context))
      channel)))

(defn- context-buffer
  [operation locals context]
  (let [buffer (get context (:buffer-id operation))
        channel (buffer-channel (:channel operation) locals context)]
    (when-not buffer
      (throw (ex-info "unknown DSP IR buffer" {:buffer-id (:buffer-id operation)})))
    (when (or (neg? channel) (>= channel (:channels context)))
      (throw (ex-info "DSP buffer channel is out of bounds"
                      {:buffer-id (:buffer-id operation) :channel channel
                       :channels (:channels context)})))
    (if (= 1 (:channels context))
      buffer
      (aget buffer channel))))

(defn- checked-buffer-index
  [operation locals context]
  (let [buffer (context-buffer operation locals context)
        index (buffer-index (:index operation) locals context)]
    (when (and (= :development (:checks context))
               (or (neg? index) (>= index (alength buffer))))
      (throw (ex-info "DSP buffer index is out of bounds"
                      {:buffer-id (:buffer-id operation) :index index
                       :length (alength buffer) :frame (:frame context)})))
    [buffer index]))

(defn- evaluate-call
  [expression locals context]
  (let [callee (get-in context [:definitions (:function-id expression)])]
    (when-not callee
      (throw (ex-info "DSP IR call refers to an unknown function"
                      {:function-id (:function-id expression)})))
    (let [arguments (mapv #(evaluate-expression % locals context) (:args expression))
          callee-locals (volatile!
                         (into {}
                               (map (fn [{:keys [name type]} value]
                                      [name (coerce-value type value)])
                                    (:params callee) arguments)))
          result (execute-statement (:body callee) callee-locals context)]
      (coerce-value (:return-type callee) (:value result)))))

(defn- evaluate-expression
  [expression locals context]
  (case (:op expression)
    :const (:value expression)
    :local (get @locals (:name expression))
    :state-load (coerce-value (:type expression)
                              (state-value (:state context)
                                           (state-element-index expression locals context)))
    :frame (:frame context)
    :frames (:frames context)
    :sample-rate (:sample-rate context)
    :frame-offset (+ (:frame context) (:offset expression))
    :frame-plus (buffer-index expression locals context)
    :channel (:channel context)
    :buffer-load (let [[buffer index] (checked-buffer-index expression locals context)]
                   (aget buffer index))
    :call (evaluate-call expression locals context)
    :binary (evaluate-binary (:type expression) (:operator expression)
                             (evaluate-expression (:left expression) locals context)
                             (evaluate-expression (:right expression) locals context))
    :convert (coerce-value (:type expression)
                           (evaluate-expression (:value expression) locals context))
    :compare (evaluate-comparison
              (:operator expression)
              (evaluate-expression (:left expression) locals context)
              (evaluate-expression (:right expression) locals context))
    :logical (case (:operator expression)
               :not (not (evaluate-expression (first (:args expression)) locals context))
               :and (and (evaluate-expression (first (:args expression)) locals context)
                         (evaluate-expression (second (:args expression)) locals context))
               :or (or (evaluate-expression (first (:args expression)) locals context)
                       (evaluate-expression (second (:args expression)) locals context)))
    :bitwise (evaluate-bitwise expression locals context)
    :builtin (coerce-value
              (:type expression)
              (evaluate-builtin
               (:builtin expression)
               (:type expression)
               (mapv #(evaluate-expression % locals context) (:args expression))))
    (throw (ex-info "unknown DSP IR expression" {:expression expression}))))

(defn- control
  [kind & [value]]
  {:control kind :value value})

(defn- set-local!
  "Associate `value` with `name` in the `locals` volatile.

  ClojureScript's persistent maps short-circuit `assoc` when the new value is
  `identical?` (JavaScript `===`) to the stored one.  Because JavaScript says
  `0 === -0`, overwriting a `+0.0` local with `-0.0` (or the reverse) would
  silently keep the old sign.  Dropping the key first forces a genuine
  insertion; the JVM has no such fast path, so it keeps the direct `assoc`."
  [locals name value]
  (vreset! locals
           #?(:clj (assoc @locals name value)
              :cljs (assoc (dissoc @locals name) name value)))
  nil)

(defn- execute-block
  [statements locals context]
  (loop [statements statements]
    (if-let [statement (first statements)]
      (if-let [result (execute-statement statement locals context)]
        result
        (recur (next statements)))
      nil)))

(defn- execute-statement
  [statement locals context]
  (case (:op statement)
    :block (execute-block (:statements statement) locals context)
    :declare (set-local! locals (:name statement)
                         (if-let [init (:init statement)]
                           (coerce-value (:type statement)
                                         (evaluate-expression init locals context))
                           (case (:type statement)
                             :float (logical-real 0.0)
                             :int (logical-int 0)
                             :boolean false)))
    :assign (set-local! locals (:name statement)
                        (coerce-value (:type statement)
                                      (evaluate-expression (:value statement)
                                                           locals context)))
    :expression (do (evaluate-expression (:value statement) locals context) nil)
    :if (execute-statement
         (if (evaluate-expression (:condition statement) locals context)
           (:then statement)
           (:else statement))
         locals context)
    :while (loop [iterations 0]
             (if-not (evaluate-expression (:condition statement) locals context)
               nil
               (do
                 (when (and (:bound statement) (>= iterations (:bound statement)))
                   (throw (ex-info "DSP loop exceeded its iteration bound"
                                   {:bound (:bound statement)})))
                 (let [result (execute-statement (:body statement) locals context)]
                   (case (:control result)
                     :break nil
                     :continue (recur (inc iterations))
                     :return result
                     (recur (inc iterations)))))))
    :break (control :break)
    :continue (control :continue)
    :return (control :return (evaluate-expression (:value statement) locals context))
    :buffer-store (let [[buffer index] (checked-buffer-index statement locals context)
                        value (logical-real
                               (evaluate-expression (:value statement)
                                                    locals context))]
                    (set-real! buffer index value)
                    nil)
    :state-store (do
                   (set-state-value! (:state context)
                                     (state-element-index statement locals context)
                                     (:type statement)
                                     (evaluate-expression (:value statement)
                                                          locals context))
                   nil)
    (throw (ex-info "unknown DSP IR statement" {:statement statement}))))

;; ---------------------------------------------------------------------------
;; Public entry points
;; ---------------------------------------------------------------------------

(defn invoke
  [ir args]
  (binding [*precision* (precision! ir)]
    (check-arity! ir args)
    (when (seq (:state ir))
      (throw (ex-info "stateful DSP definitions require the :process entry"
                      {:name (:name ir)})))
    (let [locals (volatile!
                  (into {}
                        (map (fn [{:keys [name type]} value]
                               [name (coerce-value type value)])
                             (:params ir) args)))
          result (execute-statement (:body ir) locals
                                    {:definitions (:definitions ir)})]
      (coerce-value (:return-type ir) (:value result)))))

(defn process-context
  "Execute a process IR against explicit runtime values.

  `:sample-rate` is part of the portable ABI even though the legacy array
  callable does not pass it. `:controls` is a vector in process-signature
  order. This function is the reference implementation used by richer host
  adapters and by the browser conformance runner."
  [ir {:keys [input output frames controls state sample-rate]
       :or {controls [] sample-rate 0.0}}]
  (binding [*precision* (precision! ir)]
    (check-control-arity! ir controls)
    (let [frames (check-block! ir input output frames state)
          sample-rate (coerce-value :float sample-rate)
          control-locals (into {}
                               (map (fn [{:keys [name type]} value]
                                      [name (coerce-value type value)])
                                    (:controls ir) controls))]
      (dotimes [channel (:channels ir)]
        (dotimes [frame frames]
          (execute-statement
           (:frame-body ir)
           (volatile! control-locals)
           {:input input
            :output output
            :channels (:channels ir)
            :channel channel
            :frame frame
            :frames frames
            :sample-rate sample-rate
            :checks (get-in ir [:options :checks] :development)
            :state state
            :definitions (:definitions ir)})))
      nil)))

(defn process
  [ir input output frames controls state]
  (process-context ir {:input input
                       :output output
                       :frames frames
                       :controls controls
                       :state state}))

