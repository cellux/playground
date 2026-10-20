(ns omkamra.dsp.targets.interpreter
  "Portable reference interpreter for portable DSP IR.

  This is the executable semantic specification for the DSP language.  It is
  a `.cljc` namespace with a small platform abstraction so the same code runs on
  the JVM (over primitive `float`/`double` arrays) and in ClojureScript (over
  `Float32Array`/`Float64Array`), which lets the browser compute expected
  outputs from compiled IR instead of trusting vectors shipped from the JVM.")

;; The current kernel precision, a keyword or nil. It is fixed for a whole
;; entry-point call, but it used to be a dynamic var, and reading one allocates a
;; thread-binding map entry on every access -- `f64?` is called constantly, so
;; that came to dominate the interpreter's allocation. A thread-local (JVM) /
;; volatile (CLJS) with an explicit save/restore is equivalent to `binding`
;; without the per-read cost.
(def ^:private precision-holder
  #?(:clj (ThreadLocal.)
     :cljs (volatile! nil)))

;; Whether the instance arena (persistent state plus scratch) is a primitive
;; real array rather than an object array. Bound once per process call so state
;; access can use a primitive `aget` instead of an unhinted one, which the
;; compiler turns into a reflective `RT.aget` at ~24 KB per call (D26).
(def ^:private ^:dynamic *real-state* true)

(defn- current-precision
  []
  #?(:clj (.get ^ThreadLocal precision-holder)
     :cljs (deref precision-holder)))

(defn- set-precision!
  [precision]
  #?(:clj (.set ^ThreadLocal precision-holder precision)
     :cljs (vreset! precision-holder precision)))

(defn- with-dsp-precision
  "Run `thunk` with `precision` current, restoring the previous value. The
  save/restore discipline of `binding`, without the dynamic-var read cost."
  [precision thunk]
  (let [previous (current-precision)]
    (set-precision! precision)
    (try
      (thunk)
      (finally
        (set-precision! previous)))))

(defn- precision!
  [ir]
  (case (:precision ir)
    :f32 :f32
    :f64 :f64
    (throw (ex-info "interpreter requires explicit DSP precision"
                    {:precision (:precision ir)}))))

(defn- f64?
  []
  (case (current-precision)
    :f64 true
    :f32 false
    (throw (ex-info "interpreter precision is not bound"
                    {:precision (current-precision)}))))

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

;; Buffers (input, output, lanes) are always primitive real arrays at the
;; kernel's precision, so their type is known and `aget`/`alength` can be
;; primitive. Without the hint they are reflective too.
(defn- buffer-value
  [buffer index]
  #?(:clj (if (f64?)
            (aget ^doubles buffer (int index))
            (aget ^floats buffer (int index)))
     :cljs (aget buffer index)))

(defn- buffer-length
  [buffer]
  #?(:clj (if (f64?)
            (alength ^doubles buffer)
            (alength ^floats buffer))
     :cljs (alength buffer)))

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
    ;; A process-shaped callee writes its own region buffers and returns nothing
    ;; (F2); a `:void` result is no value at all.
    :void nil
    ;; A record value is a Clojure map of leaf path -> value; nothing to narrow.
    :record value
    (throw (ex-info "unknown DSP logical type" {:type type :value value}))))

;; ---------------------------------------------------------------------------
;; Local environments
;; ---------------------------------------------------------------------------
;;
;; Locals live in one flat mutable map. The IR guarantees that a local's name is
;; unique across the whole compilation unit (see
;; `omkamra.dsp.ir/unique-local-name`), so a single map is equivalent to the
;; persistent map the interpreter used to thread -- without copying a HAMT on
;; every assignment, which dominated the interpreter's allocation.
;;
;; Keys are name *strings*: ClojureScript's `js/Map` compares object keys by
;; identity, and two `cljs.core/Symbol`s for the same name are not identical, so
;; symbol keys would silently miss.

(defn- locals-get
  [locals local-name]
  #?(:clj (.get ^java.util.HashMap locals (clojure.core/name local-name))
     :cljs (.get locals (clojure.core/name local-name))))

(defn- locals-put!
  [locals local-name value]
  #?(:clj (.put ^java.util.HashMap locals (clojure.core/name local-name) value)
     :cljs (.set locals (clojure.core/name local-name) value))
  nil)

#_{:clj-kondo/ignore [:unused-binding]}
(defn- empty-locals
  ([]
   #?(:clj (java.util.HashMap.)
      :cljs (js/Map.)))
  ([capacity]
   ;; `capacity` sizes the JVM `HashMap`; a ClojureScript `js/Map` takes none.
   #?(:clj (java.util.HashMap. (int (clojure.core/max 4 capacity)))
      :cljs (js/Map.))))

(defn- seeded-locals
  "A mutable environment with each `entry` (`{:name ... :type ...}`) bound to
  the value at the same position in `values`."
  [entries values]
  (let [entry-count (clojure.core/count entries)
        locals (empty-locals entry-count)]
    (dotimes [index entry-count]
      (let [entry (nth entries index)]
        (locals-put! locals (:name entry)
                     (coerce-value (:type entry) (nth values index)))))
    locals))

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
(defn- math-exp [x] #?(:clj (Math/exp (double x)) :cljs (js/Math.exp x)))
(defn- math-sin [x] #?(:clj (Math/sin (double x)) :cljs (js/Math.sin x)))
(defn- math-cos [x] #?(:clj (Math/cos (double x)) :cljs (js/Math.cos x)))
(defn- math-tan [x] #?(:clj (Math/tan (double x)) :cljs (js/Math.tan x)))
(defn- math-asin [x] #?(:clj (Math/asin (double x)) :cljs (js/Math.asin x)))
(defn- math-acos [x] #?(:clj (Math/acos (double x)) :cljs (js/Math.acos x)))
(defn- math-atan [x] #?(:clj (Math/atan (double x)) :cljs (js/Math.atan x)))
(defn- math-atan2 [y x] #?(:clj (Math/atan2 (double y) (double x)) :cljs (js/Math.atan2 y x)))
(defn- math-log [x] #?(:clj (Math/log (double x)) :cljs (js/Math.log x)))
(defn- math-pow [x y] #?(:clj (Math/pow (double x) (double y)) :cljs (js/Math.pow x y)))
(defn- math-sinh [x] #?(:clj (Math/sinh (double x)) :cljs (js/Math.sinh x)))
(defn- math-cosh [x] #?(:clj (Math/cosh (double x)) :cljs (js/Math.cosh x)))
(defn- math-tanh [x] #?(:clj (Math/tanh (double x)) :cljs (js/Math.tanh x)))

(defn- math-copy-sign
  [x y]
  #?(:clj (Math/copySign (double x) (double y))
     :cljs (if (or (neg? y) (and (zero? y) (neg? (/ 1 y))))
             (- (js/Math.abs x))
             (js/Math.abs x))))

(defn- truncate
  [x]
  (if (neg? x) (math-ceil x) (math-floor x)))

;; Reference semantics for primitive builtins. This was a multimethod dispatched
;; on `[operator type]`, but that allocated a dispatch vector and a `mapv` of
;; arguments on every builtin evaluation -- 24% of the interpreter's allocation
;; -- so it is a plain `case`. Argument evaluation lives in the caller, which
;; passes the (already evaluated) operands positionally; the reference emits at
;; most two, so one `y` covers every primitive.
(defn- evaluate-builtin
  [operator type x y]
  (case operator
    :abs (if (= type :int) (math-abs-int x) (math-abs x))
    :min (if (= type :int) (math-min-int x y) (math-min x y))
    :max (if (= type :int) (math-max-int x y) (math-max x y))
    :floor (math-floor x)
    :ceil (math-ceil x)
    :trunc (truncate x)
    :sqrt (math-sqrt x)
    :exp (math-exp x)
    :sin (math-sin x)
    :cos (math-cos x)
    :tan (math-tan x)
    :asin (math-asin x)
    :acos (math-acos x)
    :atan (math-atan x)
    :atan2 (math-atan2 x y)
    :log (math-log x)
    :pow (math-pow x y)
    :sinh (math-sinh x)
    :cosh (math-cosh x)
    :tanh (math-tanh x)
    :copysign (math-copy-sign x y)
    ;; `rem` is the truncated remainder on every target.  ClojureScript's
    ;; `unchecked-remainder-int` is floored (it uses `mod`), so the reference
    ;; uses `rem` and narrows explicitly.
    :rem (if (zero? (logical-int y))
           0
           (int32 (rem (logical-int x) (logical-int y))))
    (throw (ex-info "DSP builtin has no interpreter implementation"
                    {:operator operator :type type}))))

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

(defn- arena-slots
  "Persistent state followed by local-array scratch: one instance arena."
  [ir]
  (concat (:state ir) (:scratch ir)))

(defn- float-arena?
  [ir]
  (every? #(= :float (:type %)) (arena-slots ir)))

(defn- arena-length
  [ir]
  (reduce + 0 (map state-size (arena-slots ir))))

(defn- check-state!
  [ir state]
  (if (float-arena? ir)
    (do
      (when-not (real-buffer? state)
        (throw (ex-info "DSP process state has the wrong representation"
                        {:expected :float-array :value state :name (:name ir)})))
      (when-not (= (arena-length ir) (alength state))
        (throw (ex-info "DSP process state has the wrong size"
                        {:expected (arena-length ir)
                         :actual (alength state)
                         :name (:name ir)}))))
    (do
      (when-not (object-buffer? state)
        (throw (ex-info "DSP process state has the wrong representation"
                        {:expected :object-array :value state :name (:name ir)})))
      (when-not (= (arena-length ir) (alength state))
        (throw (ex-info "DSP process state has the wrong size"
                        {:expected (arena-length ir)
                         :actual (alength state)
                         :name (:name ir)})))))
  state)

(defn- state-value
  [state index]
  #?(:clj (if *real-state*
            (if (f64?)
              (aget ^doubles state (int index))
              (aget ^floats state (int index)))
            (aget ^objects state (int index)))
     :cljs (aget state index)))

(defn- set-state-value!
  [state index type value]
  (let [value (coerce-value type value)]
    (if *real-state*
      (set-real! state index value)
      (set-object! state index value))
    nil))

(defn- check-block!
  [ir input output frames state]
  (let [frames (logical-int frames)]
    (when (neg? frames)
      (throw (ex-info "DSP process frame count must not be negative"
                      {:frames frames :name (:name ir)})))
    ;; A generator (D23) declares no input buffer, so there is nothing to check
    ;; and no input value to thread through.
    (when (some #(= :input (:direction %)) (:buffers ir))
      (check-buffer! ir input :input frames))
    (check-buffer! ir output :output frames)
    (check-state! ir state)
    frames))

;; ---------------------------------------------------------------------------
;; State lifecycle
;; ---------------------------------------------------------------------------

(defn create-state
  [ir]
  (with-dsp-precision (precision! ir)
    (fn []
      (if (float-arena? ir)
        ;; Scratch follows persistent state in one flat arena; its slots start
        ;; zeroed and retain their contents between frames.
        (make-real-array
         (concat (state-elements (:state ir))
                 (mapcat (fn [{:keys [type size]}]
                           (repeat (or size 1) (coerce-value type 0)))
                         (:scratch ir))))
        (make-object-array
         (concat
          (mapcat (fn [{:keys [type] :as state}]
                    (map #(coerce-value type %)
                         (if (= 1 (state-size state))
                           [(:init state)]
                           (:init state))))
                  (:state ir))
          (mapcat (fn [{:keys [type size]}]
                    (repeat (or size 1) (coerce-value type 0)))
                  (:scratch ir))))))))

(defn reset-state!
  [ir state]
  (with-dsp-precision (precision! ir)
    (fn []
      (binding [*real-state* (float-arena? ir)]
        (check-state! ir state)
        (doseq [{:keys [index type init] :as state-description} (:state ir)
                [element-index value] (map-indexed vector
                                                   (if (= 1 (state-size state-description))
                                                     [init]
                                                     init))]
          (set-state-value! state (+ index element-index) type value))
        nil))))

(defn reset-partition!
  "Reset the persistent state one call partition owns, leaving its neighbours
  alone (M6a). `base`/`size` are the partition's logical arena range, which the
  ABI records per call site; a fused slot then resets on its own, so stealing one
  does not clobber another."
  [ir state base size]
  (with-dsp-precision (precision! ir)
    (fn []
      (binding [*real-state* (float-arena? ir)]
        (check-state! ir state)
        (let [lo (long base)
              hi (+ (long base) (long size))]
          (doseq [{:keys [index type init] :as state-description} (:state ir)]
            (let [index (long index)]
              (when (and (>= index lo) (< index hi))
                (doseq [[element-index value]
                        (map-indexed vector
                                     (if (= 1 (state-size state-description))
                                       [init]
                                       init))]
                  (set-state-value! state (+ index element-index) type value)))))
          nil)))))

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
  (let [args (:args expression)
        x (logical-int (evaluate-expression (nth args 0) locals context))
        y (when (> (count args) 1)
            (logical-int (evaluate-expression (nth args 1) locals context)))]
    (int32
     (case (:operator expression)
       :bit-and (bit-and x y)
       :bit-or (bit-or x y)
       :bit-xor (bit-xor x y)
       :bit-not (bit-not x)
       :bit-shift-left (bit-shift-left x (shift-distance y))
       :bit-shift-right (bit-shift-right x (shift-distance y))
       :unsigned-bit-shift-right (unsigned-bit-shift-right
                                  (bit-and x 0xffffffff)
                                  (shift-distance y))))))

(defn- indexed-element-index
  [expression locals context]
  (let [element-index (if-let [index (:element-index expression)]
                        (logical-int (evaluate-expression index locals context))
                        0)
        size (or (:size expression) 1)]
    (when (or (neg? element-index) (>= element-index size))
      (throw (ex-info "DSP indexed element is out of bounds"
                      {:id (or (:state-id expression) (:array-id expression))
                       :index element-index :size size})))
    ;; State offsets are relative to the current module's partition; scratch
    ;; offsets are already absolute in the one arena.
    (+ (if (#{:state-load :state-store} (:op expression))
         (or (:state-base context) 0)
         0)
       (or (:index expression) 0)
       element-index)))

(defn- buffer-index
  [expr locals context]
  (case (:op expr)
    :frame (:frame context)
    :frame-offset (+ (:frame context) (:offset expr))
    ;; A lane read in a fused artifact resolves inside the *slice* its call site
    ;; was handed, so the slice base comes before the lane's own offset (F4).
    :lane-frame-offset (+ (or (:lane-base context) 0)
                          (:frame context)
                          (:offset expr))
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
    ;; A broadcast buffer is read the same at every channel, so it is never
    ;; subscripted; `:lanes` is the case that matters, since a lane region is
    ;; flat even when the process is not.
    (if (:per-channel? operation)
      (aget ^objects buffer (int channel))
      buffer)))

(defn- checked-buffer-index
  [operation locals context]
  (let [buffer (context-buffer operation locals context)
        index (buffer-index (:index operation) locals context)]
    (when (and (= :development (:checks context))
               (or (neg? index) (>= index (buffer-length buffer))))
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
    (let [params (:params callee)
          arg-forms (:args expression)
          param-count (clojure.core/count params)
          callee-locals (empty-locals param-count)]
      ;; Evaluate every argument in order, straight into the callee environment:
      ;; no argument vector, no lazy seq, no `MapEntry` per parameter.
      (dotimes [index param-count]
        (let [param (nth params index)]
          (locals-put! callee-locals (:name param)
                       (coerce-value (:type param)
                                     (evaluate-expression (nth arg-forms index)
                                                          locals context)))))
      ;; A stateful callee runs against its own partition: its zero-based state
      ;; indices are resolved relative to the call-site base (D19). A region a
      ;; `with-buffers` form binds is *in* that partition (F2, D57), so a
      ;; process-shaped callee writes its region the same way it writes any other
      ;; state, and the caller reads it by index -- no buffer to pass. A lane
      ;; slice is handed over the same way: the caller resolves this call site's
      ;; base and the callee adds it to every lane read (F4).
      (let [result (execute-statement
                    (:body callee) callee-locals
                    (-> context
                        (update :state-base (fnil + 0)
                                (or (:state-base expression) 0))
                        (assoc :lane-base
                               (if-let [base (:lane-base expression)]
                                 (logical-int (evaluate-expression base locals context))
                                 0))))]
        (coerce-value (:return-type callee) (:value result))))))

(defn- evaluate-expression
  [expression locals context]
  (case (:op expression)
    ;; Narrow float constants to the kernel precision, matching what the
    ;; backends emit as a constant (`f32.const`, `(float ...)`, `Math.fround`).
    :const (if (= :float (:type expression))
             (logical-real (:value expression))
             (:value expression))
    :local (locals-get locals (:name expression))
    :state-load (coerce-value (:type expression)
                              (state-value (:state context)
                                           (indexed-element-index expression locals context)))
    :array-load (coerce-value (:type expression)
                              (state-value (:state context)
                                           (indexed-element-index expression locals context)))
    :frame (:frame context)
    :frames (:frames context)
    :sample-rate (:sample-rate context)
    :frame-offset (+ (:frame context) (:offset expression))
    :lane-frame-offset (buffer-index expression locals context)
    :lane-base (or (:lane-base context) 0)
    :frame-plus (buffer-index expression locals context)
    :channel (:channel context)
    :buffer-load (let [[buffer index] (checked-buffer-index expression locals context)]
                   (buffer-value buffer index))
    :call (evaluate-call expression locals context)
    ;; A record value (multi-output) is a map from leaf path to value, held in an
    ;; ordinary local; a projection reads one leaf.
    :record (into {}
                  (map (fn [leaf value]
                         [(:path leaf) value])
                       (:record-leaves expression)
                       (mapv #(evaluate-expression % locals context)
                             (:values expression))))
    :project (get (evaluate-expression (:value expression) locals context)
                  (:path expression))
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
    :builtin (let [args (:args expression)
                   x (evaluate-expression (nth args 0) locals context)
                   y (when (> (count args) 1)
                       (evaluate-expression (nth args 1) locals context))]
               (coerce-value (:type expression)
                             (evaluate-builtin (:builtin expression)
                                               (:type expression) x y)))
    (throw (ex-info "unknown DSP IR expression" {:expression expression}))))

(defn- control
  [kind & [value]]
  {:control kind :value value})

(defn- set-local!
  "Associate `value` with `name` in the `locals` mutable map.

  ClojureScript's persistent maps short-circuit `assoc` when the new value is
  `identical?` (JavaScript `===`) to the stored one, and JavaScript says
  `0 === -0`; a mutable `js/Map` always replaces, so a change of signed zero is
  kept without the drop-then-assoc dance the persistent version needed."
  [locals name value]
  (locals-put! locals name value))

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
                                     (indexed-element-index statement locals context)
                                     (:type statement)
                                     (evaluate-expression (:value statement)
                                                          locals context))
                   nil)
    :array-store (do
                   (set-state-value! (:state context)
                                     (indexed-element-index statement locals context)
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
  (with-dsp-precision (precision! ir)
    (fn []
      (check-arity! ir args)
      (when (or (seq (:state ir)) (seq (:scratch ir)))
        (throw (ex-info "stateful or local-array DSP definitions require the :process entry"
                        {:name (:name ir)})))
      (let [locals (seeded-locals (:params ir) args)
            result (execute-statement (:body ir) locals
                                      {:definitions (:definitions ir)})]
        (coerce-value (:return-type ir) (:value result))))))

(defn process-context
  "Execute a process IR against explicit runtime values.

  `:sample-rate` is part of the portable ABI even though the legacy array
  callable does not pass it. `:controls` is a vector in process-signature
  order. This function is the reference implementation used by richer host
  adapters and by the browser conformance runner."
  [ir {:keys [input output lanes shared frames controls state sample-rate]
       :or {controls [] sample-rate 0.0}}]
  (with-dsp-precision (precision! ir)
    (fn []
      (binding [*real-state* (float-arena? ir)]
        (check-control-arity! ir controls)
        (let [frames (check-block! ir input output frames state)
              sample-rate (coerce-value :float sample-rate)
              ;; One mutable environment for the whole block. Every local the
              ;; frame body reads is declared earlier in the same body (the IR
              ;; guarantees it), so reusing the map across frames is safe; the
              ;; control entries seeded here stay put. A `with-buffers` region is
              ;; call-partition state (F2, D57), so it needs no entry here.
              control-locals (seeded-locals (:controls ir) controls)]
          (dotimes [frame frames]
            (execute-statement
             (:frame-body ir)
             control-locals
             (cond-> {:input input
                      :output output
                      :channels (:channels ir)
                      :channel 0
                      :frame frame
                      :frames frames
                      :sample-rate sample-rate
                      :checks (get-in ir [:options :checks] :development)
                      :state state
                      :definitions (:definitions ir)}
               ;; Buffers resolve by id, so the lane and shared regions need
               ;; only to be present under their own names.
               lanes (assoc :lanes lanes)
               shared (assoc :shared shared))))
          nil)))))

(defn process
  [ir input output frames controls state]
  (process-context ir {:input input
                       :output output
                       :frames frames
                       :controls controls
                       :state state}))

