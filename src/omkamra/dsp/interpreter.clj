(ns omkamra.dsp.interpreter)

(def ^:private float-array-class (Class/forName "[F"))
(def ^:private double-array-class (Class/forName "[D"))
(def ^:private float-2d-array-class (Class/forName "[[F"))
(def ^:private double-2d-array-class (Class/forName "[[D"))
(def ^:private object-array-class (Class/forName "[Ljava.lang.Object;"))
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

(defn- real-array-class [] (if (f64?) double-array-class float-array-class))
(defn- real-2d-array-class [] (if (f64?) double-2d-array-class float-2d-array-class))
(defn- logical-real [value] (if (f64?) (double value) (float value)))

(defn logical-float [value] (float value))
(defn logical-int [value] (int value))
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

(defn- check-float-array!
  [value name]
  (when-not (instance? (real-array-class) value)
    (throw (ex-info (if (f64?) "DSP process expects primitive double arrays"
                        "DSP process expects primitive float arrays")
                    {:argument name :value value}))))

(defn- check-buffer!
  [ir value name frames]
  (let [required-length (+ frames (get-in ir [:memory name :max-frame-offset] 0))]
    (if (= 1 (:channels ir))
      (do
        (check-float-array! value name)
        (when (< (alength value) required-length)
          (throw (ex-info "DSP buffer is shorter than the frame-access requirement"
                          {:argument name :frames frames :required-length required-length
                           :length (alength value)}))))
      (do
        (when-not (instance? (real-2d-array-class) value)
          (throw (ex-info (if (f64?) "DSP multi-channel process expects double[][] buffers"
                              "DSP multi-channel process expects float[][] buffers")
                          {:argument name :value value})))
        (when-not (= (:channels ir) (alength value))
          (throw (ex-info "DSP buffer has the wrong channel count"
                          {:argument name :expected (:channels ir)
                           :actual (alength value)})))
        (dotimes [channel (:channels ir)]
          (let [buffer (aget value channel)]
            (check-float-array! buffer [name channel])
            (when (< (alength buffer) required-length)
              (throw (ex-info "DSP buffer channel is shorter than the frame-access requirement"
                              {:argument [name channel]
                               :frames frames :required-length required-length
                               :length (alength buffer)})))))))))

(defn- float-state?
  [ir]
  (every? #(= :float (:type %)) (:state ir)))

(defn- check-state!
  [ir state]
  (let [expected-class (if (float-state? ir) (real-array-class) object-array-class)]
    (when-not (instance? expected-class state)
      (throw (ex-info "DSP process state has the wrong representation"
                      {:expected (if (float-state? ir) :float-array :object-array)
                       :value state :name (:name ir)})))
    (when-not (= (count (:state ir)) (alength state))
      (throw (ex-info "DSP process state has the wrong size"
                      {:expected (count (:state ir))
                       :actual (alength state)
                       :name (:name ir)})))
    state))

(defn- state-value
  [state index]
  (if (instance? (real-array-class) state)
    (aget state index)
    (aget ^objects state index)))

(defn- set-state-value!
  [state index type value]
  (let [value (coerce-value type value)]
    (if (instance? (real-array-class) state)
      (if (f64?)
        (aset-double ^doubles state index value)
        (aset-float ^floats state index value))
      (aset ^objects state index value))
    nil))

(defn- check-block!
  [ir input output frames state]
  (let [frames (int frames)]
    (when (neg? frames)
      (throw (ex-info "DSP process frame count must not be negative"
                      {:frames frames :name (:name ir)})))
    (check-buffer! ir input :input frames)
    (check-buffer! ir output :output frames)
    (check-state! ir state)
    frames))

(defn create-state
  [ir]
  (binding [*precision* (precision! ir)]
    (if (float-state? ir)
      (if (f64?)
        (double-array (map :init (:state ir)))
        (float-array (map :init (:state ir))))
      (object-array (map (fn [{:keys [type init]}]
                           (coerce-value type init))
                         (:state ir))))))

(defn reset-state!
  [ir state]
  (binding [*precision* (precision! ir)]
    (check-state! ir state)
    (doseq [[index {:keys [type init]}] (map-indexed vector (:state ir))]
      (set-state-value! state index type init))
    nil))

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
      :/ (if (zero? right)
           (throw (ex-info "DSP integer division by zero" {:left left :right right}))
           (logical-int (quot left right))))))

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
                              (state-value (:state context) (:index expression)))
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
    (throw (ex-info "unknown DSP IR expression" {:expression expression}))))

(defn- control
  [kind & [value]]
  {:control kind :value value})

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
    :declare (do
               (vswap! locals assoc (:name statement)
                       (if-let [init (:init statement)]
                         (coerce-value (:type statement)
                                       (evaluate-expression init locals context))
                         (case (:type statement)
                           :float (logical-real 0.0)
                           :int (int 0)
                           :boolean false)))
               nil)
    :assign (do
              (vswap! locals assoc (:name statement)
                      (coerce-value (:type statement)
                                    (evaluate-expression (:value statement)
                                                         locals context)))
              nil)
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
                    (if (f64?)
                      (aset-double ^doubles buffer index value)
                      (aset-float ^floats buffer index value))
                    nil)
    :state-store (do
                   (set-state-value! (:state context) (:index statement)
                                     (:type statement)
                                     (evaluate-expression (:value statement)
                                                          locals context))
                   nil)
    (throw (ex-info "unknown DSP IR statement" {:statement statement}))))

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
  adapters."
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
