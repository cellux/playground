(ns omkamra.dsp.jvm
  "Lower explicit typed f32-audio DSP IR to JVM bytecode using insn."
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [insn.core :as insn]))

(definterface DspKernel
  (^void process [^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats float-state ^ints int-state]))

(definterface DspMultiKernel
  (^void process [^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls
                  ^floats float-state ^ints int-state]))

(definterface DspKernelD64
  (^void process [^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles float-state ^ints int-state]))

(definterface DspMultiKernelD64
  (^void process [^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls
                  ^doubles float-state ^ints int-state]))

;; The unchecked adapters own persistent state, leaving a primitive block ABI
;; for real-time hosts.  The checked API validates this same ABI before
;; delegating to one of these interfaces.
(definterface DspProcess
  (^void process [^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls]))

(definterface DspMultiProcess
  (^void process [^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls]))

(definterface DspProcessD64
  (^void process [^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls]))

(definterface DspMultiProcessD64
  (^void process [^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls]))

(deftype ^:private F32ProcessAdapter [^DspKernel kernel
                                      ^floats float-state
                                      ^ints int-state]
  DspProcess
  (^void process [_ ^floats input ^floats output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state)))

(deftype ^:private F32MultiProcessAdapter [^DspMultiKernel kernel
                                           ^floats float-state
                                           ^ints int-state]
  DspMultiProcess
  (^void process [_ ^"[[F" input ^"[[F" output ^int frames ^float sample-rate
                  ^floats float-controls ^ints int-controls]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state)))

(deftype ^:private F64ProcessAdapter [^DspKernelD64 kernel
                                      ^doubles float-state
                                      ^ints int-state]
  DspProcessD64
  (^void process [_ ^doubles input ^doubles output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state)))

(deftype ^:private F64MultiProcessAdapter [^DspMultiKernelD64 kernel
                                           ^doubles float-state
                                           ^ints int-state]
  DspMultiProcessD64
  (^void process [_ ^"[[D" input ^"[[D" output ^int frames ^double sample-rate
                  ^doubles float-controls ^ints int-controls]
    (.process kernel input output frames sample-rate
              float-controls int-controls float-state int-state)))

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

(declare expression-instructions statement-instructions condition-instructions)

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
    :frame-plus (vec (concat [[:iload (:frame slots)]]
                             (expression-instructions (:offset index) slots labels)
                             [[:iadd]]))
    (throw (ex-info "unknown JVM buffer index" {:index index}))))

(defn- buffer-reference-instructions
  [operation slots labels]
  (let [buffer (get slots (:buffer-id operation))]
    (if (:multi-channel? slots)
      (concat [[:aload buffer]]
              (channel-instructions (:channel operation) slots labels)
              [[:aaload]])
      [[:aload buffer]])))

(defn- comparison-branch-instructions
  [comparison slots labels true-label false-label]
  (if (= :int (get-in comparison [:left :type]))
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

(defn- expression-instructions
  [expression slots labels]
  (case (:op expression)
    :const (case (:type expression)
             :boolean [(if (:value expression) [:iconst-1] [:iconst-0])]
             :int [[:ldc (int (:value expression))]]
             (real-const-instructions (:value expression)))
    :local (if (= :float (:type expression))
             [[(real-local-load-op) (get slots (:name expression))]]
             [[:iload (get slots (:name expression))]])
    :state-load (let [{:keys [slot index type]} (get (:state-layout slots)
                                                     (:index expression))]
                  [[:aload slot] [:ldc index]
                   [(if (= :float type) (real-load-op) :iaload)]])
    :frame [[:iload (:frame slots)]]
    :frames [[:iload (:frames slots)]]
    :sample-rate [[(real-local-load-op) (:sample-rate slots)]]
    :channel [[:iload (:channel slots)]]
    :frame-plus (vec (concat [[:iload (:frame slots)]]
                             (expression-instructions (:offset expression) slots labels)
                             [[:iadd]]))
    :buffer-load (vec (concat (buffer-reference-instructions expression slots labels)
                              (index-instructions (:index expression) slots labels)
                              [[(real-load-op)]]))
    :binary (vec
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
                   :/ [(if (f64?) :ddiv :fdiv)]))]))
    :compare (boolean-value-instructions expression slots labels)
    :logical (boolean-value-instructions expression slots labels)
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
    :expression (conj (vec (expression-instructions (:value statement) slots labels))
                      [:pop])
    :return (vec (concat (expression-instructions (:value statement) slots labels)
                         [(if (= :float (:type (:value statement)))
                            [(real-return-op)]
                            [:ireturn])]))
    :buffer-store (vec (concat
                        (buffer-reference-instructions statement slots labels)
                        (index-instructions (:index statement) slots labels)
                        (expression-instructions (:value statement) slots labels)
                        [[(real-store-op)]]))
    :state-store (let [{:keys [slot index type]} (get (:state-layout slots)
                                                      (:index statement))]
                   (vec (concat [[:aload slot] [:ldc index]]
                                (expression-instructions (:value statement) slots labels)
                                [[(if (= :float type) (real-store-op) :iastore)]])))
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
  (if (= :float type) (real-type) :int))

(defn- state-layout
  [state]
  (reduce (fn [{:keys [entries float-count int-count] :as layout}
               [state-index {:keys [type]}]]
            (let [storage (if (= :float type) :float :int)
                  physical-index (if (= storage :float) float-count int-count)
                  entry {:state-index state-index
                         :type type
                         :storage storage
                         :index physical-index}]
              (cond-> (assoc layout :entries (conj entries entry))
                (= storage :float) (update :float-count inc)
                (= storage :int) (update :int-count inc))))
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
                                 (if value 1 0)
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
                              {:argument [name channel] :frames frames
                               :required-length required-length
                               :length (alength buffer)})))))))))

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
    (when-not (= (:float-count layout) (alength float-state))
      (throw (ex-info "DSP process float state has the wrong size"
                      {:expected (:float-count layout) :actual (alength float-state)})))
    (when-not (= (:int-count layout) (alength int-state))
      (throw (ex-info "DSP process integer state has the wrong size"
                      {:expected (:int-count layout) :actual (alength int-state)})))
    state))

(defn- reset-state!
  [ir layout state]
  (check-state! layout state)
  (doseq [[{:keys [storage index type]} {:keys [init]}]
          (map vector (:entries layout) (:state ir))]
    (if (= storage :float)
      (if (f64?)
        (aset-double ^doubles (:float state) index (double init))
        (aset-float ^floats (:float state) index (float init)))
      (aset-int ^ints (:int state) index (control-value type init))))
  nil)

(defn- check-controls!
  [layout float-controls int-controls]
  (check-float-array! float-controls :float-controls)
  (when-not (instance? int-array-class int-controls)
    (throw (ex-info "DSP process integer controls must be an int array"
                    {:value int-controls})))
  (when-not (= (:float-count layout) (alength float-controls))
    (throw (ex-info "DSP process float controls have the wrong size"
                    {:expected (:float-count layout)
                     :actual (alength float-controls)})))
  (when-not (= (:int-count layout) (alength int-controls))
    (throw (ex-info "DSP process integer controls have the wrong size"
                    {:expected (:int-count layout)
                     :actual (alength int-controls)}))))

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

(defn process-unchecked!
  "Run a JVM process artifact through its primitive, state-owning ABI.

  This performs no representation, shape, length, or control-layout checks.
  `input` and `output` must be the artifact's f32/f64 mono or multichannel
  primitive arrays. `float-controls` and `int-controls` must be the reusable
  physical control arrays described by `:control-layout`; state remains owned
  by the artifact. Real-time hosts should retain and call `:unchecked`
  directly through its matching `Dsp*Process` interface when possible."
  [artifact input output frames sample-rate float-controls int-controls]
  (let [adapter (:unchecked artifact)
        multi-channel? (:multi-channel? artifact)]
    (case (:precision artifact)
      :f32 (if multi-channel?
             (.process ^DspMultiProcess adapter input output (int frames)
                       (float sample-rate) float-controls int-controls)
             (.process ^DspProcess adapter input output (int frames)
                       (float sample-rate) float-controls int-controls))
      :f64 (if multi-channel?
             (.process ^DspMultiProcessD64 adapter input output (int frames)
                       (double sample-rate) float-controls int-controls)
             (.process ^DspProcessD64 adapter input output (int frames)
                       (double sample-rate) float-controls int-controls)))))

(defn process-checked!
  "Validate and run the raw primitive JVM process ABI.

  This has the same arguments as `process-unchecked!`, but verifies buffer
  shapes and lengths, control storage, and artifact-owned state before it
  delegates to the unchecked adapter."
  [artifact input output frames sample-rate float-controls int-controls]
  (binding [*precision* (:precision artifact)]
    (let [frames (int frames)
          ir (:process-ir artifact)]
      (when (neg? frames)
        (throw (ex-info "DSP process frame count must not be negative"
                        {:frames frames})))
      (check-buffer! ir input :input frames)
      (check-buffer! ir output :output frames)
      (check-controls! (:control-layout artifact) float-controls int-controls)
      (check-state! (:state-layout artifact) (:state artifact))
      (process-unchecked! artifact input output frames sample-rate
                          float-controls int-controls))))

(defn- kernel-process!
  [kernel multi-channel? input output frames sample-rate controls state]
  (if (f64?)
    (if multi-channel?
      (.process ^DspMultiKernelD64 kernel input output frames sample-rate
                ^doubles (:float controls) ^ints (:int controls)
                ^doubles (:float state) ^ints (:int state))
      (.process ^DspKernelD64 kernel ^doubles input ^doubles output frames sample-rate
                ^doubles (:float controls) ^ints (:int controls)
                ^doubles (:float state) ^ints (:int state)))
    (if multi-channel?
      (.process ^DspMultiKernel kernel input output frames sample-rate
                ^floats (:float controls) ^ints (:int controls)
                ^floats (:float state) ^ints (:int state))
      (.process ^DspKernel kernel ^floats input ^floats output frames sample-rate
                ^floats (:float controls) ^ints (:int controls)
                ^floats (:float state) ^ints (:int state)))))

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
        (check-buffer! ir input :input frames)
        (check-buffer! ir output :output frames)
        (check-state! layout state)
        (write-controls! control-layout controls controls-state)
        (kernel-process! kernel (> (:channels ir) 1) input output frames
                         (if (f64?) 0.0 (float 0.0))
                         controls-state state)
        nil))))

(defn- process-context-callable
  [precision kernel ir layout state control-layout controls-state]
  (fn [{:keys [input output frames sample-rate controls]
        :or {sample-rate 0.0 controls []}}]
    (binding [*precision* precision]
      (when-not (= (count (:controls ir)) (count controls))
        (throw (ex-info "wrong number of arguments for DSP process controls"
                        {:expected (count (:controls ir)) :actual (count controls)})))
      (let [frames (int frames)]
        (when (neg? frames)
          (throw (ex-info "DSP process frame count must not be negative"
                          {:frames frames})))
        (check-buffer! ir input :input frames)
        (check-buffer! ir output :output frames)
        (check-state! layout state)
        (write-controls! control-layout controls controls-state)
        (kernel-process! kernel (> (:channels ir) 1) input output frames
                         (if (f64?) (double sample-rate) (float sample-rate))
                         controls-state state)
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

(defn compile
  [ir]
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
            state-layout (state-layout (:state ir))
            sample-rate-slot 4
            float-control-slot (+ sample-rate-slot (real-param-width))
            int-control-slot (inc float-control-slot)
            float-state-slot (inc int-control-slot)
            int-state-slot (inc float-state-slot)
            first-local-slot (inc int-state-slot)
            [control-slots next-slot] (slot-layout (:controls ir) first-local-slot)
            [local-slots next-slot] (slot-layout (:locals ir) next-slot)
            channel-slot (when multi-channel? next-slot)
            frame-slot (if multi-channel? (inc channel-slot) next-slot)
            state-slots (mapv #(assoc % :slot (if (= :float (:storage %))
                                                float-state-slot int-state-slot))
                              (:entries state-layout))
            slots (merge {:input 1 :output 2 :frames 3 :sample-rate sample-rate-slot
                          :frame frame-slot :multi-channel? multi-channel?
                          :state-layout state-slots}
                         (when multi-channel? {:channel channel-slot})
                         control-slots local-slots
                         {:next-temp (atom (inc frame-slot))})
            labels (atom 0)
            frame-body (statement-instructions (:frame-body ir) slots labels [])
            frame-loop (concat [[:iload frame-slot] [:iload 3] [:if-icmpge :frame-done]]
                               frame-body
                               [[:iinc frame-slot 1] [:goto :frame-loop]
                                [:mark :frame-done]])
            loops (if multi-channel?
                    (concat [[:iconst-0] [:istore channel-slot] [:mark :channel-loop]
                             [:iconst-0] [:istore frame-slot] [:mark :frame-loop]]
                            frame-loop
                            [[:iinc channel-slot 1] [:iload channel-slot]
                             [:ldc channels] [:if-icmplt :channel-loop]])
                    (concat [[:iconst-0] [:istore frame-slot] [:mark :frame-loop]]
                            frame-loop))
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
            type {:name (class-name (symbol (str (name (:name ir)) "_process")))
                  :flags #{:public :final}
                  :interfaces [interface]
                  :methods [{:flags #{:public}
                             :name :process
                             :desc [input-class input-class :int (real-type)
                                    (real-array-class) int-array-class
                                    (real-array-class) int-array-class :void]
                             :emit (vec (concat control-initializers loops [[:return]]))}]}
            class (insn/define type)
            kernel (.newInstance ^Class class)
            real-array (if (f64?) double-array float-array)
            state {:float (real-array (:float-count state-layout))
                   :int (int-array (:int-count state-layout))}
            controls-state {:float (real-array (:float-count control-layout))
                            :int (int-array (:int-count control-layout))}]
        (reset-state! ir state-layout state)
        (let [unchecked (unchecked-adapter precision kernel multi-channel? state)]
          {:class class :class-name (.getName class) :kernel kernel
           :bytes (insn/get-bytes type) :state state :state-layout state-layout
           :controls controls-state :control-layout control-layout
           :process-ir ir :precision precision :multi-channel? multi-channel?
           :unchecked unchecked
           :reset #(binding [*precision* precision]
                     (reset-state! ir state-layout state))
           :invoke (process-callable precision kernel ir state-layout state
                                     control-layout controls-state)
           :invoke-context (process-context-callable precision kernel ir state-layout state
                                                     control-layout controls-state)})))))
