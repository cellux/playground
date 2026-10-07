(ns omkamra.dsp.ir
  "Typed, target-neutral IR for omkamra.dsp.

  Source expressions lower to pure value nodes plus explicit statements. Buffer
  declarations live on process roots; memory operations refer to those buffers
  by ID. Effects are attached only to inherently effectful nodes and summarized
  on function/process roots."
  (:require [clojure.string :as str]))

(def ^:private arithmetic-operators
  {'+ :+ '- :- '* :* '/ :/})

(def ^:private comparison-operators
  {'= := 'not= :not= '< :< '<= :<= '> :> '>= :>=})

(def ^:private logical-operators
  {'and :and 'or :or 'not :not})

(def ^:private value-types #{:float :int :boolean})
(def ^:private numeric-types #{:float :int})

(def effect-types
  #{:read-state :write-state :read-memory :write-memory :write-local :call})

(defn- fail
  [message data]
  (throw (ex-info message (assoc data :dsp/error :invalid-definition))))

(defn- valid-name?
  [x]
  (and (symbol? x) (not (str/blank? (name x)))))

(defn- environment
  ([bindings functions options]
   (environment bindings functions options {}))
  ([bindings functions options buffers]
   {:bindings bindings
    :functions functions
    :options options
    :buffers buffers
    :loop-depth 0
    :locals (atom [])
    :counter (atom 0)}))

(defn- fresh-local!
  [env prefix type]
  (let [name (symbol (str prefix "__" (swap! (:counter env) inc)))
        local {:name name :type type}]
    (swap! (:locals env) conj local)
    local))

(defn- fragment
  ([value] (fragment [] value))
  ([statements value]
   {:statements (vec statements)
    :value value
    :type (:type value)}))

(defn- void-fragment
  [statements]
  {:statements (vec statements) :value nil :type :void})

(defn- expect-type!
  [expected expression form]
  (when-not (= expected (:type expression))
    (fail "DSP expression has the wrong type"
          {:form form :expected expected :actual (:type expression)}))
  expression)

(defn- combine-fragments
  [fragments value]
  (fragment (mapcat :statements fragments) value))

(defn- constant
  [form]
  (cond
    (integer? form) {:op :const :type :int :value (int form)}
    (number? form) {:op :const :type :float :value (float form)}
    (boolean? form) {:op :const :type :boolean :value form}
    :else nil))

(declare lower-value lower-form lower-forms)

(defn- frame-index
  "Lower a source buffer offset to an absolute per-frame index.

  Literal offsets retain the compact `:frame-offset` representation. Dynamic
  offsets are explicit typed expressions so the interpreter can check them at
  the access site and target backends can either lower or reject them honestly."
  [form offset env]
  (let [offset (lower-value offset env)]
    (expect-type! :int (:value offset) form)
    (let [value (:value offset)]
      (when (and (= :const (:op value)) (neg? (:value value)))
        (fail "DSP buffer offsets must not be negative"
              {:form form :offset (:value value)}))
      {:statements (:statements offset)
       :value (if (= :const (:op value))
                {:op :frame-offset :offset (:value value) :type :int}
                {:op :frame-plus :offset value :type :int})})))

(defn- channel-and-offset
  [form args]
  (case (count args)
    1 [0 (first args)]
    2 [(first args) (second args)]
    (fail "DSP buffer access requires an offset or channel and offset"
          {:form form})))

(defn- buffer-access!
  [form env buffer-id direction]
  (let [buffer (get-in env [:buffers buffer-id])]
    (when-not buffer
      (fail "DSP buffer access refers to an unknown process buffer"
            {:form form :buffer-id buffer-id
             :buffers (vec (sort (keys (:buffers env))))}))
    (when-not (contains? direction (:direction buffer))
      (fail "DSP buffer access is not permitted for this buffer direction"
            {:form form :buffer-id buffer-id :direction (:direction buffer)
             :required-direction direction}))
    buffer))

(defn- channel-expression
  [form channel env buffer]
  (let [channel (lower-value channel env)]
    (expect-type! :int (:value channel) form)
    (let [value (:value channel)]
      (when (and (= :const (:op value))
                 (or (neg? (:value value))
                     (>= (:value value) (:channels buffer))))
        (fail "DSP buffer channel is outside the declared channel range"
              {:form form :channel (:value value) :buffer-id (:id buffer)
               :channels (:channels buffer)}))
      {:statements (:statements channel)
       :value (if (= :const (:op value)) (:value value) value)})))

(defn- binding-expression
  [binding]
  (case (:kind binding)
    :state (do
             (when (> (or (:size binding) 1) 1)
               (fail "aggregate DSP state must be accessed with state-load"
                     {:state (:name binding) :size (:size binding)}))
             {:op :state-load
              :state-id (:name binding)
              :index (:index binding)
              :type (:type binding)
              :effects #{:read-state}})
    :buffer (:expression binding)
    :frames {:op :frames :type :int}
    :sample-rate {:op :sample-rate :type :float}
    {:op :local :name (:name binding) :type (:type binding)}))

(defn- lower-binary
  [form env operator comparison?]
  (let [[_ left-form right-form & extra] form]
    (when (or (nil? left-form) (nil? right-form) extra)
      (fail "DSP binary operators require two operands"
            {:form form :operator (first form)}))
    (let [left (lower-value left-form env)
          right (lower-value right-form env)
          left-value (:value left)
          right-value (:value right)
          type (:type left-value)]
      (when-not (= type (:type right-value))
        (fail "DSP binary operands have the wrong type"
              {:form form :left-type type :right-type (:type right-value)}))
      (when-not (if comparison?
                  (if (#{:= :not=} operator)
                    (contains? value-types type)
                    (contains? numeric-types type))
                  (contains? numeric-types type))
        (fail "DSP binary operator does not support these operand types"
              {:form form :operator (first form) :type type}))
      (combine-fragments
       [left right]
       {:op (if comparison? :compare :binary)
        :operator operator
        :left left-value
        :right right-value
        :type (if comparison? :boolean type)}))))

(defn- lower-logical
  [form env operator]
  (let [forms (vec (rest form))
        expected (if (= :not operator) 1 2)]
    (when-not (= expected (count forms))
      (fail "DSP logical operators have the wrong arity"
            {:form form :operator (first form)}))
    (let [args (mapv #(lower-value % env) forms)]
      (doseq [arg args]
        (expect-type! :boolean (:value arg) form))
      (combine-fragments args
                         {:op :logical
                          :operator operator
                          :args (mapv :value args)
                          :type :boolean}))))

(defn- branch-type
  [form then-fragment else-fragment]
  (let [then-type (:type then-fragment)
        else-type (:type else-fragment)]
    (cond
      (= then-type else-type) then-type
      (= :never then-type) else-type
      (= :never else-type) then-type
      :else (fail "DSP if branches must have the same type"
                  {:form form :then-type then-type :else-type else-type}))))

(defn- block
  [statements]
  {:op :block :statements (vec statements)})

(defn- branch-statements
  [fragment result-local]
  (cond-> (:statements fragment)
    (and result-local (:value fragment))
    (conj {:op :assign
           :name (:name result-local)
           :type (:type result-local)
           :value (:value fragment)
           :effects #{:write-local}})))

(defn- lower-if
  [form env]
  (let [[_ condition-form then-form else-form & extra] form]
    (when (or (nil? condition-form) (nil? then-form) (nil? else-form) extra)
      (fail "DSP if expressions require a condition, then branch, and else branch"
            {:form form}))
    (let [condition (lower-value condition-form env)
          _ (expect-type! :boolean (:value condition) form)
          then-fragment (lower-form then-form env)
          else-fragment (lower-form else-form env)
          type (branch-type form then-fragment else-fragment)
          result-local (when (contains? value-types type)
                         (fresh-local! env "if_result" type))
          declaration (when result-local
                        {:op :declare
                         :name (:name result-local)
                         :type type})
          statement {:op :if
                     :condition (:value condition)
                     :then (block (branch-statements then-fragment result-local))
                     :else (block (branch-statements else-fragment result-local))}
          statements (concat (:statements condition)
                             (when declaration [declaration])
                             [statement])]
      (case type
        :never {:statements (vec statements) :value nil :type :never}
        :void (void-fragment statements)
        (fragment statements
                  {:op :local :name (:name result-local) :type type})))))

(defn- lower-let
  [form env]
  (let [[_ bindings & body] form]
    (when-not (vector? bindings)
      (fail "DSP let bindings must be a vector" {:form form}))
    (when (odd? (count bindings))
      (fail "DSP let bindings require name/value pairs" {:form form}))
    (when-not (seq body)
      (fail "DSP let requires a body" {:form form}))
    (loop [pairs (partition 2 bindings)
           env env
           statements []]
      (if-let [[[name init-form] & remaining] (seq pairs)]
        (do
          (when-not (valid-name? name)
            (fail "DSP local name must be a symbol" {:form form :name name}))
          (when (or (contains? (:bindings env) name)
                    (some #(= name (:name %)) @(:locals env)))
            (fail "DSP local names must be unique and cannot shadow bindings"
                  {:form form :name name}))
          (let [init (lower-value init-form env)
                local {:name name :type (:type init) :kind :local}
                declaration {:op :declare
                             :name name
                             :type (:type init)
                             :init (:value init)}]
            (swap! (:locals env) conj (select-keys local [:name :type]))
            (recur remaining
                   (assoc-in env [:bindings name] local)
                   (into statements (concat (:statements init) [declaration])))))
        (let [body-fragment (lower-forms body env)]
          (assoc body-fragment
                 :statements (into (vec statements)
                                   (:statements body-fragment))))))))

(defn- lower-set!
  [form env]
  (let [[_ name value-form & extra] form
        binding (get-in env [:bindings name])]
    (when (or (not (valid-name? name)) extra (nil? value-form))
      (fail "DSP set! requires a local name and one value" {:form form}))
    (when-not (= :local (:kind binding))
      (fail "DSP set! can only update mutable locals" {:form form :name name}))
    (let [value (lower-value value-form env)]
      (expect-type! (:type binding) (:value value) form)
      (fragment (conj (:statements value)
                      {:op :assign
                       :name name
                       :type (:type binding)
                       :value (:value value)
                       :effects #{:write-local}})
                {:op :local :name name :type (:type binding)}))))

(defn- loop-bound
  [form value source]
  (when-not (and (integer? value) (pos? value))
    (fail "DSP loop bounds must be positive integer constants"
          {:form form :value value :source source}))
  (int value))

(defn- lower-while
  [form env]
  (let [[_ condition-form & forms] form
        options (if (map? (last forms)) (last forms) {})
        body-forms (if (map? (last forms)) (butlast forms) forms)
        unknown-options (seq (remove #{:max-iterations} (keys options)))
        bound (cond
                unknown-options
                (fail "DSP while received unknown options"
                      {:form form :options options
                       :unknown-options (vec unknown-options)})
                (contains? options :max-iterations)
                (loop-bound form (:max-iterations options) :explicit)
                (some? (get-in env [:options :loop-bound]))
                (loop-bound form (get-in env [:options :loop-bound]) :option)
                :else nil)]
    (when (or (nil? condition-form) (empty? body-forms))
      (fail "DSP while requires a condition and body" {:form form}))
    (let [condition (lower-value condition-form env)]
      (when (seq (:statements condition))
        (fail "DSP while conditions cannot contain statements" {:form form}))
      (expect-type! :boolean (:value condition) form)
      (let [body (lower-forms body-forms (update env :loop-depth inc))]
        (void-fragment
         [{:op :while
           :condition (:value condition)
           :body (block (:statements body))
           :bound bound}])))))

(defn- lower-loop-control
  [form env operator]
  (when-not (and (= 1 (count form)) (pos? (:loop-depth env)))
    (fail (if (pos? (:loop-depth env))
            "DSP loop control forms take no arguments"
            "DSP loop control forms are only valid inside while loops")
          {:form form :operator operator}))
  {:statements [{:op operator}] :value nil :type :never})

(defn- lower-buffer-load
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset] (channel-and-offset form args)
        buffer (buffer-access! form env buffer-id #{:input :output})
        channel (channel-expression form channel env buffer)
        index (frame-index form offset env)]
    (when-not (= :float (:type buffer))
      (fail "DSP buffer-load currently requires a :float buffer"
            {:form form :buffer-id buffer-id :type (:type buffer)}))
    (combine-fragments
     [channel index]
     {:op :buffer-load
      :buffer-id buffer-id
      :channel (:value channel)
      :index (:value index)
      :type :float
      :effects #{:read-memory}})))

(defn- lower-buffer-store
  [form env]
  (let [[_ buffer-id & args] form
        [channel offset value-form]
        (case (count args)
          2 [0 (first args) (second args)]
          3 [(first args) (second args) (nth args 2)]
          (fail "DSP buffer-store requires offset, value, or channel, offset, value"
                {:form form}))
        buffer (buffer-access! form env buffer-id #{:output})
        channel (channel-expression form channel env buffer)
        index (frame-index form offset env)
        value (lower-value value-form env)]
    (when-not (= :float (:type buffer))
      (fail "DSP buffer-store currently requires a :float buffer"
            {:form form :buffer-id buffer-id :type (:type buffer)}))
    (expect-type! :float (:value value) form)
    (void-fragment
     (concat (:statements channel)
             (:statements index)
             (:statements value)
             [{:op :buffer-store
               :buffer-id buffer-id
               :channel (:value channel)
               :index (:value index)
               :value (:value value)
               :effects #{:write-memory}}]))))

(defn- aggregate-state-binding!
  [form env state-id]
  (let [binding (get-in env [:bindings state-id])]
    (when-not (= :state (:kind binding))
      (fail "DSP state access requires a declared state name"
            {:form form :state state-id}))
    (when-not (> (or (:size binding) 1) 1)
      (fail "indexed state access requires aggregate state"
            {:form form :state state-id :size (:size binding)}))
    binding))

(defn- aggregate-state-index
  [form env binding index-form]
  (let [index (lower-value index-form env)]
    (expect-type! :int (:value index) form)
    (when (and (= :const (get-in index [:value :op]))
               (or (neg? (get-in index [:value :value]))
                   (>= (get-in index [:value :value]) (:size binding))))
      (fail "DSP aggregate state index is outside the declared range"
            {:form form :state (:name binding) :size (:size binding)
             :index (get-in index [:value :value])}))
    index))

(defn- lower-state-load
  [form env]
  (let [[_ state-id index-form & extra] form]
    (when (or (not (symbol? state-id)) (nil? index-form) extra)
      (fail "state-load requires an aggregate state name and one integer index"
            {:form form}))
    (let [binding (aggregate-state-binding! form env state-id)
          index (aggregate-state-index form env binding index-form)]
      (fragment (:statements index)
                {:op :state-load
                 :state-id (:name binding)
                 :index (:index binding)
                 :element-index (:value index)
                 :size (:size binding)
                 :type (:type binding)
                 :effects #{:read-state}}))))

(defn- lower-state-store
  [form env]
  (let [[_ state-id index-form value-form & extra] form]
    (when (or (not (symbol? state-id)) (nil? index-form) (nil? value-form) extra)
      (fail "state-store requires an aggregate state name, integer index, and value"
            {:form form}))
    (let [binding (aggregate-state-binding! form env state-id)
          index (aggregate-state-index form env binding index-form)
          value (lower-value value-form env)]
      (expect-type! (:type binding) (:value value) form)
      (void-fragment
       (concat (:statements index) (:statements value)
               [{:op :state-store
                 :state-id (:name binding)
                 :index (:index binding)
                 :element-index (:value index)
                 :size (:size binding)
                 :type (:type binding)
                 :value (:value value)
                 :effects #{:write-state}}])))))

(defn- lower-call
  [form env]
  (let [[_ function-id & arg-forms] form
        signature (get-in env [:functions function-id])]
    (when-not (symbol? function-id)
      (fail "DSP call requires a resolved function ID" {:form form}))
    (when-not signature
      (fail "DSP call refers to an unknown linked function"
            {:form form :function-id function-id}))
    (when-not (= (count (:params signature)) (count arg-forms))
      (fail "DSP call has the wrong number of arguments"
            {:form form :function-id function-id
             :expected (count (:params signature)) :actual (count arg-forms)}))
    (let [args (mapv #(lower-value %1 env) arg-forms)]
      (doseq [[arg parameter] (map vector args (:params signature))]
        (expect-type! (:type parameter) (:value arg) form))
      (combine-fragments
       args
       {:op :call
        :function-id function-id
        :args (mapv :value args)
        :type (:return-type signature)
        :effects #{:call}}))))

(defn- lower-conversion
  [form env operator from-type to-type]
  (let [[_ value-form & extra] form]
    (when (or (nil? value-form) extra)
      (fail "DSP conversions require one operand" {:form form :operator operator}))
    (let [value (lower-value value-form env)]
      (expect-type! from-type (:value value) form)
      (fragment (:statements value)
                {:op :convert :operator operator
                 :value (:value value) :type to-type}))))

(defn- lower-value
  [form env]
  (cond
    (constant form) (fragment (constant form))

    (symbol? form)
    (if-let [binding (get-in env [:bindings form])]
      (fragment (binding-expression binding))
      (fail "DSP expression refers to an unknown local"
            {:form form :locals (keys (:bindings env))}))

    (seq? form)
    (let [op (first form)]
      (cond
        (= 'dsp/call op) (lower-call form env)
        (= 'if op) (lower-if form env)
        (= 'let op) (lower-let form env)
        (= 'do op) (lower-forms (rest form) env)
        (= 'set! op) (lower-set! form env)
        (= 'while op) (lower-while form env)
        (= 'break op) (lower-loop-control form env :break)
        (= 'continue op) (lower-loop-control form env :continue)
        (= 'buffer-load op) (lower-buffer-load form env)
        (= 'buffer-store op) (lower-buffer-store form env)
        (= 'state-load op) (lower-state-load form env)
        (= 'state-store op) (lower-state-store form env)
        (= 'int->float op) (lower-conversion form env :int->float :int :float)
        (= 'float->int op) (lower-conversion form env :float->int :float :int)
        (contains? logical-operators op)
        (lower-logical form env (get logical-operators op))
        (contains? arithmetic-operators op)
        (lower-binary form env (get arithmetic-operators op) false)
        (contains? comparison-operators op)
        (lower-binary form env (get comparison-operators op) true)
        :else (fail "unsupported DSP expression" {:form form :operator op})))

    :else (fail "unsupported DSP expression value" {:form form})))

(defn- lower-form
  [form env]
  (lower-value form env))

(defn- lower-forms
  [forms env]
  (when-not (seq forms)
    (fail "DSP blocks must contain at least one form" {:forms forms}))
  (loop [remaining (seq forms)
         statements []]
    (let [current (lower-form (first remaining) env)
          statements (into statements (:statements current))]
      (if-let [remaining (next remaining)]
        (recur remaining
               (cond-> statements
                 (= :call (get-in current [:value :op]))
                 (conj {:op :expression :value (:value current)})))
        (assoc current :statements (vec statements))))))

(defn- collect-effects
  [value]
  (letfn [(walk [x]
            (cond
              (map? x) (into (or (:effects x) #{})
                             (mapcat walk (vals (dissoc x :effects))))
              (sequential? x) (into #{} (mapcat walk x))
              :else #{}))]
    (walk value)))

(defn- normalized-definition!
  [definition]
  (when-not (and (map? definition)
                 (= :function (:dsp/kind definition))
                 (every? map? (:params definition))
                 (every? map? (:state definition)))
    (fail "DSP IR lowering requires a normalized descriptor"
          {:definition definition}))
  definition)

(defn- signatures
  [definitions]
  (into {}
        (map (fn [[id definition]]
               [id {:params (:params definition)
                    :return-type (:return-type definition)}])
             definitions)))

(defn- state-slots
  "Assign stable element offsets to persistent state.

  The offsets are portable logical storage offsets, not target byte addresses.
  Backends choose physical arrays or linear-memory layouts from this data."
  [states]
  (second
   (reduce (fn [[offset result] state]
             (let [size (or (:size state) 1)
                   state (assoc state :index offset)]
               [(+ offset size) (conj result state)]))
           [0 []]
           states)))

(defn- base-bindings
  [definition states]
  (into {}
        (concat
         (map (fn [parameter]
                [(:name parameter) (assoc parameter :kind :param)])
              (:params definition))
         (map (fn [state]
                [(:name state) (assoc state :kind :state)])
              states))))

(defn lower
  "Lower one normalized descriptor to explicit function IR."
  ([definition] (lower definition {}))
  ([definition {:keys [functions definition-id options]}]
   (normalized-definition! definition)
   (let [return-type (:return-type definition)
         states (state-slots (:state definition))
         env (environment (base-bindings definition states) (or functions {}) (or options {}))
         body (lower-forms (:body definition) env)]
     (when-not (:value body)
       (fail "DSP function body does not produce a value" {:body (:body definition)}))
     (expect-type! return-type (:value body) (:body definition))
     (when (seq (:state definition))
       ;; State transitions are lowered directly by `lower-process`; scalar
       ;; function entries remain invalid for stateful descriptors.
       nil)
     (let [body (block (conj (:statements body)
                             {:op :return :value (:value body)}))
           ir {:op :function
               :name (:name definition)
               :id (or definition-id (:name definition))
               :params (:params definition)
               :return-type return-type
               :locals @(:locals env)
               :state states
               :precision (:precision options)
               :options (or options {})
               :body body}]
       (assoc ir :effects (collect-effects body))))))

(defn lower-unit
  "Lower every descriptor in a linked compilation unit."
  ([unit] (lower-unit unit {}))
  ([unit {:keys [options]}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP IR lowering expects a linked compilation unit" {:unit unit}))
   (let [definitions (:definitions unit)
         signatures (signatures definitions)
         lowered (into (array-map)
                       (map (fn [[id definition]]
                              [id (lower definition {:functions signatures
                                                     :definition-id id
                                                     :options options})])
                            definitions))
         entry (:entry unit)]
     (assoc (get lowered entry)
            :definitions lowered
            :entry-id entry
            :graph (:graph unit)))))

(defn- memory-requirements
  "Summarize frame-relative buffer accesses for ABI validation.

  Constant offsets give callers an up-front minimum buffer length; dynamic
  offsets remain legal in portable IR and are checked by the interpreter at
  each access in development mode. Physical width is part of the ABI because
  f32 and f64 hosts require different typed views and alignment."
  [buffers body precision]
  (let [element-bytes (if (= :f64 precision) 8 4)
        initial (into {}
                      (map (fn [{:keys [id]}]
                             [id {:max-frame-offset 0
                                  :dynamic-index? false
                                  :element-bytes element-bytes
                                  :alignment element-bytes}])
                           buffers))]
    (letfn [(access [requirements {:keys [buffer-id index]}]
              (let [static-offset (case (:op index)
                                    :frame 0
                                    :frame-offset (:offset index)
                                    nil)]
                (if (some? static-offset)
                  (update-in requirements [buffer-id :max-frame-offset]
                             max static-offset)
                  (assoc-in requirements [buffer-id :dynamic-index?] true))))
            (walk [requirements value]
              (cond
                (map? value)
                (let [requirements (if (#{:buffer-load :buffer-store} (:op value))
                                     (access requirements value)
                                     requirements)]
                  (reduce walk requirements (vals value)))
                (sequential? value) (reduce walk requirements value)
                :else requirements))]
      (walk initial body))))

(defn- process-buffers
  [definition channels]
  (let [declared (get-in definition [:process :buffers])]
    (mapv (fn [{:keys [name direction type channels]}]
            {:id name
             :direction direction
             :type type
             :channels channels})
          (or declared
              [{:name :input :direction :input :type :float :channels channels}
               {:name :output :direction :output :type :float :channels channels}]))))

(defn- process-bindings
  [definition states channels]
  (let [process (:process definition)
        sample-name (or (get-in process [:input :name])
                        (get-in definition [:params 0 :name]))
        sample-param (some #(when (= sample-name (:name %)) %) (:params definition))
        controls (if process
                   (mapv (fn [{:keys [name]}]
                           (some #(when (= name (:name %)) %) (:params definition)))
                         (:controls process))
                   (vec (rest (:params definition))))
        frames (or (get-in process [:frames]) {:name 'frames :type :int})
        sample-rate (or (get-in process [:sample-rate])
                        {:name 'sample-rate :type :float})
        channel (if (= 1 channels) 0 {:op :channel :type :int})
        index {:op :frame :type :int}
        sample-expression {:op :buffer-load
                           :buffer-id :input
                           :channel channel
                           :index index
                           :type :float
                           :effects #{:read-memory}}]
    (when-not sample-param
      (fail "a DSP process requires a sample input parameter"
            {:name (:name definition) :input sample-name}))
    (when (some nil? controls)
      (fail "DSP process controls must refer to parameters"
            {:name (:name definition) :controls (:controls process)}))
    (when-not (= :float (:type sample-param))
      (fail "DSP process sample input must have type :float"
            {:name (:name definition) :input sample-name :type (:type sample-param)}))
    {:sample-name sample-name
     :channel channel
     :controls controls
     :frames frames
     :sample-rate sample-rate
     :bindings (into
                {sample-name {:name sample-name
                              :type :float
                              :kind :buffer
                              :expression sample-expression}
                 (:name frames) {:name (:name frames) :type :int :kind :frames}
                 (:name sample-rate) {:name (:name sample-rate)
                                      :type :float
                                      :kind :sample-rate}}
                (concat
                 (map (fn [control]
                        [(:name control) (assoc control :kind :control)])
                      controls)
                 (map (fn [state]
                        [(:name state) (assoc state :kind :state)])
                      states)))}))

(defn- process-diagnostics
  "Return portable safety diagnostics for a process body.

  These are intentionally data, not backend exception strings. Dynamic indices
  remain explicit because their bounds are host/runtime responsibilities; the
  interpreter validates accesses at runtime in development mode."
  [frame-body memory]
  (letfn [(walk [value path diagnostics]
            (cond
              (map? value)
              (let [diagnostics (cond-> diagnostics
                                  (and (= :while (:op value))
                                       (nil? (:bound value)))
                                  (conj {:dsp/warning :unbounded-loop
                                         :dsp/path path}))]
                (reduce-kv (fn [diagnostics key child]
                             (walk child (conj path key) diagnostics))
                           diagnostics value))

              (sequential? value)
              (reduce-kv (fn [diagnostics index child]
                           (walk child (conj path index) diagnostics))
                         diagnostics (vec value))

              :else diagnostics))]
    (into (vec (walk frame-body [:frame-body] []))
          (keep (fn [[buffer-id {:keys [dynamic-index?]}]]
                  (when dynamic-index?
                    {:dsp/warning :dynamic-buffer-index
                     :buffer-id buffer-id
                     :dsp/path [:memory buffer-id]}))
                memory))))

(defn lower-process
  "Lower the entry descriptor directly to explicit process IR.

  The sample parameter is bound to a canonical input-buffer load while controls
  and state retain their own binding kinds. No scalar IR rewriting is involved."
  ([unit] (lower-process unit {}))
  ([unit {:keys [channels options] :or {channels 1}}]
   (when-not (= :compilation-unit (:dsp/kind unit))
     (fail "DSP process lowering expects a linked compilation unit" {:unit unit}))
   (when-not (and (integer? channels) (pos? channels))
     (fail "DSP process channels must be a positive integer" {:channels channels}))
   (let [definitions (:definitions unit)
         entry-id (:entry unit)
         definition (get definitions entry-id)
         _ (normalized-definition! definition)
         _ (when (and (> channels 1) (seq (:state definition)))
             (fail "stateful multi-channel DSP processes are not supported yet"
                   {:name (:name definition) :channels channels}))
         states (state-slots (:state definition))
         buffers (process-buffers definition channels)
         {:keys [bindings channel controls frames sample-rate]} (process-bindings definition states channels)
         env (environment bindings (signatures definitions) (or options {})
                          (into {} (map (juxt :id identity) buffers)))
         output (lower-forms (:body definition) env)
         _ (when-not (:value output)
             (fail "DSP process body does not produce an output value"
                   {:name (:name definition)}))
         _ (expect-type! :float (:value output) (:body definition))
         output-local (fresh-local! env "output" :float)
         output-declaration {:op :declare
                             :name (:name output-local)
                             :type :float
                             :init (:value output)}
         state-results
         (mapv (fn [state]
                 (when-let [next-form (:next state)]
                   (let [next (lower-value next-form env)
                         _ (expect-type! (:type state) (:value next) next-form)
                         local (fresh-local! env
                                             (str (name (:name state)) "_next")
                                             (:type state))]
                     {:state state
                      :statements (conj (:statements next)
                                        {:op :declare
                                         :name (:name local)
                                         :type (:type local)
                                         :init (:value next)})
                      :local local})))
               states)
         frame-statements
         (vec
          (concat
           (:statements output)
           [output-declaration]
           (mapcat :statements (remove nil? state-results))
           [{:op :buffer-store
             :buffer-id :output
             :channel channel
             :index {:op :frame :type :int}
             :value {:op :local :name (:name output-local) :type :float}
             :effects #{:write-memory}}]
           (map (fn [{:keys [state local]}]
                  {:op :state-store
                   :state-id (:name state)
                   :index (:index state)
                   :type (:type state)
                   :value {:op :local :name (:name local) :type (:type local)}
                   :effects #{:write-state}})
                (remove nil? state-results))))
         dependencies (dissoc definitions entry-id)
         lowered-definitions
         (into (array-map)
               (map (fn [[id descriptor]]
                      [id (lower descriptor {:functions (signatures definitions)
                                             :definition-id id
                                             :options options})])
                    dependencies))
         frame-body (block frame-statements)
         memory (memory-requirements buffers frame-body (:precision options))
         diagnostics (process-diagnostics frame-body memory)
         ir {:op :process
             :name (:name definition)
             :id entry-id
             :channels channels
             :buffers buffers
             :memory memory
             :diagnostics diagnostics
             :frames frames
             :sample-rate sample-rate
             :controls controls
             :locals @(:locals env)
             :state (mapv #(select-keys % [:name :type :size :init :index]) states)
             :precision (:precision options)
             :lifecycle (merge {:init true :reset true}
                               (get-in definition [:process :lifecycle]))
             :options (or options {})
             :definitions lowered-definitions
             :entry-id entry-id
             :graph (:graph unit)
             :frame-body frame-body}]
     (assoc ir :effects (collect-effects frame-body)))))

(defn- substitute-expression
  [expr substitutions]
  (case (:op expr)
    :local (get substitutions (:name expr) expr)
    :binary (assoc expr
                   :left (substitute-expression (:left expr) substitutions)
                   :right (substitute-expression (:right expr) substitutions))
    :compare (assoc expr
                    :left (substitute-expression (:left expr) substitutions)
                    :right (substitute-expression (:right expr) substitutions))
    :logical (update expr :args #(mapv (fn [arg]
                                         (substitute-expression arg substitutions)) %))
    :convert (update expr :value substitute-expression substitutions)
    :buffer-load expr
    :state-load expr
    :const expr
    expr))

(defn- inline-expression
  [expr definitions]
  (letfn [(inline* [expr]
            (case (:op expr)
              :call
              (let [callee (get definitions (:function-id expr))
                    statements (get-in callee [:body :statements])]
                (when-not (and (= 1 (count statements))
                               (= :return (:op (first statements))))
                  (fail "backend inlining currently requires expression-only callees"
                        {:function-id (:function-id expr)}))
                (let [args (mapv inline* (:args expr))
                      substitutions (into {}
                                          (map (fn [parameter argument]
                                                 [(:name parameter) argument])
                                               (:params callee) args))]
                  (-> (get-in statements [0 :value])
                      (substitute-expression substitutions)
                      inline*)))
              :binary (assoc expr :left (inline* (:left expr))
                             :right (inline* (:right expr)))
              :compare (assoc expr :left (inline* (:left expr))
                              :right (inline* (:right expr)))
              :logical (update expr :args #(mapv inline* %))
              :convert (update expr :value inline*)
              expr))]
    (inline* expr)))

(defn- map-statement-expressions
  [statement f]
  (case (:op statement)
    :block (update statement :statements
                   #(mapv (fn [child] (map-statement-expressions child f)) %))
    :declare (cond-> statement (:init statement) (update :init f))
    :assign (update statement :value f)
    :expression (update statement :value f)
    :return (update statement :value f)
    :buffer-store (update statement :value f)
    :state-store (update statement :value f)
    :if (-> statement
            (update :condition f)
            (update :then map-statement-expressions f)
            (update :else map-statement-expressions f))
    :while (-> statement
               (update :condition f)
               (update :body map-statement-expressions f))
    statement))

(defn inline-calls
  "Inline expression-only linked callees for the current source-emitting backends."
  [ir]
  (let [definitions (:definitions ir)
        body-key (if (= :process (:op ir)) :frame-body :body)
        body (map-statement-expressions
              (get ir body-key)
              #(inline-expression % definitions))
        ir (assoc ir body-key body :definitions {})]
    (assoc ir :effects (collect-effects body))))

(defn validate-runtime-policy!
  "Validate effect annotations and return static real-time policy metadata."
  [ir]
  (let [maps (filter map? (tree-seq coll? seq ir))
        unknown (into #{} (mapcat #(remove effect-types (or (:effects %) #{})) maps))]
    (when (seq unknown)
      (throw (ex-info "DSP IR contains unknown effects"
                      {:dsp/error :invalid-ir
                       :unknown-effects unknown})))
    {:allocations? false
     :locks? false
     :reflection? false
     :io? false
     :effects (:effects ir)}))

(defn expression-operators
  []
  (into (set (keys arithmetic-operators))
        (concat (keys comparison-operators)
                (keys logical-operators)
                ['if 'let 'do 'set! 'while 'break 'continue
                 'buffer-load 'buffer-store 'state-load 'state-store
                 'int->float 'float->int])))
