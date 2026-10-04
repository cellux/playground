(ns omkamra.supercollider.ugen
  (:require [omkamra.supercollider.env :as env])
  (:refer-clojure :exclude [compile])
  (:import (java.util IdentityHashMap)))

(def ^:private rates
  {:ir 0
   :scalar 0
   :kr 1
   :control 1
   :ar 2
   :audio 2
   :dr 3
   :demand 3})

(def metadata
  {:SinOsc
   {:name "SinOsc"
    :doc "Sine oscillator."
    :rates #{:ar :kr}
    :inputs [{:name :freq
              :type :signal-or-number
              :default 440.0}
             {:name :phase
              :type :signal-or-number
              :default 0.0}]
    :outputs [{:type :signal
               :rate :same-as-ugen}]
    :special-index 0}

   :Add
   {:name "BinaryOpUGen"
    :doc "Add two signals or numbers."
    :rates #{:ar :kr}
    :inputs [{:name :left :type :signal-or-number}
             {:name :right :type :signal-or-number}]
    :outputs [{:type :signal
               :rate :same-as-ugen}]
    :special-index 0}

   :Mul
   {:name "BinaryOpUGen"
    :doc "Multiply two signals or numbers."
    :rates #{:ar :kr}
    :inputs [{:name :left :type :signal-or-number}
             {:name :right :type :signal-or-number}]
    :outputs [{:type :signal
               :rate :same-as-ugen}]
    :special-index 2}

   :Out
   {:name "Out"
    :doc "Write one or more signals to an audio or control bus."
    :rates #{:ar :kr}
    :inputs [{:name :bus
              :type :bus-index
              :default 0}
             {:name :channels
              :type :signal
              :variadic true}]
    :outputs []
    :special-index 0}

   :EnvGen
   {:name "EnvGen"
    :doc "Generate a signal from an envelope."
    :constructor 'make-envgen
    :rates #{:ar :kr}
    :inputs [{:name :envelope :type :envelope}
             {:name :gate :type :signal-or-number :default 1.0}
             {:name :level-scale :type :signal-or-number :default 1.0}
             {:name :level-bias :type :signal-or-number :default 0.0}
             {:name :time-scale :type :signal-or-number :default 1.0}
             {:name :done-action :type :done-action :default 0}]
    :outputs [{:type :signal :rate :same-as-ugen}]
    :special-index 0}})

(defn rate-number
  [rate]
  (if (keyword? rate)
    (or (get rates rate)
        (throw (IllegalArgumentException.
                (str "unknown UGen rate: " rate))))
    (do
      (when-not (and (integer? rate) (<= 0 rate 127))
        (throw (IllegalArgumentException.
                (str "UGen rate must be an integer or known rate keyword: "
                     (pr-str rate)))))
      rate)))

(defn node
  "Create a raw UGen graph node.

  `inputs` contains numbers, control nodes, UGen nodes, or output references.
  `outputs` contains one calculation rate for each output."
  ([name rate inputs outputs]
   (node name rate inputs outputs {}))
  ([name rate inputs outputs {:keys [special-index]
                              :or {special-index 0}}]
   (when-not (string? name)
     (throw (IllegalArgumentException.
             (str "UGen name must be a string: " (pr-str name)))))
   (when-not (sequential? inputs)
     (throw (IllegalArgumentException.
             (str "UGen inputs must be sequential: " (pr-str inputs)))))
   (when-not (sequential? outputs)
     (throw (IllegalArgumentException.
             (str "UGen outputs must be sequential: " (pr-str outputs)))))
   {:type :ugen
    :name name
    :rate (rate-number rate)
    :inputs (vec inputs)
    :outputs (mapv rate-number outputs)
    :special-index special-index}))

(defn control
  ([name default]
   (control name default nil))
  ([name default index]
   (when-not (or (string? name) (keyword? name) (symbol? name))
     (throw (IllegalArgumentException.
             (str "control name must be a string, keyword, or symbol: "
                  (pr-str name)))))
   (when-not (number? default)
     (throw (IllegalArgumentException.
             (str "control default must be numeric: " (pr-str default)))))
   {:type :control
    :name (if (string? name) name (clojure.core/name name))
    :default default
    :index index}))

(defn output
  "Reference one output of a multi-output UGen node."
  [ugen output-index]
  (when-not (= :ugen (:type ugen))
    (throw (IllegalArgumentException.
            (str "output source must be a UGen node: " (pr-str ugen)))))
  (when-not (and (integer? output-index)
                 (<= 0 output-index)
                 (< output-index (count (:outputs ugen))))
    (throw (IllegalArgumentException.
            (str "UGen output index out of range: " output-index))))
  {:type :output
   :source ugen
   :index output-index})

(defn- graph-value?
  [value]
  (contains? #{:ugen :output :control} (:type value)))

(defn- parameter-key
  [value]
  (cond
    (keyword? value) value
    (symbol? value) (keyword (clojure.core/name value))
    (string? value) (keyword value)
    :else value))

(defn- type-valid?
  [type value]
  (case type
    :number (number? value)
    :bus-index (integer? value)
    :signal (or (number? value) (graph-value? value))
    :signal-or-number (or (number? value) (graph-value? value))
    :any true
    false))

(defn- validate-input!
  [ugen-name input-spec value]
  (when-not (type-valid? (:type input-spec) value)
    (throw (IllegalArgumentException.
            (str ugen-name " input " (:name input-spec)
                 " expects " (:type input-spec)
                 ", got " (pr-str value))))))

(defn- input-specs
  [spec]
  (let [inputs (:inputs spec)
        variadic (filter :variadic inputs)]
    (when (> (count variadic) 1)
      (throw (IllegalArgumentException.
              "UGen metadata may contain at most one variadic input")))
    (when (and (seq variadic)
               (not (:variadic (last inputs))))
      (throw (IllegalArgumentException.
              "a variadic UGen input must be last")))
    inputs))

(defn- required-input?
  [input-spec]
  (and (not (:variadic input-spec))
       (not (contains? input-spec :default))))

(defn- positional-inputs
  [ugen-name specs args]
  (let [variadic (last (filter :variadic specs))
        fixed (if variadic (vec (butlast specs)) (vec specs))]
    (when (< (count args) (count (filter required-input? fixed)))
      (throw (IllegalArgumentException.
              (str ugen-name " is missing required inputs"))))
    (when (and (nil? variadic) (> (count args) (count fixed)))
      (throw (IllegalArgumentException.
              (str ugen-name " received too many inputs"))))
    (let [fixed-values (mapv (fn [input-spec value]
                               (if (= ::missing value)
                                 (:default input-spec)
                                 value))
                             fixed
                             (concat args
                                     (repeat ::missing)))
          variadic-values (if variadic
                            (vec (drop (count fixed) args))
                            [])]
      (when (and variadic (empty? variadic-values))
        (throw (IllegalArgumentException.
                (str ugen-name " requires at least one " (:name variadic)
                     " input"))))
      (into fixed-values variadic-values))))

(defn- named-inputs
  [ugen-name specs named]
  (let [named (into {} (map (fn [[key value]] [(parameter-key key) value]) named))
        known (set (map :name specs))
        unknown (seq (remove known (keys named)))]
    (when unknown
      (throw (IllegalArgumentException.
              (str ugen-name " received unknown inputs: " unknown))))
    (let [values (mapcat (fn [input-spec]
                           (if (:variadic input-spec)
                             (let [value (get named (:name input-spec))
                                   values (if (and (sequential? value)
                                                   (not (graph-value? value)))
                                            (vec value)
                                            (when (some? value) [value]))]
                               (when-not (seq values)
                                 (throw (IllegalArgumentException.
                                         (str ugen-name " requires input "
                                              (:name input-spec)))))
                               values)
                             [(if (contains? named (:name input-spec))
                                (get named (:name input-spec))
                                (if (contains? input-spec :default)
                                  (:default input-spec)
                                  (throw (IllegalArgumentException.
                                          (str ugen-name " is missing input "
                                               (:name input-spec))))))]))
                         specs)]
      (vec values))))

(defn make-ugen
  "Construct a metadata-driven UGen node."
  [metadata-key args]
  (let [spec (get metadata metadata-key)]
    (when-not spec
      (throw (IllegalArgumentException.
              (str "unknown UGen metadata: " metadata-key))))
    (let [first-arg (first args)
          [rate input-args]
          (if (and (map? first-arg) (not (:type first-arg)))
            [(get first-arg :rate) [(dissoc first-arg :rate)]]
            [first-arg (next args)])
          rate (rate-number rate)
          allowed-rates (set (map rate-number (:rates spec)))]
      (when (and (seq (:rates spec)) (not (contains? allowed-rates rate)))
        (throw (IllegalArgumentException.
                (str (:name spec) " does not support rate " rate))))
      (let [specs (input-specs spec)
            values (if (and (= 1 (count input-args))
                           (map? (first input-args))
                           (not (:type (first input-args))))
                     (named-inputs (:name spec) specs (first input-args))
                     (positional-inputs (:name spec) specs input-args))
            expanded-specs (if-let [variadic (last (filter :variadic specs))]
                             (into (vec (butlast specs))
                                   (repeat (- (count values) (count (butlast specs)))
                                           variadic))
                             specs)]
        (doseq [[input-spec value] (map vector expanded-specs values)]
          (validate-input! (:name spec) input-spec value))
        (node (:name spec)
              rate
              values
              (mapv #(if (= :same-as-ugen (:rate %)) rate (:rate %))
                    (:outputs spec))
              {:special-index (:special-index spec)})))))

(def ^:private done-actions
  {:none 0
   :pause-self 1
   :free-self 2
   :free-self-and-prev 3
   :free-self-and-next 4
   :free-self-and-free-all-in-prev 5
   :free-self-and-free-all-in-next 6
   :free-self-to-head 7
   :free-self-to-tail 8
   :free-self-and-deep-free-prev 9
   :free-self-and-deep-free-next 10
   :free-all-in-group 13
   :free-group 14
   :free-self-resume-next 15})

(defn- done-action-number
  [value]
  (cond
    (keyword? value) (or (get done-actions value)
                         (throw (IllegalArgumentException.
                                 (str "unknown done action: " value))))
    (and (integer? value) (<= 0 value 15)) value
    :else (throw (IllegalArgumentException.
                  (str "done action must be an integer or keyword: "
                       (pr-str value))))))

(defn- envelope-data
  [value]
  (let [data (if (env/envelope? value) (env/as-array value) value)]
    (when-not (and (sequential? data) (seq data) (every? number? data))
      (throw (IllegalArgumentException.
              (str "EnvGen envelope must be an Envelope or numeric sequence: "
                   (pr-str value)))))
    (vec data)))

(defn make-envgen
  "Construct an EnvGen node, including envelope literal expansion."
  [_metadata-key args]
  (let [first-arg (first args)
        [rate input-args]
        (if (and (map? first-arg) (not (env/envelope? first-arg)))
          [(get first-arg :rate) [(dissoc first-arg :rate)]]
          [first-arg (next args)])
        rate (rate-number rate)
        allowed-rates (set (map rate-number (:rates (:EnvGen metadata))))
        options (if (and (= 1 (count input-args))
                          (map? (first input-args))
                          (not (env/envelope? (first input-args))))
                  (first input-args)
                  nil)
        options (if (and (nil? options)
                          (= 2 (count input-args))
                          (map? (second input-args)))
                  (assoc (second input-args) :envelope (first input-args))
                  options)
        values (if options
                 (let [known #{:envelope :gate :level-scale :level-bias
                               :time-scale :done-action}
                       unknown (seq (remove known (keys options)))]
                   (when unknown
                     (throw (IllegalArgumentException.
                             (str "EnvGen received unknown inputs: " unknown))))
                   [(get options :envelope)
                    (get options :gate 1.0)
                    (get options :level-scale 1.0)
                    (get options :level-bias 0.0)
                    (get options :time-scale 1.0)
                    (get options :done-action 0)])
                 (let [values (vec input-args)]
                   (when (or (empty? values) (> (count values) 6))
                     (throw (IllegalArgumentException.
                             "EnvGen expects envelope and at most five parameters")))
                   (vec (take 6 (concat values [1.0 1.0 0.0 1.0 0])))))
        [envelope gate level-scale level-bias time-scale done-action] values]
    (when-not (contains? allowed-rates rate)
      (throw (IllegalArgumentException.
              (str "EnvGen does not support rate " rate))))
    (let [inputs (into [gate level-scale level-bias time-scale
                        (done-action-number done-action)]
                       (envelope-data envelope))]
      (node "EnvGen" rate inputs [rate]))))

(defn- defining-ns-symbol
  [namespace name]
  (symbol (str namespace "/" name)))

(defmacro define-ugen
  "Generate a UGen constructor and rate-specific aliases from metadata."
  [constructor-name metadata-key]
  (let [spec (get metadata metadata-key)
        target-ns (ns-name *ns*)
        implementation-ns 'omkamra.supercollider.ugen]
    (when-not spec
      (throw (IllegalArgumentException.
              (str "no metadata for UGen " metadata-key))))
    (let [constructor-var (defining-ns-symbol target-ns constructor-name)
          constructor (if-let [custom (:constructor spec)]
                        (defining-ns-symbol implementation-ns custom)
                        (defining-ns-symbol implementation-ns 'make-ugen))
          aliases (for [rate (sort-by clojure.core/name (:rates spec))]
                    (let [rate-name (clojure.core/name rate)
                          doc (str (:doc spec) " (" rate-name "-rate alias.)")
                          dot-alias (symbol (str constructor-name "." rate-name))
                          colon-alias (symbol (str constructor-name ":" rate-name))]
                      `(do
                         (defn ~dot-alias
                           ~doc
                           [& args#]
                           (apply ~constructor-var ~rate args#))
                         (defn ~colon-alias
                           ~doc
                           [& args#]
                           (apply ~constructor-var ~rate args#)))))]
      `(do
         (defn ~constructor-name
           ~(:doc spec)
           [& args#]
           (~constructor ~metadata-key args#))
         ~@aliases))))

(define-ugen SinOsc :SinOsc)
(define-ugen Add :Add)
(define-ugen Mul :Mul)
(define-ugen Out :Out)
(define-ugen EnvGen :EnvGen)

(defn- identity-map
  []
  (IdentityHashMap.))

(defn- identity-contains?
  [^IdentityHashMap m value]
  (.containsKey m value))

(defn- identity-get
  [^IdentityHashMap m value]
  (.get m value))

(defn- identity-put!
  [^IdentityHashMap m key value]
  (.put m key value)
  value)

(defn- ugen-node?
  [value]
  (= :ugen (:type value)))

(defn- control-node?
  [value]
  (= :control (:type value)))

(defn- output-node?
  [value]
  (= :output (:type value)))

(defn- graph-source
  [value]
  (cond
    (ugen-node? value) value
    (output-node? value) (:source value)
    :else nil))

(defn- collect-graph
  [root controls]
  (let [states (identity-map)
        order (atom [])
        encountered-controls (identity-map)
        control-order (atom [])]
    (letfn [(register-control! [control]
              (when-not (identity-contains? encountered-controls control)
                (identity-put! encountered-controls control true)
                (swap! control-order conj control)))
            (visit [value]
              (cond
                (control-node? value)
                (register-control! value)

                (or (ugen-node? value) (output-node? value))
                (let [ugen (graph-source value)
                      state (identity-get states ugen)]
                  (case state
                    :done nil
                    :visiting (throw (ex-info "cyclic UGen graph"
                                              {:node ugen}))
                    (do
                      (identity-put! states ugen :visiting)
                      (doseq [input (:inputs ugen)]
                        (when-let [source (graph-source input)]
                          (visit source))
                        (when (control-node? input)
                          (register-control! input)))
                      (identity-put! states ugen :done)
                      (swap! order conj ugen))))

                (number? value) nil

                :else
                (throw (IllegalArgumentException.
                        (str "invalid UGen graph value: " (pr-str value))))))]
      (doseq [control controls]
        (when-not (control-node? control)
          (throw (IllegalArgumentException.
                  (str "compile controls must be control nodes: "
                       (pr-str control)))))
        (register-control! control))
      (visit root)
      {:ugens @order
       :controls @control-order})))

(defn- constant-registry
  []
  {:values (atom [])
   :indices (atom {})})

(defn- constant-index!
  [{:keys [values indices]} value]
  (let [value (float value)]
    (if (contains? @indices value)
      (get @indices value)
      (let [index (count @values)]
        (swap! values conj value)
        (swap! indices assoc value index)
        index))))

(defn- compile-graph
  [root controls]
  (let [{:keys [ugens controls] :as graph} (collect-graph root controls)
        controls (vec (sort-by #(or (:index %) Integer/MAX_VALUE) controls))
        control-indices (identity-map)
        _ (doseq [[index control] (map-indexed vector controls)]
            (identity-put! control-indices control index))
        node-indices (identity-map)
        control-ugen? (seq controls)
        _ (doseq [[index ugen] (map-indexed vector ugens)]
            (identity-put! node-indices ugen (+ index (if control-ugen? 1 0))))
        constants (constant-registry)]
    (when (not= (count controls) (count (set (map :name controls))))
      (throw (IllegalArgumentException. "duplicate control names")))
    (letfn [(node-index [ugen]
              (if (identity-contains? node-indices ugen)
                (identity-get node-indices ugen)
                (throw (IllegalArgumentException.
                        (str "UGen node is not in graph: " (pr-str ugen))))))
            (input-reference [input]
              (cond
                (number? input)
                [-1 (constant-index! constants input)]

                (control-node? input)
                (if (identity-contains? control-indices input)
                  [0 (identity-get control-indices input)]
                  (throw (IllegalArgumentException.
                          (str "control is not in graph: " (pr-str input)))))

                (output-node? input)
                [(node-index (:source input)) (:index input)]

                (ugen-node? input)
                [(node-index input) 0]

                :else
                (throw (IllegalArgumentException.
                        (str "invalid UGen input: " (pr-str input))))))
            (emit-ugen [ugen]
              {:name (:name ugen)
               :rate (:rate ugen)
               :inputs (mapv input-reference (:inputs ugen))
               :outputs (:outputs ugen)
               :special-index (:special-index ugen)})]
      {:name nil
       :constants nil
       :param-values (mapv :default controls)
       :params (mapv (fn [[index control]]
                       {:name (:name control)
                        :index index})
                     (map-indexed vector controls))
       :ugens (cond-> []
                control-ugen?
                (conj {:name "Control"
                       :rate 1
                       :inputs []
                       :outputs (vec (repeat (count controls) 1))
                       :special-index 0})

                true
                (into (map emit-ugen ugens)))
       :variants []
       :_constants constants})))

(defn compile
  "Compile a UGen graph root into a SynthDef-shaped map."
  ([root]
   (compile root []))
  ([root controls]
   (let [result (compile-graph root controls)
         constants (-> result :_constants :values deref)]
     (-> result
         (dissoc :_constants)
         (assoc :constants constants)))))
