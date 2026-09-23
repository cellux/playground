(ns omkamra.supercollider.ugen
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

(defn SinOsc
  ([rate freq]
   (SinOsc rate freq 0.0))
  ([rate freq phase]
   (node "SinOsc" rate [freq phase] [rate])))

(defn Out
  [rate bus signal]
  (node "Out" rate [bus signal] []))

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
