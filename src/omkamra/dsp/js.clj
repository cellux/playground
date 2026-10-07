(ns omkamra.dsp.js
  "Structured JavaScript lowering and ES-module emission for DSP IR."
  (:require [clojure.string :as str]))

(def ^:dynamic *precision* nil)

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "JS lowering requires explicit DSP precision"
                    {:precision *precision*}))))

(defn- identifier
  [name]
  (let [name (str/replace (clojure.core/name name) #"[^A-Za-z0-9_$]" "_")]
    (if (re-matches #"^[0-9].*" name) (str "_" name) name)))

(defn- lower-expression
  [expression channels]
  (case (:op expression)
    :const {:op :constant :value (:value expression) :type (:type expression)}
    :local {:op :local :name (:name expression) :type (:type expression)}
    :state-load {:op :state-load :index (:index expression) :type (:type expression)}
    :frame {:op :frame :type :int}
    :frames {:op :frames :type :int}
    :sample-rate {:op :sample-rate :type :float}
    :frame-offset expression
    :frame-plus {:op :frame-plus
                 :offset (lower-expression (:offset expression) channels)
                 :type :int}
    :channel {:op :channel :type :int}
    :buffer-load {:op :buffer-load
                  :buffer-id (:buffer-id expression)
                  :channel (if (map? (:channel expression))
                             (lower-expression (:channel expression) channels)
                             (:channel expression))
                  :channels channels
                  :index (lower-expression (:index expression) channels)}
    :binary {:op :binary :operator (:operator expression) :type (:type expression)
             :left (lower-expression (:left expression) channels)
             :right (lower-expression (:right expression) channels)}
    :compare {:op :compare :operator (:operator expression) :type :boolean
              :left (lower-expression (:left expression) channels)
              :right (lower-expression (:right expression) channels)}
    :logical {:op :logical :operator (:operator expression) :type :boolean
              :args (mapv #(lower-expression % channels) (:args expression))}
    :convert {:op :convert :operator (:operator expression) :type (:type expression)
              :value (lower-expression (:value expression) channels)}
    (throw (ex-info "unsupported expression in JS lowering"
                    {:expression expression}))))

(defn- lower-statement
  [statement channels]
  (case (:op statement)
    :block {:op :block
            :statements (mapv #(lower-statement % channels) (:statements statement))}
    :declare (cond-> (select-keys statement [:op :name :type])
               (:init statement) (assoc :init (lower-expression (:init statement)
                                                                channels)))
    :assign (assoc statement :value (lower-expression (:value statement) channels))
    :expression (assoc statement :value (lower-expression (:value statement) channels))
    :return (assoc statement :value (lower-expression (:value statement) channels))
    :if {:op :if
         :condition (lower-expression (:condition statement) channels)
         :then (lower-statement (:then statement) channels)
         :else (lower-statement (:else statement) channels)}
    :while {:op :while
            :condition (lower-expression (:condition statement) channels)
            :body (lower-statement (:body statement) channels)
            :bound (:bound statement)}
    :buffer-store {:op :buffer-store
                   :buffer-id (:buffer-id statement)
                   :channel (if (map? (:channel statement))
                              (lower-expression (:channel statement) channels)
                              (:channel statement))
                   :channels channels
                   :index (lower-expression (:index statement) channels)
                   :value (lower-expression (:value statement) channels)}
    :state-store {:op :state-store
                  :index (:index statement)
                  :type (:type statement)
                  :value (lower-expression (:value statement) channels)}
    (:break :continue) statement
    (throw (ex-info "unsupported statement in JS lowering"
                    {:statement statement}))))

(defn lower
  [ir]
  (case (:op ir)
    :function {:op :function
               :name (:name ir)
               :precision (:precision ir)
               :params (:params ir)
               :body (lower-statement (:body ir) 1)}
    :process {:op :process
              :name (:name ir)
              :precision (:precision ir)
              :channels (:channels ir)
              :controls (:controls ir)
              :state (:state ir)
              :frames (:frames ir)
              :sample-rate (:sample-rate ir)
              :frame-body (lower-statement (:frame-body ir) (:channels ir))}
    (throw (ex-info "unsupported IR entry in JS lowering" {:ir ir}))))

(defn- number-source
  [value]
  (let [value (if (f64?) (double value) (float value))]
    (cond
      (if (f64?) (Double/isNaN value) (Float/isNaN value)) "NaN"
      (if (f64?) (Double/isInfinite value) (Float/isInfinite value))
      (if (pos? value) "Infinity" "-Infinity")
      :else (str value))))

(defn- value-source
  [expression]
  (case (:type expression)
    :boolean (if (:value expression) "true" "false")
    :int (str (int (:value expression)))
    (if (f64?)
      (number-source (:value expression))
      (str "Math.fround(" (number-source (:value expression)) ")"))))

(defn- coerce-source
  [type source]
  (case type
    :int (str "(" source " | 0)")
    :boolean (str "!!(" source ")")
    :float (if (f64?) source (str "Math.fround(" source ")"))))

(defn- comparison-operator
  [operator]
  (case operator := "===" :not= "!==" (name operator)))

(declare emit-expression emit-statement-lines)

(defn- buffer-source
  [operation]
  (let [name (name (:buffer-id operation))]
    (if (= 1 (:channels operation))
      name
      (str name "["
           (if (map? (:channel operation))
             (emit-expression (:channel operation))
             (str (:channel operation))) "]"))))

(defn- emit-expression
  [expression]
  (case (:op expression)
    :constant (value-source expression)
    :local (coerce-source (:type expression) (identifier (:name expression)))
    :state-load (coerce-source (:type expression)
                               (str "state[" (:index expression) "]"))
    :frame "frame"
    :frames "frames"
    :sample-rate "sampleRate"
    :frame-offset (str "(frame + " (:offset expression) ")")
    :frame-plus (str "(frame + " (emit-expression (:offset expression)) ")")
    :channel "channel"
    :buffer-load (coerce-source :float
                                (str (buffer-source expression)
                                     "[" (emit-expression (:index expression)) "]"))
    :binary (coerce-source (:type expression)
                           (str "(" (emit-expression (:left expression)) " "
                                (name (:operator expression)) " "
                                (emit-expression (:right expression)) ")"))
    :compare (str "(" (emit-expression (:left expression)) " "
                  (comparison-operator (:operator expression)) " "
                  (emit-expression (:right expression)) ")")
    :logical (case (:operator expression)
               :not (str "(!" (emit-expression (first (:args expression))) ")")
               :and (str "(" (emit-expression (first (:args expression))) " && "
                         (emit-expression (second (:args expression))) ")")
               :or (str "(" (emit-expression (first (:args expression))) " || "
                        (emit-expression (second (:args expression))) ")"))
    :convert (case (:operator expression)
               :int->float (coerce-source :float (emit-expression (:value expression)))
               :float->int (coerce-source :int
                                          (str "Math.trunc(" (emit-expression (:value expression)) ")")))))

(defn- indent
  [level text]
  (str (apply str (repeat (* 2 level) " ")) text))

(defn- default-source
  [type]
  (case type
    :int "0"
    :boolean "false"
    (if (f64?) "0" "Math.fround(0)")))

(defn- emit-statement-lines
  [statement level]
  (case (:op statement)
    :block (mapcat #(emit-statement-lines % level) (:statements statement))
    :declare [(indent level
                      (str "let " (identifier (:name statement))
                           " = " (if-let [init (:init statement)]
                                   (emit-expression init)
                                   (default-source (:type statement))) ";"))]
    :assign [(indent level (str (identifier (:name statement)) " = "
                                (emit-expression (:value statement)) ";"))]
    :expression [(indent level (str (emit-expression (:value statement)) ";"))]
    :return [(indent level (str "return " (emit-expression (:value statement)) ";"))]
    :if (concat
         [(indent level (str "if (" (emit-expression (:condition statement)) ") {"))]
         (emit-statement-lines (:then statement) (inc level))
         [(indent level "} else {")]
         (emit-statement-lines (:else statement) (inc level))
         [(indent level "}")])
    :while (let [condition (emit-expression (:condition statement))
                 body (emit-statement-lines (:body statement) (inc level))]
             (if-let [bound (:bound statement)]
               (let [counter (str "__dsp_iterations_" level)]
                 (concat
                  [(indent level (str "let " counter " = 0;")) (indent level (str "while (" condition ") {"))
                   (indent (inc level)
                           (str "if (" counter "++ >= " bound ") throw new Error(\"DSP loop exceeded its iteration bound\");"))]
                  body
                  [(indent level "}")]))
               (concat [(indent level (str "while (" condition ") {"))]
                       body
                       [(indent level "}")])))
    :break [(indent level "break;")]
    :continue [(indent level "continue;")]
    :buffer-store [(indent level
                           (str (buffer-source statement) "["
                                (emit-expression (:index statement)) "] = "
                                (coerce-source :float
                                               (emit-expression (:value statement))) ";"))]
    :state-store [(indent level
                          (str "state[" (:index statement) "] = "
                               (coerce-source (:type statement)
                                              (emit-expression (:value statement))) ";"))]))

(defn- emit-lines
  [lines]
  (str (str/join "\n" lines) "\n"))

(defn- emit-function
  [{:keys [params body]}]
  (emit-lines
   (concat [(str "export function invoke("
                 (str/join ", " (map (comp identifier :name) params)) ") {")]
           (emit-statement-lines body 1)
           ["}"])))

(defn- homogeneous-float-state?
  [state]
  (every? #(= :float (:type %)) state))

(defn- emit-lifecycle-lines
  [state]
  (concat ["  function init() {"]
          (map-indexed (fn [index {:keys [init type]}]
                         (str "    state[" index "] = "
                              (coerce-source type (value-source {:type type :value init})) ";"))
                       state)
          ["  }"
           "  function reset() {"
           "    init();"
           "  }"]))

(defn- emit-process
  [{:keys [channels controls state frame-body]}]
  (let [control-params (map #(identifier (:name %)) controls)
        state-init (str "[" (str/join ", "
                                      (map #(value-source {:type (:type %) :value (:init %)})
                                           state)) "]")
        state-decl (if (homogeneous-float-state? state)
                     (str "  const state = new " (if (f64?) "Float64Array" "Float32Array")
                          "(" state-init ");")
                     (str "  const state = " state-init ";"))]
    (emit-lines
     (concat
      ["export function createKernel() {"
       state-decl]
      (emit-lifecycle-lines state)
      [(str "  function process(input, output, frames, sampleRate"
            (when (seq control-params)
              (str ", " (str/join ", " control-params))) ") {")]
      (if (= 1 channels)
        (concat ["    for (let frame = 0; frame < frames; frame += 1) {"]
                (emit-statement-lines frame-body 3)
                ["    }"])
        (concat [(str "    for (let channel = 0; channel < " channels
                      "; channel += 1) {")
                 "      for (let frame = 0; frame < frames; frame += 1) {"]
                (emit-statement-lines frame-body 4)
                ["      }" "    }"]))
      ["  }" "  init();" "  return {process, init, reset};" "}"]))))

(defn emit-module
  [js-ir]
  (binding [*precision* (:precision js-ir)]
    (case (:op js-ir)
      :function (emit-function js-ir)
      :process (emit-process js-ir))))
