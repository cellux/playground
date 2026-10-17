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
    (:state-load :array-load)
    (cond-> {:op (if (= :array-load (:op expression)) :array-load :state-load)
             :index (:index expression) :type (:type expression)}
      (:element-index expression)
      (assoc :element-index (lower-expression (:element-index expression)
                                              channels)))
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
    :bitwise {:op :bitwise :operator (:operator expression) :type :int
              :args (mapv #(lower-expression % channels) (:args expression))}
    :builtin {:op :builtin :builtin (:builtin expression) :type (:type expression)
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
    (:state-store :array-store)
    (cond-> {:op (if (= :array-store (:op statement)) :array-store :state-store)
             :index (:index statement)
             :type (:type statement)
             :value (lower-expression (:value statement) channels)}
      (:element-index statement)
      (assoc :element-index (lower-expression (:element-index statement)
                                              channels)))
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
              ;; A generator (`:input false`, D23) has no input buffer, so its
              ;; `process` takes no input argument and the emitted arity matches
              ;; the ABI's derived argument indices.
              :input? (boolean (some #(= :input (:direction %)) (:buffers ir)))
              :controls (:controls ir)
              :state (:state ir)
              :scratch (:scratch ir)
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

(defn- bitwise-operator
  [operator]
  (case operator
    :bit-and "&"
    :bit-or "|"
    :bit-xor "^"
    :bit-shift-left "<<"
    :bit-shift-right ">>"
    :unsigned-bit-shift-right ">>>"))

(declare emit-expression emit-statement-lines emit-builtin)

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
    (:state-load :array-load)
    (coerce-source (:type expression)
                   (str "state[" (:index expression)
                        (when-let [element-index (:element-index expression)]
                          (str " + " (emit-expression element-index)))
                        "]"))
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
    :bitwise (coerce-source :int
                            (if (= :bit-not (:operator expression))
                              (str "(~" (emit-expression (first (:args expression))) ")")
                              (str "(" (emit-expression (first (:args expression))) " "
                                   (bitwise-operator (:operator expression)) " "
                                   (emit-expression (second (:args expression))) ")")))
    :builtin (coerce-source (:type expression) (emit-builtin expression))
    :convert (case (:operator expression)
               :int->float (coerce-source :float (emit-expression (:value expression)))
               :float->int (coerce-source :int
                                          (str "Math.trunc(" (emit-expression (:value expression)) ")")))))

(defmulti emit-builtin
  "JavaScript emission for a primitive builtin, dispatched on `[operator type]`."
  (fn [expression] [(:builtin expression) (:type expression)]))

(defmethod emit-builtin :default
  [expression]
  (throw (ex-info "JavaScript backend does not implement this DSP builtin"
                  {:builtin (:builtin expression) :type (:type expression)})))

(defn- unary-builtin
  [method expression]
  (str method "(" (emit-expression (first (:args expression))) ")"))

(defn- binary-builtin
  [method expression]
  (str method "(" (emit-expression (first (:args expression))) ", "
       (emit-expression (second (:args expression))) ")"))

(defmethod emit-builtin [:abs :float] [e] (unary-builtin "Math.abs" e))
(defmethod emit-builtin [:abs :int] [e] (unary-builtin "Math.abs" e))
(defmethod emit-builtin [:min :float] [e] (binary-builtin "Math.min" e))
(defmethod emit-builtin [:max :float] [e] (binary-builtin "Math.max" e))
(defmethod emit-builtin [:min :int] [e] (binary-builtin "Math.min" e))
(defmethod emit-builtin [:max :int] [e] (binary-builtin "Math.max" e))
(defmethod emit-builtin [:floor :float] [e] (unary-builtin "Math.floor" e))
(defmethod emit-builtin [:ceil :float] [e] (unary-builtin "Math.ceil" e))
(defmethod emit-builtin [:trunc :float] [e] (unary-builtin "Math.trunc" e))
(defmethod emit-builtin [:sqrt :float] [e] (unary-builtin "Math.sqrt" e))
(defmethod emit-builtin [:exp :float] [e] (unary-builtin "Math.exp" e))
(defmethod emit-builtin [:sin :float] [e] (unary-builtin "Math.sin" e))
(defmethod emit-builtin [:cos :float] [e] (unary-builtin "Math.cos" e))
(defmethod emit-builtin [:tan :float] [e] (unary-builtin "Math.tan" e))
(defmethod emit-builtin [:asin :float] [e] (unary-builtin "Math.asin" e))
(defmethod emit-builtin [:acos :float] [e] (unary-builtin "Math.acos" e))
(defmethod emit-builtin [:atan :float] [e] (unary-builtin "Math.atan" e))
(defmethod emit-builtin [:atan2 :float] [e] (binary-builtin "Math.atan2" e))
(defmethod emit-builtin [:log :float] [e] (unary-builtin "Math.log" e))
(defmethod emit-builtin [:pow :float] [e] (binary-builtin "Math.pow" e))
(defmethod emit-builtin [:sinh :float] [e] (unary-builtin "Math.sinh" e))
(defmethod emit-builtin [:cosh :float] [e] (unary-builtin "Math.cosh" e))
(defmethod emit-builtin [:tanh :float] [e] (unary-builtin "Math.tanh" e))

(defmethod emit-builtin [:copysign :float]
  [expression]
  (let [x (emit-expression (first (:args expression)))
        y (emit-expression (second (:args expression)))]
    (str "(((" y " < 0) || ((" y " === 0) && ((1 / " y ") < 0))) ? -Math.abs(" x
         ") : Math.abs(" x "))")))

(defmethod emit-builtin [:rem :int]
  [expression]
  (str "(" (emit-expression (first (:args expression))) " % "
       (emit-expression (second (:args expression))) ")"))

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
    (:state-store :array-store)
    [(indent level
             (str "state[" (:index statement)
                  (when-let [element-index (:element-index statement)]
                    (str " + " (emit-expression element-index)))
                  "] = "
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

(defn- homogeneous-float-arena?
  [arena]
  (every? #(= :float (:type %)) arena))

(defn- emit-lifecycle-lines
  [state]
  (concat ["  function init() {"]
          (mapcat (fn [{:keys [index init type size]}]
                    (map-indexed (fn [element-index value]
                                   (str "    state[" (+ index element-index) "] = "
                                        (coerce-source type
                                                       (value-source {:type type :value value})) ";"))
                                 (if (= 1 (or size 1)) [init] init)))
                  state)
          ["  }"
           "  function reset() {"
           "    init();"
           "  }"
           ;; `destroy` is the release half of the lifecycle vocabulary: it drops
           ;; the instance's persistent state. The kernel object itself is then
           ;; ready to be collected.
           "  function destroy() {"
           "    state.fill(0);"
           "  }"]))

(defn- emit-process
  [{:keys [channels controls state scratch frame-body input?]}]
  (let [arena (concat state scratch)
        control-params (map #(identifier (:name %)) controls)
        state-init (str "[" (str/join ", "
                                      (concat
                                       (mapcat (fn [{:keys [type init size]}]
                                                 (map #(value-source {:type type :value %})
                                                      (if (= 1 (or size 1)) [init] init)))
                                               state)
                                       ;; Scratch occupies the arena after state
                                       ;; and starts zeroed.
                                       (mapcat (fn [{:keys [type size]}]
                                                 (repeat (or size 1)
                                                         (value-source {:type type :value 0})))
                                               scratch)))
                        "]")
        state-decl (if (homogeneous-float-arena? arena)
                     (str "  const state = new " (if (f64?) "Float64Array" "Float32Array")
                          "(" state-init ");")
                     (str "  const state = " state-init ";"))]
    (emit-lines
     (concat
      ["export function createKernel() {"
       state-decl]
      (emit-lifecycle-lines state)
      [(str "  function process("
            (str/join ", " (concat (when input? ["input"])
                                   ["output" "frames" "sampleRate"]
                                   control-params))
            ") {")]
      (if (= 1 channels)
        (concat ["    for (let frame = 0; frame < frames; frame += 1) {"]
                (emit-statement-lines frame-body 3)
                ["    }"])
        (concat [(str "    for (let channel = 0; channel < " channels
                      "; channel += 1) {")
                 "      for (let frame = 0; frame < frames; frame += 1) {"]
                (emit-statement-lines frame-body 4)
                ["      }" "    }"]))
      ["  }" "  init();" "  return {process, init, reset, destroy};" "}"]))))

(defn emit-module
  [js-ir]
  (binding [*precision* (:precision js-ir)]
    (case (:op js-ir)
      :function (emit-function js-ir)
      :process (emit-process js-ir))))
