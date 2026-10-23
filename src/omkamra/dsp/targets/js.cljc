(ns omkamra.dsp.targets.js
  "Structured JavaScript lowering and ES-module emission for DSP IR.

  Lowering turns portable IR into a JavaScript-shaped IR (as before). Emission
  then builds *ClojureScript forms* — plain data — and hands them to
  `squint.compiler`, which owns the JavaScript syntax: statements, `let`
  ordering, operator precedence, and name mangling. Nothing here writes
  JavaScript text except through `js*`, and then only for the exact spellings
  that are the point (`Math.fround`, `| 0`, `break`).

  Three rules follow from letting Squint own the syntax. Each is a *runtime*
  constraint, not a compile-time one, so they are what a change here must
  respect:

  * a local that is `set!` must carry `^:mutable`, or Squint emits `const` and
    the assignment throws when the audio runs (P10d);
  * `break` and `continue` have no ClojureScript form; they are written as
    `(js* \"break\")` and `(js* \"continue\")`;
  * `js*` never names a local — Squint mangles locals — so every value is
    spliced in as `~{}` rather than written by hand;
  * nothing may lower to a `squint_core` reference, because the artifact is a
    standalone module. Squint calls its standard library for any value it cannot
    prove is a boolean, so booleans are built from native comparisons, boolean
    arguments and locals are declared `^boolean`, and `and`/`or`/`not` only ever
    see operands Squint can prove boolean."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [omkamra.dsp.numbers :as numbers]
            ;; Squint owns the JavaScript syntax, and it is portable (P13c): the
            ;; two worlds both hand it the same ClojureScript the emitter builds
            ;; and both get the module text they compare. It is a *compiler*
            ;; dependency, so only the compiler half of a build reaches it — a
            ;; runtime-only consumer never requires this namespace.
            [squint.compiler :as squint]))

(def ^:dynamic *precision* nil)

(def ^:dynamic *callee*
  "The calling context while a definition's body is emitted: `{:base form-or-nil
  :names {definition-id emitted-name}}`.

  `:base` is the form naming the enclosing definition's arena offset, or nil for
  the process entry, whose arena starts at zero and therefore pays nothing; every
  callee adds it to the relative state indices its IR carries (D47). `:names` is one
  map for the whole module, so a call site finds its callee's emitted name."
  nil)

(def ^:dynamic *region-ids*
  "The function IDs whose callee needs the current frame (F2, F3).

  A region is frame-indexed and a lane read is per-frame, and a callee is a
  per-sample function, so a callee that does either is handed the frame; a callee
  that does neither is not. Bound while a process body is lowered, because a call
  site records the fact on its `:call`."
  #{})

(def ^:private arena-base-name
  "The callee ABI's base parameter. Prefixed like everything else the emitter
  introduces, so it cannot collide with a control or a lowered local."
  '__dsp_base)

(defn- arena-base
  "The form to add to a state index, or nil for the entry."
  []
  (:base *callee*))

(defn- based-index
  "A state or scratch index, offset by the enclosing definition's base."
  [index]
  (if-let [base (arena-base)]
    (list '+ base index)
    index))

(defn- call-base-form
  "The arena base a call passes for its callee's partition (D19): the call site's
  offset, plus the enclosing definition's own base when it is a callee rather than
  the entry."
  [partition-base]
  (let [offset (or partition-base 0)
        enclosing (arena-base)]
    (cond
      (nil? enclosing) offset
      (zero? offset) enclosing
      :else (list '+ enclosing offset))))

(defn- callee-name
  [function-id]
  (or (get-in *callee* [:names function-id])
      (throw (ex-info "JS call site refers to a definition that was not emitted"
                      {:function-id function-id
                       :emitted (vec (keys (:names *callee*)))}))))

(defn- definition-name
  "A module-level name for one definition, mangled and indexed so two definitions
  cannot mangle onto each other."
  [index id]
  (symbol (str "__dsp_fn" index "_"
               (str/replace (str id) #"[^A-Za-z0-9_$]" "_"))))

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
    ;; The slice base a fused entry hands a lane-reading callee (F4): a value the
    ;; callee adds to every lane read, exactly as it adds a partition base to
    ;; every state access (D19).
    :lane-base {:op :lane-base :type :int}
    :frames {:op :frames :type :int}
    :sample-rate {:op :sample-rate :type :float}
    :frame-offset expression
    :lane-frame-offset expression
    :frame-plus {:op :frame-plus
                 :offset (lower-expression (:offset expression) channels)
                 :type :int}
    :channel {:op :channel :type :int}
    :buffer-load {:op :buffer-load
                  :buffer-id (:buffer-id expression)
                  :channel (if (map? (:channel expression))
                             (lower-expression (:channel expression) channels)
                             (:channel expression))
                  :per-channel? (:per-channel? expression)
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
    :call (cond-> {:op :call
                   :function-id (:function-id expression)
                   :base (:state-base expression)
                   :args (mapv #(lower-expression % channels) (:args expression))
                   :type (:type expression)}
            (contains? *region-ids* (:function-id expression)) (assoc :frame? true)
            ;; A lane-reading callee is handed the lane region (F3); the IR already
            ;; knows which callees those are, and, in a fused artifact, which
            ;; slice of it this call site reads (F4).
            (:lanes expression) (assoc :lane? true)
            (:lane-base expression) (assoc :lane-base
                                           (lower-expression (:lane-base expression)
                                                             channels)))
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
                   :per-channel? (:per-channel? statement)
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
    :process (binding [*region-ids* (into #{}
                                          (keep (fn [[id definition]]
                                                  (when (or (seq (:region-slots definition))
                                                            (:lane-region? definition))
                                                    id)))
                                          (:definitions ir))]
               {:op :process
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
                :lanes (:lanes ir)
                :shared (:shared ir)
                :frame-body (lower-statement (:frame-body ir) (:channels ir))
                ;; Definitions this target was asked to call, in emission order;
                ;; the rest were copied into their call sites (D47).
                :definitions (into (array-map)
                                   (map (fn [[id definition]]
                                          [id {:params (:params definition)
                                               :frame? (boolean (or (seq (:region-slots definition))
                                                                    (:lane-region? definition)))
                                               :lane? (boolean (:lane-region? definition))
                                               :lane-base? (boolean (:lane-base? definition))
                                               :body (lower-statement (:body definition) 1)}]))
                                   (:definitions ir))})
    (throw (ex-info "unsupported IR entry in JS lowering" {:ir ir}))))

;; ---------------------------------------------------------------------------
;; JavaScript IR -> ClojureScript forms
;; ---------------------------------------------------------------------------

;; The module's own names. Everything the emitter introduces is prefixed, so it
;; cannot collide with a control name or a lowered local; the argument names of
;; `process` are the ABI's, and are the one place Squint's mangling is invisible
;; to the host.
(def ^:private state-arena '__dsp_state)
(def ^:private frame-counter 'frame)
(def ^:private init-name '__dsp_init)
(def ^:private reset-name '__dsp_reset)
(def ^:private destroy-name '__dsp_destroy)
(def ^:private process-name '__dsp_process)

(def ^:private fround-helper '__dsp_fround)
(def ^:private i32-helper '__dsp_i32)

(defn- number-form
  "A floating-point literal, already coerced to the target width.

  A `js*` snippet carrying the spelling, which is the same device this emitter
  writes every exact JavaScript spelling with — and here it is what keeps the
  literal a function of the kernel rather than of the world: a number prints as
  `2.0` on the JVM and as `2` in a browser, which has one number type and no way
  to tell them apart (D78), and a symbol whose name is the spelling would read
  back as a name rather than as a literal. It also leaves the emitted *form* as
  data both worlds can compare, which is what the harness does."
  [value]
  (let [literal (list 'js* ((if (f64?) numbers/f64->text numbers/f32->text) value))]
    (if (f64?) literal (list fround-helper literal))))

(defn- coerce-form
  "Coerce a form to a DSP type, the way the target's arithmetic requires.

  A boolean needs no coercion here: a boolean value is either a comparison or a
  logical, which already lower to a real JavaScript boolean, or an argument that
  `normalize-booleans` has already turned into one."
  [type form]
  (case type
    :int (list i32-helper form)
    :float (if (f64?) form (list fround-helper form))
    form))

(defn- default-form
  "The initial value of a declaration the lowering left without one.

  The float default goes through `number-form` like every other float literal:
  a bare `0.0` prints as `0.0` here and `0` in a browser, so it would make the
  module text host-dependent (P13c found exactly this in an `if` result)."
  [type]
  (case type
    :int 0
    :boolean false
    (number-form 0.0)))

(def ^:private binary-tokens
  {:+ "+" :- "-" :* "*" :/ "/" :rem "%"
   :bit-and "&" :bit-or "|" :bit-xor "^"
   :bit-shift-left "<<" :bit-shift-right ">>" :unsigned-bit-shift-right ">>>"})

;; Squint reaches for its standard library for anything it cannot prove is already
;; a boolean: `and`/`or`/`not` become `squint_core.truth_`/`squint_core.not`, `=`
;; becomes `squint_core._EQ_`, and an `if`/`while` condition it cannot prove
;; boolean is wrapped in `squint_core.truth_`. A kernel has to be a standalone
;; module, so the emitter stays inside what Squint lowers to plain JavaScript:
;; the native comparisons below, or from an argument declared `^boolean`.
(def ^:private native-comparisons
  {:< '< :<= '<= :> '> :>= '>= := '==})

(defn- js-binary
  [tokens operator left right]
  (list 'js* (str "((~{} " (tokens operator) " ~{}))") left right))

(declare expression-form boolean-form)

(defn- known-boolean?
  "True when Squint can already see that a form is a boolean, which is what stops
  it wrapping the form in a standard-library call. A native comparison is one, and
  so is `and`/`or`/`not` of booleans."
  [expression]
  (case (:op expression)
    :compare true
    :logical true
    :constant (= :boolean (:type expression))
    false))

(defn- truth-form
  "A form Squint reads as a boolean. Every boolean here is a real boolean, so
  comparing it with `true` is exact."
  [form]
  (list '== form true))

(defn- boolean-form
  "An expression as a boolean Squint can prove is a boolean."
  [expression]
  (if (known-boolean? expression)
    (expression-form expression)
    (truth-form (expression-form expression))))

(defn- logical-form
  "`and`, `or`, and `not` are ordinary ClojureScript forms here: given boolean
  operands, Squint lowers them to `&&`, `||`, and `!` without a library call."
  [expression]
  (let [[left right] (:args expression)]
    (case (:operator expression)
      :not (list 'not (boolean-form left))
      :and (list 'and (boolean-form left) (boolean-form right))
      :or (list 'or (boolean-form left) (boolean-form right)))))

(defn- index-form
  [statement]
  (based-index (if-let [element-index (:element-index statement)]
                 (list '+ (:index statement) (expression-form element-index))
                 (:index statement))))

(defn- buffer-form
  [operation]
  (let [name (symbol (name (:buffer-id operation)))]
    ;; Only a per-channel buffer is subscripted, so a flat lane region is read
    ;; the same way at every channel.
    (if (:per-channel? operation)
      (list 'aget name (if (map? (:channel operation))
                         (expression-form (:channel operation))
                         (:channel operation)))
      name)))

(defmulti emit-builtin
  "A JavaScript form for a primitive builtin, dispatched on `[operator type]`."
  (fn [expression] [(:builtin expression) (:type expression)]))

(defmethod emit-builtin :default
  [expression]
  (throw (ex-info "JavaScript backend does not implement this DSP builtin"
                  {:builtin (:builtin expression) :type (:type expression)})))

(defn- unary-builtin
  [method expression]
  (list 'js* (str method "(~{})") (expression-form (first (:args expression)))))

(defn- binary-builtin
  [method expression]
  (list 'js* (str method "(~{}, ~{})")
        (expression-form (first (:args expression)))
        (expression-form (second (:args expression)))))

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
  ;; JavaScript has no `Math.copysign`. `x` and `y` are pure by the time they
  ;; reach here — the lowering hoists computed arguments into locals — so
  ;; repeating them costs evaluation, not effects.
  (let [x (expression-form (first (:args expression)))
        y (expression-form (second (:args expression)))]
    (list 'js* (str "(((~{} < 0) || ((~{} === 0) && ((1 / ~{}) < 0)))"
                    " ? -Math.abs(~{}) : Math.abs(~{}))")
          y y y x x)))

(defmethod emit-builtin [:rem :int]
  [expression]
  (js-binary binary-tokens :rem
             (expression-form (first (:args expression)))
             (expression-form (second (:args expression)))))

(defn- expression-form
  [expression]
  (case (:op expression)
    :constant (case (:type expression)
                :boolean (if (:value expression) true false)
                :int (int (:value expression))
                (number-form (:value expression)))
    :local (coerce-form (:type expression) (:name expression))
    :frame frame-counter
    :frames 'frames
    :sample-rate 'sampleRate
    :channel 'channel
    :frame-offset (list '+ frame-counter (:offset expression))
    ;; A lane read in a fused artifact resolves inside the slice this call site was
    ;; handed, so the base the callee was given comes before the lane's own offset
    ;; (F4).
    :lane-frame-offset (list '+ 'lbase frame-counter (:offset expression))
    :lane-base 'lbase
    :frame-plus (list '+ frame-counter (expression-form (:offset expression)))
    (:state-load :array-load)
    (coerce-form (:type expression) (list 'aget state-arena (index-form expression)))
    :buffer-load (coerce-form :float
                              (list 'aget (buffer-form expression)
                                    (expression-form (:index expression))))
    :binary (coerce-form (:type expression)
                         (js-binary binary-tokens (:operator expression)
                                    (expression-form (:left expression))
                                    (expression-form (:right expression))))
    :compare (let [left (expression-form (:left expression))
                   right (expression-form (:right expression))
                   operator (:operator expression)]
               (if-let [native (native-comparisons operator)]
                 (list native left right)
                 ;; `:not=` has no native ClojureScript operator that is not `=`'s
                 ;; deep equality; `(not (== a b))` is exactly `!(a === b)`.
                 (list 'not (list '== left right))))
    :logical (logical-form expression)
    :bitwise (if (= :bit-not (:operator expression))
               (coerce-form :int (list 'js* "(~ ~{})"
                                       (expression-form (first (:args expression)))))
               (coerce-form :int
                            (js-binary binary-tokens (:operator expression)
                                       (expression-form (first (:args expression)))
                                       (expression-form (second (:args expression))))))
    :builtin (coerce-form (:type expression) (emit-builtin expression))
    ;; A call to a definition that was lowered to a function of this kernel (D47).
    ;; The arena is per kernel object and the functions close over it, so neither
    ;; the arena nor the entry's buffers travel: only the partition base, the two
    ;; context values every definition may reference, and the arguments.
    :call (list* (callee-name (:function-id expression))
                 (call-base-form (:base expression))
                 'frames 'sampleRate
                 ;; Something a callee needs by frame is handed the current frame
                 ;; (F2) and something that reads a lane the lane region (F3) —
                 ;; the caller's own, which is the one region a shared lane has.
                 ;; The optional elements and the arguments are one seq, or `list*`
                 ;; would splice only the last and a `when`'s nil would become a
                 ;; literal argument.
                 (concat (when (:frame? expression) ['frame])
                         (when (:lane? expression) ['lanes])
                         (when (:lane-base expression)
                           [(expression-form (:lane-base expression))])
                         (map expression-form (:args expression))))
    :convert (case (:operator expression)
               :int->float (coerce-form :float (expression-form (:value expression)))
               :float->int (coerce-form :int
                                        (list 'js* "Math.trunc(~{})"
                                              (expression-form (:value expression)))))
    (throw (ex-info "unsupported expression in JS emission"
                    {:expression expression}))))

(declare statement-forms)

(defn- loop-form
  "A `while`, with the loop's statically required iteration bound when it has
  one. The counter is checked before the body, so a loop that cannot finish
  traps instead of running until the audio thread starves."
  [{:keys [condition body bound]}]
  (let [body (statement-forms [body])]
    (if bound
      (let [counter (with-meta (gensym "__iter") {:mutable true})
            reached (gensym "__reached")]
        (list 'let [counter 0]
              (list 'while (boolean-form condition)
                    (cons 'do
                          (concat
                           [(list 'let [reached (list '>= counter bound)]
                                  (list 'set! counter (list '+ counter 1))
                                  (list 'if (truth-form reached)
                                        (list 'throw
                                              (list 'js/Error.
                                                    "DSP loop exceeded its iteration bound"))
                                        nil))]
                           body)))))
      (list 'while (boolean-form condition) (cons 'do body)))))

(defn- statement-form
  ;; A trailing `:return` is a function's value, and Squint returns a body's last
  ;; form, so it is emitted as the value itself. Any other position is an early
  ;; return the syntax needs said out loud.
  ([statement] (statement-form statement true))
  ([statement last?]
   (case (:op statement)
     :assign (list 'set! (:name statement) (expression-form (:value statement)))
     :expression (expression-form (:value statement))
     :return (if last?
               (expression-form (:value statement))
               (list 'js* "return ~{}" (expression-form (:value statement))))
     :if (list 'if (boolean-form (:condition statement))
               (cons 'do (statement-forms [(:then statement)]))
               (cons 'do (statement-forms [(:else statement)])))
     :while (loop-form statement)
     :break '(js* "break")
     :continue '(js* "continue")
     :buffer-store (list 'aset (buffer-form statement)
                         (expression-form (:index statement))
                         (coerce-form :float (expression-form (:value statement))))
     (:state-store :array-store)
     (list 'aset state-arena (index-form statement)
           (coerce-form (:type statement) (expression-form (:value statement))))
     (throw (ex-info "unsupported statement in JS emission"
                     {:statement statement})))))

(defn- statement-forms
  "The forms a statement sequence becomes.

  A declaration opens a `let`, so a local is declared where the target declared
  it and the rest of the sequence is its body; the rest of the sequence is
  therefore nested inside it rather than appended after it."
  [statements]
  (when (seq statements)
    (let [statement (first statements)
          remaining (rest statements)]
      (case (:op statement)
        :block (concat (statement-forms (:statements statement))
                       (statement-forms remaining))
        :declare (let [name (:name statement)
                       metadata (cond-> {:mutable true}
                                  (= :boolean (:type statement))
                                  (assoc :tag 'boolean))
                       init (if (contains? statement :init)
                              (expression-form (:init statement))
                              (default-form (:type statement)))]
                   ;; Every declared local is mutable: the target's assigns are
                   ;; not known here, and a `set!` to a `const` is a runtime
                   ;; error rather than a compile-time one. A boolean local is
                   ;; also tagged, so Squint lowers the boolean forms around it
                   ;; to native JavaScript.
                   [(list 'let [(with-meta name metadata) init]
                          (cons 'do (statement-forms remaining)))])
        (cons (statement-form statement (empty? remaining))
              (statement-forms remaining))))))

;; ---------------------------------------------------------------------------
;; The module
;; ---------------------------------------------------------------------------

(defn- arena-values
  "Every arena slot as `[type initial-value]`, state first then scratch. A
  fixed-size array's initial value is a vector, one entry per element."
  [js-ir]
  (concat (mapcat (fn [{:keys [type init size]}]
                    (map (fn [value] [type value])
                         (if (= 1 (or size 1)) [init] init)))
                  (:state js-ir))
          (mapcat (fn [{:keys [type size]}]
                    (repeat (or size 1) [type 0]))
                  (:scratch js-ir))))

(defn- literal-text
  [type value]
  (case type
    :int (str (int value))
    :boolean (if value "true" "false")
    (let [text ((if (f64?) numbers/f64->text numbers/f32->text) value)]
      (if (f64?) text (str "Math.fround(" text ")")))))

(defn- arena-form
  "The arena literal: a typed array when every slot is a float, so the common
  case keeps its element type, and a plain array otherwise."
  [js-ir]
  (let [values (arena-values js-ir)
        literal (list 'js* (str "["
                                (str/join ", " (map (fn [[type value]]
                                                      (literal-text type value))
                                                    values))
                                "]"))]
    (if (every? (fn [[type _]] (= type :float)) values)
      (list (if (f64?) 'js/Float64Array. 'js/Float32Array.) literal)
      literal)))

(defn- init-forms
  [js-ir]
  (mapcat (fn [{:keys [index init type size]}]
            (map-indexed (fn [offset value]
                           (list 'aset state-arena (+ index offset)
                                 (coerce-form type
                                              (expression-form {:op :constant
                                                                :type type
                                                                :value value}))))
                         (if (= 1 (or size 1)) [init] init)))
          (:state js-ir)))

(defn- parameter-symbol
  "A parameter as a binding symbol, tagged when it is a boolean so Squint lowers
  the boolean forms around it to native JavaScript."
  [parameter]
  (let [name (symbol (identifier (:name parameter)))]
    (if (= :boolean (:type parameter))
      (with-meta name {:tag 'boolean})
      name)))

(defn- normalize-booleans
  "A boolean argument may arrive as a number: an `AudioParam` is a number, and a
  pool's control arrays are numeric. The boundary turns each one into a real
  boolean, once — `(not (not x))` is `!!x`, because the argument is `^boolean` —
  so that everything downstream is a boolean and can be compared with `true`."
  [parameters body]
  (let [names (map parameter-symbol (filter #(= :boolean (:type %)) parameters))]
    (if (seq names)
      (list 'let (vec (mapcat (fn [name]
                                [name (list 'not (list 'not name))])
                              names))
            body)
      body)))

(defn- callee-form
  "One `letfn` binding per definition the target was asked to call (D47).

  The four leading parameters are the ABI: the partition base, then the two context
  values every definition may reference. A callee reaches nothing else — its
  environment has no bindings — and the arena needs no parameter because these
  functions are defined inside the kernel and close over it. A callee that
  addresses anything by frame takes the current frame next (F2, F3) and a
  lane-reading one the lane region after that (F3), in the order the call site
  passes them — and, in a fused artifact, the base of the lane slice it reads after
  that (F4).

  The form is `(name [args] body)`, like every other `letfn` binding here: emitting
  a `defn` instead makes Squint name the entry itself (`defn_6`), and two of them
  collide as `Identifier 'defn_6' has already been declared`."
  [name definition]
  (binding [*callee* (assoc *callee* :base arena-base-name)]
    (list name
          (vec (concat [arena-base-name 'frames 'sampleRate]
                       (when (:frame? definition) ['frame])
                       (when (:lane? definition) ['lanes])
                       (when (:lane-base? definition) ['lbase])
                       (map parameter-symbol (:params definition))))
          (normalize-booleans (:params definition)
                              (cons 'do (statement-forms [(:body definition)]))))))

(defn- process-form
  [js-ir]
  (let [controls (map parameter-symbol (:controls js-ir))
        params (vec (concat (when (:input? js-ir) ['input])
                            ['output 'frames 'sampleRate]
                            controls
                            ;; Any number of lanes is one flat region, so it is
                            ;; one argument rather than one per lane.
                            (when (seq (:lanes js-ir)) ['lanes])
                            ;; The pool's shared region is one more region of the
                            ;; same shape, one argument rather than one per
                            ;; shared control (M5).
                            (when (seq (:shared js-ir)) ['shared])))
        names (into (array-map)
                    (map-indexed (fn [index [id _]]
                                   [id (definition-name index id)]))
                    (:definitions js-ir))
        ;; The binding is written out at each site rather than put behind a
        ;; helper, and that is not a style choice: the emitter resolves a call
        ;; site's callee *while it builds* the form, so the binding has to wrap
        ;; the construction. Behind a function it took an already-emitted
        ;; argument, the binding came too late, and the result was `*callee*` nil
        ;; inside a call — which the module's own error message then reported as
        ;; a definition that was never emitted.
        callees (binding [*callee* {:base nil :names names}]
                  (mapv (fn [[id definition]]
                          (callee-form (get names id) definition))
                        (:definitions js-ir)))
        frame (with-meta frame-counter {:mutable true})
        ;; One evaluation per frame (D40): no channel loop.
        body (binding [*callee* {:base nil :names names}]
               (normalize-booleans
                (:controls js-ir)
                (list 'let [frame 0]
                      (list 'while (list '< frame-counter 'frames)
                            (cons 'do
                                  (concat
                                   (statement-forms [(:frame-body js-ir)])
                                   [(list 'set! frame-counter
                                          (list i32-helper
                                                (list '+ frame-counter 1)))]))))))]
    (list 'defn (with-meta 'createKernel {:export true}) []
          (list 'let [state-arena (arena-form js-ir)]
                (list 'letfn (into [(list init-name [] (cons 'do (init-forms js-ir)))
                                    (list reset-name [] (list init-name))
                                    ;; `destroy` is the release half of the lifecycle
                                    ;; vocabulary: it drops the instance's persistent
                                    ;; state, and the kernel object is then ready to be
                                    ;; collected.
                                    (list destroy-name [] (list '.fill state-arena 0))]
                                   (conj callees
                                         (list process-name params body)))
                      (list init-name)
                      ;; The kernel object exposes its arena as well as the
                      ;; lifecycle, so a host can read a voice's special slots
                      ;; (D39) without a second entry point.
                      (list 'js* (str "({process: ~{}, init: ~{}, reset: ~{},"
                                      " destroy: ~{}, state: ~{}})")
                            process-name init-name reset-name destroy-name
                            state-arena))))))

(defn- function-form
  [js-ir]
  (list 'defn (with-meta 'invoke {:export true})
        (mapv parameter-symbol (:params js-ir))
        (normalize-booleans (:params js-ir)
                            (cons 'do (statement-forms [(:body js-ir)])))))

(defn- helper-definition
  [name template]
  (list 'defn- name (vector 'x) (list 'js* template 'x)))

(def ^:private helper-definitions
  [[fround-helper (helper-definition fround-helper "Math.fround(~{})")]
   [i32-helper (helper-definition i32-helper "(~{} | 0)")]])

(defn- referenced?
  [form symbol]
  (boolean (some #(= symbol %) (tree-seq coll? seq form))))

(defn- strip-location
  "Drop reader metadata, so the only metadata that reaches Squint is the
  metadata this emitter means: `^:mutable`, `^:export`, and `^boolean`."
  [form]
  (walk/postwalk (fn [value]
                   (if #?(:clj (instance? clojure.lang.IObj value)
                          :cljs (implements? IWithMeta value))
                     (vary-meta value dissoc :line :column :file)
                     value))
                 form))

(defn emit-forms
  "The forms a kernel's module is printed from: the helpers it uses, then the
  entry point, with reader metadata dropped so the only metadata left is the kind
  this emitter means (`^:mutable`, `^:export`, `^boolean`).

  Public because this is the JS target's portable half — the part both worlds
  have to agree on, and the part the harness compares across them (P13b). Every
  host-dependent decision has already been made by the time a form is here: a
  float literal is a `js*` snippet carrying its spelling, so the form is data
  the two worlds can compare rather than text each of them prints its own way."
  [js-ir]
  (binding [*precision* (:precision js-ir)]
    (let [definition (case (:op js-ir)
                       :function (function-form js-ir)
                       :process (process-form js-ir))]
      (mapv strip-location
            (concat (map second
                         (filter (fn [[helper _]]
                                   (referenced? definition helper))
                                 helper-definitions))
                    [definition])))))

(defn emit-source
  "The ClojureScript a kernel is compiled from, as text."
  [js-ir]
  (binding [*print-meta* true]
    (str (str/join "\n\n" (map pr-str (emit-forms js-ir))) "\n")))

(defn emit-module
  "The ES module a kernel is loaded from: the forms above, compiled by Squint.

  Portable since P13c: the browser compiles the module itself, from the same
  forms and with the same Squint, and the two worlds' module texts are compared
  as the JS half of the compiler equivalence check. `:elide-imports` keeps the
  artifact a standalone module: every form above lowers to plain JavaScript, so
  it never reaches for the Squint stdlib."
  [js-ir]
  (squint/compile-string (emit-source js-ir) {:elide-imports true}))
