(ns omkamra.dsp.wasm
  "Emit WebAssembly text from explicit process IR."
  (:require [clojure.string :as str]))

(def ^:private page-size 65536)
(def ^:private channel-stride-bytes 4096)
;; One sine period sampled at `table-entry-count` intervals, stored as f32 and
;; indexed with linear interpolation. f32-only: a table accurate enough for f64
;; would be far larger than the polynomial it replaces.
(def ^:private table-entry-count 4096)
(def ^:private table-stride-bytes 4)
(def ^:dynamic *precision* nil)
(def ^:dynamic *state-layout* nil)
(def ^:dynamic *wasm-math* :polynomial)
(def ^:dynamic *table-offset* 0)

(defn- f64?
  []
  (case *precision*
    :f64 true
    :f32 false
    (throw (ex-info "Wasm lowering requires explicit DSP precision"
                    {:precision *precision*}))))

(defn- table-active?
  "True when the f32 lookup-table emulation is selected for this module. f64
  keeps the polynomial because a table accurate enough for it is too large."
  []
  (and (= :table *wasm-math*) (not (f64?))))

(defn- float-type [] (if (f64?) "f64" "f32"))
(defn- float-bytes [] (if (f64?) 8 4))

(defn- wat-symbol [value] (symbol (str value)))
(defn- wat-form [operator & args] (apply list (wat-symbol operator) args))

(defn- identifier
  [name]
  (let [name (str/replace (clojure.core/name name) #"[^A-Za-z0-9_$]" "_")]
    (if (re-matches #"^[0-9].*" name) (str "_" name) name)))

(defn- local-name [name] (wat-symbol (str "$" (identifier name))))
(defn- loop-counter-name [index] (wat-symbol (str "$source-loop-count-" index)))

(defn- wat-number
  [value]
  (let [value (if (f64?)
                (double value)
                (float value))]
    (cond
      (Double/isNaN (double value)) (wat-symbol "nan")
      (Double/isInfinite (double value))
      (wat-symbol (if (pos? value) "inf" "-inf"))
      (zero? value) (wat-symbol (if (neg? (double value)) "-0.0" "0.0"))
      :else (wat-symbol (if (f64?)
                          (Double/toString value)
                          (Float/toString (float value)))))))

(defn- align
  [offset alignment]
  (* alignment (long (Math/ceil (/ (double offset) alignment)))))

(defn- state-memory-layout
  [states start]
  (reduce (fn [{:keys [next entries] :as layout} {:keys [index type size]}]
            (let [element-bytes (if (= type :float) (float-bytes) 4)
                  size (or size 1)
                  offset (align next element-bytes)
                  entry {:index index
                         :offset offset
                         :type type
                         :size size
                         :element-bytes element-bytes
                         :alignment element-bytes}]
              (assoc layout
                     :next (+ offset (* size element-bytes))
                     :entries (assoc entries index entry))))
          {:next start :entries {}}
          states))

(defn- memory-layout
  [ir]
  (let [channels (:channels ir)
        element-bytes (float-bytes)
        frame-capacity (quot channel-stride-bytes element-bytes)
        input-bytes (* channels channel-stride-bytes)
        output-offset input-bytes
        output-bytes input-bytes
        state-layout (state-memory-layout (:state ir) (+ output-offset output-bytes))
        table? (table-active?)
        table-offset (align (:next state-layout) table-stride-bytes)
        table-bytes (if table? (* (inc table-entry-count) table-stride-bytes) 0)
        required-bytes (+ table-offset table-bytes)]
    {:page-size page-size
     :table? table?
     :element-type (keyword (float-type))
     :element-bytes element-bytes
     :alignment element-bytes
     :frame-capacity frame-capacity
     :channel-stride channel-stride-bytes
     :required-bytes required-bytes
     :initial-pages (max 1 (long (Math/ceil (/ (double required-bytes)
                                               page-size))))
     :state-layout (:entries state-layout)
     :regions {:input {:offset 0
                       :channels channels
                       :frame-capacity frame-capacity
                       :bytes input-bytes
                       :storage :linear-memory}
               :output {:offset output-offset
                        :channels channels
                        :frame-capacity frame-capacity
                        :bytes output-bytes
                        :storage :linear-memory}
               :state {:storage :linear-memory
                       :offset (+ output-offset output-bytes)
                       :bytes (- required-bytes (+ output-offset output-bytes))
                       :count (reduce + 0 (map #(or (:size %) 1) (:state ir)))}
               :table {:storage :linear-memory
                       :read-only true
                       :offset table-offset
                       :bytes table-bytes
                       :entry-count table-entry-count}
               :controls {:storage :parameters
                          :count (count (:controls ir))}}}))

(defn- initial-pages
  [memory]
  (:initial-pages memory))

(declare emit-expression emit-statement emit-builtin emit-guarded-int-div)

(defn- emit-index
  [index]
  (wat-form "i32.mul"
            (case (:op index)
              :frame (wat-form "local.get" (wat-symbol "$frame"))
              :frame-offset (wat-form "i32.add"
                                      (wat-form "local.get" (wat-symbol "$frame"))
                                      (wat-form "i32.const" (:offset index)))
              :frame-plus (wat-form "i32.add"
                                    (wat-form "local.get" (wat-symbol "$frame"))
                                    (emit-expression (:offset index)))
              (emit-expression index))
            (wat-form "i32.const" (float-bytes))))

(defn- emit-channel
  [channel]
  (cond
    (= :channel (:op channel)) (wat-form "local.get" (wat-symbol "$channel"))
    (map? channel) (emit-expression channel)
    :else (wat-form "i32.const" channel)))

(defn- emit-buffer-address
  [operation]
  (wat-form "i32.add"
            (wat-form "i32.add"
                      (wat-form "local.get"
                                (wat-symbol (str "$" (name (:buffer-id operation)))))
                      (wat-form "i32.mul" (emit-channel (:channel operation))
                                (wat-form "i32.const" channel-stride-bytes)))
            (emit-index (:index operation))))

(defn- wasm-type
  [type]
  (if (= :float type) (float-type) "i32"))

(defn- state-entry
  [operation]
  (or (get *state-layout* (:index operation))
      (throw (ex-info "Wasm state operation refers to an unknown state slot"
                      {:operation operation :state-layout *state-layout*}))))

(defn- emit-state-address
  [operation]
  (let [{:keys [offset element-bytes]} (state-entry operation)
        element-index (:element-index operation)]
    (if element-index
      (wat-form "i32.add"
                (wat-form "i32.const" offset)
                (wat-form "i32.mul" (emit-expression element-index)
                          (wat-form "i32.const" element-bytes)))
      (wat-form "i32.const" offset))))

(defn- emit-expression
  [expression]
  (case (:op expression)
    :const (if (= :float (:type expression))
             (wat-form (str (float-type) ".const") (wat-number (:value expression)))
             (wat-form "i32.const" (if (= :boolean (:type expression))
                                     (if (:value expression) 1 0)
                                     (int (:value expression)))))
    :local (wat-form "local.get" (local-name (:name expression)))
    :state-load (wat-form (str (wasm-type (:type expression)) ".load")
                          (emit-state-address expression))
    :frame (wat-form "local.get" (wat-symbol "$frame"))
    :frames (wat-form "local.get" (wat-symbol "$frames"))
    :sample-rate (wat-form "local.get" (wat-symbol "$sampleRate"))
    :channel (wat-form "local.get" (wat-symbol "$channel"))
    :frame-plus (wat-form "i32.add"
                          (wat-form "local.get" (wat-symbol "$frame"))
                          (emit-expression (:offset expression)))
    :buffer-load (wat-form (str (float-type) ".load")
                           (emit-buffer-address expression))
    :binary (if (and (= :int (:type expression)) (= :/ (:operator expression)))
              (emit-guarded-int-div (emit-expression (:left expression))
                                    (emit-expression (:right expression)))
              (wat-form (if (= :int (:type expression))
                          (case (:operator expression)
                            :+ "i32.add" :- "i32.sub" :* "i32.mul" :/ "i32.div_s")
                          (case (:operator expression)
                            :+ (str (float-type) ".add")
                            :- (str (float-type) ".sub")
                            :* (str (float-type) ".mul")
                            :/ (str (float-type) ".div")))
                        (emit-expression (:left expression))
                        (emit-expression (:right expression))))
    :compare (wat-form (if (not= :float (get-in expression [:left :type]))
                         (case (:operator expression)
                           := "i32.eq" :not= "i32.ne" :< "i32.lt_s"
                           :<= "i32.le_s" :> "i32.gt_s" :>= "i32.ge_s")
                         (case (:operator expression)
                           := (str (float-type) ".eq")
                           :not= (str (float-type) ".ne")
                           :< (str (float-type) ".lt")
                           :<= (str (float-type) ".le")
                           :> (str (float-type) ".gt")
                           :>= (str (float-type) ".ge")))
                       (emit-expression (:left expression))
                       (emit-expression (:right expression)))
    :logical (case (:operator expression)
               :not (wat-form "i32.eqz" (emit-expression (first (:args expression))))
               :and (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                              (emit-expression (first (:args expression)))
                              (wat-form "then"
                                        (emit-expression (second (:args expression))))
                              (wat-form "else" (wat-form "i32.const" 0)))
               :or (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                             (emit-expression (first (:args expression)))
                             (wat-form "then" (wat-form "i32.const" 1))
                             (wat-form "else"
                                       (emit-expression (second (:args expression))))))
    :builtin (emit-builtin expression)
    :bitwise (if (= :bit-not (:operator expression))
               (wat-form "i32.xor" (emit-expression (first (:args expression)))
                         (wat-form "i32.const" -1))
               (wat-form (case (:operator expression)
                           :bit-and "i32.and"
                           :bit-or "i32.or"
                           :bit-xor "i32.xor"
                           :bit-shift-left "i32.shl"
                           :bit-shift-right "i32.shr_s"
                           :unsigned-bit-shift-right "i32.shr_u")
                         (emit-expression (first (:args expression)))
                         (emit-expression (second (:args expression)))))
    :convert (case (:operator expression)
               :int->float (wat-form (str (float-type) ".convert_i32_s")
                                     (emit-expression (:value expression)))
               :float->int (wat-form (str "i32.trunc_" (float-type) "_s")
                                     (emit-expression (:value expression))))))

(defn- emit-guarded-int-div
  "Emit signed integer division with the D12 guards.

  Wasm `i32.div_s` traps both on a zero divisor and on `INT_MIN / -1`, so both
  are lowered to defined values.  Operands are emitted twice; DSP expressions
  are pure, so that only repeats reads and inlined pure computation."
  [left right]
  (wat-form "if" (wat-form "result" (wat-symbol "i32"))
            (wat-form "i32.eqz" right)
            (wat-form "then" (wat-form "i32.const" 0))
            (wat-form "else"
                      (wat-form "if" (wat-form "result" (wat-symbol "i32"))
                                (wat-form "i32.and"
                                          (wat-form "i32.eq" left
                                                    (wat-form "i32.const" -2147483648))
                                          (wat-form "i32.eq" right
                                                    (wat-form "i32.const" -1)))
                                (wat-form "then" (wat-form "i32.const" -2147483648))
                                (wat-form "else" (wat-form "i32.div_s" left right))))))

(defn- emit-guarded-int-rem
  "Emit signed integer remainder with the D12 zero-divisor guard."
  [left right]
  (wat-form "if" (wat-form "result" (wat-symbol "i32"))
            (wat-form "i32.eqz" right)
            (wat-form "then" (wat-form "i32.const" 0))
            (wat-form "else" (wat-form "i32.rem_s" left right))))

(defmulti emit-builtin
  "Wasm emission for a primitive builtin, dispatched on `[operator type]`."
  (fn [expression] [(:builtin expression) (:type expression)]))

(defmethod emit-builtin :default
  [expression]
  (throw (ex-info "Wasm backend does not implement this DSP builtin"
                  {:builtin (:builtin expression) :type (:type expression)})))

(defn- wat-unary
  [operator expression]
  (wat-form operator (emit-expression (first (:args expression)))))

(defn- wat-binary
  [operator expression]
  (wat-form operator
            (emit-expression (first (:args expression)))
            (emit-expression (second (:args expression)))))

(defmethod emit-builtin [:abs :float] [e] (wat-unary (str (float-type) ".abs") e))
(defmethod emit-builtin [:abs :int] [e] (wat-unary "i32.abs" e))
(defmethod emit-builtin [:min :float] [e] (wat-binary (str (float-type) ".min") e))
(defmethod emit-builtin [:max :float] [e] (wat-binary (str (float-type) ".max") e))
(defmethod emit-builtin [:min :int] [e] (wat-binary "i32.min_s" e))
(defmethod emit-builtin [:max :int] [e] (wat-binary "i32.max_s" e))
(defmethod emit-builtin [:floor :float] [e] (wat-unary (str (float-type) ".floor") e))
(defmethod emit-builtin [:ceil :float] [e] (wat-unary (str (float-type) ".ceil") e))
(defmethod emit-builtin [:trunc :float] [e] (wat-unary (str (float-type) ".trunc") e))

(defmethod emit-builtin [:sqrt :float]
  [expression]
  (let [x (emit-expression (first (:args expression)))]
    (if (f64?)
      (wat-form "f64.sqrt" x)
      ;; Match the Java/JavaScript double-rounded reference by computing the
      ;; square root in f64 and narrowing the correctly rounded result.
      (wat-form "f32.demote_f64"
                (wat-form "f64.sqrt" (wat-form "f64.promote_f32" x))))))

(defn- f64-number
  "Format a constant as an f64 literal regardless of kernel precision."
  [value]
  (let [value (double value)]
    (cond
      (Double/isNaN value) (wat-symbol "nan")
      (Double/isInfinite value) (wat-symbol (if (pos? value) "inf" "-inf"))
      (zero? value) (wat-symbol (if (neg? value) "-0.0" "0.0"))
      :else (wat-symbol (Double/toString value)))))

(def ^:private ln2 0.6931471805599453)
(def ^:private log2e 1.4426950408889634)
(def ^:private sqrt2 1.4142135623730951)

(defn- f64-add [a b] (wat-form "f64.add" a b))
(defn- f64-sub [a b] (wat-form "f64.sub" a b))
(defn- f64-mul [a b] (wat-form "f64.mul" a b))
(defn- f64-const [value] (wat-form "f64.const" (f64-number value)))
(defn- i64-const [value] (wat-form "i64.const" (wat-symbol (str value))))

(defn- f64-horner
  "Horner evaluation `c0 + r*(c1 + r*(...))` for `coefficients` low to high."
  [r coefficients]
  (reduce (fn [acc coefficient]
            (f64-add (f64-const coefficient) (f64-mul r acc)))
          (f64-const (last coefficients))
          (reverse (butlast coefficients))))

(defn- taylor-exp-coefficients
  "`1/n!` for n = 0..degree."
  [degree]
  (loop [n 1, factorial 1.0, coefficients [1.0]]
    (if (> n degree)
      coefficients
      (let [factorial (* factorial n)]
        (recur (inc n) factorial (conj coefficients (/ 1.0 factorial)))))))

(defn- f64-exp
  "Portable f64 exp by range reduction and exponent bit construction.

  TODO: a lookup table with a selectable `:wasm-math` strategy is a possible
  faster emulation; see the lookup-table TODO in `DIARY.md`. Keep any
  emulation fixed-size: nesting a self-referential term duplicates the tree.

  `x = k*ln2 + r` with `k = nearest(x*log2e)` and `|r| <= ln2/2`; `exp(r)` is a
  fixed-degree Taylor polynomial and `2^k` is built from the f64 exponent
  field, so the expression tree stays linear in size. `\u00b1inf` and NaN cannot
  reach the i64 conversion, so they are clamped and then restored with
  `select`.

  This is the `:wasm` emulation (D14); the JVM and JavaScript use `Math`."
  [x]
  (let [nan? (wat-form "f64.ne" x x)
        clamped (wat-form "f64.min"
                          (wat-form "f64.max" x (f64-const -708.0))
                          (f64-const 710.0))
        ;; Keep the integer conversion finite when `x` is NaN.
        safe (wat-form "select" (f64-const 0.0) clamped nan?)
        k (wat-form "f64.nearest" (f64-mul safe (f64-const log2e)))
        r (wat-form "f64.sub" safe (f64-mul k (f64-const ln2)))
        poly (f64-horner r (taylor-exp-coefficients 11))
        bits (wat-form "i64.shl"
                       (wat-form "i64.add" (wat-form "i64.trunc_f64_s" k)
                                 (i64-const 1023))
                       (i64-const 52))
        scale (wat-form "f64.reinterpret_i64" bits)
        general (f64-mul poly scale)
        positive-infinity (wat-form "select" (f64-const Double/POSITIVE_INFINITY)
                                    general
                                    (wat-form "f64.eq" x (f64-const Double/POSITIVE_INFINITY)))
        negative-infinity (wat-form "select" (f64-const 0.0)
                                    positive-infinity
                                    (wat-form "f64.eq" x (f64-const Double/NEGATIVE_INFINITY)))]
    (wat-form "select" (f64-const Double/NaN) negative-infinity nan?)))

(defn- taylor-coefficients
  "Taylor coefficients `1/n!` for n = 0..degree."
  [degree]
  (loop [n 1, factorial 1.0, coefficients [1.0]]
    (if (> n degree)
      coefficients
      (let [factorial (* factorial n)]
        (recur (inc n) factorial (conj coefficients (/ 1.0 factorial)))))))

(defn- alternating-sign
  "Apply `(-1)^k`: `k = (n-1)/2` for an odd sine term, `k = n/2` for an even
  cosine term. Callers only pass the terms that contribute."
  [n coefficient]
  (if (odd? (quot (if (odd? n) (dec n) n) 2))
    (- coefficient)
    coefficient))

(defn- sin-coefficients [degree]
  (mapv (fn [n coefficient]
          (if (odd? n) (alternating-sign n coefficient) 0.0))
        (range) (taylor-coefficients degree)))

(defn- cos-coefficients [degree]
  (mapv (fn [n coefficient]
          (if (even? n) (alternating-sign n coefficient) 0.0))
        (range) (taylor-coefficients degree)))

(def ^:private half-pi 1.5707963267948966)
(def ^:private two-over-pi 0.6366197723675814)

(defn- f64-sin-cos
  "Portable f64 sin and cos, sharing one quadrant reduction.

  `x = n*(pi/2) + r` with `n = nearest(x*2/pi)` and `|r| <= pi/4`; a Taylor
  polynomial gives `sin(r)`/`cos(r)`, and the quadrant `n mod 4` selects among
  them and their negations. Non-finite inputs are replaced before the i64
  conversion and their results forced to NaN, which is what `Math.sin`/`cos`
  return for `\u00b1inf` and NaN. Fixed-size; the emulation is `:wasm` only (D14)."
  [x]
  (let [abs-x (wat-form "f64.abs" x)
        nonfinite (wat-form "i32.or"
                            (wat-form "f64.eq" abs-x (f64-const Double/POSITIVE_INFINITY))
                            (wat-form "f64.ne" x x))
        safe (wat-form "select" (f64-const 0.0) x nonfinite)
        n (wat-form "f64.nearest" (f64-mul safe (f64-const two-over-pi)))
        r (f64-sub safe (f64-mul n (f64-const half-pi)))
        sin-r (f64-horner r (sin-coefficients 13))
        cos-r (f64-horner r (cos-coefficients 14))
        neg-sin (wat-form "f64.neg" sin-r)
        neg-cos (wat-form "f64.neg" cos-r)
        quadrant (wat-form "i64.and" (wat-form "i64.trunc_f64_s" n) (i64-const 3))
        zero? (wat-form "i64.eq" quadrant (i64-const 0))
        one? (wat-form "i64.eq" quadrant (i64-const 1))
        two? (wat-form "i64.eq" quadrant (i64-const 2))
        sin-general (wat-form "select" sin-r
                              (wat-form "select" cos-r
                                        (wat-form "select" neg-sin neg-cos two?)
                                        one?)
                              zero?)
        cos-general (wat-form "select" cos-r
                              (wat-form "select" neg-sin
                                        (wat-form "select" neg-cos sin-r two?)
                                        one?)
                              zero?)]
    {:sin (wat-form "select" (f64-const Double/NaN) sin-general nonfinite)
     :cos (wat-form "select" (f64-const Double/NaN) cos-general nonfinite)}))

(defn- i32-const [value] (wat-form "i32.const" (wat-symbol (str value))))

(defn- sine-table-values
  "sin over one period, sampled inclusively so index N interpolates to index 0."
  []
  (let [step (/ (* 2.0 Math/PI) table-entry-count)]
    (mapv (fn [index] (Math/sin (* step index))) (range (inc table-entry-count)))))

(defn- f32-little-endian-bytes
  [values]
  (mapcat (fn [value]
            (let [bits (Float/floatToIntBits (float value))]
              [(bit-and bits 0xff)
               (bit-and (unsigned-bit-shift-right bits 8) 0xff)
               (bit-and (unsigned-bit-shift-right bits 16) 0xff)
               (bit-and (unsigned-bit-shift-right bits 24) 0xff)]))
          values))

(defn- wat-data-string
  "A WAT data string literal. A symbol keeps the quotes and `\\xx` escapes
  verbatim through `pr-str`."
  [bytes]
  (str "\"" (apply str (map #(format "\\%02x" (bit-and 0xff %)) bytes)) "\""))

(defn- table-data-form
  [memory]
  (wat-form "data"
            (i32-const (:offset (:table (:regions memory))))
            (wat-symbol (wat-data-string (f32-little-endian-bytes (sine-table-values))))))

(defn- f64-sin-table
  "f32 lookup-table sin with linear interpolation (the `:table` strategy)."
  [x]
  (let [two-pi (* 2.0 Math/PI)
        nonfinite (wat-form "i32.or"
                            (wat-form "f64.eq" (wat-form "f64.abs" x)
                                      (f64-const Double/POSITIVE_INFINITY))
                            (wat-form "f64.ne" x x))
        safe (wat-form "select" (f64-const 0.0) x nonfinite)
        phase (f64-sub safe
                       (f64-mul (f64-const two-pi)
                                (wat-form "f64.floor"
                                          (f64-mul safe (f64-const (/ 1.0 two-pi))))))
        scaled (f64-mul phase (f64-const (/ table-entry-count two-pi)))
        index (wat-form "i32.trunc_f64_s" scaled)
        fraction (f64-sub scaled (wat-form "f64.convert_i32_s" index))
        load (fn [delta]
               (wat-form "f32.load"
                         (wat-form "i32.add"
                                   (i32-const (+ *table-offset*
                                                 (* table-stride-bytes delta)))
                                   (wat-form "i32.mul" (i32-const table-stride-bytes)
                                             index))))
        low (wat-form "f64.promote_f32" (load 0))
        high (wat-form "f64.promote_f32" (load 1))
        general (f64-add low (f64-mul fraction (f64-sub high low)))]
    (wat-form "select" (f64-const Double/NaN) general nonfinite)))

;; Emulated builtins are emitted once as shared f64 -> f64 Wasm functions and
;; called from each use site, so module size does not multiply with call sites.
(def ^:private emulated-operators
  "Builtins Wasm has no native instruction for; each gets a shared function."
  #{:exp :sin :cos :tan :sinh :cosh :tanh :log :atan :asin :acos :atan2 :pow})

(def ^:private emulated-binary-operators #{:atan2 :pow})

(def ^:private emulated-dependencies
  "Emulated builtins whose shared functions a body calls."
  {:tan #{:sin :cos}
   :sinh #{:exp}
   :cosh #{:exp}
   :tanh #{:exp}
   :asin #{:atan}
   :acos #{:atan2}
   :atan2 #{:atan}
   :pow #{:exp :log}})

(defn- emulated-function-name
  [operator]
  (wat-symbol (str "$dsp_" (identifier operator))))

(defn- emulated-call
  "Call another emulated builtin's shared f64 function."
  [operator & args]
  (apply wat-form "call" (emulated-function-name operator) args))

(defn- f64-div [a b] (wat-form "f64.div" a b))
(defn- f64-neg [a] (wat-form "f64.neg" a))
(defn- f64-abs [a] (wat-form "f64.abs" a))
(defn- f64-copysign [a b] (wat-form "f64.copysign" a b))
(defn- f64-sqrt [a] (wat-form "f64.sqrt" a))
(defn- f64-floor [a] (wat-form "f64.floor" a))
(defn- f64-lt [a b] (wat-form "f64.lt" a b))
(defn- f64-eq [a b] (wat-form "f64.eq" a b))
(defn- i32-and [a b] (wat-form "i32.and" a b))
(defn- i32-eqz [a] (wat-form "i32.eqz" a))

(defn- f64-log
  "Portable f64 natural log: split x = m*2^k with m in [1/sqrt2, sqrt2), then
  `log(m) = 2*atanh((m-1)/(m+1))` as a series. The general path is total (it
  only reinterprets bits), so NaN/zero/negative/infinity are corrected by
  `select` rather than by branching."
  [x]
  (let [bits (wat-form "i64.reinterpret_f64" x)
        exponent (wat-form "i64.and"
                           (wat-form "i64.shr_u" bits (i64-const 52))
                           (i64-const 2047))
        biased (wat-form "i64.sub" exponent (i64-const 1023))
        mantissa (wat-form "i64.or"
                           (wat-form "i64.and" bits (i64-const 4503599627370495))
                           (i64-const 4607182418800017408))
        m0 (wat-form "f64.reinterpret_i64" mantissa)
        halve? (wat-form "f64.ge" m0 (f64-const sqrt2))
        m (wat-form "select" (f64-mul m0 (f64-const 0.5)) m0 halve?)
        k (wat-form "select" (wat-form "i64.add" biased (i64-const 1)) biased halve?)
        t (f64-div (f64-sub m (f64-const 1.0)) (f64-add m (f64-const 1.0)))
        t2 (f64-mul t t)
        poly (f64-horner t2 [1.0 (/ 1.0 3.0) (/ 1.0 5.0) (/ 1.0 7.0) (/ 1.0 9.0)
                             (/ 1.0 11.0) (/ 1.0 13.0) (/ 1.0 15.0) (/ 1.0 17.0)
                             (/ 1.0 19.0) (/ 1.0 21.0) (/ 1.0 23.0)])
        log-m (f64-mul (f64-const 2.0) (f64-mul t poly))
        kf (wat-form "f64.convert_i64_s" k)
        general (f64-add (f64-mul kf (f64-const ln2)) log-m)
        nan? (wat-form "f64.ne" x x)
        negative? (wat-form "f64.lt" x (f64-const 0.0))
        zero? (wat-form "f64.eq" x (f64-const 0.0))
        infinite? (wat-form "f64.eq" x (f64-const Double/POSITIVE_INFINITY))]
    (as-> general g
      (wat-form "select" (f64-const Double/NaN) g nan?)
      (wat-form "select" (f64-const Double/NaN) g negative?)
      (wat-form "select" (f64-const Double/NEGATIVE_INFINITY) g zero?)
      (wat-form "select" (f64-const Double/POSITIVE_INFINITY) g infinite?))))

(defn- f64-atan-body
  "Portable f64 atan as a function body with a scratch local.

  `atan(a) = 2*atan(a/(1+sqrt(1+a^2)))` halves the argument four times (from
  `|x|<=1`), where a short Taylor series is accurate; the result is scaled
  back by 16 and reflected for `|x|>1`. Infinity and NaN fall out of the same
  arithmetic. A local holds the running argument so each halving does not
  duplicate the previous expression."
  [x]
  (let [local (wat-symbol "$a")
        a0 (f64-abs x)
        invert? (wat-form "f64.gt" a0 (f64-const 1.0))
        initial (wat-form "select" (f64-div (f64-const 1.0) a0) a0 invert?)
        halve (fn [a]
                (f64-div a (f64-add (f64-const 1.0)
                                    (wat-form "f64.sqrt"
                                              (f64-add (f64-const 1.0) (f64-mul a a))))))
        a (wat-form "local.get" local)
        t (f64-mul a a)
        poly (f64-horner t [1.0 (/ -1.0 3.0) (/ 1.0 5.0) (/ -1.0 7.0) (/ 1.0 9.0)
                            (/ -1.0 11.0) (/ 1.0 13.0) (/ -1.0 15.0)])
        reduced (f64-mul (f64-const 16.0) (f64-mul a poly))
        result (wat-form "select" (f64-sub (f64-const half-pi) reduced)
                         reduced invert?)]
    {:locals [["a" "f64"]]
     :statements (into [(wat-form "local.set" local initial)]
                       (map (fn [_] (wat-form "local.set" local (halve a)))
                            (range 4)))
     :result (f64-copysign result x)}))

(defn- emulated-builtin-body
  "WAT f64 body expression for an emulated builtin, or nil if not emulated."
  [operator args]
  (let [x (first args)]
    (case operator
      :exp (f64-exp x)
      :sin (if (table-active?) (f64-sin-table x) (:sin (f64-sin-cos x)))
      :cos (if (table-active?)
             (f64-sin-table (f64-add x (f64-const half-pi)))
             (:cos (f64-sin-cos x)))
      :tan (f64-div (emulated-call :sin x) (emulated-call :cos x))
      :sinh (f64-div (f64-sub (emulated-call :exp x)
                              (emulated-call :exp (f64-neg x)))
                     (f64-const 2.0))
      :cosh (f64-div (f64-add (emulated-call :exp x)
                              (emulated-call :exp (f64-neg x)))
                     (f64-const 2.0))
      ;; Bounded form: `(1 - e)/(1 + e)` with `e = exp(-2|x|)` saturates instead
      ;; of overflowing for large `x`, unlike `(exp(x)-exp(-x))/(...)`.
      :tanh (let [e (emulated-call :exp (f64-mul (f64-const -2.0) (f64-abs x)))]
              (f64-copysign (f64-div (f64-sub (f64-const 1.0) e)
                                     (f64-add (f64-const 1.0) e))
                            x))
      :log (f64-log x)
      :atan (f64-atan-body x)
      ;; `atan(x/sqrt(1-x^2))` covers the whole domain: `|x|=1` divides by zero
      ;; to `+/-inf` (atan gives `+/-pi/2`) and `|x|>1` makes the root NaN.
      :asin (emulated-call :atan
                           (f64-div x (f64-sqrt (f64-sub (f64-const 1.0)
                                                         (f64-mul x x)))))
      :acos (emulated-call :atan2 (f64-sqrt (f64-sub (f64-const 1.0)
                                                     (f64-mul x x)))
                           x)
      :atan2 (let [[y x] args
                   quotient (emulated-call :atan (f64-div y x))
                   quarter-turn (wat-form "select"
                                          (f64-copysign (f64-const Math/PI) y)
                                          (f64-const 0.0)
                                          (f64-lt x (f64-const 0.0)))
                   adjusted (f64-add quotient quarter-turn)
                   on-axis (f64-copysign (f64-const half-pi) y)]
               (wat-form "select" on-axis adjusted (f64-eq x (f64-const 0.0))))
      :pow (let [[x y] args
                 magnitude (emulated-call :exp (f64-mul y (emulated-call :log (f64-abs x))))
                 half-y (f64-mul y (f64-const 0.5))
                 integral (i32-and (f64-eq y (f64-floor y))
                                   (f64-lt (f64-abs y) (f64-const 1.0e18)))
                 even (f64-eq (f64-sub half-y (f64-floor half-y)) (f64-const 0.0))
                 odd (i32-and integral (i32-eqz even))
                 signed (wat-form "select" (f64-neg magnitude) magnitude odd)
                 undefined (i32-and (f64-lt x (f64-const 0.0)) (i32-eqz integral))
                 base (wat-form "select" (f64-const Double/NaN) signed undefined)]
             (wat-form "select"
                       (f64-const 1.0)
                       (wat-form "select" (f64-const 1.0) base
                                 (f64-eq y (f64-const 0.0)))
                       (f64-eq x (f64-const 1.0))))
      nil)))

(defn- emit-emulated-call
  [operator expression]
  (let [args (mapv emit-expression (:args expression))
        function (emulated-function-name operator)
        call (if (f64?)
               (apply wat-form "call" function args)
               (apply wat-form "call" function
                      (map #(wat-form "f64.promote_f32" %) args)))]
    (if (f64?)
      call
      ;; Match the Java/JavaScript double-rounded reference by computing in f64
      ;; and narrowing.
      (wat-form "f32.demote_f64" call))))

(defmethod emit-builtin [:exp :float] [expression] (emit-emulated-call :exp expression))
(defmethod emit-builtin [:sin :float] [expression] (emit-emulated-call :sin expression))
(defmethod emit-builtin [:cos :float] [expression] (emit-emulated-call :cos expression))
(defmethod emit-builtin [:tan :float] [expression] (emit-emulated-call :tan expression))
(defmethod emit-builtin [:sinh :float] [expression] (emit-emulated-call :sinh expression))
(defmethod emit-builtin [:cosh :float] [expression] (emit-emulated-call :cosh expression))
(defmethod emit-builtin [:tanh :float] [expression] (emit-emulated-call :tanh expression))
(defmethod emit-builtin [:log :float] [expression] (emit-emulated-call :log expression))
(defmethod emit-builtin [:atan :float] [expression] (emit-emulated-call :atan expression))
(defmethod emit-builtin [:asin :float] [expression] (emit-emulated-call :asin expression))
(defmethod emit-builtin [:acos :float] [expression] (emit-emulated-call :acos expression))
(defmethod emit-builtin [:atan2 :float] [expression] (emit-emulated-call :atan2 expression))
(defmethod emit-builtin [:pow :float] [expression] (emit-emulated-call :pow expression))

(defmethod emit-builtin [:copysign :float]
  [expression]
  (wat-binary (str (float-type) ".copysign") expression))

(defmethod emit-builtin [:rem :int]
  [expression]
  (emit-guarded-int-rem (emit-expression (first (:args expression)))
                        (emit-expression (second (:args expression)))))

(defn- block-form
  [& forms]
  (apply wat-form "block" forms))

(defn- emit-statement
  ([statement]
   (emit-statement statement [] (atom 0)))
  ([statement loop-stack labels]
   (case (:op statement)
     :block (vec (keep #(emit-statement % loop-stack labels)
                       (:statements statement)))
     :declare (wat-form "local.set" (local-name (:name statement))
                        (if-let [init (:init statement)]
                          (emit-expression init)
                          (wat-form (str (wasm-type (:type statement)) ".const") 0)))
     :assign (wat-form "local.set" (local-name (:name statement))
                       (emit-expression (:value statement)))
     :expression (wat-form "drop" (emit-expression (:value statement)))
     :buffer-store (wat-form (str (float-type) ".store")
                             (emit-buffer-address statement)
                             (emit-expression (:value statement)))
     :state-store (wat-form (str (wasm-type (:type statement)) ".store")
                            (emit-state-address statement)
                            (emit-expression (:value statement)))
     :if (apply wat-form "if"
                (concat [(emit-expression (:condition statement))]
                        [(apply wat-form "then"
                                (emit-statement (:then statement) loop-stack labels))]
                        [(apply wat-form "else"
                                (emit-statement (:else statement) loop-stack labels))]))
     :while (let [index (swap! labels inc)
                  loop-name (wat-symbol (str "$source-loop-" index))
                  done-name (wat-symbol (str "$source-loop-done-" index))
                  counter-name (loop-counter-name index)
                  bound (:bound statement)
                  loop-stack (conj loop-stack {:continue loop-name :break done-name})
                  loop-forms (concat
                              [(wat-form "br_if" done-name
                                         (wat-form "i32.eqz"
                                                   (emit-expression
                                                    (:condition statement))))]
                              (when bound
                                [(wat-form "if"
                                           (wat-form "i32.ge_u"
                                                     (wat-form "local.get" counter-name)
                                                     (wat-form "i32.const" bound))
                                           (wat-form "then" (wat-form "unreachable")))
                                 (wat-form "local.set" counter-name
                                           (wat-form "i32.add"
                                                     (wat-form "local.get" counter-name)
                                                     (wat-form "i32.const" 1)))])
                              [(apply block-form
                                      (emit-statement (:body statement)
                                                      loop-stack labels))
                               (wat-form "br" loop-name)])]
              (apply wat-form "block"
                     (concat [done-name]
                             (when bound
                               [(wat-form "local.set" counter-name
                                          (wat-form "i32.const" 0))])
                             [(apply wat-form "loop" loop-name loop-forms)])))
     :break (wat-form "br" (:break (peek loop-stack)))
     :continue (wat-form "br" (:continue (peek loop-stack)))
     :return nil
     nil)))

(defn- flatten-forms
  [forms]
  (mapcat #(if (vector? %) % [%]) (remove nil? forms)))

(defn- state-constant
  [type value]
  (if (= :float type)
    (wat-form (str (float-type) ".const") (wat-number value))
    (wat-form "i32.const" (if (= :boolean type)
                            (if value 1 0)
                            (int value)))))

(defn- emit-init
  [state]
  (apply list
         (concat [(wat-symbol "func") (wat-symbol "$init")
                  (wat-form "export" "init")]
                 (mapcat
                  (fn [{:keys [index init type size]}]
                    (let [{:keys [offset element-bytes]} (get *state-layout* index)
                          values (if (= 1 (or size 1)) [init] init)]
                      (map-indexed
                       (fn [element-index value]
                         (wat-form (str (wasm-type type) ".store")
                                   (wat-form "i32.const"
                                             (+ offset (* element-index element-bytes)))
                                   (state-constant type value)))
                       values)))
                  state))))

(defn- emit-reset
  []
  (list (wat-symbol "func") (wat-symbol "$reset")
        (wat-form "export" "reset")
        (wat-form "call" (wat-symbol "$init"))))

(defn- emit-frame-loop
  [frame-body]
  (wat-form
   "block" (wat-symbol "$frame-done")
   (apply wat-form
          "loop" (wat-symbol "$frames-loop")
          (concat
           [(wat-form "br_if" (wat-symbol "$frame-done")
                      (wat-form "i32.ge_u"
                                (wat-form "local.get" (wat-symbol "$frame"))
                                (wat-form "local.get" (wat-symbol "$frames"))))]
           (flatten-forms (emit-statement frame-body))
           [(wat-form "local.set" (wat-symbol "$frame")
                      (wat-form "i32.add"
                                (wat-form "local.get" (wat-symbol "$frame"))
                                (wat-form "i32.const" 1)))
            (wat-form "br" (wat-symbol "$frames-loop"))]))))

(defn- bounded-loop-count
  [statement]
  (cond
    (= :block (:op statement)) (reduce + 0 (map bounded-loop-count (:statements statement)))
    (= :if (:op statement)) (+ (bounded-loop-count (:then statement))
                               (bounded-loop-count (:else statement)))
    (= :while (:op statement)) (+ (if (:bound statement) 1 0)
                                  (bounded-loop-count (:body statement)))
    :else 0))

(defn- emit-process
  [{:keys [channels controls locals frame-body]}]
  (let [params (concat
                [(wat-form "param" (wat-symbol "$input") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$output") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$frames") (wat-symbol "i32"))
                 (wat-form "param" (wat-symbol "$sampleRate")
                           (wat-symbol (float-type)))]
                (map #(wat-form "param" (local-name (:name %))
                                (wat-symbol (wasm-type (:type %))))
                     controls))
        local-forms (concat
                     (when (> channels 1)
                       [(wat-form "local" (wat-symbol "$channel")
                                  (wat-symbol "i32"))])
                     [(wat-form "local" (wat-symbol "$frame") (wat-symbol "i32"))]
                     (map #(wat-form "local" (loop-counter-name %)
                                     (wat-symbol "i32"))
                          (range 1 (inc (bounded-loop-count frame-body))))
                     (map #(wat-form "local" (local-name (:name %))
                                     (wat-symbol (wasm-type (:type %))))
                          locals))
        frame-loop (emit-frame-loop frame-body)
        loop-body
        (if (= 1 channels)
          [(wat-form "local.set" (wat-symbol "$frame") (wat-form "i32.const" 0))
           frame-loop]
          [(wat-form "local.set" (wat-symbol "$channel") (wat-form "i32.const" 0))
           (wat-form
            "block" (wat-symbol "$channel-done")
            (wat-form
             "loop" (wat-symbol "$channels-loop")
             (wat-form "local.set" (wat-symbol "$frame") (wat-form "i32.const" 0))
             frame-loop
             (wat-form "local.set" (wat-symbol "$channel")
                       (wat-form "i32.add"
                                 (wat-form "local.get" (wat-symbol "$channel"))
                                 (wat-form "i32.const" 1)))
             (wat-form "br_if" (wat-symbol "$channel-done")
                       (wat-form "i32.ge_u"
                                 (wat-form "local.get" (wat-symbol "$channel"))
                                 (wat-form "i32.const" channels)))
             (wat-form "br" (wat-symbol "$channels-loop"))))])]
    (apply list
           (concat [(wat-symbol "func") (wat-symbol "$process")
                    (wat-form "export" "process")]
                   params local-forms loop-body [(wat-form "return")]))))

(defn- emulated-operators-in
  "Distinct emulated builtins reachable in the IR, for shared function emission."
  [ir]
  (letfn [(walk [value operators]
            (cond
              (map? value)
              (reduce-kv
               (fn [operators _ child] (walk child operators))
               (if (and (= :builtin (:op value))
                        (contains? emulated-operators (:builtin value)))
                 (conj operators (:builtin value))
                 operators)
               value)
              (sequential? value) (reduce (fn [operators child]
                                            (walk child operators))
                                          operators value)
              :else operators))]
    (walk ir #{})))

(defn- emulated-closure
  "`operators` plus every emulated builtin their bodies call."
  [operators]
  (loop [todo (seq operators), seen #{}]
    (if-let [operator (first todo)]
      (if (contains? seen operator)
        (recur (next todo) seen)
        (recur (concat (next todo) (get emulated-dependencies operator))
               (conj seen operator)))
      seen)))

(defn- emulation-function-form
  "Emit `(func $dsp_<op> (param f64) (result f64) ...)`.

  A body is either an expression or `{:locals [[name type] ...]
  :statements [forms] :result expr}` for emulations that need scratch
  locals; without them, reusing a temporary would duplicate its expression."
  [operator]
  (let [binary? (contains? emulated-binary-operators operator)
        name (wat-symbol "$x")
        y-name (wat-symbol "$y")
        args (if binary?
               [(wat-form "local.get" name) (wat-form "local.get" y-name)]
               [(wat-form "local.get" name)])
        body (emulated-builtin-body operator args)
        {:keys [locals statements result]} (if (map? body)
                                             body
                                             {:locals [] :statements [] :result body})]
    (apply list
           (concat [(wat-symbol "func") (emulated-function-name operator)
                    (wat-form "param" name (wat-symbol "f64"))]
                   (when binary?
                     [(wat-form "param" y-name (wat-symbol "f64"))])
                   [(wat-form "result" (wat-symbol "f64"))]
                   (map (fn [[local type]]
                          (wat-form "local" (wat-symbol (str "$" local))
                                    (wat-symbol type)))
                        locals)
                   statements
                   [result]))))

(defn emit-module-form
  [ir]
  (when-not (= :process (:op ir))
    (throw (ex-info "Wasm emission requires process IR" {:ir ir})))
  (let [memory (memory-layout ir)
        data-form (when (:table? memory) (table-data-form memory))]
    (binding [*state-layout* (:state-layout memory)
              *table-offset* (get-in memory [:regions :table :offset] 0)]
      (let [emulation-functions (mapv emulation-function-form
                                      (sort-by name (emulated-closure
                                                     (emulated-operators-in ir))))]
        (apply list
               (concat [(wat-symbol "module")
                        (wat-form "memory" (wat-form "export" "memory")
                                  (initial-pages memory))]
                       (when data-form [data-form])
                       emulation-functions
                       [(emit-init (:state ir)) (emit-reset) (emit-process ir)]))))))

(defn emit-module [ir]
  (binding [*precision* (:precision ir)
            *wasm-math* (get-in ir [:options :wasm-math] :polynomial)]
    (pr-str (emit-module-form ir))))

(defn lower
  [ir]
  (binding [*precision* (:precision ir)
            *wasm-math* (get-in ir [:options :wasm-math] :polynomial)]
    (let [memory (memory-layout ir)
          exports [:memory :init :reset :process]]
      {:format :wat
       :source (pr-str (emit-module-form ir))
       :exports exports
       :memory (assoc memory
                      :input-offset (get-in memory [:regions :input :offset])
                      :output-offset (get-in memory [:regions :output :offset]))
       :abi {:kind :wasm-process
             :exports exports
             :memory memory
             :state-storage :linear-memory
             :control-storage :parameters}})))
