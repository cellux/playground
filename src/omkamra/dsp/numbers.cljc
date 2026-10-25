(ns omkamra.dsp.numbers
  "The host's numeric facts, in one place (P13b).

  Two emitters turn a number into text — one into WebAssembly text, one into the
  ClojureScript a JavaScript module is compiled from — and both have to give the
  *same* answer whether they are running on the JVM or in a browser. The
  language underneath them does not. Clojure has a float type and ClojureScript
  has one number type made of doubles, so `(str 2.0)` is `\"2.0\"` on one and
  `\"2\"` on the other; a literal's type cannot be recovered from its value on
  either (D78); and a double is eight bytes that only one of the two hosts will
  hand over as bits. Everything a host decides differently about numbers is
  therefore decided *here*, and nowhere else — including the primitive array a
  host puts reals in, and how a value is read back out of whatever a host
  supplied.

  Three rules keep this honest, and they are what a change here has to respect:

  * the two hosts run the *same* algorithm and are made to agree, rather than one
    delegating to a library the other does not have — so a divergence is a
    failure of this namespace, never a property of the platform;
  * text keeps the JDK's *notation*: plain below `1e7` and above `1e-3`, and
    `d.dddE n` outside that, with at least one digit after the point. That is
    what a Wasm assembler and a JavaScript engine both read, and what the
    emitter's output already looks like — `1.0E7`, not JavaScript's `10000000`;
  * a float's sign is a bit, so `-0.0` is read from the bit pattern and never
    from a number comparison, where it is equal to `0.0` (P12b).

  The digits are the shortest spelling that reads back as the value, which both
  hosts find the same way: ask each host's round-to-`k`-significant-digits
  primitive for `k` from one upward — `%.e` on the JVM, `toExponential` in the
  browser — and stop at the first `k` that round-trips. Both primitives round
  half up on the magnitude, so the search lands on the same digits in both. This
  is deliberately *not* the JDK's own `toString`, whose answer deviates from that
  rule in two measured places, both outside anything a kernel emits and both
  pinned in `omkamra.dsp.tests.numbers`: the smallest subnormals, which it
  spells `4.9E-324` where the shortest spelling is `5.0E-324`; and a float whose
  shortest spelling is an exact decimal tie, which it rounds to even
  (`1439526.2`) where this rounds half up (`1439526.3`)."
  (:refer-clojure :exclude [infinite?])
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; The values and coercions that differ
;; ---------------------------------------------------------------------------

(def positive-infinity
  #?(:clj Double/POSITIVE_INFINITY :cljs js/Infinity))

(def negative-infinity
  #?(:clj Double/NEGATIVE_INFINITY :cljs js/-Infinity))

(def not-a-number
  #?(:clj Double/NaN :cljs js/NaN))

(def pi
  #?(:clj Math/PI :cljs js/Math.PI))

(defn nan?
  [value]
  #?(:clj (Double/isNaN (double value)) :cljs (js/Number.isNaN value)))

(defn infinite?
  "Core's `infinite?` already agrees across the two worlds; this one exists so
  that the pair a caller asks about — NaN and infinity — is asked in one place."
  [value]
  #?(:clj (Double/isInfinite (double value)) :cljs (not (js/isFinite value))))

(defn f32
  "`value` rounded to the nearest f32, still held at the host's width.

  Asked through the JVM's own narrowing conversion rather than Clojure's `float`,
  which is stricter than IEEE 754: it throws for anything above `Float/MAX_VALUE`,
  including values that round to it. The conversion rounds to nearest and to an
  infinity on overflow, which is what `Math.fround` does, so the two hosts agree
  on every input rather than on every input comfortably in range."
  [value]
  #?(:clj (.floatValue (Double/valueOf (double value)))
     :cljs (js/Math.fround value)))

(defn f64
  "`value` as a double. A no-op in a browser, where every number is one, and the
  cast a JVM division needs to stay a division instead of becoming a ratio."
  [value]
  #?(:clj (double value) :cljs value))

(defn truncate
  "`value` with its fraction dropped, toward zero — what `(long value)` means
  where an offset or an index has had a division in it."
  [value]
  #?(:clj (long value) :cljs (js/Math.trunc value)))

(defn ceil
  [value]
  #?(:clj (Math/ceil (double value)) :cljs (js/Math.ceil value)))

(defn ceil-div
  "`numerator / denominator`, rounded up to a whole number."
  [numerator denominator]
  (ceil (/ (f64 numerator) denominator)))

(defn sin
  [value]
  #?(:clj (Math/sin (double value)) :cljs (js/Math.sin value)))

;; ---------------------------------------------------------------------------
;; Bits
;; ---------------------------------------------------------------------------

#?(:cljs
   (def ^:private bit-buffer
     "One buffer, four views of it: a double is eight bytes and a float is the
     first four, which is how the browser reads a bit pattern it has no
     language-level way to ask for."
     (js/ArrayBuffer. 8)))

#?(:cljs (def ^:private bit-f64 (js/Float64Array. bit-buffer)))
#?(:cljs (def ^:private bit-i64 (js/BigInt64Array. bit-buffer)))
#?(:cljs (def ^:private bit-f32 (js/Float32Array. bit-buffer)))
#?(:cljs (def ^:private bit-i32 (js/Int32Array. bit-buffer)))

(defn f64-bits
  "The raw bits of `value` as an f64, as an integer of that width. The JVM
  answers with a long and the browser with a BigInt, because 64 bits do not fit
  a JavaScript number; the two compare and print as one value, which is all a
  caller asks of them."
  [value]
  #?(:clj (Double/doubleToRawLongBits (double value))
     :cljs (do (aset bit-f64 0 value)
               (aget bit-i64 0))))

(defn f32-bits
  "The raw bits of `value` as an f32, as a 32-bit integer.

  An int and not a long on the JVM, because the callers take it apart with
  shifts: a long would sign-extend, and the second byte of a negative sample's
  pattern would read as `00` rather than as the byte that is there."
  [value]
  #?(:clj (Float/floatToRawIntBits (float value))
     :cljs (do (aset bit-f32 0 value)
               (aget bit-i32 0))))

(defn f64-of-bits
  "The f64 whose bit pattern is `bits`, the inverse of `f64-bits`. A check that
  formats a value has to be handed a *value*, and a bit pattern is the one way
  both worlds can name the same double — several decimal spellings of a subnormal
  read back as it, so the text of one is not a way to say which value is meant."
  [bits]
  #?(:clj (Double/longBitsToDouble (long bits))
     :cljs (do (aset bit-i64 0 (js/BigInt bits))
               (aget bit-f64 0))))

(defn f32-of-bits
  [bits]
  #?(:clj (Float/intBitsToFloat (int bits))
     :cljs (do (aset bit-i32 0 bits)
               (aget bit-f32 0))))

(defn f64-negative-zero-bits
  []
  #?(:clj Long/MIN_VALUE :cljs (js/BigInt "0x8000000000000000")))

(defn f32-negative-zero-bits
  []
  #?(:clj Integer/MIN_VALUE :cljs -2147483648))

(defn negative-zero?
  "Whether `value` is negative zero, asked of the bits rather than of the number
  (D76). The comparison against the sign-and-nothing-else pattern above is exact
  because every other bit is zero."
  [value f64?]
  (if f64?
    (= (f64-negative-zero-bits) (f64-bits value))
    (= (f32-negative-zero-bits) (f32-bits value))))

;; ---------------------------------------------------------------------------
;; Text
;; ---------------------------------------------------------------------------

(def ^:private f64-significant-digits
  "Seventeen significant decimal digits always read back as the double they came
  from, so the search never needs to go further."
  17)

(def ^:private f32-significant-digits
  "Nine is to an f32 what seventeen is to a double."
  9)

(defn- rounded-digits
  "`value` as `{:digits :exponent :negative?}` with `significant` significant
  digits, rounded to nearest with a tie going away from zero.

  The exponent is the one an exponential spelling would carry: the power of ten
  the first digit stands for, so `1439526` has exponent six. The JVM asks
  `BigDecimal` of the double, which holds the value exactly, so a tie is a tie
  and the rule is a choice rather than an artifact of a library's own digit
  generation — which is what the JDK's `%e` is."
  [value significant]
  #?(:clj
     (let [rounded (-> (BigDecimal. (double value))
                       (.round (java.math.MathContext.
                                significant
                                java.math.RoundingMode/HALF_UP)))
           unscaled (.unscaledValue rounded)]
       {:digits (str (.abs unscaled))
        :exponent (- (.precision rounded) (.scale rounded) 1)
        :negative? (neg? (.signum rounded))})
     :cljs
     (let [[_ sign integer fraction exponent]
           (re-matches #"(-?)(\d+)(?:\.(\d+))?e([+-]\d+)"
                       (.toExponential value (dec significant)))]
       {:digits (str integer (or fraction ""))
        :exponent (js/parseInt exponent 10)
        :negative? (= "-" sign)})))

(defn- reads-back?
  "Whether `text` reads back as `value` at its own width. Reading is what both
  hosts do correctly, so the round-trip test is the one part of the search that
  can be delegated."
  [value text round]
  (= (round value)
     (round #?(:clj (Double/parseDouble text) :cljs (js/Number text)))))

(defn- java-notation
  "A digit string and decimal exponent spelled the way the JDK spells a number:
  plain when the exponent is in `[-3, 7)`, and `d.dddE n` outside it, with at
  least one digit after the point in either case."
  [{:keys [negative? digits exponent]}]
  (let [sign (if negative? "-" "")
        digits (str/replace digits #"0+$" "")
        digits (if (str/blank? digits) "0" digits)
        length (count digits)]
    (if (and (>= exponent -3) (< exponent 7))
      (if (>= exponent 0)
        (let [point (inc exponent)]
          (str sign
               (if (>= point length)
                 (str digits (apply str (repeat (- point length) "0")))
                 (subs digits 0 point))
               "."
               (if (> length point) (subs digits point) "0")))
        (str sign "0." (apply str (repeat (- -1 exponent) "0")) digits))
      (str sign (subs digits 0 1) "."
           (if (> length 1) (subs digits 1) "0")
           "E" exponent))))

(defn- shortest-text
  "The shortest spelling of `value` that reads back as it, in the JDK's
  notation. `round` reads a value back at its own width: the identity for a
  double, `f32` for a float."
  [value limit round]
  (loop [significant 1]
    (let [text (java-notation (rounded-digits value significant))]
      (if (or (>= significant limit)
              (reads-back? value text round))
        text
        (recur (inc significant))))))

(defn- float-text
  [value limit round]
  (cond
    (nan? value) "NaN"
    (infinite? value) (if (pos? value) "Infinity" "-Infinity")
    ;; The sign of a zero is a bit rather than a digit: an exponential spelling
    ;; of one is `0`, and JavaScript's own conversion drops the sign entirely.
    (zero? value) (if (neg? (/ (double 1) (double value))) "-0.0" "0.0")
    :else (shortest-text (double value) limit round)))

(defn f64->text
  [value]
  (float-text value f64-significant-digits (fn [x] x)))

(defn f32->text
  [value]
  (float-text (double (f32 value)) f32-significant-digits f32))

(defn integer-text
  "An integer as its exact decimal digits.

  JavaScript's own conversion gives the shortest decimal that reads back as the
  value, and above 2^53 that is not the integer: the f64 exponent mask
  printed as `4607182418800017400`, which is a different number from the one
  the module means. A WAT `i64.const` has to be the value, so the digits are
  asked for rather than converted."
  [value]
  #?(:clj (str (long value))
     :cljs (.toString (js/BigInt value))))

(defn hex-byte
  "One byte as a two-digit hexadecimal escape, for the data segment's string."
  [value]
  #?(:clj (format "\\%02x" (bit-and 0xff value))
     :cljs (str "\\" (-> (.toString (bit-and 0xff value) 16)
                         (.padStart 2 "0")))))

;; ---------------------------------------------------------------------------
;; The storage a host puts reals in, and how to read it back
;; ---------------------------------------------------------------------------

(defn real-array
  "A primitive array of `length` reals at the target's precision.

  A host that renders a voice needs somewhere to put its samples that the JVM
  will not box and the browser can hand to a typed view, so the shape of that
  storage is a host decision like any other."
  [f64? length]
  #?(:clj (if f64? (double-array (int length)) (float-array (int length)))
     :cljs (if f64? (js/Float64Array. length) (js/Float32Array. length))))

(defn value-length
  "How many values a host-supplied real sequence holds.

  A host hands over a primitive real array — a typed array on the browser — but
  a test or a slow host may hand over an ordinary sequence, so both are accepted.
  `count` and `nth` do not work on a JS typed array, which is why the hosts reach
  their lane values through this pair rather than directly."
  [values]
  #?(:clj (count values)
     :cljs (if (some? (.-length values))
             (.-length values)
             (clojure.core/count values))))

(defn value-at
  "One value of a host-supplied real sequence, however it is stored."
  [values index]
  #?(:clj (nth values index)
     :cljs (if (some? (.-length values))
             (aget values index)
             (nth values index))))

(defn values-vector
  "A host-supplied real sequence as an ordinary vector.

  A host hands over a primitive real array — a typed array on the browser — but
  a reader that indexes with `nth` (the reference interpreter's control seeding)
  cannot read a typed array, so a caller that needs an ordinary sequence
  converts here rather than testing the storage itself."
  [values]
  (let [length (value-length values)]
    (loop [index 0
           result (transient [])]
      (if (< index length)
        (recur (inc index) (conj! result (value-at values index)))
        (persistent! result)))))
