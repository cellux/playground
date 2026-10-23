(ns omkamra.dsp.tests.numbers
  "The host's numeric facts, and where this deliberately parts from the JDK (P13b).

  Two emitters turn a number into text, and the browser has to produce the text
  the JVM produces (D77). `omkamra.dsp.numbers` is the one place that decides, on
  the rule that the shortest spelling that reads back as the value is rounded
  half away from zero and written in the JDK's notation — not the JDK's own
  spelling, whose answer deviates from that rule in two measured places. Those
  two are pinned here, because they are the reason the rule is stated rather than
  delegated, and a change to either of them should be a decision and not a
  discovery. The cross-world agreement itself is not testable from here: it is the
  `number formatting` browser check, which spells a swept corpus in the page and
  compares it with this side's."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp.numbers :as numbers]))

(def ^:private f32-tie
  "A float whose shortest spelling is an exact decimal tie: 1439526.25, eight
  significant digits, the last dropped digit exactly five."
  (float 1439526.25))

(deftest a-double-is-spelled-in-the-jdks-notation
  (testing "plain below 1e7 and above 1e-3, with a digit after the point"
    (is (= "0.0" (numbers/f64->text 0.0)))
    (is (= "1.0" (numbers/f64->text 1.0)))
    (is (= "-2.5" (numbers/f64->text -2.5)))
    (is (= "0.5" (numbers/f64->text 0.5)))
    (is (= "0.001" (numbers/f64->text 0.001)))
    (is (= "1000000.0" (numbers/f64->text 1000000.0)))
    (is (= "1234567.0" (numbers/f64->text 1234567.0))))
  (testing "and `d.dddE n` outside it, with no sign on the exponent"
    (is (= "1.0E7" (numbers/f64->text 1e7)))
    (is (= "1.0E-4" (numbers/f64->text 1e-4)))
    (is (= "1.0E21" (numbers/f64->text 1e21)))
    (is (= "1.23456789E8" (numbers/f64->text 123456789.0))))
  (testing "the digits are the shortest that read back as the value"
    (is (= "0.1" (numbers/f64->text 0.1)))
    (is (= "0.30000000000000004" (numbers/f64->text (+ 0.1 0.2))))
    (is (= "1.7976931348623157E308" (numbers/f64->text Double/MAX_VALUE))))
  (testing "and the three values that are not a number at all"
    (is (= "NaN" (numbers/f64->text Double/NaN)))
    (is (= "Infinity" (numbers/f64->text Double/POSITIVE_INFINITY)))
    (is (= "-Infinity" (numbers/f64->text Double/NEGATIVE_INFINITY)))))

(deftest a-float-is-spelled-at-its-own-width
  (is (= "0.1" (numbers/f32->text (float 0.1))))
  (is (= "-0.16666667" (numbers/f32->text (float -0.16666667))))
  (is (= "3.4028235E38" (numbers/f32->text Float/MAX_VALUE)))
  (is (= "1.0E7" (numbers/f32->text (float 1e7))))
  (is (= "0.001" (numbers/f32->text (float 0.001)))))

(deftest the-two-places-this-parts-from-the-jdk
  (testing "the smallest subnormals: the JDK spells more digits than the shortest"
    ;; The JDK's `Double.toString` answers `4.9E-324` for a value the shortest
    ;; spelling is `5.0E-324`, which is the documented behaviour its own javadoc
    ;; disagrees with; the browser rounds and searches, so it says the shortest.
    (is (= "4.9E-324" (Double/toString Double/MIN_VALUE)))
    (is (= "5.0E-324" (numbers/f64->text Double/MIN_VALUE)))
    (is (= "9.9E-324" (Double/toString (Double/longBitsToDouble 2))))
    (is (= "1.0E-323" (numbers/f64->text (Double/longBitsToDouble 2)))))
  (testing "an exact decimal tie: the JDK rounds to even, this rounds away from zero"
    (is (= "1439526.2" (Float/toString f32-tie)))
    (is (= "1439526.3" (numbers/f32->text f32-tie)))
    ;; The rule is the same for a double, at the width where a double can tie.
    (is (= "-1.9008343391046512E14"
           (Double/toString (Double/longBitsToDouble
                             (.longValue (java.math.BigInteger. "c2e59c27f1ca1824" 16))))))
    (is (= "-1.9008343391046513E14"
           (numbers/f64->text (Double/longBitsToDouble
                               (.longValue (java.math.BigInteger. "c2e59c27f1ca1824" 16))))))))

(deftest an-integer-is-its-exact-digits
  ;; JavaScript's own conversion gives the shortest decimal that reads back as
  ;; the value, and above 2^53 that is not the integer: the f64 exponent mask
  ;; would be spelled one digit off, which assembles to a different module.
  (is (= "4607182418800017408" (numbers/integer-text 4607182418800017408)))
  (is (= "4503599627370495" (numbers/integer-text 4503599627370495)))
  (is (= "-9007199254740993" (numbers/integer-text -9007199254740993)))
  (is (= "0" (numbers/integer-text 0))))

(deftest the-sign-of-a-zero-is-a-bit
  (is (numbers/negative-zero? -0.0 true))
  (is (not (numbers/negative-zero? 0.0 true)))
  (is (numbers/negative-zero? (float -0.0) false))
  (is (not (numbers/negative-zero? (float 0.0) false)))
  ;; A comparison cannot answer this: the two are equal.
  (is (= -0.0 0.0)))

(deftest rounding-to-f32-is-ieee
  ;; Clojure's `float` throws above `Float/MAX_VALUE`, including for values that
  ;; round to it; the JVM's own narrowing conversion does not, and it is what the
  ;; browser's `Math.fround` agrees with.
  (is (= Float/MAX_VALUE (numbers/f32 3.4028235E38)))
  (is (= Float/POSITIVE_INFINITY (numbers/f32 1.0E300)))
  (is (= Float/NEGATIVE_INFINITY (numbers/f32 -1.0E300)))
  (is (= (float 0.1) (numbers/f32 0.1)))
  (is (numbers/nan? (numbers/f32 Double/NaN))))

(deftest a-value-survives-its-bits
  ;; The bit pattern is how the two worlds name the same double, so the two
  ;; directions have to be each other's inverse.
  (doseq [value [0.0 -0.0 1.0 -1.0 0.1 1439526.25 Double/MAX_VALUE
                 Double/MIN_VALUE (Double/longBitsToDouble 2)]]
    (is (= (Double/doubleToRawLongBits (double value))
           (Double/doubleToRawLongBits (numbers/f64-of-bits (numbers/f64-bits value))))))
  (doseq [value [(float 0.0) (float -0.0) (float 0.1) (float -0.16666667)
                 Float/MAX_VALUE Float/MIN_VALUE]]
    (is (= (Float/floatToRawIntBits value)
           (Float/floatToRawIntBits (numbers/f32-of-bits (numbers/f32-bits value))))))
  (is (= -0.0 (numbers/f64-of-bits (numbers/f64-bits -0.0)))))

(deftest a-byte-is-two-hexadecimal-digits
  (is (= "\\00" (numbers/hex-byte 0)))
  (is (= "\\0a" (numbers/hex-byte 10)))
  (is (= "\\ff" (numbers/hex-byte 255))))
