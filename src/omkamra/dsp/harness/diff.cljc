(ns omkamra.dsp.harness.diff
  "Exact comparison of two compiled results (P13, D79).

  The browser compiles a definition to a descriptor, a linked unit and portable
  IR of its own; the equivalence check asks whether it got what the JVM got.
  *Exactly* is subtler than `=` here, for three reasons that are all the lesson
  the box was introduced for (D78): a value's type must not be read off the value.

  - `(= 0 0.0)` is false in Clojure and true in ClojureScript, so equality itself
    differs by platform. Numbers are compared by *bit pattern* instead —
    `Object.is` in the browser, `doubleToRawLongBits` on the JVM — which also
    makes the comparison fair: an integral float and the integer that looks like
    it compare equal, because on one of the two platforms they are the same
    value, and the type they might disagree about is carried by a `:type` field.
  - `(= NaN NaN)` is false on both platforms, which would report a difference
    where the two results hold the same value. Bits compare equal.
  - `0.0` and `-0.0` are `=` while being different values, which would hide a
    real difference. Bits do not.

  Order is compared where order is the fact — sequentially, position by position
  — and not where it is not: a map's keys are compared as a set and visited in a
  stable order, and a set is compared as a whole. Which difference is *first*
  therefore does not depend on the platform's hashing.")

(defn- same-number?
  [a b]
  #?(:clj (= (Double/doubleToRawLongBits (double a))
             (Double/doubleToRawLongBits (double b)))
     :cljs (js/Object.is a b)))

(defn same-leaf?
  "Whether two non-collection values are the same value."
  [a b]
  (if (and (number? a) (number? b))
    (same-number? a b)
    (= a b)))

(def ^:private absent
  "Stands in for a key one side has and the other does not. The key sets are
  compared first, so this only ever names which side was missing a key."
  :dsp/absent)

(defn- key-difference
  [actual expected path]
  (let [actual-keys (set (keys actual))
        expected-keys (set (keys expected))]
    (when-not (= actual-keys expected-keys)
      {:path path
       :actual (vec (sort (map pr-str actual-keys)))
       :expected (vec (sort (map pr-str expected-keys)))})))

(defn first-difference
  "The first place `actual` and `expected` disagree, or nil when they agree.

  A difference is `{:path [..] :actual a :expected b}`; the path is the sequence
  of keys and indices that reaches it, so a failure says where, not only that."
  ([actual expected] (first-difference actual expected []))
  ([actual expected path]
   (cond
     (and (map? actual) (map? expected))
     (or (key-difference actual expected path)
         (some (fn [k]
                 (first-difference (get actual k absent)
                                   (get expected k absent)
                                   (conj path k)))
               (sort-by pr-str (keys actual))))

     (and (set? actual) (set? expected))
     (when-not (= actual expected)
       {:path path :actual actual :expected expected})

     (and (sequential? actual) (sequential? expected))
     (let [actual-length (count actual)
           expected-length (count expected)
           common (min actual-length expected-length)]
       (or (some (fn [index]
                   (first-difference (nth actual index) (nth expected index)
                                     (conj path index)))
                 (range common))
           (when-not (= actual-length expected-length)
             ;; Name the element one side has and the other does not, and say
             ;; both lengths: a pair of numbers at a path is otherwise read as
             ;; the values that sit there.
             {:path (conj path common)
              :actual (when (> actual-length common) (nth actual common))
              :expected (when (> expected-length common) (nth expected common))
              :lengths [actual-length expected-length]})))

     (and (coll? actual) (coll? expected))
     {:path path :actual actual :expected expected}

     (same-leaf? actual expected) nil
     :else {:path path :actual actual :expected expected})))
