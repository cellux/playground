(ns omkamra.cgen.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.cgen.core :as c]))

(c/defn twice [x]
  (return (* x 2)))

(c/defn plus-two [x]
  (return (::twice (+ x 1))))

(c/defn sum-to [n]
  (let [total 0
        i 0]
    (while (< i n)
      (assign! total (+ total i))
      (assign! i (+ i 1)))
    (return total)))

(c/defn signum [x]
  (if (> x 0)
    (return 1)
    (if (< x 0)
      (return -1)
      (return 0))))

(c/defn min-value []
  (return -9223372036854775808))

(c/defn ^double identity-double [^double x]
  (return x))

(def anonymous-double
  (c/function-definition
   'anonymous-double
   [(with-meta 'x {:tag 'double})]
   :double
   [(list 'return 'x)]))

(def anonymous-double-fn
  (c/fn ^double [^double x]
    (return (* x 2.0))))

(deftest transpile-emits-a-c17-translation-unit
  (let [source (c/transpile '((def add [x y] (return (+ x y)))))]
    (is (str/includes? source "#include <stdint.h>"))
    (is (str/includes? source "int64_t add(int64_t x, int64_t y);"))
    (is (str/includes? source "return (x + y);"))))

(deftest qualified-quasiquoted-dsl-symbols-are-normalized
  (let [source (c/transpile
                '((def add [x y]
                    (example/return (clojure.core/+ x y)))))]
    (is (str/includes? source "return (x + y);"))))

(deftest callable-definitions-compile-and-run
  (is (= 42 (twice 21)))
  (is (= 42 (plus-two 20)))
  (is (= 45 (sum-to 10)))
  (is (= 1 (signum 7)))
  (is (= 0 (signum 0)))
  (is (= -1 (signum -7)))
  (is (= Long/MIN_VALUE (min-value))))

(deftest source-for-includes-reachable-dependencies-only
  (let [source (c/source-for #'plus-two)]
    (is (str/includes? source "cgen_twice__"))
    (is (str/includes? source "cgen_plus_two__"))
    (is (not (str/includes? source "cgen_sum_to__")))
    (is (str/includes? source "int main(int argc, char **argv)"))))

(deftest binary-invocation-round-trips-raw-records
  (let [input (byte-array 16)
        input-buffer (doto (java.nio.ByteBuffer/wrap input)
                       (.order java.nio.ByteOrder/LITTLE_ENDIAN))]
    (.putDouble input-buffer 1.5)
    (.putDouble input-buffer -2.25)
    (let [{:keys [bytes count return-type]}
          (c/invoke-binary anonymous-double {:input input})
          output-buffer (doto (java.nio.ByteBuffer/wrap ^bytes bytes)
                          (.order java.nio.ByteOrder/LITTLE_ENDIAN))]
      (is (= 2 count))
      (is (= :double return-type))
      (is (= 1.5 (.getDouble output-buffer)))
      (is (= -2.25 (.getDouble output-buffer))))))

(deftest anonymous-definitions-do-not-require-vars
  (is (= {:params [:double] :return-type :double}
         (c/describe anonymous-double)))
  (is (= 3.5
         (c/invoke anonymous-double 3.5)))
  (is (= 7.0
         (c/invoke anonymous-double-fn 3.5))))

(deftest invocation-validates-arguments
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"wrong number"
                        (twice 1 2)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"integer arguments"
                        (twice "1")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"signed 64-bit"
                        (twice (inc (bigint Long/MAX_VALUE))))))
