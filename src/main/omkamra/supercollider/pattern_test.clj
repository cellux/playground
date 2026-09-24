(ns omkamra.supercollider.pattern-test
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.supercollider.pattern :as pattern]))

(defn- take-values
  [stream n]
  (loop [stream stream
         values []]
    (if (= n (count values))
      values
      (if-let [result (pattern/step stream {})]
        (recur (:stream result) (conj values (:value result)))
        values))))

(deftest pseq-yields-items-in-order
  (let [p (pattern/pseq [1 2 3])
        s (pattern/stream p)]
    (is (= [1 2 3] (take-values s 10)))
    (is (pattern/pattern? p))
    (is (pattern/stream? s))))

(deftest pseq-repeats-a-finite-number-of-times
  (is (= [1 2 3 1 2 3]
         (take-values (pattern/stream
                       (pattern/pseq [1 2 3] {:repeats 2}))
                      10)))
  (is (= []
         (take-values (pattern/stream
                       (pattern/pseq [1 2 3] {:repeats 0}))
                      10))))

(deftest pseq-supports-infinite-repetition
  (is (= [2 3 1 2 3]
         (take-values (pattern/stream
                       (pattern/pseq [1 2 3]
                                     {:repeats :inf :offset 1}))
                      5))))

(deftest pseq-embeds-nested-patterns
  (let [p (pattern/pseq [0 (pattern/pseq [1 2]) 3])]
    (is (= [0 1 2 3]
           (take-values (pattern/stream p) 10)))))

(deftest streams-from-one-pattern-are-independent
  (let [p (pattern/pseq [:a :b] {:repeats :inf})
        first-stream (pattern/stream p)
        second-stream (pattern/stream p)
        first-step (pattern/step first-stream {})
        second-step (pattern/step second-stream {})]
    (is (= :a (:value first-step)))
    (is (= :a (:value second-step)))
    (is (= :b (:value (pattern/step (:stream first-step) {}))))
    (is (= :a (:value (pattern/step second-stream {}))))))

(deftest constant-patterns-yield-forever
  (is (= [42 42 42]
         (take-values (pattern/stream (pattern/constant 42)) 3)))
  (is (= [42 42]
         (take-values (pattern/stream 42) 2))))

(deftest pseq-validates-options
  (is (thrown? IllegalArgumentException (pattern/pseq [])))
  (is (thrown? IllegalArgumentException
               (pattern/pseq [1 2] {:repeats -1})))
  (is (thrown? IllegalArgumentException
               (pattern/pseq [1 2] {:repeats 1.5})))
  (is (thrown? IllegalArgumentException
               (pattern/pseq [1 2] {:offset 1.5}))))
