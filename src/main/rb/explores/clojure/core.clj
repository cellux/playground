(ns rb.explores.clojure.core
  (:require [clojure.test :refer [is]])
  (:require [clojure.string :as str]))

;; Clojure version
(is (= #{:major :minor :incremental :qualifier}
       (apply hash-set (keys *clojure-version*))))

;; command line arguments
(is (seq? *command-line-args*))

;; stdin / stdout / stderr
(is (instance? java.io.Reader *in*))
(is (instance? java.io.Writer *out*))
(is (instance? java.io.Writer *err*))

;; path of the file being evaluated
(is (string? *file*))

;; current namespace
(is (instance? clojure.lang.Namespace *ns*))

;; equality

(is (= 2 (+ 1 1)) "One plus one shall be two")
(is (not= 3 (+ 1 1)) "One plus one shall not be three")

(is (= nil nil))
(is (= [1 2 3] '(1 2 3)))
(is (not= [1 2 3] #{1 2 3}))
(is (= (conj [1 2 3] 4) (cons 1 '(2 3 4))))
(is (= (assoc {:a 3 :b 4} :c 5)
       (assoc {:a 3 :c 5} :b 4)))

(is (identical? nil nil))
(is (not (identical? [1 2 3] [1 2 3])))
(let [v [1 2 3] a v b v]
  (is (identical? a b)))

(is (= 5 10/2))
(is (= 30/9 10/3))

(is (not= 5 5.0))
(is (== 5 5.0))

;; arithmetic

(is (= 5 (+ 3 2)))
(is (= 10 (+ 3 2 5)))

(is (= -5 (- 5)))

(is (= 1 (- 3 2)))
(is (= -4 (- 3 2 5)))

(is (= 6 (* 3 2)))
(is (= 30 (* 3 2 5)))

(is (= 1/4 (/ 4)))
(is (= 3 (/ 6 2)))
(is (= 3/4 (/ 6 2 4)))
(is (not= 0.75 (/ 6 2 4)))

(is (= 0.75 (/ 6.0 2 4)))
(is (= 0.75 (/ 6 2.0 4)))
(is (= 0.75 (/ 6 2 4.0)))

;; ordering comparisons

(is (< 3 4))
(is (< 3 4 5))
(is (<= 3 3 4))
(is (>= 3 3 2))
(is (> 3 2))

;; threading macros

(is (= 2 (-> [1 [2 3 4] {:a 3 :b 5 :c 9}]
             second
             first)))

(is (= 5 (-> [1 [2 3 4] {:a 3 :b 5 :c 9}]
             (nth 2)
             :b)))

(is (= "Apple, Orange, Blueberry"
       (->> ["apple" "orange" "blueberry"]
            (map str/capitalize)
            (str/join ", "))))

;; method and field chaining

(is (= "4d2" (.. Integer (toHexString 1234))))
(is (pos-int? (.. System (getProperties) (get "os.name") length)))

;; taps

(let [messages (atom [])
      tap-fn (fn [x] (swap! messages conj x))]
  (add-tap tap-fn)
  (try
    (tap> "hello")
    (tap> "world")
    (finally
      (remove-tap tap-fn)))
  (is (= ["hello" "world"] @messages)))

;; atoms

(let [x (atom 0)
      count (ref 0)
      sum (ref 0)
      watch-fn (fn w [watch-id watched-ref old-val new-val]
                 (assert (identical? watch-id w))
                 (assert (identical? watched-ref x))
                 (dosync
                  (alter count inc)
                  (alter sum + new-val)))]
  ;; (add-watch reference key fn)
  (add-watch x watch-fn watch-fn)
  (try
    (reset! x 3)
    (reset! x 2)
    (swap! x * 5/2)
    (finally
      (remove-watch x watch-fn)))
  (is (= 3 @count))
  (is (= 10 @sum)))
