(ns rb.explores.clojure.core
  (:require [clojure.string :as str]))

;; Clojure version
(assert (= #{:major :minor :incremental :qualifier}
           (apply hash-set (keys *clojure-version*))))

;; command line arguments
(assert (seq? *command-line-args*))

;; stdin / stdout / stderr
(assert (instance? java.io.Reader *in*))
(assert (instance? java.io.Writer *out*))
(assert (instance? java.io.Writer *err*))

;; path of the file being evaluated
(assert (string? *file*))

;; current namespace
(assert (instance? clojure.lang.Namespace *ns*))

;; equality

(assert (= 2 (+ 1 1)) "One plus one shall be two")
(assert (not= 3 (+ 1 1)) "One plus one shall not be three")

(assert (= nil nil))
(assert (= [1 2 3] '(1 2 3)))
(assert (not= [1 2 3] #{1 2 3}))
(assert (= (conj [1 2 3] 4) (cons 1 '(2 3 4))))
(assert (= (assoc {:a 3 :b 4} :c 5)
       (assoc {:a 3 :c 5} :b 4)))

(assert (identical? nil nil))
(assert (not (identical? [1 2 3] [1 2 3])))
(let [v [1 2 3] a v b v]
  (assert (identical? a b)))

(assert (= 5 10/2))
(assert (= 30/9 10/3))

(assert (not= 5 5.0))
(assert (== 5 5.0))

;; arithmetic

(assert (= 5 (+ 3 2)))
(assert (= 10 (+ 3 2 5)))

(assert (= -5 (- 5)))

(assert (= 1 (- 3 2)))
(assert (= -4 (- 3 2 5)))

(assert (= 6 (* 3 2)))
(assert (= 30 (* 3 2 5)))

(assert (= 1/4 (/ 4)))
(assert (= 3 (/ 6 2)))
(assert (= 3/4 (/ 6 2 4)))
(assert (not= 0.75 (/ 6 2 4)))

(assert (= 0.75 (/ 6.0 2 4)))
(assert (= 0.75 (/ 6 2.0 4)))
(assert (= 0.75 (/ 6 2 4.0)))

;; ordering comparisons

(assert (< 3 4))
(assert (< 3 4 5))
(assert (<= 3 3 4))
(assert (>= 3 3 2))
(assert (> 3 2))

;; threading macros

(assert (= 2 (-> [1 [2 3 4] {:a 3 :b 5 :c 9}]
             second
             first)))

(assert (= 5 (-> [1 [2 3 4] {:a 3 :b 5 :c 9}]
             (nth 2)
             :b)))

(assert (= "Apple, Orange, Blueberry"
       (->> ["apple" "orange" "blueberry"]
            (map str/capitalize)
            (str/join ", "))))

;; method and field chaining

(assert (= "4d2" (.. Integer (toHexString 1234))))
(assert (pos-int? (.. System (getProperties) (get "os.name") length)))

;; taps

(let [messages (atom [])
      tap-fn (fn [x] (swap! messages conj x))]
  (add-tap tap-fn)
  (try
    (tap> "hello")
    (tap> "world")
    (finally
      (remove-tap tap-fn)))
  ;; we cannot safely assert that @messages hold both strings because
  ;; the tap handlers are invoked asynchronously
  #_(assert (= ["hello" "world"] @messages)))

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
  (assert (= 3 @count))
  (assert (= 10 @sum)))

;; if we read the same symbol twice, we get two different symbols
;; (equal by value but not the same object)

(let [foo '[^int x ^long x]]
  (assert (= (first foo) (second foo)))
  (assert (not (identical? (first foo) (second foo))))
  (assert (symbol? (:tag (meta (first foo)))))
  (assert (= 'int (:tag (meta (first foo)))))
  (assert (symbol? (:tag (meta (second foo)))))
  (assert (= 'long (:tag (meta (second foo))))))

;; type hints result in a tag which is a symbol

(let [foo '[^java.lang.Object x]]
  (assert (symbol? (:tag (meta (first foo)))))
  (assert (= 'java.lang.Object (:tag (meta (first foo))))))

(let [foo '[^Object x]]
  (assert (symbol? (:tag (meta (first foo)))))
  (assert (= 'Object (:tag (meta (first foo))))))

(defn make-object
  ^Object []
  (Object.))

;; function metadata is placed on the var, not on the fn
(assert (nil? (meta make-object)))
(assert (map? (meta #'make-object)))
(assert (every? #{:arglists :line :column :file :name :ns}
                (keys (meta #'make-object))))

;; the ^Object type hint is placed on the first arglist
(let [arglist-meta (meta (first (:arglists (meta #'make-object))))]
  ;; the tag is still a symbol (naming a class) but the class has been
  ;; apparently resolved by some hidden magic
  (assert (= {:tag 'java.lang.Object} arglist-meta) arglist-meta))

;; redefinition of a function preserves the var

(defn test-fn [] 5)
(def test-fn-var-1 #'test-fn)

(defn test-fn [] 6)
(def test-fn-var-2 #'test-fn)

(assert (= test-fn-var-1 test-fn-var-2))

;; a map with varied metadata is the same map
(let [m {:a 5 :b 8}]
  (assert (= m (vary-meta m assoc :foo 1))))

;; a function with varied metadata is not the same function (yikes)
(let [f (fn [x] x)]
  (assert (not= f (vary-meta f assoc :foo 1))))

(defmacro test-hidden-macro-params
  [within-let? & args]
  (assert (= &form (list* 'test-hidden-macro-params within-let? args)))
  (if within-let?
    (assert (and (map? &env)
                 (every? #(instance? clojure.lang.Compiler$LocalBinding %) (vals &env))))
    (assert (nil? &env))))

(test-hidden-macro-params false :foo)

(let [x 5]
  (test-hidden-macro-params true :foo))
