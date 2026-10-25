(ns omkamra.dsp.tests.source-reader
  "The source-text recovery of a literal's spelling (D91).

  The walk is built on the real reader's `read+string`, so this corpus is checked
  against Clojure's own reader: for a form, boxing from the text must equal what
  the JVM reader types. That is the invariant the browser depends on, and the JVM
  can prove it without a browser."
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp.literals :as literals]
            [omkamra.dsp.source-reader :as source-reader]))

(def ^:private reader-opts
  {:read-cond :allow :features #{:clj}})

(def ^:private corpus
  ["(* x 2.0)"
   "(+ sample 0.5)"
   "{:state [{:name n :type :int :init 0 :next (+ n 1)}]}"
   "(let [x 1.0] (+ x 2))"
   "(clamp cutoff 20.0 8000.0)"
   "(g -1.0e3 0.5 0x1E 1N)"
   "[1 2.0 [3.0 4]]"
   "#{1.0 2}"
   "{:a 1.0 :b 2}"
   "(f (g 1.0) [2.0] {:k 3.0})"
   "(if (> x 2.0) #_(+ x 1.0) (* x 3.0))"
   "#?(:clj (* x 2.0) :cljs (* x 3.0))"])

(defn- host-boxed
  "The host reader's own typing of `s`, boxed as the capture would box it."
  [s]
  (literals/box-float-literals (read-string reader-opts s)))

(deftest text-recovery-matches-the-host-reader
  (doseq [s corpus]
    (testing s
      (is (= (host-boxed s) (source-reader/read-boxed s))))))

(deftest annotation-preserves-the-form-and-carries-its-text
  (doseq [s corpus]
    (let [annotated (first (source-reader/annotate s))]
      (testing s
        (is (= (read-string reader-opts s) annotated)
            "annotating must not change what the form means")
        (is (= (host-boxed s) (source-reader/box-form annotated))
            "boxing from the annotated text matches the host reader")))))

(deftest a-bare-number-is-boxed-from-the-form-that-encloses-it
  ;; A browser reads `2.0` as the integer `2`; the value cannot say otherwise, so
  ;; the text of the position it came from must. `box-form` with an explicit text
  ;; is the crux; `box-body`/`box-entry` are how the capture finds that text.
  (testing "the text decides, not the value"
    (is (= {:dsp/float 2.0} (source-reader/box-form 2 "2.0")))
    (is (= 2 (source-reader/box-form 2 "2"))))

  (testing "a body form that is a bare number"
    (let [[definition] (source-reader/annotate "(dsp/defn g [x] 2.0)")]
      (is (= [{:dsp/float 2.0}] (source-reader/box-body definition [2])))))

  (testing "the packed explicit-spec body, inside the parameter vector"
    (let [[definition] (source-reader/annotate "(dsp/defn g [{:params [x]} 2.0])")]
      (is (= [{:dsp/float 2.0}] (source-reader/box-body (nth definition 2) [2])))))

  (testing "several body forms, each with its own text"
    (let [[definition] (source-reader/annotate "(dsp/defn g [x] 2.0 (+ x 1.0))")]
      (is (= [{:dsp/float 2.0} '(+ x {:dsp/float 1.0})]
             (source-reader/box-body definition [2 '(+ x 1)])))))

  (testing "a state transition that is a bare number"
    (let [[state] (source-reader/annotate "{:name n :init 0 :next 2.0}")]
      (is (= {:name 'n :init 0 :next {:dsp/float 2.0}}
             (source-reader/box-entry state :next)))))

  (testing "a state transition that is an expression"
    (let [[state] (source-reader/annotate "{:name n :next (+ n 1.0)}")]
      (is (= {:name 'n :next '(+ n {:dsp/float 1.0})}
             (source-reader/box-entry state :next)))))

  (testing "a state without a transition is left alone"
    (let [[state] (source-reader/annotate "{:name n :init 0}")]
      (is (= {:name 'n :init 0} (source-reader/box-entry state :next))))))
