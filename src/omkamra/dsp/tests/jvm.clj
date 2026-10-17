(ns omkamra.dsp.tests.jvm
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [allocated-bytes gain one-pole
                                                  raw-process-noop stereo-cross
                                                  thread-allocation-meter]]))

(deftest jvm-checked-and-unchecked-primitive-process-adapters
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            artifact (dsp/compile gain {:target :jvm
                                        :entry :process
                                        :precision precision})
            input (if f64?
                    (double-array [1.0 -2.0])
                    (float-array [1.0 -2.0]))
            output (if f64?
                     (double-array 2)
                     (float-array 2))
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (if f64?
          (aset-double ^doubles float-controls 0 2.0)
          (aset-float ^floats float-controls 0 (float 2.0)))
        (is (if f64?
              (instance? omkamra.dsp.targets.jvm.DspProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.targets.jvm.DspProcess (:unchecked artifact))))
        (is (nil? (dsp/process-checked! artifact input output 2 48000.0
                                        float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (java.util.Arrays/fill output (if f64? 0.0 (float 0.0)))
        (is (nil? (dsp/process-unchecked! artifact input output 2 48000.0
                                          float-controls int-controls)))
        (is (= [2.0 -4.0] (vec output)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"float controls have the wrong size"
             (dsp/process-checked! artifact input output 2 48000.0
                                   (if f64? (double-array 0) (float-array 0))
                                   int-controls)))))))

(deftest jvm-unchecked-process-adapter-supports-multichannel-arrays
  (doseq [precision [:f32 :f64]]
    (testing (name precision)
      (let [f64? (= :f64 precision)
            array-class (if f64? (Class/forName "[D") (Class/forName "[F"))
            artifact (dsp/compile stereo-cross {:target :jvm
                                                :entry :process
                                                :precision precision})
            input (into-array array-class
                              [(if f64? (double-array [1.0 2.0])
                                   (float-array [1.0 2.0]))
                               (if f64? (double-array [3.0 4.0])
                                   (float-array [3.0 4.0]))])
            output (into-array array-class
                               [(if f64? (double-array 2) (float-array 2))
                                (if f64? (double-array 2) (float-array 2))])
            float-controls (get-in artifact [:controls :float])
            int-controls (get-in artifact [:controls :int])]
        (is (if f64?
              (instance? omkamra.dsp.targets.jvm.DspMultiProcessD64 (:unchecked artifact))
              (instance? omkamra.dsp.targets.jvm.DspMultiProcess (:unchecked artifact))))
        (dsp/process-checked! artifact input output 2 48000.0
                              float-controls int-controls)
        (is (= [[4.0 6.0] [-2.0 -2.0]] (mapv vec output)))
        (dsp/process-unchecked! artifact input output 2 48000.0
                                float-controls int-controls)
        (is (= [[4.0 6.0] [-2.0 -2.0]] (mapv vec output)))))))

(deftest jvm-unchecked-process-adapter-does-not-allocate-after-warmup
  (if-let [meter (thread-allocation-meter)]
    (let [artifact (dsp/compile gain {:target :jvm :entry :process})
          input (float-array (repeat 64 1.0))
          output (float-array 64)
          float-controls (get-in artifact [:controls :float])
          int-controls (get-in artifact [:controls :int])
          calls 10000]
      (aset-float ^floats float-controls 0 (float 2.0))
      (dotimes [_ calls]
        (raw-process-noop artifact input output 64 48000.0
                          float-controls int-controls)
        (dsp/process-unchecked! artifact input output 64 48000.0
                                float-controls int-controls))
      (let [baseline (allocated-bytes
                      meter
                      #(dotimes [_ calls]
                         (raw-process-noop artifact input output 64 48000.0
                                           float-controls int-controls)))
            processed (allocated-bytes
                       meter
                       #(dotimes [_ calls]
                          (dsp/process-unchecked! artifact input output 64 48000.0
                                                  float-controls int-controls)))
            ;; Comparing the two totals for exact equality passes with no
            ;; headroom at all: the measured no-op loop already reports a few
            ;; bytes of its own, so a single stray byte anywhere in the measured
            ;; region fails the check. Allow a few bytes per call instead.
            ;;
            ;; That is still a real bound rather than a fudge, because every
            ;; possible per-call allocation is far larger: a boxed scalar or a
            ;; small object is tens of bytes, and the regression this guards
            ;; against — an unhinted primitive-array access — was ~24 KB per
            ;; call (D26). This bound is ~6000x below it.
            allowance (* 4 calls)]
        (is (<= processed (+ baseline allowance))
            (str "unchecked JVM process allocated " processed
                 " bytes against a fixed-arity baseline of " baseline
                 " over " calls " calls (allowed " allowance " bytes)"))))
    (is true "thread allocation counters are unavailable on this JVM")))

(deftest javascript-and-jvm-f64-backends
  (let [javascript (dsp/compile one-pole {:target :js
                                          :entry :process
                                          :precision :f64})
        jvm (dsp/compile one-pole {:target :jvm
                                   :entry :process
                                   :precision :f64})
        scalar {:dsp/kind :function
                :name 'f64-add
                :params [{:name 'x :type :float}
                         {:name 'y :type :float}]
                :return-type :float
                :body ['(+ x y)]}
        scalar-js (dsp/compile scalar {:target :js :precision :f64})
        scalar-jvm (dsp/compile scalar {:target :jvm :precision :f64})
        input (double-array [1.0 0.0 -1.0])
        output (double-array 3)]
    (is (str/includes? (:source javascript) "Float64Array"))
    (is (not (str/includes? (:source javascript) "Math.fround")))
    (is (instance? (Class/forName "[D") (get-in jvm [:state :float])))
    (is (= :f64 (get-in jvm [:options :precision])))
    (is (not (str/includes? (:source scalar-js) "Math.fround")))
    (is (< (Math/abs (- 0.3 (dsp/invoke scalar-jvm 0.1 0.2)))
           1.0e-15))
    (dsp/process! jvm {:input input
                       :output output
                       :frames 3
                       :sample-rate 48000.0
                       :controls [0.5]})
    (is (= [1.0 0.5 -0.75] (vec output)))))

(deftest jvm-process-supports-typed-controls-and-sample-rate
  (let [definition {:dsp/kind :function
                    :name 'typed-controls
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'previous :type :float :init 0.0
                             :next 'sample}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :body ['(if enabled
                              (+ sample
                                 (+ (int->float step)
                                    (/ sample-rate 1000.0)))
                              previous)]}
        artifact (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 2.0])
        output (float-array 2)]
    (dsp/process! artifact {:input input :output output :frames 2
                            :sample-rate 2000.0
                            :controls [true 3]})
    (is (= [6.0 7.0] (vec output)))))
