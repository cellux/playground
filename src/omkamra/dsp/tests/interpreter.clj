(ns omkamra.dsp.tests.interpreter
  (:require [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [one-pole]]))

(deftest interpreter-f64-is-the-precision-reference
  (let [artifact (dsp/compile one-pole {:target :interpreter
                                        :entry :process
                                        :precision :f64})
        input (double-array [1.0 0.0 -1.0])
        output (double-array 3)]
    (is (= :f64 (get-in artifact [:ir :precision])))
    (is (instance? (Class/forName "[D") (:state artifact)))
    (dsp/process! artifact {:input input
                            :output output
                            :frames 3
                            :controls [0.5]})
    (is (= [1.0 0.5 -0.75] (vec output)))
    (dsp/reset! artifact)
    (is (= 0.0 (aget ^doubles (:state artifact) 0)))))

(deftest interpreter-and-jvm-differential
  (let [definition {:dsp/kind :function
                    :name 'differential-process
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'count :type :int :init 0
                             :next '(+ count step)}
                            {:name 'previous :type :float :init 0.0
                             :next 'sample}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :body ['(if enabled
                              (+ sample (int->float count))
                              previous)]}
        interpreter (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        blocks [[1.0 2.0] [3.0] [4.0 5.0 6.0]]]
    (doseq [[block-index samples] (map-indexed vector blocks)]
      (let [input (float-array samples)
            interpreter-output (float-array (count samples))
            jvm-output (float-array (count samples))
            controls [(even? block-index) 2]]
        (dsp/process! interpreter {:input input :output interpreter-output
                                   :frames (count samples) :controls controls})
        (dsp/process! jvm {:input input :output jvm-output
                           :frames (count samples) :controls controls})
        (is (= (vec interpreter-output) (vec jvm-output)))))))
