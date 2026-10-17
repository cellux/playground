(ns omkamra.dsp.tests.composition
  (:require [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.library.delay :as delay]
            [omkamra.dsp.library.filter :as filter]))

(deftest record-owning-modules-compose
  ;; A module that owns a record/buffer composes like any other: each call site
  ;; gets its own line, so two lines with different delays stay independent.
  (let [interpreter (dsp/compile #'delay/delay-line-pair-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'delay/delay-line-pair-voice
                         {:target :jvm :entry :process})
        frames 8
        input (float-array (assoc (vec (repeat frames 0.0)) 0 1.0))
        output (float-array frames) jvm-output (float-array frames)]
    ;; Two independent records: 64-slot buffer plus cursor each.
    (is (= 2 (count (:records (:ir interpreter)))))
    (is (= 130 (get-in interpreter [:abi :arena :regions :state :count])))
    (apply dsp/invoke interpreter input output frames [2.0])
    (apply dsp/invoke jvm input jvm-output frames [2.0])
    (is (= (vec output) (vec jvm-output)))
    ;; Delay 2 puts the impulse at frame 2, delay 4 at frame 4, half scale each.
    (is (= [2 4] (vec (keep-indexed (fn [index x]
                                      (when (not= 0.0 x) index))
                                    (vec output)))))
    (is (= [0.5 0.5] [(nth output 2) (nth output 4)]))))

(deftest stateful-module-composition
  ;; A stateful module called twice: each call site owns its own partition, so
  ;; the two one-poles inside `one-pole-voice` are independent (D19).
  (let [interpreter (dsp/compile #'filter/one-pole-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'filter/one-pole-voice
                         {:target :jvm :entry :process})
        input (float-array [1.0 0.0 0.0 0.0 0.0]) output (float-array 5)
        jvm-output (float-array 5)]
    (is (= [{:name 'previous :type :float :init 0.0 :index 0}
            {:name 'previous :type :float :init 0.0 :index 1}]
           (:state (:ir interpreter))))
    (is (= 2 (get-in interpreter [:abi :arena :regions :state :count])))
    (apply dsp/invoke interpreter input output 5 [0.5])
    (apply dsp/invoke jvm input jvm-output 5 [0.5])
    (is (= (vec output) (vec jvm-output)))
    ;; Impulse response of 0.5 * (one-pole(0.5) + one-pole(0.25)). Sharing state
    ;; between the two call sites would give 0.3125 at the second sample.
    (is (= [1.0 0.375 0.15625 0.0703125 0.033203125]
           (map double (vec output)))))
  ;; Nested ownership: a state-free module that owns two one-poles, called twice
  ;; from the process, gives four independent partitions.
  (let [interpreter (dsp/compile #'filter/one-pole-pair-voice
                                 {:target :interpreter :entry :process})
        jvm (dsp/compile #'filter/one-pole-pair-voice
                         {:target :jvm :entry :process})
        input (float-array [1.0 0.0 0.0 0.0]) output (float-array 4)
        jvm-output (float-array 4)]
    (is (= 4 (count (:state (:ir interpreter)))))
    (apply dsp/invoke interpreter input output 4 [0.5])
    (apply dsp/invoke jvm input jvm-output 4 [0.5])
    (is (= (vec output) (vec jvm-output)))
    (is (every? pos? (rest (vec output))))))

(deftest filter-voice-composition
  ;; The P6 done-when: a stateful biquad and a stateful exponential ADSR composed
  ;; into one voice module, identical on the interpreter and the JVM.
  (let [render (fn [target]
                 (let [artifact (dsp/compile #'filter/filter-voice
                                             {:target target :entry :process})
                       frames 24
                       gate (float-array (concat (repeat 4 0.0) (repeat 20 1.0)))
                       output (float-array frames)]
                   (dsp/process! artifact
                                 {:input gate :output output :frames frames
                                  :sample-rate 44100.0
                                  :controls [2000.0 0.7 0.0002 0.0001 0.4 0.0002]})
                   (vec output)))
        interpreter (render :interpreter)
        jvm (render :jvm)
        artifact (dsp/compile #'filter/filter-voice
                              {:target :interpreter :entry :process})]
    ;; Biquad history (4 fields, one record) plus the ADSR's level and stage.
    (is (= 6 (count (:state (:ir artifact)))))
    (is (= 1 (count (:records (:ir artifact)))))
    (is (= interpreter jvm))
    (is (every? #(Float/isFinite %) jvm))
    (is (every? #(<= 0.0 % 1.0) jvm))
    ;; The envelope opens the filter smoothly and monotonically.
    (is (apply <= (subvec jvm 4)))))

(deftest multi-output-module-composition
  ;; A module returning a named record: one local per output, projected by field.
  (let [render (fn [target]
                 (let [artifact (dsp/compile #'filter/svf-voice
                                             {:target target :entry :process})
                       frames 16
                       input (float-array (assoc (vec (repeat frames 0.0)) 0 1.0))
                       output (float-array frames)]
                   (dsp/process! artifact
                                 {:input input :output output :frames frames
                                  :sample-rate 44100.0 :controls [500.0 0.7]})
                   (vec output)))
        interpreter (render :interpreter)
        jvm (render :jvm)
        artifact (dsp/compile #'filter/svf-voice
                              {:target :interpreter :entry :process})]
    ;; The callee owns the filter record; the three outputs are erased to locals.
    (is (= 2 (count (:state (:ir artifact)))))
    (is (= 1 (count (:records (:ir artifact)))))
    (is (= interpreter jvm))
    (is (every? #(Float/isFinite %) jvm))
    (is (some #(not= 0.0 %) jvm))))

(deftest if-branches-are-separate-scopes
  ;; A local declared in one `if` branch is not visible in the other, so the same
  ;; name may be reused there: it simply gets a fresh IR name. Genuine shadowing
  ;; of an in-scope binding is still rejected.
  (let [definition {:dsp/kind :function
                    :name 'branch-scopes
                    :params [{:name 'sample :type :float}]
                    :return-type :float
                    :process {:controls []}
                    :body ['(if (> sample 0.0)
                              (let [v (* sample 2.0)] v)
                              (let [v (- 0.0 sample)] v))]}
        artifact (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        input (float-array [1.0 -2.0 3.0]) output (float-array 3) jvm-output (float-array 3)]
    (is (= '[v v__1] (mapv :name (take 2 (:locals (:ir artifact))))))
    (apply dsp/invoke artifact input output 3 [])
    (apply dsp/invoke jvm input jvm-output 3 [])
    (is (= [2.0 2.0 6.0] (vec output)))
    (is (= (vec output) (vec jvm-output)))
    ;; Branches with *different* types must not share a slot either.
    (let [mixed (assoc-in definition [:body 0]
                          '(if (> sample 0.0)
                             (let [v (float->int sample)] (int->float v))
                             (let [v (* sample 2.0)] v)))
          mixed-interpreter (dsp/compile mixed {:target :interpreter :entry :process})
          mixed-jvm (dsp/compile mixed {:target :jvm :entry :process})]
      (apply dsp/invoke mixed-interpreter input output 3 [])
      (apply dsp/invoke mixed-jvm input jvm-output 3 [])
      (is (= [1.0 -4.0 3.0] (vec output)))
      (is (= (vec output) (vec jvm-output))))
    ;; Real shadowing is still a diagnostic.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"cannot shadow an in-scope binding"
         (dsp/compile (assoc-in definition [:body 0] '(let [sample 1.0] sample))
                      {:entry :process})))))
