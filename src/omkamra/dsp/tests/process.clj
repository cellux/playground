(ns omkamra.dsp.tests.process
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.library.delay :as delay]
            [omkamra.dsp.library.noise :as noise]
            [omkamra.dsp.library.oscillator :as oscillator]
            [omkamra.dsp.harness.fixtures :refer [gain gated-one-pole one-pole ramp-generator]]))

(deftest process-abi-runs-single-channel-blocks
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile gain {:target target :entry :process})
          input (float-array [1.0 -2.0 0.5])
          output (float-array [99.0 99.0 99.0 99.0])]
      (is (= :process (:entry artifact)))
      (is (= [{:name 'amount :type :float}]
             (:controls (:abi artifact))))
      (is (nil? (dsp/invoke artifact input output 2 2.0)))
      (is (= [2.0 -4.0 99.0 99.0] (vec output))))))

(deftest process-zero-frames-leaves-output-untouched
  (let [artifact (dsp/compile gain {:entry :process})
        input (float-array [1.0])
        output (float-array [7.0])]
    (is (nil? (dsp/invoke artifact input output 0 2.0)))
    (is (= [7.0] (vec output)))))

(deftest conditional-state-updates-are-evaluated-per-frame
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile gated-one-pole {:target target :entry :process})
          input (float-array [1.0 -1.0 0.0 1.0])
          output (float-array 4)]
      (dsp/invoke artifact input output 4 0.5)
      (is (= [1.0 0.0 0.0 1.5] (vec output))))))

(deftest stateful-process-preserves-state-between-blocks-and-resets
  (doseq [target [:interpreter :jvm]]
    (let [artifact (dsp/compile one-pole {:target target :entry :process})
          input (float-array [1.0 0.0 0.0])
          output (float-array 3)]
      (is (= [{:name 'previous :type :float :init 0.0}]
             (:state (:abi artifact))))
      (dsp/invoke artifact input output 3 0.5)
      (is (= [1.0 0.5 0.25] (vec output)))
      (dsp/invoke artifact (float-array [0.0]) output 1 0.5)
      (is (= 0.125 (aget output 0)))
      (dsp/reset! artifact)
      (dsp/invoke artifact (float-array [0.0]) output 1 0.5)
      (is (= 0.0 (aget output 0))))))

(deftest process-backends-match-reference-blocks
  (let [block-lengths [0 1 127 128 256]]
    (doseq [target [:interpreter :jvm]]
      (let [artifact (dsp/compile one-pole {:target target :entry :process})]
        (dsp/reset! artifact)
        (loop [block 0
               previous 0.0]
          (when (< block (count block-lengths))
            (let [frames (nth block-lengths block)
                  input (float-array
                         (map #(float (* 0.25
                                         (Math/sin (+ % (* 17 block)))))
                              (range frames)))
                  output (float-array (if (zero? frames)
                                        [77.0]
                                        (repeat frames 0.0)))
                  expected (float-array frames)
                  next-previous
                  (loop [frame 0
                         previous previous]
                    (if (= frame frames)
                      previous
                      (let [next (float (+ (double (aget input frame))
                                           (* 0.5 previous)))]
                        (aset-float expected frame next)
                        (recur (inc frame) next))))]
              (dsp/invoke artifact input output frames 0.5)
              (if (zero? frames)
                (is (= 77.0 (aget output 0)))
                (is (= (vec expected) (vec output))))
              (recur (inc block) next-previous))))))))

(deftest process-abi-describes-instance-bindings-and-physical-memory
  (let [f32 (dsp/compile one-pole {:target :interpreter
                                   :entry :process
                                   :precision :f32})
        f64 (dsp/compile one-pole {:target :interpreter
                                   :entry :process
                                   :precision :f64})
        multi (dsp/compile gain {:target :interpreter
                                 :entry :process
                                 :channels 2})
        f32-artifacts [(dsp/compile one-pole {:target :jvm
                                              :entry :process
                                              :precision :f32})
                       (dsp/compile one-pole {:target :js
                                              :entry :process
                                              :precision :f32})
                       (dsp/compile one-pole {:target :wasm
                                              :entry :process
                                              :precision :f32})]
        f64-artifacts [(dsp/compile one-pole {:target :jvm
                                              :entry :process
                                              :precision :f64})
                       (dsp/compile one-pole {:target :js
                                              :entry :process
                                              :precision :f64})
                       (dsp/compile one-pole {:target :wasm
                                              :entry :process
                                              :precision :f64})]
        expected (fn [precision element-type bytes view]
                   {:precision precision
                    :logical-type :float
                    :element-type element-type
                    :element-bytes bytes
                    :alignment bytes
                    :views {:jvm view
                            :js (if (= :f64 precision) :float64-array :float32-array)
                            :wasm element-type}})]
    (is (= {:count 2 :type :int}
           (:channels (:abi multi))))
    (is (not (contains? (:bindings (:abi multi)) :channel)))
    (doseq [[artifact element-bytes alignment element-type]
            [[f32 4 4 :f32] [f64 8 8 :f64]]]
      (let [abi (:abi artifact)]
        (is (= 1 (:abi-version abi)))
        (is (= :process (:entry abi)))
        (is (= element-type (get-in abi [:memory-layout :element-type])))
        (is (= element-bytes (get-in abi [:memory-layout :element-bytes])))
        (is (= alignment (get-in abi [:memory-layout :alignment])))
        (is (= (expected (:precision abi) element-type element-bytes
                         (if (= :f64 (:precision abi)) :double-array :float-array))
               (:physical-type artifact)))
        (is (= (:physical-type artifact)
               (get-in abi [:memory-layout :physical-type])))
        (doseq [target-artifact (if (= :f64 (:precision abi))
                                  f64-artifacts
                                  f32-artifacts)]
          (is (= (:physical-type artifact)
                 (:physical-type target-artifact)))
          (is (= (:physical-type target-artifact)
                 (get-in (:abi target-artifact) [:physical-type]))))
        (is (= {:role :frame-count :argument-index 2}
               (select-keys (get-in abi [:bindings :frames])
                            [:role :argument-index])))
        (is (= {:role :sample-rate :argument-index 3}
               (select-keys (get-in abi [:bindings :sample-rate])
                            [:role :argument-index])))
        (is (= :instance (get-in abi [:state-layout :ownership])))
        (is (= [:init :reset] (:lifecycle-operations abi)))
        (is (= :persistent (get-in abi [:instance :state])))
        (is (nil? (dsp/initialize! artifact)))))))

(deftest stateful-functions-require-process-entry
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"require the :process entry"
       (dsp/compile one-pole))))

(deftest typed-process-context-and-lifecycle
  (let [definition {:dsp/kind :function
                    :name 'typed-process
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'step :type :int}]
                    :state [{:name 'counter :type :int :init 0
                             :next '(+ counter step)}
                            {:name 'open :type :boolean :init false
                             :next 'enabled}]
                    :process {:input :sample
                              :frames {:name :block-frames}
                              :sample-rate {:name :hz}
                              :controls [{:name :enabled :type :boolean}
                                         {:name :step :type :int}]}
                    :return-type :float
                    :body ['(if open
                              (+ (int->float counter)
                                 (/ hz (int->float block-frames)))
                              sample)]}
        artifact (dsp/compile definition {:entry :process})
        input (float-array [1.0 2.0])
        output (float-array 2)]
    (is (= {:name 'block-frames :type :int} (get-in artifact [:abi :frames])))
    (is (= {:name 'hz :type :float} (get-in artifact [:abi :sample-rate])))
    (is (= [{:name 'enabled :type :boolean}
            {:name 'step :type :int}]
           (:controls (:abi artifact))))
    (is (= [{:name 'counter :type :int :init 0}
            {:name 'open :type :boolean :init false}]
           (:state (:abi artifact))))
    (is (= {:init true :reset true} (:lifecycle (:abi artifact))))
    (is (nil? (dsp/process! artifact {:input input
                                      :output output
                                      :frames 2
                                      :sample-rate 48000.0
                                      :controls [true 1]})))
    (is (= [1.0 24001.0] (vec output)))
    (dsp/reset! artifact)
    (dsp/process! artifact {:input input
                            :output output
                            :frames 1
                            :sample-rate 48000.0
                            :controls [false 3]})
    (is (= 1.0 (aget output 0)))
    (let [jvm-artifact (dsp/compile definition {:target :jvm :entry :process})
          jvm-output (float-array 2)]
      (is (= {:float-count 0 :int-count 2}
             (select-keys (:state-layout jvm-artifact) [:float-count :int-count])))
      (is (some #(= "omkamra.dsp.targets.jvm.DspKernel" (.getName ^Class %))
                (.getInterfaces ^Class (:class jvm-artifact))))
      (is (some? (:kernel jvm-artifact)))
      (is (= {:float "[F" :int "[I"}
             (into {} (map (fn [[storage values]]
                             [storage (.getName (class values))])
                           (:state jvm-artifact)))))
      (dsp/process! jvm-artifact {:input input :output jvm-output :frames 2
                                  :sample-rate 48000.0
                                  :controls [true 1]})
      (is (= [1.0 24001.0] (vec jvm-output)))
      (dsp/reset! jvm-artifact)
      (dsp/process! jvm-artifact {:input input :output jvm-output :frames 1
                                  :sample-rate 48000.0
                                  :controls [false 3]})
      (is (= 1.0 (aget jvm-output 0))))))

(deftest process-instances-are-independent-and-destroyable
  (let [artifact (dsp/compile #'delay/delay-line {:target :jvm :entry :process})
        render (fn [holder input]
                 (let [frames (count input)
                       output (float-array frames)]
                   (dsp/process! holder {:input (float-array input)
                                         :output output
                                         :frames frames
                                         :sample-rate 44100.0
                                         :controls [16.0]})
                   (vec output)))
        impulse (into [1.0] (repeat 31 0.0))
        silence (vec (repeat 16 0.0))
        a (dsp/create-instance artifact)
        b (dsp/create-instance artifact)]
    (is (dsp/process-instance? a))
    (is (not (dsp/process-instance? artifact)))
    ;; Instantiating allocates a private state region rather than sharing the
    ;; artifact's.
    (is (not (identical? (:state a) (:state b))))
    ;; Differential independence: `a` hears an impulse and its line still holds
    ;; it; `b`, fed only silence, stays silent with a zeroed state.
    (is (= 1.0 (nth (render a impulse) 16)))
    (is (every? zero? (render b silence)))
    (is (some (complement zero?) (:float (:state a))))
    (is (every? zero? (:float (:state b))))
    ;; Resetting one instance leaves the other alone.
    (dsp/reset! b)
    (is (some (complement zero?) (:float (:state a))))
    ;; Destroying releases the state and rejects further use.
    (dsp/destroy! a)
    (is (every? zero? (:float (:state a))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"destroyed" (render a [0.0])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"destroyed" (dsp/reset! a)))
    ;; The artifact is not an instance: it cannot be destroyed, and destroying
    ;; an instance never disturbs it.
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"create-instance" (dsp/destroy! artifact)))
    (is (= 1.0 (nth (render artifact impulse) 16))))
  ;; The interpreter exposes the same lifecycle and agrees with the JVM.
  (let [artifact (dsp/compile #'delay/delay-line {:target :interpreter :entry :process})
        impulse (into [1.0] (repeat 31 0.0))
        render (fn [holder]
                 (let [output (float-array 32)]
                   (dsp/process! holder {:input (float-array impulse)
                                         :output output
                                         :frames 32
                                         :sample-rate 44100.0
                                         :controls [16.0]})
                   (vec output)))
        instance (dsp/create-instance artifact)]
    (is (dsp/process-instance? instance))
    (is (= (render artifact) (render instance)))
    (dsp/destroy! instance)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"destroyed" (render instance))))
  ;; Every target names the same create/reset/destroy lifecycle, and no handle
  ;; is shared between instances of one artifact.
  (is (= {:interpreter {:create :create-instance :reset :reset! :destroy :destroy!}
          :jvm {:create :create-instance :reset :reset! :destroy :destroy!}
          :js {:create :createKernel :reset :reset :destroy :destroy}
          :wasm {:create :instantiate :reset :reset :destroy :release}}
         (into {}
               (map (fn [target]
                      [target (get-in (dsp/compile #'delay/delay-line
                                                   {:target target :entry :process})
                                      [:abi :instance :operations])]))
               [:interpreter :jvm :js :wasm])))
  (doseq [target [:interpreter :jvm :js :wasm]]
    (let [handle (get-in (dsp/compile #'delay/delay-line {:target target :entry :process})
                         [:abi :instance :handle])]
      (is (contains? #{:state-region :kernel-handle :wasm-instance} (:kind handle)))
      (is (false? (:shared? handle))))))

(deftest generators-have-no-input-port
  (let [abi (:abi (dsp/compile #'ramp-generator {:target :interpreter :entry :process}))]
    ;; The port and its buffer are gone, and the dead `:buffer-load` with them,
    ;; so a generator no longer claims to read memory.
    (is (= [] (:inputs abi)))
    (is (= [:output] (mapv :id (:buffers abi))))
    (is (not (contains? (:bindings abi) :input)))
    (is (= #{:read-state :write-state :write-memory} (:effects abi)))
    ;; Argument indices are derived, so removing the input shifts every later
    ;; argument down by one and the ABI matches the emitted arity.
    (is (= [0 1 2]
           (mapv #(get-in abi [:bindings % :argument-index])
                 [:output :frames :sample-rate])))
    (is (= 3 (get-in abi [:bindings :controls 0 :argument-index])))
    ;; With no input port, every parameter is a control.
    (is (= '[increment] (mapv :name (:controls abi)))))
  ;; A body cannot mention the sample: the binding does not exist.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"unknown local"
       (dsp/compile {:dsp/kind :function
                     :name 'bad-generator
                     :params ['increment]
                     :return-type :float
                     :process {:input false}
                     :body ['(+ sample increment)]}
                    {:target :interpreter :entry :process})))
  ;; Same samples on the interpreter and the JVM, with no input at all.
  (let [render (fn [target]
                 (let [artifact (dsp/compile #'ramp-generator
                                             {:target target :entry :process})
                       output (float-array 4)]
                   (dsp/process! artifact {:output output :frames 4
                                           :sample-rate 8.0 :controls [0.25]})
                   (vec output)))]
    (is (= [0.0 0.25 0.5 0.75] (render :interpreter)))
    (is (= (render :interpreter) (render :jvm))))
  ;; JS and Wasm genuinely drop the input parameter, so the exported arity
  ;; agrees with the derived argument indices.
  (is (empty? (get-in (dsp/compile #'ramp-generator {:target :js :entry :process})
                      [:abi :inputs])))
  (is (not (str/includes? (:wat (dsp/compile #'ramp-generator
                                             {:target :wasm :entry :process}))
                          "(param $input")))
  ;; The library's oscillators and noise sources are generators now.
  (doseq [v [#'oscillator/sine-oscillator #'oscillator/saw-oscillator
             #'oscillator/pulse-oscillator #'oscillator/square-oscillator
             #'oscillator/triangle-oscillator #'oscillator/lfo
             #'oscillator/sync-oscillator #'noise/white-noise #'noise/pink-noise]]
    (let [abi (:abi (dsp/compile v {:target :interpreter :entry :process}))]
      (is (= [] (:inputs abi)) (str (:name abi)))
      (is (not (contains? (:effects abi) :read-memory)) (str (:name abi))))))
