(ns omkamra.dsp.tests.automation
  (:require [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.descriptor :as descriptor]
            [omkamra.dsp.library.control :as control]
            [omkamra.dsp.event :as event]
            [omkamra.dsp.schedule :as schedule]
            #_{:clj-kondo/ignore [:unused-referred-var]}
            [omkamra.dsp.harness.fixtures :refer [abi-controls allocated-bytes first-difference lane-carried-control lane-carrying-caller lane-gain lane-smoothed-gain one-pole one-pole-samples render-automation render-gain render-lane render-manual-split render-on render-single-parameter-event render-timeline shared-gain smoothed-targets thread-allocation-meter two-lane-mix]]))

(deftest event-vocabulary-and-ingress-queue
  (testing "the v1 event set accepts valid payloads"
    (is (= :note-on (:type (event/validate-event {:frame 0 :sequence 0 :type :note-on
                                                  :channel 0 :note 60 :velocity 0.5}))))
    (is (= :transport (:type (event/validate-event {:frame 3 :sequence 4 :type :transport
                                                    :tempo 128.0 :beat 1.5}))))
    (is (= event/event-types
           [:control-change :note-off :note-on :parameter :pitch-bend :pressure
            :sample-rate :transport])))
  (testing "malformed events are rejected on the producer side"
    (doseq [[label message message-event]
            [["unknown type" #"not in the v1" {:frame 0 :sequence 0 :type :nope}]
             ["out of range" #"out of range" {:frame 0 :sequence 0 :type :note-on
                                              :channel 0 :note 60 :velocity 2.0}]
             ["missing key" #"out of range" {:frame 0 :sequence 0 :type :note-on
                                             :channel 0 :note 60}]
             ["negative frame" #"non-negative" {:frame -1 :sequence 0 :type :pressure
                                                :channel 0 :note 60 :value 0.5}]
             ["unknown key" #"outside the v1" {:frame 0 :sequence 0 :type :pressure
                                               :channel 0 :note 60 :value 0.5
                                               :bogus 1}]]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo message
                            (event/validate-event message-event))
          label)))
  (testing "equal frames are ordered by ingress sequence"
    (is (= [{:frame 4 :sequence 9} {:frame 5 :sequence 0} {:frame 5 :sequence 1}]
           (sort event/compare-events [{:frame 5 :sequence 1} {:frame 5 :sequence 0}
                                       {:frame 4 :sequence 9}]))))
  (testing "overflow drops the oldest future event"
    (let [queue (event/event-queue 3)]
      (doseq [frame [10 20 30]]
        (is (= :accepted (event/push-event! queue {:frame frame :sequence frame}
                                            5))))
      (is (= :dropped-oldest (event/push-event! queue {:frame 40 :sequence 40} 5)))
      (is (= [20 30 40] (mapv :frame (map #(event/event-at queue %) (range 3)))))
      (is (= {:accepted 3 :dropped 1 :rejected 0 :dispatched 0 :high-water 3 :depth 3}
             (event/queue-stats queue)))))
  (testing "a due event is never dropped: the incoming one is rejected instead"
    (let [queue (event/event-queue 2)]
      (event/push-event! queue {:frame 10 :sequence 0} 5)
      (event/push-event! queue {:frame 20 :sequence 1} 5)
      (is (= :rejected (event/push-event! queue {:frame 99 :sequence 2} 100)))
      (is (= [10 20] (mapv :frame (map #(event/event-at queue %) (range 2)))))
      (is (= 1 (:rejected (event/queue-stats queue))))
      (is (= 0 (:dropped (event/queue-stats queue))))))
  (testing "the queue reuses its storage and zeroes its counters"
    (let [queue (event/event-queue 2)]
      (event/push-event! queue {:frame 5 :sequence 0} 0)
      (event/push-event! queue {:frame 6 :sequence 1} 0)
      (is (= 2 (event/reset-queue! queue)))
      (is (event/queue-empty? queue))
      (is (= {:accepted 0 :dropped 0 :rejected 0 :dispatched 0 :high-water 0 :depth 0}
             (event/queue-stats queue)))
      (is (= :accepted (event/push-event! queue {:frame 1 :sequence 0} 0))))))

(deftest sample-accurate-events-render-identically-across-partitionings
  (testing "an event changes the signal, so these comparisons are not vacuous"
    (is (not= (render-single-parameter-event 17) (render-single-parameter-event nil))))
  (doseq [[target precision] [[:interpreter :f32] [:interpreter :f64]
                              [:jvm :f32] [:jvm :f64]]
          :let [reference (render-timeline 200 target precision)]]
    (is (not (every? zero? reference))
        (str target " " precision " rendered silence"))
    (testing (str target " " precision " is partition-independent")
      (doseq [block-size [100 64 32 16 7 3 1 5 199]]
        (is (= reference (render-timeline block-size target precision))
            (str target " " precision " differs at block size " block-size))))))

(deftest event-dispatch-is-sample-accurate
  (testing "a dispatch at frame 17 is identical to splitting the block by hand"
    (is (= (render-manual-split 17 0.1) (render-single-parameter-event 17))))
  (testing "moving the event by one frame moves the output by one frame"
    (is (= 17 (first-difference (render-single-parameter-event 17)
                                (render-single-parameter-event 18))))
    (is (= 16 (first-difference (render-single-parameter-event 17)
                                (render-single-parameter-event 16))))
    (is (= 17 (first-difference (render-single-parameter-event 17)
                                (render-single-parameter-event nil))))))

(deftest segmentation-allocates-nothing-per-event-boundary
  (if-let [meter (thread-allocation-meter)]
    (let [queue (event/event-queue 16)
          scheduler (schedule/scheduler queue)
          calls 2000
          ;; Built once: a producer builds an event off the audio thread, so the
          ;; test must not charge its own message construction to the scheduler.
          messages (mapv (fn [frame]
                           {:frame frame :sequence frame :type :parameter
                            :name :coefficient :value 0.5})
                         (range 1 17))
          ;; Refill, then segment the whole block. A handful of boxed longs per
          ;; *block* is expected; what must not happen is allocation that grows
          ;; with the number of event boundaries.
          segment (fn [boundaries]
                    (event/reset-queue! queue)
                    (dotimes [index boundaries]
                      (event/push-event! queue (nth messages index) 0))
                    (schedule/begin-block! scheduler 0 256)
                    (loop [segments 0]
                      (if (schedule/next-segment! scheduler)
                        (recur (inc segments))
                        segments)))]
      (dotimes [_ 200] (segment 1) (segment 16))
      (is (= 2 (segment 1)) "one event must yield two segments")
      (is (= 17 (segment 16)) "16 events must yield 17 segments")
      (let [one (allocated-bytes meter #(dotimes [_ calls] (segment 1)))
            many (allocated-bytes meter #(dotimes [_ calls] (segment 16)))]
        ;; The residue is a couple of boxed indices per call in this reference
        ;; host; the claim is that 15 extra boundaries cost *bytes*, not
        ;; kilobytes, and that nothing scales with the event payloads or the
        ;; segment count in any structural way.
        (is (< (- many one) (* 1024 calls))
            (str "15 extra event boundaries cost " (- many one) " bytes over "
                 calls " blocks; the scheduler itself must not allocate per "
                 "segment"))))
    (is true "thread allocation counters are unavailable on this JVM")))

(deftest declared-control-modes-are-carried-by-the-kernel
  (testing "a `:smooth` control is a one-pole filter, one step per frame"
    (is (= (one-pole-samples 1.0 0.01 8)
           (render-gain #'control/smoothed-gain 1.0 8)))
    ;; A coefficient of 1 would reach the target immediately, so the smoothing
    ;; is genuinely rate-limited rather than a pass-through.
    (is (not= (render-gain #'control/smoothed-gain 1.0 8)
              (vec (repeat 8 1.0)))))
  (testing "a `:slew` control moves a bounded step per frame and lands on target"
    (let [output (render-gain #'control/slewed-gain 1.0 200)
          steps (map - (rest output) output)]
      (is (every? #(<= 0.0 % 0.0100001) steps))
      (is (apply <= output))
      (is (= 1.0 (last output)))))
  (testing "the ABI describes the mode, so a host knows setting it is a target"
    (is (= [{:name 'amount :type :float :smooth 0.01}]
           (abi-controls #'control/smoothed-gain)))
    (is (= [{:name 'amount :type :float :slew 0.01}]
           (abi-controls #'control/slewed-gain))))
  (testing "invalid declared modes are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must have type :float"
         (descriptor/normalize {:dsp/kind :function
                                :name 'bad-smooth
                                :params ['sample 'count]
                                :process {:controls [{:name 'count :type :int
                                                      :smooth 0.5}]}
                                :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"alternatives"
         (descriptor/normalize {:dsp/kind :function
                                :name 'bad-both
                                :params ['sample 'amount]
                                :process {:controls [{:name 'amount :smooth 0.5
                                                      :slew 0.5}]}
                                :body [1.0]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"invalid shape"
         (descriptor/normalize {:dsp/kind :function
                                :name 'bad-amount
                                :params ['sample 'amount]
                                :process {:controls [{:name 'amount :smooth :fast}]}
                                :body [1.0]})))))

(deftest smoothed-and-slewed-controls-are-partition-independent
  (doseq [[label definition-var control-name]
          [["smoothed" #'control/smoothed-gain :amount]
           ["slewed" #'control/slewed-gain :amount]]
          [target precision] [[:interpreter :f32] [:jvm :f32] [:jvm :f64]]
          :let [reference (render-automation definition-var control-name smoothed-targets
                                             200 target precision)]]
    (testing (str label " " target " " precision)
      ;; The target changes inside the block, so the smoothing must be doing
      ;; something for these comparisons to mean anything.
      (is (not (every? zero? reference)))
      (is (not= reference
                (render-automation definition-var control-name [] 200 target precision)))
      (doseq [block-size [100 64 37 16 7 3 1 5 199]]
        (is (= reference
               (render-automation definition-var control-name smoothed-targets
                                  block-size target precision))
            (str label " " target " " precision " differs at block size " block-size))))))

(deftest per-frame-lanes-carry-host-automation
  (testing "a lane is described by name, with its own slice of the region"
    (let [abi (:abi (dsp/compile #'lane-gain {:target :interpreter :entry :process}))]
      ;; A lane is not a block-rate scalar, so it leaves `:controls` entirely.
      (is (= [] (:controls abi)))
      (is (= [{:name 'amount :type :float :required? true :role :lane :index 0
               :buffer :lanes :offset 0}]
             (:lanes abi)))
      (is (= 1024 (:lane-stride abi)))
      (is (some #{:lanes} (mapv :id (:buffers abi))))))
  (testing "the lane supplies one float per frame"
    (is (= [0.25 0.5 0.75 1.0]
           (render-lane #'lane-gain {:amount (float-array [0.25 0.5 0.75 1.0])} 4))))
  (testing "the ABI names a lane with a symbol, and the keyword spelling works too"
    (is (= (render-lane #'lane-gain {:amount (float-array [1.0 0.5])} 2)
           (render-lane #'lane-gain {'amount (float-array [1.0 0.5])} 2))))
  (testing "lanes are independent slices of one region"
    (let [abi (:abi (dsp/compile #'two-lane-mix {:target :interpreter :entry :process}))]
      (is (= [{:name 'a :offset 0} {:name 'b :offset 1024}]
             (mapv #(select-keys % [:name :offset]) (:lanes abi)))))
    (is (= [1.5 1.25 1.0 0.5]
           (render-lane #'two-lane-mix
                        {:a (float-array [1.0 1.0 1.0 1.0])
                         :b (float-array [0.5 0.25 0.0 -0.5])}
                        4))))
  (testing "a lane composes with kernel smoothing"
    (let [plain (render-lane #'lane-gain {:amount (float-array [1.0 1.0 1.0 1.0])} 4)
          smoothed (render-lane #'lane-smoothed-gain
                                {:amount (float-array [1.0 1.0 1.0 1.0])} 4)]
      (is (= [1.0 1.0 1.0 1.0] plain))
      ;; A one-pole from zero: 0, 0.5, 0.75, 0.875.
      (is (= [0.0 0.5 0.75 0.875] smoothed))))
  (testing "a lane with no values fails loudly rather than publishing zeros"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"no values"
         (render-lane #'lane-gain {} 2))))
  (testing "invalid lanes are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must have type :float"
         (descriptor/normalize {:dsp/kind :function
                                :name 'bad-lane
                                :params ['sample 'count]
                                :process {:controls [{:name 'count :type :int :lane true}]}
                                :body [1.0]}))))
  (testing "every target describes the same lane region"
    (doseq [target [:interpreter :jvm :js :wasm]]
      (let [abi (:abi (dsp/compile #'lane-gain {:target target :entry :process}))]
        (is (= [{:name 'amount :type :float :required? true :role :lane :index 0
                 :buffer :lanes :offset 0}]
               (:lanes abi))
            (name target))
        (is (some #{:lanes} (mapv :id (:buffers abi))) (name target)))))
  (testing "the interpreter and the JVM render lanes identically"
    (doseq [precision [:f32 :f64]]
      (is (= (render-lane #'two-lane-mix
                          {:a (float-array [1.0 1.0 1.0 1.0])
                           :b (float-array [0.5 0.25 0.0 -0.5])}
                          4)
             (render-on #'two-lane-mix :jvm precision
                        {'a [1.0 1.0 1.0 1.0] 'b [0.5 0.25 0.0 -0.5]} 4))
          (name precision))))
  (testing "a lane's stride is the declared frame capacity, at either width"
    ;; A region holds one *block* per lane, and a block is a frame count, so the f32
    ;; and f64 artifacts of one IR now accept the same blocks. The earlier version of
    ;; this test pinned the opposite — when the stride derived from a 4 KB byte
    ;; budget, an f64 voice silently held half as many frames per lane as an f32 one
    ;; (P12). That is also what the byte/element mix-up this test once caught was a
    ;; symptom of: a lane offset is a compile-time element index, so the two counts
    ;; could not be the same number.
    (doseq [precision [:f32 :f64]]
      (let [artifact (dsp/compile #'two-lane-mix {:target :interpreter
                                                  :entry :process
                                                  :precision precision})
            abi (:abi artifact)]
        (is (= 1024 (:lane-stride abi)) (name precision))
        (is (= [0 1024] (mapv :offset (:lanes abi))) (name precision))
        (is (= 1024 (get-in artifact [:options :frame-capacity])) (name precision))))
    (testing "a declared capacity shortens the stride"
      (let [abi (:abi (dsp/compile #'two-lane-mix {:target :interpreter
                                                   :entry :process
                                                   :frame-capacity 64}))]
        (is (= 64 (:lane-stride abi)))
        (is (= [0 64] (mapv :offset (:lanes abi)))))))
  (testing "the generated targets take one region and read it per frame"
    (let [js (:source (dsp/compile #'lane-gain {:target :js :entry :process}))
          wasm (dsp/compile #'lane-gain {:target :wasm :entry :process})]
      ;; The generated names and spellings are Squint's, so these pin the
      ;; target-level shape: the argument list, an input read per frame, and a
      ;; lane read per frame from the region.
      (is (re-find #"\(input, output, frames, sampleRate, lanes\)" js))
      (is (re-find #"input\[frame" js))
      (is (re-find #"lanes\[\(frame" js)
          "a lane is read per frame from the region")
      (is (re-find #"\$lanes" (:wat wasm)))
      (is (= 4096 (get-in wasm [:memory :regions :lanes :bytes]))
          "one lane is one channel stride")
      (is (= 8192 (get-in wasm [:memory :lanes-offset]))
          "the lane region follows the input and output ports")))
  (testing "the raw JVM ABI takes the lane region and validates it"
    (let [artifact (dsp/compile #'lane-gain {:target :jvm :entry :process})
          region (float-array 1024)
          out (float-array 2)]
      (aset region 0 (float 0.5))
      (aset region 1 (float 0.25))
      (dsp/process-checked! artifact (float-array [1.0 1.0]) out 2 48000.0
                            (float-array 0) (int-array 0) region)
      (is (= [0.5 0.25] (vec out)))
      (dsp/process-unchecked! artifact (float-array [1.0 1.0]) out 2 48000.0
                              (float-array 0) (int-array 0) region)
      (is (= [0.5 0.25] (vec out)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"shorter than the lane layout"
                            (dsp/process-checked! artifact (float-array 2) (float-array 2) 2
                                                  48000.0 (float-array 0) (int-array 0)
                                                  (float-array 10))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"wrong primitive type"
                            (dsp/process-checked! artifact (float-array 2) (float-array 2) 2
                                                  48000.0 (float-array 0) (int-array 0)
                                                  (double-array 1024))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"declares per-frame lanes"
                            (dsp/process-checked! artifact (float-array 2) (float-array 2) 2
                                                  48000.0 (float-array 0) (int-array 0))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"declares per-frame lanes"
                            (dsp/process-unchecked! artifact (float-array 2) (float-array 2) 2
                                                    48000.0 (float-array 0) (int-array 0)))))))

(deftest a-control-carries-an-optional-lane-beside-its-scalar
  ;; `:has-lane` is the additive counterpart of `:lane`: the control keeps its
  ;; block-rate scalar, gains a per-frame lane beside it in the ABI, and the DSP
  ;; composes the two itself through the `(lane control)` accessor. Because the
  ;; region is optional, a host that never sends automation gets the scalar.
  (let [render (fn [target controls lanes frames]
                 (let [artifact (dsp/compile #'lane-carried-control
                                             {:target target :entry :process})
                       output (float-array frames)]
                   (dsp/process! artifact
                                 (cond-> {:output output :frames frames
                                          :sample-rate 48000.0 :controls controls}
                                   (seq lanes) (assoc :lanes lanes)))
                   (mapv double (vec output))))]
    (testing "the control keeps its scalar and gains an optional lane"
      (let [abi (:abi (dsp/compile #'lane-carried-control {:target :interpreter
                                                           :entry :process}))]
        (is (= [{:name 'amount :type :float}] (:controls abi))
            "the scalar control is unchanged")
        (is (= [{:name 'amount :type :float :required? false :role :lane :index 0
                 :buffer :lanes :offset 0}]
               (:lanes abi))
            "and the lane beside it is optional, not mandatory")))
    (testing "the accessor composes the lane with the scalar"
      (doseq [target [:interpreter :jvm]]
        (is (= [11.0 12.0 13.0]
               (render target [10.0] {'amount (float-array [1.0 2.0 3.0])} 3))
            (name target))))
    (testing "with no lane data the scalar stands, because an absent lane reads zero"
      (doseq [target [:interpreter :jvm]]
        (is (= [10.0 10.0 10.0] (render target [10.0] nil 3)) (name target))))
    (testing "a mandatory lane still has to be supplied"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"declares per-frame lanes"
                            (render-lane #'lane-gain nil 2))))
    (testing "an accessor with no lane is refused"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"carries no lane"
           (dsp/compile {:dsp/kind :function
                         :name 'no-lane
                         :params '[sample amount]
                         :process {:controls [{:name 'amount}]}
                         :body '[(lane amount)]}
                        {:target :interpreter :entry :process}))))
    (testing "an accessor inside a callee reads the caller's lane (F3)"
      ;; There is one lane region and the entry lays it out, so a callee is
      ;; *handed* it rather than reaching for one: the same slice is read wherever
      ;; the accessor appears, which is what makes automation survive composition.
      (let [render-callee (fn [target controls lanes frames]
                            (let [artifact (dsp/compile #'lane-carrying-caller
                                                        {:target target :entry :process})
                                  output (float-array frames)]
                              (dsp/process! artifact
                                            (cond-> {:output output :frames frames
                                                     :sample-rate 48000.0
                                                     :controls controls}
                                              (seq lanes) (assoc :lanes lanes)))
                              (mapv double (vec output))))]
        (doseq [target [:interpreter :jvm]]
          (is (= [11.0 12.0 13.0]
                 (render-callee target [10.0] {'amount (float-array [1.0 2.0 3.0])} 3))
              (name target))
          (is (= [10.0 10.0 10.0] (render-callee target [10.0] nil 3))
              (str (name target) " with no automation")))))
    (testing "a control cannot be a lane and lane-carrying at once"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot be a lane and lane-carrying"
           (descriptor/normalize {:dsp/kind :function
                                  :name 'both
                                  :params '[amount]
                                  :process {:input false
                                            :controls [{:name 'amount
                                                        :lane true
                                                        :has-lane true}]}
                                  :body [1.0]}))))))

(deftest a-shared-control-is-read-from-the-pool-wide-region
  ;; A shared control leaves the scalar control list and is read from the
  ;; process's `:shared` region instead — one region for the whole pool, so a
  ;; broadcast parameter is written once rather than once per voice (M5). The
  ;; region has the same shape as a lane region, so it reuses the lane address
  ;; and a host with a per-frame global curve can write one.
  (let [render (fn [target shared]
                 (let [artifact (dsp/compile #'shared-gain {:target target
                                                            :entry :process})
                       output (if (= :f64 (:precision artifact))
                                (double-array 4) (float-array 4))]
                   (dsp/process! artifact {:output output :frames 4
                                           :sample-rate 48000.0
                                           :controls [] :shared shared})
                   (mapv double (vec output))))]
    (testing "the region's value is the control"
      (is (= [1.5 1.5 1.5 1.5] (render :interpreter {:amount 0.5})))
      (is (= [1.5 1.5 1.5 1.5] (render :jvm {:amount 0.5})) "the two hosts agree"))
    (testing "a value the host writes reads for every frame of the block"
      (is (= [1.0 1.0 1.0 1.0] (render :jvm {:amount 0.0}))))
    (testing "a sequence is a per-frame global curve"
      (is (= [1.0 2.0 3.0 4.0] (render :jvm {:amount (list 0.0 1.0 2.0 3.0)}))))
    (testing "a shared control is a region, not a scalar control"
      (let [abi (:abi (dsp/compile #'shared-gain {:target :interpreter
                                                  :entry :process}))]
        (is (= [] (mapv :name (:controls abi))))
        (is (= '[amount] (mapv :name (:shared abi))))
        (is (= (:lane-stride abi) (:shared-stride abi))
            "the shared and lane regions have one stride, so one addressing")))
    (testing "a missing shared region is refused rather than read as zeros"
      (let [artifact (dsp/compile #'shared-gain {:target :jvm :entry :process})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"shared control has no value"
                              (dsp/process! artifact {:output (float-array 4)
                                                      :frames 4
                                                      :sample-rate 48000.0
                                                      :controls []
                                                      :shared {}})))))
    (testing "a control cannot be shared and a lane at once"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot be a shared variable and a lane"
           (descriptor/normalize {:dsp/kind :function
                                  :name 'both
                                  :params '[amount]
                                  :process {:input false
                                            :controls [{:name 'amount
                                                        :shared true
                                                        :lane true}]}
                                  :body [1.0]}))))))
