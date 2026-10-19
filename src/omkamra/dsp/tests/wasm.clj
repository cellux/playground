(ns omkamra.dsp.tests.wasm
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [call-fixture-arena
                                                  call-fixture-inlined
                                                  call-fixture-nested one-pole
                                                  shared-gain]]))

(deftest wasm-backend-emits-wat-and-abi
  (let [{:keys [wat module memory] :as artifact}
        (dsp/compile one-pole {:target :wasm :entry :process})]
    (is (= :wasm (:target artifact)))
    (is (= {:format :wat :exports [:memory :init :reset :process]}
           (select-keys module [:format :exports])))
    (is (= {:page-size 65536
            :initial-pages 1
            :element-type :f32
            :element-bytes 4
            :alignment 4
            :frame-capacity 1024
            :channel-stride 4096
            :input-offset 0
            :output-offset 4096
            :required-bytes 8196}
           (select-keys memory [:page-size :initial-pages :element-type
                                :element-bytes :alignment :frame-capacity
                                :channel-stride :input-offset :output-offset
                                :required-bytes])))
    (is (= {:offset 0 :channels 1 :frame-capacity 1024 :bytes 4096
            :storage :linear-memory}
           (get-in memory [:regions :input])))
    (is (= {:storage :linear-memory :offset 8192 :bytes 4 :count 1}
           (get-in memory [:regions :state])))
    (is (= :wasm-process (get-in artifact [:wasm-abi :kind])))
    (is (str/starts-with? wat "(module "))
    (is (str/includes? wat "(export \"init\")"))
    (is (str/includes? wat "(export \"reset\")"))
    (is (str/includes? wat "(export \"process\")"))
    (is (str/includes? wat "(f32.store (i32.const 8192)"))))

(deftest wasm-backend-calls-definitions-instead-of-copying-them
  ;; D47 stage 4. The IR keeps a call graph for this target now, and the module
  ;; emits one function per retained definition — taking the partition base it is
  ;; handed and addressing its own state relative to it — and calls it with that
  ;; base, the two context values, and the arguments.
  (let [artifact (dsp/compile (var-get #'call-fixture-arena)
                              {:target :wasm :entry :process :precision :f32})
        wat (:wat artifact)]
    (is (= 2 (count (:definitions (:ir artifact)))))
    (is (str/includes? wat "(func $dsp_fn0_"))
    (is (str/includes? wat "(param $__dsp_base i32)"))
    (is (re-find #"\(call \$dsp_fn0_[A-Za-z0-9_]+ \(i32\.const 8196\) \(local\.get \$frames\) \(local\.get \$sampleRate\)"
                 wat)
        "the first `accumulate` site passes its partition's byte offset")
    (is (str/includes? wat "(f32.load (local.get $__dsp_base))")
        "the callee addresses state relative to the base it was handed"))
  (testing "a callee derives a nested partition's base from the base it was handed"
    (let [wat (:wat (dsp/compile (var-get #'call-fixture-nested)
                                 {:target :wasm :entry :process :precision :f32}))]
      (is (str/includes? wat "(i32.add (local.get $__dsp_base) (i32.const 4))"))))
  (testing "an `^:inline` callee is copied instead, so no function is emitted"
    (let [artifact (dsp/compile (var-get #'call-fixture-inlined)
                                {:target :wasm :entry :process :precision :f32})]
      (is (empty? (:definitions (:ir artifact))))
      (is (not (str/includes? (:wat artifact) "$dsp_fn"))))))

(deftest wasm-f64-artifact-emits-physical-layout
  (let [artifact (dsp/compile one-pole {:target :wasm
                                        :entry :process
                                        :precision :f64})]
    (is (str/includes? (:wat artifact) "f64.load"))
    (is (str/includes? (:wat artifact) "f64.store"))
    (is (= :f64 (get-in artifact [:memory :element-type])))
    (is (= 8 (get-in artifact [:memory :element-bytes])))
    (is (= 512 (get-in artifact [:memory :frame-capacity])))
    (is (= {:format :wasm :source :wat :generator :browser-wabt :bytes nil}
           (:binary artifact)))))

(deftest wasm-lowers-loop-control
  (let [definition {:dsp/kind :function
                    :name 'wasm-loop-control
                    :params [{:name 'sample :type :float}
                             {:name 'limit :type :int}]
                    :process {:input :sample
                              :controls [{:name :limit :type :int}]}
                    :body ['(let [index 0
                                  total sample]
                              (do
                                (while (< index limit)
                                  (do
                                    (set! index (+ index 1))
                                    (if (= index 2)
                                      (continue)
                                      (if (= index 4)
                                        (break)
                                        (set! total (+ total 1.0))))))
                                total))]}
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (str/includes? (:wat wasm) "$source-loop-1"))
    (is (str/includes? (:wat wasm) "$source-loop-done-1"))
    (is (str/includes? (:wat wasm) "(br $source-loop-1)"))
    (is (str/includes? (:wat wasm) "(br $source-loop-done-1)"))))

(deftest bounded-process-loops-are-lowered-across-native-targets
  (let [definition {:dsp/kind :function
                    :name 'bounded-process-loop
                    :params [{:name 'sample :type :float}
                             {:name 'limit :type :int}]
                    :process {:input :sample
                              :controls [{:name :limit :type :int}]}
                    :return-type :float
                    :body ['(let [index 0
                                  total sample]
                              (do
                                (while (< index limit)
                                  (do
                                    (set! index (+ index 1))
                                    (set! total (+ total 1.0)))
                                  {:max-iterations 2})
                                total))]}
        interpreter (dsp/compile definition {:target :interpreter :entry :process})
        jvm (dsp/compile definition {:target :jvm :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})
        process! (fn [artifact limit]
                   (let [input (float-array [1.0])
                         output (float-array 1)]
                     (dsp/process! artifact {:input input :output output :frames 1
                                             :controls [limit]})
                     (aget output 0)))]
    (is (= 2.0 (process! interpreter 1)))
    (is (= 2.0 (process! jvm 1)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"loop exceeded its iteration bound"
                          (process! interpreter 3)))
    (is (thrown-with-msg? IllegalStateException
                          #"loop exceeded its iteration bound"
                          (process! jvm 3)))
    (is (str/includes? (:wat wasm) "$source-loop-count-1"))
    (is (str/includes? (:wat wasm) "(unreachable)"))
    (let [f64-wasm (dsp/compile definition {:target :wasm :entry :process
                                            :precision :f64})
          f64-jvm (dsp/compile definition {:target :jvm :entry :process
                                           :precision :f64})
          input (double-array [1.0])
          output (double-array 1)]
      (is (str/includes? (:wat f64-wasm) "(local $source-loop-count-1 i32)"))
      (is (str/includes? (:wat f64-wasm) "(f64.const 1.0)"))
      (dsp/process! f64-jvm {:input input :output output :frames 1 :controls [1]})
      (is (= 2.0 (aget output 0)))
      (is (thrown-with-msg? IllegalStateException
                            #"loop exceeded its iteration bound"
                            (dsp/process! f64-jvm {:input input :output output
                                                   :frames 1 :controls [3]}))))))

(deftest wasm-backend-emits-the-shared-region
  ;; A shared control is read from one region the *pool* owns, rather than from
  ;; this instance's control storage, so Wasm takes the region's address and
  ;; reads its own linear memory at a compile-time slice (M5). The region is the
  ;; lane region's shape and sits right after it.
  (let [{:keys [wat abi memory]} (dsp/compile shared-gain {:target :wasm
                                                           :entry :process})]
    (testing "the ABI reports one shared control and no scalar control"
      (is (= [] (mapv :name (:controls abi))))
      (is (= '[amount] (mapv :name (:shared abi))))
      (is (= 1024 (:shared-stride abi)))
      (is (= 1024 (:lane-stride abi)) "one stride for both regions"))
    (testing "the process takes the region's address and reads it"
      (is (str/includes? wat "(param $shared i32)"))
      (is (str/includes? wat "local.get $shared")))
    (testing "the layout gives the region a slice of its own"
      (is (= {:offset 8192 :channels 1 :frame-capacity 1024 :bytes 4096
              :storage :linear-memory}
             (get-in memory [:regions :shared])))
      (is (= 12288 (:required-bytes memory))
          "input, output and the shared region; this voice has no state")
      (is (= 12288 (get-in memory [:regions :state :offset]))
          "state starts after the shared region"))))
