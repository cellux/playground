(ns omkamra.dsp.tests.wasm
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [one-pole]]))

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
