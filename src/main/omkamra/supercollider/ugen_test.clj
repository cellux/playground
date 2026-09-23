(ns omkamra.supercollider.ugen-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider.env :as env]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen :as ugen]
            [omkamra.supercollider.ugen.all :as all]))

(synthdef/define-synthdef test-beep
  [[freq 440.0]]
  (ugen/Out :ar 0 (ugen/SinOsc :ar freq 0.0)))

(deftest compile-produces-synthdef-map
  (is (= {:name "test-beep"
          :constants [0.0]
          :param-values [440.0]
          :params [{:name "freq" :index 0}]
          :ugens [{:name "Control"
                   :rate 1
                   :inputs []
                   :outputs [1]
                   :special-index 0}
                  {:name "SinOsc"
                   :rate 2
                   :inputs [[0 0] [-1 0]]
                   :outputs [2]
                   :special-index 0}
                  {:name "Out"
                   :rate 2
                   :inputs [[-1 0] [1 0]]
                   :outputs []
                   :special-index 0}]
          :variants []}
         test-beep)))

(deftest create-does-not-bind-a-var
  (let [sdef (synthdef/create
              "anonymous"
              [["freq" 220.0]]
              (fn [freq]
                (ugen/Out :ar 0 (ugen/SinOsc :ar freq 0.0))))]
    (is (= "anonymous" (:name sdef)))
    (is (= [{:name "freq" :index 0}] (:params sdef)))))

(deftest envgen-expands-envelope-and-supports-rate-aliases
  (let [envelope (env/perc 0.01 1.0)
        positional (ugen/EnvGen :ar envelope)
        named (ugen/EnvGen.ar {:envelope envelope
                               :done-action :free-self})]
    (is (= 2 (:rate positional)))
    (is (= 2 (:rate named)))
    (is (= 2 (nth (:inputs named) 4)))
    (is (= (drop 5 (:inputs positional))
           (drop 5 (:inputs named))))))

(deftest all-namespace-contains-metadata-generated-constructors
  (is (= (ugen/SinOsc.ar 440.0 0.0)
         (all/SinOsc.ar 440.0 0.0)))
  (is (= (ugen/Out.ar 0 (ugen/SinOsc.ar 220.0 0.0))
         (all/Out.ar 0 (all/SinOsc.ar 220.0 0.0))))
  (is (= (ugen/SinOsc.ar 440.0 0.0)
         (all/SinOsc:ar 440.0 0.0)))
  (is (fn? all/EnvGen.ar)))

(deftest metadata-generated-constructors-generate-rate-aliases
  (is (= (ugen/SinOsc :ar 440.0 0.0)
         (ugen/SinOsc.ar 440.0 0.0)))
  (is (= (ugen/SinOsc :kr {:freq 220.0 :phase 0.0})
         (ugen/SinOsc.kr {:freq 220.0 :phase 0.0})))
  (is (= (ugen/Out :ar 0 1)
         (ugen/Out.ar 0 1))))

(deftest metadata-generated-constructors-support-named-inputs
  (is (= [440.0 0.0]
         (:inputs (ugen/SinOsc :ar {:freq 440.0 :phase 0.0}))))
  (is (= [0 1 2]
         (:inputs (ugen/Out :ar {:bus 0 :channels [1 2]}))))
  (is (thrown-with-msg? IllegalArgumentException
                          #"unknown inputs"
                          (ugen/SinOsc :ar {:frequency 440.0})))
  (is (thrown-with-msg? IllegalArgumentException
                          #"requires input :channels"
                          (ugen/Out :ar {:bus 0 :channels []}))))

(deftest output-references-select-multi-output-channel
  (let [multi (ugen/node "Multi" :ar [] [:ar :ar])
        root (ugen/node "Use" :ar [(ugen/output multi 1)] [])
        sdef (ugen/compile root)]
    (is (= [{:name "Multi"
             :rate 2
             :inputs []
             :outputs [2 2]
             :special-index 0}
            {:name "Use"
             :rate 2
             :inputs [[0 1]]
             :outputs []
             :special-index 0}]
           (:ugens sdef)))))
