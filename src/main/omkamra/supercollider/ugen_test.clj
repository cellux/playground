(ns omkamra.supercollider.ugen-test
  (:require [clojure.test :refer [deftest is]]
            [omkamra.supercollider.synthdef :as synthdef]
            [omkamra.supercollider.ugen :as ugen]))

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
