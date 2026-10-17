(ns omkamra.dsp.tests.js
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.fixtures :refer [channel-mix clip one-pole shifted-buffer]]))

(deftest javascript-backend-emits-es-module
  (let [{:keys [source module] :as artifact}
        (dsp/compile one-pole {:target :js :entry :process})]
    (is (= :js (:target artifact)))
    (is (= {:format :es-module :exports [:createKernel]} module))
    (is (str/includes? source "export function createKernel()"))
    (is (str/includes? source "new Float32Array"))
    (is (str/includes? source "Math.fround"))
    (is (str/includes? source "function init()"))
    (is (str/includes? source "function reset()"))
    (is (str/includes? source "return {process, init, reset, destroy}"))))

(deftest javascript-and-wasm-backends-emit-conditionals
  (let [{js-source :source} (dsp/compile clip {:target :js})
        {wat :wat} (dsp/compile clip {:target :wasm :entry :process})]
    (is (str/includes? js-source "if ("))
    (is (str/includes? js-source " < "))
    (is (str/includes? wat "f32.lt"))
    (is (str/includes? wat "(if "))))

(deftest javascript-and-wasm-backends-emit-all-comparisons
  (doseq [[operator javascript-token wasm-operator]
          [['= "===" "f32.eq"]
           ['not= "!==" "f32.ne"]
           ['< "<" "f32.lt"]
           ['<= "<=" "f32.le"]
           ['> ">" "f32.gt"]
           ['>= ">=" "f32.ge"]]]
    (let [definition {:dsp/kind :function
                      :name 'compare-values
                      :params ['x 'y]
                      :body [(list 'if (list operator 'x 'y) 1.0 0.0)]}
          {js-source :source} (dsp/compile definition {:target :js})
          {wat :wat} (dsp/compile definition {:target :wasm :entry :process})]
      (is (str/includes? js-source javascript-token))
      (is (str/includes? wat wasm-operator)))))

(deftest javascript-and-wasm-backends-emit-buffer-access
  (let [{js-source :source} (dsp/compile shifted-buffer
                                         {:target :js :entry :process})
        {wat :wat} (dsp/compile shifted-buffer
                                {:target :wasm :entry :process})]
    (is (str/includes? js-source "input[(frame + 1)]"))
    (is (str/includes? js-source "output[(frame + 1)]"))
    (is (str/includes? wat "(f32.load"))
    (is (str/includes? wat "(f32.store"))))

(deftest javascript-and-wasm-backends-emit-multichannel-access
  (let [{js-source :source} (dsp/compile channel-mix
                                         {:target :js :entry :process
                                          :channels 2})
        {wat :wat memory :memory}
        (dsp/compile channel-mix
                     {:target :wasm :entry :process :channels 2})]
    (is (str/includes? js-source "input[channel][frame]"))
    (is (str/includes? js-source "output[channel][frame]"))
    (is (str/includes? wat "$channel"))
    (is (= 8192 (:output-offset memory)))
    (is (= 16384 (:required-bytes memory)))))

(deftest typed-js-and-wasm-artifacts
  (let [definition {:dsp/kind :function
                    :name 'typed-artifact
                    :params [{:name 'sample :type :float}
                             {:name 'enabled :type :boolean}
                             {:name 'amount :type :int}]
                    :state [{:name 'count :type :int :init 0
                             :next '(+ count amount)}]
                    :process {:input :sample
                              :controls [{:name :enabled :type :boolean}
                                         {:name :amount :type :int}]}
                    :body ['(if enabled
                              (+ sample (int->float count))
                              sample)]}
        javascript (dsp/compile definition {:target :js :entry :process})
        wasm (dsp/compile definition {:target :wasm :entry :process})]
    (is (str/includes? (:source javascript) "sampleRate"))
    (is (str/includes? (:source javascript) "state[0] | 0"))
    (is (str/includes? (:wat wasm) "(i32.store (i32.const 8192)"))
    (is (= false (get-in javascript [:realtime :allocations?])))
    (is (= false (get-in wasm [:realtime :allocations?])))))
