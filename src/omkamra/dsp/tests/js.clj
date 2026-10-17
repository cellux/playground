(ns omkamra.dsp.tests.js
  "The JavaScript target: its ES-module contract, and the standalone-module rule.

  Emission goes through `squint.compiler`, so the exact spelling of the generated
  JavaScript is Squint's and is not what these tests pin down. What they pin down
  is the part that is a contract — the module a host loads, the ABI's physical
  type and real-time metadata — and the rule that keeps a kernel a standalone
  module: nothing may lower to a `squint_core` reference. Behaviour is the browser
  conformance suite's job; see `TESTING.md`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [omkamra.dsp :as dsp]
            [omkamra.dsp.harness.catalog :as catalog]
            [omkamra.dsp.harness.conformance :as conformance]
            [omkamra.dsp.harness.fixtures :refer [clip one-pole shifted-buffer
                                                  stereo-cross]]))

(defn- standalone?
  "True when a module stands on its own: no Squint standard-library reference,
  and no import of any kind. A kernel is installed as one self-contained script,
  so this is the property the emitter has to preserve."
  [source]
  (and (not (str/includes? source "squint_core"))
       (not (str/includes? source "import"))))

(deftest javascript-backend-emits-standalone-es-module
  (let [{:keys [source module] :as artifact}
        (dsp/compile one-pole {:target :js :entry :process})]
    (is (= :js (:target artifact)))
    (is (= {:format :es-module :exports [:createKernel]} module))
    (is (str/includes? source "export { createKernel }"))
    (is (str/includes? source "Float32Array"))
    (is (str/includes? source "Math.fround"))
    (is (standalone? source))))

(deftest every-catalog-case-is-a-standalone-javascript-module
  ;; The rule over every construct the language has: comparisons and the boolean
  ;; operators, loops with `break`/`continue` and with a bound, fixed-size array
  ;; and record state, multichannel stores, and the math builtins. The browser
  ;; suite runs these; this is the cheap check that the module they run is one
  ;; script.
  (doseq [case catalog/all-cases
          precision (:precisions case)
          :let [source (:source (conformance/target-artifact case precision :js))]]
    (is (standalone? source) (str (:id case) " " (name precision)))))

(deftest javascript-backend-emits-conditionals
  (let [{js-source :source} (dsp/compile clip {:target :js :entry :process})
        {wat :wat} (dsp/compile clip {:target :wasm :entry :process})]
    (is (str/includes? js-source "if ("))
    (is (str/includes? js-source " < "))
    (is (str/includes? wat "f32.lt"))
    (is (str/includes? wat "(if "))))

(deftest javascript-and-wasm-backends-lower-all-comparisons
  ;; Both native targets implement every comparison operator. `:not=` has no
  ;; native ClojureScript operator that is not `=`'s deep equality, so on
  ;; JavaScript it is a negation of `===` rather than `!==`.
  (doseq [[operator javascript-token wasm-operator]
          [['= "===" "f32.eq"]
           ['not= "!(" "f32.ne"]
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
      (is (str/includes? js-source javascript-token)
          (str operator " in JavaScript"))
      (is (standalone? js-source) (str operator " stays standalone"))
      (is (str/includes? wat wasm-operator)))))

(deftest javascript-and-wasm-backends-emit-buffer-access
  (let [{js-source :source} (dsp/compile shifted-buffer
                                         {:target :js :entry :process})
        {wat :wat} (dsp/compile shifted-buffer
                                {:target :wasm :entry :process})]
    (is (str/includes? js-source "input["))
    (is (str/includes? js-source "output["))
    (is (str/includes? wat "(f32.load"))
    (is (str/includes? wat "(f32.store"))))

(deftest javascript-and-wasm-backends-emit-multichannel-access
  ;; D40: multi-channel access is explicit (`input[1]`, `output[1]`), not a
  ;; per-channel loop.
  (let [{js-source :source} (dsp/compile stereo-cross
                                         {:target :js :entry :process})
        {memory :memory}
        (dsp/compile stereo-cross
                     {:target :wasm :entry :process})]
    (is (str/includes? js-source "input[0]"))
    (is (str/includes? js-source "input[1]"))
    (is (str/includes? js-source "output[1]"))
    (is (not (str/includes? js-source "for (let channel")))
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
    (is (standalone? (:source javascript)))
    (is (= false (get-in javascript [:realtime :allocations?])))
    (is (= false (get-in wasm [:realtime :allocations?])))))
