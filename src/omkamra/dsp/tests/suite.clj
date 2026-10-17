(ns omkamra.dsp.tests.suite
  "Every DSP test namespace, so one call still runs the whole language suite.

  The suite is split by subject so a change can be checked against the part it
  concerns. It is also one call when that is what you want, which is what this
  namespace is for: require this, not the individual namespaces."
  (:require [clojure.test :as test]
            [omkamra.dsp.tests.automation]
            [omkamra.dsp.tests.combinators]
            [omkamra.dsp.tests.composition]
            [omkamra.dsp.tests.conformance]
            [omkamra.dsp.tests.descriptor]
            [omkamra.dsp.tests.interpreter]
            [omkamra.dsp.tests.ir]
            [omkamra.dsp.tests.js]
            [omkamra.dsp.tests.jvm]
            [omkamra.dsp.tests.library]
            [omkamra.dsp.tests.linker]
            [omkamra.dsp.tests.memory]
            [omkamra.dsp.tests.process]
            [omkamra.dsp.tests.synth]
            [omkamra.dsp.tests.wasm]))

(def test-namespaces
  "The namespaces `run-all` runs, in the order a failure is worth reading."
  '[omkamra.dsp.tests.descriptor
    omkamra.dsp.tests.linker
    omkamra.dsp.tests.ir
    omkamra.dsp.tests.interpreter
    omkamra.dsp.tests.jvm
    omkamra.dsp.tests.js
    omkamra.dsp.tests.wasm
    omkamra.dsp.tests.process
    omkamra.dsp.tests.memory
    omkamra.dsp.tests.composition
    omkamra.dsp.tests.combinators
    omkamra.dsp.tests.library
    omkamra.dsp.tests.conformance
    omkamra.dsp.tests.automation
    omkamra.dsp.tests.synth])

(defn run-all
  "Run every DSP test namespace and return the `clojure.test` summary."
  []
  (apply test/run-tests test-namespaces))
