(ns omkamra.dsp.tests.suite
  "Every DSP test namespace, so one call still runs the whole language suite.

  The suite is split by subject so a change can be checked against the part it
  concerns. It is also one call when that is what you want, which is what this
  namespace is for: require this, not the individual namespaces. `run-everything`
  is the whole of it: these tests and the browser conformance suite, at once."
  (:require [clojure.test :as test]
            [omkamra.dsp.harness.browser :as browser]
            [omkamra.dsp.harness.conformance :as conformance]
            [omkamra.dsp.tests.automation]
            [omkamra.dsp.tests.combinators]
            [omkamra.dsp.tests.composition]
            [omkamra.dsp.tests.conformance]
            [omkamra.dsp.tests.descriptor]
            [omkamra.dsp.tests.harness]
            [omkamra.dsp.tests.interpreter]
            [omkamra.dsp.tests.ir]
            [omkamra.dsp.tests.js]
            [omkamra.dsp.tests.jvm]
            [omkamra.dsp.tests.library]
            [omkamra.dsp.tests.linker]
            [omkamra.dsp.tests.memory]
            [omkamra.dsp.tests.modulation]
            [omkamra.dsp.tests.process]
            [omkamra.dsp.tests.reference]
            [omkamra.dsp.tests.sequencer]
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
    omkamra.dsp.tests.modulation
    omkamra.dsp.tests.synth
    omkamra.dsp.tests.reference
    omkamra.dsp.tests.sequencer
    omkamra.dsp.tests.harness])

(defn run-all
  "Run every DSP test namespace and return the `clojure.test` summary."
  []
  (apply test/run-tests test-namespaces))

(defn parity
  "The four-target parity table, in one call.

  Builds the interpreter and JVM rows from the catalog and the JavaScript and Wasm
  rows from a browser run of the same catalog, then reports what the four targets
  proved *together*: `{:cases n :coverage {...} :gaps [...]}`, where a gap is a
  declared target a row never reached — a mismatch or a case that did not run.
  `:passed?` is true only when the browser suite passed and no row has a gap. The
  JVM *test* namespaces are `run-all`'s business, not this report's.

  With no argument it runs the browser suite itself; pass a result from
  `browser/run!` to reuse one that already ran. This is the check to make before a
  phase that changes a target's code path: it answers whether a target is behind,
  for every case at once, which a green test run cannot, because a case that passed
  and a case that never ran are equally green.

  The reads are the ones `TESTING.md` documents one target at a time;
  `conformance/coverage` and `conformance/parity` are the same functions over the
  same rows."
  ([] (parity (browser/run!)))
  ([browser-result]
   (let [rows (into (vec (conformance/case-rows)) (browser/case-rows browser-result))
         {:keys [cases gaps]} (conformance/parity rows)]
     {:cases cases
      :coverage (conformance/coverage rows)
      :gaps gaps
      :passed? (and (browser/passed? browser-result) (empty? gaps))})))

(defn- timeout-result
  [half]
  (if (= ::timeout half) {:timeout true} half))

(defn run-everything
  "Run everything that can run here, and report it once.

  The JVM suite and the browser conformance suite are independent — one needs a
  browser and a dev server, the other neither — so they run in two futures and the
  report waits for both. The tooling suite (`omkamra.dev.tools-test`) is deliberately
  *not* here: it reloads namespaces, which would race the tests it ran beside.

  Options:

  - `:timeout` milliseconds to wait for each half (default 30 minutes). A half that
    never finishes is reported, not hung on;
  - `:only`    passed to the browser half, so a single catalog case can be checked
    there while the whole JVM suite still runs (see
    `omkamra.dsp.harness.browser/run!`).

  Returns `{:jvm … :browser … :seconds … :passed? …}`; each half carries `:timeout
  true` when it did not finish."
  ([] (run-everything {}))
  ([{:keys [timeout only] :or {timeout 1800000}}]
   (let [started (System/nanoTime)
         ;; `run-tests` reports through `*test-out*`, which it may bind to `*out*`
         ;; of its own accord, so capture both: the suite must not interleave its
         ;; output with the browser half's.
         jvm-output (java.io.StringWriter.)
         jvm (future (binding [test/*test-out* jvm-output
                               *out* jvm-output]
                       (run-all)))
         browser (future (browser/run! (cond-> {} only (assoc :only only))))
         jvm-result (deref jvm timeout ::timeout)
         browser-result (deref browser timeout ::timeout)
         summary (timeout-result jvm-result)
         conformance (timeout-result browser-result)]
     (println "=== DSP JVM suite ===")
     (print (str jvm-output))
     (flush)
     (let [jvm-failed (or (:timeout summary)
                          (pos? (:fail summary 0))
                          (pos? (:error summary 0)))
           browser-failed (or (:timeout conformance)
                              (not (true? (:passed? conformance))))
           report {:jvm (assoc (select-keys summary [:test :pass :fail :error])
                               :passed? (not jvm-failed))
                   :browser (if (:timeout conformance)
                              {:timeout true :passed? false}
                              {:passed? (true? (:passed? conformance))
                               :passed (:passed conformance)
                               :total (:total conformance)
                               :failed (mapv (juxt :name :detail)
                                             (remove :passed? (:results conformance)))})
                   :seconds (/ (double (- (System/nanoTime) started)) 1.0e9)
                   :passed? (not (or jvm-failed browser-failed))}]
       (println (if (:timeout conformance)
                  "=== browser: timed out ==="
                  (format "=== browser: %s/%s checks passed (%.1fs total) ==="
                          (:passed conformance) (:total conformance)
                          (:seconds report))))
       (doseq [[name detail] (:failed (:browser report))]
         (println (str "  " name ": " detail)))
       (println (format "=== %s ===" (if (:passed? report) "all green" "FAILED")))
       (flush)
       report))))
