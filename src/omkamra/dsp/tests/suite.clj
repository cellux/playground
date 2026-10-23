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
            [omkamra.dsp.tests.equivalence]
            [omkamra.dsp.tests.harness]
            [omkamra.dsp.tests.interpreter]
            [omkamra.dsp.tests.ir]
            [omkamra.dsp.tests.js]
            [omkamra.dsp.tests.jvm]
            [omkamra.dsp.tests.library]
            [omkamra.dsp.tests.linker]
            [omkamra.dsp.tests.literals]
            [omkamra.dsp.tests.memory]
            [omkamra.dsp.tests.modulation]
            [omkamra.dsp.tests.numbers]
            [omkamra.dsp.tests.process]
            [omkamra.dsp.tests.reference]
            [omkamra.dsp.tests.sequencer]
            [omkamra.dsp.tests.synth]
            [omkamra.dsp.tests.wasm]))

(def test-namespaces
  "The namespaces `run-jvm` runs, in the order a failure is worth reading."
  '[omkamra.dsp.tests.descriptor
    omkamra.dsp.tests.equivalence
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
    omkamra.dsp.tests.literals
    omkamra.dsp.tests.numbers
    omkamra.dsp.tests.conformance
    omkamra.dsp.tests.automation
    omkamra.dsp.tests.modulation
    omkamra.dsp.tests.synth
    omkamra.dsp.tests.reference
    omkamra.dsp.tests.sequencer
    omkamra.dsp.tests.harness])

(defn run-jvm
  "Run every DSP test namespace and return the result.

  `clojure.test`'s summary in its own vocabulary — `:test`, `:pass`, `:fail`,
  `:error` — plus `:passed?`: a half answers in the same shape whichever half it
  is, so `run-everything` reports each as it came instead of reshaping it.

  This is the half that needs neither a browser nor a dev server. The other half
  is `run-browser`, and `run-everything` runs both."
  []
  (let [{:keys [fail error] :as summary} (apply test/run-tests test-namespaces)]
    (assoc summary :passed? (and (zero? (or fail 0)) (zero? (or error 0))))))

(defn run-browser
  "Run the browser conformance suite and return `browser/run!`'s result.

  That result is already the shape a half reports in — `:passed`, `:total`,
  `:results`, `:failures` and a boolean `:passed?` — so this adds nothing to it.

  Separate from `run-jvm` because it needs a browser and a dev server, and a
  green JVM run says nothing about either. Options are `browser/run!`'s — `:only`
  and `:views` — so one case can be looked at while the rest stays out of the way."
  ([] (run-browser {}))
  ([options] (browser/run! options)))

(defn parity
  "The four-target parity table, in one call.

  Builds the interpreter and JVM rows from the catalog and the JavaScript and Wasm
  rows from a browser run of the same catalog, then reports what the four targets
  proved *together*: `{:cases n :coverage {...} :gaps [...]}`, where a gap is a
  declared target a row never reached — a mismatch or a case that did not run.
  `:passed?` is true only when the browser suite passed and no row has a gap. The
  JVM *test* namespaces are `run-jvm`'s business, not this report's.

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

(defn- reported-half
  "A half as it is reported: its own result, or a timeout in its place. Carries
  `:passed?`, because a half that never finished did not pass."
  [half]
  (if (= ::timeout half) {:timeout true :passed? false} half))

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

  The report is two halves and a verdict:

  - `:jvm`     `run-jvm`'s result, as it came;
  - `:browser` `run-browser`'s result, as it came. Both halves answer `:passed?`,
    so neither has to be interpreted here to be placed;
  - `:passed?`, `:seconds` and `:failures`   the two together: whether everything
    passed, how long the whole thing took, and every failure either half reported
    as `[half name detail]` so one call answers what went wrong where.

  A half carries `:timeout true` — and `:passed? false` — when it did not finish."
  ([] (run-everything {}))
  ([{:keys [timeout only] :or {timeout 1800000}}]
   (let [started (System/nanoTime)
         ;; `run-tests` reports through `*test-out*`, which it may bind to `*out*`
         ;; of its own accord, so capture both: the suite must not interleave its
         ;; output with the browser half's.
         jvm-output (java.io.StringWriter.)
         jvm (future (binding [test/*test-out* jvm-output
                               *out* jvm-output]
                       (run-jvm)))
         browser (future (browser/run! (cond-> {} only (assoc :only only))))
         jvm (reported-half (deref jvm timeout ::timeout))
         browser (reported-half (deref browser timeout ::timeout))]
     (println "=== DSP JVM suite ===")
     (print (str jvm-output))
     (flush)
     (let [failures (into (if (:timeout browser)
                            [[:browser "(timed out)" nil]]
                            (mapv (fn [failure] [:browser (:name failure) (:detail failure)])
                                  (:failures browser)))
                          ;; clojure.test reports a failure by printing it, and
                          ;; that output is directly above; naming the half is the
                          ;; honest summary rather than a detail to go looking for.
                          (when-not (:passed? jvm)
                            [[:jvm "(the suite failed)" "see the suite output above"]]))
           report {:jvm jvm
                   :browser browser
                   :passed? (and (true? (:passed? jvm)) (true? (:passed? browser)))
                   :seconds (/ (double (- (System/nanoTime) started)) 1.0e9)
                   :failures failures}]
       (when-not (:passed? jvm)
         (println "=== jvm: failed (see the suite output above) ==="))
       (println (if (:timeout browser)
                  "=== browser: timed out ==="
                  (format "=== browser: %s/%s checks passed (%.1fs total) ==="
                          (:passed browser) (:total browser)
                          (:seconds report))))
       (doseq [[half name detail] failures]
         (println (str "  " half ": " name (when detail (str ": " detail)))))
       (println (format "=== %s ===" (if (:passed? report) "all green" "FAILED")))
       (flush)
       report))))
