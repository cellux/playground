(ns omkamra.dsp.tests.harness
  "The browser harness's own contracts.

  Two things live here: the wire encoding a page result arrives in — a
  ClojureScript keyword is its printed form, so the leading colon has to be
  dropped before keywordizing, or every lookup by case id silently matches
  nothing — and the two queries over a result that a parity check needs: whether
  one case ran and on which targets, and what a whole run proved per case."
  (:require [clojure.test :refer [deftest is]]
            [omkamra.dsp.harness.browser :as browser]
            [omkamra.dsp.harness.conformance :as conformance]))

(def ^:private page-result
  "A result shaped the way the page sends it: keys and enums as strings."
  {:results [{:name "shared process matrix f32"
              :passed? true
              :case-count 2
              :case-failures 1
              :targets [":js" ":wasm"]
              :cases [{:case ":gain" :precision ":f32" :failures []
                       :javascript true :wasm true :passed? true}
                      {:case ":region-buffers" :precision ":f32"
                       :javascript false :wasm true :passed? false
                       :failures [{:target ":js" :detail "mismatch at frame 3"}]}]}
             {:name "shared process matrix f64"
              :passed? true
              :case-count 1
              :case-failures 1
              :targets [":js" ":wasm"]
              :cases [{:case ":gain" :precision ":f64"
                       :javascript true :wasm false :passed? false
                       :failures [{:target ":wasm" :detail "mismatch at frame 512"}]}]}]})

(deftest a-page-result-decodes-its-enumerated-values
  (let [decoded (browser/decode-result page-result)]
    (is (= :gain (get-in decoded [:results 0 :cases 0 :case])))
    (is (= :f32 (get-in decoded [:results 0 :cases 0 :precision])))
    (is (= [:js :wasm] (get-in decoded [:results 0 :targets])))
    ;; Why the decoding is not just `keyword`: the colon ends up *inside* the
    ;; name, and the keyword then compares equal to nothing.
    (is (not= :gain (keyword ":gain")))
    (is (= ":gain" (name (keyword ":gain"))))))

(deftest one-case-is-named-by-its-id-with-or-without-the-colon
  (let [decoded (browser/decode-result page-result)]
    (doseq [id [:gain "gain" ":gain" :omkamra.dsp.harness.catalog/gain]]
      (is (= 2 (count (browser/case-results decoded id)))
          (str "case id " (pr-str id))))
    ;; A row carries its check's name and declared targets, which is what lets
    ;; `coverage` and `parity` read browser rows and interpreter/JVM rows alike.
    (is (= [{:case :region-buffers :precision :f32
             :javascript false :wasm true :passed? false
             :targets [:js :wasm] :check "shared process matrix f32"
             ;; A row carries its own reason now, so a failing case can be read
             ;; without going back to the check that reported it.
             :failures [{:target :js :detail "mismatch at frame 3"}]}]
           (browser/case-results decoded :region-buffers)))
    ;; A case that did not run is an empty answer, not a passing one.
    (is (= [] (browser/case-results decoded :no-such-case)))))

(deftest coverage-names-the-targets-that-ran-a-case-and-matched
  ;; A target that mismatched is absent from the set, so a parity check reads a
  ;; gap by comparing two of these rather than by trusting a single `:passed?`.
  (is (= {:gain {:f32 #{:js :wasm} :f64 #{:js}}
          :region-buffers {:f32 #{:wasm}}}
         (browser/coverage (browser/decode-result page-result)))))

(deftest failures-name-the-checks-that-failed
  (let [checks (:results page-result)
        decoded (browser/decode-result
                 (assoc page-result
                        :passed? false
                        :results [(assoc (first checks) :passed? true)
                                  (assoc (second checks) :passed? false)]))]
    (is (= ["shared process matrix f64"]
           (mapv :name (browser/failures decoded))))
    (is (= [] (browser/failures (browser/decode-result page-result))))
    ;; A check with no verdict at all is reported, not silently counted as passed.
    (is (= ["nameless"]
           (mapv :name (browser/failures
                        (browser/decode-result {:results [{:name "nameless"}]})))))))

(deftest case-failures-name-the-case-and-the-target
  ;; `failures` answers with the checks; this answers with the cases, one level
  ;; down, so that a reader never has to read a target out of a message. The target
  ;; arrives as `\":wasm\"` on the wire and has to be decoded, or `#{:wasm}` below
  ;; would be a set holding only the string.
  (let [decoded (browser/decode-result page-result)]
    (is (= [{:check "shared process matrix f32" :case :region-buffers
             :precision :f32 :targets #{:js}
             :failures [{:target :js :detail "mismatch at frame 3"}]}
            {:check "shared process matrix f64" :case :gain
             :precision :f64 :targets #{:wasm}
             :failures [{:target :wasm :detail "mismatch at frame 512"}]}]
           (browser/case-failures decoded)))
    ;; A check that failed without naming a case is still reported: a check that
    ;; fails saying nothing is the fact.
    (is (= [{:check "selection"}]
           (browser/case-failures
            (browser/decode-result {:results [{:name "selection" :passed? false}]}))))
    ;; The check rows in this synthetic result claim `:passed? true` while their
    ;; cases say otherwise — deliberately, because a case's verdict is the fact and
    ;; a check's summary is a summary.
    (is (= 2 (count (browser/case-failures decoded))))
    (is (= [] (browser/case-failures
               (browser/decode-result
                {:results [{:name "shared process matrix f32" :passed? true
                            :cases [{:case ":gain" :precision ":f32" :failures []
                                     :javascript true :wasm true :passed? true}]}]}))))))

(deftest check-names-are-the-vocabulary-only-selects-from
  (is (= ["shared process matrix f32" "shared process matrix f64"]
         (browser/check-names (browser/decode-result page-result))))
  (is (= ["selection"]
         (browser/check-names
          (browser/decode-result
           {:results [{:name "selection" :passed? false
                       :checks ["Wasm pool host"] :needles ["wasm pool"]}]})))))

(deftest checks-are-found-the-way-only-selects-them
  (let [decoded (browser/decode-result page-result)]
    (is (= ["shared process matrix f32" "shared process matrix f64"]
           (mapv :name (browser/check-results decoded "matrix"))))
    (is (= ["shared process matrix f32"]
           (mapv :name (browser/check-results decoded "MATRIX F32"))))
    (is (= [] (browser/check-results decoded "no-such-check")))))

(deftest parity-reports-the-cases-a-target-did-not-reach
  ;; The synthetic result is the JS bite's shape: `:region-buffers` mismatched on
  ;; JavaScript in f32 only, and `:gain` mismatched on Wasm in f64 only.
  (let [{:keys [cases gaps]} (browser/parity (browser/decode-result page-result))]
    (is (= 3 cases))
    (is (= [{:check "shared process matrix f32" :case :region-buffers
             :precision :f32 :target :js}
            {:check "shared process matrix f64" :case :gain
             :precision :f64 :target :wasm}]
           gaps))
    ;; A check that declares no targets (a per-target check, whose name says which
    ;; target it is) contributes no cases and no gaps.
    (is (= {:cases 0 :gaps []}
           (browser/parity (browser/decode-result
                            {:results [{:name "Wasm pool host" :passed? true}]}))))))

(deftest views-are-attached-by-name-or-refused
  (let [decoded (browser/decode-result page-result)
        attached (browser/attach-views decoded [:parity :failures])]
    (is (= (browser/parity decoded) (:parity attached)))
    (is (= (browser/failures decoded) (:failures attached)))
    (is (not (contains? attached :coverage)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown browser result view"
                          (browser/attach-views decoded [:parityy])))))

(deftest one-row-vocabulary-reads-both-halves
  ;; The interpreter/JVM rows and the browser rows are read by the same two
  ;; functions, which is what lets one table cover all four targets: the keys a
  ;; target's verdict lives under are the only thing `conformance` has to know
  ;; about either half.
  (let [jvm-rows [{:case :gain :precision :f32 :targets [:interpreter :jvm]
                   :interpreter true :jvm true :passed? true}
                  {:case :gain :precision :f64 :targets [:interpreter :jvm]
                   :interpreter true :jvm false :passed? false}]
        browser-rows (browser/case-rows (browser/decode-result page-result))]
    (is (= {:gain {:f32 #{:interpreter :jvm} :f64 #{:interpreter}}}
           (conformance/coverage jvm-rows)))
    (is (= {:gain {:f32 #{:js :wasm} :f64 #{:js}}
            :region-buffers {:f32 #{:wasm}}}
           (conformance/coverage browser-rows)))
    (is (= [{:case :gain :precision :f64 :target :jvm}]
           (:gaps (conformance/parity jvm-rows))))
    (is (= 2 (:cases (conformance/parity jvm-rows))))))

(def ^:private page-result-with-a-targetless-check
  "A result carrying the compiler equivalence check beside a matrix: the
  equivalence check reports cases but no targets, because no kernel ran."
  (update page-result
          :results
          conj
          {:name "compiler equivalence"
           :passed? false
           :case-count 1
           :case-failures 1
           :skipped []
           :cases [{:case ":gain" :passed? false
                    :failures [{:detail "{:path [:locals 2 :name]}"}]}]}))

(deftest only-a-check-that-declares-targets-contributes-parity-rows
  ;; A case that was never run and a case that ran and failed must not look
  ;; alike, and the compiler-equivalence rows ran no kernel at all.
  (let [decoded (browser/decode-result page-result-with-a-targetless-check)
        rows (browser/case-rows decoded)]
    (is (= ["shared process matrix f32" "shared process matrix f64"]
           (distinct (map :check rows))))
    (is (every? seq (map :targets rows)))))

(deftest a-failure-without-a-target-reports-no-targets
  ;; The equivalence check's rows name a case and a path; they have no target to
  ;; name, and a nil in the set would be a target that does not exist.
  (let [decoded (browser/decode-result page-result-with-a-targetless-check)
        rows (browser/case-failures decoded)
        row (first (filter #(= "compiler equivalence" (:check %)) rows))]
    (is (= :gain (:case row)))
    (is (= #{} (:targets row)))
    (is (= 1 (count (:failures row))))))
