(ns omkamra.dsp.harness.browser
  "Run the shared DSP conformance catalog in a real browser.

  The catalog lives in `omkamra.dsp.harness.catalog` (`.cljc`), so the same cases
  drive the interpreter/JVM suite in `omkamra.dsp-test` and the browser
  JavaScript/Wasm suite in `omkamra.dsp.harness.testbed`.

  This namespace is the Clojure-side driver.  It starts the `:on-demand-http`
  system when needed (which also serves the Shadow page and starts its watcher),
  opens the `omkamra.dsp.harness.testbed` page in a headless browser through spel, and
  calls the page's programmatic `runConformance` hook.  The hook is installed
  by `omkamra.dsp.harness.testbed/init`; navigating with `?autorun=0` suppresses the
  page's own automatic run so the programmatic call is the only run.

  The result is a plain map: `{:passed? :passed :total :results}`.  Each entry
  in `:results` has `:name`, `:passed?`, and `:detail`; the shared matrices also
  carry `:targets` and a per-case `:cases` breakdown such as
  `{:case :gain :precision :f32 :javascript true :wasm true}`.  Use `run!`
  interactively or from a REPL; `passed?` and `assert-passed!` are convenient
  from tests or scripts."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.spel.core :as spel]
            [com.blockether.spel.page :as page]
            [omkamra.dev.on-demand-http :as http]
            [omkamra.dev.systems :as systems]
            [omkamra.dsp.harness.conformance :as conformance]))

(def testbed-path
  "Browser testbed page served by `:on-demand-http`."
  "/omkamra/dsp/harness/testbed")

(defn server-url
  "Return the base URL of the configured on-demand HTTP server."
  []
  (let [{:keys [host port]} (http/server-config)]
    (str "http://" host ":" port)))

(defn ensure-server!
  "Ensure `:on-demand-http` is running and return its base URL."
  []
  (when-not (:running? (systems/status :on-demand-http))
    (http/start!))
  (server-url))

(def ^:private ready-hook
  "() => !!(window.omkamraDspTestbed && window.omkamraDspTestbed.runConformance)")

(declare keywordize)

(defn- browser-keyword
  "The keyword a browser-encoded enum denotes.

  The page sends a ClojureScript keyword as its printed form, so `:gain` arrives
  as the *string* `\":gain\"`. `(keyword \":gain\")` would build a keyword whose
  name contains the colon, which compares equal to nothing — so the leading colon
  has to go first. What follows may still carry a namespace, as a catalog case id
  does."
  [value]
  (if (string? value)
    (keyword (if (str/starts-with? value ":") (subs value 1) value))
    value))

(defn- normalize-value
  "Decode the enumerated values the browser encodes as strings."
  [k v]
  (case k
    (:case :precision) (browser-keyword v)
    :targets (mapv browser-keyword v)
    (keywordize v)))

(defn- keywordize
  "Recursively keywordize keys and known enum values from a Playwright value."
  [value]
  (cond
    (map? value) (into {}
                       (map (fn [[k v]]
                              (let [k (keyword k)]
                                [k (normalize-value k v)])))
                       value)
    (vector? value) (mapv keywordize value)
    :else value))

(defn decode-result
  "Decode a raw page result into the shape `run!` returns.

  This is the wire contract, kept public so it can be pinned by a test: a
  Playwright value has string keys, and a ClojureScript keyword arrives as its
  printed form — `:gain` is the string `\":gain\"` — so the enumerated values are
  decoded by dropping a leading colon rather than by `keyword` alone."
  [result]
  (keywordize result))

(defn case-rows
  "Every per-case row a result reports, in check order.

  Each row carries the targets its check declares and the name of that check, so
  the rows are self-describing: `conformance/coverage` and `conformance/parity`
  read them the same way they read the interpreter/JVM rows, which is what lets
  one table cover all four targets."
  [result]
  (into [] (mapcat (fn [check]
                     (map #(assoc % :targets (:targets check) :check (:name check))
                          (:cases check))))
        (:results result)))

(defn case-results
  "The rows a result reports for one catalog case.

  `case-id` may be a keyword or a string, with or without a leading colon or a
  namespace, so `:gain`, `\"gain\"`, `\":gain\"` and
  `:omkamra.dsp.harness.catalog/gain` all name the same case — the catalog's ids are
  flat, and a caller usually has only the printed id from a failure. Each row is one
  matrix entry: `{:case :gain :precision :f32 :javascript true :wasm true
  :passed? true}`, where a target's boolean says it ran the case *and* matched the
  interpreter.

  An empty vector is a fact worth reporting, not a formality: a case that never
  ran and a case that passed both produce no failures, so a comparison against the
  interpreter cannot tell them apart."
  [result case-id]
  (let [wanted (browser-keyword (str case-id))
        wanted-name (name wanted)]
    (filterv (fn [row]
               (let [case (:case row)]
                 (or (= wanted case) (= wanted-name (name case)))))
             (case-rows result))))

(defn coverage
  "What a browser result proved, per catalog case and precision:
  `{case-id {precision #{targets}}}`.

  `conformance/coverage` over this result's rows; the set holds the targets that
  ran the case *and* matched the interpreter."
  [result]
  (conformance/coverage (case-rows result)))

(defn failures
  "The checks a result reports as failed, in run order.

  `:passed?` on the whole result answers *whether*; this answers *which*, which is
  the first thing a bite check has to read. Only `true` counts as passed, so a check
  whose verdict is missing is reported rather than hidden: an absent verdict and a
  passing one must not look alike."
  [result]
  (into [] (remove #(true? (:passed? %))) (:results result)))

(defn check-results
  "The checks whose name contains one of `needles`, the way `:only` selects them.

  Matching is case-insensitive substring matching, so `\"post-mix\"` finds both
  post-mix checks and `\"matrix f32\"` finds one matrix."
  [result & needles]
  (let [needles (mapv (comp str/lower-case str) needles)]
    (filterv (fn [check]
               (let [name (str/lower-case (str (:name check)))]
                 (boolean (some #(str/includes? name %) needles))))
             (:results result))))

(defn parity
  "The cases a browser result did not take to every target their check declares.

  `conformance/parity` over this result's rows: `{:cases n :gaps [...]}`, where a
  gap names the check, the case, the precision and the target that is missing. A
  green run with an empty `:gaps` is the claim; a non-empty one names exactly what
  to look at."
  [result]
  (conformance/parity (case-rows result)))

(def ^:private views
  "The derived views `run!` can attach to a result, by the name a caller asks for."
  {:failures failures
   :coverage coverage
   :parity parity})

(defn attach-views
  "Add the derived views named by `names` to a result and return it.

  Each name is a key of a view function in this namespace, and the view is attached
  under that name, so `{:views [:parity]}` gives `(:parity result)` exactly what
  `(parity result)` would. An unknown name is an error rather than a quiet no-op: a
  mistyped view that silently returns nothing looks like a view with nothing to
  say."
  [result names]
  (reduce (fn [result name]
            (if-let [view (get views name)]
              (assoc result name (view result))
              (throw (ex-info "unknown browser result view"
                              {:view name :known (vec (sort (keys views)))}))))
          result
          names))

(defn installed-headless-shell
  "Return the newest installed Playwright headless-shell path, if any.

  spel's bundled Playwright may expect a different browser revision than the
  one installed under `PLAYWRIGHT_BROWSERS_PATH`.  Passing the installed
  executable explicitly keeps the driver working in the sandbox."
  []
  (when-let [root (some-> (System/getenv "PLAYWRIGHT_BROWSERS_PATH") io/file)]
    (when (.isDirectory root)
      (->> (.listFiles root)
           (filter #(str/starts-with? (.getName %) "chromium_headless_shell-"))
           (sort-by #(.getName %))
           reverse
           (keep (fn [dir]
                   (let [exe (io/file dir "chrome-headless-shell-linux64"
                                      "chrome-headless-shell")]
                     (when (.canExecute exe) (.getPath exe)))))
           first))))

(defn run!
  "Open the browser testbed and run the complete conformance suite.

  Options:

  - `:headless`  launch headless (default true);
  - `:timeout`   readiness/hook timeout in milliseconds (default 60000);
  - `:server?`   start `:on-demand-http` if needed (default true);
  - `:autorun?`  let the page run on load before the programmatic call
                 (default false).  The programmatic result is returned either
                 way, but leaving this false avoids running the suite twice;
  - `:views`     attach derived views to the result, by name: `:failures`
                 (which checks failed), `:coverage` (case -> precision -> the
                 targets that ran it and matched) and `:parity` (the cases that did
                 not reach every target their check declares). Each is a function
                 in this namespace too, so `{:views [:parity]}` saves only the
                 keystrokes: `(parity (run! ...))` is the same value. An unknown
                 name throws;
  - `:only`      run only the checks whose name contains one of these
                 strings, e.g. `[\"instrument\"]` for both instrument hosts or
                 `[\"Wasm instrument host\"]` for one — or, for the two shared
                 matrices, a catalog case ID such as `[\"call-int-led\"]`, which
                 narrows the matrix to that case.  A needle that names neither a
                 check nor a case is reported as a failure instead of running
                 nothing, since an empty run looks exactly like a passing one.
                 The result then contains only those checks, which is much faster
                 than the full suite.

  Returns `{:passed? boolean :passed int :total int :results [...]}` or a spel
  anomaly map if the browser interaction failed. Each result has `:name`,
  `:passed?` and `:detail`; the shared matrices also carry `:targets` and a
  per-case `:cases` breakdown (`{:case :gain :precision :f32 :javascript true
  :wasm true :passed? true}`), which is what `case-results` and `coverage` read:
  the first asks whether one case ran and where, the second turns a whole run into
  a case-by-target table so a parity check can compare two runs."
  ([] (run! {}))
  ([{:keys [headless timeout server? autorun? executable-path only views]
     :or {headless true timeout 60000 server? true autorun? false}}]
   (let [base (if server? (ensure-server!) (server-url))
         url (str base testbed-path (when-not autorun? "?autorun=0"))
         launch-options (cond-> {:headless headless}
                          executable-path (assoc :executable-path executable-path)
                          (and (nil? executable-path)
                               (installed-headless-shell))
                          (assoc :executable-path (installed-headless-shell)))]
     (spel/with-playwright [pw (spel/create)]
       (spel/with-browser [browser (spel/launch-chromium pw launch-options)]
         (spel/with-page [p (spel/new-page browser)]
           (page/navigate p url)
           (page/wait-for-function p ready-hook {:timeout timeout})
           (let [result (if only
                          (page/evaluate
                           p "(selection) => window.omkamraDspTestbed.runConformance(selection)"
                           (into-array String (map str only)))
                          (page/evaluate
                           p "() => window.omkamraDspTestbed.runConformance()"))]
             (if (spel/anomaly? result)
               result
               (attach-views (decode-result result) views)))))))))

(defn passed?
  "True when `run!` reported that every check passed."
  [result]
  (true? (:passed? result)))

(defn bench!
  "Open the browser testbed and time `(poly N voice)` on both browser hosts.

  Each count is reported the way the page times it — milliseconds per 128-frame
  block, after a warmup — for three lowerings: the **pool** of N independent
  instances (D45's other axis), the fused process **called** (one function per
  definition, what `dsp/compile` does since D47 stage 3), and the same fused process
  **inlined** (`?inline=1`: every definition copied into its call sites, which is
  what every target did before). The last two are the pair that says what calling
  costs and what it removes.

  `voices` is one count or a sequence of them. Returns a vector of rows

      [{:voices 16
        :js {:pool 0.41 :fused 0.55 :fused-inlined 0.91}
        :wasm {...}}]

  or a spel anomaly map if the browser interaction failed. Options are `run!`'s
  minus `:only`. Timing is machine-dependent, so this reports numbers and never
  passes or fails; the page's own loop is the measurement, this is only the driver."
  ([voices] (bench! voices {}))
  ([voices {:keys [headless timeout server? executable-path]
            :or {headless true timeout 60000 server? true}}]
   (let [base (if server? (ensure-server!) (server-url))
         url (str base testbed-path "?autorun=0")
         launch-options (cond-> {:headless headless}
                          executable-path (assoc :executable-path executable-path)
                          (and (nil? executable-path)
                               (installed-headless-shell))
                          (assoc :executable-path (installed-headless-shell)))
         counts (mapv int (if (sequential? voices) voices [voices]))]
     (spel/with-playwright [pw (spel/create)]
       (spel/with-browser [browser (spel/launch-chromium pw launch-options)]
         (spel/with-page [p (spel/new-page browser)]
           (page/navigate p url)
           (page/wait-for-function p ready-hook {:timeout timeout})
           (let [result (page/evaluate
                         p (str "(counts) => Promise.all(counts.map((n) => "
                                "window.omkamraDspTestbed.benchConcurrency(n)"
                                ".then((row) => Object.assign({voices: n}, row))))")
                         (into-array Integer counts))]
             (if (spel/anomaly? result)
               result
               (attach-views (decode-result result) views)))))))))

(defn assert-passed!
  "Throw unless `run!` reported that every check passed.

  Returns the result so callers can inspect individual checks."
  [result]
  (when-not (passed? result)
    (throw (ex-info "Browser DSP conformance failed"
                    {:passed (:passed result)
                     :total (:total result)
                     :failures (into []
                                     (remove :passed?)
                                     (:results result))})))
  result)
