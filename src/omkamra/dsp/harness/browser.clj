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
            [omkamra.dev.systems :as systems]))

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

(defn- normalize-value
  "Keywordize the enumerated values the browser encodes as strings."
  [k v]
  (case k
    (:case :precision) (if (string? v) (keyword v) v)
    :targets (mapv (fn [target] (if (string? target) (keyword target) target)) v)
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
  - `:only`      run only the checks whose name contains one of these
                 strings, e.g. `[\"instrument\"]` for both instrument hosts or
                 `[\"Wasm instrument host\"]` for one.  The result then contains
                 only those checks, which is much faster than the full suite.

  Returns `{:passed? boolean :passed int :total int :results [...]}` or a spel
  anomaly map if the browser interaction failed."
  ([] (run! {}))
  ([{:keys [headless timeout server? autorun? executable-path only]
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
             (if (spel/anomaly? result) result (keywordize result)))))))))

(defn passed?
  "True when `run!` reported that every check passed."
  [result]
  (true? (:passed? result)))

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
