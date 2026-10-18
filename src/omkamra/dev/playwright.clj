(ns omkamra.dev.playwright
  "Browser automation for agents: a curated subset of spel.

  Start here when an agent needs a real browser — open a page, wait for an
  application to become ready, evaluate JavaScript, read and drive DOM
  elements, and capture a screenshot — without reaching for spel's full,
  several-hundred-function API. Playwright implementations live in
  `com.blockether.spel.core`, `com.blockether.spel.page`, and
  `com.blockether.spel.locator`; consult them when you need something this
  namespace does not expose.

  ## Sessions

  `launch!` creates a Playwright instance, launches a browser, opens a page,
  and returns a plain session map:

      {:playwright pw :browser browser :page page}

  Pass it to `close!` when done. `with-page` wraps that lifecycle for a single
  evaluation. A session is ordinary data, so it can be stored in a var and
  reused across REPL evaluations — the usual agent workflow.

  Launch options are the useful subset: `:browser` (`:chromium`, `:firefox`,
  `:webkit`), `:headless`, `:url`, `:ready`, `:timeout`, `:executable-path`,
  and `:launch` for anything else spel understands. By default Chromium runs
  headless against the browser revision actually installed under
  `PLAYWRIGHT_BROWSERS_PATH` (see `installed-executable`), which avoids the
  revision mismatch spel's bundled Playwright can otherwise hit.

  ## Targets

  Every element operation takes a `target` that is one of:

  * a selector string — `\"#submit\"`, `\"button.primary\"`;
  * a `Locator` already built by `locator` or a `get-by-*` function;
  * a keyword, meaning the `data-testid` attribute — `:submit-button`.

  ## Results

  Operations return their value, or an anomaly map when the browser refuses or
  times out, exactly as spel does. No exception is thrown for an expected
  failure; test with `anomaly?` and turn it into an exception with `ensure!`
  when a failure should stop the caller."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.spel.core :as spel]
            [com.blockether.spel.locator :as loc]
            [com.blockether.spel.page :as page])
  (:import [com.microsoft.playwright Locator]
           [com.microsoft.playwright.options AriaRole]))

;; ---------------------------------------------------------------------------
;; Results
;; ---------------------------------------------------------------------------

(defn anomaly?
  "True when `value` is a Playwright anomaly map rather than a result."
  [value]
  (spel/anomaly? value))

(defn ensure!
  "Return `value`, or throw an `ex-info` when it is an anomaly.

  Playwright failures are data so an agent can inspect and recover from them;
  call `ensure!` at a point where a failure really should stop the work."
  [value]
  (if (anomaly? value)
    (throw (ex-info (str "Playwright operation failed: "
                         (or (:com.blockether.anomaly.core/message value)
                             (pr-str value)))
                    value))
    value))

;; ---------------------------------------------------------------------------
;; Installed browser executables
;; ---------------------------------------------------------------------------

(def ^:private browser-prefixes
  {:chromium "chromium"
   :firefox "firefox"
   :webkit "webkit"})

(def ^:private executable-names
  {"chromium_headless_shell" "chrome-headless-shell"
   "chromium" "chrome"
   "firefox" "firefox"
   "webkit" "pw_run.sh"})

(defn- browsers-path
  "The directory Playwright installs browsers under."
  []
  (or (System/getenv "PLAYWRIGHT_BROWSERS_PATH")
      (str (System/getProperty "user.home") "/.cache/ms-playwright")))

(defn- revision
  "The numeric revision suffix of an installed browser directory, if any."
  [^java.io.File dir]
  (some-> (re-find #"-(\d+)$" (.getName dir)) second Long/parseLong))

(defn- executable-in
  "The executable named `name` anywhere under `dir`, or nil."
  [^java.io.File dir name]
  (->> (file-seq dir)
       (filter #(and (.isFile ^java.io.File %)
                     (.canExecute ^java.io.File %)
                     (= name (.getName ^java.io.File %))))
       (sort-by #(.getPath ^java.io.File %))
       first
       (#(when % (.getCanonicalPath ^java.io.File %)))))

(defn installed-executable
  "The newest installed Playwright browser executable, or nil.

  `browser` is `:chromium` (default), `:firefox`, or `:webkit`. For a headless
  Chromium, the headless-shell build is preferred; pass `{:headless false}` for
  the full browser. Searches `PLAYWRIGHT_BROWSERS_PATH` (or the default
  `~/.cache/ms-playwright`) because spel's bundled Playwright may expect a
  browser revision that is not the one installed."
  ([] (installed-executable :chromium))
  ([browser] (installed-executable browser {}))
  ([browser {:keys [headless] :or {headless true}}]
   (let [prefix (if (and (= :chromium browser) headless)
                  "chromium_headless_shell"
                  (get browser-prefixes browser :chromium))
         root (io/file (browsers-path))
         name (get executable-names prefix)]
     (when (and (.isDirectory root) name)
       (->> (.listFiles root)
            (filter #(.isDirectory ^java.io.File %))
            (filter #(str/starts-with? (.getName ^java.io.File %) (str prefix "-")))
            (sort-by revision)
            reverse
            (keep #(executable-in % name))
            first)))))

;; ---------------------------------------------------------------------------
;; Sessions
;; ---------------------------------------------------------------------------

(declare navigate wait-for)

(defn- launch-browser
  "Launch `browser` on playwright instance `pw` with launch options `opts`."
  [pw browser opts]
  (case browser
    :firefox (spel/launch-firefox pw opts)
    :webkit (spel/launch-webkit pw opts)
    (spel/launch-chromium pw opts)))

(defn close!
  "Close a session map and release its browser and Playwright instance.

  Accepts the map `launch!` returned; missing or already-closed resources are
  ignored, and further calls are harmless."
  [{:keys [page browser playwright]}]
  (when page (try (spel/close-page! page) (catch Throwable _ nil)))
  (when browser (try (spel/close-browser! browser) (catch Throwable _ nil)))
  (when playwright (try (spel/close! playwright) (catch Throwable _ nil)))
  nil)

(defn launch!
  "Create Playwright, launch a browser, open a page, and return a session.

  Options:

  * `:browser`         `:chromium` (default), `:firefox`, or `:webkit`;
  * `:headless`        run without a window (default true);
  * `:url`             navigate to this URL before returning;
  * `:ready`           JavaScript expression passed to `wait-for` after
                       navigation — returning truthy signals readiness;
  * `:timeout`         readiness timeout in milliseconds;
  * `:executable-path` skip detection and use this browser binary;
  * `:launch`          extra spel launch options merged over the defaults.

  Returns `{:playwright :browser :page}`, or an anomaly map when a step fails
  (in which case everything already started is cleaned up first)."
  ([] (launch! {}))
  ([{:keys [browser headless url ready timeout executable-path launch]
     :or {browser :chromium headless true}}]
   (let [exe (or executable-path
                 (installed-executable browser {:headless headless}))
         launch-opts (cond-> (merge {:headless headless} launch)
                       exe (assoc :executable-path exe))
         pw (spel/create)]
     (if (anomaly? pw)
       pw
       (let [b (launch-browser pw browser launch-opts)]
         (if (anomaly? b)
           (do (spel/close! pw) b)
           (let [p (spel/new-page b)]
             (if (anomaly? p)
               (do (spel/close-browser! b) (spel/close! pw) p)
               (let [session {:playwright pw :browser b :page p}
                     nav (when url (navigate p url))]
                 (if (anomaly? nav)
                   (do (close! session) nav)
                   (let [ready-result (when ready (wait-for p ready timeout))]
                     (if (anomaly? ready-result)
                       (do (close! session) ready-result)
                       session))))))))))))

(defmacro with-page
  "Run `body` with `sym` bound to a page from `launch!`, then close it.

  `opts` is passed to `launch!`. When the launch fails, the anomaly map is
  returned and `body` does not run. The session is closed even when `body`
  throws.

      (with-page [page {:url \"http://127.0.0.1:8080\" :ready \"() => !!window.app\"}]
        (evaluate page \"window.app.state()\"))"
  [[sym opts] & body]
  `(let [session# (launch! ~opts)]
     (if (anomaly? session#)
       session#
       (try
         (let [~sym (:page session#)]
           ~@body)
         (finally
           (close! session#))))))

(defn new-page
  "Open an additional page on a session's browser."
  [{:keys [browser]}]
  (spel/new-page browser))

;; ---------------------------------------------------------------------------
;; Navigation and page state
;; ---------------------------------------------------------------------------

(defn navigate
  "Navigate `page` to `url`. Returns the response, or an anomaly map."
  ([page url] (page/navigate page url))
  ([page url opts] (page/navigate page url opts)))

(defn reload
  "Reload `page`. Returns the response, or an anomaly map."
  [page]
  (page/reload page))

(defn current-url
  "The current URL of `page`."
  [page]
  (page/url page))

(defn title
  "The title of `page`."
  [page]
  (page/title page))

(defn content
  "The full HTML of `page`."
  [page]
  (page/content page))

;; ---------------------------------------------------------------------------
;; Evaluation and waiting
;; ---------------------------------------------------------------------------

(defn evaluate
  "Evaluate a JavaScript expression in `page` and return Clojure data.

  JavaScript objects and arrays become maps and vectors. An optional `arg` is
  passed to the expression. Returns an anomaly map on failure."
  ([page expression] (page/evaluate page expression))
  ([page expression arg] (page/evaluate page expression arg)))

(defn wait-for
  "Wait until a JavaScript expression returns truthy.

  `opts-or-timeout` is either a timeout in milliseconds or a spel option map
  (`:timeout`, `:polling`, `:arg`). Returns a handle, or an anomaly map on
  timeout. This is the hook wait used to let a client application become ready."
  ([page expression] (page/wait-for-function page expression))
  ([page expression opts-or-timeout]
   (page/wait-for-function page expression
                           (if (number? opts-or-timeout)
                             {:timeout opts-or-timeout}
                             opts-or-timeout))))

(defn wait-for-selector
  "Wait for a selector to satisfy a condition (`{:state :visible}` etc.).

  Returns nil, or an anomaly map on timeout."
  ([page selector] (page/wait-for-selector page selector))
  ([page selector opts] (page/wait-for-selector page selector opts)))

(defn wait-for-url
  "Wait for `page` to reach a URL (string glob, regex, or predicate)."
  ([page url] (page/wait-for-url page url))
  ([page url opts] (page/wait-for-url page url opts)))

(defn wait-for-load-state
  "Wait for `page` to reach a load state: `:load`, `:domcontentloaded`, or
  `:networkidle`."
  [page state]
  (page/wait-for-load-state page state))

;; ---------------------------------------------------------------------------
;; Locators
;; ---------------------------------------------------------------------------

(defn- as-locator
  "Coerce a target (Locator, selector string, or test-id keyword) to a Locator."
  [page target]
  (cond
    (instance? Locator target) target
    (keyword? target) (page/get-by-test-id page (name target))
    :else (page/locator page (str target))))

(defn- as-role
  "Coerce a role (AriaRole, keyword, or string) to an AriaRole."
  [role]
  (cond
    (instance? AriaRole role) role
    (keyword? role) (AriaRole/valueOf (str/upper-case (name role)))
    (string? role) (AriaRole/valueOf (str/upper-case role))
    :else role))

(defn locator
  "Create a locator for a CSS or text selector."
  [page selector]
  (page/locator page selector))

(defn get-by-text
  "Locate elements by their text content."
  [page text]
  (page/get-by-text page text))

(defn get-by-role
  "Locate elements by ARIA role.

  `role` is an `AriaRole`, a keyword, or a string (`:button`, `\"textbox\"`).
  `opts` may carry `:name`, `:exact`, `:checked`, `:level`, and so on."
  ([page role] (page/get-by-role page (as-role role)))
  ([page role opts] (page/get-by-role page (as-role role) opts)))

(defn get-by-label
  "Locate form controls by their label text."
  [page text]
  (page/get-by-label page text))

(defn get-by-placeholder
  "Locate inputs by their placeholder text."
  [page text]
  (page/get-by-placeholder page text))

(defn get-by-test-id
  "Locate elements by their test-id attribute."
  [page test-id]
  (page/get-by-test-id page test-id))

(defn all
  "All elements matching `target`, as individual locators."
  [page target]
  (loc/all (as-locator page target)))

(defn first-element
  "The first element matching `target`."
  [page target]
  (loc/first-element (as-locator page target)))

(defn nth-element
  "The zero-based `index`-th element matching `target`."
  [page target index]
  (loc/nth-element (as-locator page target) index))

(defn filter-locator
  "Narrow `target` by `opts` (`:has-text`, `:has`, `:has-not`, ...)."
  [page target opts]
  (loc/loc-filter (as-locator page target) opts))

;; ---------------------------------------------------------------------------
;; Interaction
;; ---------------------------------------------------------------------------

(defn click
  "Click the element at `target`."
  ([page target] (loc/click (as-locator page target)))
  ([page target opts] (loc/click (as-locator page target) opts)))

(defn dblclick
  "Double-click the element at `target`."
  ([page target] (loc/dblclick (as-locator page target)))
  ([page target opts] (loc/dblclick (as-locator page target) opts)))

(defn fill
  "Fill an input at `target` with `value`, replacing its content."
  ([page target value] (loc/fill (as-locator page target) value))
  ([page target value opts] (loc/fill (as-locator page target) value opts)))

(defn type-text
  "Type `text` into the element at `target`, character by character."
  ([page target text] (loc/type-text (as-locator page target) text))
  ([page target text opts] (loc/type-text (as-locator page target) text opts)))

(defn press
  "Press a key or combination (`\"Enter\"`, `\"Control+a\"`) on `target`."
  ([page target key] (loc/press (as-locator page target) key))
  ([page target key opts] (loc/press (as-locator page target) key opts)))

(defn clear
  "Clear the input at `target`."
  [page target]
  (loc/clear (as-locator page target)))

(defn check
  "Check the checkbox or radio button at `target`."
  ([page target] (loc/check (as-locator page target)))
  ([page target opts] (loc/check (as-locator page target) opts)))

(defn uncheck
  "Uncheck the checkbox at `target`."
  ([page target] (loc/uncheck (as-locator page target)))
  ([page target opts] (loc/uncheck (as-locator page target) opts)))

(defn hover
  "Move the mouse over the element at `target`."
  ([page target] (loc/hover (as-locator page target)))
  ([page target opts] (loc/hover (as-locator page target) opts)))

(defn focus
  "Focus the element at `target`."
  [page target]
  (loc/focus (as-locator page target)))

(defn select-option
  "Select `values` (a string or a vector of strings) in the select at `target`.

  Returns the selected values, or an anomaly map."
  [page target values]
  (loc/select-option (as-locator page target) values))

(defn scroll-into-view
  "Scroll the element at `target` into view."
  [page target]
  (loc/scroll-into-view (as-locator page target)))

(defn highlight
  "Briefly highlight the element at `target`, for debugging."
  [page target]
  (loc/highlight (as-locator page target)))

;; ---------------------------------------------------------------------------
;; Reading
;; ---------------------------------------------------------------------------

(defn text
  "The text content of the element at `target`.

  Use `inner-text` for rendered text with visible whitespace collapsed."
  [page target]
  (loc/text-content (as-locator page target)))

(defn inner-text
  "The rendered inner text of the element at `target`."
  [page target]
  (loc/inner-text (as-locator page target)))

(defn inner-html
  "The inner HTML of the element at `target`."
  [page target]
  (loc/inner-html (as-locator page target)))

(defn value
  "The input value of the element at `target`."
  [page target]
  (loc/input-value (as-locator page target)))

(defn attribute
  "The value of attribute `name` on the element at `target`, or nil."
  [page target name]
  (loc/get-attribute (as-locator page target) name))

(defn element-count
  "How many elements match `target`."
  [page target]
  (spel/safe (loc/count-elements (as-locator page target))))

(defn all-texts
  "The text content of every element matching `target`."
  [page target]
  (spel/safe (loc/all-text-contents (as-locator page target))))

(defn visible?
  "Whether the element at `target` is visible."
  [page target]
  (spel/safe (loc/is-visible? (as-locator page target))))

(defn enabled?
  "Whether the element at `target` is enabled."
  [page target]
  (spel/safe (loc/is-enabled? (as-locator page target))))

(defn checked?
  "Whether the element at `target` is checked."
  [page target]
  (spel/safe (loc/is-checked? (as-locator page target))))

(defn bounding-box
  "The bounding box of the element at `target`, or nil."
  [page target]
  (spel/safe (loc/bounding-box (as-locator page target))))

;; ---------------------------------------------------------------------------
;; Capture
;; ---------------------------------------------------------------------------

(defn screenshot
  "Take a screenshot of `page` and return the image bytes.

  Options include `:path` (also write the image to this file), `:full-page`,
  `:type` (`:png` or `:jpeg`), `:quality`, and `:clip`. Returns an anomaly map
  on failure."
  ([page] (page/screenshot page))
  ([page opts] (page/screenshot page opts)))

(defn pdf
  "Render `page` to PDF and return the bytes. Chromium only."
  ([page] (page/pdf page))
  ([page opts] (page/pdf page opts)))

(defn element-screenshot
  "Take a screenshot of the element at `target` and return the image bytes."
  ([page target] (loc/locator-screenshot (as-locator page target)))
  ([page target opts] (loc/locator-screenshot (as-locator page target) opts)))

;; ---------------------------------------------------------------------------
;; Diagnostics
;; ---------------------------------------------------------------------------

(defn on-console
  "Call `handler` with each `ConsoleMessage` on `page`."
  [page handler]
  (page/on-console page handler))

(defn on-page-error
  "Call `handler` with each uncaught page error string on `page`."
  [page handler]
  (page/on-page-error page handler))

(defn on-request-failed
  "Call `handler` with each `Request` that never produced a response."
  [page handler]
  (page/on-request-failed page handler))

(defn collect-console!
  "Register a console collector on `page` and return an atom of message maps.

  Each entry is `{:type :text :location}`, appended in arrival order."
  [page]
  (let [messages (atom [])]
    (page/on-console page
                     (fn [message]
                       (swap! messages conj {:type (page/console-type message)
                                             :text (page/console-text message)
                                             :location (page/console-location message)})))
    messages))

(defn collect-page-errors!
  "Register a page-error collector on `page` and return an atom of error strings."
  [page]
  (let [errors (atom [])]
    (page/on-page-error page (fn [error] (swap! errors conj (str error))))
    errors))
