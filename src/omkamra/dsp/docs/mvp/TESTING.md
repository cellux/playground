# DSP testing guide

How to run the `omkamra.dsp` tests on the JVM/interpreter and in the browser
(JavaScript and WebAssembly). For design and roadmap context see `PLAN.md`,
`PHASES.md`, and `DECISIONS.md`.

**Run everything through the project's running nREPL** — the `clojure_eval`
tool, a CIDER REPL, or any connected nREPL client. Do not start a fresh
`clojure -M ...` JVM per test run; that pays full classpath and compilation
cost every time and is dog slow. A dev server is started once, and every test
below evaluates in it.

## Where the tests live

| File | Role |
|---|---|
| `src/omkamra/dsp/acceptance.cljc` | Shared semantic catalog (pure data). Both testbeds read it. |
| `src/omkamra/dsp/conformance.clj` | Builds target-neutral plans; JVM differential driver; plan serialization. |
| `src/omkamra/dsp/interpreter.cljc` | Portable reference interpreter (the oracle), runs on JVM and in the browser. |
| `src/omkamra/dsp_test.clj` | Interpreter/JVM suite plus compiler-internal checks (descriptor, linker, ABI, allocation). |
| `src/omkamra/dsp/testbed.cljs` | Browser differential suite; runs the in-page interpreter against JavaScript/Wasm kernels. |
| `src/omkamra/dsp/testbed.clj` | On-demand HTTP endpoints: `source`, `wasm-source`, `wasm-metadata`, and `plan`. |
| `src/omkamra/dsp/browser.clj` | Clojure-side browser driver (spel + `:on-demand-http`). |

The split follows `DECISIONS.md` D7/D13: **runtime semantics live once in
`acceptance.cljc`**, and the **interpreter is the single oracle** for every
target; compiler-internal checks stay in the JVM suite because they are not
runtime semantics.

## Prerequisites

- The Clojure CLI and a JDK.
- A running dev server: `clojure_start_dev`, or
  `clojure -M:dev:linux -m nrepl.cmdline` started once. All test commands below
  are evaluated in that process.
- For the browser suite only: a Playwright browser install under
  `PLAYWRIGHT_BROWSERS_PATH` (in this sandbox `/ms-playwright`, already set)
  and port `8080` free for `:on-demand-http`.

The browser driver finds the installed `chromium_headless_shell-*` itself
because spel's bundled Playwright may pin a different browser revision; override
with the `:executable-path` option if needed.

---

## JVM / interpreter tests

The suite namespace is `omkamra.dsp-test`. It exercises the interpreter and the
JVM backend against the shared catalog and checks descriptor validation, linking,
IR lowering, the process ABI, precision variants, and JVM allocation behavior.

Evaluate in the dev nREPL:

```clojure
(require '[clojure.test :as t] 'omkamra.dsp-test)
(t/run-tests 'omkamra.dsp-test)
;; => {:test 60, :pass 612, :fail 0, :error 0}
```

With `clojure_eval`, send that whole form as one expression.

To reload the compiler after an edit before re-running, reload the changed
namespaces explicitly — **avoid `:reload-all`**, which can reload
`clojure.core.async` and break the running Shadow server (see Troubleshooting):

```clojure
(require 'omkamra.dsp.ir 'omkamra.dsp.interpreter 'omkamra.dsp.jvm :reload)
(t/run-tests 'omkamra.dsp-test)
```

### A single test

```clojure
(clojure.test/test-vars
 [(resolve 'omkamra.dsp-test/shared-function-cases-match-interpreter-and-jvm)])
```

Notable groups:

- `shared-function-cases-match-interpreter-and-jvm` — runs every
  `acceptance/function-cases` entry against interpreter **and** JVM.
- `acceptance-cases-match-interpreter-and-jvm` — runs every
  `acceptance/cases` process case against interpreter and JVM.
- `bitwise-operators-are-emitted-by-js-and-wasm` — checks emitted operator text.
- `jvm-unchecked-process-adapter-does-not-allocate-after-warmup` — allocation
  regression guard for the real-time process path.

Expected baseline: **60 tests, 612 assertions, 0 failures**.

---

## Browser tests (JavaScript and WebAssembly)

The browser suite is served by the `:on-demand-http` system as the
`omkamra.dsp.testbed` Shadow page. The page installs a JS hook,
`window.omkamraDspTestbed.runConformance`, which runs the complete suite and
resolves a structured result.

Evaluate in the dev nREPL:

```clojure
(require '[omkamra.dsp.browser :as browser])
(browser/run!)
;; => {:passed? true, :passed 8, :total 8, :results [...]}
```

The browser suite is a **differential** test: it runs the portable
`.cljc` reference interpreter (compiled into the page) on the IR served by
`/omkamra/dsp/testbed/plan`, then runs the generated JavaScript/Wasm kernel on
the same inputs and compares the two. No expected vectors are shipped from the
JVM.

`run!` starts `:on-demand-http` if needed (which starts the Shadow watcher on
the first page request), opens
`http://127.0.0.1:8080/omkamra/dsp/testbed?autorun=0` in headless Chromium via
spel, waits for the hook, and evaluates `runConformance()`. The `?autorun=0`
query suppresses the page's own automatic run so the programmatic call is the
only run. `run!` starts and stops its own browser; no connected runtime or
`cljs-eval` session is required.

Inspect failures:

```clojure
(into {}
      (comp (remove :passed?)
            (map (juxt :name :detail)))
      (:results (browser/run!)))
```

Each result entry also carries the target and per-case breakdown. For example,
the process matrix lists which of JavaScript and Wasm passed each case and
precision, and the function-case matrices report `:targets [:js]` because Wasm
is process-only:

```clojure
(->> (browser/run!)
     :results
     (filter #(= "shared process matrix f32" (:name %)))
     first
     ((juxt :targets :cases)))
;; => [[:js :wasm]
;;     [{:case :gain     :precision :f32 :javascript true :wasm true}
;;      {:case :one-pole :precision :f32 :javascript true :wasm true}
;;      ...]]
```

`run!` options: `:headless` (default `true`), `:timeout` (ms, default
`60000`), `:server?` (start `:on-demand-http` if needed, default `true`),
`:autorun?` (default `false`), and `:executable-path` (explicit browser binary).
The first run also compiles the Shadow build and can take longer than later
runs; raise `:timeout` on a cold cache.

### The interactive page

`browser/run!` uses a fresh headless browser. To watch a run, open
`http://127.0.0.1:8080/omkamra/dsp/testbed` in a normal browser and click **Run
checks**; the results render in the page. Append `?autorun=0` to prevent the
automatic run.

### What the 8 checks cover

1. the shared **process** matrix — every `acceptance/cases` entry, f32 and f64,
   on JavaScript and Wasm;
2. the shared **function-case** matrix — every `acceptance/function-cases`
   entry, f32 and f64, on JavaScript;
3. a WAT bounded-loop trap check;
4. the Squint JavaScript AudioWorklet adapters (f32 and f64);
5. the Wasm AudioWorklet adapter (f32).

Wasm is process-only, so function-entry cases run on the interpreter, JVM, and
JavaScript; Wasm coverage for the same operators comes from process cases such
as `:bitwise-lfsr` and the P2 builtin cases.

### Driving `cljs-eval` directly

If you want to evaluate arbitrary ClojureScript in a connected page rather than
run the whole suite, keep the testbed page open and use
`omkamra.dev.frontend/cljs-eval` (see that namespace's docstring). The browser
driver above does not need a connected runtime; it launches its own browser.

---

## The shared catalog

`omkamra.dsp.acceptance` is **pure data** — there are no `:step` or `:reference`
functions any more. It has two vectors:

- `process-cases` (alias `cases`) — block-processing kernels. Each entry
  carries a `:definition`, `:entry :process`, `:precisions`, `:controls`, and
  an `:input-samples` trace generator (with optional `:blocks`).
- `function-cases` — scalar functions. Each entry carries a `:definition`,
  `:entry :function`, `:precisions`, and `:vectors` of argument lists.

`all-cases` is both concatenated; `case-by-id` searches all of them;
`function-case?` distinguishes the two.

`omkamra.dsp.conformance` turns a case + precision into a **plan** (the compiled
portable IR plus controls/vectors plus an invocation sequence including reset
and special values). `conformance/run-plan` runs a plan on a target artifact.
The interpreter computes expected outputs; the JVM backend and the browser's
JavaScript/Wasm kernels are compared against it.

**To add a shared test**, add data to the appropriate vector. It is
automatically run by the JVM suite and by the browser matrix. Keep input
generators in portable `.cljc` and avoid target-divergent special values unless
the case is specifically about them.

Use the JVM suite for compiler-internal checks (descriptor errors, linker
diagnostics, ABI shape, allocation) and `testbed.cljs` for browser-only checks
(adapter protocol, page behavior).

---

## Linting and formatting

```bash
clj-kondo --lint src/omkamra/dsp src/omkamra/dsp.clj src/omkamra/dsp_test.clj
cljfmt check src/omkamra/dsp src/omkamra/dsp.clj src/omkamra/dsp_test.clj
```

Both should report no errors. `.clj-kondo/config.edn` mirrors spel's
`with-*` `:lint-as` entries so the browser driver lints cleanly.

---

## CI

CI has no long-lived dev nREPL, so it must start a JVM. Keep this path for CI
only; for local work use the nREPL commands above.

```bash
# JVM / interpreter suite
clojure -M:dev -e "
  (require 'omkamra.dsp-test '[clojure.test :as t])
  (let [r (t/run-tests 'omkamra.dsp-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ (long (:fail r)) (long (:error r)))) 0 1)))"

# Browser suite (nothing else may own port 8080)
clojure -M:dev:linux -e "
  (require '[omkamra.dsp.browser :as b])
  (let [r (b/run! {:timeout 180000})]
    (println (select-keys r [:passed? :passed :total]))
    (shutdown-agents)
    (System/exit (if (:passed? r) 0 1)))"
```

---

## Troubleshooting

- **Port 8080 already in use:** a dev server already owns `:on-demand-http`.
  Just evaluate `browser/run!` there instead of starting another.
- **`Executable doesn't exist at .../chromium_headless_shell-<rev>`:** spel's
  bundled Playwright expects a browser revision that is not installed.
  `omkamra.dsp.browser/installed-headless-shell` picks the installed revision
  automatically; if the layout differs, pass `:executable-path` explicitly.
- **The browser build does not pick up an edit:** recompile synchronously with
  `(omkamra.dev.frontend/recompile! :omkamra.dsp.testbed)`. The on-demand HTTP
  server starts the watcher on the first compatible asset/page request.
- **`No implementation of method: :put! ... ManyToManyChannel` after a
  `:reload-all`:** reloading every transitive dependency can reload
  `clojure.core.async` and break the running Shadow server. Reload only the
  namespaces you changed (`:reload`), and restart the dev process if it happens.
- **First browser run times out:** it also compiles the Shadow build; raise
  `:timeout` or warm the build by loading
  `http://127.0.0.1:8080/omkamra/dsp/testbed` once.
