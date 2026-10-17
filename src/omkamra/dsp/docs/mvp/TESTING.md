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
| `src/omkamra/dsp/harness/catalog.cljc` | Shared semantic catalog (pure data). Both testbeds read it. |
| `src/omkamra/dsp/harness/conformance.clj` | Builds target-neutral plans; JVM differential driver; plan serialization. |
| `src/omkamra/dsp/targets/interpreter.cljc` | Portable reference interpreter (the oracle), runs on JVM and in the browser. |
| `src/omkamra/dsp_test.clj` | Interpreter/JVM suite plus compiler-internal checks (descriptor, linker, ABI, allocation). |
| `src/omkamra/dsp/harness/testbed.cljs` | Browser differential suite; runs the in-page interpreter against JavaScript/Wasm kernels. |
| `src/omkamra/dsp/harness/testbed.clj` | On-demand HTTP endpoints: `source`, `wasm-source`, `wasm-metadata`, and `plan`. |
| `src/omkamra/dsp/harness/browser.clj` | Clojure-side browser driver (spel + `:on-demand-http`). |
| `src/omkamra/dsp/harness/fixtures.clj` | Shared `dsp/defn` fixtures and measuring helpers the tests use. |
| `src/omkamra/dsp/tests/suite.clj` | Requires every test namespace; `run-all` runs the whole language suite. |
| `src/omkamra/dsp/synth.cljc` | The synth description and the pure-data runtime configuration (P10). |
| `src/omkamra/dsp/synth/pool.cljc` | Preallocated voices, allocation/stealing, the note lifecycle, block rendering. |
| `src/omkamra/dsp/hosts.clj` | The assembler: `synth-bundle`, `compile-synth`, and which kernel a target gets. |
| `src/omkamra/dsp/hosts/*.{clj,cljc,cljs}` | The kernels (`jvm`, `interpreter`, `js`) and the plumbing they share (`mixing`). |
| `src/omkamra/dsp/tests/synth.clj` | The synth runtime suite: description, pool, modes, lifecycle, allocation. |
| `src/omkamra/dsp/examples/demo_synth.clj` | The example instrument and one possible host-side modulation wiring (D34). |
| `src/omkamra/dsp/targets/*.{clj,cljc}` | The four backends; `targets/capability.cljc` declares what each serves. |

The split follows `DECISIONS.md` D7/D13: **runtime semantics live once in
`harness/catalog.cljc`**, and the **interpreter is the single oracle** for every
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

The suite is split by subject under `omkamra.dsp.tests.*`; `omkamra.dsp.tests.suite` requires them all and is the
namespace to require when you want the whole language suite. It exercises the interpreter and the
JVM backend against the shared catalog and checks descriptor validation, linking,
IR lowering, the process ABI, precision variants, and JVM allocation behavior.

Evaluate in the dev nREPL:

```clojure
(require 'omkamra.dsp.tests.suite)
(omkamra.dsp.tests.suite/run-all)
;; => {:test 60, :pass 612, :fail 0, :error 0}
```

With `clojure_eval`, send that whole form as one expression.

To reload the compiler after an edit before re-running, reload the changed
namespaces explicitly — **avoid `:reload-all`**, which can reload
`clojure.core.async` and break the running Shadow server (see Troubleshooting):

```clojure
(require 'omkamra.dsp.ir 'omkamra.dsp.interpreter 'omkamra.dsp.jvm :reload)
(omkamra.dsp.tests.suite/run-all)
```

### A single test

```clojure
(clojure.test/test-vars
 [#'omkamra.dsp.tests.conformance/interpreter-oracle-conformance])
```

Notable groups:

- `interpreter-oracle-conformance` — runs every
  `acceptance/function-cases` entry against interpreter **and** JVM.
- `interpreter-oracle-conformance` — runs every
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
(require '[omkamra.dsp.harness.browser :as browser])
(browser/run!)
;; => {:passed? true, :passed 8, :total 8, :results [...]}
```

The browser suite is a **differential** test: it runs the portable
`.cljc` reference interpreter (compiled into the page) on the IR served by
`/omkamra/dsp/harness/testbed/plan`, then runs the generated JavaScript/Wasm kernel on
the same inputs and compares the two. No expected vectors are shipped from the
JVM.

`run!` starts `:on-demand-http` if needed (which starts the Shadow watcher on
the first page request), opens
`http://127.0.0.1:8080/omkamra/dsp/harness/testbed?autorun=0` in headless Chromium via
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

### The JavaScript target emits through Squint

The JavaScript backend does not build JavaScript text. It builds ClojureScript
*forms* (plain data) and hands them to `squint.compiler`, which owns the syntax —
statements, `let` ordering, precedence, and name mangling (D41). Two
consequences for tests:

- the exact spelling of a generated kernel is Squint's, so `tests/js.clj` pins
  the **contract** — the ES-module export shape, the physical type, and the rule
  that the module is standalone — and leaves behaviour to the browser suite;
- `every-catalog-case-is-a-standalone-javascript-module` compiles every catalog
  case to JavaScript and asserts that no source contains a `squint_core`
  reference or an import. That is the regression guard for the rule: a construct
  that lowered to a Squint standard-library call would break an AudioWorklet
  install, so it fails here rather than in the browser.

While working on the emitter, Node is the fast loop: compile a fixture to `:js`,
write `:source` to a `.mjs` file, and run it against the interpreter's output.
The browser suite is the authority.

### The interactive page

`browser/run!` uses a fresh headless browser. To watch a run, open
`http://127.0.0.1:8080/omkamra/dsp/harness/testbed` in a normal browser and click **Run
checks**; the results render in the page. Append `?autorun=0` to prevent the
automatic run.

### What the browser checks cover

1. the shared **process** matrix — every `acceptance/cases` entry, f32 and f64,
   on JavaScript and Wasm;
2. the shared **function-case** matrix — every `acceptance/function-cases`
   entry, f32 and f64, on JavaScript;
3. a WAT bounded-loop trap check;
4. a WAT check of the f32 sine **lookup-table** strategy (`:wasm-math :table`);
5. instance independence and schedule invariance, JavaScript and Wasm, f32 and
   f64;
6. the JavaScript AudioWorklet adapter (f32 and f64) and the Wasm adapter
   (f32 and f64);
7. the **pool hosts** — `omkamra.dsp.hosts.js` and `omkamra.dsp.hosts.wasm`
   driving `omkamra.dsp.synth.pool`, each compared against the interpreter pool
   over the same events (P10d).

The pool host checks are the only ones that exercise the sequencing layer rather
than a single kernel. The page fetches a compiled voice — `/synth-bundle` carries
its runtime configuration, ABI, and interpreter IR (and, for `?target=wasm`, the
module's WAT and linear-memory layout), and `/synth-source` its JavaScript module
— plays one note through a pool over each kernel, and compares the rendered block
and the released voice's fate. The two hosts differ in exactly one way: the
JavaScript kernel is handed the callback's arrays, while the Wasm kernel writes
into its own linear memory.

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

**How the DSP forms are linted.** `dsp/defn`, `dsp/fn`, and `dsp/defstruct`
contain embedded DSP data, not Clojure, so `:hooks` replace them with a plain
`def` and clj-kondo does not look inside. A namespace required *only* for
symbols used inside those bodies can therefore look unused; prefer
fully-qualified symbols, or rarely a local
`#_{:clj-kondo/ignore [:unused-namespace]}`. `dsp/def` is different: its value is
ordinary Clojure (the combinators), so it is `:lint-as clojure.core/def`, which
lints the expression and counts its aliases. Treating `dsp/defn`/`dsp/fn` as
`defn`/`fn` (`:lint-as` plus a form-scoped `:unresolved-symbol` exclusion) was
tried and rejected: it still reports `redundant do` and nested-`+` noise that the
DSP language legitimately uses, and a per-symbol exclusion list is not worth it.

### Future: a DSP-aware clj-kondo linter

The compromise above is fine for now; the ideal is a linter that understands DSP
forms. clj-kondo's extension point is an `analyze-call` hook that can call
`api/reg-finding!`. The linter would parse a `dsp/defn`/`dsp/fn` form (docstring,
parameters or spec, options, body), walk the body with the DSP's scopes, and
report DSP-level problems: an unknown builtin or wrong builtin arity, a call to
a symbol that is neither a local, language form, builtin, record, nor resolvable
var, and an unresolvable fully-qualified library reference. That is the check
that would catch `omkamra.dsp.library.filter/one-pol` while leaving `pow` and
`sample-rate` alone.

Three findings from an experiment make that a substantial piece, and they are
why it is postponed:

1. **Hooks cannot reuse our language definition.** A hook is loaded from
   `.clj-kondo`, not the project classpath (`(require '[omkamra.dsp.builtins])`
   fails with `Could not find namespace`). The linter would have to embed the
   builtin/operator tables, or have them generated into the hook file — the
   maintenance burden that made the `:lint-as` approach unattractive.
2. **There is no `mark-used`.** `clj-kondo.hooks-api` offers node construction,
   `sexpr`, `resolve`, `ns-analysis`, and `reg-finding!`, but no way to mark a
   namespace or var as used, and `api/resolve` does **not** do it as a side
   effect (verified: the unused-namespace warning persists). So an opaque,
   DSP-aware linter cannot fix the unused-namespace false positive that started
   this. Only a hook that hands the body back to clj-kondo's analyzer can, which
   is the approach rejected above.
3. **Delegating to Clojure analysis is noisy and can mis-type.** `:lint-as
   clojure.core/defn` marks usages but also emits `redundant-nested-call` and,
   worse, makes clj-kondo believe a definition var is a *function*, so unrelated
   `(:field definition)` uses fail `:type-mismatch`. A hook returning
   `(do (def name nil) (clojure.core/fn params body))` avoids the mis-typing and
   lints clean, but it is still "analyze as Clojure", which is not what we want.

The eventual shape: an `omkamra.dsp.lint` namespace in the project with the
language tables, referenced from the hook config, plus a separate decision on
usage tracking. Until then, `dsp/def` is linted as `def`,
`dsp/defn`/`dsp/fn`/`dsp/defstruct` are opaque, and a namespace used only inside
them gets a local ignore.

---

## CI

CI has no long-lived dev nREPL, so it must start a JVM. Keep this path for CI
only; for local work use the nREPL commands above.

```bash
# JVM / interpreter suite
clojure -M:dev -e "
  (require 'omkamra.dsp-test '[clojure.test :as t])
  (let [r (omkamra.dsp.tests.suite/run-all)]
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
  `http://127.0.0.1:8080/omkamra/dsp/harness/testbed` once.

## The synth runtime suite

`omkamra.dsp.tests.synth` tests the host-side runtime, not DSP code, so its oracle
is a pool running over the **reference interpreter**. The JVM pool's output must
equal the interpreter pool's output sample for sample, bit for bit, over a
sequence with two voices, a release, and a third note; mixing must equal the sum
of voices rendered alone; and rendering a block must be invariant under splitting
it into smaller blocks.

Properties that are stated rather than compared carry a negative twin, so none of
them can pass vacuously: the three stealing policies are checked on a note order
where they *disagree*; "a released voice is given up first" is checked where the
policy alone would pick a different voice; the block-size check verifies the
comparison is over real audio; and the allocation check measures the allocating
reference path to show its bound has teeth.

The **voice modes** (D33) are checked two ways, because most of their difference
is structural and one of them is not. State and held-note fallback are read
through `voice-snapshot`; the legato/retrigger difference is only audible, so it
is rendered — a retriggered voice starts its block from zero while a legato one
keeps sounding — and every mode is also rendered on the JVM *and* the reference
interpreter, so a fallback pitch change is exercised across the kernel seam.

The **host control channel and the example instrument** (D34) are checked the
same way: `set-voice-controls!`/`read-controls!` must reach the voice and refuse
a bad index or a destroyed pool, and the demo synth must sound and be changed by
each kind of modulation source (velocity, mod wheel, pitch bend), with the
interpreter host rendering the same samples. The **standard pitch-domain
sources** (D35) are read back directly: a note-on delivers the gate, note, bend
(scaled by `:bend-range`), and detune; a release closes the gate but keeps the
pitch; and a `:unison` pool delivers the spread as `:detune` cents.

Two things worth knowing before adding a check:

* **An allocation assertion must warm.** Thread-allocation numbers taken before
  the JIT has compiled the code — and especially right after a reload — read the
  compiler rather than the code, by up to 400x, and they look stable enough to
  believe. The suite warms 20 000 cycles before measuring.
* **The bound is set below the mistake, not at zero.** A whole cycle is held
  under 2 KB, deliberately far below the ~24 KB of a single reflective call
  (D26), so the class of regression it guards against cannot hide under it. What
  remains is measured and documented (D31), not hidden.
