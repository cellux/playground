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
| `src/omkamra/dsp/harness/testbed.cljs` | Browser differential suite; runs the in-page interpreter against JavaScript/Wasm kernels. |
| `src/omkamra/dsp/harness/testbed.clj` | On-demand HTTP endpoints: `source`, `wasm-source`, `wasm-metadata`, and `plan`. |
| `src/omkamra/dsp/harness/browser.clj` | Clojure-side browser driver (spel + `:on-demand-http`). |
| `src/omkamra/dsp/harness/fixtures.clj` | Shared `dsp/defn` fixtures and measuring helpers the tests use. |
| `src/omkamra/dsp/tests/suite.clj` | Requires every test namespace; `run-all` runs the language suite, `run-everything` runs it and the browser suite together. |
| `src/omkamra/dsp/synth.cljc` | The synth description and the pure-data runtime configuration (P10). |
| `src/omkamra/dsp/synth/pool.cljc` | Preallocated voices, allocation/stealing, the note lifecycle, block rendering. |
| `src/omkamra/dsp/hosts.clj` | The assembler: `synth-bundle`, `compile-synth`, and which kernel a target gets. |
| `src/omkamra/dsp/hosts/*.{clj,cljc,cljs}` | The kernels (`jvm`, `interpreter`, `js`) and the plumbing they share (`mixing`). |
| `src/omkamra/dsp/tests/synth.clj` | The synth runtime suite: description, pool, modes, lifecycle, allocation. |
| `src/omkamra/dsp/instrument.cljc` | The Clojure-level instrument graph: voice, polyphony, post-mix chain (D42). |
| `src/omkamra/dsp/hosts/instrument.clj` | The instrument lowering (value → program) and its live host. |
| `src/omkamra/dsp/tests/reference.clj` | The P11 reference instrument: matrix, stereo, post-mix chain, allocation. |
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

## Everything at once

The JVM suite and the browser suite are independent — one needs no browser, the
other no test image — so `run-everything` starts each in its own future and reports
once both are done:

```clojure
(require '[omkamra.dsp.tests.suite :as suite])
(suite/run-everything)
;; prints the JVM suite's own report under a header, then:
;;   === browser: 50/50 checks passed (73.3s total) ===
;;   === all green ===
;; => {:jvm {:test 239, :pass 2063, :fail 0, :error 0, :passed? true}
;;     :browser {:passed? true, :passed 50, :total 50, :failed []}
;;     :seconds 73.4, :passed? true}
```

Use it before a commit, or after a change that touches both a target and the
harness. The two halves overlap, so `:seconds` is the slower half's wall clock rather
than the sum; the JVM half's output is captured and printed after both finish, so the
two reports never interleave. `:timeout` (ms, default 30 minutes) bounds the wait for
*each* half — a half that never finishes appears as `{:timeout true}` rather than
hanging the call.

`:only` is passed to the browser half, so a single case can be checked there while
the whole JVM suite still runs:

```clojure
(suite/run-everything {:only ["call-int-led"]})
```

The tooling suite (`omkamra.dev.tools-test`) is deliberately *not* part of this: it
reloads namespaces, which would race the DSP tests running beside it.

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
;; => {:test 208, :pass 1840, :fail 0, :error 0}
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
- `omkamra.dsp.tests.harness` — the harness's own contracts, not the DSP language:
  how a page result is decoded off the wire, and what `case-results`, `coverage`
  and `parity` answer about one — including that the interpreter/JVM rows and the
  browser rows are read by the same two functions, which is what lets one table
  cover four targets. They run in the JVM suite because they are pure functions
  over a row map, needing no browser.

Expected baseline: **239 tests, 2063 assertions, 0 failures**.

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
;; => {:passed? true, :passed 50, :total 50, :results [...]}
```

Run one check while iterating — `:only` matches check names, and the result
contains only the selected checks:

```clojure
(browser/run! {:only ["instrument"]})
;; => runs just the two instrument hosts
```

It also matches **catalog case IDs**, which narrows the two shared matrices to those
cases — the fast loop for one semantic case's *behaviour* (its *shape* belongs in the
JVM suite, beside the emitter it concerns):

```clojure
(browser/run! {:only ["call-int-led"]})
;; => {:passed? true, :passed 2, :total 2, ...}  ; one case, two precisions
```

A needle that names neither a check nor a case is reported as a failure rather than
running nothing, because an empty run is indistinguishable from a passing one:

```clojure
(browser/run! {:only ["no-such-check"]})
;; => {:passed? false, :total 1,
;;     :results [{:name "selection", :passed? false,
;;                :detail "no check or case matched: [\"no-such-check\"]"}]}
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

### Reading a result

`run!` returns the page's own report: `{:passed? :passed :total :results}`, where
each result is one check with `:name`, `:passed?`, `:detail`, and — for the two
shared matrices — `:targets` and a per-case `:cases` breakdown. Five functions
answer the questions that breakdown invites, and `:views` attaches any of them to
the result in the same call:

```clojure
(browser/failures (browser/run!))
;; => []          ; the checks that did not pass, with their :detail (only `true`
;;                ; counts as passed, so a missing verdict is reported too)

(browser/run! {:views [:failures :parity]})
;; => {:passed? true :failures [] :parity {:cases 207 :gaps []} ...}

(browser/check-results result "post-mix")
;; => the checks whose name contains the needle, the way `:only` selects them

(browser/case-results result :region-buffers)
;; => [{:case :region-buffers :precision :f32 :javascript false :wasm true
;;      :passed? false} {... :f64 ...}]   ; every matrix row for one case

(browser/coverage result)
;; => {:gain {:f32 #{:js :wasm} :f64 #{:js :wasm}} ...}
;;    each precision maps to the targets that ran the case *and* matched

(browser/parity result)
;; => {:cases 207 :gaps []}
;;    the case/precision/target triples that missed a target their check declares
```

A target's boolean in a row means *ran it and matched the interpreter*, and a
case that never ran is simply absent — which is why these queries exist: a case
that passed and a case that ran nowhere produce the same green run, so a bite
check has to ask where a case went rather than whether the suite passed.

### All four targets in one call

The browser reports JavaScript and Wasm; the JVM suite's conformance test
compares the interpreter with the JVM one case at a time. `conformance/case-rows`
turns that second comparison into the *same kind of row*, so one table can cover
all four targets:

```clojure
(suite/parity)
;; => {:passed? true
;;     :cases 209   ; process and function cases, f32 and f64, across four targets
;;     :gaps []
;;     :coverage {:stereo-region-buffers {:f32 #{:interpreter :jvm :js :wasm}
;;                                        :f64 #{:interpreter :jvm :js :wasm}}
;;                ...}}
```

`:coverage` is `conformance/coverage` over the pooled rows and `:gaps` is
`conformance/parity`, so a target that mismatched — or a case that never reached
it — is an entry in `:gaps` rather than an absent line in a table. `:passed?` is
true only when the browser suite passed and no row has a gap; the JVM *test*
namespaces stay `run-all`'s business. With no argument `suite/parity` runs the
browser suite itself; pass a `browser/run!` result to reuse one that already ran.

The row vocabulary is the whole contract: `conformance/target-row-keys` says which
key each target's verdict lives under (`:js` → `:javascript`, and so on), and both
halves' rows are read by those two functions and no others.
`parity` is the whole-suite form of that question, and an empty `:gaps` on a run
whose `:only` selection named the check is the claim worth quoting.

The shared matrices declare their targets — `[:js :wasm]` for the process
matrix, `[:js]` for the function-case matrices, because Wasm is process-only — so
`parity` reads a declared target that a row does not reach as a gap. A check whose
name already says which target it is (`Wasm pool host`) declares none and
contributes no cases.

Case IDs are matched by name, with or without a colon or a namespace, because the
page sends a ClojureScript keyword as its *printed* form: `:gain` arrives as the
string `":gain"`, and `(keyword ":gain")` would build a keyword whose name
contains the colon — one that compares equal to nothing. `decode-result` is the wire
contract that strips it, and `omkamra.dsp.tests.harness` pins it: without the strip,
every lookup by case ID quietly found nothing.

`run!` options: `:headless` (default `true`), `:timeout` (ms, default
`60000`), `:server?` (start `:on-demand-http` if needed, default `true`),
`:autorun?` (default `false`), `:only` (see above), `:views` (see above; an
unknown view name throws rather than attaching nothing), and `:executable-path`
(explicit browser binary).
The first run also compiles the Shadow build and can take longer than later
runs; raise `:timeout` on a cold cache. One caveat from a full cache wipe: the very
first suite run afterwards can report `events did not reach the worklet` while the
worker and worklet modules are still being written, and passes in isolation and on
every run after it.

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
   on JavaScript and Wasm, mono and multi-channel alike (`:stereo-region-buffers`
   is the multi-channel one: a region holds one sample per channel per frame, so
   the channel is part of the index the target computes);
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
   over the same events (P10d), including a **fused** pool (`{:fusion {:slots
   S}}`, M6c) whose `slots` voices live in one process — checked across a steal
   and a release/free;
8. the **instrument hosts** (P11c) — the same two hosts driving the full
   reference synthesizer: its voice pool, modulation matrix (the portable
   `omkamra.dsp.reference` host), and post-mix bus (one instance per channel),
   checked for finiteness and block-size invariance. JavaScript is also compared
   sample-for-sample against the interpreter (exact, both use `Math`); Wasm is
   not, because its approximated `pow`/`sin` drift through the long resonant
   voice, so per-operation equivalence there is left to the shared process
   matrix. The **fused** instrument (F1, `{:fusion {:slots S}}` threaded by
   `omkamra.dsp.hosts.instrument`) is checked against the **plain instrument on
   the same host** instead: the page fetches `/fused-instrument-bundle` and
   `/fused-instrument-source`, rebuilds the fused voice kernel and the plain one
   over the same bus, and compares. That is what puts the fused host's kernel
   wiring under test without mixing in the target's transcendentals; it is also
   the check that caught the doubled ramp D63 records.
9. the **containment policies** (P10c) — `:reset`, `:sanitize`, and `:off`
   exercised on JavaScript and Wasm: `:reset` silences a non-finite block,
   resets the voice, and counts each event; `:sanitize` contains the sample
   without resetting and clamps a finite runaway to the ceiling; `:off` lets a
   non-finite sample reach the block.

10. the **worker → worklet event transport** (M1) — a real Web Worker
    (`omkamra.dsp.hosts.worker`, served from the separate `:omkamra.dsp.hosts.worker`
    build) posts timestamped events over a `MessageChannel` to an
    `OfflineAudioContext` worklet running the `follow-amount` kernel; the
    rendered block is a step function at the exact event frames;
11. **lane delivery** (M4) — a *shared* lane and a *per-voice* lane through the
    pool on both browser hosts, each compared against the interpreter pool **and**
    against literal numbers: a shared lane of 1,2,3,4 beside a scalar of 10 must
    render 11..14, and a shared 1,2,3,4 lane on one voice beside a 10 lane on the
    other must render 31..34. The literal half matters, because a differential
    check passes when both sides are equally wrong — which is exactly how the
    Wasm pool's missing lane *pointer* stayed invisible while this check compared
    two blocks of nothing (`array-seq` does not walk a JS typed array under
    Squint; the harness converts with `real->vector` instead). A lane also reaches
    a **callee** (F3, D64): the same matrix runs `:automation-lane-in-callee`,
    where the entry declares the lane and the definition that *reads* it is a
    module the entry calls.
12. **shared parameters** (M5) — a parameter bound to a `:shared` control, written
    once into the pool-wide region on both browser hosts, compared against the
    interpreter pool and against literal numbers (two voices of `0.5 + 1.0` must
    render `3.0`), with the declared default checked before the write.
13. **the post-mix chain's own regions** (H2, D67) — a post-mix stage that declares
    a lane, and one that reads a shared control, through the bus runner on both
    browser hosts, each compared against the in-page interpreter bus runner **and**
    against the curve itself. The fixture voice (`bus-gain-voice`) renders exactly
    `gate`, so the samples *are* the chain's gain: a kernel that never received the
    region renders zeros, which a purely differential check would miss when both
    sides are silent. The page fetches `/bus-lane-bundle`/`-source` and
    `/bus-shared-bundle`/`-source`.
14. **a fused pool whose slots have no state** (H4, D70) — the same voice
    (`stateless-voice`, `:guard false`) through a real fused pool on both browser
    hosts against the in-page interpreter pool, with literal samples (92 per
    frame). Its slots own *empty* call partitions, so this is the check that
    exercises the Wasm host's byte-range scan for a partition no layout segment
    overlaps: a stub there fails Wasm and leaves JavaScript passing.
    `/stateless-fused-bundle` and `/stateless-fused-source` serve it.
15. **a lane on a per-channel control** (H3, D68) — one voice taking three
    channels in turn through a real pool on both browser hosts, against the
    in-page interpreter pool and against literal numbers: each channel's curve,
    then a channel nobody wrote (zero), then a curve published while the voice is
    sounding. `:bend` is the pool's per-channel source, so its lane is a curve
    per channel and the pool keeps a copy — a stub of the delivery renders
    `[[1 2 3 4] [1 2 3 4] [1 2 3 4] [5 5 5 5]]`, the voice never following its
    channel. `/bend-lane-bundle` and `/bend-lane-source` serve the fixture.

The pool and instrument host checks are the ones that exercise the sequencing
layer rather than a single kernel. The page fetches a compiled voice —
`/synth-bundle` carries
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
  an `:input-samples` trace generator (with optional `:blocks`). The compiled
  plan carries the process's `:channels`, which is the one thing a runner needs to
  allocate both audio ports: a flat buffer for mono, one buffer per channel
  otherwise.
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
clj-kondo --lint src/omkamra/dsp src/omkamra/dsp.clj src/omkamra/dsp/tests
cljfmt check src/omkamra/dsp src/omkamra/dsp.clj src/omkamra/dsp/tests
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
  (require 'omkamra.dsp.tests.suite)
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
  server starts the watcher on the first compatible asset/page request. A run
  issued immediately after an edit can be served the *previous* bundle — the
  watcher rebuilds asynchronously — so give it a moment, or check the dev log for
  the `Build completed` line, before believing a surprising pass.
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
