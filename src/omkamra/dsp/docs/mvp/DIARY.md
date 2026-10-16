# Implementation diary

This file is an append-only progress log for implementation. The implementing agent must add one entry for each completed phase and must not edit earlier entries.

The final entry, **Handoff — current state**, is a rolling entry rather than a
completed-phase log: it records what exists, how it fits together, what is
open, and what to do next, and may be rewritten in place.

---

## P0 — Baseline (recorded before P1)

The pre-existing system was recorded as P0 in `PHASES.md`: typed portable IR,
the reference interpreter, JVM/JS/Wasm backends, scalar and fixed-size typed
state, the process ABI, and the shared `acceptance.cljc` process catalog.

---

## P1 — Integer bitwise and shift operations (completed)

**Delivered.**

- Portable IR: a single `bitwise-operators` signature table, `:bitwise` nodes
  with `:args`, lowering via `lower-bitwise`, and updated `expression-operators`
  and call inlining/substitution walkers.
- Reference interpreter: explicit 32-bit two's-complement semantics. Shift
  distances are masked to five bits and results narrowed with `unchecked-int`;
  logical right shift reinterprets the operand as unsigned 32-bit before
  shifting. This is the documented reference because Clojure's boxed `bit-shift-*`
  operate on 64-bit longs and mask modulo 64.
- JVM: `iand`/`ior`/`ixor`, `iconst-m1`+`ixor` for `bit-not`, `ishl`/`ishr`/`iushr`.
- JavaScript: `&`, `|`, `^`, `~`, `<<`, `>>`, `>>>`, each coerced with `| 0` so
  the result is a signed 32-bit value.
- Wasm: `i32.and`/`i32.or`/`i32.xor`, `i32.shl`, `i32.shr_s`, `i32.shr_u`, with
  `i32.xor` by `-1` for `bit-not`.
- Linker `language-forms` and descriptor `expression-type` recognize the seven
  operators.
- Shared `acceptance.cljc`:
  - process case `:bitwise-lfsr`, an integer recurrence exercising all seven
    operators across interpreter, JVM, JS, and Wasm in both precisions;
  - function-entry cases for the seven operators plus float arithmetic,
    comparisons, boolean logic, `while`/`break`/`continue`, and conversions.
- `omkamra.dsp.browser`, a spel-based Clojure driver that starts
  `:on-demand-http`, opens `omkamra.dsp.testbed` with `?autorun=0`, and calls the
  page's `runConformance` hook.
- `omkamra.dsp.testbed/run-conformance!`, a programmatic browser entry point
  returning a structured `clj->js` result; the testbed now also runs the shared
  function-case matrix on JavaScript.

**Verified.**

- Clojure/JVM suite: 60 tests, 612 assertions, 0 failures.
- Browser conformance through `omkamra.dsp.browser/run!`: 15/15 checks, covering
  the process acceptance matrix (f32/f64 JS and Wasm), the JS function-case
  matrix (f32/f64), bounded-loop Wasm traps, generated modules, and all four
  AudioWorklet adapters.
- Direct Node runs of the emitted JavaScript and of WABT-compiled Wasm matched
  the interpreter bit-for-bit on the LFSR and on a bitwise-process fixture.

**Incidental fixes.**

- JVM functions with `:boolean` parameters boxed `Long` instead of `Integer`,
  so any reflected invocation of such a function failed. Exercising a boolean
  parameter through the shared catalog exposed and fixed this.
- The shared `ushr32` reference returned an unsigned JavaScript number; the DSP
  `:int` result is the signed 32-bit reinterpretation, so the reference now
  narrows with `| 0`.

**Deferred.** The Wasm backend remains process-only, so function-entry cases
run on the interpreter, JVM, and JavaScript. Wasm coverage for the same
operators is provided by the `:bitwise-lfsr` process case.

---

## P1 follow-up — per-case target breakdown in the browser result

The browser conformance result now makes the JavaScript/Wasm split visible
instead of reporting only an aggregate pass/fail. The shared matrices carry
`:targets` and a per-case `:cases` vector, for example
`{:case :gain :precision :f32 :javascript true :wasm true}` for the process
matrix, and `:targets [:js]` with JavaScript-only entries for the function
matrix. `omkamra.dsp.browser` keywordizes the keys and the enumerated values
(`:case`, `:precision`, `:targets`) so the result is idiomatic Clojure.

`TESTING.md` documents how to read the breakdown. Verified through
`clojure_eval` against the running nREPL: browser 15/15, JVM 60 tests / 612
assertions.

---

## P2 — Portable scalar math builtins (completed)

**Delivered.** The capability-aware `omkamra.dsp.builtins` registry and a
generic `:builtin` IR node; primitive lowerings on interpreter/JVM/JS/Wasm via
multimethods; derived `clamp`/`fract`/`sign`/`round`/`mod` expanded with
per-argument temporaries; the D9–D12 semantics; and D12 total wrapping integer
`/`/`rem`/`mod` across all targets.

**First library entries.** `omkamra.dsp.library` (`.cljc`) is the seed of the
P7 standard library: `clip` and `triangular-window`, written in the DSP
language with `dsp/defn` and composed from `min`/`max`/`abs`, plus the catalog
cases `:library-clip` and `:library-triangular-window` that call them through
the ordinary call mechanism on every target. The namespace is `.cljc` on
purpose: the JVM compiler expands `dsp/defn` today, and an SCI-hosted browser
compiler will expand the same source forms once the front end is portable
(P13). It was verified to compile under ClojureScript as well, with the
correct captured `:namespace`.

**Conformance rewrite.** `omkamra.dsp.acceptance` is now pure data, and
`omkamra.dsp.conformance` builds target-neutral plans. The reference
interpreter became portable (`interpreter.cljc`) and now runs in the browser,
so the browser computes expected outputs from the served IR instead of trusting
vectors shipped from the JVM. `omkamra.dsp.browser` drives the page with spel
and `:on-demand-http`; `omkamra.dsp.testbed` serves a `plan` endpoint.

**Verified.** JVM 59 tests / 445 assertions; browser 8/8 (27 process cases ×
f32/f64 on JS and Wasm, 13/3 function cases on JS, bounded-loop trap, three
AudioWorklet adapters).

**Negative zero, resolved.** The browser oracle briefly had to treat `+0`/`-0`
as equal because ClojureScript's `PersistentArrayMap.-assoc` short-circuits on
`identical?` (`===`), and JavaScript says `0 === -0`, so `assign`-ing `-0.0`
over a `+0.0` local kept the old sign. Locals now go through `set-local!`,
which drops the key first on ClojureScript to force a genuine insertion. The
`:sign` case regression-tests it, and the browser comparison is strict again.

**Open.** The capability check is still only implicit: a backend's
`builtin-instructions`/`emit-builtin` `:default` throws a structured error for
an unimplemented `[operator type]`. Every P2 builtin is supported on every
target, so nothing exercises a richer capability model yet; P3 is where the
first genuine gap (Wasm transcendentals) appears. Separately, the Wasm f64
AudioWorklet check was dropped in the rewrite, and function-entry cases remain
JavaScript-only. See the handoff entry below for detail and next steps.

---

## Handoff — current state after P2 (rolling entry)

This is the one living entry, as opposed to the completed-phase entries above:
what exists, how it fits together, what is open, and what to do next. It may be
rewritten in place. Read with `PLAN.md`, `PHASES.md`, and `DECISIONS.md`.

### Where things stand

- **P1 (integer bitwise/shift) is complete.** The seven operators exist across
  interpreter/JVM/JS/Wasm per `DECISIONS.md` D3.
- **P2 (portable scalar math builtins) is complete**, including the
  capability-aware registry, the D9–D12 semantics, and the first standard
  library entries (`omkamra.dsp.library`).
- **The conformance harness is interpreter-oracle-driven** (D7/D13): the shared
  catalog is pure data, and the reference interpreter is the single oracle on
  the JVM *and* in the browser.
- **North star:** users eventually edit DSP definitions in the browser
  (CodeMirror) and compile them there, which means the compiler itself becomes
  portable and runs in SCI (P13). The library and every new language form are
  therefore authored as portable `.cljc` source forms.
- Verified through `clojure_eval` on the running nREPL: JVM 59 tests / 445
  assertions, browser 8/8, `clj-kondo`/`cljfmt` clean. The git working tree
  holds this uncommitted.

### Architecture

```text
omkamra.dsp.acceptance        (.cljc)  data-only conformance catalog
omkamra.dsp.conformance       (.clj)   plan building, JVM differential driver
omkamra.dsp.interpreter       (.cljc)  portable reference interpreter (the oracle)
omkamra.dsp.library           (.cljc)  first DSP standard-library entries (seed of P7)
omkamra.dsp.builtins          (.clj)   builtin registry (signatures/effects/expansion)
omkamra.dsp.browser           (.clj)   spel/on-demand-http browser driver
omkamra.dsp.testbed           (.clj)   HTTP endpoints: source, wat, metadata, plan
omkamra.dsp.testbed           (.cljs)  browser differential suite
```

### Interpreter-oracle conformance

- A **plan** (`conformance/plan`) is target-neutral data: the compiled portable
  IR, controls or function vectors, and the invocation sequence (blocks, reset,
  special values). It contains **no expected outputs**.
- **JVM:** the interpreter computes expected outputs; the JVM backend is
  compared against it (`interpreter-oracle-conformance` in `dsp_test.clj`).
- **Browser:** `/omkamra/dsp/testbed/plan` serves the plan; the page reads the
  EDN IR with `cljs.reader`, runs the bundled `.cljc` interpreter to compute
  expected, runs the generated JavaScript/Wasm kernel, and compares.
- Adding a test is now **data-only**: append to `acceptance/process-cases` or
  `acceptance/function-cases`; both suites pick it up automatically.
- Comparison is exact, NaN-equal-as-NaN, and signed-zero-aware on both the JVM
  and the browser.

### P2 builtins

- Registry: `omkamra.dsp.builtins` (data + small pure fns, ready to become
  `.cljc`). Each entry has `:operator`, `:kind` (`:primitive`/`:derived`),
  `:signatures` overloads, `:effects`, and `:expand` for derived ops.
- IR: primitives lower to a generic `{:op :builtin :builtin ... :args [...]}`
  node; derived ops expand during lowering, binding each argument to a fresh
  local so effectful arguments are evaluated once.
- Backends use multimethods dispatched on `[operator type]`
  (`builtin-instructions`, `emit-builtin`).
- Contract: D9 (`min`/`max` NaN-propagating, signed-zero-aware), D10 (`round`
  half-away, derived), D11 (small primitive set), D12 (integer `/`, `rem`,
  `mod` total and wrapping), D13 (registry + `:builtin` node).

### Standard library

- `omkamra.dsp.library` is the `.cljc` seed of the P7 library. `clip` and
  `triangular-window` are written with `dsp/defn` and compose builtins only.
- The library is `:clj`-required by `acceptance.cljc` so the JVM linker can
  resolve the calls; the browser only receives compiled IR and never loads it.
- Fully-qualified symbols (`omkamra.dsp.library/clip`) are used in catalog
  bodies so no alias/`:resolution` plumbing is needed in hand-built data.
- It is deliberately one namespace for now; P7 will decide the sub-namespace
  seams (`omkamra.dsp.library.<category>`).

### Open issues

1. **Capability diagnostics are implicit.** There is no registry-level,
   pre-flight capability check; an unimplemented `[operator type]` fails in the
   backend's `:default` method at emission time. P2's builtins are supported
   everywhere, so nothing exercises a richer model yet. P3 (Wasm has no
   `sin`/`cos`/`exp`/`log`) is where this should be made explicit.
2. **Reduced browser coverage from the rewrite.** The old suite had separate
   generated-kernel and Wasm-module checks plus a Wasm f64 AudioWorklet check;
   those were folded into the shared matrix or dropped. The Wasm **f64**
   AudioWorklet adapter is no longer exercised. Re-add if desired.
3. **Function-entry cases are JS-only.** Wasm is process-only by design, so the
   function matrix runs on interpreter/JVM/JS; Wasm coverage for the same
   operators comes from process cases.

### Gotchas burned into this session

- In Clojure/CLJS source, `-0` is **integer zero**; negative zero is `-0.0`.
- Clojure's `(float x)` **throws** on infinity; use `unchecked-float` (JVM) and
  `unchecked-float`/`Math.fround` (CLJS).
- ClojureScript `unchecked-remainder-int` is **floored** (it uses `mod`); use
  `rem` plus explicit `int32`. `unchecked-divide-int` may not narrow
  `INT_MIN / -1`; wrap with `int32`.
- Do not name a parameter `case` — it shadows the `case` special form in CLJS.
- **ClojureScript's map `assoc` short-circuits on `identical?`/`===`**, so it
  loses a `+0.0` → `-0.0` (or reverse) value change because `0 === -0` in JS.
  Force a fresh insertion (`dissoc` first) whenever signed zero matters.
- Avoid `:reload-all` in the dev JVM (it reloads `clojure.core.async` and
  breaks the running Shadow server). To refresh `omkamra.dsp-test`, use
  `(remove-ns 'omkamra.dsp-test)` then `(require 'omkamra.dsp-test :reload)`,
  because `:reload` alone leaves removed `deftest` vars behind.
- `spel`'s bundled Playwright wants a different Chromium revision than the
  installed one; `omkamra.dsp.browser/installed-headless-shell` finds the
  installed headless shell and passes it as `:executable-path`.
- The Shadow watcher sometimes reports `0 compiled`; call
  `omkamra.dev.frontend/recompile!` and re-check. Dev assets are served with
  `cache-control: no-store` now.

### Next steps

1. Begin **P3 (transcendental math intrinsics)**. Important design input:
   **Wasm has no `sin`/`cos`/`exp`/`log`/`pow`**, so those must either be
   emitted as portable sequences on Wasm or reported unsupported; the D13
   registry already models both outcomes. `sqrt` is implemented and conformance
   uses a guard for f32 double-rounding (Wasm computes
   `f32.demote_f64(f64.sqrt(f64.promote_f32(x)))` to match Java/JS).
2. When P3 lands, decide where the capability model lives (registry metadata
   vs backend `:default`) and record it in `DECISIONS.md`.
3. Optionally restore the dropped Wasm f64 worklet and generated-kernel checks.

### How to run everything

See `TESTING.md`. Short version, through `clojure_eval`:

```clojure
;; JVM
(require '[clojure.test :as t] 'omkamra.dsp-test)
(t/run-tests 'omkamra.dsp-test)

;; Browser (JS + Wasm)
(require '[omkamra.dsp.browser :as browser])
(browser/run!)
```
