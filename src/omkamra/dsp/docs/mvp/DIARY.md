# Implementation diary

This file is an append-only progress log for implementation. The implementing agent must add one entry for each completed phase and must not edit earlier entries.

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
