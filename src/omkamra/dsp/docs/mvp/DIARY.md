# Implementation diary

An append-only record of the work: one entry per phase or slice, each dated with the
day it was recorded. An entry is never rewritten — it says what was true when it was
written, including names that have since changed. Several entries can share a date;
the order of the file is the order of the work.

The closing **Handoff — current state** is the one exception: a rolling entry,
rewritten in place so that it is always current, and always last.

`README.md` maps this file among the others and states the conventions.

---

## 2026-10-16 — P0 — Baseline (recorded before P1)

The pre-existing system was recorded as P0 in `PHASES.md`: typed portable IR,
the reference interpreter, JVM/JS/Wasm backends, scalar and fixed-size typed
state, the process ABI, and the shared `acceptance.cljc` process catalog.

---

## 2026-10-16 — P1 — Integer bitwise and shift operations (completed)

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

## 2026-10-16 — P1 follow-up — per-case target breakdown in the browser result

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

## 2026-10-16 — P2 — Portable scalar math builtins (completed)

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

## 2026-10-16 — P3 — Transcendental math intrinsics (completed)

**Decision.** `DECISIONS.md` D14: compile to a native intrinsic wherever one
exists, emulate where it does not, and put the resulting deviation in a
**target-dispatched matcher** rather than in the oracle. The interpreter stays
the single oracle and uses the host `Math`, so it counts as a native target;
Wasm, whose core has no transcendental instructions, gets an f64-internal
emulation emitted as WAT. The matcher is exact by default and tolerant only on
`:wasm`.

**Landed (first slice: `exp`, `sin`, `cos`).**

- `exp`, `sin`, `cos` are primitive builtins (`omkamra.dsp.builtins`).
- Native lowerings: the interpreter uses `Math`/`js/Math`, the JVM `Math`
  through the existing `double-transcendental-instructions` helper (f32
  promotes to f64 and narrows), JavaScript `Math`.
- Wasm emulations (f64, fixed size): `exp` reduces by `k = nearest(x*log2e)`
  and builds `2^k` from the f64 exponent field by bit construction; `sin`/`cos`
  share one quadrant reduction (`n = nearest(x*2/pi)`, `|r| <= pi/4`, Taylor
  polynomials, quadrant `n mod 4` selects and negations). `\u00b1inf` and NaN
  cannot reach the i64 conversion, so they are replaced and their results
  forced to NaN with `select`.
- Emulated builtins are emitted once as shared f64→f64 Wasm functions
  (`$dsp_sin`, `$dsp_cos`, `$dsp_exp`) and `call`ed from each use site, so
  module size no longer multiplies with call sites: `sin`/`cos` went from
  ~78 KB to ~19.5 KB and `exp` from ~14.6 KB to ~5.2 KB, and each extra `sin`
  call site now costs ~93 bytes instead of re-emitting the whole expression.
- Compiler option `:wasm-math` selects the Wasm emulation strategy:
  `:polynomial` (default) or `:table`.
- Wasm lookup table (`:table`, f32 only): one shared 4097-entry f32 sine table
  in a `(data ...)` segment, read with linear interpolation, used by
  `$dsp_sin` and `$dsp_cos` (`cos` via a π/2 phase shift). f64 keeps the
  polynomial because a table accurate enough for f64 would be far larger than
  the code it replaces. The table WAT text is ~53 KB (16 KB of payload) but a
  lookup is a handful of ops against a degree-13 polynomial.
- Matcher: `conformance/scalar-match?` and the browser `scalar-match?` dispatch
  on target; `:wasm` allows `1e-6` (f32) / `1e-9` (f64) relative tolerance,
  and every other target stays exact.
- Catalog cases `:exp`, `:sin`, `:cos`, so the shared process matrix is now 30
  cases.
- A browser check, **Wasm sin lookup table f32**, renders the `:table` kernel
  and compares it to the interpreter within tolerance.

**P3 done-when met.** `omkamra.dsp.library` now defines a `sine-oscillator`
(phase accumulator wrapping at 1.0) and an `exponential-adsr` (per-sample
one-pole segments over attack/decay/sustain/release, with threshold-based
stage changes because an exponential approach only reaches its target in the
limit), written only in DSP source. The catalog has `:sine-oscillator` and
`:exponential-adsr` cases; a case may name a user-facing library Var with
`:definition-var` instead of inlining a descriptor, and the JVM side resolves
it when building the plan. Both render within tolerance on
interpreter/JVM/JS/Wasm for f32 and f64.

**Cross-cutting fix: constant precision.** Float literals were narrowed to a
Clojure `Float` during IR lowering regardless of the artifact's precision.
That lost f64 precision, and because a `Float` prints a short decimal it did
not round-trip through the EDN served to the browser: Java widens `Float`
`6.2831855` to `6.283185482025146` while JavaScript reading the EDN number
`6.2831855` gets `6.2831855`. The oscillator's `2*pi` constant exposed it as a
browser-only f32/f64 mismatch. Literals now stay `double` in the IR and are
narrowed per precision at use: the interpreter's `:const`, the JVM
`(float ...)`, the JS `Math.fround`, and the Wasm `<f32/f64>.const`.

**Full operation set.** The other ten landed: `tan`/`sinh`/`cosh`/`tanh`
derive from the shared `sin`/`cos`/`exp` functions; `log` (mantissa/exponent
split by bit reinterpretation, then `log(m) = 2*atanh((m-1)/(m+1))`) and
`atan` (four argument halvings then a Taylor series) are the two new accurate
building blocks; `asin`/`acos`/`atan2`/`pow` derive from those. Emulation
bodies may now carry WAT locals and statements, which is also what keeps them
bounded: before that, `atan`'s halving loop duplicated its argument three
ways per step and produced 430 KB of WAT; a scratch local brought it to 3 KB.

**Verified.** JVM 59 tests / 475 assertions; browser 9/9, with all thirteen
P3 operations, plus `:sine-oscillator` and `:exponential-adsr`, passing on
JavaScript and Wasm at f32 and f64, and the table strategy exercised.

**Trap avoided.** The first emulation computed `exp(x/2^20)` and then squared
it twenty times by nesting `(f64.mul p p)`. Nesting duplicates the expression
tree, so the WAT grew as 2^20 copies of the polynomial and killed the dev
server with an out-of-memory. `exp` now uses exponent bit construction and
stays fixed-size; every future emulation must do the same.

**Deferred.** The `:wasm-math :table` lookup-table lowering is still an
optimization with a pending benchmark (see the TODO below); it does not block
the phase.

---

## 2026-10-16 — P4 — Local arrays, constant tables, aggregate state (completed)

P4 extends the language from scalar locals and flat state arrays to fixed-size
memory for delay lines, wavetables, and structured filter state. It is being
delivered as vertical slices.

### Slice 1 (P4a) — local arrays and the instance scratch arena

**Surface.** `(array :float N)` / `(array :int N)` declares a fixed-size local
array as a `let` binding. Indexed access is `(aget a i)` / `(aset a i v)`, with
`array-load` / `array-store` kept as equivalent aliases (Clojure-core names per
D3). An array name is not a scalar value; using it directly is a diagnostic.

**One arena.** A real-time kernel cannot allocate per call, so a local array is
not created inside `process()`; it is **preallocated scratch**. Scratch slots
get distinct logical offsets that continue immediately after persistent state,
and the arena is realized in each backend's existing storage: the flat array for
the interpreter, the typed arrays for JVM and JavaScript, and one linear-memory
region for Wasm. Because `state`, `scratch`, and the per-backend layout
functions are shape-identical, the load/store code paths are shared: JVM/JS/Wasm
route `:array-load`/`:array-store` through the same `case` branches as
`:state-load`/`:state-store`. No method signature or ABI parameter changed.

**Semantics.** Scratch is zero at instance creation and a declaration emits no
run-time statement, so its contents persist between frames: a scratch array must
be written before it is read within a frame. Each declaration owns a distinct
slot (lifetime-based aliasing is deferred). Constant indices are checked while
lowering; dynamic indices are checked at run time by the interpreter and flagged
by a `:dynamic-array-index` diagnostic. Local arrays require the `:process`
entry, like state. `reset` restores state only, never scratch. Recorded as
**D15**.

**Landed through the full pipeline.** Descriptor/linker (the three forms are
language forms), portable IR (`:array-load`/`:array-store` nodes, `:scratch`
slots, `:dynamic-array-index`), the reference interpreter (arena allocation and
runtime bounds checks), JVM, JavaScript, and Wasm (a `:scratch` memory region in
the plan), plus four shared catalog cases: `:local-array`, `:local-array-int`,
`:local-array-dynamic` (uses the `aget`/`aset` aliases), and
`:local-array-with-state` (scratch offsets continue after state). The ABI now
exposes `:scratch` and `:scratch-layout` next to `:state`/`:state-layout`.

**Latent bug found (out of scope).** A sample-to-int conversion in an early
int-array case exercised `float->int` of infinity, which throws in the
interpreter (`logical-int`) while JVM `d2i` saturates — the special-value
conversion contract is still open (`PLAN.md`). The case was made
control-driven to avoid it; the divergence is recorded under open issues.

**Verified.** JVM 61 tests / 495 assertions; browser 9/9 (the process matrix
now covers the four array cases on JavaScript and Wasm at f32 and f64).
`clj-kondo`/`cljfmt` clean.

### Slice 2 (P4b) — aggregate records as named structs

`dsp/defstruct` binds a named field-shape to a Var (shorthand bare field names,
explicit descriptors, or an options map), and a state slot references it with
`:name slot :struct shape` plus an optional `:size` for an array of records.
The **linker** resolves the reference through the declaring namespace's captured
resolution — the same path calls use — and inlines the fields, keeping the
fully-qualified id as `:struct-name`. Inline `:fields [...]` is accepted for
one-off records. Access is `field`/`set-field!`, with an optional leading record
index, or the Clojure-style `(:field record)` / `(record :field)` reads and
`(set! (:field record) value)` write; the linker treats a record-name-headed
form as field access rather than a call.

Records are laid out **struct-of-arrays**: the field tree flattens to scalar
leaves, each leaf becomes one flat state entry of `:size N`, and record `i` of
leaf `j` sits at `leafOffset(j) + i`. Every leaf is therefore an ordinary
`state-load`/`state-store` with a constant base and the record index as the
element index, so the four backends are unchanged. This is D16.

### Slice 3 (P4c) — constant tables and layout metadata

`:constant true` marks a table or record read-only: it is materialized at
initialization and every store to it is rejected while lowering. The ABI now
exposes `:records` and `:arena` (region offsets, element size, alignment), the
Wasm state layout marks constant entries, and a `:dynamic-state-index`
diagnostic joins `:dynamic-array-index`. Float `:init` values stay double in the
IR (narrowed per precision at use), so an f64 table survives the EDN round trip
to the browser; the earlier `(float value)` narrowing was the last place
carrying a `Float`.

### P4 complete

All six deliverables are in: local arrays (1), aggregate records (2),
initialization-time constant tables (3), explicit layout/alignment metadata (4),
development-time bounds checks and unproven-index diagnostics (5), and
primitive-array / linear-memory lowering with no allocation after
initialization (6). Generated-target (JS/Wasm) *development guards* remain a
tracked optimization: the reference interpreter is the development-time
checker, and `PLAN.md` still notes the generated-target gap.

**Final verification.** JVM 62 tests / 511 assertions; browser 9/9, with the
process matrix covering local arrays, inline and `:struct` records (scalar,
nested, and arrays of records), and a read-only constant table on JavaScript and
Wasm at f32 and f64. `clj-kondo`/`cljfmt` clean.

---

## 2026-10-16 — P5 — Delay lines, circular buffers, and interpolation (completed)

Circular-buffer delay lines are written in the DSP language on top of the P4
arena and exercised by the catalog. The convention is D17: a persistent `:float`
buffer plus an `:int` cursor, **write then read** each frame, and wrapped indices
via the floored `mod`.

**Library.** `omkamra.dsp.library` gains the stateless helpers
`linear-interpolate` and `allpass-coefficient`, and two reusable **processes**
that own their state: `delay-line` (64-sample line, linearly interpolated
fractional tap) and `allpass-delay-line` (same line, first-order allpass tap).
Each holds its whole state in one **record that owns its buffer**
(`{:name line :fields [{:name buffer :type :float :size 64} {:name cursor ...}]}`),
accessed with `(aget line k)`/`(aset line k v)` and `(:cursor line)`. Processes
rather than functions because the call mechanism still rejects stateful
callees — that is P6.

**Catalog vectors.** `:delay-line-zero` (zero delay is the identity),
`:delay-line-unit`, `:delay-line-fractional` (4.5), `:delay-line-wrap` (30.25,
which wraps the 64-sample buffer over the run), and `:allpass-delay-line`. All
run through the shared matrix on the interpreter, JVM, JavaScript, and Wasm. A
readable JVM test pins the convention with an impulse: delay 3.0 moves the
impulse to sample 3, delay 3.5 splits it 0.5/0.5 across samples 3 and 4, and
zero delay is the identity.

### P4 follow-up — records can own their buffers

P5 needed a record to own a buffer, so the P4b omission was closed:

- a field in a **scalar** record may declare `:size M` (an array field), so a
  record is `{buffer, cursor, ...}` rather than the buffer being a separate
  top-level state array. An array of records cannot yet contain an array field
  (rejected with a diagnostic); that nested case stays for P6/P10 voices.
- the accessors gained the missing element index: `(aget rec k)`/`(aset rec k v)`
  index a record's single array field, and `(field rec :buf k)` /
  `(set-field! rec :buf k v)` is the explicit spelling. Selectors are
  `[record-index?] field-name+ [element-index?]`, so the leading index selects a
  record and the trailing one an array element.
- as a side fix, a control port that declares a non-default type (e.g. `:int`)
  now types its local, so `[sample index]` with `{:controls [{:name index :type
  :int}]}` works instead of silently making `index` a float.

### P5 follow-up — static array places (D18)

`(:line rec)` / `(field rec :line)` with no element index is now a **place** — a
compile-time reference to storage — and `(aget (:line rec) k)` /
`(aset (:line rec) k v)` index it. Places also arise from a local array, a
top-level state array, a record name with a single array field, and a `let` alias
of any of these (`(let [buf (:line rec)] (aget buf k))`). A place used where a
scalar is expected is a diagnostic. No backend changed: a place lowers to
`state-load`/`state-store` with a constant base offset. This is a strict subset
of first-class arrays — places are static, so passing/returning them and runtime
bases (into an array of records) remain P6. The `:array-place` catalog case
exercises the place form and a `let` alias on all four targets.

**Verified.** JVM 65 tests / 545 assertions; browser 9/9. `clj-kondo`/`cljfmt`
clean.

**Deferred.** The library line length is fixed at 64; a delay of `N` aliases by
design. Length *parameterization* needs compile-time capacities/modules, which is
P6's territory rather than an ad-hoc mechanism here.

---

## 2026-10-16 — P6a — Stateful module composition (D19/D20) (completed)

Calls are no longer limited to pure functions. A **module** is any DSP
definition with state: used as an entry it has ports and a per-frame transition;
used as a callee it takes params and applies its transition per call. The
semantics (D19):

- **Ownership is per call site.** Lowering allocates one private state partition
  per call site, so `one-pole-voice`'s two `one-pole`s are independent, and a
  module called twice in a loop reuses one copy — no runtime allocation and no
  shared state.
- **The transition is immediate.** A module emits its own `:next` and stores at
  the end of its body, so consecutive calls in a frame observe each other.
- The entry arena is now its own state, then one partition per call site, then
  local-array scratch. Partition sizes are static (`module-state-sizes`), so
  scratch starts after the whole state region.
- Nested composition works: a state-free module that calls a stateful one
  (`one-pole-pair`) still owns the partitions of its callees.

Mechanism: a `:call` node carries a partition base relative to the enclosing
module, while the callee IR keeps zero-based state indices. The **interpreter
executes `:call` against that partition** (`:state-base`), so it stays an
independent oracle. The three source backends share one **statement-level
inliner** that rebases state, shifts nested call bases, renames locals,
substitutes params, and rebuilds the local layout from the expanded body — under
a statement budget that turns a pathological graph into a diagnostic. The linker
still rejects a module that reaches into the caller's process buffers, and still
rejects cycles.

The bug the nested case caught: a state-free module with an expression-only body
took the pure substitution path, which did not rebase *nested* call bases, so a
second instantiation reused the first's partitions. The pure path now rebases
too.

**Verified.** JVM 66 tests / 556 assertions; browser 9/9 with 60 process cases
matched on JavaScript and Wasm (`:one-pole-voice`, `:one-pole-pair-voice`).
`clj-kondo`/`cljfmt` clean.

**Remaining for P6.** Record returns for multi-output; then P6c (the done-when
voice, the inlining-policy write-up, and the feedback/ordering notes) and P6d
(the coloring pass over partitions, D20). P6b's record-owning half is the entry
immediately below.

---

## 2026-10-16 — P6b — Modules that own records and arrays (completed)

A module may own a record with an array field, end to end:

- The entry IR, ABI, and arena now carry **callee record metadata**, rebased per
  partition: `:records` lists every record from every call site with its leaf
  offsets shifted into the partition (`line` at 0/8 and 9/17 for a pair voice).
  `lower` seeds `:records-more` like `:state-more`, and `lower-call` rebases
  both.
- `omkamra.dsp.library/delay-line` and `allpass-delay-line` are now **modules**,
  not processes — the P5 workaround is gone. `delay-line-voice` and
  `delay-line-pair-voice` compose them, and the pair voice (two lines, delays
  `d` and `2d`) proves each call site gets its own 64-sample buffer.

Two bugs the new cases caught:

1. The statement inliner rebased a callee's `:state-load`/`:state-store`
   `:index` but **not their `:element-index`**, so a record's array accesses kept
   their unrenamed locals and unshifted indices. Fixed for `:state-*` and
   `:array-*` on both the expression and statement paths.
2. The interpreter decided whether to add the partition base from `:state-id`
   presence, but place-based (`aget`/`aset`) state ops carried no `:state-id`, so
   the interpreter silently ignored the base while the backends applied it. It
   now keys off the op (`:state-load`/`:state-store` vs `:array-*`), and place
   ops carry `:state-id`, which also restores the P4 `:dynamic-state-index`
   diagnostic for record fields. The oracle caught this; the earlier
   `one-pole-voice` vector could not, because both poles saw the same input.

**Verified.** JVM 67 tests / 563 assertions; browser 9/9 with 61 process cases
matched on JavaScript and Wasm. `clj-kondo`/`cljfmt` clean.

**Record returns (multi-output) are not built yet.** They are a larger language
feature — a struct-typed return, record-value bindings, projection, and a
record-value path in both the interpreter and the inliner — and the P6 gate (a
biquad + ADSR voice) does not need them: a biquad has one output, and stereo is
already a process `:channels` concern. Recommend finishing P6c first and
returning to record returns after the gate.

---

## 2026-10-16 — P6c — The composed filter voice (completed)

**The P6 gate is met:** `omkamra.dsp.library/filter-voice` composes a stateful
RBJ biquad with the stateful exponential ADSR and runs identically on the
interpreter, JVM, JavaScript, and Wasm (`:filter-voice`; 62 process cases matched
in the browser).

- `exponential-adsr` kept its `:process` metadata and is *called as a module* —
  one definition, two usage modes (D19). The linker admits it because its body
  touches no process buffers.
- `biquad` is Direct Form I with its history in a `biquad-state` record, so the
  update order is explicit; coefficients are recomputed per sample because
  controls are per-sample in this architecture.
- The bug this surfaced: **a module lowered as a callee could not reference
  `sample-rate` or `frames`** — `lower` bound only params and state, while
  `lower-process` bound the context, and no earlier module used them as a
  callee. `context-bindings` now supplies both names to every definition, entry
  or module.

Policy recorded as **D21** (composition is inlined under a statement budget,
with a diagnostic and deliberately no run-time call fallback) and **D22**
(source-ordered state updates: simultaneous entry scalars, per-call module
transitions, explicit ordered aggregate stores).

**Verified.** JVM 68 tests / 571 assertions; browser 9/9 with 62 process cases
matched on JavaScript and Wasm. `clj-kondo`/`cljfmt` clean.

**Still open in P6.** Record returns for multi-output (additive per the D19
design) and the partition-coloring pass (P6d/D20).

---

## 2026-10-16 — P6b-ii — Record returns (multi-output) (completed)

The other half of deliverable 1. A module may now return **several typed outputs**:

```clojure
(dsp/defstruct svf-output [low band high])

(dsp/defn state-variable-filter
  [sample cutoff q]
  {:state [{:name s :struct svf-state}]
   :return-type svf-output}
  (let [f (* 2.0 (sin (* 3.141592653589793 (/ cutoff sample-rate))))
        q1 (/ 1.0 q)
        low (+ (:low s) (* f (:band s)))
        high (- (- sample low) (* q1 (:band s)))
        band (+ (* f high) (:band s))]
    (do (set! (:low s) low)
        (set! (:band s) band)
        {:low low :band band :high high})))
```

- `:return-type` names a `defstruct`; the linker resolves it (like `:struct`)
  to `:record` plus the field list, and `descriptor/LogicalType` gained a sibling
  that admits a reference. `descriptor/expression-type` already returns nil for a
  map, so the descriptor check defers to lowering — and `SourceForm` already
  allowed map literals.
- The body is a map mirroring the struct's field tree; a `let`/`do` around it is
  an ordinary form, because the expected shape travels in the lowering
  environment.
- The value is **erased**: an `:declare` bound to a record becomes one local per
  leaf. The interpreter holds a Clojure map and evaluates `:project` as a lookup;
  the statement inliner splits the declaration and rewrites `:project` to the
  leaf's local. No backend changed, and the arena is untouched — exactly the
  property D19's deferral rested on.
- Constraints: a record value cannot cross an `if`, and is immutable.

`omkamra.dsp.library` gained `state-variable-filter` (one filter, three
simultaneous outputs) and `svf-voice`; catalog case `:svf-voice` covers it on all
four targets.

Two bugs, both caught by the new cases: the inliner bound a local called `name`,
shadowing `clojure.core/name` so every per-leaf local was named `out__`; and the
record body needed a `let`/`do` around it, which the env-threaded shape now
allows.

**P6 is complete.** All five deliverables are met and the done-when voice
(`filter-voice`) runs identically on the interpreter, JVM, JavaScript, and Wasm.

**Note on P6d.** The partition-coloring pass proposed under D20 is an *arena
optimization*, not one of P6's deliverables, and all five are met without it. It
is deferred: persistent module state is almost always live across the frame
boundary (which is why the criterion is "dead on frame entry"), so it fires
rarely today and matters most once P10 gives many sibling modules.

---

## 2026-10-17 — P7a — Library namespace seams and a generated reference (completed)

P7's first decision was the sub-namespace seams the single `omkamra.dsp.library`
was deferring. It is now five namespaces, each definition moved verbatim:

- `omkamra.dsp.library.math` — `clip`, `triangular-window`,
  `linear-interpolate`, `allpass-coefficient`
- `omkamra.dsp.library.oscillator` — `sine-oscillator`
- `omkamra.dsp.library.envelope` — `adsr-coefficient`, `exponential-adsr`
- `omkamra.dsp.library.filter` — the one-pole, biquad, and state-variable families
- `omkamra.dsp.library.delay` — the delay lines

Cross-namespace DSP calls resolve through the linker's captured `:refer`
mapping, so `delay-line` calls `linear-interpolate` unqualified (clj-kondo needs
an inline ignore, because the call is quoted DSP data it cannot see as a use).

`dsp/defn` now accepts an optional docstring after the name, like Clojure,
captured on the descriptor as `:doc`. `omkamra.dsp.library.doc` links every
public definition and renders `docs/mvp/LIBRARY.md` **from the descriptors** —
parameters, controls, state, and return shape are derived, so the page cannot
drift; only the latency/modulation prose a descriptor cannot carry is authored.
A test regenerates the page and compares, so a stale reference fails the suite.

**Verified.** JVM 70 tests / 600 assertions; browser 9/9 with 63 process cases on
JavaScript and Wasm; `clj-kondo`/`cljfmt` clean.

**Remaining P7.** Oscillators (band-limited PolyBLEP saw/square/triangle, pulse
width, sync), noise (white, pink), LFOs, smoothing/slew, multimode biquad,
effects (chorus, distortion), and cubic interpolation — each with acceptance
cases and a docstring. Stereo spreading needs multi-channel stateful processes,
so it belongs with P8.

---

## 2026-10-17 — P7b — Band-limited oscillators (completed)

`omkamra.dsp.library.oscillator` gained the PolyBLEP family:

- `polyblep` — the two-sample polynomial band-limited step correction. Pure, no
  state, no table, so it stays portable and allocation-free.
- `saw-oscillator` — a ramp corrected at the wrap.
- `pulse-oscillator` — two edges, so two corrections; `width` is a control.
- `square-oscillator` — the half-width pulse, reached through the ordinary call
  mechanism (a module calling a module), so it holds no state of its own.
- `triangle-oscillator` — the integral of the band-limited square, held in a
  record so the update order is explicit.

Two language facts bit while writing these, both worth remembering:

- `-` and `*` are **binary**; `(* 4.0 dt square)` must be nested.
- **Each `if` branch is now its own scope** — see the entry below; the `polyblep`
  workaround (`x` in one branch, `y` in the other) has been reverted.

The triangle's leaky integrator trades DC removal against amplitude: the gain is
scaled to reach ±1, the amplitude varies about ±1.5% across 50 Hz..2 kHz, and the
first cycle has a start-up transient (the DC removal has no history yet). All
three are documented in the module docstring rather than hidden.

**Verified.** JVM 71 tests / 634 assertions; browser 9/9 with 67 process cases
matched on JavaScript and Wasm (the four new oscillator cases included), plus a
JVM test showing the corrected saw's largest sample step is far smaller than a
naive sawtooth's — the measurable point of PolyBLEP. `clj-kondo`/`cljfmt` clean.

---

## 2026-10-17 — P7c — `if` branches are separate scopes (completed)

A language wart: `lower-if` lowers both branches with the same environment, and
the flat `:locals` list carried a "names must be unique" rule, so a local name
used in *both* branches — perfectly legal, since they are disjoint — failed with
"cannot shadow bindings". `polyblep` had to use `x` in one branch and `y` in the
other to work around it.

A local now keeps its own IR name unless that name is **already used elsewhere
in the compilation unit**, in which case it gets a fresh one
(`(if c (let [x 1] x) (let [x 2] x))` yields locals `x` and `x__1`). The flat
local list stays unambiguous, the two branches get separate slots even when their
types differ, and the common case keeps readable names — the existing test that
asserts `:locals` for an ordinary `let` is unchanged.

Genuine shadowing is still rejected: a local may not rebind a name that is
**in scope** at that point (`(let [sample 1.0] …)` where `sample` is a parameter,
or a nested `let` rebinding an outer local). That is a deliberate language
restriction, only the false positive across sibling branches was removed.

**Verified.** JVM 72 tests / 640 assertions, including a new test that reuses one
name in both branches, does it again with **different types** per branch (which
would corrupt a shared slot), and asserts shadowing is still a diagnostic;
browser 9/9 with 67 process cases. `clj-kondo`/`cljfmt` clean.

---

## 2026-10-17 — P7d — Noise, LFO, and a parameter-typing fix (completed)

**Noise** (`omkamra.dsp.library.noise`):

- `white-noise` — a 32-bit xorshift written with the P1 integer bitwise
  operations, so it is deterministic and identical on every target. The output
  takes the top 24 bits of the state, which are exactly representable in both f32
  and f64, giving uniform samples in [-1, 1) (measured rms 0.573 ≈ 1/√3).
- `seed` is a **control**, not state: a compile-time `:init` cannot vary per
  instance, and a voice needs decorrelated noise. The seed *rotates* the state
  before extracting bits — a bit permutation, so it decorrelates seeds without
  skewing the distribution, unlike adding or xoring a seed into the top bits.
- `pink-noise` — white shaped by the seven one-pole sections of Kellet's filter,
  the section outputs in a record so the update order is explicit. Measured lag-1
  autocorrelation is **0.82** against white's **0.01**, which is the property the
  test asserts.

**LFO** (`oscillator/lfo`) — a unipolar [0, 1] modulation oscillator with a
`shape` selector (saw, triangle, sine). The same module at audio rate is an
ordinary oscillator.

**The bug this surfaced: a shorthand parameter is always `:float`.** So a
parameter was `:int` when a definition was compiled as a *process entry* (where
`normalize`/`process-bindings` control typing already applied) but `:float` when
the same definition was called as a *module*. `pink-noise` calls `white-noise` as
a module and hit exactly that. `descriptor/normalize` now applies the same rule
the IR used — a control that declares a **non-`:float`** type types its parameter
— so both uses agree. The rule is deliberately narrow: only a non-default type
overrides. The first attempt overrode *every* parameter from its control (because
`normalize-control` always fills a default `:float`), which clobbered an
explicitly typed parameter and broke the state-table test.

**Verified.** JVM 73 tests / 665 assertions; browser 9/9 with 70 process cases
matched on JavaScript and Wasm. `clj-kondo`/`cljfmt` clean.

---

## 2026-10-17 — P7e — Control-rate helpers, distortion, cubic interpolation (completed)

Three self-contained additions across two new namespaces:

- `omkamra.dsp.library.control`:
  - `smoothing` — one-pole approach to a target with a time constant, reusing
    `envelope/adsr-coefficient` through the ordinary call mechanism. Measured:
    one time constant reaches 1 - 1/e (0.633), as it should.
  - `slew-limit` — bounded slope, so a step becomes a ramp of
    `rate/sample-rate` per sample (measured 4.535e-5 = 2/44100).
- `omkamra.dsp.library.effect/distortion` — `tanh` saturation normalised by
  `tanh(drive)`, so full scale reaches ±1 at any drive; odd and stateless.
- `omkamra.dsp.library.math/cubic-interpolate` — Catmull-Rom, **exact on a linear
  ramp** (which the test asserts), for fractional delay taps and resampling.

Nothing needed new language support: `adsr-coefficient` is reused across
namespaces, and `slew-limit`'s `:next` and its body use the same local names
(`step`, `delta`) — which only works because P7c made `if` branches separate
scopes, with `unique-local-name` freshening the second occurrence.

**Verified.** JVM 74 tests / 694 assertions; browser 9/9 with 74 process cases
matched on JavaScript and Wasm. `clj-kondo`/`cljfmt` clean.

**Remaining P7.** Chorus (a modulated delay reusing the LFO), a multimode biquad,
and converting the older `;;` comments to docstrings. On the multimode biquad:
`state-variable-filter` already yields low/band/high simultaneously, and an RBJ
multimode cannot share one feedback history across three outputs — it would need
three states or a `mode` selector that reuses one. The selector is the useful
addition; three parallel biquads are already expressible with the call mechanism.

---

## 2026-10-17 — P7f — Multimode, linear envelopes, chorus, spreading, sync (P7 complete)

The last of the contents list:

- `filter/biquad-multimode` — one biquad, four responses selected by `mode`
  (lowpass, bandpass, highpass, notch). One feedback history suffices because
  only one mode is active at a time, and all four share the RBJ denominator.
  Lowpass mode is **bit-identical** to `biquad`, which the test asserts.
- `envelope/linear-step`, `ar-envelope`, `linear-adsr` — straight-segment
  envelopes sharing the exponential ADSR's stage machine. AR reaches 1.0 in
  exactly `attack` seconds (asserted).
- `delay/chorus` — a delay line whose tap is swept by the ordinary `lfo`, so the
  modulation needs no machinery of its own; `mix` 0 is bit-exact dry (asserted).
- `effect/stereo-spread` — Haas-style spreading as a **record-returning module**,
  with one delay line shared by both outputs so they stay coherent. Writing the
  two channels needs a multi-channel process, so a stereo *process* entry waits
  for P8; the module itself is exercised on all four targets through an inline
  catalog case that projects one field.
- `oscillator/sync-oscillator` — hard sync with the reset placed at the master's
  sub-sample zero crossing, which removes the timing quantisation. The reset step
  is deliberately **not** BLEP-corrected and is documented as such: it is the one
  oscillator here that is not band-limited.

Earlier definitions also gained real docstrings (their prose lived in `;;`
comments), so the generated reference now describes every substantive module.

**P7 is complete.** Contents covered: band-limited saw/square/triangle/sine;
white and pink noise; PWM and sync; linear and exponential AR/ADSR envelopes; an
LFO; smoothing and slew limiting; one-pole, biquad, multimode biquad, and SVF
filters; delay, chorus, and distortion; linear, cubic, and allpass
interpolation; and stereo spreading as a module. Eight library namespaces, a
generated reference, and every module in the shared acceptance catalog.

**Verified.** JVM 75 tests / 714 assertions; browser 9/9 with 80 process cases
matched on JavaScript and Wasm. `clj-kondo`/`cljfmt` clean.

**Deferred out of P7.** Stereo spreading's multi-channel *hosting* (writing two
output channels) needs multi-channel stateful processes, which P6 left closed;
plus the D23 optional input port (P8) and the D20 coloring pass.

---

## 2026-10-17 — P8a/P8b — Target-neutral process instances (deliverables 1, 2, 4, 5 done)

The artifact/program and the instance are now separate concepts.

- **`dsp/create-instance`** returns an independent instance of a compiled
  process artifact; **`dsp/destroy!`** releases it; `dsp/reset!`,
  `dsp/process!`, `dsp/process-checked!`, `dsp/process-unchecked!`,
  `dsp/invoke` and `dsp/initialize!` accept *either* a plain artifact (which
  keeps owning one implicit state region, exactly as before) *or* an instance.
  `dsp/process-instance?` distinguishes them.
- **The JVM already had the right shape.** Its generated kernel class takes the
  state arrays as *arguments*, so state never lives in the kernel object.
  Instantiating therefore allocates a fresh state region against the *same*
  compiled class and recompiles nothing. The interpreter gets the same factory
  over `interpreter/create-state`.
- **Independence is asserted, not assumed**: two instances of one delay line,
  one fed an impulse and one fed silence — the fed instance's state is live, the
  other's stays exactly zero; `reset!` on one leaves the other's ring intact;
  `destroy!` zeroes the state and makes further use throw; the artifact is never
  disturbed. Interpreter and JVM instances of one artifact agree exactly.
- **One lifecycle vocabulary on every target** (deliverable 4). `:instance`
  gained `:operations` and an abstract `:handle`, so no target-specific object
  leaks into portable IR:

  | target | create | reset | destroy | handle kind |
  |---|---|---|---|---|
  | interpreter / JVM | `create-instance` | `reset!` | `destroy!` | `:state-region` |
  | JS | `createKernel` | `reset` | `destroy` | `:kernel-handle` |
  | Wasm | `instantiate` | `reset` | `release` | `:wasm-instance` |

  A top-level `:program {:ownership :artifact :mutable? false :shareable? true}`
  states the artifact/instance split. The JS kernel gained a real `destroy()`
  (it had only `init`/`reset`); a Wasm instance owns its linear memory, so its
  release is dropping the instance.

**A latent bug this caught.** `process-unchecked!` is the audio hot path, and
my first version routed it through a helper returning `[artifact instance]` — a
2-element vector per call. The existing
`jvm-unchecked-process-adapter-does-not-allocate-after-warmup` test failed with
651 376 bytes against an 11 304-byte baseline (≈65 bytes/call), consistently.
Replacing the helper with an `or` over a map lookup made it allocation-free. The
test that looked flaky in P7 was doing its job; the GC noise is separate, this
was real.

**Two small traps worth remembering.** The JVM instance factory runs long after
`compile-process` returns, so it must rebind `*precision*` itself — the layout
helpers read it, and `f64?` is a *function* (`(f64?)`), not a value, so
`(if f64? ...)` silently takes the f64 branch. And `target-instance` returns the
*instance description*; the Wasm path initially used its result as the whole
ABI, which made `:abi` the instance map.

**Still to do in P8 — deliverable 6, D23 (optional input ports).** See the P8d
entry below, where it was implemented; the JVM's kernel-internal signature
deliberately keeps the input parameter, and the reasoning is recorded there.

---

## 2026-10-17 — P8d — Optional input ports (D23, deliverable 6)

A process may now declare `:input false`. It then has **no sample parameter, no
input buffer, and no input binding**, and every parameter is a control:

```clojure
(dsp/defn sine-oscillator
  [frequency]
  {:process {:input false :controls [{:name frequency}]}}
  ...)
```

What the declaration actually removes:

- **The dead `:buffer-load` and the false `:read-memory`.** A generator's
  `:effects` is now `#{:read-state :write-state :write-memory}`. Before, the
  ignored `sample` still emitted a real input load, so `:effects` — a real-time
  policy signal — lied about the process reading memory. This was D23's whole
  point.
- **The `:input` binding**, and with it the port in `:buffers`/`:inputs`.
- **The argument-index arithmetic.** `process-abi` used to hardcode `0,1,2,3`
  plus `4 + index`; it now derives every index from the buffer list. A generator
  is `output 0, frames 1, sample-rate 2, controls 3+`; an ordinary process is
  unchanged. This is the rework P8 was scheduled to absorb.

**One predicate holds it together.** `ir/process-input?` must distinguish *no
`:process` map at all* (an ordinary block processor whose first parameter is the
sample — `{:entry :process}` on a plain definition still works) from *a
`:process` map whose `:input` normalized to nil* (an explicit `:input false`).
Both look like `nil` in different places, so the rule is on the presence of the
map, not of the value.

**Adapters: host-facing yes, kernel-internal no — a deliberate amendment to
D23.** The context adapters (interpreter and JVM) tolerate a missing `:input`,
and JS and Wasm *emit the shorter signature* — `function process(output, frames,
sampleRate, increment)` and a `$process` with no `(param $input …)` — so the
exported arity genuinely agrees with the derived indices, and the browser
runners dispatch on the ABI rather than guessing. The **JVM's generated kernel
method still takes the input array, and a generator's adapter passes nil.**
D23 said "no input argument in any adapter"; doing that literally would need
`DspSourceKernel`-style interfaces ×4 (f32/f64 × mono/multi-channel), duplicating
the kernel emission and every adapter for no semantic gain: the array never
reaches the ABI, the effects, or the frame body. Recorded rather than quietly
skipped.

**Library migrated.** All nine generators dropped their ignored `sample`:
`sine`/`saw`/`pulse`/`square`/`triangle`/`sync` oscillators, `lfo`, and
`white-noise`/`pink-noise`. Call sites followed — `chorus` now says
`(lfo rate 2)` instead of `(lfo 0.0 rate 2)`, `square-oscillator` says
`(pulse-oscillator frequency 0.5)` — and the docstrings that explained the
ignored parameter no longer have to.

**One subtlety in the plan/runner path.** The frame count now always comes from
the plan's `:frames`, never from the input length. A 0-frame invocation of an
ordinary process has an *empty but present* input, and `(seq input)` cannot tell
that apart from "no input at all"; the decision belongs to the compiled ABI
(`:inputs`), not to the data. The browser's interpreter oracle had the same bug,
which surfaced as a `sine-oscillator` mismatch whose describer could only say
"block count actual 5 expected 5".

**Verified.** JVM 77 tests / 781 assertions; browser 9/9 with 80 process cases
matched on JavaScript and Wasm, now including the migrated generators. New test
`generators-have-no-input-port` asserts the empty `:inputs`, the clean effects,
the derived indices, that every parameter became a control, that mentioning
`sample` is an "unknown local" error, that interpreter and JVM agree, that the
JS/Wasm exports drop the parameter, and that all nine library generators are
clean of `:read-memory`.

**Still to do in P8 — P8c.** A browser-side demonstration that JS and Wasm
instances are independent (two kernels / two instantiations). The JVM test
already asserts the lifecycle vocabulary on all four targets, and each Wasm
instantiation owns its own memory, so this is a verification gap rather than a
capability gap.

---

## 2026-10-17 — P8c — Instance independence in the browser (P8 complete)

The browser suite now proves that instances of one compiled kernel are
independent on both generated targets, adding four checks (JavaScript and Wasm,
each at f32 and f64) and taking the suite from 9 to **13**. The protocol drives
the same impulse through one instance alone and through a second instance with a
*third*, silent instance driven between every block:

- the impulse instance's output must be **identical** in both runs, so the silent
  instance never disturbed it;
- the silent instance's output must be **all zeros**, so it never observed the
  impulse instance's state;
- the impulse must leave *observable* state, or the comparison would pass
  vacuously.

**The check carries its own negative control, and it immediately caught a real
weakness.** The protocol is run a second time over a *single* instance handed to
all three roles. That is sharing, so it must be rejected; a check that cannot
fail its own control cannot detect sharing at all, and a silent control is
reported as a failure rather than ignored.

The first version used `:delay-line-unit` — a 1-sample delay — with 4-frame
blocks. The negative control found nothing, and the reason is instructive: with a
1-sample delay the line **flushes itself inside every block**, so a shared
instance and two independent ones produce exactly the same outputs. The check
would have passed forever while being unable to detect the thing it exists to
detect. Switching to `:delay-line-wrap` (a 30.25-sample delay) over 12 rounds
makes the state survive across block boundaries and lets the impulse emerge, so
sharing shifts the output timing and the silent instance reads out the impulse's
tail. The control's own findings are now printed as evidence — "the impulse
instance's output changed when a second instance ran between its blocks; a silent
instance produced output, so it observed another instance's state" — both of
which are sharing signals, not vacuity.

One implementation trap: the Wasm instantiations must use `mapv`, not `repeat`.
`(repeat n (js/WebAssembly.instantiate bytes))` evaluates `instantiate` **once**
and repeats the value, so all three "instances" would have been the same object
sharing one linear memory.

**Verified.** Browser 13/13; JVM 77 tests / 781 assertions; `clj-kondo`/`cljfmt`
clean.

**P8 is complete** — all six deliverables: the target-neutral
`create-instance`/`reset!`/`destroy!` lifecycle, interpreter and JVM factories
producing independent instances without recompiling, one create/reset/destroy
vocabulary on all four targets, ABI instance handles and ownership, differential
independence tests on the JVM *and* in the browser, and optional input ports
(D23).

---

## 2026-10-17 — P9a/P9b — The sequencing layer (deliverables 1, 2, 4 and 6)

The PLAN already drew the line this phase needs: the **sequencing layer** is
host-side and portable, owns the timeline and the capacity, and delivers
canonical timestamped messages; the kernel owns typed state and never learns
about events, host objects, or absolute time. So P9 added no backend change at
all, and the whole phase is two new `.cljc` namespaces.

**Two clocks, deliberately.** An event's `:frame` is an absolute, monotonic
position on the synth timeline and is 64-bit, because 32 bits of frames is only
about 12 hours at 48 kHz. An offset handed to a kernel is block-relative and
small, so it stays inside the language's 32-bit `:int`. `:sequence` orders
events sharing a frame, which is what makes rendering independent of where the
host cut its blocks.

**`omkamra.dsp.event`** — the v1 vocabulary (`:note-on`/`:note-off` with
velocity and release velocity, `:pitch-bend`, `:control-change`, `:parameter`,
`:pressure`, `:transport`, `:sample-rate`), with closed payload validation (a
typo cannot silently become a no-op), `compare-events` ordering, and the SPSC
ingress queue. The queue is fixed-capacity and preallocated; on the JVM its
indices are `AtomicLong`s and its slots an `AtomicReferenceArray`, so a
published event needs no lock. Overflow drops the **oldest future** event —
never one already due or being dispatched — and when nothing is droppable it
**rejects the incoming event** instead, so a burst can never make the audio
thread lose an event it owes the kernel. Either way a fixed counter moves;
nothing logs, allocates, blocks, or throws.

**`omkamra.dsp.schedule`** — block-relative segmentation. `next-segment!`
returns the scheduler's own reusable segment: an offset, a frame count, and the
events due at that offset, read out of a preallocated buffer. An event due
exactly at the block's end is deliberately left for the next block.

**The kernel did not have to change, and that is the point.** A segment boundary
is exactly a point where a control value changes, and the existing process ABI
already lets a host set controls and render a block, so segmenting a block is
repeated `process!` calls with the controls updated in between. Partition
invariance then follows from the language's per-frame updates — which is
*deliverable 6*, and it is now proven rather than argued: three partitionings
(200, 64 and 7-frame blocks) of the same 200-frame timeline render **exactly
identically** on the interpreter and JVM at f32 and f64, and in the browser on
JavaScript and Wasm.

The one real cost is slicing. A kernel renders into the start of its output
buffer, so rendering frames `[k, k + n)` means copying that slice in and out of
a preallocated scratch. That is the honest price of block-relative rendering
against an ABI with no block offset; a host that grows one would drop both
copies.

**The checks carry their own falsification.** A dispatch at frame 17 renders
*identically* to splitting the block by hand at 17, and moving the event by one
frame moves the output by exactly one frame:

```
first sample where event@17 differs from event@18: 17
first sample where event@17 differs from event@16: 16
```

So the equivalence is not vacuous. The browser check adds a second control: the
same block with the events *quantised* to the block boundary — what a host that
ignored frame offsets would produce — must **differ**, and it does.

### Three bugs the tests caught

1. **`copy-range!` lost its target offset** when I simplified it, so a later
   segment's rendered frames were copied back to index 0. Every no-event case
   passed (all segments start at 0) and every multi-block case failed. This is
   precisely why the invariance test exists.
2. **The `Segment` was given its own event array** while `next-segment!` filled
   the scheduler's, so events were never readable and *no control ever
   changed*. The invariance test passed — **vacuously**, because the timeline
   was equivalent to no events at all. What exposed it was asserting that an
   event *changes* the signal; the same lesson as P8c's negative control, which
   is why every check here now has a positive or negative twin.
3. **A reflective array access costs ~24 KB on this JVM.** `begin-block!` —
   three writes to a `long[]` — allocated 74 KB per call, and one `push-event!`
   22 KB, because Clojure could not resolve the `long[]` overload of
   `aset`/`aget` and fell back to `Reflector`. `(set! *warn-on-reflection* true)`
   named every site; adding a `^longs` hint (or an explicit `(long ...)`) took
   the whole scheduling path from **6.5 MB per block to 48 bytes**, and a
   push/pop cycle from 24,624 bytes to **0**. The pattern is worth remembering:
   an unhinted primitive-array access in a hot path is not merely slow, it is
   catastrophic, and reflection warnings are the tool that finds it.

Two of the three were found only because a test asserted a *control* outcome as
well as the intended one. That is now the house style for this phase.

### Not done, and why

- **Deliverable 3 (parameter ramps and one-pole smoothing as ABI-level
  constructs)** is not implemented. P7 already ships `control/smoothing` and
  `control/slew-limit` as DSP-language modules, and an ABI-level ramp is not the
  same thing: it needs a per-*frame* interpolation inside the kernel (a `:ramp`
  IR op across four backends), because a per-*segment* ramp would make the
  result depend on where the host cut its blocks and would break deliverable 6
  outright. That is a real piece of work, not a detail, and it is better done
  deliberately than bolted on here.
- **Deliverable 5's non-finite containment** is not implemented. The overflow
  and rejection counters are (`queue-stats`). Containment — detecting a
  non-finite output, silencing the affected voice — is a kernel-side policy
  belonging with P10's voice pool, where "the affected voice" exists.

**Verified.** JVM 81 tests / 852 assertions; browser 17/17 (four shared
matrices, the WAT bounded-loop and sin-table checks, four instance-independence
checks, four sample-accurate-scheduling checks, and three AudioWorklet
adapters); `clj-kondo`/`cljfmt` clean.

---

## 2026-10-17 — P9c — Control smoothing and slew, declared on the control (D27)

Segmentation alone cannot animate a control *within* a segment, so a control can
now declare its own per-frame carrier:

```clojure
(dsp/defn smoothed-gain
  [sample amount]
  {:process {:controls [{:name amount :smooth 0.01 :default 0.0}]}}
  (* sample amount))
```

`:smooth` is a one-pole coefficient and `:slew` a maximum step per frame, both in
the control's own units. Setting such a control becomes a *target* rather than a
jump, and the ABI reports the mode so a host knows that.

**No new IR node, no ABI change, no backend change.** A per-frame one-pole is
three arithmetic operations, and lowering already has the mechanism: the input
sample is bound to an arbitrary expression (`:buffer-load`), so a control can
equally be bound to a *state read*, with the update prepended as that state's
`:next`. `descriptor.clj` desugars a declared mode into one float state slot plus
a `:next`, and rewrites reads of the control to read the state. The raw control
remains the target, so a host drives it through the ordinary control mechanism —
mid-block events included.

Both amounts are **per frame**, which is the whole point: a per-*block* amount
would make rendering depend on the host's block size and would break P9's
invariance outright.

**Verified on all four targets.** `smoothed-gain` and `slewed-gain` are library
definitions with catalog cases, so the shared matrix covers JavaScript and Wasm
(82 process cases now), and a dedicated test asserts the engine matches an
independently computed f32 one-pole recurrence, that a slewed control takes
bounded steps and lands exactly on its target, that the ABI reports the mode,
and that `:smooth`/`:slew` on an `:int` control, both modes at once, and a
non-numeric amount are all rejected. Partition independence is asserted with a
target change *inside* the block, on interpreter and JVM at f32 and f64,
including a check that the automation is doing something.

**Three of my own mistakes, all caught by running it:**

1. `ramp-state-name`'s parameter was called `name`, shadowing `clojure.core/name`
   — so `(name name)` became a map lookup returning nil and every state came out
   `-ramped`. The same class of mistake as the `case` parameter noted in the
   reference section.
2. The body/`:next` rewrite ran *after* the synthesized states were added, so it
   rewrote the updates' own target into the smoothed value, making the update
   `current += (current - current) * c`. The order is load-bearing: rewrite the
   user's forms first, then append the synthesized states.
3. `normalize-control` received shorthand controls as *symbols*, and my mode
   checks assumed maps. Every mode check now establishes `map?` first.

**Not done: host-supplied automation curves** (block-rate control buffers), which
is the other half of D27. A lane needs a buffer region in the ABI, its Wasm
memory layout, and a per-frame read in place of the control — the same
`{:op :buffer-load}` shape the input sample already uses. Linear/critical ramps
to a target stay unimplemented: they need a per-frame division plus a counter, or
a block-rate prologue the IR does not have, and per D27 a host can supply them as
a control buffer instead.

---

## 2026-10-17 — P9d — Host-supplied automation curves: per-frame lanes (D28)

A control can now declare `:lane true`, and its value comes from the host one
float **per frame** — arbitrary curves, envelopes, sequenced lanes:

```clojure
(dsp/defn lane-gain
  [sample amount]
  {:process {:controls [{:name amount :lane true}]}}
  (* sample amount))
```

```clojure
(dsp/process! artifact {:input in :lanes {:amount cutoff-curve} :output out
                        :frames 128 :sample-rate fs :controls []})
```

**The reconnaissance is the story.** My first estimate was that this needed a new
IR op and a new region concept across four backends. It needed neither, because
three things were already true and I checked rather than assumed:

- every backend resolves `:buffer-load` **generically by `:buffer-id`** — the
  interpreter via a context lookup, the JVM via a `slots` map keyed on the id;
- `:frame-offset` (`frame + constant`) is **already implemented in all four**
  backends and is already the IR's canonical form for a literal offset;
- `frame-capacity` is already a fixed compile-time constant in the Wasm layout.

So a lane read is *the same shape as the input sample read* —
`{:op :buffer-load :buffer-id :lanes :index {:op :frame-offset :offset 0}}` —
which is the strongest signal available that it fits the language instead of
being bolted on.

**What is implemented.** The descriptor accepts `:lane` and appends one
`:lanes` region (`:direction :control`, which the buffer schema already had);
lowering binds a lane control to the per-frame read and drops it from the scalar
`:controls` list, since it is not a block-rate value; the ABI reports
`:lanes [{:name amount :offset 0 …}]`, `:lane-stride`, and the region in
`:buffers`/`:memory`; `process!` flattens `{:amount …}` into the region,
accepting either spelling of the name; and the interpreter needed only to thread
one more key, because its buffer lookup is already generic.

Verified: one lane renders `[0.25 0.5 0.75 1.0]`; two lanes land at offsets 0 and
1024 and stay independent; a lane with no values **fails loudly** rather than
publishing zeros; and a lane composes with `:smooth` for free, since a smoother's
target is just the control read — `[0.0 0.5 0.75 0.875]` for a coefficient of
0.5.

**One trap worth keeping.** The ABI names a lane with a *symbol*, as it names a
control, and my first probe passed keywords — so the copy silently did nothing
and the lane read zeros. That is the worst possible failure for automation, so
the adapter now accepts either spelling *and* throws when a lane has no values.

**All four targets, and the catalog.** The JVM took one extra array parameter on
all eight `DspKernel`/`DspProcess` interfaces, threaded through the adapters,
`kernel-process!`, `process-context-callable` and the raw `process-checked!`/
`process-unchecked!` (which gained a 7th *arity* rather than a new signature, so
no existing caller had to change). JavaScript and Wasm were small: JS gained a
`lanes` parameter and reads `lanes[(frame + 0)]`; Wasm gained a `$lanes` pointer
and a `:lanes` region between the audio ports and the state arena, and needed
**no addressing change at all**, because a lane read carries `:channel 0` and the
existing `channel * stride` term reduces to zero. `:automation-lane` is now a
case in the shared catalog, so the browser matrix verifies both generated targets
against the portable-IR oracle. Language suite **84 tests / 974 assertions**, and
browser **17/17** with 84 process cases.

**Three bugs, all worth keeping.**

1. **Channel-ness was a per-process fact, and that was already wrong.** A lane
   region is flat even in a multi-channel process, so "is this access subscripted
   by channel" had to become a per-*buffer* question. Recording `:per-channel?` on
   each buffer access in the IR answers it for all four backends at once — and it
   exposed a latent bug: a multi-channel process that *declared* its `:process`
   had its audio ports normalized at descriptor time, where no channel count is
   known yet, so `process-buffers` trusted a placeholder `1` and never
   subscripted the input at all. The audio ports now take the compile-time
   channel count by definition, and a one-channel buffer stays a broadcast
   buffer.
2. **The lane stride is a byte count but a lane offset is an element index.**
   f32 passed and f64 failed: 1024 elements span 4096 bytes only at 4 bytes per
   element. The stride is now derived from the precision, so a lane holds 1024
   frames in f32 and 512 in f64. This is exactly the class of bug a single-target
   check would have missed — and the fix is pinned by an assertion on both
   strides and both offsets.
3. **`(str :amount)` is not `(name :amount)`.** The plan travels as JSON, so
   serializing a lane's name with `str` produced `":amount"`, which could never
   match a lookup on the page. It failed *quietly* as a mismatch rather than as
   an error, since a missed lane simply reads zeros.

**How the generated targets were verified before the catalog existed.** A short
Node harness: compile a case for `:js` and `:wasm`, run it with the interpreter's
output as the expectation, and drive it directly — `node_modules/.bin/wat2wasm`
plus `WebAssembly.instantiate` for the module, and the emitted ES module for the
JavaScript. It runs both precisions in seconds and reports the first differing
sample, which is how the f64 stride bug surfaced. The catalog case is what keeps
them honest from now on.

**One ordering rule for the page.** The browser's oracle calls
`interpreter/process-context` *directly*, so it does not pass through
`dsp/process!`'s flattening and must flatten lanes itself — it now shares the
drivers' `lane-region`, so all three agree by construction.

---

## 2026-10-17 — P15a — Explicit capability diagnostics (pulled forward, D29)

`dsp/compile` now refuses an option the chosen target cannot serve, before any
lowering, instead of letting a backend ignore it or substitute something else:

```
DSP target :wasm cannot serve the requested options: it serves :process entries,
not :function; it serves :wasm-math :polynomial at :f64, not :table
```

Every unsupported request is reported at once, and the error carries the
machine-readable form too (`{:dsp/error :unsupported-capability :capabilities
[{:capability :entry :requested :function :supported #{:process}} ...]}`).

**The work was mostly finding out that the note was wrong.** It claimed the gap
to declare was the operator set, because Wasm has no native transcendentals. P3
had already closed that with emulations, so a per-operator table would have been
entries that all say `true` — untestable code. The differences that actually
exist are in the options and entry kinds, and two were silent: validation
(`:checks :development`) is implemented only by the interpreter, and the f32
lookup table silently became `:polynomial` at f64.

**One consequence was load-bearing, not cosmetic.** `:checks` had to become a
per-target default. With the single global default of `:development`, every
generated-target compile would have carried `:development` in its effective
options while honouring nothing, and an unsupported value would have been
indistinguishable from the default — so the check would have fired on every
build. Deriving the default from the target makes the effective value always
servable, which is exactly what lets an unsupported value mean "somebody asked
for this". The `:entry` default is the opposite case and stays global on purpose:
the same definition must not compile to a function on JS and a process on Wasm.

**Two mistakes of mine, both instructive.**

- The first draft of `problems` destructured `{:keys [checks wasm-math]}` from the
  target table *and* took them from the requested options under the same names.
  The shadowing made `(contains? checks requested-checks)` compare the table
  against itself, so the `:checks` capability could **never** be reported — a
  vacuous check of the exact kind this project keeps rediscovering. The test's
  negative twin (the interpreter must still accept `:development`) is what would
  have caught it had the assertion not been written against the table.
- A test inserted at a prefix-matching anchor landed *inside* the neighbouring
  `deftest`, which is legal enough to compile but produces an inline `def`.
  `cljfmt` then re-indented it to match its new nesting, which made the mistake
  look deliberate. Indentation is not evidence of structure; the lint warning
  was. The anchor now matches the closing parens, and the replacement is verified
  by checking what precedes and follows it.

---

## 2026-10-17 — Wasm lookup table — benchmarked, cosine fixed, with follow-ups

Steps 1 and 2 are done (shared emulation functions; f32 sine table via
`:wasm-math :table`). Measured on this machine with the four-calls-per-sample
benchmark, warmed, best of three, at f32:

| | `:polynomial` | `:table` |
|---|---|---|
| cost per `sin` | 137.1 ns | **24.3 ns** (5.65x faster) |
| cost per `cos` | 137.0 ns | **25.4 ns** |
| module text | 25 584 bytes | 51 928 bytes |
| linear memory | 8 192 bytes | 24 580 bytes |

**`cos` is fixed.** It was looked up as `sin(x + π/2)`, which rounds the
argument *before* the range reduction — an error of order `ulp(x)`. The quarter
period is now applied *after* reduction, in index space, where it is exactly
`entry-count / 4`. Measured against `Math.cos` at |x| ~ 1e10, cosine error went
from 2.0e-6 to 1.4e-6, and the cos/sin error ratio from 1.415 to 0.99: cosine is
now no worse than sine, with the residual being the terms they share.

**But the item was smaller than it looked, and that is the useful finding.** The
posited shift error is ~`|x| * 2^-52`, while the range reduction's own error is
~2π times larger and therefore dominates. So the shift contributes at most about
a sixth of the total, and it only *appears* to double the error when a maximum is
taken over few samples, because the error oscillates. Any real accuracy work at
large arguments belongs in the **range reduction**, not here: it is what makes
the polynomial strategy degrade from 3e-8 at |x| ~ 1e6 to 1.7e-7 at |x| ~ 1e9,
and it affects `sin` exactly as much as `cos`. A Cody-Waite or double-double
reduction would lift both.

**Two incidental improvements.** Binding the scaled index to a Wasm local was
needed because a WAT expression is a tree: the first version of the fix
referenced the reduction three times, which emitted it three times and *cost*
half the table strategy's speed (32.6 ns to 47.5 ns). Doing it with locals took
it to 24.3 ns — better than before the change — and shrank the module text below
its original size. The index wrap also uses a remainder rather than a
compare-and-subtract, which covers a latent edge case: a reduced phase landing
exactly on the period produced an index of N, and reading entry N+1 one past the
last.

Remaining:

- **f64**: a raw table would be too large; consider a piecewise minimax
  coefficient table if f64 Wasm speed ever matters.
- **`exp`** has no table (polynomial only); fine while it is not hot.
- **Table size**: 4097 f32 entries (16 KB payload, ~52 KB WAT text). Could
  shrink to 2049 entries if the interpolation error stays acceptable, or grow
  for margin.
- **`:wasm-math :table` is f32-only and silently ignored at f64** (see open
  issues; it is now a compile error rather than a silent fallback).
- **`:wasm-math :table` is f32-only and silently ignored for f64** (the table is
  a byte-stride region, so it has no f64 form). A caller asking for `:table` at
  f64 gets the polynomial emulation without being told.

---

## 2026-10-17 — Layout after the reorganisation (before P10)

The suite and its scaffolding were reorganised so a change can be checked against
the part it concerns, and so it is obvious which namespaces are the compiler and
which are the machinery around it. The suite is unchanged in substance: **85
tests, 995 assertions**, and browser **19/19** either side of it.

| before | after |
|---|---|
| `omkamra.dsp-test` — one 2864-line file | `omkamra.dsp.tests.{descriptor,linker,ir,interpreter,jvm,js,wasm,process,memory,composition,library,conformance,automation}`, plus `tests.suite/run-all` |
| *(new)* | `omkamra.dsp.harness.fixtures` — the shared `dsp/defn` fixtures and measuring helpers |
| `omkamra.dsp.acceptance` | `omkamra.dsp.harness.catalog` |
| `omkamra.dsp.conformance` | `omkamra.dsp.harness.conformance` |
| `omkamra.dsp.testbed` (`.clj`/`.cljs`) | `omkamra.dsp.harness.testbed` |
| `omkamra.dsp.browser` | `omkamra.dsp.harness.browser` |
| `omkamra.dsp.call-fixture` | `omkamra.dsp.harness.call-fixture` |
| `omkamra.dsp.{interpreter,jvm,js,wasm}` | `omkamra.dsp.targets.*` |
| `omkamra.dsp.capability` | `omkamra.dsp.targets.capability` |

Entries above this one use the old names, and should: they record what was true
when they were written.

**Three couplings that the file tree does not show.**

1. **HTTP routes follow the namespace.** `omkamra.dev.on-demand-http` derives the
   handler namespace from the URI, so moving the testbed moved every endpoint to
   `/omkamra/dsp/harness/testbed/...`. The *page* is a separate mechanism: it is
   served for whichever URI matches a Shadow build's `:asset-path`, and the page
   HTML's script tag is generated from that same path. So `shadow-cljs.edn`'s
   build id, `:output-dir`, `:asset-path` and `:init-fn` had to move together, or
   the page would 404 its own bundle. The Shadow content cache also had to be
   cleared, as ever.
2. **A `dsp/call` inside quoted data resolves against the aliases of the
   namespace that *initiates the link*, not the one that defines the body**: the
   linker uses `find-ns`, which only sees namespaces already loaded. Splitting
   the fixtures away from the tests broke two calls, so the fixtures now call
   their dependencies by full namespace, and `harness.fixtures` requires
   `harness.call-fixture` purely so the linker can find it — a require that
   appears nowhere in code, and that a lint-driven tidy-up will try to remove.
   It is load-bearing; the reason is recorded at the require.
3. **`dsp/defn` bakes its descriptor at compile time**, so a moved definition is
   not in effect until it is reloaded — and clj-reload captures its baseline on
   first use, so files edited *before* that must be touched to be seen at all.
   Those two together made a correct edit look like a live bug for two runs.

---

## 2026-10-18 — P10a — The sequencing runtime: description, voice pool, allocation (P10 partly complete)

P10 is the largest phase left, so it is cut into slices. **P10a** is the
runtime's foundation and the whole of it is `PLAN.md`'s second layer:

* `omkamra.dsp.synth` (.cljc) — the description: a closed schema, validation,
  note-to-frequency tuning, and the **runtime configuration**, which is pure data
  (keywords, numbers, strings, nil) and therefore portable between hosts;
* `omkamra.dsp.synth.pool` (.cljc) — the live pool: preallocated voices,
  deterministic allocation and stealing, the note lifecycle, and block rendering,
  behind one small `Kernel` protocol;
* `omkamra.dsp.synth.host` (.clj) — the JVM and reference-interpreter adapters,
  and `compile-synth`, which compiles the voice artifact and derives the runtime
  configuration *from that artifact's own ABI*.

No language, IR, ABI, or backend surface changed. A voice is an ordinary
generator process (D23) and per-voice state is the artifact's own instance state
(P8), so a pool of N voices is N instances of one compiled program, and a target
that can already run a process can already host a pool. The compiler is untouched.

**What the pool guarantees.** Allocation is the lowest free voice. Stealing
prefers a *releasing* voice to a sounding one, then applies the declared key
(oldest note-on, lowest note, highest note), and the lowest index breaks a tie —
so the sequence of events, and nothing else, determines which voice is taken. A
stolen voice is reset, so it is indistinguishable from a fresh one. `note-off!`
releases every voice sounding that note, which is what makes a repeated note-on
followed by one note-off behave as a player expects, and an unmatched note-off is
ignored rather than an error. All of it is documented, tested, and checked
against a twin.

**The oracle for a runtime, not just for a kernel.** The same pool runs over the
reference interpreter (`interpreter-kernel`), so the JVM pool's output is
compared sample-for-sample against the interpreter's, bit for bit, over a
sequence that includes two voices, a release, and a third note. Mixing is checked
against the sum of voices rendered alone, and rendering a block is checked to be
invariant under partitioning it into smaller blocks (one 128-frame block equals
two 64-frame blocks) — the same invariance P9 required of automation.

**Three findings worth keeping.**

1. **`dsp/reset!` allocated about 50 KB a call.** `reset-state!` re-derives the
   initial state from the IR with a `doseq` over lazy seqs. Nothing had noticed
   because a reset was assumed to be a setup-time operation — but a voice *steal*
   is a reset, so P10a is the first caller that needed one in an audio path. A
   reset is now a snapshot copy (D32), which is allocation-free and faster.
2. **An un-inferred value at `aset` is a reflective `RT.aset` call.** Rewriting a
   `case` as a `cond` in the control writer left the compiler unable to infer a
   primitive, and `aset` silently became reflective: about 24 KB per control, per
   note (D26's class of mistake). Nothing failed; the *allocation test* is what
   found it, and only after reflection warnings were switched on did the compiler
   say so outright. **Keep `*warn-on-reflection*` on across reloads.**
3. **Clojure cannot implement a primitive-hinted protocol method.** `reify` and
   `deftype` both refuse to implement one ("Can't find matching method ... leave
   off hints for auto match"), so primitive arguments at a protocol seam box. That
   is why the pool's seam costs a few hundred bytes per block and cannot be made
   exactly zero without dropping to a whole-block call or a generated interface
   (D31).

**A measurement lesson, learned the hard way.** Thread-allocation numbers were
taken without warming past the JIT's compilation tiers, first on freshly reloaded
classes. A cold measurement read **296 KB** per cycle where the steady state is
**~690 bytes** — a 400x error, and it was stable enough to look real. Worse, it
supported a wrong conclusion: I attributed the cost to `case` boxing and rewrote
working code on the strength of it, *introducing* the reflective `aset` above.
The real, compiler-reported boxing was the `steal-index` loop argument, and the
`^long` return hint is what fixed it. **An allocation assertion must warm, and a
cold number is not evidence.**

**Not in P10a.** The monophonic and unison modes (which are refused rather than
accepted-and-ignored), per-voice and global modulation routing, voice and effect
tails, stereo spreading, and the CLJS/AudioWorklet host. The pool is `.cljc` from
the start and its logic is host-neutral, but only the JVM and interpreter adapters
exist so far.

**The open question P10c inherits, with the evidence already gathered.**

Voice tails need the kernel to tell the host that a voice has finished, and the
reconnaissance says that is the one genuinely new mechanism in P10:

* a **JS or Wasm instance does not expose its state** — the JavaScript kernel
  closes over its arena and returns only `{process, init, reset, destroy}` — so
  reading a voice's envelope out of its state array is JVM-only;
* a **multi-channel process replicates** its body per channel rather than
  carrying a different signal per channel, so a second "activity" output cannot
  ride alongside the audio;
* the **output buffer** is therefore the only channel every target already
  writes, and the `:lanes` region is the one structure that is *already* a
  host-supplied flat region with a compile-time stride — which makes a symmetric
  *output* status region, or a declared state readback, the two candidates.

Whichever is chosen, P10a's only assumption is that a released voice is
expendable: a pool of capacity C behaves correctly under any number of notes
because stealing prefers released voices.

---

## 2026-10-18 — P10b-i — Voice modes (D33)

The four missing modes are in, and **no compiler surface changed**: a mode is a
policy the pool applies to the same compiled voice, so P10b-i touches only
`omkamra.dsp.synth` (the vocabulary and its validation) and
`omkamra.dsp.synth.pool` (the dispatch), and adds tests. The five modes now
differ observably, and every difference is asserted:

* `:poly` — unchanged from P10a (lowest free voice, else the declared steal).
* `:mono` — one voice; every note-on retriggers it; a note-off releases only
  when the sounding note is released, so there is no fallback.
* `:legato` — one voice; an overlapping note-on takes over the pitch while the
  envelope keeps its level; a release falls back to the previously held note.
* `:retrigger` — the same note stack as `:legato`, but every note-on retriggers.
* `:unison` — the whole pool sounds one note, each voice detuned by a symmetric
  spread in cents, so the centre of the pool stays in tune.

**The one new structure is a bounded held-note stack.** `:legato` and
`:retrigger` need to know which notes are down, in order, to fall back on a
release; the stack is sized by a declared `:held-notes` capacity, lives in
primitive `long` arrays beside the pool's other state, and forgets its *oldest*
note when full — the drop-oldest discipline D25 already chose for the ingress
queue, rather than growing without bound. Retriggering is a reset (D32), so it
restarts the oscillator phase and the envelope from zero; a click-free "attack
from the current level" would need a trigger input the kernel does not have and
is deliberately not claimed.

**How the modes are checked.** State transitions are read through
`voice-snapshot`, and the legato/retrigger distinction — which is *only* audible
— is checked by rendering: a retriggered voice starts its block from zero while a
legato voice keeps sounding, and the two blocks must differ. Every mode is also
rendered on the JVM **and** the reference interpreter, so a pitch change made by
falling back is exercised through the kernel seam as well as the pool.
`omkamra.dsp.tests.synth` is now 19 tests / 118 assertions; the full JVM suite is
104 tests / 1113 assertions, all green.

**Not in P10b-i.** The per-voice **pan** offset that unison and stereo spreading
both want, because `:channels` is still fixed at 1 (P10d). Release tails,
because a released voice is not freed until the kernel can report that its
envelope has finished (P10c) — so a capacity-1 pool retriggers its releasing
voice, which is documented behaviour rather than a defect. And modulation
routing, which is the rest of P10b.

---

## 2026-10-18 — P10b-ii — Modulation as an example instrument (D34)

Modulation is delivered as `omkamra.dsp.examples.demo-synth`, an example
instrument, rather than as a core mechanism. The distinction matters: `PLAN.md`
lists modulation routing as a sequencing-layer *goal*, and there are several
non-equivalent ways to reach it (a declarative route table, a per-target
callback, routing in DSP source, a first-class modulation signal). Freezing one
of them into `omkamra.dsp.synth` now would presume additive
`base + depth × source` routing and a host-side matrix before any consumer has
asked. So the example is where the experiment lives.

**The one thing core gained.** `omkamra.dsp.synth.pool/set-voice-controls!`
publishes a `double` array in ABI order to a voice. It is a *delivery channel*,
not a routing model: every design above needs the host to be able to write a
voice's controls, and this is the smallest affordance that allows it. The plan is
still the default a host may compose with, override, or ignore.

**What the example demonstrates.** A voice (band-limited saw → multimode biquad
→ exponential ADSR) and a route table,
`{:source ... :target :control :depth n}` applied as `base + depth × value` with
an optional `:cents` multiplicative form for pitch. Sources are per-voice
(`:velocity`, `:note`, the latter in octaves from middle C) or global
(`{:controller n}`, `:pitch-bend`). The demo **owns its control values** — it
tracks note, velocity, gate, and frequency by the voice index `note-on!` returns,
which is what a routing layer is — so its render loop allocates nothing and does
not need the pool to store per-voice values. It is checked two ways: the pool
primitive directly, and the instrument end to end (velocity, mod wheel, and pitch
bend each change the output, and the interpreter host renders the same samples).
`omkamra.dsp.tests.synth` is now 21 tests / 128 assertions; the full JVM suite is
106 tests / 1123 assertions, all green.

**Patterns the example exposes, for inspection before anything is promoted.**
These are observations, not proposals; each would be a decision of its own:

1. **`note-off!` reports a count, not the voices it released.** The demo mirrors
   the release in its own arrays to know which gates to drop. A core API that
   reports the released indices — into a preallocated buffer, so the audio path
   stays clean — would remove the mirroring entirely.
2. **Reading a voice without allocating.** `voice-snapshot` is diagnostic and
   allocates; the demo sidesteps it by owning its state. Either the pool offers a
   preallocated per-voice read, or host-owned control state is the intended shape
   and the pool should say so.
3. **Control-name resolution.** The demo rebuilds a name→position map from
   `:control-plan` in every instrument. That mapping looks like it belongs beside
   the plan rather than in each host.
4. **A global source bank.** The demo invents `controllers` and `bends` arrays.
   The scheduler already carries these values as events; a single host structure
   both could share is plausible.
5. **The instrument lifecycle.** "Compile a description, build a pool, allocate
   the host's arrays" is boilerplate every host will repeat. A core synth
   *instance* object is the obvious candidate, once a second instrument shows the
   same shape.

### Pitch moved into the DSP (D35)

The first thing the inspection produced was not a promotion but a **direction**,
and it came from a question the example raised: where does pitch bend live? The
answer, prototyped here and then promoted, is the kernel. A voice takes `note`,
`bend`, and `detune` (cents) and converts them to Hz in the DSP language, per
sample and inlined; the runtime never computes a frequency, and pitch bend is not
a route.

**Stage (a), then the promotion.** First the conversion was the example's own
`dsp/defn`; it is now the library default `omkamra.dsp.library.pitch`
(`pitch->frequency` over `cents->ratio`), rendered into `LIBRARY.md` like every
other module. Then the standard vocabulary moved into core: `:bend` and `:detune`
are in `control-sources`, `:bend-range` (cents) is a description field, the pool
has a **global source bank** (`set-pitch-bend!`) and a per-voice detune, and
`read-controls!` was added as the read half of `set-voice-controls!`. Unison's
spread is delivered as `:detune`. With that, the example stopped re-implementing
the plan: it reads the pool's values, adds its route deltas, and publishes them
back.

**What protects the decision** is a differential check: the compiled
`pitch->frequency` must match the host's `note->frequency`, note for note (it
does, within f32), and 200 cents must be two semitones. If the two drift, the
test says so.

Stage (b) — a transformer that bakes a named conversion into the voice, defaults
with one option and any piece overridable (a plain Clojure function over a
`dsp/defn` value, not a new DSL) — stays a P11 candidate; it is mostly sugar but
also derives the ABI.

**A language constraint the promotion surfaced.** The DSP arithmetic operators
are **strictly binary**: `(+ a b c)` is an error, not a three-operand sum. The
first version of `pitch->frequency` used one and was rejected at lowering with
`DSP binary operators require two operands`. Any n-ary arithmetic must be nested;
the compiler's message is exact and the fix is mechanical.

### State layout was undersized for nested composition (found by D36, fixed)

Building the `unison` transform (D36) surfaced a **pre-existing compiler bug**:
the state arena is sized too small when a process calls a composite process whose
body feeds a record-state module a *product* of two stateful calls. The generated
program compiles for the interpreter and JavaScript but cannot *run*, and the JVM
backend fails at class definition.

A hand-written wrapper reproduces it, so it is not the transformer:

```clojure
(dsp/defn w-voice [gate note bend detune cutoff resonance attack decay sustain release]
  {:process {:input false}}
  (omkamra.dsp.examples.demo-synth/demo-voice
   gate note bend detune cutoff resonance attack decay sustain release))
```

- interpreter: compiles, then `dsp/process!` throws
  `Index 3 out of bounds for length 3` (from `interpreter.cljc` `state-value`);
- JVM: `dsp/compile` throws `VerifyError: Bad type on operand stack` (or an NPE
  inside `insn`);
- JavaScript: compiles — the layout error is not exercised at compile time.

Narrowing, all with `:process` entries over the same voice core:

| wrapper body | result |
|---|---|
| `(saw-oscillator f)` | ok |
| `(biquad (* adsr saw) cutoff q)` | ok |
| `(biquad-multimode adsr cutoff q 0)` | ok |
| `(filter-voice gate cutoff q …)` | ok |
| `(biquad-multimode (* adsr saw) cutoff q 0)` | **fails** |

So the trigger is the combination of a record-state module, a product of two
stateful calls as its sample, and one more level of composition. `biquad` and
`biquad-multimode` share the `biquad-state` record, so the difference is the
multimode body — the `:int` `mode` control and its `if` branches — which points at
the interaction between `if` scoping (P7c), state partitioning, and inlining.

**Root cause and fix.** The linker builds `:definitions` by `into` an
`array-map`, but past **eight** entries an array-map becomes a hash-map. The IR
lowering then iterated that map (`lower-definitions`, `module-state-sizes`), so
the dependency order was lost exactly when a unit grew past eight definitions —
which is why `inner-pf` (8 definitions) worked and `outer-pf` (9) did not.
Lowering and state sizing now iterate the linker's authoritative `:order`
(`ir/dependency-order`). The regression test
`nested-composition-keeps-dependency-order-past-eight-definitions` builds a
nine-definition unit and asserts the wrapper's arena equals the callee's; its
teeth are real — the raw map order puts the callee *after* its dependency.

**Why it mattered beyond transforms.** The same lost order sized the state
arena, and JavaScript compiled the undersized arena **without complaint**, so
any composition past eight definitions was silently wrong on that target.

### Unison moved out of the pool (D36, done)

The transform is validated and the pool mode is gone. **Removed**: `:unison` from
`voice-modes` and `mode-codes`; `:unison-detune` (description field, validation,
config); `mode-unison` and `unison-note-on!`; the one dispatch branch; and
`sound-voice!`'s `detune` parameter (a fresh voice is in tune). **Kept**:
`:detune` as a standard host source, now produced by `set-detune!` (per voice),
with `sound-voice!` setting each voice to zero.

**The oracle changed, for the better.** The transform test no longer compares to
the pool mode (which is gone); it asserts that the generated voice equals the sum
of N separately-rendered detuned copies, and the match is **bit-exact**. Writing
it surfaced a trap worth keeping: `pool/render!` does **not** re-resolve
controls, so `set-detune!` after `note-on!` has no effect until controls are
written again — the oracle therefore sets the detune in the description. (The
demo's own layer sidesteps this because it re-writes controls every block, via
`read-controls!` + `set-voice-controls!`.)

### `dsp/fn`, and control modes as transformers (D37, step 1)

Two things landed, and they are the same idea: a definition is a value.

* **`dsp/fn`** is the anonymous counterpart of `dsp/defn` — the descriptor
  returned as a value instead of bound. `dsp/defn` collapsed to
  `(def name (capture …))`; both share a new private `capture-definition`, so
  there is one capture path. The value carries a generated internal `:name` and
  the defining namespace's source, so it links directly. `dsp/fn` shadows
  `clojure.core/fn` in `omkamra.dsp`, so every `fn` inside that namespace is now
  `clojure.core/fn`, and `fn` is in `:refer-clojure :exclude`.
* **`smooth`/`slew` transformers** in the example. They only *set metadata*
  (`:smooth`/`:slew` on a control) and let `descriptor/normalize` desugar it,
  exactly as the inline declaration does. So the inline form and the transformer
  are one mechanism, and the transform works on anonymous `dsp/fn` values.

`(def voice (dsp/fn …))` and `(-> (dsp/fn …) (smooth :cutoff 0.01))` both work.

**Checks.** `dsp-fn-is-an-anonymous-definition-value` (the value carries a
raw `:name` and the defining namespace, and renders); and
`control-transforms-match-the-inline-declaration` (the transform and the inline
`:smooth`/`:slew` render **bit-identically** on the interpreter and the JVM,\nthe plain voice does not, and an unknown control is refused). JVM suite is 112
tests / 1152 assertions; the browser matrix is 19/19.

### Step 2 — call-emitting transforms on anonymous values (D37)

`unison` emits calls to the definition it wraps, so a fresh anonymous `dsp/fn`
value had no Var for the linker to resolve. The fix is *value-carried
dependencies*: a call-emitting transform returns a **derived definition** — the
example's `derived-definition` — which keeps the wrapped voice's interface
(parameters, return type, process declaration, options, source), takes a
**fresh internal name**, and records the original under its fully-qualified id
in `:dsp/dependencies`. `dsp/link` gathers `:dsp/dependencies` transitively from
the entry and any supplied `:definitions`, and passes them to the linker as
supplied definitions.

The fresh name is load-bearing. The derived body calls the original, so keeping
the original's id would be a self-call; reusing it as the entry id also collides
with the carried dependency. The helper carries *all* interface fields rather
than enumerating them, so a new interface field needs no transformer change.
Two smaller pieces: the descriptor schema admits `:dsp/dependencies`
(`normalize` still drops it, and `link` reads the raw value before normalizing),
and the `fn`-shadowing trap recurred — the new helper first used unqualified
`fn`, which `dsp/fn` turned into a descriptor and `reduce-kv` then tried to call
as a function; it is `clojure.core/fn` now.

**Checks.** `a-call-emitting-transform-links-an-anonymous-value`: an anonymous
voice transformed with `unison` links (its `:definitions` holds the carried
original) and renders the summed detuned copies bit-for-bit on the interpreter
and the JVM. `transforms-compose-and-carry-dependencies-transitively`: `smooth`
∘ `unison` keeps the control mode and links; `unison` ∘ `unison` carries a
three-definition chain; both render. The named unison oracle still passes. JVM
114 tests / 1163 assertions; browser 19/19; kondo/cljfmt clean.

That closed D37. The helpers have since graduated to core: `omkamra.dsp.graph`
holds the substrate and `omkamra.dsp.combinators` the combinators, with `share`
as the last of the tree-expressible set. The next step is the D36 inventory.

---

## 2026-10-18 — D38 — Clojure assembles the static graph; the engine owns dynamic instances

The transform work landed on a principle, now written down as D38: an audio
graph is assembled in ordinary Clojure and crosses into the compiler exactly
once — *build → optimize → lower → link*, with the linker as the boundary. A
definition value *is* the fixed expression, so combinators are plain functions
and `->`/`reduce`/`map` compose them.

Two things make it concrete. First, the fixed expression already is a graph; the
one thing a tree cannot express is **fan-out**, because state is per call site
(D19), so writing a node twice makes two independent instances. A graph
description therefore earns its existence by **sharing** — one stateful node for
several consumers — plus room for optimization passes. Until a combinator needs
fan-out, the definition-value combinators are the graph; `share` is the forcing
function. Second, **optimizing transformers are data → data rewrites** that
choose among lowerings the system can already express: constant folding, dedup
of *stateless* blocks (never stateful ones), dead-node elimination,
unroll-versus-reuse, and backend code-dedup. Rewrites such as "fixed-N poly →
runtime loop" need arrayed state we do not have, and "unrolled voices → one
kernel, K instances" *is* the pool. An optimizer removes cost the naive shape
does not need; it cannot invent a lowering.

This round also added two combinators: `sum` (`reduce` over `mix`) and `poly`
(fixed-N, with a per-slot copy of every control via a `derived-definition`
override). `poly` is the *static* half of polyphony — the pool is the dynamic
half — so it is a node with a lowering policy, not a peer of `unison`/`mix`.
`three-voice-poly` matches `three-voice-poly-by-hand` bit-for-bit on a three-note
chord, and `three-oscillator-by-sum` matches `three-oscillator`.

**Checks.** `a-fixed-n-poly-voice-has-independent-slots` (the chord matches the
hand-written sum) and the extended mix test (`reduce` over `mix` agrees). JVM
117 tests / 1173 assertions; browser 19/19; kondo/cljfmt clean.

### Hoisting: computed arguments and combinator controls

Duplicating a voice should not duplicate its parameters. Investigating found two
things. Parameters are already top-level (they are the definition's params), so
the duplication is not there; it is in the **inliner**: it substitutes a call's
argument expression into the callee body once per use of the parameter. A
callee that uses its parameter twice therefore duplicated a computed argument —
`(tap (+ a b))` with `(defn tap [x] (* x x))` lowered to `(* (+ a b) (+ a b))`.
Worse, a *stateful* argument duplicated this way ran twice against one state
partition, so a generated target diverged from the interpreter (which does not
inline). `lower-call` now binds every computed argument to a fresh local, so the
inliner substitutes a local read; `a-computed-argument-is-evaluated-once` pins
interpreter-equals-JVM for a stateful argument used twice.

At the combinator layer, `poly` now hoists the **shared** controls and expands
only the **per-slot** ones (`:per-slot`, default `[:gate :note]`): a
`[gate note detune]` voice becomes `[detune gate_0 note_0 gate_1 note_1 ...]`,
not three copies of every control.

That also corrects D38's premise. The fixed expression has `let`, so read-only
fan-out is expressible without any graph IR; a `share` combinator would be a
code-generation pattern, not a new layer. The graph description is now justified
only by optimization/inspection, or by shared *mutable* state (which is a record,
not a value).

**Checks after hoisting.** JVM 118 tests / 1174 assertions; browser 19/19;
kondo/cljfmt clean.

### `share`, and the optimization stage

`share` landed as the combinator form of `let`: it binds the voice's output once
and hands it to a builder, so several consumers read one instance instead of
creating one call site — and therefore one state partition (D19) — each. The
demo keeps the signal identical while the duplicated version owns more state
slots, which is exactly what the combinator buys.

This is the point where the compiler needs an **optimization stage**, and it is
already tracked (P15 deliverable 4, `PLAN.md` §9). What is new is the concrete
candidate list, now recorded in D38: deduplicate identical inlined bodies
(unrolled voices emit K copies through `inline-calls`), fold **stateless** common
subexpressions only, and note that argument hoisting is the first piece and is
already in the lowerer.

---

## 2026-10-18 — P10c — Voice tails and containment (completed)

Tails turned out to hinge on one design question: how does the pool know a voice
has finished? Audio level is not an answer — a delay pre-roll, an LFO gate, a
reverb, or a self-sequencing voice can be silent for seconds and then sound — so
the signal must be *declared*, not inferred (D39). The default is unchanged and
safe: a released voice is kept until it is stolen (it is exactly the voice
stealing prefers), and tails already sound because `render!` mixes every
non-`free` voice.

The enabling piece landed first: a **scalar state slot is now body-writable**.
`set!` on a declared scalar state lowers to the same `:state-store` an aggregate
slot already used. That is the general mechanism that was missing — persistent
state could previously only be computed through `:next`, which cannot see body
locals, so a library ADSR's output could not become a flag.

On top of it, `store-special` and the builtin `:activity` slot:

* `(store-special :activity value)` writes a builtin, host-readable slot.
* `effective-state` scans the body for `store-special` ids and adds the slot
  *before* `expand-state` numbers slots, so it gets an ordinary index and
  `module-state-sizes` (the arena sizing that must agree with lowering) counts
  it. No dynamic mid-lowering append, so the ordering bug class from earlier
  cannot recur.
* The process ABI exposes `:special-slots {:activity {:name :type :index}}`.

One gotcha: `store-special` had to be added to the linker's `language-forms`
(and the IR's operator set), otherwise the linker treated it as a call to a
missing definition `store-special`.

**Checks.** `a-body-can-write-a-scalar-state-slot` (a counter written and read
back across frames, interpreter and JVM),
`a-builtin-special-slot-is-writable-and-exposed` (the round trip plus the ABI
shape), the end-to-end gate: `a-declared-activity-frees-a-released-voice`,
`a-voice-without-activity-stays-until-stolen`, and
`a-declared-activity-defaults-to-active`, and containment:
`a-non-finite-voice-is-contained`, `a-runaway-magnitude-is-clamped`, and
`containment-can-be-disabled`. JVM 129 tests / 1215 assertions; browser 19/19;
kondo/cljfmt clean.

**Readback and gate.** `Kernel/read-specials!` fills a *preallocated* double
array from the voice's state, so the audio path still allocates nothing. Each
kernel resolves a special's logical index to a **physical array slot once, at
construction** (like `control-slots`), so the audio path never re-enters a map —
this replaced a `(get state-slots logical)` hash lookup per block. Specials are
encoded as a **mask** with fixed bits (`:activity` = 0), and each definition's
specials occupy its **first** state slots in ascending-bit order; because
activity is the lowest bit, its readback position is 0. The ABI exposes
`:special-mask` and `:special-slots {:activity {:name :type :bit :index}}`; the
runtime config carries the mask and the count. A callee's `store-special` is
rejected at lowering (its slot would be buried in a call partition). The pool
resets and frees a *releasing* voice when `:activity` reads <= 0. Two gotchas:
the voice map nests the instance, so `:state` lives at `(:state (:instance
voice))`; and releasing a voice resets it first, because `sound-voice!` treats a
`free` voice as fresh and would otherwise reuse stale state.

**Containment (D39).** A kernel policy, not a compiler one. `:reset` (the
default, per `PLAN.md`) scans the block and, on a non-finite sample, silences it
and `reset!`s the voice so it recovers, bumping a `:containment` pool counter;
`:sanitize` instead contains each sample before summing (non-finite -> 0, a
finite runaway clamped to `1e6`) but does not reset, and is the only policy that
clamps *finite* runaways; `:off` runs the plain loop. Traps: a boxed `contain`
allocated a Double per sample (the zero-allocation test caught it, so it is
primitive-hinted), and Clojure's `(float Inf)` *throws*, so the f32 mixing path
uses `unchecked-float`. The policy branch is per-block (like `f64?`), never
per-sample.

**Remaining.** The JS/Wasm `read-specials!` and containment, with the P10d host.
(This is host code: the compiled voice never contains containment.)

---

## 2026-10-18 — P10d — The JavaScript target emits through Squint (D41)

**Delivered.** The JavaScript backend no longer builds JavaScript text. The
portable IR still lowers to the same JS-shaped IR, but emission now builds
*ClojureScript forms* — plain data — and hands them to `squint.compiler`, which
owns the syntax: statements, `let` ordering, operator precedence, and name
mangling. The emitter decides only the structure of the module, and `js*` is left
to the handful of spellings that are the point (`Math.fround`, `| 0`, `break`,
`continue`, `NaN`, and the array and kernel literals).

**Why.** The string emitter had to get precedence, temporaries, the
statement/expression split, and `let` ordering right by hand, and the result was
only checkable by asserting on the text it produced. With Squint owning the
syntax, the emitter is form construction — the same "assemble the graph in
Clojure" D38 asks for — and the forms are data, so the path to `PLAN.md` §4's
browser-side compilation opens up. Squint was already a dependency (the worklet
adapters use it).

**Four rules, each enforced at run time rather than compile time**, so each is
what a change here has to respect:

* a local that is `set!` must carry `^:mutable`, or Squint emits `const` and the
  assignment throws while audio runs;
* `break` and `continue` have no ClojureScript form, so they are `js*`;
* `js*` never names a local — Squint mangles locals — so values are spliced in;
* nothing may lower to a `squint_core` reference, because a kernel is installed
  as one standalone script.

The last is the subtle one, and it took a while to pin down. Squint calls its
standard library for any value it cannot prove is a boolean: `and`/`or`/`not`
become `squint_core.truth_`/`squint_core.not`, `=` becomes `squint_core._EQ_`,
and an `if`/`while` condition it cannot prove boolean is wrapped in `truth_`.
`^boolean` hints are *not* the answer — they make `and`/`or` lower to a
value-preserving ternary, which then needs `truth_` in a condition. What works is
to stay inside what Squint already recognises as boolean: the native comparisons
(`<`, `<=`, `>`, `>=`, and `==`), with `:not=` written as `(not (== a b))` — that
is `!(a === b)`; `and`/`or`/`not` only over operands that are provably boolean
(anything else is compared with `true`); and a `^boolean` declaration for a
boolean argument or local, so that `(not (not x))` is `!!x`, which is how a
boolean argument is normalised at the boundary — an `AudioParam`, or a pool's
control array, arrives as a number.

**Verified.** Node runs the generated module for `one-pole` (mono, stateful) and
`stereo-cross` (two channels) bit-identically to the interpreter. The browser
conformance suite — every catalog case, f32 and f64, on JavaScript and Wasm —
passes 19/19. `tests/js.clj` now asserts the module contract (the export shape,
the physical type, and no `squint_core` over the whole catalog) instead of the
emitted text, and four older tests that pinned JavaScript text were rewritten to
pin the target-level shape or the ABI (`ir.clj`, `process.clj`, `memory.clj`,
`automation.clj`). JVM 137 tests / 1437 assertions.

**Consequences.** The kernel object exposes its state arena, so a host can read a
voice's special slots (D39) without a second entry point. The Wasm target is
untouched — WAT is still hand-emitted — so the two native backends now differ in
authoring style.

---

## 2026-10-18 — P10d — The browser host and the worklet boundary (D40, slice 3)

**Delivered.** A pool runs in the browser over a generated JavaScript module. The
kernels moved out of `omkamra.dsp.synth.host` into an `omkamra.dsp.hosts.jvm` family
— `.jvm`, `.interpreter`, `.js`, with `.wasm` still to come — and the plumbing
every one of them shares (the containment policy, and the planar blocks a voice
is rendered into and mixed through) into `omkamra.dsp.kernels.mixing`. `mixing` is
`.cljc`, so the JVM suite and the browser testbed run the *same* reference oracle
rather than two copies of it. `omkamra.dsp.hosts.jvm` itself is the assembler:
`synth-bundle` compiles a description and derives its runtime configuration from
the artifact's own ABI, and `compile-synth` adds a JVM-side kernel. A browser host
builds its own from the bundle, which is what makes the browser check a
differential test rather than a comparison against shipped vectors.

**The browser check.** `omkamra.dsp.harness.testbed` fetches a compiled voice
(`/synth-bundle`: runtime, ABI, and interpreter IR; `/synth-source`: the
JavaScript module) and plays one note through two pools that differ only in their
kernel — one over the module's `createKernel`, one over the interpreter — then
compares the rendered block and asserts the released voice was freed. That single
check covers what a browser kernel has to provide beyond rendering: two planar
channels, per-frame shared state, a host-readable special slot (`:activity`), and
containment.

**The worklet boundary.** The adapter used to call the kernel once per channel.
With one evaluation per frame (D40) the kernel indexes its own channels, so the
adapter now calls it **once** and hands it the callback's own arrays — one channel
array for a mono kernel, the array of them otherwise — and silences the channels
the kernel does not write. The kernel's shape (`channels`, `inputChannels`)
travels in processor options from the ABI. Nothing is up-mixed any more: placing
a mono voice in a stereo field is the voice's business (`effect/pan`), exactly as
slice 2 decided. Keeping the one-pole worklet check honest meant stating in the
page that its kernel is mono with one input channel, while the callback still has
two output channels.

**Verified.** Browser 20/20 (the new `JavaScript pool host` among them); JVM 137
tests / 1437 assertions; `clj-kondo`/`cljfmt` clean.

**The Wasm host.** `omkamra.dsp.kernels.wasm` is the same kernel seam over a Wasm
module, and is where the two browser hosts genuinely differ: a Wasm kernel is not
handed the block. `process` takes the byte offset of its output region and writes
channel `c` at `offset + c * channel-stride` in its own linear memory, so a
voice's scratch is a set of typed-array views over its instance's memory and
mixing is arithmetic rather than a copy. A Wasm instance owns its memory, so a
voice *is* an instance — `create-voice` instantiates the module — and
`read-specials!` reads each special out of the state region, where element `index`
sits at `offset + index * element-bytes` at both widths (D39). The worklet
adapter calls the export once per block and copies each channel out of memory
into the callback's arrays, silencing channels the kernel did not write.

**P10 is complete.** The pool, the four kernel hosts, the browser checks, and the
worklet boundary are all in place; what remains for the reference instrument is
P11.

---

## 2026-10-18 — P11a — the reference instrument (completed)

The P11 acceptance fixture begins as `omkamra.dsp.examples.reference-synth`: a
stereo voice assembled from library blocks and combinators (D38) rather than
written as one voice, plus a host-side modulation matrix.

* **The voice.** `oscillator-bank` (mono, unison-able) blends a band-limited
  `saw-oscillator` and a band-limited PWM `pulse-oscillator` with a
  `sync-oscillator`, all driven by one LFO (vibrato in cents and PWM depth);
  `unison` wraps it with three detuned copies at half gain. `reference-source`
  adds white noise, an amp ADSR, a filter ADSR opening a resonant
  `state-variable-filter`, and `distortion`. `reference-voice` pans the result
  into two output channels with `effect/pan`.
* **The matrix** is deliberately host-side and lives here, not in core (D34):
  rows of `{source -> [{:target :depth}]}`, sources `:velocity`, `:note`, and
  MIDI controllers, applied as `base + depth x source` on top of the pool's
  plan. It is studied at the end of P11 for promotion.
* **The host is portable in shape** — `instrument` wraps a live pool and keeps
  only primitive arrays — but the namespace is `.clj` for now; P11c extracts the
  portable half for the browser.
* **Tests** (`omkamra.dsp.tests.reference`, 8 tests): interpreter/JVM
  bit-identity over a phrase with a controller change, every matrix source
  changes the render (with an identical-repeat negative twin), stereo panning,
  block-size invariance, stealing, unison structure, and a zero-allocation
  guard for the whole host path.

**The allocation guard found a real bug in the P10d stereo mixing path.**
`omkamra.dsp.kernels.mixing/channel-of` did `(aget buffer channel)` with no type
hint, so selecting a plane of a planar block was a *reflective* `RT.aget` —
about 24 KB per channel per block on the JVM. It never showed up because the
mono path does not index, and no allocation check covered stereo. P11's
instrument is stereo, so the guard caught it: ~195 KB per block before, ~2 KB
after.

**Remaining P11.** Post-mix effects and effect tails (P11b, ideally as a
transform, possibly needing a new `dsp/def*` form), all-backend offline
rendering (P11c), and the property battery (P11d).

---

## 2026-10-18 — Interpreter allocation pass (P11 prep)

The suite had grown to ~46 s, and the interpreter — the oracle every conformance
case runs — was the reason. A profile (`clj-async-profiler`, `:event :alloc`, on
the reference instrument's mono voice) found that *allocation*, not dispatch,
dominated. The pass cut the reference voice's allocation ~22x in the same
micro-benchmark and the existing suite from **46 s to ~24 s**, with no change to
any result.

1. **Reflection (the big one, 88% of samples).** `state-value`,
   `checked-buffer-index`, `context-buffer`, and `:buffer-load` called
   `aget`/`alength` on *unhinted* arrays, which Clojure compiled to a reflective
   `Reflector.invokeStaticMethod` whose `Class.getMethods`/`Method.copy` path
   allocated ~24 KB per call (the D26 class). State now dispatches on a
   `*real-state*` flag (bound per process call) to a primitive `aget`, and
   buffers use precision-typed helpers.
2. **Mutable locals.** Locals were a persistent map in a `volatile!`, so every
   `:declare`/`:assign`/`set!` copied a HAMT. They are now one
   `java.util.HashMap` (JVM) / `js/Map` (CLJS), keyed by the local's *name
   string* — the IR guarantees names are globally unique
   (`ir/unique-local-name`), and string keys are required because `js/Map`
   compares object keys by identity. This also removed the CLJS
   drop-then-assoc dance for signed zero, since a mutable map always replaces.
3. **No per-call temporaries.** `evaluate-call` evaluates arguments straight into
   the callee environment by index, and `seeded-locals` does the same for
   parameters/controls, so there is no argument vector, lazy seq, or `MapEntry`.
4. **Builtin dispatch.** `evaluate-builtin` was a multimethod dispatched on
   `[operator type]` (a vector per call) fed by a `mapv` of arguments (~24% of
   allocation); it is a plain `case` with positional operands.
5. **Precision holder.** `f64?` read a dynamic var, and `Var.getThreadBinding`
   allocated a map entry on every read (~27%). Precision now lives in a
   `ThreadLocal` (JVM) / `volatile` (CLJS) with an explicit save/restore
   (`with-dsp-precision`), equivalent to `binding` without the per-read cost.

The full JVM suite and the browser suite (21/21) validate it; the browser run is
the check that the `.cljc` changes survive ClojureScript. What remains is boxing
(`Float`/`Double`) and the tree-walk itself; a closure-compiling interpreter
(compile IR to fns once) is the next real step, not another allocation tweak.

---

## 2026-10-18 — P11b-i — the instrument graph and the post-mix chain (D42)

P11's effects land as a **Clojure-level instrument graph** lowered to a
**program**, which is the shape the later slices will refine.

**The source layer** (`omkamra.dsp.instrument`, `.cljc`) is a value plus plain
functions: `instrument` (the description's top-level fields), `voice`,
`routes`, and `with-effect`. Nothing there knows about kernels or channel
layout.

**The lowering** (`omkamra.dsp.instrument.program`, `.clj`) has one strategy so
far: the voice goes through `synth-bundle` (the P10 pool), and the post-mix
chain is folded by `chain-effect` into **one mono processor definition** whose
interface grows by each effect's controls. That compiles to one artifact and is
run as **one bus instance per output channel**, after the pool. `compile-instrument`
returns a *program* (`{:bundle, :bus, :routes, …}`); `instrument-pool` builds the
live pool + bus; `render!` renders the pool then the bus in place.

**The reference synth uses it.** `reference-instrument` is now a value:

```clojure
(-> (inst/instrument description)
    (inst/routes modulation-matrix)
    (inst/with-effect chorus ['chorus-rate 'chorus-depth 'chorus-mix])
    (inst/with-effect distortion ['drive]))
```

so the post-mix chorus and distortion are **global** controls the host sets
(`set-effect-control!`), deliberately not per-voice routes. The modulation
matrix stays in the example, as agreed (D34).

**A second reflection bug, found by the bus allocation guard.** The bus first
exposed `dsp/process!` — the *checked* context adapter — and it allocated
~100 KB per call: `check-buffer!`, `check-state!`, and `check-controls!` called
`alength` on unhinted arrays, the same D26 class as the interpreter's. Two fixes:
the checked adapter now measures with a precision-typed `real-array-length`, and
the bus's audio path uses `dsp/process-unchecked!` with preallocated physical
control arrays, exactly as the voice pool does. The bus render went from
~400 KB/cycle to **178 bytes/render**.

**Tests.** `omkamra.dsp.tests.reference` is 12 tests / 37 assertions: the
post-mix chain is audible against the same voice without it, a global control
changes the render (with an identical-repeat twin), there is one bus instance per
channel, and interpreter/JVM agree with the bus controls set. JVM 149 tests /
1474 assertions; browser unaffected (the new layers are JVM-only until P11c).

**Gap.** There is no musical delay yet: the library's `delay-line` is a 64-sample
buffer (a comb/flanger span), used by `chorus`. A post-mix *delay* wants a
larger line with feedback — a small library addition, and the remaining half of
deliverable 2's "delay/chorus/distortion".

---

## 2026-10-18 — P11c — all-backend offline rendering

The instrument host now runs on JavaScript and Wasm, so the reference fixture is
rendered on all four targets and checked for block-size invariance.

**The bus host.** A post-mix bus is a *processor*, so the browser kernels differ
from the pool's voice kernels: `hosts.js/js-bus-kernel` hands the generated
`process` the input array, the output array, frames, sample-rate, and the
controls; `hosts.wasm/wasm-bus-kernel` copies the channel into the module's input
region, calls `process(input-offset, output-offset, …)`, and copies the output
region back. One instance runs per output channel, in place, after the pool.
`hosts.instrument/render!` is the JVM/interpreter side of the same shape.

**The browser check.** `omkamra.dsp.harness.testbed` serves
`instrument-bundle` (voice runtime/ABI/IR plus bus ABI/IR, and the WaT + memory
layouts for Wasm) and `instrument-source` (both JavaScript modules). The page
builds the browser voice pool and one bus per channel, plays a note, and compares
the result against the in-page interpreter doing the same — then repeats with the
held block split into single-frame blocks, so **block-size invariance** is
checked on the browser host too. The fixture
(`fixtures/browser-instrument`) is the stereo voice plus a **chorus and a
feedback delay**, so the check exercises the large-state bus and its tail on
JavaScript and Wasm exactly as it does on the JVM.

**What this covers.** The bus *DSP* is in the shared catalog (`:feedback-delay`)
and so runs on all four targets through the process matrix; the *host
composition* (pool + bus, per channel, per block) is now differential-tested on
JavaScript and Wasm against the interpreter. The full reference instrument's own
differential and block-size check remain on the JVM and interpreter.

**Verified.** Browser 23/23 (the two instrument hosts among them); JVM 150 tests
/ 1486 assertions; `clj-kondo`/`cljfmt` clean. `LIBRARY.md` regenerated for
`feedback-delay`.

---

## 2026-10-18 — P11d — the property battery (P11 complete)

The acceptance properties land, each where it is honestly measured rather than
only at the instrument level:

* **Filter stability under modulation** — the test
  `the-filter-stays-stable-under-extreme-modulation` drives every matrix source
  to its extreme (velocity, note, both controllers, three voices, all released)
  and asserts no sample is non-finite and none runs away. The voice clamps the
  SVF cutoff below a sixth of the sample rate and its damping above zero, which
  is the documented stability bound; this is the end-to-end version of it.
* **Absence of zipper noise** — the voice now declares `:smooth` (D27) on every
  continuous control the matrix modulates (`cutoff`, `resonance`, `pulse-width`),
  with a `:default` so a fresh voice opens at its base settings rather than
  zero. `the-reference-voice-smoothes-its-modulated-controls` is the structural
  guard; the per-frame ramp mechanism itself is tested by the automation suite.
  `pan` is deliberately bare: the pan law is smooth across the field, and a hard
  pan must reach silence.
* **Non-finite containment** — the P10c pool tests cover `:reset`, `:sanitize`,
  and `:off`, and the instrument host passes the policy through to the pool.
* **Zero allocation after initialization** —
  `the-reference-instrument-does-not-allocate-in-the-audio-path` (JVM) plus the
  pool's own guard.
* **Aliasing** — the library's `band-limited-oscillators` test compares each
  PolyBLEP oscillator's largest step against a naive sawtooth's, which is the
  band-limiting property the reference voice inherits.
* **Event-boundary correctness** — the pool lifecycle tests and P9's
  sample-accurate scheduling suite.

**A note on the browser check.** The browser instrument host (P11c) renders a
*fixture* instrument — the stereo voice plus a chorus and the feedback delay —
rather than the full reference synth, which is heavy (unison, SVF, a 48000-sample
delay per channel) and would only slow the suite. The bus DSP is covered on all
four targets through the shared catalog (`:feedback-delay`), and the reference
instrument's own differential and block-size checks run on the JVM and the
interpreter. If the refinement round wants the full reference synth in the
browser, the host and endpoints are already in place; it is a fixture swap.

**Verified.** JVM 152 tests / 1493 assertions; browser 23/23; `clj-kondo`/
`cljfmt` clean.

**P11 is complete** (P11a–P11d). The refinement round the phase was scoped to
leave open — D42's instrument graph, the modulation matrix (D34), and the exact
per-feature layer boundary — is next.

---

## 2026-10-18 — Refinement — a control name is a symbol, however it is spelled (D43)

`with-effect` (and `comb/through`) now accept a keyword or a symbol for the
controls they introduce. `graph/normalize-control-name` folds a symbol, keyword,
string, quoted symbol, or `{:name …}` map to the canonical symbol; its companion
`normalize-control-argument` does the same for a call argument while leaving a
literal or an expression alone. Both are idempotent, and the lowering normalizes
too, so a hand-built instrument value is honest.

The lead-in question was whether keywords read better than quoted symbols;
`'chorus-rate` is source-only syntax that *evaluates* to the symbol `chorus-rate`,
so the instrument value already held symbols. The reason the surface can take a
keyword at all is that the DSP language has **no keyword literal** — `ir` lowers
only numbers and booleans as constants — so a keyword in an argument position is
unambiguously a reference. The reference synth's chain is now spelled with
keywords. `combinators/control-name` is the shared helper; `synth` and
`descriptor` keep their own folds for now (D43 notes them as collapsible).

**Verified.** JVM 154 tests / 1513 assertions; browser 23/23; `clj-kondo`/`cljfmt`
clean. Two new tests: the normalizer's accepted spellings and idempotence, and
that `with-effect` accepts either spelling and lowers identically.

---

## 2026-10-18 — The reference synth on every target, and the SVF stability bound it exposed

**The portable host.** The reference synthesizer's modulation matrix and render
loop moved into `omkamra.dsp.reference` (`.cljc`), shared by the JVM example
(`omkamra.dsp.examples.reference-synth`, which keeps the instrument value and the
JVM constructor) and the browser hosts. The matrix is pure data and the loop
touches only the pool API, so the same host code now runs on the interpreter, the
JVM, JavaScript, and Wasm.

**The browser instrument host.** `/instrument-bundle` serves the full reference
instrument (voice runtime/ABI/IR, the modulation matrix, and the bus
ABI/IR/control names), and the testbed renders the reference acceptance phrase —
note, mod wheel, controller 74, four 64-frame blocks and the same split into
32-frame blocks — through the browser and interpreter kernels with
`:containment :off`. JavaScript matches the interpreter exactly and is block-size
invariant; Wasm runs finite and block-size invariant without a sample
differential, because its approximated `pow`/`sin` drift through the long
resonant voice and no meaningful sample tolerance exists (per-operation
equivalence is the shared process matrix's job).

**The bug it found.** With containment off, the reference voice went non-finite
under a mod-wheel setting (`controller 1 = 0.7`) that raised the cutoff. The
`state-variable-filter` docstring claimed stability below `fs/6`, but that bound
is missing the damping condition: for `f = 2 sin(pi fc/fs)` the Chamberlin SVF is
stable only while `q > f/(2 - f^2/2)`, which at `fc = fs/6` means `q > 2/3`. The
voice clamped damping only to `0.05`, so a modulated cutoff could cross the
boundary. `:reset` containment had hidden it: it silences the *whole block*
containing a non-finite sample, and silence is finite, so
`the-filter-stays-stable-under-extreme-modulation` had been passing vacuously.
The voice now clamps the damping above the bound, with margin, and the reference
tests run with containment off; `build-instrument` threads `:containment`
through to the pool so a test can select it.

**Selective execution.** `browser/run!` and the page's `runConformance` accept a
selection of check-name substrings (`{:only ["instrument"]}`), so one check can
be iterated on without paying for the suite.

**Containment on every target.** The P10c containment fixtures
(`containment-runaway-voice`, `containment-loud-voice`, `containment-reset-voice`
and their descriptions) moved into `omkamra.dsp.harness.fixtures`, shared by the
JVM synth suite and the browser testbed, and the page now exercises `:reset`,
`:sanitize`, and `:off` over JavaScript and Wasm pools. The browser check paid for
itself immediately, finding two real bugs: the JavaScript kernel's `:reset!`
returned `reset` without calling it, so a `:reset` event counted but never reset
the voice's state; and the Wasm emitter lowered integer `min`/`max` to
`i32.min_s`/`i32.max_s`, which are not WebAssembly instructions (the float path
uses the real `f32.min`/`f32.max`). The JavaScript kernel now calls `reset`, and
Wasm lowers integer min/max to `select` with a comparison. An unknown containment
mode is refused by a test too.

**Verified.** JVM 155 tests / 1527 assertions; browser 25/25; `clj-kondo`/`cljfmt`
clean.

---

## 2026-10-19 — M1 — Event-driven host (completed)

**Goal.** Drive the voice pool from timestamped events rather than per-block
polling (M1 of the M workstream; `docs/mvp/MODULATION.md`, D44).

**Delivered (offline).**

- `omkamra.dsp.schedule/render-block!` gained an optional `:dispatch!` hook,
  called with each segment before `:process!`, so a host that consumes event
  types other than `:parameter` can act at the segment boundary. The existing
  single-process path is unchanged.
- New `omkamra.dsp.sequencer` (`.cljc`): a `Sequencer` over a voice pool
  and the existing `omkamra.dsp.event`/`omkamra.dsp.schedule`. `submit!` enqueues
  timestamped events; `render!` cuts the block at every event frame and
  dispatches due notes (`:note-on`/`:note-off`) to the pool at exactly their
  frame. Mono in this slice, because `schedule/render-block!` slices one flat
  output array.
- `omkamra.dsp.tests.sequencer`: rendering is bit-identical across block sizes
  512/256/128/64/37/16/7/1/5/100 for a five-event phrase; a note at frame *f* is
  silent before *f* and sounds from *f*; a one-frame shift moves the onset. The
  negative twin (no events) is all zeros, so the invariance check is not vacuous.

One sequencing layer, not two: a voice pool already carries its per-pool runtime,
so the sequencer owns the one timeline and will route to several pools in M2.
*Performance* is reserved for a future multi-track coordinator.

**Delivered (browser transport).**

- `omkamra.dsp.worker` (`.cljc`) is the **central** worker-side runtime:
  it validates a `[frame value]` timeline through `omkamra.dsp.event` and posts
  timestamped events on a `MessagePort`. The script-to-events logic is portable;
  only the worker plumbing is ClojureScript. The browser testbed is one consumer.
- The worklet adapter (`worklet_adapters/js.cljs`) gained a preallocated event
  ingress queue and sub-block rendering: for a mono kernel it cuts each block at
  the buffered event frames and applies each control value at its exact frame; a
  control an event has set persists across blocks. With no events the block is
  rendered in one kernel call exactly as before.
- The browser check `worker event transport`: a real Web Worker posts three
  timestamped events over a `MessageChannel` to an `OfflineAudioContext` worklet
  running the `follow-amount` kernel; the rendered block steps at frames 17, 40,
  and 155. (Two adapter bugs it caught: absolute event frames were used as
  block-relative array indices, and AudioParams reset event-set controls every
  block.)
- The Shadow build `:omkamra.dsp.worker` points `:init-fn` at the runtime
  (build id and paths mirror the namespace).

**Browser build note.** The worker is a separate, self-contained Shadow build
(`:devtools {:enabled false}`), not a web-worker module of the page build. The
documented `:shared`/`:main`/`:web-worker` pattern compiles in development but the
worker fails at run time: Shadow's dev module loading emits
`shadow.loader`/`module_loaded` append files that need the `shadow` runtime, which
a web-worker module never gets (the default module hosts it). Release generation
is unaffected. The runtime stays a library namespace; only the build is separate.

**Two `omkamra.dev` bugs found and fixed.**

- `frontend.shadow/browser-build` silently dropped any browser build with more
  than one module — it handled only the single-module case. It now selects the
  page module by Shadow's own rule (excluding web workers), honours
  `:omkamra.dev.frontend/module`, and throws on genuine ambiguity instead of
  hiding the build.
- `frontend.shadow/page-response` emitted only the page module's script, never the
  base modules it depends on, so a multi-module page never loaded them. It now
  emits one script tag per module, dependencies first.
- Regression tests in `omkamra.dev.frontend-test`.

**Remaining for M2/M3.** Only note events are routed through the sequencer so
far; `:parameter`/`:control-change` and instrument-defined triggers are the M2
(parameters/triggers) work, and the matrix generators are M3.

**Verified.** JVM 157 tests / 1543 assertions; `omkamra.dev` 7 tests / 19
assertions; browser 26/26; `clj-kondo`/`cljfmt` clean.

---

## 2026-10-19 — M2 — Instrument parameters and triggers (completed)

**Goal.** The parameter layer above the compiler: declared parameters, bindings,
the API, and the trigger/handler dispatcher, with note-on/off as the built-in
case; the provenance vocabulary retires (`docs/mvp/MODULATION.md`, D44).

### M2a — the parameter layer (done)

**Schema.** A description declares `:parameters` and, optionally, `:voice
:bindings`. A control binds to the parameter of the same name unless a binding
renames it. A parameter is `:per-voice` (default) or `:shared`; the seven runtime
names (`gate note velocity frequency bend detune pan`) bind without a
declaration, and a control with no declaration infers a same-named parameter
whose default is the control's own (the ABI now exposes a control's `:default`,
as it already did `:smooth`/`:slew`). `:min`/`:max`/`:enum`/`:step`/`:unit`/`:curve`
are validated or declaration metadata; `:lane` is reserved and orthogonal to
scope (M4). An unknown name, a wrong scope, or a domain violation is a loud
error.

**Storage.** The pool allocates one flat per-voice parameter array and one
shared array. The plan — one entry per ABI control, in ABI order — carries
`{:parameter … :storage {:kind :runtime|:per-voice|:shared :index n}}`, and
`resolve-controls!` reads the parameter store instead of switching on a *source*.
A `:shared` parameter is one cell every voice reads, and the post-mix bus binds
the same cells (`bus-shared-index`), so one setting reaches the voices and the
bus at once.

**API.** `set-parameter!`/`parameter-value` for `:shared`;
`set-voice-parameter!`/`voice-parameter-value` for `:per-voice`. `set-detune!` now
re-resolves a sounding voice, as `set-pan!` already did, so a host that sets it
hears it. `set-bus-control!` is now a write to a shared parameter.

**Retired.** `synth/control-sources`, `normalize-control-source`,
`plan-source-codes`, and the `:source`/`:constant` halves of the runtime plan.
The reference synth and the demo are declared with `:parameters`; the reference
instrument's post-mix globals are `:shared` parameters (the bus distortion
argument was renamed `:post-drive` to avoid colliding with the voice's `drive`).

**Verified.** JVM 163 tests / 1571 assertions; browser 26/26; `clj-kondo`/
`cljfmt` clean. New tests: a default inherited from the control; an inferred
parameter; a `:shared` parameter read by three voices; a `:per-voice` parameter
addressing one voice; a setting reaching a sounding voice; a binding renaming a
control's parameter; a post-mix control being a shared parameter.

### M2b — the dispatcher and the multi-pool sequencer (done)

**The dispatcher.** The sequencer is keyed by an event's trigger (`:type`): a
handler is `(fn [definition state event] state')`. The built-ins are `:note-on`
and `:note-off`, which allocate and release a voice on the pool the event routes
to, and `:parameter`, which sets a shared parameter on every pool that declares
it. A host adds or overrides triggers with `:handlers`; a handler sees the whole
sequencer state and returns a (possibly changed) one, so a trigger can touch any
pool, parameter, or assignment. `handle!` is the one place an event becomes a
state change; an event whose type has no handler is ignored.

**Several pools, one clock.** The state carries `:pools` and a `:routing` table
(channel to pool name, with an optional `:default`). The table is validated at
construction, so an unrouted channel is a loud error rather than a vanished note.
Because `synth.pool/render!` clears the block before mixing, the first pool
renders into the block and the rest render into the sequencer's scratch and are
added; that scratch is separate from the segment scratch `schedule` uses. One
scheduler still cuts the block at the union of every event frame.

**Verified.** JVM 169 tests / 1581 assertions; `clj-kondo`/`cljfmt` clean. New
tests: channel routing sends a note to the right pool; two pools are summed and a
`:parameter` event reaches both; a handler is handed the whole state and can
change it; a handler can override a built-in; an unknown event type is ignored;
the routing table is validated at construction.

---

## 2026-10-19 — M3 — Generators (completed)

**Goal.** Turn the matrix into executable logic on two axes — a host reducer for
the sequencer and a kernel transform for the assembler — sharing one route schema
and one scheduling pass, validated against each other and against the
hand-written matrix (`docs/mvp/MODULATION.md`, D44).

### M3a — the route schema and the scheduling pass (done)

New `omkamra.dsp.modulation` (`.cljc`): `normalize-matrix` expands the grouped and
flat forms into canonical routes; `validate-matrix` checks names, depth, and delay
against the declared names; `matrix-plan` resolves the one thing that is not
per-step — it builds the dependency graph over the targets (a `:delay 0` route
from a target is an edge), topologically sorts the `:delay 0` subgraph once, and
refuses a cycle with no delayed edge, naming the targets it spans. A `:delay n>0`
route is not an edge, so a feedback loop is well defined. The order is
deterministic, so a generator never sorts per step.

**Verified.** JVM 173 tests / 1599 assertions. New `omkamra.dsp.tests.modulation`:
both matrix forms normalise; names, depth, and delay are validated; a chain is
ordered; a delayed edge breaks a cycle; the order is deterministic.

### M3b — the instrument reducer generator (done)

`instrument-reducer` consumes the matrix and the parameter scopes and returns a
reducer `(fn [state] state')` that closes over the plan. The instrument state is
the instrument's parameter values as data — `{:shared {name value} :voices
[{name value} …]}` — and the reducer returns the same shape with each target set
to `base + Σ depth × source`, in the plan's topological order, reading sources
from the in-progress result so a chain is complete. It is immutable data in and
out, so the host keeps the base and calls the reducer each control step without
the modulated values compounding; a `:delay n` route reads its source from `n`
steps earlier (the reducer keeps the earlier results). A `:shared` target with a
`:per-voice` source is refused.

**Verified.** JVM 178 tests / 1608 assertions. New tests: a chain uses the
modulated source; a shared source; a shared target (once, not per voice); a depth
read from a parameter; a delayed source read from an earlier step; a shared
target with a per-voice source refused.

### M3b' — the publish step and the reference (done)

`omkamra.dsp.modulation` gained two pool-bound generators: `instrument-state`
(reads a pool's current values for every name the matrix references, precomputing
the plan and each pool's control positions) and `publisher` (reads each voice's
base controls, replaces the target controls with the reduced values, publishes).

`reference-voice` now exposes `velocity` and its two controller sources
(`mod-wheel`, `brightness`) as controls, the description declares the latter two
`:shared`, and `reference/modulation-matrix` names them instead of
`{:controller n}`. `new-host` builds the reducer, the state reader, and the
publisher; `apply-modulation!` reduces the pool's parameter values and publishes;
`note-on!` no longer tracks note/velocity (they are runtime sources) and
`set-controller!` sets a shared parameter.

The note routes needed the octaves-from-middle-C mapping to survive, so a route
now carries an optional **`:offset`** — `base + depth × (source + offset)`; the
note routes use `:depth 100.0 :offset -60.0` (`0.0125` for pulse width).

**Verified.** JVM 178 tests / 1609 assertions; browser 26/26; `clj-kondo`/
`cljfmt` clean. The generated modulation reproduces the hand-written matrix: the
reference tests (interpreter vs JVM, every matrix source changes the render,
block-size invariance) pass on the generated path.

**Allocation note.** The generated state is data (maps), so it allocates per
control step (~90 KB here) and belongs off the audio thread; the allocation test
now measures the pool + bus audio path (which stays ~1.2 KB/cycle) separately
from the matrix step.

**Also fixed.** `omkamra.dev.tools.reload` advanced its content baseline for
*every scanned file*, so an explicit `(reload 'some.ns)` silently absorbed
pending edits in other files and the next `(reload)` reported `:unchanged`. The
baseline now advances only for files whose namespaces were actually (re)loaded.

### M3c — the kernel reducer generator (done)

`modulation/kernel-reducer` takes the matrix and the declared names, exactly as
`instrument-reducer` does, and returns `(fn [definition] derived-definition)`. The
derived definition:

- exposes the voice's controls plus one for every **source** the voice lacks
  (the two generators take one name universe, validated once by the shared
  `validate-routes`);
- computes each target into a fresh `-mod` local in the plan's order, reading an
  earlier target's `-mod` local so a chain is complete (the language forbids a
  local shadowing a control, so the substitution happens at the call);
- turns a `:delay n` route into a chain of scalar state slots `src-z1 … src-zn`,
  shifted once per frame with `set!` (`src-z2 ← src-z1`, `src-z1 ←` the source's
  effective value), so slot `n` holds the value `n` frames ago (D22);
- **requires a target to already be a control of the voice.** A name the voice
  never reads cannot be modulated; refusing it names the offender instead of
  silently adding a dead control. The instrument generator requires the same, so
  the two agree name for name.

`ir`'s `+` is binary, not variadic, so the target expression folds pairwise;
`derived-definition` records its callee in `:dsp/dependencies`, so the derived
value must be derived from the *original* voice (not the widened one) or the
linker resolves the callee with the wrong arity.

**Verified.** JVM 183 tests / 1624 assertions; browser 26/26; `omkamra.dev`
13/49. New tests in `tests/modulation.clj`: the kernel renders `base + depth x
source`; a chain reads the *modulated* source; a source the voice lacks becomes a
control and is read; a delay slot advances one frame per frame; a target the
voice does not read is refused; and — the acceptance test — the **reference
matrix** run through the kernel generator renders what the instrument generator
reduces.

**Two bugs fixed on the way.** `graph/definition-id` bound a local named
`namespace`, shadowing `clojure.core/namespace`, so a definition value with no
`:source` crashed instead of yielding an unqualified id. And a
`omkamra.dev.tools.reload` regression test left the scan scope pointing at a
deleted temp directory; the test namespace now restores the classpath scope in a
`use-fixtures :once` teardown.

---

## 2026-10-19 — M4 — lanes through the pool (completed)

The M4 design note lives in `MODULATION.md` (with the DAW case and the accessor
rule), and D46 records the language surface.

**M4-0 — `:has-lane` and the `(lane control)` accessor. Done.** A lane is a
property of a *control*: `{:name cutoff :has-lane true}` keeps `cutoff` an
ordinary block-rate scalar in the ABI — the parameter layer, the plan, and the
pool all work unchanged — and adds an **optional** lane beside it
(`:required? false`). The DSP reads it with the compile-time accessor
`(lane cutoff)`, which resolves to an ordinary `:buffer-load` from the flat
`:lanes` region (no new IR op, D28) and composes it with the scalar itself. The
accessor is refused in a callee module and for a control with no lane, and
`:lane` with `:has-lane` is refused. A process whose lanes are all optional is
valid with no region at all: it is built zeroed, so an untouched lane reads zero
and the scalar stands.

**M4a — one shared region, JVM and interpreter. Done.** `Kernel` gains exactly
one method, `write-lane!`: the kernel allocates and owns its flat region — a lane
belongs to the process, not to a voice — and copies a lane's values into it, so
the name is validated by the one component that has the ABI, and `mix-voice!`
keeps its arity and reads that region. `pool/set-lane!` delegates to it, and the
pool refuses a `:per-voice` lane at construction, because one region is fanned to
every voice. The runtime config carries `:lanes`/`:lane-stride` so a pool can
check a lane without the ABI and a browser host rebuilt from a served bundle has
the layout.

**M4b — the browser. Done.** The JavaScript kernel hands the region to the
generated process as its last positional argument; the Wasm kernel keeps its own
region and copies it into each instance's linear memory before running it, since
a Wasm instance owns its memory. Two new browser checks — one per target —
compare a lane-driving pool against the interpreter pool, so the delivery is
*verified* rather than asserted. The browser caught one real bug: `count` and
`nth` do not work on a JS typed array, so the hosts now reach lane values through
shared `mixing/value-length` and `value-at` helpers. A post-mix *bus* that
declares a lane is still refused — once, at construction, not in the audio path.

**M4c — per-voice lanes. Open.** One region per voice, plus `set-voice-lane!`.

**D45 got numbers.** The pool-against-fused question was *measured* rather than
argued: the same voice lowered both ways, timed on the JVM and on both browser
hosts (128-frame blocks, 1000 blocks after a warm-up, reproducible within ~2%).
The table and the conclusion are in D45. The headline: the pool is linear in N
with no cliff on every target; full fusion cliffs where the engine refuses to
compile the unrolled function (JVM between N=12 and 14, JavaScript between 16 and
24); Wasm has no cliff and fusion wins there by about 13%. The harness is the
testbed's `bench-*`/`fused-*` endpoints plus
`window.omkamraDspTestbed.benchConcurrency(n)` — deliberately not a conformance
check, because timing is machine-dependent.

**Also this turn.** `tools/reload`'s `ns-requires` matched the *symbols*
`'require`/`'use`, but an `ns` form spells them as keywords — so `:requires` was
empty for every file and the reload graph fell back to the live image alone. That
held for loaded namespaces and silently lost unloaded ones, which is how a reload
could report success while the JVM was missing a whole subtree. Fixed, with a
regression test in `omkamra.dev.tools-test`.

---

## 2026-10-19 — D47 — definitions as functions with calls (stage 1: the mechanism)

**Goal.** Stop copying every `dsp/defn` body into each call site: lower definitions
as separate functions with real calls, and let a callee ask to be copied with
`^:inline` (D47). `inline-calls` ran for every compiled target, which is why a
fused `(poly n voice)` was one *n times larger* method — the cliff measured in
D45.

**Delivered (the mechanism; no backend switched over yet).**

- `descriptor.clj`: `:inline` is part of a definition's schema and survives
  `normalize`.
- `dsp.clj` (`capture-definition`): the request is captured from `^:inline`
  metadata — on the name for `dsp/defn`, on the form for `dsp/fn` — or from an
  `:inline true` spec/options key.
- `ir.clj`: `signatures` carries `:inline`; `lower-call` puts it on the `:call`
  operation, so a target that *can* call still knows what it was asked to copy.
- `ir/inline-calls` takes the **set of ids to inline**. A definition outside it
  keeps being called and stays in `:definitions`, and its body is still scanned
  because it may call one that *is* inlined. The entry is never retained as a
  callee: a process IR already excluded it, a function IR did not. The
  one-argument arity still inlines everything, which is what a target that cannot
  call needs.
- `dsp/compile` still passes every definition, so **no artifact changed**.

**Verified.** JVM 186 tests / 1648 assertions (was 185/1640). New
`omkamra.dsp.tests.ir/inlining-is-per-callee`: the `^:inline` spellings reach the
descriptor; the one-argument call leaves nothing behind; nothing requested leaves
every call; a subset copies exactly the requested callee *and* rewrites the call
sites inside a definition that itself stays a call. The calling convention and
the per-backend work are in D47.

---

## 2026-10-19 — Reload — an alias held outside the scan scope (regression fix)

`require` stores the namespace *object* it found. `reload` unloads a namespace
with `remove-ns` and re-creates it, so an alias held **outside the scan scope** —
`user` at a REPL, which is the common case — keeps pointing at the dead object
and, with it, at the **old vars**. That is not merely noise:
`(require '[omkamra.dsp.ir :as ir])` threw "Alias ir already exists …" on every
reload, and a form that used `ir/foo` would run the code the reload had replaced.

`reload` now sweeps every loaded namespace after a reload and re-points an alias
whose target object is not the registered one (`stale-alias-entries` /
`refresh-stale-aliases!`, reusing `bind-alias!`), reporting the repairs under
`:aliases-repaired`. A namespace inside the scope repairs itself — it is rebuilt
from its own `ns` form — so only genuinely stale entries appear. `clojure.core/alias`
returns `nil`, so the repair is the record, not `bind-alias!`'s value.

**Verified.** `omkamra.dev.tools-test` 15 tests / 62 assertions. The new
`reload-repoints-an-alias-made-outside-the-scan-scope` failed three assertions
against the old code, including `:second` expected and `:first` actual, which is
exactly the stale read.

---

## 2026-10-19 — omkamra.dev.tools-test — the facade's test file covers both layers

`omkamra.dev.tools` is a two-delegator facade over `omkamra.dev.tools.reload`, and
every test in `tools_test.clj` is reload behaviour, so moving the file to
`tools/reload_test.clj` was considered and rejected. Under `omkamra.dev.*` the
convention is uniform and deliberate: each facade (`on_demand_http`, `systems`,
`frontend`, `tools`) is a thin re-export over a `core`/`shadow`/`reload`
implementation, each has exactly one flat `<facade>_test.clj`, and there is no
test file inside a dev subdirectory. (The `a/b_test.clj` shape is real, but it is
`omkamra.vice`'s — a tree with no facade. `frontend_test.clj` shows how this repo
handles "the logic is in the sub-namespace": it requires **both**.) Moving the
tests would also leave the namespace AGENTS.md tells an agent to read first
without a test.

Instead the file now states its two layers, in the code and in its docstring: it
requires `omkamra.dev.tools.reload` and reads the units a whole reload cannot
isolate through their vars (`#'reload/stale-alias-entries`,
`#'reload/refresh-stale-aliases!`, `#'reload/failed-namespace!`,
`#'reload/initial-digest`). Three unit tests follow the end-to-end ones: the
sweep finds an alias whose target was unloaded and re-created, ignores one whose
target is gone entirely (there is nothing to point at), re-points it, and is
idempotent; the stub is addressable, carries the failure in its metadata, throws
with the target in its message and `ex-data`, and is stable per target; and the
digest baseline flags a file written since *this* JVM started, digests an older
one by content, and gives the same content the same digest. Its boundary is
`#'reload/jvm-start-ms` read from the var rather than a fixed age, because an
hour of backdating is not "before startup" on a session that has been up longer
than that — the trap the end-to-end alias test hit.

**Verified.** `omkamra.dev.tools-test` 18 tests / 79 assertions; stubbing
`stale-alias-entries` fails three assertions in the sweep unit test alone, and
stubbing `initial-digest` fails two in the baseline one, so they test units and
not paths. Bite-check by running a **single test var** (`clojure.test/test-var`),
not the suite: the suite's `:once` fixture ends with `(tools/reload {})`, so a
stubbed baseline makes every classpath file look changed, and the resulting
whole-tree reload takes the nREPL with it.

---

## 2026-10-19 — Reload — refusing to rebuild the whole scope

**The accident.** While verifying the `initial-digest` unit test I stubbed that
function and then ran the whole `tools-test` suite. The stub corrupted the
*baseline* (`configure!` rebuilds it from `initial-digest`), the suite's `:once`
fixture ends with `(tools/reload {})`, and detection therefore reported **275 of
275** files changed. `reload` believed it: it unloaded and reloaded the whole
classpath tree — including namespaces that had never been loaded — and the nREPL
died with no exception to show for it. That is the `:all`-shaped hazard AGENTS.md
already warns about, reached without asking for `:all`.

**The guard.** `wholesale-reload?` (`reload.clj`): a **detection-driven** reload —
no explicit target, not `:all` — refuses when it would (re)load at least as many
namespaces as the scope holds (`>=`, because a mass deletion makes the closure
larger than the scope), in a scope of at least `wholesale-reload-floor` (8)
namespaces. The floor is what still allows a small, deliberately scoped reload to
change wholesale; the rule is *everything* and not a fraction, so a change to a
widely depended-on namespace keeps cascading to its dozens of dependents.

**What it reports.** `:status :refused`, `:changed` (what it saw), `:would-reload`
(the closure size), and a `:note` written for the agent that hits it: what
detection claimed, that nothing was touched, `(reload :all)` **and what that
costs** (it loads namespaces that have never been loaded, and they may do work
when they load), and `(reload 'some.ns)` as the narrower way through. The baseline
is deliberately not advanced, so the refusal repeats until the caller means it or
the scope is re-established.

**Verified.** `omkamra.dev.tools-test` 20 tests / 95 assertions:
`wholesale-reload-guard-fires-on-the-whole-scope-only` (the predicate, the floor,
and the deletion case) and `reload-refuses-to-rebuild-the-whole-scope` (eight
fixture files all rewritten at once: `:refused`, nothing loaded —
`(not-any? find-ns …)` — then `:all` loads them and fixes the baseline). End to
end the original accident now answers `{:status :refused, :would-reload 275,
:scanned 275}` and the session survives it.

---

## 2026-10-19 — Reload — only namespaces the session has run

**Supersedes** *Reload — refusing to rebuild the whole scope*. That entry put a
threshold on the symptom (a huge reload) and left the capability in place; this
one removes the capability: the accident was not that the reload was large, it is
that it *loaded code that had never run*, and loading code runs it.

**The rule.** `reload` concerns a namespace only if (a) **this session has loaded
it at least once** and (b) it changed, or depends on one that changed — the
latter unchanged, which is what the reload graph was always for. Loading is
`require`'s job: deliberate, by name, and a file digest is no reason to run a
namespace for the first time. The evidence is in the tree: `src/rb/explores/quil.clj`
ends in a top-level `(q/defsketch trigonometry …)`, so loading it launches a
sketch, and a headless JVM dies of it. A namespace this session has run has
already done whatever it does, which is why (a) is *at least once* rather than
*currently loaded*: a namespace a **failed** reload unloaded has run here, so a
reload may bring it back, and `load-file`/`reload` recovery still works.

**Mechanism.** `session-namespaces` (`defonce` set of symbols) is added to — from
`(all-ns)` — at the start of every reload, so a namespace is remembered before
anything unloads it. `select-roots` returns `{:root-set … :not-loaded …}`: the
selection is the old one (changed files, gone files, explicit targets, `:all`),
and every branch is filtered by the remembered set, as is the dependent closure.
A changed file whose namespace has never been loaded is *reported* under
`:not-loaded` with a note naming `require`, so the tool still never silently
no-ops. `:all` and named targets remain force-paths — they ignore detection — but
they are still held to (a).

**Removed.** `namespaces-to-load` and the `:would-load` key: nothing loads by
detection any more, so there is no such count. `wholesale-reload?` stays, as a
second layer, but now measures the refresh against the namespaces the session may
touch at all (`(>= (count closure) (max wholesale-reload-floor (share …)))`,
floor 50, share half) rather than against the whole scan scope.

**Tests.** `omkamra.dev.tools-test` 20 tests / 98 assertions. Five tests encoded
the old contract and were re-pointed: `reload-loads-new-and-unloads-deleted-namespaces`
became `reload-refreshes-what-the-session-has-loaded` (a changed, never-loaded
file is left alone and reported; loaded, it is refreshed; deleted, unloaded),
`reload-pulls-in-dependents` and `reload-pulls-in-an-unloaded-dependent-from-source`
load their fixtures with a new `load-fixture!` instead of `(reload :all)` (fixtures
are outside the classpath, so a fixture that `:require`s another needs both
`load-file` and the `*loaded-libs*` registration), `reload-reports-every-failure`
breaks namespaces *after* loading them, and the two alias/refusal tests load
theirs. `forget!` now also unremembers, so a test is hermetic whatever order it
runs in.

**Verified.** JVM 186 tests / 1648 assertions, `omkamra.dev.tools-test` 20/98. The
original accident — `initial-digest` stubbed, then `(tools/reload {})` — now
answers `{:status :refused, :would-reload 63, :not-loaded [214 namespaces …]}`, and
that list contains `rb.explores.quil`: the namespace that killed the JVM is named
as one reload will not run, and the session survives.

---

## 2026-10-19 — Operators fold three or more operands (D48)

**Why.** Writing a three-term sum in the DSL failed: `(+ a b c)` was a lowering
error, and so were `(and a b c)`, `(bit-or a b c)`. The playground's `pygen` and
`cgen` already accept the variadic form, so the DSP DSL was the odd one out.

**What changed.** Lowering only, in `ir.clj`. `lower-binary` split into
`lower-arithmetic` (two or more operands, folded left through the same node
builder, so the type rules and messages are stated once) and `lower-comparison`
(exactly two). `lower-logical` and `lower-bitwise` fold two or more for
`and`/`or`/`bit-and`/`bit-or`/`bit-xor`. `(+ a b c)` lowers as `(+ (+ a b) c)`:
each operand is lowered once, in source order, and the IR keeps its two-operand
nodes, so no backend, no ABI, no artifact and no WAT changed. Nothing downstream
knows this happened — which is the point.

**Tests.** `omkamra.dsp.tests.ir/operators-fold-three-or-more-operands` pins the
fold *direction* (its `(* (- x y z) (/ x y z))` case is the one no other grouping
reproduces), that both backends agree, and that a three-operand sum is two binary
nodes and not one n-ary node. `operators-keep-their-arity-floor` pins what is
*refused*: one-operand arithmetic, one-operand `and`/`bit-and`, chained
comparisons ("exactly two operands"), three-operand shifts, three-operand `min`
("wrong number of arguments" — the builtin table), and mixed int/float ("wrong
type", i.e. no implicit promotion). Two catalog cases make all four targets
render a fold, browser included. 188 tests / 1694 assertions, 0 failures.

**Deliberately left out.** Chained comparisons: `(< a b c)` reads `b` twice, and
reading it once means hoisting it into a local first — a real change to lowering,
not a fold. Variadic `min`/`max`: a builtin's arity is its signature table entry,
and a variadic signature has no representation there. Unary `(- x)`/`(/ x)` and
the identities `(+)`/`(*)`. And int/float promotion, which is a question about the
value domain rather than arity, and still open.

---

## 2026-10-19 — D47 stage 2 — the JVM calls

**What happened.** A process compiled for `:jvm` no longer copies a callable
definition into every call site: each retained definition becomes a **static method**
on the process class, and a call site emits `invokestatic`. The ABI is small because
a callee can reach nothing but its own params and state — `lower-definitions` builds a
callee environment with no bindings, and `context-bindings` gives every definition
only `frames` and `sample-rate`:

    float-state int-state float-base int-base frames sample-rate args…

The two bases are the point: the arena is one logical index space realized as two
physical arrays, and D19 gives each call site its own partition, so a call passes the
physical index of that partition's first float and first int slot. One method serves
every call site, and a callee's own nested call sites add a compile-time constant to
the base *it* was handed. The **entry's own accesses stay base-free** (its base is 0),
so the hot loop does not pay for any of this; only a callee method's layout entries
carry a `:base-slot`.

**What decides.** `dsp.clj`'s `prepare-ir` hands a process on a calling target the set
`uncallable ∪ requested`: `^:inline` still opts a definition back into being copied,
and `ir/uncallable-definition-ids` names the ones no method can take — a **record
return** (a call returns one value) and **scratch arrays** (`lower-process` seeds the
entry's scratch after the whole state region, but the function path never resets it,
so a callee's arrays start at 0 and overlap its own state). JavaScript, Wasm and the
interpreter are untouched; a **function** entry stays flat on every target, since its
class has no arena for a partition.

**Two things the first attempts got wrong**, both worth remembering for the next two
backends. A call's **descriptor needs the same ABI prefix as the method's** — a
descriptor of just the formal parameters leaves the emitted pushes unbalanced, and
ASM reports that as a frame-merge failure that never mentions the call. And
`partition-bases` must turn a storage the partition does not use from `nil` into 0.

**What verified it.** 189 tests / 1708 assertions, and a bite-check: stubbing
`partition-bases` to return zero makes the conformance test fail **14 times**, four of
those from the two cases added for this (`:call-arena` — two partitions of one
stateful callee with the entry's own state between them, plus a callee whose state is
integer, so its float base is a value it cannot observe — and `:call-nested`, where a
callee passes on a base it derived). Browser 28/28, which also means the two new
catalogue cases render correctly on JavaScript and Wasm through the still-inlined
path.

**One accident worth recording.** After `jvm.clj` and `dsp.clj` were edited in the
same pass, the JVM kept reporting the frame-merge failure for a definition that a
direct `compile-process` call in the same image compiled fine. `tools/reload` had
reloaded both namespaces and reported no failure; only a **dev-server restart**
settled it. Generated-class code paths are worth a restart when a backend and its
dispatch change together — the reload rule (only namespaces this session has loaded)
keeps the image *small*, not necessarily *consistent*.

**Not done.** No measurement yet: the testbed bench is browser-side, so it can weigh
the JavaScript and Wasm flips but not this one. What this flip removes is *method
size*, which is what the D45 cliff was about, so a JVM timing measurement is owed —
`harness/fixtures` already has an allocation meter to build it on.

---

## 2026-10-19 — D47 measured — the cliff is inlining's, and leaving it costs 15%

**What was missing.** D45's table had JVM pool/fused columns but no *calls-against-
inlined* axis, and no committed timing code at all: those numbers came from an ad-hoc
REPL loop. So the JVM half of D47's justification was unmeasured.

**`harness/bench.clj`** (new) fills it: it compiles the same `(poly N voice)` twice —
the linked unit, and the same unit with every definition flagged `:inline true`, which
is what every target did before stage 2 — and times both with criterium, round-robin
so drift reaches both arms, through `dsp/process!`. The arms' first block is compared
**bit for bit** (`Arrays/equals`, so NaN is a value and not a fake difference), and
`:finite?` reports whether that block is finite at all.

| voices | called ms/block | inlined ms/block | called bytes | inlined bytes |
|---|---|---|---|---|
| 4 | 0.046 | 0.039 | 1 935 | 17 784 |
| 8 | 0.085 | 0.072 | 2 073 | 56 692 |
| 12 | 0.127 | 0.106 | 2 213 | 117 753 |
| 14 | 0.165 | **3.858** | 2 283 | 156 461 |
| 16 | 0.166 | 4.057 | 2 353 | 200 569 |
| 24 | 0.248 | 6.136 | 2 633 | 430 997 |
| 32 | 0.325 | 8.323 | 2 913 | **747 805** |

**Three findings.** The cliff is *still* between N=12 and N=14 and still 36× at the
crossing — D47's diagnosis holds, and the called column is linear end to end. Calling
costs about **15%** while the engine can still inline (0.046 against 0.039 at 4), which
is the price of a floor: above the crossing the called arm is 23-26× faster and never
stops scaling. And artifact size is O(code): the called class grows 25 bytes per voice
while the inlined one reaches **748 KB, 256× larger** — one ~700 KB method. The
inlined arm is fully inlined at every N (`:definitions` empty), so nothing was
silently falling back to calls.

**Three bugs in the harness, all instructive.** `:invoke` passes **sample-rate 0**, so
`(/ frequency 0.0)` made every block NaN — the harness must use `dsp/process!`, and
that is also why a timing comparison of NaN audio is not a comparison at all. A copy
of the voice descriptor with `:inline true` was **discarded**: `dsp/link` resolves a
callee through its Var, so the flag has to go on the *linked unit*, which `dsp/compile`
takes as-is (and which is also *why* `^:inline` works in real code — the linker reads
it off the resolved Var). And rewriting a namespace's file mid-session leaves the *old*
interns behind under plain `require`/`:reload`, which cost a confusing detour until
`tools/reload` (which removes the namespace first) cleaned it up.

---

## 2026-10-19 — D47 stage 3 — the JavaScript target calls

**What happened.** A process compiled for `:js` now emits one `letfn` binding per
retained definition and calls it, exactly as the JVM does. The differences from the
JVM are both in the target's favour: the emitted functions are defined *inside*
`createKernel`, so they close over the instance's arena and need no array parameters;
and the JS arena is a single array, so a partition base is its plain offset with no
per-storage resolution. `dsp.clj`'s `calling-targets` is now `#{:jvm :js}` — Wasm
still gets one flat function.

**Two traps worth keeping.** A function cannot bind a dynamic var and then emit: my
`with-module-names` was a function first, so its argument — the form — was built
*before* the binding, and the call sites saw no callee names. It has to be a *macro*,
because the emitter resolves a callee while it builds the form. And inside `letfn` a
binding is `(name [args] body)`, not a `defn` form: emitting `defn` made Squint name
the entry itself and two collided as `Identifier 'defn_6' has already been declared`.
The second one is instructive — it is a `SyntaxError` that only a real engine reports,
and the JVM-side test (which greps the emitted source for a function and a call) could
not see it. `browser/run!` did, on the first try.

**What verified it.** 191 tests / 1716 assertions on the JVM, browser 28/28 — the
whole shared catalogue, with the two stateful-call cases from stage 2 included, now
executing through calls on js. A new test in `tests/js.clj` pins the emitted shape:
a function per definition, a call passing partition base 1, and a callee addressing
`__dsp_state_1[(__dsp_base + 0)]`.

**The axis, and what it measured.** The testbed's `fused-bundle`/`fused-source` gained
`?inline=1` (the pre-D47 lowering, which D45's numbers were taken with), the page now
times three lowerings per host instead of two, each wrapped so a failure is reported
as data rather than killing the run, and `harness/browser.clj` gained `bench!` — so
this measurement is a committed, repeatable driver rather than a REPL session.
`harness/bench.clj`'s `unit`/`arms` are shared by the JVM bench and the testbed.

| voices | js pool | js called | js inlined |
|---|---|---|---|
| 4 | 0.227 | 0.154 | 0.150 |
| 8 | 0.338 | 0.211 | 0.396 |
| 12 | 0.484 | 0.331 | 0.619 |
| 16 | 0.671 | 0.436 | 0.908 |
| 24 | 1.212 | 0.663 | **4.958** |
| 32 | 1.595 | 0.846 | *cannot be built* |

Three findings. The **cliff is between N=16 and N=24** (0.908 → 4.958, 5.5×) — exactly
where D45 put it — and the called column is linear throughout. **Calling is free on
V8** while it can still optimise (0.154 against 0.150 at N=4, against the JVM's ~15%),
though the inlined arm is *already* 1.9× slower by N=8: V8 charges for the big function
gradually. And **at N=32 the inlined lowering cannot be compiled at all** —
`dsp/compile` overflows Squint's compiler (`StackOverflowError`), the endpoint answers
500 with an empty body, and importing an empty module gives one with no
`createKernel`. That last one cost an hour of investigation: the symptom is a *missing
export*, far from the cause. Wasm is still flat, so its column is the same lowering
twice and will only mean something at stage 4 — it shows no cliff (1.17 at N=32).

**Not done.** Wasm. `harness/bench.clj`'s per-block control schedule is the voice's own
descriptions, which is right for the JVM bench; the browser numbers here come from the
page's own loop and schedule instead, so the two tables are not comparable to each
other or to D45's.

---

## 2026-10-19 — D47 stage 4 — the Wasm target calls, and every target is measured

**What happened.** A process compiled for `:wasm` now emits one function per retained
definition and calls it, so `calling-targets` is `#{:jvm :js :wasm}` and D47 is
finished. Two things are simpler here than on the other targets and one is harder.
Simpler: a Wasm instance's memory is the module's, so a callee needs no arena parameter
and the emitted functions close over nothing. Harder: the partition base still has to
be *passed*, because a callee's slice of the arena is not a constant — a Wasm function
has no equivalent of the JavaScript closure's captured arena.

**Two bugs.** The first was mine and would have been wrong rather than noisy: the
callee's body was emitted against `*state-layout*`, which is the *enclosing* arena's
layout, so every state access was off by the partition base a second time. A callee
needs its own layout — `state-memory-layout` over the definition's slots starting at
zero — and `definition-layout` is that. The second was a missing `(result …)` on the
emitted functions, so a call pushed nothing and every surrounding expression failed to
validate. My JVM-side test, which greps the emitted text, was perfectly happy; WABT was
not. That is the third time in two stages that the real engine saw what a text
assertion could not (the JS `letfn`/`defn` `SyntaxError` was the second, and it cost an
hour of chasing a *missing export*), and it is the argument for the browser suite being
the authority on emitted code.

**The invariant only Wasm depends on.** A callee is emitted once and serves every call
site, so its partition's layout has to be the enclosing layout translated by the base
it is handed. That is *not* automatic: aligning `base + x` equals `base + align(x)` only
while the base and every entry offset are multiples of the element size, and in an f64
block an int slot is four bytes wide — so an int slot before a float partition put the
callee's float slot four bytes early, reading memory the reset never wrote.
`state-memory-layout` now places every slot at the *block's* element alignment, which
costs a little padding and makes the translation hold; in f32 the alignment is 4 either
way, so nothing there changes. I pinned it with a fixture rather than an argument:
`call-fixture/mixed-state` (float slot after int slot) called from
`call-fixture-int-led` (own int slot, so the partition starts four bytes in), with a
`:call-int-led` catalog case. Reverting the alignment one-liner fails that case — Wasm,
f64, one wrong sample, f32 untouched — and restoring it passes. 28/28.

**Measured, all three targets.** The bench's two browser arms were already wired in
stage 3; now `:fused` really calls on Wasm too, so the axis means something on both
hosts (the testbed's per-arm failure reporting earned its keep here, by turning a
mysterious "missing `createKernel`" into "the endpoint answered 500").

| voices | js pool | js called | js inlined | wasm pool | wasm called | wasm inlined |
|---|---|---|---|---|---|---|
| 4 | 0.222 | 0.148 | 0.199 | 0.142 | 0.128 | 0.117 |
| 8 | 0.357 | 0.224 | 0.382 | 0.268 | 0.248 | 0.230 |
| 12 | 0.458 | 0.323 | 0.618 | 0.399 | 0.370 | 0.342 |
| 16 | 0.668 | 0.441 | 0.963 | 0.562 | 0.493 | 0.458 |
| 24 | 1.183 | 0.657 | 4.603 | 0.796 | 0.740 | 0.683 |
| 32 | 1.570 | 0.867 | *cannot be built* | 1.077 | 0.980 | 0.908 |

Wasm: **no cliff, a flat ~7%** for calling (0.128 against 0.117 at N=4; 0.980 against
0.908 at N=32). JavaScript: the cliff is between N=16 and N=24 (0.963 → 4.603, 4.8×),
calling is free below it, and at N=32 the inlined lowering cannot be compiled at all —
`dsp/compile` overflows Squint's compiler, the endpoint answers 500, and importing the
empty body yields a module with no `createKernel`. The JVM's cliff is N=12→14 and
calling costs ~15% below it (stage 2). So the three targets tell the same story from
three distances: the smaller artifact is what calls buy, and speed is a side effect —
large and negative for the JVM, large and positive for JavaScript, and a flat 7% for
Wasm, which is why it could safely go last. A user who wants the 7% back writes
`^:inline`.

---

## 2026-10-19 — Testing: case-level `:only`, and one call that runs both suites

**Why.** Two small things the work above wanted. First, `browser/run!`'s `:only`
matched *check* names only, so a bite-check that distinguishes one case ("does
`:call-int-led` still pass on Wasm, f64?") had to run 89 cases × 2 precisions behind
each name — I ran the full 28-check suite twice for what turned out to be a one-case
question. Second, the JVM suite and the browser suite are independent and took their
~50s and ~2 minutes in series.

**Case-level `:only`.** A needle now selects a check by name (as before, running it
whole) or a catalog case by ID (narrowing the two shared matrices to it);
`(browser/run! {:only ["call-int-led"]})` runs one case in two precisions, two checks.
The subtlety is the report filter: a *passing* matrix result whose name no longer
matches the selection was being dropped, so the run looked empty — a matrix result is
now kept when any case it ran matches too. And since an empty run is indistinguishable
from a passing one, a needle that names neither a check nor a case is *reported* as a
failure ("no check or case matched: …") rather than running nothing.

**`run-everything`.** `tests/suite.clj` gained a runner that starts `run-all` and
`browser/run!` in two futures and `deref`s both, capturing the JVM half's
`clojure.test` output so the two reports cannot interleave: 192 tests / 1729 assertions
and 28/28 checks, **62.3 s total**, with `:only` passed through to the browser half.
Each half has its own `:timeout` (default 30 minutes) and reports `{:timeout true}`
rather than hanging. `omkamra.dev.tools-test` is deliberately excluded: it reloads
namespaces, which would race the tests beside it. `TESTING.md` documents all of it,
and its baselines now read 192/1729 and 28/28.

---

## 2026-10-19 — M4c — per-voice lanes, and two bugs the honest check found

M4 closes: a lane now has a *scope*, and the pool delivers it accordingly.

- **The scope is the control's storage.** `pool/voice-pool` no longer refuses a
  non-`:shared` lane; it computes `lane-scope` per lane from the control plan —
  `:shared` → shared, `:per-voice` → per voice, a runtime control (`frequency`,
  `detune`, …) → per voice because the pool resolves those per voice. A lane-only
  (`:lane`) control leaves the ABI's scalar list and so has no plan entry: its
  scope comes from its own parameter declaration, and an undeclared one is per
  voice, matching the inference an unbound ABI control already got. A `:bend` lane
  is refused at construction: `:bend` is per *channel* and has no per-lane
  delivery.
- **One region per voice.** `write-lane!` became `write-voice-lane!` taking the
  voice handle; each kernel allocates the region in `create-voice`. `set-lane!`
  (shared) writes the curve into *every* voice's region — including silent ones,
  so "write now, a voice that starts later still reads it" survives — and
  `set-voice-lane!` writes one. The cost is `N * lanes * 4096` bytes and `N`
  copies per shared write, which is small next to a voice's own state; the
  alternative was a second region in the ABI, and a lane's scope is only known
  after the artifact is compiled, so the descriptor could not have placed it.
- **The Wasm ABI passes lanes by address.** The generated Wasm `process` takes a
  trailing `$lanes i32` pointer into its own linear memory; the direct process
  path passed it and the pool did not, so **Wasm lane delivery through the pool
  had never worked** — while the direct path's `:automation-lane` catalog case
  passed, which is why nobody saw it. The host now passes the instance's own lane
  region address, set once per voice. JavaScript hands the array itself.
- **The browser harness was comparing nothing.** `array-seq` does not walk a JS
  typed array under Squint — it yields an empty sequence — so the lane checks
  compared `[[null null null null]]` against `[[null null null null]]` and
  passed. The same `planar-block` was also wrong for a *mono* pool: a mono voice's
  block *is* the sample array (`mixing/channel-of`), so a one-channel planar block
  gets written through as if its plane were the samples. Both are fixed
  (`real->vector`, `render-block`) and every lane check now also pins **literal**
  numbers, so a shared lane of 1,2,3,4 beside a scalar of 10 must render 11..14
  and the two-voice case 31..34. The really uncomfortable part: the pool-host and
  worklet checks used the same conversion, so a comparison there could pass on two
  blocks of nothing. They pass with real samples now.

Verified: JVM 193 tests / 1744 assertions, browser 30/30 (`run-everything`,
all green in 63.7 s), and the bite-checks
behave — stubbing `set-voice-lane!` to always write voice 0 fails the per-voice
JVM test, and writing the Wasm lane into a throwaway array fails both Wasm lane
checks.

Next: **M5** (a shared kernel region for scalar broadcast), then **M6** (fused
lowering as an option, D45).

---

## 2026-10-19 — M5a — the shared region: language, ABI, and the raw JVM/interpreter path

M5 in three steps (PHASES), of which this is the first. The design is D49.

- **The language**: a control may declare `:shared true`. It leaves the ABI's
  scalar control list and its *name* becomes a read of the process's `:shared`
  region — the `:lane` control's trick applied to a global. `:shared` beside
  `:lane`/`:has-lane`, or on a non-float control, is refused at descriptor time.
- **The shape**: the region is deliberately the *lane* region's shape — one
  `:frame-capacity` slice per shared control, `:shared-stride` = `:lane-stride`.
  That is why this slice needed **no new IR op and no new emitter**: a shared read
  is a `:frame-offset` `:buffer-load` with a different `:buffer-id`, and the Wasm
  emitter's base local is already derived from it (`$shared`). It also means a
  host can write a per-frame *global* curve into the same slice.
- **The ABI**: `:shared` (name/type/role/index/buffer/offset) and `:shared-stride`,
  beside `:lanes`/`:lane-stride`. One region for the whole *pool*, which is the
  point: a broadcast parameter is written once instead of once per voice.
- **Delivery**: interpreter + JVM take a real region; `process!`'s context gained
  `:shared {:name value-or-sequence}` (a bare number is broadcast across the
  slice); the JVM raw ABI gained an optional trailing `shared` array on all eight
  process interfaces and their adapters, with the 7- and 8-argument arities kept
  so no existing caller broke. JavaScript takes it as a trailing positional
  argument and Wasm takes its *address* — the lane region's rule again.

Two mistakes worth remembering, both about wiring rather than design: the lowered
process IR dropped `:shared` because the assembly destructures an explicit key
list (`{:keys [bindings controls lanes …]}`), and the JavaScript target builds its
own private `js-ir` map, so `(:shared ir)` had to be carried across that seam too.
Both were caught by the first test run — the ABI had no shared entry, and the
emitted JavaScript had no `shared` argument.

Verified: JVM 196 tests / 1767 assertions, browser 30/30 (the emitter change
touches both browser targets, so the matrix was run too).

Next: **M5b** (the hosts own and deliver the region: interpreter/JVM kernels, the
JavaScript argument, the Wasm memory copy, the testbed's direct path), then
**M5c** (the pool maps a `:shared` parameter to the region, so `set-parameter!`
stops resolving every voice's controls), then M6.

---

## 2026-10-19 — M5b/M5c — the shared region reaches the pool, and one write serves every voice

M5 closes. The design (`:shared true`, the lane-shaped region) landed in M5a; this
is delivery and the pool's use of it.

- **The kernels own one region.** `Kernel/write-shared!` is the only new method:
  the interpreter and the JVM keep a real array for the whole pool and pass it by
  reference (the JVM's raw ABI already had the trailing `shared` argument from
  M5a); JavaScript keeps an array and sets a trailing argument slot; Wasm keeps a
  host-side *master* and copies it into each instance's own `:shared` region in
  `mix-voice!` — only the `frames` about to be rendered, since that is all the
  module can read.
- **The pool maps a parameter to the region.** `binding-plan` now takes the ABI's
  shared controls as well: a shared control's parameter is shared whether the
  description declares it or not (undeclared infers `:scope :shared`), and a
  `:per-voice` declaration beside a shared control is refused. The runtime config
  carries both the region's layout (`:shared`/`:shared-stride`) and the
  parameter-to-control map (`:shared-plan`). The pool seeds the declared defaults
  into the region at construction, and `set-parameter!` writes it *once* — the
  per-voice fan-out is skipped entirely, which was the point: `voices × plan` work
  becomes one region write, independent of how many voices exist. A shared
  parameter bound to an *ordinary* control still fans out, so declaring the DSP
  control is what switches a parameter over.
- **A bus** that declares a lane or a shared control is refused at construction on
  every host, as it already was for lanes.

Verified: JVM 198 tests / 1781 assertions, browser 32/32 (two new pool checks,
literal-number pinned), and the bite-check behaves — skipping the Wasm copy makes
the Wasm check fail while JavaScript still passes.

Next: **M6** (fused voice lowering as an option, D45) — the pool remains the
default.

---

## 2026-10-19 — M6-0 — the measurement that gated fusion, and what it overturned

M6's plan said to measure before building, because D47 keeps scratch inlined and
the worry was that `(poly S voice)` would copy a delay line's body into every slot
and bring back the pre-D47 cliff. A new fixture, `echo-voice` (a module-shaped
generator with a real 64-sample delay line), makes that measurable beside
`sine-voice`:

| voice | S=1 code / arena | S=2 | S=8 | S=32 |
|---|---|---|---|---|
| `sine-voice` | 1 841 / 3 | 1 876 / 6 | 2 081 / 24 | 2 921 / 96 |
| `echo-voice` | 2 875 / 68 | 2 904 / 136 | 3 096 / 544 | 3 864 / 2 176 |

- **Code grows ~30 bytes per slot either way.** What unrolls is the *call site*;
  the delay line's buffer is data and lives in the arena, and its per-frame code
  is a handful of operations. The D45 cliff was inlining a whole *voice* per call
  site, not inlining scratch.
- **Memory is arena-linear and equals the pool's**: 68 slots per slot is one
  64-float buffer and a cursor per line, which is what N instances would each own.
- **`poly` needs module-shaped generators.** It refuses `reference-voice`, which
  accesses the process's buffers. So M6's option is defined for module-shaped
  voices; a process-shaped one would need a call-into-scratch wrapper, which is
  not built.

Consequence for the slice: no artifact cliff to dodge, so **S = capacity is the
natural first target** and chunking is the escape hatch — and the remaining work
is the slot-aware host (M6a), skipping (M6b), and the browser hosts (M6c). Nothing
else was built this turn: the plan's own order was to measure first, and the
answer changed what M6a should be (no chunking default to design around, and a
module-shape precondition to state).

---

## 2026-10-19 — M6a — the realistic voice, timed, and the four seams that remain

M6-0 settled the artifact; this is the other half of the question — whether a
voice that *does* work is still worth fusing. `bench/realistic-sweep!` weighs a
pool of N `echo-voice` instances (a generator with a 64-sample delay line, all
slots sounding) against one `(poly N echo-voice)` process:

| slots | pool ms | fused ms | pool/fused |
|---|---|---|---|
| 1 | 0.032 | 0.028 | 1.13 |
| 2 | 0.065 | 0.048 | 1.35 |
| 4 | 0.141 | 0.089 | 1.58 |
| 8 | 0.258 | 0.175 | 1.48 |
| 16 | 0.360 | 0.188 | 1.92 |
| 32 | 0.754 | 0.393 | **1.92** |

Fusion pays for a realistic voice too (~1.9× at 32), the pool is linear in slots
while the fused process is sublinear — the fan-out has moved out of the host — and
at one voice the two lowerings agree **bit for bit**, delay line included, which
`tests/jvm.clj` now pins along with the flatness of the pool's artifact size.

The remaining M6a work is the slot-aware host, and writing it down in seams was the
useful part of the exercise, because two of them are *not* host-side at all:

1. an **instance key** on the protocol, so the pool mixes one fused instance per
   block and skips an all-silent one — this is what keeps the pool's per-slot view
   unchanged;
2. a **base → fused control mapping** in the kernel, which has both ABIs;
3. **per-slot reset** — the fused state is partitioned per call site (D19), but
   nothing exposes those ranges, so stealing a slot would otherwise clobber its
   neighbours;
4. per-slot **`:activity`** — deferrable, and worth deferring: `poly`'s slots are
   *callees*, and a callee's special store is rebased into a call partition no host
   reads (D39), so a fused voice declares no `:activity` and the pool holds its
   voices until stolen. That is the tail-freedom the pool gives up, not a detail.

Next: seams 1–3, then the instrument option (`{:fusion {:slots S}}`).

### M6a, attempt one: the instance key, backed out

Started seam 1 (the `instance-key` method on `Kernel` plus the pool mixing one
instance per block) and rolled it back: the first pass left 42 JVM errors and broke
the *browser* lane check — "(10+1..4) + (10+10) must render 31..34, not
[[11 12 13 14]]", i.e. the second voice was never mixed, which is what a wrong key
looks like: two voices sharing one. The lesson worth keeping is not "the key is
wrong" but that the kernels' keys are a *silent* surface: a duplicate key looks
like a working pool until a second voice sounds. Next attempt should pin the key
function itself — one test per kernel, asserting distinct keys for distinct voices
and that the pool's dedup changes nothing when every key is distinct — before the
fused kernel is written at all.

The design (four seams, in `PHASES.md`) is unchanged and the tree is green again:
JVM 199 tests / 1785 assertions, browser 32/32.

### M6a, second attempt: the key surface, landed inert

Following the recorded lesson, this time the *surface* went in before any consumer,
in steps that each stayed green:

- `Kernel/instance-key` on the protocol — a dense, **positive** key assigned in
  creation order, with 0 left free to mean "no key yet";
- the four kernels implement it (JVM, interpreter, JavaScript, Wasm) as the
  creation counter, which is what a plain one-instance-per-voice kernel means;
- `every-voice-has-an-instance-key-of-its-own` pins the surface: four voices get
  four distinct, increasing, positive keys, on the JVM *and* the interpreter — and
  **nothing calls it yet**, so the pool's behaviour is unchanged and the suite stays
  green (200 tests / 1791 assertions, browser 32/32).

Then the consumer, which failed twice and is rolled back again — but with real
information this time. The first attempt's failure (42 errors, a dropped voice in
the browser) had *two* causes, both now understood: the mark array was indexed by a
total order that could exceed `capacity` (fixed: one mark per key, so `capacity + 1`
entries), and the reified kernel lost `write-shared!` to a duplicate when I re-applied
edits over the *staged* file (`git checkout --` restores the index, not HEAD). The
second attempt got past the keys and still produced 13 failures and 13 errors, all
of the same shape: **voices not sounding** — per-voice pan at 0.0, legato and
retrigger rendering identically, a released voice never freed.

That last shape is the diagnosis: those tests exercise the pool's *voice lifetime*
(the `:activity` readback and the free/reset path), and restructuring `render!`
around the mix moved that block. So the next attempt should add the instance mark
*without* touching the lifetime logic — the guard belongs around the `mix-voice!`
call alone, with that block left exactly where it is — and it should pin the
consumer the same way the key is pinned: a two-voice, one-instance-fake-kernel test
where the mix count is asserted, before any real fused kernel exists.

### M6a, third attempt: seam 1 lands, with three bugs of its own

The pool now mixes a kernel instance once per block (`instance-key` +
`instance-already-mixed?`, marks cleared per block), and the suite is green:
202 tests / 1798 assertions, browser 32/32 — including the two tests written for the
consumer before it existed (`a-pool-mixes-one-instance-once-per-block` and
`every-voice-is-mixed-when-every-voice-has-its-own-key`).

Getting there cost three bugs, all now understood, all in the *mark* machinery
rather than the fused design:

1. **The mark array was one short.** Keys start at 1 (0 means "no key"), so an
   array of `capacity` was indexed 1..capacity and threw. Sized `capacity + 1`.
2. **The clearing loop was one short** — and this is what broke thirteen existing
   tests: it cleared `capacity` entries of a `capacity + 1` array, so the *last*
   key stayed marked and **every voice after the first block was silently
   skipped**. The symptoms were "voices not sounding" and "a released voice never
   freed", which look nothing like an off-by-one. It survived the first two
   attempts because the fake-kernel probe I wrote watched voice 0 only; the
   strengthened test sounds *every* slot, including the last key, and renders two
   blocks.
3. **A kernel-wide key is not a pool-wide index.** `bundles` share a kernel
   between pools (the lane test builds two pools from one bundle), so the second
   pool's keys fell outside any array the first pool had sized. The contract is now
   stated as *equality* — same instance, equal keys — and the pool numbers the
   instances it created itself, so keys need not be dense or numeric. That also
   removed the `capacity + 1` shim: marks are one per owner slot, at most one per
   slot.

The lesson from attempt one ("pin the surface before the consumer") held: the key
test was in place and passing while all this was wrong, and it was the *consumer*
tests that found each bug. What it needed in addition was a test that exercises
*every* key across *two* blocks, and one that says out loud that two pools may
share a kernel.

### M6a, seam 2: the base -> fused control mapping (JVM)

The pool writes *base* values; a fused instance's interface is `gate_0`, `gate_1`,
... with the rest hoisted; the kernel is the only component that has both, so the
mapping lives there (D50). `hosts/fusion/fused-mapping` is pure data — the JVM and,
in M6c, the browser compute the same answer from the interfaces each holds — and
`fused-jvm-kernel` applies it through one ABI-position row per slot, precomputed at
construction, so the write path stays one branchless loop.

**What it proves.** A pool over a fused kernel renders bit-for-bit what a pool over
the plain kernel renders: slot 0 with its own note, and then — the assertion the
whole mapping exists for — slot 1 sounding note 72 while its neighbour holds 69 and
is gated shut, matching note 72's solo block exactly. The bite check is worth
recording because the failure it produces is so legible: drop the `slot` term from
`fused-position` and the write for slot 1 lands on slot 0's copy, so gating slot 0
shut silences the instance and the test sees 128 zeros where the pool has audio.
Two seams' worth of work is a pool, not a sum of voices.

**Two limits, one new.** Resetting a slot resets the instance until seam 3 exposes
the per-call-site ranges, so a fused pool must not steal or retrigger a slot while
its neighbours sound. And `poly` redeclares a voice's controls as `{:name name}`: a
fused interface loses the voice's declarations -- measured for `:shared`
(it compiles as an ordinary scalar with an empty shared region) and for a lane
(refused by the IR outright); `:default` and `:has-lane` read the same way. So the instrument option cannot be a bare
`(poly S voice)` — either `poly` carries a control's declaration through, or the
option refuses a voice that declares one. That is a decision for the option slice,
not for the mapping, which maps names and can only report the interface it was
handed.

JVM 205 tests / 1813 assertions, browser 32/32.

### M6a, seam 3: one slot's reset, and the entries that own several slots

Seam 2 put a fused instance's *controls* in the right slot; this puts its *state*
there. D19 already gave every call site a private state partition, but no host
could see the ranges, so the only reset on offer restored the whole instance -- and
a steal in one slot restarted its neighbours' envelopes. Now the lowering records
each call site's partition as it allocates it (evaluation order, which for a
`(poly S voice)` process is slot order), the JVM target resolves each logical range
to a physical state range per storage, the instance keeps each partition's initial
values, and `reset-voice!` is one `arraycopy` into one range (D51).

The bug worth remembering: **a state entry may own several slots**. A delay line's
buffer is *one* entry with `:size 64`, not 64 entries, so the first version's float
range came out as `(0 1 65)` and the contiguity check it had just gained fired --
correctly, because the range was wrong, not because contiguity is optional. That
check is now part of the slice rather than an accident: a target that publishes
layout should say when the layout is not the shape it promises.

The bite check: replace the slot's `:reset-partition` with `dsp/reset!` and the new
test fails with 128 zeros where the plain pool has a sounding note. Stealing into
slot 0 of a two-slot instance now leaves slot 1's envelope and delay line
bit-for-bit alone, across the steal, against a plain pool that never stole at all.

JVM 206 tests / 1818 assertions, browser 32/32.

### M6a, the option itself: `{:fusion {:slots S}}`

The option is a compile option beside `:target`/`:precision`/`:containment`, and
the only one consumed rather than passed on. `synth-bundle` compiles the base
artifact (the runtime config -- plan, lanes, shared region, declarations -- stays
base-shaped, D50) and then, when asked, `(poly S voice)`, which is the artifact that
runs. The runtime carries `{:slots :controls :per-slot}` so a host with only the
artifact can rebuild the mapping; that is what M6c's browser host will use.

The per-slot set is "what a pool writes per voice": runtime storage and per-voice
parameters. So `delay`/`mix` are expanded per slot even though they are "just
parameters" -- each slot keeps its own value rather than the last one written --
and only a shared control is hoisted, which is what it already means.

`poly` had to learn what D50 measured it was dropping: it carries each control's
declaration to its expanded name. `shared-gain` now keeps its shared region through
fusion, and two things are refused with a reason instead of quietly mangled:
expanding a shared control per slot, and expanding a lane (a lane belongs to the
entry process).

The end-to-end statement, which is what the whole M6a run was for: a pool over the
option renders bit-for-bit what a pool of separate voices renders at one sounding
voice, across three blocks; the fused artifact has one state partition per slot;
four voices share one instance. JVM 208 tests / 1839 assertions, browser 32/32.

**Next**: the bench's fused arm still builds its `(poly N voice)` directly, with
`poly`'s default per-slot set, so D45's fused numbers describe a slightly different
lowering than the option produces -- worth pointing at the option so there is one
definition of "the fused lowering" and re-running the sweep. Then **M6b** (per-slot
skipping) and **M6c** (the browser hosts, publishing partitions the way D51 does).

### The bench's fused arm goes through the option, and the number changes

`echo-fused-artifact` used to build `(poly N voice)` itself with `poly`'s default
per-slot set (`[:gate :note]`). The option expands every control a pool writes per
voice, so `frequency`, `delay` and `mix` are per slot as well -- four controls read
per frame per slot instead of two. With the bench pointed at the option (and
`echo-control-value` taught to strip the `_0` suffix, or the fused arm would have
been handed `0.0` for the note the pool arm was handed), the realistic sweep's
ratio drops from ~1.9× to **1.3–1.7×**: the earlier figure was measured on a shape
no instrument produces. Both surviving conclusions were re-verified: bit-exactness
at one voice (the test pins it through the option now) and a pool artifact flat at
2 741 bytes against a fused one that grows 2 875 → 4 608.

### Why seam 4 waited, asked as a question

Small correction while answering it, then the answer.

**The correction.** `synth-bundle` built the runtime config's `:special-mask`/
`:special-count` from the *base* voice's ABI even when the kernel is the fused
artifact -- and the pool reads the *kernel* when it asks a releasing voice whether
it is finished. A readback nothing fills stays at zero, so a fused synth whose
runtime claimed an activity special would free a releasing voice on its first
block. It is now sourced from the artifact that runs, which is what the documented
behaviour always assumed. Chasing whether it was *reachable* answered the question
properly: `special-mask` is "the specials a definition *writes*" (`used-special-ids`
of its body), so the two arms can only disagree if a voice writes a special -- and
such a voice cannot be fused, because the IR refuses a callee's `store-special`
outright. The mismatch was unreachable; the wiring is still the right one, and the
option test now pins the runtime's specials so the day a callee *can* write one,
the line is already there.

**The answer.** Seam 4 is the only one of the four whose absence cannot produce a
wrong sample. Seams 1–3 are load-bearing: without the key a fused instance is mixed
once per slot, without the mapping controls land in the wrong slot, without the
per-slot reset a steal clobbers its neighbours -- all three bite-checked, and the
third showed up as silence where the pool had audio. Seam 4's failure mode is the
opposite: the pool holds a finished voice a while longer. That is *already* the
documented safe default for a voice that declares nothing (D39), so the deferral
degrades into a supported path rather than a new one. And the work it needs is
independent: the IR must allow and rebase a callee's special store across every
calling target, and the artifact must publish a per-slot special map -- the sibling
of what D51 did for state ranges, but for the special index space.

---

## 2026-10-19 — M6b — per-slot skipping, and the latch that makes it correct

M6a gave fusion a slot-aware host; M6b stops a fused process from evaluating a slot
nothing is using. The measurement that opened the slice is worth recording first,
because it chose the design: on `echo-voice` through the pool at capacity 32, the
pool is flat in capacity (0.047 ms/block at two voices whether capacity is 8 or
32), while one `(poly 32 echo-voice)` process costs 0.375 at two voices and a
`(poly 2 …)` costs 0.035. So a fixed `:slots` cannot serve every occupancy — S=32
wins at a full patch and S=2 wins at a sparse one — and the thing that would make
one instance serve both is a **branch per slot**.

**The mechanism: a per-call-site latch, not a gate check.** The obvious lowering,
`(if (> gate 0) (voice …) 0)`, is wrong, and quietly: a pool closes a voice's gate
to *release* it, so a bare gate check cuts every release tail and breaks the
property fusion exists to keep — that it renders what the pool renders, bit for
bit. The wrapper that ships (`comb/guard`) owns one boolean state slot that latches
the first time `gate` rises above zero, *stays* set when the gate closes, and is
cleared only by the slot's reset. Three states fall out: never gated (skip), gated
and released (evaluate, tail), freed and reset (skip again). Because `poly` calls
the guarded voice once per slot, the latch lives in that call's state partition
(D19), which is what lets `reset-voice!` clear one slot without touching its
neighbours.

**What it did *not* need is the point.** No IR node, no ABI field, no artifact
change, no kernel change, no `fused-mapping` change, and no backend work: the guard
is scalar state, a comparison, and a `set!`, all of which the language already had,
wrapped by a combinator. The fused interface is byte-for-byte the same shape, so
`fused-mapping` and the per-slot reset ranges (D50/D51) apply unchanged, and M6c's
JavaScript and Wasm hosts inherit the skip for free. `synth-bundle` applies it when
fusing (`:guard`, default true) so the option's artifact is the guarded one; the
runtime configuration is unchanged, because the guard is a compile-time property.

**The numbers** (JVM, f32, 128-frame blocks, criterium best estimate; pool at
capacity 32 against one fused process at `:slots S`):

| slots | sounding | plain pool | guarded fused | unguarded fused |
|---|---|---|---|---|
| 8 | 2 | 0.045 | 0.036 | 0.097 |
| 32 | 2 | 0.045 | 0.045 | 0.467 |
| 8 | 8 | 0.235 | 0.108 | 0.104 |
| 32 | 8 | 0.235 | 0.121 | 0.392 |
| 8 | 32 | 0.825 | 0.447 | 0.438 |
| 32 | 32 | 0.825 | 0.482 | 0.371 |

So `:slots 32` at two voices falls from 0.467 to 0.045 — *the pool's own number* —
and at eight voices from 0.392 to 0.121, half the pool; a guarded single S=N
instance therefore beats the pool at every occupancy measured (0.045/0.121/0.482
against 0.045/0.235/0.825). The cost is at a full patch: ~2% at `:slots 8` and
~30% at `:slots 32`, where the extra branch sits in a larger method — and even
there the guarded process beats the pool. At one voice the guarded and unguarded
lowerings are still bit-identical to the pool, delay line included.
`realistic-row` gained `:sounding` and `:guard` so the comparison is a rerunnable
driver rather than a REPL session.

**Tests, all behavioral (no timing assertions).** A free-running oscillator voice
makes the skip visible in the output: a fresh guarded slot renders zeros where the
unguarded one advances; after its gate has opened and closed it is bit-identical to
the unguarded voice (the tail is preserved); and after a reset it renders zeros
again (the latch is clearable). The pool-level test runs a note-on, a note-off and
24 blocks of tail through a plain pool and a fused one and asserts every block
equals. JVM 213 tests / 1884 assertions, browser 32/32, all green.

**What is left.** M6c (the browser hosts over the same testbed arms); and the
fused voice still exposes no per-slot `:activity`, so a used slot is held until it
is stolen (seam 4, deferred — D45). `:guard false` restores the bare lowering.

---

## 2026-10-19 — Seam 4 — a callee's special is published per call partition, and a fused slot can finish

M6b's entry ends "a used slot is held until it is stolen, because a fused process
exposes no per-slot `:activity` (seam 4, deferred)". This closes that seam, so the
sentence is now history rather than a limit.

**What was in the way.** D39 made `:activity` an entry-process contract and
rejected a callee's `store-special` outright, because the slot would be rebased
into a call partition "where no host reads it". That is exactly where a *fused*
voice's activity has to live: the fused process is S callees, and there is no
single entry slot that means "slot 3 has finished". So seam 4 was not "read an
offset we know"; it was "publish the offsets, per slot" (D45).

**The change.** A callee may publish a special. The entry's own
`:special-mask`/`:special-slots` are unchanged and still describe only the entry;
each call site that owns a partition now records the specials **inside** it, one
per bit, the outermost (lowest logical index) winning when a nested module
declares the same bit. Because a partition covers its whole subtree, an activity
nested behind the definition the call site names is found there — and for
`(poly S voice)` call site `k` is slot `k`. The process ABI publishes the vector
as `:call-partitions`, the JVM target carries it on `:partitions`, and the fused
JVM kernel resolves each slot's activity to a physical float/int slot **once at
construction**, so `read-specials!` still reads primitives and touches no map on
the audio path. `synth-bundle` carries the **base voice's** mask (not the fused
entry's, which is empty) so the pool's `:activity` position is the same plain or
fused, and refuses at compile time if any slot's mask differs from the voice's.

**Why it is safe, and why per partition.** The slot is published, not lost: a host
that does not read it is unaffected, so a non-fused process that calls a module
with a special now records it per partition instead of failing. And the partition
is the unit a per-slot reset already restores (D51), so the activity and the reset
share one address space — the same reset that frees a slot also clears the guard's
latch (D53), which is why freeing a fused slot makes it immediately skippable
again.

**Verified.** The fixture `fixtures/activity-voice` is a module-shaped generator
whose `:activity` follows a linear envelope, so it reports *active* for a few
blocks after its gate closes. That tail is load-bearing: an activity that dropped
to 0 the instant the gate closed could not tell a working per-slot readback from
one that returned nothing, because both would free immediately.
`a-fused-pool-frees-a-released-slot-from-its-own-activity` runs a note-on, a
release and the tail through a plain pool and a `:slots 2` fused pool; both hold
the release tail for a block and then free it at the same block, and a fused
kernel that could not read its slot's special would free on the first release
block and fail. `a-fused-slot-publishes-its-own-activity` reads two slots directly
with a 42.0 sentinel — one held and one released past its tail must read 1.0 and
0.0 — so a readback that wrote nothing cannot pass. Before this slice the fused
pool held the released slot forever. JVM 215 tests / 1907 assertions, browser
32/32, all green.

**What remains.** The JavaScript and Wasm *kernels* do not read a fused slot's
special yet — that is M6c. The IR and ABI they consume already carry it, and they
continue to compile a callee's `store-special` as an ordinary state store. D39 is
amended (not deleted) to point here; D45's seam 4 is closed.

---

## 2026-10-19 — M6c — the browser fused hosts apply the partitions the ABI already publishes

This closes M6. Seam 4's entry ends "the JavaScript and Wasm *kernels* do not read
a fused slot's special yet — that is M6c". This is that slice, and it turned out
to need **no target, IR, ABI, or bundle-schema change**: seam 4 had already put
each call site's partition on the process ABI as `:call-partitions`, and a
browser host holds it.

**What the browser hosts were missing.** The guard (M6b) is ordinary DSP, so the
browser kernels inherited per-slot skipping for free. The other three seams are
host work: a base → fused control mapping (D50), a per-slot reset (D51), and a
per-slot `:activity` readback (D54). None existed outside `hosts/jvm.clj`.

**The shape of the fix.** `hosts/fusion.cljc` gained `runtime-mapping`, the
portable half of what `hosts.clj`'s private `fused-mapping` did, so "the fused
lowering" is one definition on both sides of the host boundary. `js-kernel` and
`wasm-kernel` take an optional `:mapping`; with one, `slots` handles are grouped
behind a single instance (the JVM kernel's `(mod @created slots)` grouping, one
instance key), `write-voice-controls!` writes through a precomputed per-slot row
of `fusion/fused-position`, `reset-voice!` restores one call partition, and
`read-specials!` reads that slot's partition's specials. Without one, every path
is the identity and the existing nine browser checks are unchanged.

**Resolution is a host loop, not a target output.** On the JVM the *target*
resolves partitions because the arena is two primitive arrays and a logical index
is not a physical one (D51). JavaScript's arena is one flat region indexed by
state slot, so a partition's `:base`/`:size` *are* its range: `create-voice`
snapshots the arena once and a reset copies the range back. Wasm's state region is
byte-addressed and its layout is already served as data in `:memory`
(`:state-layout`, because an int slot in an f64 block is padded to the block's
alignment), so the host resolves each partition's byte range and copies it from an
initial snapshot. Adding a `:partitions` artifact key and shipping it would
duplicate information the browser already has, so — like the ABI's
`:call-partitions` being enough for JS — the decision is to keep it host-side.

**How it is checked.** `harness/testbed.clj` serves a `fused-pool-bundle` (the
base artifact's ABI and IR for the interpreter oracle, the fused artifact's ABI,
the `:fusion` mapping, and, for Wasm, its WAT and memory) and a `fused-pool-source`
(the fused JavaScript module). The two new browser checks — "JavaScript fused
pool" and "Wasm fused pool" — drive the fused pool and a plain interpreter pool
over the *base* voice through the same operations and compare the rendered blocks:
two slots across a steal (slot 0 gated shut so its contribution is exactly zero,
which lets the block be compared sample for sample), then a release, asserting the
tail is held for a block and the voice is then freed. `fixtures/fused-browser-voice`
is module-shaped, arithmetic (no transcendental, so the differential is exact),
and carries both a per-voice envelope and a delay line, so a whole-instance reset
would be noticed; it publishes `:activity` from an envelope with a short tail, so
a readback that returned nothing would free a block early.

**The checks bite.** With a whole-instance reset stubbed into the JavaScript fused
kernel the steal check fails (the neighbour's block diverges — the same bug the
JVM test guards); with one stubbed into the Wasm kernel it fails too. With the
fused `read-specials!` stubbed to return nothing, the activity check fails
(`{:held 0}` against the interpreter's `{:held 1}`). A no-op reset only touches
the slot that is silent by construction, so it is not observable — the same limit
the JVM test has, and the reason the bite is a whole-instance reset.

**Verified.** JVM 215 tests / 1907 assertions, browser **34/34** (two new arms),
`clj-kondo` and `cljfmt` clean. D55 records the decision; the pool checks that
were there stay green, because a kernel with no mapping is unchanged.

---

## 2026-10-20 — F0 decided, and F1's JVM half — fusion reaches the instrument layer

M6 built a fused pool that renders a plain one and runs on every host, but it was
reachable only by calling `compile-synth`/a pool host directly, and only for a
module-shaped, lane-free voice. This opens the **F workstream** (PHASES.md) to
deploy it, and settles F0.

**F0 — the spine (D56).** Every per-process region a callee touches is owned by
the *call site* and carried by the callee ABI, exactly as the state arena already
is (D19): output buffers partitioned per call site (F2), the lane region passed
to the callee (F3) and partitioned per call site for a per-voice lane (F4). The
reason is D19's: two slots of one voice are two voices, and one buffer would make
them overwrite each other. Reading the code while deciding turned up two facts
worth recording: the module/process refusal is a **linker** rule
(`linker.clj:253`), not the IR, so F2's load-bearing change is relaxing it *only*
for a call that owns a region; and a process body is already evaluated per frame
(D40) with `buffer-store` writing the current frame (confirmed: `reference-voice`
stores `:output` by channel and the current frame), so the fused entry needs no
new scheduling model — call each slot, then store the sum to `:output`.

**F1's JVM half.** `instrument-pool` now threads the bundle's `:fusion` into
`host-kernel`. This closed a **live bug**, not just a gap: `compile-instrument`
already forwarded `:fusion` to `synth-bundle` (so the artifact really was fused),
while `instrument-pool` built a *plain* kernel over it — a module-shaped
instrument compiled that way threw `Index 4 out of bounds for length 4` at the
first `note-on!` (verified before the fix). The regression test
`a-fused-instrument-renders-what-a-plain-instrument-renders` builds the same
module-shaped instrument both ways and compares a rendered block; it bites —
reverting the thread makes it throw the original error. F1's browser half (the
instrument testbed, which would serve the base voice artifact for the interpreter
oracle and the fused one for the kernel, as the fused-pool endpoints do) is next.

F5 (a fused AudioWorklet) was moved from optional to in scope at the user's
request.

---

## 2026-10-20 — F2 scoped — process-shaped fusion needs a mechanism *and* a surface

Before writing F2 I read the three pieces it touches, and it is two changes, not
one. The record, because it is the shape of the slice:

- **The refusal is a linker rule**, not the IR (`linker.clj`'s `resolve-definition`
  rejects any called definition whose body or state accesses a process buffer).
  So relaxing it is F2's load-bearing change, and it must be relaxed *only* for a
  call that owns a region — a call with no region keeps today's honest error.
- **A process body is per-frame, and `buffer-store` writes the current frame.**
  `frame-index` maps a literal offset to `:frame-offset` ("current frame + 0"), so
  `reference-voice`'s `(buffer-store :output 0 0 left)` writes channel 0 of the
  *current* frame. That is why F0's answer is small: the callee stays a per-frame
  function, its region base is one more ABI parameter, and the fused entry sums
  one frame at a time. No new process structure, no block-level call.
- **The hard part is the surface.** `poly` must declare the per-slot regions
  (scratch records), call each slot into its region, and read the regions to sum
  them. But `graph/derived-definition` forces `:state nil`, and there is no DSL
  form for "call this callee with its output buffers bound to *this* region".
  Records are state, not call returns, so the voice→module adapter is not
  expressible (a call returns a scalar, and the reference voice is stereo).

So F2 is scoped F2a (mechanism + one new form, on the interpreter) → F2b (JVM) →
F2c (JS/Wasm) → F2d (reference instrument fuses; the browser instrument host then
switches on, which is F1's second half). The new form's shape is the open design
decision — it becomes DSL surface, so it is worth choosing deliberately rather
than by accident.

---

## 2026-10-20 — F2a — process-shaped voices fuse on the interpreter (completed)

**Delivered.** The mechanism, the surface, and the invariant beneath them:

- **Surface (D57).** `(with-buffers <binding> <call>)` binds a process-shaped
  callee's output buffers to region names the call site reads; the binding is a
  map from callee buffer id to region name, or the bare name as sugar when the
  callee writes exactly one output buffer. `linker.clj` threads a `buffered?`
  context through `call-sites`/`rewrite-calls`, so the buffer refusal relaxes only
  for the wrapped call; a plainly-called buffer writer still refuses with
  "cannot access the caller's process buffers".
- **Mechanism.** A bound buffer is a **region buffer** of the entry
  (`:direction :region`, per channel like `:output`); the callee's output buffers
  lower as a process-shaped module's own buffers, and `ir/lower-call` records
  `:regions {callee-buffer region}` on the call. The interpreter allocates the
  regions once per block and `evaluate-call` overlays them into a *fresh* callee
  context, so the caller's context is untouched. No ABI, target, or bundle-schema
  change was needed for F2a.
- **A process-shaped callee returns `:void`** (D58): the descriptor allows it for
  a `:process` definition, `lower` omits the return, `coerce-value` handles
  `:void`, and the module-buffer setup is gated on `(:process definition)` so a
  plain function still has no buffer namespace.

**The trap that cost the most.** `descriptor/default-options` carries
`:channels 1` and `normalize-options` merges it unconditionally, so
`(:channels options)` is never a "was a width declared?" signal. F2's first cut
read it in `lower`, pinning every module to one channel; the failure was a
stereo voice writing `:output 1` into a one-channel buffer. Worse, the *first*
fix only *dissoc'd* `:channels` when the top-level width was inferred and never
*wrote* the resolved value when it came from `:process :channels`, so an explicit
2 still reached modules as the default 1. `compile` now writes the resolved width
into `lowering-options` (or removes it when inferred), and `lower` reads the
forced width first, then the module's own declaration, then its body. Inference
does not propagate: a mono helper in a stereo entry stays mono (**D58**).

**Tests.** `a-fused-process-voice-writes-the-call-sites-region` renders the
fused entry and asserts it is exactly the sum of two separately-rendered voices,
frame and channel, with an audibility guard so `0 == 0` cannot pass;
`a-callee-that-writes-its-output-must-be-given-a-region` and
`with-buffers-refuses-an-unbindable-region` pin the two refusals. **Bite-check:**
disabling the region overlay (a `false` branch) collapsed the fused output to
zeros and failed all three assertions. The JVM target does not yet handle
regions — that is **F2b**, which the interpreter-only test waits on.

**Suite.** JVM **219 tests / 1918 assertions, 0 failures**; `clj-kondo` and
`cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F2b — regions on the JVM, and the F2a rework that made them free (completed)

**Delivered.** F2b is small because F2a was reworked first.

- **F2a reworked to D57's design (D59).** F2a had given the entry a per-call-site
  `:region` *buffer* (a per-channel array) and overlaid it into the callee's
  context. That worked on the interpreter but asked the JVM for a region *buffer
  argument* — a new ABI shape — and every other target for the same plumbing. The
  rework makes a region exactly what D57 said: a **state slot in the callee's call
  partition**. A process-shaped module's output buffers become state (`channels *
  stride` floats, the lane/shared stride), its `buffer-store`/`buffer-load` lower
  to `:state-store`/`:state-load` with the channel folded into the element index,
  and a `with-buffers` name aliases the callee's rebased slot. The interpreter's
  region code (an allocation in `process-context`, an overlay in `evaluate-call`)
  was **deleted**; the entry's `:buffers` keeps only `:output`.
- **The frame (D59).** A region is frame-indexed; a per-sample callee has no
  frame local. So a callee with `:region-slots` gets one extra leading `:int`
  parameter — the current frame — between `sample-rate` and its arguments, and
  `callee-descriptor`/`callee-slots`/`call-instructions` agree on it. A callee
  with no regions is unchanged. The interpreter needed nothing: its callee context
  already carries `:frame`. **Bite-checked** — passing a constant zero left only
  frame 0 correct and failed the test on frames 1..7.
- **The JVM's `:void`.** `jvm-type` maps `:void` to the `:void` descriptor insn
  uses; a void module's method gets a trailing `return`; a void call is not
  `pop`ped. Two compile-time guards (a state access or a local with no layout
  entry) now say so instead of emitting a null var index — they cost nothing, being
  emitter-time.

**Tests.** `a-fused-process-voice-writes-the-call-sites-region` now runs the
interpreter *and* the JVM and requires them bit-for-bit equal to the sum of two
separately-rendered voices, and pins that the regions are state (entry `:buffers`
holds only `:output`, two call partitions, `:state-size` = two regions).

**Suite.** JVM **219 tests / 1926 assertions, 0 failures**; browser **34/34**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F2c/F2d — the rest of fusion (done)

**Delivered.**

- **F2c (targets).** JavaScript and Wasm apply a region-writing callee's per-slot
  region as ordinary state, so the only target facts were the frame parameter and
  the `:void` return. JS needed only the frame (it is dynamically typed); Wasm
  also needed `:void` in `wasm-type`'s result position and no `drop` after a void
  call. The region itself, the void module, and the frame were already correct in
  the shared IR.
- **F2d (`poly`).** `poly` now fuses a *process-shaped* voice (D60): each slot's
  call is wrapped `(with-buffers :slot-k …)` and the fused body sums the regions
  into `:output` per channel, requiring the voice to declare `:process :channels`.
  `reference-voice` now declares `:void` and `:channels 2`, and the reference
  instrument fuses on every target.
- **The interpreter hosts fused pools (D60).** The interpreter kernel applies the
  same base → fused mapping and resets one slot's partition from the ABI's
  `:call-partitions`, so a fused pool on *any* host has an oracle. This was the
  gap you flagged; without it the JVM and the browser only agreed with each other.
- **Guard.** A process-shaped voice is fused without the M6b guard (it writes a
  region, not a scalar), an optimisation withheld rather than a correctness gap.
- **Checks.** `a-fused-pool-runs-on-the-interpreter-oracle` (fused == plain on the
  interpreter, and the JVM matches it); `a-fused-reference-instrument-renders-
  what-a-plain-one-renders` (the stereo reference instrument, interpreter + JVM,
  bit for bit, with an audibility guard); and a browser check that renders the
  fused reference voice against the *plain* voice on the same kernel, so the
  transcendentals cancel (JavaScript matches exactly). **Bite-checked**: a
  one-channel region (the channel factor dropped) errored the stereo test.

**Bugs found on the way.** A `list*` splices only its *last* argument, so the JS
call emitted the optional frame as a literal `null` argument on *every* call — the
whole shared JS matrix regressed, which is how it was caught. Wasm `drop`ped a
void call's absent result. And a restored bite-check whose `ir.clj` was never
reloaded made the region one channel wide in the running image; the browser
fused-reference check is what exposed it.

**The Wasm item, closed.** The **Wasm** fused reference voice drifted from the
plain Wasm voice (~0.26), and the cause was not fusion at all: a Wasm instance
starts from zeroed memory and reaches the IR's initial values only through the
module's `reset` — and the pool resets a voice only when it **steals** one, so a
*fresh* Wasm voice rendered from zero, while the JavaScript host seeds the state
array in `create`. The fused path had a second, independent bug: it snapshotted
`:initial` before seeding (so the per-slot restore copied zeros), and
`partition-ranges` resolved a partition one arena index at a time, but a region is
a *single* layout entry spanning thousands of indices — so the interior had no
entry, `min` collapsed the start to nil, and the reset built a negative-index view
that restored nothing while writing state bytes into the input region. Both
fixed (D61), both bite-checked independently, and the browser check now steals a
voice so the per-slot reset is exercised. The fused and plain reference voices
now render **bit for bit** identically on JavaScript *and* Wasm (`worst 0`).

Two lessons, both about "the oracle": the drift was a *host* divergence that the
fused check merely made visible, and the segment bug was invisible to every
module-shaped fused check because only a region makes a multi-slot partition.

**Suite.** JVM **221 tests / 1933 assertions, 0 failures**; browser **36/36**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`. Verified in a fresh JVM as
well as the long-lived one.

---

## 2026-10-20 — F1's browser half — served, written, and held on a kernel divergence

**What landed.**

- **The browser fused instrument is served.** `fused-instrument-bundle` and
  `fused-instrument-source` carry the same pair the fused-pool endpoints do: the
  fused voice's runtime, ABI, IR, and `:fusion` mapping; the *plain* lowering of
  the same voice as the same-target oracle (D14), so the transcendentals cancel;
  and the modulation matrix and post-mix bus, which are not fused.
- **`check-fused-instrument`** rebuilds the host over them and asserts the fused
  instrument renders the plain one's phrase, finite and block-size invariant. It
  is written for both JavaScript and Wasm and is **not registered** yet.
- **A fused runtime holds its slots (D62).** `runtime-config` takes `:capacity`
  from the description, and the fusion branch never overrode it, so a fused build
  of an eight-voice description produced a pool of eight voices over a two-voice
  artifact. The kernels map a pool voice onto a slot by creation order, so several
  handles landed on one slot — and `modulation/publisher`, which writes the
  matrix's targets to *every* voice each block, had them clobber each other. The
  browser fused-pool endpoints had been setting `:capacity slots` by hand, which
  is what the lowering should have done. Pinned by
  `a-fused-instrument-holds-exactly-its-slots`.

**The blocker, and what is eliminated.** The fused **reference instrument** —
process-shaped, with smoothed controls — does not match its plain twin on the
**JVM** or **JS/Wasm**, though it matches exactly *until a smoothed control
departs from its default* (`pulse-width`, `cutoff`, `resonance`); every other
control is bit-identical, and `:guard` on or off, `slots` 1, 2 or 4, and f32 or
f64 all behave the same. The fused signal is ~1e-4 smaller at first and settles a
few percent low.

The **interpreter is exact** for the same comparison with the same IR and the
same control values, and interpreter-plain equals JVM-plain, so the fused
lowering is right and the JVM/JS kernels are wrong. Three candidates are already
ruled out, each by measurement rather than by reading:

- **The control mapping.** `fusion/fused-position` maps every base control onto
  its fused twin — printed position by position — and for `slots 1` the fused
  interface is the base interface with the hoisted pair moved to the front, so
  delivery is effectively an identity.
- **The control values.** The pool resolves identical values for both, and the
  controls that diverge are exactly the smoothed ones.
- **Rebasing.** If the callee's state accesses were unrebased, the partition base
  would matter: dropping the entry's duplicated ramp slots moves that base and
  must change the audio. It does not — the divergence is bit-identical — which is
  also how we know those duplicated entry slots were dead code.

A minimal voice with a single smoothed control fuses *exactly* on both targets, so
the reproducer needs more of the reference voice's structure; that is where this
picks up. One near-miss is worth recording: the Wasm host had a real bug of the
same family (resolving a partition by arena *index* rather than by layout
*segment*, which a region breaks), but the JVM's `partition-ranges` already
handles multi-slot entries, so that is not this one.

**Suite.** JVM **222 tests / 1935 assertions, 0 failures**; browser **36/36**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F1 closed — the drift was a doubled ramp, not a kernel bug

The fused **instrument** disagreed with the plain one as soon as a smoothed
control left its default, on the JVM and in the browser, by ~1e-4 and settling
percent low. It looked like numerical drift — the interpreter seemed exact, which
sent the hunt into the JVM host's control delivery, the fused partition's arena
base, and the region address lowering. All three were innocent.

**What it actually was.** A **derived definition ramped the controls it merely
forwards**. `derived-definition` carries the voice's control *declarations* across
— including `:smooth` — and `normalize` turned each of them into a ramp state, so
the wrapper smoothed the raw control and handed the *smoothed* value to a voice
that smoothed it again. Two poles compound, so the derived signal lagged: percent
for the first milliseconds, settling only after thousands of frames. The decay
that made it look like a transient was real, and it was the tell.

It was never `poly`'s alone. `guard`, `unison`, `share`, the instrument wrapper,
and the modulation transformers all build on `derived-definition`, so any voice
that passed through a wrapper doubled its smoothed controls.

**How it was pinned.** A generator whose whole output *is* a smoothed control
made it exact and legible in eight samples: the plain voice ramps
`[0.0 0.5 0.75 0.875 …]` and the fused one `[0.0 0.0 0.0 0.125 0.3125 …]`. Reading
the interpreter's own ramp state settled the remaining doubt — plain and derived
ramp *identically*, so it was never the ramp's shape; and the state showed the
same control ramped twice, once in the wrapper and once in the voice. Everything
larger was consistent with several stories; the small subject was not.

**Two things the hunt got wrong, worth remembering.**

- **The "interpreter is exact" evidence was a stale Var.** `tools/reload`
  unloads and rebuilds a namespace, so a REPL helper compiled *before* the reload
  keeps calling the *old* Var, and therefore the old `poly`, forever. Several
  measurements taken after the fix were really measurements of the unfixed code.
  Helper functions in `user` must be re-evaluated after a reload.
- **`guard` doubled, but it was not the only doubler.** The M6b guard is withheld
  for process-shaped voices, so the process-shaped *reference* instrument was
  never guarded — its divergence came through `poly` itself. Had the fix gone
  into `guard` alone, the reference would still have drifted.

**The fix.** `graph/derived-definition` marks its result
`:dsp/forwarded-controls`; `descriptor/normalize` reads the mark and skips ramp
creation, so the declaration still travels — the ABI a host reads still reports
the mode — while the value stays raw until the voice that owns it ramps it, once
per call site. That is D63. The `:smooth`/`:slew` stripping tried earlier inside
`combinators/poly` is gone: one rule, in one place. It was bite-checked by
removing the mark: the first block becomes
`[0.0 0.0 0.0 0.125 0.3125 …]`, and the check fails.

**Also landed.** The fused runtime's capacity is the slot count (D62): a fused
build of an eight-voice description used to make an eight-voice pool over a
two-voice artifact, so several handles shared one slot and the modulation
publisher — which writes every voice each block — had them clobber each other.

**Registered.** `javascript-fused-instrument-test` and
`wasm-fused-instrument-test` are checks now, not comments: F1's browser half.
They were failing before this and pass after, which is what proves they are not
comparing a plain kernel with itself.

**Suite.** JVM **223 tests / 1937 assertions, 0 failures**; browser **38/38**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F3 — a lane reaches a callee

**What it was.** `(lane control)` was refused anywhere but the entry process
("lane is only available in the entry process", `ir.clj`), for a shared lane as
much as a per-voice one, so automation died at the first module boundary and a
caller had to compose at the call site. That refusal is gone: a definition that
reads a lane is now handed the region.

**The shape (D64).** One pointer per lane-reading definition, plus the current
frame. Not a base into the callee's own partition: a lane's offset is
`index * frame-capacity` — a constant of the *entry's* layout (D28) — so the
region is all a callee needs, and every definition in the artifact reads the same
slices. A definition that only calls a lane reader forwards its own region, so
`lower-definitions` propagates the flag up the call graph.

**The two halves, and where each was caught.**

- The *IR* half — the lane table reaches callees, the refusal goes, and a
  lane-reading definition is marked — was caught by the JVM suite: bite-checked
  by withholding the table, which refuses with "lane is only available where a
  lane region is".
- The *target* half — passing the region and, with it, **the frame** — was caught
  by the browser. A lane read is per-frame, so a lane reader needs the frame for
  the same reason a `with-buffers` region writer does (F2), and the first cut
  gave the lanes path the region but not the frame: the JVM emitted `[:iload
  nil]` for the read's index (found by printing the callee's instructions) and
  the browser matrix failed on both JavaScript precisions with `ReferenceError:
  lanes is not defined` and, once that was in, `frame` likewise. One predicate
  now answers for regions and lanes on all four targets.

**Verified on all four targets** by a case in the shared catalog,
`:automation-lane-in-callee`: an entry that declares a lane and calls a callee
whose body reads it with `(lane amount)`. The JVM suite runs it on the
interpreter and the JVM; the browser matrix runs it on JavaScript and Wasm, f32
and f64. Nothing was hand-written for any target — that is the catalog's point —
and the browser went from two failures to green on exactly this case, which is
what makes it a bite check rather than a formality.

**A note on the tooling.** Two of this slice's measurements were wrong before
they were right, both for the reason F1 recorded: `(tools/reload '[a b])`
*forces* those namespaces and skips detection, so a file edited but not named
stays stale in the image — which is how the JavaScript target kept emitting a
callee without its lane parameter while the source clearly had it. The
no-argument `(tools/reload)` is what finds everything. The tool now says so
itself: a named target reports `:stale` — the loaded namespaces whose content
changed and were not reloaded, with a note — so the image's freshness is never
something a caller has to remember to check.

**Suite.** JVM **223 tests / 1944 assertions, 0 failures**; browser **38/38**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F4 — a lane reaches every fused slot

**What it was.** A fused entry renders every voice in one call, so it owns *one*
lane region — and a lane is per voice. `poly` therefore refused one on a per-slot
control outright ("poly cannot expand a lane per slot: a lane belongs to the entry
process"), and a lane-only control slipped past that refusal with a worse outcome:
it was hoisted, one slice was laid out, and every slot read the same automation,
whichever voice happened to write it last. Two different curves make that a wrong
block rather than a missing one, which is how the pin was written.

**The shape (D65).** The region is the entry's, `slots` slices of it, and a call
site hands its callee the base of **its** slice: zero-based lane offsets plus a
base, exactly as a callee's state indices are resolved against a partition base
(D19). Nothing else in the ABI changes — a lane-reading callee gains one `int`
next to the region it already had — and a callee that merely forwards the region
passes the base it was handed. Three further facts fell out of "one slice per
voice":

- **Expansion does not multiply lanes.** `poly` expands a per-slot control, but a
  lane belongs to the *control*: the expanded declarations carry the lane's name
  (`:lane-name`), so the entry lays out one lane per base control and the slots
  differ by slice. Without this the callee's `(lane amount)` could not resolve at
  all: the entry's layout held `amount_0`/`amount_1` and the voice declared
  `amount`.
- **A lane-only control's name is a lane read wherever it is read.** In the entry
  that was already true; a callee now binds it to a read of *its own* slice rather
  than to the value the caller passed. Where the artifact has one slice the two
  are the same read, so no existing check moved.
- **A slot is a call partition.** The base a call site hands out is the partition
  ordinal — the same index a host resets one voice by (M6a) — so a fused entry's
  lane-reading call site has to own one; it is refused loudly otherwise rather
  than given an arbitrary slice.

**Where each half was caught.** The IR half and the JVM/interpreter hosts were
caught by the JVM suite; the two pins were written first and failed for exactly
the two reasons above (`[40 40 40 40]` where the plain pool gave `[22 24 26 28]`,
and the refusal). The JavaScript *host* half — its fused branch still allocated a
one-slice lane region, so slice 1's writes went out of bounds and read as zeros —
was caught by the new browser check, which is the bite check for a line no JVM
test can reach: it reported `[[0 0 0 0]]` against the interpreter's
`[[22 24 26 28]]` and went green on the fix.

**Verified on all four targets.** The JVM suite renders both pins on the
interpreter and the JVM (`a-fused-pool-gives-each-slot-its-own-lane`,
`a-fused-pool-expands-a-lane-carrying-control-per-slot`), each against a plain
pool over the same description as the oracle; the browser runs `JavaScript fused
lane` and `Wasm fused lane` over a served fused bundle, with two curves written to
the two voices and the interpreter pool as the oracle. The per-slot-lane refusal
in `poly`'s tests became a positive test of the lane that travels with an expanded
control.

**Suite.** JVM **225 tests / 1953 assertions, 0 failures**; browser **40/40**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-20 — F5 — a fused AudioWorklet

**What it was.** The worklet adapter serves one kernel instance per node, so a
fused voice — one process holding `slots` voices — needed a node that renders all
of them, with a parameter of its own for every per-slot control. F5 is that node.

**What it turned out to be: plumbing, not a mechanism.** The adapter writes each
parameter into the kernel's control array at the parameter's own index and builds
its own kernel, so there is no pool inside a worklet and *nothing has to be
mapped*: the node's parameters simply **are** the fused interface, in ABI order
(`gate_0`, `frequency_0`, `delay_0`, `mix_0`, `gate_1`, ...). The plan had assumed
the pool's `:mapping` would have to be applied here; it is not needed, because
`:mapping` exists to translate the *pool's* base-shaped writes, and a worklet has
no pool. So the change is: the served adapter's parameter descriptors are
generated from the fused artifact's ABI instead of being a `one-pole` literal, and
one fused arm is served per target.

**Two bugs fell out, and the check caught both.**

- The adapter read `processorOptions` unguarded, so a node created without them
  threw where nothing could see it: the worklet reported a bare `processorerror`
  and the page saw only a node that never answered. Both adapters now default it
  to an empty object, and the JavaScript one also reports a constructor throw
  through its own port, so `worklet-ready` rejects with the *reason* instead of
  timing out. That is how "Cannot read properties of undefined (reading
  'controlNames')" was found — and it was mine: the page had put the processor
  fields at the top level of the node options rather than under
  `processorOptions`.
- A stub of the descriptor *order* did **not** bite, which is worth writing down:
  the adapter indexes by `processorOptions.controlNames`, not by the descriptor
  list, so `parameterDescriptors` only has to *name* what the page drives. The
  check does bite when the per-slot *names* go missing ("rendered silence for a
  gated slot").

**The check.** `JavaScript fused worklet` and `Wasm fused worklet` each render one
node three times — slot 0 alone, slot 1 alone, and both — with the second slot at
twice the first's `frequency`, and ask two things of the blocks: that slot 1's
peak is twice slot 0's (a parameter reached the slot it names, not a neighbour's
control block) and that the both-slots block *is* their sum (one node rendered
both voices, not one). The worst difference is one f32 ulp-ish (3e-5), and the
same numbers come out of both hosts.

**Suite.** JVM **225 tests / 1953 assertions, 0 failures**; browser **42/42**;
`clj-kondo` and `cljfmt` clean on `src/omkamra/dsp`.

---

## 2026-10-21 — H2 — the post-mix chain gets its own lane and shared regions

The audit item was "the browser bus carries neither lanes nor a shared region":
`hosts/js.cljs` and `hosts/wasm.cljs` refused at bus construction. Reading the
code first changed what the slice is — the refusal was **unreachable**. A
post-mix stage *could* write `{:name amount :lane true}` and the compiler would
honour it, but `chain-effect` (which rebuilds the chain's interface at every
stage, and overrides `:process :controls` to do it) rebuilt each control as bare
`{:name control}`, so the declaration never reached the entry process. The gap
was therefore in **lowering** as much as in the hosts, and the same is true one
level down: `hosts/instrument.clj`'s `run-bus!` handed the bus neither region
(`process-unchecked!` with only the physical control arrays; `dsp/process!` with
only `:controls`), so even the interpreter target could not run a lane-carrying
chain.

**What landed (D67).**

- `chain-effect` keeps each control's declaration — the chain's own, and the
  effect's (paired positionally, because the effect names its controls by *its*
  parameters while the chain names them by the *arguments*). A declaration has to
  reach the entry process: the derived body calls the effect, and the effect
  evaluates its own controls, but the region a lane or shared control is read from
  belongs to the entry.
- The chain's regions are **instrument-level**: one lane region and one shared
  region, written once and handed to every channel's instance — the pool's
  `:shared` region, one level up. The instrument host owns them
  (`set-bus-lane!`; the shared region is filled from the shared *parameter* of the
  same name, the rule a scalar bus control already follows, so `set-parameter!`
  reaches the chain too) and `run-bus!` passes them to the JVM *and* the
  interpreter path.
- `dsp/process!` now accepts a region a host already owns, not only named values
  to flatten. Its docstring already promised that path ("a real-time host writes
  the region directly and skips this path"); there was no way to take it.
- The browser buses do the same. JS hands the host's region to every instance in
  its argument array; Wasm copies the frames a block can read into each instance's
  own memory before the call — the handoff the pool already gives a voice's shared
  region, because a Wasm instance owns its memory. Both runners gained
  `:write-lane!` / `:write-shared!` (a bare number broadcasts, as `dsp/process!`
  does for a shared control).
- The in-page interpreter bus runner gained the same two writers, so the browser
  checks stay differential.

**The fixture is the check.** `fixtures/bus-gain-voice` renders exactly `gate`,
i.e. 1.0 for every frame it sounds, so a post-mix stage whose gain is the region
makes the samples *be* the curve — nothing to compute, nothing to tolerate.
`bus-lane-instrument` and `bus-shared-instrument` are the two chains, pinned:

- JVM (the oracle) and interpreter: `a-post-mix-lane-reaches-the-bus` (renders
  `[1..8]`, plus the two loud refusals: an unknown lane name, and
  `set-bus-control!` on a lane-only control, which has no scalar) and
  `a-post-mix-shared-control-follows-its-parameter` (the parameter's default fills
  the region at construction, and setting it reaches the chain — not only the
  voices).
- Browser: `JavaScript post-mix lane/shared` and `Wasm post-mix lane/shared`,
  each compared with the in-page interpreter host *and* against the exact curve.

**Bites, all four.** Dropping the declaration merge in `chain-effect` → exactly
the new assertions fail (6 failures, 1 error); a no-op `set-bus-lane!` → the lane
samples become zeros; a no-op `write-bus-shared!` → the shared region stays zero;
on the page, not handing the lane to the JS instance → the suite fails with
`Cannot read properties of undefined (reading '0')`, and skipping the Wasm
per-block copy → `[[0 …]]` against `[[1 2 3 4 5 6 7 8]]`.

**Counts.** JVM 227 tests / 1973 assertions / 0 failures; browser 46/46 (was
225/1953 and 42/42). H3 (a lane on a per-channel control) is next.

---

## 2026-10-21 — H3 — a lane beside `:bend` is a curve per channel

The audit item was "a lane on a per-channel control has no delivery", refused at
pool construction. Reading the code made the shape of the answer clear: the
pool's `:bend` is a *source bank indexed by channel* — `set-pitch-bend!` takes a
channel, and `resolve-controls!` reads `(aget bends (aget channels index))` — so a
lane beside it is a curve per channel, and the scope should be inferred from the
source, which is exactly what `lane-scope` already does for `:shared` and
`:per-voice` parameters. `:bend` was the one exception, and it was an exception
because the delivery did not exist, not because the scope was wrong.

**What landed (D68).** A third lane scope, `:channel`, and the delivery it needs:
the pool allocates a `channel-count` × `lane-stride` curve bank at construction,
`set-channel-lane!` copies a host's curve into it and writes it to every voice
already on that channel, and `sound-voice!` delivers a channel's curves to a voice
as it starts. The two facts that force the copy are worth stating: a voice that
starts *later* must read the curve published for its channel (a setter cannot
reach a voice that has not started), and a voice can *move* to another channel (a
steal, a mono retrigger, a host playing several channels) while its lane region is
per voice — so without re-delivery it would keep reading the channel it left. The
other two setters now refuse a channel lane with a message that names the right
one.

**The check.** `a-per-channel-lane-reaches-the-voice-on-its-channel` runs on the
JVM and the interpreter: two curves published before any note, one voice per
channel reading its own curve frame for frame; a channel nobody wrote reading
zero; a host's buffer mutated after the write, so a pool that only *pointed* at it
would read the wrong thing; a curve published while a voice is already sounding
(the write-time delivery); and one voice taking three channels in turn (the
activation delivery). In the browser, `JavaScript`/`Wasm channel lane` run the
same steps through a real pool over a browser kernel against the in-page
interpreter pool, with the same literal numbers.

**Bites, all three, plus the page.** No activation delivery → 6 failures (the
other channel reads channel 0's curve, and the moved voice keeps the old one); no
copy (the bank stays zero) → 14 failures; no set-time delivery → the "already
sounding" check keeps the old curve (2 failures). On the page, the activation stub
renders `[[1 2 3 4] [1 2 3 4] [1 2 3 4] [5 5 5 5]]` — the voice never follows its
channel.

**Counts.** JVM 228 tests / 1991 assertions / 0 failures; browser 48/48 (was
227/1973 and 46/46). H4 — a fused slot identity that does not require state — is
next.

---

## 2026-10-21 — H4 — a slot is the call site's identity, and state is one thing it may own

The audit item was "a fused voice with no state and no `gate` cannot fuse", refused
by every host with "a slot cannot be reset on its own". Reading the code showed the
refusal is *false* for that voice: a stateless slot has nothing to reset. What the
refusal actually said was that a slot was *derived from* a state partition — the
hosts counted partitions, the lane base indexed by partition ordinal, the specials
readback hung off one — so a voice with no state had no identities at all. The
plan's two options were "give every call site an identity of its own" or "decide a
fused voice must carry state"; the first is what the ABI's own name
(`:call-partitions`) and every host's message ("the number of call sites the
process has") already claimed.

**What landed (D70).** `ir.clj`'s `lower-call` always records a partition, empty
when the callee has no state, so slot *k* is the *k*-th call site whether or not it
carries anything. Three consequences: a stateless voice fuses (S identities, a
no-op reset, no specials — the pool already handles a voice with no `:activity`); a
lane-reading callee gets its slice without state (the base is `slot × lanes ×
stride` off the call-site ordinal, which also removes a latent shift where a
stateless call site before a stateful one skewed every later slice); and the Wasm
host's byte-range scan has to accept an empty range, since an empty partition
overlaps no layout segment. The JVM's range resolution already returned an empty
range for one and the JavaScript host loops `size` times, so only Wasm named the
degenerate case.

**The check.** `a-fused-slot-does-not-need-state-of-its-own` pins both halves on
the JVM and the interpreter: `stateless-voice-description` with `{:fusion {:slots 2
:guard false}}` renders what two plain voices render (92 = 60×1.0 + 64×0.5, so a
comparison of silence cannot pass), and `gated-lane-only` — no state at all —
renders F4's own numbers `[22 24 26 28]`, each slot reading its own lane slice. The
artifact is asserted directly too: two partitions, each `{:size 0}`. In the
browser, `JavaScript`/`Wasm stateless fused pool` run the stateless voice through
a real kernel against the in-page interpreter pool, which is what exercises the
Wasm host's empty-range path.

**Bites, and one that did not bite.** Reverting the partition to "stateful call
sites only" → all three new assertions fail with the original refusal (the pool
cannot even be built). Throwing on an empty Wasm range → the Wasm check fails
("empty partition") while JavaScript still passes — precisely the asymmetry that
motivated the fix. The *first* attempt at that bite removed the guard but left the
parentheses unbalanced; Shadow could not rebuild that stub and served the previous
build, so the check passed — a false negative indistinguishable from "the guard is
not needed". A bite stub has to be checked for validity before its silence is
believed.

**Counts.** JVM 229 tests / 1999 assertions / 0 failures; browser 50/50 (228/1991
and 48/48 after H3, 225/1953 and 42/42 before H2). H5 — array fields in records,
the one remaining H item — is a decision, not a slice; H1 stays deferred until
P13/P14.

---

## 2026-10-21 — H5 — an array of records owns an array field, and a bank is just a record

The last H item was a *language* omission, not a host gap: `descriptor.clj` refused
an array of records whose field declared `:size`, and D16 had recorded it as
"(rejected)". The plan had it as a decision ("decide and
record *not now*"), and my recommendation was the same, on the grounds that a flat
`(array :float (* n m))` with hand arithmetic loses no expressiveness. Asking what
it would *buy* first is what exposed that it was cheap; the user then overrode the
default — "The DSP language should not have such omissions" — and was right. D16's own layout rule already made the record
dimension a stride ("for a slot of `:size N`, each leaf becomes one flat state entry
of `:size N`"), and the selector grammar already had both positions, so what was
missing was the refusal, one `:stride` key, and one multiply.

**What landed (D71).** A leaf of an array of `n` records with a field of `:size m`
is one flat block of `n * m` slots carrying `:stride m`, and an access folds the
two indices into the single element index the ABI already had: `record * stride +
element`. The consequence worth stating is the one that kept the slice small —
**no backend changed**. Every target already saw a constant base offset and one
element index expression; they now see `bank * 4 + index` where they used to see
`index`. `strided-index` folds the index at compile time when both parts are
constant, so a literal bank read stays a `:const` and reports no dynamic
diagnostic. Per dimension, a constant index is checked against its own bound —
which is the safety a flat array cannot offer, because one flat bound cannot tell a
wrong record from a wrong element.

**Grammar now matches D16.** `(field voices i :line k)` and
`(set-field! voices i :line k v)` are the general spellings; `(aget voices i k)` and
`(aset voices i k v)` are the terse ones for a record array owning exactly one
array field, generalizing `(aget store k)` for a scalar record. In exchange, an
array of records is now *always* indexed: `(field voices :phase)` used to read
record 0's element silently and is now refused. The catalog case `:constant-table-bank`
is the wavetable bank the user asked for, and `:record-array-buffer` is the
writable twin — both shared cases, so the browser's process matrix runs them on
JavaScript and Wasm in f32 and f64, alongside the interpreter and JVM runs the JVM
suite already makes.

**The four bites.** Ignoring the stride in the accessor flips the shape detection
and refuses the element index; folding with a wrong stride keeps everything
compiling and changes the numbers (`[4.0 13.0 23.0]` for `[6.0 13.0 23.0]`,
`[0.25 0.0 0.75 0.25]` for `[0.75 0.0 0.5 0.25]`, and `:value 1` where the folded
index should be 4); not repeating the field's initializers per record gives a
12-slot block a 4-value `:init` and the interpreter says "state has the wrong size
{:expected 12, :actual 4}" — that bite also broke an existing record-array case,
which is how I learned that `:init` is the *source of truth* for the state array's
length, not `:size`; and removing the constant fold fails the folded-node assertion
and both per-dimension refusals at once.

**Counts.** JVM 230 tests / 2024 assertions / 0 failures; browser 50/50, with 92
cases per shared process matrix (was 90). H's remaining open items are H1
(deferred to after P13 → P14 by design) and nothing else: H2, H3, H4 and H5 are
done.

---

## 2026-10-21 — Parity audit before P12 — three targets, and one hole in the *verification*

The question was whether JVM, JavaScript and Wasm have feature parity now. There are
two kinds of answer and the second is the one that matters: what the targets
implement, and what the suite would *notice* if one of them stopped.

**Implementation.** The builtin surface is identical — 25 `[operator type]` methods in
each of `targets/jvm.clj`, `targets/js.clj` and `targets/wasm.clj`, no target-only and
no missing entry (compared as `defmulti` method tables in the REPL). No `"this target
cannot …"` refusal is left anywhere in the hosts: the only target-flavoured message
still standing is the *same* fused-slot refusal, word for word, in `hosts/js.cljs`,
`hosts/wasm.cljs` and `hosts/interpreter.cljc`. The IR vocabulary reaches all three
emitters; `:project` is the one exception, and by design — the interpreter holds a
record-returning call's result as a map while the backends split it into locals while
inlining, which `:biquad-multimode` pins on all four targets.

**Coverage.** 93 process cases × f32/f64 on JavaScript **and** Wasm, plus the function
cases on interpreter/JVM/JavaScript. Wasm has no function-entry form at all (it is
process-only), so the function matrix declares `:targets [:js]` and the operators
reach Wasm through process cases instead — an architectural asymmetry, not a gap in
what Wasm can compute. `parity` over a whole run: 207 case/precision pairs, no gaps.

**The hole was in the verification.** `with-buffers` (D57's regions) was tested only
for `[:interpreter :jvm]`: a process-shaped callee writing its own `:output` is handed
a region by each call site, and no browser check ever did that. Closed with a *shared*
case — `:region-buffers` in the catalog, over a `region-gain` fixture — so the
existing matrices pin it on all four targets, with no new page code and no host region
involved. Its read-back is deliberately one frame back (`(buffer-load :lo 0 1)`): a
region is state addressing, so the *runtime frame* is the only target-visible part of
the index, and a same-frame read-back would pass whether or not a target got the frame
term right.

**Two bites, one of which taught something.** The first stub dropped the call-site
base from the region alias, so both calls share one slot — and *nothing failed*. The
catalog is a **differential** check: expected outputs come from the interpreter built
from the same IR, so a lower-level bug is invisible to it by construction. That is
exactly what a target-parity case is for, and worth remembering before designing the
next one. The second stub was at the target: `targets/js.clj`'s `index-form` ignoring
the element index. Now `coverage` reads `{:region-buffers {:f32 #{:wasm} :f64 #{:wasm}}}`
— JavaScript absent, Wasm present — and `:constant-table` and `:delay-line-unit` lose
JavaScript with it. A *failure* also settles the other question: Shadow had rebuilt
(0 warnings, 2 files per rebuild) and the case ran before the stub, so the pass that
followed the first stub was a real result, not a stale page.

**The harness was lying, quietly, and that is where the time went.** Every lookup by
case id returned nothing: the page sends a ClojureScript keyword as its printed form,
so `:gain` arrives as the string `":gain"`, and `(keyword ":gain")` builds a keyword
whose *name* contains the colon — one that compares equal to nothing, so
`(= :gain (:case row))` was false for a row that was right there. `decode-result` now
strips it, and it is public precisely so a test can pin it
(`omkamra.dsp.tests.harness`). Three things came out of the same seam: the function
matrix's rows carried no `:case`/`:precision` (they collapsed into one spurious `:` key
in a coverage map), the extraction I kept hand-writing is now `failures`,
`check-results`, `case-results`, `coverage` and `parity`, and `run!` takes
`:views [:failures :parity]` to attach them. `parity` is the one worth keeping: a case
that ran nowhere and a case that passed are equally green, so parity is only ever
proved by reading which targets a case *reached*.

**Counts.** JVM 237 tests / 2053 assertions / 0 failures; browser 50/50 with no parity
gaps. TESTING.md now documents the result shape, the five queries and the wire
encoding.

### The two residuals: four targets in one table, and a stereo region on every target

Both were named in the parity audit as things left unverified, and both are now
closed.

**A row is a row, whichever half produced it.** The browser reported per-case rows
and the JVM suite compared interpreter against JVM one *assertion* at a time, so
"did every target run every case?" took two vocabularies and a hand-written merge.
Now `conformance/case-rows` builds the interpreter/JVM comparison as the same kind
of row the page sends, `conformance/target-row-keys` is the single place that says
which key a target's verdict lives under, and `conformance/coverage` and
`conformance/parity` read rows from either half without asking where they came
from. `(suite/parity)` is the one call: it runs the browser suite, adds the
interpreter/JVM rows, and returns the four-target table — 209 case/precision pairs
with the targets each one reached, `:gaps` empty, `:passed?` true only when the
browser suite passed and no row left a declared target unreached. A mismatch and a
case that never ran are both *gaps*, which is the property the whole thing exists
for.

**A stereo region, executed on every target.** `with-buffers` regions were pinned
mono, and a region's channel stride — `channel * stride + offset + frame` — was
therefore computed but never *executed* off the JVM. The plan now carries
`:channels` (the one thing a runner needs to allocate both audio ports), the JVM
runner allocates a primitive array of primitive arrays, and the page does the same
in its oracle, its JavaScript runner and its Wasm runner — the last one addressing
each channel at the module's own `:channel-stride`, not at a guess. The case is
`:stereo-region-buffers` over a new `stereo-region-gain` fixture: two regions read
back one frame late on *both* channels, with the callee's channels carrying
opposite signs, so a channel folded into the index wrongly flips an output's sign
instead of merely changing its magnitude. Interpreter and JVM agreed on the first
try; JavaScript and Wasm passed the moment their runners could allocate.

**The bite.** The same target-level one as before — `targets/js.clj`'s `index-form`
ignoring the element index — now takes *both* region cases off JavaScript in f32
and f64 and leaves Wasm untouched: `:gaps` reads `{:case :region-buffers … :target
:js}` and `{:case :stereo-region-buffers … :target :js}`, four entries, and the
stereo case is shown to be load-bearing rather than decorative. That a *lowering*
stub still cannot be seen this way is unchanged and by design: these rows pin
parity between targets, not the value of an expression all four compute.

---

## 2026-10-21 — P12 readiness spike: what the binary step actually is

Before committing to P12 I went looking for the one thing the phase's first
deliverable depends on and the plan does not name: *which* WABT — and whether
"WAT → WABT → binary" hides a compiler we would have to write. It does not. WABT is
an existing tool; the work is picking, pinning and invoking it (D72).

The page already depends on `wabt` 1.0.39 through `package.json`, and that package
ships **CLIs** (`wat2wasm`, `wasm-validate`, `wasm-interp`, `wasm-objdump`), so the
zero-install answer was sitting in `node_modules`: `node_modules/wabt/bin/wat2wasm`
runs under the installed node and needs no new dependency. It is also slow — node
starts up per invocation, ~535 ms a module, ~96 s for the 179 Wasm artifacts the
catalog compiles. The upstream 1.0.39 release has a native Linux x64 tarball with a
`.sha256` next to it, so the second answer is a pinned download: the checksum
verifies, and the native tool assembles the same 179 modules with **zero
byte-differences** against the npm CLI, the whole corpus in under a second and the
largest (768 KB) module in 143 ms. So the JVM gets a native `wat2wasm` when it is
present and the npm CLI when it is not, and the two are interchangeable by
measurement rather than by hope. Two rejected candidates are worth remembering: the
distribution's `wabt` is 1.0.36, which is a version skew against the browser and
therefore a reproducibility hazard, and `org.bytedeco:wabt` does not exist (404) —
there is no JVM binding to reach for.

The spike's side findings are as valuable as the pin. **Every module validates**, so
P12's binary step is wiring, not emitter repair: the emitted WAT is already
WABT-clean across both precisions and every case. And **the size is in the
initializers**: `feedback-delay-f64` is a 768 KB module because its state
initializers are emitted as 48 002 separate `f64.store` instructions — all of them
`0.0`, which is what linear memory already contains. A data segment for the non-zero
part (and nothing at all for the zeros) turns that into a few KB, so state
initialization became deliverable 5 and the third slice.

P12 is re-scoped in `PHASES.md` around that ordering: the layout first (no new
dependency, verifiable with today's tooling), the binary step second, the init blob
third. No DSP code changed for this spike.

---

## 2026-10-21 — The final lowering belongs to the target's world (D73)

I had recommended compiling WAT on the JVM, with a pinned WABT release, on the
grounds that a complete artifact should carry its binary. The user pushed back twice
and then named the principle that settles it: each backend does as much lowering as
it can in portable Clojure — the same code on the JVM and, after P13, in the browser
under SCI — and then hands the result to a *target-specific* final step, which uses a
tool from that target's world. `insn` produces JVM bytecode and lives on the JVM;
`squint` produces native JS and is cljc, so it runs wherever the compiler runs;
WABT produces a Wasm binary and lives in the browser's world. Asking the JVM to
assemble Wasm is the mirror image of asking JavaScript to assemble JVM class files.

My three arguments did not survive that reframing, and it is worth writing down why,
because they looked strong:

- *Identity must be pinned by the compiler.* For the other two targets the identity
  is pinned by the tool's **version in this repository** — `insn 0.5.4`,
  `squint 0.14.211`, `wabt 1.0.39` — never by where the lowering runs.
- *The JVM suite cannot validate modules.* Each target is validated by **executing it
  in its own world**: JVM kernels in the JVM suite, JS and Wasm kernels in the browser
  suite, which assembles and runs all 179 catalog modules on every run.
- *A complete artifact has bytes.* A JS artifact's payload is source, and that is
  fine because a JS engine takes source. A Wasm engine takes bytes and nothing else —
  a fact about the platform, not a defect in our artifact.

So D5 and D72 are superseded: the Wasm artifact stays WAT plus ABI plus the **name and
version of its assembler**, `:bytes nil` now means *assemble this with the named
tool* rather than *not implemented yet*, and P13/P14 — which already describe
`… → WAT → WABT → worklet` — own the step. The artifact change is two lines and a
drift guard: `omkamra.dsp.tests.wasm` reads the `wabt` version out of `package.json`
and asserts the artifact reports it, so the name cannot go stale. P12 is left with the
memory layout and the state-initializer blob, and the blob slice is worth more under
this decision than it was before, because those 48 002 redundant stores are 2.2 MB of
WAT *text* that the page otherwise assembles on every request.

### The original monolith is gone as well

`src/omkamra/dsp_test.clj` — the 2 800-line suite that preceded the split into
`omkamra.dsp.tests.*` — is deleted. It had been "kept for reference" since the split,
but its assertions pin the generated shapes *of the time* (Squint's output spelling,
state naming, ABI fields added by later phases), so it failed 31 assertions and could
only mislead: a reader who ran it would see red and have no way to tell design history
from regression. The split had already carried every one of its subjects across — 82
of its 85 test names survive verbatim, and the three that do not were renamed
(`javascript-backend-emits-es-module` → `…emits-standalone-es-module`,
`…emit-conditionals` → `javascript-backend-emits-conditionals`,
`…emit-all-comparisons` → `javascript-and-wasm-backends-lower-all-comparisons`), which
is why the file was worth checking against the current namespaces before deleting
rather than counting tests. Nothing else referenced the namespace: the only remaining
mentions are historical (this diary, `PLAN.md`'s original file tree, and one old
decision's prose), and the two *live* references — the file table and a CI snippet in
`TESTING.md` — are fixed.

---

## 2026-10-21 — Dependency refresh: squint and SCI, with insn already current

`squint` 0.14.211 → **0.15.212**, `sci` 0.15.58 → **0.15.59**, and `insn` stays at
**0.5.4**, which is the newest stable release Clojars has (checked, not assumed).
The two that moved are the ones this compiler actually runs: `squint.compiler`
produces the JavaScript target's modules *and* is a macro library the page itself
compiles against (`squint.defclass` in the worklet adapters), and SCI is the reader
P13 will compile source with in the browser. `insn` assembles the JVM target's
bytecode and did not need touching.

Squint's minor bump is the interesting one, because it includes *Boolean return tags*
— the exact area D41 fences off with the rule that nothing may lower to a
`squint_core` reference and that `and`/`or`/`not`/`=` stay provably boolean. That rule
is already a functional test (`every-catalog-case-is-a-standalone-javascript-module`
plus `tests/js.clj`'s contract assertions), so the upgrade was checked the way the
project checks everything: by running the suite, not by diffing generated text.

Results, all functional: JVM 239 tests / 2 063 assertions, browser 50/50 — including
the Squint and Wasm worklet adapter checks — and, because squint is shared with a
neighbouring project in this repository, `omkamra.vice.assembler-test` as well (82
tests / 208 assertions). The page needed a real rebuild rather than Shadow's cached
compilation: a dependency bump does not invalidate Shadow's per-namespace cache, so
the suite would otherwise have tested a worklet adapter compiled by the *old* macro
library while everything else came from the new one. `.shadow-cljs` was wiped, the
page rebuilt from scratch (142 files, 0 warnings), and the suite ran again warm.

One observation worth keeping: the first run after that wipe failed a single check,
`events did not reach the worklet`, and passed on every run after it and in isolation.
It is a cold-build timing artefact of the worker/worklet paths, not a regression from
either bump — but it is recorded in `TESTING.md` so the next person does not chase it.

---

## 2026-10-22 — P12a — the memory layout declares itself

The fixed 4096-byte channel stride is gone, and with it a duplication that had been
invisible because the two copies agreed. Capacity is now a **frame count**, declared
once (`descriptor/default-frame-capacity`, 1024) and settable through
`:frame-capacity` — which the IR and the JVM target had honoured all along while the
closed compiler-options schema refused it, so no caller could reach it. The Wasm
target reads the IR's capacity instead of deriving its own, derives every stride and
region size from it, and validates the assembled layout (alignment, whole blocks,
non-overlap, state slots inside their regions, declared pages covering what the
regions need) before publishing it per region.

The layout is also what f64 was quietly losing: half the frames of f32, for no reason
anyone chose. The new shared case `:long-block` asks for a 1024-frame block at both
widths and runs on all four targets; restoring the old stride makes it fail on Wasm
f64 from sample 512 — exactly where the old capacity ended — while f32, the
interpreter, the JVM and JavaScript stay green. Two f64 pins moved, including the
lane-stride test whose rationale inverted: it used to assert that f64 fits fewer
frames per lane, as a *fix*, and now asserts that both widths accept the same block.

The readers were the other half of the deliverable. The Wasm host already took its
numbers from the ABI and now refuses a block longer than the declared capacity rather
than writing past its output region into a neighbour's samples. The page's runners
had `4096` defaults in four places, and its two standalone WAT fixtures had no
metadata to read at all — they now fetch it (`wasm-bounded-loop-metadata`, and the
existing `wasm-metadata` route for the lookup-table probe), which is why those
routes exist at all. The worklet path's own hardcoding is untouched and noted: it is
P14's deliverable to install a kernel *from* ABI metadata, and today it is consistent
because the page passes the adapters the ABI's numbers.

### Two traps in the browser harness, removed

The harness cost real time twice in this session, both times by inviting a *wrong
conclusion* rather than reporting an error. `run!`'s derived views were opt-in, so
`(:failed result)` returned `nil` — a key that does not exist is indistinguishable
from "nothing failed" — and a failing run was briefly read as a passing one. And a
matrix check reported a failure as prose ("matched 95 cases; first failure:
:long-block — …") with the target buried in a JavaScript stack trace: in a
four-target comparison the only question is *which half broke*, and the answer had to
be parsed out of a sentence.

Now `:failures` is always attached, `:views` adds to it rather than replacing it, and
a failing case carries `:failures [{:target :wasm :detail "…"}]` — the case, the
precision and the target as fields. `browser/case-failures` reports one row per
failing case with its check and its failing targets, `check-names` reports the
vocabulary `:only` selects from, and a matrix check reports `:case-count` and
`:case-failures` so that a matrix's 95 cases are not confused with the suite's 50
checks. A case's own verdict is the fact, not its check's summary: the first draft of
`case-failures` trusted the summary and reported nothing for exactly the failures it
exists to find, which the test caught rather than a reader.

The wire decode had to learn `:target` for this: it arrives as the string `":wasm"`,
and `(keyword ":wasm")` builds a keyword whose *name contains the colon* — comparing
equal to nothing while printing as `:wasm`. The harness test pins `#{:wasm}`, not the
string. One improvement turned out to be a trap of its own: listing "the checks are
…" in the stray-needle failure reported an *empty* list, because the page builds a
check only when the selection asks for it, so a stray needle leaves nothing to list.
The report is factual again and names the needles as data; making the vocabulary
available there would mean building the check list as data before running it, which is
a larger change to that dispatch than this session should make uninvited.

---

## 2026-10-22 — P12b — initialization in constant space, and a signed zero that was not one

`$init` was one store per initializer element, and for `feedback-delay` at f64 that was
48 004 stores: 2 251 108 bytes of WAT text, 99.8% of the module, for a delay line whose
entire initial state is zero — and the page assembles that text on every request. Across
the whole catalog there are 54 839 initializer elements and exactly 27 of them are
non-zero, so the zeros *were* the cost.

`$init` now clears the state region with one `memory.fill` and writes only the non-zero
initializers. Assembled with the pinned WABT, the same delay line went from 2 251 090 to
5 562 bytes of WAT and from **768 644 to 645 bytes of module** — a 1 192× smaller
artifact, assembled in 0.52 s before and nothing after. The phase note had suggested a data segment; working it
through showed why that cannot be the whole answer — an *active* segment applies at
instantiation and never again, so it cannot serve `reset`, which is exactly why `$init`
exists and why `reset` calls it. The zeros therefore still need the fill, and the
non-zero part still needs to be re-appliable. Bitten both ways: removing the fill fails
eight catalog cases on **Wasm alone** (`:one-pole`, `:one-pole-record`, `:one-pole-voice`,
`:one-pole-pair-voice`, both precisions) because `reset` then leaves the previous note's
state in place; going back to one store per initializer fails the new size and structure
pins, one of which reports the old 2 251 108-byte number as its expectation.

Writing "zero means the zero *bits*" for the skip test found a real bug elsewhere: both
Wasm constant formatters decided the sign of a zero with `neg?`, which is defined through
`<` — where `-0.0` and `0.0` are equal — so `-0.0` was emitted as `0.0` in *every*
constant, not only in initializers. The fix reads the sign bit. Its pin lives in the Wasm
tests rather than in the catalog, and the reason is worth keeping: the differential suite
*cannot* see this class, because the Wasm half allows its transcendental emulations the
tolerance D14 documents, and a sign flip on an exact zero sits inside it.

That tolerance is also the honest answer to a finding this work turned up. Tightening the
Wasm comparison to demand equal signs on equal zeros made `:sin` fail — the polynomial
emulation returns `+0.0` where `Math/sin` returns `-0.0` — and the right response was to
revert the tightening, not to chase the emulation: expanding a documented numerical
contract is a decision about the numerical policy, not a side effect of a phase about
module size. What is left behind is the reason, written where it can be found: the page's
comparison says why it forgives this, and `conformance.clj`'s `:wasm` `scalar-match?`
method is marked as never reached from the JVM runner, so nobody edits the copy that
changes nothing. The catalog case written for the signed zero was dropped instead of
kept as a case that cannot fail.

**P12 is complete** — both slices, the layout and the initializers, with the target
parity table at 211 case/precision pairs and no gaps.

---

## 2026-10-22 — P13 reconnaissance — what the browser can already do

Before writing any of P13 I measured it, in a real browser, with a throwaway Shadow
build that I then deleted.

**Shadow compiles squint's compiler for the browser.** 87 files, 0 warnings, 12 s,
with `malli.core`, `malli.error` and `edamame.core` in the same build — and they run:
`compile-string` returns JavaScript, `m/validate` answers true, and `me/humanize`
produces exactly the `{":frame-capacity" ["should be an integer"]}` shape our
descriptor errors already use. A cold page load that compiled ten real DSP kernels
took 890 ms including browser launch.

**Two of the port's biggest unknowns turned out to be non-events.** Metadata survives
the round trip to squint — `^:mutable` was honoured in the browser build, so `*print-meta*`
works under ClojureScript — and D41's standalone-module rules hold for real kernels. The
only `squint_core` reference I saw came from a synthetic source of my own that
deliberately omitted a type hint, which is the best possible confirmation that the rules
exist because they are load-bearing.

**The one divergence is number formatting, and it is the same trap as P12b's.** I took
the squint *source text* of ten real kernels (via the JS target's own source builder),
compiled each through the JVM's squint and through the browser's squint, and diffed.
Three were byte-identical. Seven differed, every line of every difference an integral
float literal:

```
JVM:     return (__dsp_state_1[0] = 0.0);    browser: return (__dsp_state_1[0] = 0);
JVM:     let inl__1__f_8 = ((2.0 * Math.sin(...)));   browser: ((2 * Math.sin(...)));
```

ClojureScript has one number type, so `(pr-str 0.0)` is `0`. Harmless in JavaScript —
`0 === 0.0` — but it means no text-level comparison of browser and JVM artifacts can
hold, and it is a *printing* difference hiding a *value* difference, exactly like `-0.0`
printed as `0.0` in the Wasm emitter two days ago. The fix is the same shape as that
one: do not let the printed form decide the type. The emitter formats float literals
from the IR's declared type.

The deeper form of the same fact: `(= 0 0.0)` is false in Clojure and true in
ClojureScript. Equality, printing, and inference are all platform-blind and none of them
may be trusted with a number's type. D77 records that as the rule, together with the
canonical IR form that the cross-platform comparison and the cache key share.

**The Var seam was already anticipated.** `synth.cljc` carries
`#?(:clj (var? definition) :cljs false)` with a comment saying that in the browser the
registry's symbol is all there is. The F workstream met the boundary first and left the
browser branch behind, which is a good sign for deliverable 2: the linker's three
resolution points (`find-ns` + `ns-resolve` + `@var`, twice) are all it needs replaced.

**The layering that must survive.** `hosts/wasm.cljs` and `hosts/js.cljs` reach
`hosts/fusion` → `hosts/mixing` → `synth` and `synth/pool`, and `synth`'s docstring
already says it joins the halves 'without the compiler in reach'. Adding the compiler to
the browser build must not make a runtime-only consumer bundle malli and squint.

The port itself is smaller than its 7 200 lines suggest: `ir` and `builtins` contain no
JVM-only construct at all, `descriptor` needs nothing beyond malli (itself `.cljc`), and
`linker` needs the resolver; the JS emitter has three sites and the Wasm emitter the
float-bit helpers and a handful of constants.

### The numeric literal's type, and where it goes missing

The browser check of P13a turned up a divergence that is not about porting at
all. `descriptor.cljc:489` and `ir.cljc:144` both type a numeric literal with
`(integer? form)` — a host-type rule that is true of the JVM and false of
ClojureScript, where one number type makes `1.0` and `1` the same value. Four
catalog cases failed for it (`(+ sample 1.0)` became float-plus-int), and the
deeper fact is that **the type cannot be recovered from the value anywhere**: a
JVM-produced descriptor shipped as EDN loses it, and so does a `dsp/defn` in a
`.cljc` file, because Shadow runs the macro on the JVM and emits the descriptor's
numbers into JS as JS numbers. A JS number does not remember being written
`1.0`. So the type has to be data: `{:dsp/float 1.0}`, with `#dsp/float` as sugar
that reads to exactly that map. Descriptors stay plain EDN and every reader of
one stays dumb.

I verified the tag in all four places rather than trusting the idea. Clojure via
`resources/data_readers.cljc`; SCI via `:readers`; Shadow's reader via the same
file — after a *fresh* process, because `data_readers.cljc` is read once per
process by both Clojure and Shadow (I found Shadow computing `cljs-data-readers`
in a `def` in `shadow/build/data.clj`, which is why the running server could not
see a file that appeared after it started); and `clojure.edn` plus `cljs.reader`
reading the map form with no registration, which is the property that matters for
a descriptor in flight.

Two things fell out that were not part of the question. The dev server must be
**restarted** after a data reader is added. And the box's printed form differs by
platform: Clojure abbreviates a one-namespace map to `#:dsp{:float 1}` and
ClojureScript prints `{:dsp/float 1}`, so the two agree on the value and disagree
on the text — a second, independent reason for the canonical form D77 asks for,
alongside map ordering and number printing.

The box is written by the capture where it can be — from the host type on the
JVM, from the source text in the browser through the `:line`/`:column` that
`&form` carries — and must be in the data where no text exists: the acceptance
catalog's seventeen hand-written bodies, whose eight float literals a diagnostic
can name. That keeps 82 macro-captured definitions untouched and puts the whole
cost where a human can see it.

### Boxes where nobody was looking, and names that came from the process

The box from D78 was supposed to be the capture's business. The equivalence check
found out otherwise. `sign`'s entry in the builtin table expands to `(if (> x 0.0)
1.0 (if (< x 0.0) -1.0 x))` — a *generated* form, built by code that ships
compiled into the page, so its `0.0` is the integer it looks like there and the
browser refused the expansion as binary operands of different types. It was right
to. The same shape was waiting in `combinators/guard` (a `0.0` comparison, a `0.0`
branch and the transition form that becomes `:next`) and in `combinators/unison`
(computed detune offsets spliced into generated call arguments, and a gain spliced
into the mix). Generated forms are source too, so the box vocabulary now has one
home — `omkamra.dsp.literals` — and one test walks every builtin expansion, every
library and fixture definition, every transformer output and every catalog
definition for a bare float, naming the operator and the path when it finds one.
Unboxing `sign` fails it on all four literals; unboxing `guard` fails it on three.

The check also turned up something with nothing to do with floats. Three cases
(`:mod-zero`, `:mod-neg`, `:mod-flip`) compiled to *different IR* on the two
platforms — not because anything was wrong, but because `mod` expands through a
`gensym` and a gensym reads a counter shared by the whole process: `mod_remainder1`
in a fresh page against `mod_remainder66419` here. Same program, different IR, and
no exact comparison possible. Lowering now binds a counter of its own
(`omkamra.dsp.names`) and asks it for the next name; un-binding it makes the check
fail on exactly those three cases, naming the path and both names. The JS
emitter's `__iter`/`__reached` are the same question one layer down, and P13b's
emitted-text comparison is where they land.

**We then moved the check into the suite**, which is where it belonged: the page
compiles the catalog with the compiler compiled into it — real ClojureScript, not
Clojure on a server — and compares, per definition, the normalized descriptor, the
linked unit and the portable IR with the JVM's. Sixty definitions, fifty-two
compared and bit-exact, in every browser run. The eight it cannot link yet are
*skipped*, with the count in the check's detail: they call library definitions the
page has not been given, which is P13d's registry, and saying so is more honest
than failing on something the page was never told how to do. Two harness contracts
fell out: a check that declares no targets contributes no rows to `case-rows` (a
case that was never run must not look like one that ran and failed, or the parity
table would gain a gap per definition), and a failure without a target no longer
puts a `nil` into `:targets`.

**One tooling lesson.** `cljfmt fix` reformatted three files, and the reload tool
then *refused*: `:status :refused`, "the whole session changed (53 of 82), a
replaced baseline rather than an edit". That is the design doing its job — a bulk
rewrite invalidates a content-based baseline and silently reloading would hide it —
and the cost was one explicit `(tools/reload :all)`. It mattered because the stale
session still held a *bite*, so the equivalence check failed with names from a
broken JVM: a failure that was true about the session and false about the code.

Also renamed, as asked: `run-all` is `run-jvm`, and `run-browser` joins it, so the
two halves of `run-everything` can be run by name. The living docs were updated;
the diary's older entries were left as they were written.

---

## 2026-10-23 — P13b — the emitters (D80)

### First, a question that did not become a change

Before starting P13b we stopped to ask whether the box was avoidable after all:
collect the float literals that had to be boxed, and see whether a cheap inference
would let most of them stay bare. The measurement was a scratch pass that walked
every definition's form with an environment of declared types and asked each box
whether the place it sat in demanded a type — a sibling operand's type, a declared
parameter or state slot, a builtin's signature, the enclosing demand propagated
through `if`, `do` and the operators.

**168 boxed literals in 144 definitions**, and **142 (85%) had a context the rule
read**. The 26 that did not grouped into causes, and one group of 17 was the
analysis's own fault: it read *raw* captures, where an untyped parameter means
`:float` and the normalizer fills that in. With that fixed, roughly **159 of 168
(94%)** resolve. The residue is about nine: seven nested-`if` branches where the
pass had no demand to propagate, and two `let`-binding initializers — the hole
named when the box was introduced, and the only one that is a hole in principle
rather than in that pass. **No box contradicted the type its context demanded.**

We kept the box and deferred the inference. The trick is sound, but it is not cheap
in this architecture: the lowering types bottom-up, so a context-directed literal
means pushing types down — a top-down variant of `descriptor/expression-type`
threaded through `ir`, on the order of 150–250 lines plus diagnostics and test
churn, which is more than the box work it would remove. It also loosens the
language (`(+ sample 1)` becomes legal where it is an error today), and the places
it would save a human are sixteen — the catalog's eight and ~8 generator call
sites; the other 152 are synthesized by the capture and invisible to authors. The
middle path the data supports, if we want it later, is narrow: an untyped integral
literal in a position that demands `:float` *is* that float, and everywhere else
stays `:int`, so the ~9 unresolved cases keep their box and nothing is silently
overruled. The equivalence check makes the whole question revisitable: if we adopt
that rule, both worlds would have to infer identically, and the check says whether
they do.

### The port

The work began with a corpus rather than with the port, because D77's lesson was
that the surprises here are *measured*. A scratch Node build of a probe namespace
compared this side's spelling of a value with the JVM's over **30,743 doubles and
26,017 floats** — every float constant the catalog emits, the smallest subnormals
one by one, a sweep of powers of ten, and random bit patterns. It found four
divergences, none of which reading would have produced:

- **Clojure's `float` is stricter than IEEE 754.** It throws for anything above
  `Float/MAX_VALUE`, including `Float/MAX_VALUE`'s own printed value, where the
  JVM's narrowing conversion rounds to the maximum and `Math.fround` agrees. The
  portable coercion asks the JVM's cast.
- **The smallest subnormals.** The JDK spells `4.9E-324` for the double whose
  shortest spelling is `5.0E-324`, against its own javadoc, and does the same for
  its neighbours.
- **An exact decimal tie at f32 width.** `1439526.25` has `1439526.2` and
  `1439526.3` equidistant at eight digits: the JDK rounds to even, JavaScript round
  half up on the magnitude.
- **The notation.** `1.0E7` against `10000000`, `1.0E21` against `1e+21`: the same
  digits, two conventions.

So `omkamra.dsp.numbers` does not delegate to either library. The rule is the
shortest digits that read back as the value, rounded to nearest with a tie away from
zero, written in the JDK's notation — computed by the *same search* on both sides,
since each host's round-to-`k`-digits primitive already rounds half up on the
magnitude. And the digits are not taken from `String.format`: the JDK's `%e` rounds
half up *except where it recognises the value*, which is what produced the f64 tie
mismatch before `BigDecimal` replaced it. With the rule implemented twice the
corpus reads **0 mismatches in both directions**, and the catalog's own constants
deviate from `Double/toString` **zero times**.

**Bite-checking is how the rule stayed honest.** Making the browser delegate to its
own conversion (`(str value)`) fails both checks — 10,604 of 10,733 doubles differ,
and *zero* floats, because only the f64 path was stubbed. Making the browser hand
the number where it hands the spelling fails only the JS-form comparison, and number
formatting stays green: a bite that points at one target and not the other is a bite
that is measuring something.

The port itself was mechanical and verifiable, so it was verified: a baseline of all
**240** emitted modules (120 JS modules and 120 WAT) was captured *before* the port
and compared after — **byte-identical**, 240 of 240. `Byte-identical` is the claim
worth making for a port, and it is cheap when there is an oracle.

### The check

`compiler equivalence` now emits as well as compiles: per definition and per width
the page lowers its own IR and produces the JS target's forms and the Wasm target's
module, and compares them with the JVM's. The forms are compared as *data* — a float
literal is a `js*` snippet carrying its spelling, so the spelling is visible and the
metadata the two printers render differently is not compared at all — and the Wasm
module as *text*, through `diff/text-difference`, which reports the line and a window
around the character that differs rather than a page of text.

The squint *source* is deliberately not compared. `pr-str` renders metadata
differently on the two platforms (`^:mutable` against `^{:mutable true}`), which is
host-independent in the emitter's own code and invisible in the artifact; comparing
it would have meant writing a Clojure-compatible printer to compare something nobody
reads, and the module text, where squint's own host-dependence would show, is P13c's.
That is the (C) choice, and it kept squint out of the page for this phase.

### What the check found that we would not have

- **`(:require [omkamra.dsp.targets.js :as js])` compiles, warns about nothing, and
  fails at run time.** ClojureScript reserves the `js` prefix for JavaScript
  globals, so `js/emit-forms` is a global lookup named `emit_forms`. The JVM never
  would have noticed; the page failed on every case with "`emit_forms` is not
  defined".
- **JavaScript's number→string gives the shortest decimal, not the integer.** The
  f64 exponent mask was emitted as `4607182418800017400` where the module means
  `4607182418800017408` — a different number, in a WAT `i64.const`. `integer-text`
  asks for the digits.
- **`prepare-for-target` was in the JVM entry point.** A target is handed *prepared*
  IR, and preparation lived in `dsp.clj`, so the page could not produce the IR
  either emitter is given: `stereo-spread` reached the JS emitter as a record
  expression it has no case for, because the inlining that removes records happens
  in preparation. The step is the compiler's and it is portable, so it moved into
  `ir`, and both worlds now emit from prepared IR.

### Two fresh-JVM lessons

A restart is part of the workflow, and two things about it are now known. The
shadow build registry belongs to the *running* server: a build removed from
`shadow-cljs.edn` stays in `list-builds` until the server restarts, which is how an
old scratch build was still being listed after it had been deleted. And a
`(tools/reload :all)` immediately after a restart left the session able to fail on a
reader Var it had `:reloaded` — a require of the same namespace then failed with
"Attempting to call unbound fn: #'omkamra.dsp.readers/float-literal" in a file that
requires that namespace itself. The failure did not survive a restart, so it was a
property of the session, not of the code — the same shape as the bite that a stale
session kept alive two slices ago.

### And a shape, on request

`run-everything` used to *build* its halves: `select-keys` on the JVM summary, a
renamed copy of the browser result, and `:passed?` derived in the consumer from each
half's own numbers. Now `run-jvm` returns the `clojure.test` summary plus
`:passed?`, `run-browser` returns exactly what `browser/run!` returns (which already
has one), and `run-everything` places each as it came and adds only what is about the
two together: `:passed?`, `:seconds`, and `:failures` as `[half name detail]`. A
half can be run and read on its own with the same keys it has inside the whole,
which is the test of whether a report is composed or patched up.

### State

JVM **275 tests / 2 532 assertions / 0 failures**; browser **52/52** — the
`compiler equivalence` check (now including both emitters) and the new `number
formatting` check among them. Verified in a fresh JVM, both halves in parallel.
Bites: the number rule (both checks fail, 10,604 of 10,733 doubles differ), the
spelling representation (the JS-form comparison fails, number formatting stays
green), and the two traps above, which failed before they were fixed. The scratch
probe build was removed from `shadow-cljs.edn` and `src/p13b` deleted once the
corpus became a check.

---

---

## 2026-10-23 — Reload — the full-reload option is gone

`omkamra.dev.tools/reload` no longer accepts `:all`. A detection-driven reload that
would rebuild the whole session still refuses with `:status :refused`, but the note
it leaves now says what to do instead: **restart the dev server**. The argument
error for an unknown target stops offering the option too, and `AGENTS.md`,
`RULES.md` `R3` and `TESTING.md` were updated to match.

**Why it went.** The option existed to be the answer to the tool's own safety
refusal, and it was a bad answer. The refusal fires when the baseline no longer
describes the tree — a check-out, a corruption, a bulk rewrite — which means the one
thing that cannot be trusted at that moment is the baseline. Asking it to rebuild
everything routes through the same unload-then-load sequence that a single failure
leaves half-done, and it does so on the strength of a verdict already known to be
wrong. A fresh process re-establishes the baseline in the one way that cannot
half-succeed.

**And the tool cannot reach its own case.** Its own namespaces are reported under
`:deferred` and are never reloaded in process, so the situation that most needs to
start over is exactly the one the tool cannot fix. Recommending a restart is not a
retreat from that; it is admitting what the tool is for.

**What it cost to keep.** `:all` was a second definition of "what to reload" sitting
beside detection, and the root-set resolution carried it as a case of its own: the
selection is now `(seq explicit)` or detection, with nothing between them.

**How the removal is pinned.** `omkamra.dev.tools-test` asserts that the refusal's
note names a restart rather than a call, that `(tools/reload :all)` throws, and that
naming the namespaces you want still works — so the option cannot come back by
accident. The tooling suite is **20 tests / 102 assertions / 0 failures**, run in a
fresh process, which this change required rather than merely preferred.

The two remaining mentions are in records and stay as written: `DECISIONS.md` `D79`
and the older entries of this file say what was true when they were written.

---

## 2026-10-23 — The documentation set has one owner per fact

The handoff at the end of this file had grown to 250 lines, and it was a fourth copy
of four documents: the phase status of `PHASES.md`, the architecture of `PLAN.md`,
the oracle contract of `TESTING.md`, and the narration of the previous handoff. That
is the diagnosis, and it explains both symptoms — a handoff too long to read and too
stale to trust, and entries that kept landing *inside* it, because a rolling entry
that is also the last one is where anything with nowhere else to go accumulates.

**Each fact now has an owner.** `README.md` is new: which file answers what, in what
order to read them, and the conventions. `RULES.md` (new — what to do, `R1`) and
`TRAPS.md` (new — what will happen, `T1`) take the accumulated toolchain lessons that
had no home. The oracle contract moved into `TESTING.md` §The oracle contract, the
namespace map into `PLAN.md` §Current architecture, and the open questions into
`PLAN.md` §Known limitations — which was rewritten, because about half of it
described limitations that P8, P9, P12 and D47 had already closed. The handoff is 41
lines: where we are, what is next, and what shapes the next decision.

**Why rules and traps are two files rather than two sections.** One numbered series
with sections cannot be appended to: a new rule in the first section renumbers every
trap, and every citation elsewhere goes stale. Two files, one series each, both
appended and never re-sorted, is the shape that survives being added to. The
constraint is written into `README.md`'s conventions, because any numbered list here
will have to respect it.

**What the move found drifting.** `TESTING.md` was reloading with `require :reload`,
which `AGENTS.md` forbids; it named `omkamra.dsp.acceptance` and
`omkamra.dsp.conformance`, which are now `harness.catalog` and `harness.conformance`;
it gave the Shadow build id as `:omkamra.dsp.testbed`, which is
`:omkamra.dsp.harness.testbed`; and its JVM baseline was twelve tests and 76
assertions behind. `PLAN.md` §Prioritized work still said "Next add stateful module
composition (P6)". Each was a copy of a fact whose owner had moved on.

**One thing was retired rather than moved.** Three handoff subsections went:
"P2 builtins" and "Standard library", whose facts `DECISIONS.md` (`D9`–`D13`) and
`LIBRARY.md` own, and "Next steps", which `PHASES.md` owns. Every chronicle entry is
otherwise byte-identical apart from its date and level — verified by comparing the
content lines as a multiset before and after, which is how the move could be claimed
rather than hoped.

## 2026-10-24 — P13c — the page compiles its own artifacts

**The pipeline moved out of `dsp.clj`.** `omkamra.dsp.compiler` (`cljc`) now holds
the whole compile pipeline — resolve the options, check the target can serve them,
link, lower to portable IR, `prepare-for-target`, emit — and `dsp/compile` is the
JVM entry point around it. The only step that cannot be portable is the JVM backend
(`insn`), so `dsp/compile` hands it to `compiler/compile` as an extra target
compiler; `:interpreter`, `:js` and `:wasm` are built by the same code in either
world. That is what makes D77's rule hold rather than merely be intended: a runtime
consumer requires `synth`/`hosts`, never `compiler`, and the worker build still
bundles neither malli nor Squint, while the page bundles both because that is the
compiler's half.

`targets/js/emit-module` lost its `#?(:clj ...)`: Squint runs in the page too, so
the module a kernel is loaded from is made by the same call on either side.

**The check upgraded from forms to artifacts.** `compiler equivalence` now compiles
each catalog definition *in the page* through `compiler/compile` and compares the
resulting JS module and (for a process entry) WAT as text with the JVM's, beside
the normalized descriptor, linked unit, portable IR and emitter forms it already
compared. The artifact leg is the one that exercises the entry point — option
resolution, the capability check and the ABI — rather than the emitter's halves.

**The canonical IR form landed.** `ir/canonical-form`/`canonical-text` is
order-stable (map keys and set elements sorted by their printed form) and explicit
about numbers (every number is its f64 bit pattern as exact decimal digits). It is
compared as text in the check and is the cache key P13e asks for. Printing is made
host-independent as well: Clojure abbreviates a one-namespace map to `#:ns{...}` and
ClojureScript does not, so the JVM printer is told not to — without that,
`{:dsp/kind :function}` would be two spellings of one value.

**It found a real bug on its first run, which is the point.** Two definitions —
`gated-one-pole` and `sign` — emit an `if` result whose default is a float, and
`default-form` wrote it as a bare `0.0`: the JVM prints `__dsp_fround(0.0)` and the
page prints `__dsp_fround(0)`. D79 covered *generated forms* with the box and this
was still one more generator. Fixed by routing the default through `number-form`
(the `js*` spelling device, D80), and
`omkamra.dsp.tests.literals/every-emitted-js-form-boxes-its-floats` now holds the
line on the JVM — reverting the fix fails it 94 times, so the cheap JVM check
catches what only the cross-world comparison saw.

**Bites checked.** The module comparison bit first (the two cases above). The
canonical comparison bites when the two worlds spell it differently: prefixing the
JVM branch fails the browser check. The whole refactor is bite-free against the 275
pre-existing JVM tests, which is what comparing before and after is for.

**Two things deliberately not done.** The page does not compile a `:definition-var`
case, because its unit reaches a library definition the page has not been given;
the check *skips* those eight and counts them, which is P13d's registry and P13c
was told not to pre-empt. And the shared matrix still runs JVM-compiled kernels,
not page-compiled ones: since its artifacts are now proven byte-identical, the
behaviour it checks is the behaviour of the artifacts the page makes. When P13d
supplies the registry, the matrix can compile them itself.

### State

JVM **277 tests / 2 541 assertions / 0 failures**; browser **52/52**, parity table
empty. `compiler equivalence` compiles 52 catalog definitions per width and skips
eight.

---

## 2026-10-25 — the P13d front-end shape, measured (D82)

P13c left the boundary P13d is about: how the page gets the definitions it
compiles. Two candidate mechanisms were on the table — inject *host* macros via
`sci.core/new-macro-var`, or define the DSP macros as *SCI source* — and the
mechanism turns on one question: can the macro read the definition site's aliases
and refers?

Rather than reason about it, I ran it in the dev JVM. `new-macro-var` does exist,
and a host macro gets `&form` (line and column included), but its `*ns*` is the
compiled namespace, not SCI's; it can reach SCI's namespace object through
`@sci.core/ns`, and `clojure.core/ns-name` on *that* throws, because SCI exposes
only `ns-name`/`resolve` from `sci.core`, not `ns-aliases`/`ns-refers`. An
interpreted SCI `defmacro`, by contrast, reads `(ns-name *ns*)`,
`(ns-aliases *ns*)` and `(ns-refers *ns*)` directly, and gives them to a compiled
capture function. So the thin macro is SCI source and the capture stays compiled.

Three more measured facts went into the write-up: `require`/`:refer`/`(ns …
(:require …))` resolve against namespaces pre-registered in `sci/init` (no
`:load-fn`); `:readers {'dsp/float …}` round-trips the box; and SCI's `ns-refers`
includes all of `clojure.core`, which the browser capture must filter out the way
the JVM capture does, or the descriptors will diverge and `compiler equivalence`
will say so.

The decision is D82. One thing is deliberately not claimed: the browser-side SCI
build under Shadow is unmeasured (SCI ships cljs sources, and nbb is a cljs host,
but D77's standard is a spike, not an inference), so it is P13d's first check.

---

## 2026-10-25 — P13d, part one — the source layer (D83, D84)

P13d's job is to let a page compile *source* — the user's, not the catalog's —
and the first question was where the compiler's input comes from. D82 had
measured that an interpreted SCI macro can read the definition site where a host
macro cannot, so the shape was known; what this slice did was build it and let
the equivalence checks say whether it was right.

**What landed.**

- `omkamra.dsp.capture` (`.cljc`) — step one, a pure refactor: the reading of
  `dsp/defn`/`fn`/`defstruct`/`def` moved out of the JVM macros into a portable
  namespace that takes the definition site as *data*. The JVM macros became thin
  callers supplying `&form`, the namespace, and the resolved aliases and refers.
  The pre-existing tests passed unchanged, which is the only evidence a refactor
  has.
- `omkamra.dsp.registry` and `omkamra.dsp.library-registry` (`.cljc`) — the
  definition registry, and the library seeded from the namespaces the page
  compiles. Wiring `equivalence.cljs` through it closed the last gap P13c left:
  `compiler equivalence` compiles all **60** catalog definitions with **0
  skipped**, the eight library-reaching cases included.
- `omkamra.dsp.sci` (`.cljc`) — `evaluate-source`: a context seeded with the
  library, the compiled capture functions injected, and a bootstrap of
  SCI-source `defmacro`s.
- `omkamra.dsp.source` (`.cljc`) — the front end: `compile` dispatches on the
  input (definition vs string) and `evaluate` on the evaluator (`:sci` in both
  worlds, `:native` on the JVM).
- `omkamra.dsp.cljs` — the browser world face, so `dsp/compile` exists on both
  sides of the macro/runtime split (`cljs.core.async`'s arrangement).
- `compiler/compile-target` — a multimethod replacing `extra-targets`; the JVM
  registers `:jvm`.
- `source equivalence` — a browser check that compiles five source strings with
  SCI in the page and compares descriptor, canonical IR, JS module and WAT with
  the JVM's own reader and macros. It passes.

**Three bugs, all found by building it.**

1. A reader discrepancy only a real cross-world link could show: Clojure's
   reader gives a macro form no `:file`, ClojureScript's does, so the JVM's
   library descriptor and the page's differed by one key. The portable source is
   now namespace+line+column (D84).
2. `box-float-literals` double-boxed an explicit `#dsp/float` — on the JVM too,
   because a `postwalk` visits the number before the box around it (D84).
3. ClojureScript's `ns-publics` is a macro that needs a *literal* namespace
   symbol; there is no runtime namespace enumeration (T7). It reshaped how the
   library registry is written.

**Verified.** JVM **277 / 2 541 / 0**; browser **53/53**, the new check
included; parity table empty.

**Still open in P13d**, and the subject of D85: the bus still carries JVM control
slot arrays into what should be a host-neutral instrument host. The slices are in
the handoff.

---

## 2026-10-25 — P13d, part two — the reference instrument and a `.bss` (D85–D88)

Part one built the source layer. This part makes the *reference instrument* the
worked example of a portable one, and in doing so closed the last host seam and
found a piece of the linker's own idea one level down.

**The host seam closed.** The bus was the last place a target's representation
leaked into shared code: the JVM bus drove its physical control arrays from
`hosts.instrument` itself, where JavaScript and Wasm already agreed on a five-op
bus kernel. D85 is now in force — `hosts.jvm/jvm-bus-kernel` and
`hosts.interpreter/interpreter-bus-kernel` join the other two, `control-slots`
moved behind the JVM one, `hosts.instrument` is `.cljc` whose live host takes
`{kernel, bus-kernel}` (defaulted per world), and the page's bus-region checks
run the shared host over browser kernels.

**The reference synth is one `.cljc`.** Its definition half (three `dsp/defn`s, a
`unison` combinator, the description, the instrument value) and its host half (a
pool, a bus, a modulation loop) are portable; only `build-instrument`, which
lowers and compiles the instrument, is `#?(:clj …)`. The page compiles its *own*
capture of the voice and the bus definition the JVM folded, and the new
`reference instrument equivalence` check finds them identical on JavaScript and
Wasm — through the same generic comparison the catalog uses. A test pins the
`definitions` map's ids to each definition's own namespace, because on the JVM
nothing would notice a mismatch.

**The `.bss`.** The bus's canonical IR was 1.77 MB and its JavaScript module
3.65 MB — a 48 000-slot delay line whose whole state is zero, written out one
zero at a time, Squint taking 4.7 s to compile the literal. The idea is the
linker's, one level down: a zero region needs no bytes anywhere. The canonical
form now carries `{:dsp/zeros n}`, the JS arena is the zero-filled shape of the
right length, and `init` zero-fills and *then* writes the non-zero initializers —
so `reset` puts the zeros back rather than trusting a fresh region (T10).
Canonical 1.77 MB → 40 KB, JS module 3.65 MB → 11 KB, emission 4.7 s → 0.16 s;
the WAT was already compact (P12b).

**"With or without SCI".** *Without*: the static macros capture it and the page
compiles that capture (the check above). *With*: the sandbox now holds the
*combinator* namespace's functions — `unison` runs while a source is evaluated, so
SCI needs the function, not a descriptor — and the reference instrument's own
source, read back from the `.cljc` and never transcribed, compiles identically
through SCI and the JVM's reader (a JVM test). The one thing between the same
check and the page is the language's: a browser reader has one number type, so it
cannot keep a bare `:int` literal apart from an integral float (D78), and
`box-float-literals`'s ClojureScript branch boxes every number. That is a contract
decision, and it is open.

**Two principles recorded.** D87 — the compiler and its general harness name no
example; a payload is built from entries, and the example is named where the
example is known. D88 — the reference instrument is the single `.cljc` above.

**Verified.** JVM **280 tests / 2 549 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**.

---

## 2026-10-25 — P13d, part three — the facade thins (D89)

The smallest slice P13d's handoff named, and the one that pays off later: the
instance lifecycle moved out of `omkamra.dsp` into namespaces of its own, and
the facade re-exports it (D89). No behaviour changed; the point is a dependency
that is no longer there.

**Why it was worth doing.** `omkamra.dsp.kernels.jvm` drives a voice and a bus
instance in its own body, and it required `omkamra.dsp` — the macro namespace
that also loads the definition forms, the SCI front end and the library registry
— to call `create-instance`, `reset!`, `destroy!` and `process-unchecked!`. A
facade every internal host goes through is not a facade. The JVM host now
requires `omkamra.dsp.instance`; `omkamra.dsp` keeps `compile`, `link` and the
macros, which already delegated.

**The concept is target-neutral, so the namespace names no target.** The first
cut put the two raw ABIs in the same namespace and dragged
`omkamra.dsp.targets.jvm` — and with it the `insn` emitter — in beside the
generic vocabulary. They are the *only* functions that name a target, because
they hand a JVM artifact's physical control arrays to the JVM target, so they
moved to `omkamra.dsp.instance.jvm` and `omkamra.dsp.instance` now requires
nothing. That is also what would let the generic half become `.cljc` later
without untangling a target first.

**The re-export is a re-export.** `(def dsp/process! instance/process!)`, not a
wrapper: one implementation, one docstring, one `:arglists`, and a stack frame
that names the defining namespace. The little macro that does it quotes
`:arglists`, because a `def` name's metadata is evaluated by the compiler — the
same reason `clojure.core/defn` quotes it. The first attempt did not, and failed
with "Unable to resolve symbol: value". A test in `omkamra.dsp.tests.process`
pins the identity and the metadata — the seven generic ones against
`omkamra.dsp.instance`, the two raw ones against `omkamra.dsp.instance.jvm` —
and under R5 a wrapper in place of the alias was observed to fail it, and
restoring the alias to pass it.

**Verified.** JVM **281 tests / 2 576 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D90 — browser source can spell an `:int`

The one open item P13d left (D88's wrinkle), and it had *two* causes, not one.
The reader is the first: a ClojureScript runtime has one number type, so `12345`
and `2.0` arrive as the same value. The second was ours — `box-float-literals`
tested `(or (double? form) (float? form))`, and in ClojureScript both of those
*are* `number?` (read from `cljs/core.cljs`), so it boxed every number and turned
a bare integer into a float.

The fix is one host-independent predicate, `(and (number? form) (not (integer?
form)))`, and a stated contract: a bare integral literal is an `:int`, and an
integral float in browser source is written `#dsp/float 2.0`. It changes only one
path — SCI reading a source *string* in the browser. Clojure's reader and
Shadow's compile-time reader both keep `Long`/`Double`, so the JVM macro still
boxes a `.cljc` definition's bare floats, and nothing on the JVM side changed.

Two browser checks pin it: `:source-int`, a bare integer increment that must stay
`:int` (observed to fail under the old predicate, R5), and `:source-chain`, whose
integral float is now written `#dsp/float`. The reference synth is deliberately
**not** annotated: its bare floats are the test corpus for the compile-time path,
which `reference instrument equivalence` already exercises by compiling the page's
own capture and comparing descriptors. Annotating it would have made the
user-facing example noisier and weakened that test to "does a box survive". D90
records the choice.

**Verified.** JVM **281 tests / 2 576 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D91 — a namespace compiles the same pasted as in place

The goal, stated plainly: take a Clojure namespace from Emacs, paste it into a
browser editor, and get the same artifact as compiling it in the namespace. D90
did not do that — it made an integral float in browser source need
`#dsp/float 2.0`, which defeats the point. So D90's contract is superseded (its
`box-float-literals` predicate stands).

The spelling exists only in the text, and `cljs.tools.reader`'s private
`read-number` throws it away. The browser front end now reads the source with the
*real* reader — `read+string` on a source-logging reader returns each form and its
source text — and evaluates the forms with the text attached, printed back for
`sci/eval-string*`. The capture became polymorphic: `box-form` boxes from the text
when a form carries one and from the host value otherwise, so the JVM macro path
is untouched. `#dsp/float` is now the representation only, plus what a
hand-written descriptor (which has no text) still needs.

Two wrinkles cost real time. A browser re-reading printed forms stamps its own
`:line` over the printed one, so the original position travels under
`:dsp/source-line`/`:dsp/source-column` and `source-of` prefers it — without that
the descriptors differed on `:source`. And the walk needed care with `#_`
(`read+string` returns the *following* form with the discard in its text) and with
`#(...)`/`#?(...)`, whose forms do not match their inner text.

The payoff is a test. `:source-chain` is a bare `(* sample 2.0)` again, and the
reference synth's own `.cljc` source — `:int` seed and bare integral floats and
all — is a browser `source equivalence` case with no annotation: the page compiles
it with SCI and matches the JVM's own reader. The user's sentence, as an
assertion.

**Verified.** JVM **283 tests / 2 612 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D92 — the last atom

D91 left one gap: a body or a state `:next` that was a *bare number*. The
recovery read a form's own source text, and an atom cannot carry metadata — so a
bare `2.0` fell back to the host value, and the page read it as `2`. A silent
difference, which is worse than a loud one.

The fix is to let a form hand its *children's* texts to the walk: `annotate` now
attaches `:dsp/source-children`, and the capture boxes a bare number with the text
of the position it came from. Two details carried the weight. The body is a
suffix of its container's children, so `box-body` takes the last `(count body)`
texts — and the container is the definition form *except* in the packed
explicit-spec shape, where the body sits inside the parameter vector, which
`capture-function` passes instead. And a `:next` is found by re-reading the state
map's text and walking key/value pairs in reader order, not by the map's iteration
order, because past eight entries an array-map becomes a hash-map and the orders
part.

Scope is exactly the positions the capture boxes. `:init` turned out to need
nothing — `normalize-initializer` coerces it from the declared type — and
`:process`/`:options` are untouched, since a box there would fail validation.

The proof is the negative control, and it is clean: removing the two call sites —
and nothing else — fails exactly one case, `:source-atom`, with
`:expected :float, :actual :int` at `[:body 0]`. That is the pre-D92 bug,
reproduced by the check that is supposed to catch it.

**Verified.** JVM **284 tests / 2 620 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D93 — P13e is dropped

The question was whether the last P13 slice earns its keep. It bundled three
things: incremental recompilation, descriptor/IR caching, and error
classification. Two don't pay, and the third belongs elsewhere.

The A/B case is real in the architecture: a linked unit holds every reachable
definition and `lower-unit` lowers them all, so editing the reference voice
re-links, re-lowers and re-emits 17 definitions — 13 of them untouched library
ones. But the measurement puts the cache in the wrong place. Link 9.5 ms, lower
6.7 ms, emit ~58 ms, total ~74 ms; emission is ~80%, and `emit-module` is
whole-program, so a per-definition cache key cannot reach it. A one-definition
compile is 9 ms. And capture is already effectively cached — it happens once, at
`dsp/defn` evaluation.

Nothing depends on the slice: P14 needs P13's compiler-in-browser, which is done.
And the error classification is P15's deliverable 2 almost verbatim, so building
it here would be a duplicate. If compilation ever needs to be faster, the leverage
is emission, and that needs its own decision.

**Verified.** Docs only; no code changed. JVM **284 tests / 2 620 assertions / 0
failures**; browser **54/54**; parity **211 cases, no gaps** were green at the
measurement.

---

## 2026-10-25 — D94 — the pipeline stops knowing what a target's artifact looks like

A target-inventory pass asked where shared code is really used by only some of the
four lowering paths. The answer was mostly "it isn't": `builtins` names no target,
the four `targets/*` are self-contained, and the host layer splits cleanly into
`fusion`/`mixing`/`pool` plus one kernel per host. The exception was
`compiler.cljc` — the "portable pipeline" owned each target's artifact:
`target-instance`'s `case` over four targets, `compile-js-target`, the
interpreter's instance closures, the whole Wasm artifact map, and the assembler
pin. About 130 lines — the file went from 553 to 432.

The tell was that `:jvm` already did it right — it registers its own
`compile-target` method from the side that has `insn`. So the other three now match
it: the pipeline builds the shared base, each target's `compile-artifact` adds what
only it has. That is also the honest shape, because the artifacts genuinely differ
— the interpreter and JVM carry `:invoke`, the two code generators carry a module —
and the pipeline no longer has to know either way.

The same pass caught `ir`'s inlining policy: a `#{:jvm :js :wasm}` set plus an
`(= :interpreter target)` special case. That is a fact about targets, so it became
`targets.capability/calls?`, and `ir` names no target now.

No behaviour change, and that *is* the verification: the artifact maps come out
identical, so `compiler equivalence` (JS forms, Wasm module text, canonical IR) and
`parity` are the proof rather than a new test.

**Verified.** JVM **284 tests / 2 620 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D95 — the bundle leaves the JVM's assembler

`omkamra.dsp.hosts.jvm` was a `.clj` file whose name promised all hosts and whose
content was one world's assembly. The user kept coming back to it, and the
instinct was right: its own docstring said the bundle "can be sent to another
host", and `hosts/instrument.cljc` said outright that "a browser host has no
compiler-side assembler here". So the portable half was locked in a JVM-only
file, and a browser could only receive a bundle over the wire — which is exactly
what the harness does.

`synth-bundle` now lives in `omkamra.dsp.synth.bundle` (`.cljc`), and
`omkamra.dsp.hosts.jvm` keeps the JVM world's kernels: 265 lines to 103. Three seams
made it portable. The voice resolves through `:definitions` — the same map
`dsp/compile` takes — with `requiring-resolve` only as the JVM's fallback. A
descriptor rather than a Var is what the combinators are handed. And the fused
path dropped a `(dsp/link …)`: `dsp/compile` already links, and the extra call
linked with `{}`, so it could not see `:definitions`. That one is a latent bug
fixed by deleting a call.

`:target` still defaults to `:jvm`, deliberately — the descriptor's own default is
`:interpreter`, so a bundle that omitted a target would silently compile one.
That is the last JVM-flavoured thing, and it is documented where it lives.

The `.cljc` claim was checked rather than assumed: with it required from the
testbed page build, Shadow emitted `cljs-runtime/omkamra.dsp.synth.bundle.js`, so
it is ClojureScript-legal today. Nothing in the tree requires it yet — the browser
assembler is P14's — so the check is not recurring, and that is the honest state:
a browser *can* build a bundle, but nothing does.

**Verified.** JVM **284 tests / 2 620 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean on the
touched paths.

---

## 2026-10-25 — D96 — the names catch up with the axes

Three naming passes in, the map is literal: **world** = JVM/browser, **target** =
`:jvm`/`:interpreter`/`:js`/`:wasm`, **kernel** = a target's `Kernel`.
`omkamra.dsp.hosts` was the JVM world's assembler sitting as the *parent* of
`omkamra.dsp.hosts.jvm`, which was the `:jvm` kernel — a parent and a child on
different axes, which is why the file kept nagging. Now `hosts/jvm.clj` is the
world and `kernels/jvm.clj` is the target, and one glance tells them apart.

The four namespaces that shared the old tree turned out not to be one kind of
thing, and the requires settled it: `mixing` and `fusion` are required by every
kernel, `instrument` by the portable reference synth (a layer *above* the pool),
and `worker` by nobody — it is a Shadow `:web-worker` entry. So `mixing` went to
`kernels/`, `fusion` to the top level (pure data, no dependencies, a concept),
`instrument` to `instrument/program`, and `worker` to the top level as the build
entry it is.

`mixing` turned out not to be one subject, so it was cut by subject instead of
renamed: its numeric half — the typed array, and how to read a value back out of
whatever a host supplied — went to `numbers`, and its region half — where a pool's
lanes and shared values actually live — became `kernels/regions`. What is left is
what the name says: rendering a voice into scratch and summing it into the block,
under the containment policy. `kernels/common` was on the table and lost: it would
say what code is shared *by*, not what it does, and would be the only
subject-less namespace in the tree.

One incidental find: the browser suite failed 47/48 on the first run and passed on
the second. The page build was still being rebuilt with the renamed namespaces, so
that was a raced build rather than a check — worth knowing when a rename looks
like a regression.

**Verified.** JVM **284 tests / 2 620 assertions / 0 failures**; browser
**54/54**; parity **211 cases, no gaps**. `clj-kondo` and `cljfmt` clean.

---

## Handoff — current state

The one entry rewritten in place, and always the last. It holds where we are, what
is next, and what shapes the next decision. Everything durable belongs to the file
that owns it — `README.md` says which — so this points rather than restates, and a
bullet that is still true two slices later is in the wrong file.

### Where we are

**P13 is done** — P13a–P13d implemented and verified, P13e dropped (D93) — and
three settling passes followed: a target's artifact shape now lives in the target's
own namespace (D94), the world-independent synth bundle left the JVM assembler
(D95), and the names now follow the axes — world, target, kernel (D96).
`dsp/compile` takes a definition or a source string in both worlds; the bus lives
behind one kernel interface on all four targets; `instrument.program` is `.cljc`; the reference synth
is a single `.cljc` the page compiles its own capture of; a state region's zeros
travel as a size (canonical IR 1.77 MB → 40 KB, JS module 3.65 MB → 11 KB);
`omkamra.dsp` is a facade again — the instance lifecycle lives in
`omkamra.dsp.instance` and is re-exported. And the round trip works: the browser
front end recovers a literal's spelling from the source text — a form's own, and
the children's for a bare number (D92) — so the reference synth's own `.cljc`
source compiles the same in the page as on the JVM.

Suite: JVM **284 tests / 2 620 assertions / 0 failures**, browser **54/54**,
parity **211 cases, no gaps**.

### What's next

1. **P14** (artifact-driven worklet installation), then **H1**, **P15**,
   **P16** per `PHASES.md`. P13 is complete; P13e is dropped (D93). The browser
world's assembler — the kernel it builds from a bundle — is still missing (D95);
P14 is where it belongs.

### What shapes the next decision

P14's choice is named in `PLAN.md` §Open questions: the artifact's byte transport
(a data URL, a `Uint8Array` message, or a blob URL) and whether `:wat` ships in
every artifact — both decided when the artifact becomes what a worklet is
installed from. Everything else undecided is in `PLAN.md` §Open questions.
