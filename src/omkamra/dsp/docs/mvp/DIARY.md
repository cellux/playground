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

## P3 — Transcendental math intrinsics (completed)

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

## P4 — Local arrays, constant tables, aggregate state (completed)

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

## P5 — Delay lines, circular buffers, and interpolation (completed)

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

## P6a — Stateful module composition (D19/D20) (completed)

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

## P6b — Modules that own records and arrays (completed)

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

## P6c — The composed filter voice (completed)

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

## P6b-ii — Record returns (multi-output) (completed)

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

## P7a — Library namespace seams and a generated reference (completed)

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

## P7b — Band-limited oscillators (completed)

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

## P7c — `if` branches are separate scopes (completed)

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

## P7d — Noise, LFO, and a parameter-typing fix (completed)

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

## P7e — Control-rate helpers, distortion, cubic interpolation (completed)

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

## P7f — Multimode, linear envelopes, chorus, spreading, sync (P7 complete)

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

## P8a/P8b — Target-neutral process instances (deliverables 1, 2, 4, 5 done)

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

## P8d — Optional input ports (D23, deliverable 6)

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

## P8c — Instance independence in the browser (P8 complete)

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

## P9a/P9b — The sequencing layer (deliverables 1, 2, 4 and 6)

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

## P9c — Control smoothing and slew, declared on the control (D27)

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

## P9d — Host-supplied automation curves: per-frame lanes (D28)

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

## Handoff — current state after P6 (rolling entry)

This is the one living entry, as opposed to the completed-phase entries above:
what exists, how it fits together, what is open, and what to do next. It may be
rewritten in place. Read with `PLAN.md`, `PHASES.md`, and `DECISIONS.md`.

### Where things stand

- **P1 (integer bitwise/shift) is complete.** The seven operators exist across
  interpreter/JVM/JS/Wasm per `DECISIONS.md` D3.
- **P2 (portable scalar math builtins) is complete**, including the
  capability-aware registry, the D9–D12 semantics, and the first standard
  library entries (`omkamra.dsp.library`).
- **P3 (transcendentals) is complete** under D14 (native intrinsic per
  target, emulation fallback, target-dispatched matcher). All thirteen
  operations are done end to end on interpreter/JVM/JS/Wasm; the `:wasm-math`
  option selects the Wasm emulation strategy (`:polynomial` or the f32
  `:table`); and the library's `sine-oscillator` and `exponential-adsr`
  render on all four targets.
- **P4 (local arrays, records, constant tables) is complete** under D15/D16.
  `(array :float|:int N)` declares preallocated scratch; `aget`/`aset` (or
  `array-load`/`array-store`) index it. `dsp/defstruct` names a record shape,
  referenced with `{:name slot :struct shape}` (plus `:size` for arrays of
  records) and accessed with `field`/`set-field!`; the linker inlines the
  fields, laid out struct-of-arrays in the same arena as state and scratch. A
  scalar record may own an array field (`:size M`), indexed with `(aget rec k)`
  or `(field rec :buf k)`. `:constant` tables are read-only and materialized at
  initialization. The ABI exposes `:records` and `:arena`.
- **P5 (delay lines, circular buffers, interpolation) is complete** under D17
  and D18. The library provides `linear-interpolate`, `allpass-coefficient`, and
  the `delay-line` / `allpass-delay-line` modules (with `-voice` process
  wrappers); the convention is a
  persistent buffer plus an `:int` cursor with write-then-read ordering and
  floored `mod` wrap. Array storage is a static *place* (D18), so a record owns
  its buffer and `(aget (:buf rec) k)` / `let` aliases index it. Catalog vectors
  cover zero, unit, fractional, wrap, allpass, and the place form.
- **P6 (stateful module composition) is complete** under D19/D21/D22, and the
  phase gate is met: `filter-voice` composes a stateful RBJ biquad with a
  stateful exponential ADSR and runs identically on all four targets. A call to
  a stateful module owns one private state partition per call site; the
  transition applies at the call; nested composition works; a module may own a
  record/array end to end; and a module may return a **named record** for
  multi-output (`state-variable-filter` yields low/band/high). The interpreter
  executes `:call` against the partition (the oracle); JVM/JS/Wasm share one
  statement-level inliner. The coloring pass under D20 (partition coloring)
  is deferred: it is an arena optimization, not a P6 deliverable.
- **The conformance harness is interpreter-oracle-driven** (D7/D13): the shared
  catalog is pure data, and the reference interpreter is the single oracle on
  the JVM *and* in the browser.
- **North star:** users eventually edit DSP definitions in the browser
  (CodeMirror) and compile them there, which means the compiler itself becomes
  portable and runs in SCI (P13). The library and every new language form are
  therefore authored as portable `.cljc` source forms.
- Verified through `clojure_eval` on the running nREPL: JVM 81 tests / 852
  assertions, browser 17/17 (four shared matrices matching 80 process cases and
  13/3 function cases on JavaScript and Wasm, the WAT bounded-loop and sin-table
  checks, four instance-independence checks, four sample-accurate-scheduling
  checks, and the three AudioWorklet adapters), and `clj-kondo`/`cljfmt` clean.

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
- P6a added the first *callable modules*: `one-pole` (scalar state) and
  `one-pole-pair` / `one-pole-voice` / `one-pole-pair-voice` that compose it
  through calls. P6b turned `delay-line` / `allpass-delay-line` into modules
  (with `-voice` wrappers and a `delay-line-pair-voice`). P6c added `biquad`
  (record-owned history) and `filter-voice`, the done-when ADSR + biquad voice.
- **P7 (DSP standard library) is complete.** Eight namespaces
  (`library.{math,oscillator,noise,envelope,filter,delay,control,effect}`) cover
  the phase's contents list: band-limited oscillators (PolyBLEP saw/pulse/
  square/triangle, plus non-band-limited hard sync), white/pink noise, an LFO,
  linear and exponential AR/ADSR envelopes, smoothing and slew limiting,
  one-pole/biquad/multimode/SVF filters, delay/chorus/distortion, linear/cubic/
  allpass interpolation, and stereo spreading as a record-returning module. The
  reference page `docs/mvp/LIBRARY.md` is generated from the descriptors by
  `omkamra.dsp.library.doc` (with a drift-guard test).
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
4. **Non-finite sample-to-int conversion is undefined across targets.**
   `float->int` of NaN/infinity throws in the interpreter but saturates on the
   JVM (`d2i`), with JavaScript/Wasm behavior of their own. The conversion
   special-value contract is still open (`PLAN.md`); P4 hit it through an
   int-array test and routed around it.

### Gotchas burned into this session

- **A stale Shadow cache can silently mask an editor's mistakes.** After the P7
  namespace split, `.shadow-cljs` still held a cache entry for the *deleted*
  `omkamra.dsp.library`, and it expanded `.cljc` library sources with a **stale
  cached `omkamra.dsp/defn`** (from before docstring support), printing
  `Syntax error macroexpanding dsp/defn … params must be a vector` on a
  background thread while every eval and test still passed. Touching the macro
  namespace did not help (Shadow keys its cache on content, not mtime). Fix:
  stop the dev server, `rm -rf .shadow-cljs .dev` (both gitignored build
  artifacts), restart. The restart then surfaced two genuine breakages that the
  in-memory state had been hiding: two catalog cases still called
  `omkamra.dsp.library/clip` and `…/triangular-window` (library calls *inside*
  case bodies, which the split's rewrite had missed), and `LIBRARY.md` was stale
  (caught by the drift-guard test, exactly as intended). Both fixed.
- `-0` in source is **integer zero**; negative zero is `-0.0`.
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
- A `:reload` flag placed mid-list — `(require 'a :reload 'b :reload)` — does
  not reliably reload every namespace, and a stale served catalog shows up as a
  browser 500 for a case the server does not know. Reload explicitly:
  `(doseq [n [...]] (require n :reload))`.
- `spel`'s bundled Playwright wants a different Chromium revision than the
  installed one; `omkamra.dsp.browser/installed-headless-shell` finds the
  installed headless shell and passes it as `:executable-path`.
- The Shadow watcher sometimes reports `0 compiled`; call
  `omkamra.dev.frontend/recompile!` and re-check. Dev assets are served with
  `cache-control: no-store` now.

### Next steps

1. Finish **P7 — the DSP standard library**: smoothing/slew, multimode biquad,
   effects (chorus, distortion), and cubic interpolation, plus converting the
   older `;;` comments to docstrings. Composition is in place, so library entries
   are reused modules rather than processes.
2. **P8 must include optional input ports (D23).** A process with `:input false`
   has no sample parameter and no `:input` buffer in the ABI, so generators stop
   carrying an ignored `sample` and a dead buffer load that falsely reports
   `:read-memory`. It belongs in P8 because that phase already moves the ABI's
   argument indices (instance handle), so the index/descriptor/adapter rework is
   paid once instead of twice.
3. Keep a structured diagnostic for any operation a target cannot serve at all.
4. Optionally add generated-target (JS/Wasm) development guards and restore the
   dropped Wasm f64 worklet checks.
5. Deferred, optional: the D20 partition-coloring pass (dead-on-frame-entry
   state and P4 scratch), most valuable once P10 gives many sibling modules.

### Wasm lookup table — done, with follow-ups

Steps 1 and 2 are done (shared emulation functions; f32 sine table via
`:wasm-math :table`). Remaining:

- **Benchmark** the table against the polynomial before changing the default;
  `:polynomial` is still the default.
- **f64**: a raw table would be too large; consider a piecewise minimax
  coefficient table if f64 Wasm speed ever matters.
- **`exp`** has no table (polynomial only); fine while it is not hot.
- **Table size**: 4097 f32 entries (16 KB payload, ~53 KB WAT text). Could
  shrink to 2049 entries if the interpolation error stays acceptable, or grow
  for margin.
- **Cos** currently reuses `$dsp_sin` with a π/2 phase shift; acceptable for
  the tested ranges but a direct quarter-period offset would be more accurate
  for large arguments.

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
