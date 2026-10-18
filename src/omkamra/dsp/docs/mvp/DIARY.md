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

## P10a — The sequencing runtime: description, voice pool, allocation (P10 partly complete)

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

## P10b-i — Voice modes (D33)

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

## P10b-ii — Modulation as an example instrument (D34)

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

## D38 — Clojure assembles the static graph; the engine owns dynamic instances

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

## P10c — Voice tails and containment (in progress)

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

## P10d — The JavaScript target emits through Squint (D41)

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

## P10d — The browser host and the worklet boundary (D40, slice 3)

**Delivered.** A pool runs in the browser over a generated JavaScript module. The
kernels moved out of `omkamra.dsp.synth.host` into an `omkamra.dsp.hosts` family
— `.jvm`, `.interpreter`, `.js`, with `.wasm` still to come — and the plumbing
every one of them shares (the containment policy, and the planar blocks a voice
is rendered into and mixed through) into `omkamra.dsp.hosts.mixing`. `mixing` is
`.cljc`, so the JVM suite and the browser testbed run the *same* reference oracle
rather than two copies of it. `omkamra.dsp.hosts` itself is the assembler:
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

**The Wasm host.** `omkamra.dsp.hosts.wasm` is the same kernel seam over a Wasm
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

## P11a — the reference instrument (in progress)

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
`omkamra.dsp.hosts.mixing/channel-of` did `(aget buffer channel)` with no type
hint, so selecting a plane of a planar block was a *reflective* `RT.aget` —
about 24 KB per channel per block on the JVM. It never showed up because the
mono path does not index, and no allocation check covered stereo. P11's
instrument is stereo, so the guard caught it: ~195 KB per block before, ~2 KB
after.

**Remaining P11.** Post-mix effects and effect tails (P11b, ideally as a
transform, possibly needing a new `dsp/def*` form), all-backend offline
rendering (P11c), and the property battery (P11d).

---

## Interpreter allocation pass (P11 prep)

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

## P11b-i — the instrument graph and the post-mix chain (D42)

P11's effects land as a **Clojure-level instrument graph** lowered to a
**program**, which is the shape the later slices will refine.

**The source layer** (`omkamra.dsp.instrument`, `.cljc`) is a value plus plain
functions: `instrument` (the description's top-level fields), `voice`,
`routes`, and `with-effect`. Nothing there knows about kernels or channel
layout.

**The lowering** (`omkamra.dsp.hosts.instrument`, `.clj`) has one strategy so
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

## P11c — all-backend offline rendering

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

## P11d — the property battery (P11 complete)

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

## Refinement — a control name is a symbol, however it is spelled (D43)

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

## Handoff — current state (rolling entry)

This is the one living entry, as opposed to the completed-phase entries above:
what exists, how it fits together, what is open, and what to do next. It may be
rewritten in place. Read with `PLAN.md`, `PHASES.md`, and `DECISIONS.md`.

### Resume here

- **Where.** `omkamra.dsp` is a DSP compiler (macros → descriptors → typed IR →
  interpreter/JVM/JS/Wasm) plus a host-side sequencing layer. P0–P11 are done:
  **P11 is complete** (P11a the instrument and matrix, P11b the instrument graph
  and post-mix chain per D42, P11c all-backend rendering, P11d the property
  battery), along with an interpreter allocation pass. The phase was scoped to
  end with a **refinement round** over D42's graph, the modulation matrix (D34),
  and the per-feature layer boundary; that is the next work.
- **Pitch domain (D35).** A voice takes `note`/`bend`/`detune` (cents) and
  converts to Hz in the DSP (`omkamra.dsp.library.pitch/pitch->frequency`); the
  pool delivers the standard sources and stays pitch-agnostic.
- **Synth runtime.** `omkamra.dsp.synth` (description → pure-data config);
  `synth.pool` (modes `{poly, mono, legato, retrigger}`, allocation/stealing,
  global source bank `set-pitch-bend!`/`set-detune!`, `read-controls!` +
  `set-voice-controls!`); `omkamra.dsp.hosts` (the assembler: `synth-bundle`,
  `compile-synth`) over the kernel family `hosts.{jvm,interpreter,js}` and their
  shared plumbing `hosts.mixing`.
- **Stereo (P10d, D40, slices 1a–3 done).** One evaluation per frame;
  multi-channel output is explicit per-channel stores; state may be shared across
  channels. A stereo voice renders a **planar** block through the pool, which
  sums voices plainly (it never pans). `:pan` is a standard **per-voice** source
  (`set-pan!`, the stereo counterpart of `:detune`); a voice that binds no `:pan`
  control pans inside its own DSP instead. The library `effect/pan` (equal-power,
  -1..1) and `effect/stereo-spread` (Haas) both return a `stereo-pair`. The
  browser host is `omkamra.dsp.hosts.js` (a `Kernel` over a generated JavaScript
  module) and `omkamra.dsp.hosts.wasm` (views over a Wasm instance's own
  linear memory), each checked against the interpreter pool by the browser suite
  (`JavaScript pool host`, `Wasm pool host`); both worklet adapters call a kernel
  once per block. **P10 is complete.**
- **JavaScript target (D41).** The JS backend emits by building ClojureScript
  forms and compiling them with Squint, so the module is standalone (no
  `squint_core`, no import) and the emitter no longer writes JavaScript text.
  `tests/js.clj` pins the contract and the catalog-wide standalone rule; Node is
  the fast loop while working on it. The Wasm target still emits WAT by hand.
- **Voice tails (P10c, D39).** Tails are *declared, not inferred*: a
  released voice is kept until stolen by default; a voice that writes the builtin
  `:activity` slot is reset + freed when it reads ≤ 0. A scalar state slot is now
  body-writable (`set!`), which is what lets a body local (e.g. an ADSR's output)
  become a flag; `(store-special :activity v)` writes the builtin slot, added to
  the state by scanning the body, with default `:init 1` (active). Specials are a
  **bitmask** with fixed bits (`:activity` = 0) placed at the **front** of the
  entry's state; the ABI exposes `:special-mask` + `:special-slots`, and each
  kernel resolves the physical slot **at construction** (no map lookup on the
  audio path). `compile-synth` carries the mask. A callee's `store-special` is
  rejected. Non-finite containment is a **kernel policy** (`:containment
  :reset` default — silences + resets + counts, per `PLAN.md`; `:sanitize`
  contains; `:off`), applied per voice before mixing. **Remaining:** the JS/Wasm
  readback and containment (with P10d).
- **Definition values and transformers.** `dsp/fn` returns an anonymous
  definition value; `dsp/defn` is `def` + the same capture; `dsp/def` names a
  derived value; `->` chains transforms. **Promoted to core:**
  `omkamra.dsp.graph` holds the substrate (`definition-id`, `parameter-symbols`,
  `definition-call`, `derived-definition`), and `omkamra.dsp.combinators` holds
  `unison`, `mix`, `sum`, `through`, `poly`, `share`, and `smooth`/`slew`.
  `omkamra.dsp.examples.demo-synth` is the instrument that uses them.
- **Graph model (D38).** Build in Clojure, lower once, link; the fixed expression
  is already a DAG via `let`, so fan-out does not force a new IR. Hoisting is the
  cost control: computed call arguments bind once at lowering, `poly` hoists
  shared controls and expands only `:per-slot` ones, and `share` is a `let`.
- **Optimization stage.** Already planned (P15 deliverable 4, `PLAN.md` §9);
  D38 records the concrete candidates (dedup of identical inlined bodies,
  stateless CSE) and that argument hoisting is in place.
- **Removed from the pool:** `:unison`, `unison-detune`, the unison branch; the
  mode set is `{poly, mono, legato, retrigger}`.
- **Tests.** JVM `(omkamra.dsp.tests.suite/run-all)` — 154 tests / 1513
  assertions; browser `(omkamra.dsp.harness.browser/run! {:server? false})` —
  23/23. See `TESTING.md`.
- **P10d — stereo (D40, in progress).** The compiler model is now *one evaluation
  per frame*, not run-per-channel: the four targets lost their channel loop, the
  implicit `channel` is 0, a `:void` body that writes `:output` (a process, by
  that fact alone — no `:process` map needed) writes per channel, and stateful
  multi-channel compiles — so a shared-state stereo voice (reverb,
  `stereo-spread`) is expressible. Bodies also follow `defn`: several forms are
  an implicit `(do ...)`. Slices 1a–2 are done (the audio width is *counted* from
  the body's buffer accesses, and a stereo voice renders a **planar** block
  through the pool, whose voices are summed plainly); remaining: the
  CLJS/AudioWorklet host (3).
- **Next.** P11 (the reference synthesizer acceptance fixture), or first the
  D36 inventory — the transforms still unimplemented (oversampling; stereo
  spread, now unblocked), promoted one at a time as the instrument needs them.
- **Linting.** `.clj-kondo` uses opaque hooks for `dsp/defn`/`dsp/fn`/`dsp/defstruct`
  and `:lint-as clojure.core/def` for `dsp/def`; a real DSP-aware linter is
  designed but deferred (three findings in `TESTING.md`).
- **Exploration (deferred).** Plain Clojure functions as macros inside DSP forms
  — an expansion pass ahead of normalized descriptors; rationale, candidates, and
  hazards are in `PLAN.md` §10.
- **State.** Everything since P10a is uncommitted (staged/working tree); the user
  commits when P10 is finalized. The dev nREPL is running; the `:on-demand-http`
  Shadow watcher is running.

- **P7 (DSP standard library) is complete.** Eight `.cljc` sub-namespaces under
  `omkamra.dsp.library` cover band-limited oscillators (PolyBLEP saw/pulse/
  square/triangle, PWM, hard sync), noise, linear and exponential AR/ADSR and
  LFOs, one-pole/biquad/multimode/SVF filters, delay/chorus/distortion and
  linear/cubic/allpass interpolation, and stereo spreading. `docs/mvp/
  LIBRARY.md` is generated from the docstrings with a drift guard.
- **P8 (process instances, D23) is complete.** `dsp/create-instance` returns an
  independent state region over the same compiled program, `dsp/destroy!`
  releases it, and every lifecycle function accepts an artifact *or* an
  instance. A process input port is optional (`:input false`), which makes a
  generator just a process with no input buffer.
- **P9 (sample-accurate automation) is complete** — see P9a–P9d above. The
  sequencing layer (`omkamra.dsp.event`, `omkamra.dsp.schedule`) is host-side
  and drives the kernel by segmentation; the kernel itself carries declared
  `:smooth`/`:slew` control modes (D27) and host-supplied per-frame automation
  lanes (D28), and no backend has a ramp or curve op.
- **P10a is complete** — see the P10a entry above. A synth is one serialisable
  description that yields a voice artifact and a pure-data runtime
  configuration; `omkamra.dsp.synth.pool` runs N kernel instances with
  deterministic allocation and stealing, the note lifecycle, and block
  rendering; and `omkamra.dsp.synth.host` adapts the JVM and the reference
  interpreter, so a pool is checked against the interpreter hosting the same
  pool. No compiler surface changed.
- **P10b-i (voice modes) is complete** — see the P10b-i entry above. `:mono`,
  `:legato`, `:retrigger`, and `:unison` are declared allocation policies over
  the same pool (D33); the monophonic held-note stack is bounded by a declared
  `:held-notes` capacity, and unison detunes symmetrically in cents. Still
  host-side, still no compiler surface.
- **P10b-ii (modulation) is complete, as an example (D34).** Core gained exactly
  one affordance, `omkamra.dsp.synth.pool/set-voice-controls!`; the wiring lives
  in `omkamra.dsp.examples.demo-synth`, the growing example instrument. Patterns
  the example exposes are listed for inspection before any is promoted to core.
- **Pitch is pitch-domain, converted in DSP (D35).** The vocabulary is core
  now: `control-sources` has `:bend`/`:detune`, the description has `:bend-range`,
  the pool has a global source bank (`set-pitch-bend!`) and a per-voice detune,
  and the conversion default is
  `omkamra.dsp.library.pitch/pitch->frequency`. `read-controls!` was added as the
  read half of the host control channel. Only the (b) transformer remains.
- **Voice transforms are functions over definition values (D36).** `dsp/def`
  names a derived definition, `->` chains transforms, and `unison` is the first
  one. **Validated and promoted**: the transform is bit-identical to the sum of
  its detuned copies (interpreter and JVM), and the pool's `:unison`,
  `unison-detune`, and unison branch are removed. `:detune` stays a host source,
  set by `set-detune!`. The mode set is `{poly, mono, legato, retrigger}`.
- **A definition is a value (D37).** `dsp/fn` is the anonymous counterpart of
  `dsp/defn` (one shared `capture-definition`), so definitions compose with `->`.
  `smooth`/`slew` are transformers that *set metadata* the compiler already
  desugars (D27), so the inline and transformer surfaces are one mechanism and
  match bit-for-bit; they work on anonymous `dsp/fn` values. Call-emitting
  transforms (`unison`) work on anonymous values too: the derived value carries
  the original in `:dsp/dependencies`, which `link` supplies.
- **Verification.** `omkamra.dsp-test` on the JVM is the primary suite, with an
  `:automation-lane` and other catalog cases shared through `omkamra.dsp
  .acceptance`; the browser testbed runs the same catalog on JavaScript and Wasm
  against the portable-IR oracle. Every check carries a positive or negative
  twin: a value that must change, a lane that must be reported missing, a stride
  that must differ between precisions.

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

### Layout after the reorganisation (before P10)

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

### Architecture

```text
omkamra.dsp.targets.capability   (.cljc)  what each target serves (D29)
omkamra.dsp.targets.interpreter  (.cljc)  portable reference interpreter (the oracle)
omkamra.dsp.targets.{jvm,js,wasm}         the three code generators
omkamra.dsp.descriptor, ir, linker, builtins   the compiler front end
omkamra.dsp.library.*            (.cljc)  the DSP standard library (P7)
omkamra.dsp.event, schedule      (.cljc)  the host-side sequencing layer (P9)
omkamra.dsp.synth                (.cljc)  synth description + runtime configuration (P10a)
omkamra.dsp.synth.pool           (.cljc)  preallocated voices, allocation, rendering (P10a)
omkamra.dsp.synth.host           (.clj)   JVM/interpreter kernels, compile-synth (P10a)
omkamra.dsp.harness.catalog      (.cljc)  data-only conformance catalog
omkamra.dsp.harness.conformance  (.clj)   plan building, JVM differential driver
omkamra.dsp.harness.{browser,testbed}     spel driver and the browser suite
omkamra.dsp.harness.fixtures     (.clj)   shared test fixtures
omkamra.dsp.tests.*              (.clj)   the suite, by subject (tests.suite/run-all)
```

### Interpreter-oracle conformance

- A **plan** (`conformance/plan`) is target-neutral data: the compiled portable
  IR, controls or function vectors, and the invocation sequence (blocks, reset,
  special values). It contains **no expected outputs**.
- **JVM:** the interpreter computes expected outputs; the JVM backend is
  compared against it (`interpreter-oracle-conformance` in `dsp_test.clj`).
- **Browser:** `/omkamra/dsp/harness/testbed/plan` serves the plan; the page reads the
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

1. **(Done, D29.) Capability diagnostics are now explicit — and the real gaps
   were not where the original note guessed.** It assumed the operator set was the place to declare
   support, because Wasm has no native `sin`/`cos`/`exp`/`log`. That is stale:
   P3 gave every transcendantal an emulation, so **every builtin is served on all
   four targets** and a per-operator capability table would be speculative —
   unreachable code with no way to test it. The differences that actually exist
   are in the *options and entry kinds*, and two of the three are silent:
   - **`:checks :development` is honoured only by the interpreter.** Verified: no
     backend mentions it, and it is not surfaced in the ABI, so requesting it for
     the JVM, JS or Wasm buys nothing and says nothing. This is also what the old
     "generated-target development guards" item meant.
   - **`:wasm-math :table` is f32-only** and silently becomes `:polynomial` at
     f64 (the table is a byte-stride region, so it has no f64 form).
   - **The `:function` entry is unavailable on Wasm** — by design, but reported as
     a generic unsupported-target failure rather than as "Wasm serves process
     entries only".

   All three are now declared in `omkamra.dsp.capability` and refused at compile
   time with a message naming what the target does serve; `:checks` is a
   per-target default, so a generated-target compile no longer claims validation
   it does not implement. The rest of P15 deliverable 1 — allocation, reflection,
   locks, unbounded loops, unvalidated memory access — is still outstanding,
   though the IR's `:diagnostics` channel already carries the loop and dynamic
   index warnings.
2. **(Done.) Browser coverage is restored.** The dropped Wasm **f64**
   AudioWorklet check is back: the worklet step now renders at both precisions,
   and the f64 case is the one that matters, because the adapter reads its
   element type and width from processor options — so the *same* adapter source
   has to serve f64, and only the module behind it changes. Checking f32 alone
   left that untested. Browser suite 19/19.

   The other two checks the rewrite folded away are genuinely covered by the
   shared matrix: the "generated-kernel" checks are the JavaScript path the
   matrix already drives per case, and the "Wasm-module" checks are the Wasm path
   it drives the same way. Neither needs a separate check.

   One thing worth knowing about the restored check: the f32 and f64 renders
   report the *same* peak, because the worklet case is a one-pole at coefficient
   0.5, whose arithmetic is exact in both precisions, and `OfflineAudioContext`
   narrows to f32 in any case. So it does not discriminate the precisions
   numerically — what it tests is the adapter's element width, where getting 8
   bytes wrong would misalign every read and show up as silence or garbage.
3. **Function-entry cases are JS-only.** Wasm is process-only by design, so the
   function matrix runs on interpreter/JVM/JS; Wasm coverage for the same
   operators comes from process cases.
4. **Non-finite sample-to-int conversion is undefined across targets.**
   `float->int` of NaN/infinity throws in the interpreter but saturates on the
   JVM (`d2i`), with JavaScript/Wasm behavior of their own. The conversion
   special-value contract is still open (`PLAN.md`); P4 hit it through an
   int-array test and routed around it.
5. **The allocation test no longer compares exact totals** (fixed). It asserted
   `processed <= baseline` over two exact thread-allocation measurements, which
   passes with *zero* headroom: both sides read 9880 bytes over 10 000 calls, so
   one stray byte failed it. It now allows four bytes per call, which is still a
   real bound rather than a fudge — every possible per-call allocation is far
   larger (a boxed scalar is tens of bytes; the regression it guards against was
   ~24 KB per call, D26). Negative control: the same measurement reads an
   allocating reference path at 994 MB across the same loops, 24 862x the
   allowance, so the bound has teeth.

### P15a — Explicit capability diagnostics (pulled forward, D29)

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

P1–P9 are complete, and P10 is under way, except for the residuals listed under
"Open issues" and the two partial P9 deliverables (`PHASES.md`, P9 "Not done"):
linear/critical ramps, which a host can supply as a lane instead, and non-finite
containment, which belongs with P10's voice pool.

1. **Finish the transformer layer (D36/D37).** Step 2: give anonymous `dsp/fn`
   values an internal id and carry their anonymous dependencies in the value
   (`:dsp/dependencies`), fed to `link`'s `:definitions`, so
   `(-> (dsp/fn …) (unison …))` resolves. Then work the inventory one transform
   at a time: oversampling; stereo spread (needs P10d's multi-channel output);
   per-voice effects. (`:smooth`/`:slew` is done as D37 and stays compiler sugar.)
2. **P10c — voice and effect tails, and non-finite containment.** This is where
   the one genuinely new mechanism lives: the kernel has to report that a voice
   has finished. The P10a entry records the reconnaissance — a JS/Wasm instance
   does not expose its state, a multi-channel process replicates rather than
   carrying a different signal per channel, and the output buffer is the only
   channel every target already writes — so the candidates are an *output* status
   region symmetric with `:lanes`, or a declared state readback.
3. **P10d — stereo spreading, effect tails, and the CLJS/AudioWorklet host.**
   The pool is `.cljc` and host-neutral already; this is the adapter, plus the
   second channel everywhere.
4. **A structured capability diagnostic.** There is still no registry-level,
   pre-flight check: an unimplemented `[operator type]` fails in a backend's
   `:default` method at emission time. Worth doing before P12 adds more surface.
5. **Generated-target (JS/Wasm) development guards**, and restoring the dropped
   Wasm f64 AudioWorklet check. Lanes are a live example of the gap: the Wasm
   *worklet* adapter does not carry a lane region, because no case drives it.
6. **The D20 partition-coloring pass is deferred, now with numbers.** D20
   permits reusing a partition only when it is **dead on frame entry**, and
   colours both persistent state and P4 scratch. Measured over all 83 catalog
   process cases: **663** persistent-state slots against **14** scratch slots,
   and only 4 cases have any scratch at all, each a single array — so there is
   nothing to share *within* a case, and the pass would save nothing on any case
   that exists.

   There is also a structural reason the state half can never pay. A partition is
   dead on frame entry exactly when every path writes it before reading it; but
   that is the definition of state that **carries no information across the
   frame boundary**. Every library module reads its state before writing it —
   that is why it is state — so a state partition the analysis would accept is
   one whose contents were never needed. The payoff is therefore real but
   confined to *scratch*: a module that refills a buffer each frame before
   reading it, such as an FFT or convolution block, which the corpus does not
   have.

   So if it is built, it should be **scoped to scratch**: that removes the
   riskiest half (touching persistent state means instance-state sizing and the
   ABI) and keeps the half with a reachable beneficiary. Otherwise it waits for a
   real one.

### Wasm lookup table — benchmarked, cosine fixed, with follow-ups

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

### How to run everything

See `TESTING.md`. Short version, through `clojure_eval`:

```clojure
;; The edit-reload-test loop: initialize clj-reload BEFORE editing, then reload
;; after each edit. Do not substitute `require :reload`: it leaves dependents
;; stale, which shows up as stale protocol classes and phantom failures.
(require '[clj-reload.core :as reload])
(reload/init {:dirs ["src"] :output :quiet})       ; once, before editing
(let [r (reload/reload)] (select-keys r [:unloaded :loaded]))

;; JVM (99 tests, 1074 assertions)
(require 'omkamra.dsp.tests.suite)
(omkamra.dsp.tests.suite/run-all)

;; Browser (JS + Wasm), 19 checks. `:on-demand-http` starts the Shadow watcher;
;; do not start a second one.
(require '[omkamra.dev.on-demand-http :as on-demand-http])
(on-demand-http/start!)
(require '[omkamra.dsp.harness.browser :as browser])
(browser/run!)
```

Two traps worth remembering: reloading a namespace **invalidates REPL aliases**
and class identity, so prefer fully-qualified names in evals and expect
`instance?` checks against reloaded classes to be meaningless; and a reloaded
namespace is JIT-cold, so an allocation measurement taken right after one reads
the compiler, not the code.
