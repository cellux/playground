# Design decisions

This file records the design decisions that remain in force across implementation phases. Decisions are never deleted: when one changes, the old decision is marked superseded by a new decision, which is added as a separate entry. The diary records when a decision was resolved, but this file records the decision itself.

---

## D1 — The roadmap is vertical slices, not horizontal layers

**Status.** In force (adopted before P1).

**Decision.** `PHASES.md` sequences work as vertical slices. Except for
explicitly infrastructure-only phases (P12–P16), a language feature is
delivered through the full pipeline — descriptor, portable IR, reference
interpreter, JVM, JS, Wasm, and shared conformance vectors — before the next
language phase begins.

**Rationale.** `PLAN.md` fixes the order "syntax and descriptor validation ->
portable IR -> reference interpreter -> JVM -> JavaScript -> Wasm -> shared
conformance tests". Horizontal completion (all language features, then all
backends) would leave the compound-interest backend work unvalidated for too
long and create a large untested surface.

**Consequences.** A phase is not complete until all supported targets pass
differential tests. Backend effort is paid early and re-checked by every later
phase.

---

## D2 — The reference interpreter is the semantic oracle

**Status.** In force (adopted before P1).

**Decision.** `omkamra.dsp.interpreter` defines the executable semantics of the
language. Backends are differentially tested against it. Comparisons are
bitwise-exact wherever target math can be identical; only operations whose
target math libraries cannot be identical (the transcendentals of P3) may use a
narrow, documented tolerance.

**Consequences.** New operations are implemented in the interpreter first, with
their special-value behavior (NaN, infinities, signed zero, wrapping, shift
masking) made explicit there.

---

## D3 — Bitwise operators use Clojure core names and 32-bit semantics

**Status.** In force (adopted at P1).

**Decision.** The bitwise/shift operators added in P1 are named
`bit-and`, `bit-or`, `bit-xor`, `bit-not`, `bit-shift-left`,
`bit-shift-right`, and `unsigned-bit-shift-right`, matching `clojure.core`.
Operands are logical `:int`, interpreted as signed two's-complement 32-bit
values. `bit-shift-right` is an arithmetic shift; `unsigned-bit-shift-right` is
a logical shift. Shift distances are masked modulo 32, matching JVM and Clojure
`bit-shift-*` semantics.

**Rationale.** Using Clojure core names keeps DSP source valid under the future
SCI front end (P13) and matches the existing Clojure-like operator vocabulary
(`+`, `<`, `and`). A single 32-bit interpretation avoids target-specific
integer widths and matches the JVM/JS/Wasm primitive types already used.

**Consequences.** JS emission must force 32-bit results (`| 0`) around bitwise
operations and integer shifts. Out-of-range shift distances are defined rather
than diagnosed.

---

## D4 — Capability-bearing operations are portable builtins, not host calls

**Status.** In force (mechanism introduced at P2, established at P1).

**Decision.** Operations that require a target primitive are represented as
stable portable-IR builtins, not as calls into Clojure, JavaScript, the JVM, or
Wasm. Each builtin declares a typed signature, effects, reference semantics,
per-target lowering, and a structured capability/unsupported-target diagnostic.

**Rationale.** `PLAN.md` forbids backends interpreting arbitrary host values or
adding private semantics. A single registry keeps the JVM, JS, and Wasm
lowerings honest and gives P13's browser compiler one portable contract.

**Consequences.** P1 introduces the signature table that P2/P3 generalize. A
builtin that cannot be lowered on a target fails with a structured diagnostic
rather than silently falling back.

---

## D5 — Wasm stays WAT-only until P12

**Status.** In force.

**Decision.** The Wasm backend continues to emit WAT plus linear-memory ABI
metadata; binary bytes and instance regions are delivered by P12.
`PLAN.md`'s completion criteria treat binary bytes and a stable instance ABI as
required, so P12 is mandatory, not optional.

**Rationale.** WAT is sufficient for the interpreter/JVM acceptance path and the
existing browser testbed (WABT compiles it there), and deferring the binary ABI
keeps P1–P11 focused on semantics.

**Consequences.** Artifacts keep reporting `:bytes nil` until P12 is complete;
no downstream code may depend on compiled bytes before then.

---

## D6 — Phase identifiers are stable and phases are closed when finalized

**Status.** In force.

**Decision.** Once a phase is completed and recorded in `DIARY.md`, its defined
scope and completion criteria are not rewritten. Scope changes are made by
adding a later phase or a new decision.

**Rationale.** This mirrors the append-only policy `PLAN.md` and this file
already require, and keeps the roadmap honest about what was actually
delivered.

---

## D7 — Semantic tests live in one `.cljc` catalog consumed by every testbed

**Status.** In force (established at P1).

**Decision.** Runtime-semantic conformance cases live in
`omkamra.dsp.acceptance` (`.cljc`) and are consumed by both the
interpreter/JVM suite (`omkamra.dsp-test`) and the browser suite
(`omkamra.dsp.testbed`). Process-entry cases carry an input trace, initial
state, a shared `:step` reference, and run on interpreter, JVM, JavaScript, and
WasM. Function-entry cases carry argument vectors and a shared `:reference`
function, and run on interpreter, JVM, and JavaScript. JVM-only tests remain
for compiler-internal concerns (descriptor validation, linking, ABI metadata,
allocation), which are not runtime semantics.

**Rationale.** `PLAN.md` requires the interpreter to be the oracle and every
supported target to be differentially tested. Describing cases once prevents
the JVM and browser suites from drifting, and it already caught two real
defects during P1 (JVM boolean-parameter boxing and the `ushr` signedness
reference).

**Consequences.** A new shared case is automatically exercised by the JVM
suite; the browser suite picks it up from the same catalog. Expectations must
be written in portable `.cljc` and must avoid target-divergent special values
unless the case is explicitly about them.

---

## D8 — Browser conformance is driven through the dev server, not a bespoke runner

**Status.** In force (established at P1).

**Decision.** The browser conformance suite is served by the
`:on-demand-http` system as the `omkamra.dsp.testbed` Shadow page. The page
installs a small JS hook (`window.omkamraDspTestbed.runConformance`) that runs
the complete suite and resolves a structured result. `omkamra.dsp.browser` is
the Clojure-side driver: it ensures `:on-demand-http` is running, opens the page
with `?autorun=0` using spel's Playwright bindings, and evaluates the hook.
The page's own button still renders results for interactive use.

**Rationale.** The dev server already owns Shadow watcher lifecycle and page
serving (`omkamra.dev.on-demand-http`), and `omkamra.dev.frontend` already
exposes build/runtime operations. Reusing them keeps one source of truth for
how the browser testbed starts. The programmatic hook makes the same suite
runnable from the REPL and from scripts, rather than only by clicking.

**Consequences.** Browser conformance requires a browser; the driver prefers the
installed `chromium_headless_shell` under `PLAYWRIGHT_BROWSERS_PATH` because
spel's bundled Playwright may pin a different revision. `?autorun=0` keeps the
programmatic run from racing the page's automatic run.

---

## D9 — `min`/`max` are NaN-propagating and signed-zero-aware

**Status.** In force (decided before P2).

**Decision.** `min` and `max` (and therefore `clamp`, defined as
`min(max(x, lo), hi)`) return NaN if any operand is NaN, and order `-0 < +0`.
So `min(-0, +0)` is `-0`, `max(-0, +0)` is `+0`, and `clamp(NaN, lo, hi)` is
NaN. This is the native behavior of JVM `Math.min`/`Math.max`, JavaScript
`Math.min`/`Math.max`, and Wasm `f32.min`/`f64.min` (verified during P2
planning), so all three lower directly.

**Rationale.** It is the lowest-glue contract and the one every target already
implements. It is also the modern IEEE 754-2019 direction: the old NaN-ignoring
`minNum`/`maxNum` were deprecated in favor of NaN-propagating `minimum`/`maximum`
with separate `minimumNumber`/`maximumNumber` variants. Finally, `PLAN.md`'s
non-finite policy detects and contains NaN at the voice level, which requires
NaN to remain visible rather than being swallowed by `min`/`max`.

**Consequences.** Conformance comparison for these operations must be
signed-zero-aware (`-0` and `+0` are distinct results) while treating any NaN
as equal to any NaN. If NaN-ignoring semantics are ever wanted, they are added
as separate `min-number`/`max-number` operations rather than by changing
`min`/`max`.

---

## D10 — `round` rounds half away from zero, and is derived

**Status.** In force (decided before P2).

**Decision.** `round` rounds ties away from zero: `round(0.5) = 1`,
`round(-0.5) = -1`, `round(2.5) = 3`, `round(-2.5) = -3`. It is not a
per-backend primitive; it expands to `copysign(floor(abs(x) + 0.5), x)`.
Signed zero is preserved (`round(-0.0) = -0.0`) and NaN propagates.

**Rationale.** No target's native `round` agrees with another's: JVM/JS are
half-up-toward-`+inf` (and JVM `Math.round` returns an integer type),
while Wasm `nearest` and JVM `Math.rint` are half-to-even; JS and JVM even
disagree on the sign of `round(-0.5)`. Half-away-from-zero (the C `round` and
Rust `f32::round` convention) is the only tie rule with a single portable
definition and no target quirks, and it is symmetric, which is what DSP
quantization and pitch code expects.

**Consequences.** `round` adds no backend surface. Its correctness follows
from `copysign`, `floor`, and `abs`, so conformance can compare it exactly
(subject to D9's signed-zero-aware comparison).

---

## D11 — P2 has a small primitive set; most named operations are derived

**Status.** In force (decided before P2).

**Decision.** P2 primitives are `abs`, `min`, `max`, `floor`, `ceil`,
`trunc`, `sqrt`, `copysign`, and integer `rem`/`mod`. Derived operations are
`clamp` = `min(max(x, lo), hi)`, `fract` = `x - floor(x)`, `sign`, and
`round` (D10). A primitive is added only where a target instruction is the
natural lowering or the semantics genuinely cannot be expressed in the
language. `trunc` is a primitive even though only JS and Wasm have it
natively; JVM derives it as `copysign(floor(abs(x)), x)`.

**Rationale.** `PLAN.md` says to prefer the DSP language and to keep target
intrinsics a constrained escape hatch. Deriving the composable operations keeps
the portable-IR and backend surface small, makes their semantics follow from
the primitives, and matches the fact that `copysign` (JS) and `trunc` (JVM)
already need derivation on some targets.

**Consequences.** The builtin registry distinguishes primitives from derived
operations. Derived operations may be expanded during IR lowering and are not
required to appear as capability-bearing intrinsics.

---

## D12 — Integer division and remainder are total and wrapping

**Status.** In force (decided before P2).

**Decision.** Integer `/`, `rem`, and `mod` never throw or trap:

- `x / 0 = 0`, `x rem 0 = 0`, `x mod 0 = 0`;
- `INT_MIN / -1` wraps to `INT_MIN`;
- `INT_MIN rem -1 = 0` and `INT_MIN mod -1 = 0`;
- `rem` is truncated (sign of the dividend); `mod` is floored (sign of the
  divisor).

This applies to the existing integer `/` as well, normalizing a pre-existing
four-way divergence (interpreter threw, JVM threw, JavaScript returned `0`,
Wasm trapped; and the interpreter threw on `INT_MIN / -1` while the JVM
wrapped).

**Rationale.** `PLAN.md` requires bounded, predeclared runtime behavior that
never throws, traps, blocks, or allocates through the audio callback, and fixes
`:int` as signed two's-complement with explicit wrapping arithmetic. Returning
`0` is the smallest bounded total extension; wrapping `INT_MIN / -1` follows
two's-complement arithmetic and matches the existing JVM and JavaScript
behavior. The remainder path needs no special case (`INT_MIN rem -1 = 0` is
native on all targets).

**Consequences.** The interpreter must use unchecked integer division with
guards; the JVM and Wasm lowerings add a zero-divisor/normalization guard
around `idiv`/`div_s`; JavaScript is unchanged. A fixed diagnostic counter for
integer division by zero is deferred to the runtime-diagnostics work (P9).

---

## D13 — A capability-aware builtin registry and a generic `:builtin` IR node

**Status.** In force (decided before P2).

**Decision.** P2 introduces a single builtin registry (namespace
`omkamra.dsp.builtins`) as the source of truth for every non-arithmetic
operation. Each entry declares name, operator keyword, arity, parameter types,
return type, effects, and whether it is a primitive or a derived operation
(plus its expansion when derived). The registry feeds linker `language-forms`,
descriptor type inference, and IR lowering so those never drift.

Primitives lower to one generic portable-IR node,
`{:op :builtin :builtin :sqrt :args [...]}`. Each backend owns a
builtin → lowering map whose value may emit a sequence of instructions and may
return a structured unsupported-target diagnostic. Derived operations (D11)
are expanded during IR lowering and never reach a backend.

Derived expansions must not duplicate effectful arguments. Each argument is
bound to a fresh IR local before the expansion is lowered, so an argument that
is a `:call` or `:buffer-load` is evaluated exactly once.

**Rationale.** `PLAN.md` requires every intrinsic to have a typed signature,
an explicit effect and capability contract, and a clear unsupported-target
diagnostic. Centralizing that in one registry removes the seven-file edit
surface per operation and gives P3 a single place to express the mixed
outcomes it will face — JVM and JavaScript can lower transcendentals to
`Math`, but Wasm has no `sin`/`cos`/`exp`/`log`/`pow` and must either emit a
portable sequence or report the operation unsupported. Existing arithmetic,
comparison, and logical nodes keep their current representation; bitwise may
migrate to the registry later.

**Consequences.** Adding an operation becomes a registry entry plus one
lowering per backend. The registry is expected to become `.cljc` when the
browser compiler (P13) is ported. Capability gaps are data, not backend
exception strings.

---

## D14 — Transcendentals: native intrinsic per target, portable emulation as fallback, target-dispatched comparison

**Status.** In force (decided before P3).

**Decision.** P3 compiles each transcendental to whatever the target has:

- a target with a native intrinsic lowers the operation to it — the JVM to
  `java.lang.Math`, JavaScript to `Math`, and the reference interpreter to the
  host `Math`, so the interpreter also counts as native;
- a target without one lowers to a portable **emulation**.

The only target without one today is Wasm, whose core has no transcendental
instructions at all. Its emulation computes in f64 and narrows to the kernel
precision, following the existing `sqrt` precedent.

This is decided per backend, using the multimethods that already exist
(`evaluate-builtin`, `builtin-instructions`, `emit-builtin`); the registry does
not need a capability field for it.

**The reference interpreter remains the single oracle** (D2/D7). What is
specialized per target is the **matcher** that compares a target kernel against
the interpreter's expected value:

- the default matcher is exact, with NaN-equal-as-NaN and signed-zero
  awareness (D9);
- the `:wasm` matcher additionally allows a small relative/absolute deviation,
  because an emulation cannot reproduce the host `Math` bit-for-bit.

So JVM and JavaScript comparisons stay exact — both call the same native op the
interpreter calls — and tolerance is confined to the one target whose result is
an approximation. That tolerance doubles as the emulation's accuracy gate.

**Rationale.** Speed: prefer the native intrinsic wherever one exists and
emulate only where the target has nothing. `PLAN.md` permits a target intrinsic
once the language-level operation exists; Wasm needs the emulation regardless,
so it is written once in the Wasm backend. Keeping the deviation in the matcher
leaves the catalog, the plan format, and the oracle model unchanged.

**Consequences.** New registry entries are ordinary primitives. The interpreter
implements the ops with the host `Math` (`.cljc`); JVM and JavaScript gain
native lowerings; Wasm gains f64-internal emulations. `omkamra.dsp.conformance`
and `omkamra.dsp.testbed` gain a target-dispatched matcher whose only tolerant
specialization is `:wasm`.

---

## D15 — Local arrays are preallocated scratch in one instance arena

**Status.** In force (adopted at P4).

**Decision.** The language gains fixed-size local arrays:

- declaration `(array :float N)` or `(array :int N)` as a `let` binding;
- indexed access `(aget a i)` and `(aset a i v)`, with `array-load`/
  `array-store` retained as equivalent aliases (matching the Clojure-core
  naming convention of D3).

Persistent state and local-array **scratch** share a single instance **arena**:
scratch slots are assigned distinct logical offsets that continue immediately
after state, and every backend realizes the arena in its existing storage — the
flat primitive array (interpreter), the typed arrays (JVM, JavaScript), and one
linear-memory region (Wasm). No new ABI parameters, method-signature changes, or
storage concepts are introduced.

Scratch is **preallocated and zero at instance creation**, and a declaration
emits no run-time statement. Its contents therefore persist between frames
within an instance, so a scratch array must be written before it is read in a
frame. Nothing is allocated after initialization.

Each declaration owns a **distinct** slot; lifetime-based slot aliasing is
deferred. Constant indices are checked while lowering; dynamic indices are
checked at run time by the interpreter and reported by a `:dynamic-array-index`
diagnostic (they are not proven in bounds). Local arrays require the `:process`
entry, exactly like persistent state.

**Rationale.** Real-time safety forbids per-call allocation, so a local array
cannot be a fresh array created inside `process()`; it must be reserved up front.
That makes its "lexical" lifetime a compile-time discipline over instance
storage, which collapses local arrays, aggregate state, and constant tables onto
one layout model. Continuing offsets after state makes `state`, `scratch`, and
the per-backend layout functions shape-identical, so the backends reuse their
existing state load/store paths unchanged.

**Consequences.** The ABI exposes `:scratch` and `:scratch-layout` alongside
`:state`/`:state-layout`, and the Wasm memory plan gains a `:scratch` region.
Because scratch is not reset, `reset` restores state only. P4's later slices
(aggregate records, initialization-time constant tables) are expected to reuse
this same arena layout.

---

## D16 — Records are named struct shapes referenced by `:struct`, laid out struct-of-arrays

**Status.** In force (adopted at P4).

**Decision.** Aggregate state records are declared as named *struct shapes* and
referenced from state declarations:

- `(dsp/defstruct name [f1 f2])` — shorthand: `:float` fields initialised to
  `0.0`;
- `(dsp/defstruct name [{:name f1 :type :float :init 0.0}
  {:name f2 :fields [...]}] {:constant true})` — explicit descriptors,
  possibly nested, plus an optional options map;
- a state slot references the shape with `{:name slot :struct name}`,
  optionally with `:size N` for an array of records. Inline `:fields [...]` is
  also accepted for one-off records.

The linker resolves `:struct` through the declaring namespace's captured
resolution (the same mechanism as calls) and inlines the struct's fields,
retaining the fully-qualified struct id as `:struct-name`. Field access comes in
two equivalent spellings: the explicit `(field slot :a :b)` and
`(set-field! slot :a :b value)`, and the Clojure-style `(:field slot)` (keyword
in head position, like `(:key map)`), `(slot :field)` (the record name in call
position), and `(set! (:field slot) value)`. An optional leading record index
handles arrays of records in the explicit form: `(field voices i :phase)`.
The linker treats a form headed by a record name as field access rather than a
call, so `(slot :field)` is never mistaken for a function call.

**Layout — struct of arrays.** A record's field tree is flattened into scalar
leaves; for a slot of `:size N`, each leaf becomes one flat state entry of
`:size N`, and leaves are laid out in declaration order. Field `j`'s record `i`
lives at logical slot `leafOffset(j) + i`, where `leafOffset(j)` is the sum of
the sizes of the preceding leaves. Nested records contribute their leaves under
path-qualified names. Record slots continue the same arena as scalar state and
scratch (D15), and each leaf is an ordinary `:state-load`/`:state-store` with a
constant base offset and the record index as the element index — so every
backend reuses the existing state paths unchanged.

**Constant tables.** A state slot (or record) may be `:constant true`. It is
materialized into instance state at initialization and is **read-only**; any
`state-store`, `aset`, or `set-field!` targeting it is rejected while lowering.
`:constant` requires an aggregate table or record and forbids `:next`.

**Rationale.** Declaring the shape once outside the DSP body makes records
reusable across definitions and keeps the shape portable data (P13). The
struct-of-arrays layout keeps each leaf contiguous so it maps onto the existing
flat state representation, and continuing the arena means records need no new
storage concept. `:constant` is the natural place for wavetables and windows:
materialize once, never write.

**Consequences.** The ABI gains `:records` (named, with `:struct`, `:size`,
`:fields` paths and offsets, and `:constant`) and `:arena` (region offsets,
element size, alignment) alongside `:state`/`:state-layout` and
`:scratch`/`:scratch-layout`; Wasm's state layout marks `:constant` entries.
Float `:init` values stay double in the IR and are narrowed per precision at
use, so an f64 table survives the EDN round trip to the browser. Structured
`:dynamic-state-index` diagnostics join `:dynamic-array-index`; the reference
interpreter is the development-time bounds checker.
