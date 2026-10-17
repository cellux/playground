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
  also accepted for one-off records;
- a field inside a **scalar** record may declare `:size M`, making it an array
  field, so a record can own its buffer. An array of records cannot yet contain
  an array field (rejected).

The linker resolves `:struct` through the declaring namespace's captured
resolution (the same mechanism as calls) and inlines the struct's fields,
retaining the fully-qualified struct id as `:struct-name`. Field access comes in
two equivalent spellings: the explicit `(field slot :a :b)` and
`(set-field! slot :a :b value)`, and the Clojure-style `(:field slot)` (keyword
in head position, like `(:key map)`), `(slot :field)` (the record name in call
position), and `(set! (:field slot) value)`. Selectors are
`[record-index?] field-name+ [element-index?]`: the leading index selects a
record in an array of records (`(field voices i :phase)`) and the trailing one
selects an element of an array field (`(field slot :buf k)`). A record that owns
exactly one array field is indexable directly — `(aget slot k)` / `(aset slot k v)`
— since `aget`/`aset` are the array accessors; this is the terse spelling of
`(field slot :buf k)`. The linker treats a form headed by a record name as field
access rather than a call, so `(slot :field)` is never mistaken for a function
call.

**Layout — struct of arrays.** A record's field tree is flattened into scalar
leaves; for a slot of `:size N`, each leaf becomes one flat state entry of
`:size N`, and leaves are laid out in declaration order. Field `j`'s record `i`
lives at logical slot `leafOffset(j) + i`, where `leafOffset(j)` is the sum of
the sizes of the preceding leaves. Nested records contribute their leaves under
path-qualified names. In a scalar record an array field occupies its own
contiguous block of `M` slots. Record slots continue the same arena as scalar
state and scratch (D15), and each leaf is an ordinary
`:state-load`/`:state-store` with a constant base offset and a single element
index — so every backend reuses the existing state paths unchanged.

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

---

## D17 — Circular-buffer delay lines use write-then-read ordering

**Status.** In force (adopted at P5).

**Decision.** A delay line is written in the DSP language on top of the P4
arena, with one documented convention:

- **State.** A persistent `:float` array of length `N` holds the samples, and an
  `:int` cursor names this frame's write position. The library holds both in a
  **record that owns its buffer** — `{:name line :fields [{:name buffer :type
  :float :size 64} {:name cursor :type :int :init 0}]}` — read and written with
  `(aget line k)`/`(aset line k v)` and `(:cursor line)`. A top-level state
  array with `state-load`/`state-store` is equally valid; either way this is
  persistent state, not local scratch (`array`).

- **Ordering — write, then read.** Within a frame the new sample is stored at
  `cursor` *first*, every tap is then read relative to `cursor`, and the cursor
  advances last. Consequences:

  - a delay of `0` returns the current input (the sample just written);
  - a delay of `d` samples returns the value written `d` frames ago;
  - all taps read in one frame see a consistent buffer, and a tap feeding back
    into the line is deterministic;
  - a delay at or beyond `N` aliases around the buffer; a negative delay is
    undefined.

  The wrap uses the floored `mod` builtin, so `(mod (- cursor d) N)` is always a
  valid index for `d >= 0`. The cursor advance is either a state `:next`
  transition or a body write to a record field (`(set! (:cursor line) ...)`).

- **Interpolation.** Reads use `linear-interpolate` between the two neighbouring
  taps, or a first-order allpass recursion `y = a*(x0 - previous) + x1` with
  `a = (1-frac)/(1+frac)`. The allpass `previous` output is feedback state: it
  lives in the same record as the buffer, because a record field can be written
  from the body whereas scalar state is `:next`-only and `:next` cannot see body
  locals.

**Rationale.** Every synth effect needs the same memory-access pattern, so one
convention keeps multi-tap delays and feedback readable and deterministic, and
makes the update order visible in source and IR (a stated P5 goal). Write-then-
read is chosen so zero delay is the identity and taps are frame-consistent.

**Consequences.** `omkamra.dsp.library` gains `linear-interpolate`,
`allpass-coefficient`, `delay-line`, and `allpass-delay-line`; the acceptance
catalog gains zero-, unit-, fractional-, and wrap-delay vectors exercised on the
interpreter, JVM, JavaScript, and Wasm. A delay-line-like module that *owns* its
state is a library **process**, since the call mechanism still rejects stateful
callees; encapsulating such state behind reusable functions is P6's work.

---

## D18 — Named array storage has static places

**Status.** In force (adopted at P5).

**Decision.** Array storage may be named as a **place** and indexed with
`aget`/`aset`. A place is a *compile-time* reference — region (`:state` or
scratch), base offset, element type, and length — **not a runtime value**. A
place arises from:

- a local array name (`(array :float N)`);
- a top-level state array name;
- a record's single array field, named by the record itself (`(aget rec k)`);
- an array-field accessor with no element index: `(:field rec)` or
  `(field rec :field)` — the place;
- a `let` alias of any of the above (`(let [buf (:line rec)] (aget buf k))`).

`(aget place k)` / `(aset place k v)` index a place; `(field rec :field k)` /
`(set-field! rec :field k v)` is the explicit spelling. A place used where a
scalar is expected is a diagnostic ("array places must be indexed (aget/aset) or
bound in a let"), so a bare array name can no longer be silently misused.

**Rationale.** It makes buffer access read like the storage it is and keeps a
buffer-owning record cohesive — `(aget (:line rec) k)` rather than a form that
must name both the field and the index. It costs nothing at runtime: a place
lowers to `:state-load`/`:state-store` (or `:array-load`/`:array-store` for local
scratch) with a constant base offset, so **no backend changed**. Local arrays,
state arrays, and record fields all become the same indexed-storage concept.

**Consequences.** Array indices are checked while lowering when constant and at
run time by the interpreter otherwise, and the constant-table read-only check
follows places through `let` aliases. This is a strict subset of first-class
arrays: places are static, so they cannot be passed to or returned from
functions and cannot have a runtime base (e.g. selecting a record in an array of
records). Those remain P6's "buffer-accessing callees".

---

## D19 — Stateful module composition by ownership-partitioned calls

**Status.** In force (adopted at P6).

**Decision.** A **module** is any DSP definition with state, arrays, or records;
there is no separate module kind. Used as the entry it has I/O ports and its
`:next` runs once per frame; **called** it takes params and its `:next` runs once
per call. Composition is defined by four rules:

1. **Ownership is per call site.** Each syntactic call site owns one private
   copy of the callee's persistent state, local-array scratch, and records. Two
   sites never share; one site reused in a loop reuses one copy.
2. **A module's transition is immediate.** The callee applies its `:next` at the
   end of its body, so consecutive calls in a frame observe each other. This is
   ordinary mutable-module semantics and is identical in the interpreter.
3. **Restrictions preserved.** No recursion; modules are not first-class values;
   no runtime state base (selecting a module in an array of modules is P10
   voices). A called module never accesses the **caller's** I/O buffers.
4. **Ports.** More inputs are more params. More outputs are a **record return**:
   `:return-type` names a `defstruct`, the body produces a map mirroring the
   struct's field tree (a `let`/`do` around it is an ordinary form), and each
   call site binds the result in a `let`. It is erased at lowering — one local
   per leaf — so `(:field r)` is a local read and no backend changed. A record
   value cannot be produced by an `if` and cannot be written.

**Mechanism.** Lowering allocates a state partition per call site. A definition's
partition size is its own expanded state plus the partition of each *static* call
site in its body, so it is a compile-time constant (recursion is forbidden). The
callee IR keeps 0-based state indices; each `:call` node carries its partition
base relative to the enclosing module. The **interpreter executes `:call` nodes
against that partition** — a genuinely independent oracle, preserving D2 — while
the three source backends share one **statement-level inliner** that renames
locals and rebases state ids. The interleaving is deliberate: an inlining bug
cannot hide behind the reference semantics.

---

## D20 — One arena of named partitions; reuse is dead-on-entry

**Status.** In force (adopted at P6); the coloring pass that acts on it is
follow-up work rather than a P6 deliverable — all five P6 deliverables are met
without it. It is most valuable once P10 gives many sibling modules.

**Decision.** Every instance has one preallocated arena of **named partitions
with explicit offsets** (persistent state and per-frame scratch), and the same
`:arena` offsets are handed to every backend. A partition may be reused by
another only when it is **dead on frame entry**: on every path from frame start
it is written before it is read, so its value at a frame boundary can never
matter. Persistent state that is live across the frame boundary is **never**
reused across nodes — a semantic constraint, not an implementation limit.

**Rationale.** A per-sample inlining compiler has no intermediate audio buffers
between nodes, so the only reusable storage is per-frame transient state. The
reuse question is therefore exactly "dead on frame entry", which ordinary
liveness analysis over the frame body plus a write-before-read check at frame
boundaries can decide. Keeping partitions explicit and offset-based means
coloring is a pure offset rewrite: no backend change, no semantic change.
Explicit aliasing is available as the predictable alternative for a shared
workspace.

**Consequences.** P6a lays the arena out as partitions and records the
criterion so the coloring pass drops in later; it also colors the P4 local-array
scratch, so one pass pays off in both places.

---

## D21 — Composition is inlined under a statement budget

**Status.** In force (adopted at P6c).

**Decision.** A call to a stateful module is **inlined**, not emitted as a call.
The inliner carries a fixed statement budget (`inlining-statement-limit`, 20 000
statements added per artifact); exceeding it is a compile error naming the
expansion, not a silently larger artifact. There is deliberately **no run-time
call fallback**: a call frame would allocate, and allocation through the audio
callback is forbidden.

**Rationale.** Inlining is what makes composition target-free — one portable
pass serves the JVM, JavaScript, and Wasm. Growth is bounded because recursion
is rejected at link time and every call site is static, so an expansion is a
finite tree; the budget only guards against a pathological fan-out.

**Consequences.** More call sites to one module means more code, not a shared
function. When that becomes a real cost (many voices), the answer is the one the
rest of the system already uses: explicit compile-time configuration, not a
run-time mechanism.

---

## D22 — State updates follow source order

**Status.** In force (adopted at P6c).

**Decision.** Three rules, by state kind:

1. **Entry scalar state.** All `:next` expressions are evaluated against the
   frame-start values and then stored at the end of the frame, so updates within
   a frame are simultaneous. `:next` cannot see body locals (D17).
2. **Called module state.** A module applies its own transitions at the end of
   its body, i.e. *at the call*, so a later call in the same frame sees the
   update (D19).
3. **Aggregate state (records, arrays).** Updates are explicit, ordered
   `set!`/`set-field!`/`aset` statements, so feedback and delay boundaries are
   visible in the source and in the IR. A module that wants a one-frame delay
   writes before it reads (the delay-line convention, D17).

**Rationale.** Ordering the source does not state cannot be relied on. These
rules make every transition's position determinate and identical in the
interpreter and every backend.

**Consequences.** A zero-delay feedback loop is not expressible without an
explicit state slot — the intended discipline, since such a loop has no
well-defined sample-serial evaluation order.

---

## D23 — A process input port is optional

**Status.** Decided; implemented in P8d. See `DIARY.md` for the record. One
consequence is amended there: "no input argument in any adapter" holds for every
host-facing adapter (the conformance context map, the JS `process` export, the
Wasm `$process` export) and for the derived ABI argument indices, but the JVM's
generated kernel method keeps its input array parameter and receives `nil` from a
generator's adapter. Dropping it there would require `DspSourceKernel`-style
interfaces ×4 (f32/f64 × mono/multi-channel) and duplicate the kernel emission and
every adapter, because the array never reaches the ABI, the effects, or the frame
body. The amendment is recorded rather than quietly skipped.

**Decision.** A process may declare `:input false`. It then has no sample
parameter, no `:input` buffer in `:buffers`/`:bindings`/`:memory`, and no
input argument in any adapter; its `:controls` default to *all* its parameters.
Absent or `nil` `:input` keeps today's meaning (the first parameter is the input
port).

**Rationale.** Generators have nothing to read, but the process ABI currently
makes the first parameter the input port by construction, so every oscillator and
noise source must name an ignored `sample`. That is not merely awkward: the
ignored parameter still emits a real `:buffer-load`, so `:effects` claims
`:read-memory` for a module that reads nothing, and that metadata is a real-time
policy signal. It also forces the input to be the first parameter, which fights
typed parameters (P7d).

**Consequences.** The ABI's argument indices must be derived from the buffer list
rather than hardcoded, and all four targets plus the conformance plan and the
browser testbed move together — which is exactly why it is bundled with P8's
instance-handle change instead of done separately.

---

## D24 — The sequencing layer is host-side, and segmentation drives the kernel

**Status.** Implemented in P9 as `omkamra.dsp.event` and `omkamra.dsp.schedule`.

**Decision.** Events, the monotonic timeline, the ingress queue, its overflow
policy, and event segmentation all live in a portable host-side layer
(`.cljc`). The compiled kernel knows nothing about events, absolute time, or
host objects; what crosses into it is a control value applied at a
block-relative frame offset. A segment boundary is therefore just a point where
the host sets controls and calls the process again, and **no backend changes to
support sample-accurate automation**.

**Rationale.** The PLAN already drew this line ("the sequencing layer owns
timeline scheduling, preallocated event and queue memory, and delivers canonical
timestamped messages to the kernel; it does not require the kernel to know about
GUI or host-language objects"). Following it means the automation layer is
written once, in code that runs identically on the JVM and in the browser, and
that partition invariance follows from the language's per-frame updates instead
of being re-established per backend.

**Consequences.** Two clocks: `:frame` is an absolute 64-bit timeline position
(32 bits is only ~12 hours at 48 kHz), while a kernel offset is small and
32-bit. A kernel renders into the start of its buffer, so a segment's frames are
copied through a preallocated scratch; a future ABI with a block offset would
remove both copies. `:sequence` orders equal frames, and rendering must be
identical under any partitioning — proven on all four targets, not assumed.

## D25 — Overflow drops the oldest *future* event, or rejects the new one

**Status.** Implemented in P9.

**Decision.** When the ingress queue is full, the overflow policy evicts the
oldest event that is still strictly in the future. An event that is already due
or being dispatched is never evicted. If every queued event is due — so nothing
is droppable — the incoming event is **rejected** instead, and the rejection is
counted separately from the eviction.

**Rationale.** Dropping the oldest *anything* would let a burst cause the audio
thread to lose an event it already owes the kernel, which turns an overflow into
a wrong note at the wrong time rather than a missing one. Counting evictions and
rejections separately keeps those two very different failures distinguishable
from the outside.

**Consequences.** The producer must supply the current timeline position to
`push-event!`, which keeps the queue free of a third shared field. All counters
are fixed slots in a preallocated array, read in one snapshot (`queue-stats`),
and the policy never logs, allocates, blocks, or throws.

## D26 — Primitive-array access must be type-hinted

**Status.** Learned and enforced in P9.

**Decision.** Every `aget`/`aset` on a primitive array in a path that can run
per frame, per block, or per event carries a type hint (`^longs`, `^objects`) or
an explicit `(long ...)` coercion, and `(set! *warn-on-reflection* true)` is the
tool that verifies it.

**Rationale.** Without a resolvable overload, Clojure falls back to `Reflector`
and a single `aset` into a `long[]` allocated about **24 KB**. In P9 this showed
up as 74 KB per `begin-block!` call (three array writes) and 22 KB per queue
push: the scheduling path allocated 6.5 MB per block. Adding hints took it to 48
bytes per block and a push/pop cycle to zero.

**Consequences.** An allocation test that looks unreasonably strict is not
noise — P8's caught a per-call vector and P9's caught this. Both were real, and
both were invisible to correctness tests.

---

## D27 — The kernel carries smoothing and slew; curves come from the host

**Status.** The kernel half is implemented (P9c); the host half is the next slice.

**Decision.** A control may declare its own per-frame carrier: `:smooth <one-pole
coefficient>` or `:slew <maximum step per frame>`, both in the control's own
units and both applied by the compiled frame loop. Setting such a control is a
*target*, not a jump, and the ABI says so. Automation that is not one of those
two shapes — arbitrary curves, envelopes, sequenced lanes — is supplied by the
host as block-rate control buffers, not computed by the kernel.

**Rationale.** Segment boundaries alone cannot animate a control *within* a
segment, so something has to; and the two mechanisms split cleanly along a line
of cost and generality. A per-frame one-pole is three arithmetic operations that
desugar into state and arithmetic the language already has, so it needs **no new
IR node, no ABI change and no backend change** — the input sample's binding to a
`:buffer-load` expression was already the proof that a control can be bound to
an arbitrary expression. Arbitrary curves, by contrast, cannot be expressed by
segmentation at all, and computing them in the kernel would mean inventing curve
types the DSP language does not have.

**Consequences.** Both amounts are per *frame*; a per-block amount would make
rendering depend on the host's block size and break P9's invariance requirement,
which is why the mode is lowered into the frame body rather than applied at
segment boundaries. A control's smoothed value lives in one arena slot and its
update is one frame of latency, consistent with D22's simultaneous entry scalars.
Linear/critical ramps to a target over a known duration remain unimplemented:
they need either a per-frame division plus a counter, or a block-rate *prologue*,
and the IR has no prologue concept today (the backends do — control slots are
loaded per call — but the IR does not describe one). Per D27's split, a host that
wants a linear ramp can supply it as a control buffer instead, which is the
cheaper path to the same result. Control buffers themselves are not implemented
yet; they need a buffer region in the ABI and its Wasm memory layout.

---

## D28 — Lanes are flat in the kernel and named in the ABI

**Status.** Implemented on all four targets (P9d): the descriptor, the IR, the ABI,
the interpreter, the JVM, JavaScript and Wasm — with an `:automation-lane` case in
the shared catalog, so the browser matrix verifies JavaScript and Wasm against the
portable-IR oracle alongside the JVM suite.

**Decision.** A control may declare `:lane true`, and its value is then supplied
by the host one float *per frame* — the host's mechanism for arbitrary automation
curves. All of a process's lanes live in **one** flat `:lanes` region with a
compile-time stride (`:frame-capacity`, default 1024 frames), so lane *i*
occupies `[i * stride, …)` and a lane read is an ordinary `:buffer-load` with a
`:frame-offset` index. The ABI names each lane individually with its buffer and
offset, and `process!` takes `:lanes {:amount …}`; the host never sees the flat
array unless it wants to write memory directly.

**Rationale.** The flat layout buys the whole feature with **no new IR op and no
new addressing concept**: `:buffer-load` already resolves by `:buffer-id` in all
four backends, and `:frame-offset` already exists in all four, so a lane read is
*the same shape as the input sample read*. The stride has to be compile-time for
those offsets to be constants, which costs nothing because the Wasm layout
already fixes a frame capacity (`:frame-capacity`, derived from its 4096-byte
channel stride) and already imposes exactly this "no block longer than capacity"
constraint on the input and output regions. Per-lane parameters would make
addressing trivial but give the kernel a variable arity, i.e. a per-shape
interface — the problem that made separate `DspSourceKernel` interfaces
unacceptable in P8.

**Consequences.** The lane region is a fixed allocation sized by the *maximum*
frame capacity, so N lanes cost `N * 4096` bytes of every instance whether used
or not. Because the stride is a *byte* count, a lane holds 1024 frames in f32 and
only 512 in f64, and the ABI's `:lane-stride` reflects that — a lane offset is a
compile-time *element* index, and the element width is what differs.

**Two things the implementation taught.** First, channel-ness had to become a
per-buffer fact rather than a per-process one. A lane region is flat even in a
multi-channel process, and a one-channel buffer in a multi-channel process is a
*broadcast* buffer: one lane driving both channels. Attaching `:per-channel?` to
each buffer *access* in the IR covers all four backends with one question, and it
uncovered a genuine latent bug — a multi-channel process that *declared* its
`:process` had its audio ports normalized at descriptor time, before a channel
count was known, so it was never subscripted. The audio ports now take the
compile-time channel count by definition. Second, a lane is read with the same
`:channel 0`, so Wasm's existing `channel * stride` term reduces to zero and the
lane needs no new addressing at all. Lanes are excluded from the scalar `:controls` list, since they are
not block-rate values at all — so lane arity never affects `:controls` arity
checks. `:smooth` and `:slew` compose with a lane for free: a smoother's target
is simply the control read, which is now a per-frame read. Specifying a lane for
a target that cannot address the region is a compile error rather than a lane
that silently reads zeros.

---

## D29 — A target declares what it serves, and refuses what it cannot

**Status.** Implemented (P15a, deliverable 1's capability half pulled forward).

**Decision.** `omkamra.dsp.capability` is the single place that records what each
target can serve, and `dsp/compile` rejects a request a target cannot honour
*before* lowering, with a structured error that names what the target does serve
instead. For validated options the default is derived from the target, so an
unsupported effective value can only have been asked for explicitly.

**Rationale.** Three capability differences exist, and two of them were silent:
`:checks :development` is implemented only by the interpreter, and `:wasm-math
:table` is f32-only. A silent substitution is worse than a refusal — a caller who
asks for validation a target does not implement, or for a strategy it will not
use, has no way to find out. The third (`:function` on Wasm) already failed, but
as a generic unsupported-target error rather than as "Wasm serves process
entries".

The **operator set is deliberately absent**. The original note assumed Wasm's
missing `sin`/`cos`/`exp`/`log` intrinsics were the gap to declare; P3 closed
that with emulations, so every builtin is now served on all four targets. A
per-operator table whose entries would all say `true` is untestable code, so it
is omitted and the backends' `:default` methods remain the backstop. Adding it
later is a one-line change.

**Consequences.** The `:checks` default must be per-target. With a single global
default every generated-target compile would carry `:development` in its
effective options while honouring nothing, and an unsupported value could not be
told apart from the default — the check would fire on every build. The `:entry`
default is deliberately **not** target-derived: the same definition must not
quietly compile to a function on JS and a process on Wasm, so a defaulted entry
that a target cannot serve is refused and says which entry it serves.

This is strict refusal rather than a warning. The intent is to relax individual
capabilities to warnings later, with evidence of what a refusal cost — rather
than to start permissive and never tighten.
