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

**Status.** Superseded by D73: WAT plus a named, version-pinned assembler is the
Wasm artifact's form *by design*, not a deferral, and the assembly step lives in the
browser's world rather than in this compiler's.

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

**M4 — who owns the region, and how it reaches a voice (M4a/M4b/M4c).** The
kernel owns the region and the protocol grows one method to fill it; the pool owns
the name, so a typo is refused by the component that has the ABI. Where the
region *lives* is the part M4c revised: **one region per voice**, not one per
kernel. A lane's delivery scope is its **control's storage scope** — a `:shared`
parameter's lane is one curve every voice reads, a `:per-voice` parameter's lane
is a curve per voice, a runtime control (`frequency`, `detune`, …) is resolved per
voice so its lane is per voice, and `:bend` is per channel and has no per-lane
delivery yet. A lane-only (`:lane`) control is not in the control plan at all
(it leaves the scalar list), so its scope comes from its own parameter
declaration, and an undeclared one is per voice — the same inference an unbound
ABI control gets.

With one region per voice, `pool/set-lane!` writes a shared curve into *every*
voice's region, including voices that are not sounding yet, so the "write once,
read by whoever starts later" property survives; `pool/set-voice-lane!` writes one
voice's curve. The cost is `N * lanes * 4096` bytes of per-voice region instead of
one shared allocation, and `N` copies of a shared curve per write instead of one
— both small next to a voice's own state, and the price of per-voice lanes not
needing a second region in the ABI. Chunked/fused lowering (D45, PHASES M6)
shrinks this the same way it shrinks everything else: fewer instances, same
delivery.

**The per-target ABI for a lane region.** The JVM, interpreter and JavaScript
hosts hand the region over by reference (a `float[]`/`double[]`, a JS array); the
**Wasm** process takes it as a trailing positional **pointer** into its own linear
memory, which is why a Wasm lane was already per voice the moment the host stopped
sharing one region — the host writes the values into the instance's region and
passes its address. (That pointer was never passed until M4c, so Wasm lane
delivery through the *pool* was broken in M4b while the *direct* process path,
which does pass it, worked — and the M4b browser check could not see the
difference. See the harness note under D47's testing entry.) A post-mix *bus* that
declares a lane is still refused at construction, on every host, until it gets the
same delivery.

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

---

## D30 — A synth is one serialisable description, and the voice pool is host-side

**Status.** Decided and implemented in P10a (`omkamra.dsp.synth`,
`omkamra.dsp.synth.pool`, `omkamra.dsp.synth.host`).

**Decision.** A synth is described once, as data, and that one description is the
source of both halves of a running instrument: the **kernel artifact** (the
voice's DSP code, compiled per target) and the **runtime configuration**
(capacities, policies, tuning, and the note-to-control plan). The runtime
configuration contains only keywords, numbers, strings, and nil, so it can be
written to a file or sent to a browser and rebuilt there with no compiler in
reach. The voice pool lives on the host — the JVM, or ClojureScript in a browser
AudioWorklet — and a pool of N voices is N independent instances of one compiled
process artifact. No part of the sequencing layer is compiled into a kernel, and
P10a adds no language, IR, ABI, or backend surface at all.

**Rationale.** `PLAN.md` already requires this split: "the sequencing layer owns
timeline scheduling, preallocated event and voice storage, voice allocation and
stealing, modulation routing, and delivery of timestamped messages to the
kernel", and "the sequencing layer is not compiled into the Wasm module". A voice
is therefore an ordinary generator process (D23) whose controls are driven from
note events, and per-voice state is the artifact's own instance state (P8). A
voice pool written as host code can also be checked against the reference
interpreter hosting the *same* pool — which is how P10a's mixing and lifecycle
are verified rather than asserted.

**Consequences.** The description is validated in one place, which is what lets
the pool's audio-path operations be *total*: `note-on!`, `note-off!`, `render!`
and `all-notes-off!` never throw, allocate, lock, log, or consult a map for
a policy. A control the description does not map is refused rather than defaulted
to zero, because a silent zero is the worst failure an automation path can have.
Mode support is closed: `:mono`, `:legato`, `:retrigger`, and `:unison` are
*absent* rather than accepted-and-ignored, since a mode that silently behaved
like `:poly` would be worse than one that is refused.

## D31 — The kernel seam is a protocol over voices, and its residual allocation is bounded and documented

**Status.** Decided and implemented in P10a; measured.

**Decision.** The pool reaches a target through one small protocol
(`omkamra.dsp.synth.pool/Kernel`): create, reset, destroy, publish controls, clear
the output, and mix one voice. Clojure cannot implement a primitive-hinted
protocol method — `reify` and `deftype` both refuse with "Can't find matching
method ... leave off hints for auto match" — so the `frames` and `sample-rate`
arguments of that seam **box**. The residual is accepted, bounded, and asserted
rather than hidden: the tests hold a whole note-on/render/note-off/render cycle
under 2 KB, with a negative control showing the allocating reference path (the
interpreter host) at four orders of magnitude above the bound.

**Rationale.** The alternative is to drop the protocol and hand each adapter the
pool's own arrays, so that one call per *block* replaces one call per *voice*.
That moves voice selection into every host and couples each adapter to the pool's
layout. The seam is what makes one pool drive a JVM artifact, the interpreter, and
(later) a browser kernel object; a few hundred bytes per block is a bounded,
block-size-independent cost, while the coupling would be permanent.

**Consequences.** A target that must be exactly zero-allocation needs either a
generated interface (as the JVM kernel itself uses: `DspProcess` has primitive
parameters and is implemented by emitted bytecode) or the whole-block call. Both
remain open. The bound in the test is deliberately far below the ~24 KB of a
single reflective call (D26), so the class of mistake it guards against cannot
hide underneath it.

## D32 — Stealing resets the voice, and a reset is a snapshot copy

**Status.** Decided and implemented in P10a; the reset change is in
`omkamra.dsp.targets.jvm`.

**Decision.** A stolen voice is reset, so it is indistinguishable from a fresh
one and a sequence's output does not depend on which voice happened to be taken.
A reset copies a snapshot of the initial state captured at instance creation,
rather than re-deriving the initial values from the IR.

**Rationale.** Determinism is what the phase requires of stealing, and "the same
state as a fresh voice" is a property a test can hold. The re-derivation was also
a real defect: `reset-state!` walks a `doseq` over lazy seqs and cost this JVM
about **50 KB per call**, which no lifecycle operation in an audio path can
afford — and a voice steal *is* a reset. A snapshot copy is both allocation-free
and faster.

**Consequences.** `reset` no longer reads the dynamic `*precision*`, so the
instance factory captures `f64?` as a value while it is still bound. State slots
come first in the arena, so the prefix of each physical array is exactly what
`reset-state!` wrote; scratch is left alone, as it was there too.

## D33 — A voice mode is an allocation policy over one pool, and monophonic modes keep a bounded note stack

**Status.** Decided and implemented in P10b-i (`omkamra.dsp.synth`,
`omkamra.dsp.synth.pool`, `omkamra.dsp.tests.synth`). **Revised by D36**: `:unison`
is a voice transform, not a pool mode, and the pool's unison **has been removed**
(the mode set is now `{poly, mono, legato, retrigger}`).

**Decision.** The declared `:mode` is a policy the *pool* applies to the same
compiled voice, not a different kind of voice. Five modes exist, each with
observable behaviour that the others do not have:

| mode | voices | a note-on | a note-off |
|---|---|---|---|
| `:poly` | `:capacity` | lowest free voice, else stolen | every voice on that note releases |
| `:mono` | 1 | always retriggers the voice | releases only if the sounding note was released |
| `:legato` | 1 | an *overlapping* note takes over the pitch without retriggering | the sounding note falls back to the previously held one |
| `:retrigger` | 1 | every note-on retriggers, overlapping or not | falls back like `:legato` |
| `:unison` | `:capacity` | every voice sounds the note, detuned | every voice on that note releases |

`:mono`, `:legato`, and `:retrigger` require `:capacity` 1. `:legato` and
`:retrigger` keep a **held-note stack** so a release can fall back to the note
that was pressed before it; the stack is bounded by the declared `:held-notes`
(default 8, at most 64) and a full stack forgets its *oldest* note rather than
refusing the new one. `:unison` detunes voice `i` of `n` by
`unison-detune * (2i - (n-1)) / (n-1)` cents, symmetrically about the note's own
frequency, so the centre of the pool stays in tune.

**Rationale.** Every mode is host-side: it only chooses a voice and writes the
controls the pool already writes, so no language, IR, ABI, or backend surface
changes — the same reason P10a needed none. The five behaviours are pairwise
distinguishable and each is asserted, which is what D30 asked of a mode: a mode
that silently behaved like `:poly` would be worse than one that is refused.
Keeping the note stack in primitive arrays preserves the pool's promise that
`note-on!` and `note-off!` never throw, allocate, lock, or log, and the
drop-oldest overflow policy matches the ingress queue (D25) rather than growing
without bound.

**Consequences.** Retriggering is a reset (`reset-voice!`, D32), so a retrigger
restarts the oscillator phase and the envelope from zero; a true click-free
*attack from the current level* would need a trigger input the kernel does not
have and is not claimed. `:unison` spreads pitch only: a per-voice **pan** offset
needs the stereo output that is P10d's, so `:channels` is still fixed at 1.
Monophonic modes do not keep release tails, because a released voice is not freed
until P10c gives the kernel a way to report that its envelope has finished; a
pool of capacity 1 therefore retriggers the releasing voice, which is the
documented behaviour rather than a defect.

## D34 — Modulation routing is demonstrated in an example, not built into core

**Status.** Decided and implemented in P10b-ii. The only core change is
`omkamra.dsp.synth.pool/set-voice-controls!`; the wiring lives in
`omkamra.dsp.examples.demo-synth`.

**Decision.** `PLAN.md` lists "modulation routing" and "modulation routes" — as a
sequencing-layer responsibility and as a capacity — but that names a *goal*, not
an interface. Core therefore keeps **no** modulation schema: `omkamra.dsp.synth`
does not learn about routes, depths, sources, or a matrix. Instead:

* the pool gains exactly one unopinionated affordance,
  `set-voice-controls!`, which publishes a `double` array in ABI order to a voice
  — a *delivery channel*, not a routing model;
* one possible wiring is implemented end to end in
  `omkamra.dsp.examples.demo-synth`, as an example instrument; and
* the shape is left open until the reference synthesizer (P11) characterizes what
  it actually needs.

**Rationale.** The interface that must be frozen early is the kernel's **input
surface** — controls, `:lanes`, declared `:smooth`/`:slew`, the process call —
because it is cross-target and real-time. Routing sits entirely above it and is
swappable, so freezing it early would only be a guess. There are several live
alternatives, and they are not equivalent: a declarative route table (what this
example demonstrates), a per-target callback, routing expressed in DSP source,
or a first-class modulation signal in the language. Baking the first into the
core would presume additive `base + depth × source` routing and a host-side
matrix before any consumer has asked for them. An example is the proving ground:
a pattern that recurs across instruments can be promoted to core by a decision of
its own, and one that does not stays an example.

**Consequences.** `omkamra.dsp.examples.demo-synth` is the growing instrument the
remaining phases build on: P10c's tails and containment, P10d's stereo spreading
and browser host, and the P11 reference synthesizer all extend it rather than
starting over. Its routing deliberately re-derives the pool plan's control values
itself — that is what a routing layer *is* — and it tracks note/velocity/gate by
the index `note-on!` returns, which is why the render loop allocates nothing.
`set-voice-controls!` bypasses the pool's plan for a voice; the plan remains the
default a host may compose with or ignore. This decision is revisited when the
example is inspected for patterns worth promoting, and when P11 lands.

## D35 — Pitch is a pitch-domain standard source, converted in DSP

**Status.** Direction decided; prototyped in `omkamra.dsp.examples.demo-synth`.
Not yet promoted to core's `control-sources` or the library.

**Decision.** The standard performance vocabulary is stated in the **pitch
domain**, and the conversion to Hz is **DSP code**:

- the voice consumes `note` (MIDI number), `bend` (cents), and `detune` (cents)
  as ordinary controls;
- a DSP function — now the library default
  `omkamra.dsp.library.pitch/pitch->frequency` (over `cents->ratio`) — turns them
  into Hz, per sample, inlined into the frame loop;
- the runtime (and the voice manager) stays **pitch-agnostic**: it delivers the
  standard source values and never computes a frequency; and
- pitch bend is therefore **not** a modulation route and not host math. The host
  only scales the player's bend (-1..1) by the declared instrument range, and the
  kernel does the rest.

**Rationale.** SuperCollider computes `freq` host-side, which works because its
engine is generic; Web Audio splits it (`frequency` + `detune` cents, engine-side
conversion). This project compiles a typed, cross-target-verified kernel, which
tilts to the Web Audio shape: `2^(cents/1200)` is sample-rate-dependent math that
belongs in the verified unit and must be smooth and per-sample, and unison
detune, glide, and vibrato are all naturally expressed there. It also removes the
accidental half-wiring this project had: pitch bend was in the event ABI but not
in the control binding, so it fell between host and kernel and had to be
reinvented as a route.

**Staging, so nothing is foreclosed.** (a) **explicit** — the voice calls the
conversion, the runtime delivers the raw sources; this is what the example does
now, and it needs no compiler change. (b) **declarative** — the description names
the conversion and a transformer bakes it into the voice, so defaults are
requested with one option and any piece is overridable. (b) is mostly sugar over
(a), with one caveat: it also derives the voice's **ABI and configuration** (the
control surface becomes standard-sources + parameters), so it is a source-to-source
transform plus a config derivation, not pure syntax.

A **transformer** is a plain Clojure function — not a macro layer or a new DSL —
that takes a `dsp/defn` value (its descriptor) and returns a new one with the
named conversions baked in. That is what makes the declarative form ergonomic:
one option requests the whole default set, and any single conversion is replaced
by passing a different function. It composes with the existing `dsp/defn`/linker
machinery and is a P11 candidate.

**Promoted into core.** The standard vocabulary is now implemented:
`:control-sources` includes `:bend` and `:detune`; the description declares
`:bend-range` (cents, default 200); the pool keeps a **global source bank**
(`set-pitch-bend!` per channel) plus a per-voice detune, and resolves `:bend` to
`range × bend` and `:detune` to the voice's offset. The offset is set by
`set-detune!` — the per-voice fine-tune counterpart of `:bend`; it is no longer
unison's spread, because unison became a transform (D36). The **read half**
`read-controls!` was added so a host can read the plan's resolved values, change
what it means to, and publish them back — which is what let the example drop its
own plan re-implementation and become a thin modulation layer.

**Consequences.** `:frequency` remains the host-computed legacy source (note to
Hz, no bend) and is simply unused by the example; a pitch-domain voice takes
`:note`/`:bend`/`:detune` instead. The conversion is a documented library default
(`pitch->frequency` over `cents->ratio`). What is left is (b): the transformer,
which stays a P11 candidate, and any decision to retire `:frequency` entirely.
The promotion also earned inspection candidate #2 (`read-controls!`) and #4 (the
global source bank) their places in core.

## D36 — A voice transform is a function over a definition value, and `->` chains them

**Status.** Implemented and **validated**. `dsp/def` names a transformed
definition; the `unison` transform's output is bit-identical to the sum of its N
separately-rendered detuned copies, on both the interpreter and the JVM; and the
pool's `:unison` mode, `unison-detune`, and unison branch are **removed**, with
`:detune` kept as a standard host source set by `set-detune!`. Getting there
exposed and fixed a pre-existing nested-composition bug (see the DIARY entry
"State layout was undersized for nested composition"): the IR lowering had
iterated the linker's `:definitions` map, which past eight entries is a hash-map,
losing dependency order. Lowering now uses the linker's `:order`. The
combinators themselves now live in `omkamra.dsp.combinators`, on the
substrate in `omkamra.dsp.graph` (D38).

**Decision.** A `dsp/defn` value is a descriptor — data. Therefore a **transformer**
is a plain Clojure function `(definition, options) -> definition` over that value,
and derived voices compose with `->`:

```clojure
(dsp/def unison-lead
  (-> lead-voice
      (unison {:count 3 :detune 15})
      (with-oversampling {:factor 2})))
```

`dsp/def` (new, in `omkamra.dsp`) binds the result: it takes an expression whose
value is already a definition, stamps `:name` and `:source {:namespace *ns*}`, and
interns it, so the result is resolvable by the linker exactly like a `dsp/defn` Var.
Because the transformer emits N fully-qualified calls, its descriptor needs no
aliases. `unison` is the first user: it emits `count` detuned copies of the voice,
summed, with each copy's state per call site (D19) — so the copies are independent
for free, and *polyphonic* unison falls out.

**Rationale.** Unison is a **voice shape**, not an allocation policy: `:poly`,
`:mono`, `:legato`, and `:retrigger` decide how notes map to voices over time and
belong in the pool; unison decides how many copies of one voice sound, which is a
property of the voice. Putting it in the pool (D33) was a miscategorisation, and
the pool-mode version cannot do polyphonic unison. Expressing it as a transform
needs **no** language, IR, ABI, or backend change beyond `dsp/def` — it is the same
"data in, data out" power a macro has, but over values, so it also works at runtime
(which is where P13's browser registry is headed).

**Inventory — what is engine-hardcoded today and whether it is a transform.**

| mechanism | verdict |
|---|---|
| unison | **transform** (this decision) |
| stereo spreading / pan (P10d) | **transform** (per-copy pan; needs the multi-channel output) |
| oversampling + reported latency | **transform** (wrap a voice in up/down-sampling) |
| per-voice effects (delay/chorus/distortion) | **transform** (`(-> voice (with-distortion …))`) |
| waveshaping / drive | **transform** |
| control `:smooth` / `:slew` (D27) | **transform candidate, strong** — a control is per-sample, so smoothing is a one-pole on the control; a transform could insert it and delete ABI/engine surface |
| portamento / glide | **transform candidate** (a slew on pitch) |
| pitch conversion (note/bend/detune → Hz) | already a library default (D35); a transform would inject it |
| mono / legato / retrigger | **engine** — allocation and note-lifetime policy |
| stealing policy | **engine** — allocation policy |
| standard sources (gate/note/velocity/bend/detune) | **engine** — the standard interface a transform *analyzes*, not a transform |
| voice tails / non-finite containment (P10c) | **engine** — reporting whether a voice has finished |
| optional input port (D23) | **engine** — structural |
| multi-channel output | **engine** — ABI |

The split that falls out: **voice-shape** things (how many copies, how they are
mixed, how a control is conditioned) are transforms; **allocation, lifetime, and
the standard I/O interface** stay in the engine. The recurring transform shape is
"wrap the voice, add per-copy or per-control processing, mix".

**Consequences.** D33's mode set is now `{poly, mono, legato, retrigger}`, and
the pool's `:unison`, `unison-detune`, and unison branch are gone. `:detune`
stays a standard *host* source, set by `set-detune!` (per voice; a fresh voice
starts in tune); `:bend` stays. The transform's test no longer uses the pool mode
as its oracle — it compares against the sum of N separately-rendered detuned
copies, which is a stronger, self-contained check of what the transform claims to
generate.

## D37 — A definition is a value, and a control mode is also a transformer

**Status.** Complete. `dsp/fn`; the `smooth`/`slew` transformers; and the
call-emitting step — `unison` works on anonymous `dsp/fn` values, because a
derived value carries the definitions it calls in `:dsp/dependencies`.
Transforms also compose: `link` walks `:dsp/dependencies` transitively, so
`smooth` ∘ `unison` and `unison` ∘ `unison` link. The transformer helpers stay in
the example for now; a second call-emitting transform will say whether they
graduate to core.

**Decision.**

- **`dsp/fn`.** `(dsp/fn …)` is to `dsp/defn` what `clojure.core/fn` is to `defn`:
  it captures the same descriptor and *returns it as a value*. `dsp/defn` is now
  `(def name (capture …))`; both share `capture-definition`. The value carries a
  generated internal `:name` and the defining namespace's `:source`, so it links
  directly; `dsp/def` names it when a Var is wanted. Note `dsp/fn` shadows
  `clojure.core/fn` in `omkamra.dsp`, so every internal use is
  `clojure.core/fn` and `fn` is excluded from `clojure.core`'s refer.
- **A control mode is already sugar, so a transformer just sets it.**
  `:smooth`/`:slew` is not an engine or backend feature:
  `descriptor/normalize` desugars it into a state slot plus a per-frame `:next`
  and rewrites the body so the control reads the smoothed value (D27). So
  `smooth`/`slew` are transformers that set that *metadata* on a control, and the
  same `normalize` pass does the rest. `(-> voice (smooth :cutoff 0.01))` and
  declaring `{:name cutoff :smooth 0.01}` are the *same mechanism*, so they
  cannot drift, and the transformer works on anonymous `dsp/fn` values because it
  edits no code.

**Rationale.** Both surfaces are wanted: the inline declaration is ergonomic for a
voice's author, the transformer is composable (apply it to a voice you cannot
edit, or build a `->` chain). Having the transformer set the metadata rather than
rewrite DSP keeps one implementation. And `:smooth`/`:slew` stays in the
compiler deliberately: it is language sugar (a peer of `:type`), not a backend
hack, so the inventory's original hope of deleting engine surface does not apply
— the win is the added authoring surface.

**Call-emitting transforms.** `unison` emits calls to the definition it wraps, so
that definition's id must resolve. A `dsp/defn` original is a Var and resolves by
name; an anonymous `dsp/fn` original has no Var. So a call-emitting transform
returns a *derived* definition (`omkamra.dsp.graph/derived-definition`): it takes a
fresh internal name, carries the wrapped voice's interface (parameters, return
type, process declaration, options, source) so it normalizes on its own, and
records the original under its fully-qualified id in `:dsp/dependencies`.
`dsp/link` collects `:dsp/dependencies` transitively from the entry and any
supplied `:definitions`, and hands them to the linker as supplied definitions.
The fresh name is load-bearing: the derived body calls the original, so sharing
the original's id would be a self-call. `smooth`/`slew` emit no calls and need
none of this.

## D38 — Assemble the static graph in Clojure; the engine owns dynamic instances

**Status.** Adopted as the guiding model for the graph layer. Only the parts
that already exist are implemented (definition values, `:dsp/dependencies`,
`unison`/`mix`/`through`/`smooth`/`slew`); the graph-description intermediate and
the optimizing transformers are planned, not built.

**Decision.** An audio graph is assembled in ordinary Clojure and crosses into
the compiler exactly once:

    build (Clojure data) → optimize (Clojure data → data) → lower (one fixed
    expression) → link

The linker is the boundary: we stay in Clojure-land as long as possible, but not
longer. A definition value *is* the fixed expression the linker consumes;
combinators are plain functions over definition values, so `->`, `reduce`, and
`map` compose them.

**The fixed expression is already a DAG.** It has `let`, so a value computed
once can be named and read by several consumers: read-only fan-out needs no new
representation. Combinators currently emit trees — a node written twice is two
call sites, hence two independent state partitions (D19) — but they can emit
`let` bindings instead. So a graph description is **not** required for
expressiveness; what it could still buy is a place to run optimization passes and
to inspect the graph, and it does not cover *shared mutable state* (a bus several
writers add into), which is a state record, not a value. Until a pass needs it,
the definition-value combinators are the graph. A `share` combinator is the
natural test: if `let` suffices, `share` is a code-generation pattern, not an IR.
(An earlier draft claimed fan-out forced a graph layer; that was wrong, because
the body language has `let`.)

**Hoisting is the recurring cost control.** Two instances of one idea:

- *At lowering*, a computed call argument is bound to a local once, so inlining
a callee that uses its parameter several times does not duplicate the
computation — and a stateful argument is evaluated exactly once. This is a
correctness fix, not only a size one: the generated targets inline, and a
duplicated `:call` would have advanced one partition twice (the interpreter,
which does not inline, was already correct). The argument expression is now a
`:local`, so the inliner substitutes a local read.
- *At the combinator layer*, a duplication transform hoists the **shared**
controls to the wrapper and expands only the **per-slot** ones (`poly` takes
`:per-slot`, default `[:gate :note]`). The interface therefore grows by the
per-slot controls rather than by all of them: `poly` of a `[gate note detune]`
voice is `[detune gate_0 note_0 gate_1 note_1 ...]`, not three copies of every
control.

**Optimizing transformers are data → data rewrites** that choose among lowerings
the system can already express. They split by reach:

- *Pure rewrites.* Constant folding; common-subexpression/dedup of **stateless**
  blocks — never stateful ones, whose two call sites are deliberately
  independent (D19); dead-node elimination; sharing a stateless subgraph;
  choosing unroll versus reuse; backend code-dedup of repeated bodies (which
  removes the artifact-size cost of unrolling without changing semantics).
- *Rewrites that need a shape to exist first.* A fixed-N `poly` to a runtime loop
  needs **arrayed state**, which the language does not have; "unrolled voices to
  one kernel, K instances" *is* the pool, a runtime change; naive to oversampled
  needs rate-conversion blocks.

The rule: an optimizer can remove cost the naive shape does not need, but it
cannot invent a lowering the system cannot express.

**`poly` is a node with a lowering policy, not a peer of `unison`/`mix`.** It has
two available lowerings — naive unroll (static, one artifact) or pool instances
(shared code, dynamic) — because a fixed-N poly kernel is the *static* half of
polyphony and the pool is the *dynamic* half (capacity, stealing, tails).

**Trade-offs, stated once.** Static assembly unrolls: generated targets inline
calls, so K call sites emit K bodies and K state partitions, and the graph is
fixed at compile time (no runtime topology). The pool trades those for shared
code and dynamic instances. Neither subsumes the other; they are the static and
dynamic halves of the same picture.

**Consequences.** The core stays tiny: `dsp/defn`/`dsp/fn` produce a value,
`dsp/link` resolves it, `dsp/compile` lowers it, and `:dsp/dependencies` carries
composed values. The combinators live in `omkamra.dsp.combinators`, built on the
definition-value substrate in `omkamra.dsp.graph`; `demo_synth` is the instrument
that uses them. D34, D36, and D37 are all instances of this one principle. P11's
reference synth is assembled from blocks and combinators rather than written as
one voice.

**Optimization stage.** The compiler-level optimization pass this picture needs
is already tracked as P15 deliverable 4 and `PLAN.md` §9 — precision-aware
constant folding, dead-local/unreachable-branch elimination, copy propagation,
bounded-loop simplification, size-limited inlining, differential tests, and a
no-reassociation rule. This session added concrete candidates and one piece that
is already built:

- *Done:* `lower-call` hoists a computed argument into a local before inlining.
  Parameter substitution otherwise re-duplicates the argument once per use, and a
  **stateful** argument duplicated that way advances one state partition more
  than once — a correctness bug, not just size.
- *Needed:* deduplicate identical inlined bodies. Generated targets inline every
  call, so an unrolled voice (`unison`, `poly`, `mix`) emits K copies of each
  callee body; a backend code-dedup or a shared-helper lowering removes that
  without changing semantics.
- *Needed:* common-subexpression folding for **stateless** subexpressions only;
  stateful call sites are deliberately independent (D19).

The graph-level optimizing transformers above are a separate layer that should
feed this one: the graph optimizer chooses a lower-level shape, then the compiler
optimizer removes whatever cost remains.

## D39 — Voice tails are declared, not inferred; `activity` is a builtin slot

**Status.** In progress. Implemented: the generic writable scalar slot,
`store-special`, the builtin `:activity` slot, its ABI `:special-mask` readback,
the `Kernel` readback (resolved at construction), the pool's free gate (default
until-stolen), and non-finite containment (a policy-selected kernel concern).
Remaining beyond P10c: the JS/Wasm side, with P10d.

**Decision.**

- **A released voice is kept until it is stolen.** This is already the P10a
  behaviour and it is *correct* — `render!` mixes every non-`free` voice, so a
  release tail already sounds, and a released voice is exactly the voice stealing
  prefers. Freeing it earlier is an optimization, not a correctness fix.
- **Audio level is not a finish signal.** A voice may legitimately be silent and
  then sound again: a delay pre-roll, an LFO gate, a reverb, or a self-sequencing
  voice can be quiet for seconds. Any "peak below a threshold for N frames"
  heuristic would cut such a voice. This rules out the audio-peak gate (and, more
  generally, any inference from the output).
- **The voice declares its activity.** A voice that wants to be freed early
  publishes a host-readable slot, and the pool frees it when that slot says so.
  The mechanism reuses ordinary slots, not a bespoke API: the body can **write a
  scalar state slot** (the same store path as an aggregate slot), and `activity`
  is one of a small **builtin set** of such slots, written with
  `(store-special :activity value)` and exposed in the process ABI. Default
  (unwritten) means "still active", so a voice that does not ask for it is
  unchanged.
- **Polarity.** The activity slot reads ≤ 0 when the voice is finished. The voice
  author owns the rule — for an ADSR, bind the envelope once and store
  `(if (< env 1.0e-4) 0 1)`, optionally deriving it from the envelope's own
  `stage`.
- **Non-finite containment** is separate: replace non-finite samples with 0 and
  clamp to a ceiling, per voice at the kernel seam, so one bad voice cannot
  poison the block and cannot keep the pool busy forever.

**Why a builtin slot and not a declared one.** `activity` needs no declaration
and no per-voice opt-in flag: it is a reserved slot in the builtin set, exactly
like the read-only `sample-rate`/`frames` bindings, and the voice opts in by
writing it. The enabling language change is that a *scalar* state slot is
body-writable at all — before this, persistent state could only be computed
through `:next`, which cannot see body locals (so a library ADSR's output could
not become a flag).

**What is implemented.**

- `set!` on a declared scalar state lowers to the same `:state-store` an
  aggregate slot uses; a definition can write a slot and read it back across
  frames, identically on the interpreter and the JVM.
- `(store-special :activity value)` writes a builtin slot. `effective-state`
  scans the body for `store-special` ids and adds each slot before
  `expand-state` numbers slots, so it has an ordinary index and the arena sizing
  (`module-state-sizes`) agrees with lowering. Each builtin has a **fixed bit**
  (`:activity` is bit 0) and a definition's specials occupy its **first** state
  slots in ascending-bit order, so a host derives a slot's index from the mask
  alone. The process ABI exposes `:special-mask` (an integer) and
  `:special-slots {:activity {:name :type :bit :index}}`. Specials are the entry
  process's contract; **amended by D54** — a callee's `store-special` was
  originally rejected at lowering because its slot would be buried in a call
  partition where no host reads it, and D54 now publishes it per call partition
  instead, so a fused slot's `:activity` is readable (M6 seam 4).
- A `Kernel` method `read-specials!` fills a *preallocated* double array, one
  entry per declared special in ascending-bit order. Each kernel resolves a
  special's logical index to a **physical array slot once, at construction** (the
  same treatment `control-slots` gives controls), so `read-specials!` reads a
  primitive and never touches a map on the audio path. `compile-synth` carries
  `:special-mask` (and the declared count) in the pure-data runtime config. The
  pool resets and frees a *releasing* voice when `:activity` reads <= 0; a voice
  with no declaration is untouched. `a-declared-activity-frees-a-released-voice`,
  `a-voice-without-activity-stays-until-stolen`, and
  `a-declared-activity-defaults-to-active` cover the paths on the interpreter and
  the JVM.

- Non-finite containment is a **kernel policy** (D39), selected at
  `compile-synth`/`host-kernel` time:
  - `:reset` (the default, per `PLAN.md`) scans the block; a non-finite block is
    **silenced** (not mixed) and the voice is `reset!`, so it recovers next
    block, and the pool bumps a `:containment` counter. This is the *state*
    guarantee, not just an output one.
  - `:sanitize` contains each voice's samples before they are summed instead:
    non-finite -> 0, finite runaway clamped to a ceiling (`1e6`, far above any
    musical signal), no reset. This is the only policy that clamps *finite*
    runaways.
  - `:off` mixes the block untouched.
  The per-sample `contain` helper is primitive-hinted so it does not box on the
  audio path (a boxed version failed the zero-allocation test); the mix loops use
  `unchecked-float` because Clojure's `(float Inf)` *throws*. The policy branch is
  per-block (the same shape as `f64?`), never per-sample. Containment is
  per-voice, so one runaway voice cannot poison the rest of the block.
  `a-non-finite-voice-is-contained`, `a-runaway-magnitude-is-clamped`,
  `containment-can-be-disabled`, `a-reset-policy-silences-and-recovers-a-voice`,
  and `a-sanitize-policy-does-not-reset` cover it on both hosts.

**What remains.** The JS/Wasm `read-specials!` and containment, which arrive with
the P10d host.

**Layout and readback (what was built).** The special set is encoded as a
**bitmask**, and the slots sit at a fixed place — the **front** of the entry's
state, in ascending-bit order — so a host derives each index from the mask
without a per-slot id list. Because `:activity` is the lowest bit, its readback
position is 0 even if later specials arrive above it, so no popcount is needed.
The cost this avoids is real: the previous readback did a hash-map lookup
(`(get state-slots logical)`) per special per block; resolving at construction
removes it. The mask bits are an append-only ABI contract, and only the entry's
own specials are exposed.

---

## D40 — Stereo: one evaluation per frame, with explicit channels (proposed)

**Context.** `library/effect.cljc/stereo-spread` (P7) returns a `stereo-pair` from
one evaluation that shares a delay line. Reverb has the same shape: a shared
network of delay lines producing left and right. The current multi-channel model
runs the **whole body per channel** — `(dotimes [channel] (dotimes [frame] …))`
— with state shared across channels, so a shared-state stereo body would advance
its state twice per frame. That is exactly why `lower-process` rejects *stateful*
multi-channel, and why stereo spreading has stayed unimplemented (P10d).

**Decision.** A process's frame body is evaluated **once per frame**; channels
are explicit, in the SuperCollider `In`/`Out` sense (the runtime provides the
buffers, the body indexes them):

- The body reads `:input` and writes `:output` with an explicit channel index
  (`buffer-load`/`buffer-store`). The implicit `channel` binding and the
  per-channel target loop are removed.
- A `dsp/defn` body of several forms is an implicit `(do ...)`, as in `defn`,
  so an explicit `do` is never needed just to sequence statements.
- A scalar body is the one-channel sugar (stored to channel 0); a `:void` body
  has written `:output` itself, per channel. The rule is **semantic**: a body
  that stores into `:output` is a process and may be `:void` (no `:process` map
  needed); a body that neither returns a value nor writes `:output` is still a
  type error, so an ordinary scalar function must return a value.
- The audio width is the declared `:channels`, or otherwise **counted** from the
  body's `:input`/`:output` accesses (highest channel + 1), so a stereo body
  needs no declaration.
- State is per-instance and may be **shared** across channels — the point.

**Why not a returned `:record`/`:array`.** Records are the only first-class
aggregate *value* today, and arrays are *places*, not values (`expect-type!`
rejects them outside `aget`/`aset`/`let`). But the process output is already an
array — the per-channel output buffer — so no new value type is needed to write
several channels. A returned record would be sugar over the same stores, and
first-class array values are a separate language feature.

**Consequences.** The four targets lose their channel loop. `lower-process` stops
rejecting stateful multi-channel, so a stereo voice or a reverb compiles. The
ABI drops the `:channel` binding and reports `:channels {:count N :type :int}`
(no `:binding :loop`). Input channel counts remain declared per buffer. The
SIMD-style fixtures and tests (`channel-mix`, `gain` at `:channels 2`) are
rewritten in the new surface. The pool and host kernels then carry stereo blocks;
the CLJS/AudioWorklet host (the other half of P10d) follows.

**Explicitly deferred.** (1) first-class multichannel **array values** for
signals that travel *between* nodes — add only if the reference synth needs it;
(2) a shared **bus** namespace with host-bound node offsets, and additive stores
(`Out` adds, `buffer-store` replaces) — a P11 routing concern.

**Progress.**

- **Slice 1a (done).** One evaluation per frame across all four targets (the
  channel loop is gone from interpreter, JVM, JS, and Wasm); the implicit
  `channel` is always 0; the descriptor accepts a `:void` process body; stateful
  multi-channel compiles. Tests: `multichannel-processes-all-channels`
  (explicit I/O), `stateful-multichannel-runs-once-per-frame` (shared state
  advances once per frame, not once per channel), and the rewritten JVM/JS/Wasm
  channel tests. JVM 132 tests / 1231 assertions; browser 19/19.
- **Slice 1b (done).** When `:channels` is not declared, the lowerer counts the
  width from the body's constant `:input`/`:output` channels; a *dynamic* channel
  requires a declaration. `normalize-process` now records `:channels-declared?`,
  since its `:channels` always carries a default of 1. `stereo-cross` and
  `stereo-state-ramp` compile with no `:channels` and the ABI reports `:count 2`.
- **Slice 2 (done).** Synth descriptions accept 1 or 2 channels, but the voice's
  own width (from the ABI) is authoritative and a declared `:channels` must
  agree. Kernels capture the width; a voice's scratch and the output block are
  **planar** for stereo (a mono array otherwise) — one conversion at the host
  boundary, and none in the audio path. Containment is per channel; `:reset`
  treats the whole voice (all channels) as the unit. The pool sums voices
  plainly: panning is a per-voice concern, so the pool never pans. `:pan` is a
  **standard per-voice source** (`set-pan!`, the stereo counterpart of `:detune`
  — set after `note-on!` returns the index, reset on steal; it re-resolves the
  voice so the new value is heard on the next render). A stereo voice binds it
  and maps it to its channels; a voice that binds no `:pan` control is untouched
  and pans inside its own DSP. `a-stereo-voice-renders-a-planar-block` and
  `a-per-voice-pan-reaches-the-voice` cover it on the interpreter and the JVM.
- **Slice 3 (done).** The browser host and the worklet boundary. The kernels
  moved out of `omkamra.dsp.synth.host` into a `omkamra.dsp.hosts.jvm` family —
  `.jvm`, `.interpreter`, `.js` — with the plumbing they share (the containment
  policy and the planar blocks a voice is rendered into and mixed through) in
  `omkamra.dsp.kernels.mixing`. That namespace is `.cljc`, so the JVM suite and the
  browser testbed run the *same* reference oracle; `omkamra.dsp.hosts.jvm` itself is
  the assembler (`synth-bundle`, `compile-synth`). `omkamra.dsp.kernels.js` is a
  `Kernel` over a generated JavaScript module, and a pool of those is checked
  against an interpreter pool in the browser over the same events (`JavaScript
  pool host`). The worklet adapter now calls the kernel **once** per block and
  hands it the callback's own channel arrays — one channel array for a mono
  kernel, the array of them otherwise — and silences the channels the kernel does
  not write; the kernel's shape (`channels`, `inputChannels`) comes from its ABI.
  Nothing is up-mixed, which is what slice 2 decided: placing a mono voice is the
  voice's own business (`effect/pan`). `omkamra.dsp.kernels.wasm` is the same host
  over a Wasm module, with one real difference: a Wasm kernel is not handed the
  block — `process` takes the byte offset of its output region and writes channel
  `c` at `offset + c * channel-stride` — so a voice's scratch is a set of typed
  array views over its own instance's memory, and the Wasm adapter copies each
  channel out of that memory after the call. Both hosts are checked against the
  interpreter pool in the browser (`JavaScript pool host`, `Wasm pool host`).

**Status.** Accepted; slices 1a–3 done. **P10 is complete.**

---

## D41 — The JavaScript target builds ClojureScript forms, and Squint emits the module

**Context.** The JavaScript backend lowered portable IR to a JS-shaped IR and
then emitted the ES module as *text*, with roughly 250 lines of string building:
operator precedence, statement-versus-expression duality, temporary naming, and
`let` ordering were the emitter's problem, and the only affordable checks were
assertions on the text it produced. Squint is already a dependency — the two
AudioWorklet adapters compile through it.

**Decision.** Emission builds **ClojureScript forms** (plain data, plus `js*` for
a few exact spellings) and hands them to `squint.compiler`:

```text
portable IR -> JS-shaped IR (lowering, unchanged) -> ClojureScript forms
            -> pr-str -> squint/compile-string {:elide-imports true} -> JS
```

Squint owns the syntax; the emitter owns only the structure of the module.
`js*` is reserved for what has no ClojureScript spelling that means the same
thing: `Math.fround`, `| 0`, `break`/`continue`, `NaN`/`Infinity`, the array
literal, and the kernel object literal.

**Rules.** Four properties hold by construction, and each is a *runtime*
constraint rather than a compile-time one, so each is what a change to the
emitter has to respect:

1. a local that is `set!` carries `^:mutable` — Squint otherwise emits `const`,
   and the assignment throws while audio runs;
2. `break` and `continue` are `(js* "break")` and `(js* "continue")`;
3. `js*` never names a local — Squint mangles locals — so every value is spliced
   with the template placeholder;
4. **nothing lowers to a `squint_core` reference**, because a kernel is installed
   as one standalone script.

Rule 4 is the subtle one. Squint calls its standard library for any value it
cannot prove is a boolean: `and`/`or`/`not` become `squint_core.truth_` and
`squint_core.not`, `=` becomes `squint_core._EQ_`, and an `if`/`while` condition
it cannot prove boolean is wrapped in `truth_`. `^boolean` hints are *not* a
general fix — they make `and`/`or` lower to a value-preserving ternary, which
then needs `truth_` in a condition. The emitter instead stays inside what Squint
recognises as boolean:

- the native comparisons (`<`, `<=`, `>`, `>=`, and `==`), with `:not=` written
  as `(not (== a b))` — which is `!(a === b)`;
- `and`/`or`/`not` only over operands that are already provably boolean; anything
  else is compared with `true` first;
- a `^boolean` declaration for a boolean argument or local, so that
  `(not (not x))` is `!!x` — which is how a boolean argument is normalised at the
  boundary, since an `AudioParam` or a pool control array arrives as a number.

**Consequences.** The module stays a standalone ES module with no import, so the
AudioWorklet and the testbed load it exactly as before. The kernel object also
exposes its state arena now, so a host can read a voice's special slots (D39)
without a second entry point. `tests/js.clj` asserts the module contract — export
shape, physical type, and no `squint_core` across the whole catalog — rather than
the emitted text, and the browser conformance suite is the behavioural net. The
Wasm target is unchanged: WAT is still hand-emitted, so the two native backends
now differ in authoring style. Because the forms are data, the emitter can become
`.cljc`, which is a step toward `PLAN.md` section 4's browser-side compilation.

**Status.** Done. JVM 137 tests / 1437 assertions; browser 19/19.

## D42 — The Clojure instrument graph is a source layer; the compiler lowers it to a program

**Status.** Provisional, first slice (P11b-i). The reference synthesizer uses it
end to end; the shape is expected to be refined at the end of P11.

**Decision.** An *instrument* is a **Clojure-level value**, not a kernel: a voice
definition, its polyphony, a post-mix effect chain, and the (provisional)
modulation matrix, composed with ordinary functions and `->`. The compiler
**lowers** that value to a **program** — kernels, topology, and controls — and the
host runs the program. The organization at the Clojure level is deliberately
**not** mirrored at the kernel level: the lowering may fuse, split, unroll, or
reorder what it finds. Concretely (P11b-i):

```
omkamra.dsp.instrument            the value + transforms (instrument, voice,
                                  routes, with-effect)
omkamra.dsp.instrument.program      lowering: compile-instrument -> program
                                  {:bundle (voice pool), :bus, :routes}
                                  + the live host (instrument-pool, render!)
```

**The first lowering strategy.** The voice goes through the existing P10
machinery (`synth-bundle`: an artifact plus a runtime configuration), and the
post-mix chain is folded into **one mono processor definition** (a `chain-effect`
that extends the interface with each effect's controls), compiled to **one bus
kernel per output channel**, run after the pool with the unchecked JVM adapter.
That is a *choice*, not a consequence: a later strategy may compile the whole
instrument to one polyphonic kernel, keep the effects as separate stages, or
unroll them, without touching the source layer.

**Per-feature layer assignment** is the working rule. For each feature, decide
whether it is stated in Clojure (composability), realized by the lowering
(optimization), or expressed in the kernel (semantics):

| feature | Clojure layer | lowering | kernel |
|---|---|---|---|
| polyphony | pool node / `poly` | instances vs poly-kernel vs unroll | N kernels or one loop |
| post-mix effects | `with-effect` chain | per-channel vs stereo; stage placement | one process per stage |
| per-voice effects | `with-effect` on a voice | inline into the voice body | in the frame loop |
| modulation | `routes` data + host policy | resolve/hoist; bake or leave host | controls / DSP |
| stereo | expressed by the voice body | count and layout | `:channels` |

**Rationale.** The two goals are different and pull apart: easy composition wants
a small, uniform, inspectable algebra; an ideal kernel wants the opposite of
whatever shape was convenient to write. Mirroring them would force one to pay for
the other. Lowering is where the reconciliation happens, and where `PLAN.md` §9's
optimizations (P15) belong.

**Consequences.** `dsp/compile`'s "one definition → one artifact" remains the
*kernel* API; instruments compile to a *program* with several artifacts. The
post-mix controls are **global** (the host sets them), which generalizes "voice"
to *per-voice controls + global controls + output width* — the pool is the first
such thing, not a special case. The modulation matrix stays in the reference
synth (D34) and is not part of the lowering. The Clojure value layer is `.cljc`;
the lowering and live host are JVM-only until P11c adds the browser hosts.

## D43 — A control name's canonical form is a symbol; a surface API accepts any spelling

**Status.** Accepted (post-P11 refinement).

**Decision.** Inside the definition and body layers, a control's name is a
**symbol** — a definition's parameter list is symbols, the ABI's `:name` is a
symbol, and the runtime plan keys by `(name control)`. A *surface* API that takes
names from the user accepts any of the spellings that appear across the layers —
a symbol, a keyword, a string, a quoted symbol, or a map with `:name` — and folds
them to the canonical symbol through one helper, `graph/normalize-control-name`.
A companion, `graph/normalize-control-argument`, applies the same fold to a call
argument while leaving a literal or an expression untouched. Both are idempotent.

**Why a symbol and not a keyword.** The name it must match is the *definition's
parameter*, which is a symbol in the source and the ABI; a keyword would be a
second token type to translate at every boundary. A symbol is also the
unambiguous marker: the lowering decides "this argument is a control reference"
versus "this is a literal" by `symbol?`. A keyword is free to accept anyway,
because the DSP language has **no keyword literal** — `ir` lowers only numbers
and booleans as constants — so a keyword in an argument position can only be a
reference, and it collides with nothing.

**Consequences.** `with-effect` (and `comb/through`) may be written
`[:rate :depth :mix]`; the stored value and the lowered bus are canonical
symbols regardless of spelling. `combinators/control-name` is now this helper;
`synth`'s keyword/symbol/string acceptance and `descriptor`'s `process-name` are
the same fold and may be collapsed onto it later. The host setters
(`set-bus-control!`, the pool) already compare by `name`, so they accept either
spelling at no cost.

---

## D44 — Instrument parameters and modulation (provisional)

**Status.** Accepted; the M2a parameter layer is implemented (see `DIARY.md`).
The full design note is `docs/mvp/MODULATION.md`; this entry records the durable
decisions so later work has a stable reference. Details may be refined there
without changing the decisions below.

**Decisions.**

- **Control vs parameter.** A *control* is a DSP process input, resolved per
  voice and per frame. A *parameter* is an instrument-level, host-addressable
  named value (range or enum, default). A *binding* maps parameters to controls.
- **Parameters are uniform.** omkamra.dsp does not record where a value came
  from. A note, a control change, a knob, or a sequencer curve all arrive through
  the instrument API; provenance is the client's concern. This retires the
  `control-sources`/`:from :event` distinction.
- **Scope.** A parameter declares `:scope :shared` (one value for the whole
  instrument, read by every voice and/or by the post-mix bus — the old
  `:instrument`/`:voice` split was only *who reads it*, which is the binding's
  business) or `:per-voice` (one value per voice). `:lane` is an orthogonal
  modifier: a parameter may also carry a per-frame buffer (M4). Declarations are
  optional; a control with no declaration infers a same-named `:per-voice`
  parameter whose default is the control's own.
- **Triggers.** Events are timestamped on a monotonic timeline. Triggers are the
  general concept: `note-on`/`note-off` are built-in triggers whose handlers
  allocate/release a voice and set the conventional `gate`/`note`/`velocity`
  parameters; instrument-defined triggers run user handlers. Handlers run in the
  sequencer, off the audio thread, in ingress order.
- **The matrix is a generator input.** It is data of routes
  `{:from parameter :to parameter :depth amount :delay frames}`, passed to one of
  two generators: an *instrument reducer generator* (a Clojure/ClojureScript
  function for the sequencer) or a *kernel reducer generator* (a new kernel
  description for the assembler). The instrument value never holds the matrix.
- **Edge scheduling.** Edges are instantaneous by default (zero latency). A
  route's `:delay` (default 0) reads the source from N steps earlier and is the
  only way a cycle is well defined. The `:delay 0` subgraph must be acyclic; the
  generator resolves its topological order once, at generation time.
- **Depth** is a compile-time literal by default; a route may name a parameter
  as its depth for second-order modulation.
- **Sequencer placement.** The sequencer runs off the audio thread. In the
  browser it belongs in a dedicated Web Worker running ordinary ClojureScript;
  the AudioWorklet runs only the generated kernel plus a preallocated ingress
  queue and `schedule`. Timestamped events with a lookahead window; no lockstep.

**Deferred.** Instrument-level (broadcast) parameters should be one state
variable every voice reads — a shared kernel region — rather than a per-voice
copy. Postponed; see `MODULATION.md`. Interim: one host-side value written only
on change.

**Consequences.** `omkamra.dsp.instrument` drops `:routes` and gains parameter
declarations and bindings. `synth/control-sources` and the provenance half of the
pool's control plan retire. `omkamra.dsp.reference` becomes the oracle for the
instrument reducer generator. The kernel reducer generator is a combinator over
`graph/derived-definition`.

## D45 — Lowering the instrument: pool + bus, or one fused process (proposed)

**Status.** Proposed, **narrowed** (see *Narrowing* below), and now **decided**:
after the post-D47 measurements at the end of this entry, fusion ships as a **chunked
option** on every target and **pool stays the default**. The M4 half of the extension
argument was already withdrawn; M5 is unaffected by the choice.

**Context.** `omkamra.dsp.instrument.program/compile-instrument` lowers an
`omkamra.dsp.instrument` value to **a voice pool plus a post-mix bus**. The pool
is a host-side allocator and mixer over N instances of one voice artifact (a
*generator*, no input port); the bus is the instrument's effect chain folded into
one *processor* and instantiated once per output channel. After the pool has
summed the voices, each channel's bus instance runs over the block in place.

The two share an implementation shape — a compiled artifact, a control plan, a
per-target "run a block" seam — but differ in role: the pool resolves polyphony
into a mix, the bus transforms the mix. The question this entry answers is
whether the chain could instead be part of **one DSP process that generates and
sums the voices**.

**Decision (provisional).** Keep **pool + bus as the default lowering**, and treat
a **fused instrument process** as a first-class *alternative lowering* of the same
instrument value, expressed at the compiler layer:

- the existing **`comb/poly`** combinator (D38): it already unrolls a fixed
  number of independent slots, expanding `:per-slot` controls (`gate_0`,
  `note_0`, ...) and hoisting every other control to the top, so a fused voice
  kernel is `(comb/poly voice {:count N})` and the host writes the per-slot
  controls; and
- the existing **processor composition** (`chain-effect`/`through`) for the chain,

so the fused instrument is `(post-mix-chain (poly N voice))`, compiled to one
artifact with one instance that writes the channels.

This is a lowering, not a rewrite: `compile-instrument` already promises that the
same value "can be lowered differently later — an unrolled poly kernel, a stereo
bus, a fused instrument kernel". The choice is *which lowering is the default,
and when* — not whether M4 can proceed, which it can (see *Narrowing*).

**What stays host-side.** Voice **allocation** — which note takes which voice,
stealing order, `:legato`/`:retrigger`, the held-note stack, `:activity`
tail-freeing — is event-driven host policy (D34) and stays in the sequencer,
whatever the lowering. Fusion moves the *renderer* into DSP; it does not move the
allocator. The host keeps writing per-voice `gate`/`note`/`velocity` into the
fused kernel's per-voice control region.

**What must be solved.** The seam the allocator needs becomes DSP-visible:

- **reset/steal.** The host can no longer call `reset-voice!` on a handle. A
  fused kernel needs a per-voice way to re-initialise state — a `:reset` control,
  or an activation id the voice compares against and re-initialises on.
- **readback.** `:activity` and containment reporting move to the ABI's specials
  (per voice) or become host-visible state.
- **voice skipping.** The pool skips free voices; a fused kernel evaluates all N
  unless the generated code branches on each voice's gate.
- **fixed capacity.** N is baked into the artifact; changing polyphony is a
  recompile, and artifact/code size grows roughly ×N on JavaScript and Wasm.

**Why it is attractive.** Two planned slices exist only because voices are
separate instances:

- **M5 (shared kernel region)** is needed because a broadcast parameter must be
  copied into each voice instance. In one fused region it is read once and used N
  times — the M5 problem dissolves. (Caveat: `poly` hoists shared *controls*, but
  D19 keeps state per call site, so a shared *state* variable is a separate
  question even under fusion.)
- **M4 (lanes through the pool)** is *shrunk*, not dissolved — see *Narrowing*.

### Narrowing (after reading the code)

The M4 half of that argument does not hold up. Lanes are already implemented
*below* the host: D28 ships `:lane` controls on all four targets, a lane read is
an ordinary `:buffer-load` from the flat `:lanes` region, and `process!` already
accepts `:lanes {:amount …}` — the shared catalog's `:automation-lane` case
proves it end to end. What M4 adds is the **host seam**: `synth/pool`'s `Kernel`
protocol (`mix-voice!`, `write-voice-controls!`) carries no lane region, and
neither does the instrument host's bus.

Fusion changes *who holds* the fan-out, not whether it exists. One fused artifact
with a hoisted shared control needs one lane region instead of N — a real saving —
but a `:per-voice` lane is expanded by `poly` into `cutoff_0 … cutoff_{N-1}`, so
the host still renders and delivers N lanes' worth of per-frame values, just into
one artifact. And the substance of M4 — shapes 2 and 3, *worklet-side
interpolation* vs *host-rendered buffers over `SharedArrayBuffer`* — is about how
the host *produces* the values, which no lowering changes.

So the decision is narrowed to: **should the fused lowering also ship, and should
it become the browser default?** — a question about artifact/code size and the
reset/readback/skipping seam, taken at **M5**, where a shared region actually
matters. M4 proceeds as pool/bus lane plumbing, which the current default
lowering and the browser instrument host need today, and whose host-side half any
fused path would reuse.

### Measurements (pool against fused)

This question was expected to be settled by reasoning. It was measured instead:
the *same voice*, lowered both ways, timed on all three hosts — 128-frame blocks,
milliseconds per block, 1000 blocks after a 200-block warm-up, repeated runs
agreeing within ~2%. The browser hosts are driven through
`window.omkamraDspTestbed.benchConcurrency(n)`, served by the testbed's
`bench-*` and `fused-*` endpoints.

| voices | JVM pool | JVM fused | JS pool | JS fused | Wasm pool | Wasm fused |
|---|---|---|---|---|---|---|
| 4  | 0.081 | 0.058 | 0.106 | 0.138 | 0.143 | 0.116 |
| 8  | 0.157 | 0.094 | 0.218 | 0.384 | 0.253 | 0.233 |
| 12 | 0.234 | 0.135 | 0.309 | 0.615 | 0.390 | 0.342 |
| 16 | 0.310 | **4.030** | 0.410 | 0.911 | 0.537 | 0.456 |
| 24 | 0.464 | 6.164 | 0.591 | **4.943** | 0.785 | 0.681 |
| 32 | 0.619 | 8.532 | 0.794 | 6.958 | 1.013 | 0.905 |

Three findings, none of which the reasoning predicted:

1. **The pool is linear in N on every target, and has no cliff** — about 150 ns
   per voice per JVM frame, and the same shape in both browser hosts.
2. **Full fusion falls off a cliff where the engine refuses to compile the
   unrolled function.** On the JVM that is between N=12 and N=14 — fused is 1.7×
   *faster* than the pool at 12 and 13× *slower* at 14, and from there it scales
   as interpreted code. On JavaScript the cliff is between 16 and 24. That is
   HotSpot's huge-method rule and V8's optimizing-compiler bail-out, not a
   property of the DSP.
3. **Wasm has no cliff, and fusion wins there — modestly.** About 13% at
   N=8…32 (0.227 vs 0.251 at 8; 0.681 vs 0.785 at 24). Plausibly because a Wasm
   function is compiled as one unit, with no inlining heuristic to defeat, so
   fusion recovers the per-voice boundary crossing and instance overhead without
   paying for them.

Fairness note: the pool writes its voices' controls once, at note-on, while the
fused call re-passes every control each block — so the table slightly *favors*
the pool, and fusion is no worse off than shown.

### Measurements (post-D47: the pool against *called* fusion, all three targets)

D45's table above was taken before D47, so its fused arm was **inlined** — the arm
whose cliff those findings describe. D47 changed what a call emits, and the fused arm
of the bench is now a *called* artifact: one function per definition, N call sites.
Re-measured with it, the pool no longer looks like the safe choice.

`harness/bench.clj` gained the pool arm for this (N instances of the *voice* artifact
driven by `synth/pool`, `:containment :off` to match `dsp/process!` on the fused
arms), so the same three arms now exist on every target: the browser testbed already
had them. JVM, 128-frame blocks, `fixtures/sine-voice`, one schedule — the pool's note
69 is the 440 Hz the fused arms are handed, and at N=1 the pool's block is **bit for
bit** the fused one's, so the two really are two lowerings of one voice:

| voices | pool ms/block | fused ms/block | fused-inlined ms/block | pool bytes | fused bytes | fused-inlined bytes |
|---|---|---|---|---|---|---|
| 4 | 0.085 ± 0.007 | 0.047 ± 0.003 | 0.039 ± 0.000 | 1 695 | 1 938 | 17 787 |
| 8 | 0.170 ± 0.001 | 0.088 ± 0.001 | 0.073 ± 0.001 | 1 695 | 2 076 | 56 695 |
| 12 | 0.259 ± 0.004 | 0.130 ± 0.002 | 0.107 ± 0.002 | 1 695 | 2 216 | 117 756 |
| 14 | 0.297 ± 0.007 | 0.148 ± 0.003 | **3.581 ± 0.148** | 1 695 | 2 286 | 156 464 |
| 16 | 0.340 ± 0.005 | 0.172 ± 0.003 | 4.137 ± 0.125 | 1 695 | 2 356 | 200 572 |
| 24 | 0.550 ± 0.066 | 0.268 ± 0.028 | 6.799 ± 0.499 | 1 695 | 2 636 | 431 000 |
| 32 | 0.697 ± 0.016 | 0.339 ± 0.007 | 8.510 ± 0.565 | 1 695 | 2 916 | 747 808 |

1. **On the JVM the pool pays about 2× at every N** — 0.085 against 0.047 at N=4,
   0.697 against 0.339 at N=32 — and neither arm has a cliff; the inlined arm still
   has one exactly where D45 found it (3.581 at N=14), which is D47's diagnosis
   confirmed a third time. Per voice that is ~171 ns per frame of host-side work
   (a protocol-dispatched kernel call per sounding voice per block, plus the loop over
   capacity), i.e. ~12% of a 128-frame block's real-time budget at N=32.
2. **The same comparison on the browser hosts, post-D47** (D47 stage 4's table): the
   pool is **1.5–1.8× slower on JavaScript** at every N and **~1.1× on Wasm**. So
   across the three targets, fusion buys ≈2× on the JVM, 1.5–1.8× on JS, and ~10% on
   Wasm — the last being the target where D45 measured the smallest win.
3. **`^:inline` cannot reach any of it.** Inlining substitutes a body inside *one*
   artifact; what the pool pays is the boundary *between* artifacts. Both fused arms
   above are lowerings of one artifact, and the called one — the one with a call graph
   intact — is the fast one everywhere it can be built.
4. **Size is now the pool's win.** The pool's artifact is one voice and does not grow
   with N (1 695 bytes at N=4 and N=32, in a test); the fused artifact does (2 916
   bytes, and 747 808 inlined). Capacity stays a host-side number for the pool.

**Occupancy flips it.** The table above is a *full* patch: every voice sounding, for
the pool and for the fused arms alike. `bench.clj` now takes a `:sounding` count,
because the two lowerings are not symmetric in it: `pool/render!` loops over capacity
but calls the kernel only for *sounding* slots, while a fused process evaluates every
slot it was compiled with. Two voices sounding, same schedule:

| voices | sounding | pool ms/block | fused ms/block | fused-inlined ms/block |
|---|---|---|---|---|
| 8 | 2 | 0.047 ± 0.006 | 0.094 ± 0.014 | 0.079 ± 0.011 |
| 32 | 2 | 0.042 ± 0.000 | 0.315 ± 0.006 | 7.996 ± 0.519 |

1. **The pool's cost is flat in capacity and the fused one's is not** — 0.047 at N=8
   and 0.042 at N=32 for the pool, against 0.094 and 0.315 for fusion. At two voices
the pool is 2× faster at N=8 and **7.5× faster at N=32**, where at full occupancy it
was 2× *slower*.
2. **So the sign of the comparison is a property of the patch, not of the lowering.**
   Fusion is worth it when a chunk's slots are sounding together; the pool is
   structurally better when they are not, because it does not touch a free voice at
   all, while a fused process pays for every slot it was compiled with unless the
   generated code branches per slot. That is D45's *voice skipping* objection with a
   number under it, and it is why the pool must remain the default and fusion must
   stay *requested* — occupancy is runtime behaviour and cannot be a compile-time
   choice.
3. **Per-slot skipping is therefore not an optimisation but a precondition.** With a
gate check generated per slot, a silent slot costs a branch instead of a whole
voice's work, which bounds how badly fusion can lose at low occupancy — but it can
never beat the pool there, since the pool skips the slot entirely and fusion still
evaluates its branch. Chunk-level skipping only helps when chunks are small; at one
chunk (S=N) there is nothing to skip.

**Consequence (revised).** The earlier conclusion — keep the pool as the *only*
lowering — does not survive those numbers, but neither does fusion as a default: it
would cost dynamic polyphony, per-instance reset/steal, `:activity` readback, and the
skipping of free slots, all of which the pool gets from per-instance isolation. So:
**fusion ships as an option, with the pool remaining the default** — the option being
a *slot count per instance* (S), where the pool is the S=1 case realized by the host
and fusion is S>1 realized in DSP. With D47 the natural first target is **one instance
for the whole capacity (S=N)**, since nothing cliffs any more and that removes every
voice crossing; chunking is the escape hatch for a very large capacity or for skipping
granularity, at the price of the post-mix chain having to stay with the host.
The seam it must solve is unchanged from *What must be solved* above; the measurements
only moved the question from *whether* to *how*.

**The scratch question, measured (M6's first step).** D47 keeps definitions that
use scratch arrays inlined — a record field or a local array makes a definition
*uncallable* — so the worry was that `(poly S voice)` would copy a delay line's
body into every slot and re-create the pre-D47 cliff. `dsp/compile` on the JVM,
f32, code size and arena slots per instance:

| voice | S=1 code / arena | S=2 | S=8 | S=32 |
|---|---|---|---|---|
| `sine-voice` (no scratch) | 1 841 / 3 | 1 876 / 6 | 2 081 / 24 | 2 921 / 96 |
| `echo-voice` (one 64-sample delay line) | 2 875 / 68 | 2 904 / 136 | 3 096 / 544 | 3 864 / 2 176 |

1. **Code grows ~30 bytes per slot, for both.** What unrolls is the *call site*,
   not the body — even when the body is inlined. A delay line's per-frame code is
   a handful of loads, stores, and one interpolation; its *buffer* is data, and
   data goes to the arena, not to the method. The cliff D45 saw at N=12–14 was
   inlining a whole voice per call site (748 KB at N=32), not inlining scratch, so
   D47's caveat is about code volume a realistic voice does not have.
2. **Memory is arena-linear and pool-equivalent.** `echo-voice`'s 68 slots per
   slot is a 64-float buffer plus a cursor per line — exactly what N instances of
   the same voice would each allocate. Fusion is memory-neutral; the only
   difference is per-instance overhead, and that is in the pool's favour.
3. **A fused voice must be module-shaped.** `poly` refuses the reference synth's
   own voice: it accesses the process's buffers, and a module may not. `poly`
   stacks *module* generators — a body that returns its signal — which is what the
   fused arms and the library voices are. A process-shaped voice would need a
   wrapper that calls it into a scratch block first; not built, and the reason
   M6's option is defined for module-shaped voices.

**The realistic voice, timed (M6a's sweep).** The scratch measurement settled the
artifact; the other half of the question is whether a voice that *does* work is
still worth fusing. `echo-voice` (a generator with a 64-sample delay line), all
slots sounding, JVM, f32, milliseconds per 128-frame block:

| slots | pool | fused | pool/fused |
|---|---|---|---|
| 1 | 0.032 | 0.028 | 1.13 |
| 2 | 0.065 | 0.048 | 1.35 |
| 4 | 0.141 | 0.089 | 1.58 |
| 8 | 0.258 | 0.175 | 1.48 |
| 16 | 0.360 | 0.188 | 1.92 |
| 32 | 0.754 | 0.393 | **1.92** |

1. **Fusion pays for real work too** — ~1.9× at 32 slots, the same figure the
   smallest voice showed, so a heavier voice does not hide the boundary cost. The
   pool is linear in slots (0.032 → 0.754 is 23× for 32×) while the fused process
   is sublinear (14×): the fan-out has moved out of the host.

**Re-measured through the option (D52), same voice and settings.** The table above
was taken on a `(poly N echo-voice)` written by the bench itself, with `poly`'s
*default* per-slot set -- `[:gate :note]`. The option expands every control a pool
writes per voice, so `frequency`, `delay` and `mix` are per slot too: each slot
reads four controls per frame instead of two, which is real work the earlier figure
did not include. Re-run through `{:fusion {:slots N}}`:

| slots | pool | fused | pool/fused | pool bytes | fused bytes |
|---|---|---|---|---|---|
| 1 | 0.028 | 0.022 | 1.27 | 2 741 | 2 875 |
| 2 | 0.055 | 0.038 | 1.45 | 2 741 | 2 927 |
| 4 | 0.113 | 0.075 | 1.51 | 2 741 | 3 040 |
| 8 | 0.229 | 0.152 | 1.51 | 2 741 | 3 264 |
| 16 | 0.369 | 0.220 | 1.68 | 2 741 | 3 712 |
| 32 | 0.844 | 0.605 | **1.39** | 2 741 | 4 608 |

So the honest figure for the lowering an instrument actually gets is **1.3–1.7×**,
not ~1.9×: the earlier number was measured on a shape the option does not produce,
which is exactly what pointing the bench at the option was meant to find. The two
conclusions that matter survive unchanged -- bit-exactness at one voice (the test
still pins it through the option) and the pool's flat artifact against a fused one
that grows with its slots (2 875 → 4 608).
2. **Bit-exactness holds at one voice.** The pool's block and the fused process's
   block are identical sample for sample, delay line included — the strongest
   statement available about two lowerings of the same voice, and what
   `tests/jvm.clj` pins.
3. **The pool's artifact is flat in N.** 2 741 bytes at N=1 and at N=8, one voice
   instantiated N times, against the fused process's 2 875 → 3 096. That is the
   argument's other half, and the test pins it too.

**Alternatives considered.**

- **Bundle, don't fuse.** Keep both artifacts but have one host object own both
  and expose a single `render!`. This is the current pool + bus with a friendlier
  facade; it does not change M4/M5, and it is not what "same DSP process" means.
- **Chain as a processor over the pool's mix (today).** The chain already *is* a
  DSP process over the summed voices; the only thing not in DSP is the generation
  and summing. Fusion is precisely about moving those in.
- **Dynamic voice count in DSP.** Rejected: the language is static, N is a
  compile-time constant, and the dynamic part (allocation) is host policy.

**Consequences.** M4 is *not* provisional — it is the host seam over a kernel
feature that already works, and it is needed by the default lowering; fusion
would shrink it, not remove it. M5 is the slice the answer changes. The reference
instrument is the oracle either way: a fused lowering must render what pool + bus
renders — mode for mode, voice for voice — before it can replace it. Designing
the `poly` combinator and the per-voice reset/readback seam is the enabling work.

## D46 — A lane belongs to a control, and derived values are read through accessors

**Status.** Implemented: `:has-lane` in the descriptor, the `(lane control)`
accessor in the linker and IR, and the ABI's `:required?` lane flag.

**Decision.** A per-frame automation lane is declared **on a control**
(`{:name cutoff :has-lane true}`), which keeps the control an ordinary block-rate
scalar in the ABI and adds an *optional* lane beside it. The DSP reads the lane
with the compile-time accessor `(lane control)` — an ordinary `:buffer-load` from
the flat `:lanes` region, so D28's "no new IR op" still holds — and composes it
with the scalar itself; nothing is hard-wired. `:lane` keeps its existing meaning
(lane-only, mandatory region). The accessor is refused in a callee module (a
caller composes at its own call site) and for a control that carries no lane;
`:lane` and `:has-lane` on one control is refused.

**Rationale.** `:lane` removes its control from the ABI's scalar list, so
`binding-plan` — which iterates `:controls` — never sees it: a lane became a
second, parallel kind of declaration that the parameter layer, the plan, and the
pool each had to learn about separately. Binding a lane to a control makes the
lane inherit the control's name, scope, parameter binding, and default, so no
orphan lane can exist and `runtime-config` needs no new scope vocabulary. It also
lets `:has-lane` be genuinely optional: a region the host never fills is built
zeroed, so the scalar stands.

**Consequences.** Composition is the client's, so replacement and multiplication
cost no new mechanism — a hard-wired `scalar + lane` would have made replacement
need the *post-matrix* effective scalar, an ordering coupling. The wider rule:
when the compiler exposes a value it derived, it does so through a compile-time
accessor, never by an internal name the client must spell. The lane accessor
works only where the lane region lives — the entry process — so a wrapper that
wants to expose automation declares the lane on its own control and composes
before calling down.

## D47 — Lower definitions as functions with calls; inlining is opt-in

**Status.** In force. All four stages are done: the JVM, JavaScript and Wasm emit and
call definitions of their own, so a process on any of them keeps its call graph and
copies only what was asked. The interpreter is unchanged — it always called, and
remains the oracle.

**Context.** `dsp/compile` lowers a unit to portable IR and then, for every
target except the interpreter, runs `ir/inline-calls`: the interpreter keeps the
call graph (`:call` ops plus `:definitions`), while the JVM, JavaScript and Wasm
backends receive one flat function with every callee body copied in at each call
site. `inline-calls`'s own docstring says it exists "for the source-emitting
backends" — it is a workaround for those backends not being able to call, chosen
as the default on the assumption that inlining would be fastest.

**Decision.** Lower `dsp/fn` and `dsp/defn` as **separate functions with real
calls, on every target, by default**, and make inlining a per-definition,
opt-in request:

- the *callee* declares it, with `^:inline` metadata on the `dsp/defn`/`dsp/fn`
  form — a property of the function ("this is small and hot, copy me"), not of a
  particular call site;
- the linker carries the flag onto the call expression, so
  `(ir/inline-calls ir inline-set)` copies exactly the requested definitions and
  leaves the rest as calls. Partial inlining is well defined: an inlined body may
  still contain `:call` ops;
- with nothing annotated the pass is a no-op, which is the new default;
- the interpreter is unchanged — it already executes calls, so it becomes the
  *un-optimised* shape rather than a special case.

**Calling convention.** The one the interpreter already implements
(`targets/interpreter`'s `evaluate-call`): a callee is called with its arguments
plus its **state partition base**, and a call site passes
`caller_base + call_site_base`, so nesting composes. Callees keep zero-based
state indices (D19); only the base is threaded.

**Per backend.** Each source-emitting backend gains:

1. one function per definition (the process entry keeps its own ABI);
2. `:call` emission — an invocation passing `base + call_site_base`;
3. **state access relative to a runtime base** instead of a compile-time
   constant.

Item 3 is the real work, and the hidden cost of the change: with everything
inlined, every state access is `arena[constant]`; a call makes it
`arena[base + constant]` — one extra parameter and one add per access, on the
JVM, JavaScript and Wasm alike. The JVM is the easiest first target:
`targets/jvm.clj` already generates one class with a `public static invoke` per
*function* IR, so callees want to become static methods on the process class and
`:call` an `invokestatic` with an extra `int`.

**Rationale.**

- **It removes the artifact-size cliff, which is measured.** Because inlining
  happens before codegen, a fused `(poly n voice)` is one method *n times larger*:
  on the JVM it crosses HotSpot's huge-method threshold between n=12 and n=14 and
  is never JIT-compiled afterwards (13× slower than a pool of the same size), and
  V8 bails between n=16 and n=24 (D45's *Measurements*). With calls, `poly` and
  `unison` emit *call sites* rather than body copies, so the entry function stops
  growing with n — which also removes the main measured objection to D45's fused
  lowering.
- **Artifact size becomes O(code) instead of O(code × call sites)** — most
  visible on JavaScript and Wasm.
- **The engine's inliner is profile-guided; ours is static and global.** HotSpot
  and V8 choose what to inline from a profile, within their own size budgets, so
  calls by default lets them inline the hot small helper and refuse the giant one
  — the choice we currently make once, blindly, for a whole artifact.
- **It removes an oracle asymmetry.** Today the reference interpreter executes
  calls while the compiled targets execute inlined copies, so the oracle and the
  things it certifies run structurally different IR. Calls by default gives one
  IR and four execution strategies, and demotes `inline-calls` to an optimisation
  that must merely preserve semantics.
- **Wasm needs no special treatment.** An engine that inlines little simply makes
  `^:inline` worth using, and the user annotates the functions that matter.

**Consequences.**

- A call-heavy voice on Wasm pays per-call cost a straight-line method did not;
  `^:inline` on the important functions is the answer, and the statement budget
  guarding `inline-calls` (20000) becomes a guard on explicit requests rather
  than a diagnostic on a pathological graph.
- `:call` is already in the effect vocabulary, so `validate-runtime-policy!` and
  the real-time checks keep working: a call site carries its callee's effects.
- The change is verifiable with what already exists — the conformance matrix
  compares interpreter/JVM/JS/Wasm bit-for-bit — and the browser bench (D45)
  should gain a **calls-versus-inlined axis** before any default is flipped on
  Wasm.

**Stage 1 (implemented).** The request travels: `^:inline` on the definition (or
`:inline true` in the spec map) → `:inline` in the normalized descriptor →
`:inline` on the `:call` operation, via `signatures` and `lower-call`.
`ir/inline-calls` now takes the **set of ids to inline**; a definition outside it
keeps being called and stays in `:definitions` (its body is still scanned, since
it may call one that is inlined), and the entry is never retained as a callee.
`dsp/compile` still passes every id, so no artifact changed.

**Stage 2 design (the JVM), revised after reading the lowering.** A callee becomes
a **static method** on the process class. The ABI is far smaller than a process
preamble, because a callee can reach nothing but its own params and state:
`lower-definitions` builds a callee's environment with no `:bindings`, so input,
output, controls, lanes and the frame index are *unreachable in a callee by
construction*. `context-bindings` does give every definition `frames` and
`sample-rate` (under whatever names the definition chose), so the method takes
those and the two base registers:

    float-state int-state float-base int-base frames sample-rate args…

- **State access becomes `base + constant`.** `state-load`/`state-store` are
  emitted today as `aload <array slot>; ldc <physical index>; faload/aload`; a
  callee's gain `iload <base slot>` and `iadd`. The **entry's own accesses stay
  base-free** — its base is 0 — so the process method's hot loop does not pay for
  any of this, and only a callee method's layout entries carry a `:base-slot`.
- **The constant is a physical index.** A call site's `:state-base` is a *logical*
  index into the enclosing arena, and the two storages are physically separate, so
  the backend resolves it per storage: the physical index of that partition's
  first `:float` (resp. `:int`) slot, from the same `state-layout` the entry
  already computes. Relative physical positions inside a partition are stable
  under rebasing, so a callee's own zero-based indices add to the base directly.
- **Why two bases.** The arena is one *logical* index space but is realized as two
  physical arrays (`:float`, `:int`), so a single logical base cannot be added to
  a physical index. A call site derives each callee base from its own base plus
  the number of that storage's slots below its logical base — a compile-time
  constant, because the caller's arena (its own state, then each partition in
  call order) is statically laid out. Only the caller's own base is dynamic, so
  `callee_base = own_base + constant`.
- **Why not specialize per base.** D19 gives *every* call site its own partition,
  so one method per base would mean one per call site — the same code size as
  inlining, and the cliff would return in a new form. (It would still dodge the
  huge-method rule, each method staying small, so keep it as a fallback.)
- **Definitions a target cannot call**, inlined even without a request: a
  **record return** (a method returns one value), and a definition with **scratch
  arrays** — `lower-process` seeds the entry's scratch offsets after the whole
  state region, but the function path never resets them, so a callee's arrays
  start at 0 and overlap its own state, and a partition covers state only
  (`module-state-sizes` sums `state-total`). Both are pre-existing and inlining
  does not rebase them either. Making scratch callable is therefore a *lowering*
  change — seed function scratch after its own state, count it into the partition
  — and worth its own slice, because a delay line is the ordinary case.
- `dsp.clj` therefore decides per target: `effective = requested ∪ uncallable` for
  a target that can call, `all` for one that cannot. The interpreter needs no
  change: it already calls, and becomes the *un-optimized* shape.
- **insn facts, checked.** A method may call a sibling of the class being defined
  with `[:invokestatic <owner> "<name>" <desc-vector>]`. The owner must be a
  *runtime* symbol — a dot-bearing symbol literal compiles as a class reference
  and a bare method-name symbol as a Var — and the method name a string.
  Class and method names must be built at emit time anyway, since the class does
  not exist until `insn/define` runs.

**Order.** Stage 2 JVM first (the smallest delta, and the engine's own inliner is
observable there), then JavaScript, then Wasm; keeping `inline-calls` working
throughout so each change lands as a flag flip rather than a rewrite.

- **Stage 2, built.** `targets/jvm.clj` emits one static method per retained
definition, `call-instructions` emits `[:invokestatic <class-symbol> "<method>"
<desc>]` in expression and statement position, and the entry's own state accesses
stay base-free while a callee's carry a `:base-slot`. `dsp.clj` gained
`calling-targets` (`#{:jvm}` for now), `requested-inline-ids` and `prepare-ir`, and
`ir.clj` gained `uncallable-definition-ids` and `:inline` on a lowered definition.
Two things had to be got right, and the first attempts got both wrong: a call's
**descriptor must carry the same ABI prefix as the method's** (a descriptor of just
the formal parameters leaves the emitted pushes unbalanced, which ASM reports as a
frame-merge failure with no mention of the call), and `partition-bases` must
turn an unused storage's `nil` into 0.
- **What verified it.** The catalogue, in both directions: 189 tests / 1708
assertions with the JVM calling, and a bite-check that stubs `partition-bases` to
zero — 14 conformance failures, from `:call-arena` and `:call-nested` among others.
Those two new cases are the ones a mis-derived base breaks: two partitions of one
stateful callee with the entry's own state between them, a callee whose state is
integer (so its float base is unobservable), and a callee that passes on a base it
derived.
- **Not measured yet.** The testbed bench is browser-side, so it can measure the
JavaScript and Wasm flips but not this one. What the JVM flip removes is *method
size* — the D45 cliff was a JIT refusing to compile a huge method — so a JVM timing
measurement is still owed; `harness/fixtures` already has an allocation meter to
build one on.
- **A function entry stays flat** even on a calling target: its class has no state
arrays, so there is no arena for a callee's partition. D19's partitions exist only
on the process path, which is where the cliff was measured.
- **Reload can leave an inconsistent image** across a backend change like this one:
after `jvm.clj` and `dsp.clj` were edited in the same pass, the JVM kept reporting a
frame-merge failure that a `compile-process` call in the same image did not
reproduce, and only a dev-server restart settled it. The generated-class path is
worth a restart whenever a backend and its dispatch change together.

**Measurements (called against inlined).** `harness/bench.clj` compiles the same
`(poly N voice)` twice for `:jvm` — one linked unit as it is, and the same unit with
every definition flagged `:inline true`, which is the lowering every target had
before stage 2 — and times both with criterium, round-robin, through `dsp/process!`
on `fixtures/sine-voice`'s own schedule. 128-frame blocks, milliseconds per block;
the arms' first block is compared bit for bit and is identical and finite in every
row, so the two are the same computation and the comparison means something.

| voices | called ms/block | inlined ms/block | called bytes | inlined bytes |
|---|---|---|---|---|
| 4 | 0.046 ± 0.000 | 0.039 ± 0.000 | 1 935 | 17 784 |
| 8 | 0.085 ± 0.002 | 0.072 ± 0.001 | 2 073 | 56 692 |
| 12 | 0.127 ± 0.005 | 0.106 ± 0.004 | 2 213 | 117 753 |
| 14 | 0.165 ± 0.016 | **3.858 ± 0.438** | 2 283 | 156 461 |
| 16 | 0.166 ± 0.002 | 4.057 ± 0.216 | 2 353 | 200 569 |
| 24 | 0.248 ± 0.003 | 6.136 ± 0.454 | 2 633 | 430 997 |
| 32 | 0.325 ± 0.003 | 8.323 ± 0.759 | 2 913 | **747 805** |

1. **The cliff is still exactly where D45 found it — between N=12 and N=14 — and it is
   still 36× at the crossing** (0.106 → 3.858). D47's diagnosis holds: the cliff
   belonged to inlining, and the called arm does not have it. That column is linear
   end to end, 0.046 at N=4 to 0.325 at N=32.
2. **Calling costs about 15% while the engine can still inline** — 0.046 against
   0.039 at N=4, 0.127 against 0.106 at 12. That is the price of a floor: above the
   crossing the called arm is 23× faster at 14 and 26× at 32, and it never stops
   scaling.
3. **Artifact size is O(code), not O(code × call sites).** The called class goes from
   1 935 to 2 913 bytes across the whole range — 25 bytes per voice, which is the call
   sites — while the inlined class reaches **747 805 bytes, 256× larger**, a single
   ~700 KB method that the engine has long since stopped compiling. (Checked: the
   inlined arm is fully inlined at every N in the table — `:definitions` is empty — so
   nothing is silently falling back to calls.)

Fairness notes. Both arms run through `dsp/process!`, the public context adapter, so
the numbers include the host-side call and its checks — equally in both arms, and
small beside a 128-frame block. The schedule is `fixtures/sine-voice-description`'s
(`attack` 0.005, `decay` 0.1, `sustain` 0.7, `release` 0.2) with every gate on and
`frequency` 440, given identically to both. The *absolute* numbers are not comparable
with D45's page-driven table: that drove N pool *instances* through the host's synth
layer and this drives one `(poly N voice)` artifact, and the schedules differ too.

The browser side had no such axis when this was taken: the testbed served
`bench-*`/`fused-*` for the pool-against-fused question only. Stage 3 below added
these same two arms there, for JavaScript and for Wasm when it flips. `bench.clj`
later gained the *pool* arm as well (N instances of the voice artifact driven by
`synth/pool`), so D45 now carries the pool-against-called-fusion table for all three
targets — the measurement that decided to ship fusion as a chunked option with the
pool as the default.

**Stage 3, built (JavaScript).** `targets/js.clj` emits one `letfn` binding per
retained definition, and `expression-form` emits a call with the partition base, the
two context values and the arguments; the emitted functions close over the instance's
arena, so unlike the JVM they need no array parameters. The arena is one array here,
so a partition base is its plain offset and no per-storage resolution is needed.
`calling-targets` is now `#{:jvm :js}`.

Two traps, both found the hard way, both worth knowing before Wasm:

- **A function cannot bind a dynamic var and then emit.** `with-module-names` was a
  function first, so its argument — the emitted form — was built *before* the binding
  took effect, and the call site saw `*callee*` as nil. It has to be a macro: the
  emitter resolves a callee's name *while it builds the form*.
- **Inside `letfn`, a binding is `(name [args] body)`, not a `defn` form.** Emitting
  `defn` made Squint name the entry itself, and two of them collided as
  `Identifier 'defn_6' has already been declared` — a `SyntaxError` only a real engine
  reports, which the JVM-side test (it greps the emitted source) could not see.

**Measurements (JavaScript, called against inlined).** The testbed grew the same two
arms — `?inline=1` on `fused-bundle`/`fused-source`, and the page now times three
lowerings instead of two — and `harness/browser.clj` gained `bench!`, so this is
repeatable rather than a REPL session. Each arm's failure is reported as data.

| voices | js pool | js called | js inlined |
|---|---|---|---|
| 4 | 0.227 | 0.154 | 0.150 |
| 8 | 0.338 | 0.211 | 0.396 |
| 12 | 0.484 | 0.331 | 0.619 |
| 16 | 0.671 | 0.436 | 0.908 |
| 24 | 1.212 | 0.663 | **4.958** |
| 32 | 1.595 | 0.846 | *cannot be built* |

1. **The JavaScript cliff is between N=16 and N=24** — 0.908 → 4.958, a 5.5× step —
   which is exactly where D45 put it, and the called column is linear throughout.
2. **Calling is free on V8 while it can still optimise** — 0.154 against 0.150 at N=4,
   unlike the JVM's ~15%. But the inlined arm *regresses before* the cliff too: 1.9×
   slower by N=8 and 2.1× by N=16, so V8 charges for the big function gradually rather
   than all at once.
3. **At N=32 the inlined lowering cannot be compiled at all**: `dsp/compile` overflows
   Squint's recursive compiler (`StackOverflowError`), the endpoint answers 500 with an
   empty body, and the page's `import` of that empty body produces a module with no
   `createKernel`. The ceiling is not "the engine gives up" — the artifact cannot be
   produced.

Wasm is unaffected and still flat: its two arms are the same lowering, so its column
is meaningless until stage 4, and it again shows **no cliff** (1.17 at N=32, D45's
finding).

**Stage 4, built (Wasm).** `targets/wasm.clj` emits one function per retained
definition and a `call` at each site. Two things differ from the other targets, both
because a Wasm instance's memory is the module's own: there is no arena *parameter*
(the emitted functions close over nothing — they index linear memory directly), and
the ABI is `base frames sample-rate args…`, the base being the only state a callee
cannot recover from its own code.

Two bugs, and both are worth knowing:

- **The callee first used the module's layout instead of its own.** `*state-layout*`
  is the enclosing arena's (absolute byte offsets), and a callee's body must be
  emitted against its *partition's* layout (offsets relative to the base it is
  handed) — otherwise every state access is that far off again. `definition-layout`
  is the fix: `state-memory-layout` over a definition's own slots starting at zero.
- **The emitted functions had no `(result …)`**, so a call pushed nothing and every
  surrounding expression failed to validate. The JVM-side test (which greps the
  emitted text) was satisfied; WABT was not. Exactly like stage 3's `letfn`/`defn`
  trap: on these targets only a real engine or validator sees the mistake.

**The invariant Wasm alone depends on.** A callee is one function serving every call
site, so its partition's layout has to be the *enclosing* layout translated by the
base it is handed. With per-slot alignment that is false: aligning `base + x` equals
`base + align(x)` only while the base and every entry offset are multiples of the
element size, and in an f64 block an int slot is four bytes wide. `state-memory-layout`
now places every slot at the *block's* element alignment, which costs a little padding
and makes the translation hold (in f32 the alignment is 4 either way, so nothing
changes there). This is a real hazard, not a hypothetical one, and
`call-fixture/mixed-state` exists to make it bite: its float slot follows its int slot,
and `call-fixture-int-led` puts that partition four bytes into an f64 arena. Reverting
the alignment makes the new `:call-int-led` catalog case fail — on Wasm, in f64, one
wrong sample — and nothing else; restoring it passes.

**Measurements (Wasm).** The same driver with the same 128-frame blocks and the same
schedule as stage 3's table above; milliseconds per block. `bench!` reports both
browser hosts, and the JavaScript columns are unchanged.

| voices | wasm pool | wasm called | wasm inlined |
|---|---|---|---|
| 4 | 0.142 | 0.128 | 0.117 |
| 8 | 0.268 | 0.248 | 0.230 |
| 12 | 0.399 | 0.370 | 0.342 |
| 16 | 0.562 | 0.493 | 0.458 |
| 24 | 0.796 | 0.740 | 0.683 |
| 32 | 1.077 | 0.980 | 0.908 |

1. **There is no cliff, and calling is a flat ~7%** — 0.128 against 0.117 at N=4, 0.980
   against 0.908 at N=32. Compare that with the JVM (a 36× cliff, ~15% below it) and
   JavaScript (a 5.5× cliff, free below it until the artifact cannot be built at all).
2. **That is why Wasm was last, and why it is the easiest**: the reason to call is not
   speed here — V8 and the JVM both reward the smaller function eventually — but that a
   definition is emitted once instead of once per call site, and that the artifact stays
   O(code). A user who wants those 7% back can still write `^:inline`.

**Open — does the fused path still earn its keep?** With inlining reachable from the
source on every target, a definition can be copied exactly where it is worth copying,
which is the question this decision leaves open: whether the *fused* lowering — one
kernel per instrument instead of a pool of voices (D45) — is still worth having, or
whether a pool plus `^:inline` covers it. Not decided.

---

## D48 — Operators take the operand counts `clojure.core` takes, where folding is free

**Status.** In force (front end only: `ir.clj` lowering).

**Context.** `(+ a b c)` was a lowering error — `lower-binary` required exactly two
operands for arithmetic and comparison alike, and `and`/`or` and the bitwise
operators were the same. A three-term sum had to be written nested, which is
friction in the DSL's own fixtures and library, and a gratuitous divergence from
the host language whose forms the DSL is written in. The playground's other
languages (`pygen`, `cgen`) already accept the variadic form.

**Decision.** A variadic operator form lowers to the fold it means: `(+ a b c)`
becomes `(+ (+ a b) c)`. Where that is a pure syntactic fold the operator is now
variadic; where it is not, it stays binary and says so.

- **2 or more operands:** `+ - * /`, `and or` (operands still `:boolean`),
  `bit-and bit-or bit-xor`.
- **Exactly 2, unchanged:** the comparisons `= not= < <= > >=` — `(< a b c)` means
  `(and (< a b) (< b c))`, which reads `b` twice, and reading it once means
  hoisting it into a local first. Also the shifts, which have no folding meaning.
- **Exactly 1, unchanged:** `not`, `bit-not`.
- **Exactly 2, unchanged:** `min`, `max` — a builtin's arity *is* its signature
  table entry, and a variadic signature has no representation there yet.
- **Refused:** unary `(- x)`/`(/ x)` and the identities `(+)`/`(*)`. N-ary means
  2+; Clojure's one- and zero-operand meanings are a separate decision. So is the
  degenerate `(< a)` → true.

**Rationale.** The fold is lowering-only. The IR keeps its two-operand `:binary`,
`:logical` and `:bitwise` nodes, so no backend, no ABI, no artifact and no WAT
changed — and the test asserts exactly that, by counting binary nodes (three
operands are two nodes). Each operand is lowered once, left to right, which is the
order the nested form had, so a stateful or effectful operand still evaluates
exactly once. Refusing the chained comparison keeps the honest option available
(hoist the middle operand, then chain) rather than pretending it is free, and
refusing mixed int/float is deliberate: promotion is a question about the value
domain, not about arity.

**Consequences.** `(min a b c)` remains an error while `(bit-or a b c)` works —
the one asymmetry a reader will notice, and a variadic builtin signature is the
slice that removes it. Mixed int/float arithmetic (`(+ x 1)`) is still refused
rather than promoted; that decision is still open and is *about* types, not
arity. The catalog gained two function cases, so the fold is checked on every
target (browser included) against the interpreter oracle.

---

## D49 — A shared control is one region for the pool, shaped like a lane region

**Status.** Implemented on all four targets, through the pool (M5a, M5b, M5c):
the language, the ABI, the four kernels' delivery, and the pool's parameter
mapping. See `PHASES.md` M5 and `MODULATION.md`'s shared-region section.

**Decision.** An instrument-level (broadcast) parameter is **one state variable
every voice reads**. A DSP control may declare `:shared true`: it leaves the ABI's
scalar control list, and its name becomes a read of the process's `:shared` region.
Each target represents that region where it can be addressed — a real array passed
by reference on the interpreter, the JVM and JavaScript; a region of its own linear
memory, passed by *address*, on Wasm (which is why a Wasm shared value is copied
into each instance even though the value is one). The region is mandatory when a
shared control is declared: it *is* the control's value, not an optional layer.

**Why the lane region's shape.** One `:frame-capacity`-frame slice per shared
control, the same stride and the same compile-time bases as the lane region. A
shared read is then an ordinary `:frame-offset` `:buffer-load` with a different
`:buffer-id`: no new IR op, no new addressing, and no new emitter on any target —
the addressing came for free with D28. The second consequence is that a host may
write a per-frame *global* curve into the same slice, which is what the M-series'
kernel-side generator wants to do.

**What it costs.** `shared-count * 4096` bytes: once per *pool* on the reference
hosts, but once per *instance* on Wasm, so a browser voice pool pays
`voices * shared-count * 4 KB` — the same order as the lane region it mirrors, and
the same reason (each Wasm instance owns its memory). On Wasm the host also copies
the pool's region into each instance before running it: the one place that host
cannot pass by reference, and `voices * frames` element copies per block when a
host writes a per-frame global.

**Alternatives considered.**

- **A scalar region with a new IR op.** One element per control, read without a
  frame term. It needs a new op — every buffer access in portable IR is
  frame-relative by construction — and a new emitter case on all four targets, to
  save `stride - frames` unused bytes per control. Rejected: the lane-shaped region
  is strictly less machinery and comes with per-frame globals.
- **Keeping the pool's per-voice fan-out.** Today's mechanism: resolve the controls
  of every sounding voice when a shared parameter changes. Correct, but it leaves
  no place for a kernel-side generator to write a global, and its cost is
  `voices * plan` per change instead of one region write. Kept as the **migration
  path**: a `:shared` parameter bound to an ordinary control still fans out, so
  declaring the DSP control is what switches a parameter to the shared region.
- **A shared *lane* in the same region.** A lane on a shared control is still
  delivered per voice (M4c's shape). Deferred: the region could host it, but the
  choice must be the DSP's — the pool cannot change a compiled buffer id — and a
  shared scalar *plus* a shared lane on one control wants two slots, which nothing
  needs yet.

## D50 — A fused instance's control mapping belongs to the kernel, and the pool stays base-shaped

**Status.** Implemented for the JVM kernel (M6a, seam 2): `hosts/fusion/fused-mapping`
is the mapping, `hosts/jvm/fused-jvm-kernel` applies it, and three tests pin it.

**The problem.** The pool's control plan is one entry per *voice* control, in the
base ABI's order, because that is what a description's parameters bind to
(`binding-plan` resolves them by control name) and what `note-on!` and
`set-parameter!` write. A fused instance's interface is a different thing
entirely: `poly` expands the per-slot controls to `gate_0`, `gate_1`, ... and
hoists the rest to the top. A pool that had to speak the fused interface would
have to know which lowering produced its kernel, and a description could not name
`gate_0` even if it wanted to.

**The decision.** The mapping lives in the **kernel**, which is the one component
that holds both interfaces, and the pool never learns that several of its voices
share a process. Concretely:

- `fused-mapping` takes the base controls in ABI order, the names that were
  expanded per slot, the fused ABI's controls, and the slot count; it returns, for
  every *base* position, either the fused ABI position it was hoisted to or its
  index inside a slot's block, and it refuses an interface that is not the shape
  `poly` expands to — at construction, not in the audio.
- The kernel precomputes one row per slot at construction, so the write path is
  still one loop with no branch and no per-note arithmetic, exactly like the plain
  path's identity row. A base control's value therefore reaches the slot whose
  handle wrote it, which is what makes two seams' worth of work a *pool* rather
  than a sum of voices.
- The mapping is pure data in a `.cljc`, so the browser host computes the same
  answer in M6c from the two interfaces *it* holds: the fused artifact's ABI, and
  the base control names, which travel in the runtime config beside the plan.

**Which controls may be hoisted.** Hoisting is faithful exactly when every voice
is handed the same value, so the rule for the option is: expand every control the
pool writes **per voice** — a runtime control (`gate`, `frequency`, ...) or a
parameter whose plan storage is `:per-voice` — and hoist only what the pool writes
uniformly, which is a shared control (D49) or a shared parameter on a scalar
control (the migration path). Anything else hands all of a fused instance's slots
whatever slot wrote last, silently. The seam-2 tests deliberately use a *wider*
hoist than the option may, and stay correct because the pool does write one value
to every voice for a parameter no host varies per voice.

**What building it found.** `poly` redeclares a voice's controls as
`{:name name}` — by name alone — so a fused interface **loses the voice's
declarations**. Measured: a voice whose control is `:shared true` compiles fused
with that control as an ordinary scalar and an empty shared region, and a voice
with a lane is refused outright by the IR ("lane is only available in the entry
process"). Reading the same one line, `:default` and `:has-lane` are dropped the
same way. So the instrument option cannot simply lower a voice to `(poly S voice)`
and be done: either `poly` learns to carry a control's declaration through, or the
option refuses a voice that declares a shared control or a lane. The mapping is
not the place to fix that — it maps *names*, and can only report the interface it
was handed.

**Alternatives considered.**

- **A pool-side mapping.** The pool would keep a slot-shaped plan and write slices
  itself. Rejected: it puts knowledge of the fused ABI on the side that speaks the
  base plan, and it would have to be repeated in every host that embeds a pool.
- **Rebuilding the plan over the fused ABI.** Rejected: a parameter binds by
  control name, and `gate_0` is not a name a description can bind to; it would
  also make one description mean different things depending on the lowering.
- **Resolving names per write in the kernel.** Rejected: a string lookup on the
  note path for information that is fixed at construction.

## D51 — Per-slot reset needs the target to publish its call partitions

**Status.** Implemented on the JVM (M6a, seam 3): `:call-partitions` on the IR,
`:partitions` on the JVM artifact, `:reset-partition` on the instance, applied by
`fused-jvm-kernel`'s `reset-voice!`.

**The problem.** Seam 2 made a fused instance's *controls* land in the right slot.
State is the other half: one instance holds `slots` state partitions, and stealing
or retriggering slot `k` must restore partition `k` alone. D19 already gives every
call site a private partition — nothing exposed the ranges, so the only reset
available was the instance's, which restarts every slot's envelope. That is not a
theoretical difference: with the whole-instance reset the bite check's fused block
collapses to silence where the pool is still sounding a note.

**The decision.** The *target* publishes the ranges, because the partition is its
arena layout, and the *kernel* applies them, because it is what knows a handle is
a slot. Concretely:

- The lowering already allocates a partition per call site in evaluation order;
  it now records `{:function-id :base :size}` as it goes. For a `(poly S voice)`
  process that vector *is* slot order, which is the whole point.
- The JVM target resolves each logical range to a physical range per storage
  (`:partitions` on the artifact, in the same order), and the instance keeps each
  partition's initial values, so a per-slot reset is one `arraycopy` into one
  range. Nothing is added to the audio path.
- A fused kernel checks `slots = (count (:partitions artifact))` at construction.
  A mismatch means a slot cannot be reset on its own, and that is exactly the
  invariant "the entry's call sites are the slots".

**What it cost to get right.** A state entry may own several slots: a delay line's
buffer is *one* entry with `:size 64`, not 64 entries. The first version built a
range from the entries' first indices, so a partition's float range looked like
`(0 1 65)` and the contiguity check — which the same version added — fired. The
check was right to fire; the range was wrong. An engine that publishes layout must
expand `:size`, and saying so out loud is why the check is worth having.

**Alternatives considered.**

- **Derive the initial state at reset time from the IR.** Correct, and about 50 KB
  of lazy-seq walking per call — the same reason the instance captures its
  `initial-*` arrays once.
- **Emit a reset method per partition.** A new emitter path on every target, for
  state the host already holds as two arrays.
- **Reset the whole instance.** What the kernel did before, and what the bite
  check shows: a steal in one slot restarts its neighbours' envelopes.

## D52 — Fusion is a compile option, and `poly` carries a voice's control declarations

**Status.** Implemented for the JVM: `{:fusion {:slots S}}` in a synth's compile
options (`synth-bundle`, `compile-synth`), consumed by `host-kernel`'s `:fusion`.
The `poly` fix it needed is in `combinators.clj`.

**Where it sits.** A compile option, sibling of `:target`/`:precision`/
`:containment` -- and the only one *consumed* rather than passed on, because it
changes what is compiled. The others travel to `dsp/compile`, whose options are
closed, so a stray `:fusion` reaching it would fail loudly rather than be ignored.

**The split.** `synth-bundle` compiles the *base* artifact first and builds the
runtime config from it: plan, lane scopes, shared region, parameter declarations.
Everything the pool speaks stays base-shaped (D50). Then, when asked, it compiles
the *fused* one -- `(poly S voice)` -- and that is the artifact a kernel is built
from. The runtime carries `{:slots S :controls [...] :per-slot [...]}` so a host
that holds only the artifact can rebuild the mapping, which is what M6c's browser
will do.

**Which controls are expanded.** The ones a pool writes per *voice*: runtime
storage (`gate`, `frequency`, ...) and per-voice parameters. Hoisting is faithful
only for what the pool writes uniformly, so a shared control is hoisted -- and
`delay`/`mix`, declared per-voice parameters, are expanded per slot, so each slot
keeps its own value rather than the last one written.

**What `poly` had to learn.** It redeclared a voice's controls as `{:name name}`,
which dropped every declaration (D50 measured what that costs). It now carries the
voice's own declaration to each expanded name, so a `:shared` control stays shared
in the fused interface -- `shared-gain` keeps its shared region -- and it refuses
two things it cannot honour: expanding a shared control per slot (one region for
the whole process), and expanding a lane per slot (a lane belongs to the entry
process).

**Costs.** Two compilations instead of one, and an artifact that grows with
`slots` (D45's measurements). A voice with nothing per slot has nothing to fuse and
is refused when the kernel is built, rather than compiled into `slots` copies of
one signal.

**Verified.** A pool over the option renders bit-for-bit what a pool of separate
voices renders at one sounding voice across three blocks; the fused artifact has
one state partition per slot; the runtime carries the fusion description and a
plain bundle carries none; the option is closed and checked.

## D53 — Fused slots are skipped by a per-call-site gate latch

**Status.** Implemented (M6b). `comb/guard` in `omkamra.dsp.combinators`;
`synth-bundle` applies it when fusing (`{:fusion {:slots S :guard true}}`, default
true); tests in `tests/combinators.clj` and `tests/synth.clj`.

**The problem.** A fused process evaluates every slot it was compiled with, while
a pool calls its kernel only for sounding voices. So the sign of the
pool-against-fusion comparison is a property of the patch (D45's occupancy table):
at a full patch fusion is ~2× faster, and at two voices of a 32-slot chunk it was
7.5× *slower*. A silent slot costing a whole voice is what makes fusion unusable
for sparse patches.

**The decision.** A slot a pool has not gated is **not evaluated**. The lowering is
a per-call-site latch, `(comb/guard voice)`: the wrapper owns one boolean state
slot that latches the first time its `gate` control rises above zero, is cleared by
the slot's reset, and guards the wrapped voice's evaluation. `poly` calls the
guarded voice once per slot, so each slot's latch lives in that call's state
partition (D19) — which is exactly what makes it clearable by `reset-voice!` when a
slot is stolen or freed.

**Why a latch and not a bare gate check.** A pool closes a voice's gate to
*release* it, so `(if (> gate 0) … 0)` would cut every release tail and break the
property the whole fusion question rests on: a fused lowering must render what the
pool renders, bit for bit. The latch distinguishes the two reasons a gate is shut —
never opened (skip) and opened then closed (keep evaluating) — and the reset is the
third state (free again). A released slot's tail is therefore bit-identical to the
plain pool's, which `a-fused-pool-preserves-a-release-tail` pins across 24 blocks.

**Why not a host-supplied enable.** A per-slot control or region the pool wrote
would change the ABI, need a per-target emitter, and leave the interpreter and the
browser hosts to implement it separately. The latch is ordinary DSP the language
already has (scalar state, `set!`, a comparison) and a definition value built by a
combinator, so it needs **no IR, ABI, artifact, kernel, or backend change** — the
`fused-mapping`, the partition ranges, and every emitter are untouched, and M6c's
JavaScript and Wasm hosts inherit it with no work. It is also a public combinator,
so `unison`, `mix`, or a hand-built graph can use it.

**Measurements** (JVM, f32, 128-frame blocks, `echo-voice` — a generator with a
64-sample delay line; criterium, best estimate). A pool at capacity 32 against one
fused process at `:slots S`, milliseconds per block:

| slots | sounding | plain pool | guarded fused | unguarded fused |
|---|---|---|---|---|
| 8 | 2 | 0.045 | 0.036 | 0.097 |
| 32 | 2 | 0.045 | 0.045 | 0.467 |
| 8 | 8 | 0.235 | 0.108 | 0.104 |
| 32 | 8 | 0.235 | 0.121 | 0.392 |
| 8 | 32 | 0.825 | 0.447 | 0.438 |
| 32 | 32 | 0.825 | 0.482 | 0.371 |

Two findings. First, **the guard removes the low-occupancy penalty**: `:slots 32`
at two sounding voices falls from 0.467 to 0.045, *equal to the pool*, and at eight
voices from 0.392 to 0.121, half the pool. A guarded single S=N instance therefore
dominates the pool across occupancy (0.045/0.121/0.482 against 0.045/0.235/0.825),
which is the property M6 was after. Second, the guard is not free: at a full patch
it costs ~2% at `:slots 8` and ~30% at `:slots 32`, where the extra branch and
state traffic sit in a proportionally larger method. Even at `:slots 32` the
guarded process still beats the pool (0.482 against 0.825), so `:guard false` is an
escape hatch for a host that wants the bare lowering's last few percent, not the
recommended default. The guarded artifact is 209 bytes larger at one slot and
exposes one more state slot per call site; the interface is unchanged.

**Consequences.** Fusion is now viable as a fixed `:slots` choice across occupancy,
because the per-slot skip removes the worst of the low-occupancy penalty; chunking
is still the lever for the fully-free-instance half (already handled by the
instance key, D50), and `:guard false` restores the bare lowering for a host that
wants the last few percent at a full patch. **Amended by D54** — seam 4 is now
done, so a fused slot publishes its own `:activity` and the pool frees a finished
voice; the same reset clears the loop's latch, so a freed slot is skipped again. A
slot whose gate has never opened since its last reset is still skipped, and a used
slot is held only until its own voice reports that it has finished. The bench's `realistic-row` gained `:sounding` and `:guard` so the table above
is repeatable.

## D54 — A callee may publish a special, and its call partition is the contract

**Status.** Implemented (M6 seam 4). `ir.clj` (`partition-specials`,
`special-mask-of`, the entry no longer rejects a callee's `store-special`); the
process ABI gains `:call-partitions`; the JVM target passes each partition's
specials into `:partitions`; `hosts/jvm.clj` resolves them per slot and
`read-specials!` reads the slot the pool asked about; `synth-bundle` carries the
base voice's specials and asserts every fused slot matches. Tests in
`tests/composition.clj` and `tests/synth.clj`.

**The problem.** A fused voice's `:activity` (D39) would have to live in the call
partition of its slot, and D39 rejected a callee's `store-special` outright
because that slot "would be buried in a call partition where no host reads it".
So a fused process exposed no specials at all, the pool could not learn that a
released slot had finished, and it held every voice until it was stolen — the
tail-freedom fusion gave up. That was the one remaining seam (D45's seam 4), and
it was not "read an offset we know" but "publish the offsets, per slot".

**The decision.** A callee may publish a special, and the **call partition is its
contract**:

- The entry's own specials stay its contract: `:special-mask`/`:special-slots`
  are unchanged and still only describe the entry (indices below
  `:special-count`).
- Each call site that owns a partition records the specials inside it — one per
  bit, the outermost (lowest logical index) wins when a nested module declares
  the same bit — with the logical index and a mask. Because a partition covers
  its whole subtree, a special nested behind the definition the call site names
  is found there. For a `(poly S voice)`, call site `k` is slot `k`.
- The **process ABI** publishes that vector as `:call-partitions`, so it is
  target-neutral; the JVM target resolves each logical index to a physical slot
  and carries it on `:partitions`.
- `read-specials!` for a **fused** voice reads the specials of *its* slot's
  partition, resolved once at kernel construction like every other special
  offset, so the audio path still touches no map.
- `synth-bundle` carries the **base voice's** `:special-mask`/`:special-count`
  (not the fused entry's, which is empty) so the pool's `:activity` position is
  the same plain or fused, and it refuses at compile time if any fused slot's
  mask differs from the voice's — a lowering bug, not a muted readback.

**Why per partition, not the entry.** A fused process is several voices; there is
no single entry special that means "slot 3 has finished". The partition is
exactly the unit that a per-slot reset already restores (D51), so the activity
and the reset share one address space and one lifecycle: the reset that frees a
slot also clears the guard's latch (D53), so a freed slot is immediately
skippable again.

**What this supersedes.** D39's entry-only rejection. Everything else D39 says
stands: the fixed bit, the leading-slot order, the preallocated readback, and the
entry's own `:special-slots` as its contract. The ABI's shape is additive — a new
`:call-partitions` key, and `:partitions` entries may carry `:specials` — so no
existing artifact or host changes meaning.

**Consequences.** A fused pool now frees a finished voice exactly as a plain pool
does (`a-fused-pool-frees-a-released-slot-from-its-own-activity`), restoring the
tail-freedom fusion had given up; the guard and the activity compose, since the
free is a partition reset. A non-fused process that calls a module publishing a
special also records it per partition now, rather than failing: it is published,
not lost, and a host that does not read it is unaffected. The JavaScript and Wasm
*kernels* do not read a fused slot's special yet — that is M6c — but the IR and
ABI they consume already carry it, and they compile a callee's `store-special` as
an ordinary state store as before.

## D55 — The browser fused hosts apply the partitions the ABI already publishes

**Status.** Implemented (M6c). `hosts/fusion.cljc` gains `runtime-mapping` (the
portable `:fusion` → mapping conversion `hosts.clj` now shares); `hosts/js.cljs`
and `hosts/wasm.cljs` take a `:mapping`, share one instance across `slots`
handles, map each slot's controls, restore one call partition on reset, and read
a slot's specials from its partition. `harness/testbed.clj` serves a fused pool
bundle (the base artifact as the interpreter oracle, the fused artifact as the
kernel); `harness/testbed.cljs` adds "JavaScript fused pool" and "Wasm fused
pool". No target, IR, ABI, or bundle-schema change.

**The problem.** M6a–M6b made a *JVM* pool able to hold `slots` voices in one
`(poly slots voice)` process: a base → fused control mapping the kernel applies
(D50), a per-slot reset from partition ranges the target publishes (D51), the
guard (D53), and a per-slot `:activity` readback (D54). The browser kernels got
the guard for free — it is ordinary DSP — but they had none of the other three,
so a fused pool could not run on JavaScript or Wasm.

**The decision.** The browser hosts apply the **same published partitions**, and
because the ABI already carries them (`:call-partitions`, seam 4), the browser
targets need no change:

- `fusion/runtime-mapping` turns the `:fusion` map a bundle carries (base
  controls, per-slot set, `slots`) and the fused artifact's own controls into the
  one mapping both the JVM host and the browser hosts use — so "the fused
  lowering" is one definition, not one per host (the D50 argument, extended).
- The **JavaScript** arena is one flat region indexed by state slot, so a
  partition's `:base`/`:size` *are* its physical range: `create-voice` snapshots
  the arena once, and a reset copies that range back. A `slots` group shares one
  generated object and one key.
- The **Wasm** state region is byte-addressed and its layout is already served in
  `:memory` (an int slot in an f64 block is padded to the block's alignment), so
  the host resolves each partition's byte range from `:state-layout` and copies
  it from an initial snapshot. `slots` handles share one module instance and one
  linear memory.
- `read-specials!` for a fused handle reads the specials of *its* partition, from
  the arena (JS) or a view into the instance's state region (Wasm), in ascending
  bit order — the same contract the JVM host reads.

**Why the host, not the target.** On the JVM the *target* resolves partitions
because the arena is two primitive arrays and a logical index is not a physical
one (D51). JavaScript has one logical arena and Wasm's physical layout is already
published as data, so resolution is a host-side loop over what the ABI and
`:memory` already say. Adding a `:partitions` artifact key and shipping it would
duplicate information the browser already has.

**Consequences.** A fused pool on JavaScript or Wasm renders what a plain pool
renders, frees a released slot from its own `:activity`, and survives a steal
without touching its neighbour — checked differentially against the interpreter
pool over the *base* voice, since the fused process is not itself an oracle. The
checks bite: a whole-instance reset fails the steal check (a neighbour is
clobbered) and an empty readback fails the activity check (the tail is freed a
block early). What remains is the same browser-vs-JVM edge the pool already has:
a fused *instrument* host and the AudioWorklet path are not fused, and per-voice
lanes with fusion are still refused by the interface (D52).

## D56 — The remaining per-process regions are partitioned per call site (F0)

**Status.** Decided (F0). Not implemented: output buffers in F2, a shared lane in
F3, per-voice lanes in F4. This entry fixes the spine those three build on; the
concrete ABI shapes are their decisions.

**The problem.** M6 made a fused process, but only for a module-shaped, lane-free
voice (D52). Two unrelated refusals keep the rest out:

- A **process-shaped** voice — one that writes the caller's output buffers — is
  refused while linking ("DSP modules cannot access the caller's process
  buffers", `linker.clj`). That is every stereo or multi-output voice, including
  `reference-voice`, so no realistic voice can fuse.
- A **lane** is resolved from the entry process, so a `(lane control)` read in a
  callee is refused ("lane is only available in the entry process", `ir.clj`),
  for a shared lane as much as a per-voice one; `poly` additionally refuses a
  per-slot lane.

**The decision.** Every per-process region a callee touches is **owned by the
call site** and carried by the callee ABI, exactly as the state arena already is
(D19):

| region | today | F |
|---|---|---|
| state | partitioned per call site (D19) | — |
| output buffers | the callee may not touch them (linker) | partitioned per call site; the fused entry sums them (F2) |
| lane region | the callee cannot reach it | passed to the callee (one region; F3), partitioned per call site for a per-voice lane (F4) |

A call site that calls a definition touching such a region allocates it a private
region in the enclosing arena (instance memory, preallocated on the audio path),
the callee ABI carries that region's base, and the **caller** decides what the
region means: for a fused voice, the entry sums the slots' buffer regions into
the process's own output; for a lane, the record is the region the host wrote.
This is the same move D19 made for state, applied to the two regions it did not
cover.

**Why per call site, not one shared region.** For the same reason state is
(D19): two slots of one voice are two independent voices, and one buffer would
make them overwrite each other's samples. D19 already answers the aliasing,
reset, and per-slot-lifecycle questions, so the buffer and lane partitions
inherit those answers instead of inventing new ones.

**Why the fused body needs no new scheduling model.** A process-shaped body is
already evaluated **per frame** (D40) and `buffer-store` writes the current
frame, so the fused entry's body is: call each slot (each writing its own region
at this frame), then store the sum of the region's channels to `:output` at this
frame. The private regions are block-shaped (`channels × frame-capacity`) because
the callee addresses them by frame, but only the current frame is summed.

**What F0 deliberately leaves open.** The exact ABI shape — one combined region
with per-buffer offsets versus one region per named buffer, and how many bases a
callee takes — is settled in F2 (and mirrored in F4). Whether `poly` is extended
or a sibling combinator is added is also F2's. F0 fixes only the spine: *regions
are per call site, and the callee ABI carries their bases.*

**Consequences.** The load-bearing change is the linker rule: it must be relaxed
**only** for a call that owns a region, so a call with no region keeps today's
refusal and the error stays honest. The linked unit grows by one region per call
site (instance scratch), and the callee ABI grows by the region bases — a change
felt by every call site on all four targets (D47). The pool stays the default;
none of this changes a non-fused artifact.

## D57 — A fused call site binds its callee's output buffers (`with-buffers`) (F2)

**Status.** Decided (surface settled); implementation in F2a–F2d.

**The form.** A definition body may bind the output buffers a call's callee writes
to names the caller can read:

```clojure
(dsp/with-buffers {:output :left :aux :tail} (voice args…))
(dsp/with-buffers :slot-0 (voice args…))       ; sugar for {:output :slot-0}
```

`with-buffers` is the **only** new form. The bound names are read with the existing
`(buffer-load id channel offset)`, where `offset 0` is the current frame — the same
vocabulary `buffer-store` already uses.

**The sugar is never ambiguous.** The bare form is legal *iff* the callee writes
exactly one output buffer; it binds that buffer. A callee that writes none (a
scalar module) or more than one is refused with a reason naming the map form. So a
bare name always denotes exactly one buffer, and which buffer is never guessed —
the same strictness the descriptor validates declarations with.

**Why a form and not a combinator.** A combinator returns a source form, but what
this changes is *where a callee's output goes* — lowering behaviour, not a body. It
is a DSL form resolved at link time, when the callee's buffer set is known, so the
bare/map choice is made against real data rather than at macro-expansion.

**The mechanism it implies — and why it is small.** A bound buffer is a
per-call-site region, and the region reuses the call-site **state partition**
(D19): a called definition's output buffers live in its own partition, and its
`buffer-store`/`buffer-load` on them lower to state addressing relative to the
partition base the callee is already handed (D47).

- **No new ABI parameter.** The state base already flows to every callee, so
  region passing is free; a target changes only if the region's channel/frame
  addressing differs from the state array's.
- **Uniform per definition.** The link-time refusal relaxes *only* for a call
  inside `with-buffers`, so a called definition always has its buffers bound and
  its relative addressing is the same at every call site. A call with no binding
  keeps today's error ("DSP modules cannot access the caller's process buffers").
- **The per-slot lifecycle is inherited.** Because the region is in the call
  partition, D51's reset and the M6 seam-4 activity readback already cover it.

**What remains for the slices.** F2a fixes the exact region layout (channel stride
× frame capacity) and the interpreter form; F2b the JVM; F2c JS/Wasm; F2d the
reference instrument. Whether `buffer-load` names the region directly or a
declared buffer aliases it is F2a's detail.

## D58 — A process-shaped module returns nothing, and an explicit top-level `:channels` is unit-wide (F2a)

**Status.** Decided; F2a implements it on the interpreter.

**1. A process-shaped callee returns `:void`.** A definition that writes its own
`:output` has no value to return. The descriptor now permits `:return-type :void`
when the definition has a `:process` (the old refusal stays for a scalar
expression entry), the lowerer then omits the `:return` and the return coercion
handles `:void` as "no value", and a call to it is a statement. A process entry
already ignored its (defaulted) return type for a buffer-writing body; making the
void explicit lets a *callee* carry the same shape through `signatures`.
A module's output buffers only exist when the definition is process-shaped: a
plain function with `(buffer-load :input x)` still has no buffer namespace and is
still rejected, so the F2 path cannot silently give every function ports.

**2. An explicit top-level `:channels` is the unit-wide channel policy.**
`descriptor/default-options` always contains `:channels 1`, and
`normalize-options` merges it unconditionally, so `(:channels options)` is never
a trustworthy "was a width declared?" signal — reading it pinned every module to
one channel. `compile` resolves the width from the **raw** maps (an explicit
compile option, then a definition option, then the top definition's
`:process :channels` declaration; otherwise nil to count from the body) and
writes that resolved value into the options module lowering reads, *replacing*
the injected default. When the width is inferred, `:channels` is removed instead,
so each definition counts its own.

`lower` therefore consults, in order: the forced top-level width, the module's own
`:process :channels` declaration, then its body (D40). The container's explicit
declaration is authority — a voice in a stereo instrument is stereo whether or not
its own body would infer it — but **inference does not propagate**: a genuinely
mono helper called from a stereo entry keeps its own width. This is the invariant
that an explicit width takes effect at every layer, and that the default never
masks it.

## D59 — A region is call-partition state, and its writer is handed the frame (F2a rework, F2b)

**Status.** Decided; F2a's region representation was reworked to it, and F2b
implements it on the JVM.

**A region is state, not a buffer.** D57 already said a bound output buffer
"reuses the call-site state partition". F2a's first cut instead gave the entry a
per-call-site `:region` *buffer* (a per-channel array) and overlaid it into the
callee's context. That was a deviation with a cost: the JVM would have needed the
region as a buffer argument — a new ABI shape — and every target its own region
plumbing. The rework is faithful to D57 and smaller everywhere:

- a process-shaped module's output buffers **become state slots** in its own call
  partition — one flat slot of `channels * stride`, the stride being the
  lane/shared region stride, so it is a compile-time constant every target shares;
- its `buffer-store`/`buffer-load` on those buffers lower to
  `:state-store`/`:state-load` with the channel folded into the element index;
- a `with-buffers` region name is an **alias** for the callee's rebased state slot,
  which `buffer-load`/`buffer-store` resolve;

so the interpreter needs no region code at all, and no target does either: the
region is addressed by machinery they already have. The entry's `:buffers` keeps
only its own `:output`; the region is state.

**A region-writing callee is handed the frame.** A callee is a per-sample
function (D40) and has no frame local, but a region is frame-indexed. So a callee
whose `:region-slots` is non-empty gets one extra leading `:int` parameter — the
current frame — pushed between `sample-rate` and its own arguments; a callee with
no regions is unchanged, so ordinary composition pays nothing. The interpreter
needs no equivalent: its callee context is dynamically scoped and already carries
`:frame`. **Bite-checked** on the JVM: passing a constant zero instead leaves only
frame 0 correct and the rest stale.

Two smaller facts fall out. A void-returning module needs a `:void` JVM type
(`jvm-type`), a `return` for it, no `pop` after the call, and a trailing `return`
in its method — all added. And `state-elements`/`create-state` require an array
state's `:init`, so a region carries a zeroed `:init` like the large delay-line
buffers the codebase already materializes.

## D60 — `poly` fuses a process-shaped voice, and the interpreter hosts fused pools (F2c/F2d)

**Status.** Decided; implemented.

**1. `poly` fuses a process-shaped voice by binding its regions.** A voice whose
`:return-type` is `:void` writes its own `:output` rather than returning a sample,
so its copies cannot be summed as values. `poly` now branches on that: each slot's
call is wrapped `(with-buffers :slot-k (voice args...))`, and the fused body sums
the regions into `:output`, one channel at a time (F2, D57). A module-shaped voice
keeps the scalar sum, so the M6 lowering is unchanged. The fused body is
`(do <calls> <per-channel stores>)` and the derived definition carries `:void`.

This moves one requirement onto the voice: it must **declare `:process :channels`**.
The summing is unrolled per channel, and a combinator holds only data — it cannot
count the width the compiler would infer from the body (D40). A process-shaped
voice without the declaration is refused with a reason rather than summed wrongly.

**2. The interpreter hosts fused pools.** A fused pool on any host needs an
oracle, or the JVM and the browser only agree with each other. The interpreter
kernel now applies the same base -> fused control mapping the JVM and browser
kernels do (D50), and resets one slot's partition on its own — using the `:base`/
`:size` the ABI already publishes per call site (`:call-partitions`, M6 seam 4)
rather than new artifact fields. It reads a fused slot's specials from that
partition too. The interpreter is therefore the reference for a fused pool *and*
for a fused instrument, not only for the fused process IR.

**3. The M6b guard stays module-shaped.** `guard` skips a slot by reproducing the
sample it would have produced; a process-shaped voice writes a region instead, so
guarding it is a different mechanism. A process-shaped voice is fused **without**
the guard until that exists — an optimisation withheld, not a correctness change.

**4. Two void-call target facts.** A void call leaves no value: JavaScript must not
pass a stray argument for an absent frame (a `list*` splices only its last
argument, so the optional element and the arguments are one seq), and Wasm must not
`drop` it. Both are the same shape as the JVM's "no `pop`" (D59).

## D61 — a Wasm instance seeds its state, and a partition range comes from layout segments (F2d)

**Status.** Decided; implemented.

**1. A Wasm kernel seeds a fresh voice at creation.** A Wasm module's memory is
zero at instantiation and the IR's initial values reach it only through the
module's own `init`/`reset`. The JavaScript host gets the same effect from the
module's `create`, which allocates the state array already initialised; the Wasm
host had left it to `:reset!` — and the pool resets a voice **only when it steals
one** (`poly-note-on!` passes `stolen?`), so a *fresh* Wasm voice rendered from
zeroed state. That is a host-level divergence from JavaScript, not a fusion bug,
and it is why the plain Wasm voice never matched its JavaScript twin. Both paths
now seed once, right after instantiation: the plain kernel before it returns the
voice, and the fused kernel **before** it snapshots `:initial`, or the per-slot
restore would copy zeros. The snapshot must also happen after a whole-arena seed,
since the seed writes every slot, not just one slot's partition.

**2. A call partition's byte range resolves layout segments, not indices.** The
Wasm host addressed each partition by looking up every arena index in
`[base, base + size)` — but the memory layout holds one entry per *slot*, and a
per-call-site region is a single entry spanning thousands of indices, so the
region's interior had no entry. `min` over the missing entries produced a nil
start (`(long nil)` -> 0), so `:start` collapsed to zero and the reset built a
view at `-state-offset`, restored nothing, and wrote the state region's bytes into
the input region. A partition is now the set of layout segments that *overlap* it.
The bug only appears when a partition contains a multi-slot entry, which is
exactly what a region is — so it was invisible to every module-shaped fused check.

**3. What this closes.** The fused reference voice (stereo, process-shaped)
renders what the plain voice renders **bit for bit** in the browser on both
JavaScript and Wasm, and the check now includes a steal so the per-slot reset is
exercised rather than merely written. Both fixes are bite-checked independently.

## D62 — a fused runtime holds exactly its slots (F1)

**Status.** Decided; implemented.

A fused artifact *is* `slots` voices, so the runtime configuration a fused
`synthesize` returns takes its `:capacity` from the slot count rather than from
the description. `runtime-config` reads `:capacity` straight out of the
description, and the fusion branch of `synthesize` added `:fusion` without
touching it, so a fused build of an eight-voice description produced a pool of
eight voices over an artifact that holds two.

The kernel maps a pool voice onto a slot by creation order (`mod created slots`),
so a larger pool hands several handles the *same* slot. That is wrong in itself —
two pool voices would share one voice's state — and it is silently worse for any
host step that writes every voice: `modulation/publisher` publishes the matrix's
targets to every voice in the pool each block, so several handles write one slot's
controls in turn and the last one wins.

The existing browser fused-pool endpoints had been working around this by setting
`:capacity slots` themselves (`browser-fused-pair`), which is what showed the
configuration the lowering should produce. A plain build is unchanged: only
fusion asks for `slots` voices.

## D63 — a derived definition forwards its controls; it does not ramp them (F1)

**Status.** Decided; implemented.

`graph/derived-definition` now marks its result `:dsp/forwarded-controls`, and
`descriptor/normalize` reads that mark to **skip ramp creation** for the derived
definition's controls. The control *declarations* still travel — the derived
interface is the voice's, `:smooth`/`:slew` included, so the ABI a host reads
still reports the mode — but the wrapper no longer smooths a value the voice is
about to smooth again.

A derived definition's body only *calls* the voice it was derived from. Every
occurrence of a control in that body is an argument to that voice, which owns the
control and ramps it once per call site. Inheriting the declaration therefore
created a second ramp: the wrapper smoothed the raw control and handed the
*smoothed* value to a voice that smoothed it again. Two poles compound, so the
derived signal lagged the plain one — percent for the first milliseconds, settling
only after thousands of frames. It looked like a numerical drift and was not one.

This was not `poly`'s alone. `guard`, `unison`, `share`, the instrument wrapper
(`hosts/instrument`), and the modulation transformers all build on
`derived-definition`, so every one of them doubled a smoothed control. The fused
*synth* path showed it first as `poly` + `guard`, which is what `synthesize`
wraps a module-shaped voice in.

**Rejected alternative.** Stripping `:smooth`/`:slew` from the declaration at
derivation time. It fixes the sound but loses the mode from the interface, and the
mode is *true* from the host's side: setting the control does move it through a
smoother, one owned by the callee. `tests/combinators.clj` already asserted the
declaration survives derivation ("the wrapped voice keeps its control mode"), and
that is the behaviour worth keeping.

## D64 — a lane reaches a callee by being handed the region (F3)

**Status.** Decided; implemented. Applies F0's spine (D56) to the shared-lane
case; F4 extends it to a per-voice lane.

A definition whose body reads `(lane control)` is now **handed the lane region**,
and the entry-only refusal is gone. The region is one array for the whole
artifact — the entry lays the lanes out, exactly as it already did — and a call
site passes **its own** region on, so every definition in the artifact reads the
same slices. That is why no base arithmetic is involved: a lane's offset is
`index * frame-capacity`, a compile-time constant of the *entry's* layout (D28),
so `(lane x)` means the same memory wherever it appears. A definition that only
*calls* one that reads a lane is handed the region too, because it forwards its
own; `lower-definitions` marks it by propagating the flag up the call graph.

The callee ABI therefore grows by **one pointer** per lane-reading definition
(the region) and by **the current frame** — a lane read is per-frame, exactly as
a `with-buffers` region write is (F2), and a per-sample callee has no frame local
of its own. The frame is what the first cut of this missed: the JVM emitted
`[:iload nil]` for the read's index and the JS emitted a reference to a `frame`
that was not in scope, which two targets reported in their own way (a `nil`
instruction operand, a `ReferenceError`). Both are one condition — a callee that
addresses anything by frame takes the frame — so one predicate now answers for
regions and lanes alike on all four targets.

**Not partitioned (yet).** A *shared* lane is one region, not one per call site
(D56), so the pointer every call site passes is the same one. F4 makes a
per-voice lane one region per slot; then the call site passes a different pointer
and nothing else in the ABI changes — which is the point of passing a pointer
rather than a base into the callee's own partition.

**Rejected: making the lane region part of the state arena.** Then a callee would
need only an int base — the region's place in the arena — and could address it as
`state[base + offset]`. It was rejected because the lane region is *host-owned*:
the pool writes automation into it, and it is one region for the whole pool
(M4/M5), while the arena is per instance. Moving it would change the pool, all
four hosts, and the Wasm memory layout to save one ABI argument. The pointer also
makes F4 fall out rather than needing a second base.

**What did not change.** The linker needed no relaxation: its rule is about a
callee whose *source* body uses `buffer-load`/`buffer-store`, and `(lane control)`
is not that form. A lane read's effects are the ones it already had
(`:read-memory`).

---

## D65 — a lane is one per control, and a fused entry slices the region per voice (F4)

**Status.** Decided; implemented. Extends D64 to a per-voice lane.

`poly`'s refusal is gone: a lane on a per-slot control fuses. The entry's lane
region gains a slot dimension — `slots` slices of the shape it already had — and a
call site hands its callee the base of **its** slice, so a lane-reading callee
keeps zero-based lane offsets and adds a base, exactly as its state indices are
resolved against a partition base (D19). A callee that only forwards the region
passes the base it was handed. The region is host-owned and pool-written, so it
stays where it is (D64's rejection still holds): the slice is an offset into it,
not a second region.

**A lane belongs to the control, not to the slot.** `poly` expands a per-slot
control — `gate_0`, `gate_1` — but a lane is one lane the slots read from
different slices. The expanded declarations therefore carry the lane's *name*
(`:lane-name`), and the entry lays out one lane per base control. Without this the
callee could not resolve `(lane amount)` at all: the entry's layout held
`amount_0`/`amount_1`, while the voice that reads it declares `amount`.

**A lane-only control's name is a read of the lane region wherever it is read.**
In the entry that was already how it bound; in a callee it now binds to a read of
*its own* slice instead of to the value the caller passed. The caller's value is
still passed — a call site cannot know the callee's slice — but the binding
overrides it. Where the artifact has one slice the two are the same read, which is
why no existing check moved.

**A slot is a call partition.** The base is the call site's partition ordinal —
the same index a host resets one voice by (M6a) — so a fused entry's lane-reading
call site must own a partition, and is refused with a message that says so rather
than being handed an arbitrary slice. A fused entry's *own* lane read is refused
too: the entry is nobody's call site, so it has no slice of its own.

**Rejected: one lane region per slot.** The entry would take `slots` region
parameters, or a list of them, changing the process ABI for every target, the
memory layout, and the host's argument assembly. One region and a per-call-site
base changes only what a lane-reading callee receives — one `int` — and leaves the
`slots`-versus-one distinction a property of the layout, where it belongs.

**Rejected: a compile-time base inside the callee.** A callee is emitted once and
serves every call site (D47), so the slice is not a property of its body. Baking a
slot into the emitted code would mean emitting a copy per slot, which is exactly
what D47 exists to avoid.

**Where the reservation remains.** A fused entry still needs a positive slot count
and at least one per-slot control (the fused interface is the expanded one), and a
stateless fused voice is still refused: a slot is a partition to reset.

---

## D66 — a fused worklet node is the fused interface, with no mapping (F5)

**Status.** Decided; implemented.

One worklet node renders `slots` voices. Its AudioWorklet parameters **are** the
fused interface, in ABI order — `gate_0`, `frequency_0`, `delay_0`, `mix_0`,
`gate_1`, ... — generated from the fused artifact's controls. The adapter already
writes a parameter into the kernel's control array at the parameter's own index
and builds its own kernel, so no mapping step exists: the pool's
`fusion/fused-position` translates the *pool's* base-shaped writes into a fused
control array, and a worklet has no pool.

This is the *static* half of polyphony, which is what `poly` has documented since
D38: the page owns slot assignment and drives `gate_k`. A keyed voice allocator
inside the worklet would be a different slice, not a consequence of this one.

**Descriptor order is not load-bearing; names are.** An adapter reads a parameter
by name and writes it at the index `processorOptions.controlNames` gives, so
`parameterDescriptors` only has to declare the names the page drives. Reversing the
descriptor list changes nothing; dropping the per-slot names silences a gated slot.

**A constructor that throws is otherwise invisible.** A worklet reports a bare
`processorerror`, and the page then sees a node that never answers. Both adapters
now default missing `processorOptions` to an empty object — which routes a
misconfigured node into the adapter's own reported "invalid metadata" path — and
the JavaScript one reports a constructor throw through its port, so
`worklet-ready` rejects with the reason instead of timing out.

---

## D67 — the post-mix chain has instrument-level lane and shared regions (H2)

**Status.** Decided; implemented.

A post-mix stage is a processor like any other, so it may declare a control as a
lane (`:lane`, `:has-lane`) or read one from the shared region (`:shared`), and
the declaration must survive the chain's own lowering: `chain-effect` rebuilds the
interface at every stage (`graph/derived-definition` with an overriding
`:controls`), and it used to rebuild each control as bare `{:name control}` —
silently dropping what the effect had declared. It now carries the declaration
across, pairing the effect's controls with the chain's arguments **positionally**,
because the effect names controls by its own parameters while the chain names them
by the arguments the instrument passes.

**The regions are instrument-level.** The chain is one mono processor run once
per output channel over the same summed signal, so its lane region and its shared
region are each **one region for the whole instrument**, written once and handed
to every channel's instance — the shape M5 gave the pool's shared region, one
level up. The host owns them: `set-bus-lane!` writes a per-frame curve, and the
shared region is filled from the shared *parameter* of the same name (refused,
once at construction, when no such parameter drives it) — which is exactly the
rule a post-mix *scalar* control already follows, so one `set-parameter!` reaches
the voices and the chain together. Rejected: a region per channel instance (three
copies of one curve, and the chain has no per-channel identity to justify one); a
new `set-bus-shared!` API (it would be a second way to write the same shared
value, and would not follow a parameter).

**Wasm cannot share the region, so it copies.** A Wasm instance owns its linear
memory, so the host's region has to reach the instance: the bus kernel copies the
frames a block can read into the instance's own region before each call — the
handoff the pool already gives a voice's shared region. JS hands the host-owned
array to every instance in its argument array, so nothing is copied. Both browser
runners expose the same `:write-lane!` / `:write-shared!` pair, so the page's code
is target-independent.

**`dsp/process!` accepts a host-owned region.** Its context may now carry the
region itself (a primitive array), not only a map of named values to flatten. The
docstring already promised this path for a real-time host ("writes the region
directly and skips this path"); there was no way to take it, and without it the
interpreter bus path could not be handed the regions the JVM path passes
positionally. A `:shared` value may also be a bare number, broadcast across the
slice — the rule the shared region already had.

**Consequences.** The fixtures are the check: `bus-gain-voice` renders exactly
`gate`, so a chain stage whose gain is the region makes the samples *be* the
curve, and neither the JVM/interpreter pins nor the browser ones need a tolerance.
The browser checks stay differential against the in-page interpreter host, which
now delivers the same regions. `write-region!` / `region-layout` /
`region-offset` live in `hosts/mixing.cljc`, the plumbing every host kernel
shares, because three hosts (JS, Wasm, the in-page interpreter runner) now write
the same kind of slice.

**Evidence.** JVM 227 tests / 1973 assertions, browser 46/46. Four bites:
dropping the declaration merge fails exactly the new assertions; a no-op
`set-bus-lane!` renders zeros; a no-op `write-bus-shared!` leaves the shared region
zero; on the page, withholding the JS lane argument fails the run and skipping the
Wasm copy renders `[[0 …]]` against the curve.

---

## D68 — a lane on a per-channel control is a curve per channel (H3)

**Status.** Decided; implemented.

`bend` is the pool's global source *bank*, indexed by channel: `set-pitch-bend!`
takes a channel, and a `:bend` control resolves to the value of the channel the
voice is on. The lane beside it therefore has a third scope — `:channel` — beside
`:per-voice` (the default) and `:shared`: the pool keeps one curve per channel and
a voice reads the curve for the channel it is on, exactly as it reads that
channel's bend. The scope is inferred from the *source*, like the other two: a
lane's values belong to whoever owns the thing it sits beside, which is the rule
`lane-scope` already stated and `:bend` was the exception to.

**The pool keeps its own copy, and re-delivers it when a voice starts.** Two
facts make that necessary rather than merely convenient:

- a voice that starts on a channel *later* must read the curve published for that
  channel, so a setter cannot simply hand the host's array to whichever voices
  exist at that moment;
- a voice can *move* to another channel — a steal, a mono retrigger, a host
  playing several channels — and its lane region is per voice, so it would
  otherwise keep reading the channel it was on.

So `set-channel-lane!` copies the curve into a `channel-count` × `lane-stride`
bank allocated at construction (`lane-stride` from the runtime config, like every
other array the pool owns), writes it into every voice already on that channel,
and `sound-voice!` delivers the channel's curves to a voice as it starts.
Rejected: declaring the bend lane per voice (a lane whose scope differs from its
own source's is the inconsistency this removes — and `:pan`, the genuinely
per-voice source, keeps `set-voice-lane!` for that); and leaving re-delivery to
the host (a per-voice lane needs no such rule because a voice's region persists,
and a channel lane cannot have that property).

**The other two setters refuse it, loudly and accurately.** `set-lane!` says a
channel lane is not shared, `set-voice-lane!` says it is not per voice, and each
names the setter that is.

**Consequences.** The refusal "a lane on a per-channel control has no per-lane
delivery yet" is gone, and `lane-on-bend`'s fixture docstring and its test flipped
from a construction failure to a delivery check. `synth/pool.cljc` grows the bank,
the delivery, and two private value helpers (`value-count` / `value-at`), which
the hosts already have as `mixing/value-length` / `value-at` but the pool cannot
reach: it deliberately does not depend on the host layer.

**Evidence.** JVM 228 tests / 1991 assertions, browser 48/48. Three bites: no
activation delivery → the channel-change and later-start checks read the *old*
channel's curve (6 failures); no copy (the bank stays zero) → 14 failures; no
set-time delivery → the "already sounding" check keeps the old curve (2 failures).
On the page the same activation stub renders `[[1 2 3 4] [1 2 3 4] [1 2 3 4]
[5 5 5 5]]` instead of the per-channel curves.

---

## D69 — the MIDI-shaped sources are host configuration; the language stays protocol-free

**Status.** Position, recorded.

The pool's runtime sources are MIDI-shaped by construction:
`synth/pool.cljc`'s "The global source bank is indexed by MIDI channel" and its
`channel-count` of 16; `note` as a MIDI number; `bend` arriving as a normalized
wheel and leaving as cents; `sequencer.cljc`'s `:routing` mapping a MIDI channel
to a pool. **None of it reaches the compiler**: the descriptor, the IR, the ABI and
all four targets see only float controls (`gate`, `note`, `bend`, `detune`,
`frequency`, `pan`, `velocity`), and the *pool* is what turns a channel index into
a bend value. That is D34's boundary (host policy above the kernel) and D35's
(pitch domain in the kernel), and it is what keeps the language usable by a host
that speaks something else.

The position to keep: **MIDI is one possible configuration of the host layer, not
a property of the language.**

- No protocol vocabulary in the descriptor, IR, ABI or a target: a control is a
  name and a type.
- The host layer may be as MIDI-shaped as it likes — 16 indices, note numbers,
  wheel-to-cents, channel routing — because another protocol supplies the same
  controls differently.
- A later refactor toward generality happens *there*, and it should be a
  reconfiguration rather than a rewrite: the runtime sources are a bank the pool
  reads, and a lane takes its scope from the control it sits beside (D46/D68), so
  "channels" can become "source indices" with a host-level policy for how many
  exist and what maps to them.

Two vocabulary notes, so that the asymmetry is deliberate rather than accidental:

- H3's lane scope is spelled `:channel` because the pool already speaks channels
  in five places (`note-on!`, `set-pitch-bend!`, `voice-snapshot`, `:routing`, the
  bank comment). One concept with two names would be worse than a protocol-flavored
  name in a protocol-flavored layer. Renaming it is a small, local change (the
  keyword, `set-channel-lane!`, `lane-scope`, the bank) if the source index ever
  stops being a channel.
- A scalar's host-facing value is in *source* units and a lane's is in *kernel*
  units. The pool scales a scalar `:bend` by `:bend-range` (a wheel in, cents out)
  and passes a lane through verbatim — uniformly for every lane, because a host
  that owns a curve writes what the kernel reads. A host that thinks in wheels
  applies `:bend-range` itself; `Kernel/write-voice-lane!` documents it.

---

## D70 — a call site owns a slot; state is one of the things it may own (H4)

**Status.** Decided; implemented.

A fused entry renders S voices in one call, and a host needs a per-slot identity
for four things: which slice of the fused control list a handle writes (M6), which
slice of the lane region it reads (F4), what a reset restores (D51), and which
specials it publishes (M6 seam 4). All four were *derived from* the call site's
**state partition**, so a voice with no state had no slots at all — every host
refused it, with "a slot cannot be reset on its own", which is not even true of a
voice that has nothing to reset.

The ABI now records **one partition per call site**, in evaluation order, with an
empty range when the callee has no state: `ir.clj`'s `lower-call` always conj's an
entry, and the ordinal stays the *call-site* ordinal. `:call-partitions` therefore
means what its name and every host's refusal message already said — the call sites
a slot indexes.

- A stateless voice fuses: S identities, a no-op reset, no specials — exactly how
  S plain stateless voices behave in a pool (a voice with no `:activity` is
  already a case the pool handles).
- A lane-reading callee no longer needs a state partition to have a slice: the
  fused base is `slot × lanes × stride` off the call-site ordinal. That also fixes
  a latent shift, where a stateless call site before a stateful one skewed the
  ordinals and so the slice bases.
- The Wasm host's partition-range resolution must accept an empty range: an empty
  partition overlaps no layout segment, and that is a slot whose reset does
  nothing rather than a lowering error. The JVM's `partition-ranges` already
  returned an empty range for one, and the JavaScript host loops `size` times, so
  only the Wasm byte-range scan needed the degenerate case named.

Rejected: deciding that a fused voice must carry state. It would have been
cheaper, and H's rules allow a recorded refusal — but it would have had to assert
something false ("a slot cannot be reset on its own" for a voice with nothing to
reset) and it would forgo the *most* fusable voice there is, the one whose only
per-slot cost is the entry call.

**Evidence.** JVM 229 tests / 1999 assertions, browser 50/50. Bites: making the
partition conditional on state again → all three new assertions fail with the
original refusal (the test cannot even build the pool); throwing on an empty
partition range in the Wasm host → the Wasm check fails with "empty partition"
while the JavaScript one still passes.

**A bite lesson, recorded because it nearly slipped through.** The first attempt at
the Wasm bite removed the guard but left the parentheses unbalanced. The stub threw
a reader error, Shadow served the *previous* build, and the check passed — a false
negative that looks exactly like "the guard is not needed". A bite stub has to be
checked for validity before its result is trusted (the second attempt kept the
structure and threw instead, which also proved the empty range *is* reached).

---

## D71 — an array of records may own an array field (H5)

**Status.** Decided; implemented. Closes the last item H named as a *language*
omission.

**What this supersedes.** D16's "an array of records cannot yet contain an array
field (rejected)". The rule that made it look expensive ("for a slot of `:size N`,
each leaf becomes one flat state entry of `:size N`") is the same rule that makes it
cheap: the record dimension is already a stride.

**The omission.** D16 said "an array of records cannot yet contain an array field
(rejected)", and `descriptor.clj` refused `{:name voices :size 2 :fields
[{:name :line :type :float :size 4}]}` outright. The layout was the reason it
looked expensive; it was not. D16's own rule — "for a slot of `:size N`, each leaf
becomes one flat state entry of `:size N`" — already made the record dimension a
*stride*, and the selector grammar already had both positions
(`[record-index?] field-name+ [element-index?]`). The missing pieces were the
descriptor's refusal, the leaf's stride, and the accessor's arithmetic.

**Decision.** A leaf of an array of `n` records whose field declares `:size m` is
one flat block of `n * m` slots, and carries `:stride m`. An access folds the two
indices into the single element index the ABI already had — `record * stride +
element` — so a leaf still has one constant base offset and one element index and
**no backend changed**: the JVM, JavaScript, Wasm and interpreter emitters saw a
constant base plus an expression before and after. The field's initializers repeat
once per record, flattened, because `:init` is also the state array's length (the
interpreter builds the arena from it, and a short vector is caught as "state has
the wrong size").

Consequences, all deliberate:

- **The grammar now means what D16 said.** `(field voices i :line k)` reads element
  `k` of record `i`; `(set-field! voices i :line k v)` writes it; `(aget voices i
  k)` / `(aset voices i k v)` are the terse spellings for a record array that owns
  exactly one array field, exactly as `(aget store k)` is for a scalar record.
- **A record array now needs its record index.** An array of records is always
  indexed: `(field voices :phase)` was a silent read of record 0's element and is
  now "a record array's field needs a record index". A *write* through
  `set-field!`/`set!` with no index used to be refused with a misleading message
  ("use aset to write an array field element") for a field that was not an array;
  it is now refused for the right reason.
- **A constant bank is a wavetable bank.** A `:constant` array of records owning
  an array field, indexed by a control and a phase, is the shape
  `:constant-table-bank` pins on all four targets. A fully constant access folds at
  compile time, so with literal indices the access stays a constant and reports no
  dynamic diagnostic.
- **Static checks are per dimension.** A constant record index outside
  `[0, n)` and a constant element index outside `[0, m)` are refused separately,
  each naming its dimension. This is the safety the flat-array workaround could
  not give: one flat bound cannot tell a wrong record from a wrong element, so a
  hand-linearized bank could silently read the neighbouring table.

**Rejected alternatives.** (a) *The constant-only first slice* I sketched as a
cheaper variant — unnecessary, because the writable case is the same layout and a
bank of buffers is the obvious second consumer. (b) *Leaving the flat
`(array :float (* n m))` workaround as the answer*, which was my recommendation in
the discussion and the plan's own default: overridden — a language that can name
the shape should not require the user to linearize it by hand, and the cost turned
out to be descriptor + layout + accessor only. (c) *The terse `(aget voices k)`*
for a record array: ambiguous between a record index and an element index, so both
are required. (d) *A row place* — `(field voices i :samples)` with no element
index: a row's base offset is dynamic and the language has no array values, so the
element is required.

**Not in scope, and said out loud.** A *nested* record field still cannot declare
`:size` ("DSP nested record fields cannot declare :size") — a different omission,
and one with an exact workaround: hoisting the nested records to a top-level
sibling record uses the same flat arena with the same layout. Named here so the
next reader knows it was considered rather than missed.

**Known residual.** The interpreter validates the *flat* index against the flat
block (as it does for every state access), so with a dynamic element index and an
in-range record index, an out-of-range element reads into the neighbouring record.
A per-dimension runtime check is a candidate for P15, where the diagnostics live;
the static checks above cover the constant case, which is where hand-written
linearization goes wrong most.

**Evidence.** JVM 230 tests / 2024 assertions, browser 50/50. The shape is a
shared catalog case both ways — `:constant-table-bank` and `:record-array-buffer` —
so the shared process matrix proves it on **all four targets** in f32 and f64 (92
cases per matrix, was 90). Bites, all bit: the stride ignored in the accessor →
shape refusals; a wrong stride in the fold → `[4.0 13.0 23.0]` instead of
`[6.0 13.0 23.0]` and `[0.25 0.0 0.75 0.25]` instead of `[0.75 0.0 0.5 0.25]`;
the initializers not repeated → "state has the wrong size {:expected 12, :actual
4}" and a broken pre-existing record-array case; constant folding removed → the
folded-node assertion and both per-dimension refusals fail.

---

## D72 — the JVM's WAT→binary step is a pinned WABT, not an encoder we write (P12)

**Status.** Superseded by D73. The pin and the measurements below stand — they are
what any future encoder would be checked against — but the compiler does not invoke a
host binary: the artifact names the assembler and the consumer that has it produces
the module.

**The question.** P12's first deliverable asks for "a defined WAT → WABT → binary
compilation step producing validated `:bytes`". Since Wasm is only ever *executed*
in the browser, and the page already compiles WAT with WABT (`wabt` 1.0.39 in
`package.json`, wired through `omkamra.dsp.harness.testbed`), that step can look
like a second, pointless implementation of something we already have.

**Decision.** The step *invokes* WABT. We write no WAT parser, no binary encoder, no
validator. The JVM looks for a native `wat2wasm` — the pinned release below — and
falls back to the page's own pinned npm CLI (`node_modules/wabt/bin/wat2wasm`). Both
were measured to produce **byte-identical** modules for every Wasm artifact the
catalog compiles, so the fallback is a performance choice, not a semantic one.

The pin: `wabt-1.0.39-linux-x64.tar.gz`, sha256
`1df1254b6639f5f1f89665907ee8cd0cd30114f40483515e583cccb86240840f`, from the
upstream 1.0.39 release — deliberately the *same version* the browser pins, because
the artifact's bytes and the page's bytes must agree.

**Why the JVM needs it at all.** `:bytes` is part of the artifact, and the artifact
is a JVM value. PLAN.md §6 and completion criterion 9 require validated binary bytes
in it; D5's placeholder (`:bytes nil`, `:generator :browser-wabt`) exists precisely
so that nothing depends on bytes until P12; and nothing on the JVM parses the module
today — the page's WABT is the only validator, so the fast loop cannot see a
malformed module at all. The step therefore buys **completeness and JVM-side
validation**, not execution: there is no Wasm runtime here, and Wasm still only runs
in the browser. P13 keeps WABT in the browser on purpose (source → SCI → … → WAT →
WABT → worklet), so the two paths coexist: the JVM's for artifacts, the page's for
editing source.

**Evidence.** 179 modules — every process case × its precisions, compiled for
`:wasm` — assembled with the 1.0.39 tools. Native: 0 failures, 0 byte-differences
against the npm CLI, the whole corpus in under a second, slowest module 143 ms. npm
CLI: 0 failures, ~535 ms per module (node start-up per invocation; ~96 s for the
corpus). DSP compilation itself is ~9 ms per module. Native `wasm-validate` accepts
every module, so the emitted WAT is already WABT-clean: P12's binary step is wiring,
not emitter repair.

**Rejected.** A JVM WABT binding: `org.bytedeco:wabt` does not exist (404), and no
other candidate was found. The distribution's `wabt` (1.0.36): version skew against
the page is exactly what would make artifact bytes unreproducible. `cargo install
wasm-tools`: a different tool with the same skew. Writing an encoder driven from our
own S-expression module form: smaller than a WAT compiler — we would translate our
forms and never parse text — but still real work that duplicates WABT while a pinned
tool exists.

**Consequences.** P12's binary step needs no JVM library; the artifact can carry real
`:bytes`; the JVM suite can validate modules; `:wat` stays in the artifact as the
debugging form; the page's WABT stays exactly where it is. One cost to respect: the
npm fallback is ~0.5 s per module and the testbed compiles a Wasm artifact per
request, so a build that always assembles should use the native tool (or cache by
WAT hash) rather than silently paying node start-up 200 times per browser run.

---

## D73 — lowering is portable up to the target's final step, which lives in the target's world

**Status.** Decided; the artifact side is implemented.

**The principle.** A target's compilation splits in two: everything that can be
written once is, in `.cljc`, so the same code runs on the JVM and — after P13 —
in the browser under SCI (descriptor, linking, portable IR, and the target-shaped
code or data each backend produces). What remains is the final lowering to the
target's native format, and that step uses a tool from the world the format belongs
to:

| target | final step | tool | where it lives | pinned in |
|---|---|---|---|---|
| JVM | JVM bytecode | `insn` | a Maven library | `deps.edn` |
| JS | native JS | `squint.compiler` | a cljc library, so it runs on the JVM today and under SCI later | `deps.edn` |
| Wasm | binary module | WABT | native C++ / the JS world | `package.json` |

**Decision.** The Wasm artifact does **not** carry assembled bytes and the compiler
does not assemble them. It carries WAT, the ABI and memory layout, and the *name and
version of the assembler* its consumer must use:

```clojure
:binary {:format :wasm :source :wat :generator :browser-wabt :version "1.0.39" :bytes nil}
```

The host that has that assembler produces the module. P12's deliverable 1 (a
WAT → WABT → binary step on the JVM) is dropped; P13 and P14, which already describe
the browser-side pipeline `… → WAT → WABT → worklet`, own that step.

**Why.** The tool for the other two targets is a *library the compiler can load*, so
their final lowering happens wherever the compiler happens to run — including, for
JS, on the JVM today. WABT is the one assembler that is not available in this
compiler's world, and bridging that gap on the JVM would mean a host binary, a JVM
binding (none exists: `org.bytedeco:wabt` is a 404), or an encoder of our own. None
of those follows from the architecture; the plan's deliverable assumed a JVM-only
compiler, where an artifact could only mean a JVM value, and P13 contradicts that
assumption anyway. Three things that looked like reasons for the JVM step do not
survive the principle:

- **Binary identity** is pinned by the *tool's version in the repository* —
  `insn 0.5.4`, `squint 0.14.211`, `wabt 1.0.39` — never by where the lowering runs.
- **Validation** happens by *executing a target in its own world*: JVM kernels in the
  JVM suite, JavaScript and Wasm kernels in the browser suite, which assembles and
  runs every catalog module on every run.
- **Completeness**: a Wasm engine takes bytes and nothing else. That is a property of
  the platform, not a defect in the artifact — and the artifact stays self-describing
  because it names the step.

**What this supersedes.** D5's "Wasm stays WAT-only *until P12*": WAT plus a named
assembler is now the Wasm artifact's form by design, not a deferral. D72's decision
to invoke a pinned WABT **from the JVM** — its pin and its measurements stay on the
record as evidence, and they are what a future hand-written encoder would be checked
against, but the invocation does not happen. PLAN.md §6 and completion criterion 9
are amended accordingly: the artifact names its toolchain rather than carrying bytes.

**Rejected.** A host binary or a JVM binding (the first is a dependency this project
does not want; the second does not exist). A binary encoder of our own: it would
duplicate WABT and would need a validation path of its own, and nothing needs bytes
outside the browser world today. It stays the fallback of record if that ever
changes — the spike sized it at 97 distinct forms across all 179 artifacts, so it is
bounded work, and the page's WABT would validate its output for free.

**Consequences.** `:bytes nil` now means "the consumer assembles this with the named
tool" rather than "not implemented yet". `omkamra.dsp.tests.wasm` reads the version
out of `package.json` and asserts the artifact reports it, so the name cannot drift
from the tool the browser actually loads. P12 is left with the memory layout and the
state-initializer blob — the parts that make the module *smaller and faster* on every
host.


## D74 — capacity is a declared frame count, and the memory layout is derived and validated (P12a)

**The defect was a duplicated constant, and it was invisible at the widths in use.**
Two hardcoded 4096s decided how many frames a region could hold: `ir.clj`'s
`lane-stride-bytes` — a *byte* budget, so the capacity it yielded was 1024 frames at
f32 and 512 at f64 — and the Wasm target's own `channel-stride-bytes`, which derived
its capacity from the same budget independently. They agreed, which is why nothing
ever failed; and the `:frame-capacity` option that both `ir.clj` and the JVM target
honoured was not in the closed compiler-options schema at all, so no caller could set
it through `dsp/compile`. The practical consequence was that an f64 artifact of the
same IR could render half the block an f32 artifact could, for no reason anyone had
chosen: a host rendering 1024-frame blocks simply could not use f64 on Wasm.

**Capacity is declared once, in frames.** `descriptor/default-frame-capacity` (1024)
is the default, `:frame-capacity` is a supported option validated as `pos-int?`, the IR
carries it, and every backend derives its strides from it:
`channel-stride = frame-capacity × element-bytes`. Frames rather than bytes, because
frames are what a host asks for — a host renders blocks, not budgets, and a capacity
that depends on the element width is not a capacity.

**The layout declares itself, and is validated where it is decided.** Each Wasm region
publishes `:offset`, `:bytes`, `:channels`, `:frame-capacity` and `:alignment`;
`validate-memory-layout!` then checks what a host will rely on — the stride *is* the
declared capacity at the declared width, every per-frame region is a whole number of
blocks, every region starts on its alignment, no two regions share a byte, every state
slot lies inside the state or scratch region, and the declared page count covers what
the regions need. It runs on every artifact, so a layout bug is a compile-time error
with the disagreeing numbers in it rather than a corrupted sample three layers away.

**Every reader takes the numbers from the ABI.** The Wasm host already read
`:channel-stride`, `:frame-capacity` and the region offsets from `:memory`, and now
refuses a block longer than the capacity instead of writing past its regions. The
page's runners were the ones recomputing: the process runner's `4096` defaults, the
`wasm-process-instance` offsets, and the two standalone WAT fixtures, which had no
metadata to read and now fetch it (`wasm-bounded-loop-metadata`, plus the existing
`wasm-metadata` route for the lookup-table probe).

**Consequences.** The f32 layout is *unchanged* — 1024 frames at four bytes is the old
4096-byte stride — so only f64 moves, gaining the frames it should always have had
(stride 8192) and doubling its lane, shared and buffer regions. `automation.clj`'s
lane-stride test had pinned the old asymmetry *as a fix* ("a lane's stride is a byte
count, so f64 fits fewer frames per lane"); it now asserts the opposite, that both
widths accept the same block, and that a shorter declared capacity shortens the stride.

**Verification.** A new shared catalog case, `:long-block`, asks for a 1024-frame block
at both widths, so it runs on all four targets: two catalog cases' worth of parity
(209 → 211 pairs, no gaps). Bitten by restoring the fixed stride, it fails on Wasm
**f64** from sample 512 — precisely where the old capacity ended — while f32, the
interpreter, the JVM and JavaScript pass. Alongside it, a layout-invariant test over
fixtures × precisions × capacities × channels, and a validator test that injects each
violation to prove the checks reject something.

**Rejected.** Keeping the byte budget (an f64 voice that cannot render the blocks an
f32 one can is not a design, it is an accident); leaving capacity internal (a host that
wants longer blocks must be able to declare that); a runtime layout (a Wasm module
declares its memory at compile time, so capacity is a compile-time fact by nature).

**Not in scope, recorded.** The *worklet* path still hardcodes processor options in
`harness/testbed.clj` (capacity 256, offsets 4096) and the Wasm worklet adapter keeps a
4096 `channelStride` fallback. That is P14's deliverable — installing a kernel *from*
ABI metadata — and nothing here breaks it, because the page passes the ABI's numbers to
the adapters it creates.

## D75 — a failure names its target, and a result always carries what failed (harness)

Two legibility traps in the browser harness cost real time in this session, both of the
kind that produce a *wrong conclusion* rather than an error:

- `run!` returned `:passed?`, `:passed`, `:total` and `:results`, and the derived views
  were opt-in. Reading `(:failed result)` therefore returned `nil` — a key that does not
  exist looks exactly like "nothing failed" — and a failing run was briefly read as a
  passing one.
- A matrix check's failure said *which check* failed and buried the target and the
  precision in a prose `:detail` that also contains a JavaScript stack trace. In a
  four-target comparison, "which half broke" was the only question, and the answer had
  to be parsed out of a sentence.

**Now:** `run!` always attaches `:failures` (`:views` adds to it, never removes it);
each failing case row carries `:failures [{:target :wasm :detail "…"}]`, so the case,
the precision and the target are fields rather than prose; `browser/case-failures` reads
a run one level below `browser/failures`, reporting one row per failing case with its
check and its failing targets; and a matrix check reports `:case-count` and
`:case-failures`, because a matrix's case count is not comparable with the suite's
*check* count and saying which is which costs nothing. A case's own verdict is the
fact: `case-failures` reports a failing case even when its check's summary claims
success, since the summary is a summary.

Two more things make the vocabulary discoverable: `browser/check-names` reads the check
names off any result, and a stray `:only` needle is reported with the needles as data.
That report deliberately does **not** list the check names — the page builds a check
only when the selection asks for it, so a stray needle leaves nothing to list, and an
empty vocabulary would be worse than none.

The wire decode had to learn `:target`: it arrives as the string `":wasm"`, and
`(keyword ":wasm")` builds a keyword whose *name contains the colon*, which compares
equal to nothing while still printing as `:wasm`. That is the trap this harness exists
to prevent, so every place a target can now appear is listed in `normalize-value`, and
the harness test pins `#{:wasm}` rather than the string.


## D76 — initialization is constant space, and signed zero is a bit, not a number (P12b)

**The state was written one store per element.** `feedback-delay` at f64 was 48 004
stores — 2 251 108 bytes of WAT text, 99.8% of the module, for a delay line whose
entire initial state is zero, and the page assembles that text on every request. The
zero initializers were the whole cost: across the entire catalog there are 54 839
initializer elements and exactly **27** of them are non-zero.

**One `memory.fill`, then only what is non-zero.** `$init` clears the state region in
constant space and then writes the non-zero initializers element by element, so module
size follows the non-zero part of the state rather than its capacity. Measured on
`feedback-delay` at f64, assembled with the pinned WABT: **768 644 bytes → 645 bytes of
module**, and 2 251 090 → 5 562 bytes of the WAT text the page assembles per request,
in 0.52 s → nothing. 48 004 stores became one instruction. `reset` still calls `$init`, and that is
the crux: an **active data segment cannot serve `reset`** (it applies at instantiation
and never again), so the zeros could not simply be dropped, and the non-zero part had
to stay re-appliable. The zeroing is load-bearing and was proven so — removing the fill
fails eight catalog cases on Wasm alone, because `reset` then leaves the previous
note's state behind.

**What "zero" means is the bit pattern, not the number.** `-0.0` equals `0.0` and is not
the same bytes, and the interpreter preserves the difference; so the test for "needs no
store" compares raw bits at the kernel's width. Writing that test found a real bug: both
Wasm constant formatters decided the sign of a zero with `neg?`, which is defined
through `<` — where `-0.0` and `0.0` are equal — so `-0.0` was emitted as `0.0` in every
constant, not only in initializers. Fixed by reading the sign bit, and pinned in
`omkamra.dsp.tests.wasm`, because the differential suite *cannot* see it: the Wasm half
allows transcendental emulations a documented tolerance (D14), and a sign flip on an
exact zero is inside that tolerance. The pin is the guard, and it was bite-checked.

**Rejected.** A data segment for the initializers (an active one cannot be re-applied on
`reset`, which is the whole point of `$init`; a passive one costs a segment and a
`memory.init` per contiguous run, and at 27 non-zero elements across the catalog there
are no runs worth a segment). Skipping the zeros without a fill (breaks `reset`).
Treating `-0.0` as zero (loses a bit the interpreter keeps).

**Recorded, not fixed: a signed zero *computed* by an emulation.** Tightening the Wasm
comparison to require equal signs on equal zeros turned up `sin(-0.0)`, which the Wasm
polynomial emulation returns as `+0.0` where `Math/sin` returns `-0.0`. That lives
inside D14's documented tolerance, so the tightening was reverted rather than the
emulation changed: expanding a documented contract is a decision about the numerical
policy, not a side effect of a phase about module size. Two consequences are recorded
in the code: the comparison rule now carries a comment saying *why* it forgives this,
and `conformance.clj`'s `:wasm` `scalar-match?` method is marked as not reached from the
JVM-side runner — the live copy is the page's, so an edit to the tolerance has to be
made in `testbed.cljs` or it changes nothing.

**Also found while working here:** the two-matrices' check names (from D75) made the
bit-exactness question visible at a glance; the catalog case written to exercise the
signed zero was dropped, because a case that cannot fail is not coverage.

## D77 — the compiler is compiled, SCI evaluates source, and a float is its type (P13)

**The fork, decided.** The plan's toolchain note says SCI is how *our* Clojure code
is meant to run in the browser, and that squint runs inside SCI there. Read
literally that means the browser interprets the compiler's source, and two things
make that the wrong reading. SCI reads `.cljc` in its `:clj` mode unless it is told
`:features #{:cljs}`, so malli and squint would take their JVM branches — and every
dependency's source (~250 KB for squint's compiler, ~10 k lines for malli) would have
to be served to the browser and interpreted at page load.

So: **the compiler is AOT-compiled by Shadow, and SCI evaluates the user's DSP source
and nothing else.** The `.cljc` ports deliverable 3 names are the mechanism; SCI keeps
the job it is uniquely good at, and the macros that register descriptors call into the
compiled functions rather than reimplementing them.

Measured before deciding. Shadow compiles `squint.compiler` + `malli` + `edamame` for
the browser — 87 files, 0 warnings, 12 s — and they *run*: `compile-string` returns
JavaScript, `m/validate` and `me/humanize` work. A cold page load that compiled ten
real DSP kernels took under a second, browser launch included. The rejected reading
would have cost seconds per load and a source-serving server, to reach the same place.

**The bundling consequence.** A CLJS consumer that requires the compiler gets malli,
squint and edamame in its bundle, transitively, because that is what requiring a
namespace means. That is the right default for a project that wants to compile in the
browser, and the wrong one for a project that only wants to *run* kernels — so the
split is a rule, not an accident: **the runtime half never requires the compiler.**
It already holds (`hosts/js`, `hosts/wasm` → `hosts/fusion`, `hosts/mixing` → `synth`
and `synth/pool`, whose docstring already says it works 'without the compiler in
reach'), and P13 must not break it while adding the compiler to the same build.

**A float is its declared type, never its printed value.** Two measured facts make
this a platform rule. First, `pr-str` is not platform-independent for numbers:
ClojureScript has one number type, so `(pr-str 0.0)` is `0`, and the JS emitter's
source text — built by `pr-str` — differed between the JVM and the browser on seven of
ten real kernels, in every case an integral float literal (`0.0` → `0`, `2.0` → `2`,
`1.0` → `1`). Second, equality is not platform-independent either: `(= 0 0.0)` is
false in Clojure and true in ClojureScript, because there is no integer type to
compare against. So nothing may *infer* floatness from a value: the JS emitter formats
float literals from the IR's declared type, and the IR's canonical form does the same.

**The canonical form.** A cross-platform comparison of the phase's deliverable 4 needs
one serialization that is stable in order (map keys sorted, sets sorted) and explicit
about numbers. It lives with the IR, because the IR is what knows the types. It is
also the cache key deliverable 5 asks for — the same property serves both, which is
part of why it is the IR's business and not the test's.

**Stays JVM-only, deliberately.** `targets/jvm` (it uses `insn`), `hosts/jvm`,
`hosts/instrument`, `harness/*.clj`, and the JVM side of `dsp.clj`'s target dispatch,
which currently requires every target unconditionally and must be reader-conditioned.

**Verified by the spike, not assumed:** metadata survives the round trip to squint
(`^:mutable` was honoured in the browser build, so `*print-meta*` works under CLJS);
D41's rules hold for real kernels, and the only `squint_core` reference seen came from
a synthetic source that deliberately omitted type hints; the Var/`ns-resolve` seam was
already anticipated — `synth.cljc` carries `#?(:clj (var? definition) :cljs false)`
with a comment about the browser registry.

## D78 — a numeric literal carries its type, and the type is data (P13)

**The hole.** `descriptor.cljc:489` and `ir.cljc:144` both decide a numeric
literal's type with `(integer? form)`. That is a host-type rule: on the JVM
`(integer? 1.0)` is false and `1.0` is a `:float`, while ClojureScript has one
number type, `(integer? 1.0)` is true, and `1.0` becomes an `:int`. Everything
else in the descriptor is already typed — params, state, control ranges,
`:return-type`, record and struct fields — and every IR node carries `:type`, so
literals are the single place where a type is *guessed* rather than declared.

**It is not a browser-only problem, which is what settled the design.** A
JVM-produced descriptor does not survive transport either: P13a's browser check
fed the browser descriptors as EDN and `(+ sample 1.0)` came back as
`(+ sample 1)`, failing the type checker in four catalog cases. The same fate
awaits `dsp/defn` in a `.cljc` file compiled by Shadow: the macro runs on the
JVM, sees a real `Double`, and the descriptor's numbers are emitted into the
compiled JS as JS numbers — and a JS number has no memory of having been written
`1.0`. Nothing can be recovered from a value, because the distinction is
destroyed where the text is read: Clojure's reader, `clojure.edn`,
`cljs.reader`, edamame and JSON all give one number type on the browser side.

So the type must be **in the data**.

**The representation is a plain map; the tag is sugar.** A float literal is
`{:dsp/float 1.0}`. The tag `#dsp/float` reads to exactly that map and nothing
more, so a descriptor stays plain EDN: `clojure.edn` and `cljs.reader` read it
with no registration at all, which is what matters, because that is how
descriptors travel. There is no `#dsp/int` — an unboxed literal is an `:int`, so
the default needs no spelling and gets none. The tag is registered in
`resources/data_readers.cljc`, which both Clojure and Shadow read; SCI gets it
through `:readers` (or its `*data-readers*`), verified in `sci.impl.parser`.

**Who writes the box, and where recovery happens.** The capture synthesizes it:
on the JVM from the host type, so the 82 macro-captured definitions in the repo's
library, fixtures and examples need no edit; and in the browser's SCI capture
from the *source text*, consulting the line and column that `&form` carries
(verified: SCI supplies `:line` and `:column`), so a user who writes `1.0` gets a
float and never has to write a box. **Recovery needs text, so it cannot cover
everything**: a descriptor crossing as data, and a hand-written quoted body in a
`.cljc` file — the acceptance catalog's seventeen — have no text to consult. There
the box must already be in the data, and a bare float value in a hand-written
descriptor is an error naming the literal, so the JVM points at exactly the eight
public spellings a human has to fix, and can never point at macro-captured code.

**Rejected: literals that take their type from context.** A numeric literal that
is an untyped constant, adapting to the expected type of the operation around it
and defaulting to `:int`, would make the problem vanish — context is
platform-independent, so both worlds would agree by construction, and the 142
integral-valued floats in the repo's bodies would stop being dangerous. It is
incomplete, and the gap is not marginal: a literal with no context, as in
`(let [x 1.0] (float-use x))`, has no expected type at the binding, so it would
default to `:int` and silently change the meaning of code that works on the JVM
today — making the browser's current *wrong* behaviour the *defined* behaviour.
Completing it needs a real inference engine over a local's uses, which would
replace the explicit-type spine the IR, the targets and the conformance work are
built on. That is a different project, not a slice of P13. The hybrid — adapt
where a context exists, box where it does not — is possible and would need fewer
boxes, but it is more machinery than the box plus recovery, and it changes
diagnostics that currently fire.

**Verified, not assumed, in four readers.** Clojure via
`resources/data_readers.cljc`; SCI via `:readers`; Shadow's ClojureScript reader
via the same file; and `clojure.edn` plus `cljs.reader` reading the *map* with no
registration. Two operational facts came out of it: `data_readers.cljc` is read
**once per process** by both Clojure and Shadow — Shadow computes
`cljs-data-readers` in a `def` in `shadow/build/data.clj` — so a running dev
server does not see a reader file that appeared after it started, and must be
restarted; and the box's *printed* form differs by platform, because Clojure
abbreviates a map whose keys all share a namespace (`{:dsp/float 1}` prints as
`#:dsp{:float 1}`) and ClojureScript does not. The values are equal and the text
is not, which is a second independent reason the canonical form of D77 cannot be
`pr-str` and must write keys itself.

## D79 — generated names are deterministic, and the compiler equivalence is a check (P13a)

**The box reaches further than the capture.** D78 said the capture synthesizes a
float literal's box, and that is where author-written source is canonicalized. It
is not the only place a float enters a DSP form, and the browser found the others:
`sign`'s expansion in the builtin table generates `(> x 0.0)` with `0.0`, `1.0`
and `-1.0` in it, and `combinators/guard` and `combinators/unison` generate `0.0`
comparisons, `0.0` branches and computed detune offsets. Those are *forms built by
code*, and the code is compiled into the page, so a bare float there is read as the
integer it looks like — the equivalence check reported `sign` as *binary operands
have the wrong type*, and the browser was right to refuse it. Generated forms are
source too. So the box vocabulary lives in one place, `omkamra.dsp.literals`, and
three kinds of code use it: the capture, the builtin expansions, and the
description transformers. A test holds the line — every builtin's expansion, every
library and fixture definition, every transformer output and every catalog
definition is walked for a bare float, and the message names the operator and the
path. Bites: unboxing `sign` fails it on all four of its literals; unboxing
`guard` fails it on the body's two and the transition's one.

**Generated names must not come from the process.** The IR the browser produces
for a definition has to be the IR the JVM produces for it, and `gensym` reads a
process-global counter, so the same `(mod a b)` lowered here was
`mod_remainder66419` and in the page `mod_remainder1` — the same program, a
different IR, and no exact comparison of the two possible. Lowering now binds a
counter of its own (`omkamra.dsp.names`), and a generator asks it for the next
name; outside a lowering the fallback is still `gensym`, because there is nothing
to be deterministic within. Three cases (`:mod-zero`, `:mod-neg`, `:mod-flip`)
stopped differing when this landed, and un-binding the counter makes the check
fail on exactly those three, naming the path into the IR and both names. The JS
emitter's own names (`__iter`, `__reached`) are the same question one layer down
and belong to P13b, where the emitted-text comparison will need them.

**The equivalence is a check in the suite, not a claim.** `compiler equivalence`
has the page compile every catalog definition it can with the compiler compiled
into it and compare, per definition, the normalized descriptor, the linked unit
and the portable IR with the JVM's — 60 definitions, 52 compared and bit-exact,
and the check is part of every browser run. A case whose unit reaches a library
definition is *skipped* rather than failed, and the count is in the check's detail
and its `:skipped`: the page has not been given those definitions yet, which is
P13d's registry, and a check that says how much of the catalog is waiting for it
is more honest than one that fails on something it was never told how to do. The
expected values for those cases ship anyway, so closing the gap needs no change on
the server side.

**Two harness contracts follow from that.** A check that declares no `:targets`
contributes no rows to `case-rows`, however many cases it reports: comparing the
compiler with itself ran no kernel, and a case that was never run must not look
like one that ran and failed, or the parity table would gain a gap for every
definition. Those rows are still reported by `case-failures` when they fail, one
per case, with an empty `:targets` — a failure without a target used to put a
`nil` in that set, which is a target that does not exist.

**A tooling lesson worth keeping.** `cljfmt fix` rewrote three files and the reload
tool then *refused* to reload, reporting `:status :refused` with "the whole session
changed, a replaced baseline rather than an edit". That is the design working: a
bulk rewrite invalidates a content-based baseline, and silently reloading would
have hidden it. The cost is one explicit `(tools/reload :all)`, and the reason it
mattered here is that the stale session kept a *bite* in place, so the equivalence
check failed with names from a broken JVM — a failure that was true about the
session and false about the code.

## D80 — the emitters are portable, and the text they make is one rule (P13b)

**A number's text is decided once, and neither host's answer is taken on faith.**
`omkamra.dsp.numbers` holds every decision the two worlds make differently about a
number: its bits (`BigInt64Array` against `doubleToRawLongBits`, and 64 bits as a
BigInt in the browser because they do not fit a JavaScript number), whether it is
NaN or infinite, rounding it to f32, spelling it. The spelling is **the shortest
digits that read back as the value, rounded to nearest with a tie away from zero,
written in the JDK's notation** — plain below `1e7` and above `1e-3`, `d.dddE n`
outside that, always a digit after the point. It is computed by the *same search*
on both sides: round the exact value to `k` significant digits, `k` from one up,
and stop at the first `k` that reads back (`BigDecimal` of the double on the JVM,
`toExponential` in the browser). Both round half up on the magnitude, so the search
lands on the same digits in both worlds.

It is deliberately **not** the JDK's own spelling, because that spelling is not
the rule. Two deviations were measured, both outside anything a kernel emits and
both pinned in `omkamra.dsp.tests.numbers`: the smallest subnormals, which the JDK
spells `4.9E-324` where the shortest spelling is `5.0E-324`, against its own
javadoc; and a value whose shortest spelling is an exact decimal tie, which the JDK
rounds to even (`1439526.2`) where the rule says away from zero (`1439526.3`). The
digits are not taken from `String.format` either: the JDK's `%e` rounds half up
except where it recognises the value, so it is not a rule a second host can be made
to agree with. Two further divergences came out of the same sweep: Clojure's
`float` is stricter than IEEE and throws for values above `Float/MAX_VALUE` that
round to it, so the coercion asks the JVM's own narrowing conversion; and
JavaScript's number→string gives the *shortest decimal*, so an `i64.const` of the
f64 exponent mask was spelled one digit off — `4607182418800017400` for
`4607182418800017408`, which assembles to a different module.

**A float literal leaves the JavaScript emitter as a spelling, not as a number.**
It is written as a `js*` snippet carrying its text, which is the device this
emitter already uses for every exact JavaScript spelling (`Math.fround`, `| 0`,
`break`) and therefore costs no new rule. The reason is D78's: on the JVM a
literal prints `2.0` and in a browser `2`, and a symbol whose *name* were the
spelling would read back as a name rather than a literal. As a snippet it also
leaves the emitted **form** comparable data on both sides, which is what the
harness compares.

**The emitters are compared as forms and as text, not as squint source.** The
`compiler equivalence` check now emits as well: for each definition and each width
the page lowers its own IR and produces the JS target's forms and the Wasm target's
module, and compares them with the JVM's — the forms as *data* (numbers by bit
pattern, and a literal's spelling is visible in the snippet's string) and the module
as *text*, through `diff/text-difference`, which reports the line and a window
around the character that differs rather than a page of text. The squint *source*
is not compared, and that is deliberate: `pr-str` renders metadata differently on
the two platforms (`^:mutable` against `^{:mutable true}`), which is host-independent
in the emitter's own code and invisible in the artifact, so comparing it would have
meant writing a Clojure-compatible printer to compare something nobody reads. The
module text — where squint's own host-dependence would show — is P13c's, along with
the browser compile entry point; the JS emitter requires squint only under
`#?(:clj …)` and exposes `emit-forms` as its portable half.

**`prepare-for-target` moved from `dsp.clj` into `ir.cljc`.** The check found the
gap rather than a suspicion: a target is handed *prepared* IR, and the preparation
lived in the JVM-only compile entry, so the page could not produce the IR either
emitter is actually given. A definition that returns a record — which no backend's
calling convention can take and which is therefore copied into its call sites —
reached the JS emitter as an expression it has no case for. The step is the
compiler's and it is portable, so it is now in `ir`, and both worlds emit from
prepared IR.

**The check found two ClojureScript traps on its first runs, which is the point of
having it.** `(:require [omkamra.dsp.targets.js :as js])` compiled, warned about
nothing, and failed at run time with `emit_forms is not defined`: ClojureScript
reserves the `js` prefix for JavaScript globals, so `js/emit-forms` is a global
lookup and not the namespace's function — the JVM never would have noticed. And the
integer above is the second. Neither is the sort of thing reading finds.

**The spelling is verified over a corpus wide enough to be wrong about.**
`number formatting` is a browser check of its own, fed by a swept corpus rather
than by the catalog, for the reason the two deviations above give. Checked
separately from `compiler equivalence` because it is a different question: that one
asks whether the emitters agree over the catalog, this one whether the one function
they both spell constants with agrees at all.

## D81 — the compile pipeline is portable, and the page compiles its own artifacts (P13c)

**Status.** In force (P13c, adopted 2026-10-24).

**Decision.** The pipeline that turns a descriptor into an artifact — resolve the
options, check the target can serve them, link, lower to portable IR, prepare that
IR for the target, emit — lives in `omkamra.dsp.compiler`, a `.cljc` namespace that
runs unchanged on the JVM and in a browser. `omkamra.dsp/compile` is the JVM entry
point around it; the one thing it adds is the JVM backend, whose bytecode generator
(`insn`) is a JVM library, supplied to `compiler/compile` as an extra target compiler.
`targets/js/emit-module` requires `squint.compiler` in both worlds, so the ES module
a kernel is loaded from is made by the same code on either side.

The browser's `compiler equivalence` check compiles every catalog definition it can
*in the page* and compares, per definition and per precision, the JVM's normalized
descriptor, linked unit, portable IR, canonical IR text, emitted JS forms, emitted
Wasm module, and the **compiled artifacts** — the JS module and, for a process
entry, the WAT — with the browser's own. The artifact leg calls `compiler/compile`
from the raw definition, so it exercises the entry point (option resolution,
capability check, ABI) and not only the emitters' halves.

`ir/canonical-form`/`canonical-text` is the one serialization the two worlds compare
and a cache keys on (D77): map keys and set elements sorted by their printed form,
and every number replaced by its f64 bit pattern as exact decimal digits, because
`(= 0 0.0)` and `(pr-str 0.0)` are both host-dependent (D78). A value's type is not
lost — the IR carries it in `:type` — and the JVM printer is told not to abbreviate
a one-namespace map to `#:ns{...}`, which ClojureScript does not do, so the text is
host-independent as well.

**Rationale.** D77's rule is "the runtime half never requires the compiler": the
worker build and the browser hosts must bundle neither malli nor Squint. Putting the
pipeline in `compiler.cljc` rather than in `dsp.clj` is what lets the page *have* the
compiler without the runtime having it, and what makes `dsp/compile` and the page's
compile the same pipeline instead of two implementations that will drift. Comparing
the compiled module text, not only the emitter's forms, is the strongest statement of
"one compiler in two worlds" available: the artifact a page produces is the artifact
the JVM produces.

**Consequences.**

- A runtime-only consumer (`hosts/js`, `hosts/wasm`, `synth`, the worker build)
  requires none of `omkamra.dsp.compiler`, `malli` or `squint`.
- The page build does require them; that is the compiler's bundle and D77's stated
  default for a project that compiles in the browser.
- A `.cljc` change to `compiler`, `ir`, `descriptor`, `linker` or either emitter is
  a change both worlds see, and `compiler equivalence` is what says so.
- The `:definition-var` cases — those whose linked unit reaches a library definition
  — cannot be compiled in the page until P13d's browser registry supplies those
  definitions, so the check **skips** them and counts them rather than failing.
  P13c deliberately does not pre-empt the registry.

**Evidence (bites).** The module-text comparison found a real host difference on its
first run: `default-form` emitted a bare `0.0` for an `if` result, which prints
`__dsp_fround(0.0)` on the JVM and `__dsp_fround(0)` in the page. It is fixed by
routing the default through `number-form` (the `js*` spelling device, D80), and
`omkamra.dsp.tests.literals/every-emitted-js-form-boxes-its-floats` now holds the
line on the JVM (reverting the fix fails it 94 times). The canonical-text comparison
bites when the JVM and cljs spellings differ (perturbing the JVM branch fails the
browser check). The refactor is bite-free for the 275 pre-existing JVM tests, which
is the point of comparing before and after.

## D82 — the browser source front end is SCI-source macros over a compiled capture (P13d)

**Status.** In force for P13d (adopted 2026-10-25). The JVM-side mechanics below
are measured; the browser-side SCI build is the slice's first check.

**Decision.** The browser's DSP source front end is:

1. **SCI evaluates only the user's source; the compiler stays AOT.** D77 already
   fixed this — Shadow compiles `descriptor`, `linker`, `ir`, `builtins`,
   `numbers`, `compiler` and both emitters, and SCI expands the user's `dsp/defn`
   forms and constructs descriptors. Neither the bodies (they are data) nor the
   compiler are interpreted.
2. **The `omkamra.dsp` macros are defined as *SCI source*, not injected as host
   macros.** A `prepare-context` bootstrap evaluates `defmacro`s that read the
   definition site with SCI's own `ns-name`/`ns-aliases`/`ns-refers` and
   `&form`'s `:line`/`:column`, then delegate descriptor construction to compiled
   helper functions injected as ordinary SCI vars. Each macro returns
   `(def name (quote descriptor))`, exactly as the JVM capture does, so the
   definitions live in SCI Vars.
3. **`prepare-context` seeds the compiled-in library namespaces**, injects the
   compiled capture helpers, evaluates the macro bootstrap, sets
   `:readers {'dsp/float …}`, and curates `clojure.core` (the macro expansions use
   `let`/`when-not`/`assoc`/`throw`/`ex-info`, so it is not an empty core). The
   user's source `require`s what it needs; SCI resolves against the registered
   namespaces, with no filesystem or `:load-fn`.
4. **A host-side registry `{qualified-id → descriptor}`, seeded with the
   library, is what `linker/link` resolves against** (through the injectable
   `:resolve` built in P13a), not JVM Vars. SCI Vars may also hold the
   definitions; the map is what the linker and P13e's cache read.

**Rationale for (2), and the measurement that decided it.** `sci.core/new-macro-var`
exists, so compiled macros *can* be injected. But a host macro runs in its compiled
namespace: its `*ns*` is not SCI's, and while it can reach SCI's namespace as an
object through `@sci.core/ns`, `sci.core` exports only `ns-name`/`resolve`, not
`ns-aliases`/`ns-refers`, so reading the definition site's aliases from the host
means leaning on `sci.impl.*`. An interpreted SCI macro has all of it for free.
The capture logic stays compiled either way; only the thin macro that reads the
definition site is SCI source.

**Measured (dev JVM, 2026-10-25).**

- `sci.core/new-macro-var` exists (`new-var` + `:macro true`).
- A host macro's `*ns*` is the compiled ns, and `clojure.core/ns-name` on the
  `sci.lang.Namespace` it can reach throws; `sci.core` exports no alias accessor.
- An interpreted SCI `defmacro` reads `(ns-name *ns*)`, `(ns-aliases *ns*)` and
  `(ns-refers *ns*)`; `&form` carries `:line`/`:column`.
- `(require '[lib :as x])`, `:refer`, and `(ns u (:require …))` resolve against
  namespaces pre-registered in `sci/init`.
- `(sci/init {:readers {'dsp/float f}})` round-trips `#dsp/float 1.0`.
- **SCI's `ns-refers` includes all of `clojure.core`**, whereas the JVM capture
  deliberately excludes clojure.core from `:resolution {:refers …}`. The browser
  capture must filter the same way, or descriptors will not match and `compiler
  equivalence` will flag it.

**Consequences.**

- The runtime/compiler split still holds (D77, D81): SCI and the registry are the
  compiler half, and the worker build never requires them.
- The JVM macro and the SCI macro share one compiled capture function, so
  `compiler equivalence` can compare browser-captured descriptors with JVM-captured
  ones — and the eight `:definition-var` cases become comparable rather than
  skipped.
- The macro must *be* a macro, so the body arrives as data; and the other DSP
  macros' expansions must be evaluable in SCI's curated core.
- Anonymous `dsp/fn` values and transformer output (`(dsp/def x (-> (dsp/fn …)
  (unison …)))`) carry `:dsp/dependencies`; the JVM-only `carried-definitions` in
  `dsp.clj` must be re-expressed for the registry.

**Open (P13d, not yet decided).**

- Whether the browser SCI build compiles under Shadow — the first check, in the
  style of D77's spike.
- Whether descriptors are read back from SCI Vars or mirrored into the host
  registry (or both).
- How the library registry is generated: a generated `.cljc` manifest, or
  per-namespace registration.
- Error classification separating SCI reader/expansion/eval errors from
  descriptor/linker/ir and backend errors (P13e).

## D83 — one source pipeline over two worlds and four inputs (P13d)

**Status.** In force (implemented and verified).

**Decision.**

- `omkamra.dsp.source` (`.cljc`) is the front end. `compile` dispatches on the
  *input*: `:definition` for a descriptor, a linked unit, or a JVM Var;
  `:source` for a string. `evaluate` dispatches on `:evaluator`: `:sci` (the
  default, and the only option in the browser world) or `:native` (the JVM's own
  reader and macros, JVM-only). Both inputs compile to the *same* artifact, so a
  handle is determined by the target and not by the path that produced it.
- `omkamra.dsp.capture` (`.cljc`) is the one reading of a definition form. The
  JVM macros and the browser's macros both call it, so a definition reads the
  same in either world. It takes the definition site as data — the form, the
  namespace, the resolved aliases and refers — and never reads `*ns*` itself.
- The browser's `omkamra.dsp` macros are **SCI source**, not host macros. A host
  macro made with `new-macro-var` runs in its compiled namespace: its `*ns*` is
  not SCI's, and SCI exports no alias accessor a host could apply to the
  namespace object it can reach. An interpreted macro reads `(ns-name *ns*)`,
  `(ns-aliases *ns*)` and `(ns-refers *ns*)` for free, which is exactly what a
  definition's `:resolution` needs. `omkamra.dsp.sci` builds the context:
  library namespaces seeded, the compiled capture functions injected as host
  vars, one bootstrap of `defmacro`s evaluated into it, `:readers
  {'dsp/float …}`, and SCI's own `clojure.core`.
- `omkamra.dsp` has two **world faces**, the arrangement ClojureScript forces on
  any macro namespace: `omkamra/dsp.clj` (the macros, the JVM runtime, the `:jvm`
  target method, the `:native` evaluator) and `omkamra/dsp.cljs` (the runtime).
  A browser app writes `(:require [omkamra.dsp :as dsp])` **and**
  `(:require-macros [omkamra.dsp :as dsp])` and gets `dsp/defn` and `dsp/compile`
  from one alias, as `cljs.core.async` gives `go` and `chan`.
- `compiler/compile-target` is a multimethod. `:interpreter`, `:js` and `:wasm`
  share the `:default` method; the JVM registers `:jvm` from the side that has
  `insn` (D77). It replaces the ad-hoc `extra-targets` map, so a host can add a
  backend without the pipeline changing.
- The four cells are `{JVM, browser} × {static definition, source string}`.
  Static is the macros in the app's own namespaces (a Var on the JVM, a
  descriptor Var in the page); dynamic is a string through SCI (or `:native` on
  the JVM). Call resolution is the one asymmetry: the JVM resolves through Vars,
  the browser through a registry, because ClojureScript has no runtime
  namespace resolution (T7).

**Evidence.** The `source equivalence` browser check compiles five source
strings with SCI *in the page* and matches the JVM's own reader and macros
byte-for-byte — descriptor, canonical IR, JS module and WAT. `compiler
equivalence` compiles all 60 catalog definitions with 0 skipped. JVM 277 tests /
2541 assertions / 0 failures; browser 53/53 checks, parity gaps empty.

## D84 — a portable source location is namespace+line+column, and a box is never re-boxed

**Status.** In force (amends D78/D79).

- A macro's `&form` carries `:file` under ClojureScript's reader and not under
  Clojure's, so `:file` cannot be part of a descriptor that two worlds compare.
  `omkamra.dsp.capture/source-of` selects `[:line :column]` plus the namespace;
  a front end that knows a file may layer it on *beside* the descriptor. (This
  was found by the first real cross-world link of a library definition.)
- `omkamra.dsp.literals/box-float-literals` must not descend into a
  `{:dsp/float x}` box. A plain `postwalk` visits the inner number before the
  box around it, so an explicit `#dsp/float -1.0` became
  `#dsp/float #dsp/float -1.0` — on the JVM as well as in the page.

## D85 — the host API is uniform; a target's representation stays behind its kernel

**Status.** In force as the architecture. The voice half is implemented; the bus
half is the next slice (see the handoff).

- The voice half is already uniform: `synth.pool/Kernel` is a protocol and
  `write-voice-controls!` takes a values array in ABI order, so each host maps it
  to its own representation. The JVM `insn` backend keeps its physical
  `double[]`/`long[]` control arrays *inside* `hosts/jvm`, private.
- The bus half is not yet. `js-bus-kernel` and `wasm-bus-kernel` return the same
  `{:create-bus :run-bus! :reset-bus! :write-lane! :write-shared!}` and the page
  drives them with one runner, but `hosts/instrument.clj` drives the JVM bus
  instance directly and therefore carries `control-slots` /
  `write-bus-controls!` in its own body — a representation leak, not a semantic
  difference.
- Decision: give the JVM (and the interpreter) a bus kernel with those same five
  operations, with `control-slots` private to it; make `hosts/instrument`
  `.cljc`, taking `{kernel, bus-kernel}` as arguments defaulted per world (the
  linker's `:resolve` pattern). Then `instrument-pool`, `render!` and the
  play/`set-*` vocabulary are host-neutral — the same functions in all four
  cases, with the target the only difference.
- Consequence: the reference instrument's voice DSP and description can be
  `.cljc` (the live host staying in a JVM namespace), which makes it the worked
  example of a portable instrument and the natural subject of the four-path
  acceptance test.

## D86 — a state region's zeroes travel as a size, not as elements (`.bss`)

**Status.** In force (implemented and verified).

- A state region whose whole initial value is the zero pattern is the `.bss`
  half of the state: the host supplies it by *zero-filling* — a typed array,
  linear memory, a fresh primitive array — so only its size has to travel.
  Everything else is `.data`, written explicitly. The idea is the linker's, one
  level down: the zeroes need no bytes anywhere.
- The canonical form carries `{:dsp/zeros n}` where a state entry carried `n`
  zeroes. The test is on the *bits*: `-0.0` equals `0.0` but is not what a zeroed
  region holds, so it stays explicit (D76). A region with any non-zero element
  stays a vector, so the compaction can never eat a value.
- The JS emitter's arena is the zero-filled *shape* of the right length
  (`new Float64Array(n)`, or `Array(n).fill(0)` for a mixed arena), and `init`
  zeroes it and then writes only the non-zero initializers. `reset` runs the same
  code over state a voice has since written, so the zeros are *put back*, not
  merely left alone — which is the whole reason the compaction is safe. The Wasm
  emitter already did this (`memory.fill` + non-zero stores, the P12b fix); the
  JVM and the interpreter allocate fresh arrays, which the runtime zeroes.
- What is *not* compacted: the descriptor's materialized array initializers. The
  compaction lives where the region is used (the IR's canonical form and the
  emitters), not in the normalized descriptor, so the descriptor stays a plain
  declaration of what the author wrote.

**Evidence.** The reference instrument's folded post-mix bus (a 48 000-slot delay
line whose whole state is zero): canonical IR **1.77 MB → 40 KB**, JS module
**3.65 MB → 11 KB**, emission **4.7 s → 0.16 s**; the WAT is unchanged, because it
was already compact. JVM 279 tests / 2548 assertions / 0 failures; browser 54/54;
parity 211 cases, no gaps. A browser run that had been timing out on the payload
now finishes in well under a second.

## D87 — the compiler names no example; an example is compiled as data

**Status.** In force.

- Nothing in the compiler or its general harness — `descriptor`, `linker`, `ir`,
  `source`, `sci`, `capture`, `registry`, `targets`, `hosts`, or
  `harness.conformance` — names a particular example. The payload a check
  compares against is built from *entries* (`{:id, :definition, :entry,
  :channels, …}`) and knows nothing about whose they are; the example is named
  where the example is known, which is the harness that assembles its payload and
  the browser check that hands over its own definitions.
- The reference instrument is one `.cljc` file both worlds capture with their own
  macros, and it is compiled through the same `dsp/compile` as any other
  definition. It is not special-cased anywhere. If compiling it ever needed a
  change to the compiler, that change must be a *general* capability with its own
  justification and its own test — a combinator the source layer may use, a
  resolution rule the linker applies to any definition — never a hook for this
  example.
- The check is `reference instrument equivalence`: the page compiles its own
  capture of the voice (the `dsp/defn` forms Shadow compiled into the bundle) and
  the bus *definition* the JVM folded (a derived definition is plain data, so it
  travels), and compares descriptor, canonical IR, and both artifacts with the
  JVM's. It is the same generic comparison the catalog cases use.

## D88 — the reference instrument is one `.cljc`, portable on every target

**Status.** In force.

- `omkamra.dsp.examples.reference-synth` is a single `.cljc`. Its *definition*
  half — the three `dsp/defn`s, the `unison` combinator, the description, and the
  `omkamra.dsp.instrument` value with its modulation matrix and post-mix chain —
  is portable and is captured by both worlds' macros. Its *host* half is portable
  too (a pool, a bus and a modulation loop hold only primitive arrays and the pool
  API); the one JVM-side function is `build-instrument`, which lowers and compiles
  the instrument value, and it is `#?(:clj …)`. A browser builds the same live
  host from a served program with `hosts.instrument/instrument-pool` and the
  kernels it built there.
- The `definitions` map declares the namespace's own definitions by qualified id,
  because ClojureScript has no runtime namespace resolution (T7) and the page must
  resolve a same-namespace call. The ids are the ones the linker derives from each
  definition's `:source :namespace`; a test asserts it, because a mismatch is
  silent on the JVM (which resolves through Vars).
- It compiles and runs on the interpreter, the JVM, JavaScript and Wasm, from
  both worlds, and with or without SCI. *Without*: the static macros capture it,
  and the page compiles its own capture of the voice (the `dsp/defn` forms Shadow
  compiled) plus the bus definition the JVM folded — that is the `reference
  instrument equivalence` check, and it holds on all four targets. *With*: its
  own source, read back from the `.cljc` it is written in (never transcribed),
  compiles identically through SCI and through the JVM's reader and macros; a
  test asserts it. The capability SCI needs for it — a combinator, which runs
  while the source is evaluated — is exposed to the sandbox and checked by the
  `:source-combinator` source case.
- **The one wrinkle, and it is the language's, not the example's.** A browser
  reader has a single number type, so it cannot keep a bare `:int` literal
  (`white-noise`'s seed) apart from an integral float — the ambiguity `#dsp/float`
  exists for (D78). `literals/box-float-literals`'s ClojureScript branch currently
  boxes *every* number, which makes a bare integral literal a float and so makes
  an `:int` literal unspellable in browser source. The reference source is
  therefore checked with SCI on the JVM, where the reader keeps the type. Making
  browser source spell ints (box only non-integral numbers, and write integral
  floats as `#dsp/float`) is a language-contract change and is left open.

## D89 — the instance lifecycle is its own namespace; the facade re-exports it

**Status.** In force.

- The process-instance lifecycle splits by what it *names*. `omkamra.dsp.instance`
  holds the target-neutral vocabulary — `process-instance?`, `create-instance`,
  `destroy!`, `invoke`, `process!`, `initialize!`, `reset!` — and requires no
  target: it drives the `:create-instance`/`:invoke`/`:invoke-context`/
  `:initialize`/`:reset`/`:destroy` an artifact carries, so the interpreter and
  the JVM run through it and a browser's kernel does the same job its own way.
  `omkamra.dsp.instance.jvm` holds the two raw ABIs (`process-checked!`,
  `process-unchecked!`), which hand a JVM artifact's physical control arrays to
  `omkamra.dsp.targets.jvm`. The generic namespace also exposes `check-live!`,
  the holder check the JVM half shares; it is the one public helper the facade
  does not re-export.
- The facade `omkamra.dsp` **re-exports** all nine rather than defining or
  wrapping them, so `dsp/process!` is the same function as `instance/process!`:
  one implementation, one docstring, one `:arglists`, and a stack frame that
  names the defining namespace. A test pins the re-export (and its metadata),
  because a wrapper is otherwise indistinguishable until it drifts.
- The reason is the host, not the API. `omkamra.dsp.kernels.jvm` drives an instance
  in its own body, and it used to require the facade — the macro namespace that
  also pulls the SCI front end, the library registry and the definition forms —
  to call `create-instance`/`reset!`. It now requires `omkamra.dsp.instance`
  (and the JVM half for the raw ABI), so the facade is a pure entry point and
  internal code no longer depends on it.
- The lifecycle is JVM-only *today* because that is how far it reaches: a browser
  drives a pool through the `Kernel` protocol (`omkamra.dsp.synth.pool`), and a
  JavaScript or Wasm kernel is an instance of its target's own making. The split
  does not prevent `instance` becoming `.cljc` if a browser app ever needs the
  artifact-level lifecycle — there is now no target dependency in the way.
- `compile`, `link` and the macros stay in the facade, where they already
  delegate.
- **Why a re-export and not a builtin alias.** Clojure has no facility that makes
  a *qualified* name resolve to another namespace's Var. `:refer`/`refer` shares
  the Var — a refer would make `ns-resolve` return the implementation's Var — but
  qualified resolution uses `Namespace.findInternedVar`, which skips refers, so
  `omkamra.dsp/process!` would not compile. `intern` gives a qualified name but a
  *new* Var, with a double indirection (`@#'dsp/process!` is a Var, not the
  function) and no `:doc`/`:arglists`. So the copy is the price of the facade's
  stable qualified name, and the defining namespace's Var is the one seam a
  caller can redefine. (This is why `potemkin/import-vars`, were it a
  dependency, would change nothing here.)

## D90 — browser source spells an `:int` bare and an integral float `#dsp/float`

**Status.** Superseded as to the *spelling contract* by D91; the
`box-float-literals` predicate it decided stands. (Amends D78.)

- D78 decided a float literal travels as `{:dsp/float 1.0}`, because a platform
  with one number type cannot carry the type in the value. It did not say what a
  *reader* does when it cannot tell the spellings apart at all.
  `box-float-literals` tested `(or (double? form) (float? form))`, and in
  ClojureScript both `double?` and `float?` are `number?` — so it boxed **every**
  number: a bare `12345` became a float and an `:int` literal had no spelling in
  browser source at all.
- The rule is now `(and (number? form) (not (integer? form)))` — *whether the
  value has a fractional part*, one predicate for both hosts. On the JVM it boxes
  `2.0` (a `Double` is not an `integer?`) and leaves `2`; in ClojureScript it
  boxes `0.5` and leaves `2`. The contract follows:
  - a bare integral literal is an `:int`, in both worlds;
  - an integral **float** in browser source is written `#dsp/float 2.0`, because
    a one-number-type reader reads `2.0` as the integer `2`.
- This changes only **SCI reading a source string in the browser**. The other
  paths are unaffected: Clojure's reader (the `:native` evaluator, SCI on the
  JVM) keeps `Long` vs `Double`; and a `.cljc`/`.cljs` definition is read by
  Shadow's *compile-time* reader, which runs on the JVM, so the JVM macro boxes
  its bare floats correctly. `#dsp/float` is therefore a **browser-source-string**
  spelling, not something a statically-compiled definition needs.
- **The reference synth is deliberately left in natural spelling.** Its `.cljc`
  source is the test corpus for the compile-time path: those bare floats are what
  `reference instrument equivalence` checks, compiling the page's own capture and
  comparing descriptors with the JVM's. Writing `#dsp/float` into it would make
  the user-facing example noisier and would replace a test of the reader's type
  preservation with a test that a box survives. The browser source-string path is
  `source-cases`: `:source-int` (a bare integer that must stay `:int`) and
  `:source-chain` (an integral float written `#dsp/float`).
- **Rejected: a Clojure-typed reader in the browser.** A ClojureScript host has
  one number type and no `Long`, so no reader running there can keep the
  distinction — `cljs.core/double?` and `float?` are `number?` for the same
  reason. A pre-tokenizer that re-tags numeric lexemes before SCI reads them is a
  second reader that has to stay in step with the real one (strings, comments,
  chars, regexes), which is more machinery than the spelling contract.

## D91 — the front end recovers a literal's spelling from the source text

**Status.** In force (supersedes D90's spelling contract; the D90 predicate stands).

- The goal is round-tripping, not a contract: a Clojure namespace written in an
  editor and pasted into a browser app that compiles DSP source must produce the
  same artifact as compiling it in the namespace. D90 got the `:int` half but
  made an integral float in browser source need `#dsp/float 2.0` — a user-visible
  rule that defeats the goal.
- The spelling exists only in the text. `cljs.tools.reader`'s private
  `read-number` has the raw string and discards it; a JS number cannot hold it.
  So the *front end* reads the source with the real reader —
  `read+string` on a `source-logging-push-back-reader` returns each form **and its
  source text** — and evaluates the forms with the text attached under
  `:dsp/source-text` (`omkamra.dsp.source-reader/annotate`, printed back for
  `sci/eval-string*`).
- The capture is **polymorphic**: `source-reader/box-form` boxes from the text
  when a form carries one, and from the host value otherwise. So the JVM macro
  path — a `.cljc` file Shadow compiles, or Clojure reading a namespace — is
  untouched, and the SCI path recovers `2.0` from `2.0` and `12345` as an
  `:int`. `#dsp/float` remains the *representation*, and is still needed where
  there is no text: hand-written descriptors and transported EDN (D78).
- Only the structural walk is ours (delimiters, prefixes, `#_`, `#?`); every
  token is read by the real reader, so there is no second lexer to keep in step.
  `omkamra.dsp.tests.source-reader` pins the walk against Clojure's reader on a
  corpus, including `#_` and reader conditionals.
- Position needed the same treatment: a browser re-reading printed forms stamps
  its own `:line` over the printed one, so the original line and column travel
  under `:dsp/source-line`/`:dsp/source-column` and `capture/source-of` (and the
  SCI `dsp/def`) prefers them. Without that the descriptor's `:source` differed
  between the worlds.
- Acceptance: `:source-chain` is a bare `(* sample 2.0)` again, and the reference
  synth's own `.cljc` source — `:int` seed and bare integral floats and all — is
  a browser `source equivalence` case with no annotation. The page compiles it
  with SCI and matches the JVM's own reader, which is the goal stated as a test.

## D92 — a bare number is boxed from the form that encloses it

**Status.** In force (closes the gap D91 left).

- D91 recovered a literal's spelling from the form's *own* source text. An atom
  cannot carry metadata, so a body or a state `:next` that was a *bare number*
  fell back to the host value, and the page read `2.0` as `2` — a silently
  different artifact. That was the one gap D91 recorded.
- `annotate` now also attaches each collection's children's texts
  (`:dsp/source-children`, in reader order, a map's flat key/value), and the
  capture boxes a bare number with the text of the position it came from:
  `source-reader/box-body` for the body forms, `box-entry` for a state's `:next`.
- The scope is exactly the two positions the capture *boxes*. `:init` needs none —
  it is typed by its slot, and `descriptor/normalize-initializer` coerces it from
  the declared type — and `:process`/`:options` are neither boxed nor
  text-recovered, because a box there would fail validation.
- The body is a suffix of its container's children, so `box-body` takes the last
  `(count body)` child texts. The container is the definition form, except in the
  packed explicit-spec shape (`[{:params …} body]`), whose body lives inside the
  parameter vector; `capture-function` passes that vector then.
- A `:next` is found by re-reading the state map's own text and walking its
  key/value pairs in reader order, not by the map's iteration order: an array-map
  becomes a hash-map past eight entries, and the two orders would part.
- The JVM macro path is still unchanged. It carries no texts, so every helper
  falls back to the host value, where the type is already right.
- Proved by `:source-atom` in `source equivalence` — a body of `0.0` and a
  `:next` of `0.0`. Removing the two call sites and nothing else makes exactly
  that case fail, with `:expected :float, :actual :int` at `[:body 0]`, which is
  the pre-D92 bug; with them, the page compiles it and matches the JVM.
  `omkamra.dsp.tests.source-reader` pins `box-form`, `box-body` and `box-entry`
  directly.

## D93 — P13e is dropped: the cache does not pay, and the diagnostics are P15's

**Status.** In force (P13's slice 5 is decided-not-built, not omitted).

- P13e bundled three things (PLAN §4): incremental recompilation, descriptor/IR
  caching keyed on `ir/canonical-text`, and error classification separating SCI
  from DSP-validation and backend errors. The first two are dropped; the third
  moves to P15, which already owns structured diagnostics.
- **Incremental recompilation is real, but the cache is in the wrong place.** A
  linked unit holds every transitively reachable definition and `lower-unit`
  lowers them all, so editing a voice re-links, re-lowers and re-emits the whole
  unit — the reference voice's unit is 17 definitions, 13 of them unchanged
  library ones. A per-definition cache would hit only the descriptor and IR, and
  the phases measured on the dev JVM (reference voice → JS, 17 definitions) are:
  link 9.5 ms, lower 6.7 ms, emit ~58 ms, total ~74 ms. Emission is ~80% and is
  **whole-program** — `emit-module` lowers and emits the entire unit, and the
  Wasm artifact is one module — so one definition's emission is not reachable by
  a cache key at all. A one-definition compile is 9 ms end to end. And the
  descriptor half is already cached in effect: capture happens once, when the
  `dsp/defn` form is evaluated, and lives in a Var or in the registry.
- **Nothing depends on it.** The phase graph is P14 ← P12+P13, P15 ← P9+artifacts,
  P16 ← P11+P12+P14, H1 ← P13→P14. P14 needs the compiler-in-browser, which is
  P13a–P13d and done; no phase names P13e. It was the only slice of P13 off the
  critical path.
- **The error classification is P15's deliverable 2** — "structured diagnostics
  carrying source location, definition ID, dependency path, IR path, target,
  precision, offending form, and expected/actual types" subsumes it. Doing it in
  P13e would be a duplicate, or a prefix built out of order.
- **If compilation ever needs to be faster, the leverage is emission.** The
  emitters would have to produce per-definition pieces plus an assembly step
  before a cache could reach the 80%. That is its own decision, not this slice.

## D94 — a target's artifact shape lives in the target's namespace

**Status.** In force (P13d settling pass; no behaviour change).

- `compiler.cljc` is the portable pipeline (options → capability → link → lower →
  prepare → dispatch). It used to own what each target produces: `target-instance`'s
  `case` over all four targets, `compile-js-target`, the interpreter's instance
  closures and the whole Wasm artifact map inside
  `compile-function-target`/`compile-process-target` — about 130 lines of target
  code in the shared file.
- Now the pipeline computes the shared base (`artifact-base`: target, entry, IR,
  ABI, physical type, real-time policy, options) and hands it to the target's own
  `compile-artifact`, which adds only what that target has: the JS IR and ES
  module, the WAT and its assembler pin, the interpreter's executable and instance
  lifecycle. `:jvm` already worked this way — it registers `compile-target`'s
  `:jvm` method from the side that has `insn` (D77) — so the other three now match
  the pattern instead of being the special case.
- The Wasm assembler pin (`:generator`/`:version`, D73) moved with it. It names the
  Wasm target's assembler, so it lives in `targets/wasm`.
- The artifact shapes stay deliberately different — the interpreter and JVM carry
  `:invoke`, the two code generators carry a module — without the pipeline knowing.
- The **inlining policy** moved to the same place. `ir`'s `calling-targets` set and
  its `(= :interpreter target)` special case became `targets.capability/calls?`:
  whether a backend emits a call from a call site or evaluates the call graph
  itself. `ir/prepare-for-target` reads it, and `ir` names no target.
- **Verified by construction.** The artifact maps come out identical, so the proof
  is the existing cross-world checks — `compiler equivalence` (JS forms, Wasm
  module text, canonical IR) and `parity` — not a new test. JVM 284 tests / 2 620
  assertions / 0 failures, browser 54/54, parity 211 cases, no gaps.

## D95 — the synth bundle is world-independent; only the kernel is a world's own

**Status.** In force (P13d settling pass; no behaviour change).

- `omkamra.dsp.hosts.jvm` (`.clj`) was the JVM world's *assembler* under a name that
  promised all hosts: it compiled a description to a bundle, added a JVM kernel,
  and made the pool. The bundle half is not JVM-specific — its own docstring said
  `:runtime` "is pure data, so it can be sent to another host", and the browser's
  kernels are built from the same bundle — but it lived in a `.clj`, so a browser
  could only be handed one over the wire, which is what the harness does.
- `synth-bundle` and its real-time policy now live in
  `omkamra.dsp.synth.bundle` (`.cljc`). `omkamra.dsp.hosts.jvm` keeps what is really
  the JVM world's — `host-kernel`, `bus-kernel`, `compile-synth`, `synth-pool` —
  and went from 265 to 103 lines.
- Three seams made it portable, and each is a property rather than a conditional:
  - the voice is resolved by `resolve-voice`: a Var or a descriptor is taken as it
    is, an id is looked up in the caller's `:definitions` first, and only the JVM
    falls back to `requiring-resolve` — ClojureScript has no runtime namespace
    resolution (T7);
  - a descriptor, not a Var, is what `comb/guard`/`comb/poly` are handed, so a
    world that supplies descriptors works the same way;
  - the fused path's `(dsp/link …)` is gone. `dsp/compile` already links what it
    is given, with the caller's options, where the extra call linked with `{}` and
    so could not see `:definitions` — a latent bug removed by deleting a call.
- `:target` still defaults to `:jvm`, deliberately: `descriptor/default-options`
  defaults to `:interpreter`, so a bundle must not fall through to it. That is the
  one JVM-flavoured thing left, and it is documented in the namespace that owns it.
- **The browser world still has no assembler.** `hosts/instrument.cljc` says so —
  "a browser host has no compiler-side assembler here, so it passes `:kernel`
  built from the served bundle" — and the harness builds those kernels inline.
  P14 owns that assembler; `synth.bundle` is now the half it can be built from.
- Verified: the suite is unchanged (JVM 284 tests / 2 620 assertions, browser
  54/54, parity 211 cases). The `.cljc` was *checked*, not assumed: requiring it
  from the testbed page build made Shadow emit
  `cljs-runtime/omkamra.dsp.synth.bundle.js`, so it is ClojureScript-legal. Nothing
  in the tree requires it yet, so that is not a recurring check — the honest state
  is that a browser *can* build a bundle, but nothing does until P14.

## D96 — the names follow the axes: world, target, kernel

**Status.** In force (naming pass; no behaviour change).

- "Host" had come to mean three things — the external audio host, the *world* (JVM
  or browser), and a per-target `Kernel` implementation — and the last two shared
  a tree. `omkamra.dsp.hosts` was the JVM **world**'s assembler and the *parent* of
  `omkamra.dsp.hosts.jvm`, which was the **`:jvm` target's** kernel: parent and
  child on different axes, which is why the file kept nagging.
- Now the tree says which is which:
  - `omkamra.dsp.hosts.jvm` (`.clj`) — the **world**'s assembler (was
    `omkamra.dsp.hosts`). Its browser counterpart is P14's.
  - `omkamra.dsp.kernels.{jvm,interpreter,js,wasm}` — one **`Kernel` per target**,
    the protocol's own word, with the shared plumbing in `kernels.mixing`.
  - `omkamra.dsp.fusion` (top level) — pure data with no dependencies and a
    DSP-level concept (M6), so it sits beside `modulation`/`combinators` rather
    than inside a tree.
  - `omkamra.dsp.instrument.program` — the instrument program + live host, a layer
    *above* the pool (was `hosts.instrument`).
  - `omkamra.dsp.worker` — the off-thread sequencing entry, reachable only from
    `shadow-cljs.edn`; not library API at all.
  - the parent `omkamra.dsp.hosts` is gone.
- The four that shared the old tree were not one kind of thing, and the requires
  settled it: `mixing`/`fusion` are required by every kernel, `instrument` by the
  portable reference synth, and `worker` by nobody.
- `mixing` was not one subject, so it was cut by subject rather than renamed. Its
  host-different numeric half — `real-array`, `value-length`, `value-at`,
  `values-vector` — moved to `omkamra.dsp.numbers`, which is defined as where a
  host's numeric decisions live, and its region half — `region-layout`,
  `region-offset`, `write-region!` — became `omkamra.dsp.kernels.regions`. What is
  left in `kernels.mixing` is exactly what its docstring claims: rendering a voice
  into scratch, summing it into the block, and the containment policy. Renaming it
  to `kernels.common` was considered and rejected — that name says what code is
  shared *by* rather than what it does, and would have been the only subject-less
  namespace in the tree.
- Local aliases follow their namespaces: `*-host` → `*-kernels`, and the
  instrument program's → `*-program`.
- Module references were rewritten in place everywhere, including in
  `DECISIONS.md`/`DIARY.md` history: those entries point at code, and a stale
  pointer is worse than a rewritten one. A *file path* in the prose of an entry
  written before this one still names the tree as it was — history is not
  otherwise rewritten.
- Verified by the suite: 284 tests / 2 620 assertions, browser 54/54, parity 211
  cases — unchanged — and every moved namespace is compiled in its world
  (`kernels/js.cljs`, `kernels/wasm.cljs` and the page build are ClojureScript).
