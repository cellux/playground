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
  process's contract: a callee's `store-special` is rejected at lowering, because
  its slot would be buried in a call partition where no host reads it.
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
  moved out of `omkamra.dsp.synth.host` into a `omkamra.dsp.hosts` family —
  `.jvm`, `.interpreter`, `.js` — with the plumbing they share (the containment
  policy and the planar blocks a voice is rendered into and mixed through) in
  `omkamra.dsp.hosts.mixing`. That namespace is `.cljc`, so the JVM suite and the
  browser testbed run the *same* reference oracle; `omkamra.dsp.hosts` itself is
  the assembler (`synth-bundle`, `compile-synth`). `omkamra.dsp.hosts.js` is a
  `Kernel` over a generated JavaScript module, and a pool of those is checked
  against an interpreter pool in the browser over the same events (`JavaScript
  pool host`). The worklet adapter now calls the kernel **once** per block and
  hands it the callback's own channel arrays — one channel array for a mono
  kernel, the array of them otherwise — and silences the channels the kernel does
  not write; the kernel's shape (`channels`, `inputChannels`) comes from its ABI.
  Nothing is up-mixed, which is what slice 2 decided: placing a mono voice is the
  voice's own business (`effect/pan`). `omkamra.dsp.hosts.wasm` is the same host
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
omkamra.dsp.hosts.instrument      lowering: compile-instrument -> program
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
