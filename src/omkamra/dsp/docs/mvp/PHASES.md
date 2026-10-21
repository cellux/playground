# Implementation phases

This file turns `PLAN.md` into an ordered set of independently testable
implementation phases. Each phase is finalized when completed and is not
rewritten afterward. Progress belongs in the append-only `DIARY.md`; persistent
design decisions belong in `DECISIONS.md`.

## How to read this plan

The plan is a sequence of **vertical slices**. Except where a phase is
explicitly infrastructure-only, every language phase is delivered through the
full pipeline in the order required by `PLAN.md`:

```text
descriptor/syntax -> portable IR -> reference interpreter -> JVM -> JS -> Wasm
  -> shared conformance vectors
```

The reference interpreter is always the executable semantic specification. A
phase is not complete until the new behavior is differentially tested against
it for every supported target and precision, and the shared acceptance catalog
and browser harness are updated.

Phases are ordered by dependency, not by polish. Infrastructure phases
(P12–P16) are largely independent of the language/runtime spine and may be
reordered if a consumer needs them earlier, provided their stated dependencies
are met.

Phase identifiers are stable. Later phases never silently change an earlier
phase's meaning; a changed decision is recorded in `DECISIONS.md`.

Three lettered sections sit *off* the P spine, because nothing in the spine
depends on them: **M** (instrument parameters and modulation), **F** (fusion,
deployed) and **H** (closing the declared gaps). Each states its own goal,
dependencies and slices, and each ends with a status line saying what is done.

---

## P0 — Baseline (completed)

**Goal.** The existing system that every later phase builds on, recorded so the
roadmap has a fixed starting point.

**Delivered.**

- `omkamra.dsp` capture macros producing normalized source descriptors.
- Descriptor validation/normalization, deterministic linking, typed portable
  IR, and the reference interpreter.
- JVM (via `insn`), JavaScript, and Wasm (WAT) lowering and emission.
- Logical `:float`/`:int`/`:boolean`, mutable locals, `if`/`let`/`do`/`while`/
  `break`/`continue`, comparisons, short-circuit logic, explicit conversions,
  resolved expression-only calls.
- Persistent scalar state and fixed-size typed state arrays with explicit
  indexed `state-load`/`state-store`.
- Mono/multichannel block-process ABI, lifecycle, memory metadata, and the
  shared `omkamra.dsp.acceptance` catalog.
- JVM checked/unchecked primitive process adapters and the browser
  JS/Wasm AudioWorklet testbed.

**Known gaps at P0.** No bitwise integer operations, no math intrinsics, no
local arrays or delay lines, no stateful module composition, one owned state
instance per artifact, no timestamped event ABI, WAT-only Wasm artifacts, and a
JVM-only compiler front end. These are the subjects of P1–P16.

---

## P1 — Integer bitwise and shift operations

**Goal.** Complete the integer value type with bitwise and shift operations so
integer phase accumulators, phase masks, and LFSR-style noise can be expressed
in the language.

**Depends on.** P0.

**Operation set.**

- `bit-and`, `bit-or`, `bit-xor` — binary, `:int -> :int -> :int`.
- `bit-not` — unary, `:int -> :int`.
- `bit-shift-left`, `bit-shift-right` — binary, arithmetic shifts.
- `unsigned-bit-shift-right` — binary, logical shift.

Clojure-core names are used deliberately so the same source remains valid under
the future SCI front end (P13). Shift-distance masking and two's-complement
wrapping must match Clojure/JVM semantics on every target.

**Deliverables.**

1. Descriptor/intrinsic signature table in the portable IR (single source of
   truth for operator names, arity, operand types, result type, and effects).
2. IR lowering for the seven operators, wired into `expression-operators`.
3. Interpreter reference semantics (`bit-and` etc., arithmetic vs logical
   shift).
4. JVM bytecode lowering (`iand`/`ior`/`ixor`, `iconst_m1`+`ixor`, `ishl`,
   `ishr`, `iushr`).
5. JS emission with explicit 32-bit coercion (`&`, `|`, `^`, `~`, `<<`, `>>`,
   `>>>`).
6. Wasm emission (`i32.and/or/xor`, `i32.shl`, `i32.shr_s`, `i32.shr_u`).
7. Shared acceptance vectors plus JVM and browser (JS/Wasm) coverage.

**Done when.** Every operator is bitwise-exact against the interpreter for both
precisions where applicable, produces identical results on JVM/JS/Wasm for a
vector spanning negative operands, all shift distances, and the 32-bit
boundary, and an end-to-end integer phase-accumulator definition links,
compiles, and processes blocks on all four targets.

**Out of scope.** `rem`/`mod`, integer `min`/`max`, and bit-test/set/clear
helpers; these belong to P2.

---

## P2 — Portable scalar math builtins

**Goal.** Introduce the general builtin-operation mechanism and land every
scalar math operation whose result is bit-exact across targets.

**Depends on.** P1.

**Builtin contract (introduced here, reused by P3 and P12).** A builtin is a
stable portable-IR operation, not a host call. Each declares:

- a typed signature and arity;
- an effect set (none for these builtins);
- reference semantics implemented by the interpreter;
- a per-target lowering;
- a capability check that yields a structured unsupported-target diagnostic.

**Operation set.**

- Exact float operations: `abs`, `min`, `max`, `clamp`, `floor`, `ceil`,
  `round`, `trunc`, `fract`, `sign`, `sqrt`, `copysign`.
- Integer arithmetic completion: `rem`, `mod`, and integer `min`/`max`.
- `sqrt` is included here because IEEE-754 square root is correctly rounded on
  all three targets; transcendental results are explicitly deferred to P3.

`min`/`max`/`clamp`/`abs` may be lowered inline or delegated to the standard
library, but their reference semantics must be defined once in the interpreter.

**Deliverables.**

1. Builtin registry in the portable IR and a uniform lowering path.
2. Interpreter reference implementations, including signed-zero and NaN
   behavior for `min`/`max`/`copysign`/`sign`.
3. JVM/JS/Wasm lowerings and capability diagnostics.
4. Acceptance vectors covering NaN, infinities, signed zero, negative zero,
   and the rounding-mode boundaries.
5. A first language-level DSP helper (`clip`, windowing) expressed using the
   builtins to prove the mechanism is usable.

**Out of scope.** Transcendentals (`sin`, `cos`, `exp`, `log`, `pow`, `tanh`,
`atan2`); those require a documented tolerance policy and belong to P3.

---

## P3 — Transcendental math intrinsics

**Goal.** Add the transcendental functions required by oscillators, envelopes,
filters, and modulation, with a documented cross-target tolerance policy.

**Depends on.** P2.

**Operation set.** `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`,
`exp`, `log`, `pow`, `sinh`, `cosh`, `tanh`.

**Deliverables.**

1. Intrinsic definitions under the P2 contract. Each target lowers to what it
   has: the JVM to `java.lang.Math` and JavaScript to `Math` (native); Wasm,
   whose core has **no** transcendental instructions, to a portable emulation
   emitted as WAT and computed internally in f64 (see `DECISIONS.md` D14).
2. Documented reference semantics: the interpreter's host-`Math` IEEE-754
   `double` result, narrowed per precision, is the oracle, and the interpreter
   counts as a native target for this purpose.
3. A target-dispatched comparison: exact for the native targets and tolerant
   only on `:wasm`, where the emulation cannot reproduce the host `Math`
   bit-for-bit. The tolerance is the emulation's accuracy gate.
4. A structured diagnostic for an operation a target cannot serve at all.

**Done when.** A sine oscillator and an exponential ADSR envelope defined only
in DSP source render within tolerance on interpreter/JVM/JS/Wasm for f32 and
f64.

---

## P4 — Local arrays, constant tables, and aggregate state records

**Goal.** Extend the language from scalar locals and flat state arrays to
fixed-size memory usable for delay lines, wavetables, and structured filter
state.

**Depends on.** P3.

**Deliverables.**

1. Fixed-size local arrays with lexically scoped lifetime and explicit indexed
   load/store.
2. Aggregate state records: typed named fields, potentially nested, with a
   stable logical layout, plus arrays of records where required by P10 voices.
3. Initialization-time constant tables (wavetables, windows) materialized into
   instance state.
4. Explicit alignment and logical layout metadata surfaced into the ABI and the
   Wasm memory plan.
5. Development-time bounds checks on local and aggregate indexed access, and a
   structured diagnostic for indices that cannot be proven in bounds.
6. Lowering to primitive arrays on JVM/JS and to linear-memory regions on Wasm,
   with no allocation after initialization.

**Out of scope.** Dynamic allocation and resizable containers; all layout is
fixed at compile time.

---

## P5 — Delay lines, circular buffers, and interpolation

**Goal.** Provide the memory-access patterns every synth effect needs, built in
the DSP language on top of P4 and exercised by the acceptance catalog.

**Depends on.** P4.

**Deliverables.**

1. Circular-buffer state with an explicit write cursor and wrap arithmetic.
2. Fractional-delay reads with linear and allpass interpolation.
3. Documented update ordering and feedback framing so multi-tap delays are
   deterministic.
4. Shared acceptance vectors for wrap-around, minimum/zero delay, and
   fractional positions.

**Out of scope.** FFT/spectral buffers and complex layouts; these follow once
the array representation is proven.

---

## P6 — Stateful module composition

**Goal.** Make reusable stateful DSP modules first-class, replacing the current
expression-only call restriction.

**Depends on.** P5.

**Deliverables.**

1. Typed multi-input/multi-output module definitions with explicit state,
   buffers, and update ordering.
2. Calls to stateful and buffer-accessing callees: interpreter executes them;
   JVM/JS/Wasm emit or inline them under a defined policy.
3. Explicit feedback and delay boundaries and a deterministic state-update
   order within a frame.
4. Ownership rules stating which instance owns each piece of state.
5. A conservative inlining policy with code-size limits and a documented
   fallback.

**Done when.** A biquad filter and an ADSR envelope compose into a voice module
that runs identically on all four targets.

**Slices.** P6a (done) — per-call-site state ownership, the interpreter oracle,
and the shared statement inliner for JVM/JS/Wasm. P6b (done) — a module that owns
a record/array end to end, and record returns for multi-output. P6c (done) — the
done-when voice (`filter-voice`), plus the inlining policy (D21) and the
state-ordering rules (D22). All five deliverables are met. The partition-coloring
pass proposed under D20 is an arena optimization and is deferred, not part of
P6.

---

## P7 — DSP standard library

**Goal.** A portable library defined in the DSP language (intrinsics only where
P2/P3 justify them) covering the primitives named by the reference synth.

**Depends on.** P6.

**Contents.** Band-limited saw/square/triangle/sine oscillators; white and pink
noise; PWM and oscillator sync; AR/ADSR envelopes; LFOs; parameter smoothing
and slew limiting; one-pole and biquad (multimode, stable) filters; stereo
spreading; delay, chorus, and distortion effects; interpolation and
resampling helpers.

**Deliverables.** Library namespaces, per-module acceptance cases, and a
documentation page describing each module's state, latency, and modulation
inputs.

**Slices.** P7a (done) — the sub-namespace seams, docstring capture, and the
generated `LIBRARY.md` reference with a drift guard. P7b (done) — the
band-limited PolyBLEP oscillator family. P7c (done) — `if` branches are separate
scopes. P7d (done) — white/pink noise, an LFO, and a parameter-typing fix so a
non-`float` control type also types its parameter. P7e (done) — parameter
smoothing, slew limiting, a `tanh` waveshaper, and Catmull-Rom cubic
interpolation. P7f (done) — a `mode`-selected multimode biquad, linear AR/ADSR
envelopes, chorus, hard sync, and stereo spreading as a record-returning module;
every substantive definition gained a docstring. **P7 is complete.** Stereo
spreading's multi-channel *hosting* needs a multi-channel process and is
deferred to P8, as are D23 (optional input ports) and D20 (coloring).

---

## P8 — Generalize process instances

**Goal.** Separate reusable compiled artifacts from mutable per-instance state
and give every target an explicit create/reset/destroy lifecycle.

**Depends on.** P7.

**Deliverables.**

1. Target-neutral instance abstraction: `create-instance`, `reset!`,
   `destroy!`, independent of portable IR.
2. Interpreter/JVM factories creating multiple independent instances from one
   artifact, with primitive allocation-free process calls after setup.
3. JS `createKernel` and Wasm instance-region APIs unified under the same
   lifecycle vocabulary.
4. ABI additions for instance handles and ownership, without leaking
   target-specific handles into portable IR.
5. Differential tests for independence: two instances do not share state.
6. **Optional input ports.** A process may declare `:input false`: it then has no
   sample parameter and no `:input` buffer in the ABI, so a generator (oscillator,
   noise, LFO) stops carrying an ignored `sample` and a dead `:buffer-load` that
   falsely reports `:read-memory` in `:effects`. Done here rather than in P7
   because this phase *already* moves the process ABI's argument indices (the
   instance handle), so the descriptor/ABI/adapter rework is paid once.

**Slices.** P8a (done) — the target-neutral instance lifecycle for
`:interpreter` and `:jvm` (deliverables 1, 2), with differential independence
tests (deliverable 5). P8b (done) — the ABI describes one create/reset/destroy
vocabulary on every target (deliverable 4), and the JS kernel gained its
`destroy` (deliverable 3, the JS half). P8c (done) — the browser suite proves
JavaScript and Wasm instances are independent, completing deliverable 3; the
check carries a negative control, which caught that a 1-sample delay flushes
itself inside a block and therefore could not have detected sharing at all.
P8d (done) — deliverable 6, **optional input ports** (D23): `:input false` gives
a process no sample parameter, no input buffer, and no `:read-memory` effect, and
the process ABI's argument indices are now derived rather than hardcoded. The
whole library's generators use it. **P8 is complete.**

---

## P9 — Sample-accurate automation and event ABI

**Goal.** Bounded, allocation-free automation and timestamped events dispatched
at exact frame offsets.

**Depends on.** P8.

**Deliverables.**

1. Monotonic absolute frame timestamps and a strictly increasing ingress
   sequence number ordering equal timestamps.
2. The full v1 event set: note-on/off with velocity and release velocity,
   pitch bend, controller and parameter changes, pressure/per-note expression,
   transport tempo/beat, and sample-rate changes.
3. Parameter ramps and one-pole smoothing as ABI-level block-rate constructs.
4. A preallocated ingress queue with SPSC semantics and the deterministic
   drop-oldest overflow policy, never dropping a due/being-dispatched event.
5. Fixed diagnostic counters for overflow and non-finite containment, updated
   outside the audio path.
6. Tests proving rendering is identical across block sizes and event
   partitionings on interpreter, JVM, JS, and Wasm.

---

**Slices.** P9a (done) — `omkamra.dsp.event`: the v1 vocabulary with closed
payload validation, frame-then-sequence ordering, and the preallocated SPSC
queue with the drop-oldest-future policy and fixed counters (deliverables 1, 2,
and the queue half of 4). P9b (done) — `omkamra.dsp.schedule`: block-relative
segmentation, a reusable allocation-free segment, and host-driven rendering, with
partition invariance proven exactly on interpreter, JVM, JavaScript, and Wasm
(deliverable 6). The kernel needed **no** change, because a segment boundary is
just a point where the host sets controls and calls the process again (D24).

P9c (done) — a control may declare `:smooth` (one-pole) or `:slew` (bounded
step), both applied **per frame** by the compiled frame loop. This is the
smoothing half of deliverable 3, and it needed no new IR node, no ABI change and
no backend change: a per-frame one-pole desugars into state and arithmetic the
language already has, with the control bound to a state read exactly as the input
sample is bound to a buffer load (D27). P9d (done, all four targets) —
host-supplied per-frame automation **lanes**: arbitrary host curves, flat in one
region with a compile-time stride and named individually in the ABI (D28). Lanes
are covered by an `:automation-lane` case in the shared catalog, so the browser
matrix checks JavaScript and Wasm against the portable-IR oracle.

**Not done.** Two halves of two deliverables:

- **Deliverable 3, the *linear/critical* ramp half.** A ramp to a target needs a
  per-frame division plus a counter, or a block-rate prologue the IR does not
  have. Per D27 the host can supply such a shape as a lane instead, which is why
  this is not blocking: the kernel carries the general mechanism and a host
  supplies the curve.
- **Deliverable 5, the non-finite *containment* half.** The overflow and
  rejection counters are done and tested; containment belongs with P10's voice
  pool, where "the affected voice" exists to contain it.

---

## P10 — Voice pool and synth runtime

**Goal.** Polyphonic voice management above the kernel layer.

**Depends on.** P9.

**Slices.** P10a (description, voice pool, allocation/stealing, note lifecycle,
block rendering) is complete — see `DIARY.md`. P10b-i (voice modes:
mono/legato/retrigger, D33) is complete; `:unison` turned out to be a voice
*transform* rather than a mode (D36), and is out of the pool. P10b-ii delivers per-voice and
global modulation as an **example instrument** rather than a core schema (D34),
and promotes the standard pitch-domain sources it demonstrated into core (D35):
`:bend`/`:detune` in `control-sources`, `:bend-range`, the pool's global source
bank and `read-controls!`, and the library `pitch->frequency` default. P10c adds
voice and effect tails with
non-finite containment, which is where the kernel must start reporting that a
voice has finished; P10d adds stereo spreading and the browser hosts (JavaScript
and Wasm) behind the pool, together with the worklet boundary.
Deliverables 1, 2, 6 and the core of 3 land in P10a; the modes of 3 in P10b-i; 4
in P10b-ii; 5 across P10c and P10d.

**Status.** Complete. See `DIARY.md` for P10a–P10d and `DECISIONS.md` D33–D41.

**Deliverables.**

1. Preallocated voice pools with explicit capacity and per-voice state layout
   (using P4 aggregate/array facilities).
2. Deterministic, documented voice allocation and stealing.
3. Per-voice init/release/reset/terminate lifecycle, mono/legato/retrigger/
   poly/unison modes.
4. Per-voice and global modulation routing with bounded route counts.
   *(Demonstrated in `omkamra.dsp.examples.demo-synth`; the route schema is
   deliberately left out of core until the reference synth forces its shape — D34.)*
5. Voice and effect tails, stereo spreading, and arbitrary block sizes.
6. A serializable host-target sequencing configuration separate from the Wasm
   module.

---

## P11 — Reference synthesizer acceptance fixture

**Goal.** The v1 acceptance instrument from `PLAN.md`, end to end.

**Depends on.** P10.

**Deliverables.**

1. Two band-limited oscillators plus noise; sync, PWM, unison; envelopes and
   LFOs; a resonant filter; a modulation matrix.
2. Polyphonic allocation and stealing, stereo output, and delay/chorus/
   distortion effects.
3. Deterministic offline rendering across block sizes and all backends.
4. Tests for filter stability under modulation, aliasing, absence of zipper
   noise, event-boundary correctness, non-finite containment, and zero
   allocation after initialization.

**Status.** Complete. P11a delivered the reference instrument and its modulation
matrix (`omkamra.dsp.examples.reference-synth`); P11b the Clojure-level
instrument graph and a post-mix chain (chorus, distortion, and a musical
feedback delay), lowered to a program (D42); P11c all-backend offline rendering,
with a JavaScript/Wasm bus host and an instrument host check in the browser;
P11d the property battery (filter stability under modulation, zipper-noise
smoothing, containment, allocation, aliasing, event boundaries). See `DIARY.md`
for P11a–P11d and `DECISIONS.md` D42. The browser instrument check uses a fixture
instrument rather than the full reference synth; the bus DSP and the reference
instrument's own checks cover that gap, and the phase ends with a refinement
round over D42's graph and D34's matrix.

---

## P12 — Complete Wasm artifacts and memory ABI

**Goal.** Turn WAT-only Wasm output into a complete, instance-oriented artifact.

**Entry state — audited.** Parity across JVM, JavaScript and Wasm was verified
first, because P12 changes the Wasm memory ABI and nothing in the spine should have
to wonder whether a target is behind: identical builtin surfaces, no target-specific
refusal left in any host, 93 process cases with no parity gap in f32 or f64, and one
verification hole closed (`with-buffers` regions, now a shared catalog case). The one
standing asymmetry is architectural and untouched here: Wasm has no function-entry
form, so its operators are covered through process cases. See the diary entry *Parity
audit before P12*.

**Entry state — the binary step was spiked, then moved out of this phase** (D72 →
D73). WABT 1.0.39 was pinned and measured (byte-identical output from the native
release and the page's npm CLI, all 179 artifacts valid, the corpus in under a
second), and that is what settled the question the other way: the assembler belongs
to the browser's world, like `insn` belongs to the JVM's and `squint` to the JS
one, so the artifact *names* it (`:generator :browser-wabt`, `:version "1.0.39"`)
instead of carrying bytes, and the consumer that has it assembles. P13/P14 already
describe that pipeline. The spike also found where the size is:
`feedback-delay-f64` is a 768 KB module because its 48 000 zero initializers are
emitted one `f64.store` each, while Wasm linear memory already starts zeroed. That is
deliverable 5 below.

**Depends on.** P0; benefits from P8 (instance regions) and P4 (layout).

**Deliverables.**

1. ~~A defined WAT → WABT → binary compilation step producing validated
   `:bytes`.~~ **Dropped (D73)**: the artifact names the assembler its consumer
   uses (`:generator`, `:version`, both pinned) and the browser assembles, as P13's
   pipeline already says. `:bytes nil` means "assemble this with the named tool",
   not "not implemented".
2. Persistent state moved from globals into explicit linear-memory instance
   regions; reusable independent instances. *Already true since P8*: the emitter
   has no globals, instance independence is proven on Wasm, and this phase should
   confirm it rather than rebuild it.
3. Removal of the fixed 4096-byte channel stride in favor of ABI-declared
   layout, with validated alignment, capacity, and non-overlap.
4. An artifact exposing `:wat`, `:exports`, `:memory`, `:abi`, `:metadata`, and
   the named, version-pinned assembler — the shape PLAN.md's criterion 9 asks for,
   amended by D73 from "contains validated binary bytes" to "names the step that
   produces them".
5. **State initialization as a blob, not one store per initializer.** The
   initializers become a data segment (or nothing at all where they are zero,
   which linear memory already guarantees), so module size follows the *non-zero*
   part of a state region instead of its capacity. Measured today: 48 002 stores,
   768 KB module, for a buffer that needs no initialization whatsoever.

**Slices.** Two, and neither needs a new dependency or a new tool.

- **P12a — the layout.** Remove the fixed channel stride; pack the regions; declare
  alignment, capacity and non-overlap in the ABI and *validate* them; make every
  reader (`hosts/wasm.cljs`, the conformance runners, `tests/wasm.clj`) take the
  numbers from the ABI rather than recomputing them. Verified exactly as today:
  WAT through the page's WABT, plus the four-target parity table.
- **P12b — the init blob (deliverable 5).** Emit the non-zero initializers as one
  data segment and drop the zero ones, pinning the size delta on the case that has
  it (`feedback-delay-f64`). This slice matters *more* under D73 than it did before:
  the 48 002 redundant stores are 2.2 MB of WAT **text** that the page otherwise
  assembles on every request, so the data segment shrinks the artifact, the module
  and the compile time at once.

---

## P13 — Browser-side compilation (SCI)

**Goal.** Edit and compile DSP source in the browser with no server-side
compiler.

**Depends on.** P2–P4 for the language surface; independent of P8–P12
otherwise.

**Deliverables.**

1. An SCI-compatible `omkamra.dsp` macro namespace that registers serializable
   descriptors, with an allowlisted namespace and safe literal/core forms.
2. A browser definition registry replacing Var/`ns-resolve` linking, with
   explicit representations for aliases, refers, source locations, and
   diagnostics.
3. `.cljc`/`.cljs` ports of descriptor validation, linking, IR, and the JS/Wasm
   emitters (or semantically identical browser implementations).
4. A JVM/browser equivalence test: equivalent source yields byte-identical
   normalized descriptors and portable IR.
5. Incremental recompilation, descriptor/IR caching, and error classification
   separating SCI, validation, and backend errors.

---

## P14 — Artifact-driven worklet installation

**Goal.** Install AudioWorklet kernels from artifact ABI metadata rather than
testbed-assembled configuration.

**Depends on.** P12, P13.

**Deliverables.**

1. Adapter configuration generated from ABI metadata: processor name and module
   source, channel layout, control names/defaults/ranges/rates, element type
   and frame capacity, and Wasm offsets/strides/exports/memory.
2. Complete status, initialization, and reset protocols for both the JS and
   Wasm adapters.
3. Removal of worklet configuration reconstruction from
   `omkamra.dsp.testbed`.
4. A no-allocation audio callback verified after initialization.

---

## P15 — Safety, diagnostics, and optimization

**Goal.** Enforce real-time safety statically and add conservative optimization
after conformance holds.

**Depends on.** P9 (event/voice runtime) and the target artifacts.

**Deliverables.**

1. Static or policy diagnostics for allocation, reflection/dynamic invocation,
   locks/logging/I-O, unbounded loops, unvalidated memory access, and
   unsupported capabilities in the audio path.
2. Structured diagnostics carrying source location, definition ID, dependency
   path, IR path, target, precision, offending form, and expected/actual types.
3. Optional debug dumps for normalized descriptors, linked units, portable IR,
   target IR, emitted source/WAT, and ABI layouts.
4. Conservative optimization: precision-aware constant folding, dead-local and
   unreachable-branch elimination, local copy propagation, bounded-loop
   simplification, and size-limited inlining, each guarded by differential
   tests and a no-reassociation rule for floating point. Two further candidates
   surfaced by the combinator work (D38): deduplicating identical inlined bodies
   (an unrolled voice emits K copies through `inline-calls`) and folding
   **stateless** common subexpressions only. `lower-call` already hoists a
   computed argument to a local before inlining, so substitution cannot
   re-duplicate it per use — nor run a stateful argument twice.

*(P15a, done — pulled forward.)* Deliverable 1's capability half: every target
declares what it serves (`omkamra.dsp.capability`), and `dsp/compile` refuses a
request it cannot honour, naming what it does serve (D29). The three real
differences are runtime validation, the f32-only lookup table, and Wasm's
process-only entry. The remaining half of deliverable 1 — allocation, reflection,
locks, unbounded loops, unvalidated memory access — is open, though the IR's
`:diagnostics` channel already carries the loop and dynamic-index warnings.

---

## P16 — Automated conformance and release gates

**Goal.** Make the correctness and performance gates from `PLAN.md` executable.

**Depends on.** P11, P12, P14.

**Deliverables.**

1. A CI browser test job running generated JS, Wasm, and both AudioWorklet
   adapters, replacing the manual testbed.
2. Precision-aware differential reports treating NaN and signed zero
   explicitly.
3. Expanded shared vectors covering all operators, control flow, calls, state,
   memory, partial/zero blocks, special values, overflow, conversion
   boundaries, malformed source, and memory/export validation.
4. Allocation, latency, throughput, and worst-case-work budgets recorded as
   regression thresholds separate from semantic tests.

---

## M — Instrument parameters and modulation

**Goal.** The instrument layer above the compiler: named instrument parameters,
trigger/event handling, and a modulation matrix lowered by generators into either
the sequencer (Clojure/ClojureScript) or the kernel. The design is recorded in
`docs/mvp/MODULATION.md` and `DECISIONS.md` D44.

**Why a letter, not a number.** M is a separate workstream, not part of the P
spine's dependency order. It builds on the sequencing layer (P9), the voice pool
(P10), and the reference instrument (P11/D42), but it is a different axis —
instrument semantics and host topology rather than language or target features.
Lettering it keeps the P identifiers stable.

**Depends on.** P9 (events/`schedule`), P10 (voice pool), P11 (reference
instrument, D42). M4 benefits from P12 (instance memory); M5, when built in the
browser, from P13/P14.

**Deliverables.**

1. An instrument **parameter layer**: declared parameters with a scope
   (`:instrument` or `:voice`) and, for a voice parameter, an addressing
   (`:shared` or `:per-voice`); bindings from parameters to controls; and an API
   to set them.
2. **Triggers and handlers**: a general trigger event, a dispatcher keyed by
   trigger, and `note-on`/`note-off` as built-in triggers whose handlers
   allocate/release a voice and populate `gate`/`note`/`velocity` as ordinary
   parameters.
3. The **matrix as a generator input** (never instrument state): an instrument
   reducer generator and a kernel reducer generator sharing one route schema.
4. The **edge-scheduling model** (D44): instantaneous edges by default, `:delay`
   to close a cycle, acyclicity of the instantaneous subgraph validated once at
   generation time.
5. **Sequencer placement**: the sequencer off the audio thread, driving the pool
   through the preallocated event queue and `schedule`, sample-accurate and
   invariant across block sizes; in the browser, a Web Worker feeding the
   AudioWorklet's queue.
6. **Lane delivery through the pool**, so a control can carry a host-supplied
   per-frame curve (D28) and compose with `:smooth`/`:slew` (D27).
7. *(Deferred)* a **shared kernel region** for instrument-level parameters, so a
   broadcast parameter is one state variable every voice reads.

**Slices.**

- **M1 — Event-driven host (transport first). Done.** The sequencer consumes the
  event queue and drives the existing pool/kernel through `schedule`,
  sample-accurately, with no new DSP or ABI: the offline core (JVM/interpreter
  partition invariance) and the browser worker/worklet transport (a Web Worker
  posting timestamped events over a `MessageChannel` to a worklet that dispatches
  them at their exact frames). See `DIARY.md`.
- **M2 — Uniform parameters and triggers. Done.** M2a: parameter declarations
  (one `:per-voice`/`:shared` scope, optional, defaults inherited from the
  control), name-matching bindings, the pool's parameter store and API, and the
  retirement of the provenance half of `synth`/pool. M2b: the sequencer is keyed
  by an event's trigger (`(fn [definition state event] state')`), drives several
  channel-routed pools on one timeline, and sums them.
- **M3 — Generators. Done.** M3a: the route schema (`normalize-matrix`,
  `validate-matrix`) and the shared scheduling pass (`matrix-plan`). M3b: the
  instrument reducer generator (a reducer over the instrument's parameter
  values, `(fn [state] state')`), the state reader and publish step, and the
  reference re-expressed on them (a per-route `:offset` added for the note
  mapping). M3c: the kernel reducer generator
  (`(fn [definition] derived-definition)`) — added source controls, `-mod` locals
  in topological order, a `:delay` shift register, and a test that the reference
  matrix run through the kernel generator agrees with the instrument reducer.
- **M4 — Lanes through the pool. Done.** Deliver per-sample control
  values to voices. M4-0 done: the language surface — a lane belongs to a control
  (`:has-lane`) and is read through the compile-time `(lane control)` accessor,
  so the client composes it. M4a done: one shared region through the pool on the
  JVM and the interpreter (`Kernel/write-lane!`, `pool/set-lane!`, the layout in
  the runtime config). M4b done: the JavaScript and Wasm kernels deliver the
  region too (Wasm copies it into each instance's memory), verified by two
  browser checks against the interpreter pool. Open: M4c (per-voice lanes).
- **M5 — Shared kernel region. Done.** Broadcast as one shared variable: a control
  may declare `:shared true`, which takes it out of the scalar control list, and
  its name is a read of the process's `:shared` region — one region for the
  *pool*, so a broadcast parameter is written once instead of once per voice. The
  region is deliberately the lane region's shape (one `:frame-capacity` slice per
  control, `:shared-stride` = `:lane-stride`), so it needs no new IR op and no new
  addressing. M5a: the language surface (with `:shared` refused beside a lane and
  on a non-float control), the IR binding, the ABI, all four emitters, and the
  interpreter/JVM raw path. M5b: the kernels own the region — the interpreter and
  the JVM pass it by reference, JavaScript as a trailing argument, Wasm by
  address with a copy of the pool's region into each instance's own memory before
  it runs. M5c: a `:shared` parameter bound to a `:shared` control is written once
  into that region (the pool seeds the declared defaults at construction and
  never touches a voice), an undeclared shared control infers a shared parameter,
  and a `:per-voice` declaration beside a `:shared` control is refused. Browser
  checks pin literal numbers on both hosts; a post-mix bus that declares a lane or
  a shared control is still refused at construction.
- **M6 — Fused voice lowering, as an option. In progress: M6a, M6b and seam 4
  done, M6c remains.** Compile
  an instrument's voices into one DSP process per *instance* rather than one
  instance per voice, so the host crosses the kernel boundary ⌈capacity/slots⌉
  times per block instead of capacity. **The pool stays the default**, and is the
  S=1 case: fusion is *requested*, because occupancy is runtime behaviour and the
  sign of the comparison depends on it (D45's post-D47 measurements: the pool is
  ~2× slower at a full patch and 2–7.5× *faster* at two voices, flat in capacity).

  **M6-0 (done) — the measurement that gated the rest.** For `(poly S voice)` on
  the JVM, code grows ~30 bytes per slot even when the body uses scratch, because
  what unrolls is the call site and the buffer is arena data; memory is
  arena-linear and equal to the pool's. So there is no artifact cliff to dodge,
  and `S = capacity` (one instance) is the natural first target — chunking is the
  escape hatch for a very large capacity or for coarse skipping. One limit found:
  `poly` stacks *module-shaped* generators only; a process-shaped voice
  (`reference-voice`) is refused, since a module may not touch the caller's
  buffers. See D45.

  The sweep is done too (D45): on a voice with a real delay line, all slots
  sounding, fusion is ~1.9× the pool at 32 slots, the two lowerings are **bit-exact
  at one voice**, and the pool's artifact stays flat in N while the fused one grows
  with its slots. What remains of **M6a** is the slot-aware host, which needs four
  seams, in this order:

  1. an **instance key** on the `Kernel` protocol, so the pool mixes a fused
     instance *once* per block (its other slots are no-ops for that block) and can
     skip an instance whose slots are all silent — the pool's own view stays
     per-slot, which is what keeps the pool path unchanged. **Done**: the key is
     any value with an *equality* contract, the pool numbers the instances it
     created itself, and the marks are cleared per block. Pinned by three tests,
     including one that sounds every slot across two blocks and one that builds two
     pools over one kernel;
  2. a **base → fused control mapping**, so `write-voice-controls!` for one slot
     writes that slot's slice of the fused control array and the shared controls
     once. The kernel owns it: it is the component that has both ABIs. **Done**
     for the JVM (D50): `hosts/fusion/fused-mapping` is the pure mapping — the same
     answer on every host that holds both interfaces — and
     `hosts/jvm/fused-jvm-kernel` applies it through one precomputed row per slot,
     so the write path is still a single branchless loop. A fused pool now renders
     exactly what a plain pool renders, slot by slot, and the routing bites: with
     the slot term removed the test's fusion is silent where the pool sounds.
     **Found on the way**: `poly` redeclares a voice's controls as `{:name name}`,
     so a fused interface loses a voice's declarations — measured for `:shared`
     (it compiles as an ordinary scalar, with an empty shared region) and for a
     lane (the IR refuses it outright); `:default`/`:has-lane` follow from the same
     line. The instrument option therefore needs `poly` to carry a control's
     declaration through — or must refuse a voice that declares one;
  3. **per-slot reset**, so stealing or retriggering one slot does not clobber its
     neighbours. The fused state is partitioned per call site (D19), but nothing
     exposes those ranges yet. **Done** for the JVM (D51): the lowering records
     each call site's partition in evaluation order (slot order for a fused
     process), the target resolves it to physical state ranges on the artifact, the
     instance keeps each partition's initial values, and the kernel resets one
     range. Stealing into slot 0 of a two-slot instance leaves slot 1's envelope
     and delay line bit-for-bit alone, and a whole-instance reset is caught: the
     fused block collapses to silence where the pool still sounds;
  4. per-slot **`:activity` readback**. **Done** (D54): a callee may now publish a
     special, the call site's partition records it — one per bit, covering the
     whole subtree — the ABI publishes `:call-partitions`, and the JVM fused
     kernel resolves each slot's activity to a physical slot so
     `read-specials!` reads the voice the pool asked about. A fused pool now frees
     a finished voice exactly as a plain pool does, and the free is a partition
     reset, so it clears the guard's latch too. Originally deferred, with this
     reasoning recorded as the shape of the slice:
     a fused process exposed **no specials at all**, so a fused voice declared no
     `:activity` and the pool kept its voices until they were stolen. Deferring
     this was cheap because it is the pool's existing safe default (a voice that
     declares nothing is held until stolen, D39), whereas seams 1–3 are each
     load-bearing for correctness. **Measured while answering "why deferred"**: the
     IR refused a callee's `store-special` outright ("store-special is only
     available in the entry definition", the same shape as a lane), so a voice that
     reports liveness could not be fused *at all* — seam 4 was not "read an offset
     we know" but "allow and rebase a callee's special store, and publish the
     per-slot map", across the targets that call;

  With all four seams — and the instrument option — `{:fusion {:slots S}}`,
  lowered to `(poly S voice)`, can be turned on
  for module-shaped voices. **Done** (D52): it is a compile option beside
  `:target`/`:precision`/`:containment`, `synth-bundle` compiles the base artifact
  for the runtime and the fused one for the kernel, and a `poly` that carries a
  voice's control declarations is what makes the fused interface the voice's
  interface (a `:shared` control stays shared; expanding one per slot, or a lane,
  is refused with a reason). A pool over the option renders bit-for-bit what a pool
  of separate voices renders at one sounding voice. The bench's fused arm now builds
  its artifact *through* the option too, so there is one definition of "the fused
  lowering" — and the realistic sweep's ratio fell from ~1.9× to **1.3–1.7×**,
  because the option expands four controls per slot where `poly`'s default expanded
  two (D45's re-measured table). **M6b — per-slot skipping. Done** (D53): fusion
  compiles the voice through the `guard` combinator, which owns a per-call-site
  latch that skips a slot whose `gate` has never opened and is cleared by the
  slot's reset, so a slot a pool has not allocated costs a comparison while a
  released slot keeps its tail. It is pure DSP — no ABI, IR, or backend change — so
  it applies on every target, and M6c's browser hosts get it for free. Measured
  (criterium, capacity 32), a `:slots 32` instance at two sounding voices falls
  from 0.467 to 0.045 ms/block — the pool's own 0.045 — and `:slots 8` from 0.097
  to 0.036, at a full-patch cost that grows with S (~2% at `:slots 8`, ~30% at
  `:slots 32`, where the guarded process still beats the pool). **M6c — the
  browser hosts through the same testbed arms. Done** (D55): `fusion/
  runtime-mapping` is the one portable mapping, the JS and Wasm kernels take a
  `:mapping` and apply the partitions the ABI already publishes (a JS arena is
  indexed by state slot, so a partition's `:base`/`:size` *are* its range; a Wasm
  partition's byte range comes from the served `:state-layout`), and each slot
  reads its own `:activity`. `testbed` serves a fused pool bundle and the browser
  suite checks a JavaScript and a Wasm fused pool differentially against the
  interpreter. Chunking (`:slots` below capacity) remains the other lever; the
  guard is the per-slot half of "per-slot or per-chunk".

**Done when.** The reference instrument is built from declared parameters and a
matrix lowered by a generator; its events are dispatched sample-accurately from
the sequencer; and it renders identically on interpreter/JVM/JavaScript/Wasm under
arbitrary block sizes, with no allocation after initialization.

**Status.** M1, M2, M3, M4 (M4-0/M4a/M4b/M4c) and M5 (M5a/M5b/M5c) are done: the
lane feature is complete through the pool on every host, and a broadcast parameter
is one region the whole pool reads. **M6a is done** — the three load-bearing seams
(instance key, base→fused control mapping, per-slot reset) and the instrument option
`{:fusion {:slots S}}`. **M6b is done** — the `guard` combinator skips a slot a
pool has not gated, per-slot and per-call-site, with no ABI change (D53). **Seam 4
is done** — a callee may publish a special, each call site records its own, and a
fused pool frees a finished slot from its slot's `:activity` (D54), so fusion no
longer gives up tail-freedom. **M6c is done** — the JavaScript and Wasm kernels
apply the same published partitions, map each slot's controls, reset one call
partition, and read that slot's `:activity` (D55), so a fused pool runs on every
host. M6 is complete.

Note what that means for this phase's *Done when*: it is **already met** — the
reference instrument is built from declared parameters and a matrix lowered by a
generator, its events are dispatched sample-accurately from the sequencer, and it
renders identically on interpreter, JVM, JavaScript and Wasm under arbitrary block
sizes. M6 is opt-in efficiency work layered on top of a complete phase: the pool
stays the default, and fusion is requested (D45).

**Browser build note.** The worker runtime is the library namespace
`omkamra.dsp.hosts.worker`; the project runs it as a separate, self-contained
Shadow build (`:omkamra.dsp.hosts.worker`, `:devtools {:enabled false}`). Shadow's
documented `:shared`/`:main`/`:web-worker` single-build pattern compiles in
development but the worker fails at run time (its dev module-append files need
the `shadow` runtime the worker does not get); release generation is unaffected.

---

## F — Fusion, deployed

**Goal.** Make the fusion work of M6 *reachable*: requestable through the
instrument layer, applicable to the voice shapes real instruments use, and
composable with lanes — so the speedups M6 measured describe a configuration a
user can actually select. Fusion stays opt-in and the pool stays the default
(D45); F adds no DSP semantics.

**Why a letter, not a number.** Like M, F is not part of the P spine's dependency
order, and nothing in the spine depends on it. It is a *deployment* axis over an
existing optimization: the language, the IR, and the targets already exist, and F
extends them only where a fused process needs a region a callee cannot reach yet.
Lettering it keeps the P identifiers stable.

**Depends on.** P10 (voice pool), P11 (reference instrument, D42), D28 (lanes),
and all of M (D45, D50–D55). F also uses instance memory and the browser hosts
(the P12–P14 capabilities); those already exist in the code, so F is **not**
gated on those phases being marked done — its only hard gate is M.

**The problem it removes.** M6 built a fused pool that renders a plain pool's
samples and, since M6c, runs on every host. But it can only be switched on by
calling `compile-synth` or a pool host *directly*, and only for a
**module-shaped, lane-free** voice. Three limits keep it out of reach of real
instruments:

1. The **instrument layer never requests `:fusion`**. `instrument-pool` calls
   `host-kernel` without it, so no instrument — not even a module-shaped one —
   can fuse. Worse, the option is *accepted and then ignored*:
   `compile-instrument` forwards `:fusion` to `synth-bundle` (producing a fused
   artifact whose runtime carries `:fusion`), and `instrument-pool` then builds a
   **plain** kernel over that artifact. A module-shaped instrument compiled this
   way throws at the first `note-on!` (`Index 4 out of bounds for length 4`,
   verified). F either threads the option or refuses it — never half-accepts it.
2. **`poly` refuses a process-shaped voice** — one that writes the caller's
   output buffers (`buffer-store :output`). That is every *stereo* or
   *multi-output* voice, including `reference-voice`, so the reference instrument
   cannot fuse even in principle (M6-0's boundary).
3. **A lane is resolved from the entry process.** A `(lane control)` read inside
   a callee is refused (`lane is only available in the entry process`), so *any*
   lane in a called voice — shared or per-voice — is refused; `poly` additionally
   refuses a per-slot lane. Neither the callee ABI nor the fused interface
   carries a lane region.

Because the guard (M6b), the per-slot `:activity` readback (D54), and chunking
(`:slots < capacity`) all live on the fused path, these limits make the whole M6
investment inert for the voices it was measured on.

**Deliverables.**

1. A **fused instrument host**: `compile-instrument`/`instrument-pool` thread
   `:fusion` (or refuse it loudly), on the JVM and in the browser instrument
   host, with a module-shaped fixture pinned end to end.
2. **Process-shaped voices fuse**: each call site gets its own output-buffer
   region — the buffer analogue of the state partition D19 already gives it — the
   slots render into their regions, and the fused entry sums them into its own
   output. The reference instrument becomes fusable and renders the plain one's
   samples.
3. **A lane reaches a callee**, so a shared lane survives fusion.
4. **Per-voice lanes fuse**, so per-voice automation and fusion compose.
5. A **fused AudioWorklet**: one worklet node renders `slots` voices, so the
   browser worklet path crosses the kernel boundary once per `slots` voices
   rather than once per voice.

**Slices.**

- **F0 — decide the region shapes (design only, no code).** Two decisions,
  recorded in `DECISIONS.md` before anything is built:
  1. *How a process-shaped voice is fused.* The candidate: partition the
     process's **output buffers per call site**, exactly as D19 partitions state
     — a private region per call site, the callee writing there, the fused entry
     summing the regions into its output. Alternatives to weigh: the same idea
     restricted to `:output` plus a per-slot address; or a *voice → module*
     adapter that turns the voice's buffers into scratch records and returns the
     signal. The answer must state how it treats M6-0's module/process boundary.
  2. *How a lane reaches a callee.* The candidate: add the lane region to the
     callee ABI the way the state base already is (D19/D47), rebased per call
     site; a per-voice lane gets one region per slot.
  The two answers share a spine — **the remaining per-process regions (output
  buffers, lanes) are partitioned per call site and passed through the callee
  ABI** — and F0 exists to settle that spine, and its cost on four backends,
  before F2–F4 build on it.
- **F1 — the fused instrument host (module-shaped voice). DONE.** `instrument-pool`
  threads the bundle's `:fusion` into `host-kernel` (bite-checked: reverting the
  thread throws `Index 4 out of bounds for length 4`), the fused runtime takes
  its `:capacity` from the slot count (D62, pinned by
  `a-fused-instrument-holds-exactly-its-slots`), and the fused instrument renders
  the plain one bit for bit. The browser half is registered too:
  `fused-instrument-bundle` / `fused-instrument-source` carry the fused voice's
  ABI, IR, and `:fusion` mapping, the plain lowering as the same-target oracle,
  and the post-mix bus, and `check-fused-instrument` rebuilds the host over them
  for JavaScript and Wasm.

  **What the browser half found.** Comparing the *reference instrument* —
  process-shaped, with smoothed controls — surfaced a real bug that the
  module-shaped JVM fixture had hidden: a **derived definition ramped the
  controls it merely forwards**, so every wrapper a voice passed through smoothed
  a value the voice then smoothed again. The fused signal lagged the plain one by
  percent for the first milliseconds and settled only after thousands of frames;
  `poly`, `guard`, `unison`, `share`, and the instrument and modulation wrappers
  all doubled this way. D63 fixes it in `graph/derived-definition` +
  `descriptor/normalize`, and
  `a-fused-voice-does-not-ramp-a-smoothed-control-twice` pins it (bite-checked:
  without the fix the first block is `[0.0 0.0 0.0 0.125 0.3125 …]` where the
  plain voice's is `[0.0 0.5 0.75 0.875 0.9375 …]`).
- **F2 — process-shaped voices fuse.** The load-bearing slice, and the one F0
  says reopens M6-0's boundary. Reading the code for it turned up that it needs
  **two** changes, and the second is the harder:
  1. *The mechanism.* A call site that calls a definition touching a process
     buffer owns a per-call-site **buffer region** — the buffer analogue of
     D19's state partition — and the callee ABI carries its base. Automatic, as
     D19 is: the call site always owns the region, so nothing new is needed to
     *pass* it. Relax `linker.clj`'s refusal only for a call that owns a region.
     The callee stays a per-frame function (D40), so its `buffer-store` writes
     `region + channel·stride + frame` and the fused entry sums per frame; no
     new process structure is needed.
  2. *The surface.* `poly` must declare the per-slot regions, call each slot
     into its region, and read the regions to sum them into `:output`. The
     region is a buffer of the entry with `:direction :region` (per channel, like
     `:output`), and the missing piece was a form for "call this callee with its
     output buffers bound to *this* region". **Settled (D57)**: one new form,
     `(dsp/with-buffers <binding> <call>)`, where `<binding>` is a map from the
     callee's buffer id to a caller-visible region name, or — when the callee
     writes exactly one output buffer — the bare name as sugar; the bound names
     are read with the existing `buffer-load`. The region reuses the call-site
     state partition (D19), so no new ABI parameter is added.
  Sub-slices: **F2a** the mechanism plus the new form, on the interpreter (the
  oracle) — **done**, see D57, D58, D59; **F2b** the JVM — **done** (the region
  is state, so the only target change is the void return and the callee's frame
  parameter); **F2c** JavaScript and Wasm — **done** (the same two, plus a void
  call is no longer `drop`ped in Wasm); **F2d** the reference instrument fuses
  (stereo and process-shaped, D60) and the browser renders the fused reference
  voice as the plain one **bit for bit** on both JavaScript and Wasm, through a
  steal — **done**, see D61.
- **F3 — a shared lane reaches a callee. DONE.** F0's spine (D56) applied to the
  lane region: a definition that reads `(lane control)` is *handed* the region —
  one pointer, since a shared lane is one region the entry laid out — and the
  call site passes its own on. No base arithmetic is involved, because a lane's
  offset is a compile-time constant of the entry's layout (D28), so `(lane x)`
  means the same slice wherever it appears; a definition that only calls a lane
  reader forwards its own region and is marked for it. The callee ABI grows by
  the region plus **the current frame**, which a lane read needs for the same
  reason a region write does (F2). Decision: **D64**.

  Pinned on all four targets by `:automation-lane-in-callee` in the shared
  catalog — an entry that declares a lane and calls a callee which reads it —
  which the JVM suite runs on the interpreter and JVM and the browser matrix runs
  on JavaScript and Wasm, f32 and f64. The JVM suite also renders it directly
  (`a-control-carries-an-optional-lane-beside-its-scalar`), and that check was
  bite-checked twice: without the lane table reaching callees it refuses
  ("lane is only available where a lane region is"), and without the JVM call
  site's argument the callee fails a `VerifyError`.
- **F4 — per-voice lanes fuse. DONE.** The entry's one lane region now holds one
  slice per voice, and a call site hands its callee the base of *its* slice —
  zero-based lane offsets plus a base, exactly as a callee's state indices are
  resolved against a partition base (D19) — so `poly`'s refusal of a lane on a
  per-slot control is gone. A lane belongs to the *control*, so expanding a control
  per slot does not multiply lanes: the expanded declarations name the lane they
  carry (`:lane-name`) and the entry lays out one lane per base control. A
  lane-only control's name is a read of the lane region wherever it is read, so a
  callee reads its own slice rather than the value its caller passed. Decision:
  **D65**.

  Pinned on all four targets by two checks: the JVM suite renders
  `a-fused-pool-gives-each-slot-its-own-lane` and
  `a-fused-pool-expands-a-lane-carrying-control-per-slot` on the interpreter and
  the JVM against a plain pool over the same description, and the browser runs
  `JavaScript fused lane` and `Wasm fused lane` over a served fused bundle with two
  curves written to the two voices. Both failed before the change for the two
  reasons it exists (`[40 40 40 40]` where the plain pool gave `[22 24 26 28]`, and
  the refusal); the browser check was also the bite check for the JavaScript host's
  one-slice region.
- **F5 — a fused AudioWorklet. DONE.** One worklet node renders `slots` voices:
  its parameters are the *fused* interface in ABI order (`gate_0`, `frequency_0`,
  ...), generated from the fused artifact's controls, so the page drives the slots
  apart directly. No mapping is involved — the pool's `fused-position` exists for
  the pool's base-shaped writes, and a worklet has no pool — and slot assignment
  stays the page's, which is the static half of polyphony `poly` has documented
  since D38. Decision: **D66**.

  Pinned by `JavaScript fused worklet` and `Wasm fused worklet`: one node rendered
  three times — one slot, the other, and both — with slot 1 at twice slot 0's
  `frequency`, asking that slot 1's peak be twice slot 0's and that the both-slots
  block be their sum. The check found two real bugs (an unguarded
  `processorOptions` read, and the page nesting the processor fields wrongly) and
  bites when the per-slot names go missing; a stub of the descriptor *order* does
  not bite, which is expected — adapters index by `controlNames`.

**Done when.** `{:fusion {:slots S}}` can be requested for the **reference
instrument** — stereo and lane-using — through the instrument layer on
interpreter/JVM/JS/Wasm, and a fused instrument renders bit-for-bit what the
plain instrument renders (transcendentals within the documented tolerance), with
the pool still the default and every existing check unchanged; and the
AudioWorklet path renders `slots` voices per node. The M6 measurements then
describe a configuration a user can select.

**Risks and open questions.** F2 is load-bearing and reopens M6-0's
module/process boundary; if output-buffer partitioning needs a new ABI shape,
that is a decision, not a fix. F3/F4 touch all four backends and the interpreter,
and F3's callee-ABI change is felt by every call site (D47). The instrument
layer's half-acceptance of `:fusion` is a *current* bug, not only a missing
feature; F1 closes it in both hosts (D62, D63).

**Status.** **F0, F1, F2, F3, F4 and F5 are done — the workstream is complete.**
F1's instrument
layer threads `:fusion` on both the JVM and in the browser (D62, D63), a lane
reaches a callee on every target (D64), and a fused entry gives each of its slots
its own lane slice (D65). **F2a–F2d are
done**: the surface is
`with-buffers` (map or bare-name sugar, D57); a bound buffer is a **call-partition
state slot** (D59), read with the existing `buffer-load`, so the entry's `:buffers`
keeps only its own `:output` and no target needs region-specific code; a
process-shaped callee returns `:void` and is handed the frame; and **`poly` fuses
such a voice** by binding each slot's region and summing them into `:output`
(D60). The reference instrument therefore fuses on every target: a fused pool
renders its plain pool **bit for bit** on the interpreter and the JVM (D60), and
the browser renders the fused reference voice exactly as the plain one on both
JavaScript and Wasm, through a steal (D61).

Four traps fell out and are fixed: `normalize-options` always injects a default
`:channels 1`, so an explicit top-level width must be written into the options
module lowering reads (D58); a per-sample callee has no frame, so a
region-writing one is handed the current frame (D59); a void call must not
`pop`/`drop`/pass a stray argument (D59/D60); and the Wasm host must seed a
fresh instance's state itself and resolve a partition's byte range from layout
*segments* (D61). The first F2a cut used a `:region` *buffer* instead of state
and was reworked to D57's design (D59).

**Done when** is met: a fused instrument renders bit for bit what the plain one
renders on every target, and the AudioWorklet path renders `slots` voices per node.

---

## H — Closing the declared gaps

**Goal.** Leave nothing that the code itself declares missing. An audit of
`src/omkamra/dsp` for "not yet" / "cannot … yet" / "until it has it" finds a
handful of capabilities that a *user* would reasonably expect and that no
decision entry blesses as a design choice. H implements them — or, where
implementation is a genuine design change, replaces the refusal with a decision
that says why not, so nothing keeps reading as an unfinished edge.

**Why a letter, not a number.** Like M and F, H is not in the P spine's
dependency order and nothing in the spine depends on it. It adds no language
surface: every gap is in a *host* — the browser worklet, the browser bus, the
pool's lane routing — where the compiler, the IR and the targets already work.

**Depends on.** All of M (the fused hosts) and all of F — in particular F5's
fused worklet node, which H grows into a runtime. D28/D46 (lanes), D52/D56–D66
(fusion). H2–H4 are not gated on P12–P16: the browser hosts and the pool already
exist in the code. **The worklet-runtime slice, H1, is** — it waits for P13 and
P14, for the reason given under H1.

**The problem it removes.** The audit, with the evidence for each item:

1. **The worklet path is a renderer; the pool path is a runtime.** A worklet
   node has no lanes, no shared region, no specials read-back (so no `:activity`,
   hence no tail-aware freeing), no per-slot reset (`worklet_adapters/js.cljs`
   has none at all; the Wasm one resets the whole module), no allocator, and
   mono-only event dispatch ("left for the planar-worklet slice"). Everything a
   pool gives for free, a node makes the page rebuild.
2. **The browser bus carries neither lanes nor a shared region**:
   `hosts/js.cljs` and `hosts/wasm.cljs` refuse at bus construction ("this target
   cannot deliver lanes or a shared region to a bus yet"). The JVM folds the
   post-mix chain into the instrument's own artifact, so it handles them; the
   browser builds the bus as a separate kernel and does not. **Closed (D67).**
3. **A lane on a per-channel control has no delivery** — `synth/pool.cljc`
   refuses `:bend`'s lane ("no per-lane delivery yet"). **Closed (D68).**
4. **A fused voice with no state and no `gate` cannot fuse**: every host refuses
   with "a slot cannot be reset on its own", because slot identity *is* a call
   partition and only a stateful call site (or `guard`'s latch) has one.
   **Closed (D70).**
5. **Arrays of records cannot contain array fields** — `descriptor.clj` refuses
   it. Unlike the others this is a *language* gap (nested aggregate layout in a
   flat arena), not a host gap. **Closed (D71).**

Two ledger entries that are **decisions, not gaps**, and so stay: `poly`
refusing a `:shared` control per slot (D50 — one value is the point), and the
linker refusing a module access to the caller's process buffers (D57's
`with-buffers` is the sanctioned path). `guard` not skipping *process-shaped*
fused voices is a withheld optimisation, explicitly not a correctness gap.

**Deliverables.**

1. A **worklet node that owns a pool** — *after P13/P14*: it renders, allocates,
   automates per voice and frees tails exactly as a pool over the same kernel
   does, with the page's surface being the pool's own API rather than one message
   per capability.
2. **Sample-accurate note timing** in that node — finer than the pool path,
   which applies control writes at block boundaries.
3. A **browser bus** that carries lanes and a shared region, pinned per target.
4. **Per-channel lane delivery**, or a decision.
5. A **fused slot identity that does not require state**, or a decision.
6. A **recorded position on array fields in records**, so the language's
   remaining edge is a decision rather than a refusal.

**Note on the toolchain (durable).** Three layers, and they are not
interchangeable.

- **Shadow** compiles this project's ClojureScript — the page, the worker build,
  the browser hosts, the pool, `synth` — and keeps cljs semantics: persistent data
  structures, real protocol dispatch, `deftype`/`reify`.
- **SCI** is how *our* Clojure code is meant to run in the browser (P13). It is an
  interpreter, so it preserves those semantics too.
- **Squint** is not a ClojureScript compiler: it compiles *syntax* to native JS,
  so data structures are JS objects and arrays rather than persistent ones, and
  protocols dispatch through `Symbol.for`. Here it has exactly one intended job —
  the **JS backend of the emitter** (D41), running *inside* SCI in the browser
  (P13). The emitter builds ClojureScript forms and hands them to
  `squint.compiler`; D41's four rules keep the result a standalone module (in
  particular, nothing may lower to a `squint_core` reference).

So the squint-compiled code in the repository today is either emitter output (the
kernels) or written in squint's own idiom (`defclass`, `aget`/`aset`, `js*`,
`squint_core`) — the two worklet adapters. No *library* namespace goes through
squint: `synth/pool.cljc`, `synth.cljc` and `hosts/{js,wasm,mixing,fusion}` are
real ClojureScript, and running them through it would be a semantic change, not a
portability question.

**Slices.** H2–H5 land before P12; **H1 waits for P13 and P14.**

- **H2 — the post-mix chain carries lanes and a shared region. DONE (D67).** The
  chain is a processor, so it needs the region handoff the pool gives voices — but
  the gap was in *lowering* before it was in the hosts: `chain-effect` rebuilt each
  control as bare `{:name …}`, so a stage's declaration never reached the entry
  process, and the browser refusal was unreachable. The chain's lane and shared
  regions are instrument-level (one each, written once, handed to every channel's
  instance) and every host delivers them: the JVM and interpreter paths, the JS
  bus (the host's array in each instance's arguments), the Wasm bus (copied into
  the instance's memory, as a voice's shared region is). Pinned by
  `bus-gain-voice`, whose output is exactly 1.0 while it sounds, so the samples
  *are* the curve: `a-post-mix-lane-reaches-the-bus` and
  `a-post-mix-shared-control-follows-its-parameter` on the JVM and interpreter,
  `JavaScript`/`Wasm post-mix lane` and `… shared` on the page.
- **H3 — a lane on a per-channel control. DONE (D68).** `:bend` is the pool's
  source bank indexed by *channel*, so its lane is a curve per channel and the
  scope is inferred from the source, like `:shared` and `:per-voice`. The pool
  keeps a curve bank (allocated at construction), `set-channel-lane!` copies a
  host's curve into it and writes it to the voices already on that channel, and
  `sound-voice!` delivers a channel's curves to a voice as it starts — which is
  what makes both "a voice that starts later reads the curve published before its
  note" and "a voice that moves to another channel stops reading the old one"
  true. Pinned on the JVM and the interpreter
  (`a-per-channel-lane-reaches-the-voice-on-its-channel`, with the host's buffer
  mutated after the write) and on the page (`JavaScript`/`Wasm channel lane`).
- **H4 — a fused slot identity without state. DONE (D70).** Slot `k` was the
  `k`-th *state* partition, so a voice with no state had no slots and every host
  refused it ("a slot cannot be reset on its own" — false for a voice with nothing
  to reset). Every call site now owns a partition, empty when the callee has no
  state, so the ordinal is the call-site ordinal: a stateless voice fuses (S
  identities, a no-op reset), a lane-reading callee gets its slice without state,
  and the Wasm host's byte-range scan accepts an empty range. Pinned by
  `a-fused-slot-does-not-need-state-of-its-own` (JVM and interpreter) and
  `JavaScript`/`Wasm stateless fused pool` on the page.
- **H5 — array fields in records. DONE (D71).** A language omission, not a host
  gap: an array of records whose field declared `:size` was refused outright, and
  D16 recorded it as "(rejected)". A leaf is now one flat block of `n * m` slots
  carrying `:stride m`, and an access folds the two indices into the one element
  index the ABI already had (`record * stride + element`), so **no backend
  changed**; a constant bank with dynamic indices (a wavetable bank) and a bank of
  writable buffers are both expressible, and a constant index is checked per
  dimension. Pinned by `record-array-fields` (JVM and interpreter) and, as shared
  catalog cases, by `:constant-table-bank` and `:record-array-buffer` on all four
  targets. A *nested* record field still cannot declare `:size` — a different
  omission with an exact hoisting workaround; see D71.
- **H1 — the worklet runtime (deferred; depends on P13 and P14).** The five
  worklet gaps are one gap: they all dissolve if the pool — or an equivalent
  allocator — lives in the node. But *how it gets there* is not H's to invent:

  - **P13** puts the compiler in the browser with squint as its JS backend
    *inside* SCI, so a node's kernel is produced by the page (a blob URL for
    `addModule`) instead of being fetched from a per-request server URL. That
    makes H1's original problem — delivering a per-request kernel into a
    statically built module — disappear rather than get solved.
  - **P14** makes worklet installation artifact-driven and explicitly *removes*
    worklet configuration reconstruction from `omkamra.dsp.testbed` — the very
    code H1 would otherwise have extended.

  So H1 starts after P14, and begins with a **decision** about where the
  allocator lives, taken in that world:

  1. **SCI in the worklet**, hosting the pool (`synth/pool.cljc`). The
     architecture-aligned option: real cljs semantics (persistent data, protocol
     dispatch), one implementation of the allocation policy, no second build. The
     cost is SCI's runtime in the worklet and interpretation overhead on
     control-rate work — per block and per note, never per sample.
  2. **A Shadow-built worklet runtime.** Faithful and fast, but a second build
     target, and a direction P13 does not take.
  3. **An allocator emitted by the compiler**, in the squint-safe forms D41
     already constrains kernels to. Smallest at runtime, but a second
     implementation of a policy the pool's contract says must not be duplicated.

  Once chosen, the slices are the ones sketched earlier: the port's protocol is
  the pool's own API (note-on/off, all-notes-off, set-control,
  set-voice-controls, set-lane, set-voice-lane, set-shared, stats,
  voice-snapshot), so lanes, shared delivery, `:activity` and per-slot reset need
  no new messages at all; and note events are applied at their exact frame by
  splitting the block at event frames and calling `note-on!`/`note-off!` between
  sub-block renders — finer than the pool path's block-rate writes. The node's
  *parameters* stay (D66): the fused interface remains a manual surface, notes
  are a second one.

**Done when.** Every refusal the audit found is either gone or replaced by a
decision entry: H2–H4 done before P12, H5 decided, and H1 — with P13/P14 in place
— leaving a worklet node that renders, allocates, automates and frees like a
pool.

**Risks and open questions.**

- **H1 is deferred by design, not by omission.** Its mechanism belongs to
  P13/P14; taking it earlier would either duplicate them or invent a
  worklet-build path the roadmap does not have. The spike that was going to prove
  a Shadow build loads as a worklet module is dropped: with the compiler in the
  browser (P13) the kernel is a blob the page creates, and P14 owns how a worklet
  is configured.
- **H1's decision has a realtime edge.** Wherever the allocator lives it runs on,
  or is consulted from, the audio thread: the pool is documented as a total audio
  path (no throw, allocation, lock or log), SCI interpretation adds overhead to
  control-rate work, and an emitted allocator needs the same no-allocation
  discipline. That is why it is a decision with a measurement, not a default.
- **H3 and H4 change assumptions, not just code.** H3 fans channels into voices;
  H4 changes what a slot is. Both touch what D19/D62 rest on, so each gets a
  decision before an implementation.
- **H2 has no JVM parallel to copy.** The JVM's bus is folded into the
  instrument's artifact; the browser's is a separate kernel, so the fix and its
  check are per browser target.
- **Stereo worklet events stay out of scope.** H1's sample-accurate notes are
  mono, the same limit the control-event path already has; a planar (stereo)
  worklet dispatch remains its own slice.

**Status.** **H2–H5 done (D67, D68, D70, D71)**: the chain's own lane and shared
regions, a per-channel lane, a slot that is the call site's identity, and array
fields in arrays of records. **H1** is the one outstanding item and it waits for
**P13 → P14** by design — P13's own dependencies (P2–P4) are all done, so the
plan's reordering rule permits those ahead of P12.

---

## Dependencies at a glance

```text
P0 ─ P1 ─ P2 ─ P3 ─ P4 ─ P5 ─ P6 ─ P7 ─ P8 ─ P9 ─ P10 ─ P11
                             └───────────────────────────┴── P12
P2/P3/P4 ──────────────────────────────────────────────── P13
P12 + P13 ────────────────────────────────────────────── P14
P9 + artifacts ───────────────────────────────────────── P15
P11 + P12 + P14 ──────────────────────────────────────── P16
P9 + P10 + P11 ──────────────────────────────────────────── M
M ───────────────────────────────────────────────────────── F
```

P12–P16 are infrastructure/quality phases; their position reflects the point at
which their inputs and consumers exist, not a required serialization. **M** and
**F** are separate, lettered workstreams — M is instrument semantics and host
topology, F deploys M6's fusion through the instrument layer and to the voice
shapes real instruments use (stereo, lane-using); see their entries above.
