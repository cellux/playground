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

## P10 — Voice pool and synth runtime

**Goal.** Polyphonic voice management above the kernel layer.

**Depends on.** P9.

**Deliverables.**

1. Preallocated voice pools with explicit capacity and per-voice state layout
   (using P4 aggregate/array facilities).
2. Deterministic, documented voice allocation and stealing.
3. Per-voice init/release/reset/terminate lifecycle, mono/legato/retrigger/
   poly/unison modes.
4. Per-voice and global modulation routing with bounded route counts.
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

---

## P12 — Complete Wasm artifacts and memory ABI

**Goal.** Turn WAT-only Wasm output into a complete, instance-oriented artifact.

**Depends on.** P0; benefits from P8 (instance regions) and P4 (layout).

**Deliverables.**

1. A defined WAT → WABT → binary compilation step producing validated
   `:bytes`, with WABT version pinning recorded.
2. Persistent state moved from globals into explicit linear-memory instance
   regions; reusable independent instances.
3. Removal of the fixed 4096-byte channel stride in favor of ABI-declared
   layout, with validated alignment, capacity, and non-overlap.
4. An artifact exposing `:bytes`, `:wat`, `:exports`, `:memory`, `:abi`, and
   `:metadata`.

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
   tests and a no-reassociation rule for floating point.

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

## Dependencies at a glance

```text
P0 ─ P1 ─ P2 ─ P3 ─ P4 ─ P5 ─ P6 ─ P7 ─ P8 ─ P9 ─ P10 ─ P11
                             └───────────────────────────┴── P12
P2/P3/P4 ──────────────────────────────────────────────── P13
P12 + P13 ────────────────────────────────────────────── P14
P9 + artifacts ───────────────────────────────────────── P15
P11 + P12 + P14 ──────────────────────────────────────── P16
```

P12–P16 are infrastructure/quality phases; their position reflects the point at
which their inputs and consumers exist, not a required serialization.
