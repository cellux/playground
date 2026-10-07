# DSP compiler plan and roadmap

## Goal

Build a small C-like DSP language embedded in Clojure data. Definitions are
captured by `omkamra.dsp` macros, linked explicitly, lowered through a portable
typed IR, and compiled to:

- JVM bytecode through `insn`;
- JavaScript ES modules through a structured JS-like IR;
- WebAssembly through structured S-expressions, WAT, and WABT.

The result must work as a real-time JVM block-processing kernel and as the
numeric kernel behind browser AudioWorklets. JavaScript and Wasm use separate
ClojureScript worklet adapters.

This document records the design, current implementation, known limitations,
and remaining work. It is a living roadmap rather than a history of
implementation phases.

## Design rules

The portable typed IR is the semantic boundary:

```text
omkamra.dsp macros
  -> source descriptors and dependency graph
  -> normalization and linking
  -> portable typed DSP IR
       +-> JVM lowering -> insn -> generated JVM class
       +-> JS-like lowering -> emitter -> ES module
       +-> Wasm lowering -> S-expressions -> WAT -> WABT -> binary
```

Every language feature must be implemented in this order:

```text
syntax and descriptor validation
  -> portable IR
  -> reference interpreter
  -> JVM
  -> JavaScript
  -> Wasm
  -> shared conformance tests
```

Backends must not interpret arbitrary Clojure values or add private language
semantics. Parsing/lowering remains separate from emission. This follows the
useful data-capture and explicit-linking model of `omkamra.pygen`; `oben` may
inform typed expression semantics, but its LLVM-oriented node model is not the
universal DSP IR.

Logical types remain target-neutral until backend lowering:

```clojure
:float
:int
:boolean
:void
```

The compilation policy resolves logical `:float` values to physical storage:

| Target | `:f32` | `:f64` |
|---|---|---|
| JVM | primitive `float` | primitive `double` |
| JavaScript | `Math.fround` and `Float32Array` | `Number` and `Float64Array` |
| Wasm | `f32` | `f64` |

Source code does not select the physical width of `:float`; compilation options
do. Changing precision may change results. Floating-point reassociation and
other transformations that change observable NaN, infinity, signed-zero,
overflow, conversion, or rounding behavior are not allowed by default.

## Scope and implementation policy

This library targets two deployment environments:

- JVM applications, with Clojure/ClojureScript UI integration such as cljfx;
- browser applications, with dynamic compilation to JavaScript/Wasm kernels and
  AudioWorklet adapters.

Host/plugin integration is outside the scope of this compiler. It does not need
to implement CLAP, VST, Audio Unit, standalone plug-in packaging, or other
plugin APIs. AOT compilation to C, LLVM, or native machine code is also outside
the current scope. Browser-time compilation is a core use case: users should
be able to edit DSP source, compile it in the browser, install the resulting
AudioWorklet kernel, and test it without a server-side compiler.

Prefer the DSP language over target-specific implementations. If an operation
can reasonably be expressed in the language, implement it there first and
extend the language, IR, interpreter, and backends as necessary. Do not add a
native implementation merely because it is convenient for one target.

Target intrinsics and external DSP kernels are a constrained escape hatch for
operations that genuinely require a runtime primitive, a backend-specific
instruction, or a measured performance implementation that the language cannot
provide. Every such operation must have:

- a typed signature and explicit memory/effect contract;
- documented precision, special-value, and determinism semantics;
- a reference implementation or portable fallback where practical;
- capability checks and a clear unsupported-target diagnostic;
- no hidden allocation, locking, I/O, or host callback in the process path.

An intrinsic should represent a stable semantic operation in the portable IR,
not an arbitrary call into Clojure, JavaScript, or the JVM. Native variants are
optimizations or compatibility mechanisms, not the primary extension model.

## Current architecture

### Definitions and normalization

`omkamra.dsp/defn` captures a DSP definition as data. It supports the shorthand
form:

```clojure
(dsp/defn gain
  [sample amount]
  (* sample amount))
```

It also accepts an explicit descriptor containing typed parameters, return
type, state, process metadata, and compiler options. Macro expansion captures
source location and the namespace aliases/refers needed to resolve calls later.
DSP bodies are never evaluated as ordinary Clojure expressions.

`omkamra.dsp.descriptor` is the source of truth for normalization and
validation. It currently validates:

- descriptor shape and supported host values;
- logical parameter, state, control, and return types;
- unique names and binding collisions;
- state initializers and transitions;
- process ports, controls, buffers, channels, and lifecycle flags;
- recognized compiler options and their values.

Compiler options are normalized to visible defaults:

```clojure
{:target :interpreter
 :entry :function
 :precision :f32
 :channels 1
 :checks :development
 :optimize false
 :loop-bound nil}
```

### Linking and calls

`omkamra.dsp/link` accepts a descriptor or Var and returns a deterministic
compilation unit. It:

- discovers dependencies from ordinary DSP call forms;
- resolves local, referred, aliased, and qualified names using the definition's
  captured namespace context;
- accepts an explicit `:definitions` map for anonymous definitions and tests;
- reports missing definitions, duplicate IDs, unsupported callees, and cycles;
- orders definitions dependency-first;
- rewrites resolved calls to `(dsp/call qualified/id ...)`.

There is no source-level `:dependencies` list. Inferred calls are the dependency
source of truth.

Calls are currently limited to non-recursive, stateless, buffer-free functions.
The interpreter executes explicit `:call` nodes. JVM, JavaScript, and Wasm
artifacts use a target-neutral inlining pass, currently restricted to
expression-only callees.

### Portable IR

`omkamra.dsp.ir` lowers normalized linked definitions into typed expression and
statement IR. The implemented language includes:

- `:float`, `:int`, and `:boolean` constants and bindings;
- typed locals and mutable local assignment;
- same-type arithmetic and comparisons;
- short-circuit boolean operations;
- explicit `int->float` and `float->int` conversions;
- `if`, `let`, `do`, and returns;
- `while`, optional iteration bounds, `break`, and `continue`;
- resolved function calls;
- persistent typed state;
- explicit buffer loads and stores;
- frame, channel, frame-count, and sample-rate values.

Process IR is lowered directly from process metadata. It contains canonical
buffer descriptors, controls, state slots, a per-frame body, lifecycle flags,
memory requirements, effects, and diagnostics. State transitions are computed
before stores so updates within a frame are simultaneous.

IR nodes identify state and memory effects explicitly. Function and process
roots contain aggregate effect sets. The runtime-policy validator rejects
unknown effects and publishes policy metadata, but it is not yet a complete
proof of allocation-free or real-time-safe execution.

Constant frame offsets contribute to minimum buffer-size metadata. Dynamic
indices remain explicit and produce diagnostics. The structured IR can be
lowered directly today without preventing a later SSA representation.

### Reference interpreter

`omkamra.dsp.interpreter` executes function and process IR and is the reference
implementation for language behavior. It supports:

- both `:f32` and `:f64` precision policies;
- integer and boolean values;
- structured control flow and bounded-loop exhaustion;
- linked calls;
- typed state and simultaneous state updates;
- mono and multichannel buffers;
- explicit process contexts with sample rate and controls;
- initialization and reset;
- development-time dynamic buffer bounds checks.

The interpreter owns process state through each compiled artifact. A generalized
API for creating multiple independent instances from one artifact is not yet
implemented.

## Target status

### JVM

The JVM backend emits bytecode with `insn` for scalar functions and process
kernels. It supports the current typed statement IR in both precisions,
including typed controls and state, conversions, dynamic frame indices,
conditionals, loops, and loop-control branches.

Process kernels use primitive interfaces for mono and multichannel buffers.
Persistent state and controls are split into reusable real and integer arrays.
The hot block-processing path has direct interface dispatch and no reflective
invocation or per-frame boxing.

Two raw process adapters are available:

- `dsp/process-checked!` validates primitive storage, shapes, lengths, controls,
  and artifact-owned state before dispatch;
- `dsp/process-unchecked!` dispatches directly through the primitive ABI.

The convenience `dsp/process!` adapter accepts an explicit process-context map.
Scalar function invocation still uses reflection outside the real-time process
ABI.

A HotSpot allocation regression test warms the unchecked process path and
compares its thread-local allocation count with a fixed-arity no-op baseline.

Java Sound does not provide a DSP render callback. A host must own the render
loop, fill PCM blocks, and write them to a
`javax.sound.sampled.SourceDataLine`. The generated primitive process interface
is intended to be called once per block from that loop, not once per sample
through Clojure.

### JavaScript

The JavaScript backend lowers portable IR to structured JS-like IR and emits an
ES module. Function artifacts export `invoke`; process artifacts export
`createKernel`. Process kernels include instance-local state and `init`,
`reset`, and `process` operations.

Both precision policies are emitted. The `:f32` policy inserts `Math.fround` at
semantic boundaries and uses `Float32Array`; `:f64` uses JavaScript numbers and
`Float64Array` storage.

JavaScript artifacts contain source and metadata but no JVM-side executable
adapter. They must be loaded by a JavaScript runtime. Emitted kernel modules
must not depend on Node, CommonJS, or browser APIs unavailable in an
AudioWorklet.

The ClojureScript AudioWorklet adapter:

- uses the actual browser block length rather than assuming 128 frames;
- supports configured channel counts and input-channel upmixing;
- reads positional control metadata;
- reuses its process argument array;
- silences unavailable or unsupported output channels.

The adapter is exercised through the browser testbed, but its complete
configuration is still assembled there rather than generated directly from a
standalone artifact installation API.

### WebAssembly

The Wasm backend currently implements:

```text
portable IR -> structured Wasm S-expressions -> WAT
```

It emits typed arithmetic, comparisons, conversions, control flow, bounded-loop
traps, buffer access, and process lifecycle exports for `:f32` and `:f64`.
The browser testbed uses pinned WABT `1.0.39` to parse, validate, and compile the
WAT before instantiation.

A Wasm process artifact contains:

- WAT source;
- exported operation names;
- physical type information;
- input/output memory regions;
- alignment, element width, channel stride, frame capacity, and required pages;
- process and lifecycle ABI metadata;
- a binary descriptor whose `:bytes` value is currently `nil`.

Input and output buffers use linear memory. Persistent state currently uses
mutable Wasm globals, and controls are process parameters. The preferred final
ABI places state in linear memory so hosts can inspect, serialize, and manage
instances consistently.

Wasm is not itself an AudioWorklet; it is the numeric module invoked by a
worklet wrapper. The Wasm AudioWorklet adapter validates required exports,
memory size, and alignment; supports both precisions; refreshes typed views
after memory growth; passes positional controls; and reuses its argument array.

## Process ABI and artifacts

The primary executable abstraction is a block-processing kernel, not an
ordinary boxed Clojure function. The target-neutral ABI describes input and
output buffers, frames, channel layout, sample rate, controls, persistent
per-instance state, initialization, and reset. Real-time process paths must
avoid allocation, locking, logging, reflection, avoidable dynamic dispatch,
and avoidable boxing; the compiler should enforce these constraints where it
can and document host responsibilities where it cannot.

Compilation returns a structured artifact rather than pretending every target
is a Clojure function:

```clojure
(dsp/compile definition {:target :interpreter})
(dsp/compile definition {:target :jvm})
(dsp/compile definition {:target :js})
(dsp/compile definition {:target :wasm :entry :process})
```

Process artifacts expose ABI version 1 metadata for:

- input and output buffers;
- frames, sample rate, channel behavior, and positional controls;
- logical state values and ownership;
- physical precision, element width, and alignment;
- memory requirements;
- initialization and reset;
- effects and diagnostics.

Interpreter and JVM artifacts own one persistent state instance and expose host
lifecycle operations. JavaScript kernels create independent instances through
`createKernel`. Wasm state is module-instance state held in globals.

The final target-neutral lifecycle model should support explicit instance
creation, reset, and destruction without changing portable process IR.
Compilation and browser/worklet attachment remain separate operations because
target artifacts have different deployment and lifecycle requirements.

## Synthesizer readiness

The compiler should eventually support polyphonic, modulation-heavy software
synthesizers without requiring backend-specific DSP source. Synthesizer support
requires a language/runtime layer above individual sample kernels while keeping
host UI and plugin integration outside this project.

### Language and memory

The language must provide:

- fixed-size arrays and initialization-time lookup tables;
- mutable array state, circular buffers, delay lines, and wavetable storage;
- explicit alignment and memory layout where useful;
- bounded indexed access with development checks;
- aggregate state that can be owned per voice, per effect, or per synth;
- reusable workspaces whose allocation occurs before audio processing;
- typed integer/bit operations, wrapping arithmetic, and phase accumulators;
- mixed physical numeric types when needed, such as integer phase with f32 audio;
- complex or interleaved data layouts sufficient for FFT/IFFT and spectral work.

The current scalar-only state model is not sufficient for these requirements.
Arrays and aggregate state should be added to the portable language before
adding target-specific FFT or filter implementations.

### Math and DSP library

Add a portable, backend-tested DSP standard library implemented in the DSP
language wherever practical. It should include:

- `abs`, `min`, `max`, rounding, fractional, and sign operations;
- `sin`, `cos`, `exp`, `log`, `pow`, `sqrt`, `tanh`, and `atan2`;
- oscillators, phase modulation, FM, pulse-width modulation, and noise;
- envelopes, LFOs, parameter smoothing, and slew limiting;
- stable multimode, nonlinear, and oversampled filters;
- interpolation, resampling, delay, chorus, distortion, EQ, and dynamics;
- FFT/IFFT and windowing once arrays, complex data, and workspaces exist.

A target intrinsic may accelerate one of these operations only after the
language-level operation and its reference semantics are defined, unless the
operation fundamentally depends on a target capability. FFT/IFFT is therefore
not a reason to immediately add three independent native libraries: first make
its algorithm expressible, then optimize it behind one portable contract if
benchmarks justify doing so.

### Modules and composition

The language must support reusable stateful DSP modules, not only the current
pure expression-only calls. Composition must make state ownership, feedback,
buffer access, and update ordering explicit. It should support:

- typed multi-input and multi-output modules;
- explicit feedback and delay boundaries;
- separately testable module definitions;
- compile-time constants and specialization without uncontrolled code growth;
- backend capability checks for any optimized implementation.

### Rates, automation, and events

The process ABI must distinguish compile-time, initialization-time, block-rate,
and sample-rate values. It must support bounded, allocation-free automation and
events at frame offsets, including:

- parameter ramps and one-pole smoothing;
- note-on and note-off with velocity and release velocity;
- pitch bend, controller changes, pressure, and per-note expression;
- tempo, beat position, transport, and sample-rate changes;
- deterministic event ordering at equal frame offsets.

Event queues must be preallocated or supplied by the host and must not assume a
128-frame block.

### Voices and synth instances

A synth-oriented runtime must support:

- preallocated voice pools and deterministic voice stealing;
- per-voice initialization, release, reset, and termination;
- mono, legato, retrigger, polyphonic, and unison modes;
- per-voice and global modulation;
- stereo spreading, voice tails, and effect tails;
- independent synth instances created from one compiled artifact.

Voice scheduling may remain a runtime layer above oscillator/filter kernels, but
its state layout and event ABI must be explicit and portable. It must not force
allocation or locking into the audio callback.

### Real-time and performance requirements

Synthesizer-ready artifacts must:

- work at arbitrary sample rates and host block lengths;
- support explicit internal oversampling factors and report introduced latency;
- perform no allocation, locking, I/O, logging, or first-use initialization in
  the audio callback;
- define denormal handling and behavior for unstable or non-finite state;
- provide bounded event processing and predictable worst-case work;
- support scalar fallbacks and optional JVM/Wasm SIMD optimization with matching
  semantics.

The browser path must remain dynamically compilable. SIMD and other target
optimizations are optional lowerings of portable DSP operations, not a reason
to introduce an AOT or native-code toolchain.

### Synthesizer-level acceptance fixture

Conformance must eventually include at least one end-to-end instrument with:

- two band-limited oscillators and noise;
- sync, pulse-width modulation, unison, envelopes, and LFOs;
- a resonant filter and modulation matrix;
- polyphonic allocation and voice stealing;
- stereo output and delay/chorus/distortion effects.

Test it for deterministic offline rendering across block sizes and backends,
filter stability under modulation, aliasing behavior, absence of zipper noise,
voice/event boundary correctness, non-finite-state containment, and zero
allocation after initialization.

## Validation and conformance

### Clojure/JVM suite

The current Clojure suite has 56 tests and 431 assertions, all passing. It
covers descriptor validation, linking, IR lowering, interpreter behavior, JVM
bytecode, artifacts, process ABIs, typed controls/state, precision variants,
loops, calls, lifecycle behavior, multichannel buffers, and malformed programs.

`omkamra.dsp.acceptance/cases` is a shared `.cljc` catalog. Each case declares:

```clojure
{:id ...
 :definition ...
 :controls ...
 :precisions ...
 :initial-state ...
 :input-samples ...
 :step ...}
```

Every case and supported precision runs against the interpreter and JVM.

### Browser testbed

The browser testbed runs the same acceptance catalog against generated
JavaScript and Wasm. It dynamically imports JS modules, compiles WAT with WABT,
instantiates Wasm, and checks block boundaries, zero-length blocks, state across
blocks, reset, NaN, and infinity.

Its full run also covers bounded-loop Wasm traps and JavaScript/Wasm
AudioWorklet adapters in an `OfflineAudioContext`, including f32/f64 and
configured multichannel output.

Useful browser entry points are:

- `omkamra.dsp.testbed/run-cross-target!` for the original f32 trace;
- `omkamra.dsp.testbed/run-shared-target-vectors!` for all catalog cases and
  precisions.

The browser harness is currently a development testbed, not a separately
automated CI browser job. There is no separate Node conformance runner.

## Known limitations

### Language and IR

- Calls cannot target recursive, stateful, or buffer-accessing definitions.
- Current non-interpreter backends inline expression-only callees instead of
  emitting calls.
- Stateful multichannel process definitions are rejected.
- Process input/output buffers are currently float buffers with a fixed overall
  channel count.
- State is scalar-only; arrays, tables, complex layouts, and circular buffers
  are not yet part of the language.
- Dynamic buffer indices are not statically proven safe.
- Sample-accurate automation, timestamped musical events, and voice management
  are not yet part of the process ABI.
- Unbounded loops remain legal and produce diagnostics rather than compilation
  failure.
- The special-value and conversion contract is not yet fully specified across
  targets.

### Runtime and ABI

- Interpreter and JVM process artifacts own one state instance rather than a
  reusable factory for independent instances.
- Wasm state is stored in globals instead of linear-memory instance regions.
- Wasm artifacts expose WAT but do not contain compiled binary bytes.
- Wasm channel stride is currently a fixed 4096 bytes.
- JS/Wasm worklet setup still depends on testbed-assembled metadata.
- The JavaScript worklet status protocol is less complete than the Wasm
  adapter's init/reset protocol.
- The compiler currently runs on the JVM; browser tests receive generated
  artifacts rather than compiling user DSP source in the browser.

### Safety and tooling

- Generated JS and Wasm memory accesses do not yet have complete explicit
  development checks; JVM relies on primitive array bounds.
- Effect metadata does not prove absence of allocation, locks, I/O, reflection,
  or dynamic dispatch in every host adapter.
- Diagnostics are structured in descriptor/linker/IR paths but are not yet
  uniform across every backend.
- There is no general debug-dump API for every compiler stage.
- Browser conformance and AudioWorklet checks are not yet automated in CI.

## Prioritized work

### 1. Extend the language for synth DSP

Add fixed-size arrays, tables, circular buffers, aggregate state, mixed numeric
representations, bit operations, complex/interleaved data, and the math/DSP
library needed for oscillators, envelopes, filters, modulation, resampling, and
FFT/IFFT. Implement these in the portable language and IR before introducing
backend-specific kernels.

Add stateful module composition with explicit ownership, feedback boundaries,
multiple ports, and update ordering. Keep native intrinsics limited to measured
or fundamentally target-specific operations with portable contracts.

### 2. Define the synth event and voice ABI

Add bounded, allocation-free sample-accurate automation and timestamped musical
events. Define preallocated voice pools, deterministic voice stealing, per-voice
lifecycle, global/per-voice modulation, unison, tails, arbitrary block sizes,
and independent synth instances.

### 3. Generalize process instances

Define a target-neutral instance lifecycle with explicit creation, reset, and
destruction. Separate reusable compiled artifacts from mutable per-instance
state. Preserve primitive allocation-free process calls after setup.

The ABI must describe:

- ownership and representation of every state value;
- control and buffer bindings;
- initialization and reset behavior;
- target-specific handles without leaking them into portable IR.

### 4. Provide browser-side DSP compilation

Make source editing and compilation independent of JVM-only macros. The
browser-facing source front end should use SCI, which is already a project
dependency, as a sandboxed reader/evaluator and macro-expansion layer:

```text
DSP source text
  -> SCI evaluation/macro expansion
  -> serializable DSP descriptors
  -> CLJS portable compiler
  -> JavaScript or WAT
  -> WABT
  -> AudioWorklet kernel
```

SCI must not execute DSP bodies sample-by-sample. The DSP definition macro
should capture each body as data, as it does on the JVM, and SCI should expose
only an allowlisted DSP namespace and safe literal/core forms. Java, arbitrary
JavaScript, I/O, dynamic evaluation, and unrestricted host functions must not
be available to edited DSP source.

Provide an SCI-compatible `omkamra.dsp` macro namespace that registers
serializable descriptors in an in-memory definition registry. Browser linking
must resolve that registry rather than JVM Vars, `find-ns`, or `ns-resolve`.
Captured aliases, refers, source locations, and diagnostics must have an
explicit browser representation.

Port the descriptor validator, linker, portable IR, and JavaScript/Wasm
emitters to `.cljc`/`.cljs` or provide semantically identical browser
implementations. The current JVM-only `.clj` namespaces cannot be loaded in the
browser unchanged. The JVM and browser front ends must produce the same
normalized descriptor and portable IR for equivalent source.

The browser compiler must emit JavaScript or WAT, pass WAT through WABT, and
install the resulting AudioWorklet without a server-side compiler. Compilation
and worklet attachment must remain separate from audio-thread execution.
Support incremental recompilation, descriptor/IR caching, clear source
locations, and errors that distinguish SCI errors from DSP validation and
backend errors.

### 5. Make worklet installation artifact-driven

Generate JavaScript and Wasm adapter configuration directly from artifact ABI
metadata rather than reconstructing it in `omkamra.dsp.testbed`. Include:

- processor name and module source/URL;
- channel layout and absent-input behavior;
- control names, defaults, ranges, and automation rates;
- physical element type and frame capacity;
- Wasm offsets, strides, required memory, and exports;
- status, initialization, and reset messages.

Keep the audio callback free of avoidable allocation.

### 6. Complete Wasm artifacts and memory ABI

Produce validated binary bytes as part of a defined compilation step. Move
persistent state from globals to explicit linear-memory instance regions.
Remove fixed-layout assumptions where practical and validate representability,
alignment, capacity, and non-overlap.

A complete artifact should expose at least:

```clojure
{:target :wasm
 :bytes ...
 :wat ...
 :exports ...
 :memory ...
 :abi ...
 :metadata ...}
```

### 7. Strengthen safety and diagnostics

Add static checks or explicit policy diagnostics for:

- allocation in process execution;
- reflection and dynamic invocation;
- locks, logging, and I/O;
- unbounded loops;
- unvalidated memory access;
- unsupported target capabilities.

Use structured diagnostics containing source location, definition ID,
dependency path, IR path, target, precision, offending form, and expected/actual
types. Add optional debug output for normalized descriptors, linked units,
portable IR, target IR, emitted source/WAT, and ABI layouts.

### 8. Expand automated conformance

Use the interpreter as the oracle for every applicable backend. Add
precision-aware comparison reports that treat NaN and signed zero explicitly.
Extend shared vectors to cover:

- all arithmetic, comparisons, boolean operations, and conversions;
- bounded loops, `break`, and `continue`;
- function calls and local mutation;
- typed controls and state;
- mono and multichannel memory access;
- partial and zero-length blocks;
- NaN, infinities, signed zero, overflow, and conversion boundaries;
- malformed source, missing dependencies, and cycles;
- memory-layout and export validation.

Automate generated JS, Wasm, and AudioWorklet checks in a browser test job.

### 9. Add conservative optimization

Optimization follows conformance and safety work. Initial safe candidates are:

- target-precision-aware constant folding;
- dead-local and unreachable-branch elimination;
- local copy propagation;
- bounded-loop simplification;
- a conservative inlining policy with code-size limits.

Run differential tests before and after optimization. Do not reassociate
floating-point expressions unless a separately selected policy permits it.

## Completion criteria

The compiler is complete when:

1. Definitions link deterministically with actionable source-aware diagnostics.
2. Portable IR fully represents supported types, control flow, effects, calls,
   memory, state, and process metadata.
3. The interpreter is the executable semantic specification.
4. JVM, JavaScript, and Wasm consume the same validated IR.
5. Every backend returns a complete artifact with executable payload and ABI
   metadata appropriate to that target.
6. Compiled artifacts can create independent process instances with explicit
   lifecycle operations.
7. The JVM real-time path avoids reflection, allocation, locking, logging,
   avoidable dynamic dispatch, and avoidable boxing.
8. Worklet adapters are configured from artifact metadata and honor actual
   browser block and channel layouts.
9. Wasm artifacts contain validated binary bytes and a stable instance-oriented
   linear-memory ABI.
10. Automated differential tests cover every backend, precision, lifecycle,
    memory, and special-value policy.
11. Safety guarantees and remaining runtime responsibilities are documented and
    enforced where possible.
12. The language can express the state, memory, math, modulation, and event
    processing required by the reference polyphonic synthesizer.
13. The reference synthesizer supports independent voices, deterministic voice
    allocation/stealing, sample-accurate events, arbitrary block sizes, and
    zero allocation after initialization.
14. Browser users can edit DSP source, compile it dynamically, install the
    generated AudioWorklet kernel, and test it without an AOT or server-side
    compiler.

## Source organization

Current implementation files:

```text
src/omkamra/dsp.clj                       public API, macros, artifacts, ABI
src/omkamra/dsp/descriptor.clj            normalization and validation
src/omkamra/dsp/linker.clj                dependency graph and linking
src/omkamra/dsp/ir.clj                    typed portable IR
src/omkamra/dsp/interpreter.clj           reference semantics
src/omkamra/dsp/jvm.clj                   JVM bytecode backend
src/omkamra/dsp/js.clj                    JS-like lowering and emitter
src/omkamra/dsp/wasm.clj                  Wasm lowering and WAT emission
src/omkamra/dsp/acceptance.cljc           shared semantic cases
src/omkamra/dsp/worklet_adapter.cljs      JavaScript worklet adapter
src/omkamra/dsp/wasm_worklet_adapter.cljs Wasm worklet adapter
src/omkamra/dsp/testbed.clj                browser development endpoints
src/omkamra/dsp/testbed.cljs               browser conformance harness
src/omkamra/dsp_test.clj                   Clojure/JVM test suite
```

ABI helpers may eventually move into `omkamra.dsp.abi`, and shared worklet
metadata helpers into a `.cljc` namespace, when doing so makes ownership clearer.
Backend namespaces must not take responsibility for source validation, linking,
or target-neutral ABI inference.
