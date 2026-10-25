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

## Planning baseline

This plan covers the whole v1 system rather than a narrow compiler MVP. The
non-negotiable gates are semantic correctness and real-time safety. Browser
usability, performance, and the complete synthesizer capability are also v1
goals, delivered in dependency order rather than traded away.

JVM, JavaScript, and WebAssembly are equivalent first-class targets for the DSP
kernel, with both `:f32` and `:f64` modes. The sequencing/runtime layer is
portable in behavior but is hosted on the JVM for JVM applications and in
ClojureScript for browser applications; it is not required to run on
WebAssembly. A single synth description should produce both the DSP kernel
artifacts and the host-target sequencing/runtime configuration.

The system has two explicit layers:

1. The **DSP kernel layer** owns typed sample processing, state, memory,
   controls, lifecycle, and the canonical timestamped message ABI.
2. The **sequencing layer** owns timeline scheduling, preallocated event and
   voice storage, voice allocation/stealing, modulation routing, and delivery
   of timestamped messages to the kernel.

All capacities are explicit compile-time configuration: voice count, event
queue capacity, modulation routes, workspaces, effect storage, and timeline
lookahead. Runtime allocation is not an overflow strategy. The default event
queue overflow policy is deterministic drop-oldest, and all other overflow and
failure policies must be explicit in the compiled configuration and diagnostics.
The sequencing layer must support note-on/off with velocities, pitch bend,
controller and parameter changes, pressure/per-note expression, transport
tempo/beat data, and sample-rate changes. Events are applied at exact sample
or frame offsets with deterministic ordering for equal timestamps and arbitrary
host block sizes.

The v1 acceptance instrument is the reference definition of a general-purpose
synth: polyphonic band-limited oscillators and noise; sync, pulse-width
modulation, envelopes, LFOs, and modulation routing; a resonant filter; unison;
deterministic voice stealing; stereo output; and delay/chorus/distortion
effects. It must render deterministically across block sizes and supported
backends, remain stable under modulation, handle event boundaries correctly,
contain non-finite state according to a documented policy, and allocate
nothing after initialization.

V1 excludes plugin APIs and formats, GUI design, project/preset file formats,
network collaboration, distribution/packaging, and native or AOT toolchains.
The DSP language remains deliberately limited to the constructs necessary for
the reference synthesizer; arbitrary host-language execution is forbidden in
DSP code.

## Runtime topology and compilation products

The DSP compiler and sequencing runtime are separate layers with a shared,
serializable synth schema. Compiling a synth description produces:

- reusable DSP kernel artifacts for the interpreter, JVM, JavaScript, and Wasm;
- a target-neutral ABI and metadata manifest;
- host-target sequencing configuration for JVM or ClojureScript; and
- any precomputed memory layout, voice, event, modulation, and workspace
  requirements.

The sequencing layer is not compiled into the Wasm module. On the JVM it runs
on the JVM; in the browser it runs as ClojureScript in the AudioWorklet. It may
also be driven from a GUI, CLJ REPL, or CLJS REPL through a non-real-time
control API. The shared synth schema defines the compatibility boundary, but
kernel instances and sequencing services have independent lifecycles.

A real-time instance has this topology:

```text
non-real-time GUI/REPL/timeline producer
  -> control/event transport
  -> preallocated real-time scheduler and voice manager
  -> timestamped kernel-message ABI
  -> DSP kernel instance
```

The scheduler owns event and voice-allocation metadata; the kernel owns typed
signal-processing state. Both use a preallocated per-instance memory arena.
The scheduler runs once per audio block, dispatches messages at exact frame
offsets, and supports deterministic ordering for equal timestamps and arbitrary
host block sizes. The browser AudioWorklet owns the real-time scheduler and
kernel. `MessagePort` is the required browser control transport; the worklet
copies incoming data into its fixed queue before processing. `process()` never
allocates and never depends on message delivery timing. Each instance exposes a
single-producer, single-consumer queue: a non-real-time ingress component
serializes GUI, REPL, and timeline inputs, while the audio thread is the sole
consumer. Multi-producer support, if added later, must be host-side and must
not introduce audio-thread locks.

All capacities are explicit compile-time configuration: maximum voices, event
queue length, modulation routes, workspaces, effect storage, and timeline
lookahead. The event queue's default overflow policy is to drop the oldest
future event, never an event already due or being dispatched, and increment a
fixed diagnostic/status counter outside the audio path. Other overflow and
failure policies must be explicit in compiled metadata. Runtime allocation is
never an overflow strategy.

The sequencing ABI includes note-on/off with velocity and release velocity,
pitch bend, controller and parameter changes, pressure and per-note
expression, transport tempo/beat data, and sample-rate changes. It delivers
canonical timestamped messages to the kernel; it does not require the kernel
to know about GUI or host-language objects. A timestamp is an absolute integer
sample/frame position on a monotonic synth timeline. A monotonically increasing
ingress sequence number orders events with equal timestamps, making behavior
independent of host block boundaries and comparable across JVM and browser
runtimes.

The numeric contract uses IEEE-754 `f32` or `f64` according to compilation
mode. `:int` is a signed two's-complement 32-bit value with explicit wrapping
arithmetic. Conversions, NaN, infinity, signed zero, overflow, and rounding
are specified and tested. Reference semantics preserve subnormals. Flush to
zero is an explicit opt-in compilation/runtime policy with separate
conformance expectations; it is not the default. The default non-finite-state
policy detects NaN or infinity, silences and resets the affected voice, and
increments a fixed diagnostic counter; propagation is an explicit diagnostic
mode rather than the default audio behavior.

Every DSP and sequencing operation in the audio path must have statically
bounded work. Unbounded loops, recursion, dynamic allocation, and unconstrained
event dispatch are rejected before audio starts. Runtime failures use bounded,
predeclared policies such as dropping events, silencing a voice, or incrementing
a fixed diagnostic counter; they never log, allocate, block, or throw through
the audio callback.

Recompilation occurs off the audio thread. A replacement is initialized as a
new instance and switched at a block boundary without automatic state
migration. An optional host-level crossfade renders both instances for a
configured bounded duration using preallocated resources; if resources are
insufficient, the host performs a hard block-boundary switch.

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

### Namespaces

```text
omkamra.dsp.numbers              (.cljc)  every host-different numeric decision (D80)
omkamra.dsp.compiler             (.cljc)  the portable compile pipeline (D81)
omkamra.dsp.capture              (.cljc)  the reading of a definition form, shared by both worlds (D83)
omkamra.dsp.source               (.cljc)  one `compile`: a definition, or a source string (D83)
omkamra.dsp.source-reader        (.cljc)  recover a literal's spelling from source text (D91)
omkamra.dsp.sci                  (.cljc)  the SCI context the browser evaluates source in (D83)
omkamra.dsp.registry             (.cljc)  descriptors by id; linking's portable resolver (D83)
omkamra.dsp.library-registry     (.cljc)  the DSP library as that registry's seed (D83)
omkamra.dsp                      (.clj/.cljs)  the user-facing facade: macros, and the two world faces
omkamra.dsp.instance             (.clj)   the target-neutral process-instance lifecycle
omkamra.dsp.instance.jvm         (.clj)   the JVM target's raw process ABI; the facade re-exports both (D89)
omkamra.dsp.{descriptor,ir,linker,builtins,literals,readers,names}   the compiler front end
omkamra.dsp.targets.capability   (.cljc)  what each target serves (D29)
omkamra.dsp.targets.interpreter  (.cljc)  the portable reference interpreter (the oracle)
omkamra.dsp.targets.{jvm,js,wasm}         the three code generators (js/wasm are .cljc)
omkamra.dsp.library.*            (.cljc)  the standard library; LIBRARY.md is generated
omkamra.dsp.{graph,combinators}  (.cljc)  definition values, transforms, `poly`, `share`
omkamra.dsp.event, schedule      (.cljc)  the host-side sequencing layer (P9)
omkamra.dsp.{modulation,sequencer,instrument}  the instrument layer (M)
omkamra.dsp.synth, synth.pool    (.cljc)  description -> config; voices, allocation, rendering
omkamra.dsp.synth.bundle         (.cljc)  description -> artifact + runtime config, in either world (D95)
omkamra.dsp.reference            (.cljc)  the reference synthesizer
omkamra.dsp.fusion               (.cljc)  where a pool's controls land in a fused voice (M6)
omkamra.dsp.hosts.jvm            (.clj)   the JVM world's assembler: a bundle + its kernel -> a pool
omkamra.dsp.kernels.{jvm,interpreter,js,wasm}   one `Kernel` per target (js/wasm are .cljs)
omkamra.dsp.kernels.mixing       (.cljc)  rendering a voice into scratch, summing it into the block
omkamra.dsp.kernels.regions      (.cljc)  the flat regions a pool writes automation into
omkamra.dsp.instrument.program   (.cljc)  the instrument program + live host; drives a bus kernel (D85)
omkamra.dsp.worker               (.cljc)  off-thread sequencing; a Shadow `:web-worker` entry
omkamra.dsp.worklet_adapters.js  (.cljs)  the AudioWorklet adapter
omkamra.dsp.harness.catalog      (.cljc)  the data-only conformance catalog
omkamra.dsp.harness.conformance  (.clj)   plans, payloads, the JVM differential driver
omkamra.dsp.harness.{diff,emission}       comparison, including the emission check
omkamra.dsp.harness.{browser,testbed}     the browser suite and the page it drives
omkamra.dsp.harness.{equivalence,formatting}  the two compiler checks, run in the page
omkamra.dsp.tests.*              (.clj)   the suite by subject (tests.suite/run-jvm)
```

The front end has three parts worth keeping apart: **descriptor** normalizes the
macros into data, **linker** resolves `dsp/call` and inlines what is not called, and
**ir** lowers to a small typed IR whose `:diagnostics` channel carries what the
compiler noticed. `targets.capability` is what lets a target refuse an option it
cannot serve, before any lowering (D29), and what says whether a target emits a
call or evaluates the call graph (D94). **compiler** is the pipeline around them —
options, capability check, link, lower, `prepare-for-target`, dispatch — as
`.cljc`, so the JVM's `dsp/compile` and the browser's own compile are one
implementation; the final target step is each target's own `compile-artifact`,
and only the JVM backend is injected from the side that has `insn` (D81, D94).
**source** is the front end over it: `compile`
takes a definition or a source string, and `evaluate` takes `:sci` (both worlds)
or `:native` (the JVM's own reader and macros); **capture** is the reading both
kinds of macro call; **sci** builds the context the page evaluates a string in.
**omkamra.dsp** is the facade, `.clj` and `.cljs` as the two world faces, so a
browser app gets `dsp/defn` (via `:require-macros`) and `dsp/compile` (via
`:require`) from one alias (D83). It owns `compile`, `link` and the macros; the
process-instance lifecycle is not its own. The target-neutral half lives in
`omkamra.dsp.instance`, the JVM target's raw ABI in `omkamra.dsp.instance.jvm`,
and the facade re-exports both, so internal code — the JVM host above all — can
drive an instance without loading the macro namespace (D89).

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

Calls are non-recursive and may target stateful modules. Each call site owns a
private partition of the callee's persistent state (D19), so two calls never
share state and one call site reused in a loop reuses one copy; a module still
may not reach into the caller's process buffers. The interpreter executes
explicit `:call` nodes against the call site's partition, keeping it an
independent semantic oracle (D2). JVM, JavaScript, and Wasm share one
statement-level inlining pass that renames the callee's locals and rebases its
state into the call site's partition, bounded by a documented statement budget.

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
- resolved function calls, including calls to stateful modules whose state is
  partitioned per call site (D19);
- record values returned from a module and projected by field (`(:field r)`),
  erased to one local per output at lowering (D19);
- persistent typed scalar state, fixed-size typed state arrays, named records
  (`:struct`), and read-only constant tables (`:constant`);
- explicit buffer loads/stores and indexed `state-load`/`state-store`
  operations, and `field`/`set-field!` record access;
- preallocated fixed-size local arrays (`array` with `aget`/`aset`);
- frame, channel, frame-count, and sample-rate values.

Process IR is lowered directly from process metadata. It contains canonical
buffer descriptors, controls, state slots, a per-frame body, lifecycle flags,
memory requirements, effects, and diagnostics. The instance arena is the
entry's own state, then one partition per call site, then local-array scratch.
A process's own scalar state transitions are computed before its stores so
updates within a frame are simultaneous; a *called* module applies its
transition at the call, so successive calls in a frame observe each other.
Fixed-size aggregate state uses explicit ordered loads and stores, making
feedback and delay update order visible in source and IR.

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

The JavaScript backend lowers portable IR to a structured JS-shaped IR, turns
that into ClojureScript forms, and lets `squint.compiler` emit the ES module
(D41). Function artifacts export `invoke`; process artifacts export
`createKernel`. Process kernels include instance-local state and `init`,
`reset`, and `process` operations, and expose the state arena so a host can read
a voice's special slots.

Because a kernel is installed as one script, the module stays **standalone**:
nothing lowers to a `squint_core` reference and the module imports nothing.

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

Input, output, and persistent state use linear memory. State regions have
explicit offsets, alignment, element widths, and initialized contents; controls
remain process parameters. Reusable independent instance regions are still not
part of the ABI.

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
`createKernel`. Wasm state is module-instance state held in linear memory.

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

The sequencing/runtime layer must separately provide preallocated event and
voice storage, deterministic voice allocation and stealing, modulation routing,
and exact timestamp-to-frame dispatch. Its event ABI must include note-on/off,
velocity and release velocity, pitch bend, controller and parameter changes,
pressure and per-note expression, transport tempo/beat data, and sample-rate
changes. These concerns must not be hidden inside target-specific DSP kernels.

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

Persistent state, local-array scratch, and record fields share one flat
instance **arena**. A state declaration may supply `:size` and a same-length
typed initializer vector accessed with `(state-load name index)` and
`(state-store name index value)`; `(array :float N)` declares preallocated
scratch indexed with `(aget a i)`/`(aset a i v)`; and `dsp/defstruct` names a
record shape referenced with `{:name slot :struct shape}` and accessed with
`field`/`set-field!` (or `(:field slot)`). A scalar record may own an array
field (`:size M`); array storage is a static *place*, so `(aget rec k)`,
`(aget (:buf rec) k)`, and a `let` alias all index the same buffers. Records are
laid out struct-of-arrays across the same arena. All of this lowers and executes
on the interpreter, JVM, JavaScript, and Wasm backends. P5 adds circular-buffer delay lines with linear
and allpass fractional interpolation as library processes (D17). Stateful
module composition (P6), complex/interleaved layouts, and generated
initialization-time tables remain to be designed before backend-specific FFT or
filter implementations.

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
- mono, legato, retrigger, and polyphonic modes (unison is a voice transform,
  D36, not a mode);
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

The current Clojure suite has 57 tests and 440 assertions, all passing. It
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

What is still not there, as of P13b. Items a later phase closed are not listed:
the diary entry for that phase is the record. Choices that are deliberately still
open are in §Open questions below.

### Language and IR

- Recursion is rejected at link time: a definition cannot call itself.
- A definition that returns a record, or that accesses scratch arrays, is inlined
  into its call sites rather than called (D47's `uncallable` set).
- Dynamic buffer indices are not statically proven safe; they are diagnosed (the
  IR's `:diagnostics` channel carries them) rather than rejected.
- Fixed-size state arrays and tables, preallocated local arrays, named records and
  `defstruct`, read-only constant tables, and circular-buffer delay lines with
  interpolation are part of the language. Complex and interleaved layouts, and
  generated initialization-time tables, are not.
- Process input and output buffers are float buffers with a per-process channel
  count; there is no other element type on the audio path.
- Constructs whose worst-case work cannot be statically bounded are rejected before
  audio starts, and the diagnostic names the construct and the bound requirement.
- The special-value and conversion contract is not complete or uniform across
  targets: reference semantics preserve subnormals, flush-to-zero is an explicit
  non-default policy, and non-finite `float->int` has no cross-target contract at
  all (§Open questions).

### Runtime and ABI

- Wasm artifacts carry the WAT text beside the binary bytes; whether the WAT belongs
  in a shipped artifact is undecided (§Open questions).
- JS and Wasm worklet setup still depends on testbed-assembled metadata. The
  kernel's own shape comes from the artifact ABI (D40), but the processor options
  that carry it are assembled by the testbed rather than generated from a standalone
  installation API. That is P14.
- The JavaScript worklet status protocol is less complete than the Wasm adapter's
  init/reset protocol.
- The compiler core, both emitters and the compile entry point run in the browser
  (P13a–P13c): the page compiles a definition to a JS module or WAT itself, through
  the same portable pipeline the JVM uses, and `compiler equivalence` checks the
  artifact. What remains is the *source* front end — the SCI capture macros and a
  browser definition registry (P13d) — so the page compiles catalog definitions
  rather than user-entered source, and the eight cases whose unit reaches a library
  definition are still skipped.
- The sequencing runtime is ClojureScript rather than Wasm.

### Harness and tooling

- Generated JavaScript and Wasm memory accesses do not yet have complete explicit
  development checks; the JVM relies on primitive array bounds.
- Effect metadata does not prove the absence of allocation, locks, I/O, reflection,
  or dynamic dispatch in every host adapter.
- Diagnostics are structured in the descriptor, linker and IR paths but are not
  uniform across every backend, and the capability declarations cover the options
  and entry kinds only — not allocation, reflection, locks, unbounded loops or
  unvalidated memory access (P15).
- There is no general debug-dump API for every compiler stage.
- Browser conformance and AudioWorklet checks are not yet automated in CI.

### Open questions

Choices not made yet, each with what decides it.

- **Non-finite `float->int`.** The interpreter throws, the JVM saturates (`d2i`), and
  JavaScript and Wasm do as their own conversions do. It is routed around, not
  decided.
- **Does `:wat` ship in every artifact?** It is what the emission check compares and
  what a Wasm failure is read from, and it is dead weight in a bundle. P14 decides,
  when the artifact becomes the thing that ships.
- **The artifact byte transport.** A data URL, a `Uint8Array` message, or a blob URL?
  P14's choice, and it decides what the binary step must produce.
- **Where the WABT pin lives.** The WAT-to-binary step is a pinned toolchain step on
  the JVM (D72). If the browser ever assembles, the pin has to become something a
  page can have.
- **What `dsp/compile` assembles, and what a caller assembles.** The assembly policy
  is unsettled, which is why P12 and P14 describe the binary step rather than
  assuming a place for it.
- **Does the fused lowering still earn its keep?** D47 asks it, now that `^:inline`
  is reachable from the source.
- **The boxed float literal.** D78 keeps the box. A measurement over the catalog
  says a context rule would resolve 142 of 168 boxed literals (85%, or about 94% if
  an untyped parameter means `:float`) with no conflicts, at the cost of threading
  types down through lowering. Deferred, and safe to revisit because
  `compiler equivalence` would catch the two worlds inferring differently.

## Prioritized work

The plan's priority areas, stated as scope; `PHASES.md` owns the ordering and the
status of each. As of P13: **1** language and library, **2** the event and voice
ABI, and **3** process instances are delivered across P1-P11; **4** browser-side
compilation is P13, and its slices P13a–P13d are done; the remaining slice,
P13e, is **dropped** (D93 — incremental recompilation and caching measured as not
paying, and the error classification belongs to P15);
**5** artifact-driven installation is P14;
**6** Wasm artifacts and the memory ABI is P12 and is done; **7** safety and
diagnostics is P15; **8** conformance and release gates is P16.

### 1. Extend the language for synth DSP

P1–P7 delivered the scalar and fixed-size aggregate cases on every target — bitwise
operations, the portable math builtins and transcendentals, state arrays and tables,
preallocated local arrays, named records, read-only constant tables, circular-buffer
delay lines, and stateful module composition with explicit ownership, feedback
boundaries, multiple ports and update ordering.

What this priority still wants is mixed numeric representations, complex and
interleaved data, and whatever the DSP library needs for oscillators, envelopes,
filters, modulation, resampling and FFT/IFFT that the portable language cannot
express. Keep it in the portable language and IR before introducing backend-specific
kernels, and keep native intrinsics limited to measured or fundamentally
target-specific operations with portable contracts.

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
Keep clear source locations, and distinguish SCI errors from DSP validation and
backend errors (that boundary is P15's structured diagnostics). Incremental
recompilation and descriptor/IR caching were dropped (D93): measured, the
cacheable phases are a fifth of a recompile, and emission — which dominates — is
whole-program.

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
- a conservative inlining policy with code-size limits;
- deduplication of identical inlined bodies (unrolled voices emit K copies);
- common-subexpression folding for **stateless** subexpressions only.

Call-argument hoisting (binding each computed argument to a local before
inlining) is already in the lowerer; it is what keeps parameter substitution from
re-duplicating an argument once per use, and from running a stateful argument
twice.

Run differential tests before and after optimization. Do not reassociate
floating-point expressions unless a separately selected policy permits it.

### 10. DSP form macros (exploration, not scheduled)

Explore letting **plain Clojure functions act as macros inside DSP forms** —
functions `(forms…) -> form` that the compiler calls at compile time during an
expansion pass ahead of `descriptor/normalize` (the way Clojure expands before
analysis), with the result spliced into the body. A symbol→function registry
replaces `defmacro`, shared by the JVM front end and the P13 SCI front end.

Where it would help:

- **Expression-scope transformers.** `unison`/`through`/`mix` are whole
  *definition* transforms today (D36–D38); an in-body `(unison 3 detune (osc …))`
  would cover one-off structures that do not deserve a named definition.
- **The strictly-binary operator surface.** A `nary` macro expanding
  `(+ a b c)` to the required nesting removes a real, ongoing annoyance.
- **Generation and partial application.** Chord voicings, currying a library
  definition with baked-in constants, splicing a small subgraph fragment.

Where it does not help: sharing/fan-out (already `let`; D38), and inline control
declaration such as `smooth` (controls live in the `:process` map, so a macro
that needs the whole definition *is* a transformer).

Hazards to design against: **hygiene** (a macro introducing locals needs gensym
discipline); **effects/linearity** (expanding a form containing `store-special`
or a stateful call duplicates side effects — the same trap `lower-call` argument
hoisting fixes); a **bounded expansion budget** (as `inlining-statement-limit`);
and error attribution to the macro's own name and form.

Rule: a macro is a way to *write* the existing typed language, not to bypass IR
invariants — the same constraint as optimizer lowerings (only among lowerings the
system can express). The lowest-risk first prototype is `nary` sugar plus
expression-scope transformers.

## Validation and release gates

Correctness is the release gate. Every supported kernel operation and runtime
operation must be differentially tested against the interpreter/reference model
for each target, precision, lifecycle state, event boundary, and arbitrary
partitioning of a block. Tests must cover NaN, infinity, signed zero,
conversions, overflow, subnormals, the explicit flush-to-zero policy, queue
overflow, voice stealing, sample-rate changes, and malformed configurations.

Where target math can be identical, comparisons must be bitwise exact. Only
operations whose target math libraries cannot be identical (notably
transcendentals) may use narrowly documented tolerances. Rendering must be
deterministic across block sizes and event partitioning. Browser tests must
exercise both JavaScript and Wasm kernels in an AudioWorklet. JVM and browser
audio paths must demonstrate zero allocation after initialization, including
scheduler dispatch and crossfade when enabled, and bounded-work tests must
verify all configured limits.

Performance budgets are recorded as regression thresholds rather than replacing
semantic tests. Allocation, latency, throughput, and worst-case work must be
measured separately for interpreter/reference, JVM, JavaScript, and Wasm paths.

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
15. A serializable synth description produces compatible kernel artifacts and
    host-target sequencing configuration without coupling sequencing to Wasm.
16. The sequencing layer supports all v1 event categories, exact frame-offset
    dispatch, deterministic voice allocation/stealing, fixed capacities, and
    the specified drop-oldest overflow policy.
17. Recompilation can replace an instance at a block boundary, optionally with
    a bounded preallocated crossfade and no automatic state migration.
18. A complete conformance and performance suite enforces semantic equivalence,
    real-time allocation/work bounds, and the documented numeric policies.

## Source organization

Current implementation files:

The namespace list above is the map; the DSP subtree's files follow it. A few
entries are worth naming because their extension is the portability decision:
`descriptor`, `linker`, `ir`, `builtins`, `literals`, `numbers` and `compiler` are
`.cljc`; `targets/interpreter`, `targets/js` and `targets/wasm` are `.cljc`;
`targets/jvm` and `kernels/jvm` are `.clj` because `insn` is a JVM library. Backend
namespaces must not take responsibility for source validation, linking, or
target-neutral ABI inference — `compiler` is where those meet a target.
