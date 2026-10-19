# Instrument parameters and modulation

**Status.** Working design note. It records the design that emerged while
reviewing the idea collection in `src/omkamra/dsp/docs/modulation.md`. It is not
yet a locked `DECISIONS.md` entry; items marked **open** still need a decision
before implementation. Read with `PLAN.md`, `PHASES.md`, `DECISIONS.md` (D34,
D36–D38, D42), and the original idea file.

This document covers the layer *above* the kernel: instrument parameters,
events and triggers, the modulation matrix and the two generators that consume
it, and where the sequencer runs. The kernel and the assembler remain as
described in `PLAN.md`.

## The model

### Two levels: control and parameter

- A **control** is an input of a DSP process definition (`dsp/defn`). It is a
  kernel-level value, resolved per voice, per frame.
- A **parameter** is an instrument-level, host-addressable named value, with a
  range or an enumerated set and a default. It is what the instrument API
  addresses and what a UI binds to.
- A **binding** maps a parameter to a control. A parameter may drive several
  controls and a control may be driven by more than one parameter.

Parameters are **uniform**: omkamra.dsp does not care where a value came from.
A note, a control change, a knob/fader, an XY pad, or a sequencer curve all
arrive through the same API. Provenance (the old `control-sources` /
`:from :event` distinction) is the client's concern, not the compiler's.

Every parameter declares a **scope**:

- `:scope :instrument` — one value for the whole instrument (a global control,
  typically a post-mix/effect parameter).
- `:scope :voice` — a per-voice value, and then an **addressing**:
  - `:addressing :shared` — every voice reads the same value (broadcast);
  - `:addressing :per-voice` — each voice has its own value (e.g. `pan`).

"Broadcast" must mean *each voice consumes the same state variable*, not a
copy fanned out to N voices every frame. See the deferred shared-region item
below.

### Events and triggers

External occurrences are **events** on a monotonic timeline, carrying an
absolute frame and an ingress sequence number (see `omkamra.dsp.event`).

**Triggers** are the general concept. `note-on` and `note-off` are built-in
trigger instances; an **event handler** for a trigger performs the runtime
action and populates parameters. The built-in note handlers allocate/release a
voice and set the conventional `gate`/`note`/`velocity` parameters; instrument
defined triggers run user handlers. The note behaviour is therefore a special
case of the general machinery rather than logic hard-coded into instruments.

A handler runs in the **sequencer**, off the audio thread, in ingress order
(frame, then sequence), so parameter writes are deterministically ordered.

### The matrix is a generator input

The instrument never holds a modulation matrix. A matrix is a data structure
of routes `{:from parameter :to parameter :depth amount :delay frames}`
(both sides name parameters; `:delay` defaults to 0), passed by the user to
one of two generators:

- the **instrument reducer generator** returns a Clojure/ClojureScript function
  used in the sequencer's event-processing path; or
- the **kernel reducer generator** takes a kernel description and returns a new
  description that implements the matrix in the process logic, for the
  assembler.

The generator validates routes against the instrument's parameter declarations
(names, scopes, types), so it needs the declarations (or the instrument).

## Where the sequencer runs

The sequencer — event processing, parameter updates, modulation reducers — runs
**off the audio thread**. In the browser it belongs in a dedicated **Web
Worker** running ordinary ClojureScript (full `cljs.core`, a normal build), not
in the AudioWorklet. The AudioWorklet runs only the generated standalone DSP
kernel plus a preallocated ingress queue and `omkamra.dsp.schedule`.

This keeps two compilation pipelines separate: the worker is plain CLJS; the
worklet is the generated JS/Wasm (Squint/WABT).

**No lockstep.** The worker emits timestamped events with a lookahead window;
the worklet owns the monotonic timeline and dispatches events at exact frames.
The only coupling is "the event was in the queue before its frame," bounded by
the lookahead and the drop-oldest-future overflow policy. Lockstep would
reintroduce GC/jank coupling between the threads and is unwanted.

Mechanics:

- The `AudioWorkletGlobalScope` and its `port.onmessage` handlers run on the
  audio thread, between render quanta, so the worklet's ingress queue is
  single-threaded and needs no atomics.
- Give the worker and the worklet a direct `MessageChannel` (the main thread
  creates it and transfers one end to each) rather than routing through the
  main thread.
- Treat `currentFrame`/`currentTime` as the authoritative clock; have the worker
  estimate offset and drift and schedule in audio frames.

## The sequencer and multiple pools

A modular piece has several voice pools and is sequenced as one piece. That is
a single **sequencer**, not one per pool: one ingress queue and timeline, and
one scheduler cutting the block at the union of every event frame, with events
routed to the pools they address. Per-pool timelines would have to be kept in
sync and their boundaries unioned by hand, which is what one scheduler avoids.

Open points the multi-pool host must settle:

- **Routing.** The event vocabulary has `:channel` (0..15) on note events, so a
  routing table keyed by channel (and/or program) is the cheapest destination.
  More than 16 destinations, or structured targets, need an explicit `:target`
  field — a deliberate extension of the closed vocabulary.
- **Mixing.** `synth.pool/render!` clears the block before mixing its voices, so
  several pools are rendered into their own scratch buffers and summed; the
  sequencer owns that scratch.
- **Piece value and lowering.** The D42 instrument value covers one voice and its
  chain; a piece is a collection of them with routing and a global bus, compiled
  to several bundles plus a bus (the deferred shared-bus/offsets work in D40).
- **Parameter scoping.** D44's `:scope :instrument | :voice` needs instrument
  qualification, or a third `:scope :piece`, before the parameter API hardens.
- **Cross-instrument modulation.** The kernel reducer generator works on one
  kernel, so a route that crosses instruments, or drives a piece parameter, is
  the instrument reducer generator's job (or a fused kernel).

The M1 single-pool `Sequencer` is the seed: M2 generalizes it to a routing table
over several pools on the one timeline. *Performance* is reserved for a future
multi-track coordinator with independent per-pool transports, if that is ever
wanted.

## Data shapes for continuous modulation

The choice is **per control**, not per instrument. Three shapes, which compose:

1. **Sparse event scalars + kernel `:smooth`/`:slew`.** One scalar per event,
   exact at the event boundary; the kernel synthesizes a one-pole or bounded
   step per frame (D27). Cheapest; available today.
2. **Sparse control points + worklet-side interpolation into lane buffers.**
   Low transport cost, per-sample values; the curve shape is whatever the
   worklet interpolates. Needs lane delivery through the pool.
3. **Host-rendered lane buffers over `SharedArrayBuffer`.** Arbitrary
   host-computed curves, highest bandwidth. Avoid `postMessage` for these;
   requires cross-origin isolation.

Lanes already exist in the map/process ABI (D28) and compose with kernel
smoothing, but the **pool and the JVM/JS/Wasm kernels do not deliver lanes to
voices yet** — `process-unchecked!` is called with no lane region. That
plumbing is the enabling substrate for shapes 2 and 3.

A modulation lane is a per-control responsibility: it carries the per-sample
values of that control, either per voice or shared across voices.

## Shared kernel region for instrument-level parameters (M5)

An instrument-level (broadcast) parameter is **one state variable that every
voice reads**, not a value copied into N voices. The pool's interim mechanism —
resolve the controls per voice and rewrite whole control arrays, on change — is
correct but has the wrong shape for a global parameter, and there is no place for
a kernel-side generator to write a global *to*.

**The language.** A control may declare `:shared true`. It then leaves the ABI's
scalar control list, and its **name is a read of the shared region**, exactly as a
`:lane` control's name is a read of the lane region. It composes with nothing
else: `:shared` beside `:lane`/`:has-lane`, or on a non-float control, is refused
at descriptor time.

**The region.** One `:shared` region per *pool*, with the **lane region's shape**:
one `:frame-capacity`-frame slice per shared control, `:shared-stride` equal to
`:lane-stride`, addressed by the same compile-time base. That is the whole design
argument: a shared read lowers to an ordinary `:frame-offset` `:buffer-load` with
a different `:buffer-id`, so it needed **no new IR op, no new addressing, and no
new emitter** — and the host gets per-frame *global* curves for free, because a
slice is a slice.

**Delivery**, per target, as D28's lanes already are:

| target | representation | delivery |
|---|---|---|
| interpreter | a real array in the context | by reference |
| JVM | one more array argument, last after `lanes` | by reference |
| JavaScript | one more trailing positional argument | by reference |
| Wasm | a region of its own linear memory | its **address**, since an instance owns its memory |

A shared region is *mandatory* when the process declares a shared control (unlike
a `:has-lane` lane): it is the control's value, not an optional layer over it, so
a missing region is refused rather than read as zeros.

**Slices.**

- **M5a — language, ABI, and the JVM/interpreter raw path. Done.** `:shared true`
  and its validation, `shared-binding` in the IR, `:shared`/`:shared-stride` in the
  ABI, the four emitters, `process!`'s `:shared` context (a bare number is
  broadcast across the slice, a sequence is a per-frame global curve), and the
  JVM's optional `shared` raw argument.
- **M5b — host delivery. Done.** Every kernel owns one region: the interpreter
  and the JVM pass it by reference (`Kernel/write-shared!` fills it once),
  JavaScript takes a trailing argument, and Wasm passes its address and copies the
  pool's region into each instance's own memory before running it — the frames
  about to be rendered, which is all the module can read.
- **M5c — the pool. Done.** `binding-plan` binds the ABI's shared controls too:
  the parameter of a shared control is shared whether the description said so or
  not (an undeclared one is inferred), and a `:per-voice` declaration beside it is
  refused. The runtime config carries the region's layout and the
  parameter-to-control map; the pool seeds the declared defaults into the region
  at construction and `set-parameter!` writes it **once** — no voice is touched.
  A shared parameter bound to an *ordinary* control still fans out, which is the
  migration path. Browser checks pin literal numbers on both hosts, as the lane
  checks do.

**Interim mechanism, still in place:** a `:shared` parameter bound to an ordinary
control keeps the per-voice fan-out. That is the migration path — declaring the
DSP control `:shared` is what switches a parameter to the shared region.

## Matrix evaluation model (decided)

Routes form a dependency graph over parameters, so a target must be computed
from its sources' *modulated* values. The model is one graph with per-edge
scheduling, not two kinds of matrix:

- **Instantaneous by default (zero latency).** An edge reads its source's value
  for the current evaluation step. This matches how a parameter change is
  expected to be heard, and keeps the common graph stateless.
- **`:delay` (default 0)** reads the source's value from N steps earlier. This
  is the only way a cycle is well defined: a cycle must contain at least one
  delayed edge. `:delay` counts evaluation steps — one audio frame for the
  kernel reducer generator, one reducer invocation (the sequencer's control
  step) for the instrument reducer generator.
- **The instantaneous subgraph must be acyclic.** The generator computes a
  topological order over the `:delay 0` edges and lowers them in that order; a
  cycle with no delayed edge is a compile-time error naming the cycle. The
  order is resolved once at generation time, never per frame.
- **One generator per axis**, not per topology. The kernel generator emits
  instantaneous edges as straight-line arithmetic in topological order, and a
  delayed edge as a state slot read at the top of the frame and written at the
  end (D22 gives the ordering). The host generator keeps the previous step's
  values. Named `instant`/`feedback` wrappers are deferred; the acyclicity
  check already gives a loud error when a graph cannot be scheduled.
- **Depth is a literal by default.** A route may name a parameter as its depth
  (read per step and multiplied) for second-order modulation; the default is a
  compile-time constant folded into the generated arithmetic.

## Remaining open

- **Where the generators run.** The instrument reducer generator must exist in
  ClojureScript if a browser worker generates its own reducer (the generator is
  `.cljc`); the kernel reducer generator output is a definition/IR that can
  still be compiled on the JVM and shipped as an artifact, so it does not
  depend on browser-side compilation (P13) yet.

## The parameter layer (M2, proposed)

The concrete schema M2 implements. It replaces the provenance vocabulary
(`control-sources`) with declared parameters and bindings.

### Declaration

In the instrument description:

```clojure
{:parameters
 {:cutoff     {:default 800.0 :min 20.0 :max 8000.0 :unit :hz :curve :log}
  :lfo-shape  {:enum #{0 1 2 3} :default 1}
  :mix        {:default 0.5 :min 0.0 :max 1.0 :step 0.01}
  :chorus-mix {:scope :shared :default 0.35}}
 :voice
 {:definition #'reference-voice}}
```

- `:scope` is `:per-voice` (default) or `:shared` — one storage cell per voice,
  or one for the whole instrument. A `:shared` parameter may be read by voices
  and/or the post-mix bus; the old `:instrument`/`:shared` split was only *who
  reads it*, which is the binding's business, not the declaration's. (Deferred:
  `:per-channel`, for `bend`.)
- `:lane` (optional) is **orthogonal to `:scope`**: it says the parameter also
  carries a per-frame buffer, in addition to its scalar. `:per-voice` + `:lane` is
  a per-voice scalar and a per-voice lane; `:shared` + `:lane` is a shared scalar
  and a shared lane. The kernel's lane control then holds *two* values — the host
  scalar and the lane sample — so the client can compose them (for example, add
  the scalar to every sample of the lane, the DAW-knob-plus-automation case).
  Delivery is M4; M2 reserves the syntax.
- `:default`, `:min`/`:max` (inclusive), `:enum`, `:step`, `:unit`, and `:curve`
  describe the value and its mapping. `:min`/`:max`/`:enum` are validated when a
  value is set; `:step`/`:unit`/`:curve` are declaration metadata the UI reads
  and the DSP path does not.
- `:parameters` is **optional**: a voice control with no declaration becomes a
  same-named `:per-voice` parameter whose default is the control's own declared
  `:default` (0.0 when it has none). Declare a parameter to give it `:shared`
  scope, a domain, or UI metadata, or to add one the voice does not bind (a bus
  or matrix parameter).

### Runtime parameters

Seven names are the runtime's own (D35): `gate`, `note`, `velocity`, `bend`,
`detune`, `pan`, `frequency`. They may be used without declaration; their storage
is the pool's existing per-voice (or per-channel, for `bend`) arrays, and the
note handlers and `set-pitch-bend!`/`set-detune!`/`set-pan!` write them. They can
be declared too, to attach a domain or default. Every other name is a user
parameter with fresh storage.

### Bindings

`:voice :bindings` is **optional**. By default a control binds to the parameter of
the same name — the voice already declares its controls in its `dsp/defn`, so
name-matching is exactly "the parameters the DSP code touches". An explicit entry
overrides that (a rename, or one parameter driving several controls):

```clojure
:bindings {:filter-cutoff :cutoff    ; control <- parameter
           :mod-mix [:a :b :c]}      ; one parameter, three controls
```

A parameter the voice does not use is legal: it may drive the bus or the matrix.
A control with neither a same-named parameter nor a binding is a loud error.

### API

- `(set-parameter! instrument name value)` — a `:shared` parameter.
- `(set-voice-parameter! instrument index name value)` — a `:per-voice`
  parameter.
- `(parameter-value instrument name)` / `(voice-parameter-value instrument index
  name)`.
- `set-pitch-bend!`/`set-detune!`/`set-pan!` remain, as writes to their runtime
  parameters.
- `:parameter` events address `:shared` parameters by name.

A wrong scope for the call, an unknown name, or an out-of-domain value is a loud
error on the control thread.

### Triggers and handlers

A trigger is an event type; a **handler** is `(fn [definition state event]
state')`, where `state` is the **sequencer state** and the return value is the new
state. Handlers run in the sequencer, off the audio thread, in ingress order.

- The definition and the state are separate. The state is the whole sequencer's,
  so a handler can touch any pool, parameter, or voice assignment — not only the
  instrument the trigger named.
- Built-in handlers: `:note-on` allocates a voice and sets `gate`, `note`,
  `velocity` (and the legacy `frequency`); `:note-off` releases it.
  Instrument-defined triggers are declared by name and bound to handlers on the
  host. Handlers are functions, not serialised data, matching the instrument
  reducer generator's host-side function.
- A dispatcher maps trigger to handler; the sequencer routes every event through
  it, so note handling is no longer hard-coded.
- The live pools and the parameter store are mutable handles *referenced from*
  the state: the audio path needs preallocated arrays, so a handler calls them
  for effect while the state map carries the durable bookkeeping. A strictly pure
  state→state model would mean reworking the pool away from its preallocated
  arrays, which is not worth it.

### What retires

| old | new |
|---|---|
| `synth/control-sources`, `normalize-control-source` | parameter declarations |
| `{:source :gate}` / `{:constant 800.0}` | control → parameter binding |
| runtime config `:control-plan` `{:source … :constant …}` | `{:parameter … :storage …}` |
| pool `plan-source-codes`, the source switch in `resolve-controls!` | parameter storage resolution |

### Decided

- **Value domain.** Physical: `cutoff` *is* 800.0 Hz, `:curve` is only the UI
  mapping, and a cross-domain binding transform is M2's scale/offset and M3's
  generator. Values stay meaningful in `:parameter` events, matrix depths stay
  physical, and the DSP path pays no per-frame `pow` unless the voice needs one.
- **Domain violations.** Reject on the control thread.

### Open in this proposal

- **`bend`'s channel dimension.** Proposed: declare it `:scope :shared`; the pool
  resolves it from the sounding voice's channel. A true per-channel scope is
  deferred with the shared region (M5).

## Generators (M3, proposed)

The matrix is data; a **generator** turns it into executable logic on one of two
axes. Both share one route schema and one validation and scheduling pass.

### Route schema

```clojure
{:from  :velocity     ; a source name
 :to    :cutoff       ; a target name
 :depth 1800.0        ; a number, or a source name for second-order modulation
 :delay 0}            ; evaluation steps; the default is 0 (instantaneous)
```

A matrix is a flat vector of routes, or the grouped form
`[{:source :velocity :routes [{:target :cutoff :depth 1800.0} …]} …]`, which
`normalize-matrix` expands. `:from`/`:source` and `:to`/`:target` are aliases.

### Scheduling (shared)

`matrix-plan` builds the dependency graph over the targets: a route
`{:from S :to T :delay 0}` adds an edge `S → T` when `S` is itself a target. It
topologically sorts that `:delay 0` subgraph — the order every generator emits
its arithmetic in — and refuses a cycle with no delayed edge, naming it. A
`:delay n>0` route reads its source from `n` steps earlier, so it is *not* an
edge in that graph, which is what makes a feedback loop well defined.

Names are validated against the instrument's declared names: an unknown
`:from`/`:to`, a `:delay` that is not a non-negative integer, or a symbolic
`:depth` that is not a name is a loud error.

### The two generators

- **Instrument reducer generator** — a Clojure/ClojureScript function used in the
  sequencer's event-processing path. It generalizes `omkamra.dsp.reference`: the
  plan is resolved once and the returned closure walks it over the host's
  primitive arrays (`base + Σ depth × source`, in topological order), keeping the
  previous step's values for `:delay`.
- **Kernel reducer generator** — a new kernel description for the assembler,
  built over `graph/derived-definition`: it emits the same arithmetic as
  straight-line DSP, with a `:delay` edge as a chain of scalar state slots
  shifted once per frame (D22).

### Decided

- **The route domain is the control.** A kernel can only read a local, so a
  route's `:from` and `:to` name controls — which, with automatic binding, are
  usually the parameter names too. The kernel reducer **adds a control** for
  every *source* the voice does not already expose, and the description binds
  it: a `:shared` parameter for a global source, the runtime source for
  `velocity`/`note`. A *target*, by contrast, **must already be a control of the
  voice**: a name the voice never reads cannot be modulated, and a typo must not
  silently add a dead control. The instrument generator requires the same thing
  — it reads and writes the pool's resolved controls, and an unknown name is an
  error — so the two generators agree name for name. The reference voice
  therefore carries the controls the matrix needs (`velocity`, and its
  controller sources as `:shared` parameters).
- **The kernel reducer does not rebind a target.** The language refuses to let a
  local shadow an in-scope binding, so the reducer computes each target into a
  fresh `-mod` local in topological order, reading an earlier target's local so
  a chain is complete, and calls the voice with each target control replaced by
  its `-mod` local. No body rewriting is needed.
- **A `:delay n` edge is a shift register.** The reducer declares one scalar
  state slot per (source, level) and shifts it once per frame: the highest slot
  takes its predecessor, the first takes the source's effective value, so slot
  `n` holds the value `n` frames ago. The unit differs from the instrument
  generator's (frames vs control steps) by design, so the two are compared on
  the instantaneous subgraph, not on a delayed route.

## Lanes through the pool (M4, proposed)

Lanes already work *below* the host. D28 ships `:lane` controls on all four
targets: the ABI lists them under `:lanes` rather than `:controls`, a lane read is
an ordinary `:buffer-load` from one flat region at a compile-time offset
(`lane-frame-capacity` frames per lane), and the generated process takes that
region as its **last positional argument** — `output, frames, sample-rate,
controls…, lanes?` — so any number of lanes costs one argument. `process!`
already accepts `:lanes {:amount values}` and flattens it into that region.

What is missing is the **host seam**, and that is M4:

- `synth/pool`'s `Kernel` protocol carries no lane region — `mix-voice!` passes
  the scalar controls only, and every one of the four kernels passes `nil`;
- the pool has no lane storage and no way to address a lane by name;
- `runtime-config` never carries the lane names, so a host rebuilt from a served
  bundle could not find them.

### The region and where it lives

The layout rule is the IR's: lane *i* occupies `[i * frame-capacity, …)`, and in a
pool the frame capacity is `max-block-frames`, so the pool can derive every lane's
offset from its ABI index. Where the region *lives* differs by target, though:

| target | region | a shared lane costs |
|---|---|---|
| interpreter | a host array passed per call | one array, no copy |
| JVM | a host array passed per call | one array, no copy |
| JavaScript | a JS array passed per call | one array, no copy |
| Wasm | inside the instance's linear memory | a copy into every instance |

So on three targets a shared lane is free and on Wasm it is an N-way copy, and a
*per-voice* lane needs a region per voice everywhere. That asymmetry is what M5
and the D45 narrowing are about; M4 should not pretend it away.

### Decided: a lane belongs to a control, and is read through an accessor

A `:lane` control *leaves* the ABI's scalar control list, so `binding-plan` —
which iterates `:controls` — never sees it; the lane is a separate thing the
parameter layer has to learn about. Instead, a lane is declared **on a control**
and the client reads it through a compile-time accessor:

- `{:name cutoff :has-lane true}` keeps `cutoff` an ordinary scalar control (so
  `:scope`, `:curve`, `:default`, the pool's binding, and the plan all work
  unchanged) and adds a lane beside it in the ABI, marked `:required? false`;
- the DSP reads it with **`(lane cutoff)`**, which resolves to an ordinary
  `:buffer-load` from the flat `:lanes` region, so D28's "no new IR op" still
  holds, and **composes it itself** — nothing is hard-wired: addition,
  replacement, and multiplication are all just code;
- the accessor is refused where there is no region to read: in a callee module (a
  caller composes at its own call site, `(voice (+ cutoff (lane cutoff)))`), or
  for a control that carries no lane. `:lane` and `:has-lane` together are also
  refused;
- `:lane` keeps its meaning (lane-only, mandatory region). A process whose lanes
  are all `:has-lane` is valid with **no region at all**: it is built zeroed, so
  an untouched lane reads zero and the scalar stands.

Because a lane's identity *is* the control's, the two questions that used to
block — where a lane binds, and what scope it has — answer themselves, and no
orphan lane can exist.

**Accessors, not names.** The wider rule: when the compiler has to expose a value
it derived (a lane, and later others), it does so through a compile-time
accessor, and the client never spells an internal name.

### The DAW case: automation is a host action

Consider a DAW where the player turns the `cutoff` knob and then draws an
automation lane for it. Nothing in the DSP changes:

- the compiled ABI already **advertises** the lane (`:lanes`), so the DAW
  enumerates the automatable parameters from the artifact — the way a plugin
  publishes its parameter list;
- the knob keeps writing the scalar control;
- the moment a lane exists, the host starts writing that lane's slice of the
  region each block, off the audio thread.

So *automatable* is an **interface decision taken at compile time** (the author
marks the control `:has-lane`), while *having* a lane is a **run-time decision**.
Automation-off is then free: the region is left untouched at zero and the scalar
stands. A parameter that was never declared automatable cannot gain a lane
without a recompile — the language has no dynamic ABI — which is the same trade a
plugin makes when it fixes its parameter list up front.

The two shapes are both just the *expression the client composes*, so neither
needs new mechanism:

- **the lane offsets the control** — `(+ cutoff (lane cutoff))`. Recommended as
  the default: "no automation" costs nothing and the scalar keeps its meaning.
- **the lane replaces the control** — `(lane cutoff)`. Here the kernel cannot
  tell "automation off" from "automation at zero", so the host must keep the
  lane filled at all times (the knob folded into it) and the scalar is dead
  weight. That is today's `:lane` with the scalar retained for the host's
  convenience.

**The helper belongs to authoring, not to the host.** A host rebuilt from a
served bundle has the runtime config and the artifact, not the definition value,
so it cannot rewrite a DSP body. The mechanical plumbing — mark a control,
rewrite its uses to the composed expression — is a *combinator over a definition
value*:

```clojure
(comb/automate voice :cutoff)             ; the default: value + lane
(comb/automate voice :cutoff :replace)    ; the lane is the value
(comb/automate voice {:cutoff :offset :resonance :offset})
```

Arbitrary expressions stay where they read best — in DSP source, with
`(lane cutoff)`, which the combinator need not know about. One detail to design
around: a plain symbol replacement would rewrite the control *inside* an existing
`(lane cutoff)`, so the walk must skip `lane`'s argument position.

### Decided: the kernel owns the region, the pool owns the name

The region lives where the target can read it, so the **kernel** allocates it, at
that kernel's precision, `lane-stride` reals per lane. The protocol grows one
method, **`write-voice-lane!`**, which copies a lane's values into *that voice's*
region; the pool's `set-lane!` and `set-voice-lane!` delegate to it, so the name
is validated against the ABI by the one component that has it. `mix-voice!` keeps
its arity and reads the voice's own region — a host-array target passes it
straight through, and Wasm passes its *address*, because a Wasm instance owns its
memory.

M4a first made the region one-per-kernel ("a lane belongs to the process"). **M4c
splits it per voice**, because that is what per-voice lanes need and it costs the
shared case only copies:

| lane's control | scope | who writes | who reads |
|---|---|---|---|
| `:shared` parameter | `:shared` | `set-lane!`, into every voice's region | every voice, same curve |
| `:per-voice` parameter | `:per-voice` | `set-voice-lane!`, one voice | that voice |
| runtime control (`frequency`, `detune`, …) | `:per-voice` | `set-voice-lane!` | that voice |
| `:bend` (per channel) | — | refused at construction | — |
| lane-only (`:lane`), declared `:shared` | `:shared` | `set-lane!` | every voice |
| lane-only, undeclared | `:per-voice` | `set-voice-lane!` | that voice |

A lane's scope is therefore its **control's** storage scope, and a lane-only
control — which leaves the ABI's scalar list and so has no plan entry — takes its
scope from its own parameter declaration, defaulting to per voice like every
other undeclared control.

The runtime config carries the layout (`:lanes`, `:lane-stride`) so a pool can
check a lane's name and scope without the ABI, and so a browser host rebuilt from
a served bundle has it.

**What the host writes.** Shapes 2 and 3 differ only in *who* interpolates — the
worklet, or the host rendering buffers — and both end at "this lane has `frames`
values for this block": `(set-lane! pool name values)` for a shared lane and
`(set-voice-lane! pool index name values)` for a per-voice one.

### Slices

- **M4a — one shared region, JVM and interpreter. Done.** `write-lane!` on the
  protocol, `set-lane!` on the pool, the JVM and interpreter kernels, and the
  layout in the runtime config. (M4c replaced the single shared region with one
  region per voice and the shared-lane *refusal* at construction with the scope
  rule.)
- **M4b — the browser. Done.** The JavaScript kernel hands the region to the
  generated process as its last positional argument; the Wasm kernel hands it the
  region's *address*, because a Wasm instance owns its memory. Both are checked in
  the browser against the interpreter pool. A post-mix *bus* that declares a lane
  is still refused — once, at construction, not in the audio path — until it gets
  the same delivery.
- **M4c — per-voice lanes. Done.** One lane region per voice, `write-voice-lane!`
  on the protocol, `set-voice-lane!` on the pool, and the scope rule above. The
  scope comes from the control's storage, so a `:per-voice` parameter — and a
  runtime control, which the pool already resolves per voice — needs no new
  declaration. The JavaScript host hands each kernel its own region; the Wasm host
  writes into each instance's own memory and passes that region's *address* as the
  process's trailing argument, which is the one ABI detail M4b had missed: the
  direct process path passed the pointer, the pool did not, so pool lanes on Wasm
  read zeros. Both browser checks now pin literal numbers rather than only
  comparing host against oracle, so they cannot pass by comparing two blocks of
  nothing (see `TESTING.md`).

The instrument host and the sequencer then expose the pool's lane API to the
worker, which is where shapes 2 and 3 finally differ.

## Relationship to the current code

| concept | current artifact | direction |
|---|---|---|
| control | a `dsp/defn` parameter / ABI control | unchanged |
| parameter | `synth/control-sources`, `:constant` controls, bus controls | new parameter layer; provenance retires |
| binding | `synth`'s control map / `runtime-config` control plan | generalize; add scope and addressing |
| event / trigger | `omkamra.dsp.event`, `omkamra.dsp.schedule` | add the general trigger and handlers; note as a built-in trigger |
| instrument value | `omkamra.dsp.instrument` (`:voice`, `:effects`, `:routes`) | drop `:routes`; add `:parameters` and `:bindings` |
| host matrix | `omkamra.dsp.reference` (hand-written) | becomes the oracle; generalize into the instrument reducer generator |
| kernel transform | `graph/derived-definition` / combinators | generalize into the kernel reducer generator |
| process lanes | ABI `:lanes` (D28) | add pool/kernel lane delivery |
| sequencer | browser hosts run in the page today | move to a Web Worker; worklet keeps the queue/scheduler |

## Proposed slices

1. **Transport first.** Worker emits timestamped events; the worklet's queue
   and `schedule` dispatch them to the existing pool/kernel sample-accurately.
   No new DSP or ABI. Testable offline (feed the queue directly) and in the
   browser; validates the decoupling.
2. **Uniform parameters.** Parameter declarations with scope/addressing,
   bindings, and the instrument API; triggers and handlers, with note-on/off as
   the built-in case. Retire provenance from `synth`/pool.
3. **Generators.** The instrument reducer generator (generalizing
   `reference.cljc`) and the kernel reducer generator (a combinator over
   `graph/derived-definition`). Prove them against each other and against the
   hand-written matrix.
4. **Lanes through the pool.** Deliver per-sample control values to voices,
   enabling shapes 2 and 3.
5. **Shared kernel region.** Realize broadcast as one state variable (deferred
   item above).
