# VICE Capture and Analysis Pipeline v2

## Status

- **Design status:** Initial design complete.
- **Implementation status:** **Complete.** Phases 0–9 are implemented. The full
  pipeline runs end to end — capture → `:dedupe` → `:video`/`:assets`/`:audio`
  → `:features` → `:classification` → `:segments` → `:disassembly` — with an
  OS-visible writer lock, transactional all-or-nothing commits, signature and
  input-provenance reuse, and parallel independent branches. `:dedupe` owns
  explicit execution frames and auditable timeline leveling; `:features` streams
  a sparse evidence projection plus a binary occurrence channel;
  `:classification` carries observed context identity; `:segments` derives
  demoparts from the *display-handler set*; `:disassembly` renders annotated
  assembly. v1 orchestration is retired and the `omkamra.vice.v2.*` prefix is
  gone, so the public API is `omkamra.vice.*`. Tests are capture-free
  (a synthetic dataset), and the whole tree is `clj-kondo`/`cljfmt`/
  reflection clean.
- **Current phase:** none — pipeline-v2 is delivered. The structural
  reconstruction is validated on two captures: `no-booze-triad-v2`
  reconstructs as exactly **three file groups and ten demoparts** (3 / 3 / 4)
  matching the known part list, and the single-part `fairlight-intro`
  reconstructs as one init transition plus one demopart — both with full
  coverage. On the current stage versions (`dedupe-v4`/`features-v7`/
  `classification-v7`/`segments-v7`/`disassembly-v1`) the Triad dataset is
  3 file groups, 10 demoparts (3 / 3 / 4), 0 gaps, 0 overlaps, and every stage
  reuses by signature. **Possible future work is listed below; none of it is
  required for the plan to be considered complete.**
- **Compatibility:** v2 is a clean replacement for the current capture and analysis APIs. Backwards compatibility with the v1 API is not a requirement.

### Possible future work

Pipeline-v2 is complete. This section records **optional** future work and a
short reference of what is already implemented; none of it is required for the
plan to be considered done.

**Implemented (reference)**

- `:features` streams `units.edn`/`handoffs.edn` through a monotonic occurrence
  attribution reducer, and the heavy per-occurrence evidence of an IRQ
  invocation is written to a binary `occurrences.bin` channel (delta +
  evidence-dictionary encoded) referenced from the unit line by offset and
  length; `:classification` consumes them in a single streaming fold. On the
  full capture this cut the features footprint from ~830 MB to ~70 MB
  (`units.edn` 32 MB + `occurrences.bin` 37 MB) and classification from ~104 s
  to ~24 s.
- `store/delete-tree!` is symlink-safe (`store_test`).
- `:dedupe` models **contexts**, **frames**, and **levels** explicitly;
  interrupt handlers resolve through the tracked user/physical vectors;
  long-lived frames are *annotated* (never mutated) as `:mainline` from
  `:level-start`.
- Stage formats derive from schema versions: `dedupe-v4`, `features-v7`,
  `classification-v7`, `segments-v7`, `disassembly-v1`.
- Demoparts come from a stable *display-handler set*; `:loader`/`:decruncher`
  are evidence *roles*; boundaries are refined to the first invocation of the
  new handler set and snapped to the preceding write-to-execute handoff.
- `:disassembly` renders each semantic segment and role into annotated `.asm`
  files (each block once per file; volatile operands as `??`).
- `omkamra.vice.pipeline` is complete: dependency-closure discovery, normalized
  configuration, signature reuse, **multiple targets / `:all`**, **parallel
  independent branches** (default = available processors), **transactional
  all-or-nothing commit**, and **input-provenance-checked reuse** (a
  same-signature defective rebuild invalidates its dependents). `adopt!`
  back-fills signatures and provenance for pre-existing generations.
- The analysis dataset has an **OS-visible writer lock** (`analysis/.lock`).
- Tests are **capture-free**: a synthetic 6,602-event dataset builds the whole
  chain in an isolated temp root; no test or source references a capture path.
- v1 orchestration is retired and the `omkamra.vice.v2.*` prefix is gone. The
  public analysis API is
  `omkamra.vice.{capture,reader,pipeline,dedupe,video,assets,audio,features,classification,segments,disassembly}`,
  with `omkamra.vice.{binary-monitor,trace}` retained as VICE adapters.
- `:dedupe` is ~8x faster after the reflection, primitive-decode, and
  mutable-state work (full capture ~3.4 min, ~231 k events/s, byte-identical
  output).
- `clj-kondo`, `cljfmt`, and `*warn-on-reflection*` are clean for the whole
  `src/omkamra/vice` tree.

**Future work**

- **`:dedupe` core scaling.** `:dedupe` is still a single sequential pass
  (~3.4 min full capture). A per-segment map-reduce — a map phase of
  order-independent summaries per raw chunk, seeded from each chunk's exact
  boundary snapshot, and a sequential reduce that assigns global ids, stitches
  blocks across boundaries, orders occurrences, and accumulates edges — is the
  only path that scales with cores. Caveat found in the profiling pass: the
  online `:known-targets` split rule is order-dependent, so a map phase must
  preserve observation order (or the block semantics must change deliberately).
- **`:video` scan parallelism.** `:video` still scans VIC writes in one pass;
  split it by raw chunk with boundary-frame merging.
- **`:assets` snapshot coalescing.** Snapshotting every effective layout change
  still produces many snapshots (205 for the 450 k prefix, only 4 distinct
  layouts). Replay is now shared per raw chunk and streamed, so the prefix build
  fell from ~45 s to ~4–5 s with byte-identical `assets.bin`; a versioned
  coalescing policy could reduce the snapshot count itself.
- **`:dedupe` remaining micro-optimizations.** The fold's frame/open and
  CFG-edge handling is still persistent-map heavy; a mutable current frame with
  a growable instruction-id buffer and mutable per-context edge maps would give
  roughly another 1.3–1.5x. Optional; the ceiling is Amdahl-bound and the
  change is invasive.
- **Freshness: rebuild `:video`/`:assets` on the current capture.** No
  `:video`/`:assets` generations are committed for the fresh
  `no-booze-triad-v2` capture; a full `pipeline/build! … :all` would build them.
- **`:full-capture? true` boot path.** Under monitor-log tracing the
  `:full-capture? true` mode was observed parked in the KERNAL IEC wait loop
  rather than reaching the demo; diagnose or document as unsupported.
- **Raw-capture reproducibility.** Record a capture-manifest checksum plus a
  documented `capture/start!` recipe in a dataset README, and ideally commit a
  small demo so acceptance runs are self-contained.
- **NMI vs IRQ.** Every hardware interrupt is registered as `:irq` keyed by its
  resolved handler PC; exact `:nmi` kind resolution is deferred.
- **CPU-port timeline.** Only direct `STA/STX/STY $01` writes feed the port
  timeline; a write through a zero-page indirect pointer is not tracked.
- **Piecewise leveling.** Leveling is decided once per frame; a handler that
  first returns quickly and later becomes persistent could be annotated per
  segment. A first-class promoted-continuation unit / explicit timeline marks
  were also proposed.
- **`:audio` candidate shape.** Candidates are heterogeneous
  (`{:kind :block …}` / `{:kind :context …}`), which once caused a
  nil-matching bug; a single uniform shape would make consumers safer.
- **Stale-generation pruning.** Every rebuild leaves a new generation below
  `analysis/<stage>/`; there is no automatic pruning of superseded generations.
- **Capture stub configuration.** The BASIC stub address (`$0801`) and the
  `SYS` pattern are fixed; making them configurable and following a relocated
  BASIC program (`:basic-start`) would broaden the program-only start policy.
- **`omkamra.decode64`.** This cljfx/JavaFX desktop UI fails to load on the
  current JDK with a JavaFX SVG-loader `IncompatibleClassChangeError`
  (`ImageStorage.addImageLoaderFactory`) — a pre-existing version mismatch,
  unrelated to the pipeline.

### Handoff notes (current implementation state)

- **Namespaces** (`src/omkamra/vice/v2/`): `contract`, `event`, `store`,
  `snapshot`, `capture`, `recorder`, `feasibility`, `isa`, `reader`, `analysis`,
  `parallel`, `dedupe`, `video`, `assets`, `audio`, `features`,
  `classification`, `segments`. Tests: `contract_test`, `store_test`,
  `capture_test`, `snapshot_test`, `reader_test`, `dedupe_test`, `domains_test`,
  `features_classification_test`, `segments_test`, `parallel_test`, with shared
  fixture support in `acceptance_test_support`.
- **Per-stage API**: every stage exposes `build!` (synchronous) and most an
  `open`/`status` reader over `analysis/manifest.edn`, plus `build-async!` for
  the long ones (`dedupe`, `features`). `analysis/commit-stage!` is the only
  manifest writer; `analysis/start-async!`/`report!`/`cancel-async!` manage
  deref-able progress handles; `analysis/summarize-result` keeps a handle's
  `:result` light by dropping heavy payloads.
- **On-disk generations**: `analysis/<stage>/<generation>/`, pointed at by
  `analysis/manifest.edn`. `:dedupe` uses `index.edn` + binary dictionaries +
  `image.bin`/`windows.idx`; `:features` uses `index.edn` +
  `units.edn`/`handoffs.edn` (newline-delimited EDN records); other stages use
  `index.edn` (and `assets.bin`, `boundary-audit.edn` where relevant).
- **Acceptance dataset (`captures/no-booze-triad-v2`, gitignored): fresh**
  **capture, full chain rebuilt.** Recaptured from the host demo
  `/c64/Triad/r676_No Booze__No_Booze_Triad.d64` (a read-only mount) with
  `capture/start!` in the default program-only mode: **47,763,778 events /
  463 chunks** (~32 min at ~24.5 k events/s). This dataset predates the
  SYS-target start policy below and therefore begins at the BASIC RUN entry
  `$E144`; a recapture would begin at `$0816` (2070), the stub's
  `10 SYS 2070 [TRIAD]` target. Sanity scan: 5,149,643 VIC stores,
  140,400 SID stores, 1,343,467 `$dd00` reads. **Current:** rebuilt on
  `dedupe-v4`/`features-v6`/`classification-v7`/`segments-v7`
  (`pipeline/build!` with `:force`, ~41 min). `:dedupe` resolves **19
  contexts** (the KERNAL `$FF48`/`$EA31` context is no longer created);
  `:audio` reports 132,208 SID writes and 61 candidates; `:features` has
  36,619 units (210 mainline windows, 36,409 IRQ invocations, 2,033 handoffs);
  `:classification` has 35,729 spans; `:segments` yields **17 segments = 10
  demoparts (3 / 3 / 4), 7 transitions, 3 file groups, 0 gaps, 0 overlaps**.
  A second `pipeline/build!` reuses all five stages in ~7 s. Older analysis
  generations were pruned; only the five committed generations remain.
- **Secondary dataset (`captures/fairlight-intro`, gitignored): current.** A
  single-file intro (1,542,592 events / 16 chunks) that runs entirely in one
  raster IRQ handler; used to validate the demopart model, system contexts,
  invocation-level IRQ roles, and the simplification pass. Its chain is built on
  the current versions:
  `:transition [0,47537)` roles `#{:decruncher}` (self-relocating init) and
  `:demopart [47537,1542592)` handler `{2}` roles `#{:music :raster}` — 1 file
  group, 0 gaps, 0 overlaps.
- **Demopart ground truth (for validation).** File 1 (3 parts): TRIAD logo +
  scroller intro (mainline); sprite DYSP + logo; purplish raster splits +
  stretching sprite scroller. File 2 (3 parts): 1×1 scroller + sinewave raster
  splits + sprite scroller; TRIAD tech-tech + hammer-and-sickle (longest, with
  a half-dead transition tail); two 2×2 scrollers + logo + sample-triggered
  sprites. File 3 (4 parts): top/bottom rasterbars + logo + scroller; two DYCPs
  + sprite balls + level meters; greetings; four Y-stretched sprites + "BOB" +
  "TRIAD!!".
- **Immediate follow-up:** start Phase 8 (`:disassembly`) from `:dedupe` +
  `:segments`; the structural reconstruction now matches the known part list.
  Secondary: `:video`/`:assets` are stale (built against the previous capture),
  and the audit's `:unaligned-hard-handoff-count` is no longer meaningful now
  that parts are handler-defined rather than handoff-defined.
- **Loader evidence**: the fast loader is visible as `$dd00` serial traffic.
  The fresh capture has 1,343,467 `$dd00` reads and 24,860 writes; `:features`
  records serial reads and `:classification` treats serial access as `:loader`,
  which the `:segments` handler-based partition now keeps as the two
  file-ending loader regions.
- **Conventions**: phase numbers live only in this document, never in code
  identifiers or docstrings; progress handles must not retain heavy objects;
  code must not hardcode the acceptance-capture path (strip those references
  before the project is considered finished).
- **Tests**: `clojure.test` through the project nREPL; acceptance tests are
  guarded by the presence of the acceptance capture and build prefix
  generations, so they skip cleanly without it.

This document is both the implementation plan and the implementation diary. The implementing agent must update the **Status diary** after every phase, whether the phase succeeds or fails.

---

## 1. Goals

Pipeline v2 should provide:

1. A strict separation between capture and analysis.
2. An immutable, self-contained raw capture which can be analyzed without VICE.
3. Fast continuous capture from VICE's trace FIFO.
4. Exact, synchronized C64 snapshots at raw-chunk boundaries.
5. A compact structural execution image for most downstream analysis.
6. Exact raw-capture APIs for stages that need historical memory or device-write values.
7. Sequential and random-access analysis APIs without exposing raw chunk boundaries.
8. Streaming, transducer-oriented stage pipelines with bounded memory and backpressure.
9. Transactional stage publication and safe stage reuse through versions and dependency signatures.
10. Assembly output which explains execution without reproducing an enormous instruction trace.

## 2. Non-goals

- Exact replay of the entire C64 execution from the deduplicated image.
- Retaining every historical operand value in the deduplicated instruction dictionary.
- Retaining every exact write in `:dedupe`.
- Preserving the current v1 public API.
- Driving VICE one instruction at a time during normal capture.
- Encoding the raw VICE textual trace in the capture.
- Putting semantic analysis results into the immutable capture.

The raw capture remains the authoritative source for exact trace events and for writes inferred from those events. The deduplicated image is a structural analysis representation optimized for producing understandable assembly and supporting downstream stages.

### 2.1 Supported replay envelope

Within v2, "exact" means exact for the supported stock-C64 model and the information observable through the unmodified VICE monitor and trace stream:

- CPU instruction execution and CPU-generated logical memory effects, including implicit stack/control-flow writes;
- underlying RAM, CPU-visible memory, color RAM, CPU registers, canonical device-register images, and trace timing;
- VIC/SID/CIA register effects which can be obtained from the boundary snapshot or inferred from traced CPU writes.

It does not imply serialization of opaque VICE internals such as SID oscillator phase, CIA internal divider phase, disk-drive CPU state, cartridge/REU DMA state, or other state which VICE does not expose. Machine configurations which can mutate C64 RAM without a traced 6510 instruction are unsupported unless a later adapter captures those mutations explicitly. Unsupported configurations must be rejected or fail closed, not silently produce an "exact" capture.

### 2.2 Mandatory VICE feasibility gate

Before implementing the full rewrite, Phase 0 must prove the following against the supported unmodified VICE version:

1. disabling monitor logging creates an observable end-of-stream/flush barrier, not merely a timing-based quiet period;
2. after that barrier, VICE is paused and no unassigned execution event can occur before the next snapshot;
3. the monitor can read underlying RAM independently of `$01` mapping and can separately read the CPU-visible 64 KiB image;
4. every mandatory canonical CPU/VIC/SID/CIA/port/color-RAM snapshot field can be obtained with documented semantics, including write-only or latched registers;
5. trace events have pre-instruction semantics for PC, instruction bytes, registers, and timing;
6. the textual flags field can be converted unambiguously to the native 6502 status byte;
7. the first event after a snapshot supplies the agreed boundary timestamp without an intervening emulated cycle.

If any point cannot be demonstrated, implementation stops and records the blocker in the diary. The agent must not weaken the invariant or substitute a best-effort snapshot without a new design decision.

---

## 3. Core invariants

### 3.1 Capture is immutable

Once a capture is finalized:

- files under `capture/` are never modified by analysis;
- `capture/manifest.edn` is the authoritative commit log;
- analysis never writes an analysis pointer back into the capture manifest;
- all analysis status lives under `analysis/`;
- a capture is usable only when its manifest says it is finalized and every committed artifact validates.

If capture fails, the entire capture directory is destroyed. Partially committed chunks are not exposed as a usable forensic capture.

### 3.2 Capture is fail-closed

Capture fails immediately when:

- a supported trace record cannot be parsed;
- an unknown or malformed trace form is encountered;
- a required snapshot section cannot be read completely;
- a snapshot is internally inconsistent;
- a binary artifact cannot be written and committed safely.

There is no silent event loss and no degraded partial snapshot mode.

### 3.3 Continuous capture is mandatory

Normal capture runs VICE continuously and consumes trace messages from the FIFO. Monitor-controlled instruction stepping is not used because it has unacceptable performance.

The configured event count is an **approximate chunk target**, not an exact chunk cardinality. The target is crossed while VICE is running; FIFO and monitor latency can add events before the synchronized boundary is established.

### 3.4 Snapshot boundary

A raw chunk snapshot is the C64 state immediately before the first trace event assigned to that chunk.

At a boundary, capture:

1. requests trace logging to stop;
2. waits for VICE to process the request and pause;
3. waits for a verified logging flush/end-of-stream barrier and drains every trace record emitted before it;
4. assigns all drained records to the chunk being closed;
5. reads the next snapshot while VICE remains paused;
6. establishes a fresh FIFO reader before logging is re-enabled, if disabling logging closed the writer;
7. resumes trace logging and execution.

A quiet-time heuristic is not a valid drain barrier. The exact stock-VICE mechanism—expected to be FIFO EOF after `MonitorLogEnabled` is disabled—must be established by the feasibility gate and covered by an integration test.

The paused snapshot has no independent raw frame counter. Its authoritative boundary timestamp is taken from the first trace event after execution resumes, whose timing tuple is `(global-cycle, raster-line, cpu-cycle)`. Chunk metadata therefore remains pending until that first event arrives. This relies on the verified pre-instruction trace semantics and on pausing not advancing emulated time. Every later event carries the same timing *fields* with its own values. Empty trailing chunks are not committed.

As a continuity check, replaying one completed chunk from its starting snapshot should produce the next chunk's underlying RAM and canonical mutable device state. CPU-visible `.mem` bytes are also compared wherever the required mapped source (RAM, I/O shadow, or available ROM bytes) is known within the explicitly supported replay envelope.

### 3.5 Analysis is immutable-input, transactional-output

Every analysis stage:

- reads the immutable capture and/or completed upstream stages;
- writes to a private, uniquely named generation directory;
- may feed downstream stages through private in-process streams before publication;
- publishes its output only after successful completion;
- discards uncommitted generations on failure;
- records its version, configuration, and dependency signatures in `analysis/manifest.edn`.

The analysis manifest is the commit point. It atomically switches stage pointers to completed generations only after the requested run has succeeded. Previously committed compatible generations remain usable throughout a failed or running replacement. A changed stage version invalidates all dependent stages. A changed configuration invalidates that stage and its dependents.

### 3.6 Capture orchestration remains a capture concern

The v2 capture API still owns VICE process/monitor lifecycle, PRG/D64 loading, configured start policy, FIFO setup, and final shutdown. A normal program-only capture may stop at the configured program-entry barrier before taking the initial snapshot; a full capture takes its initial snapshot before the configured boot/load operation. In both modes, logging starts only after the initial snapshot boundary is established. Unexpected VICE exit, reader failure, writer failure, or monitor failure aborts and removes the new capture.

**Program-only start policy.** The autostart path first stops at `$E144` (the BASIC RUN entry) through an execute checkpoint, then reads the loaded stub at `$0801`, walks its line links, and parses the operand of the first `SYS` token (`SYS 2070`, `SYS (2064)`, …). It resumes and stops again at that address, so the initial snapshot is taken immediately before the program's first machine instruction rather than inside the BASIC interpreter; the capture therefore begins exactly at the program's entry. When the loaded program has no BASIC `SYS` stub, the capture falls back to starting at `$E144`. Verified end-to-end on two demos with different stub styles: Triad's `10 SYS 2070 [TRIAD]` starts at `$0816` (2070) and Fairlight's `1998 SYS (2064)` starts at `$0810` (2064).

Stopping uses the same verified disable/drain barrier as an intermediate chunk boundary, commits the non-empty final chunk, and only then marks the capture manifest `:finalized`. No analysis code participates in these operations.

---

## 4. Dataset and capture layout

Public APIs receive a **dataset root** which owns sibling `capture/` and `analysis/` directories. The directory structure is a code-level convention and is not reconstructed from paths stored in chunk metadata. Capture reserves a new dataset/capture directory and refuses to overwrite an existing finalized capture. Failure cleanup may delete only the directory reserved by that capture session, never an arbitrary caller-supplied parent or an older capture.

```text
<dataset-root>/
  capture/
    manifest.edn
    chunks/
      chunk-000001.edn
      chunk-000001.ram
      chunk-000001.mem
      chunk-000001.bin
      chunk-000002.edn
      chunk-000002.ram
      chunk-000002.mem
      chunk-000002.bin
  analysis/
    manifest.edn
    ...
```

### 4.1 Top-level capture manifest

`capture/manifest.edn` contains:

- capture format and version;
- capture ID;
- `:capturing` or `:finalized` status;
- capture-level timing model needed to interpret raster and cycle values;
- capture options, including the fixed event target and VIC value blacklist;
- binary format descriptors for event, RAM, and memory files;
- canonical snapshot schema descriptors;
- chunk count and total event count;
- committed chunk entries in order;
- file sizes and SHA-256 checksums for every committed artifact.

A chunk commit is ordered: write payloads to temporary files, atomically install payloads, atomically install the chunk EDN, then atomically rewrite the top-level manifest as the final commit point. The manifest checksum used by analysis is computed over the finalized manifest; it is not stored recursively inside that same manifest.

The manifest also records the default VIC-value blacklist used by `:video`. This policy is fixed for the lifetime of the capture.

The manifest does not need to contain broad VICE provenance such as the full command line or every resource setting unless a future requirement makes it necessary. An unfinalized directory is never accepted for analysis. A caught failure removes it immediately; after an uncatchable process/host crash, the next capture/open operation detects and removes or quarantines it before starting from scratch.

### 4.2 Per-chunk EDN metadata

Each chunk EDN file contains:

- capture ID;
- chunk number;
- global half-open event range `[start end)`;
- actual event count;
- approximate target used to request the boundary;
- references by artifact kind (`:ram`, `:mem`, `:events`) rather than arbitrary filesystem paths;
- snapshot state;
- snapshot timing;
- schema/version identifiers;
- payload file sizes and checksums;
- any boundary/context metadata needed by the reader.

The chunk EDN does not contain a checksum of itself. Its size and checksum are owned by the top-level commit entry, avoiding a circular checksum definition.

The final event count is the actual count, not the configured target.

### 4.3 Raw RAM image: `.ram`

Every chunk has a full 64 KiB image of underlying C64 RAM, independent of the current `$01` CPU-port mapping. It includes RAM shadowed by BASIC/KERNAL ROM and RAM beneath the I/O area.

The file is exactly 65,536 bytes and uses the canonical address order `$0000` through `$FFFF`.

### 4.4 CPU-visible image: `.mem`

Every chunk has a full 64 KiB image of what the CPU currently sees at the snapshot boundary, including currently mapped ROM and I/O-visible values.

The file is exactly 65,536 bytes in CPU address order.

### 4.5 Event binary: `.bin`

The raw event file contains only parsed binary `EXEC` records. Capture configures an execute-only trace; an unexpected operation type, unpaired header/instruction line, malformed field, or extra unsupported trace form fails capture. No textual VICE trace is retained.

All v2 binary formats use little-endian integer encoding. The exact layout is recorded in the top-level capture metadata.

The initial packed fixed-width event record is **23 bytes** with no implicit padding:

```text
u16  pc
u8   instruction-length
u8[3] instruction-bytes
u8   a
u8   x
u8   y
u8   sp
u8   flags                 ; native 6502 status bitmask
u64  global-cycle
u16  raster-line
u16  cpu-cycle
```

The instruction length is separate from the fixed three-byte instruction field. Unused instruction-byte positions are zero-filled and ignored according to `instruction-length`. The record descriptor must define offsets, widths, byte order, status-flag bit meanings, padding policy, and valid ranges. Codec tests must assert both the declared record size and every field offset so schema arithmetic cannot drift.

The capture parser uses pooled primitive buffers and direct array manipulation; a Clojure record per event is not required in the hot path. Parsed records are encoded directly into bounded writer buffers and appended to the current temporary `.bin` file while an incremental SHA-256 digest is updated. Buffers are returned to the pool after the writer consumes them. Backpressure blocks capture rather than dropping records, and writer failure aborts the entire capture. A chunk boundary cannot commit until all buffers belonging to that chunk are drained.

At 23 bytes per event, one billion instructions require about 23 GB for event payloads before snapshots and metadata. Phase 2 must benchmark FIFO throughput, disk throughput, barrier pause time, and snapshot overhead at representative event targets. The default target should be selected from measurements and remain configurable; the format must not assume that default.

### 4.6 Canonical snapshot EDN

The chunk EDN snapshot uses verbose EDN values:

- scalar values as integers;
- byte sequences as vectors of integers;
- register files as vectors of integers;
- sections and fields as maps with stable canonical keys.

The snapshot contains at least:

```clojure
{:cpu
 {:pc ... :a ... :x ... :y ... :sp ... :flags ...}
 :timing
 {:global-cycle ... :raster-line ... :cpu-cycle ...}
 :registers
 {:vic  [...]
  :sid  [...]
  :cia1 [...]
  :cia2 [...]
  ...}
 :cpu-port [...]
 :color-ram [...]}
```

Register vectors use fixed canonical windows, including reserved positions, so offsets are stable. The schema documents each vector's canonical address range and offsets.

The snapshot must include all replay-relevant mutable state outside the two 64 KiB images, including:

- VIC register file;
- SID register file;
- CIA1 and CIA2 register files;
- CPU-port state and relevant latches;
- 1 KiB color RAM;
- CPU registers and flags;
- the boundary timing tuple obtained from the first event.

The schema must say whether each register value is a hardware-readable value, a VICE-exposed latch, or a capture-maintained shadow. A CPU-visible I/O read is not automatically equivalent to the last value written to a write-only register. Capture fails if a mandatory canonical value cannot be obtained with known semantics.

---

## 5. Capture reader API

All access to raw chunks goes through a shared, thread-safe capture-reader service. Higher layers never construct chunk paths or parse `.bin` files themselves.

The reader owns:

- manifest and schema validation;
- chunk lookup by internal index;
- checksum verification;
- fixed-width event decoding;
- snapshot loading;
- nearest-checkpoint selection;
- CPU memory/write replay;
- LRU caching of decoded chunks and snapshots;
- sequential read-ahead;
- optional scan sharing/fan-out between stages.

The public API is logical-range based. Chunk-index methods are private implementation details.

Representative operations:

```clojure
(read-events reader event-range)
(scan-events reader event-range rf init)
(scan-accesses reader event-range rf init)
(scan-writes reader event-range rf init)
(scan-vic-writes reader event-range rf init)
(scan-sid-writes reader event-range rf init)
(boundary-snapshot reader chunk-start-event-index)
(replay-state reader event-index)
```

Long-range operations are reducer/transducer-oriented. Convenience materializers may exist for small ranges and complete logical windows. `scan-accesses` supplies resolved CPU read/write addresses for feature analysis such as serial-bus reads; consumers must not reimplement effective-address semantics independently.

The reader uses the nearest preceding raw-chunk snapshot as an internal replay checkpoint and replays events until the requested range. Replay state is not exposed as a raw chunk implementation detail. Checksums are validated once when an artifact enters the reader/cache, not redundantly for every event scan.

`boundary-snapshot` succeeds only at a committed chunk start and returns the exact captured boundary snapshot. `replay-state` starts from the nearest preceding boundary and returns reconstructed underlying RAM, color RAM, CPU-port state, and device write shadows together with explicit capability metadata. It must not present autonomous CIA/SID/VIC internal state as exact unless a corresponding device model exists.

The replay API also distinguishes underlying mutable RAM/device state from the 64 KiB CPU-visible snapshot image. `.mem` is authoritative at its chunk boundary. Because separate ROM images are intentionally not stored, synthesizing a new full CPU-visible image after a later `$01` mapping change is not guaranteed unless the needed immutable ROM bytes are available from the boundary image or an explicitly configured, checksum-verified ROM source. Exact stages in this plan require mutable RAM/color/write-shadow state, not arbitrary reconstructed ROM images or opaque device internals. Phase 0 must verify that this limitation does not undermine write inference in the supported envelope.

### 5.1 Logical write inference

Write inference is implemented once inside the shared reader API and reused by all exact consumers.

It:

- uses the snapshot memory and CPU-port timeline;
- decodes instruction bytes and reads pre-instruction registers;
- resolves effective addresses, including indirect addressing;
- applies complete logical 6510 memory-effect semantics for every official and undocumented opcode emitted by the supported VICE CPU core;
- emits one resulting mutation per affected destination address, which may be multiple mutations for one instruction or context transition;
- includes explicit stores, logical final RMW results, `PHA`/`PHP`, `JSR`, `BRK`, and interrupt-entry stack pushes needed for exact RAM replay;
- emits writes even when the resulting value equals the prior value;
- does not retain old values in the public result;
- records both the original CPU address and canonical device address/domain;
- records the global event index and timing;
- can be joined with `:dedupe` by event index to obtain context, block, and instruction location.

Timing uses observed trace timestamps where possible. Differences between adjacent global cycles provide observed elapsed cycles and naturally include branch/page-crossing behavior. Interrupt/context-switch gaps are represented explicitly rather than being silently attributed to an instruction. Write placement within an instruction uses the applicable 6510 write-cycle rule. Scans read one event of lookahead beyond the requested range when needed to time its final instruction; the final event of the complete capture uses verified opcode timing and is marked accordingly when no observed successor exists.

A write's structural repeat identity, where needed by a consumer, includes:

```clojure
[context block-id instruction-offset address value]
```

Timing is not part of that repeat identity.

---

## 6. Foundational `:dedupe` stage

`:dedupe` is the only general-purpose structural normalization stage. It replaces the current foundational `:writes` and `:structure` stages.

It reads raw chunks sequentially and produces a global logical execution image. Capture chunks are transport/storage units only; no downstream analysis API exposes them.

`:dedupe` retains:

- global instruction identities;
- global basic-block definitions;
- chronological basic-block occurrence data;
- context switches and context-qualified CFGs;
- edge counts, predecessor/successor counts, and loop/repeat statistics;
- independently compactly encoded timing data;
- CPU-port/ROM-visibility data;
- logical-window indexes.

Hard write-to-execute handoffs belong to `:features`, which owns behavioral write evidence; they are not duplicated in `:dedupe`.

`:dedupe` does **not** retain detailed write histories. Exact write evidence remains available through the capture reader for stages which need it.

### 6.1 Instruction identity

An instruction identity is based on:

- PC;
- opcode.

A changed opcode at the same PC creates a new instruction identity. Operand bytes do not create new identities. Each identity retains the first observed operand template and a per-byte volatility mask. When another value is observed at an operand position, that position becomes volatile and renders as `??`; its historical values may then be discarded.

This deliberately allows a control-transfer instruction with changing operand bytes to retain one instruction/block identity. Actual dynamic successor blocks and their counts live in the context-qualified CFG, so changing branch or jump targets do not disappear.

### 6.2 Basic blocks

A block is identified by its ordered sequence of instruction identities. The same sequence is globally deduplicated even when it is reached from different contexts.

A changed opcode sequence at the same start PC creates a distinct block definition. There is no v2 lifecycle/version relationship between those definitions.

Blocks end at ordinary control-transfer terminators and split at observed dynamic control-flow targets. The chronological stream is retained at block-occurrence granularity rather than as a full per-instruction event stream. Runs should be represented compactly where possible.

An interrupt can suspend a block occurrence between two instructions. Such an occurrence has one or more global-event fragments plus explicit suspend/resume markers; it is not assumed to occupy one contiguous event range. Range readers must preserve those fragments so an IRQ never manufactures a mainline CFG edge or splits the block definition.

### 6.3 Execution contexts

The global block dictionary is shared by all contexts. CFGs are separate context-qualified tables.

At minimum, contexts include:

- mainline execution;
- one context for each distinct resolved IRQ handler entry PC.

Context identity should include interrupt kind and resolved handler PC, for example `[:irq 0x8200]`, so an NMI cannot be conflated with an IRQ at the same address. NMI must either be supported with the same context-stack model or rejected explicitly.

The same block may be referenced by multiple context CFGs. Each context has its own edges and counts.

Interrupt entry and return are explicit context-stack operations. An interrupt does not split the interrupted mainline basic block; mainline execution suspends, the interrupt context executes, and the mainline block resumes. Nested supported interrupts push another context rather than overwriting the suspended context.

The CFG context is stable. Semantic roles are not attached permanently to a whole context; they are attached later to time-bounded execution spans.

### 6.4 Timing channel

Timing is independent of block identity and is encoded in a separate channel. The channel may use:

- global-cycle deltas;
- raster/cycle deltas;
- run-length encoding of repeated timing patterns;
- explicit interrupt/context-switch gaps;
- window-local checkpoints for random access.

The public API reconstructs event timing without exposing the encoding.

### 6.5 CPU-port visibility

The stage retains a compact CPU-port timeline or equivalent visibility channel because it changes infrequently and is needed by `:disassembly` to distinguish RAM from visible BASIC/KERNAL ROM.

### 6.6 On-disk layout

A default layout is:

```text
analysis/dedupe/<generation>/
  index.edn
  instructions.bin
  blocks.bin
  blocks.idx
  contexts.bin
  contexts.idx
  windows.idx
  image.bin
```

All dictionaries use implicit append-order integer IDs. IDs are not repeated unnecessarily in dictionary records. Fixed-size binary index records provide offsets for variable-size dictionaries and logical windows. `index.edn` remains small: it describes schemas, file roles, counts, and top-level statistics rather than containing one EDN entry per block or occurrence.

`image.bin` contains independently addressable logical windows. `windows.idx` records event ranges and byte offsets for channels such as:

- block occurrences;
- timing;
- context switches;
- CPU-port visibility.

No compressor is used beyond deduplication and direct binary encoding. In particular, no Zstandard, Deflate, or GZIP dependency is required.

Each window is independently readable. A public `dedupe/read-range` API loads and expands the relevant data into a high-level range object. The API hides offsets, IDs, run encoding, and dictionary files.

### 6.7 Logical windows

Dedupe windows:

- use a configurable logical event count;
- are independent of raw capture chunk boundaries;
- may span multiple raw chunks;
- are indexed by the original global VICE event coordinate;
- are processed sequentially by the writer;
- are the unit at which downstream readers can materialize data efficiently.

The logical event index is the original global event numbering, not a chunk-local index.

During construction, memory is bounded by the current logical window plus global interning/CFG tables. Those global tables grow with the number of distinct instructions, blocks, contexts, and edges—not with the number of repeated events. If real captures make structural cardinality too large, the lookup tables may later gain a spillable index without changing the persisted format or public API.

---

## 7. Stage graph and contracts

The v2 stage registry is:

```text
capture ───────────────────────────▶ dedupe ───────────────────┐
   ├───────────────────────────────▶ video ───────▶ assets     │
   ├──▶ capture + dedupe ──────────▶ audio ────────┐           │
   └──▶ capture + dedupe + video ──▶ features      │           │
                         features + video + audio ──▶ classification
                                  features + classification ──▶ segments
                                           dedupe + segments ─▶ disassembly
```

More precisely:

- `:dedupe` reads raw capture.
- `:video` reads raw capture and emits visual evidence.
- `:audio` reads raw capture plus `:dedupe` context/block mappings and emits music evidence.
- `:assets` reads raw capture plus `:video` output.
- `:features` reads raw capture plus `:dedupe` and `:video` output.
- `:classification` reads `:features`, `:video`, and `:audio` output.
- `:segments` reads `:features` and `:classification` output.
- `:disassembly` reads `:dedupe` and `:segments` output.

All stage communication uses high-level APIs. No stage directly opens raw chunk files.

### 7.1 `:video`

`:video` is an exact raw-capture consumer.

It:

- scans exact VIC writes through the capture reader;
- canonicalizes mirrored VIC addresses;
- retains all VIC write occurrences structurally;
- records ordering, timing, and global event index, which can be joined with `:dedupe` when context/block attribution is needed;
- derives frame boundaries from raster-line wraps using the capture timing model, with global-cycle consistency checks;
- reconstructs VIC configuration changes incrementally;
- publishes configuration changes to `:assets` and `:features` as soon as they are known.

Video pattern storage deduplicates repeated ordered write shapes across frames. A shape includes canonical register, order, event offset, and timing relative to the derived frame; it excludes absolute global cycle, values, and dedupe-specific IDs. Each frame occurrence stores its base global event/cycle plus the pattern ID and non-blacklisted value vector, so exact temporal joins remain possible. The high-level API expands patterns back into ordered writes; blacklisted values expand as explicitly unknown, never as zero or an omitted write.

The capture-level VIC-value blacklist is fixed for the capture and has a built-in default. Blacklisted registers retain address/order/timing information but omit values from the video representation. Registers required for VIC-state reconstruction cannot be blacklisted; capture configuration validation rejects such a blacklist.

The exact raw capture API remains available if future analysis needs a blacklisted value.

Because `:video` reads exact raw events, it has no dedupe-detail budget and no retry-with-a-higher-budget path. A range without VIC writes simply emits no video writes. Missing or uninterpretable data in a range which does contain a VIC write is a raw-capture/decoder failure and fails the stage explicitly.

### 7.2 `:assets`

`:assets` is also an exact raw-capture consumer. It does not depend on `:dedupe` retaining writes.

It consumes incremental VIC configuration points from `:video`, replays exact relevant memory/color-RAM writes from capture checkpoints, and emits reconstructed snapshots according to an explicit sampling policy. The initial v2 policy is: snapshot the initial configuration and every effective VIC asset-layout change (bank, screen base, charset base, bitmap mode/base). Ordinary screen/bitmap animation does not itself create another full snapshot. Any later representative-frame or segment-driven policy must be a versioned/configured extension, not an implicit behavior change.

Each asset snapshot is a fixed binary payload:

```text
65536 bytes underlying RAM
 1024 bytes color RAM
```

The assets stage writes all reconstructed snapshots to an append-only binary file. Its EDN index describes:

- snapshot event range and timing;
- payload offset/length;
- screen-memory range;
- charset range;
- bitmap range, when active;
- color-RAM range;
- sprite pointer and sprite-data ranges;
- VIC configuration metadata.

Large asset byte arrays are never embedded in EDN.

### 7.3 `:audio`

`:audio` is an exact raw-capture consumer used only to identify music routines.

It does not retain SID write history or values. It produces compact evidence:

- SID-write counts and density;
- counts by context, resolved handler, block, and instruction using `:dedupe` mappings;
- sustained activity over ordered logical units;
- candidate music-role evidence.

Music evidence is not an exclusive classification. It contributes the `:music` role to execution spans.

Because music routines are often VMs with changing control flow, `:audio` does not require identical whole-frame SID-write patterns.

### 7.4 `:features`

`:features` extracts generic behavioral evidence required by `:classification`. It is an exact raw-capture consumer and uses `:dedupe` for block/context identity and `:video` for VIC-derived information. Domain-specific summaries from `:video` and `:audio` are fused by `:classification` directly.

Feature evidence includes, as applicable:

- IRQ invocation units and mainline logical units;
- write-domain counts and densities;
- RAM destination footprints and growth;
- code-overwrite and write-to-execute evidence;
- serial access and loader-transfer evidence;
- SID/VIC activity evidence;
- per-PC domain counts;
- self-loop/idle evidence;
- hard write-to-execute handoff records.

A write-to-execute handoff is a hard mainline span boundary when a direct `JMP`, `JSR`, or resolved indirect `JMP` targets a PC whose opcode address was written since its previous execution. Operand-only self-modification does not qualify. The boundary is placed before the target basic block, and the write generation is consumed so repeated execution without another qualifying write does not create repeated boundaries.

`:features` emits evidence, not roles or semantic spans.

### 7.5 `:classification`

`:classification` is a pure ordered consumer of evidence. It combines `:features`, `:video`, and `:audio` evidence and emits composable role-tagged execution spans.

Possible roles include:

```clojure
#{:raster :music :loader :decruncher :idle}
```

A span may carry multiple roles. A mixed IRQ handler may therefore be represented as several spans or as one span with multiple roles, depending on the evidence boundaries.

Classification uses hysteresis to avoid fragmentation from short-lived noise. Hard handoff boundaries are honored without hysteresis.

### 7.6 Execution spans

An execution span is a time-bounded interval in one execution context:

```clojure
{:context ...
 :event-range [start end]
 :roles #{...}
 :confidence ...
 :signals [...]}
```

Spans are non-overlapping within one context. `:event-range` is the global-event envelope; membership means events in that range which belong to the named context. Consequently, spans from different contexts may have overlapping envelopes while an interrupt suspends mainline execution. APIs must expose the underlying occurrence fragments when an exact event set is needed. A context provides the CFG; a span provides the temporal role classification.

IRQ handler spans begin with the handler invocation and initially cover the handler through `RTI`. Classification splits them into sub-spans at role boundaries snapped to basic-block occurrence boundaries. Evidence must remain attributable at block-occurrence granularity; whole-handler totals alone cannot split a handler which performs raster, music, loading, and decrunching sequentially.

Mainline spans are inferred from changes in behavioral evidence. Neutral fixed windows count events in the mainline context projection, so time spent in IRQ contexts neither consumes nor dilutes a mainline window. Persistent evidence changes create candidate boundaries, which are snapped to basic-block occurrence boundaries. Mainline span boundaries are not inferred from raw capture chunks.

### 7.7 `:segments`

`:segments` consumes `:features` and `:classification` only.

It assembles execution spans into semantic segments and file/part groups. It no longer independently rediscovers low-level loader, decruncher, raster, or demopart boundaries.

All descriptors use the global logical event coordinate. No descriptor contains source-chunk references.

The existing concepts of transitions, demoparts, effect candidates, file groups, boundary confidence, and boundary audits may be retained where useful, but their inputs are execution spans and role evidence rather than chunk-local structure.

### 7.8 `:disassembly`

`:disassembly` consumes `:dedupe` and `:segments` only.

It:

- selects globally deduplicated blocks which occur in a semantic segment;
- respects context-qualified CFGs;
- applies CPU-port/ROM visibility data from `:dedupe`;
- renders each selected block once per assembly output;
- renders volatile operands as `??`;
- computes predecessor/successor, execution, and loop statistics for the selected segment/context rather than blindly reusing whole-capture totals;
- emits compact block annotations;
- emits routine-specific assemblies for raster/music/loader/decruncher roles when requested.

The annotation vocabulary is fixed and documented by the disassembly format. Every annotation fits on one comment line and uses identifiers of one or two characters at most.

Example shape:

```asm
; B:12 C:m P:04 S:07 E:1234 R:18
```

The exact identifiers and their meanings must be specified before implementation of the renderer.

---

## 8. Streaming and concurrency

The analysis runner remains able to execute any requested target stage and its dependency closure. A full request interleaves stages through bounded queues. `:dedupe` and `:video` can begin from raw capture together; `:audio` joins raw SID writes with ordered dedupe mappings; `:features` joins raw data with ordered dedupe/video messages; `:classification`, `:segments`, and finally `:disassembly` consume their ordered dependencies as those become available. `:assets` is an independent downstream consumer of `:video`.

Rules:

- each stage consumes logical units in global order even when different stages overlap;
- a stage protocol has explicit initialize, consume-window, complete, and abort operations;
- downstream stages may consume private ephemeral messages before an upstream generation is durably published;
- bounded queues provide backpressure and cancellation propagates both upstream and downstream;
- exact raw scans may share decoded data internally through the common reader;
- no public API exposes whether the reader used independent scans, read-ahead, LRU hits, or fan-out;
- stages append outputs to private generation files;
- final indexes are written only after all expected input is consumed;
- any stage failure cancels the invocation, removes every uncommitted generation created by it, and leaves previously committed generations usable.

Capture remains immutable throughout.

---

## 9. Analysis manifest and reuse

`analysis/manifest.edn` is separate from the capture manifest and is the only durable analysis status/commit record. Only one writer may modify an analysis dataset at a time; use an OS-visible lock so separate JVMs cannot race.

Each stage stores immutable generations below its own directory, for example `analysis/dedupe/<generation>/`. The manifest points to the currently committed generation. A running invocation records its progress while retaining all prior committed pointers. On success, one atomic manifest rewrite switches all newly completed stage pointers together; on failure, the pointers remain unchanged and the unreferenced generations are removed. Readers pin the manifest snapshot they opened so a concurrent successful commit cannot change their inputs mid-read.

It records:

- source capture ID;
- source capture-manifest checksum;
- requested target stage;
- running/complete/failed status;
- stage versions;
- stage configurations;
- dependency signatures;
- output directories and indexes;
- progress and failure information.

Stage outputs are reusable only when:

- the stage ID and version match;
- its configuration matches;
- all dependency signatures match;
- its output checksum/index is valid;
- the source capture identity/checksum matches.

A stage-version change invalidates all dependents automatically.

The analysis runner may execute a single target stage or the complete dependency closure. The current safe reuse behavior should be retained, but all output paths are code-defined conventions under `analysis/`.

---

## 10. Proposed public API shape

The exact namespace names may be chosen during implementation, but the public design should provide these conceptual APIs.

### Capture

```clojure
(capture/start! options)
(capture/status session)
(capture/stop! session)
```

Capture APIs return the dataset root and manifest information, never internal worker state as a required input to analysis.

### Capture reader

```clojure
(capture-reader/open dataset-root)
(capture-reader/scan-events reader range rf init)
(capture-reader/scan-accesses reader range rf init)
(capture-reader/scan-writes reader range rf init)
(capture-reader/scan-vic-writes reader range rf init)
(capture-reader/scan-sid-writes reader range rf init)
(capture-reader/boundary-snapshot reader chunk-start-event-index)
(capture-reader/replay-state reader event-index)
```

All large operations should support transducer/reducer composition.

### Dedupe

```clojure
(dedupe/open dedupe-output)
(dedupe/read-range trace start end)
(dedupe/read-context trace context-id)
(dedupe/read-block trace block-id)
```

The API returns high-level objects. Callers do not handle binary offsets, dictionary files, run encoding, or window payload formats.

### Analysis

```clojure
(analysis/stages dataset-root)
(analysis/run! dataset-root options)
(analysis/run-async! dataset-root options)
(analysis/status dataset-root)
```

---

## 11. Implementation phases

The phases are intentionally ordered so each phase establishes a testable contract for the next one. The agent may split a phase further, but must update the diary after each completed sub-phase.

### Phase 0 — Prove VICE feasibility and lock the contracts

- Build focused probes for every item in the mandatory VICE feasibility gate.
- Record exact trace-line and monitor-response fixtures from the supported stock VICE version.
- Prove a non-heuristic logging flush/EOF barrier and pre-instruction trace semantics.
- Prove how canonical write-only/latched device-register values are obtained.
- Define the supported machine/configuration envelope and explicit rejection rules.
- Create the v2 namespace/layout skeleton only after the probes pass.
- Define capture, snapshot, event, dedupe, stage, and manifest format keywords.
- Define half-open global event ranges.
- Define little-endian binary conventions.
- Define format descriptor representation in EDN.
- Define the canonical register windows and protected VIC blacklist registers.
- Define the disassembly annotation vocabulary.
- Add format/schema validation helpers.

Acceptance:

- every feasibility-gate claim is demonstrated against unmodified VICE;
- a failed probe blocks implementation and is recorded in the diary rather than weakened;
- schemas can be validated without VICE;
- malformed descriptors fail clearly;
- no existing v1 code is silently reused as a v2 contract.

### Phase 1 — Raw capture binary format and reader

- Implement the packed 23-byte fixed-width event encoding/decoding and assert every declared offset.
- Implement `.ram`, `.mem`, and chunk metadata writers/readers.
- Implement manifest commit-log entries and checksums.
- Implement atomic artifact publication and complete-capture validation.
- Implement pooled primitive parsing buffers.
- Add fixture-based parser tests for supported VICE trace forms and malformed input.

Acceptance:

- a synthetic FIFO trace produces valid binary events;
- decoded fields exactly match parsed input;
- malformed/unsupported records fail and clean up the entire capture;
- committed artifacts validate through the manifest.

### Phase 2 — VICE lifecycle, snapshot, and barrier adapter

- Implement VICE process/monitor lifecycle and PRG/D64 start policies.
- Establish the initial paused snapshot for both program-only and full-capture modes.
- Implement continuous FIFO setup.
- Implement approximate event-target boundary requests.
- Implement trace disable/pause, verified FIFO flush/EOF drain, reader re-establishment, snapshot, and resume sequencing.
- Implement full underlying RAM read.
- Implement CPU-visible memory read.
- Implement CPU/device-register/color-RAM snapshot acquisition and assign boundary timing from the verified first event.
- Validate that all required snapshot sections are complete.
- Record the capture-level VIC blacklist and reject protected registers.
- Benchmark event throughput, disk throughput, snapshot cost, and boundary overshoot before selecting the configurable default event target.

Acceptance:

- capture remains continuous between boundaries;
- snapshot boundaries precede the next assigned event;
- actual chunk counts may overshoot configured targets;
- no partial capture survives a failed snapshot;
- stop commits one non-empty final chunk and finalizes only after its writer drains;
- failure cleanup cannot delete an older capture or caller-owned parent directory.

### Phase 3 — Shared capture reader and logical write inference

- Implement manifest/schema/checksum validation.
- Implement internal chunk/snapshot LRU caching.
- Implement nearest-checkpoint replay.
- Implement logical CPU write inference.
- Implement canonical address/domain mapping and mirrored-device handling.
- Implement transducer/reducer event, access, and write scan APIs.
- Add cross-chunk and random-range tests.

Acceptance:

- sequential and random range scans produce identical results;
- writes crossing raw chunk boundaries are not lost or duplicated;
- explicit stores (including unstable undocumented stores), final RMW effects, stack pushes, calls, BRK, and interrupt-entry writes are deterministic;
- unchanged writes are retained;
- CPU-port mapping and device mirrors are correct;
- boundary snapshots are exact while derived replay states declare unavailable/unmodeled device capabilities explicitly.

### Phase 4 — `:dedupe`

- Build instruction identity tables.
- Detect volatile operand positions.
- Build global basic blocks.
- Build mainline and resolved-IRQ contexts.
- Emit context-switch markers.
- Build context-qualified CFGs and edge counts.
- Encode chronological block occurrences.
- Encode timing and interrupt gaps separately.
- Encode CPU-port/ROM-visibility data.
- Implement independently indexed logical windows.
- Write `instructions.bin`, `blocks.bin`, `blocks.idx`, `contexts.bin`, `contexts.idx`, `windows.idx`, `image.bin`, and the small `index.edn` transactionally.
- Implement high-level range/block/context readers.

Acceptance:

- repeated blocks are represented once globally;
- changed operands mark volatility without duplicating instruction identities;
- changed opcodes create distinct instruction/block identities;
- shared blocks can appear in multiple CFG contexts;
- IRQ interruption does not split the interrupted mainline block;
- logical ranges cross raw chunk boundaries transparently;
- peak memory grows with current-window size and distinct structural vocabulary, not with repeated event count.

### Phase 5 — Exact domain projections: `:video`, `:audio`, `:assets`

`:video`:

- scan/canonicalize all VIC writes;
- derive frame boundaries;
- apply the fixed capture blacklist;
- build structural frame patterns and value streams;
- emit incremental configuration changes and exact structural write output.

`:audio`:

- scan raw SID writes;
- join them with dedupe context/block mappings by global event index;
- compute density and per-context/per-block evidence;
- emit music-role evidence without write history or values.

`:assets`:

- consume incremental video configuration events;
- replay exact relevant memory/color-RAM writes;
- write `64 KiB RAM + 1 KiB color RAM` snapshots;
- write EDN asset indexes and ranges.

Acceptance:

- video detects and persists every VIC write structurally with reconstructable global event positions;
- blacklisted values are absent from video output but available through raw capture;
- animated non-blacklisted values use one structural pattern plus value streams;
- audio identifies sustained SID-writing handler evidence;
- assets reconstruct the exact initial sample and every configured VIC asset-layout-change sample;
- all three stages can consume raw capture through APIs without direct chunk access.

### Phase 6 — `:features` and `:classification`

- Implement neutral logical evidence units.
- Build IRQ invocation units and mainline fixed-window units.
- Compute write-domain, destination, serial, code-overwrite, and idle evidence.
- Detect hard write-to-execute handoffs.
- Fuse feature/video/audio evidence.
- Emit hysteresis-stabilized role-tagged execution spans.
- Support multiple roles on a span.

Acceptance:

- mainline role transitions are detected without chunk boundaries;
- IRQ handlers can split into multiple role spans;
- write-to-execute boundaries are hard boundaries;
- mixed handlers can receive `:raster`, `:music`, `:loader`, and `:decruncher` roles;
- stable self-loops receive `:idle`.

### Phase 7 — `:segments`

- Consume execution spans and evidence only.
- Assemble spans into semantic segments and file groups.
- Preserve global event ranges and confidence/signals.
- Generate boundary audits using logical ranges.
- Remove source-chunk dependencies from descriptors.

Acceptance:

- segment descriptors contain no chunk-local implementation details;
- boundaries are reproducible from persisted upstream evidence;
- audits identify overlaps, gaps, hard handoffs, and role evidence.

### Phase 8 — `:disassembly`

- Select blocks from dedupe contexts and segment ranges.
- Apply CPU-port/ROM visibility filtering.
- Render volatile operands as `??`.
- Render every selected block once per assembly output.
- Add fixed terse one-line annotations.
- Produce segment, routine, and role-specific assembly files.

Acceptance:

- no raw capture access is needed;
- shared blocks are rendered consistently;
- annotations fit on one comment line;
- assembly exposes context, predecessor/successor, count, and loop information.

### Phase 9 — Orchestration, reuse, and replacement

- Implement the dependency graph and target-stage closure.
- Implement bounded inter-stage queues and cancellation.
- Implement immutable stage generations, writer locking, manifest-pinned readers, and one atomic manifest commit for each successful invocation.
- Implement version/config/dependency invalidation.
- Implement full-run interleaving and independent-stage execution.
- Replace current public capture/analysis APIs.
- Remove or quarantine obsolete v1 orchestration once migration is complete.

Acceptance:

- any requested stage can run independently with its dependency closure;
- a full run overlaps stages without violating logical order;
- failed analysis leaves no published partial stage;
- version changes invalidate dependents safely;
- capture remains unchanged after analysis.

---

## 12. Testing strategy

### Unit tests

- packed 23-byte event codec and field offsets;
- flags encoding/decoding;
- register-vector schema validation;
- address mirroring and canonicalization;
- CPU-port mapping;
- instruction identity and volatile masks;
- basic-block construction;
- CFG context separation;
- IRQ/NMI context-stack switching and suspended block fragments;
- cycle delta and interrupt-gap handling;
- logical write inference, including stack/control-flow effects and final-event lookahead;
- exact boundary-snapshot versus capability-tagged replay-state semantics;
- frame boundary detection;
- structural video pattern/value-stream and blacklist handling;
- role-span hysteresis;
- hard write-to-execute boundaries;
- terse annotation formatting.

### Property and invariant tests

- encode/decode preserves every capture event field;
- logical ranges are independent of raw chunk boundaries;
- repeated block definitions have stable IDs within one dedupe image;
- changed opcode creates a distinct identity;
- operand volatility never creates a duplicate identity;
- manifest entries appear only after all artifacts are complete;
- chunk EDN and top-level checksums have no circular definition;
- a failed stage publishes nothing and leaves prior generation pointers intact;
- capture failure removes the entire capture;
- raw capture is byte-for-byte unchanged by analysis.

### Integration tests

- synthetic FIFO capture and real stock-VICE capture with verified, non-timeout chunk-boundary barriers;
- multiple raw chunks with a span crossing every possible boundary;
- an IRQ suspending and resuming a mainline block without splitting its CFG block;
- a mainline decruncher with a write-to-execute handoff;
- nested calls and interrupt entry whose stack writes cross logical/raw boundaries;
- a multi-raster IRQ handler with mixed roles;
- a handler which writes SID registers as a music routine;
- animated VIC writes with stable structural shape;
- blacklisted VIC values;
- exact asset snapshot reconstruction;
- random-access scans compared with sequential scans;
- LRU cache hit/miss behavior;
- concurrent stage execution and backpressure;
- restart/reuse after stage-version changes.

### Acceptance capture

Maintain at least one representative modern multi-disk trackmo capture target, approximately twenty minutes of execution. The acceptance run should verify:

- raw capture completes continuously within the explicitly supported machine envelope;
- raw files remain readable offline;
- dedupe memory remains bounded by dictionaries/current windows;
- exact consumers can scan raw ranges without loading the complete capture;
- classification identifies meaningful mainline and IRQ spans;
- disassembly output is understandable and compact;
- the full analysis can be rerun from the immutable capture.

---

## 13. Implementation diary

The implementing agent must append an entry after every phase or sub-phase. Do not rewrite earlier entries; append corrections and follow-up decisions.

Use this format:

```markdown
### Diary entry: <phase/date>

- **Status:** complete | blocked | partial | failed
- **Changes:**
  - ...
- **Tests run:**
  - ...
- **Observed issues:**
  - ...
- **Decisions or deviations:**
  - ...
- **Next step:**
  - ...
```

### Initial design entry

- **Status:** design complete; implementation not started.
- **Changes:** established the v2 architecture, immutable capture contract, raw snapshot/file formats, shared capture-reader APIs, foundational `:dedupe` stage, exact domain stages, execution spans, stage graph, transactional orchestration, implementation phases, and acceptance criteria.
- **Tests run:** none; implementation has not started.
- **Observed issues:** exact VICE monitor support for every required snapshot section and the FIFO flush barrier must be verified before implementation. The capture adapter must fail closed if any required section is unavailable.
- **Decisions or deviations:** v2 replaces the current public APIs. Analysis must never modify the capture manifest.
- **Next step:** Phase 0 — prove VICE feasibility and lock schemas/contracts.

### Diary entry: design review

- **Status:** complete.
- **Changes:** corrected the event record from 26 to 23 bytes; added a mandatory stock-VICE feasibility gate and a non-heuristic FIFO barrier contract; clarified dataset roots, snapshot timestamps, checksum ownership, interrupt-suspended blocks, context stacks, binary indexes, complete stack/control-flow write inference, write-to-execute semantics, asset sampling, and generation-based atomic analysis publication.
- **Tests run:** documentation review only; no implementation exists yet.
- **Observed issues:** write-only/latched VIC/SID/CIA snapshot semantics remain the largest feasibility risk and must be proven in Phase 0.
- **Decisions or deviations:** global dictionaries may grow with distinct structural vocabulary, but repeated event count must not drive heap growth. Exact write details remain outside `:dedupe`.
- **Next step:** Phase 0 — execute the feasibility probes before committing to the rest of the rewrite.

### Diary entry: Phase 0 feasibility gate and frozen contracts

- **Status:** complete. The stock-VICE feasibility gate passed every item and
  the v2 contracts are frozen.
- **Changes:**
  - Added `omkamra.vice.contract`, the single source of truth for the v2
    format keywords (`:omkamra.vice/capture-v2`, `.../exec-events-v2`,
    `.../ram-image-v2`, `.../mem-image-v2`, `.../snapshot-v2`, `.../chunk-v2`,
    `.../dedupe-v2`, `.../analysis-v2`, `.../analysis-stage-v2`,
    `.../semantic-segment-v2`), the little-endian binary convention, the packed
    23-byte event-record descriptor, native status-flag conversion, canonical
    VIC/SID/CIA/CPU-port/color-RAM windows, the protected-register blacklist
    policy, half-open event ranges, and the fixed one-line disassembly
    annotation vocabulary. Descriptor, register-window, snapshot, range, and
    blacklist validators all run without VICE.
  - Added `omkamra.vice.feasibility`, four probes that run against an
    unmodified stock VICE through `omkamra.vice.binary-monitor`, each returning
    checked claims plus raw evidence, and `run!`/`-main` to execute and persist
    them.
  - Added `omkamra.vice.contract-test`.
  - Recorded the generated report at
    `src/omkamra/vice/docs/v2-feasibility-report.edn`.
- **Tests run:**
  - `omkamra.vice.contract-test` through the project nREPL: 9 tests, 61
    assertions, 0 failures.
  - `omkamra.vice.feasibility/run!` against VICE 3.9 (`x64sc`): 4/4 probes
    passed, no blockers.
  - `clj-kondo` (0 errors, 0 warnings) and `cljfmt check` clean for all three
    new files.
- **Feasibility-gate evidence (VICE 3.9, `x64sc`):**
  1. Disabling `MonitorLogEnabled` closes the log file
     (`monitor.c`/`set_monitor_log_enabled` -> `mon_log_file_close`), so the
     FIFO reader observes a real end of stream. This is a flush barrier, not a
     quiet period.
  2. After the barrier VICE remains paused: the monitor PC read twice, 200 ms
     apart, was identical.
  3. The computer memory space (`memspace 0`) exposes underlying 64 KiB RAM as
     bank 1 (`ram`) and the CPU-visible image as bank 0 (`cpu`); `rom` is bank
     2 and `io` bank 3. A signature written to bank-1 `$E000` reads back
     through bank 1 while banks 0 and 2 return the KERNAL ROM byte; both images
     are exactly 65,536 bytes.
  4. Snapshot acquisition has documented semantics: A/X/Y/SP/PC/FL, the
     `$00`/`$01` ports, and LIN/CYC come from `registers-get`; VIC registers
     read back their hardware value; SID write-only registers are exposed as
     VICE latches (`$D400`/`$D401`/`$D418` round-trip); CIA ports read back,
     while `$DC0D` is a live hardware read (the written control byte is not
     echoed); color RAM reads as nibbles.
  5. Trace records are pre-instruction: the record at `$0336` carries the A
     value produced by the `$0334` load, and `$0338` carries the value produced
     by `$0336`. Instruction bytes are complete.
  6. The textual flags field converts unambiguously. VICE renders `NV-BDIZC`
     with the set letter and `.` for clear (`mon_register6502.c`); the unused
     bit is always `-`. `..-..I.C` and the monitor `FL` register both yielded
     status byte 5.
  7. The first event after the snapshot continues the global cycle exactly: the
     last drained record (`JMP $0334`, cycle 78623, 3 cycles) was followed by
     the paused `$0334` event at cycle 78626 with no intervening cycle.
     Fixtures: last trace line
     `.C:0339  4C 34 03    JMP $0334      - A:02 X:00 Y:A7 SP:fd ..-..I.C      78623`;
     first post-snapshot record
     `[820 [169 1] 0 2 2 0 167 253 "..-..I.C" 78626]`.
- **Observed issues:**
  - `memspace 1` is not the RAM space: `get_requested_memspace` maps memspaces
    1-4 to the disk-drive spaces. Underlying RAM is `memspace 0`, bank 1;
    this is why the naive `memspace 1` read looked like a separate store.
  - VICE writes the monitor prompt (`(C:$0337) `) into the log file whenever
    the monitor is entered, so the trace stream contains non-trace lines. The
    v2 parser must ignore that prompt explicitly rather than fail closed on it.
  - The installed VICE 3.9 does not force status bit 5 in `FL`; the contract
    stores bit 5 exactly as observed so a captured status byte equals the
    monitor register without masking.
- **Decisions or deviations:**
  - v2 development namespaces use the `omkamra.vice.*` prefix while v1 is
    still present; Phase 9 replaces the public v1 APIs and the prefix can then
    be retired.
  - The default VIC value blacklist omits only the sprite coordinate registers
    (`$D000-$D010`); `$D011`, `$D016`, `$D018`, and `$DD00` are protected.
  - The event descriptor keeps the agreed 23-byte layout with no implicit
    padding.
- **Next step:** Phase 1 — implement the packed event codec plus the `.ram`,
  `.mem`, chunk, and manifest writers/readers with synthetic fixtures, before
  touching the VICE lifecycle adapter.

### Diary entry: Phase 0 review correction

- **Status:** complete.
- **Changes:** the project owner corrected the default VIC value blacklist.
  `omkamra.vice.contract/default-vic-value-blacklist` is now `$D000-$D010`
  (sprite coordinates and MSB), `$D015` (sprite enable), `$D017` (sprite Y
  expansion), and `$D020-$D02E` (border, background, and sprite colours).
  This supersedes the sprite-coordinates-only default recorded in the previous
  entry.
- **Tests run:** `omkamra.vice.contract-test` — blacklist policy and every
  contract validator pass.
- **Observed issues:** none.
- **Decisions or deviations:** the protected set is unchanged (`$D011`,
  `$D016`, `$D018`, `$DD00`), so asset-layout reconstruction remains possible
  while all pure display state is omitted from the video value stream.
- **Next step:** Phase 1.

### Diary entry: Phase 1 — raw capture binary format and recorder

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.event`: the packed 23-byte little-endian event codec
    (`encode-event-into!`, `encode-event`, `decode-event`, `decode-events`,
    `read-events-file`), an allocation-light `write-events!` that reuses one
    record buffer, and `trace-record->event`, which adapts forensic
    `omkamra.vice.trace` records into v2 events and fails closed on unsupported
    instruction lengths or missing forensic fields.
  - Added `omkamra.vice.store`: SHA-256 checksums, atomic artifact
    publication (`write-artifact!`), 64 KiB `.ram`/`.mem` image writers and
    readers, chunk-metadata paths, the top-level manifest, complete-capture
    validation, and `open-capture`.
  - Added `omkamra.vice.recorder`: dataset-root capture reservation (refuses
    a finalized capture, removes a stale unfinalized one), transactional
    `commit-chunk!` (payload temp files, atomically installed payloads,
    atomically installed chunk EDN, atomically rewritten manifest as the commit
    point), `finalize-capture!`, `abort-capture!`, and `ingest-trace-records!`,
    which batches converted events to an event target and destroys the whole
    capture on any failure.
  - Added `omkamra.vice.capture-test`.
- **Tests run:**
  - `omkamra.vice.contract-test` and `omkamra.vice.capture-test` through
    the project nREPL: 17 tests, 97 assertions, 0 failures.
  - `clj-kondo` 0 errors/0 warnings; `cljfmt check` clean.
- **Observed issues:**
  - The manifest, not the chunk EDN, is the commit point. A crash between
    payload installation and the manifest rewrite can leave orphan payloads;
    the next reservation or open removes or overwrites them.
  - The v1 trace parser is deliberately lenient about unrecognized lines (it
    must ignore VICE's monitor prompt), so strict line-level fail-closed
    parsing remains a Phase 2 item; Phase 1 fails closed on any record it cannot
    convert.
- **Decisions or deviations:**
  - `.ram`/`.mem` files carry no header: each is exactly 65,536 bytes in
    ascending address order, and all format metadata lives in the manifest and
    chunk EDN.
  - The recorder takes boundary snapshots and memory images from the caller, so
    it stays free of VICE; the Phase 2 adapter will supply them.
- **Next step:** Phase 2 — VICE lifecycle, initial and boundary snapshots, and
  the verified disable/drain barrier adapter.

### Diary entry: Phase 2 — VICE lifecycle, boundary snapshot, and capture pipeline

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.snapshot`: side-effect-free canonical snapshot
    acquisition from a paused monitor. CPU A/X/Y/SP/PC/FL, the `$00`/`$01`
    ports, and LIN/CYC come from named monitor registers; underlying RAM and the
    CPU-visible image come from the computer space's `ram` and `cpu` banks;
    device registers and color RAM come from the `io` bank, so they are correct
    even while `$01` banks I/O out. Boundary timing is attached from the first
    event after the resume.
  - Added `omkamra.vice.capture`: the public `start!`/`status`/`stop!`/
    `stop-async!` lifecycle, PRG/D64 program-entry and full-capture start
    policies, the execute-only non-stopping tracepoint, the FIFO, the verified
    disable/drain/EOF barrier, boundary snapshots, transactional chunk
    commitment, and capture metrics. Responsibilities are split across a reader
    thread (FIFO -> bounded `ArrayBlockingQueue`), a controller thread (queue ->
    events -> chunks -> boundaries -> finalization), and a short-lived resume
    future.
  - Added `omkamra.vice.capture-live-test`, an on-demand live integration
    test.
  - Fixed two latent bugs the live path exposed: the event encoder used
    Clojure's checked `short`, which threw for addresses or raster lines above
    32767 (now `unchecked-short`, with a regression test); and the capture
    deref'd the recorder handle, a plain map, instead of using it directly.
- **Tests run:**
  - Fast suite: `contract-test`, `capture-test`, `snapshot-test` — 20 tests,
    105 assertions, 0 failures.
  - Live: `capture-live-test` against VICE 3.9 — 1 test, 10 assertions, 0
    failures. It crosses several boundaries, finalizes, validates every chunk
    and artifact through the manifest, and checks per-chunk event-cycle
    monotonicity.
  - Benchmark: 20 s warp capture of a looping PRG — 9 chunks, 495,186 events
    (~19.6k events/s including startup and shutdown), snapshot cost ~330 ms
    average / 372 ms max per boundary, boundary pause 620 ms max, chunk write
    166 ms max, and 9% maximum target overshoot.
- **Observed issues:**
  - The controller must already be draining before VICE resumes: monitor-log
    writing blocks both VICE and the reader once the bounded queue fills. The
    resume/autostart is therefore issued from a short-lived future after the
    controller is running. This was the actual cause of the earlier
    "black screen" hangs, amplified by the recorder-handle deref bug.
  - Each boundary spends ~330 ms acquiring the snapshot (seven monitor
    round-trips while paused), a large share of a 50k-event boundary.
  - The hot path still materializes one Clojure map per event. The design
    permits a pooled primitive buffer, and a faster pipeline is a Phase 3 /
    performance follow-up; it does not change the persisted format or API.
- **Decisions or deviations:**
  - Reader and controller are separate threads with a bounded queue, matching
    the design's streaming/backpressure model; `status` exposes queue depth.
  - The default `:event-target` is raised from 50,000 to 100,000 based on the
    measurement above, halving the per-event snapshot overhead while keeping a
    single chunk's events comfortably within heap. It stays configurable, and
    actual chunks overshoot it by roughly 9%.
  - `:capture-directory` is included in the stopped result alongside
    `:manifest-path`.
- **Next step:** Phase 3 — the shared capture reader and logical write
  inference.

### Diary entry: Phase 2 acceptance capture — No Booze, No Phone, No Party

- **Status:** complete. The capture path produced a finalized, validated v2
  capture of the acceptance demo.
- **Changes:** none (validation run only); the adapter was exercised end to end.
- **Input:** `/c64/Triad/r676_No Booze__No_Booze_Triad.d64` — one disk image
  holding the three files `triad/no booze!`, `2. no phone !`, and
  `3. no party!!!`. This is the three-file structure the acceptance target
  calls for (a decruncher before each demopart, and a fast-loader at the end of
  each file that loads the next one).
- **Results:**
  - `captures/no-booze-triad-v2/capture`, finalized and validated by
    `store/open-capture`: **420 chunks**, **43,220,373 events**, contiguous
    event ranges with no gaps; 420 × `.bin`/`.ram`/`.mem`/`.edn`, 1,005 MB total
    (994 MB events, 55 MB memory images).
  - Events per chunk: min 17,083 (final), max 106,586, mean 102,905 at a 100,000
    target (≈6.6% maximum overshoot).
- **Tests run:** `store/open-capture` validated every manifest entry, chunk
  checksum, artifact checksum, fixed memory-image size, and event-record count.
- **Observed issues / follow-ups:**
  1. **Our hot path needs a dedicated optimization pass.** The reader allocates
     a vector per parsed record, the controller allocates a map per event and
     retains the whole chunk as a vector of maps, and the chunk is written
     synchronously at the boundary. Measured: **~26.7k events/s**, boundary
     pause **265 s of 1,620 s wall time (~16%)** — of which snapshot acquisition
     is 158.7 s — plus 71.7 s of chunk writes (off the pause path). The planned
     fix is to keep events in pooled primitive buffers and encode the 23-byte
     records directly into the writer buffer, without changing the persisted
     format or the reader/controller API.
  2. **VICE leaks under continuous tracing** (confirmed; not the JVM and not
     GC). VICE RSS grew linearly at **~3.1 MB/s (~110–140 bytes per traced
     instruction)**, entirely anonymous/private-dirty, with a stable 13 threads,
     while the JVM heap oscillated (468–726 MB) and even uncommitted memory. The
     leak is per traced instruction, so a larger event target does not reduce
     it; it bounds capture length. The per-instruction `CHECKPOINT_INFO` we
     ignore is a fixed 23-byte checkpoint struct (not disassembly) and has no
     resource to disable it; a one-line VICE patch emitting it only for
     stopping checkpoints would remove the traffic and, if the leak is in that
     path, the leak. A discriminator run (non-stopping checkpoint with logging
     disabled versus enabled) is queued to confirm the site before patching.
  3. The expected decruncher/demopart/fast-loader structure has not been
     checked yet: the raw capture is byte-exact event data, and structural
     evidence arrives with `:dedupe` and the later stages.
- **Decisions or deviations:** the capture is retained as the v2 acceptance
  capture; no format or API changes were needed to obtain it.
- **Next step:** Phase 3 — the shared capture reader and logical write
  inference.

### Diary entry: VICE tracing leak — decision (no patch)

- **Status:** complete (decision recorded).
- **Changes:** per project-owner decision, VICE will **not** be patched for the
  per-instruction tracing leak. The leak is accepted as an operational
  constraint: a capture must be stopped before VICE's resident memory becomes
  excessive. The logging-off/on discriminator experiment is dropped with the
  patch.
- **Tests run:** none.
- **Observed issues:** the leak is ~3.1 MB/s (~110–140 bytes per traced
  instruction) and is per-instruction, so it bounds capture length rather than
  being reducible by the event target. During the acceptance capture VICE RSS
  was 3.41 GB at 28.7 M events, and host free memory recovered by ~4.3 GB when
  VICE exited, implying a peak near 5 GB at 43.2 M events.
- **Decisions or deviations:** no VICE-side change. The planned optimization
  pass remains the only change on our side; it shortens wall-clock time per
  capture but does not affect VICE's leak.
- **Next step:** Phase 3 — the shared capture reader and logical write
  inference.

### Diary entry: Phase 3 — shared capture reader and logical write inference

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.isa`, a self-contained MOS 6510 vocabulary: the
    256-entry opcode table, addressing-mode widths, `decode` (which now returns
    the instruction bytes used for operand resolution), instruction-effect
    classification, final store/RMW value rules including unstable
    undocumented high-byte masking, branch decisions from the instruction's own
    flags, and stack-pointer and successor helpers. It deliberately does not
    depend on the v1 `omkamra.vice.asm` disassembler so retiring v1 cannot
    change inferred writes.
  - Added `omkamra.vice.reader`, the shared capture reader. It validates a
    finalized capture once through `store/validate-capture!`, resolves a global
    event index to a committed chunk by binary search, and caches raw chunk
    payloads and snapshots in a bounded LRU. It exposes `scan-events`,
    `read-events`, `scan-writes`, `read-writes`, `scan-accesses`,
    `read-accesses`, `scan-vic-writes`, `scan-sid-writes`,
    `boundary-snapshot`, `boundary-snapshots`, `chunk-boundaries`, and
    `replay-state`.
  - Implemented exact logical write inference. Stores, final RMW results,
    `PHA`/`PHP`, `JSR`, `BRK`, and hardware-interrupt stack pushes each emit one
    mutation per destination; writes are emitted even when unchanged; old
    values are read internally for RMW but not retained in the public result.
    `RTS`/`RTI`/`PLA`/`PLP` stack reads and indirect pointer reads are resolved
    from the replayed state. Successor PCs are deterministic, including the
    NMOS `JMP (addr)` page-wrap bug and branch taken/not-taken from the
    branch's own pre-instruction flags.
  - Interrupt entry is detected as an unexplained three-byte stack-pointer drop
    relative to the previous instruction's modelled post-instruction SP. The
    three pushes are emitted at the handler's first event: return address from
    the interrupted instruction's successor, and status from the handler's
    first-event flags with the I bit cleared for IRQ (I preserved for NMI). A
    straddling interrupt whose entry is the first event of a chunk is
    reconstructed byte-for-byte from that boundary snapshot's underlying RAM
    (already containing the pushes) and emitted without re-applying it.
  - Implemented canonical address/domain mapping and mirrored-device handling:
    CPU port, underlying RAM, color RAM, and canonical VIC/SID/CIA1/CIA2
    register windows with `mod`-based mirror canonicalization.
  - Implemented nearest-checkpoint replay with an exact CPU-visible memory
    model. Underlying RAM, color RAM, the CPU port, and device write shadows
    are replayed forward from the boundary; basic/KERNAL ROM bytes come from
    the immutable boundary `.mem` image, so effective addresses, indirect
    targets, and IRQ vectors stay correct after `$01` changes.
  - `replay-state` returns underlying RAM, color RAM, CPU-port bytes, device
    write shadows, and explicit capability metadata. It also has a 3-arity
    `(replay-state reader event-index checkpoint-index)` used by the continuity
    test to start from an earlier committed boundary.
  - Added `omkamra.vice.reader-test`.
- **Tests run:**
  - Fast suite (`contract-test`, `capture-test`, `snapshot-test`,
    `reader-test`) through the project nREPL: 28 tests, 184 assertions,
    0 failures.
  - `reader-test` alone: 8 tests, 79 assertions, 0 failures. It covers the
    256-opcode vocabulary, operand resolution (indexed, indirect, JMP-indirect
    page wrap), store/RMW/branch/stack values, a synthetic two-chunk capture
    with known stores/RMW/`PHA`/`PHP`/`JSR`/`RTS`/interrupt writes, device
    domains and VIC mirrors, boundary-snapshot addressing, and sequential vs
    split write/event scans across a chunk boundary.
  - Acceptance continuity against `captures/no-booze-triad-v2` for chunk pairs
    beginning at 1, 2, 3, 4, 51, 101, 201, 301, and 419: after replaying each
    chunk from its boundary snapshot, underlying RAM (excluding the
    CPU-port window `$0000-$0001`) is byte-for-byte identical to the next
    chunk's committed `.ram`, and color RAM is identical in the stored low
    nibble. Random-access ranges also match sequential ranges across a chunk
    boundary.
  - `clj-kondo` 0 errors/0 warnings; `cljfmt check` clean for all three new
    files.
- **Observed issues:**
  - VICE's raw underlying-RAM bank at `$0001` changes without any traced CPU
    store. The address is the 6510 port and is not CPU-observable through RAM
    (its observable value lives in `$00`/`$01` and the boundary `.mem`), so it
    is treated as a VICE-internal artifact: the continuity comparison excludes
    `$0000-$0001`, and replay does not write the port value into underlying
    RAM.
  - CPU-port data bits configured as inputs are externally driven. Traced
    writes reconstruct the output bits exactly (and the `$01` bits 0-2 that
    select ROM/I-O banking were correct for every tested pair), but an
    input-direction bit can differ from VICE's `$01` register value. This is
    reported through the `:opaque-device-state :unmodeled` capability rather
    than presented as exact.
  - Color RAM reads return bus garbage in the high nibble; only the low nibble
    is a real color value and only the low nibble is inferred and compared.
  - The reader decodes events into maps lazily on top of cached raw chunk
    bytes. The Phase 2 hot-path optimization (pooled primitive buffers encoding
    directly into the writer) remains a separate capture-side follow-up; it
    does not change the reader API or the persisted format.
- **Decisions or deviations:**
  - `scan-accesses` emits resolved reads and writes; a read-modify-write yields
    the read followed by the final-value write, so consumers such as serial-bus
    read analysis do not reimplement effective-address semantics.
  - `replay-state` never presents a domain as exact beyond what is modelled:
    `:ram`, `:color-ram`, and `:cpu-port` are `:exact`; VIC/SID/CIA are
    `:write-shadow`; ROM is `:boundary-image`; char ROM and opaque device state
    are `:unmodeled`.
  - Interrupt status bytes are derived from the handler's first-event flags
    instead of a full post-instruction flags model, which keeps the inference
    self-contained and was sufficient for exact RAM continuity on the
    acceptance capture.
  - The `.ram`/`.mem` boundary images are re-read through `store/read-image`
    and cached alongside the decoded chunk, so replay never constructs paths
    outside the reader.
- **Next step:** Phase 4 — implement the `:dedupe` foundational structural
  normalization stage (instruction identity, global basic blocks, execution
  contexts and context-qualified CFGs, timing channel, CPU-port visibility,
  logical windows, and the transactional on-disk layout).

### Diary entry: Phase 4 — `:dedupe`

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.dedupe`, the foundational structural normalization
    stage and its high-level reader. `build!` streams the capture through the
    shared reader and writes a private generation; the readers are
    `open`/`status`/`read-range`/`read-block`/`read-context`, and `open`
    verifies the analysis manifest, index format/version, and every artifact's
    size and SHA-256 once (including the nested index/image files).
  - Instruction identity is `[pc opcode]`. A changed opcode creates a distinct
    identity; operand bytes never do. Each identity retains the first observed
    operand template and a per-operand volatility mask, so a later value at an
    operand position marks it volatile without duplicating the identity.
  - Global basic blocks are keyed by their ordered instruction-identity vector
    and interned once per distinct sequence. Blocks end at the ordinary
    control-transfer terminators and split when an event's PC is a previously
    observed branch/`JMP abs`/`JSR abs` target.
  - Execution contexts are mainline plus one context per resolved interrupt
    handler entry PC (`[:irq handler-pc]`), with `:brk` for `BRK`. Hardware
    interrupt entry is detected by the same unexplained three-byte stack drop
    used by the Phase 3 reader; `RTI` returns to the suspended context. Each
    context owns its own block dictionary view and its own context-qualified
    CFG with dynamic edge counts.
  - An interrupt suspends the current mainline block instead of closing it. The
    occurrence keeps its instruction sequence and accumulates fragments; on
    `RTI` the suspended frame resumes and appends to the same occurrence. An
    interrupt therefore never splits the interrupted mainline block or
    manufactures a mainline CFG edge.
  - Logical windows are `:window-events`-sized and are flushed only at a
    mainline block boundary (context depth one, no open block), so every
    occurrence in a flushed window is complete and no occurrence is split
    across windows. Windows are independent of raw capture chunk boundaries.
  - Per-window channels are the occurrence channel (identity, context, block,
    fragments), an independent timing channel (start/end global cycle per
    occurrence), the CPU-port visibility timeline, and the context-switch
    channel. `windows.idx` records each window's event range and the byte
    offset/length of every channel in `image.bin`.
  - On-disk layout: `index.edn`, `instructions.bin`, `blocks.bin`,
    `blocks.idx`, `contexts.bin`, `contexts.idx`, `windows.idx`, `image.bin`.
    All multi-byte integers are little-endian fixed-width records.
  - Transactional publication: the whole generation is written under
    `analysis/dedupe/<generation>/`, then `analysis/manifest.edn` is atomically
    rewritten to point at it. A failure deletes the generation and publishes
    nothing, leaving any previous generation usable.
  - Added `omkamra.vice.dedupe-test`; extended `omkamra.vice.reader`
    with `manifest`, `directory`, and a lazy `event-pairs` accessor.
- **Tests run:**
  - Full fast v2 suite (`contract-test`, `capture-test`, `snapshot-test`,
    `reader-test`, `dedupe-test`): 32 tests, 212 assertions, 0 failures.
  - `dedupe-test` synthetic tests build a two-chunk capture with one hardware
    interrupt and assert: two globally deduplicated blocks; instruction
    identity with a volatile operand; mainline plus `[:irq $2000]` contexts;
    the mainline self-loop CFG edge; one mainline occurrence with fragments
    `[[0 2] [5 7]]` (suspended and resumed) interned to the same block as its
    next iteration; the `:irq`/`:return` switch records; the `$01` port change;
    per-occurrence timing; window-independent range reads; and transactional
    cleanup on a rejected build.
  - `dedupe-test` acceptance test over a 450,000-event prefix of
    `no-booze-triad-v2`: mainline and resolved-IRQ contexts, positive IRQ edge
    count, multi-fragment (interrupt-suspended) mainline occurrences, operand
    volatility, blocks shared between context CFGs, and range transparency
    across the raw chunk boundary at event 104,833.
  - `clj-kondo` 0 errors/0 warnings; `cljfmt check` clean for both new files.
- **Observed issues:**
  - The build hot loop threads a persistent state map per event and measures
    about 37k events/s (450k events in roughly 12s), so a full 43M-event build
    would take on the order of twenty minutes. Correctness and memory bounds
    are unaffected; a mutable/primitive hot-loop optimization is a follow-up,
    comparable to the capture-side hot-path pass.
  - NMI is not vector-classified: every hardware interrupt entry is registered
    as `:irq` keyed by its resolved handler PC, so an NMI is never conflated
    with an IRQ at a different handler PC but shares the `:irq` kind. Exact
    `:nmi` kind resolution is deferred; `BRK` already uses `:brk`.
  - `read-range` returns any occurrence with a fragment intersecting the
    requested range, so an occurrence spanning a range boundary appears whole
    in both halves. Consumers should group by `:occurrence-id`.
  - Only direct `STA`/`STX`/`STY $01` writes feed the CPU-port timeline;
    a write through a zero-page indirect pointer is not tracked. In practice
    `$01` is always set directly.
- **Decisions or deviations:**
  - Windows flush at mainline block boundaries rather than at an exact event
    count, trading an approximate `:window-events` for whole occurrences per
    window and bounded memory.
  - CFG edges are context-qualified dynamic transition counts; interrupt entry
    and return do not create mainline edges.
  - `analysis/manifest.edn` is the commit point for the stage. The full §9
    stage registry, reuse invalidation, and OS-visible writer lock remain
    Phase 9; old successful generations are left on disk (under the gitignored
    `captures/` tree) rather than pruned automatically.
  - `build!` accepts a test-only `:max-events` prefix bound; a prefix image is
    a valid closed prefix of the full image.
- **Next step:** Phase 5 — the exact domain projections `:video`, `:audio`, and
  `:assets`.

### Diary entry: Phase 5 — exact domain projections: `:video`, `:audio`, `:assets`

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.analysis`, the shared analysis-manifest and
    generation helper (`manifest-path`, `read-manifest`, `stage-entry`,
    `stage-directory`, `capture-manifest-sha`, `new-generation`,
    `commit-stage!`). `:dedupe` was refactored onto it while keeping its public
    `read-manifest`/`status` behaviour, so every stage now shares one atomic
    commit log.
  - Added `omkamra.vice.video`. `build!` scans exact VIC state writes
    through the shared capture reader, canonicalizes VIC-II mirrors
    (`addr & 0x3f <= 0x2e`) plus CIA2 `$dd00`, derives frame boundaries from
    raster-line wraps, applies the capture-level value blacklist, deduplicates
    repeated ordered write shapes, and publishes an incremental layout
    configuration point whenever the bank, screen base, charset base, bitmap
    mode, or bitmap base changes. A pattern is an ordered vector of
    `[canonical-register event-offset cycle-offset]` triples; a frame stores its
    base event/cycle, pattern id, and non-blacklisted value vector. Readers:
    `open`, `status`, `frames`, `configurations`, `expand-frame`, `vic-writes`;
    blacklisted values expand with `:value nil` and `:value-known? false`.
  - Added `omkamra.vice.assets`. It consumes the video configuration points,
    replays exact underlying RAM and color RAM through `reader/replay-state`, and
    writes a fixed 65536-byte RAM + 1024-byte color-RAM payload per snapshot to
    an append-only `assets.bin`. The EDN index records payload offset/length,
    the VIC configuration, and the screen, charset, color-RAM, sprite-pointer,
    sprite-data, and bitmap ranges. Readers: `open`, `status`, `snapshots`,
    `read-payload`, `snapshot-ram`, `snapshot-color-ram`.
  - Added `omkamra.vice.audio`. It scans exact SID writes, joins them with
    the `:dedupe` block/context mappings by event index with a two-pointer walk
    over occurrence fragments, and emits compact evidence only: total SID-write
    count, counts by context/block/instruction PC/canonical register, sustained
    logical units and runs, and candidate `:music` spans. No SID values or write
    history are retained. Readers: `open`, `status`, `evidence`,
    `music-candidates`.
  - Reader enhancement: inferred logical writes and accesses now carry
    `:global-cycle`, `:raster-line`, and `:cpu-cycle`, as required by the §5.1
    write API and used by `:video` frame derivation.
  - Added `omkamra.vice.domains-test`.
- **Tests run:**
  - Full fast v2 suite (`contract`, `capture`, `snapshot`, `reader`, `dedupe`,
    `domains`): 36 tests, 246 assertions, 0 failures.
  - Synthetic domains test: three VIC frames with a blacklisted `$d020` and one
    `$d018` layout change intern to one structural pattern and expand back to
    twelve writes with the blacklisted values explicitly unknown; the initial
    and layout-change configurations carry the expected charset bases; assets
    produce two 66560-byte snapshots with the expected ranges; six SID writes
    join to block/context/register evidence with a sustained run and a music
    candidate.
  - Acceptance prefix (450,000 events): video derives 70 frames, 6 structural
    patterns, 205 configuration points (4 distinct layouts cycling), 6376 VIC
    writes with 5408 blacklisted values omitted; assets produce 205 snapshots
    for 13,644,800 payload bytes; audio builds and joins. The prefix's music has
    not started (1 SID write), so the acceptance audio check is structural.
  - `clj-kondo` 0 errors/0 warnings; `cljfmt check` clean for all new files.
- **Observed issues:**
  - `:video` emits a configuration point for every effective layout change, so
    a demo that toggles screen/charset bases per raster effect produces many
    snapshots (205 for the 450k prefix, only 4 distinct layouts). This is
    faithful to "every effective asset-layout change" but heavy; a future
    configurable coalescing policy must be a versioned extension, not an
    implicit behaviour change.
  - The acceptance prefix ends before the demo's music starts, so `:audio` sees
    only one SID write there; the synthetic test is the meaningful audio
    validation. A later phase should run audio over a range that contains the
    music routine.
  - Stage throughput is low because `:video`/`:audio` replay memory through the
    reader and every stage is single-threaded. This is recorded in the
    **Open follow-ups** section: investigate bounded parallel stage execution,
    shared scan fan-out between exact consumers, and parallel test execution so
    the work uses all CPU cores.
- **Decisions or deviations:**
  - Frame boundaries are derived from VIC-write raster-line wraps; the frame
    base is the first VIC write of the frame rather than a continuous event
    scan. Exact per-write timing remains in the capture.
  - Asset snapshots store the replay's exact low-nibble color RAM; VICE's
    high-nibble bus garbage is not modelled.
  - `:assets` replays at `(inc config-event-index)` so the layout-changing
    write is included in the snapshot.
  - The video frame catalog is EDN for inspection; if real captures make it too
    large it can move to a binary window without changing the reader API.
- **Next step:** Phase 6 — `:features` and `:classification`.

### Diary entry: Phase 6 — `:features` and `:classification`

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.features`, a streaming evidence builder over exact
    writes and `:dedupe` occurrences. It partitions execution into mainline
    fixed windows (counting only mainline-context events, so interrupt time
    neither consumes nor dilutes a window) and IRQ invocations (one per
    interrupt entry through its `RTI`). Each unit carries write-domain counts,
    an ordinary-RAM destination footprint (page bitmap, unique count, range,
    page count/min/max), code-overwrite and hard write-to-execute handoff
    records, serial/IEC activity, VIC/SID writing PCs, and KERNAL IEC execution
    counts. IRQ invocations additionally keep per-occurrence evidence.
  - Added `omkamra.vice.classification`, a pure ordered consumer. It assigns
    composable roles (`:raster`, `:music`, `:loader`, `:decruncher`, `:idle`)
    from unit evidence plus `:audio` music candidates, applies per-role
    hysteresis over the unit sequence, merges mainline windows into
    non-overlapping spans, always starts a new span at a hard write-to-execute
    handoff, and splits IRQ invocations at block-occurrence boundaries wherever
    the role set changes. Empty-role spans are not emitted.
  - Added a `features-classification-test` namespace: a synthetic VIC/SID
    capture with a mixed IRQ handler, plus direct `classify` tests for role
    assignment, hysteresis, hard handoffs, and IRQ sub-span splitting.
  - Per project-owner direction, removed phase numbers from code identifiers
    and docstrings; the phase vocabulary now lives only in this plan document.
    The former `phase6_test.clj` is now `features_classification_test.clj`.
- **Tests run:**
  - Full v2 suite (contract, capture, snapshot, reader, dedupe, domains,
    features/classification): 41 tests, 271 assertions, 0 failures.
  - The new namespace verifies mainline-only window counts, an IRQ invocation
    with per-occurrence evidence, VIC/SID attribution, handoff presence, role
    assignment from the hardware write mix, hysteresis delaying role entry,
    handoff splitting a same-role window, IRQ occurrences splitting into
    raster/music sub-spans, and an end-to-end features→classification build.
  - Acceptance prefix (450,000 events): 21 units (18 mainline windows, 3 IRQ
    invocations), 11 handoffs, and 17 spans with role counts
    `{:raster 13 :idle 3 :decruncher 2}` (14 mainline, 3 IRQ). The prefix still
    precedes the demo's music and loader phases, so `:music`/`:loader` are not
    exercised there.
  - `clj-kondo` 0 errors/0 warnings; `cljfmt check` clean for all v2 sources.
- **Observed issues:**
  - Write-to-execute handoffs treat any mainline block whose entry PC was
    written since its previous execution as a handoff. The design names direct
    `JMP`/`JSR`/resolved indirect `JMP`; the broader rule also catches
    fall-through into freshly written code, which is in the same spirit but is a
    documented approximation.
  - The acceptance prefix ends before the demo's music/loader routines, so the
    real `:music`/`:loader` roles are not exercised; the synthetic capture covers
    the role logic instead. A later validation should classify a range that
    contains those phases.
  - `:features` scans writes once per dedupe window (to keep one window of
    occurrences in memory), so it re-replays from the raw chunk boundary for
    each window. This is part of the tracked stage/test parallelism follow-up.
  - `:video` is not read directly by `:features`/`:classification`; exact VIC
    evidence already comes from the same raw writes. The video image remains the
    source for asset layout and can be fused into classification when a raster
    role needs VIC-configuration identity.
- **Decisions or deviations:**
  - Roles are decided only in `:classification`; `:features` emits neutral
    evidence, per the design.
  - IRQ handler sub-spans snap to block-occurrence boundaries and group
    consecutive occurrences with the same role set; hysteresis is applied to
    the mainline unit sequence (the design's neutral-window projection).
  - Hard handoffs are honored without hysteresis, matching the design.
- **Next step:** Phase 7 — `:segments`.

### Diary entry: Phase 7 — `:segments`

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.segments`, a pure assembly over `:classification`
    spans and `:features` handoffs. Mainline spans merge into `:demopart`
    epochs (raster/music) and split at `:loader`/`:decruncher`/`:idle`/
    `:transition` roles and hard handoffs; IRQ spans attach to the containing
    segment as `:irq-roles`; file groups split at each `:loader` transition; the
    boundary audit reports covered/uncovered ranges, overlaps, hard handoff
    alignment, and per-role evidence. All descriptors use global event
    coordinates and contain no chunk-local details.
  - Added `segments-test` (pure assembly/audit plus a guarded acceptance).
- **Tests run:**
  - Full suite: 44 tests, 287 assertions, 0 failures.
  - Acceptance prefix (450,000 events): 14 segments (12 demopart, 2
    decruncher), 1 file group, 1 uncovered startup gap `[0 25000)`, 0
    overlaps, and 11 hard handoffs all aligned with segment boundaries.
- **Observed issues:**
  - The uncovered startup gap is the boot/autostart window. Its feature unit
    roles are `#{:loader :raster}`, but two-unit hysteresis delays both roles
    until the second unit, so no span is emitted for the first window and the
    audit correctly reports it as uncovered. A future option could seed
    hysteresis from the first unit.
  - Demopart epochs are fragmented by hard handoffs (each starts a new
    segment), so the assembly does not yet recover whole file/demopart groups;
    loader evidence was also incomplete (see below).
  - The acceptance prefix ends before the demo's fast-loaders, so file grouping
    is not exercised there.
- **Decisions or deviations:** all semantic decisions are persisted as global
  event ranges; nothing references a raw chunk.
- **Next step:** validate on a full capture.

### Diary entry: stage parallelism and serial-read loader evidence

- **Status:** complete.
- **Changes:**
  - Added `omkamra.vice.parallel/pmap-ordered`, an ordered worker-pool map
    that preserves input order (identical results to a sequential `mapv`).
  - `:features` was split into a parallel extraction phase (scan/attribute one
    dedupe window's accesses) and a cheap sequential assembly phase; windows
    are processed in bounded batches of `2 x parallelism`.
  - `:assets` computes its per-configuration payloads in parallel batches and
    writes them sequentially to preserve the append-only offsets; `:audio`
    computes its per-window SID evidence in parallel batches.
  - Added serial-read (`$dd00-$ddff`) evidence to `:features` and used it,
    together with serial writes, for the `:loader` role. Loaders sample the
    serial bus by *reading* CIA2 port A, which the previous write-only scan
    missed.
  - Added `analysis/start-async!`/`report!`/`cancel-async!`, and
    `dedupe/build-async!`/`features/build-async!`: long stages now run on a
    background future and return a deref-able handle atom carrying `:status`,
    `:processed-events`/`:total-events` (dedupe) or
    `:processed-windows`/`:windows-total` (features), and, on completion,
    `:result`. The handle carries `:future` so a run can be cancelled. This
    replaces polling the analysis directory for progress.
  - Added `parallel-test` and a features parallel/sequential determinism test.
  - Fixed a long-capture memory bug in `:dedupe`: `process-all!` bound the lazy
    `reader/event-pairs` sequence to a local, so the realized head retained every
    decoded event. Long captures grew heap linearly until OOM (observed at
    ~7 M events) and GC throttled throughput. Passing the lazy sequence
    directly to `reduce` keeps peak memory bounded.
- **Tests run:** full suite in progress; synthetic feature/asset/audio tests
  and the parallel/determinism tests pass.
- **Observed issues:**
  - `:dedupe` remains a single sequential pass, but after the lazy-sequence fix
    it runs at roughly 41 k events/s with bounded memory; a 43 M-event build is
    about 17 minutes. Parallelising `:dedupe` (or pipelining decode/process) is
    still an open optimisation.
  - `:video` still scans writes in one pass and `:assets` snapshots every
    effective layout change; a toggling demo produces many snapshots.
- **Next step:** dedupe hot-path optimisation, then the full-capture
  reconstruction run.

### Diary entry: full-capture performance observations

- **Status:** partial. `:dedupe` now completes on the full
  43,220,373-event capture; the downstream `:features` stage does not yet fit in
  the 4 GB JVM heap, so the complete segment reconstruction is still open.
- **`:dedupe` full-capture results:** 43,220,373 events → 10,265 instructions,
  2,892 blocks, 9 contexts, 88 logical windows, 15,313,197 occurrences,
  42,615 context switches, 3,636 port changes. Wall clock ~28.5 minutes
  (~25 k events/s). Two bugs had to be fixed first:
  - `process-all!` bound the lazy `reader/event-pairs` sequence to a local, so
    the realized head retained every decoded event; long captures grew heap
    until OOM (observed near 7 M events). Passing the lazy sequence straight to
    `reduce` keeps peak memory bounded.
  - The mainline-boundary window flush requires depth one with no open block.
    During long stretches with a suspended mainline (for example a raster
    effect running mostly inside an IRQ context) that condition rarely holds, so
    the window buffer grew without bound and OOM'd. A 200 k-occurrence safety
    valve now force-closes open occurrences and flushes. This is a deliberate
    deviation: a forced split creates two occurrences of the same block and
    omits the CFG edge between them.
- **`:features` full-capture result:** OOM at window 16 (extract batch 16),
  window 20 (batch 4), and window 20 (batch 1). Reducing the extraction batch
  from 16 to a single window did not fix it, so the growth is not only the
  per-window raw access maps; the stage also accumulates every unit in memory
  and writes one EDN index at the end. Both need to become streaming.
- **Performance follow-ups (all open):**
  - `:features` must stream: derive per-occurrence evidence without retaining the
    raw access maps, and spill units to per-window files instead of a single
    in-memory EDN index. `:classification`/`:segments` then consume those
    windows. A large heap only postpones the problem.
  - `:dedupe` remains a single sequential pass (~25 k events/s); parallelise it
    or pipeline decode and state processing.
  - `:video` remains a single write scan; split by raw chunk with
    boundary-frame merging.
  - `:assets` snapshots every effective layout change; a toggling demo produces
    very many snapshots (a coalescing policy would be a versioned extension).
- **Decisions or deviations:** the window memory valve trades occurrence/CFG
  exactness at artificial boundaries for bounded memory; it should be replaced
  by a streaming window format that can split fragments across windows.
- **Next step:** make `:features` (and the unit consumers) streaming, then rerun
  the full reconstruction.

### Diary entry: bounded-memory `:features`/`:classification` streaming and full-capture reconstruction

- **Status:** complete. `:features` no longer OOMs, and the full 43.2 M-event
  acceptance capture now reconstructs through `:features`, `:classification`,
  and `:segments` under a bounded heap. The reconstruction also exposed the next
  blocker: a `:dedupe` execution-context closure defect.
- **Changes:**
  - `omkamra.vice.features` (version 2) now streams its output. Units are
    written to `units.edn` and handoffs to `handoffs.edn`, one EDN record per
    line, as they close; `index.edn` keeps only the small
    format/configuration/summary and the two file names. `build!` opens both
    writers for the generation and never retains a unit list. A test-only
    `:max-events` bound restricts the dedupe windows considered.
  - Per-window access extraction no longer materialises the whole access vector:
    `scan-window-accesses` feeds a monotonic `attribute-access!` reducer that
    assigns each chronological read/write to the covering occurrence fragment
    and accumulates the per-occurrence `:writes`/`:reads` directly. The result
    is byte-for-byte identical to the former two-pass `attribute-writes` (checked
    on the full first dedupe window: 11,516 write-occurrences and 5,248
    read-occurrences match exactly).
  - Added an IRQ-invocation memory valve. An IRQ invocation that never returns
    (or whose `RTI` is never recorded) would otherwise accumulate every
    subsequent occurrence in one unit; the full capture contained a single
    `[:irq 65352]` invocation spanning 2.4 M events whose one EDN line was
    518 MB. `:max-invocation-occurrences` (default 50,000) and
    `:max-invocation-events` (default 500,000) force-close the invocation and
    start a continuation with the same context id, which is emitted when the
    handler finally returns. The same invocation now yields ~33 MB lines.
  - `omkamra.vice.classification` (version 2) consumes the streamed units
    through a single `features/reduce-units` fold. `stabilize` became the
    incremental `advance-role-state` (roles are tracked from first appearance,
    which is equivalent to the batch all-roles union); `mainline-spans` became
    the streaming `mainline-step`/`flush-mainline`; `classify` stays as an
    in-memory convenience and shares the same step function. No unit is retained
    after it is folded.
  - Readers: `features/reduce-units`/`units`/`handoffs`,
    `features/open` returns only `:index`/`:directory` (no eager unit list).
- **Tests run:**
  - The seven guarded-synthetic tests in `features-classification-test`
    (`synthetic-features`, `features-parallel-determinism`,
    `classification-roles-and-hysteresis`,
    `classification-hard-handoff-and-irq-splitting`,
    `synthetic-features-classification`) and the two pure
    `segments-test` tests (`assemble-segments`, `boundary-audit`) pass through
    the project nREPL.
  - Streaming-equivalence check against the former attribution algorithm on the
    full first dedupe window: identical write/read occurrence maps.
  - Full-capture builds on `captures/no-booze-triad-v2` (8 cores):
    `:features` ~13 minutes, 88 windows → 21,650 units (65 mainline windows,
    21,585 IRQ invocations, 185 handoffs), 9,694,502,431-byte `units.edn`,
    8,708-byte `handoffs.edn`; `:classification` ~20 minutes → 58,283 spans
    (151 mainline, 58,132 IRQ; roles `{:idle 58130, :loader 7129, :raster 12,
    :decruncher 127}`); `:segments` ~2 seconds → 140 segments
    (`{:demopart 1, :decruncher 127, :idle 12}`), 1 file group.
  - Heap after the full run, after `System/gc`: **76 MB used / 3,966 MB max**.
    The previous code OOM'd at window 20 with a 4 GB heap.
  - `clj-kondo` 0 errors/0 warnings and `cljfmt check` clean for the two edited
    namespaces.
- **Observed issues:**
  - **`:dedupe` execution-context closure (new blocker).** The full dedupe image
    has 9 contexts; context 1 is `[:irq 65352]` (`$FF48`, the KERNAL IRQ entry).
    It accumulates one invocation from event ~3.34 M onward whose recorded
    return is never seen, so after ~9.4 M events `:features` sees essentially
    only IRQ units. `:classification` therefore emits 58,132 IRQ spans and only
    151 mainline spans, and `:segments` (which assembles mainline epochs) covers
    only `[375016 9431761]` and leaves `[9431761 43220373)` uncovered. The
    demo's decruncher/demopart/fast-loader structure lives in that tail and is
    not reconstructed. The `:loader` role is detected (7,129 spans) but almost
    entirely inside IRQ spans, so no `:loader` segments/file groups are produced
    beyond the single prefix group.
  - **Unit-file size.** Verbose EDN per-occurrence evidence makes the full
    `units.edn` ~9.7 GB and classification spends ~20 minutes parsing it. Memory
    is bounded, but a compact per-occurrence projection or binary unit channel is
    warranted before treating the stage as final.
  - The `:features` test-only `:max-events` bound is window-granular (it keeps a
    whole dedupe window whose start precedes the bound), unlike `:dedupe`'s exact
    bound. This only affects prefix testing.
- **Decisions or deviations:**
  - The invocation valve trades exact one-unit-per-invocation identity at
    artificial caps for bounded memory; this mirrors the dedupe occurrence valve
    and is a deliberate deviation. A continuation invocation keeps the original
    context id so role classification is unaffected.
  - `:features` and `:classification` versions were bumped 1 → 2 for the changed
    on-disk/consumption contract. `:dedupe` and `:segments` versions are
    unchanged.
- **Next step:** fix `:dedupe` interrupt-return/context-pop detection so the
  KERNAL IRQ context closes and mainline execution resumes; then rerun
  `:features`/`:classification`/`:segments` and confirm the three-file,
  decruncher/demopart/fast-loader structure. In parallel, move the `:features`
  unit channel to a compact format.

### Diary entry: IRQ handler resolution and per-frame mainline promotion

- **Status:** complete. The "KERNAL IRQ context never closes" symptom was traced
  to real execution, not a detection bug: the demo runs its main program inside
  the IRQ handler and never RTIs. `:dedupe` now resolves the effective IRQ
  handler and promotes long-lived IRQ frames to the mainline timeline; the full
  capture reconstructs with mainline coverage over the whole run.
- **Diagnosis (capture evidence):**
  - Counting events by context showed `:main` = 1,616,575 and
    `[:irq 65352]` (`$FF48`) = 38,390,639 of 43.2 M (89%). The stack-depth
    timeline has only two long `depth > 1` spans: `[3343809 9420711]` (6.08 M)
    and `[11293382 EOF]` (31.9 M).
  - The stack-pointer model is exact: over a 2 M-event sample and over the whole
    31.9 M-event tail, consecutive-event SP deltas were only `0` and `-3` (the
    hardware interrupt/BRK signature). Every entry is a real interrupt and every
    RTI is recognized (19,739 RTIs vs 19,739 `:return`s in the tail).
  - The kernel entry `$FF48` trampolines through the user vector `$0314/$0315`,
    and that target changes: `$EA31` 32, `$1100` 540, `$1300` 414, `$1800` 125,
    `$23B8` 466, `$2071` 431. Keying a context on the first PC merged those
    distinct routines. The engine frame at 3,343,809 dispatches to `$1300`; the
    one at 11,293,382 dispatches to `$23B8`.
  - Whole-context promotion would have relabelled ~270 k occurrences / ~1.7 M
    events of the same context as mainline, so a per-frame mechanism was used
    instead.
- **Changes:**
  - `omkamra.vice.dedupe` (version 2):
    - Tracks direct stores to `$0314/$0315`, `$0316/$0317`, `$0318/$0319`, and
      `$FFFA-$FFFF` the same way it tracks `$01`.
    - `resolved-handler` uses the `$0314` target for an entry at `$FF48` (the
      physical `$FFFE/$FFFF` target), the `$0316` target for BRK, and the entry
      PC otherwise; contexts are keyed `[kind resolved-handler]`. The full image
      now has 20 contexts instead of 9.
    - Each frame records `:kind`/`:entry-index`. A non-main frame older than
      `:max-irq-invocation-events` (default 50000, `initial-state` and `build!`
      option) is marked promoted; its `[entry end)` range is recorded in
      `index.edn` as `:context-overrides` when it ends (RTI, `close-all!`, or
      EOF). The full image records 10 overrides, e.g. context 3 `$1300`
      `[3343809 9420711)` and context 6 `$23B8` `[11293382 43220373)`.
    - `open` loads `:context-kinds` from `contexts.bin`; `read-range` tags every
      occurrence `:context-kind` (`:main`, `:irq`, or `:mainline` for an
      override range). This is retroactive because contexts/overrides are written
      after the build and occurrences store only the context id.
  - `omkamra.vice.features` (version 3): groups occurrences as mainline when
    `:context-kind` is `:main` or `:mainline` (previously `(zero? context-id)`).
  - `omkamra.vice.contract`: added `format-keyword`, moved `dedupe-version`
    here, and derived `dedupe-format` from it. `:features`/`:classification`/
    `:segments` derive their formats from their own versions, fixing the earlier
    `classification-v2`-with-`classification-version-3` drift:
    `features-v3`, `classification-v3`, `segments-v1`.
- **Tests run:**
  - Fast v2 suite (contract, capture, snapshot, reader, dedupe, domains,
    features/classification, segments, parallel), acceptance tests excluded: 41
    tests, 252 assertions, 0 failures.
  - The three `^:acceptance` tests: 3 tests, 16 assertions, 0 failures.
  - Fixed a latent bug found by the tests: the streaming `mainline-step` used
    `update` instead of `update-in` for `[:current :event-range]`, so merging
    adjacent same-role windows turned `:current` into the raw range and dropped
    the span.
  - Prefix validation (`:max-events 4000000`): context 3 `$1300` occurrences
    before 3,343,809 are `:irq` (34,715) while `[3343809 ...)` are `:mainline`,
    confirming the override is per-frame; mainline coverage extends through the
    engine frame.
  - Full capture: `:dedupe` ~25 min → 20 contexts, 10 overrides; `:features`
    ~10 min → 22,125 units (814 mainline windows, 21,311 IRQ invocations, 2,771
    handoffs, 3.84 GB `units.edn`); `:classification` ~10 min → 49,977 spans
    (2,026 mainline, 47,951 IRQ; roles `{:idle 48464, :raster 396,
    :decruncher 900, :loader 488}`); `:segments` ~26 s → 1,761 segments
    (`{:demopart 6, :decruncher 754, :idle 513, :loader 488}`), 489 file groups,
    0 overlaps, 0 unaligned hard handoffs.
- **Observed issues:**
  - **File-group over-fragmentation (new follow-up).** `:segments` splits file
    groups at every `:loader` transition, and `:loader` fires on a single serial
    read, so the three-file demo yields 489 groups. The role/segment grouping
    needs threshold tuning before `:disassembly`.
  - **Large uncovered gaps.** `:segments` only covers role-tagged mainline spans,
    so neutral mainline stretches (e.g. `[8011192 11348274]`,
    `[28250120 29777808]`) remain uncovered. This is expected for the current
    assembly rule but should be reviewed against the demo's structure.
  - `units.edn` shrank from 9.7 GB to 3.84 GB because the engine frames are now
    mainline (bounded per-window evidence instead of huge IRQ invocations), but
    the compact-encoding follow-up still stands.
  - `:video`/`:assets` were not rebuilt against the new `:dedupe` generation.
- **Decisions or deviations:**
  - Promotion is per-frame and event-range scoped; a context's `:kind` and
    `handler-pc` are never changed, so the IRQ origin is preserved.
  - The cap is `:max-irq-invocation-events` default 50000; the observed normal
    handler p99.9 is 5,613 events and the smallest promoted frame is 92,917, so
    the default separates them cleanly for this capture.
  - The physical vector is always the first executed PC, so only the KERNAL
    trampoline needs `$0314` resolution; `$FFFE/$FFFF` tracking is still kept for
    the direct-vector case and future use.
  - Stage format keywords are derived from schema versions rather than
    hardcoded, so they cannot drift.
- **Next step:** tune `:classification` loader evidence and `:segments` file
  grouping so the three-file, decruncher/demopart/fast-loader structure is
  recovered, then start Phase 8 (`:disassembly`).

### Diary entry: correctness/performance stabilization and three-file recovery (2026-10-11)

- **Status:** partial. The reader, feature, asset, segment, and acceptance-test
  hot paths are substantially faster and several correctness defects are fixed.
  The full acceptance capture now yields exactly three file groups, but only five
  demopart epochs; the expected 3–4 demoparts per file are not yet recovered.
- **Changes:**
  - Fixed `reader/scan-accesses`: ordinary memory-reading instructions now emit
    their effective-address reads (including addressing-mode pointer reads),
    rather than exposing reads only for RMW/stack/control-flow instructions.
    Added `scan-filtered-accesses`, which filters before allocating read records,
    so domain consumers do not pay for unrelated reads.
  - Removed a major reader hot-path cost found with `clj-async-profiler`.
    `run-replay` now carries the current chunk bytes and decoded lookahead event
    through its loop instead of resolving the chunk, hitting the synchronized
    LRU atom, and decoding the same next event again for every event.
    `event-pairs` likewise loads each chunk byte array once. In the profile,
    `event-at-index` was inclusive in ~47% of samples and LRU/cache atom work in
    ~27%; the new loop removes that per-event cache traffic.
  - Added `reader/scan-replay-states`/`replay-states`. Ordered target indexes in
    one raw chunk now share one forward replay from its exact snapshot.
    `:assets` streams those shared states directly to `assets.bin` rather than
    launching one independent replay per configuration point. The 205-snapshot
    acceptance-prefix build fell from ~45 seconds to ~4–5 seconds, and the old
    and new `assets.bin` files have the same SHA-256
    (`f5a5a5e742860b5e4be4b1ed08904fc3a60279471fab46c72275f9268f7c1ee3`).
  - Corrected loader evidence. CIA2 writes count as serial output only for IEC
    output bits 3–5, not VIC-bank bits or nonexistent `:previous-value` data.
    Reads count only when the resolved domain is CIA2. Classification no longer
    labels one noisy serial read or four incidental KERNAL instructions as a
    loader: defaults are now 32 serial accesses and 64 KERNAL IEC executions.
    Loader confidence now includes reads and KERNAL evidence.
  - `:features` version 4 persists a sparse role-relevant projection for each
    IRQ occurrence instead of repeating footprint, domain, and per-PC maps.
    Serial reads are counted while streaming instead of retaining millions of
    read maps in per-occurrence vectors. Full unit evidence remains available at
    unit level.
  - Promoted-mainline feature windows retain their actual context id and close
    when the effective mainline context changes. Previously every such window
    was incorrectly written as context 0.
  - `:segments` version 2 sorts all effective-mainline spans by event time,
    preserves every contributing context id, and closes a file only after a
    contiguous loader run. Adjacent loader fragments caused by hard handoffs no
    longer manufacture hundreds of files.
  - Replaced the quadratic segment/IRQ overlap join with an ordered interval
    sweep. On the full 76,521-span classification, `:segments` fell from ~40.6
    seconds to ~1.49 seconds with byte-for-byte-equivalent segment, file-group,
    and audit values.
  - Added a shared isolated acceptance fixture. It symlinks the immutable
    capture into a temporary dataset, builds the 450 k-event dependency chain
    once, and shares the resulting stage readers across test namespaces. Tests
    no longer replace the full-capture manifest or rebuild dedupe/features in
    three namespaces. Nine independent chunk-continuity checks run four-way
    parallel.
- **Tests run:**
  - Complete non-live v2 suite through nREPL: 47 tests, 305 assertions, 0
    failures/errors. A clean run with a newly built isolated acceptance fixture
    took ~37.0 seconds; an immediate rerun took ~6.1 seconds. The parallel
    full-capture continuity test alone took ~5.46 seconds.
  - Reader tests cover ordinary `$DD00` loads, shared multi-target replay versus
    independent `replay-state`, unordered-target rejection, and all previous
    write/replay invariants.
  - Synthetic feature/classification tests verify sparse occurrence evidence and
    loader-noise thresholds. Segment tests verify one boundary for adjacent
    loader fragments, chronological promoted contexts, and retained context ids.
  - `clj-kondo` reports 0 errors/0 warnings; `cljfmt check` is clean for all v2
    sources.
  - Full acceptance reconstruction with eight feature workers:
    - `:features` ~5 m 48 s (formerly ~10 m) -> 22,135 units: 824 mainline,
      21,311 IRQ, 2,771 handoffs; `units.edn` 701 MB (formerly 3.84 GB).
    - `:classification` ~104 s without audio and ~102 s with full audio
      (formerly ~10 m parsing verbose units).
    - `:audio` ~89 s -> 100,647 SID writes, 63 sustained units, 7 sustained
      runs, and 61 candidates.
    - optimized `:segments` ~1.49 s -> 1,604 segments and exactly 3 file groups.
- **Full-capture result:**
  - The two sustained loader runs terminate file groups near events 7,991,630
    and 26,602,509. The resulting three groups cover the expected three-file
    order instead of the former 489-way fragmentation.
  - Current segment counts are `{:demopart 5, :decruncher 810, :idle 504,
    :loader 285}` with 27 uncovered ranges, no overlaps, and all 2,771 hard
    handoffs aligned to segment boundaries.
  - Full audio fusion produces 76,521 spans and music evidence, but currently
    applies audio candidate sets only while splitting IRQ invocations; mainline
    windows ignore those sets. Consequently audio does not yet improve
    demopart assembly.
- **Observed issues:**
  - Five demopart epochs are fewer than the demo's expected 9–12. Neutral gaps,
    fragmented decruncher spans, and missing mainline audio fusion are the main
    remaining semantic blockers before disassembly.
  - The sparse EDN stream is much smaller but 701 MB is still large because it
    stores maps for roughly 15.3 M occurrences.
  - Full `:video`/`:assets` generations were not rebuilt; their manifest entries
    still refer to the 450 k-event prefix.
  - `:dedupe` remains a persistent-map-heavy sequential pass. The chunk-byte
    cache fix benefits its event source, but a new full 43.2 M-event dedupe
    benchmark was not run in this pass.
- **Decisions or deviations:**
  - Exact access completeness remains in the shared reader; feature-level
    filtering occurs only through the explicit filtered API.
  - Loader thresholds were selected from the full capture: incidental units had
    only 1–8 serial/KERNAL hits, while the two fast-loader runs had hundreds to
    thousands per 25 k-event mainline window.
  - Acceptance analysis is isolated rather than parallel writes to one manifest;
    this preserves test value while removing races and duplicate dependency
    builds.
- **Promising future work:**
  1. Feed audio block/context candidates into mainline `unit-roles`, and let
     sustained IRQ raster/music activity help identify the containing demopart.
  2. Assign neutral mainline gaps to neighboring epochs using hard handoffs and
     decruncher->execute transitions, then add a full-capture assertion for
     three files with 3–4 demoparts each.
  3. Replace EDN occurrence maps with a binary delta/run channel (context/block
     ids, event ranges, and sparse role counters); occurrence ids can likely be
     implicit. This should cut both 701 MB and the remaining ~102 s parse time.
  4. Profile and rewrite the `:dedupe` event fold around primitive/mutable
     window builders, or implement a decode/process pipeline before attempting
     chunk-parallel deterministic merge.
  5. Split `:video` by raw chunk with boundary-frame merging, then rebuild full
     `:video`/`:assets`; consider versioned asset-layout coalescing after exact
     output is established.
  6. Add dependency signatures, stage reuse, an OS-visible writer lock, and
     stale-generation pruning so tests/runs can reuse valid products safely.
- **Next step:** correct mainline music/neutral-gap part assembly and verify the
  expected 3–4 demoparts inside each of the now-correct three file groups; then
  begin Phase 8 disassembly.

### Diary entry: explicit execution frames and timeline leveling (2026-10-11)

- **Status:** implemented and unit-validated; full-capture re-validation blocked.
  `:dedupe` now models contexts, frames, and levels as three separate things and
  owns frame leveling as an auditable annotation instead of an implicit
  context-kind override. All non-acceptance tests pass, including precise
  synthetic leveling tests. The acceptance capture was accidentally deleted
  mid-session (see below), so the previous full-capture numbers could not be
  reproduced in this pass.
- **Motivation:** the earlier "promotion" mechanism conflated three ideas. A
  *context* is a reusable structural identity (`[entry-kind handler-pc]`), a
  *frame* is one dynamic invocation, and a *level* is how the mainline/interrupt
  timeline should interpret a frame range. Promotion mutated the whole frame's
  effective context kind, was keyed only by context id, and offered no audit
  record of where the change began.
- **Design decisions:**
  - Leveling stays in `:dedupe` because `:dedupe` already owns the inferred
    execution stack, exact interrupt entry, exact `RTI` return, and nested frame
    structure. Moving it later would require re-deriving that state machine from
    persisted fragments.
  - Leveling is an annotation, not a mutation: the context keeps its observed
    kind and identity, and only a frame-scoped range changes level.
  - The IRQ prefix is preserved. Leveling begins at `:level-start` (where the
    frame first exceeds the threshold), so the handler's entry/prologue stays
    IRQ and only the persistent continuation joins the mainline timeline.
  - Each frame instance gets a stable `:frame-id`; context ids alone cannot
    distinguish repeated or nested invocations of one handler.
- **Changes:**
  - `omkamra.vice.contract`: `dedupe-version` 2 -> 3.
  - `omkamra.vice.dedupe` (version 3):
    - Every inferred interrupt/BRK entry allocates a frame with `:frame-id`
      from a monotonic `:frame-counter`; the main frame is id 0. Entry and
      return switches encode the frame id.
    - Occurrence records gained `:frame-id`; the occurrence channel record grew
      from a 16-byte to a 20-byte header, and the switch record from 16 to 24
      bytes (frame id + context id).
    - `:max-irq-invocation-events`/`:context-overrides`/promotion were removed.
      `:leveling-threshold-events` (default `50000`,
      `default-leveling-threshold-events`) replaced them; the option is read
      from `:configuration`, matching the previous option's plumbing.
    - `level-overflow!` runs before each event: once a non-main frame exceeds
      the threshold it force-closes the open occurrence at that exact index and
      records `:level-start`, then re-derives the frame from the *post-close*
      state. (The first implementation re-applied a stale pre-close frame
      snapshot, re-emitting the closed block; the synthetic test caught it.)
    - `frame-leveling` produces the audit map persisted as `index.edn`
      `:levelings`; `exit-context!` and `close-all!` emit it so an unterminated
      frame is still annotated at EOF.
    - `read-range` tags each occurrence with `:effective-level` (`:mainline` for
      the main context or a leveled frame range, otherwise `:irq`) while leaving
      `:context-kind` as the observed kind, and exposes the levelings whose
      continuation starts inside the range. Levelings are filtered once against
      the query range, not per window (a per-window filter duplicated them).
  - `omkamra.vice.features` (version 4): mainline routing now uses
    `:effective-level`; mainline windows are keyed by `:frame-id` and carry the
    observed `:context-kind`; IRQ invocations are matched by frame id; every
    unit carries `:frame-id`, `:context-kind`, and `:effective-level`.
  - `omkamra.vice.classification` (version 4): spans carry `:effective-level`
    (the timeline selector) in addition to the observed `:context-kind`; mainline
    merging also requires a matching frame id.
  - `omkamra.vice.segments` (version 2, unchanged format): assembly selects
    mainline/IRQ spans by `:effective-level` instead of `:context-kind`.
  - `omkamra.vice.store`: `delete-tree!` no longer follows symbolic links.
- **Tests run:**
  - Full non-acceptance v2 suite through nREPL, including the new `store_test`:
    53 tests, 286 assertions, 0 failures/errors. (The 29-assertion difference
    from the previous 315 is the skipped acceptance tests.)
  - New `dedupe_test/synthetic-frame-leveling`: with
    `:configuration {:leveling-threshold-events 1}` the synthetic IRQ frame
    records exactly one leveling `{:frame-id 1 :context-id 1 :observed-kind :irq
    :event-range [2 4] :level-start 3 :effective-level :mainline ...}`, the
    prefix occurrence stays `:irq`, the continuation is `:mainline` while keeping
    `:context-kind :irq`, and the range surfaces the annotation.
  - New `dedupe_test/synthetic-unleveled-frame-is-unchanged`: with the default
    threshold a short IRQ produces no levelings and no `:mainline` occurrences.
  - `features_classification_test` now asserts both `:context-kind` and
    `:effective-level` on split mainline spans; `segments_test` builds spans with
    explicit effective levels and checks a leveled (`:irq` context, `:mainline`
    level) frame contributes to mainline segments.
  - New `store_test`: recursive deletion, symlink-holder deletion leaving the
    target intact, direct-link deletion removing only the link, and missing-path
    tolerance.
  - Real-data prefix probe (450 k events, threshold lowered to 2000): no leveling
    occurs there because the first long-lived frame begins near 3.34 M events, so
    the prefix only exercised the unleveled path.
  - `clj-kondo` reports 0 errors/0 warnings; `cljfmt check` is clean.
- **Data loss (important):**
  - While probing prefix leveling I wrote a throwaway eval that created a
    temporary directory containing a symlink to the acceptance capture and then
    cleaned up with `store/delete-tree!` on the *parent* directory.
    `delete-tree!` used `File.isDirectory`, which follows symlinks, so it
    recursed through the link and deleted `captures/no-booze-triad-v2/capture`
    contents (the raw `manifest.edn` and `chunks/`). The demo input lives on the
    host at `/c64/Triad/r676_No Booze__No_Booze_Triad.d64`, not in the
    repository, and no copy of the capture remains on this filesystem.
  - The committed analysis generations still exist but are unusable: the v2
    `:dedupe` generation is now rejected as a stale format, and downstream stages
    cannot be rebuilt without raw capture access. The acceptance dataset must be
    recaptured before the full-capture numbers are re-established.
  - Root cause fixed: `delete-tree!` is now symlink-safe and covered by
    `store_test`, so the acceptance fixture's own cleanup (which deletes an
    isolated root that links the capture) can never do this again.
- **Findings:**
  - The three bugs the new tests caught were all boundary/state bugs rather than
    schema bugs: a stale-frame re-emission at the leveling point, a leveling
    range that started at frame entry instead of `:level-start`, and per-window
    duplication of trace-global levelings.
  - `:dedupe` retains one feature-side memory valve
    (`:max-invocation-events`/`:max-invocation-occurrences`) as a backstop, but
    leveling now bounds long handlers before that valve is needed.
  - Naming caveat: `:video` already uses `:frame-id` for a raster frame. The two
    never meet in one map, but the overload is worth remembering when reading
    spans and segments.
- **Promising future work:**
  1. Recapture `no-booze-triad-v2` from the host demo and rebuild the full chain;
     re-establish the 20-context/22,135-unit/76,521-span/1,604-segment numbers
     and confirm the number and boundaries of levelings (the old promotion model
     recorded 10 overrides).
  2. Consider persisting a first-class `:promoted-continuation` unit (or at least
     a `:level` field on feature units) so consumers need not join levelings to
     occurrences; also expose level transitions as explicit timeline marks.
  3. Keep the leveling decision piecewise if a frame's access signature changes:
     a handler that first returns quickly and later becomes persistent could be
     annotated per segment rather than once per frame.
  4. Feed audio block/context candidates into mainline `unit-roles`, and let
     sustained IRQ raster/music activity identify the containing demopart.
  5. Assign neutral mainline gaps to neighboring epochs using hard handoffs and
     decruncher->execute transitions; then assert three files with 3–4 demoparts
     each.
  6. Replace EDN occurrence maps with a binary delta/run channel; occurrence ids
     can likely become implicit.
  7. Make raw captures cheap to reproduce and hard to lose: a capture manifest
     checksum plus a documented `capture/start!` recipe in the dataset README,
     and ideally a committed small demo so acceptance runs are self-contained.
- **Next step:** recapture and rebuild the full chain, then resume the demopart
  grouping work before Phase 8 disassembly.

### Diary entry: fresh capture, handler-based demoparts, and a reuse-aware driver (2026-10-11)

- **Status:** success. A fresh full capture was recorded and the whole analysis
  chain rebuilt. The demo reconstructs as **3 file groups and 10 demoparts
  (3 / 3 / 4)**, matching the known part list, with zero gaps and zero
  overlaps. A dependency-graph driver with signature-based reuse was added.
- **Capture:** record from the host demo (read-only mount of the host `c64`
  directory at `/c64`):
  `(capture/start! {:input "/c64/Triad/r676_No Booze__No_Booze_Triad.d64"
  :dataset-root "captures/no-booze-triad-v2" :capture-id "no-booze-triad-v2"
  :event-target 100000})` — i.e. the **default program-entry mode**, not
  `:full-capture? true`. Stopped on request at **47,763,778 events / 463
  chunks** (~32.5 min, ~24.5 k events/s); the trace shows the demo actually ran
  (5,149,643 VIC stores, 140,400 SID stores, 1,343,467 `$dd00` reads).
- **Findings (capture):**
  - With `:full-capture? true` the capture begins before VICE autostart and
    includes the C64 reset/boot and the KERNAL `LOAD"*",8,1`; under monitor-log
    tracing (~25 k events/s) that prefix is minutes of wall time, so the screen
    still shows `READY.` while the capture is running. It should work, but the
    observed run ended parked in the KERNAL IEC wait loop (`$E5CD`–`$E5D4`)
    rather than in the demo. Reproducing or fixing that is a follow-up.
  - The default (non-full) mode stops at the loaded program's entry via a
    `$E144` execute checkpoint, so the capture starts exactly when the demo
    runs. This is the mode the previous acceptance capture must have used.
  - The demo d64 holds exactly three files, matching the three reconstructed
    file groups: `triad/no booze!` (78 blocks), `2. no phone !` (188),
    `3. no party!!!` (103).
- **Findings (classification):**
  - **Effects live in IRQ handlers.** Aggregated over the capture, mainline
    evidence is 6,960 VIC and 218 SID writes, while IRQ evidence is 5,114,491
    VIC and 131,990 SID writes. The user confirmed this: only file 1 part 1
    runs in mainline. Any demopart model built from mainline spans is therefore
    looking at the wrong timeline.
  - **IRQ spans lost their context identity.** `irq-spans` built sub-spans from
    occurrence maps, which carry no `:context-id`/`:frame-id`, so every IRQ
    span had `:context-id nil`. Fixed by attaching the invocation's context and
    frame identity to each sub-span; `classification-version` 4 → 5. This is
    what makes any IRQ-context-based grouping possible downstream.
  - **Mainline windows ignored audio candidates.** `classify-step` called
    `unit-roles` for mainline windows without the audio block/context sets (only
    IRQ invocations received them), so no mainline span could ever carry
    `:music`. Fixed; after the fix every mainline span is music, which also
    showed that `:music` alone is not a useful boundary signal (the audio stage
    flags nearly every context).
  - Raster is detectable at the **invocation** level, not per occurrence:
    5,920 IRQ invocations have ≥32 VIC writes and cover 5,094,134 VIC writes,
    but a single block occurrence rarely reaches the threshold.
- **Demopart model (the main design change):** a part is a stable set of
  *display handlers*, i.e. `:irq`/`:nmi` contexts whose resolved handler is
  outside the KERNAL ROM. The KERNAL default IRQ routine must be ignored, or
  runs fragment (31 vs 13 runs at 250 k buckets). The pipeline:
  1. buckets the logical timeline (250 k events) and marks every bucket each
     display-handler span overlaps;
  2. collapses equal sets into runs, including empty runs (the empty runs are
     exactly the loaders/decrunchers where only the KERNAL IRQ fires);
  3. merges a short (<3 M events) run whose handler set nests inside a
     neighbour's — a transition bridge, so `{7} {7 8} {8}` is one part;
  4. makes each remaining run a segment: a run with handlers or raster activity
     is `:demopart`; a run with no display handler is `:loader`, `:decruncher`,
     or `:idle`;
  5. derives file groups from the loader runs.
  This never relies on activity gaps, so it also works for trackmos that load
  and decrunch inside IRQ handlers alongside the effects. On the fresh capture
  the ten parts are `[0,1M)`, `[1M,3.25M) {2,3}`, `[3.75M,4.75M) {2,5}` (file
  1); `[9.25M,11.75M) {6}`, `[12.75M,23.25M) {7,8}`, `[24.75M,27M)
  {9,10,11,12}` (file 2); `[30.5M,34.25M) {2}`, `[36M,40.25M) {15}`,
  `[40.75M,45M) {16,17}`, `[45M,47.76M) {18,19}` (file 3). `segments-version`
  2 → 3; each segment exposes `:handler-contexts`.
- **Pipeline driver:** `omkamra.vice.pipeline` discovers a requested stage's
  dependency closure from a static graph and reuses a stage when its recorded
  signature still matches. The signature folds the schema version, the
  *normalized* configuration (defaults merged, signature-relevant keys only, so
  inert knobs like `:parallelism` neither force a rebuild nor block reuse), the
  capture manifest checksum, and the upstream signatures. `adopt!` back-fills
  signatures for generations built before the mechanism existed.
- **Findings (driver):**
  - A signature is a pure function of the declared inputs, so a generation
    produced by a *defective* build keeps a matching signature. This bit us: an
    early driver bug passed a nil `:dedupe` image to `:features` (an atom was
    dereferenced in the wrong place), which committed an **empty** features
    generation; because the declared inputs were unchanged, the rebuilt
    (correct) features generation had the *same* signature and downstream
    stages were reused from the empty one. Forcing `:features`, then
    `:classification`, then `:segments` once, restored consistency.
    Stage output validation or an input-provenance check would close this.
  - `:segments` was also reading `(:handoffs features-image)`, which is always
    nil; it now calls `features/handoffs`, so the boundary audit sees the real
    handoffs.
  - Verified reuse: after one build, a second `(pipeline/build! root :segments)`
    reports `:built []` and `:reused [:dedupe :audio :features :classification
    :segments]` in ~7 s (dominated by capture-reader validation).
- **Tests run:** full v2 suite through nREPL — 55 tests, 324 assertions, 0
  failures. New/updated coverage: `segments_test` (handler-set parts, transition
  bridge merging, full-timeline coverage, file grouping, boundary audit);
  `store_test` (symlink safety). `clj-kondo` 0 errors/0 warnings; `cljfmt`
  clean.
- **Promising future work:**
  1. Diagnose `:full-capture? true` (why the captured boot stalls in the KERNAL
     IEC wait instead of reaching the demo) or document it as unsupported.
  2. Extend the driver to accept multiple stages / `:all` and to schedule
     independent branches in parallel, as v1 did. True item streaming between
     stages does not map onto v2's immutable generations.
  3. Harden reuse: validate stage output (e.g. non-empty) and/or record builder
     provenance so a bad generation cannot be silently reused.
  4. Store `:contexts` (kind + handler-pc) in the features index so `:segments`
     need not open `:dedupe` directly; currently it does.
  5. Recompute/retire the audit's `:unaligned-hard-handoff-count`; parts are no
     longer handoff-defined, so handoffs inside a part are expected.
  6. Make the demopart bucketing/bridge parameters (`display-bucket-events`,
     `bridge-max-events`) configurable and validate the handler-set model on a
     trackmo, where effects and loading overlap by design.
  7. `:dedupe` is still a sequential, allocation-heavy fold; the decode/process
     pipeline and mutable-state rewrite remain the main runtime wins.
  8. Rebuild `:video`/`:assets` against the fresh capture (still stale), and
     prune stale generations.
- **Next step:** start Phase 8 (`:disassembly`) over `:dedupe` + `:segments`,
  now that the file/part structure is correct.

### Diary entry addendum: loader/decruncher as roles and handoff-refined part boundaries (2026-10-11)

- **Status:** success. `:segments` v5 unifies `:raster`/`:music`/`:loader`/
  `:decruncher` as evidence *roles* and refines demopart boundaries onto the
  decruncher/loader handoff. Every one of the ten parts now shows its preceding
  transition, and the two boundaries that previously had no visible decruncher
  are exact.
- **Why:** the handler set is the right *boundary* signal (robust for trackmos,
  where loading and decrunching run under live handlers), but treating
  `:loader`/`:decruncher` as partition kinds meant a decruncher that overlapped
  live effects was either absorbed into a part or split one. Making them roles
  fixes that asymmetry.
- **Inspection that motivated the boundary rule:** a decruncher is *not*
  quiescent while it runs. In file 1 part 2's decruncher the CPU:
  1. executes a zero-page copy loop (`$0080-$0083`, `$0039-$0046`: `STA ($41),Y`
     / `DEY` / `LDA $4EA5,Y` / `CMP`/`BEQ`/`BNE`), writing 37,718 distinct
     addresses; then
  2. executes a chain of generated trampolines it wrote (`$0087`–`$0141`,
     `$A6xx`, `$1000`, `$1BA9`, `$9000`, `$93xx`), each one a recorded
     write-to-execute handoff; and finally
  3. jumps into the depacked part code at `$10DD` (handoff index 1,099,681);
     the new display handler (context 2 at `$1100`) fires one event later
     (1,099,682).
  So the handoffs are part of the decruncher's own operation, and the *last*
  one is the part entry. The same holds for file 3 part 4 (`$4088` at
  45,069,071, then context 18 at `$408F` at 45,071,281).
- **Over-extension explained:** classification marked the decruncher as running
  to 1,970,065 / 46,126,398, i.e. ~0.9–1.1 M events into the next part. The
  cause is not the thresholds but the role hysteresis combined with very large
  mainline windows: because the effect runs in IRQ handlers, the mainline
  window `[1104830,1970065)` contains 0 mainline writes yet spans 865 k events,
  so the "decruncher on" state lingers. Trimming loader/decruncher phases at
  their last handoff removes the artefact without touching thresholds.
- **Changes:**
  - `:segments` (v5): `role-phases` now takes the handoffs and returns
    `{:event-range :entry-event :exit-event}` per interval, trimming
    `:loader`/`:decruncher` intervals to their last handoff; `active-handler-runs`
    are refined by `precise-run-start` (first invocation of the run's handler
    set) and `snapped-starts` (last handoff before each boundary, within a 2 M
    event tolerance); `assemble` takes the handoffs; `run-phases` also keeps the
    phase that ends exactly at a run boundary, so a part carries the transition
    that precedes it. Segment kinds are now only `:demopart` / `:transition`,
    and file groups split after a `:transition` whose roles include `:loader`.
  - Fixed `precise-run-start`: it seeded its accumulator with the bucket edge,
    so `min` could never advance and precise starts were a no-op.
- **Result (fresh capture):** 17 segments = 10 demoparts, 7 transitions, 3 file
  groups, 0 gaps, 0 overlaps. Boundaries: part 1 at 1,099,681, part 16 at
  45,069,071 (`:exit-event` of the preceding decruncher).
- **Tests:** full v2 suite 55 tests / 324 assertions, 0 failures; `clj-kondo`
  and `cljfmt` clean. `pipeline/build!` rebuilt only `:segments` after the
  version bump and reused the rest.
- **Promising future work:**
  1. Compute phase intervals on a timeline that counts IRQ occurrences rather
     than wall events, so mainline windows stop ballooning when effects run in
     IRQ; that would also let `:decruncher` stop earlier without relying on
     handoffs.
  2. Expose `:entry-event`/`:exit-event` and `:handler-contexts` through the
     `:disassembly` input contract explicitly.
  3. Consider whether a part should begin at the handler switch or at the
     decruncher handoff when they disagree by more than a few thousand events
     (for this capture they agree to within ~2 k).

### Diary entry addendum: program-only captures start at the stub's SYS target (2026-10-11)

- **Status:** success, verified on two independent demos.
- **Change:** the program-only start policy no longer begins at the BASIC RUN
  entry `$E144`. After the autostart path stops there, `capture` reads the
  loaded stub at `$0801`, walks its BASIC line links, parses the operand of the
  first `SYS` token, resumes, and stops again at that address. The initial
  snapshot is then taken immediately before the program's first machine
  instruction, so the capture starts exactly at the program entry instead of
  inside the BASIC interpreter. If no `SYS` stub is found, it falls back to
  `$E144`. The parser skips spaces and an optional `(` so both `SYS 2070` and
  `SYS (2064)` work.
- **Verification:**
  - Triad `r676_No Booze__No_Booze_Triad.d64`, stub
    `10 SYS 2070 [TRIAD]` (`$9E 20 "2070"`): a throwaway capture's chunk-1
    snapshot reports `:pc 2070` (`$0816`), and the intro was observed running.
  - Fairlight `r53390_Fairlight Intro (the Legendary one).prg`, stub
    `1998 SYS (2064)` (`$9E 20 28 "2064" 29`): a throwaway capture reports
    `:pc 2064` (`$0810`), the first byte of which is `78` (`SEI`), a clean
    entry; the intro was observed running. The parenthesized operand is why the
    original `decimal-after` (spaces + digits only) needed extending.
- **Note:** the committed `captures/no-booze-triad-v2` dataset predates this
  change and still begins at `$E144`; only a recapture would start at `$0816`,
  so its stage generations stay valid as-is.
- **Tests:** new `capture-test/basic-sys-stub-parsing` covers plain,
  parenthesized, trailing-`:`, and SYS-less lines; full v2 suite 56 tests / 326
  assertions, 0 failures; `clj-kondo` and `cljfmt` clean.
- **Promising future work:** make the stub address (`$0801`) and the pattern
  configurable, and consider following a relocated BASIC program
  (`:basic-start` vector) for stubs that move themselves.

### Diary entry addendum: FIFO reader busy-spin after stop (2026-10-11)

- **Status:** fixed (leak), plus a note on why it was noticed.
- **Symptom:** after several start/stop capture cycles the JVM sat at ~400% CPU
  with `load average` climbing, long after every capture and VICE process had
  exited.
- **Cause:** `capture/start-fifo-reader!` had its `(recur)` **outside** the
  `when-not @stop-requested` guard:
  ```clojure
  (loop []
    (when-not @stop-requested (…read…))
    (recur))            ; always loops
  ```
  Once `:stop-requested` was set the loop body became empty and the thread spun
  at 100% forever. `Thread.interrupt` cannot break a non-blocking spin, so
  `stop-fifo-reader!`'s interrupt had no effect and each stopped capture leaked
  one spinning daemon thread (three were alive when this was found).
- **Fix:** move `(recur)` inside the guard, so a stopped reader queues the final
  EOF sentinel and then leaves the loop (no `recur` → `loop` returns).
- **Verification:** restarted the dev server to drop the existing spinning
  threads; started and stopped a throwaway Fairlight capture and confirmed the
  thread set no longer contains any `vice-v2-reader` and the JVM returns to
  ~0% CPU. The same run also re-confirmed the SYS-target start (`:pc 2064`).
- **Lesson:** the reader's stop path is easy to get wrong because the sentinel
  must still be delivered. A reader thread whose name is unique (`vice-v2-reader`)
  makes this class of leak easy to check with a thread-CPU dump.
- **Tests:** full v2 suite 56 tests / 328 assertions, 0 failures.

### Diary entry addendum: reader stop hardening and a live-session registry (2026-10-11)

- **Status:** implemented and verified.
- **Blocking FIFO open.** A reader that has been started but not yet opened sits
  inside a blocking FIFO open (a POSIX FIFO opens for reading only once a writer
  appears), and that open is not interruptible, so `stop-fifo-reader!` could not
  release it — e.g. when `set-logging!` fails and `cleanup!` runs with the reader
  still waiting. `stop-fifo-reader!` now detects that case (reader thread alive,
  no opened reader) and releases it by briefly opening the write end from a
  short-lived `vice-v2-reader-release` daemon, then joining with the existing
  timeouts. The release runs off-thread because the writer open itself blocks
  when no reader is waiting.
- **Live-session registry.** `capture` now keeps every started-but-unfinished
  session in a private `sessions` atom keyed by a generated `:session-id`,
  registering in `start!` and de-registering at the end of `run-session!` (after
  `cleanup!`). The new public `capture/live-sessions` returns each session's
  `status` map, and `status` gained `:session-id`, `:worker-done?` and
  `:stop-requested?` alongside `:reader-alive?`. A stopped capture whose reader
  or worker is still alive is therefore visible instead of silently forgotten.
- **Verification:** live run — while capturing, `live-sessions` reported one
  session with `:status :running`, `:reader-alive? true`, `:worker-done? false`,
  `:stop-requested? false`; after `stop!` it reported zero sessions, no
  `vice-v2-reader` or `vice-v2-reader-release` threads remained, and the JVM was
  idle. New tests: `release-fifo-open-unblocks-a-blocked-reader` (real FIFO +
  blocked reader thread) and `session-registry-lifecycle` (register, inspect,
  unregister).
- **Tests:** full v2 suite 58 tests / 337 assertions, 0 failures; `clj-kondo`
  and `cljfmt` clean.

### Diary entry addendum: system contexts, invocation-level IRQ roles, isolated mainline units (2026-10-11)

- **Status:** implemented and verified against the persistent `fairlight-intro`
  capture, which now reconstructs perfectly.
- **`$FF48` and system contexts.** An IRQ entry at `$FF48` is the KERNAL
  trampoline, which dispatches through `$0314/$0315`; the earlier fallback of
  keeping `$FF48` as the handler happened only because the user vector had not
  been *observed* yet. Dedupe now seeds the stock power-on interrupt vectors
  (`$0314/$0315 = $EA31`, `$0316/$0317 = $E38B`, `$FFFA-$FFFF` defaults), so the
  resolution always succeeds and `$FF48` never becomes a context. Further, the
  trampoline resolution is gated on the KERNAL actually being mapped: `:cpu-port`
  is now tracked from stores to `$01`, and `$FF48` is treated as an ordinary RAM
  address when HIRAM is banked out (RAM under `$E000-$FFFF` dispatches through
  the RAM copy of `$FFFE/$FFFF` and never reaches `$FF48`).
- **Do we care about `$EA31`?** No — it is the stock KERNAL IRQ service routine,
  and a handler in the KERNAL ROM is never program behaviour. But it is only
  system code *while that ROM is mapped*, so each context now records a `:rom?`
  flag (the flag byte in `contexts.bin`, previously reserved) set when the
  handler address is in the KERNAL range **and** the KERNAL was mapped at entry.
  `dedupe/system-contexts` exposes the ids where kind ∈ `:irq/:nmi/:brk` and
  `:rom?` is true. `:features` drops those contexts' enter/return switches, so a
  system interrupt never becomes an invocation unit (its occurrences are then
  ignored because no invocation is open). `dedupe-version` 3 → 4.
- **IRQ roles at the invocation level.** `:classification` now derives an IRQ
  invocation's base roles from the *aggregated invocation evidence* and uses
  per-occurrence evidence only to add roles and to split. A single raster
  handler writes ~2 VIC registers per block occurrence but ~54 per invocation,
  so per-occurrence classification reported `:idle` and missed raster entirely.
  The split decision uses **evidence-only** occurrence roles, so a block-level
  audio candidate that grants an occurrence `:music` cannot mask the
  invocation's `:raster`. `classification-version` 5 → 6.
- **Isolated mainline unit.** `:enter-units` hysteresis needs two consecutive
  units before a role switches on, so a mainline timeline that never stabilized
  a role (a short intro whose only mainline unit is its init window) emitted
  *nothing* and left an uncovered gap. At flush, if no role ever stabilized, the
  pending unit's own evidence is now used. Once any role has stabilized the
  trailing unit is left to hysteresis as before (the existing hysteresis test
  pins that).
- **Leading run.** `:segments` could drop the region before the first handler
  run: when a handler first fires inside the first bucket, that run's precise
  start moves past 0. `with-leading-run` prepends a handler-less run covering
  `[0, first-run-start)`.
- **Fairlight intro result** (`captures/fairlight-intro`, 1,542,592 events):
  contexts = main, `$EA31` (`:rom? true`), `$C174` (`:rom? false`);
  `system-contexts = #{1}`; `:features` 282 units (1 mainline + **281**
  invocations, one system invocation dropped); `:classification` 282 spans with
  `{:raster 280, :music 282, :decruncher 1}` and 1 mainline span; `:segments`
  **2 segments — `:transition [0,47537)` roles `#{:decruncher :music}` (the
  self-relocating init) and `:demopart [47537,1542592)` handler {2} roles
  `#{:music :raster}` — 1 file group, 0 gaps, 0 overlaps**. Analysis: the whole
  intro runs in one raster-synchronised IRQ handler (`$C0F9: CMP $D012; BNE`,
  `$C105: CMP $D012; BEQ`, `$C174: LDA #$01`) that fires back-to-back after the
  init, so the mainline never resumes.
- **Tests:** full v2 suite 58 tests / 337 assertions, 0 failures; `clj-kondo` and
  `cljfmt` clean. (Note: an earlier "passing" run was against stale namespaces
  in the JVM; re-running after `clj-reload` confirmed the results.)
- **Cost of the version bumps:** `captures/no-booze-triad-v2` was built with
  `dedupe-v3`/`features-v4`/`classification-v5`, so it is now stale and needs a
  rebuild (dedupe ~30 min) before it can be reopened. Its *structure* would be
  unchanged (the `$FF48`/`$EA31` context is filtered as ROM either way).
- **Promising future work:**
  1. Expose `:system-contexts` (and `:rom?`) through `:classification`/
     `:segments` indices so a consumer does not need to open `:dedupe`.
  2. Consider not creating a context at all for system interrupts, rather than
     creating one and filtering it in `:features`; a "system frame" marker would
     keep the structure without downstream noise.
  3. The `with-leading-run` region has no handler, so `run-kind` depends on the
     overlapping spans' roles; a program whose init writes nothing classifiable
     would still leave a gap.
  4. Rebuild `no-booze-triad-v2` on the new versions and re-confirm 10 demoparts.

### Diary entry addendum: nil audio candidates, unclassified mainline flush (2026-10-11)

- **Status:** two more real defect fixes, prompted by asking why the Fairlight
  init's `:transition` segment carried `:music`.
- **The `:music` was not the music routine spilling over.** `:audio` emits
  heterogeneous candidates: `{:kind :block :block b ...}` and
  `{:kind :context :context c ...}`. `:classification` built its sets with
  `(map :block ...)` / `(map :context ...)` over *all* candidates, so each set
  contained `nil`. `unit-roles` then tested `(contains? audio-blocks (:block-id
  unit))`, and a mainline window has no `:block-id` — so `nil` matched and every
  mainline window was granted `:music`. This also explains the earlier triad
  observation that "every mainline span is music": it was largely this, not the
  audio contexts. Fixed by selecting candidates by `:kind` and by requiring
  `(some? (:block-id unit))` / `(some? (:context-id unit))` before matching;
  `classification-version` 6 → 7. Regression test:
  `audio-candidates-require-a-matching-id`.
- **Unclassified mainline flush.** With hysteresis never stabilizing a role, the
  init's span was only its last fragment (`[47536,47537)`) because hard handoffs
  split the window into many fragments and each empty-role fragment was dropped
  as it was produced. `mainline-step` now remembers the extent of the pending
  unclassified episode in `:unclassified` (reset whenever a classified span is
  emitted), and the fallback flush covers that whole range. The Fairlight init
  is now one span `[0,47537)` with roles `#{:decruncher}`.
- **Fairlight intro, final form** (1,542,592 events): classification 282 spans
  `{:raster 280, :music 281, :decruncher 1}` with 1 mainline span; segments
  **`:transition [0,47537)` roles `#{:decruncher}`** (self-relocating init) and
  **`:demopart [47537,1542592)` handler {2} roles `#{:music :raster}`** —
  1 file group, 0 gaps, 0 overlaps.
- **Tests:** full v2 suite 59 tests / 340 assertions, 0 failures; `clj-kondo`
  and `cljfmt` clean.
- **Promising future work:** give `:audio` a single uniform candidate shape
  (`{:kind ... :block ... :context ...}` with both keys present, one nil) so
  consumers cannot rebuild this class of bug; or validate candidate shape on
  write.

### Diary entry addendum: simplification pass (2026-10-11)

Using what the Fairlight/Triad work established, several things became dead
weight and were removed. Behavior on `fairlight-intro` is unchanged (identical
spans, roles, segments):

- **Mainline spans no longer split at hard handoffs.** Part boundaries come from
  the display-handler set, and handoff evidence reaches `:segments` as phase
  `:entry-event`/`:exit-event`, so the split was unused. Removed
  `split-range-at`, `hard-points`, `merge-unclassified`, the `:unclassified`
  accumulator, and the `handoffs` parameter of `classification/classify` and
  `build!` (which also stops `:classification` reading `features/handoffs`).
  `mainline-step` is now just "continue while roles/context/frame match".
- **`:segments` no longer reads `:dedupe`.** Because `:features` drops system
  contexts, every IRQ span is a program handler, so display contexts are simply
  the IRQ spans' context ids. Removed the `dedupe` require, `kernal-rom-start` /
  `display-context-set`, and the `display-contexts` parameter: `assemble` is now
  `[spans handoffs total-events]`. `:features` records `:event-count` in its
  index so segments can bound the timeline without opening the capture or
  dedupe. Pipeline `:segments` deps are now `#{:features :classification}`,
  matching the documented stage graph. `segments-version` 5 → 6,
  `features-version` 5 → 6.
- **Handoff boundary off-by-one.** The handoff event is the loader/decruncher's
  final instruction, so a part begins at `handoff + 1` and the
  loader/decruncher phase includes the handoff event (trim end `exit + 1`).
  Previously the phase excluded it and a one-event span overlap leaked
  `:decruncher` into the following demopart's roles. `with-leading-run` also
  moved *before* `snapped-starts` so the first part's boundary can be snapped.
- **Docstring fix:** `analysis.clj` no longer claims dependency orchestration is
  deferred; it lives in `pipeline`.
- **Fairlight intro after the pass:** `:transition [0,47537)` roles
  `#{:decruncher}` (phase `[0,47537)` entry 12373 exit 47536) and
  `:demopart [47537,1542592)` handler {2} roles `#{:music :raster}` with the
  preceding decruncher phase attached — 1 file group, 0 gaps, 0 overlaps.
- **Tests:** 59 tests / 342 assertions, 0 failures; `clj-kondo`/`cljfmt` clean.
  `classification-span-merging-and-irq-splitting` now pins the new contract
  (merge across windows; split on a role-set change) instead of the removed
  handoff split.
- **Still to consider:** `segments`' `display-bucket-events`, `bridge-max-events`
  and `handoff-snap-tolerance` are still hardcoded (the doc's "make parameters
  configurable" item); `:video`/`:assets` remain unused by the acceptance path;
  and `:audio` candidates could be given a uniform shape so consumers cannot
  rebuild the nil-matching bug.

### Diary entry addendum: easy cleanups (2026-10-11)

- **Audio candidate accessors.** `:audio` now exposes `music-blocks` and
  `music-contexts`, which select candidates by `:kind`; `:classification` uses
  them instead of re-deriving the sets. This is where the nil-matching bug came
  from, so the knowledge of the candidate shape now lives with its owner (the
  defensive `some?` guards in `unit-roles` stay).
- **`segments` tuning is configuration.** The three hardcoded parameters are now
  `default-configuration` keys, recorded in the generation like every other
  stage's configuration: `:display-bucket-events` (250000),
  `:bridge-max-events` (3000000), `:handoff-snap-tolerance` (2000000).
  `assemble` gained a configuration arity; the 3-argument arity uses the
  defaults, so tests are unchanged.
- **Audit no longer reports handoff alignment.** Demoparts come from the
  display-handler set, so a write-to-execute handoff inside a part is expected;
  `:hard-handoffs`, `:hard-handoff-count`, and `:unaligned-hard-handoff-count`
  were noise (and the per-handoff list bloated `boundary-audit.edn`).
  `boundary-audit` lost its `handoffs` parameter, and the segment summary lost
  the field. Handoff evidence is not lost: `:features` keeps `handoffs.edn`, and
  phases carry `:entry-event`/`:exit-event`. `segments-version` 6 → 7.
- **Verified:** Fairlight intro unchanged (`:transition [0,47537)` `#{:decruncher}`
  + `:demopart [47537,1542592)` `#{:music :raster}`, 1 file group, 0 gaps,
  0 overlaps); pipeline rebuilt only `:segments` and recorded the new
  configuration. 59 tests / 342 assertions, 0 failures; `clj-kondo`/`cljfmt`
  clean.

### Diary entry addendum: Triad dataset rebuilt on current stage versions (2026-10-11)

- **Status:** success. `captures/no-booze-triad-v2` was rebuilt on the current
  stage versions; the stale-generation blocker in the Status section is closed.
- **Procedure:** `(pipeline/build! root :segments {:force #{...}})`, reusing one
  already-validated capture reader. All five stages rebuilt (`:built [:dedupe
  :audio :features :classification :segments]`, `:reused []`) in ~41 min; a
  second unforced `build!` reused all five in ~7 s.
- **Result:**
  - `:dedupe` (v4) — 19 contexts, 10,290 instructions, 2,921 blocks, 94 windows,
    16,664,262 occurrences. The KERNAL `$FF48`/`$EA31` context is correctly
    absent (`19` rather than the previous `20`).
  - `:audio` (v1) — 132,208 SID writes, 61 music candidates.
  - `:features` (v6) — 36,619 units (210 mainline windows, 36,409 IRQ
    invocations, 2,033 handoffs).
  - `:classification` (v7) — 35,729 spans (41 mainline, 35,688 IRQ), roles
    `{:music 35729 :raster 5921 :idle 28855 :decruncher 25 :loader 9}`.
  - `:segments` (v7) — **17 segments = 10 demoparts + 7 transitions, 3 file
    groups, 0 gaps, 0 overlaps**. Demoparts distribute exactly 3 / 3 / 4 across
    file groups `[0,9300052)`, `[9300052,30733201)`, `[30733201,47763778)`,
    matching the known part list.
- **Pruning:** older analysis generations were removed; only the five committed
  generations remain (verified against the manifest `:directory` entries).
- **Observed issues:**
  - Pruning by generation must be driven by an explicit keep-list read from
    `analysis/manifest.edn`. A first attempt that computed the keep-set inside
    the same form that deleted generations shadowed `clojure.core/keep`, so the
    keep-set resolved to an unbound var and the sweep removed *every*
    generation including the committed ones (it was recovered by the forced
    rebuild). Prefer a plain script or `clojure.core/keep`.
  - The reuse gap documented in the Open follow-ups is real: with a committed
    generation directory missing but its manifest entry and matching signature
    intact, `ensure-stage!` would call the generation reusable and fail only
    when opening it. An existence check plus output validation would close it.
- **Decisions or deviations:** the dataset was rebuilt, not recaptured; the raw
  capture still begins at `$E144` (pre SYS-target policy), which is unrelated to
  the stage rebuild.
- **Next step:** start Phase 8 (`:disassembly`) over `:dedupe` + `:segments`.

### Diary entry addendum: `:dedupe` profiling and a 4x speed-up (2026-10-11)

- **Status:** success. Profiled `:dedupe` with `clj-async-profiler` and fixed
  the dominant cost. Full-capture build is **~4x faster with byte-identical
  output**; the remaining speed-up and parallelization opportunities are
  recorded below.
- **Method:** profiled a 5 M-event prefix and the full capture through an
  isolated dataset (symlinked capture, temporary analysis root). Profiles taken
  on the 8-core dev JVM with `:cpu` and `:alloc` events.
- **Before (old code):**
  - `:cpu` over a 5 M-event prefix: 788,252 samples, of which **56.7% GC** and
    43.4% app thread. The app thread was ~60% *reflection*:
    `java.lang.reflect.Method.<init>` 27.2% self, `Method.copy` 19.3%,
    `Class.reflectionData` 15.2%, `Reflector.getMethods` 63.9% inclusive.
  - Root cause found with `*warn-on-reflection*`: every `ByteBuffer` created by
    `little-buffer` was untyped, so all `.put*`/`.array` calls in the four
    channel encoders, the four dictionary writers, and `flush!` were reflective.
  - Throughput was ~32 k events/s on a 3 M prefix (full capture ~28.5 min).
- **Changes (both semantics-preserving):**
  - `little-buffer` now returns `^ByteBuffer`; the four channel encoders and
    `byte-array-output` carry `^bytes`, and `flush!` hints its four channel
    locals (the encoders are `declare`d before `flush!`, so the return tag does
    not propagate there). Reflection in the hot path is gone.
  - `intern-instruction!` no longer clones the instruction vector when an
    already-seen operand's volatility mask is unchanged; it returns the state
    untouched (the common case, since operands stabilize quickly).
- **After:**
  - `:cpu`: ~204,000 samples for the same work — **~3.9x less CPU**; GC
    447,017 → ~120,000 samples.
  - Full capture `no-booze-triad-v2` (47,763,778 events): **439,527 ms
    (~108.7 k events/s)** versus ~28.5 min before.
  - **Byte-identical output:** `instructions.bin`, `blocks.bin`,
    `contexts.bin`, `image.bin`, and `windows.idx` all match the committed
    generation built with the old code (SHA-256 compared).
- **Remaining app-thread attribution (5 M prefix, current code):** GC 58.7% of
  CPU (overlapped); app thread by subtree: `fold other` 27.8%, event decode
  25.9%, block close (occurrence/edge) 17.2%, `intern-instruction!` 7.6%,
  `append-instruction!` 7.1%, `ensure-open!` 3.5%, `add-targets!` 2.6%,
  `flush`/encode 2.2%, `maybe-port!` 2.2%, `maybe-vector!` 2.1%,
  `isa/decode` 1.8%. The alloc profile is led by the event-map decode (26%),
  the occurrence machinery (~40% combined), and boxing (`Long` 6.3%).
- **Speed-up opportunities (in priority order):**
  1. **Primitive event decode.** `event/decode-event` builds an 11-key map plus
     a `ByteBuffer`, a 3-byte array, and a `mapv`/`take`/`seq` vector *per
     event* (~26% of app CPU and 26% of allocation). A reader API that yields
     primitive fields (or a reusable mutable event carrier) would remove most
     of this; the `:cpu` leaf frames inside decode are hashmap/keyword ops, so
     the map is the cost. Expected ~1.3–1.5x.
  2. **Mutable/primitive fold state.** ~60% of app time is persistent-map
     work (`fold other` + occurrence/frame machinery): the state map has ~25
     keys and is `assoc`/`update`-ed dozens of times per event, and every
     occurrence allocates a map plus three nested window updates. Replacing the
     hot fields with mutable locals/arrays and the window channels with
     growable primitive buffers (written directly, not as `mapv` of per-record
     byte arrays) is the largest remaining win. This is the documented
     "mutable-state rewrite".
  3. **`isa/decode`:** drop the unused `:bytes (vec bytes)` and consider
     per-opcode primitive arrays instead of building a result map.
  4. **Small cleanups:** `direct-operand-address` is computed twice per event
     (`maybe-port!` and `maybe-vector!`); two reader `aget` sites still reflect
     (`dedupe.clj` 991/996); the `case` on `context-kind-codes` warns on
     non-primitive input.
- **Parallelization opportunities:**
  1. **Parallel decode/extract feeding the serial fold.** Decoding is ~26% of
     app time and stateless. A worker pool that decodes raw chunks into compact
     per-chunk annotation arrays (pc, opcode, mode/width, target, terminator,
     store/port/vector flags, cycle) preserves order by concatenation and
     removes decode from the critical path, using the cores GC does not.
  2. **Per-chunk/per-segment map-reduce with a deterministic merge.** This is
     the path to scaling with cores. The key enabler is that every raw chunk
     has an exact boundary snapshot, so each segment can be seeded
     independently: `:cpu-port` from the snapshot, and the tracked vector bytes
     (`$0314-$0319`, `$FFFA-$FFFF`) from the snapshot's underlying RAM. The map
     phase computes order-independent summaries per segment (instruction
     identities + operand volatility, the observed target set, port/vector
     writes, local block sequences, occurrence candidates); the reduce phase
     assigns global ids deterministically, stitches blocks that cross segment
     boundaries, orders occurrences, and accumulates edges. The online
     `:known-targets` split rule requires either a first parallel pass that
     collects all targets or using the complete target set in the second pass.
     Hard parts: boundary-crossing block stitching, deterministic global ids,
     and frames open at a boundary (the reader's straddling reconstruction plus
     the snapshot SP make the last of these tractable).
  3. **Parallel independent pipeline branches.** `:video` is independent of
     `:dedupe`, and `:audio`/`:features` both consume `:dedupe`; scheduling them
     concurrently (already an open follow-up in the pipeline driver) shortens
     the whole run even before `:dedupe` itself is parallelized.
  4. **Window encode/write off the critical path.** Now only ~2.2% of app time,
     so low value unless the other items land first.
- **Decisions or deviations:** the two applied fixes are pure optimizations and
  do not change `dedupe-version` (the on-disk format and output are unchanged).
- **Next step:** implement opportunity (1) (primitive event decode) and
  (2) (mutable fold state) for another ~1.5–2x, then pursue the per-segment
  map-reduce for core scaling; or start Phase 8 first if structural work is
  higher priority.

### Diary entry addendum: `:dedupe` primitive decode and mutable fold state — 8x (2026-10-11)

- **Status:** success. Two further optimizations landed on top of the
  reflection fix, in each case with **byte-identical output** verified against
  the committed generation. Full-capture `:dedupe` is now **~3.4 min
  (~231 k events/s)** versus ~28.5 min originally — roughly **8x**.
- **Change 1 — primitive event decode.**
  - `reader` gained `reduce-events`: an allocation-free reducer that decodes
    each fixed-width record into a reusable `long-array` (layout: the public
    `ev-*` slot constants), with correct `reduced` handling. It replaces the
    per-event map from `event-pairs` for `:dedupe`.
  - `isa` gained per-opcode `mnemonic-by-opcode`, `mode-by-opcode`, and
    `width-by-opcode` tables, so opcode decoding is array indexing rather than
    a map construction plus `(vec bytes)`.
  - `:dedupe` now reads event fields through `aget` and resolves operands
    directly (`direct-operand-address`, `add-targets!`) instead of building a
    decoded map and calling `isa/resolve-address`. `:prev-event` (used only as
    a truthiness flag) became `:seen-prev?`.
  - Effect: decode fell from **25.9 % to ~1.5 %** of app-thread time; GC fell
    from 58.7 % to 28.6 % of CPU. Full capture 439.5 s → **237.5 s**.
- **Change 2 — mutable fold state.**
  - The per-window channels (`:occurrences`, `:timings`, `:ports`,
    `:switches`) are now mutable `java.util.ArrayList`s instead of persistent
    vectors rebuilt by `update-in` on every occurrence; `:totals` is a
    `long-array`; `:known-targets` is a 64 KiB `boolean-array`; and the
    instruction identity key is a packed `long` instead of a `[pc opcode]`
    vector.
  - Effect: occurrence emission fell from 13 % to 2.1 % of app time. Full
    capture 237.5 s → **206.6 s**.
- **Verification at every step:**
  - `instructions.bin`, `blocks.bin`, `contexts.bin`, `image.bin`, and
    `windows.idx` all match the old-code generation by SHA-256 (checked after
    each change and again after the final cleanup); counts unchanged
    (10,290 instructions, 2,921 blocks, 19 contexts, 94 windows, 16,664,262
    occurrences).
  - Full non-live suite: **59 tests / 342 assertions, 0 failures**.
  - `*warn-on-reflection*` is clean for `dedupe`, `reader`, and `isa`;
    `clj-kondo` and `cljfmt` clean.
- **Updated bottleneck (5 M prefix, current):** GC 32.9 %, app 65.9 %. App
  attribution: `close_block` 19.6 % (`record_edge` 8.5 %, `intern_block`
  4.2 %), `append_instruction` 13.3 %, `maybe_flush` 12.6 % (`flush` 5.0 %),
  `set_current_frame` 8.3 %, `ensure_open` 6.7 %, `intern_instruction` 6.6 %,
  `level_overflow` 4.6 %, `isa/sp_after` 3.3 %. Self time is now persistent
  map/vector and boxed-number dispatch (`Numbers.num` 9.0 %, `Util.equiv`
  8.9 %, `Keyword.hasheq` 7.4 %).
- **Further speed-up opportunities (not yet done):**
  1. **Mutable frame/open.** `append-instruction!` + `set_current_frame` +
     `ensure_open!` (~28 % combined) rebuild the open-block map, the frame map,
     the stack vector, and the state map on every event. A mutable current
     frame with a growable instruction-id buffer would remove most of it.
  2. **Mutable CFG edges.** `record_edge!` (8.5 %) is nested `update-in` on the
     context edge map; a mutable `HashMap` per context would help.
  3. **`flush!`/`maybe-flush!`** (5.0 % + per-event overhead).
- **Parallelization assessment (updated with evidence):**
  - The earlier "parallel decode/extract feeding the serial fold" idea is now
    **moot**: decode is ~1.5 % of app time after the primitive path. Offloading
    it would buy almost nothing.
  - GC is only ~33 % of CPU and overlaps on 8 cores; the **serial fold is the
    wall-clock bound**, so the only path that scales with cores is splitting
    the fold itself.
  - **Per-segment map-reduce remains the design** (unchanged from the previous
    addendum): each raw chunk has an exact boundary snapshot, so a segment can
    be seeded with `:cpu-port` and the tracked vector bytes from that snapshot;
    a map phase computes order-independent summaries per segment and a
    sequential reduce assigns global ids, stitches blocks across boundaries,
    orders occurrences, and accumulates edges. **Caveat found while doing
    change 1:** the online `:known-targets` split rule is *not* order
    independent — a target only splits blocks after its first observation — so
    it cannot simply be replaced by the complete target set without changing
    output; any map phase must preserve the online order (or the format's
    block semantics must change deliberately).
  - The low-risk, shorter win for the **whole run** is pipeline-level branch
    parallelism (`:video` alongside `:dedupe`; `:audio` alongside `:features`
    after `:dedupe`), which is independent of the `:dedupe` internals.
- **Decisions or deviations:** none changed the on-disk format, so
  `dedupe-version` is unchanged; `reduce-events` and the `isa` tables are
  additive. `event-pairs`/`decode-event` remain for compatibility and tests.
- **Next step:** either continue the fold rewrite (mutable frame/open and CFG
  edges) for another ~1.3–1.5x, or start the segment map-reduce / pipeline
  branch parallelism for core scaling, or proceed to Phase 8.

### Diary entry addendum: pipeline branch parallelism and multi-target builds (2026-10-11)

- **Status:** implemented and verified. The pipeline driver now schedules
  independent branches of the dependency closure concurrently, builds several
  targets (or `:all`) in one request, and defaults concurrency to the available
  processors.
- **Changes:**
  - `omkamra.vice.analysis/commit-stage!` is now serialised by a private
    JVM-wide lock. Stages run concurrently, and a stage's own `build!` may
    commit at the same moment the pipeline records its signature, so the
    manifest read-modify-write must not interleave. (A cross-JVM/OS lock is
    still the separate §9 follow-up.)
  - `omkamra.vice.pipeline`:
    - `normalize-targets` accepts a stage keyword, a collection of stages, or
      `:all`; `closure` returns one dependency-first order for the union.
    - `build!` replaced the single topological pass with a **frontier
      scheduler**: each step builds every stage whose dependencies are already
      done, in batches bounded by `:stage-parallelism`, and merges the result.
    - `default-stage-parallelism` returns `(.availableProcessors)`; frontiers
      are at most three stages, so the default runs every independent branch at
      once. `:stage-parallelism` remains overridable for memory-constrained
      hosts.
    - A scheduling priority (`downstream-count`) orders each frontier so the
      critical path is scheduled into the earliest batch and is not delayed
      behind an independent branch.
    - `:built`/`:reused` are returned in dependency-first order regardless of
      completion order, so concurrent scheduling does not make the result
      non-deterministic.
  - Added `omkamra.vice.pipeline-test` (target normalization, dependency-
    first closure, default parallelism, invalid-parallelism rejection).
- **Verification (isolated `fairlight-intro` dataset, 1.54 M events, target
  `:all`):**
  - sequential `:stage-parallelism 1`: **43.3 s**; parallel
    `:stage-parallelism 4`: **35.7 s**; default (8 cores): **36.2 s** — about
    **16–17 %** faster.
  - Instrumented stage timings show the intended overlap: `video` ∥ `dedupe`
    from t≈10 ms, then `assets` ∥ `audio` ∥ `features` from t≈7.2 s, with
    `classification` and `segments` on the critical path.
  - Outputs are **generation-name-normalised byte-identical** between the
    sequential, parallel, and default runs for all seven stages.
  - Reuse: a second `build!` reports `:built []` and `:reused` all seven;
    `no-booze-triad-v2` `:segments` reuses all five in ~6.6 s.
  - Full non-live suite: **63 tests / 360 assertions, 0 failures**;
    `clj-kondo`/`cljfmt` clean.
- **Observed issues / limits:**
  - The gain is bounded by Amdahl: `:dedupe` and `:features` dominate and lie
    on the critical path, so branch parallelism hides the *other* branches
    (`:video`, `:assets`, `:audio`) but cannot shorten the chain. Larger gains
    still require parallelising `:dedupe`/`:features` themselves.
  - All branches share one capture reader, so concurrent exact scans contend
    for its bounded chunk cache. Correctness is unaffected; a larger
    `:cache-size` or per-branch readers (which would re-validate the capture)
    may help under heavy overlap.
  - Stages are internally parallel (`:features` uses worker pools), so the
    default can oversubscribe cores; `:stage-parallelism` is the knob.
- **Decisions or deviations:** reuse signatures, stage versions, and on-disk
  formats are unchanged; only scheduling and target selection changed.
- **Next step:** parallelise the `:dedupe`/`:features` critical path (per-segment
  map-reduce) for real core scaling, or proceed to Phase 8.

### Diary entry: Phase 8 — `:disassembly` (2026-10-11)

- **Status:** complete. The stage is implemented, wired into the pipeline, and
  validated on the full `no-booze-triad-v2` dataset.
- **Changes:**
  - Added `omkamra.vice.disassembly`. It consumes exactly `:dedupe` and
    `:segments` (never the raw capture) and emits, per committed generation:
    `index.edn`, one `segments/segment-NNN.asm` per semantic segment, and one
    `roles/<role>.asm` per configured role.
  - **Selection.** A block belongs to the segment containing its first
    fragment. The occurrence channel stores each occurrence in the window that
    contains its fragments, so a boundary-straddling block is assigned to the
    segment it started in and neighbouring segments stay disjoint.
  - **Rendering.** Operand/instruction bytes come from the instruction
    dictionary; volatile operand bytes render as `??`; every selected block is
    rendered **once per file** (including the aggregated role files, which
    deduplicate by block id), so no `.asm` file contains a basic block twice.
  - **Annotation.** One `contract/format-annotation` line per block
    (`; B:.. C:.. P:.. S:.. E:.. R:..`): global block id; context (`m`, or
    `i<hex>`/`n<hex>`/`b<hex>`); distinct predecessor/successor counts within
    the segment and context, computed from the segment's own edge stream rather
    than whole-capture totals; occurrence count; and observed back-edge
    repeats. `P`/`S` are 2-digit fields, so values above 99 saturate at the
    display and the true counts stay available in the index; the fixed
    vocabulary has no room for larger values.
  - **Visibility.** Each block records the CPU-port/ROM mapping at its first
    occurrence and `:mapped-rom?`; per-segment mapping frequencies and ROM
    block counts are in the index.
  - `omkamra.vice.dedupe` gained `scan-occurrences` (an allocation-free
    streaming reader over the fragment channel, laid out by `oc-*` slots) and
    `port-at` (the CPU-port value immediately before an event).
  - `omkamra.vice.pipeline` registers `:disassembly #{:dedupe :segments}`,
    so a full `:all` build now includes it. `omkamra.vice.pipeline-test`
    covers the scheduler helpers; `omkamra.vice.disassembly-test` covers the
    acceptance behaviour; the shared fixture now builds `:disassembly` too.
- **Performance.** The first implementation took **171 s** on the full capture.
  It used `dedupe/read-range` (which materialises occurrences, timings, ports,
  and switches as maps) plus a multi-pass `block-stats`. It was rewritten as a
  **single streaming pass** over the occurrence channel using packed long keys
  and transients, with the per-segment work run in parallel via
  `parallel/pmap-ordered`. Build time fell to **6.9 s** (25x); the bare scan of
  all 16,664,262 occurrences is 3 s, so the stats pass is the remaining cost.
- **Verification (`no-booze-triad-v2`, 17 segments):**
  - An independent recomputation from the occurrence channel reproduces every
    segment's block set exactly (**0 missing, 0 extra**); all 16,664,262
    occurrences are assigned to exactly one segment and **none is unassigned**.
  - Every segment file and role file has unique block ids and matches the
    committed index; role files: decruncher 2921, music 2921, raster 2448,
    loader 1076 blocks.
  - The guarded full-dataset test `disassembly-test/full-dataset-segments-match-recomputation`
    runs the same check read-only.
  - Full suite: **66 tests / 421 assertions, 0 failures**.
- **Warning cleanup (whole v2 tree).** All v2 namespaces now compile with
  `*warn-on-reflection*` clean, and `clj-kondo`/`cljfmt` report **0 errors, 0
  warnings**. Fixes: `ByteBuffer`/`bytes`/array return hints and reader/writer
  local hints; `java.io.File`/`Path` return and binding hints in `capture`,
  `recorder`, `store`, `assets`; `int` coercions for `Arrays/copyOfRange`;
  primitive `loop` recur args in `store`/`segments`; a `char` comparison in
  `contract`; and constructing `ProcessBuilder` from a `java.util.List`-hinted
  local.
- **Decisions or deviations:** the plan says "apply CPU-port/ROM visibility
  filtering". Mapping is computed, recorded, and exposed per block and per
  segment, but blocks are **not filtered out by default**: the format has no
  vocabulary for an omitted block, and dropping KERNAL/BASIC ROM bodies would
  make the segment-representation check ambiguous. An opt-in filter is the
  natural extension once a stub convention exists.
- **Next step:** retire the remaining Open follow-ups (compact the `:features`
  unit channel; rebuild `:video`/`:assets` on the fresh capture) or take on
  `:dedupe` per-segment map-reduce for core scaling.

### Diary entry: Phase 9 — transactional commits, reuse integrity, capture-free tests (2026-10-11)

- **Status:** partial. The transactional-commit and reuse-integrity cluster is
  implemented and verified, and the test suite no longer depends on any real
  capture. The remaining Phase 9 items (OS-visible lock, v1 API replacement)
  are still open.
- **Transactional commits.**
  - `omkamra.vice.analysis` gained a `*transaction*` dynamic var. Within a
    transaction, `commit-stage!` stages entries in the transaction instead of
    writing `analysis/manifest.edn`, and `read-manifest`/`stage-entry` merge the
    staged view over the file manifest. `publish!` performs the single atomic
    rewrite that makes all staged pointers visible together; `delete-generation!`
    removes an unpublished generation.
  - `pipeline/build!` binds one transaction for the whole invocation. On
    success it calls `publish!` once, so **every newly completed stage pointer
    is published atomically**. On failure it publishes nothing and deletes
    every generation the invocation created, so a failed run leaves all prior
    committed pointers and their generations untouched. Concurrent stage
    commits still share the JVM-wide manifest lock for standalone callers.
- **Reuse integrity.**
  - Each entry now records `:provenance`, the generation name of every
    dependency it actually consumed. Reuse requires the recorded upstream
    generation names to equal the currently committed ones, in addition to the
    version/signature match.
  - This closes the documented gap where a **defective build that keeps its
    signature** (declared inputs unchanged) was silently reused: such a rebuild
    produces a new generation name, so any downstream entry that recorded the
    old name is invalidated and rebuilt. `adopt!` records provenance as well.
  - Legacy entries without provenance are reused once and backfilled
    (`:backfill`), so the change does not force a full rebuild of existing
    datasets. The decision is the pure function `pipeline/reuse-decision`.
- **Capture-free tests.**
  - Added `omkamra.vice.synthetic-test-support`: it generates a synthetic
    raw capture in memory (a mainline `INC; JMP` loop interrupted every
    iteration by an IRQ handler at `$2000` that writes 40 VIC and 40 SID
    registers and reads the serial port — 6,602 events over 3 chunks), and
    builds the whole stage chain (`:all`) in an isolated temporary dataset.
    It reads no `captures/` path.
  - Deleted `acceptance_test_support` and every guarded `^:acceptance` test
    that ran against the Triad/Fairlight captures. `disassembly-test` and
    `pipeline-test` now use the synthetic fixture; `reader-test`,
    `dedupe-test`, `domains-test`, `features-classification-test`, and
    `segments-test` keep their existing synthetic/pure tests. No v2 test
    references a capture path.
  - The pipeline failure test forces a rebuild, injects a failure in
    `:classification`, and asserts that the committed pointers are byte-for-byte
    unchanged and the dedupe/audio/features generations created by the failed
    run were removed — the all-or-nothing guarantee end to end.
- **Verification.** Full suite **69 tests / 442 assertions, 0 failures**, run
  with no capture present and in a few seconds; `clj-kondo`/`cljfmt` and
  `*warn-on-reflection*` clean for the whole v2 tree. On the real dataset, two
  `pipeline/build!` runs reused all six stages and backfilled provenance.
- **Remaining Phase 9 work:** an OS-visible cross-JVM writer lock (§9), and
  replacing/retiring the v1 public capture/analysis API (§10, and removing
  `omkamra.vice.{analysis,asm,capture,decoder,profile}`).
- **Convention satisfied:** the doc's "do not hardcode the acceptance-capture
  path" item is now met — no v2 code depends on a capture being present.

### Diary entry: Phase 9 — OS-visible lock and v1 retirement (2026-10-11)

- **Status:** the Phase 9 orchestration work is complete except for optional
  polish. The analysis dataset now has an OS-visible writer lock, and the
  obsolete v1 orchestration namespaces are gone.
- **OS-visible writer lock.**
  - `omkamra.vice.analysis/with-manifest-lock` runs a function while holding
    the dataset write lock: a private monitor serialises writers within one
    JVM, and a `FileLock` on `analysis/.lock` serialises them across JVMs
    (`FileChannel.lock`, blocking). `commit-stage!` outside a transaction and
    `publish!` both use it, so two JVMs can no longer interleave a manifest
    read-modify-write. Creating the lock file also creates `analysis/`.
  - `pipeline-test/manifest-lock-is-os-visible` asserts the lock file exists, a
    second `tryLock` while it is held throws `OverlappingFileLockException`,
    and the lock is released afterwards.
- **v1 retirement.** Removed the obsolete v1 orchestration and its tests:
  `omkamra.vice.analysis` (+ `analysis/{common,runner,segments,stages}`),
  `omkamra.vice.asm`, `omkamra.vice.capture`, `omkamra.vice.decoder`
  (+ `decoder/{artifact,capture,classification,features,stream,structure,video,write}`),
  and `omkamra.vice.profile`, with their `*_test` namespaces. Nothing outside
  v1 referenced them.
  - **Kept:** `omkamra.vice` (monitor/process control), `omkamra.vice.binary-monitor`,
    and `omkamra.vice.trace`. These are the VICE adapters v2 still uses
    (`v2.capture`, `v2.snapshot`, `v2.feasibility`, `v2.event`), and
    `omkamra.decode64` uses the first two.
  - With v1 orchestration gone, the v2 conceptual APIs (`capture/*`,
    `reader/*`, `pipeline/build!`, `dedupe/*`, `segments/*`, `disassembly/*`)
    are the project's public analysis API.
- **Verification.** Full suite (v2 tests plus the retained
  `binary-monitor-test`/`trace-test`) **73 tests / 366 assertions, 0 failures**;
  no references to any retired namespace remain; `clj-kondo` and `cljfmt` are
  clean for the whole `v2` tree and `*warn-on-reflection*` is clean for every
  v2 namespace.
- **Notes / out of scope.**
  - `omkamra.decode64` is a cljfx/JavaFX desktop UI and fails to load on this
    JDK with a JavaFX SVG-loader `IncompatibleClassChangeError`
    (`ImageStorage.addImageLoaderFactory`). This is a pre-existing
    JavaFX/cljfx/javafxsvg version mismatch and is unrelated to the retirement
    (it depends only on the retained monitor adapters).
  - `binary_monitor.clj` retains two cosmetic `clj-kondo` findings
    (`:unused-value`, `:redundant-let`) from the legacy adapter; they are
    behaviour-neutral and were left untouched to avoid churn in working code.
- **Next step:** the pipeline-v2 plan is essentially delivered end to end
  (capture → dedupe → domain stages → classification → segments →
  disassembly, orchestrated transactionally with reuse). Remaining optional
  work is the runtime/follow-up list in the Open follow-ups section
  (`:features` binary channel, `:dedupe` per-segment map-reduce, `:video`
  scan parallelism, `:assets` coalescing, `:video`/`:assets` rebuild).

### Diary entry: retire the `omkamra.vice.v2` namespace prefix (2026-10-11)

- **Status:** complete. With v1 orchestration gone, the `v2` namespace segment
  no longer distinguishes anything, so every namespace moved up one level.
- **Change:**
  - Moved all `src/omkamra/vice/v2/*.clj` to `src/omkamra/vice/` and removed the
    empty `v2/` directory.
  - Rewrote every namespace reference `omkamra.vice.v2.*` to `omkamra.vice.*`
    (namespace declarations, `:require`/`:import` aliases, docstrings, and this
    document). No `omkamra.vice.v2` reference remains anywhere under `src`, the
    build config, or this document.
  - The retained VICE adapters keep their paths: `omkamra.vice`
    (`src/omkamra/vice.clj`, monitor/process control),
    `omkamra.vice.binary-monitor`, and `omkamra.vice.trace`. There were no
    filename or namespace collisions because the v1 namespaces that would have
    clashed (`capture`, `analysis`, `decoder`, …) were already removed.
  - This supersedes the Phase 0 note that v2 development namespaces used an
    `omkamra.vice.v2.*` prefix "while v1 is still present".
- **Verification.** After restarting the dev JVM: full suite **73 tests / 366
  assertions, 0 failures** under the renamed namespaces; `*warn-on-reflection*`
  clean for every namespace; `clj-kondo` clean except two pre-existing cosmetic
  findings in `binary_monitor.clj`; `cljfmt` clean except the three pre-existing
  legacy files (`binary_monitor.clj`, `trace.clj`, `trace_test.clj`).
- **Next step:** none required; the remaining items are the optional runtime
  follow-ups in the Open follow-ups section.

### Diary entry addendum: lint/format clean across the whole vice tree (2026-10-11)

- **Status:** complete.
- **Changes:** fixed the two remaining `clj-kondo` findings in the kept legacy
  `binary_monitor.clj`: `await-event` now threads its `skipped` transient
  through the loop instead of discarding the `conj!` result (`:unused-value`),
  and `read-response MON_RESPONSE_CPUHISTORY_GET` merges its nested `let`s
  (`:redundant-let`, behaviour unchanged). Dropped the unused
  `OverlappingFileLockException` import and the unused `testing` refer.
  `cljfmt` was applied to `binary_monitor.clj`, `trace.clj`, and
  `trace_test.clj` (whitespace only), so the whole `src/omkamra/vice` tree is
  now formatted.
- **Verification:** `clj-kondo` and `cljfmt` report **0 errors / 0 warnings**
  for the entire `src/omkamra/vice` tree; `*warn-on-reflection*` is clean; full
  suite **73 tests / 366 assertions, 0 failures** after reloading.

### Diary entry: `:features` binary occurrence channel (2026-10-11)

- **Status:** complete. The heavy per-occurrence evidence is now a compact
  binary channel, and the EDN unit stream is small.
- **Change (`features-version` 6 → 7):**
  - Unit-level evidence stays in `units.edn` (one EDN line per unit; ~22 k
    units on the full capture). Each IRQ invocation's occurrences are encoded
    into a binary sidecar `occurrences.bin` and the unit line carries only
    `:occurrences {:occurrence-offset O :occurrence-length L}`.
  - Encoding is varint + zig-zag deltas with a per-unit evidence dictionary:
    the distinct role-evidence tuples are written once (11 fields in
    `occurrence-evidence-keys` order), and each occurrence stores a varint
    occurrence-id delta, a zig-zag block-id delta, a zig-zag event-start delta,
    the event length, and an evidence id. Repeated occurrences therefore cost a
    few bytes and share one evidence map.
  - `reduce-units`/`units` decode the channel back into occurrence maps before
    handing a unit to the consumer, so `:classification` is unchanged and sees
    exactly the previous shape. `index.edn` records `:occurrence-file`.
  - New test `features-classification-test/binary-occurrence-channel-round-trips`
    encodes and decodes a synthetic occurrence block and asserts equality.
- **Measured on the full `no-booze-triad-v2` capture (15.3 M occurrences):**
  - `units.edn`: **870,668,158 → 33,435,974 bytes** (~830 MB → 32 MB);
    `occurrences.bin`: **38,517,194 bytes** (37 MB). Total ~830 MB → ~70 MB
    (**~12x smaller**).
  - `:classification`: **~24.5 s** (rebuilding classification and segments,
    reusing features) versus ~104 s parsing the EDN stream — **~4x faster**.
  - `:disassembly` rebuilt in ~13.8 s; a following `pipeline/build! :segments`
    reused every stage.
- **Notes:**
  - While iterating in a live REPL the pipeline kept rebuilding `:features`
    because `pipeline/versions` had captured the old `features-version`; after
    changing a stage version, the `pipeline` namespace must be reloaded too.
    A fresh JVM is unaffected. `adopt!` re-recorded the correct signatures.
  - 16 superseded generations (including the 830 MB v6 features generation)
    were pruned so only the committed generations remain.
- **Verification.** Full suite **74 tests / 367 assertions, 0 failures**;
  `clj-kondo`/`cljfmt` and `*warn-on-reflection*` clean.

### Diary entry: pipeline-v2 complete (2026-10-11)

- **Status:** complete. Phases 0–9 are delivered and the project is wrapped up.
- **Delivered end to end:**
  - Immutable, fail-closed capture and an exact shared reader with logical
    write inference.
  - `:dedupe` structural image (contexts, frames, levels, context-qualified
    CFGs, logical windows) — **~8x faster** than the original after the
    reflection, primitive-decode, and mutable-state work (full capture
    ~3.4 min, ~231 k events/s, byte-identical output).
  - `:features` streaming with a **binary occurrence channel**
    (`occurrences.bin`, delta + evidence-dictionary encoded): full-capture
    footprint ~830 MB → ~70 MB and classification ~104 s → ~24 s.
  - `:classification`, `:segments` (display-handler demoparts, loader/decruncher
    roles, handoff-refined boundaries), and `:disassembly` (annotated `.asm`,
    each block once per file).
  - `omkamra.vice.pipeline`: dependency closure, signature + input-provenance
    reuse, `:all` / multiple targets, **parallel independent branches**,
    **transactional all-or-nothing commit**, and an **OS-visible writer lock**.
  - v1 orchestration retired, the `omkamra.vice.v2.*` prefix removed, and the
    test suite made **capture-free** (synthetic dataset).
- **Validation:** `no-booze-triad-v2` → 3 file groups / 10 demoparts (3 / 3 / 4),
  0 gaps, 0 overlaps, every segment fully represented with no neighbour spill;
  `fairlight-intro` → one transition plus one demopart.
- **Quality:** full suite **74 tests / 367 assertions, 0 failures**;
  `clj-kondo`, `cljfmt`, and `*warn-on-reflection*` clean across
  `src/omkamra/vice`.
- **Optional future work:** see **Possible future work** above. The main
  candidates are `:dedupe` per-segment map-reduce for core scaling (analysed as
  high-effort/divergence-prone, with `:features` now the larger bottleneck),
  `:video`/`:assets` rebuild and coalescing, a handful of fidelity items
  (NMI vs IRQ, indirect `$01`, piecewise leveling), and general housekeeping.
