# Chunked VICE capture implementation plan

## 1. Problem and goals

A complete modern demo/trackmo capture cannot remain in one in-memory artifact. The execution may run indefinitely, while IRQ-driven loading and decrunching continually replace code and data from earlier parts.

The implementation must:

- bound live memory used by the decoder;
- persist capture data incrementally while VICE is running;
- allow capture to continue while the final part runs indefinitely, with the user responsible for stopping it before disk usage becomes excessive;
- preserve chronological execution, inferred writes, control-flow data, and useful video state;
- identify likely loaders, decrunchers, and demo parts without making capture depend on perfect classification;
- produce one dedicated directory per capture;
- make each persisted physical chunk independently inspectable;
- recover cleanly from writer, FIFO, VICE, and process failures.

The implementation should not initially require a perfect semantic classifier. Physical chunking is the memory and durability mechanism; semantic classification is an evidence-producing layer on top.

The end goal is a tool that emits one `.asm` listing per semantic segment
(fast-loader, decruncher, demopart, and later music), where each listing
contains only the code that belongs to that segment. Boundaries and labels must
come from general electronic signals - where writes go, whether the serial bus
is driven, whether a destination advances - never from code patterns observed
in one particular demo.

## 2. Terminology

Use these terms consistently:

- **Capture session** — the public asynchronous object returned by `omkamra.vice.capture/start!`.
- **Recorder** — the decoder-side FIFO recorder returned by `omkamra.vice.decoder/start-capture`.
- **Physical chunk** — a bounded, persisted chronological unit. This is the unit that limits memory and receives `chunk-NNNNNN.*` files. Raw chunks are immutable capture sources.
- **Execution event** — one parsed executed-instruction sample, identified globally by `:event-index`.
- **Semantic activity** — evidence that a range of execution is loader-like, decruncher-like, or demopart-like. Activities may overlap in time.
- **Semantic segment** — a higher-level part/epoch inferred from activities and stable execution signatures. It may span multiple physical chunks and may begin or end in the middle of one raw chunk. It is a derived view or materialization, not a replacement for raw chunks.
- **Chunk boundary** — a forced physical boundary caused by size, time, or queue constraints rather than a semantic transition.
- **Candidate boundary** — a classifier proposal that requires confirmation/hysteresis before becoming a semantic boundary.

A traditional demo may produce non-overlapping semantic ranges:

```text
loader -> decruncher -> demopart -> loader -> decruncher -> demopart
```

A trackmo may have a demopart rendering while a loader and decruncher execute in IRQ context. Therefore semantic activities must not be represented as an exclusive partition of the instruction stream.

Online chunk boundaries are allowed to cut through semantic segments. Post-processing can later create semantic segment indexes or standalone materializations by streaming event ranges from the immutable raw chunks.

## 3. Current architecture to preserve

The current path is:

```text
VICE monitor trace FIFO
  -> omkamra.vice.trace/reduce-records!
  -> decoder stream ingester
  -> instruction interning
  -> event indexing
  -> control-flow boundary marking
  -> inferred-write analysis
  -> basic-block construction
  -> block interning and run-length encoding
  -> one final pipeline-v2 artifact
```

Important existing behavior:

- trace parsing is allocation-conscious and callback-based;
- instruction and block interning use mutable side state for throughput;
- inferred writes mutate an evolving memory image;
- finalization persists chunked raw stages rather than a monolithic artifact;
- full samples are opt-in with `:retain-samples?`;
- VICE remains paused when `decoder/stop-capture` returns;
- capture finalization commits raw chunks only; derived analysis runs only
  through an explicit analysis request.

Segmentation should evolve the stream ingester and finalization path rather than introduce a second trace parser or a second execution model.

The initial design does not attempt to compress an indefinitely repeating final part into bounded disk usage. It relies on the caller to stop the capture while the desired final-part window is running.

## 4. Target architecture

```text
trace FIFO
  -> trace parser
  -> capture-time raw execution reduction
       - local instruction/block dictionaries
       - inferred writes
       - timing evidence
       - physical boundaries
  -> physical chunk builder
  -> bounded writer queue
  -> background chunk writer
  -> committed raw capture manifest/chunks
  -> ordered analysis stages
       - structure/video views
       - feature summaries
       - semantic activities and parts
       - segment indexes/materializations
       - assets and reports
```

The stream reader remains the producer. It must not retain closed chunks. Each closed chunk transfers ownership of its immutable data to the writer queue, and the builder starts with fresh local state.

The writer must use a bounded queue. When the queue is full, the implementation must apply an explicit policy:

1. preferably block briefly while recording queue pressure;
2. fail the capture with a clear `:chunk-writer-backpressure` reason if the writer cannot catch up;
3. never silently drop trace records.

A permanently blocked producer can eventually stop VICE's FIFO writer and distort the capture, so queue size and writer throughput need metrics and tests.

## 5. Physical chunk model

A physical chunk is a self-contained local artifact with global positioning metadata.

Proposed chunk shape:

```clojure
{:format :omkamra.vice/chunk-v1
 :capture-id "..."
 :chunk-number 12
 :event-range [1234000 1456789]
 :previous-chunk 11
 :next-chunk 13
 :boundary
 {:kind :forced-size
  :reason :max-events}
 :local-event-count 222790
 :events
 {:format :omkamra.vice/instruction-block-stream-v5
  :event-count 222790
  :instructions [...]
  :blocks [...]
  :block-runs [...]
  ;; optional :samples and :sample-keys
 }
 :stages
 {:decoded {...}
  :memory {...}
  :structure {...}
  :video {...}
  :semantics {...}}
 :summary
 {:event-count 222790
  :instruction-count 83
  :block-count 19
  :write-count 45012}}
```

Local event indexes may remain zero-based inside the chunk, but every event-derived record must also be convertible to the global event range. Do not make consumers reconstruct global positions from filenames alone.

Each chunk should have a stable data file set, but should not have a persisted assembly file:

```text
chunk-000012.edn
```

For large data, use separate binary files rather than embedding huge EDN byte vectors:

```text
chunk-000012.edn
chunk-000012.evidence.bin
chunk-000012.samples.bin       # optional
```

Assembly is generated only for derived semantic segments, after classification.

Use a hybrid chunk format:

- EDN for metadata, dictionaries, indexes, summaries, and small control structures;
- binary sidecars for compact evidence columns, per-chunk RAM snapshots, semantic-segment RAM attachments, and optional full samples;
- explicit references from the EDN chunk descriptor to each sidecar, including version, encoding, byte range, and checksum where practical.

This keeps the persisted format inspectable without forcing large numeric arrays through EDN. The file contract should allow sidecars to be memory-mapped or streamed by post-processors.

### Evidence retention for future refinement

Chunks should retain a compact replayable evidence stream by default, provided benchmarks confirm that its disk footprint is acceptable relative to the current capture format. This evidence is distinct from `:retain-samples?`:

- **compact evidence** is required for future classifier versions and should contain only columns needed for replay/classification, such as local event index, instruction identity, raster line, CPU cycle, and selected I/O/register evidence;
- **full samples** remain opt-in forensic data containing the complete per-instruction register/timing sample;
- inferred writes remain in their existing compact representation and should be referenced rather than duplicated in the evidence stream.

Evidence should use packed positional arrays or a binary sidecar, not one Clojure map per event. The reader may use it for offline refinement, then discard it. The live decoder must retain only the current chunk's evidence, and the writer must release it after persistence. Disk and heap benchmarks must compare compact evidence against the current monolithic capture format; if the overhead is excessive, reduce the columns or make evidence profiles configurable rather than silently duplicating full samples.

The forensic full-sample mode should preserve the useful semantics of the current `:retain-samples?` option: for every event, retain the complete timing/register sample (`:raster-line`, `:cpu-cycle`, `:a`, `:x`, `:y`, `:sp`, `:flags`, and `:global-cycle`) alongside the instruction identity. Store these samples as chunk-local binary columns rather than Clojure maps. This keeps the mode available for detailed analysis while bounding live memory to the open chunk and making the data streamable during post-processing.

## 6. Capture directory and manifest

`capture/start!` should resolve a unique output directory for the capture itself. Existing capture directories must never be replaced or cleaned automatically. The directory name should include the capture ID and local start timestamp in `YYYYMMDD-hhmmss` form:

```text
<output-dir>/YYYYMMDD-hhmmss-<capture-id>/
  manifest.edn
  chunks/
    chunk-000001.edn
    chunk-000001.ram.bin         # writable RAM at chunk start
    ...
  analysis/                      # replaceable, explicitly requested outputs
    manifest.edn                 # analysis-stage status and provenance
    stages/
      structure/
        index.edn
      video/
        index.edn
      assets/
        index.edn
      features/
        index.edn
      ...
    segments/                    # outputs of a later stage
      segment-000001.edn
      segment-000001.asm
    assets/                      # optional later-stage outputs
  profile.edn                    # optional
```

The manifest should retain both the logical `:capture-id` and the concrete `:capture-directory`. If the exact timestamped directory already exists, fail before starting VICE rather than modifying the existing capture.

Capture status and analysis status are separate. The capture manifest becomes
`:stopped` and `:finalized? true` once all raw chunks are committed; this does
not imply that any analysis stage has run. `:analysis` records the requested
stage, completed stage provenance, and references to the replaceable analysis
manifest. An analysis run may update derived state without changing raw capture
status or raw chunk entries.

The manifest is the durable index and should be small enough to rewrite atomically.

Proposed shape:

```clojure
{:format :omkamra.vice/capture-v1
 :capture-id "demo"
 :capture-directory "captures/20261009-143012-demo"
 :input "/path/demo.prg"
 :status :running                 ; or :stopped/:failed
 :capture-mode :continuous
 :trace-transport :fifo
 :options {...}
 :chunks
 [{:number 1
   :file "chunks/chunk-000001.edn"
   :ram-file "chunks/chunk-000001.ram.bin"
   :event-range [0 823411]}
  ...]
 :analysis
 {:status :not-run
  :requested-stage nil
  :completed-stages []
  :stages {}
  :manifest-file "analysis/manifest.edn"}
 :current-chunk 13
 :event-count 2456789
 :finalized? false}
```

Manifest updates should be atomic:

1. write `manifest.edn.partial`;
2. flush and close it;
3. rename it to `manifest.edn`.

Chunk files should follow the same pattern. A chunk is visible in the manifest only after its final file has been durably renamed. On startup or recovery, `.partial` files can be deleted or inspected explicitly. Analysis stage outputs use the same atomic staging and rename rules, but their failure must not invalidate committed raw chunks.

## 7. Chunk builder and lifecycle

Replace the single long-lived stream analysis state with a capture-level coordinator containing:

```clojure
{:global-event-count ...
 :open-chunk ...
 :chunk-number ...
 :writer-queue ...
 :writer-thread ...
 :manifest-state ...
 :memory-state ...}
```

The open chunk owns:

- local instruction dictionary;
- local block dictionary and block runs;
- local writes and timing data;
- local boundary and span data;
- optional samples;
- chunk-local initial memory reference and final memory state;
- feature summaries used by classification.

The capture-level state owns only what must cross physical boundaries:

- global event count;
- global memory image;
- manifest and writer lifecycle.

Classifier state, feature windows, and cross-segment signatures belong to
ordered analysis stages and are reconstructed from committed raw chunks. They
must not be required to keep a capture running.

When a boundary is selected:

1. finish the current instruction/block transducer;
2. finish pending write inference and timing;
3. derive the chunk-local raw stages;
4. attach physical boundary metadata;
5. enqueue an immutable write job;
6. reset local dictionaries and collections;
7. continue with the current global memory image.

Semantic boundaries and classification metadata are added later by analysis;
physical chunk closure must never depend on them.

The background writer must never access mutable ingester atoms after ownership transfer. This is essential because the current ingester uses side-effecting interning state and cannot safely be retried through CAS operations.

## 8. Boundary policy

Physical closure should happen on the earliest of:

- an optional future capture-time physical hint (not required in Phase 1);
- maximum event count;
- maximum number of retained writes;
- maximum elapsed emulated frames;
- maximum estimated heap size;
- explicit stop/finalization.

A post-processing semantic boundary is independent of physical closure. It may split a raw chunk or combine ranges from multiple raw chunks when producing a semantic segment.

Recommended initial defaults should be configurable and conservative. The first implementation should enforce only event count and a conservative estimated byte limit:

```clojure
{:chunk-max-events 50000
 :chunk-max-bytes 128000000
 :chunk-queue-capacity 4}
```

Frame and write counts should initially be diagnostics rather than hard boundaries. They can become optional limits after benchmarking with real captures.

A forced boundary must not be reported as a semantic transition. Record the reason explicitly:

```clojure
{:kind :forced-size :reason :max-events}
```

A semantic boundary should include the evidence window that caused it, not just the resulting label.

## 9. Analysis evidence extraction

### 9.1 Loader evidence

Collect low-cost counters and recent event samples for:

- calls or jumps into known KERNAL IEC/disk routines;
- repeated CIA2 serial/IEC register accesses;
- repeated transfer-like I/O patterns;
- execution of a compact routine while transfer-related I/O is active;
- subsequent writes into a destination area.

The classifier should support configurable address tables because custom fastloaders will not use only KERNAL entry points. Ship a default C64 KERNAL/IEC configuration with every classifier version, and allow users to provide an override configuration when tuning for a particular demo or loader. The effective configuration and its version must be recorded in the derived analysis output and manifest.

Initial output should be evidence such as:

```clojure
{:kind :loader
 :confidence 0.72
 :signals #{:iec-register-activity :kernal-iec-call}
 :event-range [.. ..]}
```

A load is recognised by **reading** `$dd00`, not by writing it. On the IEC bus
the C64 is the master: it samples CLOCK/DATA by reading, and only writes the
register to handshake. `$dd00` bits 0-1 additionally select the VIC bank and are
written by raster routines too, so a write counts as serial traffic only when it
changes bits 2-7. A transfer is then a phase in which serial-register access is
sustained across units. That test is IRQ-agnostic, so it also covers trackmo
loaders that run inside an IRQ handler. Measured on the Triad capture, the two
fast-loader transfers show 816k and 490k serial accesses while every other
uncovered range shows 0-8, and their inferred write counts alone (about 10k)
were far too weak to identify them.

### 9.2 Decrunch evidence

Use existing inferred-write data and the evolving memory image to track:

- write rate per event/frame;
- contiguous or broad destination coverage;
- writes into previously occupied code/data areas;
- write/read-modify-write ratios;
- transition from a write-heavy routine into newly written code.

Avoid treating every large clear/copy as a decruncher. Initially expose `:possible-decruncher` evidence with confidence and preserve the underlying counters.

A decruncher moves data from memory to memory and never touches the serial bus,
so the general discriminator is ordinary-RAM writes with **no** serial-register
access. A real decruncher also writes its destination region in increasing or
decreasing address order while that region expands; copy loops such as the
Triad's `LDA ($2F),Y` / `STA ($2D),Y` with `INC $2F` / `INC $2D` at
`41.1M-41.5M` show exactly this. Scattered RAM writes with no monotonic
destination are a calculation/support routine producing data for another
handler, not a decruncher. The current `:ram-frontier-movement` is computed over
all ordinary RAM rather than the destination region, so a destination-relative
monotonicity metric is still required before decruncher and calculation can be
separated reliably.

### 9.3 Demopart/IRQ evidence

Build a rolling frame fingerprint from:

- IRQ entry instruction IDs or PCs;
- ordered IRQ routine identities;
- writes to `$D012` and their raster positions;
- relevant VIC register writes;
- approximate timing between IRQ entries;
- number and ordering of IRQs per displayed frame.

A fingerprint should be stable over a configurable window of frames before being promoted to a part identity. A new demopart candidate becomes stronger when combined with:

- completion of loader/decrunch evidence;
- a transfer into newly written code;
- a sustained new IRQ fingerprint;
- meaningful VIC configuration change.

The fingerprint should be persisted in summaries so classification can be improved offline without replaying the entire trace.

The hardware IRQ vector is `$fffe/$ffff`. While KERNAL ROM is mapped in, it
reads as the KERNAL entry `$ff48`; while ROM is banked out, the program's RAM
word is authoritative. Detect an IRQ entry whenever the executed PC equals that
effective vector, independently of control flow, because an IRQ whose entry
follows `RTI` is invisible to an unexpected-control-flow test and was lost for
258 of the Triad's 498 chunks before this rule existed. The KERNAL entry then
dispatches through `$0314/$0315`, so resolve it to the installed handler; parts
that all enter at `$ff48` are otherwise indistinguishable. A handler that runs
in only some frames of an epoch is sampling noise, not an IRQ topology change.

### 9.4 Peripheral register timelines

The raw write history should support a precise timeline of memory-mapped hardware writes. Every inferred CPU write should retain, at minimum:

```clojure
{:event-index ...
 :pc ...
 :address ...
 :value ...
 :raster-line ...
 :cpu-cycle ...
 :instruction-raster-line ...
 :instruction-cpu-cycle ...
 :write-cycle-offset ...}
```

Post-processors should classify these generic writes into address domains such as VIC-II, SID, CIA, color RAM, and other configurable I/O ranges without duplicating the underlying write records. This supports derived timelines for a semantic segment, for example:

```text
raster 120, cycle 14: VIC $D012 <- $80
raster 120, cycle 63: SID $D418 <- $0F
```

The current decoder already derives VIC-register history, while SID writes currently remain generic inferred writes. The new format should make both available through the same compact write history and a versioned peripheral-address configuration.

Write timing is inferred from instruction-start trace samples and the following event; it is not a direct VICE bus-event stream. The implementation must retain the timing fields, make PAL/NTSC raster geometry configurable, and validate write-commit calculation against known instruction timings. The timeline should distinguish sampled instruction timing from estimated write-commit timing rather than presenting the latter as directly observed.

Domain classification must use the CPU port `$01` value at the write. When the
I/O area is banked out, `$d000-$dfff` is ordinary RAM: those writes must not
inflate the IEC/VIC counters and must be included in decrunch destination
footprints. The Triad contains a banked-out range where all 256 `$dd00` writes
are RAM, which previously produced a false IEC signal.

## 10. Semantic model

Do not force semantic activities into one enum per event. Use two related structures:

```clojure
{:activities
 [{:id 4
   :kind :loader
   :event-range [800000 820000]
   :confidence 0.84
   :signals [...]}
  {:id 5
   :kind :decruncher
   :event-range [815000 830000]
   :confidence 0.91
   :signals [...]}]
 :parts
 [{:part-id 3
   :kind :demopart
   :event-range [700000 900000]
   :fingerprint-id "..."
   :confidence 0.93}]}
```

Activities may overlap. Parts are longer-lived semantic epochs and may span many physical chunk files. A physical chunk may contain the end of one part, a loader activity, and the beginning of another part.

Classify each unit (IRQ frame or write window) by a **behavior flag set** rather
than one exclusive label, so a handler that combines activities - for example a
raster routine that also loads, or a trackmo loader driven from a raster IRQ -
is represented as a combination:

- serial reads of `$dd00` (bits 2-7) -> loader;
- VIC register writes -> raster/demopart;
- SID writes -> music;
- ordinary-RAM writes with a broad, monotonic destination and no serial access
  -> decruncher;
- ordinary-RAM writes that are neither monotonic nor broad -> calculation (a
  support routine producing data for another handler).

Segment labels then come from the dominant or combined flags of their units,
which is what allows one IRQ handler to be reported as, say, both raster and
loader instead of being forced into a single kind.

The initial offline classifier should use hysteresis:

- maintain a candidate label and evidence score;
- require evidence over a window, not one instruction/frame;
- delay closing a semantic epoch until the new label is stable;
- retain only a bounded in-memory lookback window so the boundary can be placed before confirmation;
- mark uncertain boundaries as candidates rather than silently asserting them.

Offline classifiers may read arbitrary neighboring persisted chunks for lookback and context. This keeps live capture memory bounded while allowing later classifier versions to improve boundary placement across chunk boundaries.

## 11. RAM snapshots and cross-segment state

The decoder currently maintains a mutable 64 KiB CPU-visible memory image for write inference and video reconstruction. This image must continue across physical boundaries. That live image is an analysis implementation detail; persisted snapshots should contain RAM, not ROM areas.

Every physical chunk should begin with a writable-RAM snapshot stored as a compact binary sidecar, for example `chunk-000001.ram.bin`. The chunk also owns the write history needed to advance that snapshot through the chunk's event range. This makes each chunk independently replayable and provides a natural key-frame structure for long captures.

A chunk snapshot records:

- the global event/frame position at the chunk start;
- memory configuration, bank, and address ranges;
- writable RAM contents, excluding ROM-only or ROM-mapped ranges;
- the encoding and layout needed to interpret the binary sidecar.

Semantic post-processors can reconstruct RAM at any point by loading the initial snapshot of the containing chunk and replaying that chunk's inferred writes up to the target event. For a semantic segment spanning chunks, process the relevant chunk ranges in order. When the semantic segment is materialized, store the resulting RAM state as a segment attachment, for example `segment-000001.ram.bin`.

The analysis pipeline derives segment attachments from chunk snapshots and write histories. A future capture-time hint may optimize this, but semantic classification is not required during capture and the offline result remains authoritative.

Do not dump ROM-only or ROM-backed ranges into snapshots. Use VICE memory spaces/banks or an equivalent explicit range description so consumers know which RAM pages are present and which ROM data is intentionally omitted. If writable RAM is hidden by a ROM overlay, the snapshot format must document whether the underlying RAM page is included and how to address it. This is important for asset extraction and self-modifying programs.

A writable-RAM snapshot at every chunk boundary is the initial key-frame policy. Physical chunk boundaries provide the replay checkpoints. Additional periodic snapshots can be added later only if chunk sizes or replay costs make them useful.

The persisted representation must not compromise the live memory bound: after a chunk snapshot is handed to the writer, the decoder keeps only the current mutable analysis image.

## 12. Separate capture and ordered analysis stages

Capture and analysis are separate user-visible operations. A capture request
runs VICE, persists immutable raw chunks, atomically commits the capture
manifest, and returns the raw capture directory. It must not run derived
analysis as part of shutdown. This makes a successful capture useful even when
analysis is expensive, interrupted, or not wanted yet.

An analysis request targets an existing stopped capture and a stage number. The
analysis pipeline is an ordered chain of stages. Each stage consumes the raw
chunks and the committed outputs of its predecessors, then persists a new
layer of derived information—the next layer peeled back from the raw execution
universe. The initial stage registry is expected to evolve, but should begin
with roughly:

1. **structural indexing** — node/code-image versions, IRQ spans and
   control-flow views, inferred-write and peripheral timelines, and per-chunk
   structural summaries; this refactors the current bounded one-chunk analysis
   pass;
2. **behavioral feature extraction** — rolling IRQ/frame fingerprints, loader
   I/O evidence, write density, overwritten-region evidence, decrunch-like
   signals, and stable execution signatures;
3. **semantic activity classification** — versioned offline classification of
   overlapping loader, decruncher, and demopart activities, with confidence,
   signals, and evidence windows;
4. **semantic segment assembly** — combine activities into cross-chunk semantic
   segments, then write segment indexes, streamed materializations, and
   semantic-segment `.asm` files;
5. **assets and reports** — RAM replay and segment snapshots, VIC/SID
   timelines, extracted screen/charset/color/bitmap/sprite assets, and
   capture-level summaries.

The exact stage names and count are configuration, not a compatibility promise.
Every stage has a stable stage identifier, implementation version, input stage
versions, configuration, status, start/end timestamps, and output references.
A stage is complete only when its outputs and stage metadata have been atomically
committed.

The request semantics are:

- requesting stage `N` runs every missing prerequisite stage in order through
  `N`;
- requesting `analysis` means the highest configured stage and runs all
  missing stages through that stage;
- already-complete stages with matching versions and configuration are skipped;
- an explicit rerun or changed stage configuration reruns that stage and all
  dependent stages, while preserving the raw chunks and any still-valid prior
  outputs until replacement succeeds.

All stages must consume chunks lazily, one chunk or bounded event window at a
time. A stage may read neighboring chunks for context, but must not load the
complete capture into memory. Stage outputs are replaceable derived views;
raw chunks and their manifest entries remain immutable after capture commit.

The analysis manifest records the ordered stage graph, completed versions,
requested target, current progress, failure information, and output paths. A
failed stage leaves the capture valid and leaves the last successful outputs
readable. A later request can resume from the last compatible stage or rerun
the failed stage explicitly.

Analysis should be available both synchronously and asynchronously. Capture
status becomes terminal once raw persistence is complete; analysis status is
reported separately while an analysis request is running. `capture/stop!` must
not wait for analysis, and `capture/stop-async!` must not start analysis.

`omkamra.vice.asm/artifact->assembly` currently expects one pipeline artifact.
Add a semantic-segment-aware path that streams the relevant raw chunks or
completed stage outputs rather than loading the complete capture into memory.
Required modes include:

- render one completed semantic segment;
- render a manifest or analysis-stage summary;
- optionally render all semantic segments sequentially to a capture-level
  assembly file;
- optionally render a raw chunk for debugging without persisting it as a
  normal analysis output.

A semantic segment's assembly should retain the existing behavior:

- structural blocks emitted once;
- repeated exact code-image variants merged;
- varying operand bytes rendered as wildcards;
- chronological execution retained in EDN rather than expanded into assembly.

## 13. Public API changes

The chunked capture design may replace the existing capture APIs where that
produces a clearer contract. Capture and analysis must be independently
invokable; do not preserve legacy artifact readers solely for compatibility.

Candidate capture options:

```clojure
{:output-dir "captures"
 :capture-id "demo"
 :chunk-max-events 500000
 :chunk-max-bytes 128000000
 :chunk-queue-capacity 4
 :retain-samples? false}
```

Capture returns raw output paths as soon as the writer has drained. It does not
accept semantic-classification options and does not launch analysis implicitly.

Candidate analysis API:

```clojure
(analysis/stages capture-directory)
(analysis/run! capture-directory {:stage :structure})
(analysis/run! capture-directory {:stage :assets}) ; runs :writes and :video first
(analysis/run! capture-directory {:stage :all :chunk-parallelism 2}) ; opt in after heap sizing
(analysis/status capture-directory)
(analysis/iterate-chunks capture-directory)
```

`{:stage :all}` is the virtual target for the complete dependency graph.
Requesting a named stage resolves its transitive dependencies, then executes
only missing or stale stages in topological order. The analysis API should also
support an explicit rerun/force option and report which named stages were
skipped, executed, failed, or invalidated. `:latest` remains accepted as an
alias for `:all` during the transition.

Candidate status shape:

```clojure
{:capture-status :stopped
 :analysis-status :running
 :analysis-requested-stage :all
 :analysis-stages
 [{:id :writes :status :complete :version "writes-v1"}
  {:id :structure :status :complete :version "structure-v1"}
  {:id :video :status :complete :version "video-v1"}
  {:id :assets :status :running :version "assets-v1"}]
 :chunk-number 12
 :chunk-event-count 123456
 :chunk-count 11
 :chunks-written 11
 :chunks-pending 0
 :writer-status :stopped
 :writer-queue-depth 0}
```

Candidate decoder/reader functions remain:

```clojure
(start-chunked-capture conn options)
(chunked-capture-status capture)
(stop-chunked-capture capture)
(read-capture-manifest capture-directory)
(read-chunk capture-directory chunk-number)
```

The exact namespace and function names can be finalized during the separation
implementation, but the lifecycle contract is fixed: capture produces raw
chunks; analysis consumes a committed capture and advances through ordered
stages.

## 14. Failure and shutdown semantics

Shutdown order should be explicit:

1. disable VICE monitor logging;
2. close the FIFO reader and drain all parsed records;
3. close the current physical chunk;
4. wait for the writer queue to become empty;
5. finalize the manifest with `:status :stopped`;
6. restore ignored unsolicited monitor event types;
7. remove the FIFO;
8. leave VICE paused, as today.

On writer failure:

- capture the exception and writer stack/message in session state;
- stop accepting new chunk jobs;
- prevent a successful manifest finalization;
- attempt to drain/close the FIFO safely;
- report `:chunk-writer-error` in the stop reason;
- preserve already-committed chunk files and the last valid manifest.

On process interruption, the manifest should remain readable and identify the last committed chunk plus an incomplete capture status.

Analysis failures have different semantics from capture failures. If all raw
chunks were committed successfully:

- preserve the capture as valid and finalized;
- leave capture status `:stopped`, regardless of analysis status;
- record the failing stage, version, configuration, exception message, and
  generated outputs in the analysis manifest;
- leave the last successful stage outputs readable;
- return raw capture paths independently of the analysis error;
- allow the failed stage and its dependents to be rerun explicitly without
  recapturing.

Capture shutdown must never wait for analysis. A separate analysis request may
be interrupted or retried without reopening VICE or the FIFO recorder.

## 15. New artifact and API contract

This design does not need to read or write the previous monolithic pipeline artifacts. The old `:omkamra.vice/pipeline-v2` format, legacy readers, and compatibility adapters may be removed.

Define the new formats directly around the chunked model:

- `:omkamra.vice/capture-v1` — capture directory and raw-chunk manifest;
- `:omkamra.vice/chunk-v1` — immutable raw chunk data;
- `:omkamra.vice/analysis-v1` — ordered analysis-stage manifest and provenance;
- `:omkamra.vice/analysis-stage-v1` — one versioned stage output set;
- `:omkamra.vice/semantic-segment-v1` — derived semantic segment materialization;
- versioned binary sidecars for evidence, per-chunk RAM snapshots, semantic-segment RAM attachments, and optional samples.

The implementation may reuse useful internal decoder algorithms, but it should not preserve the old monolithic pipeline shape or old public API solely for migration. The new readers and post-processors should target the chunked formats directly. Explicit reruns operate on existing raw chunks within the new format.

## 16. Implementation phases

Current status is tracked here only. Phase 1 capture is implemented:
production capture creates a unique timestamped directory, writes an atomic
manifest, persists immutable numbered chunks through a bounded writer, exposes
writer/backpressure metrics, and targets the chunked format without a legacy
monolithic artifact. The Triad No Booze, No Phone, No Party capture validated
this path at 49,733,934 events and 498 chunks with contiguous ranges, zero
writer backpressure, and clean raw persistence.

Capture finalization now commits raw chunks and the capture manifest only; it
never launches derived analysis. `omkamra.vice.analysis` provides the first
ordered stage, structural/video indexing, as an explicit request. Its durable
analysis manifest, per-stage atomic staging, compatibility checks, force
reruns, and independent status form the Phase 2 foundation. Feature, classifier, asset, and semantic-segment stages are now registered.
The current refinement adds bounded cross-chunk write-to-execution handoff
evidence, per-unit behavior flags, a destination-relative decrunch footprint,
first-class loader/decruncher transition segments, KERNAL-default-handler part
demotion, exclusive part/transition code ownership, clustered file grouping,
per-demopart raster/music routine separation, and a compact disassembly input
that avoids re-parsing raw chunks. The remaining review work is loader-aware
IRQ-topology comparison over the replaceable segment index.

### Phase 1: physical chunking without classification — implemented

- [x] create capture directories and atomic manifest writing;
- [x] split the current stream ingester at configurable event-count and
  estimated-byte limits;
- [x] write numbered chunk EDN files in a background thread, with artifact
  finalization off the FIFO reader thread;
- [x] implement bounded queue, writer status, backpressure failure, and
  shutdown draining;
- [x] keep the current pipeline stages inside each chunk;
- [x] add capture/status output for chunk progress;
- [x] validate memory behavior, FIFO throughput, and writer throughput with
  the Triad No Booze, No Phone, No Party demo (49.7M events, 498 chunks,
  zero writer backpressure);
- [x] defer expensive structural/video derivation to a bounded analysis pass.

This phase supplies the memory/durability architecture. The remaining work is
to ensure capture shutdown does not invoke that analysis pass.

### Phase 2: separate capture from ordered analysis — in progress

- [x] make capture finalization commit raw chunks only and return without
  analysis;
- [x] define the ordered analysis-stage registry, versions, configuration,
  and durable analysis-manifest status; every stage implements an
  `:init`/`:step`/`:close` reducer contract. The current registry contains
  `:writes`, `:structure`, `:video`, `:assets`, `:features`,
  `:classification`, `:segments`, and `:disassembly`; `:video` depends on
  `:writes`, `:assets` depends on both `:writes` and `:video`, `:features`
  depends on `:writes`, `:structure`, and `:video`, `:classification` depends
  on `:features`, `:segments` depends on `:classification`, and
  `:disassembly` depends on `:segments`;
- [x] expose synchronous and asynchronous analysis requests for a numeric
  stage and `:latest` through `omkamra.vice.analysis/run!` and `run-async!`;
- [x] run only missing or stale registered stages in order; `:force?` reruns a
  selected stage and its registered downstream stages;
- [x] broadcast each raw chunk once to every requested stage in dependency
  order, while retaining bounded one-chunk-at-a-time execution; an in-flight
  dependency result is passed directly to its dependent stage, while a skipped
  compatible dependency is read from its committed output. The shared `:writes`
  stage persists a compact descriptor that references raw write records,
  materializes them once per raw chunk, and provides the shared in-memory
  result to both video and asset analysis;
- [x] process a bounded number of independent raw chunks concurrently
  (`:chunk-parallelism`, default 1; raise after heap sizing), allowing stages for separate chunks to
  overlap while keeping each stage's output staging and final publication
  atomic;
- [x] preserve last successful stage directories when a replacement stage
  fails, using sibling staging and rollback-safe replacement;
- [x] add durable stage progress and failure reporting separate from the raw
  capture status;
- [x] delete stale stage `.partial` directories before a retry and retain raw
  chunks after interrupted or failed analysis;
- [x] add the `:segments` stage after `:classification`. It persists exact
  cross-chunk source slices for transition and observed-demopart segments,
  retains the overlapping classifier activities, and materializes one `.edn`
  descriptor per segment. The subsequent `:disassembly` stage owns the
  independently rerunnable, globally deduplicated `.asm` dictionaries. Its
  merger keys structural templates by address/opcode and retains bounded
  per-byte value sets, masking self-modified operand bytes as wildcards; exact
  full-byte variants remain in immutable raw chunks and changed opcodes remain
  distinct templates. When the capture was not `:full-capture?`, disassembly
  reconstructs the CPU port `$01` at each event and omits only instructions
  executed while BASIC ROM (`$A000-$BFFF`) or KERNAL ROM (`$E000-$FFFF`)
  is actually mapped; those same addresses remain visible when the port maps
  them to RAM. The capture manifest persists this option for repeatable
  reruns. The existing `:assets`,
  `:features`, and classifier outputs remain independent derived views.

The ordered writes/structure/video/assets/features/classification/segments/
disassembly pipeline was validated against
`captures/20261009-082940-no-booze-triad-e2e`, the 49,733,934-event,
498-chunk Triad capture. Segment analysis ran through `run-async!` with
`:chunk-parallelism 4`; its chunk projections remained bounded. The separate
disassembly close pass reports its assembly-materialization progress while it
streams raw source chunks into globally deduplicated dictionaries. The
RAM-only decrunch evidence refinement removed VIC/SID/CIA-driven false
positives from the late demo, leaving eight observed IRQ-demopart epochs and
four loader/decrunch transition clusters. The `disassembly-v3` rerun
regenerated 12 Triad assembly files with the BASIC/KERNAL ROM filter;
high-memory addresses remain only when the captured CPU port indicates RAM
mapping. A subsequent `:latest` request skips all compatible stages. The
previous flat
`analysis/chunks/` output is left untouched; new stage output is written under
`analysis/stages/writes/`, `structure/`, `video/`, `assets/`, `features/`,
`classification/`, `segments/`, and `disassembly/`.

### Phase 3: feature summaries — implemented

- [x] add rolling IRQ/frame fingerprints;
- [x] add loader I/O counters;
- [x] add write-density and overwritten-region counters;
- [x] persist feature summaries in each chunk and the manifest;
- [x] expose feature diagnostics in analysis status.

`analysis/run!` now provides a versioned `:features` stage. It consumes the
existing `:writes`, `:structure`, and `:video` views one raw chunk at a time,
then persists exact write/domain counters, ordinary-RAM write counts, IEC and
configurable KERNAL-range activity, executed-code overwrite counts, observed
IRQ-frame fingerprints, and bounded VIC/$D012 samples. Hardware-domain decoding recognizes the VIC-II,
SID, and CIA mirrored I/O ranges, so intentionally mirrored demo writes remain
visible to later analysis. Rolling fingerprints are local to each physical
chunk; the persisted frame records retain global event ranges so the classifier
can join neighbouring chunks without recapturing. The analysis-stage index and
`analysis/status` retain capture-level aggregate diagnostics. The Triad No
Booze, No Phone, No Party capture (`20261009-082940-no-booze-triad-e2e`) was
processed successfully: 49,733,934 events across 498 chunks, with 11,624,495
inferred writes and no raw-capture rewrite.

### Phase 4: conservative offline classification — implemented

- [x] add candidate boundaries and hysteresis;
- [x] label loader/decruncher activities with confidence and evidence;
- [x] identify stable demopart fingerprints;
- [x] distinguish semantic transitions from forced physical boundaries;
- [x] add cross-chunk lookback handling;
- [x] make classifier versions and configurations explicit stage inputs.

`analysis/run!` now provides a versioned `:classification` stage after
`:features`. It persists per-chunk evidence candidates, then consumes those
chunks in manifest order during stage close so hysteresis and fingerprint
lookback cross physical chunk boundaries without retaining the capture in
memory. Loader and decruncher activities may overlap; demopart activities are
promoted from stable IRQ fingerprints and also appear as numbered `:parts`.
Physical forced boundaries remain in a separate `:physical-boundaries` index,
so they are not mistaken for semantic transitions. Confidence thresholds,
lookback length, and evidence-window size are recorded in the stage
configuration and can be changed to rerun classification without recapturing.
The Triad capture was classified successfully with four chunk workers, yielding
498 physical-boundary records, 2 loader activities, 3 decruncher activities,
and 8 inferred IRQ-demopart epochs. Before the Phase 5 refinement, the
classifier retained one coalesced stable-fingerprint `:demopart` activity while
the semantic segment stage split it into those eight epochs.
Mapped-register-heavy raster activity no longer extends decruncher activities
into the final demo part.

### Phase 5: semantic segments and offline refinement — in progress

- [x] add semantic-segment cross-chunk materialization and replaceable
  part/activity indexes;
- [x] add the separately rerunnable `:disassembly` stage with one globally
  deduplicated assembly dictionary per segment;
- [x] omit boot BASIC/KERNAL ROM instructions from non-`:full-capture?`
  assembly while retaining RAM execution by reconstructing the live `$01`
  port;
- [x] make decrunch write-rate evidence count ordinary RAM writes only,
  excluding VIC/SID/CIA/color-RAM and other mapped-register activity;
- [x] refine effect boundaries without recapturing: a coarser IRQ/VIC frame
  signature now creates only a candidate boundary, which is promoted after a
  configurable stable-frame window (`:signature-stability-frames`, default
  four frames). Exact fingerprints remain forensic evidence and are not used
  directly as segment boundaries;
- [x] split coalesced stable-fingerprint demopart activities at meaningful
  frame gaps and stable frame-signature changes instead of leaving one broad
  classifier activity;
- [x] tighten loader/decruncher boundaries using strong-evidence onset/exit
  rather than hysteresis-expanded ranges. The retained hysteresis extent is
  preserved separately as lead-in/trail-out metadata;
- [x] add bounded destination-footprint evidence for ordinary RAM writes:
  unique-address count, address span, forward/backward frontier movement, and
  exact adjacent-frame repeated-footprint detection within a chunk and a
  compact cross-chunk fingerprint check. A `BitSet` is retained only for a
  frame in the active chunk; persisted feature records contain a compact
  fingerprint and counters. This distinguishes an advancing decruncher
  destination from repeated screen, bitmap, or raster-operand copies;
- [x] keep loader evidence tied to IEC/KERNAL activity and separate it from
  RAM-write-rate evidence, so slow disk-driven writes are not classified as
  decrunching;
- [x] assign exclusive segment roles or explicit `:compound` roles to
  overlapping activities, clip activity evidence to segment ranges, and
  expose boundary confidence in the segment index;
- [x] add a durable EDN/text boundary-audit report listing adjacent segment
  overlaps/gaps, clipped activity ranges, assembly files, and
  confidence/signals for repeatable manual review;
- [x] add interrupt-independent write-window evidence for classifier units.
  `:features` now partitions a no-IRQ chunk, and a substantial pre-IRQ
  interval, into configurable bounded `:write-window` units (10,000 events by
  default). These retain the same ordinary-RAM write, destination-footprint,
  and loader counters as IRQ frames but intentionally have no fingerprint, so
  they cannot manufacture a demopart. A short pre-IRQ physical-chunk fragment
  is folded into the following IRQ unit rather than becoming a tiny negative
  observation that breaks cross-chunk hysteresis;
- [x] distinguish frame-epoch candidates from confirmed semantic demoparts.
  A sustained IRQ epoch is retained as an `:effect-candidate` and becomes a
  `:demopart` only with stable execution evidence, a meaningful signature
  transition, or corroborating loader/decrunch completion. The Triad rerun
  retains 11 effect-candidate epochs and four materialized demoparts;
- [x] make signature-change promotion depend on signature distance and
  semantic corroboration, not exact equality of every coarse field. Small
  VIC-write-band changes and event-gap-only splits remain candidates unless
  IRQ topology, VIC configuration distance, execution identity, or transition
  evidence also changes;
- [x] add visualizations or summaries for IRQ fingerprints, memory writes,
  destination footprints, and part transitions. Each classifier unit now
  persists its `:behavior-flags` and `:destination-*` profile (unique-address
  count, range, page count, compact page bitmap), and the replaceable
  `boundary-audit.edn`/`.txt` reports each segment's signals, clipped
  activities, gaps and transitions;
- [x] verify semantic `.asm` boundaries against emitted structural dictionaries
  and activity evidence. Inspecting the Triad transition assemblies confirmed
  the decruncher/loader materialisations: `segment 19` sets up screen and
  colour RAM and then `JMP $0400; SEI; STA $01`, while `segment 24` contains
  the `LDA ($2F),Y` / `STA ($2D),Y` copy loop with `INC $2F` / `INC $2D`;
- [x] prevent demopart `.asm` materializations from silently absorbing loader
  code by marking overlapping materializations `:compound` and recording the
  clipped activity evidence in both the assembly header and boundary audit;
- [x] suppress false decrunch classifications caused by stable IRQ/raster
  routines that self-modify code and produce high ordinary-RAM write counts.
  A repeated bounded footprint, stable IRQ fingerprint, and repeated
  write-then-immediate-execute behavior is classified as
  `:self-modifying-raster`, not `:decruncher`, while retaining the evidence;
- [x] require real decrunch evidence to combine an advancing or newly expanded
  ordinary-RAM destination footprint with a transition into newly written
  code. High write rate, density, or overwritten-code counts alone are not
  sufficient;
- [x] preserve bounded cross-chunk handoff evidence. The feature stage stores
  only tail-written and head-executed RAM address sets for a configured event
  window; ordered classification intersects adjacent chunks and extends the
  activity evidence across the physical boundary without retaining all events;
- [x] add provisional file grouping and trace-backed uncovered-range
  diagnostics. Strong loader starts delimit three reviewable file ranges;
  every range outside a semantic descriptor is summarized from persisted
  classifier units as `:write-heavy`, `:raster-heavy`,
  `:candidate-transition`, `:no-irq`, or `:low-evidence`. Short gaps receive
  an explicit continuation/transition recommendation instead of being merged
  silently. IEC-only transport evidence is retained and marked as possible
  custom-drive transport; it is not rejected merely because no CPU RAM
  destination is visible.
- [x] merge stable neighboring frame epochs when their coarse IRQ/VIC change
  is below the configured signature-distance thresholds. This prevents normal
  VIC write-band animation from manufacturing extra part candidates while
  preserving meaningful topology/configuration changes.
- [x] investigate the Triad's final-file grouping using the two detected
  fast-loader intervals as provisional file boundaries. The first two file
  ranges contain three part candidates each; the final range contains six and
  is retained as an explicit `:overfull` review result rather than being forced
  into the expected three-to-four range;
- [x] classify the large uncovered ranges in the boundary audit by cause:
  `:no-irq`, `:write-heavy`, `:raster-heavy`, `:low-evidence`, or
  `:candidate-transition`. IEC-only stretches also retain a possible
  custom-drive-transport interpretation;
- [x] audit short gaps between adjacent IRQ epochs and record whether each is a
  candidate continuation or candidate transition without silently merging it.
- [x] detect the hardware IRQ entry from the effective `$fffe/$ffff` vector
  instead of relying on unexpected control flow alone. With KERNAL ROM mapped
  in the vector reads as `$ff48`; with ROM banked out the program's RAM word is
  authoritative. The same rule is applied at capture time and re-derived
  offline from the persisted chunk, so an IRQ whose entry follows an `RTI` is
  no longer lost. This restored IRQ structure to 258 chunks that previously
  produced only 10,000-event write windows;
- [x] classify `$d000-$dfff` writes using the CPU port `$01` value at the
  write. When I/O is banked out those addresses are ordinary RAM, so they no
  longer inflate the IEC/VIC counters or suppress decruncher destination
  footprints;
- [x] resolve the KERNAL IRQ entry (`$ff48`) to the actually installed handler
  through `$0314/$0315`, and treat a handler that runs in only some frames as
  sampling noise rather than a part boundary. This consolidated the Triad
  candidate list from 22 to 16 frame epochs;
- [x] identify transfers generally from **serial-register reads**. On the IEC
  bus the C64 is the master: loading samples `$dd00` by reading it and only
  writes it for handshaking, so write counts measured the wrong direction.
  Serial writes are additionally masked to bits 2-7, because bits 0-1 are the
  VIC bank select that raster routines also store to the same register. In the
  Triad the two fast-loader transfers show 816k and 490k serial accesses
  against 0-8 everywhere else;
- [x] separate transfer/decode phases from raster phases by the per-unit
  hardware traffic mix (serial / VIC / ordinary RAM / SID) rather than by code
  shape, so the distinction is IRQ-agnostic and also covers trackmo loaders and
  decrunchers that run inside an IRQ handler;
- [x] replace the single traffic keyword with a per-unit *behavior flag set*
  (`:loader`, `:raster`, `:music`, `:calculation`; the `:decruncher` flag is
  added by a destination phase). The flag set is persisted per classifier unit
  and the old `:traffic-mode` is retained as its projection, so a raster
  routine that also drives the serial bus is reported as both `:raster` and
  `:loader`;
- [x] distinguish a decruncher from a calculation/support routine by
  destination-relative footprint growth. Features now persist a per-unit
  destination profile derived from ordinary RAM writes at or above a
  configurable floor, so zero-page and stack scratch (where decrunchers copy
  their own code and pointers) no longer dominate the minimum address. Ordered
  classification accumulates the page bitmap across consecutive RAM-dominant,
  serial-free units and requires a broad, contiguous destination plus a
  transfer into produced code or a following custom-handler part. This is what
  separated the Triad's `$0F89` calculation part from the real decrunchers;
- [x] promote the classified transfer/decode phases from bounded diagnostics
  into first-class `:transition` segments. The classifier accumulates a
  sustained serial-access loader phase and a destination-growth decruncher
  phase across physical chunks; segment assembly clusters them and materialises
  one assembly per transition;
- [x] give each segment exclusive ownership of the code executed in its own
  range. Part epochs are now clipped against loader/decruncher transitions that
  do not concurrently run a custom-handler IRQ frame, so a main-loop decruncher
  no longer leaks into the preceding or following part listing and the
  redundant default-handler effect fragments disappear. A transition that
  overlaps a part's own custom-handler frames (a trackmo loader running inside
  the last part) is preserved and marked `:compound` rather than being cut
  apart, because part and loader genuinely interleave there;
- [x] verify that the two compound file-swap transition assemblies really
  contain both responsibilities. `segment 5` (`5.3M-9.9M`) and `segment 14`
  (`25.16M-30.83M`) each contain a serial loader (`LDA`/`STA $DD00` around
  `$E4xx-$EExx`) together with memory-to-memory copy loops (`LDA ($FB),Y` /
  `STA ($FD),Y`, `LDA ($D6),Y`), confirming that a decrunch-load-decrunch file
  swap legitimately belongs in one materialization;
- [x] treat music and raster symmetrically: a routine is identified by the
  hardware domain its code writes. Features persist the per-unit VIC and SID
  writing PCs, `behavior-flags` marks `:raster`/`:music` on write presence
  rather than a dominant share (so a per-frame music player inside a VIC-heavy
  frame is visible), and each demopart gets a detected raster routine and music
  routine with anchor write PCs, a code range, write count, and per-frame
  cadence. The disassembly stage emits `segment-NNNNNN.music.asm` and
  `segment-NNNNNN.raster.asm`, selecting only the basic blocks that contain an
  anchor PC. All 13 Triad demoparts yield a self-contained music player; the
  final part resolves to the `$13C4-$13ED` player, and a resident player may
  legitimately be shared by several parts with different song data;
- [x] speed up disassembly materialization. Rather than re-parsing the large
  raw chunks once per segment, the disassembly step persists a compact per-chunk
  CPU-port timeline and the close pass reads the already-persisted structure
  chunk (which owns the execution and block runs). Rendering is unchanged
  byte-for-byte, while the close pass no longer re-reads roughly a gigabyte of
  raw EDN;
- [ ] a refined `topology-changed?` should ignore handlers classified as
  `:loader`, because a trackmo may replace every IRQ handler except the one
  loading the next part;
- [ ] revisit the decruncher/loader/calculation distinction against a trackmo
  whose IRQ loader decrunches in place. A decruncher may run inside a custom
  IRQ handler, and a calculation routine is expected to regenerate per-frame
  data that a raster routine then reads. The write-domain phase rules may need
  to become routine-consumption rules (output executed vs output read by a
  raster/music routine) once such a capture is available;

The current refinement produces `features-v15`, `classification-v19`,
`segments-v21`, and `disassembly-v12`. A demo segment is now identified as a
stable combination of concurrently-running routines: a demopart by its
installed IRQ handler set (so VIC-write-volume animation no longer splits an
epoch into fragments or prevents it from stabilising), and a loader or
decruncher as its own phase rather than a clustered file-swap transition. On
the Triad capture this yields 22 segments: **10 decrunchers, 2 loaders, and 10
demoparts**. Files 2 and 3 each contain exactly four demoparts; file 1
contains two. `:music` fires on 664 part frames as `#{:music :raster}` (plus
352 more with `:calculation`/`:loader`), and each demopart carries a detected
raster routine and music routine. The disassembly stage emits 10
`segment-NNNNNN.music.asm`, 10 `segment-NNNNNN.raster.asm`, 2
`segment-NNNNNN.loader.asm`, and 10 `segment-NNNNNN.decrunch.asm` listings in
addition to the 22 full segment listings; the final part's music listing is
the self-contained `$13C4-$13ED` player, while a resident player may be shared
by several parts with different song data. Segment assembly enforces
exclusive ownership: part epochs are clipped against non-concurrent
transitions, leaving no standalone default-handler effect fragments. The three
real files are recovered at `[0 5.38M]`, `[5.38M 25.16M]`, and
`[25.16M 49.73M]`; loader boundaries are clustered so that the momentary
serial idle gaps inside one fast-loader no longer fragment the capture into
spurious files. KERNAL-default handler epochs (`$0314 = $EA31`) are demoted to
non-part effects, but the check is made against every frame in an epoch,
because the epoch's stored signature can be stale; this is what kept the final
`$40B4` part from being demoted. Only two uncovered ranges remain:
`[0 181360]` (`:raster-heavy`) and `[45780000 45782087]`
(`:decruncher-stretch`). An IRQ entry that jumps to `$EA31` only to perform
stack restore and `RTI` is still a raster routine, so demotion is decided by
the resolved `$0314` handler rather than by the presence of `$EA31` in the
executed code. The two trackmo-overlap demoparts are marked `:compound` rather
than being cut apart, because their raster and loader routines genuinely
interleave. Consecutive decrunchers are deliberately allowed to coalesce into
one `:decruncher` segment: there is no per-decrun fingerprint (unlike
demoparts), and a slow decompressor followed by a fast RLE pass may
legitimately share one materialization. Calculation routines are not
separated yet. The rule will be the same symmetry: a calc routine is an IRQ
handler whose behaviour flags are exactly `#{:calculation}` (ordinary-RAM
writes with no VIC, SID or serial access), carved out by its RAM-write PCs
like raster and music; newer 3D-effect demos are where this is expected to
appear.

An earlier refinement remains repeatable and non-destructive to raw data. The
`structure-v3`, `features-v13`, `classification-v15`, `segments-v16`, and
`disassembly-v5` run was validated against the Triad capture through
`run-async!` with `:chunk-parallelism 4`. It completed with 49,733,934 events
across 498 raw chunks and 3,930 classifier units. Classification reports three
confirmed demopart activities, two loader activities, one real decrunch
transition, and two `:self-modifying-raster` activities. That segment index
then contained 19 materialized ranges: seven demoparts, nine effect
candidates, and three transitions. Vector-based IRQ recovery reduced the
uncovered area from 26.4M to 14.2M events.

The cross-chunk handoff evidence extended the confirmed decrunch observation
from the end of one physical chunk into the next (`40.7M–40.81M` events),
while removing the previous broad `37M–40.8M` false positive. That
`classification-v15`/`segments-v16` review was run asynchronously with
`:chunk-parallelism 4` against the immutable Triad capture. Recovering the IRQ
vector exposed the previously invisible parts in `13.3M–23.3M` events as
demoparts 7–9, plus a further part at `9.9M–12.1M`. Because those parts are now
visible, file 2 is explicitly `:overfull` with 13 candidates rather than
silently missing them; that overfull status is signature fragmentation, not
absent evidence. The port-aware write domain removed the false IEC signal in
`41.1M–41.48M` (all 256 `$DD00` writes are RAM under banked-out I/O), and the
read-based serial signal now names the two fast-loader transfers
`5.4M–9.9M` and `27.1M–30.8M`, with four serial-free RAM-decode stretches
classed `:decruncher-stretch`. Raw chunks remained
immutable while successful derived stage directories replaced their
predecessors atomically. Each classifier version replaces the current derived
semantic index after successful staging:

```text
analysis/
  manifest.edn
  stages/
    structure/
      index.edn
    video/
      index.edn
    assets/
      index.edn
    features/
      index.edn
    classification/
      index.edn
    segments/
      index.edn
      segments/
        segment-000001.edn
    disassembly/
      index.edn
      assemblies/
        segment-000001.asm
```

The derived index should record the classifier version, configuration, input capture format, and source chunk ranges. Reverting classifier code and rerunning the processor recreates the earlier analysis without rewriting or re-capturing the raw chunks.

## 17. Tests and acceptance criteria

### Unit tests

- physical chunk closes at event-count or estimated-byte limits;
- global/local event indexes remain consistent;
- state resets do not lose pending write inference or block tails;
- chunk dictionaries are local and complete;
- atomic file rename behavior is correct;
- bounded queue backpressure is observable;
- writer exceptions propagate to capture state;
- manifest updates remain valid after repeated writes;
- activity ranges may overlap;
- classifier hysteresis avoids one-frame transitions;
- IRQ fingerprints are stable for repeated frames;
- loader and decrunch evidence are recorded without false certainty.

### Manual end-to-end validation

End-to-end behavior will be validated manually against the demos and intros available on disk. The validation should cover, as useful examples:

- short intros and traditional load/decrunch/run sequences;
- demos with IRQ loaders overlapping a running part;
- decrunchers that overwrite earlier code/data;
- multiple semantic parts and seamless transitions;
- VIC/SID register timelines and asset extraction;
- long-running final parts stopped by the user;
- requesting later analysis stages and rerunning a stage on the same raw chunks.

The automated test suite should focus on unit-level transducers, serialization, chunk ownership, writer/manifest atomicity, and classifier primitives rather than attempting to reproduce full VICE/demo integrations.

Development workflow for the offline refinement:

- start the dev server with `clojure_start_dev` and evaluate through
  `clojure_eval`;
- run analysis with `(omkamra.vice.analysis/run-async! "<capture-dir>"
  {:stage :all :chunk-parallelism 4})` and poll
  `(omkamra.vice.analysis/status "<capture-dir>")`; a full Triad run takes
  roughly 10-15 minutes;
- use `clj-reload`, not `require :reload`, to reload edited namespaces.
  Initialize once with `(reload/init {:dirs ["src"] :output :quiet})` returning
  `nil`, then after editing call
  `(select-keys (reload/reload) [:unloaded :loaded])`. Plain `require :reload`
  leaves downstream namespaces stale;
- do not create aliases in the `user` namespace; `clj-reload` unloads and
  recreates namespaces, so aliases go stale. Use fully-qualified names;
- bumping a stage's `:version` invalidates it and its direct dependents, but not
  dependents-of-dependents, so bump `:segments` explicitly whenever `:features`
  changes;
- `artifact/read-chunk` returns the raw chunk; derived spans live under
  `analysis/stages/structure/chunks/`. Per-event timing samples are not
  retained, so re-derived IRQ boundaries take raster anchors from write-record
  `:raster-line` values.

### Acceptance criteria

The first useful implementation should demonstrate that:

1. live decoder memory remains bounded as event count grows;
2. a capture can run substantially longer than the current monolithic artifact allows;
3. stopping produces a complete, readable raw manifest and all committed chunks
   without waiting for analysis;
4. no trace events are silently lost;
5. chunk files can be analyzed independently and lazily;
6. requesting stage `N` runs missing stages in order through `N`;
7. a failed analysis stage leaves raw capture data and the last successful
   derived outputs readable;
8. semantic classification can be improved later without changing the
   physical capture mechanism.

## 18. Resolved decisions

- legacy pipeline artifacts and compatibility readers do not need to be retained;
- `chunk` is the term for physical files; `semantic segment` remains the term for inferred logical epochs;
- capture and analysis are separate user operations; capture shutdown never runs analysis;
- analysis requests for stage `N` execute missing or stale stages in dependency order through `N`, while an unspecified analysis target means the latest configured stage;
- raw chunks remain immutable; semantic segments are replaceable derived indexes plus streamed materializations over raw chunks;
- repeated stage runs replace only the affected derived analysis after successful staging, while preserving the last successful outputs on failure;
- persisted `.asm` files are produced for semantic segments only; raw chunk assembly is on-demand debugging output;
- Phase 1 uses event-count and estimated-byte limits for physical chunking; frame/write thresholds are diagnostics initially;
- Phase 1 is physical chunking only; semantic refinement and asset extraction are offline and versioned;
- chunk metadata uses EDN with binary sidecars for large numeric data and per-chunk RAM snapshots; semantic segments may carry derived RAM attachments;
- capture directories use local time in `YYYYMMDD-hhmmss` names and are never replaced;
- manifests are updated through atomic rewrite-and-rename;
- every physical chunk begins with a writable-RAM snapshot; semantic-segment RAM attachments are derived by replaying writes and ROM areas are omitted;
- no steady-state repetition compressor is required initially; callers stop captures during indefinitely repeating final parts;
- offline classifiers may read neighboring chunks for unbounded lookback;
- every classifier version ships a default loader configuration, with user overrides recorded in derived analysis.
