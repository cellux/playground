---
name: c64-analysis
description: "Build and inspect omkamra.vice analysis images from a finalized C64 capture."
compatibility: "Requires a finalized dataset root containing capture/ and the project nREPL. VICE is not needed."
---

# C64 analysis

`ds` is the dataset root containing `capture/`, not the `capture/` directory.
The reader validates the finalized manifest, artifact sizes, and checksums.
Analysis never modifies the raw capture. Evaluate examples through the project
nREPL with `clojure_eval`; after source edits, use the `clj-reload` skill.

## Build stages

```clojure
(require '[omkamra.vice.pipeline :as pipeline])

(def result (pipeline/build! ds :disassembly))
(pipeline/build! ds :all)
(pipeline/build! ds :disassembly {:force #{:dedupe}})
```

Targets are `:dedupe`, `:video`, `:assets`, `:audio`, `:features`,
`:classification`, `:segments`, `:disassembly`, `:report`, a sequential
collection of those keywords, or `:all`. `pipeline/build!` returns opened
images for the required dependency closure in `:images`, plus `:built` and
`:reused` lists. Independent stages run concurrently by default; use
`{:stage-parallelism n}` to limit that. `:configuration` maps stage ids to
stage options. `:force` rebuilds selected stages and any requested dependents.
Reuse checks stage version, configuration, capture checksum, upstream
signatures, and the exact upstream generations previously consumed.

The capture is an implicit input to every stage signature. Stage-to-stage
dependencies are:

- `:audio` and `:features` depend on `:dedupe`;
- `:classification` depends on both `:audio` and `:features`;
- `:segments` depends on `:classification` and `:features`;
- `:disassembly` depends on `:segments` and `:dedupe`;
- `:report` depends on `:segments` and `:disassembly`;
- `:assets` depends on `:video`; this branch is otherwise independent.

To inspect committed pointers, use:

```clojure
(require '[omkamra.vice.analysis :as analysis])
(analysis/read-manifest ds)
```

## Read the execution structure

```clojure
(require '[omkamra.vice.dedupe :as dedupe]
         '[omkamra.vice.segments :as segments]
         '[omkamra.vice.features :as features])

(def trace (dedupe/open! ds))
(dedupe/system-contexts trace)
(dedupe/read-context trace context-id)
(dedupe/read-range trace start end) ; half-open global event range

(def fs (features/open! ds))
(features/reduce-units fs rf init)   ; streaming, preferred for large captures
(features/handoffs fs)

(def ss (segments/open! ds))
(segments/segments ss)
(segments/file-groups ss)
(segments/audit ss)
```

`dedupe` is a structural execution image: blocks, contexts, dynamic frames,
occurrences, timing, and CPU-port changes. Its context ids include the initial
main context and interrupt contexts; stock KERNAL contexts are exposed by
`system-contexts` and are not automatically program behavior.

Segments classify ranges as `:demopart` or `:transition` and attach evidence
roles such as `:raster`, `:music`, `:loader`, `:decruncher`, and `:idle`. Roles
are evidence, not proof of a part boundary. `features` exposes behavioral units
and write-to-execute handoffs; `classification` exposes role-tagged spans.

## Inspect timing, memory, and devices

```clojure
(require '[omkamra.vice.reader :as reader])
(def cap (reader/open! ds))

(reader/read-events cap [start end])
(reader/read-writes cap [start end])
(reader/boundary-snapshots cap)
(reader/replay-state cap event-index)
```

Ranges are half-open. `(reader/boundary-snapshot cap event-index)` requires the
first event index of a committed raw chunk. `read-writes` infers logical CPU
mutations and includes `:domain`, `:canonical-address`, `:register`, and timing
fields; use `scan-vic-writes`/`scan-sid-writes` for filtered scans. The reader
also offers
streaming `reduce-events`, `event-pairs`, `scan-writes`, and replay-state scans.

Each chunk stores underlying RAM (`.ram`) and CPU-visible memory (`.mem`).
`boundary-snapshot` returns canonical CPU/device registers, CPU-port state,
colour RAM, and timing. `replay-state` returns the reconstructed state
immediately before its event index: underlying RAM, colour RAM, CPU-port bytes,
device write shadows, and explicit `:capabilities`. It does not model opaque
VICE state such as SID phase or unexposed DMA. Trust
write traces for write-only or opaque device effects.

## Video, assets, audio, and screenshots

```clojure
(require '[omkamra.vice.video :as video]
         '[omkamra.vice.assets :as assets]
         '[omkamra.vice.audio :as audio])

(def vi (video/open! ds))
(video/frames vi)
(video/configurations vi)
(video/expand-frame vi frame)

(def ai (assets/open! ds))
(assets/snapshots ai)
(assets/snapshot-ram ai snapshot-id)
(assets/snapshot-color-ram ai snapshot-id)

(def au (audio/open! ds))
(audio/music-candidates au)
```

`:video` derives ordered VIC-write patterns grouped by raster wraps and records
effective layout changes. A capture's VIC value blacklist preserves write
order/timing but appears from `expand-frame` as `:value nil` and
`:value-known? false`. `:assets` snapshots exact underlying RAM plus 1 KiB
colour RAM at layout changes. `:audio` reports SID evidence and music
candidates, not a definitive music format or authorship.

Raw screenshots are full VICE debug frames, including the border. Use recorded
`:width`, `:height`, `:x-offset`, `:y-offset`, `:inner-width`, and
`:inner-height` rather than assuming a fixed PAL geometry:

```clojure
(reader/chunk-screenshots cap)
(reader/screenshot-file cap (first (:chunks cap)))
```

A chunk screenshot belongs to that chunk's starting boundary. Chunk boundaries
are storage boundaries, not video-frame or dedupe-frame boundaries. Compare
nearby screenshots with measured VIC writes when a raster split or moving
object looks partial.

## Disassembly and report

```clojure
(require '[omkamra.vice.disassembly :as disassembly]
         '[omkamra.vice.report :as report])

(def asm (disassembly/open! ds))
(disassembly/segments asm)
(disassembly/role-files asm)
(disassembly/read-file asm "roles/raster.asm")

(def rp (report/open! ds))
(report/html-file rp)
(report/html rp)
```

Disassembly is generated from executed, deduplicated blocks, not a complete
static disassembly. Use RAM/assets to investigate code or data that never ran.
An operand rendered as `??` varied between executions; inspect raw event bytes
and the write history when its value matters. The report is one HTML overview
with relative links to generated assembly and selected boundary screenshots;
those linked artifacts remain in the dataset.

## Investigation rules

Separate observations from inferences. For raster work, measure register
writes (especially `$D011`, `$D016`, `$D020`, and `$D021`) by raster line rather
than inferring timing from one screenshot. For memory reconstruction, inspect
screen/charset/sprite pointers and compare several boundary states. Keep a
short record of the memory map, boot/transition path, per-frame evidence, and
verification results.
