---
name: c64-analysis
description: "Analyse a finalized omkamra.vice capture artifact: build the pipeline, inspect execution and hardware traces, and reconstruct C64 demo parts from code, memory, timing, audio, and screenshots."
compatibility: "Requires a finalized dataset root containing capture/ and the project's live Clojure nREPL."
---

# C64 analysis

`ds` below is the dataset root containing `capture/`, not `capture/` itself and
not a directory containing several datasets. The reader validates the
finalized manifest, artifacts, and checksums; do not analyze an active capture.

## 1. Build the analysis images

```clojure
(require '[omkamra.vice.pipeline :as pipeline])

(pipeline/build! ds :disassembly)
;; builds :dedupe; :features and :audio; :classification; :segments;
;; then :disassembly

(pipeline/build! ds :all)                       ; includes :video and :assets
(pipeline/build! ds :disassembly {:force #{:dedupe}})
```

Targets are `:dedupe`, `:video`, `:assets`, `:audio`, `:features`,
`:classification`, `:segments`, `:disassembly`, `:report`, a collection of
targets, or `:all`. Builds are signature-based and reuse committed generations;
`:force` rebuilds a stage and causes requested downstream stages to rebuild.
Independent stages are built concurrently by default. Use `{:stage-parallelism n}` on a
memory-constrained host.

To inspect committed stage pointers:

```clojure
(omkamra.vice.analysis/read-manifest ds)
```

## 2. Read the disassembly and report

```clojure
(require '[omkamra.vice.disassembly :as disassembly])
(def asm (disassembly/open ds))
(:files asm) (:roles asm)
(disassembly/read-file asm "roles/raster.asm")

(pipeline/build! ds :report)
(require '[omkamra.vice.report :as report])
(report/html (report/open ds))
```

Disassembly files are generated from executed, deduplicated blocks:

- `segments/segment-NNN.asm` and `.clj` are per-semantic-segment views;
- `roles/*.asm` are filtered views for `:raster`, `:music`, `:loader`, and
  `:decruncher` by default;
- the listing annotates block/context, predecessors, successors, execution
  counts, and repeats, and adds C64 register comments.

Executed blocks are not a complete static disassembly. Use a RAM image to
inspect code or data that never executed.

## 3. Establish the execution structure

```clojure
(require '[omkamra.vice.segments :as segments])
(mapv #(select-keys % [:segment-id :kind :event-range :roles
                       :handler-contexts :phases])
      (segments/segments (segments/open ds)))
```

The pipeline distinguishes:

- `:demopart`: a stable set of program IRQ handlers and/or visible raster work;
- `:transition`: regions without those display handlers; it may contain loader
  or decruncher evidence;
- roles (`:raster`, `:music`, `:loader`, `:decruncher`, `:idle`) as evidence,
  not necessarily part boundaries;
- `:phases` as role intervals within a segment.

For lower-level context information:

```clojure
(require '[omkamra.vice.dedupe :as dedupe])
(def trace (dedupe/open ds))
(dedupe/system-contexts trace) ; stock KERNAL IRQ/NMI/BRK contexts
(dedupe/read-context trace context-id)
```

Context id 0 is the initial main context. Interrupt contexts carry a handler
address and retain their observed `:kind` (`:irq`, `:nmi`, or `:brk`), even
when a long-lived interrupt frame is later marked `:effective-level :mainline`.
A demo whose mainline is a self-loop may put nearly all useful work in IRQs,
but verify this from the trace.

`features` exposes logical execution units and write-to-execute handoffs:

```clojure
(require '[omkamra.vice.features :as features])
(def fs (features/open ds))
(features/reduce-units fs rf init) ; streaming; preferred for large captures
(features/units fs)                ; eager convenience
(features/handoffs fs)
```

## 4. Reconstruct a frame or routine

Read deduplicated occurrences over a global event range and map block ids back
to the disassembly:

```clojure
(def r (dedupe/read-range trace start end))
;; :occurrences, :port-timeline, :context-switches, :levelings
;; occurrence: :frame-id :context-id :block-id :context-kind
;;             :effective-level :fragments :start-cycle :end-cycle
```

Group occurrences by execution frame/context, then run-length encode
consecutive equal block ids to reveal straight-line loops, waits, and effect
order. If a loop alternates blocks, identify the repeating block sequence
instead of only collapsing adjacent duplicates. `dedupe/scan-occurrences` is
the allocation-free alternative.

## 5. Measure hardware timing

```clojure
(require '[omkamra.vice.reader :as reader])
(def cap (reader/open ds))
(def writes (reader/read-writes cap [start end]))
;; :event-index :pc :address :value :kind :domain
;; :canonical-address :register :global-cycle :raster-line :cpu-cycle
```

Filter by `:domain` (`:vic`, `:sid`, `:cia1`, `:cia2`, `:color-ram`, `:ram`,
`:cpu-port`, or generic `:io`) and collapse repeated `[pc address value]` writes
when summarizing initialization. For raster effects, measure `$D011`, `$D016`, `$D021`, and
`$D020` writes by `:raster-line`; do not infer timing from a single screenshot.
Use `reader/scan-vic-writes` and `reader/scan-sid-writes` for filtered scans.

For instruction-level data, `reader/read-events`/`reader/scan-events` provide decoded
maps, while `reader/reduce-events` provides a reusable primitive buffer for
large scans. Self-modifying operands can appear as `??` in disassembly; find
the write to that operand address, then inspect the executed event's actual
`:instruction-bytes` and registers.

## 6. Inspect memory and replay state

Each capture chunk contains a 64 KiB underlying RAM image (`.ram`) and a 64 KiB
CPU-visible image (`.mem`). Colour RAM is separate and is available in the
canonical snapshot. Useful snapshot fields are `:cpu`, `:timing`, `:registers`
(`:vic`, `:sid`, `:cia1`, `:cia2`), `:cpu-port`, `:color-ram`, and `:semantics`.

```clojure
(reader/boundary-snapshots cap)
(reader/boundary-snapshot cap event-index) ; event-index must be a chunk start
(reader/replay-state cap event-index)
```

Replay reconstructs RAM, colour RAM, CPU-port state, and device write shadows
from a boundary. It does not model every opaque hardware side effect; consult
`:capabilities` in the result. `reader/scan-replay-states` and
`reader/replay-states` avoid replaying the same prefix repeatedly.

For display assets, build and open `:video` and `:assets`:

```clojure
(require '[omkamra.vice.assets :as assets]
         '[omkamra.vice.video :as video])
(def vi (video/open ds))
(video/frames vi)
(video/configurations vi)
(video/expand-frame vi frame)
(def ai (assets/open ds))
(assets/snapshots ai)
(assets/snapshot-ram ai snapshot-id)
(assets/snapshot-color-ram ai snapshot-id)
```

Assets snapshots cover the initial VIC layout and each layout change. Their
payload is exact underlying RAM followed by 1 KiB colour RAM. Start with a
region map: identify non-zero pages and code/data boundaries,
then extract tables between them. Fit suspected motion tables (for example a
sine) and check residuals rather than transcribing bytes blindly.

## 7. Decode common C64 structures

- Screen matrix row: `screen-base + 40*row`; colour row: `$D800 + 40*row`.
- Screen codes are not ASCII. For common PETSCII screen text, `$01–$1A` are
  `A–Z`, `$20` is space, `$30–$39` digits, `$2E` `.`, `$3A` `:`, and `$27` `'`.
- A charset is 2 KiB; if all used screen codes are ≤63, the first 512 bytes may
  be sufficient.
- A multicolour sprite is 63 bytes, three per line; pairs `00` are transparent,
  `01`/`11` select `$D025`/`$D026`, and `10` selects the sprite colour. X-expanded
  pixels are doubled.
- Sprite data pointers are the last eight bytes of the VIC screen matrix
  (`screen-base + $3F8`), not `$D027–$D02E` (sprite colours).
- `$D020`/`$D021` streams often encode rasterbars; values are C64 palette
  indices. `$D016` writes can be the horizontal-scroll part of a scroller.
- SID writes at `$D400+` identify audio activity. A music routine may be a small
  VM consuming its own bytecode; use the `:audio` stage's candidate blocks and
  contexts as evidence, not as proof of a particular music format.

## 8. Compare against screenshots

```clojure
(reader/chunk-screenshots cap)
(reader/screenshot-file cap (first (:chunks cap))) ; expects a chunk entry
```

A screenshot is the full VICE debug frame, including the border. Current PAL
VICE returns 504×312 with a 320×200 inner area at `(136, 51)`, but crop using
the recorded geometry rather than assuming it. Classify pixels by palette
colour per raster line to
measure bars, sprites, gradients, and splits. The image is captured at a chunk
boundary, so verify moving-object coordinates against the corresponding trace
writes.

## 9. Record and verify findings

Keep a short analysis beside the example or dataset: memory map, boot chain,
per-frame timeline with measured raster lines, tables/assets, traps, and a
reimplementation plan. Mark observations separately from inferences. Verify
reimplemented code by comparing assembled bytes with RAM and comparing several
boundary screenshots—not just one.

Run checks through the project nREPL rather than a fresh JVM. After changing
Clojure source, use the `clj-reload` skill before evaluating it.

## API quick reference

| area | useful entry points |
|---|---|
| capture reader | `open`, `read-events`, `reduce-events`, `event-pairs`, `read-writes`, `scan-writes`, `scan-accesses`, `scan-vic-writes`, `scan-sid-writes`, `boundary-snapshot`, `replay-state`, `chunk-screenshots` |
| dedupe | `open`, `read-range`, `read-block`, `read-context`, `scan-occurrences`, `system-contexts`, `port-at` |
| features/classification | `features/open`, `units`, `handoffs`; `classification/open`, `spans` |
| segments/disassembly | `segments/open`, `segments`, `file-groups`, `audit`; `disassembly/open`, `read-file` |
| projections | `video/open`, `frames`, `expand-frame`, `vic-writes`; `assets/open`, `snapshots`, `read-payload`; `audio/open`, `evidence`, `music-candidates` |
| pipeline/report | `pipeline/build!`; `analysis/read-manifest`; `report/open`, `html`, `html-file` |

## Traps

- Chunk boundaries are not frame boundaries. Dedupe frame ids are dynamic
  execution frames: frame 0 starts the main context, interrupt entries get new
  ids, and a long-lived interrupt frame may later have `:effective-level
  :mainline`. Raw chunks are storage partitions and may cut across frames.
- Device mirrors are canonicalized, so inspect `:canonical-address`/`:register`
  rather than treating `$D0xx` mirrors as separate registers.
- The capture's VIC value blacklist can preserve write order and timing while
  omitting values. Required layout registers (`$D011`, `$D016`, `$D018`,
  `$DD00`) are protected; use the trace for omitted or write-only values.
- `dedupe/read-block` instruction records contain `:opcode`, `:width`, operand
  `:template`, and `:volatile-mask`; `:template` is operand bytes, not a full
  instruction byte sequence.
- Snapshot device registers have the semantics recorded in `:semantics`, but
  write-only or opaque device state is not generally recoverable from a read.
  Trust the write trace when reconstructing it.
- `:ram` is underlying RAM; `:mem` is CPU-visible memory. Colour RAM is neither.
- Program-only captures begin at the program entry path. If initialization is
  missing, inspect the `:transition` segment and the first segment before
  assuming it was not traced.
