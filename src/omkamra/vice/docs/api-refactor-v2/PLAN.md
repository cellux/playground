# Plan: Make `omkamra.vice` the VICE/C64 API entrypoint

## Purpose

Refactor the VICE and C64 capture/analysis API so that an agent can start at
`omkamra.vice`, read its namespace and function docstrings, discover the
available high-level workflows, and drill into the existing specialized
namespaces when it needs detailed data or controls.

This is an in-workspace API reorganization. There are no external dependents;
existing callers in this workspace may be updated to the new API.

## Goals

- Make `omkamra.vice` the single documented entrypoint for VICE lifecycle,
  assembly, capture, and analysis workflows.
- Keep facade functions lightweight: implementation belongs in
  `omkamra.vice.core` for existing direct VICE functionality and in
  `omkamra.vice.analysis` for high-level analysis operations.
- Make the root namespace docstring a concise API map, with function docstrings
  explaining inputs, returned values, workflow boundaries, and links to the
  lower-level namespaces that contain detailed operations.
- Consolidate the useful operational guidance from the `vice`, `c64-capture`,
  and `c64-analysis` skills into Clojure namespace/function docstrings, then
  remove those three skill files.
- Preserve the existing lower-level namespaces and their detailed APIs. The
  root facade should expose high-level workflow entrypoints, not every reader,
  stage, monitor, or analysis operation.

## Root facade

`omkamra.vice` should expose documented proxies for:

- `assemble` and `link`;
- `start!`, `connect!`, `with-conn`, and `stop!`;
- `capture!`;
- `build-pipeline!`;
- `open-analysis`;
- `list-segments`;
- `get-segment`.

Existing direct lifecycle/assembly/capture implementation currently in
`src/omkamra/vice.clj` should move to `src/omkamra/vice/core.clj`. The root
namespace should retain the public names as thin delegates to `core`, rather
than exposing implementation details or breaking workspace callers. Preserve
`with-conn` as a thin macro proxy because it must capture the caller's body and
bindings; it is not an ordinary function delegate. Move the current default
configuration constants (`default-executable`, `default-address`,
`default-port`, `default-connect-timeout-ms`, and `default-connect-retry-ms`)
to `omkamra.vice.core` and omit them from the root workflow facade, since they
are implementation defaults rather than high-level entrypoints.

The root docstring should explain the normal workflows:

1. prepare an artifact with `assemble`/`link`, or provide an input to
   `capture!`;
2. capture and finalize a dataset;
3. call `build-pipeline!` explicitly for the required analysis targets;
4. call `open-analysis` to inspect already-committed results;
5. use `list-segments` and `get-segment` for overview and drill-down;
6. follow the returned references into `reader`, `dedupe`, `segments`,
   `disassembly`, `report`, and other stage namespaces.

## High-level analysis API in `omkamra.vice.analysis`

Add the following user-facing functions to `omkamra.vice.analysis`; the root
namespace delegates to them. Keep the moved manifest/generation primitives in
`omkamra.vice.analysis.core` as the internal persistence layer used by the
pipeline and stage implementations.

### `build-pipeline!`

Provide a high-level wrapper around `omkamra.vice.pipeline/build!` with a
workflow-oriented docstring. It must make clear that it accepts a finalized
dataset root and a stage target (`:dedupe`, `:video`, `:assets`, `:audio`,
`:features`, `:classification`, `:segments`, `:disassembly`, `:report`, a
collection, or `:all`), that it builds dependencies and reuses committed
matching generations, and that it returns opened stage images plus `:built` and
`:reused` information.

The caller must explicitly run this function before inspection. `open-analysis`
will not build missing stages.

Because `pipeline` and the analysis stages already depend on `analysis` for
manifest primitives, requiring `pipeline` or stage readers from that same
namespace would introduce namespace-load cycles. Move the existing manifest,
generation, transaction, and progress infrastructure into a lower-level
`omkamra.vice.analysis.core` namespace. Update `pipeline` and all stage
namespaces to depend on `analysis.core`; make `omkamra.vice.analysis` the
high-level analysis API namespace. It may then require `pipeline` and stage
readers directly while preserving a one-way dependency direction.

### `open-analysis`

Open a finalized dataset and whatever committed analysis stages are available;
do not build anything. The result has this explicit shape:

```clojure
{:dataset-root ...
 :capture       capture-reader
 :overview      {:capture-id ...
                 :event-count ...
                 :chunk-count ...
                 :available-stages #{...}
                 :missing-stages #{...}
                 :stage-summaries {...}}
 :dedupe        dedupe-image-or-nil
 :video         video-image-or-nil
 :assets        assets-image-or-nil
 :audio         audio-image-or-nil
 :features      features-image-or-nil
 :classification classification-image-or-nil
 :segments      segments-image-or-nil
 :disassembly   disassembly-image-or-nil
 :report        report-image-or-nil}
```

Stage keys are present with nil values when their generation is not committed.
The overview gives agents a compact execution/analysis summary and makes
missing capabilities explicit. Stage summaries should come from the existing
manifest/image metadata, including segment count when available.

The function should fail clearly for an invalid or non-finalized capture, but
should not fail merely because optional analysis stages are absent. If the
manifest claims that a stage is committed, however, that stage's existing
`open!` validation failure must propagate rather than being converted into a
missing-stage result; corrupt or incompatible generations must remain visible
as errors. Stage opening should validate committed generations through the
existing stage `open!` functions rather than duplicating their formats or
readers.

The returned handles are the existing reader/image maps, not a new heavy data
model. The docstring must describe this exact map shape and show a minimal
`list-segments`/`get-segment` drill-down example.

### `list-segments`

Signature:

```clojure
(list-segments analysis)
```

Return compact segment overviews from the `:segments` image in an
`open-analysis` result. Each overview should retain the information needed to
understand the execution partition—segment id, kind, roles, event range,
phases/contexts, timing/count summary where already available, and references
needed for drill-down—without embedding raw events, full assembly text, or
image bytes. If `:disassembly` is available, include the committed segment
assembly/object paths; if `:report` is available, include the report's curated
screenshot descriptors. These are references, not loaded content.

If the segments stage is unavailable, fail with a clear explanation that the
caller must run `build-pipeline!` for `:segments` (or a dependent target) first.

### `get-segment`

Signature:

```clojure
(get-segment analysis segment-id)
```

Return one compact segment overview. It should combine the segment metadata
with explicit drill-down references, while avoiding eager loading of the
segment's raw events, disassembly text, or screenshot bytes. The result should
reuse the same reference fields as `list-segments`, with the selected segment
identified by `:segment-id`. References should make it obvious how to use:

- the `:capture` reader with the segment's global `:event-range` for raw events,
  writes, replay state, and chunk boundaries;
- the dedupe image and `omkamra.vice.dedupe` for structural occurrences and
  blocks;
- the disassembly image and its committed `:asm-file`/`:clj-file` paths with
  `omkamra.vice.disassembly/read-file`;
- the report image and/or capture reader for curated and raw screenshot access;
- the existing `segments` image for file groups and boundary audit when useful.

The docstring must distinguish global logical event ranges from raw chunk
boundaries and point to the relevant lower-level function docstrings.

## Documentation and skill migration

- Rewrite the `omkamra.vice` namespace docstring and all facade function
  docstrings as the primary overview of the API.
- Add/expand docstrings for the new `omkamra.vice.analysis` functions,
  especially the returned map shape and drill-down references.
- Retain detailed implementation and data-model documentation in the existing
  specialized namespaces (`reader`, `capture`, `pipeline`, `dedupe`,
  `segments`, `disassembly`, `report`, and stage namespaces), adding cross-
  references where needed.
- Migrate the actionable material from:
  - `.agents/skills/vice/SKILL.md`
  - `.agents/skills/c64-capture/SKILL.md`
  - `.agents/skills/c64-analysis/SKILL.md`
  into the relevant docstrings/examples.
- Remove those three skill files after their content has been incorporated.
  Do not remove unrelated skills.
- Update in-workspace examples, tests, and documentation references to use the
  new facade where they are intended to demonstrate the public API. Keep
  specialized namespace usage in implementation-focused tests and examples
  where it is the subject under test.

## Verification

Add or update tests to verify:

- `omkamra.vice` exports the intended facade names and delegates correctly,
  including macro behavior for `with-conn`;
- the implementation-default constants live in `omkamra.vice.core` and are not
  exposed as root workflow API;
- old implementation vars are no longer defined in the root namespace and
  direct implementation now lives in `omkamra.vice.core`;
- `omkamra.vice.analysis.core` preserves the existing manifest/generation and
  transaction behavior, and all pipeline/stage dependencies are updated to use
  it;
- `omkamra.vice.analysis/build-pipeline!` delegates without introducing a
  namespace-load cycle;
- `open-analysis` opens the finalized capture and any committed stages while
  tolerating absent optional stages, reports available capabilities, and
  propagates validation failures for manifest-committed corrupt/incompatible
  stages;
- `list-segments` returns compact metadata without eagerly materializing raw
  events, assembly text, or screenshot bytes;
- `get-segment` resolves valid ids, exposes the expected event/file/screenshot
  references, and gives a clear error for missing ids or missing stages;
- the complete capture → pipeline → open → segment drill-down works with the
  existing synthetic analysis fixtures;
- the migrated documentation is discoverable through namespace and function
  docstrings; and
- the three obsolete skill files are absent and no workspace references still
  direct agents to them.

Run the relevant Clojure tests, `clj_kondo`, and `cljfmt` after implementation.
