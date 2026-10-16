# Design Decisions

## Purpose

This file records design decisions that remain in force across implementation phases. Decisions are never deleted: when an answer changes, the previous decision is marked `superseded by Dn` and a new numbered decision is added. `PHASES.md` references applicable decisions, while `DIARY.md` records when a decision was resolved rather than duplicating the decision itself.

## D1 — Root namespace is the discoverable workflow facade

`omkamra.vice` is the single documented entrypoint for high-level VICE,
capture, and analysis workflows. It exposes lightweight proxies rather than
re-exporting every lower-level operation.

## D2 — Separate implementation layers to avoid namespace cycles

`omkamra.vice.core` owns the existing direct VICE lifecycle, assembly, and
capture facade implementation. `omkamra.vice.analysis.core` owns analysis
manifest/generation/transaction/progress primitives. `omkamra.vice.analysis`
owns high-level analysis workflows and may depend on `pipeline` and stage
readers; pipeline and stage implementations depend only on `analysis.core`.

## D3 — Inspection never builds implicitly

`build-pipeline!` explicitly creates or reuses committed analysis generations.
`open-analysis` only validates and opens the finalized capture and committed
stages; absent optional stages are represented as unavailable capabilities.
Manifest-committed but corrupt or incompatible stages fail validation rather
than being silently treated as absent.

## D4 — Analysis results are handles plus compact references

`open-analysis` returns existing reader/image handles under stable stage keys and
a compact `:overview`. `list-segments` and `get-segment` return metadata and
references to lower-level APIs/files/ranges, not raw events, assembly text, or
screenshot bytes.

## D5 — Public compatibility is workspace-scoped

There are no external dependents. Existing workspace callers may be updated to
the reorganized API, while the root workflow names and lower-level data formats
specified by the plan remain stable.
