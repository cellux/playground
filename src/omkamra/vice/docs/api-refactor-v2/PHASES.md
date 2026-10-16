# Implementation Phases

## Purpose

This file turns the milestone specification in `PLAN.md` into an ordered set of implementable, independently testable phases after the implementing agent has thoroughly analyzed the plan and codebase. A completed phase is final and must not be changed; implementation progress is recorded separately in the append-only `DIARY.md`.

## Phase 1 — Separate analysis persistence infrastructure

Move the current manifest, generation, transaction, locking, and progress helpers
from `omkamra.vice.analysis` into `omkamra.vice.analysis.core`. Update every
pipeline/stage implementation and its tests to use the new internal namespace.
Preserve behavior and formats exactly. Verify with focused analysis/pipeline and
stage tests plus linting.

## Phase 2 — Split VICE implementation from the root facade

Move the existing direct implementation from `omkamra.vice` into
`omkamra.vice.core`. Preserve `with-conn` as a macro and move implementation
default constants to `core`. Rebuild `omkamra.vice` as a documented facade for
assembly, lifecycle, capture, and later analysis proxies. Verify root API and
lifecycle tests.

## Phase 3 — Add the high-level analysis workflow API

Implement `build-pipeline!`, `open-analysis`, `list-segments`, and `get-segment`
in `omkamra.vice.analysis`, using the explicit result contract in `PLAN.md`.
Open only committed stages; tolerate absent stages; propagate validation errors
for corrupt committed stages. Add focused synthetic-dataset tests for overview,
segment references, missing capabilities, and drill-down behavior. Add root
proxies and docstrings.

## Phase 4 — Migrate documentation and workspace callers

Consolidate the operational guidance from the three VICE/C64 skill files into
namespace and function docstrings and examples. Update workspace examples,
tests, and prose that demonstrate the public API. Remove only the three
superseded skill files and verify no references remain.

## Phase 5 — Full verification and handoff

Run the relevant test suite, `clj_kondo`, and `cljfmt`. Exercise the complete
capture/pipeline/open/segment workflow where the environment supports it.
Review the final public vars and docstrings against `PLAN.md`, then record the
completed phase in `DIARY.md`.
